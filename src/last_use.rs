//! Finds where each variable's value is used for the last time, so that both backends can move
//! a value instead of keeping a copy of it.
//!
//! Lists are values, so `ys = add(xs, 1)` gives `add` a copy of `xs`. When nothing reads `xs`
//! afterward, the copy is never needed: the caller can hand over `xs` itself, and `add` changes
//! it in place. [`live_after`] says which variables are still read after each statement, and
//! [`last_uses`] which variables a statement uses for the last time.

use crate::ast::{CompOp, Expr, ExprKind, Stmt, StmtKind, TypeExprKind, target_names};
use crate::checker::collect_assigned;
use crate::span::Span;
use crate::stdlib::{is_builtin, prelude_method};
use std::collections::{HashMap, HashSet};

/// For each function, the globals it or any function it calls may read, by the function's name,
/// which for a function of an `impl` block is its struct's name, a dot, and its own name.
#[derive(Debug, Default)]
pub(crate) struct GlobalReads {
    functions: HashMap<String, HashSet<String>>,
    /// The name of every function of an `impl` block, by its own name, since a call such as
    /// `x.get()` may call any of them named `get`: which struct `x` is, nothing here knows.
    methods: HashMap<String, Vec<String>>,
    /// The globals a call of a function value may read: those of every function the program
    /// names as a value, since which one a value holds, nothing here knows.
    values: HashSet<String>,
    /// The name of every struct field that may hold a function, written as a function type or a
    /// type parameter, since `p.run(x)` calls the function a field `run` holds.
    value_fields: HashSet<String>,
}

impl GlobalReads {
    /// Returns the globals the function `name` may read, if it is a function of the program.
    pub(crate) fn get(&self, name: &str) -> Option<&HashSet<String>> {
        self.functions.get(name)
    }

    /// Returns whether `name` is a function of the program.
    pub(crate) fn contains_key(&self, name: &str) -> bool {
        self.functions.contains_key(name)
    }

    /// Returns the globals a call of a function value may read, as `f(x)` with `f` a parameter
    /// does: those of every function the program names as a value.
    pub(crate) fn of_values(&self) -> &HashSet<String> {
        &self.values
    }

    /// Returns the functions of `impl` blocks a call of `name` after a dot may call, as in
    /// `p.get()` or `Point.origin()`.
    fn methods_named(&self, name: &str) -> &[String] {
        self.methods.get(name).map_or(&[], Vec::as_slice)
    }
}

impl std::ops::Index<&str> for GlobalReads {
    type Output = HashSet<String>;

    fn index(&self, name: &str) -> &HashSet<String> {
        &self.functions[name]
    }
}

/// Returns what a function with the parameters `args` returns besides its value, which must stay
/// live to its end: `self`, for a method, since one that changes `self` returns it to its caller.
///
/// For example, `fn push(self, x: int)` keeps `self`, and `fn f(x: int)` nothing.
pub(crate) fn kept_at_exit(args: &[crate::ast::Arg]) -> HashSet<String> {
    args.first()
        .filter(|arg| arg.arg == "self")
        .map(|arg| arg.arg.clone())
        .into_iter()
        .collect()
}

/// The names code mentions: variables and functions, and the names after a dot of the methods
/// it calls, as `get` in `p.get()`.
#[derive(Default)]
pub(crate) struct Names<'a> {
    pub(crate) names: Vec<&'a str>,
    methods: Vec<&'a str>,
    /// The names among `names` that are called, as `f` in `f(x)`, which is a call of a function
    /// value if `f` is a variable.
    called: Vec<&'a str>,
    /// The names among `names` mentioned other than by being called, as `f` in `g(f)`, which
    /// names a function as a value if `f` is a function.
    values: Vec<&'a str>,
    /// Whether the code calls a value that is no name, as in `fs[0](x)`.
    calls_value: bool,
}

impl Names<'_> {
    /// Returns whether the code calls a function value, given `is_function`, which tells a
    /// function's name from a variable's, and `value_fields`, the fields that may hold one.
    fn calls_a_value(
        &self,
        is_function: impl Fn(&str) -> bool,
        value_fields: &HashSet<String>,
    ) -> bool {
        self.calls_value
            || self.methods.iter().any(|name| value_fields.contains(*name))
            || self
                .called
                .iter()
                .any(|name| !is_function(name) && !is_builtin(name))
    }
}

/// Returns the name of every field of a struct in `body` that may hold a function: one written
/// as a function type, or as a type parameter, which a function type may fill in.
fn value_fields(body: &[Stmt]) -> HashSet<String> {
    let mut fields = HashSet::new();
    for stmt in body {
        if let StmtKind::StructDef {
            fields: written,
            type_params,
            ..
        } = &stmt.kind
        {
            for field in written {
                let holds = match &field.annotation.kind {
                    TypeExprKind::Function { .. } => true,
                    TypeExprKind::Named { name, .. } => type_params.iter().any(|p| p.name == *name),
                    _ => false,
                };
                if holds {
                    fields.insert(field.name.clone());
                }
            }
        }
    }
    fields
}

/// Returns every function `body` defines, with its parameters and body: each top-level `fn`,
/// and each function of an `impl` block or a trait, named by its struct's or trait's name, a
/// dot, and its own name.
fn functions(body: &[Stmt]) -> Vec<(String, &crate::ast::Arguments, &[Stmt])> {
    let mut functions = vec![];
    for stmt in body {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => functions.push((name.clone(), args, body.as_slice())),
            StmtKind::ImplDef {
                name: owner,
                methods,
                ..
            }
            | StmtKind::TraitDef {
                name: owner,
                methods,
                ..
            } => {
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name, args, body, ..
                    } = &method.kind
                    {
                        functions.push((format!("{owner}.{name}"), args, body.as_slice()));
                    }
                }
            }
            _ => {}
        }
    }
    functions
}

/// For each statement, by its span, the variables that some later code may still read.
pub(crate) type LiveAfter = HashMap<Span, HashSet<String>>;

/// Returns, for each function in `body`, every global in `globals` that it or a function it calls,
/// directly or not, may read.
///
/// For example, after `fn f() -> int: ret g()` and `fn g() -> int: ret xs[0]`, both `f` and `g`
/// read `xs`.
pub(crate) fn global_reads(body: &[Stmt], globals: &HashSet<String>) -> GlobalReads {
    let mut reads = GlobalReads {
        value_fields: value_fields(body),
        ..GlobalReads::default()
    };
    let mut calls = HashMap::new();
    let functions = functions(body);
    for (name, _, _) in &functions {
        if let Some((_, method)) = name.rsplit_once('.')
            && is_method(body, name)
        {
            reads
                .methods
                .entry(method.to_string())
                .or_default()
                .push(name.clone());
        }
    }
    let is_function = |name: &str| functions.iter().any(|(f, _, _)| f == name);
    // every function named as a value, anywhere, since a call of a value may call any of them
    let mut valued: HashSet<String> = HashSet::new();
    let mut top = Names::default();
    for stmt in body {
        stmt_names(stmt, &mut top);
    }
    let bodies = functions.iter().map(|(_, _, body)| *body);
    for body in bodies {
        let mut names = Names::default();
        for stmt in body {
            stmt_names(stmt, &mut names);
        }
        valued.extend(
            names
                .values
                .iter()
                .filter(|name| is_function(name))
                .map(|name| name.to_string()),
        );
    }
    valued.extend(
        top.values
            .iter()
            .filter(|name| is_function(name))
            .map(|name| name.to_string()),
    );
    for (name, args, body) in &functions {
        let mut locals: HashSet<String> = args.args.iter().map(|arg| arg.arg.clone()).collect();
        let mut assigned = Vec::new();
        collect_assigned(body, &mut assigned);
        locals.extend(assigned.into_iter().map(|(name, _)| name));
        let mut names = Names::default();
        for stmt in *body {
            stmt_names(stmt, &mut names);
        }
        let read: HashSet<String> = names
            .names
            .iter()
            .filter(|name| globals.contains(**name) && !locals.contains(**name))
            .map(|name| name.to_string())
            .collect();
        let mut called: HashSet<String> = names
            .names
            .iter()
            .filter(|name| functions.iter().any(|(f, _, _)| f == *name))
            .map(|name| name.to_string())
            .collect();
        for method in &names.methods {
            called.extend(reads.methods_named(method).iter().cloned());
        }
        let is_function = |name: &str| is_function(name) && !locals.contains(name);
        if names.calls_a_value(is_function, &reads.value_fields) {
            called.extend(valued.iter().cloned());
        }
        reads.functions.insert(name.clone(), read);
        calls.insert(name.clone(), called);
    }
    // a function reads whatever the functions it calls read, until nothing more is added
    let mut changed = true;
    while changed {
        changed = false;
        for (name, called) in &calls {
            let inherited: HashSet<String> = called
                .iter()
                .flat_map(|callee| reads.functions[callee].iter().cloned())
                .collect();
            let read = reads
                .functions
                .get_mut(name)
                .expect("every function has reads");
            let before = read.len();
            read.extend(inherited);
            changed |= read.len() != before;
        }
    }
    reads.values = valued
        .iter()
        .flat_map(|function| reads.functions[function].iter().cloned())
        .collect();
    reads
}

/// Returns whether `name` is the name of a function of an `impl` block or a trait in `body`.
fn is_method(body: &[Stmt], name: &str) -> bool {
    body.iter().any(|stmt| match &stmt.kind {
        StmtKind::ImplDef {
            name: owner,
            methods,
            ..
        }
        | StmtKind::TraitDef {
            name: owner,
            methods,
            ..
        } => methods.iter().any(|method| {
            matches!(&method.kind, StmtKind::FunctionDef { name: m, .. } if format!("{owner}.{m}") == name)
        }),
        _ => false,
    })
}

/// Collects every name `stmt` mentions, including in nested blocks, assignment targets, and
/// called functions.
pub(crate) fn stmt_names<'a>(stmt: &'a Stmt, out: &mut Names<'a>) {
    let block = |body: &'a [Stmt], out: &mut Names<'a>| {
        for stmt in body {
            stmt_names(stmt, out);
        }
    };
    match &stmt.kind {
        StmtKind::Assign { targets, value } => {
            targets.iter().for_each(|target| expr_names(target, out));
            expr_names(value, out);
        }
        StmtKind::AugAssign { target, value, .. } => {
            expr_names(target, out);
            expr_names(value, out);
        }
        StmtKind::Expr { value } | StmtKind::Return { value: Some(value) } => {
            expr_names(value, out)
        }
        StmtKind::If { test, body, orelse } => {
            expr_names(test, out);
            block(body, out);
            block(orelse, out);
        }
        StmtKind::While { test, body } => {
            expr_names(test, out);
            block(body, out);
        }
        StmtKind::For { target, iter, body } => {
            expr_names(target, out);
            expr_names(iter, out);
            block(body, out);
        }
        StmtKind::Match { subject, cases } => {
            expr_names(subject, out);
            for case in cases {
                let mut names = Vec::new();
                case.pattern.names(&mut names);
                out.names.extend(names.into_iter().map(|(name, _)| name));
                block(&case.body, out);
            }
        }
        StmtKind::Return { value: None }
        | StmtKind::FunctionDef { .. }
        | StmtKind::EnumDef { .. }
        | StmtKind::StructDef { .. }
        | StmtKind::ImplDef { .. }
        | StmtKind::TraitDef { .. }
        | StmtKind::Use { .. }
        | StmtKind::Break
        | StmtKind::Continue => {}
    }
}

/// Removes from `live` each name the target `target` assigns: a name, or every name of a tuple
/// target, as in `a, (b, c) = t`.
fn kill_targets(target: &Expr, live: &mut HashSet<String>) {
    let mut names = Vec::new();
    target_names(target, &mut names);
    for name in names {
        if let ExprKind::Name { id, .. } = &name.kind {
            live.remove(id);
        }
    }
}

/// Collects every name `expr` mentions, including the names of called functions and methods.
fn expr_names<'a>(expr: &'a Expr, out: &mut Names<'a>) {
    match &expr.kind {
        ExprKind::Name { .. } if expr.is_unwritten() => {}
        ExprKind::Name { id, .. } => {
            out.names.push(id);
            out.values.push(id);
        }
        ExprKind::Constant { .. } => {}
        ExprKind::BinOp { left, right, .. } => {
            expr_names(left, out);
            expr_names(right, out);
        }
        ExprKind::UnaryOp { operand, .. } | ExprKind::Try { value: operand } => {
            expr_names(operand, out)
        }
        ExprKind::BoolOp { values, .. }
        | ExprKind::List { elts: values, .. }
        | ExprKind::Tuple { elts: values, .. } => {
            values.iter().for_each(|value| expr_names(value, out))
        }
        // a struct among the values may be written as its own `display` says
        ExprKind::JoinedStr { values } => {
            out.methods.push("display");
            values.iter().for_each(|value| expr_names(value, out))
        }
        ExprKind::Dict { keys, values } => {
            for (key, value) in keys.iter().zip(values) {
                expr_names(key, out);
                expr_names(value, out);
            }
        }
        ExprKind::Compare {
            left,
            ops,
            comparators,
        } => {
            // a struct may compare with its own `eq` and order with its `cmp`, and `in` compares
            // a list's elements with `eq`
            for op in ops {
                out.methods.push(match op {
                    CompOp::Equal | CompOp::NotEqual | CompOp::In | CompOp::NotIn => "eq",
                    _ => "cmp",
                });
            }
            expr_names(left, out);
            comparators.iter().for_each(|value| expr_names(value, out));
        }
        ExprKind::Call {
            func,
            args,
            keywords,
        } => {
            if let ExprKind::Attribute { attr, .. } = &func.kind {
                out.methods.push(attr);
                // a list's method that the prelude gives calls its function
                if let Some(function) = prelude_method(attr) {
                    out.names.push(function);
                }
                // a list of a struct sorts by its `cmp`, and finds an element by its `eq`
                match attr.as_str() {
                    "sort" => out.methods.push("cmp"),
                    "contains" => out.methods.push("eq"),
                    _ => {}
                }
            }
            // a struct may be written as its own `display` says
            if let ExprKind::Name { id, .. } = &func.kind
                && matches!(id.as_str(), "print" | "eprint" | "str")
            {
                out.methods.push("display");
            }
            match &func.kind {
                ExprKind::Name { id, .. } if !func.is_unwritten() => {
                    out.names.push(id);
                    out.called.push(id);
                }
                ExprKind::Name { .. } | ExprKind::Attribute { .. } => expr_names(func, out),
                _ => {
                    out.calls_value = true;
                    expr_names(func, out);
                }
            }
            args.iter().for_each(|arg| expr_names(arg, out));
            keywords.iter().for_each(|k| expr_names(&k.value, out));
        }
        ExprKind::Subscript { value, slice, .. } => {
            expr_names(value, out);
            expr_names(slice, out);
        }
        ExprKind::Attribute { value, .. } => expr_names(value, out),
        ExprKind::Slice { lower, upper, step } => [lower, upper, step]
            .into_iter()
            .flatten()
            .for_each(|bound| expr_names(bound, out)),
    }
}

/// Returns, for every statement in `body` and the blocks nested in it, the variables that code
/// running after it may read before assigning them again. A call reads every global its function
/// may read, from `reads`.
///
/// Nothing is read after `ret` but `exit`, what the function returns besides its value, as a
/// method that changes `self` returns `self`; and after `break` and `cont` whatever is read where
/// they jump to. For example, in `x = [1]` followed by `y = x` and `print(y)`, `x` is live after
/// the first statement only, and `y` after the second.
pub(crate) fn live_after(body: &[Stmt], reads: &GlobalReads, exit: &HashSet<String>) -> LiveAfter {
    let mut liveness = Liveness {
        reads,
        exit,
        after: LiveAfter::new(),
        loops: Vec::new(),
    };
    liveness.block(body, exit.clone());
    liveness.after
}

/// Returns, for each statement in `body` and the blocks nested in it that uses a variable for the
/// last time, those variables: the ones it mentions that nothing reads after it.
///
/// For example, in `x = [1]` followed by `y = x` and `print(y)`, the second statement is the last
/// use of `x`, and the third of `y`.
pub(crate) fn last_uses(
    body: &[Stmt],
    reads: &GlobalReads,
    exit: &HashSet<String>,
) -> HashMap<Span, Vec<String>> {
    let after = live_after(body, reads, exit);
    let mut uses = HashMap::new();
    let mut statements: Vec<&Stmt> = body.iter().collect();
    while let Some(stmt) = statements.pop() {
        match &stmt.kind {
            StmtKind::If { body, orelse, .. } => statements.extend(body.iter().chain(orelse)),
            StmtKind::While { body, .. } | StmtKind::For { body, .. } => statements.extend(body),
            StmtKind::Match { cases, .. } => {
                statements.extend(cases.iter().flat_map(|case| &case.body))
            }
            _ => {}
        }
        let Some(live) = after.get(&stmt.span) else {
            continue;
        };
        let mut names = Names::default();
        stmt_names(stmt, &mut names);
        let mut last: Vec<String> = Vec::new();
        for name in names.names {
            let variable = !is_builtin(name) && !reads.contains_key(name);
            if variable && !live.contains(name) && !last.iter().any(|seen| seen == name) {
                last.push(name.to_string());
            }
        }
        if !last.is_empty() {
            uses.insert(stmt.span, last);
        }
    }
    uses
}

/// The state of [`live_after`], which walks each block backward from what is live at its end.
struct Liveness<'a> {
    reads: &'a GlobalReads,
    /// What is live where the function returns, besides its value.
    exit: &'a HashSet<String>,
    after: LiveAfter,
    /// For each enclosing loop, innermost last, what is live where `break` and `cont` go.
    loops: Vec<(HashSet<String>, HashSet<String>)>,
}

impl Liveness<'_> {
    /// Returns what is live at the start of `body`, given what is live at its end.
    fn block(&mut self, body: &[Stmt], mut live: HashSet<String>) -> HashSet<String> {
        for stmt in body.iter().rev() {
            live = self.stmt(stmt, live);
        }
        live
    }

    /// Records what is live after `stmt`, given what is live after it in order, and returns what
    /// is live before it.
    fn stmt(&mut self, stmt: &Stmt, next: HashSet<String>) -> HashSet<String> {
        // nothing after a jump runs, but what runs where it lands
        let out = match &stmt.kind {
            StmtKind::Return { .. } => self.exit.clone(),
            StmtKind::Break => self.loops.last().map(|l| l.0.clone()).unwrap_or_default(),
            StmtKind::Continue => self.loops.last().map(|l| l.1.clone()).unwrap_or_default(),
            _ => next,
        };
        self.after.insert(stmt.span, out.clone());
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                let mut live = out;
                for target in targets {
                    kill_targets(target, &mut live);
                }
                // a changed list or struct is read, along with its indexes
                for target in targets {
                    if let ExprKind::Subscript { .. } | ExprKind::Attribute { .. } = target.kind {
                        self.expr(target, &mut live);
                    }
                }
                self.expr(value, &mut live);
                live
            }
            // the target is read before it is written, along with any indexes
            StmtKind::AugAssign { target, value, .. } => {
                let mut live = out;
                self.expr(target, &mut live);
                self.expr(value, &mut live);
                live
            }
            StmtKind::Expr { value } | StmtKind::Return { value: Some(value) } => {
                let mut live = out;
                self.expr(value, &mut live);
                live
            }
            StmtKind::If { test, body, orelse } => {
                let mut live = self.block(body, out.clone());
                live.extend(self.block(orelse, out));
                self.expr(test, &mut live);
                live
            }
            StmtKind::While { test, body } => {
                // live where the test runs, before each iteration and after the last
                let mut head = out.clone();
                self.expr(test, &mut head);
                loop {
                    self.loops.push((out.clone(), head.clone()));
                    let mut next = self.block(body, head.clone());
                    self.loops.pop();
                    next.extend(out.iter().cloned());
                    self.expr(test, &mut next);
                    if next == head {
                        break head;
                    }
                    head = next;
                }
            }
            StmtKind::For { target, iter, body } => {
                // live before each iteration assigns the loop variable, and after the last
                let mut head = out.clone();
                let mut live = loop {
                    self.loops.push((out.clone(), head.clone()));
                    let mut next = self.block(body, head.clone());
                    self.loops.pop();
                    kill_targets(target, &mut next);
                    next.extend(out.iter().cloned());
                    if next == head {
                        break head;
                    }
                    head = next;
                };
                // the loop holds the list it walks, so only evaluating it reads the variable
                self.expr(iter, &mut live);
                live
            }
            StmtKind::Match { subject, cases } => {
                let mut live = HashSet::new();
                for case in cases {
                    let mut inside = self.block(&case.body, out.clone());
                    let mut names = Vec::new();
                    case.pattern.names(&mut names);
                    for (name, _) in names {
                        inside.remove(name);
                    }
                    live.extend(inside);
                }
                self.expr(subject, &mut live);
                live
            }
            StmtKind::Return { value: None }
            | StmtKind::Break
            | StmtKind::Continue
            | StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
            | StmtKind::Use { .. } => out,
        }
    }

    /// Adds every variable `expr` reads to `live`, including the globals each function it calls
    /// may read.
    fn expr(&self, expr: &Expr, live: &mut HashSet<String>) {
        let mut names = Names::default();
        expr_names(expr, &mut names);
        for method in &names.methods {
            for function in self.reads.methods_named(method) {
                live.extend(self.reads[function].iter().cloned());
            }
        }
        if names.calls_a_value(
            |name| self.reads.contains_key(name),
            &self.reads.value_fields,
        ) {
            live.extend(self.reads.of_values().iter().cloned());
        }
        for name in names.names {
            match self.reads.get(name) {
                Some(globals) => live.extend(globals.iter().cloned()),
                None if is_builtin(name) => {}
                None => {
                    live.insert(name.to_string());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::parse;

    /// Parses `source` and returns, for each top-level statement in order, the variables live
    /// after it, sorted, with the statements nested in the first `fn`, if there is one, first.
    fn live(source: &str) -> Vec<Vec<String>> {
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let mut statements: Vec<&Stmt> = Vec::new();
        for stmt in body {
            collect(stmt, &mut statements);
        }
        let globals = body
            .iter()
            .flat_map(|stmt| {
                let mut assigned = Vec::new();
                collect_assigned(std::slice::from_ref(stmt), &mut assigned);
                assigned.into_iter().map(|(name, _)| name)
            })
            .collect();
        let reads = global_reads(body, &globals);
        let mut after = live_after(body, &reads, &HashSet::new());
        for stmt in body {
            if let StmtKind::FunctionDef { body, .. } = &stmt.kind {
                after.extend(live_after(body, &reads, &HashSet::new()));
            }
        }
        statements
            .into_iter()
            .map(|stmt| {
                let mut names: Vec<String> = after
                    .get(&stmt.span)
                    .map(|live| live.iter().cloned().collect())
                    .unwrap_or_default();
                names.sort();
                names
            })
            .collect()
    }

    /// Collects `stmt` and every statement nested in it, in source order.
    fn collect<'a>(stmt: &'a Stmt, out: &mut Vec<&'a Stmt>) {
        if !matches!(stmt.kind, StmtKind::FunctionDef { .. }) {
            out.push(stmt);
        }
        let nested: Vec<&[Stmt]> = match &stmt.kind {
            StmtKind::If { body, orelse, .. } => vec![body, orelse],
            StmtKind::While { body, .. }
            | StmtKind::For { body, .. }
            | StmtKind::FunctionDef { body, .. } => vec![body],
            _ => vec![],
        };
        for block in nested {
            for stmt in block {
                collect(stmt, out);
            }
        }
    }

    fn names(names: &[&str]) -> Vec<String> {
        names.iter().map(|name| name.to_string()).collect()
    }

    #[test]
    fn a_variable_is_live_until_its_last_read() {
        assert_eq!(
            live("x = [1]\ny = x\nprint(y)\n"),
            [names(&["x"]), names(&["y"]), names(&[])]
        );
    }

    #[test]
    fn assigning_a_variable_ends_its_old_value() {
        assert_eq!(
            live("x = [1]\nx = [2]\nprint(x)\nx = x\n"),
            [names(&[]), names(&["x"]), names(&["x"]), names(&[])]
        );
    }

    #[test]
    fn unpacking_assigns_every_name() {
        // `a` and `b` are not read before the unpacking assigns them
        assert_eq!(
            live("t = (1, 2)\na, (b, c) = (t, 3)\nprint(a, b, c)\n"),
            [names(&["t"]), names(&["a", "b", "c"]), names(&[])]
        );
        assert_eq!(
            live("ps = [(1, 2)]\nfor a, b in ps:\n    print(a)\nprint(ps)\n"),
            [names(&["ps"]), names(&["ps"]), names(&["ps"]), names(&[])]
        );
    }

    #[test]
    fn an_augmented_assignment_reads_its_target() {
        assert_eq!(
            live("x = [1]\ny = [2]\nx += y\nprint(x)\n"),
            [names(&["x"]), names(&["x", "y"]), names(&["x"]), names(&[])]
        );
        assert_eq!(
            live("xs = [1]\ni = 0\nxs[i] += 1\n"),
            [names(&["xs"]), names(&["i", "xs"]), names(&[])]
        );
    }

    #[test]
    fn a_read_on_either_branch_keeps_a_variable_live() {
        let source = "x = [1]\nc = 1\nif c:\n    print(x)\nelse:\n    x = [2]\nprint(c)\n";
        assert_eq!(
            live(source),
            [
                names(&["x"]),
                names(&["c", "x"]),
                names(&["c"]),
                names(&["c"]),
                names(&["c"]),
                names(&[]),
            ]
        );
    }

    #[test]
    fn a_read_in_a_later_iteration_keeps_a_variable_live() {
        let source = "x = [1]\ni = 0\nwhile i < 3:\n    y = x\n    x = [i]\n    i = i + 1\n";
        assert_eq!(
            live(source),
            [
                names(&["x"]),
                names(&["i", "x"]),
                names(&[]),
                names(&["i"]),
                names(&["i", "x"]),
                names(&["i", "x"]),
            ]
        );
    }

    #[test]
    fn a_for_loop_assigns_its_variable_each_iteration() {
        let source = "xs = [1]\nfor x in xs:\n    y = x\n    if y:\n        break\n    \
                      if y:\n        cont\n    print(y)\nprint(xs)\n";
        assert_eq!(
            live(source),
            [
                names(&["xs"]),
                names(&["xs"]),
                names(&["xs", "y"]),
                names(&["xs", "y"]),
                names(&["xs"]),
                names(&["xs", "y"]),
                names(&["xs"]),
                names(&["xs"]),
                names(&[]),
            ]
        );
    }

    #[test]
    fn nothing_is_live_after_a_return() {
        let source =
            "fn f(xs: int, n: int) -> int:\n    ys = xs\n    if n:\n        ret ys\n    ret xs\n";
        assert_eq!(
            live(source),
            [
                names(&["n", "xs", "ys"]),
                names(&["xs"]),
                names(&[]),
                names(&[])
            ]
        );
    }

    #[test]
    fn a_call_reads_the_globals_its_function_reads() {
        let source = "fn f() -> int:\n    ret g()\nfn g() -> int:\n    ret xs[0]\nxs = [1]\nys = xs\nprint(f())\n";
        assert_eq!(
            live(source),
            [
                names(&[]),
                names(&[]),
                names(&["xs"]),
                names(&["xs"]),
                names(&[])
            ]
        );
    }

    #[test]
    fn a_statement_is_the_last_use_of_what_it_mentions_and_nothing_reads_after() {
        let source = "x = [1]\ny = x\nprint(y, x)\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let uses = last_uses(body, &GlobalReads::default(), &HashSet::new());
        let mut last: Vec<String> = uses[&body[2].span].clone();
        last.sort();
        assert_eq!(last, names(&["x", "y"]));
        assert!(!uses.contains_key(&body[0].span));
        assert!(!uses.contains_key(&body[1].span));
    }

    #[test]
    fn a_call_of_a_value_reads_the_globals_of_every_function_named_as_one() {
        let source = "fn first() -> int:\n    ret xs[0]\nfn other() -> int:\n    ret ys[0]\n\
                      fn apply(f: fn() -> int) -> int:\n    ret f()\n\
                      fn pick() -> int:\n    ret [first][0]()\n\
                      xs = [1]\nys = [2]\nprint(apply(first), other())\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let globals = HashSet::from(["xs".to_string(), "ys".to_string()]);
        let reads = global_reads(body, &globals);
        assert_eq!(reads["apply"], HashSet::from(["xs".to_string()]));
        assert_eq!(reads["pick"], HashSet::from(["xs".to_string()]));
        assert_eq!(reads.of_values(), &HashSet::from(["xs".to_string()]));
        // `xs` is still read after `ys = xs`, through the call of `f`
        let source = "fn first() -> int:\n    ret xs[0]\nfn apply(f: fn() -> int) -> int:\n    \
                      ret f()\nxs = [1]\nf = first\nys = xs\nprint(f())\n";
        assert_eq!(
            live(source),
            [
                names(&[]),
                names(&[]),
                names(&["xs"]),
                names(&["f", "xs"]),
                names(&["f", "xs"]),
                names(&[])
            ]
        );
    }

    #[test]
    fn a_method_call_reads_the_globals_every_method_of_its_name_reads() {
        let source = "struct S:\n    n: int\nimpl S:\n    fn get(self) -> int:\n        ret xs[0]\n\
                      fn f(s: S) -> int:\n    ret s.get()\nfn g() -> S:\n    ret S.make()\n\
                      xs = [1]\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let reads = global_reads(body, &HashSet::from(["xs".to_string()]));
        assert_eq!(reads["S.get"], HashSet::from(["xs".to_string()]));
        assert_eq!(reads["f"], HashSet::from(["xs".to_string()]));
        assert!(reads["g"].is_empty());
    }

    #[test]
    fn comparing_and_sorting_read_the_globals_eq_and_cmp_read() {
        let source = "struct V:\n    n: int\nimpl ord for V:\n    fn cmp(self, other: Self) -> \
                      ordering:\n        ret ws[0].cmp(other.n)\nimpl eq for V:\n    \
                      fn eq(self, other: Self) -> bool:\n        ret ys[0] == 1\n\
                      fn lt(a: V, b: V) -> bool:\n    ret a < b\nfn same(a: V, b: V) -> bool:\n    \
                      ret a == b\nfn sorted(vs: list[V]) -> list[V]:\n    vs.sort()\n    ret vs\n\
                      ws = [1]\nys = [1]\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let globals = HashSet::from(["ws".to_string(), "ys".to_string()]);
        let reads = global_reads(body, &globals);
        assert_eq!(reads["lt"], HashSet::from(["ws".to_string()]));
        assert_eq!(reads["same"], HashSet::from(["ys".to_string()]));
        assert_eq!(reads["sorted"], HashSet::from(["ws".to_string()]));
    }

    #[test]
    fn writing_a_value_as_text_reads_the_globals_display_reads() {
        let source = "struct V:\n    n: int\nimpl display for V:\n    fn display(self) -> str:\n        \
                      ret str(ws[0])\nfn show(v: V):\n    print(v)\nfn text(v: V) -> str:\n    \
                      ret str(v)\nfn own(v: V) -> str:\n    ret v.display()\nfn other(v: V) -> int:\n    \
                      ret v.n\nws = [1]\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let reads = global_reads(body, &HashSet::from(["ws".to_string()]));
        for name in ["show", "text", "own"] {
            assert_eq!(reads[name], HashSet::from(["ws".to_string()]), "{name}");
        }
        assert!(reads["other"].is_empty());
    }

    #[test]
    fn what_a_function_returns_besides_its_value_is_live_at_every_exit() {
        let source = "if c:\n    ret 1\nx = self.n\nret x\n";
        let module = parse(source).unwrap();
        let crate::ast::Mod::Module { body } = &module;
        let exit = HashSet::from(["self".to_string()]);
        let after = live_after(body, &GlobalReads::default(), &exit);
        let StmtKind::If { body: then, .. } = &body[0].kind else {
            panic!("expected an if");
        };
        assert!(after[&then[0].span].contains("self"));
        assert!(after[&body[1].span].contains("self"));
        assert!(after[&body[2].span].contains("self"));
        let uses = last_uses(body, &GlobalReads::default(), &exit);
        assert!(
            uses.get(&body[1].span)
                .is_none_or(|last| !last.contains(&"self".to_string()))
        );
    }
}
