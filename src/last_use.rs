//! Finds where each variable's value is used for the last time, so that both backends can move
//! a value instead of keeping a copy of it.
//!
//! Lists are values, so `ys = add(xs, 1)` gives `add` a copy of `xs`. When nothing reads `xs`
//! afterward, the copy is never needed: the caller can hand over `xs` itself, and `add` changes
//! it in place. [`live_after`] says which variables are still read after each statement, and
//! [`last_uses`] which variables a statement uses for the last time.

use crate::ast::{Expr, ExprKind, Stmt, StmtKind};
use crate::checker::collect_assigned;
use crate::span::Span;
use crate::stdlib::is_builtin;
use std::collections::{HashMap, HashSet};

/// For each function, the globals it or any function it calls may read.
pub(crate) type GlobalReads = HashMap<String, HashSet<String>>;

/// For each statement, by its span, the variables that some later code may still read.
pub(crate) type LiveAfter = HashMap<Span, HashSet<String>>;

/// Returns, for each function in `body`, every global in `globals` that it or a function it calls,
/// directly or not, may read.
///
/// For example, after `def f(); ret g()` and `def g(); ret xs[0]`, both `f` and `g` read `xs`.
pub(crate) fn global_reads(body: &[Stmt], globals: &HashSet<String>) -> GlobalReads {
    let mut reads = HashMap::new();
    let mut calls = HashMap::new();
    let functions: HashSet<&str> = body
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef { name, .. } => Some(name.as_str()),
            _ => None,
        })
        .collect();
    for stmt in body {
        let StmtKind::FunctionDef {
            name, args, body, ..
        } = &stmt.kind
        else {
            continue;
        };
        let mut locals: HashSet<String> = args.args.iter().map(|arg| arg.arg.clone()).collect();
        let mut assigned = Vec::new();
        collect_assigned(body, &mut assigned);
        locals.extend(assigned.into_iter().map(|(name, _)| name));
        let mut names = Vec::new();
        for stmt in body {
            stmt_names(stmt, &mut names);
        }
        let read: HashSet<String> = names
            .iter()
            .filter(|name| globals.contains(**name) && !locals.contains(**name))
            .map(|name| name.to_string())
            .collect();
        let called: HashSet<String> = names
            .iter()
            .filter(|name| functions.contains(**name))
            .map(|name| name.to_string())
            .collect();
        reads.insert(name.clone(), read);
        calls.insert(name.clone(), called);
    }
    // a function reads whatever the functions it calls read, until nothing more is added
    let mut changed = true;
    while changed {
        changed = false;
        for (name, called) in &calls {
            let inherited: HashSet<String> = called
                .iter()
                .flat_map(|callee| reads[callee].iter().cloned())
                .collect();
            let read = reads.get_mut(name).expect("every function has reads");
            let before = read.len();
            read.extend(inherited);
            changed |= read.len() != before;
        }
    }
    reads
}

/// Collects every name `stmt` mentions, including in nested blocks, assignment targets, and
/// called functions.
pub(crate) fn stmt_names<'a>(stmt: &'a Stmt, out: &mut Vec<&'a str>) {
    let block = |body: &'a [Stmt], out: &mut Vec<&'a str>| {
        for stmt in body {
            stmt_names(stmt, out);
        }
    };
    match &stmt.kind {
        StmtKind::Assign { targets, value } => {
            targets.iter().for_each(|target| expr_names(target, out));
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
        StmtKind::Return { value: None }
        | StmtKind::FunctionDef { .. }
        | StmtKind::Use { .. }
        | StmtKind::Break
        | StmtKind::Continue => {}
    }
}

/// Collects every name `expr` mentions, including the names of called functions.
fn expr_names<'a>(expr: &'a Expr, out: &mut Vec<&'a str>) {
    match &expr.kind {
        ExprKind::Name { id, .. } => out.push(id),
        ExprKind::Constant { .. } => {}
        ExprKind::BinOp { left, right, .. } => {
            expr_names(left, out);
            expr_names(right, out);
        }
        ExprKind::UnaryOp { operand, .. } => expr_names(operand, out),
        ExprKind::BoolOp { values, .. } | ExprKind::List { elts: values, .. } => {
            values.iter().for_each(|value| expr_names(value, out))
        }
        ExprKind::Compare {
            left, comparators, ..
        } => {
            expr_names(left, out);
            comparators.iter().for_each(|value| expr_names(value, out));
        }
        ExprKind::Call { func, args } => {
            expr_names(func, out);
            args.iter().for_each(|arg| expr_names(arg, out));
        }
        ExprKind::Subscript { value, slice, .. } => {
            expr_names(value, out);
            expr_names(slice, out);
        }
        ExprKind::Attribute { value, .. } => expr_names(value, out),
    }
}

/// Returns, for every statement in `body` and the blocks nested in it, the variables that code
/// running after it may read before assigning them again. A call reads every global its function
/// may read, from `reads`.
///
/// Nothing is read after `ret`, and after `break` and `cont` whatever is read where they jump
/// to. For example, in `x = [1]` followed by `y = x` and `print(y)`, `x` is live after the first
/// statement only, and `y` after the second.
pub(crate) fn live_after(body: &[Stmt], reads: &GlobalReads) -> LiveAfter {
    let mut liveness = Liveness {
        reads,
        after: LiveAfter::new(),
        loops: Vec::new(),
    };
    liveness.block(body, HashSet::new());
    liveness.after
}

/// Returns, for each statement in `body` and the blocks nested in it that uses a variable for the
/// last time, those variables: the ones it mentions that nothing reads after it.
///
/// For example, in `x = [1]` followed by `y = x` and `print(y)`, the second statement is the last
/// use of `x`, and the third of `y`.
pub(crate) fn last_uses(body: &[Stmt], reads: &GlobalReads) -> HashMap<Span, Vec<String>> {
    let after = live_after(body, reads);
    let mut uses = HashMap::new();
    let mut statements: Vec<&Stmt> = body.iter().collect();
    while let Some(stmt) = statements.pop() {
        match &stmt.kind {
            StmtKind::If { body, orelse, .. } => statements.extend(body.iter().chain(orelse)),
            StmtKind::While { body, .. } | StmtKind::For { body, .. } => statements.extend(body),
            _ => {}
        }
        let Some(live) = after.get(&stmt.span) else {
            continue;
        };
        let mut names = Vec::new();
        stmt_names(stmt, &mut names);
        let mut last: Vec<String> = Vec::new();
        for name in names {
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
            StmtKind::Return { .. } => HashSet::new(),
            StmtKind::Break => self.loops.last().map(|l| l.0.clone()).unwrap_or_default(),
            StmtKind::Continue => self.loops.last().map(|l| l.1.clone()).unwrap_or_default(),
            _ => next,
        };
        self.after.insert(stmt.span, out.clone());
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                let mut live = out;
                for target in targets {
                    if let ExprKind::Name { id, .. } = &target.kind {
                        live.remove(id);
                    }
                }
                // a changed list is read, along with its indexes
                for target in targets {
                    if !matches!(target.kind, ExprKind::Name { .. }) {
                        self.expr(target, &mut live);
                    }
                }
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
                    if let ExprKind::Name { id, .. } = &target.kind {
                        next.remove(id);
                    }
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
            StmtKind::Return { value: None }
            | StmtKind::Break
            | StmtKind::Continue
            | StmtKind::FunctionDef { .. }
            | StmtKind::Use { .. } => out,
        }
    }

    /// Adds every variable `expr` reads to `live`, including the globals each function it calls
    /// may read.
    fn expr(&self, expr: &Expr, live: &mut HashSet<String>) {
        let mut names = Vec::new();
        expr_names(expr, &mut names);
        for name in names {
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
    /// after it, sorted, with the statements nested in the first `def`, if there is one, first.
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
        let mut after = live_after(body, &reads);
        for stmt in body {
            if let StmtKind::FunctionDef { body, .. } = &stmt.kind {
                after.extend(live_after(body, &reads));
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
    fn a_read_on_either_branch_keeps_a_variable_live() {
        let source = "x = [1]\nc = 1\nif c;\n    print(x)\nelse;\n    x = [2]\nprint(c)\n";
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
        let source = "x = [1]\ni = 0\nwhile i < 3;\n    y = x\n    x = [i]\n    i = i + 1\n";
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
        let source = "xs = [1]\nfor x in xs;\n    y = x\n    if y;\n        break\n    \
                      if y;\n        cont\n    print(y)\nprint(xs)\n";
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
        let source = "def f(xs, n);\n    ys = xs\n    if n;\n        ret ys\n    ret xs\n";
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
        let source =
            "def f();\n    ret g()\ndef g();\n    ret xs[0]\nxs = [1]\nys = xs\nprint(f())\n";
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
        let uses = last_uses(body, &GlobalReads::new());
        let mut last: Vec<String> = uses[&body[2].span].clone();
        last.sort();
        assert_eq!(last, names(&["x", "y"]));
        assert!(!uses.contains_key(&body[0].span));
        assert!(!uses.contains_key(&body[1].span));
    }
}
