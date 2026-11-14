//! The lints that are not part of checking types: one walk over a checked module that finds code
//! which is valid but probably not what was meant, reading what the checker recorded in its
//! [`Analysis`].
//!
//! For example, in
//!
//! ```text
//! fn sign(x: int) -> int:
//!     if x > 0:
//!         ret 1
//!     elif x > 0:
//!         ret -1
//!     ret 0
//!     print(x)
//! ```
//!
//! the second `x > 0` is a [`Lint::RepeatedCondition`] and `print(x)` a
//! [`Lint::UnreachableCode`].

use crate::ast::{CompOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind};
use crate::checker::{Analysis, RESULT, Type};
use crate::constant::ConstValue;
use crate::diagnostic::Diagnostic;
use crate::explain::Lint;
use crate::project::SourceMap;
use crate::span::{Pos, Span};

/// Returns a warning for each piece of `module` a lint of this walk finds, given the checker's
/// `analysis` of it, which must have found no errors, and the files it came from, for the code
/// a suggestion writes.
///
/// For example, a program `x = 1\nx == 2\n` gets `this comparison's value is never used` at the
/// `x == 2`, suggesting `x = 2`.
pub fn find(module: &Mod, analysis: &Analysis, sources: &SourceMap) -> Vec<Diagnostic> {
    let Mod::Module { body } = module;
    let mut finder = Finder {
        analysis,
        sources,
        found: vec![],
    };
    finder.block(body);
    finder.found
}

struct Finder<'a> {
    analysis: &'a Analysis,
    sources: &'a SourceMap,
    found: Vec<Diagnostic>,
}

impl Finder<'_> {
    /// Returns the source text `span` covers, if it is in a file of the program.
    fn text(&self, span: Span) -> Option<&str> {
        let source = &self.sources.file(span.file).source;
        let start = offset(source, span.start)?;
        let end = offset(source, span.end)?;
        source.get(start..end)
    }

    /// Returns `span` cut at the end of its first line, so a statement of several lines is
    /// marked only where it starts.
    fn first_line(&self, span: Span) -> Span {
        if span.end.line == span.start.line {
            return span;
        }
        let source = &self.sources.file(span.file).source;
        let line = source.lines().nth(span.start.line - 1).unwrap_or_default();
        let end = Pos::new(span.start.line, line.trim_end().chars().count() + 1);
        Span { end, ..span }
    }

    fn type_of(&self, expr: &Expr) -> Option<&Type> {
        self.analysis.types.get(&expr.span)
    }

    fn warn(&mut self, diagnostic: Diagnostic) {
        self.found.push(diagnostic);
    }

    /// Walks a block's statements, reporting the first one that never runs because a statement
    /// before it always leaves the block.
    fn block(&mut self, body: &[Stmt]) {
        let mut exit: Option<&Stmt> = None;
        for stmt in body {
            // definitions take effect wherever they are written
            if let Some(exit) = exit
                && !is_definition(stmt)
            {
                let warning = Lint::UnreachableCode
                    .warning(self.first_line(stmt.span), "this code never runs")
                    .with_label(self.first_line(exit.span), exit_label(exit))
                    .with_help("remove it, or move it before the code that leaves");
                self.warn(warning);
                for rest in body.iter().filter(|stmt| is_definition(stmt)) {
                    self.stmt(rest);
                }
                return;
            }
            self.stmt(stmt);
            if exits(stmt) {
                exit = Some(stmt);
            }
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FunctionDef { body, .. } | StmtKind::Test { body, .. } => self.block(body),
            StmtKind::ImplDef { methods, .. } | StmtKind::TraitDef { methods, .. } => {
                methods.iter().for_each(|method| self.stmt(method))
            }
            StmtKind::Return { value } => value.iter().for_each(|value| self.expr(value)),
            StmtKind::Assign { targets, value } => {
                if let [target] = targets.as_slice() {
                    self.self_assignment(target, value);
                }
                targets.iter().for_each(|target| self.expr(target));
                self.expr(value);
            }
            StmtKind::AugAssign { target, op, value } => {
                self.expr(target);
                self.expr(value);
                if let Some(ty) = self.analysis.types.get(&stmt.span) {
                    self.always_fails(*op, ty.clone(), value, stmt.span);
                }
            }
            StmtKind::For { target, iter, body } => {
                self.expr(target);
                self.expr(iter);
                self.block(body);
            }
            StmtKind::While { test, body } => {
                self.expr(test);
                self.block(body);
            }
            StmtKind::If { .. } => self.if_chain(stmt),
            StmtKind::Expr { value } => {
                self.unused_value(value);
                self.expr(value);
            }
            StmtKind::Match { subject, cases } => {
                self.expr(subject);
                for case in cases {
                    self.block(&case.body);
                }
            }
            StmtKind::Assert { test, msg } => {
                self.expr(test);
                msg.iter().for_each(|msg| self.expr(msg));
            }
            StmtKind::ConstDef { value, .. } => self.expr(value),
            StmtKind::Use { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    /// Walks an `if` with its `elif`s, reporting a condition that one before it in the chain
    /// already tested, whose branch never runs.
    fn if_chain(&mut self, stmt: &Stmt) {
        let mut tests: Vec<&Expr> = vec![];
        let mut current = stmt;
        while let StmtKind::If { test, body, orelse } = &current.kind {
            // a condition of literals alone, as `elif false:`, is written so on purpose
            let repeated = tests.iter().find(|earlier| same(earlier, test));
            if let Some(first) = repeated.filter(|_| reads_variable(test)) {
                let warning = Lint::RepeatedCondition
                    .warning(
                        test.span,
                        "this condition is tested above, so this branch never runs",
                    )
                    .with_label(first.span, "tested here first");
                self.warn(warning);
            }
            tests.push(test);
            self.expr(test);
            self.block(body);
            match orelse.as_slice() {
                [
                    next @ Stmt {
                        kind: StmtKind::If { .. },
                        ..
                    },
                ] => current = next,
                _ => {
                    self.block(orelse);
                    return;
                }
            }
        }
    }

    /// Reports an expression statement whose value is lost: a result, whose error goes
    /// unnoticed, or any value that is not a call's, which does nothing.
    fn unused_value(&mut self, value: &Expr) {
        match &value.kind {
            ExprKind::Call { .. } | ExprKind::Try { .. } => {
                if let Some(Type::Enum { name, .. }) = self.type_of(value)
                    && name == RESULT
                {
                    let warning = Lint::IgnoredResult
                        .warning(
                            value.span,
                            "this result is never used, so an error it holds goes unnoticed",
                        )
                        .with_help(
                            "add '?' to return the error, use '.unwrap()' to stop the program on \
                             one, or, to ignore it on purpose, assign it to a name that starts \
                             with '_'",
                        );
                    self.warn(warning);
                }
            }
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } if matches!(ops.as_slice(), [CompOp::Equal]) => {
                let mut warning = Lint::UselessExpression
                    .warning(value.span, "this comparison's value is never used");
                let right = &comparators[0];
                let assignable = matches!(
                    left.kind,
                    ExprKind::Name { .. } | ExprKind::Attribute { .. } | ExprKind::Subscript { .. }
                );
                match self.joined(value, left, "==", right) {
                    Some((left, right)) if assignable => {
                        let replacement = format!("{left} = {right}");
                        warning =
                            warning.with_suggestion("to assign, write '='", value.span, replacement)
                    }
                    _ => warning = warning.with_help("to assign, write '=' rather than '=='"),
                }
                self.warn(warning);
            }
            _ => {
                let warning = Lint::UselessExpression
                    .warning(value.span, "this expression's value is never used")
                    .with_help("remove it, or use its value");
                self.warn(warning);
            }
        }
    }

    /// Reports an assignment of a place to itself, as `x = x`, except `self = self`, which a
    /// method writes to say that it changes `self`.
    fn self_assignment(&mut self, target: &Expr, value: &Expr) {
        if matches!(&target.kind, ExprKind::Name { id, .. } if id == "self") || !same(target, value)
        {
            return;
        }
        let shown = self.text(target.span).unwrap_or("it").to_string();
        let warning = Lint::SelfAssignment
            .warning(
                target.span.to(value.span),
                format!("'{shown}' is assigned to itself, which changes nothing"),
            )
            .with_help("remove the assignment");
        self.warn(warning);
    }

    /// Walks an expression and the expressions in it, reporting what the lints of single
    /// expressions find.
    fn expr(&mut self, expr: &Expr) {
        expr.visit(&mut |inner| match &inner.kind {
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => {
                if let ([op], [right]) = (ops.as_slice(), comparators.as_slice()) {
                    self.bool_comparison(inner, left, op, right);
                    self.self_comparison(inner, left, op, right);
                }
            }
            // literals alone, as `1 / 0`, are written so on purpose
            ExprKind::BinOp { op, left, right } if reads_variable(left) => {
                if let Some(ty) = self.type_of(inner) {
                    self.always_fails(*op, ty.clone(), right, inner.span);
                }
            }
            ExprKind::Dict { keys, .. } => self.duplicate_keys(keys, "key", "map"),
            ExprKind::Set { elts } => self.duplicate_keys(elts, "element", "set"),
            ExprKind::Call { args, .. } => {
                if let [arg] = args.as_slice() {
                    self.identity_conversion(inner, arg);
                }
            }
            _ => {}
        });
    }

    /// Returns the texts of `left` and `right` if `whole`, a comparison or assignment of them,
    /// is written as exactly the two with `op` and spaces between, so a suggestion that rewrites
    /// it loses no parentheses.
    fn joined(&self, whole: &Expr, left: &Expr, op: &str, right: &Expr) -> Option<(&str, &str)> {
        let text = self.text(whole.span)?;
        let left_text = self.text(left.span)?;
        let right_text = self.text(right.span)?;
        let middle = text.strip_prefix(left_text)?.strip_suffix(right_text)?;
        (middle.trim() == op).then_some((left_text, right_text))
    }

    /// Reports `x == true` and the like, which say no more than `x` or `not x`.
    fn bool_comparison(&mut self, whole: &Expr, left: &Expr, op: &CompOp, right: &Expr) {
        let equal = match op {
            CompOp::Equal => true,
            CompOp::NotEqual => false,
            _ => return,
        };
        let (literal, other) = match (bool_literal(left), bool_literal(right)) {
            (Some(literal), None) => (literal, right),
            (None, Some(literal)) => (literal, left),
            _ => return,
        };
        let negated = literal != equal;
        let op_text = if equal { "==" } else { "!=" };
        let message = format!("comparing a bool with '{literal}' is the bool itself");
        let mut warning = Lint::BoolComparison.warning(whole.span, message);
        match self.joined(whole, left, op_text, right) {
            Some(_) => {
                let text = self.text(other.span).unwrap_or_default();
                let replacement = match (negated, &other.kind) {
                    (false, _) => text.to_string(),
                    (true, ExprKind::BoolOp { .. }) => format!("not ({text})"),
                    (true, _) => format!("not {text}"),
                };
                warning = warning.with_suggestion("write the bool", whole.span, replacement);
            }
            None => {
                let help = match negated {
                    true => "write 'not' and the bool instead",
                    false => "write the bool alone instead",
                };
                warning = warning.with_help(help);
            }
        }
        self.warn(warning);
    }

    /// Reports a comparison of a value with itself, as `x == x`, whose answer is always the
    /// same. Floats are left out, since `x != x` is how to find a nan.
    fn self_comparison(&mut self, whole: &Expr, left: &Expr, op: &CompOp, right: &Expr) {
        let always = match op {
            CompOp::Equal | CompOp::LessThanEqual | CompOp::GreaterThanEqual => true,
            CompOp::NotEqual | CompOp::LessThan | CompOp::GreaterThan => false,
            CompOp::In | CompOp::NotIn => return,
        };
        let compared = matches!(
            self.type_of(left),
            Some(Type::Int | Type::Bool | Type::Str | Type::Bytes)
        );
        // literals compared with themselves, as `1 == 1`, are written so on purpose
        if !compared || !same(left, right) || !reads_variable(left) {
            return;
        }
        let shown = self.text(left.span).unwrap_or("a value").to_string();
        let message = format!("'{shown}' is compared with itself, so this is always {always}");
        self.warn(Lint::SelfComparison.warning(whole.span, message));
    }

    /// Reports a division, remainder, or shift by a constant that makes it fail whenever it
    /// runs, such as `x / 0` or `x << 64`, given the operation's type `ty`. The caller leaves out
    /// an operation of literals alone, such as `1 / 0`.
    fn always_fails(&mut self, op: Operator, ty: Type, right: &Expr, span: Span) {
        if !matches!(ty, Type::Int | Type::Float) {
            return;
        }
        let failure = match (op, self.constant(right)) {
            (
                Operator::Divide | Operator::Modulo,
                Some(ConstValue::Int(0) | ConstValue::Float(0.0)),
            ) => "division by zero".to_string(),
            (Operator::LShift | Operator::RShift, Some(ConstValue::Int(n)))
                if !(0..=63).contains(&n) =>
            {
                crate::stdlib::fill(crate::stdlib::SHIFT_RANGE, &[n])
            }
            _ => return,
        };
        let message = format!("this fails whenever it runs, with '{failure}'");
        let warning = Lint::AlwaysFails
            .warning(span, message)
            .with_label(right.span, "since this is always the same");
        self.warn(warning);
    }

    /// Returns the value of `expr` if it is a constant: literals, constants, and operations on
    /// them, as `const` values are worked out.
    fn constant(&self, expr: &Expr) -> Option<ConstValue> {
        let constants = &self.analysis.constants;
        let mut lookup = |name: &str, _: Span| constants.get(name).cloned().map(Ok);
        crate::constant::evaluate(expr, &mut lookup).ok()
    }

    /// Reports a key of a map literal, or an element of a set literal, that is the same
    /// constant as one before it.
    fn duplicate_keys(&mut self, keys: &[Expr], what: &str, container: &str) {
        let mut seen: Vec<(ConstValue, Span)> = vec![];
        for key in keys {
            let Some(value) = self.constant(key) else {
                continue;
            };
            match seen.iter().find(|(earlier, _)| *earlier == value) {
                Some((_, first)) => {
                    let message = match container {
                        "map" => format!(
                            "the {what} {value} is given twice in this {container}, so the value \
                             given first is lost"
                        ),
                        _ => format!("the {what} {value} is given twice in this {container}"),
                    };
                    let warning = Lint::DuplicateKey
                        .warning(key.span, message)
                        .with_label(*first, "given first here");
                    self.warn(warning);
                }
                None => seen.push((value, key.span)),
            }
        }
    }

    /// Reports a conversion of a value to the type it has, such as `int(n)` with `n` an int,
    /// suggesting the value alone.
    fn identity_conversion(&mut self, call: &Expr, arg: &Expr) {
        let Some(conversion) = self.analysis.conversions.get(&call.span) else {
            return;
        };
        // a value given to a trait records the trait as its type, and a literal, as in
        // `int(5)`, is converted so on purpose
        let identity = conversion.target == conversion.source
            && !conversion.fallible
            && conversion.function.is_none()
            && conversion.implementations.is_none()
            && !conversion.source.has_params()
            && !matches!(conversion.target, Type::Trait(_))
            && reads_variable(arg);
        if !identity {
            return;
        }
        let ty = crate::checker::type_text(&conversion.source);
        let message = format!("this converts a value to {ty}, the type it already has");
        let mut warning = Lint::IdentityConversion.warning(call.span, message);
        if let Some(text) = self.text(arg.span) {
            let replacement = match is_atom(arg) {
                true => text.to_string(),
                false => format!("({text})"),
            };
            warning = warning.with_suggestion("use the value itself", call.span, replacement);
        }
        self.warn(warning);
    }
}

/// Returns the byte offset of `pos` in `source`, if it is there.
///
/// For example, in `"ab\ncd"`, line 2, column 2 is at offset 4.
fn offset(source: &str, pos: Pos) -> Option<usize> {
    let mut line_start = 0;
    for _ in 1..pos.line {
        line_start += source.get(line_start..)?.find('\n')? + 1;
    }
    let line = source.get(line_start..)?;
    let col: usize = line
        .char_indices()
        .nth(pos.col - 1)
        .map_or(line.len(), |(i, _)| i);
    Some(line_start + col)
}

/// Returns whether `stmt` defines something rather than runs, so that its place in a block does
/// not matter.
fn is_definition(stmt: &Stmt) -> bool {
    matches!(
        stmt.kind,
        StmtKind::FunctionDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
            | StmtKind::ConstDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Test { .. }
    )
}

/// Returns whether running `stmt` never goes on to the statement after it: a `ret`, `break`, or
/// `cont`, an `if` or `match` each of whose branches does one, or a `while true:` loop with no
/// `break`.
///
/// For example, `if x: ret 1` with `else: break` exits, but `if x: ret 1` alone does not.
fn exits(stmt: &Stmt) -> bool {
    let block_exits = |body: &[Stmt]| body.iter().any(exits);
    match &stmt.kind {
        StmtKind::Return { .. } | StmtKind::Break | StmtKind::Continue => true,
        StmtKind::If { body, orelse, .. } => block_exits(body) && block_exits(orelse),
        // the checker made sure the cases cover every value
        StmtKind::Match { cases, .. } => {
            !cases.is_empty() && cases.iter().all(|case| block_exits(&case.body))
        }
        StmtKind::While { test, body } => {
            bool_literal(test) == Some(true) && !body.iter().any(breaks)
        }
        _ => false,
    }
}

/// Returns whether `stmt` may `break` out of the loop it is in, not counting a `break` of a loop
/// nested inside it.
fn breaks(stmt: &Stmt) -> bool {
    match &stmt.kind {
        StmtKind::Break => true,
        StmtKind::If { body, orelse, .. } => body.iter().chain(orelse).any(breaks),
        StmtKind::Match { cases, .. } => cases.iter().flat_map(|case| &case.body).any(breaks),
        _ => false,
    }
}

/// Returns what a label at a statement that always leaves says about it.
fn exit_label(stmt: &Stmt) -> &'static str {
    match stmt.kind {
        StmtKind::Return { .. } => "this 'ret' returns before it",
        StmtKind::Break => "this 'break' leaves the loop before it",
        StmtKind::Continue => "this 'cont' goes on to the next turn before it",
        StmtKind::While { .. } => "this loop has no 'break', so it never ends but by 'ret'",
        _ => "every branch of this leaves before it",
    }
}

/// Returns the value of `expr` if it is `true` or `false`.
fn bool_literal(expr: &Expr) -> Option<bool> {
    match &expr.kind {
        ExprKind::Constant { value, .. } => match **value {
            Constant::Bool(b) => Some(b),
            _ => None,
        },
        _ => None,
    }
}

/// Returns whether `expr` reads a variable, a parameter, or a constant, rather than being made of
/// literals alone.
///
/// For example, `x + 1` reads one, and `1 + 1` and `"a"` do not.
fn reads_variable(expr: &Expr) -> bool {
    let mut found = false;
    expr.visit(&mut |inner| found |= matches!(inner.kind, ExprKind::Name { .. }));
    found
}

/// Returns whether `expr` can take the place of a call without parentheses around it, as a name
/// or a literal can.
fn is_atom(expr: &Expr) -> bool {
    matches!(
        expr.kind,
        ExprKind::Name { .. }
            | ExprKind::Constant { .. }
            | ExprKind::Attribute { .. }
            | ExprKind::Subscript { .. }
            | ExprKind::Call { .. }
            | ExprKind::List { .. }
            | ExprKind::Dict { .. }
            | ExprKind::Set { .. }
            | ExprKind::Tuple { .. }
            | ExprKind::JoinedStr { .. }
    )
}

/// Returns whether `a` and `b` are written alike and compute the same value without calling
/// anything: names, literals, fields, indexes, and operators on them.
///
/// For example, `xs[i].x + 1` and `xs[i].x + 1` are the same, but `f(x)` and `f(x)` are not,
/// since a call may give a different value each time.
fn same(a: &Expr, b: &Expr) -> bool {
    let all = |xs: &[Expr], ys: &[Expr]| {
        xs.len() == ys.len() && xs.iter().zip(ys).all(|(x, y)| same(x, y))
    };
    let maybe = |x: &Option<Box<Expr>>, y: &Option<Box<Expr>>| match (x, y) {
        (None, None) => true,
        (Some(x), Some(y)) => same(x, y),
        _ => false,
    };
    match (&a.kind, &b.kind) {
        (ExprKind::Name { id: x, .. }, ExprKind::Name { id: y, .. }) => x == y,
        (ExprKind::Constant { value: x, .. }, ExprKind::Constant { value: y, .. }) => x == y,
        (
            ExprKind::Attribute {
                value: x, attr: p, ..
            },
            ExprKind::Attribute {
                value: y, attr: q, ..
            },
        ) => p == q && same(x, y),
        (
            ExprKind::Subscript {
                value: x, slice: i, ..
            },
            ExprKind::Subscript {
                value: y, slice: j, ..
            },
        ) => same(x, y) && same(i, j),
        (
            ExprKind::Slice {
                lower: l1,
                upper: u1,
                step: s1,
            },
            ExprKind::Slice {
                lower: l2,
                upper: u2,
                step: s2,
            },
        ) => maybe(l1, l2) && maybe(u1, u2) && maybe(s1, s2),
        (
            ExprKind::UnaryOp {
                op: o, operand: x, ..
            },
            ExprKind::UnaryOp {
                op: p, operand: y, ..
            },
        ) => o == p && same(x, y),
        (
            ExprKind::BinOp {
                op: o,
                left: l1,
                right: r1,
            },
            ExprKind::BinOp {
                op: p,
                left: l2,
                right: r2,
            },
        ) => o == p && same(l1, l2) && same(r1, r2),
        (
            ExprKind::Compare {
                left: l1,
                ops: o1,
                comparators: c1,
            },
            ExprKind::Compare {
                left: l2,
                ops: o2,
                comparators: c2,
            },
        ) => o1 == o2 && same(l1, l2) && all(c1, c2),
        (ExprKind::BoolOp { op: o, values: x }, ExprKind::BoolOp { op: p, values: y }) => {
            o == p && all(x, y)
        }
        (ExprKind::Tuple { elts: x, .. }, ExprKind::Tuple { elts: y, .. }) => all(x, y),
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::project::MapSources;
    use std::path::Path;

    /// Returns each warning `source` gets from a lint of this walk, as its lint's id, message,
    /// and position.
    fn found(source: &str) -> Vec<(String, String, usize, usize)> {
        let (_, checked) =
            crate::driver::load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.unwrap_or_else(|e| panic!("{source} should check: {e:?}"));
        checked
            .warnings
            .iter()
            .map(|d| {
                let id = d.code().unwrap_or_default().to_string();
                (id, d.message.clone(), d.span.start.line, d.span.start.col)
            })
            .collect()
    }

    /// Returns the ids of the warnings `source` gets.
    fn ids(source: &str) -> Vec<String> {
        found(source).into_iter().map(|(id, ..)| id).collect()
    }

    /// Returns the code each suggestion of the warnings of `source` writes.
    fn suggestions(source: &str) -> Vec<String> {
        let (_, checked) =
            crate::driver::load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.unwrap_or_else(|e| panic!("{source} should check: {e:?}"));
        checked
            .warnings
            .iter()
            .flat_map(|d| d.help().to_vec())
            .filter_map(|help| help.suggestion.map(|s| s.replacement))
            .collect()
    }

    #[test]
    fn stones_own_modules_get_no_warnings() {
        let names: Vec<&str> = crate::stdlib::SOURCE_MODULES
            .iter()
            .map(|(name, _)| *name)
            .collect();
        let uses: String = names.iter().map(|name| format!("use {name}\n")).collect();
        let linked = crate::project::link(Path::new("main.stn"), &uses, &MapSources::default());
        assert_eq!(linked.diagnostics, [], "{uses}");
        // before levels drop warnings in stone's own files
        let analysis = crate::driver::analyze_linked(&linked);
        let warnings: Vec<String> = analysis
            .diagnostics
            .iter()
            .filter(|d| d.span.file != crate::span::FileId(0))
            .map(|d| linked.sources.render(d))
            .collect();
        assert!(warnings.is_empty(), "{}", warnings.join(""));
    }

    #[test]
    fn literals_alone_are_written_so_on_purpose() {
        let source = "print(1 == 1, \"a\" < \"a\", int(5), str(\"x\"))\nif false:\n    print(1)\n\
                      elif false:\n    print(2)\n";
        assert_eq!(ids(source), Vec::<String>::new());
    }

    #[test]
    fn a_value_given_to_a_trait_is_no_conversion_to_itself() {
        let source = "trait Shape:\n    fn area(self) -> float\n\nstruct Sq:\n    s: float\n\n\
                      impl Shape for Sq:\n    fn area(self) -> float:\n        ret self.s\n\n\
                      q = Sq(s=1.0)\nprint(Shape(q).area())\n";
        assert_eq!(ids(source), Vec::<String>::new());
    }

    #[test]
    fn code_after_a_statement_that_leaves_never_runs() {
        let source = "fn f(x: int) -> int:\n    ret x\n    print(x)\n    print(2)\n\nprint(f(1))\n";
        assert_eq!(
            found(source),
            [(
                "W0006".to_string(),
                "this code never runs".to_string(),
                3,
                5
            )]
        );
        let after_break = "for i in range(3):\n    break\n    print(i)\n";
        assert_eq!(ids(after_break), ["W0006"]);
        let after_cont = "for i in range(3):\n    cont\n    print(i)\n";
        assert_eq!(ids(after_cont), ["W0006"]);
        let both = "fn f(x: int) -> int:\n    if x > 0:\n        ret 1\n    else:\n        ret 2\n    \
                    ret 3\n\nprint(f(1))\n";
        assert_eq!(found(both)[0].2, 6);
        let cases = "fn f(x: int) -> int:\n    match x:\n        case 0:\n            ret 1\n        \
                     case _:\n            ret 2\n    ret 3\n\nprint(f(1))\n";
        assert_eq!(ids(cases), ["W0006"]);
        let forever =
            "fn f() -> int:\n    while true:\n        print(1)\n    ret 1\n\nprint(f())\n";
        assert_eq!(ids(forever), ["W0006"]);
    }

    #[test]
    fn code_that_may_run_is_not_reported() {
        let one_branch =
            "fn f(x: int) -> int:\n    if x > 0:\n        ret 1\n    ret 2\n\nprint(f(1))\n";
        assert_eq!(ids(one_branch), Vec::<String>::new());
        let broken = "fn f() -> int:\n    while true:\n        if 1 > 0:\n            break\n    \
                      ret 1\n\nprint(f())\n";
        assert_eq!(ids(broken), Vec::<String>::new());
        let inner = "fn f() -> int:\n    while true:\n        for i in range(2):\n            \
                     break\n    ret 1\n\nprint(f())\n";
        assert_eq!(ids(inner), ["W0006"]);
        // a definition after a loop that never ends still defines
        let defined = "while true:\n    print(helper())\n\nfn helper() -> int:\n    ret 1\n";
        assert_eq!(ids(defined), Vec::<String>::new());
    }

    #[test]
    fn a_result_left_unused_is_reported() {
        let source = "use fs\n\nfs.write(\"a.txt\", \"x\")\n";
        assert_eq!(
            found(source),
            [(
                "W0007".to_string(),
                "this result is never used, so an error it holds goes unnoticed".to_string(),
                3,
                1
            )]
        );
        let used = "use fs\n\nfn f() -> result[int, str]:\n    fs.write(\"a.txt\", \"x\")?\n    \
                    ret ok(1)\n\nfs.write(\"a.txt\", \"x\").unwrap()\n_written = fs.write(\"a.txt\", \"y\")\n\
                    print(f())\n";
        assert_eq!(ids(used), Vec::<String>::new());
        let option = "xs = [1]\nxs.pop()\nprint(xs)\n";
        assert_eq!(ids(option), Vec::<String>::new());
    }

    #[test]
    fn a_value_left_unused_is_reported() {
        let source = "x = 1\nx == 2\nx + 1\n\"note\"\nprint(x)\n";
        assert_eq!(ids(source), ["W0008", "W0008", "W0008"]);
        assert_eq!(found(source)[0].1, "this comparison's value is never used");
        assert_eq!(suggestions(source), ["x = 2"]);
        let calls = "xs = [1]\nxs.append(2)\nprint(xs)\n";
        assert_eq!(ids(calls), Vec::<String>::new());
    }

    #[test]
    fn a_conversion_to_the_same_type_is_reported() {
        let source = "fn f(n: int, s: str, a: int, b: int):\n    print(int(n), str(s), int(a + b))\n    \
                      print(float(n))\n\nf(1, \"a\", 2, 3)\n";
        assert_eq!(ids(source), ["W0009", "W0009", "W0009"]);
        assert_eq!(suggestions(source), ["n", "s", "(a + b)"]);
    }

    #[test]
    fn a_comparison_with_a_bool_literal_is_reported() {
        let source = "fn f(done: bool, a: bool, b: bool):\n    if done == true:\n        print(1)\n    \
                      if done == false:\n        print(2)\n    if true != done:\n        print(3)\n    \
                      if (a and b) == false:\n        print(4)\n\nf(true, true, false)\n";
        assert_eq!(ids(source), ["W0010", "W0010", "W0010", "W0010"]);
        let rewritten = suggestions(source);
        assert_eq!(rewritten[..3], ["done", "not done", "not done"]);
    }

    #[test]
    fn a_value_compared_with_itself_is_reported() {
        let source = "fn f(x: int, s: str, y: float):\n    print(x == x, s < s, y == y, y != y)\n\n\
                      f(1, \"a\", 1.0)\n";
        assert_eq!(
            found(source),
            [
                (
                    "W0011".to_string(),
                    "'x' is compared with itself, so this is always true".to_string(),
                    2,
                    11
                ),
                (
                    "W0011".to_string(),
                    "'s' is compared with itself, so this is always false".to_string(),
                    2,
                    19
                ),
            ]
        );
        let calls = "fn g() -> int:\n    ret 1\n\nprint(g() == g())\n";
        assert_eq!(ids(calls), Vec::<String>::new());
    }

    #[test]
    fn a_place_assigned_to_itself_is_reported_but_not_self() {
        let source = "struct P:\n    x: int\n\nimpl P:\n    fn touch(self):\n        self = self\n\n\
                      x = 1\nx = x\np = P(x=x)\np.x = p.x\np.touch()\nprint(p)\n";
        assert_eq!(
            found(source)
                .into_iter()
                .map(|(id, message, line, _)| (id, message, line))
                .collect::<Vec<_>>(),
            [
                (
                    "W0012".to_string(),
                    "'x' is assigned to itself, which changes nothing".to_string(),
                    9
                ),
                (
                    "W0012".to_string(),
                    "'p.x' is assigned to itself, which changes nothing".to_string(),
                    11
                ),
            ]
        );
    }

    #[test]
    fn a_key_given_twice_is_reported() {
        let source =
            "const k = \"a\"\nm = {\"a\": 1, \"b\": 2, k: 3}\ns = {1, 2, 1}\nprint(m, s)\n";
        assert_eq!(
            found(source),
            [
                (
                    "W0013".to_string(),
                    "the key \"a\" is given twice in this map, so the value given first is lost"
                        .to_string(),
                    2,
                    22
                ),
                (
                    "W0013".to_string(),
                    "the element 1 is given twice in this set".to_string(),
                    3,
                    12
                ),
            ]
        );
        let different = "x = 1\nm = {x: 1, 2: 2}\nprint(m)\n";
        assert_eq!(ids(different), Vec::<String>::new());
    }

    #[test]
    fn a_condition_tested_twice_in_a_chain_is_reported() {
        let source = "fn f(x: int):\n    if x > 0:\n        print(1)\n    elif x < 0:\n        print(2)\n    \
                      elif x > 0:\n        print(3)\n\nf(1)\n";
        assert_eq!(
            found(source),
            [(
                "W0014".to_string(),
                "this condition is tested above, so this branch never runs".to_string(),
                6,
                10
            )]
        );
        let different = "fn f(x: int):\n    if x > 0:\n        print(1)\n    elif x > 1:\n        print(2)\n\nf(1)\n";
        assert_eq!(ids(different), Vec::<String>::new());
    }

    #[test]
    fn an_operation_that_always_fails_is_reported() {
        let source = "const zero = 0\nfn f(x: int, y: float) -> int:\n    print(x / 0, y % 0.0, x << 64)\n    \
                      x >>= -1\n    ret x / zero\n\nprint(f(1, 2.0))\n";
        assert_eq!(ids(source), ["W0015", "W0015", "W0015", "W0015", "W0015"]);
        assert_eq!(
            found(source)[2].1,
            "this fails whenever it runs, with 'shift count out of range (count 64)'"
        );
        let fine =
            "fn f(x: int, y: int) -> int:\n    ret x / 2 + x % y + (x << 3)\n\nprint(f(1, 2))\n";
        assert_eq!(ids(fine), Vec::<String>::new());
        assert_eq!(ids("print(1 / 0)\n"), Vec::<String>::new());
    }
}
