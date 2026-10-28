//! Turns lambdas into functions of their own, so that everything after linking sees a function
//! named as a value, and a lambda that uses variables from outside it into a closure holding a
//! copy of each.
//!
//! For example, in `fn over(xs: list[int], limit: int) -> list[int]: ret xs.filter(|x| x > limit)`
//! the lambda becomes `fn lambda.1.52(limit, x): ret x > limit`, whose first parameter takes the
//! copy, and the argument becomes [`ExprKind::Closure`] of `lambda.1.52` holding `limit`.

use crate::ast::{Arg, Arguments, Expr, ExprContext, ExprKind, Stmt, StmtKind, lambda_name};
use crate::checker::collect_assigned;
use crate::span::Span;
use std::collections::HashSet;

/// Turns every lambda in `body` into a function of its own, named by [`lambda_name`], which goes
/// just before the top-level statement holding it, and the lambda into that function's name, or,
/// if it uses variables from outside it, into a [`ExprKind::Closure`] of them.
///
/// For example, `ys = xs.map(|x| x + 1)` on line 2 becomes `fn lambda.2.12(x): ret x + 1`
/// followed by `ys = xs.map(lambda.2.12)`. A lambda inside another goes first.
///
/// A lambda captures a name it reads that is a variable where it is written: a parameter of an
/// enclosing function or lambda, a name the enclosing function assigns, or a name the top level
/// assigns. Names of functions, types, and modules are never assigned, so they are not captured.
pub fn lift_lambdas(body: &mut Vec<Stmt>) {
    let mut assigned = vec![];
    collect_assigned(body, &mut assigned);
    let globals: HashSet<String> = assigned.into_iter().map(|(name, _)| name).collect();
    let mut lifted = Vec::with_capacity(body.len());
    for mut stmt in std::mem::take(body) {
        let mut lifter = Lifter {
            scopes: vec![globals.clone()],
            functions: vec![],
        };
        lifter.stmt(&mut stmt);
        lifted.extend(lifter.functions);
        lifted.push(stmt);
    }
    *body = lifted;
}

/// Lifts the lambdas of one top-level statement.
struct Lifter {
    /// The variables of each scope around the code being lifted, outermost first: the top
    /// level's, then an enclosing function's, then each enclosing lambda's parameters.
    scopes: Vec<HashSet<String>>,
    /// The functions made of lambdas so far, inner ones first.
    functions: Vec<Stmt>,
}

impl Lifter {
    /// Lifts the lambdas of `stmt` and of the blocks it holds.
    fn stmt(&mut self, stmt: &mut Stmt) {
        let blocks: Vec<&mut Vec<Stmt>> = match &mut stmt.kind {
            StmtKind::FunctionDef { args, body, .. } => {
                let mut assigned = vec![];
                collect_assigned(body, &mut assigned);
                let scope = args
                    .args
                    .iter()
                    .map(|arg| arg.arg.clone())
                    .chain(assigned.into_iter().map(|(name, _)| name))
                    .collect();
                self.scopes.push(scope);
                for stmt in body.iter_mut() {
                    self.stmt(stmt);
                }
                self.scopes.pop();
                vec![]
            }
            StmtKind::For {
                target, iter, body, ..
            } => {
                self.expr(target);
                self.expr(iter);
                vec![body]
            }
            StmtKind::While { test, body } => {
                self.expr(test);
                vec![body]
            }
            StmtKind::If { test, body, orelse } => {
                self.expr(test);
                vec![body, orelse]
            }
            StmtKind::ImplDef { methods, .. } | StmtKind::TraitDef { methods, .. } => vec![methods],
            StmtKind::Match { subject, cases } => {
                self.expr(subject);
                cases.iter_mut().map(|case| &mut case.body).collect()
            }
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
                vec![]
            }
            StmtKind::Assign { targets, value } => {
                targets.iter_mut().for_each(|target| self.expr(target));
                self.expr(value);
                vec![]
            }
            StmtKind::AugAssign { target, value, .. } => {
                self.expr(target);
                self.expr(value);
                vec![]
            }
            StmtKind::Expr { value } => {
                self.expr(value);
                vec![]
            }
            StmtKind::Use { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::Break
            | StmtKind::Continue => vec![],
        };
        for block in blocks {
            for stmt in block {
                self.stmt(stmt);
            }
        }
    }

    /// Lifts the lambdas of `expr`, inner ones first.
    fn expr(&mut self, expr: &mut Expr) {
        let ExprKind::Lambda { args, body } = &mut expr.kind else {
            for child in expr.children_mut() {
                self.expr(child);
            }
            return;
        };
        let own: HashSet<String> = args.args.iter().map(|arg| arg.arg.clone()).collect();
        self.scopes.push(own.clone());
        self.expr(body);
        self.scopes.pop();
        let captures = self.captures(body, &own);

        let span = expr.span;
        let name = lambda_name(span);
        let named = match captures.is_empty() {
            true => ExprKind::Name {
                id: name.clone(),
                ctx: ExprContext::Load,
            },
            false => ExprKind::Closure {
                function: name.clone(),
                captures: captures
                    .iter()
                    .map(|(id, span)| {
                        let read = ExprKind::Name {
                            id: id.clone(),
                            ctx: ExprContext::Load,
                        };
                        Expr::new(read, *span)
                    })
                    .collect(),
            },
        };
        let ExprKind::Lambda { args, body } = std::mem::replace(&mut expr.kind, named) else {
            return;
        };
        let count = captures.len();
        let params = captures
            .into_iter()
            .map(|(arg, span)| Arg {
                arg,
                span,
                annotation: None,
            })
            .chain(args.args)
            .collect();
        let ret = Stmt::new(StmtKind::Return { value: Some(body) }, span);
        let function = StmtKind::FunctionDef {
            name,
            // no name is written, so nothing refers to the function where it is defined
            name_span: Span {
                end: span.start,
                ..span
            },
            public: false,
            type_params: vec![],
            args: Arguments {
                args: params,
                captures: count,
            },
            returns: None,
            body: vec![ret],
        };
        self.functions.push(Stmt::new(function, span));
    }

    /// Returns the variables of the scopes around a lambda that `body`, its lifted body, reads,
    /// leaving out its own parameters `own`, each once with the span of its first read, in
    /// order.
    ///
    /// For example, `x > limit` with `x` its own and `limit` an enclosing function's parameter
    /// gives `limit`.
    fn captures(&self, body: &Expr, own: &HashSet<String>) -> Vec<(String, Span)> {
        let mut captures: Vec<(String, Span)> = vec![];
        body.visit(&mut |expr| {
            if let ExprKind::Name { id, .. } = &expr.kind
                && !own.contains(id)
                && self.scopes.iter().any(|scope| scope.contains(id))
                && !captures.iter().any(|(seen, _)| seen == id)
            {
                captures.push((id.clone(), expr.span));
            }
        });
        captures
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Mod;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    /// Returns the statements of `source`, which must parse, with its lambdas lifted.
    fn lifted(source: &str) -> Vec<Stmt> {
        let tokens = Lexer::new(source).lex().expect("test source should lex");
        let Mod::Module { mut body } = Parser::new(&tokens)
            .parse()
            .expect("test source should parse");
        lift_lambdas(&mut body);
        body
    }

    /// Returns the name of each function made of a lambda in `body`, with the names of its
    /// parameters, captures first, and how many are captures.
    fn lambdas(body: &[Stmt]) -> Vec<(String, Vec<String>, usize)> {
        body.iter()
            .filter_map(|stmt| match &stmt.kind {
                StmtKind::FunctionDef { name, args, .. } if crate::ast::is_lambda(name) => Some((
                    name.clone(),
                    args.args.iter().map(|arg| arg.arg.clone()).collect(),
                    args.captures,
                )),
                _ => None,
            })
            .collect()
    }

    /// Returns the names each closure in `expr` holds, with its function's name.
    fn closures(expr: &Expr) -> Vec<(String, Vec<String>)> {
        let mut found = vec![];
        expr.visit(&mut |expr| {
            if let ExprKind::Closure { function, captures } = &expr.kind {
                let names = captures
                    .iter()
                    .map(|capture| match &capture.kind {
                        ExprKind::Name { id, .. } => id.clone(),
                        other => panic!("expected a name, got {other:?}"),
                    })
                    .collect();
                found.push((function.clone(), names));
            }
        });
        found
    }

    fn names(names: &[&str]) -> Vec<String> {
        names.iter().map(|name| name.to_string()).collect()
    }

    #[test]
    fn a_lambda_without_outside_variables_stays_a_name() {
        let body = lifted("ys = [1].map(|x| x + 1)\n");
        assert_eq!(
            lambdas(&body),
            [("lambda.1.14".to_string(), names(&["x"]), 0)]
        );
        assert_eq!(closures(body[1].exprs()[1]), []);
    }

    #[test]
    fn a_lambda_captures_a_parameter_and_a_local_of_its_function() {
        let body = lifted(
            "fn f(xs: list[int], low: int) -> list[int]:\n    \
             high = low + 10\n    ret xs.filter(|x| x > low and x < high)\n",
        );
        assert_eq!(
            lambdas(&body),
            [("lambda.3.19".to_string(), names(&["low", "high", "x"]), 2)]
        );
        let StmtKind::FunctionDef { body, .. } = &body[1].kind else {
            panic!("expected the function");
        };
        let StmtKind::Return { value: Some(value) } = &body[1].kind else {
            panic!("expected the return");
        };
        assert_eq!(
            closures(value),
            [("lambda.3.19".to_string(), names(&["low", "high"]))]
        );
    }

    #[test]
    fn a_capture_reads_at_its_first_use_inside_the_lambda() {
        let body = lifted("k = 2\nys = [1].map(|x| x * k + k)\n");
        let found = body[2].exprs()[1];
        let mut spans = vec![];
        found.visit(&mut |expr| {
            if let ExprKind::Closure { captures, .. } = &expr.kind {
                spans.extend(
                    captures
                        .iter()
                        .map(|c| (c.span.start.line, c.span.start.col)),
                );
            }
        });
        assert_eq!(spans, [(2, 22)]);
    }

    #[test]
    fn functions_types_and_modules_are_not_captured() {
        let body = lifted(
            "use math\nfn double(x: int) -> int:\n    ret x * 2\n\
             ys = [1].map(|x| double(x) + int(math.sqrt(4.0)))\n",
        );
        assert_eq!(
            lambdas(&body),
            [("lambda.4.14".to_string(), names(&["x"]), 0)]
        );
    }

    #[test]
    fn an_outer_lambda_captures_what_an_inner_one_needs() {
        let body = lifted(
            "fn f(grid: list[list[int]], k: int) -> list[list[int]]:\n    \
             ret grid.map(|row| row.map(|x| x * k))\n",
        );
        assert_eq!(
            lambdas(&body),
            [
                ("lambda.2.32".to_string(), names(&["k", "x"]), 1),
                ("lambda.2.18".to_string(), names(&["k", "row"]), 1),
            ]
        );
    }

    #[test]
    fn an_inner_lambda_captures_an_outer_lambda_s_parameter() {
        let body = lifted("ys = [[1]].map(|row| row.map(|x| x + row.len()))\n");
        assert_eq!(
            lambdas(&body),
            [
                ("lambda.1.30".to_string(), names(&["row", "x"]), 1),
                ("lambda.1.16".to_string(), names(&["row"]), 0),
            ]
        );
    }

    #[test]
    fn a_method_s_lambda_captures_self() {
        let body = lifted(
            "struct S:\n    k: int\nimpl S:\n    fn add(self, xs: list[int]) -> list[int]:\n        \
             ret xs.map(|x| x + self.k)\n",
        );
        assert_eq!(
            lambdas(&body),
            [("lambda.5.20".to_string(), names(&["self", "x"]), 1)]
        );
    }

    #[test]
    fn a_parameter_of_its_own_hides_an_outside_variable() {
        let body = lifted("x = 1\nys = [1].map(|x| x + 1)\n");
        assert_eq!(
            lambdas(&body),
            [("lambda.2.14".to_string(), names(&["x"]), 0)]
        );
    }
}
