use super::{MAX_DEPTH, Parser};
use crate::ast::*;
use crate::diagnostic::Diagnostic;
use crate::lexer::Lexer;
use crate::span::{Pos, Span};
use crate::token::{Token, TokenType};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

/// Builds a one-character token of type `r#type` starting at `line` and `col`.
///
/// For example, `tok(TokenType::Comma, 1, 4)` spans line 1, cols 4 to 5.
fn tok(r#type: TokenType, line: usize, col: usize) -> Token {
    Token::new(
        r#type,
        Span::new(Pos::new(line, col), Pos::new(line, col + 1)),
    )
}

/// Wraps an expression kind in an [`Expr`] with an empty span, so expected trees stay short.
impl From<ExprKind> for Expr {
    fn from(kind: ExprKind) -> Self {
        Expr::new(kind, Span::default())
    }
}

/// Wraps a statement kind in a [`Stmt`] with an empty span, so expected trees stay short.
impl From<StmtKind> for Stmt {
    fn from(kind: StmtKind) -> Self {
        Stmt::new(kind, Span::default())
    }
}

/// Returns `module` with every span reset to empty, for comparing against trees built with
/// `.into()`.
///
/// For example, the parsed `x = 1` and `StmtKind::Assign { .. }.into()` compare equal once spans
/// are stripped.
fn without_spans(module: Mod) -> Mod {
    let Mod::Module { body } = module;
    Mod::Module {
        body: body.into_iter().map(strip_stmt).collect(),
    }
}

fn strip_stmt(stmt: Stmt) -> Stmt {
    let strip_all = |stmts: Vec<Stmt>| stmts.into_iter().map(strip_stmt).collect();
    let kind = match stmt.kind {
        StmtKind::FunctionDef {
            name,
            public,
            args,
            returns,
            body,
            ..
        } => StmtKind::FunctionDef {
            name,
            name_span: Span::default(),
            public,
            args: Arguments {
                args: args
                    .args
                    .into_iter()
                    .map(|a| Arg {
                        arg: a.arg,
                        span: Span::default(),
                        annotation: a.annotation.map(strip_type),
                    })
                    .collect(),
            },
            returns: returns.map(strip_type),
            body: strip_all(body),
        },
        StmtKind::Return { value } => StmtKind::Return {
            value: value.map(|v| Box::new(strip_expr(*v))),
        },
        StmtKind::Assign { targets, value } => StmtKind::Assign {
            targets: targets.into_iter().map(strip_expr).collect(),
            value: Box::new(strip_expr(*value)),
        },
        StmtKind::AugAssign { target, op, value } => StmtKind::AugAssign {
            target: Box::new(strip_expr(*target)),
            op,
            value: Box::new(strip_expr(*value)),
        },
        StmtKind::For { target, iter, body } => StmtKind::For {
            target: Box::new(strip_expr(*target)),
            iter: Box::new(strip_expr(*iter)),
            body: strip_all(body),
        },
        StmtKind::While { test, body } => StmtKind::While {
            test: Box::new(strip_expr(*test)),
            body: strip_all(body),
        },
        StmtKind::If { test, body, orelse } => StmtKind::If {
            test: Box::new(strip_expr(*test)),
            body: strip_all(body),
            orelse: strip_all(orelse),
        },
        StmtKind::Expr { value } => StmtKind::Expr {
            value: Box::new(strip_expr(*value)),
        },
        StmtKind::Use { path, alias } => StmtKind::Use {
            path: path
                .into_iter()
                .map(|(name, _)| (name, Span::default()))
                .collect(),
            alias: alias.map(|(name, _)| (name, Span::default())),
        },
        StmtKind::EnumDef {
            name,
            public,
            variants,
            ..
        } => StmtKind::EnumDef {
            name,
            name_span: Span::default(),
            public,
            variants: variants
                .into_iter()
                .map(|v| Variant {
                    name: v.name,
                    span: Span::default(),
                    fields: v.fields.into_iter().map(strip_type).collect(),
                })
                .collect(),
        },
        StmtKind::StructDef {
            name,
            public,
            fields,
            ..
        } => StmtKind::StructDef {
            name,
            name_span: Span::default(),
            public,
            fields: fields
                .into_iter()
                .map(|f| Field {
                    name: f.name,
                    span: Span::default(),
                    annotation: strip_type(f.annotation),
                })
                .collect(),
        },
        StmtKind::Match { subject, cases } => StmtKind::Match {
            subject: Box::new(strip_expr(*subject)),
            cases: cases
                .into_iter()
                .map(|case| MatchCase {
                    pattern: strip_pattern(case.pattern),
                    body: strip_all(case.body),
                })
                .collect(),
        },
        kind @ (StmtKind::Break | StmtKind::Continue) => kind,
    };
    kind.into()
}

fn strip_pattern(pattern: Pattern) -> Pattern {
    let kind = match pattern.kind {
        PatternKind::Variant { path, fields } => PatternKind::Variant {
            path: path
                .into_iter()
                .map(|(name, _)| (name, Span::default()))
                .collect(),
            fields: fields.into_iter().map(strip_pattern).collect(),
        },
        kind => kind,
    };
    Pattern {
        kind,
        span: Span::default(),
    }
}

fn strip_type(ty: TypeExpr) -> TypeExpr {
    let kind = match ty.kind {
        TypeExprKind::List(elem) => TypeExprKind::List(Box::new(strip_type(*elem))),
        TypeExprKind::Map(key, value) => {
            TypeExprKind::Map(Box::new(strip_type(*key)), Box::new(strip_type(*value)))
        }
        TypeExprKind::Tuple(elts) => {
            TypeExprKind::Tuple(elts.into_iter().map(strip_type).collect())
        }
        TypeExprKind::Named { name, args } => TypeExprKind::Named {
            name,
            args: args.into_iter().map(strip_type).collect(),
        },
        kind => kind,
    };
    TypeExpr {
        kind,
        span: Span::default(),
    }
}

fn strip_expr(expr: Expr) -> Expr {
    let strip_box = |e: Box<Expr>| Box::new(strip_expr(*e));
    let strip_all = |es: Vec<Expr>| es.into_iter().map(strip_expr).collect();
    let kind = match expr.kind {
        ExprKind::BoolOp { op, values } => ExprKind::BoolOp {
            op,
            values: strip_all(values),
        },
        ExprKind::BinOp { op, left, right } => ExprKind::BinOp {
            op,
            left: strip_box(left),
            right: strip_box(right),
        },
        ExprKind::UnaryOp { op, operand } => ExprKind::UnaryOp {
            op,
            operand: strip_box(operand),
        },
        ExprKind::Compare {
            left,
            ops,
            comparators,
        } => ExprKind::Compare {
            left: strip_box(left),
            ops,
            comparators: strip_all(comparators),
        },
        ExprKind::Call {
            func,
            args,
            keywords,
        } => ExprKind::Call {
            func: strip_box(func),
            args: strip_all(args),
            keywords: keywords
                .into_iter()
                .map(|k| Keyword {
                    arg: k.arg,
                    arg_span: Span::default(),
                    value: strip_expr(k.value),
                })
                .collect(),
        },
        ExprKind::Subscript { value, slice, ctx } => ExprKind::Subscript {
            value: strip_box(value),
            slice: strip_box(slice),
            ctx,
        },
        ExprKind::Attribute { value, attr, ctx } => ExprKind::Attribute {
            value: strip_box(value),
            attr,
            ctx,
        },
        ExprKind::List { elts, ctx } => ExprKind::List {
            elts: strip_all(elts),
            ctx,
        },
        ExprKind::Dict { keys, values } => ExprKind::Dict {
            keys: strip_all(keys),
            values: strip_all(values),
        },
        ExprKind::Tuple { elts, ctx } => ExprKind::Tuple {
            elts: strip_all(elts),
            ctx,
        },
        ExprKind::Slice { lower, upper, step } => ExprKind::Slice {
            lower: lower.map(strip_box),
            upper: upper.map(strip_box),
            step: step.map(strip_box),
        },
        kind @ (ExprKind::Constant { .. } | ExprKind::Name { .. }) => kind,
    };
    kind.into()
}

/// Checks that this program parses into assignments and a return:
///
/// ```text
/// x = 42
/// y = x + 8
/// ret y
/// ```
#[test]
fn simple_program() {
    let _tokens = r#"
x = 42
y = x + 8
ret y
"#;

    let tokens = vec![
        tok(TokenType::Name("x".to_string()), 1, 1),
        tok(TokenType::Operator("=".to_string()), 1, 3),
        tok(TokenType::Number(42), 1, 5),
        tok(TokenType::Newline, 1, 7),
        tok(TokenType::Name("y".to_string()), 2, 1),
        tok(TokenType::Operator("=".to_string()), 2, 3),
        tok(TokenType::Name("x".to_string()), 2, 5),
        tok(TokenType::Operator("+".to_string()), 2, 7),
        tok(TokenType::Number(8), 2, 9),
        tok(TokenType::Newline, 2, 10),
        tok(TokenType::Keyword("ret".to_string()), 3, 1),
        tok(TokenType::Name("y".to_string()), 3, 5),
        tok(TokenType::Newline, 3, 6),
        tok(TokenType::Eof, 4, 1),
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast.map(without_spans),
        Ok(Mod::Module {
            body: vec![
                StmtKind::Assign {
                    targets: vec![
                        ExprKind::Name {
                            id: "x".to_string(),
                            ctx: ExprContext::Store
                        }
                        .into()
                    ],
                    value: Box::new(
                        ExprKind::Constant {
                            value: Box::new(Constant::Int(42)),
                            kind: None
                        }
                        .into()
                    )
                }
                .into(),
                StmtKind::Assign {
                    targets: vec![
                        ExprKind::Name {
                            id: "y".to_string(),
                            ctx: ExprContext::Store
                        }
                        .into()
                    ],
                    value: Box::new(
                        ExprKind::BinOp {
                            op: Operator::Add,
                            left: Box::new(
                                ExprKind::Name {
                                    id: "x".to_string(),
                                    ctx: ExprContext::Load
                                }
                                .into()
                            ),
                            right: Box::new(
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(8)),
                                    kind: None
                                }
                                .into()
                            )
                        }
                        .into()
                    )
                }
                .into(),
                StmtKind::Return {
                    value: Some(Box::new(
                        ExprKind::Name {
                            id: "y".to_string(),
                            ctx: ExprContext::Load
                        }
                        .into()
                    ))
                }
                .into()
            ]
        })
    );
}

/// Checks that a function with no parameters parses:
///
/// ```text
/// fn testing();
///     ret 52
/// testing()
/// ```
#[test]
fn simple_function_no_args() {
    let tokens = vec![
        tok(TokenType::Keyword("fn".to_string()), 1, 1),
        tok(TokenType::Name("testing".to_string()), 1, 5),
        tok(TokenType::LParen, 1, 12),
        tok(TokenType::RParen, 1, 13),
        tok(TokenType::Semi, 1, 14),
        tok(TokenType::Newline, 1, 15),
        tok(TokenType::Indent, 2, 1),
        tok(TokenType::Keyword("ret".to_string()), 2, 5),
        tok(TokenType::Number(52), 2, 9),
        tok(TokenType::Newline, 2, 11),
        tok(TokenType::Dedent, 3, 1),
        tok(TokenType::Name("testing".to_string()), 3, 1),
        tok(TokenType::LParen, 3, 8),
        tok(TokenType::RParen, 3, 9),
        tok(TokenType::Newline, 3, 10),
        tok(TokenType::Eof, 4, 1),
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast.map(without_spans),
        Ok(Mod::Module {
            body: vec![
                StmtKind::FunctionDef {
                    name: "testing".to_string(),
                    name_span: Span::default(),
                    public: false,
                    args: Arguments { args: vec![] },
                    returns: None,
                    body: vec![
                        StmtKind::Return {
                            value: Some(Box::new(
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(52)),
                                    kind: None
                                }
                                .into()
                            ))
                        }
                        .into()
                    ]
                }
                .into(),
                StmtKind::Expr {
                    value: Box::new(
                        ExprKind::Call {
                            func: Box::new(
                                ExprKind::Name {
                                    id: "testing".to_string(),
                                    ctx: ExprContext::Load
                                }
                                .into()
                            ),
                            args: vec![],
                            keywords: vec![],
                        }
                        .into()
                    )
                }
                .into(),
            ]
        })
    );
}

/// Checks that a function with one parameter parses:
///
/// ```text
/// fn testing(a);
///     ret a + 2
/// testing(4)
/// ```
#[test]
fn simple_function_one_arg() {
    let tokens = vec![
        tok(TokenType::Keyword("fn".to_string()), 1, 1),
        tok(TokenType::Name("testing".to_string()), 1, 5),
        tok(TokenType::LParen, 1, 12),
        tok(TokenType::Name("a".to_string()), 1, 13),
        tok(TokenType::RParen, 1, 14),
        tok(TokenType::Semi, 1, 15),
        tok(TokenType::Newline, 1, 16),
        tok(TokenType::Indent, 2, 1),
        tok(TokenType::Keyword("ret".to_string()), 2, 5),
        tok(TokenType::Name("a".to_string()), 2, 9),
        tok(TokenType::Operator("+".to_string()), 2, 11),
        tok(TokenType::Number(2), 2, 13),
        tok(TokenType::Newline, 2, 14),
        tok(TokenType::Dedent, 3, 1),
        tok(TokenType::Name("testing".to_string()), 3, 1),
        tok(TokenType::LParen, 3, 8),
        tok(TokenType::Number(4), 3, 9),
        tok(TokenType::RParen, 3, 10),
        tok(TokenType::Newline, 3, 11),
        tok(TokenType::Eof, 4, 1),
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast.map(without_spans),
        Ok(Mod::Module {
            body: vec![
                StmtKind::FunctionDef {
                    name: "testing".to_string(),
                    name_span: Span::default(),
                    public: false,
                    args: Arguments {
                        args: vec![Arg {
                            arg: "a".to_string(),
                            span: Span::default(),
                            annotation: None,
                        }]
                    },
                    returns: None,
                    body: vec![
                        StmtKind::Return {
                            value: Some(Box::new(
                                ExprKind::BinOp {
                                    op: Operator::Add,
                                    left: Box::new(
                                        ExprKind::Name {
                                            id: "a".to_string(),
                                            ctx: ExprContext::Load
                                        }
                                        .into()
                                    ),
                                    right: Box::new(
                                        ExprKind::Constant {
                                            value: Box::new(Constant::Int(2)),
                                            kind: None
                                        }
                                        .into()
                                    )
                                }
                                .into()
                            ))
                        }
                        .into()
                    ]
                }
                .into(),
                StmtKind::Expr {
                    value: Box::new(
                        ExprKind::Call {
                            func: Box::new(
                                ExprKind::Name {
                                    id: "testing".to_string(),
                                    ctx: ExprContext::Load
                                }
                                .into()
                            ),
                            args: vec![
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(4)),
                                    kind: None
                                }
                                .into()
                            ],
                            keywords: vec![],
                        }
                        .into()
                    )
                }
                .into(),
            ]
        })
    );
}

/// Checks that a function with several parameters parses:
///
/// ```text
/// fn testing(a, b, c);
///     ret b
/// testing(1, 2, 3)
/// ```
#[test]
fn simple_function_multiple_args() {
    let tokens = vec![
        tok(TokenType::Keyword("fn".to_string()), 1, 1),
        tok(TokenType::Name("testing".to_string()), 1, 5),
        tok(TokenType::LParen, 1, 12),
        tok(TokenType::Name("a".to_string()), 1, 13),
        tok(TokenType::Comma, 1, 14),
        tok(TokenType::Name("b".to_string()), 1, 16),
        tok(TokenType::Comma, 1, 17),
        tok(TokenType::Name("c".to_string()), 1, 19),
        tok(TokenType::RParen, 1, 20),
        tok(TokenType::Semi, 1, 21),
        tok(TokenType::Newline, 1, 22),
        tok(TokenType::Indent, 2, 1),
        tok(TokenType::Keyword("ret".to_string()), 2, 5),
        tok(TokenType::Name("b".to_string()), 2, 9),
        tok(TokenType::Newline, 2, 10),
        tok(TokenType::Dedent, 3, 1),
        tok(TokenType::Name("testing".to_string()), 3, 1),
        tok(TokenType::LParen, 3, 8),
        tok(TokenType::Number(1), 3, 9),
        tok(TokenType::Comma, 3, 10),
        tok(TokenType::Number(2), 3, 12),
        tok(TokenType::Comma, 3, 13),
        tok(TokenType::Number(3), 3, 15),
        tok(TokenType::RParen, 3, 16),
        tok(TokenType::Newline, 3, 17),
        tok(TokenType::Eof, 4, 1),
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast.map(without_spans),
        Ok(Mod::Module {
            body: vec![
                StmtKind::FunctionDef {
                    name: "testing".to_string(),
                    name_span: Span::default(),
                    public: false,
                    args: Arguments {
                        args: vec![
                            Arg {
                                arg: "a".to_string(),
                                span: Span::default(),
                                annotation: None,
                            },
                            Arg {
                                arg: "b".to_string(),
                                span: Span::default(),
                                annotation: None,
                            },
                            Arg {
                                arg: "c".to_string(),
                                span: Span::default(),
                                annotation: None,
                            }
                        ]
                    },
                    returns: None,
                    body: vec![
                        StmtKind::Return {
                            value: Some(Box::new(
                                ExprKind::Name {
                                    id: "b".to_string(),
                                    ctx: ExprContext::Load
                                }
                                .into()
                            ))
                        }
                        .into()
                    ]
                }
                .into(),
                StmtKind::Expr {
                    value: Box::new(
                        ExprKind::Call {
                            func: Box::new(
                                ExprKind::Name {
                                    id: "testing".to_string(),
                                    ctx: ExprContext::Load
                                }
                                .into()
                            ),
                            args: vec![
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(1)),
                                    kind: None
                                }
                                .into(),
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(2)),
                                    kind: None
                                }
                                .into(),
                                ExprKind::Constant {
                                    value: Box::new(Constant::Int(3)),
                                    kind: None
                                }
                                .into()
                            ],
                            keywords: vec![],
                        }
                        .into()
                    )
                }
                .into(),
            ]
        })
    );
}

/// Lexes and parses `source` on a separate thread, failing the test if parsing takes longer than
/// `limit`.
///
/// The thread gets the same 8 MiB stack as the main thread `stone` parses on, since
/// [`MAX_DEPTH`] is sized for that rather than for the 2 MiB default of spawned threads.
///
/// For example, `parse_within("x = 1\n".to_string(), Duration::from_secs(5))` returns the parsed
/// module.
fn parse_within(source: String, limit: Duration) -> Result<Mod, Diagnostic> {
    let (tx, rx) = mpsc::channel();
    thread::Builder::new()
        .stack_size(8 * 1024 * 1024)
        .spawn(move || {
            let tokens = Lexer::new(&source).lex().unwrap();
            let _ = tx.send(Parser::new(&tokens).parse());
        })
        .expect("parser thread should spawn");
    rx.recv_timeout(limit)
        .expect("parser did not finish in time")
}

#[test]
fn nested_calls_parse_in_linear_time() {
    let depth = 40;
    let source = format!("{}x{}\n", "f(".repeat(depth), ")".repeat(depth));
    assert!(parse_within(source, Duration::from_secs(5)).is_ok());
}

/// Builds `depth` nested `if 1;` statements around a single assignment.
///
/// For example, `nested_ifs(2)` returns `"if 1;\n    if 1;\n        x = 1\n"`.
fn nested_ifs(depth: usize) -> String {
    let mut source = String::new();
    for level in 0..depth {
        source += &format!("{}if 1;\n", "    ".repeat(level));
    }
    source + &format!("{}x = 1\n", "    ".repeat(depth))
}

#[test]
fn nested_ifs_parse_in_linear_time() {
    assert!(parse_within(nested_ifs(40), Duration::from_secs(5)).is_ok());
}

#[test]
fn if_elif_else_chain() {
    let source = "if 1;\n    x = 1\nelif 2;\n    x = 2\nelif 3;\n    x = 3\nelse;\n    x = 4\n";
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::If { orelse, .. } = &body[0].kind else {
        panic!("expected if statement");
    };
    let StmtKind::If { orelse, .. } = &orelse[0].kind else {
        panic!("expected first elif");
    };
    let StmtKind::If { orelse, .. } = &orelse[0].kind else {
        panic!("expected second elif");
    };
    assert_eq!(orelse.len(), 1);
    assert!(matches!(orelse[0].kind, StmtKind::Assign { .. }));
}

#[test]
fn deeply_nested_input_is_an_error() {
    let depth = 100_000;
    let sources = [
        format!("{}x{}\n", "f(".repeat(depth), ")".repeat(depth)),
        format!("x = {}1\n", "-".repeat(depth)),
        // indentation grows quadratically, so only go just past the limit
        nested_ifs(MAX_DEPTH + 1),
    ];
    for source in sources {
        assert!(parse_within(source, Duration::from_secs(5)).is_err());
    }
}

#[test]
fn nesting_just_under_the_limit_still_parses() {
    // the statement's own expression takes one level, so leave room for it
    let depth = MAX_DEPTH - 1;
    let sources = [
        format!("{}x{}\n", "f(".repeat(depth), ")".repeat(depth)),
        format!("x = {}1\n", "-".repeat(depth)),
        nested_ifs(depth),
    ];
    for source in sources {
        assert!(parse_within(source, Duration::from_secs(5)).is_ok());
    }
}

#[test]
fn nesting_just_over_the_limit_is_an_error() {
    let depth = MAX_DEPTH;
    let source = format!("{}x{}\n", "f(".repeat(depth), ")".repeat(depth));
    assert!(parse_within(source, Duration::from_secs(5)).is_err());
}

/// Builds the span from `(line, col)` up to but not including `(end_line, end_col)`.
fn span(line: usize, col: usize, end_line: usize, end_col: usize) -> Span {
    Span::new(Pos::new(line, col), Pos::new(end_line, end_col))
}

#[test]
fn expressions_span_their_source_text() {
    let Mod::Module { body } =
        parse_within("x = a + 12\n".to_string(), Duration::from_secs(5)).unwrap();
    assert_eq!(body[0].span, span(1, 1, 1, 11));
    let StmtKind::Assign { targets, value } = &body[0].kind else {
        panic!("expected assignment");
    };
    assert_eq!(targets[0].span, span(1, 1, 1, 2));
    assert_eq!(value.span, span(1, 5, 1, 11));
    let ExprKind::BinOp { left, right, .. } = &value.kind else {
        panic!("expected binary operation");
    };
    assert_eq!(left.span, span(1, 5, 1, 6));
    assert_eq!(right.span, span(1, 9, 1, 11));
}

#[test]
fn calls_and_unary_operators_span_their_source_text() {
    let Mod::Module { body } =
        parse_within("f(-x, y)\n".to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected expression statement");
    };
    assert_eq!(value.span, span(1, 1, 1, 9));
    let ExprKind::Call { args, .. } = &value.kind else {
        panic!("expected call");
    };
    assert_eq!(args[0].span, span(1, 3, 1, 5));
}

#[test]
fn function_definitions_span_their_name_and_parameters() {
    let source = "fn f(a, bc);\n    ret a\n";
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    // the block ends at its last token, not at the dedent after it
    assert_eq!(body[0].span, span(1, 1, 2, 10));
    let StmtKind::FunctionDef {
        name_span, args, ..
    } = &body[0].kind
    else {
        panic!("expected function definition");
    };
    assert_eq!(*name_span, span(1, 4, 1, 5));
    assert_eq!(args.args[1].span, span(1, 9, 1, 11));
}

/// Parses `source`, which must start with a function definition, and returns each parameter's
/// written type and the written return type, as source text.
///
/// For example, `signature_of("fn f(a: int) -> str; ret \"\"\n")` returns
/// `(vec![Some("int")], Some("str"))`.
fn signature_of(source: &str) -> (Vec<Option<String>>, Option<String>) {
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::FunctionDef { args, returns, .. } = &body[0].kind else {
        panic!("expected function definition");
    };
    let params = args
        .args
        .iter()
        .map(|a| a.annotation.as_ref().map(TypeExpr::to_string))
        .collect();
    (params, returns.as_ref().map(TypeExpr::to_string))
}

#[test]
fn parameters_and_the_return_value_have_types() {
    assert_eq!(
        signature_of("fn f(a: int, b: float, c: bool, d: str) -> int; ret a\n"),
        (
            ["int", "float", "bool", "str"]
                .map(|t| Some(t.to_string()))
                .to_vec(),
            Some("int".to_string())
        )
    );
}

#[test]
fn list_types_nest() {
    assert_eq!(
        signature_of("fn f(grid: list[list[float]]) -> list[str]; ret []\n"),
        (
            vec![Some("list[list[float]]".to_string())],
            Some("list[str]".to_string())
        )
    );
}

#[test]
fn types_may_be_left_out_for_the_checker_to_report() {
    assert_eq!(
        signature_of("fn f(a, b: int); ret\n"),
        (vec![None, Some("int".to_string())], None)
    );
}

#[test]
fn types_span_their_text() {
    let source = "fn f(xs: list[int]) -> bool; ret\n";
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::FunctionDef { args, returns, .. } = &body[0].kind else {
        panic!("expected function definition");
    };
    let annotation = args.args[0].annotation.as_ref().unwrap();
    assert_eq!(annotation.span, span(1, 10, 1, 19));
    let TypeExprKind::List(elem) = &annotation.kind else {
        panic!("expected a list type");
    };
    assert_eq!(elem.span, span(1, 15, 1, 18));
    assert_eq!(returns.as_ref().unwrap().span, span(1, 24, 1, 28));
}

#[test]
fn an_unknown_type_is_an_error() {
    assert_eq!(
        error_of("fn f(a: int) -> 3; ret\n"),
        ("expected a type, found '3'".to_string(), 1, 17)
    );
}

#[test]
fn a_list_type_needs_its_element_type() {
    assert_eq!(
        error_of("fn f(xs: list); ret\n"),
        ("expected '[', found ')'".to_string(), 1, 14)
    );
    assert_eq!(
        error_of("fn f(xs: list[int); ret\n"),
        ("expected ']', found ')'".to_string(), 1, 18)
    );
}

#[test]
fn a_colon_needs_a_type_after_it() {
    assert_eq!(
        error_of("fn f(a: , b: int); ret\n"),
        ("expected a type, found ','".to_string(), 1, 9)
    );
}

/// Parses `source`, which must fail, and returns the error message and where it starts.
///
/// For example, `error_of("x = \n")` returns `("expected an expression, found end of line", 1, 5)`.
fn error_of(source: &str) -> (String, usize, usize) {
    let err = parse_within(source.to_string(), Duration::from_secs(5)).unwrap_err();
    (err.message, err.span.start.line, err.span.start.col)
}

#[test]
fn missing_semicolon_after_condition() {
    assert_eq!(
        error_of("if x\n    y = 1\n"),
        ("expected ';', found end of line".to_string(), 1, 5)
    );
}

#[test]
fn missing_closing_paren() {
    assert_eq!(
        error_of("print(1\n"),
        ("expected ')', found end of line".to_string(), 1, 8)
    );
}

#[test]
fn missing_expression() {
    assert_eq!(
        error_of("x = \n"),
        (
            "expected an expression, found end of line".to_string(),
            1,
            5
        )
    );
}

#[test]
fn bad_parameter() {
    assert_eq!(
        error_of("fn f(;\n    ret 1\n"),
        (
            "expected a parameter name or ')', found ';'".to_string(),
            1,
            6
        )
    );
}

#[test]
fn trailing_tokens_after_statement() {
    assert_eq!(
        error_of("x = 1 2\n"),
        ("expected end of line, found '2'".to_string(), 1, 7)
    );
}

#[test]
fn unexpected_indent() {
    assert_eq!(
        error_of("x = 1\n    y = 2\n"),
        ("unexpected indent".to_string(), 2, 1)
    );
}

#[test]
fn too_deep_nesting_names_the_limit() {
    let depth = MAX_DEPTH;
    let source = format!("{}x{}\n", "f(".repeat(depth), ")".repeat(depth));
    let (message, _, _) = error_of(&source);
    assert_eq!(
        message,
        format!("code is nested more than {MAX_DEPTH} levels deep")
    );
}

/// Returns the number of arguments in the call that makes up the first statement of `source`.
fn call_arg_count(source: &str) -> usize {
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected expression statement");
    };
    let ExprKind::Call { args, .. } = &value.kind else {
        panic!("expected call");
    };
    args.len()
}

#[test]
fn call_arguments_allow_a_trailing_comma() {
    assert_eq!(call_arg_count("f(1, 2,)\n"), 2);
    assert_eq!(call_arg_count("f(1,)\n"), 1);
}

#[test]
fn failed_call_arguments_are_an_error_not_an_empty_call() {
    let source = "f(1, , 2)\n";
    assert!(parse_within(source.to_string(), Duration::from_secs(5)).is_err());
}

#[test]
fn failed_parameters_are_an_error_not_an_empty_list() {
    assert_eq!(
        error_of("fn f(a, );\n    ret a\n"),
        ("expected a parameter name, found ')'".to_string(), 1, 9)
    );
}

/// Parses `source`, which must be a single expression statement, and returns the expression with
/// its spans stripped.
fn expr_of(source: &str) -> Expr {
    let Mod::Module { body } =
        without_spans(parse_within(source.to_string(), Duration::from_secs(5)).unwrap());
    let StmtKind::Expr { value } = body.into_iter().next().unwrap().kind else {
        panic!("expected expression statement");
    };
    *value
}

fn name(id: &str) -> Box<Expr> {
    Box::new(
        ExprKind::Name {
            id: id.to_string(),
            ctx: ExprContext::Load,
        }
        .into(),
    )
}

fn int(n: i64) -> Box<Expr> {
    Box::new(
        ExprKind::Constant {
            value: Box::new(Constant::Int(n)),
            kind: None,
        }
        .into(),
    )
}

#[test]
fn float_literals_parse() {
    assert_eq!(
        expr_of("1.5 * 2e3\n"),
        ExprKind::BinOp {
            left: Box::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Float(1.5)),
                    kind: None,
                }
                .into()
            ),
            op: Operator::Multiply,
            right: Box::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Float(2000.0)),
                    kind: None,
                }
                .into()
            ),
        }
        .into()
    );
}

#[test]
fn a_float_without_digits_after_the_point_is_an_error() {
    assert_eq!(
        // the '.' starts a method call on 1
        error_of("x = 1.\n"),
        (
            "expected a method name, found end of line".to_string(),
            1,
            7
        )
    );
}

#[test]
fn comparisons_chain() {
    assert_eq!(
        expr_of("a < b <= c\n"),
        ExprKind::Compare {
            left: name("a"),
            ops: vec![CompOp::LessThan, CompOp::LessThanEqual],
            comparators: vec![*name("b"), *name("c")],
        }
        .into()
    );
}

#[test]
fn comparisons_bind_looser_than_arithmetic_and_tighter_than_not() {
    assert_eq!(
        expr_of("not a + 1 == b\n"),
        ExprKind::UnaryOp {
            op: UnaryOp::Not,
            operand: Box::new(
                ExprKind::Compare {
                    left: Box::new(
                        ExprKind::BinOp {
                            op: Operator::Add,
                            left: name("a"),
                            right: int(1),
                        }
                        .into()
                    ),
                    ops: vec![CompOp::Equal],
                    comparators: vec![*name("b")],
                }
                .into()
            ),
        }
        .into()
    );
}

#[test]
fn every_comparison_operator_parses() {
    for (text, op) in [
        ("==", CompOp::Equal),
        ("!=", CompOp::NotEqual),
        ("<", CompOp::LessThan),
        ("<=", CompOp::LessThanEqual),
        (">", CompOp::GreaterThan),
        (">=", CompOp::GreaterThanEqual),
    ] {
        let ExprKind::Compare { ops, .. } = expr_of(&format!("a {text} b\n")).kind else {
            panic!("expected comparison for {text}");
        };
        assert_eq!(ops, [op]);
    }
}

#[test]
fn parentheses_group_expressions() {
    assert_eq!(
        expr_of("(1 + 2) * 3\n"),
        ExprKind::BinOp {
            op: Operator::Multiply,
            left: Box::new(
                ExprKind::BinOp {
                    op: Operator::Add,
                    left: int(1),
                    right: int(2),
                }
                .into()
            ),
            right: int(3),
        }
        .into()
    );
}

#[test]
fn grouped_expressions_span_their_parentheses() {
    let Mod::Module { body } =
        parse_within("(1 + 2) * 3\n".to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected expression statement");
    };
    assert_eq!(value.span, span(1, 1, 1, 12));
    let ExprKind::BinOp { left, .. } = &value.kind else {
        panic!("expected binary operation");
    };
    assert_eq!(left.span, span(1, 1, 1, 8));
}

#[test]
fn unclosed_group_is_an_error() {
    assert_eq!(
        error_of("x = (1 + 2\n"),
        ("expected ')', found end of line".to_string(), 1, 11)
    );
}

#[test]
fn for_loops_parse() {
    let Mod::Module { body } = without_spans(
        parse_within(
            "for i in range(3);\n    print(i)\n".to_string(),
            Duration::from_secs(5),
        )
        .unwrap(),
    );
    let StmtKind::For { target, iter, body } = &body[0].kind else {
        panic!("expected for loop");
    };
    assert_eq!(
        **target,
        ExprKind::Name {
            id: "i".to_string(),
            ctx: ExprContext::Store
        }
        .into()
    );
    assert_eq!(
        **iter,
        ExprKind::Call {
            func: name("range"),
            args: vec![*int(3)],
            keywords: vec![],
        }
        .into()
    );
    assert_eq!(body.len(), 1);
}

#[test]
fn for_loop_without_in_is_an_error() {
    assert_eq!(
        error_of("for i range(3);\n    print(i)\n"),
        ("expected 'in', found 'range'".to_string(), 1, 7)
    );
}

/// Parses `source`, which must be a single assignment, and returns its first target without spans.
fn assign_target_of(source: &str) -> Expr {
    let Mod::Module { body } =
        without_spans(parse_within(source.to_string(), Duration::from_secs(5)).unwrap());
    let StmtKind::Assign { mut targets, .. } = body.into_iter().next().unwrap().kind else {
        panic!("expected assignment");
    };
    targets.remove(0)
}

fn subscript(value: Box<Expr>, slice: Box<Expr>, ctx: ExprContext) -> Box<Expr> {
    Box::new(ExprKind::Subscript { value, slice, ctx }.into())
}

fn slice(lower: Option<Box<Expr>>, upper: Option<Box<Expr>>, step: Option<Box<Expr>>) -> Box<Expr> {
    Box::new(ExprKind::Slice { lower, upper, step }.into())
}

#[test]
fn slices_parse() {
    let sliced = |source: &str| {
        let ExprKind::Subscript { value, slice, .. } = expr_of(source).kind else {
            panic!("expected subscript");
        };
        assert_eq!(value, name("a"));
        slice
    };
    assert_eq!(sliced("a[1:]\n"), slice(Some(int(1)), None, None));
    assert_eq!(sliced("a[:2]\n"), slice(None, Some(int(2)), None));
    assert_eq!(sliced("a[:]\n"), slice(None, None, None));
    assert_eq!(sliced("a[::]\n"), slice(None, None, None));
    let ExprKind::Slice {
        lower: None,
        upper: None,
        step: Some(step),
    } = sliced("a[::-1]\n").kind
    else {
        panic!("expected a slice with only a step");
    };
    assert!(matches!(step.kind, ExprKind::UnaryOp { .. }));
    assert_eq!(
        sliced("a[i:j:k]\n"),
        slice(Some(name("i")), Some(name("j")), Some(name("k")))
    );
    assert_eq!(sliced("a[1:2:]\n"), slice(Some(int(1)), Some(int(2)), None));
    assert_eq!(sliced("a[0]\n"), int(0));
}

#[test]
fn slices_of_slices_parse() {
    let ExprKind::Subscript {
        value,
        slice: outer,
        ..
    } = expr_of("a[1:][0]\n").kind
    else {
        panic!("expected subscript");
    };
    assert_eq!(outer, int(0));
    assert!(matches!(value.kind, ExprKind::Subscript { .. }));
}

#[test]
fn a_slice_needs_its_closing_bracket() {
    assert_eq!(
        error_of("a[1:2:3:4]\n"),
        ("expected ']', found ':'".to_string(), 1, 8)
    );
}

#[test]
fn subscripts_can_be_assigned() {
    assert_eq!(
        assign_target_of("a[0] = 1\n"),
        *subscript(name("a"), int(0), ExprContext::Store)
    );
}

#[test]
fn nested_subscripts_can_be_assigned() {
    assert_eq!(
        assign_target_of("a[0][1] = 2\n"),
        *subscript(
            subscript(name("a"), int(0), ExprContext::Load),
            int(1),
            ExprContext::Store
        )
    );
}

#[test]
fn subscripts_of_calls_can_be_assigned() {
    let ExprKind::Subscript { value, .. } = assign_target_of("f(1)[2] = 3\n").kind else {
        panic!("expected subscript target");
    };
    assert!(matches!(value.kind, ExprKind::Call { .. }));
}

#[test]
fn list_literals_parse() {
    let list = |elts: Vec<Expr>| -> Expr {
        ExprKind::List {
            elts,
            ctx: ExprContext::Load,
        }
        .into()
    };
    assert_eq!(expr_of("[]\n"), list(vec![]));
    assert_eq!(expr_of("[1, 2,]\n"), list(vec![*int(1), *int(2)]));
    assert_eq!(
        expr_of("[[1], x]\n"),
        list(vec![list(vec![*int(1)]), *name("x")])
    );
}

#[test]
fn dict_literals_parse() {
    let dict = |pairs: Vec<(Expr, Expr)>| -> Expr {
        let (keys, values) = pairs.into_iter().unzip();
        ExprKind::Dict { keys, values }.into()
    };
    assert_eq!(expr_of("{}\n"), dict(vec![]));
    assert_eq!(
        expr_of("{1: x, 2: [3],}\n"),
        dict(vec![
            (*int(1), *name("x")),
            (
                *int(2),
                ExprKind::List {
                    elts: vec![*int(3)],
                    ctx: ExprContext::Load,
                }
                .into()
            ),
        ])
    );
    assert_eq!(
        expr_of("{1: {2: 3}}\n"),
        dict(vec![(*int(1), dict(vec![(*int(2), *int(3))]))])
    );
}

#[test]
fn unclosed_dict_is_an_error() {
    assert_eq!(
        error_of("x = {1: 2\n"),
        ("expected '}', found end of line".to_string(), 1, 10)
    );
    assert_eq!(
        error_of("x = {1, 2}\n"),
        (
            "a map entry is a key, ':', and a value, as in '{\"a\": 1}'".to_string(),
            1,
            7
        )
    );
}

#[test]
fn map_types_parse() {
    assert_eq!(
        signature_of("fn f(m: map[str, list[int]]) -> map[int, str]; ret\n"),
        (
            vec![Some("map[str, list[int]]".to_string())],
            Some("map[int, str]".to_string())
        )
    );
}

#[test]
fn a_map_type_needs_two_types() {
    assert_eq!(
        error_of("fn f(m: map[int]); ret\n").0,
        "a map type needs a key type and a value type, as in 'map[str, int]'"
    );
}

#[test]
fn unclosed_list_is_an_error() {
    assert_eq!(
        error_of("x = [1, 2\n"),
        ("expected ']', found end of line".to_string(), 1, 10)
    );
}

/// Parses `source` with recovery, returning the statements it kept and each error's message and
/// position.
fn recover(source: &str) -> (Vec<Stmt>, Vec<(String, usize, usize)>) {
    let tokens = Lexer::new(source).lex().unwrap();
    let (Mod::Module { body }, diagnostics) = Parser::new(&tokens).parse_recovering();
    let errors = diagnostics
        .into_iter()
        .map(|d| (d.message, d.span.start.line, d.span.start.col))
        .collect();
    (body, errors)
}

#[test]
fn recovery_reports_every_bad_statement() {
    let (body, errors) = recover("x = \ny = 2\nz = (\nprint(y)\n");
    assert_eq!(
        errors,
        [
            (
                "expected an expression, found end of line".to_string(),
                1,
                5
            ),
            (
                "expected an expression, found end of line".to_string(),
                3,
                6
            ),
        ]
    );
    // the good statements are kept
    assert_eq!(body.len(), 2);
}

/// Returns the kinds of the statements in a function body, such as `["Assign", "Return"]`.
fn body_kinds(stmt: &Stmt) -> Vec<&'static str> {
    let StmtKind::FunctionDef { body, .. } = &stmt.kind else {
        panic!("expected function definition");
    };
    body.iter()
        .map(|s| match s.kind {
            StmtKind::Assign { .. } => "Assign",
            StmtKind::Return { .. } => "Return",
            StmtKind::If { .. } => "If",
            StmtKind::Expr { .. } => "Expr",
            _ => "other",
        })
        .collect()
}

#[test]
fn recovery_keeps_a_block_with_a_bad_statement() {
    let (body, errors) = recover("fn f();\n    x = \n    ret 1\ng = 2\n");
    assert_eq!(
        errors,
        [(
            "expected an expression, found end of line".to_string(),
            2,
            9
        )]
    );
    assert_eq!(body.len(), 2);
    assert_eq!(body_kinds(&body[0]), ["Return"]);
}

#[test]
fn recovery_reports_every_bad_statement_in_a_block() {
    let (body, errors) = recover("fn f();\n    x = \n    y = 1\n    z = (\n    ret y\n");
    let lines: Vec<usize> = errors.iter().map(|(_, line, _)| *line).collect();
    assert_eq!(lines, [2, 4]);
    assert_eq!(body_kinds(&body[0]), ["Assign", "Return"]);
}

#[test]
fn recovery_skips_a_bad_header_with_its_block() {
    let source = "fn f(c);\n    if c\n        print(1)\n        print(2)\n    ret 2\n";
    let (body, errors) = recover(source);
    assert_eq!(
        errors,
        [("expected ';', found end of line".to_string(), 2, 9)]
    );
    assert_eq!(body_kinds(&body[0]), ["Return"]);
}

#[test]
fn recovery_works_in_nested_blocks() {
    let source = "fn f(c);\n    if c;\n        x = \n        ret 1\n    ret 2\n";
    let (body, errors) = recover(source);
    assert_eq!(errors.len(), 1);
    assert_eq!(body_kinds(&body[0]), ["If", "Return"]);
}

#[test]
fn a_block_of_only_bad_statements_is_empty() {
    let (body, errors) = recover("fn f();\n    x = \ny = 1\n");
    assert_eq!(errors.len(), 1);
    assert_eq!(body.len(), 2);
    assert_eq!(body_kinds(&body[0]), Vec::<&str>::new());
}

#[test]
fn recovery_skips_an_unexpected_indent() {
    let (body, errors) = recover("x = 1\n    y = 2\nz = 3\n");
    assert_eq!(errors, [("unexpected indent".to_string(), 2, 1)]);
    assert_eq!(body.len(), 2);
}

#[test]
fn recovery_of_a_valid_program_has_no_errors() {
    let (body, errors) = recover("fn f();\n    ret 1\nprint(f())\n");
    assert_eq!(errors, []);
    assert_eq!(body.len(), 2);
}

/// Builds the call `value.attr(args)`.
fn method_call(value: Box<Expr>, attr: &str, args: Vec<Expr>) -> Expr {
    ExprKind::Call {
        func: Box::new(
            ExprKind::Attribute {
                value,
                attr: attr.to_string(),
                ctx: ExprContext::Load,
            }
            .into(),
        ),
        args,
        keywords: vec![],
    }
    .into()
}

#[test]
fn method_calls_parse_as_calls_of_attributes() {
    assert_eq!(
        expr_of("xs.append(1)\n"),
        method_call(name("xs"), "append", vec![*int(1)])
    );
    assert_eq!(
        expr_of("xs.len()\n"),
        method_call(name("xs"), "len", vec![])
    );
}

#[test]
fn method_calls_chain_after_subscripts_and_calls() {
    assert_eq!(
        expr_of("grid[0].len()\n"),
        method_call(
            subscript(name("grid"), int(0), ExprContext::Load),
            "len",
            vec![]
        )
    );
    let f_call = ExprKind::Call {
        func: name("f"),
        args: vec![],
        keywords: vec![],
    };
    assert_eq!(
        expr_of("f().len()\n"),
        method_call(Box::new(f_call.into()), "len", vec![])
    );
}

#[test]
fn method_calls_span_their_source_text() {
    let Mod::Module { body } =
        parse_within("xs.append(1)\n".to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected expression statement");
    };
    assert_eq!(value.span, span(1, 1, 1, 13));
    let ExprKind::Call { func, .. } = &value.kind else {
        panic!("expected call");
    };
    assert_eq!(func.span, span(1, 1, 1, 10));
}

#[test]
fn method_calls_work_as_subscript_targets() {
    let Mod::Module { body } =
        parse_within("f().x[0] = 1\n".to_string(), Duration::from_secs(5)).unwrap();
    assert!(matches!(body[0].kind, StmtKind::Assign { .. }));
}

#[test]
fn attribute_needs_a_name() {
    assert_eq!(
        error_of("xs.\n"),
        (
            "expected a method name, found end of line".to_string(),
            1,
            4
        )
    );
}

#[test]
fn augmented_assignments_parse() {
    let parsed = |source: &str| {
        let Mod::Module { mut body } =
            without_spans(parse_within(source.to_string(), Duration::from_secs(5)).unwrap());
        body.remove(0).kind
    };
    let store = |id: &str| -> Box<Expr> {
        Box::new(
            ExprKind::Name {
                id: id.to_string(),
                ctx: ExprContext::Store,
            }
            .into(),
        )
    };
    for (text, op) in [
        ("+=", Operator::Add),
        ("-=", Operator::Subtract),
        ("*=", Operator::Multiply),
        ("/=", Operator::Divide),
        ("%=", Operator::Modulo),
        ("**=", Operator::Power),
    ] {
        assert_eq!(
            parsed(&format!("x {text} y + 1\n")),
            StmtKind::AugAssign {
                target: store("x"),
                op,
                value: binop(name("y"), Operator::Add, int(1)),
            }
        );
    }
    assert_eq!(
        parsed("a[i][j] **= 2\n"),
        StmtKind::AugAssign {
            target: subscript(
                subscript(name("a"), name("i"), ExprContext::Load),
                name("j"),
                ExprContext::Store
            ),
            op: Operator::Power,
            value: int(2),
        }
    );
}

#[test]
fn an_augmented_assignment_spans_its_statement() {
    let Mod::Module { body } =
        parse_within("total += 10\n".to_string(), Duration::from_secs(5)).unwrap();
    assert_eq!(body[0].span, Span::new(Pos::new(1, 1), Pos::new(1, 12)));
}

#[test]
fn augmented_assignments_do_not_chain() {
    let at = |source: &str| {
        let (_, line, col) = error_of(source);
        (line, col)
    };
    assert_eq!(at("a += b += 1\n"), (1, 8));
    assert_eq!(at("a = b += 1\n"), (1, 7));
    assert_eq!(
        error_of("x +=\n"),
        (
            "expected an expression, found end of line".to_string(),
            1,
            5
        )
    );
}

#[test]
fn a_field_is_an_assignment_target() {
    let field = |value: Box<Expr>, attr: &str| -> Expr {
        ExprKind::Attribute {
            value,
            attr: attr.to_string(),
            ctx: ExprContext::Store,
        }
        .into()
    };
    assert_eq!(
        parse_ok("p.x = 1\n"),
        Mod::Module {
            body: vec![
                StmtKind::Assign {
                    targets: vec![field(name("p"), "x")],
                    value: int(1),
                }
                .into()
            ]
        }
    );
    let element = Box::new(
        ExprKind::Subscript {
            value: name("ps"),
            slice: int(0),
            ctx: ExprContext::Load,
        }
        .into(),
    );
    assert_eq!(
        parse_ok("ps[0].x += 2\n"),
        Mod::Module {
            body: vec![
                StmtKind::AugAssign {
                    target: Box::new(field(element, "x")),
                    op: Operator::Add,
                    value: int(2),
                }
                .into()
            ]
        }
    );
    // a call cannot be assigned to
    let (message, line, col) = error_of("p.f() = 1\n");
    assert_eq!((line, col), (1, 7), "{message}");
}

/// Builds the boxed expression `left op right`, for expected trees.
fn binop(left: Box<Expr>, op: Operator, right: Box<Expr>) -> Box<Expr> {
    Box::new(ExprKind::BinOp { op, left, right }.into())
}

/// Builds the boxed expression `-operand`, for expected trees.
fn negate(operand: Box<Expr>) -> Box<Expr> {
    Box::new(
        ExprKind::UnaryOp {
            op: UnaryOp::UnarySub,
            operand,
        }
        .into(),
    )
}

#[test]
fn modulo_binds_like_multiplication() {
    assert_eq!(
        expr_of("a % b * c + d\n"),
        *binop(
            binop(
                binop(name("a"), Operator::Modulo, name("b")),
                Operator::Multiply,
                name("c")
            ),
            Operator::Add,
            name("d")
        )
    );
}

#[test]
fn power_is_right_associative() {
    assert_eq!(
        expr_of("2 ** 3 ** 2\n"),
        *binop(
            int(2),
            Operator::Power,
            binop(int(3), Operator::Power, int(2))
        )
    );
}

#[test]
fn power_binds_tighter_than_unary_minus_on_its_left() {
    assert_eq!(
        expr_of("-2 ** 2\n"),
        *negate(binop(int(2), Operator::Power, int(2)))
    );
}

#[test]
fn a_power_exponent_can_be_negated() {
    assert_eq!(
        expr_of("2 ** -1\n"),
        *binop(int(2), Operator::Power, negate(int(1)))
    );
}

#[test]
fn power_binds_tighter_than_multiplication() {
    assert_eq!(
        expr_of("a * b ** c\n"),
        *binop(
            name("a"),
            Operator::Multiply,
            binop(name("b"), Operator::Power, name("c"))
        )
    );
}

#[test]
fn a_power_applies_to_a_whole_primary() {
    let ExprKind::BinOp { op, left, .. } = expr_of("xs[0] ** 2\n").kind else {
        panic!("expected a power");
    };
    assert_eq!(op, Operator::Power);
    assert!(matches!(left.kind, ExprKind::Subscript { .. }));
}

/// Parses `source` and returns its statements with spans stripped.
fn stmts_of(source: &str) -> Vec<Stmt> {
    let Mod::Module { body } =
        without_spans(parse_within(source.to_string(), Duration::from_secs(5)).unwrap());
    body
}

/// Builds a `use` path from its dotted names, with empty spans.
fn use_path(names: &[&str]) -> Vec<(String, Span)> {
    names
        .iter()
        .map(|n| (n.to_string(), Span::default()))
        .collect()
}

#[test]
fn use_of_a_module() {
    assert_eq!(
        stmts_of("use geometry.shapes\n"),
        [StmtKind::Use {
            path: use_path(&["geometry", "shapes"]),
            alias: None,
        }
        .into()]
    );
}

#[test]
fn use_of_a_single_name() {
    assert_eq!(
        stmts_of("use util\n"),
        [StmtKind::Use {
            path: use_path(&["util"]),
            alias: None,
        }
        .into()]
    );
}

#[test]
fn use_with_an_alias() {
    assert_eq!(
        stmts_of("use util.pad as lpad\n"),
        [StmtKind::Use {
            path: use_path(&["util", "pad"]),
            alias: Some(("lpad".to_string(), Span::default())),
        }
        .into()]
    );
}

#[test]
fn use_spans_each_name() {
    let Mod::Module { body } =
        parse_within("use a.bc as d\n".to_string(), Duration::from_secs(5)).unwrap();
    assert_eq!(body[0].span, span(1, 1, 1, 14));
    let StmtKind::Use { path, alias } = &body[0].kind else {
        panic!("expected use");
    };
    assert_eq!(path[0].1, span(1, 5, 1, 6));
    assert_eq!(path[1].1, span(1, 7, 1, 9));
    assert_eq!(alias.as_ref().unwrap().1, span(1, 13, 1, 14));
}

#[test]
fn use_needs_a_name_after_each_dot() {
    assert_eq!(
        error_of("use a.\n"),
        (
            "expected a module name, found end of line".to_string(),
            1,
            7
        )
    );
}

#[test]
fn use_needs_a_name_after_as() {
    assert_eq!(
        error_of("use a as\n"),
        ("expected a name, found end of line".to_string(), 1, 9)
    );
}

#[test]
fn pub_marks_a_function_public() {
    let body = stmts_of("pub fn f(); ret 1\nfn g(); ret 2\n");
    let StmtKind::FunctionDef { public, .. } = &body[0].kind else {
        panic!("expected function definition");
    };
    assert!(public);
    let StmtKind::FunctionDef { public, .. } = &body[1].kind else {
        panic!("expected function definition");
    };
    assert!(!public);
}

#[test]
fn pub_spans_from_the_keyword() {
    let Mod::Module { body } =
        parse_within("pub fn f(); ret 1\n".to_string(), Duration::from_secs(5)).unwrap();
    assert_eq!(body[0].span, span(1, 1, 1, 18));
}

#[test]
fn pub_must_come_before_def() {
    assert_eq!(
        error_of("pub x = 1\n"),
        (
            "expected 'fn', 'enum' or 'struct', found 'x'".to_string(),
            1,
            5
        )
    );
}

fn tuple(elts: Vec<Expr>, ctx: ExprContext) -> Expr {
    ExprKind::Tuple { elts, ctx }.into()
}

fn load(id: &str) -> Expr {
    ExprKind::Name {
        id: id.to_string(),
        ctx: ExprContext::Load,
    }
    .into()
}

fn stored(id: &str) -> Expr {
    ExprKind::Name {
        id: id.to_string(),
        ctx: ExprContext::Store,
    }
    .into()
}

#[test]
fn tuple_literals_parse() {
    use ExprContext::Load;
    assert_eq!(expr_of("(1, 2)\n"), tuple(vec![*int(1), *int(2)], Load));
    assert_eq!(
        expr_of("(1, x, 3,)\n"),
        tuple(vec![*int(1), *name("x"), *int(3)], Load)
    );
    assert_eq!(
        expr_of("(1, (x, 2))\n"),
        tuple(vec![*int(1), tuple(vec![*name("x"), *int(2)], Load)], Load)
    );
    // one value in parentheses is still a group
    assert_eq!(expr_of("(x)\n"), *name("x"));
}

#[test]
fn tuples_span_their_parentheses() {
    let Mod::Module { body } =
        parse_within("(1, 2)\n".to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected expression statement");
    };
    assert_eq!(value.span, span(1, 1, 1, 7));
}

#[test]
fn unpacking_targets_parse() {
    use ExprContext::Store;
    assert_eq!(
        assign_target_of("q, r = t\n"),
        tuple(vec![stored("q"), stored("r")], Store)
    );
    assert_eq!(
        assign_target_of("(a, b,) = t\n"),
        tuple(vec![stored("a"), stored("b")], Store)
    );
    assert_eq!(
        assign_target_of("a, (b, c) = t\n"),
        tuple(
            vec![stored("a"), tuple(vec![stored("b"), stored("c")], Store)],
            Store
        )
    );
    assert_eq!(assign_target_of("(a) = 1\n"), stored("a"));
}

#[test]
fn for_loops_unpack_into_names() {
    let Mod::Module { body } = without_spans(
        parse_within(
            "for name, score in pairs;\n    print(name)\n".to_string(),
            Duration::from_secs(5),
        )
        .unwrap(),
    );
    let StmtKind::For { target, .. } = &body[0].kind else {
        panic!("expected for loop");
    };
    assert_eq!(
        **target,
        tuple(vec![stored("name"), stored("score")], ExprContext::Store)
    );
}

#[test]
fn tuple_types_nest() {
    assert_eq!(
        signature_of("fn f(t: tuple[int, list[tuple[str, float]]]) -> tuple[int, str,]; ret t\n"),
        (
            vec![Some("tuple[int, list[tuple[str, float]]]".to_string())],
            Some("tuple[int, str]".to_string())
        )
    );
}

#[test]
fn values_separated_by_commas_need_parentheses() {
    let message =
        "values separated by commas need parentheses to make a tuple, as in '(a, b)'".to_string();
    assert_eq!(error_of("x = 1, 2\n"), (message.clone(), 1, 5));
    assert_eq!(error_of("x = a, b,\n"), (message.clone(), 1, 5));
    assert_eq!(error_of("a, b\n"), (message.clone(), 1, 1));
    assert_eq!(error_of("q, r = a, b\n"), (message.clone(), 1, 8));
    assert_eq!(error_of("for x in a, b;\n    print(x)\n"), (message, 1, 10));
}

#[test]
fn returning_several_values_needs_a_tuple() {
    assert_eq!(
        error_of("fn f() -> tuple[int, int];\n    ret 1, 2\n"),
        (
            "to return several values, return a tuple, as in 'ret (a, b)'".to_string(),
            2,
            9
        )
    );
}

#[test]
fn a_tuple_needs_two_values() {
    assert_eq!(
        error_of("x = (1,)\n"),
        (
            "a tuple needs at least two values; to group one value, leave out the comma"
                .to_string(),
            1,
            5
        )
    );
    assert_eq!(
        error_of("x = ()\n"),
        (
            "'()' is not a value; a tuple needs at least two values, as in '(a, b)'".to_string(),
            1,
            5
        )
    );
    assert_eq!(
        error_of("a, = t\n"),
        (
            "unpacking needs at least two names; to assign one value, leave out the comma"
                .to_string(),
            1,
            1
        )
    );
    assert_eq!(
        error_of("fn f(t: tuple[int]);\n    print(t)\n"),
        (
            "a tuple type needs at least two types, as in 'tuple[int, str]'".to_string(),
            1,
            9
        )
    );
}

#[test]
fn a_hint_gives_way_to_an_error_further_on() {
    assert_eq!(
        error_of("a, b = c d\n"),
        ("expected end of line, found 'd'".to_string(), 1, 10)
    );
    assert_eq!(
        error_of("x = (1, 2\n"),
        ("expected ')', found end of line".to_string(), 1, 10)
    );
}

#[test]
fn nested_tuples_parse_in_linear_time() {
    let depth = 40;
    let value = format!("{}1, 2{}", "(".repeat(depth), ", 3)".repeat(depth));
    assert!(parse_within(format!("x = {value}\n"), Duration::from_secs(5)).is_ok());
    let groups = format!("{}x{}", "(".repeat(depth), ")".repeat(depth));
    assert!(parse_within(format!("{groups} = 1\n"), Duration::from_secs(5)).is_ok());
    assert!(parse_within(format!("y = {groups}\n"), Duration::from_secs(5)).is_ok());
    let targets = format!("{}a, b{}", "(".repeat(depth), ", c)".repeat(depth));
    assert!(parse_within(format!("{targets} = t\n"), Duration::from_secs(5)).is_ok());
}

/// Builds a type written as `name` with no type arguments, such as `float`, for expected trees.
fn named_type(name: &str) -> TypeExpr {
    let kind = match name {
        "int" => TypeExprKind::Int,
        "float" => TypeExprKind::Float,
        "bool" => TypeExprKind::Bool,
        "str" => TypeExprKind::Str,
        _ => TypeExprKind::Named {
            name: name.to_string(),
            args: vec![],
        },
    };
    TypeExpr {
        kind,
        span: Span::default(),
    }
}

fn variant(name: &str, fields: &[&str]) -> Variant {
    Variant {
        name: name.to_string(),
        span: Span::default(),
        fields: fields.iter().map(|f| named_type(f)).collect(),
    }
}

#[test]
fn an_enum_lists_its_variants() {
    let module = Parser::new(
        &Lexer::new("pub enum Shape;\n    Circle(float)\n    Rect(float, float,)\n    Empty\n    Group(list[Shape])\n")
            .lex()
            .unwrap(),
    )
    .parse()
    .unwrap();
    let Mod::Module { body } = without_spans(module);
    assert_eq!(
        body,
        vec![
            StmtKind::EnumDef {
                name: "Shape".to_string(),
                name_span: Span::default(),
                public: true,
                variants: vec![
                    variant("Circle", &["float"]),
                    variant("Rect", &["float", "float"]),
                    variant("Empty", &[]),
                    Variant {
                        name: "Group".to_string(),
                        span: Span::default(),
                        fields: vec![TypeExpr {
                            kind: TypeExprKind::List(Box::new(named_type("Shape"))),
                            span: Span::default(),
                        }],
                    },
                ],
            }
            .into()
        ]
    );
}

#[test]
fn an_enum_needs_a_block_of_variants() {
    assert_eq!(
        error_of("enum Shape;\nx = 1\n"),
        ("expected an indented block, found 'x'".to_string(), 2, 1)
    );
    assert_eq!(
        error_of("enum Shape;\n    Circle(\n"),
        ("expected a type, found end of line".to_string(), 2, 12)
    );
    assert_eq!(
        error_of("enum Shape;\n    Circle()\n"),
        (
            "a variant without values is written without parentheses, as in 'Circle'".to_string(),
            2,
            5
        )
    );
}

#[test]
fn named_types_take_type_arguments() {
    assert_eq!(
        signature_of(
            "fn f(a: option[int], b: Shape, c: shapes.Shape) -> option[list[Shape]]; ret none\n"
        ),
        (
            ["option[int]", "Shape", "shapes.Shape"]
                .map(|t| Some(t.to_string()))
                .to_vec(),
            Some("option[list[Shape]]".to_string())
        )
    );
}

#[test]
fn none_is_not_a_type() {
    assert_eq!(
        error_of("fn f(a: none); ret\n"),
        (
            "'none' is not a type: a function without '->' returns no value, and 'option[T]' holds a value or none"
                .to_string(),
            1,
            9
        )
    );
}

fn pattern(kind: PatternKind) -> Pattern {
    Pattern {
        kind,
        span: Span::default(),
    }
}

fn variant_pattern(path: &[&str], fields: Vec<Pattern>) -> Pattern {
    pattern(PatternKind::Variant {
        path: path
            .iter()
            .map(|name| (name.to_string(), Span::default()))
            .collect(),
        fields,
    })
}

#[test]
fn match_runs_the_first_case_that_fits() {
    let source = "match s;\n    case Shape.Rect(w, _);\n        print(w)\n    case some(Tree.Leaf(n));\n        print(n)\n    case none; print(0)\n    case other;\n        break\n";
    let module = Parser::new(&Lexer::new(source).lex().unwrap())
        .parse()
        .unwrap();
    let Mod::Module { body } = without_spans(module);
    let print = |name: &str| -> Stmt {
        StmtKind::Expr {
            value: Box::new(
                ExprKind::Call {
                    func: Box::new(load("print")),
                    args: vec![load(name)],
                    keywords: vec![],
                }
                .into(),
            ),
        }
        .into()
    };
    let zero: Stmt = StmtKind::Expr {
        value: Box::new(
            ExprKind::Call {
                func: Box::new(load("print")),
                args: vec![
                    ExprKind::Constant {
                        value: Box::new(Constant::Int(0)),
                        kind: None,
                    }
                    .into(),
                ],
                keywords: vec![],
            }
            .into(),
        ),
    }
    .into();
    assert_eq!(
        body,
        vec![
            StmtKind::Match {
                subject: Box::new(load("s")),
                cases: vec![
                    MatchCase {
                        pattern: variant_pattern(
                            &["Shape", "Rect"],
                            vec![
                                pattern(PatternKind::Name("w".to_string())),
                                pattern(PatternKind::Wildcard),
                            ]
                        ),
                        body: vec![print("w")],
                    },
                    MatchCase {
                        pattern: variant_pattern(
                            &["some"],
                            vec![variant_pattern(
                                &["Tree", "Leaf"],
                                vec![pattern(PatternKind::Name("n".to_string()))]
                            )]
                        ),
                        body: vec![print("n")],
                    },
                    MatchCase {
                        pattern: variant_pattern(&["none"], vec![]),
                        body: vec![zero],
                    },
                    MatchCase {
                        pattern: pattern(PatternKind::Name("other".to_string())),
                        body: vec![StmtKind::Break.into()],
                    },
                ],
            }
            .into()
        ]
    );
}

#[test]
fn patterns_span_their_text() {
    let source = "match s;\n    case Shape.Rect(w, _);\n        print(w)\n";
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    let StmtKind::Match { cases, .. } = &body[0].kind else {
        panic!("expected match");
    };
    let pattern = &cases[0].pattern;
    assert_eq!(pattern.span, span(2, 10, 2, 26));
    assert_eq!(pattern.to_string(), "Shape.Rect(w, _)");
    let PatternKind::Variant { path, fields } = &pattern.kind else {
        panic!("expected a variant pattern");
    };
    assert_eq!(path[1].1, span(2, 16, 2, 20));
    assert_eq!(fields[0].span, span(2, 21, 2, 22));
}

#[test]
fn match_needs_cases() {
    assert_eq!(
        error_of("match s;\n    print(s)\n"),
        ("expected 'case', found 'print'".to_string(), 2, 5)
    );
    assert_eq!(
        error_of("match s;\n    case 1;\n        print(s)\n"),
        (
            "a pattern is a variant such as 'Shape.Circle(r)', 'some(x)', or 'none', a name, or '_'"
                .to_string(),
            2,
            10
        )
    );
    assert_eq!(
        error_of("match s;\n    case some();\n        print(s)\n"),
        ("expected a pattern, found ')'".to_string(), 2, 15)
    );
}

#[test]
fn a_bad_case_is_skipped_with_its_match() {
    let tokens = Lexer::new("match s;\n    case 1;\n        print(s)\nx = 1\n")
        .lex()
        .unwrap();
    let (module, diagnostics) = Parser::new(&tokens).parse_recovering();
    assert_eq!(diagnostics.len(), 1);
    let Mod::Module { body } = module;
    assert_eq!(body.len(), 1);
    assert!(matches!(body[0].kind, StmtKind::Assign { .. }));
}

/// Parses `source`, which must succeed, without spans, for comparing with expected trees.
fn parse_ok(source: &str) -> Mod {
    without_spans(parse_within(source.to_string(), Duration::from_secs(5)).unwrap())
}

#[test]
fn a_struct_definition_holds_its_fields() {
    let field = |name: &str, kind: TypeExprKind| Field {
        name: name.to_string(),
        span: Span::default(),
        annotation: TypeExpr {
            kind,
            span: Span::default(),
        },
    };
    let list_of_int = TypeExprKind::List(Box::new(TypeExpr {
        kind: TypeExprKind::Int,
        span: Span::default(),
    }));
    assert_eq!(
        parse_ok("pub struct Point;\n    x: float\n    ys: list[int]\n"),
        Mod::Module {
            body: vec![
                StmtKind::StructDef {
                    name: "Point".to_string(),
                    name_span: Span::default(),
                    public: true,
                    fields: vec![field("x", TypeExprKind::Float), field("ys", list_of_int)],
                }
                .into()
            ]
        }
    );
}

#[test]
fn a_struct_needs_typed_fields() {
    assert_eq!(
        error_of("struct Point;\n    x\n"),
        ("expected ':', found end of line".to_string(), 2, 6)
    );
    assert_eq!(
        error_of("struct Point;\n    x = 1\n"),
        ("expected ':', found '='".to_string(), 2, 7)
    );
    assert_eq!(
        error_of("struct Point;\nx = 1\n"),
        ("expected an indented block, found 'x'".to_string(), 2, 1)
    );
}

#[test]
fn named_values_follow_the_others_in_a_call() {
    let keyword = |arg: &str, value: Box<Expr>| Keyword {
        arg: arg.to_string(),
        arg_span: Span::default(),
        value: *value,
    };
    let call = |args: Vec<Expr>, keywords: Vec<Keyword>| -> Mod {
        Mod::Module {
            body: vec![
                StmtKind::Expr {
                    value: Box::new(
                        ExprKind::Call {
                            func: name("f"),
                            args,
                            keywords,
                        }
                        .into(),
                    ),
                }
                .into(),
            ],
        }
    };
    assert_eq!(
        parse_ok("f(x=1, y=2,)\n"),
        call(vec![], vec![keyword("x", int(1)), keyword("y", int(2))])
    );
    assert_eq!(
        parse_ok("f(1, y=2)\n"),
        call(vec![*int(1)], vec![keyword("y", int(2))])
    );
    // `==` compares, so this is one value without a name
    assert!(matches!(
        &parse_ok("f(x == 1)\n"),
        Mod::Module { body } if matches!(&body[0].kind, StmtKind::Expr { value }
            if matches!(&value.kind, ExprKind::Call { args, keywords, .. }
                if args.len() == 1 && keywords.is_empty()))
    ));
    assert_eq!(
        error_of("f(x=1, 2)\n"),
        (
            "a value without a name cannot follow a named one, as in 'x=1'".to_string(),
            1,
            8
        )
    );
}
