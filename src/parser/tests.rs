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
            name, args, body, ..
        } => StmtKind::FunctionDef {
            name,
            name_span: Span::default(),
            args: Arguments {
                args: args
                    .args
                    .into_iter()
                    .map(|a| Arg {
                        arg: a.arg,
                        span: Span::default(),
                    })
                    .collect(),
            },
            body: strip_all(body),
        },
        StmtKind::Return { value } => StmtKind::Return {
            value: value.map(|v| Box::new(strip_expr(*v))),
        },
        StmtKind::Delete { targets } => StmtKind::Delete {
            targets: targets.into_iter().map(strip_expr).collect(),
        },
        StmtKind::Assign { targets, value } => StmtKind::Assign {
            targets: targets.into_iter().map(strip_expr).collect(),
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
        kind @ (StmtKind::Break | StmtKind::Continue) => kind,
    };
    kind.into()
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
        ExprKind::Call { func, args } => ExprKind::Call {
            func: strip_box(func),
            args: strip_all(args),
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
/// def testing();
///     ret 52
/// testing()
/// ```
#[test]
fn simple_function_no_args() {
    let tokens = vec![
        tok(TokenType::Keyword("def".to_string()), 1, 1),
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
                    args: Arguments { args: vec![] },
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
                            args: vec![]
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
/// def testing(a);
///     ret a + 2
/// testing(4)
/// ```
#[test]
fn simple_function_one_arg() {
    let tokens = vec![
        tok(TokenType::Keyword("def".to_string()), 1, 1),
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
                    args: Arguments {
                        args: vec![Arg {
                            arg: "a".to_string(),
                            span: Span::default(),
                        }]
                    },
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
                            ]
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
/// def testing(a, b, c);
///     ret b
/// testing(1, 2, 3)
/// ```
#[test]
fn simple_function_multiple_args() {
    let tokens = vec![
        tok(TokenType::Keyword("def".to_string()), 1, 1),
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
                    args: Arguments {
                        args: vec![
                            Arg {
                                arg: "a".to_string(),
                                span: Span::default(),
                            },
                            Arg {
                                arg: "b".to_string(),
                                span: Span::default(),
                            },
                            Arg {
                                arg: "c".to_string(),
                                span: Span::default(),
                            }
                        ]
                    },
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
                            ]
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
    let source = "def f(a, bc);\n    ret a\n";
    let Mod::Module { body } = parse_within(source.to_string(), Duration::from_secs(5)).unwrap();
    // the block ends at its last token, not at the dedent after it
    assert_eq!(body[0].span, span(1, 1, 2, 10));
    let StmtKind::FunctionDef {
        name_span, args, ..
    } = &body[0].kind
    else {
        panic!("expected function definition");
    };
    assert_eq!(*name_span, span(1, 5, 1, 6));
    assert_eq!(args.args[1].span, span(1, 10, 1, 12));
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
        error_of("def f(;\n    ret 1\n"),
        (
            "expected a parameter name or ')', found ';'".to_string(),
            1,
            7
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
        error_of("def f(a, );\n    ret a\n"),
        ("expected a parameter name, found ')'".to_string(), 1, 10)
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
            args: vec![*int(3)]
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
    let (body, errors) = recover("def f();\n    x = \n    ret 1\ng = 2\n");
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
    let (body, errors) = recover("def f();\n    x = \n    y = 1\n    z = (\n    ret y\n");
    let lines: Vec<usize> = errors.iter().map(|(_, line, _)| *line).collect();
    assert_eq!(lines, [2, 4]);
    assert_eq!(body_kinds(&body[0]), ["Assign", "Return"]);
}

#[test]
fn recovery_skips_a_bad_header_with_its_block() {
    let source = "def f(c);\n    if c\n        print(1)\n        print(2)\n    ret 2\n";
    let (body, errors) = recover(source);
    assert_eq!(
        errors,
        [("expected ';', found end of line".to_string(), 2, 9)]
    );
    assert_eq!(body_kinds(&body[0]), ["Return"]);
}

#[test]
fn recovery_works_in_nested_blocks() {
    let source = "def f(c);\n    if c;\n        x = \n        ret 1\n    ret 2\n";
    let (body, errors) = recover(source);
    assert_eq!(errors.len(), 1);
    assert_eq!(body_kinds(&body[0]), ["If", "Return"]);
}

#[test]
fn a_block_of_only_bad_statements_is_empty() {
    let (body, errors) = recover("def f();\n    x = \ny = 1\n");
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
    let (body, errors) = recover("def f();\n    ret 1\nprint(f())\n");
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
fn attribute_is_not_an_assignment_target() {
    let (message, line, col) = error_of("xs.len = 1\n");
    assert_eq!((line, col), (1, 8), "{message}");
}
