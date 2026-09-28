use super::{MAX_DEPTH, Parser};
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

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
        Token {
            r#type: TokenType::Name("x".to_string()),
            line: 1,
            col: 1,
        },
        Token {
            r#type: TokenType::Operator("=".to_string()),
            line: 1,
            col: 3,
        },
        Token {
            r#type: TokenType::Number(42),
            line: 1,
            col: 5,
        },
        Token {
            r#type: TokenType::Newline,
            line: 1,
            col: 7,
        },
        Token {
            r#type: TokenType::Name("y".to_string()),
            line: 2,
            col: 1,
        },
        Token {
            r#type: TokenType::Operator("=".to_string()),
            line: 2,
            col: 3,
        },
        Token {
            r#type: TokenType::Name("x".to_string()),
            line: 2,
            col: 5,
        },
        Token {
            r#type: TokenType::Operator("+".to_string()),
            line: 2,
            col: 7,
        },
        Token {
            r#type: TokenType::Number(8),
            line: 2,
            col: 9,
        },
        Token {
            r#type: TokenType::Newline,
            line: 2,
            col: 10,
        },
        Token {
            r#type: TokenType::Keyword("ret".to_string()),
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("y".to_string()),
            line: 3,
            col: 5,
        },
        Token {
            r#type: TokenType::Newline,
            line: 3,
            col: 6,
        },
        Token {
            r#type: TokenType::Eof,
            line: 4,
            col: 1,
        },
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast,
        Ok(Mod::Module {
            body: vec![
                Stmt::Assign {
                    targets: vec![Expr::Name {
                        id: "x".to_string(),
                        ctx: ExprContext::Store
                    }],
                    value: Box::new(Expr::Constant {
                        value: Box::new(Constant::Int(42)),
                        kind: None
                    })
                },
                Stmt::Assign {
                    targets: vec![Expr::Name {
                        id: "y".to_string(),
                        ctx: ExprContext::Store
                    }],
                    value: Box::new(Expr::BinOp {
                        op: Operator::Add,
                        left: Box::new(Expr::Name {
                            id: "x".to_string(),
                            ctx: ExprContext::Load
                        }),
                        right: Box::new(Expr::Constant {
                            value: Box::new(Constant::Int(8)),
                            kind: None
                        })
                    })
                },
                Stmt::Return {
                    value: Some(Box::new(Expr::Name {
                        id: "y".to_string(),
                        ctx: ExprContext::Load
                    }))
                }
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
        Token {
            r#type: TokenType::Keyword("def".to_string()),
            line: 1,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 1,
            col: 5,
        },
        Token {
            r#type: TokenType::LParen,
            line: 1,
            col: 12,
        },
        Token {
            r#type: TokenType::RParen,
            line: 1,
            col: 13,
        },
        Token {
            r#type: TokenType::Semi,
            line: 1,
            col: 14,
        },
        Token {
            r#type: TokenType::Newline,
            line: 1,
            col: 15,
        },
        Token {
            r#type: TokenType::Indent,
            line: 2,
            col: 1,
        },
        Token {
            r#type: TokenType::Keyword("ret".to_string()),
            line: 2,
            col: 5,
        },
        Token {
            r#type: TokenType::Number(52),
            line: 2,
            col: 9,
        },
        Token {
            r#type: TokenType::Newline,
            line: 2,
            col: 11,
        },
        Token {
            r#type: TokenType::Dedent,
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::LParen,
            line: 3,
            col: 8,
        },
        Token {
            r#type: TokenType::RParen,
            line: 3,
            col: 9,
        },
        Token {
            r#type: TokenType::Newline,
            line: 3,
            col: 10,
        },
        Token {
            r#type: TokenType::Eof,
            line: 4,
            col: 1,
        },
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast,
        Ok(Mod::Module {
            body: vec![
                Stmt::FunctionDef {
                    name: "testing".to_string(),
                    args: Arguments { args: vec![] },
                    body: vec![Stmt::Return {
                        value: Some(Box::new(Expr::Constant {
                            value: Box::new(Constant::Int(52)),
                            kind: None
                        }))
                    }]
                },
                Stmt::Expr {
                    value: Box::new(Expr::Call {
                        func: Box::new(Expr::Name {
                            id: "testing".to_string(),
                            ctx: ExprContext::Load
                        }),
                        args: vec![]
                    })
                },
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
        Token {
            r#type: TokenType::Keyword("def".to_string()),
            line: 1,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 1,
            col: 5,
        },
        Token {
            r#type: TokenType::LParen,
            line: 1,
            col: 12,
        },
        Token {
            r#type: TokenType::Name("a".to_string()),
            line: 1,
            col: 13,
        },
        Token {
            r#type: TokenType::RParen,
            line: 1,
            col: 14,
        },
        Token {
            r#type: TokenType::Semi,
            line: 1,
            col: 15,
        },
        Token {
            r#type: TokenType::Newline,
            line: 1,
            col: 16,
        },
        Token {
            r#type: TokenType::Indent,
            line: 2,
            col: 1,
        },
        Token {
            r#type: TokenType::Keyword("ret".to_string()),
            line: 2,
            col: 5,
        },
        Token {
            r#type: TokenType::Name("a".to_string()),
            line: 2,
            col: 9,
        },
        Token {
            r#type: TokenType::Operator("+".to_string()),
            line: 2,
            col: 11,
        },
        Token {
            r#type: TokenType::Number(2),
            line: 2,
            col: 13,
        },
        Token {
            r#type: TokenType::Newline,
            line: 2,
            col: 14,
        },
        Token {
            r#type: TokenType::Dedent,
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::LParen,
            line: 3,
            col: 8,
        },
        Token {
            r#type: TokenType::Number(4),
            line: 3,
            col: 9,
        },
        Token {
            r#type: TokenType::RParen,
            line: 3,
            col: 10,
        },
        Token {
            r#type: TokenType::Newline,
            line: 3,
            col: 11,
        },
        Token {
            r#type: TokenType::Eof,
            line: 4,
            col: 1,
        },
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast,
        Ok(Mod::Module {
            body: vec![
                Stmt::FunctionDef {
                    name: "testing".to_string(),
                    args: Arguments {
                        args: vec![Arg {
                            arg: "a".to_string()
                        }]
                    },
                    body: vec![Stmt::Return {
                        value: Some(Box::new(Expr::BinOp {
                            op: Operator::Add,
                            left: Box::new(Expr::Name {
                                id: "a".to_string(),
                                ctx: ExprContext::Load
                            }),
                            right: Box::new(Expr::Constant {
                                value: Box::new(Constant::Int(2)),
                                kind: None
                            })
                        }))
                    }]
                },
                Stmt::Expr {
                    value: Box::new(Expr::Call {
                        func: Box::new(Expr::Name {
                            id: "testing".to_string(),
                            ctx: ExprContext::Load
                        }),
                        args: vec![Expr::Constant {
                            value: Box::new(Constant::Int(4)),
                            kind: None
                        }]
                    })
                },
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
        Token {
            r#type: TokenType::Keyword("def".to_string()),
            line: 1,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 1,
            col: 5,
        },
        Token {
            r#type: TokenType::LParen,
            line: 1,
            col: 12,
        },
        Token {
            r#type: TokenType::Name("a".to_string()),
            line: 1,
            col: 13,
        },
        Token {
            r#type: TokenType::Comma,
            line: 1,
            col: 14,
        },
        Token {
            r#type: TokenType::Name("b".to_string()),
            line: 1,
            col: 16,
        },
        Token {
            r#type: TokenType::Comma,
            line: 1,
            col: 17,
        },
        Token {
            r#type: TokenType::Name("c".to_string()),
            line: 1,
            col: 19,
        },
        Token {
            r#type: TokenType::RParen,
            line: 1,
            col: 20,
        },
        Token {
            r#type: TokenType::Semi,
            line: 1,
            col: 21,
        },
        Token {
            r#type: TokenType::Newline,
            line: 1,
            col: 22,
        },
        Token {
            r#type: TokenType::Indent,
            line: 2,
            col: 1,
        },
        Token {
            r#type: TokenType::Keyword("ret".to_string()),
            line: 2,
            col: 5,
        },
        Token {
            r#type: TokenType::Name("b".to_string()),
            line: 2,
            col: 9,
        },
        Token {
            r#type: TokenType::Newline,
            line: 2,
            col: 10,
        },
        Token {
            r#type: TokenType::Dedent,
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::Name("testing".to_string()),
            line: 3,
            col: 1,
        },
        Token {
            r#type: TokenType::LParen,
            line: 3,
            col: 8,
        },
        Token {
            r#type: TokenType::Number(1),
            line: 3,
            col: 9,
        },
        Token {
            r#type: TokenType::Comma,
            line: 3,
            col: 10,
        },
        Token {
            r#type: TokenType::Number(2),
            line: 3,
            col: 12,
        },
        Token {
            r#type: TokenType::Comma,
            line: 3,
            col: 13,
        },
        Token {
            r#type: TokenType::Number(3),
            line: 3,
            col: 15,
        },
        Token {
            r#type: TokenType::RParen,
            line: 3,
            col: 16,
        },
        Token {
            r#type: TokenType::Newline,
            line: 3,
            col: 17,
        },
        Token {
            r#type: TokenType::Eof,
            line: 4,
            col: 1,
        },
    ];

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();
    assert_eq!(
        ast,
        Ok(Mod::Module {
            body: vec![
                Stmt::FunctionDef {
                    name: "testing".to_string(),
                    args: Arguments {
                        args: vec![
                            Arg {
                                arg: "a".to_string()
                            },
                            Arg {
                                arg: "b".to_string()
                            },
                            Arg {
                                arg: "c".to_string()
                            }
                        ]
                    },
                    body: vec![Stmt::Return {
                        value: Some(Box::new(Expr::Name {
                            id: "b".to_string(),
                            ctx: ExprContext::Load
                        }))
                    }]
                },
                Stmt::Expr {
                    value: Box::new(Expr::Call {
                        func: Box::new(Expr::Name {
                            id: "testing".to_string(),
                            ctx: ExprContext::Load
                        }),
                        args: vec![
                            Expr::Constant {
                                value: Box::new(Constant::Int(1)),
                                kind: None
                            },
                            Expr::Constant {
                                value: Box::new(Constant::Int(2)),
                                kind: None
                            },
                            Expr::Constant {
                                value: Box::new(Constant::Int(3)),
                                kind: None
                            }
                        ]
                    })
                },
            ]
        })
    );
}

/// Lexes and parses `source` on a separate thread, failing the test if parsing takes longer than
/// `limit`.
///
/// For example, `parse_within("x = 1\n".to_string(), Duration::from_secs(5))` returns the parsed
/// module.
fn parse_within(source: String, limit: Duration) -> Result<Mod, ParserError> {
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let tokens = Lexer::new(&source).lex().unwrap();
        let _ = tx.send(Parser::new(&tokens).parse());
    });
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
    let Stmt::If { orelse, .. } = &body[0] else {
        panic!("expected if statement");
    };
    let Stmt::If { orelse, .. } = &orelse[0] else {
        panic!("expected first elif");
    };
    let Stmt::If { orelse, .. } = &orelse[0] else {
        panic!("expected second elif");
    };
    assert!(matches!(orelse[..], [Stmt::Assign { .. }]));
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
