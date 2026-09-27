use super::Parser;
use crate::ast::*;
use crate::token::{Token, TokenType};

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
