//! Token definitions and reserved keywords for the stone grammar.

use crate::span::Span;

/// The kind of a token, such as `Name("x")` or `Number(42)`.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Eof,
    Name(String),
    Keyword(String),
    Number(i64),
    Float(f64),
    String(String),
    Operator(String),
    Newline,
    Indent,
    Dedent,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Semi,
    Comma,
    Dot,
}

/// A token and the span of source text it was lexed from.
///
/// For example, `42` in `x = 42` is a `Number(42)` spanning line 1, cols 5 to 7.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub r#type: TokenType,
    pub span: Span,
}

impl Token {
    pub fn new(r#type: TokenType, span: Span) -> Self {
        Token { r#type, span }
    }
}

/// Words that are lexed as keywords instead of names, such as `def` and `ret`.
pub static RESERVED_KEYWORDS: [&str; 16] = [
    "and", "break", "cont", "def", "elif", "else", "false", "for", "if", "in", "none", "not", "or",
    "ret", "true", "while",
];
