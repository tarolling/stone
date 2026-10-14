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
    Colon,
    Dot,
    /// The `->` before a function's return type, as in `fn f() -> int;`.
    Arrow,
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

/// Words that are lexed as keywords instead of names, such as `fn` and `ret`.
pub static RESERVED_KEYWORDS: [&str; 23] = [
    "and", "as", "break", "case", "cont", "elif", "else", "enum", "false", "fn", "for", "if", "in",
    "match", "none", "not", "or", "pub", "ret", "struct", "true", "use", "while",
];
