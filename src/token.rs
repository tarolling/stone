//! Token definitions and reserved keywords for the stone grammar.

/// The kind of a token, such as `Name("x")` or `Number(42)`.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Eof,
    Name(String),
    Keyword(String),
    Number(i64),
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

/// A token with the line and column where it starts, both counted from 1.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub r#type: TokenType,
    pub line: usize,
    pub col: usize,
}

/// Words that are lexed as keywords instead of names, such as `def` and `ret`.
pub static RESERVED_KEYWORDS: [&str; 14] = [
    "and", "break", "cont", "def", "elif", "else", "false", "if", "none", "not", "or", "ret",
    "true", "while",
];
