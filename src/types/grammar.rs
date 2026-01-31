//! PEG grammar for stone

/// Token types
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    EOF,
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

/// Token
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub r#type: TokenType,
    pub line: usize,
    pub col: usize,
}

/// Keywords
pub static RESERVED_KEYWORDS: [&'static str; 12] = [
    "and", "break", "cont", "def", "elif", "else", "false", "if", "none", "or", "ret", "true",
];
