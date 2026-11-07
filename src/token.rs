//! Token definitions and reserved keywords for the stone grammar.

use crate::span::Span;
use crate::stdlib::spec::FormatSpec;

/// The kind of a token, such as `Name("x")` or `Number(42)`.
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Eof,
    Name(String),
    /// A reserved word, one of [`RESERVED_KEYWORDS`], such as `fn`.
    Keyword(&'static str),
    Number(i64),
    Float(f64),
    String(String),
    /// The text of a string up to its first `{value}`, as `"a` in `"a{x}b"`.
    StringStart(String),
    /// The text of a string between two of its values, as `b` in `"a{x}b{y}c"`.
    StringMiddle(String),
    /// The text of a string after its last value, as `b"` in `"a{x}b"`.
    StringEnd(String),
    /// The format spec after a `:` that ends a value in a string, as `.2f` in `"{x:.2f}"`.
    FormatSpec(Box<FormatSpec>),
    /// A bytes literal, as `b"a\x00"`, which may hold any byte.
    Bytes(Vec<u8>),
    /// An operator such as `+` or `+=`, held as static text so that matching one allocates nothing.
    Operator(&'static str),
    Newline,
    Indent,
    Dedent,
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Dot,
    /// The `->` before a function's return type, as in `fn f() -> int:`.
    Arrow,
    /// A `|` around a lambda's parameters, as in `|x| x * 2`.
    VBar,
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
pub static RESERVED_KEYWORDS: [&str; 26] = [
    "and", "as", "assert", "break", "case", "cont", "elif", "else", "enum", "false", "fn", "for",
    "if", "impl", "in", "match", "none", "not", "or", "pub", "ret", "struct", "trait", "true",
    "use", "while",
];
