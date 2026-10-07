//! Lexer that turns stone source code into tokens.
//!
//! Indentation is tracked Python-style, so `def f();` followed by an indented line produces
//! `Newline`, `Indent`, and later `Dedent` tokens around the function body.

use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};
use crate::token::{RESERVED_KEYWORDS, Token, TokenType};
use std::error::Error;
use std::fmt::Display;

const TAB_SIZE: usize = 4;

/// Error for source text that cannot be turned into tokens.
///
/// For example, lexing `99999999999999999999` fails because the literal does not fit in an `i64`.
#[derive(Debug, PartialEq)]
pub struct LexError {
    pub message: String,
    pub span: Span,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} on line {}, col {}",
            self.message, self.span.start.line, self.span.start.col
        )
    }
}

impl Error for LexError {}

impl From<LexError> for Diagnostic {
    fn from(err: LexError) -> Self {
        Diagnostic::error(err.span, err.message)
    }
}

/// Lexer that converts source code into a list of [`Token`]s.
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    pending_dedents: Vec<Token>,
    /// The file every span is in.
    file: FileId,
}

impl Lexer {
    /// Returns a lexer for the entry file's source.
    pub fn new(source: &str) -> Self {
        Self::with_file(source, FileId::default())
    }

    /// Returns a lexer whose tokens and errors point into `file`.
    ///
    /// For example, `Lexer::with_file("x\n", FileId(1)).lex()` gives a `Name("x")` whose span is in
    /// `FileId(1)`.
    pub fn with_file(source: &str, file: FileId) -> Self {
        Lexer {
            file,
            input: source.chars().collect(),
            pos: 0,
            line: 1,
            col: 1,
            indent_stack: vec![0],
            at_line_start: true,
            pending_dedents: vec![],
        }
    }

    /// Lexes the entire input and returns its tokens, ending with `Eof`.
    ///
    /// For example, `x = 1` produces `Name("x")`, `Operator("=")`, `Number(1)`, `Newline`, and `Eof`.
    /// Integer literals that do not fit in an `i64` produce a [`LexError`].
    pub fn lex(&mut self) -> Result<Vec<Token>, LexError> {
        let file = self.file;
        match self.lex_tokens() {
            Ok(mut tokens) => {
                for token in &mut tokens {
                    token.span.file = file;
                }
                Ok(tokens)
            }
            Err(e) => Err(LexError {
                span: e.span.in_file(file),
                ..e
            }),
        }
    }

    /// Lexes the entire input with every span in the entry file, which `lex` then moves into
    /// `self.file`.
    fn lex_tokens(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens: Vec<Token> = vec![];
        loop {
            let tok = self.next_token()?;
            if matches!(tok.r#type, TokenType::Eof) {
                // newline before EOF if missing; a trailing dedent already follows one
                if let Some(last) = tokens.last()
                    && !matches!(last.r#type, TokenType::Newline | TokenType::Dedent)
                {
                    tokens.push(Token::new(TokenType::Newline, Span::empty(self.pos())));
                }
                // emit remaining dedents at end of file
                while self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    tokens.push(Token::new(TokenType::Dedent, Span::empty(self.pos())));
                }
                tokens.push(tok);
                break;
            }
            tokens.push(tok);
        }
        Ok(tokens)
    }

    /// Returns the position of the next character to be lexed.
    fn pos(&self) -> Pos {
        Pos::new(self.line, self.col)
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    /// Returns the character `offset` places after the next one, so `peek_at(0)` is `peek()`.
    fn peek_at(&self, offset: usize) -> Option<char> {
        self.input.get(self.pos + offset).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.pos += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    /// Skips spaces, tabs, carriage returns (so `\r\n` acts like `\n`), and a trailing `//`
    /// comment, stopping before the newline.
    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            match ch {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '/' if self.peek_at(1) == Some('/') => self.skip_comment(),
                _ => break,
            }
        }
    }

    /// Skips a `//` comment up to but not including the newline that ends it.
    fn skip_comment(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == '\n' {
                break;
            }
            self.advance();
        }
    }

    /// Returns the indentation width at the start of the current line, skipping blank and
    /// comment-only lines, which never change indentation.
    ///
    /// Tabs count as `TAB_SIZE` columns, so a line starting with one tab has a width of 4.
    fn calculate_indent(&mut self) -> usize {
        let mut indent = 0;
        while let Some(ch) = self.peek() {
            match ch {
                ' ' => {
                    indent += 1;
                    self.advance();
                }
                '\t' => {
                    indent += TAB_SIZE;
                    self.advance();
                }
                '\r' => {
                    self.advance();
                }
                '/' if self.peek_at(1) == Some('/') => self.skip_comment(),
                '\n' => {
                    // skip empty lines
                    self.advance();
                    indent = 0;
                }
                _ => break,
            }
        }
        indent
    }

    /// Emits an `Indent` or `Dedent` token when the indentation of a new line changes.
    ///
    /// For example, dropping back two indentation levels at once produces two `Dedent` tokens.
    fn handle_indentation(&mut self) -> Option<Token> {
        // return pending dedents first, even though the line start has already been consumed
        if !self.pending_dedents.is_empty() {
            return Some(self.pending_dedents.remove(0));
        }

        if !self.at_line_start {
            return None;
        }

        let indent = self.calculate_indent();
        let current_indent = *self.indent_stack.last().unwrap();

        // trailing blank or comment lines leave indentation alone, and `lex` closes open blocks
        if self.peek().is_none() {
            self.at_line_start = false;
            return None;
        }

        if indent > current_indent {
            self.indent_stack.push(indent);
            self.at_line_start = false;
            Some(Token::new(
                TokenType::Indent,
                Span::empty(Pos::new(self.line, 1)),
            ))
        } else if indent < current_indent {
            // one dedent per level dropped
            while let Some(&stack_indent) = self.indent_stack.last() {
                if stack_indent <= indent {
                    break;
                }
                self.indent_stack.pop();
                self.pending_dedents.push(Token::new(
                    TokenType::Dedent,
                    Span::empty(Pos::new(self.line, 1)),
                ));
            }
            self.at_line_start = false;
            if !self.pending_dedents.is_empty() {
                Some(self.pending_dedents.remove(0))
            } else {
                // next_token keeps lexing the line
                None
            }
        } else {
            self.at_line_start = false;
            None
        }
    }

    /// Lexes an integer literal such as `42`, or a float literal such as `1.5`, `1e3`, or `2.5e-3`.
    ///
    /// A float needs a digit after its `.` and after its `e` and optional sign, so `1.` lexes as
    /// `1` then `.`, and `1e` as `1` then the name `e`.
    fn lex_number(&mut self) -> Result<TokenType, LexError> {
        let start = self.pos();
        let mut num = self.lex_digits();
        let mut is_float = false;
        if self.peek() == Some('.') && self.peek_at(1).is_some_and(|ch| ch.is_ascii_digit()) {
            self.advance();
            num.push('.');
            num += &self.lex_digits();
            is_float = true;
        }
        if matches!(self.peek(), Some('e' | 'E')) {
            let sign = matches!(self.peek_at(1), Some('+' | '-'));
            let digit = self.peek_at(1 + sign as usize);
            if digit.is_some_and(|ch| ch.is_ascii_digit()) {
                for _ in 0..1 + sign as usize {
                    num.push(self.advance().unwrap());
                }
                num += &self.lex_digits();
                is_float = true;
            }
        }
        let span = Span::new(start, self.pos());
        if is_float {
            match num.parse::<f64>() {
                Ok(value) if value.is_finite() => Ok(TokenType::Float(value)),
                _ => Err(LexError {
                    message: format!("float literal {num} is too large"),
                    span,
                }),
            }
        } else {
            num.parse().map(TokenType::Number).map_err(|_| LexError {
                message: format!("integer literal {num} is too large"),
                span,
            })
        }
    }

    /// Lexes a run of ASCII digits, which may be empty.
    fn lex_digits(&mut self) -> String {
        let mut digits = String::new();
        while let Some(ch) = self.peek().filter(char::is_ascii_digit) {
            digits.push(ch);
            self.advance();
        }
        digits
    }

    /// Lexes a string literal starting at its opening quote and returns its contents.
    ///
    /// For example, `"hi"` returns `hi`. A string must close on the line it opens on, so `"abc`
    /// followed by a newline is an error spanning `"abc`.
    fn lex_string(&mut self) -> Result<String, LexError> {
        let start = self.pos();
        self.advance();
        let mut s = String::new();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    return Ok(s);
                }
                '\n' => break,
                _ => {
                    s.push(ch);
                    self.advance();
                }
            }
        }
        Err(LexError {
            message: "unterminated string".to_string(),
            span: Span::new(start, self.pos()),
        })
    }

    fn lex_name(&mut self) -> String {
        let mut ident = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_alphanumeric() || ch == '_' {
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        ident
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        // handle indentation at line start
        if let Some(tok) = self.handle_indentation() {
            return Ok(tok);
        }

        self.skip_whitespace();
        let start = self.pos();

        let r#type = match self.peek() {
            None => TokenType::Eof,
            Some('\n') => {
                self.advance();
                self.at_line_start = true;
                // the newline covers its own character, not the whole next line
                let end = Pos::new(start.line, start.col + 1);
                return Ok(Token::new(TokenType::Newline, Span::new(start, end)));
            }
            Some(';') => {
                self.advance();
                TokenType::Semi
            }
            Some(',') => {
                self.advance();
                TokenType::Comma
            }
            Some('(') => {
                self.advance();
                TokenType::LParen
            }
            Some(')') => {
                self.advance();
                TokenType::RParen
            }
            Some('[') => {
                self.advance();
                TokenType::LBracket
            }
            Some(']') => {
                self.advance();
                TokenType::RBracket
            }
            Some('.') => {
                self.advance();
                TokenType::Dot
            }
            Some('*') => {
                self.advance();
                if self.peek() == Some('*') {
                    self.advance();
                    TokenType::Operator("**".to_string())
                } else {
                    TokenType::Operator("*".to_string())
                }
            }
            Some('+') | Some('-') | Some('/') | Some('%') => {
                let op = self.advance().unwrap().to_string();
                TokenType::Operator(op)
            }
            Some(first @ ('=' | '<' | '>' | '!')) => {
                self.advance();
                if self.peek() == Some('=') {
                    self.advance();
                    TokenType::Operator(format!("{first}="))
                } else if first == '!' {
                    return Err(LexError {
                        message: "unexpected character '!'".to_string(),
                        span: Span::new(start, self.pos()),
                    });
                } else {
                    TokenType::Operator(first.to_string())
                }
            }
            Some('"') => TokenType::String(self.lex_string()?),
            Some(ch) if ch.is_ascii_digit() => self.lex_number()?,
            Some(ch) if ch.is_alphabetic() => {
                let ident = self.lex_name();
                if RESERVED_KEYWORDS.contains(&ident.as_str()) {
                    TokenType::Keyword(ident)
                } else {
                    TokenType::Name(ident)
                }
            }
            Some(ch) => {
                self.advance();
                return Err(LexError {
                    message: format!("unexpected character '{ch}'"),
                    span: Span::new(start, self.pos()),
                });
            }
        };
        Ok(Token::new(r#type, Span::new(start, self.pos())))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Returns a token's type and start position, which is what most tests check.
    fn start_of(token: &Token) -> (TokenType, usize, usize) {
        (
            token.r#type.clone(),
            token.span.start.line,
            token.span.start.col,
        )
    }

    /// Builds the expected [`start_of`] result for a token of type `r#type` at `line` and `col`.
    fn tok(r#type: TokenType, line: usize, col: usize) -> (TokenType, usize, usize) {
        (r#type, line, col)
    }

    #[test]
    fn simple_program() {
        let source = r#"x = 42
y = x + 8
ret y
"#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        assert_eq!(tokens.len(), 14);
        assert_eq!(
            start_of(&tokens[0]),
            tok(TokenType::Name("x".to_string()), 1, 1)
        );
        assert_eq!(
            start_of(&tokens[1]),
            tok(TokenType::Operator("=".to_string()), 1, 3)
        );
        assert_eq!(start_of(&tokens[2]), tok(TokenType::Number(42), 1, 5));
        assert_eq!(start_of(&tokens[3]), tok(TokenType::Newline, 1, 7));
        assert_eq!(
            start_of(&tokens[4]),
            tok(TokenType::Name("y".to_string()), 2, 1)
        );
        assert_eq!(
            start_of(&tokens[5]),
            tok(TokenType::Operator("=".to_string()), 2, 3)
        );
        assert_eq!(
            start_of(&tokens[6]),
            tok(TokenType::Name("x".to_string()), 2, 5)
        );
        assert_eq!(
            start_of(&tokens[7]),
            tok(TokenType::Operator("+".to_string()), 2, 7)
        );
        assert_eq!(start_of(&tokens[8]), tok(TokenType::Number(8), 2, 9));
        assert_eq!(start_of(&tokens[9]), tok(TokenType::Newline, 2, 10));
        assert_eq!(
            start_of(&tokens[10]),
            tok(TokenType::Keyword("ret".to_string()), 3, 1)
        );
        assert_eq!(
            start_of(&tokens[11]),
            tok(TokenType::Name("y".to_string()), 3, 5)
        );
        assert_eq!(start_of(&tokens[12]), tok(TokenType::Newline, 3, 6));
        assert_eq!(start_of(tokens.last().unwrap()), tok(TokenType::Eof, 4, 1));

        // test with newlines, should skip over them
        let source = r#"
x = 42
y = x + 8
ret y

"#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        assert_eq!(tokens.len(), 14);
        assert_eq!(
            start_of(&tokens[0]),
            tok(TokenType::Name("x".to_string()), 2, 1)
        );
        assert_eq!(
            start_of(&tokens[1]),
            tok(TokenType::Operator("=".to_string()), 2, 3)
        );
        assert_eq!(start_of(&tokens[2]), tok(TokenType::Number(42), 2, 5));
        assert_eq!(start_of(&tokens[3]), tok(TokenType::Newline, 2, 7));
        assert_eq!(
            start_of(&tokens[4]),
            tok(TokenType::Name("y".to_string()), 3, 1)
        );
        assert_eq!(
            start_of(&tokens[5]),
            tok(TokenType::Operator("=".to_string()), 3, 3)
        );
        assert_eq!(
            start_of(&tokens[6]),
            tok(TokenType::Name("x".to_string()), 3, 5)
        );
        assert_eq!(
            start_of(&tokens[7]),
            tok(TokenType::Operator("+".to_string()), 3, 7)
        );
        assert_eq!(start_of(&tokens[8]), tok(TokenType::Number(8), 3, 9));
        assert_eq!(start_of(&tokens[9]), tok(TokenType::Newline, 3, 10));
        assert_eq!(
            start_of(&tokens[10]),
            tok(TokenType::Keyword("ret".to_string()), 4, 1)
        );
        assert_eq!(
            start_of(&tokens[11]),
            tok(TokenType::Name("y".to_string()), 4, 5)
        );
        assert_eq!(start_of(&tokens[12]), tok(TokenType::Newline, 4, 6));
        assert_eq!(start_of(tokens.last().unwrap()), tok(TokenType::Eof, 6, 1));
    }

    #[test]
    fn simple_functions() {
        let source = r#"def testing();
    ret 52
testing()
"#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        assert_eq!(tokens.len(), 16);
        assert_eq!(
            start_of(&tokens[0]),
            tok(TokenType::Keyword("def".to_string()), 1, 1)
        );
        assert_eq!(
            start_of(&tokens[1]),
            tok(TokenType::Name("testing".to_string()), 1, 5)
        );
        assert_eq!(start_of(&tokens[2]), tok(TokenType::LParen, 1, 12));
        assert_eq!(start_of(&tokens[3]), tok(TokenType::RParen, 1, 13));
        assert_eq!(start_of(&tokens[4]), tok(TokenType::Semi, 1, 14));
        assert_eq!(start_of(&tokens[5]), tok(TokenType::Newline, 1, 15));
        assert_eq!(start_of(&tokens[6]), tok(TokenType::Indent, 2, 1));
        assert_eq!(
            start_of(&tokens[7]),
            tok(TokenType::Keyword("ret".to_string()), 2, 5)
        );
        assert_eq!(start_of(&tokens[8]), tok(TokenType::Number(52), 2, 9));
        assert_eq!(start_of(&tokens[9]), tok(TokenType::Newline, 2, 11));
        assert_eq!(start_of(&tokens[10]), tok(TokenType::Dedent, 3, 1));
        assert_eq!(
            start_of(&tokens[11]),
            tok(TokenType::Name("testing".to_string()), 3, 1)
        );
        assert_eq!(start_of(&tokens[12]), tok(TokenType::LParen, 3, 8));
        assert_eq!(start_of(&tokens[13]), tok(TokenType::RParen, 3, 9));
        assert_eq!(start_of(&tokens[14]), tok(TokenType::Newline, 3, 10));
        assert_eq!(start_of(tokens.last().unwrap()), tok(TokenType::Eof, 4, 1));
    }

    /// Checks the tokens produced for a function with several parameters:
    ///
    /// ```text
    /// def testing(a, b, c);
    ///     ret b
    /// testing(1, 2, 3)
    /// ```
    #[test]
    fn simple_function_multiple_args() {
        let source = r#"def testing(a, b, c);
    ret b
testing(1, 2, 3)"#;

        let mut lexer = Lexer::new(source);
        let tokens = lexer.lex().unwrap();
        assert_eq!(
            tokens.iter().map(start_of).collect::<Vec<_>>(),
            vec![
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
                tok(TokenType::Eof, 3, 17),
            ],
        );
    }

    #[test]
    fn integer_literal_overflow_is_an_error() {
        let err = Lexer::new("x = 99999999999999999999\n").lex().unwrap_err();
        assert_eq!(err.span.start, Pos::new(1, 5));
    }

    #[test]
    fn float_literals() {
        assert_eq!(
            types_of("1.5 0.25 1e3 2.5e-3 1E+9 007.5\n"),
            [
                TokenType::Float(1.5),
                TokenType::Float(0.25),
                TokenType::Float(1e3),
                TokenType::Float(2.5e-3),
                TokenType::Float(1e9),
                TokenType::Float(7.5),
                TokenType::Newline,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn a_float_needs_digits_after_the_point_or_exponent() {
        assert_eq!(
            types_of("1. 2e x\n"),
            [
                TokenType::Number(1),
                TokenType::Dot,
                TokenType::Number(2),
                TokenType::Name("e".to_string()),
                TokenType::Name("x".to_string()),
                TokenType::Newline,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn float_literals_span_their_source_text() {
        let tokens = Lexer::new("x = 2.5e-3\n").lex().unwrap();
        assert_eq!(tokens[2].span, Span::new(Pos::new(1, 5), Pos::new(1, 11)));
    }

    #[test]
    fn float_literal_overflow_is_an_error() {
        let err = Lexer::new("x = 1e999\n").lex().unwrap_err();
        assert_eq!(err.message, "float literal 1e999 is too large");
        assert_eq!(err.span, Span::new(Pos::new(1, 5), Pos::new(1, 10)));
    }

    #[test]
    fn non_ascii_digits_are_an_error() {
        // '\u{0663}' is ARABIC-INDIC DIGIT THREE, which is numeric but not an ASCII digit
        let err = Lexer::new("\u{0663}\n").lex().unwrap_err();
        assert_eq!(err.message, "unexpected character '\u{0663}'");
    }

    #[test]
    fn long_run_of_unknown_characters_is_an_error() {
        let source = "@".repeat(1_000_000);
        let err = Lexer::new(&source).lex().unwrap_err();
        assert_eq!(err.span, Span::new(Pos::new(1, 1), Pos::new(1, 2)));
    }

    #[test]
    fn spans_carry_the_file_they_were_lexed_from() {
        let tokens = Lexer::with_file("x = 1\n", FileId(3)).lex().unwrap();
        assert!(tokens.iter().all(|t| t.span.file == FileId(3)));

        let err = Lexer::with_file("x = @\n", FileId(3)).lex().unwrap_err();
        assert_eq!(err.span.file, FileId(3));

        let tokens = Lexer::new("x\n").lex().unwrap();
        assert_eq!(tokens[0].span.file, FileId(0));
    }

    /// Returns just the token types of `source`, for tests that do not care about positions.
    fn types_of(source: &str) -> Vec<TokenType> {
        Lexer::new(source)
            .lex()
            .unwrap()
            .into_iter()
            .map(|t| t.r#type)
            .collect()
    }

    fn name(id: &str) -> TokenType {
        TokenType::Name(id.to_string())
    }

    fn op(op: &str) -> TokenType {
        TokenType::Operator(op.to_string())
    }

    #[test]
    fn comments_run_to_the_end_of_the_line() {
        assert_eq!(
            types_of("x = 1 // the answer, (sort of)\n"),
            [
                name("x"),
                op("="),
                TokenType::Number(1),
                TokenType::Newline,
                TokenType::Eof
            ]
        );
    }

    #[test]
    fn comment_only_lines_do_not_change_indentation() {
        let source = "if 1;\n    x = 1\n// at column one\n        // deeper\n    y = 2\n";
        assert_eq!(
            types_of(source),
            [
                TokenType::Keyword("if".to_string()),
                TokenType::Number(1),
                TokenType::Semi,
                TokenType::Newline,
                TokenType::Indent,
                name("x"),
                op("="),
                TokenType::Number(1),
                TokenType::Newline,
                name("y"),
                op("="),
                TokenType::Number(2),
                TokenType::Newline,
                TokenType::Dedent,
                TokenType::Eof
            ]
        );
    }

    #[test]
    fn a_file_of_only_comments_is_empty() {
        assert_eq!(types_of("// just\n    // comments"), [TokenType::Eof]);
    }

    #[test]
    fn a_single_slash_is_still_division() {
        assert_eq!(
            types_of("a / b // c / d\n"),
            [
                name("a"),
                op("/"),
                name("b"),
                TokenType::Newline,
                TokenType::Eof
            ]
        );
    }

    #[test]
    fn hash_is_not_a_comment() {
        let err = Lexer::new("x = 1 # not a comment\n").lex().unwrap_err();
        assert_eq!(err.message, "unexpected character '#'");
    }

    #[test]
    fn brackets_and_dots_are_tokens() {
        assert_eq!(
            types_of("a[0].b\n"),
            [
                name("a"),
                TokenType::LBracket,
                TokenType::Number(0),
                TokenType::RBracket,
                TokenType::Dot,
                name("b"),
                TokenType::Newline,
                TokenType::Eof
            ]
        );
    }

    #[test]
    fn windows_line_endings_are_newlines() {
        assert_eq!(types_of("x = 1\r\ny = 2\r\n"), types_of("x = 1\ny = 2\n"));
    }

    #[test]
    fn comparison_operators_are_one_token() {
        assert_eq!(
            types_of("a == b != c < d <= e > f >= g = h\n")
                .into_iter()
                .filter(|t| matches!(t, TokenType::Operator(_)))
                .collect::<Vec<_>>(),
            ["==", "!=", "<", "<=", ">", ">=", "="].map(op)
        );
    }

    #[test]
    fn modulo_and_power_are_operators() {
        assert_eq!(
            types_of("a % b ** c * * d\n")
                .into_iter()
                .filter(|t| matches!(t, TokenType::Operator(_)))
                .collect::<Vec<_>>(),
            ["%", "**", "*", "*"].map(op)
        );
    }

    #[test]
    fn for_and_in_are_keywords() {
        assert_eq!(
            types_of("for i in x;\n")[..4],
            [
                TokenType::Keyword("for".to_string()),
                name("i"),
                TokenType::Keyword("in".to_string()),
                name("x"),
            ]
        );
    }

    #[test]
    fn a_lone_bang_is_an_error() {
        let err = Lexer::new("x = !y\n").lex().unwrap_err();
        assert_eq!(err.message, "unexpected character '!'");
    }

    #[test]
    fn unknown_characters_are_an_error() {
        let err = Lexer::new("x = 1 @ 2\n").lex().unwrap_err();
        assert_eq!(err.message, "unexpected character '@'");
        assert_eq!(err.span, Span::new(Pos::new(1, 7), Pos::new(1, 8)));
    }

    #[test]
    fn unterminated_strings_are_an_error() {
        let err = Lexer::new("x = \"abc\ny = 1\n").lex().unwrap_err();
        assert_eq!(err.message, "unterminated string");
        assert_eq!(err.span, Span::new(Pos::new(1, 5), Pos::new(1, 9)));
    }

    #[test]
    fn indented_block_at_end_of_file() {
        let tokens = Lexer::new("if 1;\n    x = 1\n").lex().unwrap();
        let types: Vec<TokenType> = tokens.into_iter().map(|t| t.r#type).collect();
        assert_eq!(
            types[types.len() - 3..],
            [TokenType::Newline, TokenType::Dedent, TokenType::Eof]
        );
    }

    #[test]
    fn dropping_two_levels_emits_two_dedents() {
        let tokens = Lexer::new("if 1;\n    if 1;\n        x = 1\ny = 2\n")
            .lex()
            .unwrap();
        let types: Vec<TokenType> = tokens.into_iter().map(|t| t.r#type).collect();
        let y = types
            .iter()
            .position(|t| *t == TokenType::Name("y".to_string()))
            .unwrap();
        assert_eq!(types[y - 2..y], [TokenType::Dedent, TokenType::Dedent]);
    }

    #[test]
    fn tokens_span_their_source_text() {
        let tokens = Lexer::new("total = 42\n").lex().unwrap();
        let spans: Vec<Span> = tokens.iter().map(|t| t.span).collect();
        assert_eq!(
            spans,
            [
                Span::new(Pos::new(1, 1), Pos::new(1, 6)),
                Span::new(Pos::new(1, 7), Pos::new(1, 8)),
                Span::new(Pos::new(1, 9), Pos::new(1, 11)),
                Span::new(Pos::new(1, 11), Pos::new(1, 12)),
                Span::new(Pos::new(2, 1), Pos::new(2, 1)),
            ]
        );
    }

    #[test]
    fn while_and_not_are_keywords() {
        let tokens = Lexer::new("while not x;\n").lex().unwrap();
        assert_eq!(tokens[0].r#type, TokenType::Keyword("while".to_string()));
        assert_eq!(tokens[1].r#type, TokenType::Keyword("not".to_string()));
    }
}
