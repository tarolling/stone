//! Lexer that turns stone source code into tokens.
//!
//! Indentation is tracked Python-style, so `def f();` followed by an indented line produces
//! `Newline`, `Indent`, and later `Dedent` tokens around the function body.

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
    pub line: usize,
    pub col: usize,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} on line {}, col {}",
            self.message, self.line, self.col
        )
    }
}

impl Error for LexError {}

/// Lexer that converts source code into a list of [`Token`]s.
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    line: usize,
    col: usize,
    indent_stack: Vec<usize>,
    at_line_start: bool,
    pending_dedents: Vec<Token>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Lexer {
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
        let mut tokens: Vec<Token> = vec![];
        loop {
            let tok = self.next_token()?;
            if matches!(tok.r#type, TokenType::Eof) {
                // newline before EOF if missing; a trailing dedent already follows one
                if let Some(last) = tokens.last()
                    && !matches!(last.r#type, TokenType::Newline | TokenType::Dedent)
                {
                    tokens.push(Token {
                        r#type: TokenType::Newline,
                        line: self.line,
                        col: self.col,
                    });
                }
                // emit remaining dedents at end of file
                while self.indent_stack.len() > 1 {
                    self.indent_stack.pop();
                    tokens.push(Token {
                        r#type: TokenType::Dedent,
                        line: self.line,
                        col: self.col,
                    });
                }
                tokens.push(tok);
                break;
            }
            tokens.push(tok);
        }
        Ok(tokens)
    }

    fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
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

    fn skip_whitespace(&mut self) {
        while let Some(ch) = self.peek() {
            if ch == ' ' || ch == '\t' {
                self.advance();
            } else {
                break;
            }
        }
    }

    /// Returns the indentation width at the start of the current line, skipping blank lines.
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

        if indent > current_indent {
            self.indent_stack.push(indent);
            self.at_line_start = false;
            Some(Token {
                r#type: TokenType::Indent,
                line: self.line,
                col: 1,
            })
        } else if indent < current_indent {
            // one dedent per level dropped
            while let Some(&stack_indent) = self.indent_stack.last() {
                if stack_indent <= indent {
                    break;
                }
                self.indent_stack.pop();
                self.pending_dedents.push(Token {
                    r#type: TokenType::Dedent,
                    line: self.line,
                    col: 1,
                });
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

    fn lex_number(&mut self) -> Result<i64, LexError> {
        let (line, col) = (self.line, self.col);
        let mut num = String::new();
        while let Some(ch) = self.peek() {
            if ch.is_ascii_digit() {
                num.push(ch);
                self.advance();
            } else {
                break;
            }
        }
        num.parse().map_err(|_| LexError {
            message: format!("integer literal {} is too large", num),
            line,
            col,
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
        // loop rather than recurse past unknown characters, so long runs cannot overflow the stack
        loop {
            // handle indentation at line start
            if let Some(tok) = self.handle_indentation() {
                return Ok(tok);
            }

            self.skip_whitespace();
            let line = self.line;
            let col = self.col;

            let tok = match self.peek() {
                None => Token {
                    r#type: TokenType::Eof,
                    line,
                    col,
                },
                Some('\n') => {
                    self.advance();
                    self.at_line_start = true;
                    Token {
                        r#type: TokenType::Newline,
                        line,
                        col,
                    }
                }
                Some(';') => {
                    self.advance();
                    Token {
                        r#type: TokenType::Semi,
                        line,
                        col,
                    }
                }
                Some(',') => {
                    self.advance();
                    Token {
                        r#type: TokenType::Comma,
                        line,
                        col,
                    }
                }
                Some('(') => {
                    self.advance();
                    Token {
                        r#type: TokenType::LParen,
                        line,
                        col,
                    }
                }
                Some(')') => {
                    self.advance();
                    Token {
                        r#type: TokenType::RParen,
                        line,
                        col,
                    }
                }
                Some('+') | Some('-') | Some('*') | Some('/') | Some('=') => {
                    let op = self.advance().unwrap().to_string();
                    Token {
                        r#type: TokenType::Operator(op),
                        line,
                        col,
                    }
                }
                Some('"') => {
                    self.advance();
                    let mut s = String::new();
                    while let Some(ch) = self.peek() {
                        if ch == '"' {
                            self.advance();
                            break;
                        }
                        s.push(ch);
                        self.advance();
                    }
                    Token {
                        r#type: TokenType::String(s),
                        line,
                        col,
                    }
                }
                Some(ch) if ch.is_ascii_digit() => {
                    let num = self.lex_number()?;
                    Token {
                        r#type: TokenType::Number(num),
                        line,
                        col,
                    }
                }
                Some(ch) if ch.is_alphabetic() => {
                    let ident = self.lex_name();
                    let r#type = if RESERVED_KEYWORDS.contains(&ident.as_str()) {
                        TokenType::Keyword(ident)
                    } else {
                        TokenType::Name(ident)
                    };
                    Token { r#type, line, col }
                }
                Some(_) => {
                    // skip characters that cannot start a token
                    self.advance();
                    continue;
                }
            };
            return Ok(tok);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
            *tokens.first().unwrap(),
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 1,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(1).unwrap(),
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 1,
                col: 3
            }
        );
        assert_eq!(
            *tokens.get(2).unwrap(),
            Token {
                r#type: TokenType::Number(42),
                line: 1,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(3).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 1,
                col: 7
            }
        );
        assert_eq!(
            *tokens.get(4).unwrap(),
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 2,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(5).unwrap(),
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 2,
                col: 3
            }
        );
        assert_eq!(
            *tokens.get(6).unwrap(),
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 2,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(7).unwrap(),
            Token {
                r#type: TokenType::Operator("+".to_string()),
                line: 2,
                col: 7
            }
        );
        assert_eq!(
            *tokens.get(8).unwrap(),
            Token {
                r#type: TokenType::Number(8),
                line: 2,
                col: 9
            }
        );
        assert_eq!(
            *tokens.get(9).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 10
            }
        );
        assert_eq!(
            *tokens.get(10).unwrap(),
            Token {
                r#type: TokenType::Keyword("ret".to_string()),
                line: 3,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(11).unwrap(),
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 3,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(12).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 6
            }
        );
        assert_eq!(
            *tokens.last().unwrap(),
            Token {
                r#type: TokenType::Eof,
                line: 4,
                col: 1
            }
        );

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
            *tokens.first().unwrap(),
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 2,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(1).unwrap(),
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 2,
                col: 3
            }
        );
        assert_eq!(
            *tokens.get(2).unwrap(),
            Token {
                r#type: TokenType::Number(42),
                line: 2,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(3).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 7
            }
        );
        assert_eq!(
            *tokens.get(4).unwrap(),
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 3,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(5).unwrap(),
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 3,
                col: 3
            }
        );
        assert_eq!(
            *tokens.get(6).unwrap(),
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 3,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(7).unwrap(),
            Token {
                r#type: TokenType::Operator("+".to_string()),
                line: 3,
                col: 7
            }
        );
        assert_eq!(
            *tokens.get(8).unwrap(),
            Token {
                r#type: TokenType::Number(8),
                line: 3,
                col: 9
            }
        );
        assert_eq!(
            *tokens.get(9).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 10
            }
        );
        assert_eq!(
            *tokens.get(10).unwrap(),
            Token {
                r#type: TokenType::Keyword("ret".to_string()),
                line: 4,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(11).unwrap(),
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 4,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(12).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 4,
                col: 6
            }
        );
        assert_eq!(
            *tokens.last().unwrap(),
            Token {
                r#type: TokenType::Eof,
                line: 6,
                col: 1
            }
        );
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
            *tokens.first().unwrap(),
            Token {
                r#type: TokenType::Keyword("def".to_string()),
                line: 1,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(1).unwrap(),
            Token {
                r#type: TokenType::Name("testing".to_string()),
                line: 1,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(2).unwrap(),
            Token {
                r#type: TokenType::LParen,
                line: 1,
                col: 12
            }
        );
        assert_eq!(
            *tokens.get(3).unwrap(),
            Token {
                r#type: TokenType::RParen,
                line: 1,
                col: 13
            }
        );
        assert_eq!(
            *tokens.get(4).unwrap(),
            Token {
                r#type: TokenType::Semi,
                line: 1,
                col: 14
            }
        );
        assert_eq!(
            *tokens.get(5).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 1,
                col: 15
            }
        );
        assert_eq!(
            *tokens.get(6).unwrap(),
            Token {
                r#type: TokenType::Indent,
                line: 2,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(7).unwrap(),
            Token {
                r#type: TokenType::Keyword("ret".to_string()),
                line: 2,
                col: 5
            }
        );
        assert_eq!(
            *tokens.get(8).unwrap(),
            Token {
                r#type: TokenType::Number(52),
                line: 2,
                col: 9
            }
        );
        assert_eq!(
            *tokens.get(9).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 11
            }
        );
        assert_eq!(
            *tokens.get(10).unwrap(),
            Token {
                r#type: TokenType::Dedent,
                line: 3,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(11).unwrap(),
            Token {
                r#type: TokenType::Name("testing".to_string()),
                line: 3,
                col: 1
            }
        );
        assert_eq!(
            *tokens.get(12).unwrap(),
            Token {
                r#type: TokenType::LParen,
                line: 3,
                col: 8
            }
        );
        assert_eq!(
            *tokens.get(13).unwrap(),
            Token {
                r#type: TokenType::RParen,
                line: 3,
                col: 9
            }
        );
        assert_eq!(
            *tokens.get(14).unwrap(),
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 10
            }
        );
        assert_eq!(
            *tokens.last().unwrap(),
            Token {
                r#type: TokenType::Eof,
                line: 4,
                col: 1
            }
        );
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
            tokens,
            vec![
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
                    line: 3,
                    col: 17,
                },
            ],
        );
    }

    #[test]
    fn integer_literal_overflow_is_an_error() {
        let err = Lexer::new("x = 99999999999999999999\n").lex().unwrap_err();
        assert_eq!(err.line, 1);
        assert_eq!(err.col, 5);
    }

    #[test]
    fn non_ascii_digits_are_skipped() {
        // '\u{0663}' is ARABIC-INDIC DIGIT THREE, which is numeric but not an ASCII digit
        let tokens = Lexer::new("\u{0663}\n").lex().unwrap();
        assert_eq!(tokens.last().unwrap().r#type, TokenType::Eof);
    }

    #[test]
    fn long_run_of_unknown_characters() {
        let source = "@".repeat(1_000_000);
        let tokens = Lexer::new(&source).lex().unwrap();
        assert_eq!(tokens.last().unwrap().r#type, TokenType::Eof);
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
    fn while_and_not_are_keywords() {
        let tokens = Lexer::new("while not x;\n").lex().unwrap();
        assert_eq!(tokens[0].r#type, TokenType::Keyword("while".to_string()));
        assert_eq!(tokens[1].r#type, TokenType::Keyword("not".to_string()));
    }
}
