//! Recursive-descent PEG parser that turns tokens into a stone AST.
//!
//! Each `parse_*` method implements the rule of the same name in `docs/grammar/stone.gram`.
//! For example, `parse_sum` implements `sum: term ('+'|'-' term)*`.

use std::rc::Rc;

mod expressions;
mod statements;
#[cfg(test)]
mod tests;

use crate::ast::{Expr, Mod, ParserError};
use crate::debug;
use crate::diagnostic::Diagnostic;
use crate::span::{Pos, Span};
use crate::token::{Token, TokenType};

type ParseExprResult = Result<Box<Expr>, ParserError>;

/// Maximum nesting of expressions and blocks before parsing fails, keeping recursion off the end
/// of the stack.
///
/// For example, an expression with more than `MAX_DEPTH` unary minus signs in a row, like
/// `------1`, is rejected.
pub(crate) const MAX_DEPTH: usize = 200;

/// The token `peek` returns for an empty token stream.
const EOF: Token = Token {
    r#type: TokenType::Eof,
    span: Span {
        start: Pos { line: 1, col: 1 },
        end: Pos { line: 1, col: 1 },
    },
};

/// Parser that converts a token stream into a [`Mod`].
pub struct Parser {
    /// Tokens being parsed, shared through `Rc` because the stream can be large.
    tokens: Rc<[Token]>,
    pos: usize,
    /// Current nesting of expressions and blocks, bounded by [`MAX_DEPTH`].
    depth: usize,
    /// Index of the furthest token any rule failed at, which is where a syntax error is reported.
    furthest: usize,
    /// What the rules that failed at `furthest` expected there, in the order they asked.
    expected: Vec<Expected>,
    /// Where nesting first went past [`MAX_DEPTH`], which takes priority over other errors.
    too_deep: Option<Span>,
}

/// Something a rule expected to find, such as `';'` or `an expression`.
///
/// Quiet expectations, like an operator that could continue an expression, are only shown when
/// nothing else was expected at the same token. So `print(1` reports `expected ')'` rather than
/// listing every operator that could follow `1`.
#[derive(PartialEq)]
struct Expected {
    what: String,
    quiet: bool,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Parser {
            tokens: Rc::from(tokens),
            pos: 0,
            depth: 0,
            furthest: 0,
            expected: vec![],
            too_deep: None,
        }
    }

    // helpers

    fn peek(&self) -> &Token {
        // `lex` always ends with `Eof`, so running past the end repeats it
        self.tokens
            .get(self.pos)
            .or(self.tokens.last())
            .unwrap_or(&EOF)
    }

    fn advance(&mut self) -> Token {
        let tok = self.peek().clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, target: TokenType) -> Option<Token> {
        debug!("Expecting {:?} at pos {:?}...", target, self.pos);
        let tok = self.peek().clone();
        if target != tok.r#type {
            debug!("Failed.");
            self.record_expected(describe_expected(&target), is_quiet(&target));
            return None;
        }
        debug!("Success! Advancing position...");
        self.pos += 1;
        Some(tok)
    }

    /// Returns the span of the last token consumed, skipping layout tokens so that a statement
    /// ends at its last visible character rather than at the `Newline` or `Dedent` after it.
    ///
    /// For example, after parsing `x = 1\n`, this is the span of `1`.
    fn last_span(&self) -> Span {
        self.tokens[..self.pos.min(self.tokens.len())]
            .iter()
            .rev()
            .find(|t| {
                !matches!(
                    t.r#type,
                    TokenType::Newline | TokenType::Indent | TokenType::Dedent
                )
            })
            .map_or(self.peek().span, |t| t.span)
    }

    /// Returns the span from the token at index `start` through the last token consumed.
    ///
    /// For example, with `start` pointing at `x` in `x = a + 12`, after parsing the assignment this
    /// covers cols 1 to 11.
    fn span_from(&self, start: usize) -> Span {
        let first = self.tokens.get(start).map_or(self.peek().span, |t| t.span);
        first.to(self.last_span())
    }

    /// Consumes a name token and returns it with its span, or notes that `what` was expected here.
    ///
    /// For example, `self.expect_name("a parameter name", false)` reads `a` in `def f(a);`.
    fn expect_name(&mut self, what: &str, quiet: bool) -> Option<(String, Span)> {
        let tok = self.peek().clone();
        if let TokenType::Name(name) = tok.r#type {
            self.pos += 1;
            return Some((name, tok.span));
        }
        self.record_expected(what.to_string(), quiet);
        None
    }

    /// Runs a rule one nesting level deeper, failing once nesting exceeds [`MAX_DEPTH`].
    ///
    /// For example, `self.nested(Self::parse_factor)` parses the operand of a unary minus.
    fn nested<T>(
        &mut self,
        rule: impl FnOnce(&mut Self) -> Result<T, ParserError>,
    ) -> Result<T, ParserError> {
        if self.depth >= MAX_DEPTH {
            self.too_deep.get_or_insert(self.peek().span);
            return Err(ParserError {
                method: "nested".to_string(),
                token: self.peek().r#type.clone(),
                span: self.peek().span,
            });
        }
        self.depth += 1;
        let result = rule(self);
        self.depth -= 1;
        result
    }

    /// Notes that a rule expected `what` at the current token, keeping only the expectations at
    /// the furthest token reached so far.
    ///
    /// For example, `self.record_expected("a name".to_string(), false)` after failing to find a
    /// parameter name.
    fn record_expected(&mut self, what: String, quiet: bool) {
        if self.pos > self.furthest {
            self.furthest = self.pos;
            self.expected.clear();
        }
        let expected = Expected { what, quiet };
        if self.pos == self.furthest && !self.expected.contains(&expected) {
            self.expected.push(expected);
        }
    }

    /// Builds the syntax error for a failed parse from what was expected at the furthest token.
    ///
    /// For example, for `print(1` this is `expected ')', found end of line` at the end of the line.
    fn syntax_error(&self) -> Diagnostic {
        if let Some(span) = self.too_deep {
            return Diagnostic::error(
                span,
                format!("code is nested more than {MAX_DEPTH} levels deep"),
            );
        }
        let found = self
            .tokens
            .get(self.furthest)
            .or(self.tokens.last())
            .unwrap_or(&EOF);
        if found.r#type == TokenType::Indent {
            return Diagnostic::error(found.span, "unexpected indent");
        }
        let loud: Vec<&str> = self
            .expected
            .iter()
            .filter(|e| !e.quiet)
            .map(|e| e.what.as_str())
            .collect();
        let wanted = if loud.is_empty() {
            self.expected.iter().map(|e| e.what.as_str()).collect()
        } else {
            loud
        };
        let found_text = describe_found(&found.r#type);
        let message = match wanted.split_last() {
            None => format!("unexpected {found_text}"),
            Some((last, [])) => format!("expected {last}, found {found_text}"),
            Some((last, rest)) => {
                format!("expected {} or {last}, found {found_text}", rest.join(", "))
            }
        };
        Diagnostic::error(found.span, message)
    }

    /// Parses all tokens into a module. This is the entry point of the parser.
    ///
    /// ```ignore
    /// let tokens = Lexer::new("x = 42\n").lex()?;
    /// let module = Parser::new(&tokens).parse()?;
    /// ```
    pub fn parse(&mut self) -> Result<Mod, Diagnostic> {
        let (module, diagnostics) = self.parse_recovering();
        match diagnostics.into_iter().next() {
            Some(first) => Err(first),
            None => Ok(module),
        }
    }

    /// Parses all tokens, recovering from syntax errors so that one mistake does not hide the rest
    /// of the file. Each top-level statement that fails to parse is reported and skipped.
    ///
    /// For example, parsing `x = ` then `y = 2` reports the missing expression on line 1 and still
    /// returns the assignment to `y`.
    ///
    /// ```text
    /// program[mod]:
    ///     | [statements] EOF
    /// ```
    pub fn parse_recovering(&mut self) -> (Mod, Vec<Diagnostic>) {
        let mut body = vec![];
        let mut diagnostics = vec![];
        while self.peek().r#type != TokenType::Eof {
            let start = self.pos;
            // report each statement's own furthest failure
            self.furthest = start;
            self.expected.clear();
            self.too_deep = None;

            match self.parse_statement() {
                Ok(stmts) => body.extend(stmts),
                Err(_) => {
                    diagnostics.push(self.syntax_error());
                    self.pos = start;
                    self.skip_statement();
                }
            }
        }
        (Mod::Module { body }, diagnostics)
    }

    /// Skips the top-level statement starting at the current token, including any indented block
    /// after it, so parsing can resume at the next top-level statement.
    fn skip_statement(&mut self) {
        let mut depth = 0usize;
        loop {
            match self.peek().r#type {
                TokenType::Eof => return,
                TokenType::Indent => depth += 1,
                TokenType::Dedent => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        self.pos += 1;
                        return;
                    }
                }
                TokenType::Newline if depth == 0 => {
                    self.pos += 1;
                    // a block may follow, as after `def f();`
                    if self.peek().r#type != TokenType::Indent {
                        return;
                    }
                    continue;
                }
                _ => {}
            }
            self.pos += 1;
        }
    }
}

/// Describes a token a rule expected, such as `';'` or `end of line`.
fn describe_expected(r#type: &TokenType) -> String {
    match r#type {
        TokenType::Name(_) => "a name".to_string(),
        TokenType::Number(_) => "a number".to_string(),
        TokenType::String(_) => "a string".to_string(),
        other => describe_found(other),
    }
}

/// Describes a token that was found, such as `'x'`, `'42'`, or `end of line`.
fn describe_found(r#type: &TokenType) -> String {
    match r#type {
        TokenType::Eof => "end of file".to_string(),
        TokenType::Name(name) => format!("'{name}'"),
        TokenType::Keyword(word) => format!("'{word}'"),
        TokenType::Number(n) => format!("'{n}'"),
        TokenType::String(s) => format!("string \"{s}\""),
        TokenType::Operator(op) => format!("'{op}'"),
        TokenType::Newline => "end of line".to_string(),
        TokenType::Indent => "an indented block".to_string(),
        TokenType::Dedent => "end of block".to_string(),
        TokenType::LParen => "'('".to_string(),
        TokenType::RParen => "')'".to_string(),
        TokenType::LBracket => "'['".to_string(),
        TokenType::RBracket => "']'".to_string(),
        TokenType::Semi => "';'".to_string(),
        TokenType::Comma => "','".to_string(),
        TokenType::Dot => "'.'".to_string(),
    }
}

/// Returns whether expecting this token is quiet, meaning it is optional or one of many ways to
/// continue, such as the `+` that could follow `1` in `print(1`.
///
/// Keywords are quiet because they start optional clauses or alternative statements, except `in`,
/// which a `for` loop always requires.
fn is_quiet(r#type: &TokenType) -> bool {
    if let TokenType::Keyword(word) = r#type {
        return word != "in";
    }
    matches!(
        r#type,
        TokenType::Operator(_)
            | TokenType::LParen
            | TokenType::LBracket
            | TokenType::Dot
            | TokenType::Comma
    )
}
