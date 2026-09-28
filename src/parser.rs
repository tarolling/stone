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
use crate::token::{Token, TokenType};

type ParseExprResult = Result<Box<Expr>, ParserError>;

/// Maximum nesting of expressions and blocks before parsing fails, keeping recursion off the end
/// of the stack.
///
/// For example, an expression with more than `MAX_DEPTH` unary minus signs in a row, like
/// `------1`, is rejected.
pub(crate) const MAX_DEPTH: usize = 200;

/// Parser that converts a token stream into a [`Mod`].
pub struct Parser {
    /// Tokens being parsed, shared through `Rc` because the stream can be large.
    tokens: Rc<[Token]>,
    pos: usize,
    /// Current nesting of expressions and blocks, bounded by [`MAX_DEPTH`].
    depth: usize,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Parser {
            tokens: Rc::from(tokens),
            pos: 0,
            depth: 0,
        }
    }

    // helpers

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token {
            r#type: TokenType::Eof,
            line: 0,
            col: 0,
        })
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
            return None;
        }
        debug!("Success! Advancing position...");
        self.pos += 1;
        Some(tok)
    }

    /// Runs a rule one nesting level deeper, failing once nesting exceeds [`MAX_DEPTH`].
    ///
    /// For example, `self.nested(Self::parse_factor)` parses the operand of a unary minus.
    fn nested<T>(
        &mut self,
        rule: impl FnOnce(&mut Self) -> Result<T, ParserError>,
    ) -> Result<T, ParserError> {
        if self.depth >= MAX_DEPTH {
            return Err(ParserError {
                method: "nested".to_string(),
                token: self.peek().r#type.clone(),
                line: self.peek().line,
                col: self.peek().col,
            });
        }
        self.depth += 1;
        let result = rule(self);
        self.depth -= 1;
        result
    }

    /// Parses all tokens into a module. This is the entry point of the parser.
    ///
    /// ```ignore
    /// let tokens = Lexer::new("x = 42\n").lex()?;
    /// let module = Parser::new(&tokens).parse()?;
    /// ```
    pub fn parse(&mut self) -> Result<Mod, ParserError> {
        self.parse_program()
    }
}
