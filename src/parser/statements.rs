//! Statement rules, such as `if_stmt`, `function_def`, and `program`.

use super::Parser;
use crate::ast::{Arg, Arguments, Expr, ParserError, Stmt, StmtKind};
use crate::debug;
use crate::span::Span;
use crate::token::TokenType;

impl Parser {
    /// Parses a `for` loop, such as `for i in range(3); print(i)`.
    ///
    /// ```text
    /// for_stmt:
    ///     | 'for' star_targets 'in' ~ expressions ';' block
    /// ```
    pub(super) fn parse_for_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'for' star_targets 'in' ~ expressions ';' block
        if self.expect(TokenType::Keyword("for".to_string())).is_some()
            && let Ok(target) = self.parse_star_targets()
            && self.expect(TokenType::Keyword("in".to_string())).is_some()
            && let Ok(iter) = self.parse_expressions()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.nested(Self::parse_block)
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::For { target, iter, body },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_for_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    ////////////////////////////////////////////////////////////////
    // while statement
    ////////////////////////////////////////////////////////////////

    /// Parses a `while` loop, such as `while x; x = x - 1`.
    ///
    /// ```text
    /// while_stmt:
    ///     | 'while' expression ';' block
    /// ```
    pub(super) fn parse_while_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'while' expression ';' block
        if self
            .expect(TokenType::Keyword("while".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::While { test, body },
                self.span_from(mark),
            )));
        }
        Err(ParserError {
            method: "parse_while_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    ////////////////////////////////////////////////////////////////
    // if statement
    ////////////////////////////////////////////////////////////////

    /// Parses the `else` branch of an `if` statement, such as `else; print(x)`.
    ///
    /// ```text
    /// else_block:
    ///     | 'else' ';' block
    /// ```
    pub(super) fn parse_else_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mark = self.pos;

        // 'else' ';' block
        if self
            .expect(TokenType::Keyword("else".to_string()))
            .is_some()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(block) = self.parse_block()
        {
            return Ok(block);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_else_block".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses an `elif` branch and any branches after it, such as `elif x; print(x)`.
    ///
    /// ```text
    /// elif_stmt:
    ///     | 'elif' expression ';' block elif_stmt
    ///     | 'elif' expression ';' block [else_block]
    /// ```
    pub(super) fn parse_elif_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // both alternatives share this prefix, so parse it once instead of backtracking
        if self
            .expect(TokenType::Keyword("elif".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            let after_body = self.pos;

            // ... elif_stmt
            if let Ok(orelse) = self.parse_elif_stmt() {
                return Ok(Box::new(Stmt::new(
                    StmtKind::If {
                        test,
                        body,
                        orelse: vec![*orelse],
                    },
                    self.span_from(mark),
                )));
            }
            self.pos = after_body;

            // ... [else_block]
            let orelse = self.parse_else_block().unwrap_or_else(|_| {
                self.pos = after_body;
                vec![]
            });
            return Ok(Box::new(Stmt::new(
                StmtKind::If { test, body, orelse },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_elif_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses an `if` statement with its `elif` and `else` branches, such as `if x; print(x)`.
    ///
    /// ```text
    /// if_stmt:
    ///     | 'if' expression ';' block elif_stmt
    ///     | 'if' expression ';' block [else_block]
    /// ```
    pub(super) fn parse_if_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // both alternatives share this prefix, so parse it once instead of backtracking
        if self.expect(TokenType::Keyword("if".to_string())).is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            let after_body = self.pos;

            // ... elif_stmt
            if let Ok(orelse) = self.parse_elif_stmt() {
                return Ok(Box::new(Stmt::new(
                    StmtKind::If {
                        test,
                        body,
                        orelse: vec![*orelse],
                    },
                    self.span_from(mark),
                )));
            }
            self.pos = after_body;

            // ... [else_block]
            let orelse = self.parse_else_block().unwrap_or_else(|_| {
                self.pos = after_body;
                vec![]
            });
            return Ok(Box::new(Stmt::new(
                StmtKind::If { test, body, orelse },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_if_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one function parameter, such as `a` in `def f(a, b);`.
    ///
    /// ```text
    /// param:
    ///     | NAME
    /// ```
    pub(super) fn parse_param(&mut self) -> Result<Arg, ParserError> {
        let mark = self.pos;

        // NAME
        if let Some((arg, span)) = self.expect_name("a parameter name", false) {
            return Ok(Arg { arg, span });
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_param".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a comma-separated parameter list, such as `a, b` in `def f(a, b);`.
    ///
    /// ```text
    /// parameters:
    ///     | ','.param+
    /// ```
    pub(super) fn parse_parameters(&mut self) -> Result<Arguments, ParserError> {
        let mut args = vec![];

        let expr = self.parse_param()?;
        args.push(expr);
        loop {
            let mark = self.pos;
            if self.expect(TokenType::Comma).is_some()
                && let Ok(expr) = self.parse_param()
            {
                args.push(expr);
                continue;
            }
            self.pos = mark;
            break;
        }

        Ok(Arguments { args })
    }

    /// Parses function parameters if there are any, returning an empty list for `def f();`.
    ///
    /// ```text
    /// [parameters]
    /// ```
    pub(super) fn parse_function_def_optional(&mut self) -> Arguments {
        let mark = self.pos;
        self.parse_parameters().unwrap_or_else(|_| {
            self.pos = mark;
            Arguments { args: vec![] }
        })
    }

    /// Parses a function definition, such as `def add(a, b); ret a + b` or `pub def f(); ret 1`.
    ///
    /// ```text
    /// function_def:
    ///     | ['pub'] 'def' NAME '(' [params] ')' ';' block
    /// ```
    pub(super) fn parse_function_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // ['pub'] 'def' NAME '(' [params] ')' ';' block
        let public = self.expect(TokenType::Keyword("pub".to_string())).is_some();
        if self.expect(TokenType::Keyword("def".to_string())).is_some()
            && let Some((name, name_span)) = self.expect_name("a function name", false)
            && self.expect(TokenType::LParen).is_some()
            && let args = self.parse_function_def_optional()
            && self.expect(TokenType::RParen).is_some()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::FunctionDef {
                    name,
                    name_span,
                    public,
                    args,
                    body,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_function_def".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the body of a compound statement, either as an indented block or on the same line.
    ///
    /// ```text
    /// block:
    ///     | NEWLINE INDENT statements DEDENT
    ///     | simple_stmts
    /// ```
    pub(super) fn parse_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mark = self.pos;

        // NEWLINE INDENT statements DEDENT
        if self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(stmts) = self.nested(Self::parse_statements)
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(stmts);
        }
        self.pos = mark;

        // simple_stmts
        self.parse_simple_stmts()
    }

    pub(super) fn parse_return_stmt_optional(&mut self) -> Option<Box<Expr>> {
        self.parse_expressions().ok()
    }

    /// Parses a return statement, such as `ret x + 1`.
    ///
    /// ```text
    /// return_stmt: 'ret' [expressions]
    /// ```
    pub(super) fn parse_return_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'ret' [expressions]
        if self.expect(TokenType::Keyword("ret".to_string())).is_some()
            && let value = self.parse_return_stmt_optional()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::Return { value },
                self.span_from(mark),
            )));
        }

        Err(ParserError {
            method: "parse_return_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the chain of targets in an assignment, such as `a = b =` in `a = b = 1`.
    ///
    /// ```text
    /// (star_targets '=' )+
    /// ```
    pub(super) fn parse_assignment_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut results: Vec<Expr> = vec![];
        loop {
            let mark = self.pos;
            if let Ok(res) = self.parse_star_targets()
                && self.expect(TokenType::Operator("=".to_string())).is_some()
            {
                results.push(*res);
            } else {
                self.pos = mark;
                break;
            }
        }

        if results.is_empty() {
            Err(ParserError {
                method: "parse_assignment_loop".to_string(),
                token: self.peek().r#type.clone(),
                span: self.peek().span,
            })
        } else {
            Ok(results)
        }
    }

    /// Parses an assignment, such as `x = 42` or `a = b = 1`.
    ///
    /// ```text
    /// assignment: (star_targets '=' )+ expressions !'='
    /// ```
    pub(super) fn parse_assignment(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // (star_targets '=' )+ expressions !'='
        debug!("parse_assignment: trying to parse star_targets");
        if let Ok(targets) = self.parse_assignment_loop()
            && let Ok(value) = self.parse_expressions()
            && self.expect(TokenType::Operator("=".to_string())).is_none()
        {
            let res = Box::new(Stmt::new(
                StmtKind::Assign { targets, value },
                self.span_from(mark),
            ));
            debug!("parse_assignment: successfully parsed: {:?}", res);
            return Ok(res);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_assignment".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses an import, such as `use geometry.shapes` or `use util.pad as lpad`.
    ///
    /// ```text
    /// use_stmt: 'use' NAME ('.' NAME)* ['as' NAME]
    /// ```
    pub(super) fn parse_use_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'use' NAME ('.' NAME)* ['as' NAME]
        if self.expect(TokenType::Keyword("use".to_string())).is_some()
            && let Some(first) = self.expect_name("a module name", false)
            && let Some(path) = self.parse_use_stmt_loop(first)
            && let Some(alias) = self.parse_use_stmt_alias()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::Use { path, alias },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_use_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the rest of a `use` path after its first name, failing on a dot with no name after.
    ///
    /// ```text
    /// ('.' NAME)*
    /// ```
    fn parse_use_stmt_loop(&mut self, first: (String, Span)) -> Option<Vec<(String, Span)>> {
        let mut path = vec![first];
        while self.expect(TokenType::Dot).is_some() {
            path.push(self.expect_name("a module name", false)?);
        }
        Some(path)
    }

    /// Parses the optional `as` and name that rename an import, failing on `as` with no name.
    ///
    /// ```text
    /// ['as' NAME]
    /// ```
    fn parse_use_stmt_alias(&mut self) -> Option<Option<(String, Span)>> {
        if self.expect(TokenType::Keyword("as".to_string())).is_none() {
            return Some(None);
        }
        self.expect_name("a name", false).map(Some)
    }

    /// Parses a statement that contains a block, such as a function definition or an `if` statement.
    ///
    /// ```text
    /// compound_stmt:
    ///     | function_def
    ///     | if_stmt
    ///     | for_stmt
    ///     | while_stmt
    /// ```
    pub(super) fn parse_compound_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // function_def
        if let Ok(stmt) = self.parse_function_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // if_stmt
        if let Ok(stmt) = self.parse_if_stmt() {
            return Ok(stmt);
        }
        self.pos = mark;

        // for_stmt
        if let Ok(stmt) = self.parse_for_stmt() {
            return Ok(stmt);
        }
        self.pos = mark;

        // while_stmt
        if let Ok(stmt) = self.parse_while_stmt() {
            return Ok(stmt);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_compound_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a statement without a block, such as `x = 1`, `print(x)`, or `ret x`.
    ///
    /// ```text
    /// simple_stmt:
    ///     | assignment
    ///     | expressions
    ///     | return_stmt
    ///     | use_stmt
    ///     | 'break'
    ///     | 'cont'
    /// ```
    pub(super) fn parse_simple_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // assignment
        debug!("parse_simple_stmt: trying to parse assignment");
        if let Ok(stmt) = self.parse_assignment() {
            debug!("parse_simple_stmt: successfully parsed assignment");
            return Ok(stmt);
        }
        self.pos = mark;

        // expressions
        debug!("parse_simple_stmt: trying to parse expressions");
        if let Ok(expr) = self.parse_expressions() {
            debug!("parse_simple_stmt: successfully parsed expressions");
            return Ok(Box::new(Stmt::new(
                StmtKind::Expr { value: expr },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // return_stmt
        debug!("parse_simple_stmt: trying to parse return_stmt");
        if let Ok(stmt) = self.parse_return_stmt() {
            debug!("parse_simple_stmt: successfully parsed return_stmt");
            return Ok(stmt);
        }
        self.pos = mark;

        // use_stmt
        if let Ok(stmt) = self.parse_use_stmt() {
            return Ok(stmt);
        }
        self.pos = mark;

        // 'break'
        if self
            .expect(TokenType::Keyword("break".to_string()))
            .is_some()
        {
            debug!("parse_simple_stmt: successfully parsed 'break'");
            return Ok(Box::new(Stmt::new(StmtKind::Break, self.span_from(mark))));
        }
        self.pos = mark;

        // 'cont'
        if self
            .expect(TokenType::Keyword("cont".to_string()))
            .is_some()
        {
            debug!("parse_simple_stmt: successfully parsed 'cont'");
            return Ok(Box::new(Stmt::new(
                StmtKind::Continue,
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_simple_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a simple statement followed by a newline, such as `x = 1`.
    ///
    /// ```text
    /// simple_stmts: simple_stmt NEWLINE
    /// ```
    pub(super) fn parse_simple_stmts(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mark = self.pos;

        // simple_stmt NEWLINE
        debug!("parse_simple_stmts: trying to parse simple_stmt");
        if let Ok(stmt) = self.parse_simple_stmt()
            && self.expect(TokenType::Newline).is_some()
        {
            debug!("parse_simple_stmts: successfully parsed parse_simple_stmt");
            return Ok(vec![*stmt]);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_simple_stmts".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one statement of either kind, such as `x = 1` or `if x; print(x)`.
    ///
    /// ```text
    /// statement: compound_stmt | simple_stmts
    /// ```
    pub(super) fn parse_statement(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mark = self.pos;

        // compound_stmt
        debug!("parse_statement: trying to parse compound_stmt");
        if let Ok(stmt) = self.parse_compound_stmt() {
            debug!("parse_statement: successfully parsed compound_stmt");
            return Ok(vec![*stmt]);
        }
        self.pos = mark;

        // simple_stmts
        debug!("parse_statement: trying to parse simple_stmts");
        if let Ok(stmt) = self.parse_simple_stmts() {
            debug!("parse_statement: successfully parsed simple_stmts");
            return Ok(stmt);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_statement".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the statements of an indented block, up to the `Dedent` that ends it.
    ///
    /// A statement that fails to parse is reported and skipped, along with any block under it, so
    /// one mistake in a function body keeps the rest of the function. The block may end up empty.
    ///
    /// ```text
    /// statements: statement+
    /// ```
    pub(super) fn parse_statements(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut results: Vec<Stmt> = vec![];
        while !matches!(self.peek().r#type, TokenType::Dedent | TokenType::Eof) {
            let start = self.pos;
            match self.parse_recoverable_statement() {
                Some(stmts) => results.extend(stmts),
                // no progress is possible, so leave the error to the enclosing rule
                None if self.pos == start => break,
                None => {}
            }
        }

        debug!("parse_statements: successfully parsed statement*");
        Ok(results)
    }
}
