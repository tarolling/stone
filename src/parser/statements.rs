//! Statement rules, such as `if_stmt`, `function_def`, and `program`.

use super::Parser;
use crate::ast::{Arg, Arguments, Expr, Mod, ParserError, Stmt};
use crate::debug;
use crate::token::TokenType;

impl Parser {
    /// Parses a `for` loop, such as `for i in 10; print(i)`.
    ///
    /// This is not implemented yet, so it always returns an error.
    ///
    /// ```text
    /// for_stmt:
    ///     | 'for' star_targets 'in' ~ expressions ';' block
    /// ```
    pub(super) fn parse_for_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        Err(ParserError {
            method: "parse_for_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
        // 'while' expression ';' block
        if self
            .expect(TokenType::Keyword("while".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            return Ok(Box::new(Stmt::While { test, body }));
        }
        Err(ParserError {
            method: "parse_while_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
            line: self.peek().line,
            col: self.peek().col,
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

        // 'elif' expression ';' block elif_stmt
        if self
            .expect(TokenType::Keyword("elif".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
            && let Ok(orelse) = self.parse_elif_stmt()
        {
            return Ok(Box::new(Stmt::If {
                test,
                body,
                orelse: vec![*orelse],
            }));
        }
        self.pos = mark;

        // 'elif' expression ';' block [else_block]
        if self
            .expect(TokenType::Keyword("elif".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            if let Ok(orelse) = self.parse_else_block() {
                return Ok(Box::new(Stmt::If { test, body, orelse }));
            }

            return Ok(Box::new(Stmt::If {
                test,
                body,
                orelse: vec![],
            }));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_elif_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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

        // 'if' expression ';' block elif_stmt
        if self.expect(TokenType::Keyword("if".to_string())).is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
            && let Ok(orelse) = self.parse_elif_stmt()
        {
            return Ok(Box::new(Stmt::If {
                test,
                body,
                orelse: vec![*orelse],
            }));
        }
        self.pos = mark;

        // 'if' expression ';' block [else_block]
        if self.expect(TokenType::Keyword("if".to_string())).is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            if let Ok(orelse) = self.parse_else_block() {
                return Ok(Box::new(Stmt::If { test, body, orelse }));
            } else {
                return Ok(Box::new(Stmt::If {
                    test,
                    body,
                    orelse: vec![],
                }));
            }
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_if_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
        if let TokenType::Name(arg) = self.advance().r#type {
            return Ok(Arg { arg });
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_param".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
        while self.expect(TokenType::Comma).is_some() {
            let expr = self.parse_param()?;
            args.push(expr);
        }

        Ok(Arguments { args })
    }

    /// Parses function parameters if there are any, returning an empty list for `def f();`.
    ///
    /// ```text
    /// [parameters]
    /// ```
    pub(super) fn parse_function_def_optional(&mut self) -> Arguments {
        match self.parse_parameters() {
            Ok(args) => args,
            Err(_) => Arguments { args: vec![] },
        }
    }

    /// Parses a function definition, such as `def add(a, b); ret a + b`.
    ///
    /// ```text
    /// function_def:
    ///     | 'def' NAME '(' [params] ')' ';' block
    /// ```
    pub(super) fn parse_function_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'def' NAME '(' [params] ')' ';' block
        if self.expect(TokenType::Keyword("def".to_string())).is_some()
            && let TokenType::Name(name) = self.advance().r#type
            && self.expect(TokenType::LParen).is_some()
            && let args = self.parse_function_def_optional()
            && self.expect(TokenType::RParen).is_some()
            && self.expect(TokenType::Semi).is_some()
            && let Ok(body) = self.parse_block()
        {
            return Ok(Box::new(Stmt::FunctionDef { name, args, body }));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_function_def".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
            && let Ok(stmts) = self.parse_statements()
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
        // 'ret' [expressions]
        if self.expect(TokenType::Keyword("ret".to_string())).is_some()
            && let value = self.parse_return_stmt_optional()
        {
            return Ok(Box::new(Stmt::Return { value }));
        }

        Err(ParserError {
            method: "parse_return_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
                line: self.peek().line,
                col: self.peek().col,
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
            let res = Box::new(Stmt::Assign { targets, value });
            debug!("parse_assignment: successfully parsed: {:?}", res);
            return Ok(res);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_assignment".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
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
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// Parses a statement without a block, such as `x = 1`, `print(x)`, or `ret x`.
    ///
    /// ```text
    /// simple_stmt:
    ///     | assignment
    ///     | expressions
    ///     | return_stmt
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
            return Ok(Box::new(Stmt::Expr { value: expr }));
        }
        self.pos = mark;

        // return_stmt
        debug!("parse_simple_stmt: trying to parse return_stmt");
        if let Ok(stmt) = self.parse_return_stmt() {
            debug!("parse_simple_stmt: successfully parsed return_stmt");
            return Ok(stmt);
        }
        self.pos = mark;

        // 'break'
        if self
            .expect(TokenType::Keyword("break".to_string()))
            .is_some()
        {
            debug!("parse_simple_stmt: successfully parsed 'break'");
            return Ok(Box::new(Stmt::Break));
        }
        self.pos = mark;

        // 'cont'
        if self
            .expect(TokenType::Keyword("cont".to_string()))
            .is_some()
        {
            debug!("parse_simple_stmt: successfully parsed 'cont'");
            return Ok(Box::new(Stmt::Continue));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_simple_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
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
            line: self.peek().line,
            col: self.peek().col,
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
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// Parses one or more statements in sequence.
    ///
    /// ```text
    /// statements: statement+
    /// ```
    pub(super) fn parse_statements(&mut self) -> Result<Vec<Stmt>, ParserError> {
        // statement+
        let mut results: Vec<Stmt> = vec![];
        let statement = self.parse_statement()?;
        results.extend(statement);

        loop {
            let mark = self.pos;
            if let Ok(stmts) = self.parse_statement() {
                results.extend(stmts);
            } else {
                self.pos = mark;

                break;
            }
        }

        debug!("parse_statements: successfully parsed statement+");
        Ok(results)
    }

    pub(super) fn parse_statements_optional(&mut self) -> Vec<Stmt> {
        self.parse_statements().unwrap_or_default()
    }

    /// Parses a whole program up to the end of the file.
    ///
    /// ```text
    /// program[mod]:
    ///     | [statements] EOF
    /// ```
    pub(super) fn parse_program(&mut self) -> Result<Mod, ParserError> {
        let mark = self.pos;

        // [statements] EOF
        if let res = self.parse_statements_optional()
            && self.expect(TokenType::Eof).is_some()
        {
            return Ok(Mod::Module { body: res });
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_program".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }
}
