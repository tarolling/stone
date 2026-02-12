//! Stone Parser

use std::rc::Rc;

use crate::debug;
use crate::types::{
    Arg, Arguments, BoolOp, Constant, Expr, ExprContext, Mod, Operator, ParserError, PrimaryOp,
    Stmt, Token, TokenType, UnaryOp,
};

type ParseExprResult = Result<Box<Expr>, ParserError>;

/// Parser - converts tokens to AST
pub struct Parser {
    /// using Rc here because number of tokens might be large
    tokens: Rc<[Token]>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Parser {
            tokens: Rc::from(tokens),
            pos: 0,
        }
    }

    // helpers

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token {
            r#type: TokenType::EOF,
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

    // parser methods

    /// t_lookahead:
    /// | '('
    /// | '['
    /// | '.'
    fn parse_t_lookahead(&mut self) -> bool {
        let mark = self.pos;

        // '('
        if self.expect(TokenType::LParen).is_some() {
            self.pos = mark;
            return true;
        }

        // '['
        if self.expect(TokenType::LBracket).is_some() {
            self.pos = mark;
            return true;
        }

        // '.'
        if self.expect(TokenType::Dot).is_some() {
            self.pos = mark;
            return true;
        }

        false
    }

    /// ( '[' slices ']' | '(' [arguments] ')' )*
    fn parse_t_primary_loop0(&mut self) -> Vec<PrimaryOp> {
        let mut results = vec![];

        loop {
            let mark = self.pos;

            // '[' slices ']'
            if self.expect(TokenType::LBracket).is_some()
                && let Ok(slices) = self.parse_slices()
                && self.expect(TokenType::RBracket).is_some()
            {
                results.push(PrimaryOp::Subscript(slices));
                continue;
            }
            self.pos = mark;

            // '(' [arguments] ')'
            if self.expect(TokenType::LParen).is_some()
                && let arguments = self.parse_arguments_optional()
                && self.expect(TokenType::RParen).is_some()
            {
                results.push(PrimaryOp::Call(arguments));
                continue;
            }

            self.pos = mark;
            break;
        }
        results
    }

    /// t_primary:
    ///     | atom ( '[' slices ']' | '(' [arguments] ')' )* &t_lookahead
    fn parse_t_primary(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // atom ( '[' slices ']' | '(' [arguments] ')' )* &t_lookahead
        if let Ok(mut result) = self.parse_atom()
            && let ops = self.parse_t_primary_loop0()
            && self.parse_t_lookahead()
        {
            if ops.is_empty() {
                return Ok(result);
            }

            for op in ops {
                result = match op {
                    PrimaryOp::Subscript(slice) => Box::new(Expr::Subscript {
                        value: result,
                        slice,
                        ctx: ExprContext::Load,
                    }),
                    PrimaryOp::Call(args) => Box::new(Expr::Call { func: result, args }),
                };
            }

            debug!("parse_t_primary: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_t_primary".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// star_atom:
    /// | NAME
    fn parse_star_atom(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // NAME
        if let TokenType::Name(name) = &self.advance().r#type {
            debug!("parse_star_atom: successfully parsed rule: NAME");
            return Ok(Box::new(Expr::Name {
                id: name.to_string(),
                ctx: ExprContext::Store,
            }));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_star_atom".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// target_with_star_atom:
    /// | t_primary '[' slices ']' !t_lookahead
    /// | star_atom
    fn parse_target_with_star_atom(&mut self) -> ParseExprResult {
        debug!("parse_target_with_star_atom: parsing...");
        let mark = self.pos;

        // t_primary '[' slices ']' !t_lookahead
        if let Ok(value) = self.parse_t_primary()
            && self.expect(TokenType::LBracket).is_some()
            && let Ok(slice) = self.parse_slices()
            && self.expect(TokenType::RBracket).is_some()
            && !self.parse_t_lookahead()
        {
            let res = Box::new(Expr::Subscript {
                value,
                slice,
                ctx: ExprContext::Store,
            });
            debug!(
                "parse_target_with_star_atom: successfully parsed: {:?}",
                res
            );
            return Ok(res);
        }
        self.pos = mark;
        debug!("parse_target_with_star_atom: reset pos to {}", self.pos);

        // star_atom
        self.parse_star_atom()
    }

    /// star_target: target_with_star_atom
    fn parse_star_target(&mut self) -> ParseExprResult {
        debug!("parse_star_target: parsing...");
        // target_with_star_atom
        self.parse_target_with_star_atom()
    }

    /// star_targets: star_target !','
    fn parse_star_targets(&mut self) -> ParseExprResult {
        debug!("parse_star_targets: parsing...");
        // star_target !','
        if let Ok(expr) = self.parse_star_target() {
            if self.peek().r#type == TokenType::Comma {
                return Err(ParserError {
                    method: "parse_star_targets".to_string(),
                    token: self.peek().r#type.clone(),
                    line: self.peek().line,
                    col: self.peek().col,
                });
            }

            debug!("parse_star_targets: successfully parsed: {:?}", expr);
            return Ok(expr);
        }

        Err(ParserError {
            method: "parse_star_targets".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// args:
    /// | ','.expression+
    fn parse_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![];

        let expr = self.parse_expression()?;
        args.push(*expr);
        while self.expect(TokenType::Comma).is_some() {
            let expr = self.parse_expression()?;
            args.push(*expr);
        }

        Ok(args)
    }

    /// arguments: args [','] &')'
    fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let args = self.parse_args()?;
        self.expect(TokenType::Comma);

        if self.peek().r#type == TokenType::RParen {
            return Ok(args);
        }

        Err(ParserError {
            method: "parse_arguments".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn parse_string(&mut self) -> ParseExprResult {
        if let TokenType::String(value) = self.advance().r#type {
            return Ok(Box::new(Expr::Constant {
                value: Box::new(Constant::Str(value.to_string())),
                kind: None,
            }));
        }

        Err(ParserError {
            method: "parse_string".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn parse_strings_loop(&mut self) -> ParseExprResult {
        let mut results = vec![];

        let expr = self.parse_string()?;
        results.push(expr);

        loop {
            let mark = self.pos;
            if let Ok(expr) = self.parse_string() {
                results.push(expr);
            } else {
                self.pos = mark;
                break;
            }
        }

        // Concatenate all strings
        let mut concatenated = String::new();
        for expr in results {
            if let Expr::Constant { value, .. } = *expr
                && let Constant::Str(s) = *value
            {
                concatenated.push_str(&s);
            }
        }

        Ok(Box::new(Expr::Constant {
            value: Box::new(Constant::Str(concatenated)),
            kind: None,
        }))
    }

    fn parse_strings(&mut self) -> ParseExprResult {
        self.parse_strings_loop()
    }

    /// atom:
    ///     | NAME
    ///     | 'true'
    ///     | 'false'
    ///     | 'none'
    ///     | strings
    ///     | NUMBER
    ///     | list
    fn parse_atom(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // NAME
        if let TokenType::Name(name) = self.advance().r#type {
            return Ok(Box::new(Expr::Name {
                id: name.clone(),
                ctx: ExprContext::Load,
            }));
        }
        self.pos = mark;

        // 'true'
        if self
            .expect(TokenType::Keyword("true".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Box::new(Constant::Bool(true)),
                kind: None,
            }));
        }
        self.pos = mark;

        // 'false'
        if self
            .expect(TokenType::Keyword("false".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Box::new(Constant::Bool(false)),
                kind: None,
            }));
        }
        self.pos = mark;

        // 'none'
        if self
            .expect(TokenType::Keyword("none".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Box::new(Constant::None),
                kind: None,
            }));
        }
        self.pos = mark;

        // strings
        if let Ok(expr) = self.parse_strings() {
            return Ok(expr);
        }
        self.pos = mark;

        // NUMBER
        if let TokenType::Number(num) = self.advance().r#type {
            let res = Box::new(Expr::Constant {
                value: Box::new(Constant::Int(num)),
                kind: None,
            });
            debug!("parse_atom: successfully parsed: {:?}", res);
            return Ok(res);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_atom".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn parse_slice(&mut self) -> ParseExprResult {
        self.parse_expression()
    }

    fn parse_slices(&mut self) -> ParseExprResult {
        if let Ok(expr) = self.parse_slice() {
            if self.peek().r#type == TokenType::Comma {
                return Err(ParserError {
                    method: "parse_slices".to_string(),
                    token: self.peek().r#type.clone(),
                    line: self.peek().line,
                    col: self.peek().col,
                });
            }
            return Ok(expr);
        }
        Err(ParserError {
            method: "parse_slices".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// [arguments]
    fn parse_arguments_optional(&mut self) -> Vec<Expr> {
        self.parse_arguments().unwrap_or_default()
    }

    /// ( '(' [arguments] ')' | '[' slices ']' )*
    fn parse_primary_loop0(&mut self) -> Vec<PrimaryOp> {
        let mut results: Vec<PrimaryOp> = vec![];
        loop {
            let mark = self.pos;

            // '(' [arguments] ')'
            if self.expect(TokenType::LParen).is_some()
                && let arguments = self.parse_arguments_optional()
                && self.expect(TokenType::RParen).is_some()
            {
                results.push(PrimaryOp::Call(arguments));
                continue;
            }
            self.pos = mark;

            // '[' slices ']'
            if self.expect(TokenType::LBracket).is_some()
                && let Ok(slices) = self.parse_slices()
                && self.expect(TokenType::RBracket).is_some()
            {
                results.push(PrimaryOp::Subscript(slices));
                continue;
            }
            self.pos = mark;
            break;
        }
        results
    }

    /// primary:
    /// | atom ( '(' [arguments] ')' | '[' slices ']' )*
    fn parse_primary(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // atom ( '(' [arguments] ')' | '[' slices ']' )*
        if let Ok(mut result) = self.parse_atom()
            && let ops = self.parse_primary_loop0()
        {
            if ops.is_empty() {
                return Ok(result);
            }

            for op in ops {
                result = match op {
                    PrimaryOp::Subscript(slice) => Box::new(Expr::Subscript {
                        value: result,
                        slice,
                        ctx: ExprContext::Load,
                    }),
                    PrimaryOp::Call(args) => Box::new(Expr::Call { func: result, args }),
                };
            }

            debug!("parse_primary: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_primary".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// factor:
    ///     | '+' factor
    ///     | '-' factor
    ///     | primary
    fn parse_factor(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // '+' factor
        if self.expect(TokenType::Operator("+".to_string())).is_some()
            && let Ok(factor) = self.parse_factor()
        {
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::UnaryAdd,
                operand: factor,
            }));
        }
        self.pos = mark;

        // '-' factor
        if self.expect(TokenType::Operator("-".to_string())).is_some()
            && let Ok(factor) = self.parse_factor()
        {
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::UnarySub,
                operand: factor,
            }));
        }
        self.pos = mark;

        // factor
        self.parse_primary()
    }

    /// ('*'|'/' factor)*
    fn parse_term_loop0(&mut self) -> Vec<(Operator, Expr)> {
        let mut results: Vec<(Operator, Expr)> = vec![];
        loop {
            let mark = self.pos;

            if self.expect(TokenType::Operator("*".to_string())).is_some()
                && let Ok(term) = self.parse_factor()
            {
                results.push((Operator::Multiply, *term));
                continue;
            }
            self.pos = mark;

            if self.expect(TokenType::Operator("/".to_string())).is_some()
                && let Ok(term) = self.parse_factor()
            {
                results.push((Operator::Divide, *term));
                continue;
            }

            self.pos = mark;
            break;
        }
        results
    }

    /// term:
    /// | factor ('*'|'/' factor)*
    fn parse_term(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // factor ('*'|'/' factor)*
        if let Ok(mut result) = self.parse_factor()
            && let facts = self.parse_term_loop0()
        {
            if facts.is_empty() {
                return Ok(result);
            }

            // Build the tree left-to-right: ((a + b) - c)
            for (op, expr) in facts {
                result = Box::new(Expr::BinOp {
                    op,
                    left: result,
                    right: Box::new(expr),
                });
            }

            debug!("parse_term: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_term".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    /// ('+'|'-' term)*
    fn parse_sum_loop0(&mut self) -> Vec<(Operator, Expr)> {
        let mut results: Vec<(Operator, Expr)> = vec![];
        loop {
            let mark = self.pos;

            if self.expect(TokenType::Operator("+".to_string())).is_some()
                && let Ok(term) = self.parse_term()
            {
                results.push((Operator::Add, *term));
                continue;
            }
            self.pos = mark;

            if self.expect(TokenType::Operator("-".to_string())).is_some()
                && let Ok(term) = self.parse_term()
            {
                results.push((Operator::Subtract, *term));
                continue;
            }

            self.pos = mark;
            break;
        }
        results
    }

    /// sum:
    /// | term ('+'|'-' term)*
    fn parse_sum(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // term ('+'|'-' term)*
        if let Ok(mut result) = self.parse_term()
            && let sums = self.parse_sum_loop0()
        {
            if sums.is_empty() {
                return Ok(result);
            }

            // Build the tree left-to-right: ((a + b) - c)
            for (op, expr) in sums {
                result = Box::new(Expr::BinOp {
                    op,
                    left: result,
                    right: Box::new(expr),
                });
            }

            debug!("parse_sum: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_sum".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    fn parse_comparison(&mut self) -> ParseExprResult {
        self.parse_sum()
    }

    /// inversion:
    /// | 'not' inversion
    /// | comparison
    fn parse_inversion(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // 'not' inversion
        if self.expect(TokenType::Keyword("not".to_string())).is_some()
            && let Ok(res) = self.parse_inversion()
        {
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: res,
            }));
        }
        self.pos = mark;

        // comparison
        self.parse_comparison()
    }

    /// ('and' inversion )+
    fn parse_conjunction_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut results: Vec<Expr> = vec![];
        loop {
            let mark = self.pos;
            if self.expect(TokenType::Keyword("and".to_string())).is_some()
                && let Ok(conj) = self.parse_inversion()
            {
                results.push(*conj);
            } else {
                self.pos = mark;

                break;
            }
        }

        if results.is_empty() {
            Err(ParserError {
                method: "parse_conjunction_loop".to_string(),
                token: self.peek().r#type.clone(),
                line: self.peek().line,
                col: self.peek().col,
            })
        } else {
            Ok(results)
        }
    }

    /// conjunction:
    /// | inversion ('and' inversion )+
    /// | inversion
    fn parse_conjunction(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // inversion ('and' inversion )+
        if let Ok(conj) = self.parse_inversion()
            && let Ok(exprs) = self.parse_conjunction_loop()
        {
            let mut values = vec![*conj];
            values.extend(exprs);

            return Ok(Box::new(Expr::BoolOp {
                op: BoolOp::And,
                values,
            }));
        }
        self.pos = mark;

        // inversion
        self.parse_inversion()
    }

    /// ('or' conjunction )+
    fn parse_disjunction_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut results: Vec<Expr> = vec![];
        loop {
            let mark = self.pos;
            if self.expect(TokenType::Keyword("or".to_string())).is_some()
                && let Ok(conj) = self.parse_conjunction()
            {
                results.push(*conj);
            } else {
                self.pos = mark;
                break;
            }
        }

        if results.is_empty() {
            Err(ParserError {
                method: "parse_disjunction_loop".to_string(),
                token: self.peek().r#type.clone(),
                line: self.peek().line,
                col: self.peek().col,
            })
        } else {
            Ok(results)
        }
    }

    /// disjunction:
    /// | conjunction ('or' conjunction )+
    /// | conjunction
    fn parse_disjunction(&mut self) -> ParseExprResult {
        // conjunction ('or' conjunction )+
        let mark = self.pos;
        if let Ok(conj) = self.parse_conjunction()
            && let Ok(exprs) = self.parse_disjunction_loop()
        {
            let mut values = vec![*conj];
            values.extend(exprs);

            return Ok(Box::new(Expr::BoolOp {
                op: BoolOp::Or,
                values,
            }));
        }
        self.pos = mark;

        // conjunction
        self.parse_conjunction()
    }

    /// expression:
    /// | disjunction
    fn parse_expression(&mut self) -> ParseExprResult {
        self.parse_disjunction()
    }

    /// expressions:
    /// | expression (',' expression )+ [',']
    /// | expression ','
    /// | expression
    fn parse_expressions(&mut self) -> ParseExprResult {
        self.parse_expression()
    }

    /// for_stmt:
    /// | 'for' star_targets 'in' ~ expressions ';' block
    fn parse_for_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        Err(ParserError {
            method: "parse_for_stmt".to_string(),
            token: self.peek().r#type.clone(),
            line: self.peek().line,
            col: self.peek().col,
        })
    }

    ////////////////////////////////////////////////////////////////
    // WHILE STATEMENT
    ////////////////////////////////////////////////////////////////

    /// while_stmt:
    /// | 'while' expression ';' block
    fn parse_while_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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
    // IF STATEMENT
    ////////////////////////////////////////////////////////////////

    /// else_block:
    /// | 'else' ';' block
    fn parse_else_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
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

    /// elif_stmt:
    /// | 'elif' expression ';' block elif_stmt
    /// | 'elif' expression ';' block [else_block]
    fn parse_elif_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// if_stmt:
    /// | 'if' expression ';' block elif_stmt
    /// | 'if' expression ';' block [else_block]
    fn parse_if_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// param:
    /// | NAME
    fn parse_param(&mut self) -> Result<Arg, ParserError> {
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

    /// parameters:
    /// | ','.param+
    fn parse_parameters(&mut self) -> Result<Arguments, ParserError> {
        let mut args = vec![];

        let expr = self.parse_param()?;
        args.push(expr);
        while self.expect(TokenType::Comma).is_some() {
            let expr = self.parse_param()?;
            args.push(expr);
        }

        Ok(Arguments { args })
    }

    /// [parameters]
    fn parse_function_def_optional(&mut self) -> Arguments {
        match self.parse_parameters() {
            Ok(args) => args,
            Err(_) => Arguments { args: vec![] },
        }
    }

    /// function_def:
    /// | 'def' NAME '(' [params] ')' ';' block
    fn parse_function_def(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// block:
    /// | NEWLINE INDENT statements DEDENT
    /// | simple_stmts
    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
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

    fn parse_return_stmt_optional(&mut self) -> Option<Box<Expr>> {
        self.parse_expressions().ok()
    }

    /// return_stmt: 'ret' [expressions]
    fn parse_return_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// (star_targets '=' )+
    fn parse_assignment_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
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

    /// assignment: (star_targets '=' )+ expressions !'='
    fn parse_assignment(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// compound_stmt:
    /// | function_def
    /// | if_stmt
    /// | for_stmt
    /// | while_stmt
    fn parse_compound_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// simple_stmt:
    /// | assignment
    /// | expressions
    /// | return_stmt
    /// | 'break'
    /// | 'cont'
    fn parse_simple_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
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

    /// simple_stmts: simple_stmt NEWLINE
    fn parse_simple_stmts(&mut self) -> Result<Vec<Stmt>, ParserError> {
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

    /// statement: compound_stmt | simple_stmts
    fn parse_statement(&mut self) -> Result<Vec<Stmt>, ParserError> {
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

    /// statements: statement+
    fn parse_statements(&mut self) -> Result<Vec<Stmt>, ParserError> {
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

    fn parse_statements_optional(&mut self) -> Vec<Stmt> {
        self.parse_statements().unwrap_or_default()
    }

    /// program[mod]:
    /// | [statements] EOF
    fn parse_program(&mut self) -> Result<Mod, ParserError> {
        let mark = self.pos;

        // [statements] EOF
        if let res = self.parse_statements_optional()
            && self.expect(TokenType::EOF).is_some()
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

    /// entrypoint
    pub fn parse(&mut self) -> Result<Mod, ParserError> {
        self.parse_program()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// x = 42
    /// y = x + 8
    /// ret y
    #[test]
    fn simple_program() {
        let _tokens = r#"
x = 42
y = x + 8
ret y
"#;

        let tokens = vec![
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 1,
                col: 1,
            },
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 1,
                col: 3,
            },
            Token {
                r#type: TokenType::Number(42),
                line: 1,
                col: 5,
            },
            Token {
                r#type: TokenType::Newline,
                line: 1,
                col: 7,
            },
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 2,
                col: 1,
            },
            Token {
                r#type: TokenType::Operator("=".to_string()),
                line: 2,
                col: 3,
            },
            Token {
                r#type: TokenType::Name("x".to_string()),
                line: 2,
                col: 5,
            },
            Token {
                r#type: TokenType::Operator("+".to_string()),
                line: 2,
                col: 7,
            },
            Token {
                r#type: TokenType::Number(8),
                line: 2,
                col: 9,
            },
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 10,
            },
            Token {
                r#type: TokenType::Keyword("ret".to_string()),
                line: 3,
                col: 1,
            },
            Token {
                r#type: TokenType::Name("y".to_string()),
                line: 3,
                col: 5,
            },
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 6,
            },
            Token {
                r#type: TokenType::EOF,
                line: 4,
                col: 1,
            },
        ];

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(Mod::Module {
                body: vec![
                    Stmt::Assign {
                        targets: vec![Expr::Name {
                            id: "x".to_string(),
                            ctx: ExprContext::Store
                        }],
                        value: Box::new(Expr::Constant {
                            value: Box::new(Constant::Int(42)),
                            kind: None
                        })
                    },
                    Stmt::Assign {
                        targets: vec![Expr::Name {
                            id: "y".to_string(),
                            ctx: ExprContext::Store
                        }],
                        value: Box::new(Expr::BinOp {
                            op: Operator::Add,
                            left: Box::new(Expr::Name {
                                id: "x".to_string(),
                                ctx: ExprContext::Load
                            }),
                            right: Box::new(Expr::Constant {
                                value: Box::new(Constant::Int(8)),
                                kind: None
                            })
                        })
                    },
                    Stmt::Return {
                        value: Some(Box::new(Expr::Name {
                            id: "y".to_string(),
                            ctx: ExprContext::Load
                        }))
                    }
                ]
            })
        );
    }

    /// def testing():
    ///     ret 52
    /// testing()
    #[test]
    fn simple_function_no_args() {
        let tokens = vec![
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
                r#type: TokenType::RParen,
                line: 1,
                col: 13,
            },
            Token {
                r#type: TokenType::Semi,
                line: 1,
                col: 14,
            },
            Token {
                r#type: TokenType::Newline,
                line: 1,
                col: 15,
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
                r#type: TokenType::Number(52),
                line: 2,
                col: 9,
            },
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 11,
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
                r#type: TokenType::RParen,
                line: 3,
                col: 9,
            },
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 10,
            },
            Token {
                r#type: TokenType::EOF,
                line: 4,
                col: 1,
            },
        ];

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(Mod::Module {
                body: vec![
                    Stmt::FunctionDef {
                        name: "testing".to_string(),
                        args: Arguments { args: vec![] },
                        body: vec![Stmt::Return {
                            value: Some(Box::new(Expr::Constant {
                                value: Box::new(Constant::Int(52)),
                                kind: None
                            }))
                        }]
                    },
                    Stmt::Expr {
                        value: Box::new(Expr::Call {
                            func: Box::new(Expr::Name {
                                id: "testing".to_string(),
                                ctx: ExprContext::Load
                            }),
                            args: vec![]
                        })
                    },
                ]
            })
        );
    }

    /// def testing(a):
    ///     ret a + 2
    /// testing(4)
    #[test]
    fn simple_function_one_arg() {
        let tokens = vec![
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
                r#type: TokenType::RParen,
                line: 1,
                col: 14,
            },
            Token {
                r#type: TokenType::Semi,
                line: 1,
                col: 15,
            },
            Token {
                r#type: TokenType::Newline,
                line: 1,
                col: 16,
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
                r#type: TokenType::Name("a".to_string()),
                line: 2,
                col: 9,
            },
            Token {
                r#type: TokenType::Operator("+".to_string()),
                line: 2,
                col: 11,
            },
            Token {
                r#type: TokenType::Number(2),
                line: 2,
                col: 13,
            },
            Token {
                r#type: TokenType::Newline,
                line: 2,
                col: 14,
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
                r#type: TokenType::Number(4),
                line: 3,
                col: 9,
            },
            Token {
                r#type: TokenType::RParen,
                line: 3,
                col: 10,
            },
            Token {
                r#type: TokenType::Newline,
                line: 3,
                col: 11,
            },
            Token {
                r#type: TokenType::EOF,
                line: 4,
                col: 1,
            },
        ];

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(Mod::Module {
                body: vec![
                    Stmt::FunctionDef {
                        name: "testing".to_string(),
                        args: Arguments {
                            args: vec![Arg {
                                arg: "a".to_string()
                            }]
                        },
                        body: vec![Stmt::Return {
                            value: Some(Box::new(Expr::BinOp {
                                op: Operator::Add,
                                left: Box::new(Expr::Name {
                                    id: "a".to_string(),
                                    ctx: ExprContext::Load
                                }),
                                right: Box::new(Expr::Constant {
                                    value: Box::new(Constant::Int(2)),
                                    kind: None
                                })
                            }))
                        }]
                    },
                    Stmt::Expr {
                        value: Box::new(Expr::Call {
                            func: Box::new(Expr::Name {
                                id: "testing".to_string(),
                                ctx: ExprContext::Load
                            }),
                            args: vec![Expr::Constant {
                                value: Box::new(Constant::Int(4)),
                                kind: None
                            }]
                        })
                    },
                ]
            })
        );
    }

    /// def testing(a, b, c):
    ///     ret b
    /// testing(1, 2, 3)
    #[test]
    fn simple_function_multiple_args() {
        let tokens = vec![
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
                r#type: TokenType::EOF,
                line: 4,
                col: 1,
            },
        ];

        let mut parser = Parser::new(&tokens);
        let ast = parser.parse();
        assert_eq!(
            ast,
            Ok(Mod::Module {
                body: vec![
                    Stmt::FunctionDef {
                        name: "testing".to_string(),
                        args: Arguments {
                            args: vec![
                                Arg {
                                    arg: "a".to_string()
                                },
                                Arg {
                                    arg: "b".to_string()
                                },
                                Arg {
                                    arg: "c".to_string()
                                }
                            ]
                        },
                        body: vec![Stmt::Return {
                            value: Some(Box::new(Expr::Name {
                                id: "b".to_string(),
                                ctx: ExprContext::Load
                            }))
                        }]
                    },
                    Stmt::Expr {
                        value: Box::new(Expr::Call {
                            func: Box::new(Expr::Name {
                                id: "testing".to_string(),
                                ctx: ExprContext::Load
                            }),
                            args: vec![
                                Expr::Constant {
                                    value: Box::new(Constant::Int(1)),
                                    kind: None
                                },
                                Expr::Constant {
                                    value: Box::new(Constant::Int(2)),
                                    kind: None
                                },
                                Expr::Constant {
                                    value: Box::new(Constant::Int(3)),
                                    kind: None
                                }
                            ]
                        })
                    },
                ]
            })
        );
    }
}
