//! Stone Parser

use std::{collections::HashMap, rc::Rc};

use crate::debug;
use crate::types::{BoolOp, Expr, ExprContext, Operator, Stmt, Token, TokenType, UnaryOp};

/// Parser - converts tokens to AST
pub struct Parser {
    /// using Rc here because number of tokens might be large
    tokens: Rc<[Token]>,
    pos: usize,
    memo: HashMap<(String, usize), Option<Box<Expr>>>,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        Parser {
            tokens: Rc::from(tokens),
            pos: 0,
            memo: HashMap::new(),
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

    fn advance(&mut self) -> &Token {
        dbg!(self.pos);
        self.pos += 1;
        self.peek()
    }

    fn expect(&mut self, target: TokenType) -> Option<&Token> {
        debug!("expecting {:?} at pos {:?}", target, self.pos);
        let tok = self.peek();
        if target != tok.r#type {
            return None;
        }
        Some(self.advance())
    }

    // parser methods

    fn parse_t_lookahead(&mut self) -> bool {
        if self.expect(TokenType::LParen).is_some()
            || self.expect(TokenType::LBracket).is_some()
            || self.expect(TokenType::Dot).is_some()
        {
            dbg!(self.pos);
            self.pos -= 1;
            return true;
        }
        false
    }

    fn parse_t_primary(&mut self) -> Result<Box<Expr>, String> {
        debug!("parse_t_primary: parsing...");
        let start_pos = self.pos;
        let memo_key = ("t_primary".to_string(), start_pos);

        // Check if already memoized
        if let Some(cached) = self.memo.get(&memo_key) {
            if let Some(expr) = cached {
                return Ok(expr.clone());
            } else {
                return Err("Memoized failure".to_string());
            }
        }

        // Seed the memo with a failure to prevent infinite recursion
        self.memo.insert(memo_key.clone(), None);

        let mut result: Option<Box<Expr>> = None;
        let mut last_pos = start_pos;

        // Keep trying until no progress
        loop {
            self.pos = start_pos; // Reset to start position

            match self.parse_t_primary_raw() {
                Ok(expr) => {
                    if self.pos > last_pos {
                        // Made progress, keep going
                        result = Some(expr);
                        last_pos = self.pos;
                    } else {
                        // No progress, we're done
                        break;
                    }
                }
                Err(_) => break,
            }
        }

        // Cache final result
        self.memo.insert(memo_key, result.clone());
        self.pos = last_pos;

        match result {
            Some(expr) => Ok(expr),
            None => Err("parse_t_primary failed".to_string()),
        }
    }

    /// t_primary:
    ///
    /// | `t_primary '[' slices ']' &t_lookahead`
    /// | `t_primary '(' [arguments] ')' &t_lookahead`
    /// | `atom &t_lookahead`
    fn parse_t_primary_raw(&mut self) -> Result<Box<Expr>, String> {
        debug!("parse_t_primary_raw: parsing...");
        // t_primary '[' slices ']' &t_lookahead
        if let Ok(value) = self.parse_t_primary()
            && self.expect(TokenType::LBracket).is_some()
            && let Ok(slice) = self.parse_slices()
            && self.expect(TokenType::RBracket).is_some()
            && self.parse_t_lookahead()
        {
            debug!(
                "parse_t_primary_raw: successfully parsed rule: t_primary '[' slices ']' &t_lookahead"
            );
            return Ok(Box::new(Expr::Subscript {
                value,
                slice,
                ctx: ExprContext::Load,
            }));
        }

        // t_primary '(' [arguments] ')' &t_lookahead
        if let Ok(func) = self.parse_t_primary()
            && self.expect(TokenType::LParen).is_some()
        {
            let args = self.parse_arguments();
            if self.expect(TokenType::RParen).is_some() && self.parse_t_lookahead() {
                debug!(
                    "parse_t_primary_raw: successfully parsed rule: t_primary '(' [arguments] ')' &t_lookahead"
                );
                return Ok(Box::new(Expr::Call {
                    func,
                    args: args.unwrap_or(vec![]),
                }));
            }
        }

        // atom &t_lookahead
        if let Ok(expr) = self.parse_atom()
            && self.parse_t_lookahead()
        {
            debug!("parse_t_primary_raw: successfully parsed rule: atom &t_lookahead");
            return Ok(expr);
        }

        Err(format!("parse_t_primary_raw"))
    }

    fn parse_star_atom(&mut self) -> Result<Box<Expr>, String> {
        if let TokenType::Name(name) = &self.peek().r#type {
            debug!("parse_star_atom: successfully parsed rule: NAME");
            let name = name.clone();
            self.advance();
            return Ok(Box::new(Expr::Name {
                id: name.to_string(),
                ctx: ExprContext::Store,
            }));
        }

        Err(format!("parse_star_atom"))
    }

    /// target_with_star_atom:
    /// | t_primary '[' slices ']' !t_lookahead
    /// | star_atom
    fn parse_target_with_star_atom(&mut self) -> Result<Box<Expr>, String> {
        debug!("parse_target_with_star_atom: parsing...");
        let mark = self.pos;

        // t_primary '[' slices ']' !t_lookahead
        if let Ok(value) = self.parse_t_primary()
            && self.expect(TokenType::LBracket).is_some()
            && let Ok(slice) = self.parse_slices()
            && self.expect(TokenType::RBracket).is_some()
            && !self.parse_t_lookahead()
        {
            debug!(
                "parse_target_with_star_atom: successfully parsed rule: t_primary '[' slices ']' !t_lookahead"
            );
            return Ok(Box::new(Expr::Subscript {
                value,
                slice,
                ctx: ExprContext::Store,
            }));
        }

        // Reset position before trying fallback
        dbg!(self.pos);
        self.pos = mark;
        self.parse_star_atom()
    }

    /// star_target: target_with_star_atom
    fn parse_star_target(&mut self) -> Result<Box<Expr>, String> {
        debug!("parse_star_target: parsing...");
        // target_with_star_atom
        debug!("pos is now {}", self.pos);
        self.parse_target_with_star_atom()
    }

    /// star_targets: star_target !','
    fn parse_star_targets(&mut self) -> Result<Box<Expr>, String> {
        debug!("parse_star_targets: parsing...");
        // star_target !','
        if let Ok(expr) = self.parse_star_target() {
            if self.peek().r#type == TokenType::Comma {
                return Err(format!("parse_star_targets: ?"));
            }

            debug!("parse_star_targets: successfully parsed rule: star_target !','");
            return Ok(expr);
        }

        Err("parse_star_targets".to_string())
    }

    /// args: ','.(expression)+
    fn parse_args(&mut self) -> Result<Vec<Expr>, String> {
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
    fn parse_arguments(&mut self) -> Result<Vec<Expr>, String> {
        let args = self.parse_args()?;
        self.expect(TokenType::Comma);

        if self.peek().r#type == TokenType::RParen {
            return Ok(args);
        }

        Err("parse_arguments".to_string())
    }

    fn parse_strings() -> Result<(), String> {
        todo!()
    }

    /// atom:
    ///     | NAME
    ///     | 'true'
    ///     | 'false'
    ///     | 'none'
    ///     | strings
    ///     | NUMBER
    ///     | list
    fn parse_atom(&mut self) -> Result<Box<Expr>, String> {
        // NAME
        if let TokenType::Name(name) = &self.peek().r#type {
            let name = name.clone();
            self.advance();
            return Ok(Box::new(Expr::Name {
                id: name,
                ctx: ExprContext::Load,
            }));
        }

        // 'true'
        if self
            .expect(TokenType::Keyword("true".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Rc::new("true") as Rc<dyn std::any::Any>,
                kind: None,
            }));
        }

        // 'false'
        if self
            .expect(TokenType::Keyword("false".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Rc::new("false") as Rc<dyn std::any::Any>,
                kind: None,
            }));
        }

        // 'none'
        if self
            .expect(TokenType::Keyword("none".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::Constant {
                value: Rc::new("none") as Rc<dyn std::any::Any>,
                kind: None,
            }));
        }

        // strings

        // NUMBER
        if let TokenType::Number(num) = &self.peek().r#type {
            let num = num.clone();
            self.advance();
            return Ok(Box::new(Expr::Constant {
                value: Rc::new(num),
                kind: None,
            }));
        }

        Err(format!(
            "parse_atom: unexpected token at position {:?}: {:?}",
            self.pos,
            self.peek()
        ))
    }

    fn parse_slice(&mut self) -> Result<Box<Expr>, String> {
        self.parse_expression()
    }

    fn parse_slices(&mut self) -> Result<Box<Expr>, String> {
        if let Ok(expr) = self.parse_slice() {
            if self.peek().r#type == TokenType::Comma {
                return Err("parse_slices".to_string());
            }
            return Ok(expr);
        }
        Err("parse_slices".to_string())
    }

    /// memoized
    fn parse_primary(&mut self) -> Result<Box<Expr>, String> {
        let start_pos = self.pos;
        let memo_key = ("primary".to_string(), start_pos);

        // Check if already memoized
        if let Some(cached) = self.memo.get(&memo_key) {
            if let Some(expr) = cached {
                return Ok(expr.clone());
            } else {
                return Err("Memoized failure".to_string());
            }
        }

        // Seed the memo with a failure to prevent infinite recursion
        self.memo.insert(memo_key.clone(), None);

        let mut result: Option<Box<Expr>> = None;
        let mut last_pos = start_pos;

        // Keep trying until no progress
        loop {
            self.pos = start_pos; // Reset to start position

            match self.parse_primary_raw() {
                Ok(expr) => {
                    if self.pos > last_pos {
                        // Made progress, keep going
                        result = Some(expr);
                        last_pos = self.pos;
                    } else {
                        // No progress, we're done
                        break;
                    }
                }
                Err(_) => break,
            }
        }

        // Cache result
        self.memo.insert(memo_key, result.clone());
        dbg!(self.pos);
        self.pos = last_pos;

        match result {
            Some(expr) => Ok(expr),
            None => Err("parse_primary failed".to_string()),
        }
    }

    fn parse_primary_raw(&mut self) -> Result<Box<Expr>, String> {
        let mark = self.pos;

        // primary '(' [arguments] ')'
        if let Ok(expr) = self.parse_primary() {
            if self.expect(TokenType::LParen).is_some() {
                if let Ok(_args) = self.parse_arguments() {
                    if self.expect(TokenType::RParen).is_some() {
                        return Ok(expr);
                    }
                }
            }
        }
        dbg!(self.pos);
        self.pos = mark;

        // primary '[' slices ']'
        if let Ok(expr) = self.parse_primary() {
            if self.expect(TokenType::LParen).is_some() {
                if let Ok(_args) = self.parse_arguments() {
                    if self.expect(TokenType::RParen).is_some() {
                        return Ok(expr);
                    }
                }
            }
        }
        dbg!(self.pos);
        self.pos = mark;

        // Base case: atom
        self.parse_atom()
    }

    fn parse_factor(&mut self) -> Result<Box<Expr>, String> {
        // '+' factor
        if self.expect(TokenType::Operator("+".to_string())).is_some()
            && let Ok(factor) = self.parse_factor()
        {
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::UnaryAdd,
                operand: factor,
            }));
        }

        // '-' factor
        if self.expect(TokenType::Operator("-".to_string())).is_some()
            && let Ok(factor) = self.parse_factor()
        {
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::UnarySub,
                operand: factor,
            }));
        }

        // factor
        self.parse_primary()
    }

    fn parse_term(&mut self) -> Result<Box<Expr>, String> {
        let start_pos = self.pos;
        let memo_key = ("term".to_string(), start_pos);

        // Check if already memoized
        if let Some(cached) = self.memo.get(&memo_key) {
            if let Some(expr) = cached {
                return Ok(expr.clone());
            } else {
                return Err("Memoized failure".to_string());
            }
        }

        // Seed the memo with a failure to prevent infinite recursion
        self.memo.insert(memo_key.clone(), None);

        let mut result: Option<Box<Expr>> = None;
        let mut last_pos = start_pos;

        // Keep trying until no progress
        loop {
            dbg!(self.pos);
            self.pos = start_pos; // Reset to start position

            match self.parse_term_raw() {
                Ok(expr) => {
                    if self.pos > last_pos {
                        // Made progress, keep going
                        result = Some(expr);
                        last_pos = self.pos;
                    } else {
                        // No progress, we're done
                        break;
                    }
                }
                Err(_) => break,
            }
        }

        // Cache result
        self.memo.insert(memo_key, result.clone());
        dbg!(self.pos);
        self.pos = last_pos;

        match result {
            Some(expr) => Ok(expr),
            None => Err("parse_term failed".to_string()),
        }
    }

    fn parse_term_raw(&mut self) -> Result<Box<Expr>, String> {
        let mark = self.pos;

        // term '*' factor
        if let Ok(sum) = self.parse_term()
            && self.expect(TokenType::Operator("*".to_string())).is_some()
            && let Ok(term) = self.parse_factor()
        {
            return Ok(Box::new(Expr::BinOp {
                op: Operator::Multiply,
                left: sum,
                right: term,
            }));
        }
        dbg!(self.pos);
        self.pos = mark;

        // term '/' factor
        if let Ok(sum) = self.parse_sum()
            && self.expect(TokenType::Operator("/".to_string())).is_some()
            && let Ok(term) = self.parse_factor()
        {
            return Ok(Box::new(Expr::BinOp {
                op: Operator::Divide,
                left: sum,
                right: term,
            }));
        }
        dbg!(self.pos);
        self.pos = mark;

        // factor
        self.parse_factor()
    }

    fn parse_sum(&mut self) -> Result<Box<Expr>, String> {
        let start_pos = self.pos;
        let memo_key = ("sum".to_string(), start_pos);

        // Check if already memoized
        if let Some(cached) = self.memo.get(&memo_key) {
            if let Some(expr) = cached {
                return Ok(expr.clone());
            } else {
                return Err("Memoized failure".to_string());
            }
        }

        // Seed the memo with a failure to prevent infinite recursion
        self.memo.insert(memo_key.clone(), None);

        let mut result: Option<Box<Expr>> = None;
        let mut last_pos = start_pos;

        // Keep trying until no progress
        loop {
            self.pos = start_pos; // Reset to start position

            match self.parse_sum_raw() {
                Ok(expr) => {
                    if self.pos > last_pos {
                        // Made progress, keep going
                        result = Some(expr);
                        last_pos = self.pos;
                    } else {
                        // No progress, we're done
                        break;
                    }
                }
                Err(_) => break,
            }
        }

        // Cache result
        self.memo.insert(memo_key, result.clone());
        self.pos = last_pos;

        match result {
            Some(expr) => Ok(expr),
            None => Err("parse_sum failed".to_string()),
        }
    }

    fn parse_sum_raw(&mut self) -> Result<Box<Expr>, String> {
        let mark = self.pos;

        // sum '+' term
        if let Ok(sum) = self.parse_sum()
            && self.expect(TokenType::Operator("+".to_string())).is_some()
            && let Ok(term) = self.parse_term()
        {
            return Ok(Box::new(Expr::BinOp {
                op: Operator::Add,
                left: sum,
                right: term,
            }));
        }
        self.pos = mark;

        // sum '+' term
        if let Ok(sum) = self.parse_sum()
            && self.expect(TokenType::Operator("-".to_string())).is_some()
            && let Ok(term) = self.parse_term()
        {
            return Ok(Box::new(Expr::BinOp {
                op: Operator::Subtract,
                left: sum,
                right: term,
            }));
        }
        self.pos = mark;

        self.parse_term()
    }

    fn parse_comparison(&mut self) -> Result<Box<Expr>, String> {
        self.parse_sum()
    }

    fn parse_inversion(&mut self) -> Result<Box<Expr>, String> {
        if self.expect(TokenType::Keyword("not".to_string())).is_some() {
            let expr = self.parse_inversion()?;
            return Ok(Box::new(Expr::UnaryOp {
                op: UnaryOp::Not,
                operand: expr,
            }));
        }

        self.parse_comparison()
    }

    fn parse_conjunction(&mut self) -> Result<Box<Expr>, String> {
        let mut values: Vec<Expr> = vec![];
        let expr = self.parse_inversion()?;
        values.push(*expr);

        while self.expect(TokenType::Keyword("and".to_string())).is_some() {
            let expr = self.parse_inversion()?;
            values.push(*expr);
        }

        Ok(Box::new(Expr::BoolOp {
            op: BoolOp::And,
            values,
        }))
    }

    fn parse_disjunction(&mut self) -> Result<Box<Expr>, String> {
        let mut values: Vec<Expr> = vec![];
        let expr = self.parse_conjunction()?;
        values.push(*expr);

        while self.expect(TokenType::Keyword("or".to_string())).is_some() {
            let expr = self.parse_conjunction()?;
            values.push(*expr);
        }

        Ok(Box::new(Expr::BoolOp {
            op: BoolOp::Or,
            values,
        }))
    }

    /// expression:
    /// | disjunction
    fn parse_expression(&mut self) -> Result<Box<Expr>, String> {
        self.parse_disjunction()
    }

    fn parse_expressions(&mut self) -> Result<Box<Expr>, String> {
        self.parse_expression()
    }

    /// return_stmt: 'ret' [expressions]
    fn parse_return_stmt(&mut self) -> Result<Box<Stmt>, String> {
        if self.expect(TokenType::Keyword("ret".to_string())).is_none() {
            return Err(format!(
                "parse_return_stmt: expected 'ret', got {:?}",
                self.peek().r#type
            ));
        }

        if let Ok(value) = self.parse_expressions() {
            return Ok(Box::new(Stmt::Return { value: Some(value) }));
        }

        Ok(Box::new(Stmt::Return { value: None }))
    }

    /// assignment: (star_targets '=' )+ expressions !'='
    fn parse_assignment(&mut self) -> Result<Box<Stmt>, String> {
        let mark = self.pos;

        // (star_targets '=' )+ expressions !'='
        debug!("parse_assignment: trying to parse star_targets");
        if let Ok(target) = self.parse_star_targets()
            && self.expect(TokenType::Operator("=".to_string())).is_some()
        {
            let mut targets: Vec<Expr> = vec![];
            targets.push(*target);

            debug!("parse_assignment: trying to parse star_targets");
            while let Ok(target) = self.parse_star_targets()
                && self.expect(TokenType::Operator("=".to_string())).is_some()
            {
                targets.push(*target);
            }

            debug!("parse_assignment: trying to parse expressions");
            let value = self.parse_expressions()?;
            if self.expect(TokenType::Operator("=".to_string())).is_some() {
                return Err(format!(
                    "parse_assignment: expected '=', got {:?}",
                    self.peek().r#type
                ));
            }

            debug!(
                "parse_assignment: successfully parsed rule: (star_targets '=' )+ expressions !'='"
            );
            return Ok(Box::new(Stmt::Assign { targets, value }));
        }
        self.pos = mark;

        Err(format!("parse_assignment"))
    }

    /// simple_stmt:
    /// | assignment
    /// | expressions
    /// | return_stmt
    /// | 'break'
    /// | 'cont'
    fn parse_simple_stmt(&mut self) -> Result<Box<Stmt>, String> {
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

        Err("parse_simple_stmt: no matching statement".to_string())
    }

    fn parse_while_stmt(&mut self) -> Result<Box<Stmt>, String> {
        Err("".to_string())
    }

    fn parse_for_stmt(&mut self) -> Result<Box<Stmt>, String> {
        Err("".to_string())
    }

    fn parse_if_stmt(&mut self) -> Result<Box<Stmt>, String> {
        Err("".to_string())
    }

    fn parse_function_def(&mut self) -> Result<Box<Stmt>, String> {
        Err("".to_string())
    }

    /// simple_stmts: simple_stmt NEWLINE
    fn parse_simple_stmts(&mut self) -> Result<Box<Stmt>, String> {
        let mark = self.pos;

        // simple_stmt NEWLINE
        debug!("parse_simple_stmts: trying to parse simple_stmt");
        let stmt = self.parse_simple_stmt()?;
        if self.expect(TokenType::Newline).is_none() {
            self.pos = mark;
            return Err(format!(
                "parse_simple_stmts: unexpected token at position {:?}: {:?}",
                self.pos,
                self.peek()
            ));
        }

        debug!("parse_simple_stmts: successfully parsed parse_simple_stmt");
        Ok(stmt)
    }

    /// compound_stmt:
    /// | function_def
    /// | if_stmt
    /// | for_stmt
    /// | while_stmt
    fn parse_compound_stmt(&mut self) -> Result<Box<Stmt>, String> {
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

        Err(format!(
            "parse_compound_stmt: unexpected token: {:?}",
            self.peek()
        ))
    }

    /// statement: compound_stmt | simple_stmts
    fn parse_statement(&mut self) -> Result<Box<Stmt>, String> {
        let mark = self.pos;

        // compound_stmt
        debug!("parse_statement: trying to parse compound_stmt");
        if let Ok(stmt) = self.parse_compound_stmt() {
            debug!("parse_statement: successfully parsed compound_stmt");
            return Ok(stmt);
        }
        self.pos = mark;

        // simple_stmts
        debug!("parse_statement: trying to parse simple_stmts");
        if let Ok(stmt) = self.parse_simple_stmts() {
            debug!("parse_statement: successfully parsed simple_stmts");
            return Ok(stmt);
        }
        self.pos = mark;

        Err(format!(
            "parse_statement: unexpected token: {:?}",
            self.peek()
        ))
    }

    /// statements: statement+
    fn parse_statements(&mut self) -> Result<Vec<Stmt>, String> {
        let mut mark = self.pos;

        // statement+
        let mut statements: Vec<Stmt> = vec![];
        let statement = self.parse_statement()?;
        statements.push(*statement);

        while let Ok(statement) = self.parse_statement() {
            statements.push(*statement);
            mark = self.pos;
        }
        self.pos = mark;

        debug!("parse_statements: successfully parsed statement+");
        Ok(statements)
    }

    /// program: [statements] EOF
    pub fn parse(&mut self) -> Result<Vec<Stmt>, String> {
        let mark = self.pos;

        // [statements] EOF
        let mut stmts = vec![];
        if let Ok(statements) = self.parse_statements() {
            stmts = statements;
        }

        if self.expect(TokenType::EOF).is_none() {
            self.pos = mark;
            return Err(format!("parse: expected EOF, got {:?}", self.peek().r#type));
        }

        Ok(stmts)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_program() {
        let _tokens = r#"
x = 42
y = x + 8
ret y
"#;

        // let tokens = vec![Token {
        //     r#type: TokenType::Identifier("x".to_string()),
        //     line:
        // }];

        // let parser = Parser::new(tokens);
    }

    #[test]
    fn simple_functions() {
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
                r#type: TokenType::Colon,
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
        println!("{:?}", ast);
    }
}
