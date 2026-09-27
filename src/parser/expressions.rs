//! Expression and assignment-target rules, such as `sum`, `primary`, and `star_targets`.

use super::{ParseExprResult, Parser};
use crate::ast::{BoolOp, Constant, Expr, ExprContext, Operator, ParserError, PrimaryOp, UnaryOp};
use crate::debug;
use crate::token::TokenType;

impl Parser {
    /// Returns whether the next token can continue a primary target, such as the `[` in `a[0]`.
    ///
    /// ```text
    /// t_lookahead:
    ///     | '('
    ///     | '['
    ///     | '.'
    /// ```
    pub(super) fn parse_t_lookahead(&mut self) -> bool {
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

    /// Parses the trailing subscripts and calls of a `t_primary`, such as `[0](1)` in `a[0](1)`.
    ///
    /// ```text
    /// ( '[' slices ']' | '(' [arguments] ')' )*
    /// ```
    pub(super) fn parse_t_primary_loop0(&mut self) -> Vec<PrimaryOp> {
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

    /// Parses the primary part of an assignment target, such as `a[0]` in `a[0][1] = 2`.
    ///
    /// ```text
    /// t_primary:
    ///     | atom ( '[' slices ']' | '(' [arguments] ')' )* &t_lookahead
    /// ```
    pub(super) fn parse_t_primary(&mut self) -> ParseExprResult {
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

    /// Parses a bare name used as an assignment target, such as `x` in `x = 1`.
    ///
    /// ```text
    /// star_atom:
    ///     | NAME
    /// ```
    pub(super) fn parse_star_atom(&mut self) -> ParseExprResult {
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

    /// Parses a single assignment target, which is either a subscript like `a[0]` or a name like `x`.
    ///
    /// ```text
    /// target_with_star_atom:
    ///     | t_primary '[' slices ']' !t_lookahead
    ///     | star_atom
    /// ```
    pub(super) fn parse_target_with_star_atom(&mut self) -> ParseExprResult {
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

    /// Parses one assignment target, such as `x` or `a[0]`.
    ///
    /// ```text
    /// star_target: target_with_star_atom
    /// ```
    pub(super) fn parse_star_target(&mut self) -> ParseExprResult {
        debug!("parse_star_target: parsing...");
        // target_with_star_atom
        self.parse_target_with_star_atom()
    }

    /// Parses the target on the left side of an assignment, such as `x` in `x = 1`.
    ///
    /// ```text
    /// star_targets: star_target !','
    /// ```
    pub(super) fn parse_star_targets(&mut self) -> ParseExprResult {
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

    /// Parses a comma-separated list of call arguments, such as `1, x + 2` in `f(1, x + 2)`.
    ///
    /// ```text
    /// args:
    ///     | ','.expression+
    /// ```
    pub(super) fn parse_args(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = vec![];

        let expr = self.parse_expression()?;
        args.push(*expr);
        while self.expect(TokenType::Comma).is_some() {
            let expr = self.parse_expression()?;
            args.push(*expr);
        }

        Ok(args)
    }

    /// Parses call arguments with an optional trailing comma, such as `1, 2,` in `f(1, 2,)`.
    ///
    /// ```text
    /// arguments: args [','] &')'
    /// ```
    pub(super) fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
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

    pub(super) fn parse_string(&mut self) -> ParseExprResult {
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

    pub(super) fn parse_strings_loop(&mut self) -> ParseExprResult {
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

        // concatenate all strings
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

    pub(super) fn parse_strings(&mut self) -> ParseExprResult {
        self.parse_strings_loop()
    }

    /// Parses an atom, the smallest unit of an expression, such as `x`, `42`, `true`, or `"hi"`.
    ///
    /// ```text
    /// atom:
    ///     | NAME
    ///     | 'true'
    ///     | 'false'
    ///     | 'none'
    ///     | strings
    ///     | NUMBER
    ///     | list
    /// ```
    pub(super) fn parse_atom(&mut self) -> ParseExprResult {
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

    pub(super) fn parse_slice(&mut self) -> ParseExprResult {
        self.parse_expression()
    }

    pub(super) fn parse_slices(&mut self) -> ParseExprResult {
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

    /// Parses call arguments if there are any, returning an empty list for a call like `f()`.
    ///
    /// ```text
    /// [arguments]
    /// ```
    pub(super) fn parse_arguments_optional(&mut self) -> Vec<Expr> {
        self.parse_arguments().unwrap_or_default()
    }

    /// Parses the calls and subscripts that follow an atom, such as `(1)[0]` in `f(1)[0]`.
    ///
    /// ```text
    /// ( '(' [arguments] ')' | '[' slices ']' )*
    /// ```
    pub(super) fn parse_primary_loop0(&mut self) -> Vec<PrimaryOp> {
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

    /// Parses an atom followed by any calls or subscripts, such as `f(1)` or `a[0]`.
    ///
    /// ```text
    /// primary:
    ///     | atom ( '(' [arguments] ')' | '[' slices ']' )*
    /// ```
    pub(super) fn parse_primary(&mut self) -> ParseExprResult {
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

    /// Parses a primary with optional unary signs, such as `-x` or `+5`.
    ///
    /// ```text
    /// factor:
    ///     | '+' factor
    ///     | '-' factor
    ///     | primary
    /// ```
    pub(super) fn parse_factor(&mut self) -> ParseExprResult {
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

    /// Parses the repeated `*` and `/` operations of a term, such as `* b / c` in `a * b / c`.
    ///
    /// ```text
    /// ('*'|'/' factor)*
    /// ```
    pub(super) fn parse_term_loop0(&mut self) -> Vec<(Operator, Expr)> {
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

    /// Parses multiplication and division, such as `a * b / c`.
    ///
    /// ```text
    /// term:
    ///     | factor ('*'|'/' factor)*
    /// ```
    pub(super) fn parse_term(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // factor ('*'|'/' factor)*
        if let Ok(mut result) = self.parse_factor()
            && let facts = self.parse_term_loop0()
        {
            if facts.is_empty() {
                return Ok(result);
            }

            // left-associative, e.g. ((a + b) - c)
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

    /// Parses the repeated `+` and `-` operations of a sum, such as `+ b - c` in `a + b - c`.
    ///
    /// ```text
    /// ('+'|'-' term)*
    /// ```
    pub(super) fn parse_sum_loop0(&mut self) -> Vec<(Operator, Expr)> {
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

    /// Parses addition and subtraction, such as `a + b - c`.
    ///
    /// ```text
    /// sum:
    ///     | term ('+'|'-' term)*
    /// ```
    pub(super) fn parse_sum(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // term ('+'|'-' term)*
        if let Ok(mut result) = self.parse_term()
            && let sums = self.parse_sum_loop0()
        {
            if sums.is_empty() {
                return Ok(result);
            }

            // left-associative, e.g. ((a + b) - c)
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

    pub(super) fn parse_comparison(&mut self) -> ParseExprResult {
        self.parse_sum()
    }

    /// Parses an optional logical `not`, such as `not x`.
    ///
    /// ```text
    /// inversion:
    ///     | 'not' inversion
    ///     | comparison
    /// ```
    pub(super) fn parse_inversion(&mut self) -> ParseExprResult {
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

    /// Parses the repeated `and` operands of a conjunction, such as `and b and c` in `a and b and c`.
    ///
    /// ```text
    /// ('and' inversion )+
    /// ```
    pub(super) fn parse_conjunction_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
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

    /// Parses a logical `and` chain, such as `a and b`.
    ///
    /// ```text
    /// conjunction:
    ///     | inversion ('and' inversion )+
    ///     | inversion
    /// ```
    pub(super) fn parse_conjunction(&mut self) -> ParseExprResult {
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

    /// Parses the repeated `or` operands of a disjunction, such as `or b or c` in `a or b or c`.
    ///
    /// ```text
    /// ('or' conjunction )+
    /// ```
    pub(super) fn parse_disjunction_loop(&mut self) -> Result<Vec<Expr>, ParserError> {
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

    /// Parses a logical `or` chain, such as `a or b`.
    ///
    /// ```text
    /// disjunction:
    ///     | conjunction ('or' conjunction )+
    ///     | conjunction
    /// ```
    pub(super) fn parse_disjunction(&mut self) -> ParseExprResult {
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

    /// Parses a single expression, such as `x + 1 or y`.
    ///
    /// ```text
    /// expression:
    ///     | disjunction
    /// ```
    pub(super) fn parse_expression(&mut self) -> ParseExprResult {
        self.parse_disjunction()
    }

    /// Parses one or more comma-separated expressions, such as `1, 2`.
    ///
    /// ```text
    /// expressions:
    ///     | expression (',' expression )+ [',']
    ///     | expression ','
    ///     | expression
    /// ```
    pub(super) fn parse_expressions(&mut self) -> ParseExprResult {
        self.parse_expression()
    }
}
