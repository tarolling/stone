//! Expression and assignment-target rules, such as `sum`, `primary`, and `star_targets`.

use super::{ParseExprResult, Parser};
use crate::ast::{
    BoolOp, CompOp, Constant, Expr, ExprContext, ExprKind, Operator, ParserError, PrimaryOp,
    UnaryOp,
};
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

    /// Parses the trailing subscripts, calls, and attributes of a `t_primary`, such as `[0](1)` in
    /// `a[0](1)[2]`, stopping before the last one so that the assignment target can claim it.
    ///
    /// The grammar's `t_primary` is left-recursive, so it naturally ends right before a final
    /// subscript. Here each subscript or call is only taken if another one follows it, which gives
    /// the same result: in `a[0][1] = 2`, this takes `[0]` and leaves `[1]` for the target.
    ///
    /// ```text
    /// ( '[' slices ']' | '(' [arguments] ')' | '.' NAME )*
    /// ```
    pub(super) fn parse_t_primary_loop0(&mut self) -> Vec<PrimaryOp> {
        let mut results = vec![];

        loop {
            let mark = self.pos;

            // '[' slices ']'
            if self.expect(TokenType::LBracket).is_some()
                && let Ok(slices) = self.parse_slices()
                && self.expect(TokenType::RBracket).is_some()
                && self.parse_t_lookahead()
            {
                results.push(PrimaryOp::Subscript(slices, self.last_span()));
                continue;
            }
            self.pos = mark;

            // '(' [arguments] ')'
            if self.expect(TokenType::LParen).is_some()
                && let arguments = self.parse_arguments_optional()
                && self.expect(TokenType::RParen).is_some()
                && self.parse_t_lookahead()
            {
                results.push(PrimaryOp::Call(arguments, self.last_span()));
                continue;
            }
            self.pos = mark;

            // '.' NAME
            if self.expect(TokenType::Dot).is_some()
                && let Some((attr, span)) = self.expect_name("a method name", false)
                && self.parse_t_lookahead()
            {
                results.push(PrimaryOp::Attribute(attr, span));
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
    ///     | atom ( '[' slices ']' | '(' [arguments] ')' | '.' NAME )* &t_lookahead
    /// ```
    pub(super) fn parse_t_primary(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // atom ( '[' slices ']' | '(' [arguments] ')' | '.' NAME )* &t_lookahead
        if let Ok(mut result) = self.parse_atom()
            && let ops = self.parse_t_primary_loop0()
            && self.parse_t_lookahead()
        {
            if ops.is_empty() {
                return Ok(result);
            }

            let start = result.span;
            for op in ops {
                result = match op {
                    PrimaryOp::Subscript(slice, end) => Box::new(Expr::new(
                        ExprKind::Subscript {
                            value: result,
                            slice,
                            ctx: ExprContext::Load,
                        },
                        start.to(end),
                    )),
                    PrimaryOp::Call(args, end) => Box::new(Expr::new(
                        ExprKind::Call { func: result, args },
                        start.to(end),
                    )),
                    PrimaryOp::Attribute(attr, end) => Box::new(Expr::new(
                        ExprKind::Attribute {
                            value: result,
                            attr,
                            ctx: ExprContext::Load,
                        },
                        start.to(end),
                    )),
                };
            }

            debug!("parse_t_primary: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_t_primary".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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

        // NAME, quiet because the expression alternatives at the same token already cover names
        if let Some((id, span)) = self.expect_name("a name", true) {
            debug!("parse_star_atom: successfully parsed rule: NAME");
            return Ok(Box::new(Expr::new(
                ExprKind::Name {
                    id,
                    ctx: ExprContext::Store,
                },
                span,
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_star_atom".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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
            let res = Box::new(Expr::new(
                ExprKind::Subscript {
                    value,
                    slice,
                    ctx: ExprContext::Store,
                },
                self.span_from(mark),
            ));
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
                    span: self.peek().span,
                });
            }

            debug!("parse_star_targets: successfully parsed: {:?}", expr);
            return Ok(expr);
        }

        Err(ParserError {
            method: "parse_star_targets".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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
        loop {
            let mark = self.pos;
            // a comma with no expression after it belongs to `arguments` as a trailing comma
            if self.expect(TokenType::Comma).is_some()
                && let Ok(expr) = self.parse_expression()
            {
                args.push(*expr);
                continue;
            }
            self.pos = mark;
            break;
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

        // &')'
        if self.peek().r#type == TokenType::RParen {
            return Ok(args);
        }
        self.record_expected("')'".to_string(), false);

        Err(ParserError {
            method: "parse_arguments".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    pub(super) fn parse_string(&mut self) -> ParseExprResult {
        let mark = self.pos;
        if let TokenType::String(value) = self.advance().r#type {
            return Ok(Box::new(Expr::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Str(value.to_string())),
                    kind: None,
                },
                self.span_from(mark),
            )));
        }

        Err(ParserError {
            method: "parse_string".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    pub(super) fn parse_strings_loop(&mut self) -> ParseExprResult {
        let start = self.pos;
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
            if let ExprKind::Constant { value, .. } = expr.kind
                && let Constant::Str(s) = *value
            {
                concatenated.push_str(&s);
            }
        }

        Ok(Box::new(Expr::new(
            ExprKind::Constant {
                value: Box::new(Constant::Str(concatenated)),
                kind: None,
            },
            self.span_from(start),
        )))
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
    ///     | group
    /// ```
    pub(super) fn parse_atom(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // group
        if let Ok(expr) = self.parse_group() {
            return Ok(expr);
        }
        self.pos = mark;

        // list
        if let Ok(expr) = self.parse_list() {
            return Ok(expr);
        }
        self.pos = mark;

        // NAME
        if let TokenType::Name(name) = self.advance().r#type {
            return Ok(Box::new(Expr::new(
                ExprKind::Name {
                    id: name.clone(),
                    ctx: ExprContext::Load,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // 'true'
        if self
            .expect(TokenType::Keyword("true".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Bool(true)),
                    kind: None,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // 'false'
        if self
            .expect(TokenType::Keyword("false".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Bool(false)),
                    kind: None,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // 'none'
        if self
            .expect(TokenType::Keyword("none".to_string()))
            .is_some()
        {
            return Ok(Box::new(Expr::new(
                ExprKind::Constant {
                    value: Box::new(Constant::None),
                    kind: None,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // strings
        if let Ok(expr) = self.parse_strings() {
            return Ok(expr);
        }
        self.pos = mark;

        // NUMBER
        let value = match self.advance().r#type {
            TokenType::Number(num) => Some(Constant::Int(num)),
            TokenType::Float(x) => Some(Constant::Float(x)),
            _ => None,
        };
        if let Some(value) = value {
            let res = Box::new(Expr::new(
                ExprKind::Constant {
                    value: Box::new(value),
                    kind: None,
                },
                self.span_from(mark),
            ));
            debug!("parse_atom: successfully parsed: {:?}", res);
            return Ok(res);
        }
        self.pos = mark;
        self.record_expected("an expression".to_string(), false);

        Err(ParserError {
            method: "parse_atom".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a parenthesized expression, such as `(1 + 2)` in `(1 + 2) * 3`. The expression's span
    /// grows to cover the parentheses.
    ///
    /// ```text
    /// group:
    ///     | '(' expression ')'
    /// ```
    pub(super) fn parse_group(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // '(' expression ')'
        if self.expect(TokenType::LParen).is_some()
            && let Ok(mut expr) = self.parse_expression()
            && self.expect(TokenType::RParen).is_some()
        {
            expr.span = self.span_from(mark);
            return Ok(expr);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_group".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a list literal, such as `[1, 2]` or `[]`.
    ///
    /// ```text
    /// list:
    ///     | '[' [named_expressions] ']'
    /// named_expressions:
    ///     | ','.expression+ [',']
    /// ```
    pub(super) fn parse_list(&mut self) -> ParseExprResult {
        let mark = self.pos;

        if self.expect(TokenType::LBracket).is_some() {
            let mut elts = vec![];
            if let Ok(first) = self.parse_expression() {
                elts.push(*first);
                loop {
                    let before_comma = self.pos;
                    if self.expect(TokenType::Comma).is_some()
                        && let Ok(elt) = self.parse_expression()
                    {
                        elts.push(*elt);
                        continue;
                    }
                    // a trailing comma is allowed
                    self.pos = before_comma;
                    self.expect(TokenType::Comma);
                    break;
                }
            }
            if self.expect(TokenType::RBracket).is_some() {
                return Ok(Box::new(Expr::new(
                    ExprKind::List {
                        elts,
                        ctx: ExprContext::Load,
                    },
                    self.span_from(mark),
                )));
            }
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_list".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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
                    span: self.peek().span,
                });
            }
            return Ok(expr);
        }
        Err(ParserError {
            method: "parse_slices".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses call arguments if there are any, returning an empty list for a call like `f()`.
    ///
    /// ```text
    /// [arguments]
    /// ```
    pub(super) fn parse_arguments_optional(&mut self) -> Vec<Expr> {
        let mark = self.pos;
        self.parse_arguments().unwrap_or_else(|_| {
            self.pos = mark;
            vec![]
        })
    }

    /// Parses the calls, subscripts, and attributes that follow an atom, such as `(1)[0]` in
    /// `f(1)[0]` or `.len()` in `xs.len()`.
    ///
    /// ```text
    /// ( '(' [arguments] ')' | '[' slices ']' | '.' NAME )*
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
                results.push(PrimaryOp::Call(arguments, self.last_span()));
                continue;
            }
            self.pos = mark;

            // '[' slices ']'
            if self.expect(TokenType::LBracket).is_some()
                && let Ok(slices) = self.parse_slices()
                && self.expect(TokenType::RBracket).is_some()
            {
                results.push(PrimaryOp::Subscript(slices, self.last_span()));
                continue;
            }
            self.pos = mark;

            // '.' NAME
            if self.expect(TokenType::Dot).is_some()
                && let Some((attr, span)) = self.expect_name("a method name", false)
            {
                results.push(PrimaryOp::Attribute(attr, span));
                continue;
            }
            self.pos = mark;
            break;
        }
        results
    }

    /// Parses an atom followed by any calls, subscripts, or attributes, such as `f(1)`, `a[0]`, or
    /// `xs.len()`.
    ///
    /// ```text
    /// primary:
    ///     | atom ( '(' [arguments] ')' | '[' slices ']' | '.' NAME )*
    /// ```
    pub(super) fn parse_primary(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // atom ( '(' [arguments] ')' | '[' slices ']' | '.' NAME )*
        if let Ok(mut result) = self.parse_atom()
            && let ops = self.parse_primary_loop0()
        {
            if ops.is_empty() {
                return Ok(result);
            }

            let start = result.span;
            for op in ops {
                result = match op {
                    PrimaryOp::Subscript(slice, end) => Box::new(Expr::new(
                        ExprKind::Subscript {
                            value: result,
                            slice,
                            ctx: ExprContext::Load,
                        },
                        start.to(end),
                    )),
                    PrimaryOp::Call(args, end) => Box::new(Expr::new(
                        ExprKind::Call { func: result, args },
                        start.to(end),
                    )),
                    PrimaryOp::Attribute(attr, end) => Box::new(Expr::new(
                        ExprKind::Attribute {
                            value: result,
                            attr,
                            ctx: ExprContext::Load,
                        },
                        start.to(end),
                    )),
                };
            }

            debug!("parse_primary: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_primary".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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
            && let Ok(factor) = self.nested(Self::parse_factor)
        {
            return Ok(Box::new(Expr::new(
                ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd,
                    operand: factor,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        // '-' factor
        if self.expect(TokenType::Operator("-".to_string())).is_some()
            && let Ok(factor) = self.nested(Self::parse_factor)
        {
            return Ok(Box::new(Expr::new(
                ExprKind::UnaryOp {
                    op: UnaryOp::UnarySub,
                    operand: factor,
                },
                self.span_from(mark),
            )));
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
                let span = result.span.to(expr.span);
                result = Box::new(Expr::new(
                    ExprKind::BinOp {
                        op,
                        left: result,
                        right: Box::new(expr),
                    },
                    span,
                ));
            }

            debug!("parse_term: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_term".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
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
                let span = result.span.to(expr.span);
                result = Box::new(Expr::new(
                    ExprKind::BinOp {
                        op,
                        left: result,
                        right: Box::new(expr),
                    },
                    span,
                ));
            }

            debug!("parse_sum: successfully parsed: {:?}", result);
            return Ok(result);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_sum".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one comparison operator, such as `<=`.
    ///
    /// ```text
    /// compare_op:
    ///     | '==' | '!=' | '<' | '<=' | '>' | '>='
    /// ```
    pub(super) fn parse_compare_op(&mut self) -> Option<CompOp> {
        let ops = [
            ("==", CompOp::Equal),
            ("!=", CompOp::NotEqual),
            ("<", CompOp::LessThan),
            ("<=", CompOp::LessThanEqual),
            (">", CompOp::GreaterThan),
            (">=", CompOp::GreaterThanEqual),
        ];
        ops.into_iter()
            .find(|(text, _)| self.expect(TokenType::Operator(text.to_string())).is_some())
            .map(|(_, op)| op)
    }

    /// Parses a sum or a chain of comparisons between sums, such as `a < b <= c`, which means
    /// `a < b and b <= c`.
    ///
    /// ```text
    /// comparison:
    ///     | sum (compare_op sum)*
    /// ```
    pub(super) fn parse_comparison(&mut self) -> ParseExprResult {
        let left = self.parse_sum()?;

        let mut ops = vec![];
        let mut comparators: Vec<Expr> = vec![];
        loop {
            let mark = self.pos;
            if let Some(op) = self.parse_compare_op()
                && let Ok(right) = self.parse_sum()
            {
                ops.push(op);
                comparators.push(*right);
                continue;
            }
            self.pos = mark;
            break;
        }

        let Some(last) = comparators.last() else {
            return Ok(left);
        };
        let span = left.span.to(last.span);
        Ok(Box::new(Expr::new(
            ExprKind::Compare {
                left,
                ops,
                comparators,
            },
            span,
        )))
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
            && let Ok(res) = self.nested(Self::parse_inversion)
        {
            return Ok(Box::new(Expr::new(
                ExprKind::UnaryOp {
                    op: UnaryOp::Not,
                    operand: res,
                },
                self.span_from(mark),
            )));
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
                span: self.peek().span,
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
        // both alternatives start with inversion, so parse it once instead of backtracking
        let conj = self.parse_inversion()?;

        // inversion ('and' inversion )+
        if let Ok(exprs) = self.parse_conjunction_loop() {
            let span = conj.span.to(exprs.last().map_or(conj.span, |e| e.span));
            let mut values = vec![*conj];
            values.extend(exprs);

            return Ok(Box::new(Expr::new(
                ExprKind::BoolOp {
                    op: BoolOp::And,
                    values,
                },
                span,
            )));
        }

        // inversion
        Ok(conj)
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
                span: self.peek().span,
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
        // both alternatives start with conjunction, so parse it once instead of backtracking
        let conj = self.parse_conjunction()?;

        // conjunction ('or' conjunction )+
        if let Ok(exprs) = self.parse_disjunction_loop() {
            let span = conj.span.to(exprs.last().map_or(conj.span, |e| e.span));
            let mut values = vec![*conj];
            values.extend(exprs);

            return Ok(Box::new(Expr::new(
                ExprKind::BoolOp {
                    op: BoolOp::Or,
                    values,
                },
                span,
            )));
        }

        // conjunction
        Ok(conj)
    }

    /// Parses a single expression, such as `x + 1 or y`.
    ///
    /// ```text
    /// expression:
    ///     | disjunction
    /// ```
    pub(super) fn parse_expression(&mut self) -> ParseExprResult {
        self.nested(Self::parse_disjunction)
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
