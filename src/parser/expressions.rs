//! Expression and assignment-target rules, such as `sum`, `primary`, and `star_targets`.

use super::{ParseExprResult, Parser};
use crate::ast::{
    BoolOp, CompOp, Constant, Expr, ExprContext, ExprKind, Keyword, Operator, ParserError,
    PrimaryOp, UnaryOp,
};
use crate::debug;
use crate::span::Span;
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
                let (args, keywords) = arguments;
                results.push(PrimaryOp::Call(args, keywords, self.last_span()));
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
                    PrimaryOp::Call(args, keywords, end) => Box::new(Expr::new(
                        ExprKind::Call {
                            func: result,
                            args,
                            keywords,
                        },
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
                    PrimaryOp::Try(end) => {
                        Box::new(Expr::new(ExprKind::Try { value: result }, start.to(end)))
                    }
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

    /// Parses a bare name used as an assignment target, such as `x` in `x = 1`, or targets in
    /// parentheses, such as `(b, c)` in `a, (b, c) = t`. A single target in parentheses is just
    /// that target, so `(x) = 1` assigns `x`.
    ///
    /// ```text
    /// star_atom:
    ///     | NAME
    ///     | '(' star_targets ')'
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

        // '(' star_targets ')'
        if self.expect(TokenType::LParen).is_some()
            && let Ok(mut target) = self.nested(Self::parse_star_targets)
            && self.expect(TokenType::RParen).is_some()
        {
            target.span = self.span_from(mark);
            return Ok(target);
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_star_atom".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a single assignment target: a subscript like `a[0]`, a field like `p.x`, or a name
    /// like `x`.
    ///
    /// ```text
    /// target_with_star_atom:
    ///     | t_primary '.' NAME !t_lookahead
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

        // t_primary '.' NAME !t_lookahead
        if let Ok(value) = self.parse_t_primary()
            && self.expect(TokenType::Dot).is_some()
            && let Some((attr, _)) = self.expect_name("a field name", true)
            && !self.parse_t_lookahead()
        {
            return Ok(Box::new(Expr::new(
                ExprKind::Attribute {
                    value,
                    attr,
                    ctx: ExprContext::Store,
                },
                self.span_from(mark),
            )));
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

    /// Parses the target on the left side of an assignment or after `for`, such as `x` in
    /// `x = 1`, or names separated by commas, such as `q, r` in `q, r = divide(7, 2)`, which
    /// unpack a tuple.
    ///
    /// ```text
    /// star_targets:
    ///     | star_target (',' star_target)* [',']
    /// ```
    pub(super) fn parse_star_targets(&mut self) -> ParseExprResult {
        debug!("parse_star_targets: parsing...");
        let mark = self.pos;
        let first = self.parse_star_target()?;
        if self.peek().r#type != TokenType::Comma {
            debug!("parse_star_targets: successfully parsed: {:?}", first);
            return Ok(first);
        }

        let mut elts = vec![*first];
        while self.expect(TokenType::Comma).is_some() {
            let before = self.pos;
            match self.parse_star_target() {
                Ok(target) => elts.push(*target),
                Err(_) => {
                    // a trailing comma is allowed
                    self.pos = before;
                    break;
                }
            }
        }
        if elts.len() == 1 {
            let span = self.span_from(mark);
            self.hint(
                span,
                "unpacking needs at least two names; to assign one value, leave out the comma",
            );
            self.pos = mark;
            return Err(ParserError {
                method: "parse_star_targets".to_string(),
                token: self.peek().r#type.clone(),
                span: self.peek().span,
            });
        }
        Ok(Box::new(Expr::new(
            ExprKind::Tuple {
                elts,
                ctx: ExprContext::Store,
            },
            self.span_from(mark),
        )))
    }

    /// Parses a comma-separated list of call arguments, such as `1, x + 2` in `f(1, x + 2)`, where
    /// named values, such as `x=1.0` in `Point(x=1.0, y=2.0)`, come after the others.
    ///
    /// ```text
    /// args:
    ///     | ','.(kwarg | expression)+
    /// kwarg:
    ///     | NAME '=' expression
    /// ```
    pub(super) fn parse_args(&mut self) -> Result<(Vec<Expr>, Vec<Keyword>), ParserError> {
        let mut args = vec![];
        let mut keywords = vec![];

        let mut first = true;
        loop {
            let mark = self.pos;
            // a comma with no argument after it belongs to `arguments` as a trailing comma
            if !first && self.expect(TokenType::Comma).is_none() {
                break;
            }
            let start = self.pos;
            if let Some(keyword) = self.parse_kwarg() {
                keywords.push(keyword);
            } else if let Ok(expr) = self.parse_expression() {
                if !keywords.is_empty() {
                    let span = self.span_from(start);
                    self.hint(
                        span,
                        "a value without a name cannot follow a named one, as in 'x=1'",
                    );
                    self.pos = mark;
                    return Err(ParserError {
                        method: "parse_args".to_string(),
                        token: self.peek().r#type.clone(),
                        span: self.peek().span,
                    });
                }
                args.push(*expr);
            } else if first {
                return Err(ParserError {
                    method: "parse_args".to_string(),
                    token: self.peek().r#type.clone(),
                    span: self.peek().span,
                });
            } else {
                self.pos = mark;
                break;
            }
            first = false;
        }

        Ok((args, keywords))
    }

    /// Parses a named value in a call, such as `x=1.0` in `Point(x=1.0, y=2.0)`.
    ///
    /// ```text
    /// kwarg:
    ///     | NAME '=' expression
    /// ```
    fn parse_kwarg(&mut self) -> Option<Keyword> {
        let mark = self.pos;
        if let TokenType::Name(arg) = self.peek().r#type.clone()
            && self.tokens.get(self.pos + 1).map(|t| &t.r#type)
                == Some(&TokenType::Operator("=".to_string()))
        {
            let arg_span = self.peek().span;
            self.pos += 2;
            if let Ok(value) = self.parse_expression() {
                return Some(Keyword {
                    arg,
                    arg_span,
                    value: *value,
                });
            }
        }
        self.pos = mark;
        None
    }

    /// Parses call arguments with an optional trailing comma, such as `1, 2,` in `f(1, 2,)`.
    ///
    /// ```text
    /// arguments: args [','] &')'
    /// ```
    pub(super) fn parse_arguments(&mut self) -> Result<(Vec<Expr>, Vec<Keyword>), ParserError> {
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

    /// Parses one string literal: a `STRING`, or a string with values in it, such as
    /// `"x is {x}"`, whose values are expressions between its pieces of text.
    ///
    /// ```text
    /// string:
    ///     | STRING
    ///     | STRING_START expression (STRING_MIDDLE expression)* STRING_END
    /// ```
    pub(super) fn parse_string(&mut self) -> ParseExprResult {
        let mark = self.pos;
        let text = |value: String, span: Span| {
            Expr::new(
                ExprKind::Constant {
                    value: Box::new(Constant::Str(value)),
                    kind: None,
                },
                span,
            )
        };
        match self.peek().r#type.clone() {
            TokenType::String(value) => {
                self.advance();
                return Ok(Box::new(text(value, self.span_from(mark))));
            }
            TokenType::StringStart(value) => {
                let mut values = vec![text(value, self.advance().span)];
                loop {
                    values.push(*self.parse_expression()?);
                    let token = self.peek().clone();
                    match token.r#type {
                        TokenType::StringMiddle(value) => {
                            self.advance();
                            values.push(text(value, token.span));
                        }
                        TokenType::StringEnd(value) => {
                            self.advance();
                            values.push(text(value, token.span));
                            break;
                        }
                        _ => {
                            self.record_expected("'}'".to_string(), false);
                            return Err(ParserError {
                                method: "parse_string".to_string(),
                                token: token.r#type,
                                span: token.span,
                            });
                        }
                    }
                }
                return Ok(Box::new(Expr::new(
                    ExprKind::JoinedStr { values },
                    self.span_from(mark),
                )));
            }
            _ => {}
        }

        Err(ParserError {
            method: "parse_string".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses string literals written one after another, which make one string, as `"a" "b"`
    /// makes `"ab"`. Pieces of text next to each other join, empty ones are left out, and the
    /// result is a [`ExprKind::JoinedStr`] only if a value is among them.
    pub(super) fn parse_strings_loop(&mut self) -> ParseExprResult {
        let start = self.pos;
        let mut results = vec![*self.parse_string()?];
        loop {
            let mark = self.pos;
            if let Ok(expr) = self.parse_string() {
                results.push(*expr);
            } else {
                self.pos = mark;
                break;
            }
        }

        let mut values: Vec<Expr> = vec![];
        for expr in results {
            let pieces = match expr.kind {
                ExprKind::JoinedStr { values } => values,
                _ => vec![expr],
            };
            for piece in pieces {
                if let ExprKind::Constant { value, .. } = &piece.kind
                    && let Constant::Str(text) = &**value
                    && let Some(last) = values.last_mut()
                    && let ExprKind::Constant { value: before, .. } = &mut last.kind
                    && let Constant::Str(joined) = &mut **before
                {
                    joined.push_str(text);
                    last.span = last.span.to(piece.span);
                    continue;
                }
                values.push(piece);
            }
        }
        let span = self.span_from(start);
        let kind = match values.as_slice() {
            [] => ExprKind::Constant {
                value: Box::new(Constant::Str(String::new())),
                kind: None,
            },
            [only] if matches!(&only.kind, ExprKind::Constant { .. }) => {
                values.pop().expect("one piece").kind
            }
            _ => {
                values.retain(|piece| {
                    !matches!(&piece.kind, ExprKind::Constant { value, .. }
                        if matches!(&**value, Constant::Str(text) if text.is_empty()))
                });
                ExprKind::JoinedStr { values }
            }
        };
        Ok(Box::new(Expr::new(kind, span)))
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
    ///     | tuple
    ///     | group
    /// ```
    pub(super) fn parse_atom(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // tuple | group
        if let Ok(expr) = self.parse_tuple_or_group() {
            return Ok(expr);
        }
        self.pos = mark;

        // list
        if let Ok(expr) = self.parse_list() {
            return Ok(expr);
        }
        self.pos = mark;

        // dict
        if let Ok(expr) = self.parse_dict() {
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
            // the prelude's variant, which linking turns into `option.none`
            return Ok(Box::new(Expr::new(
                ExprKind::Name {
                    id: "none".to_string(),
                    ctx: ExprContext::Load,
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

    /// Parses a parenthesized expression, such as `(1 + 2)` in `(1 + 2) * 3`, whose span grows to
    /// cover the parentheses, or a tuple of two or more values, such as `(1, "a")`. Both start with
    /// `'(' expression`, which is parsed once, so nested parentheses take linear time.
    ///
    /// ```text
    /// tuple:
    ///     | '(' expression (',' expression)+ [','] ')'
    /// group:
    ///     | '(' expression ')'
    /// ```
    pub(super) fn parse_tuple_or_group(&mut self) -> ParseExprResult {
        let mark = self.pos;
        let error = |parser: &mut Self| {
            parser.pos = mark;
            Err(ParserError {
                method: "parse_tuple_or_group".to_string(),
                token: parser.peek().r#type.clone(),
                span: parser.peek().span,
            })
        };

        if self.expect(TokenType::LParen).is_none() {
            return error(self);
        }
        if self.peek().r#type == TokenType::RParen {
            let span = self.tokens[mark].span.to(self.peek().span);
            self.record_expected("an expression".to_string(), false);
            self.hint(
                span,
                "'()' is not a value; a tuple needs at least two values, as in '(a, b)'",
            );
            return error(self);
        }
        let Ok(first) = self.parse_expression() else {
            return error(self);
        };

        // '(' expression ')'
        if self.expect(TokenType::RParen).is_some() {
            let mut expr = first;
            expr.span = self.span_from(mark);
            return Ok(expr);
        }

        // '(' expression (',' expression)+ [','] ')'
        let mut elts = vec![*first];
        while self.expect(TokenType::Comma).is_some() {
            let before = self.pos;
            match self.parse_expression() {
                Ok(elt) => elts.push(*elt),
                Err(_) => {
                    // a trailing comma is allowed
                    self.pos = before;
                    break;
                }
            }
        }
        if elts.len() == 1 && self.peek().r#type == TokenType::RParen {
            let span = self.tokens[mark].span.to(self.peek().span);
            self.hint(
                span,
                "a tuple needs at least two values; to group one value, leave out the comma",
            );
            return error(self);
        }
        if elts.len() > 1 && self.expect(TokenType::RParen).is_some() {
            return Ok(Box::new(Expr::new(
                ExprKind::Tuple {
                    elts,
                    ctx: ExprContext::Load,
                },
                self.span_from(mark),
            )));
        }
        error(self)
    }

    /// Parses a map literal, a key and its value per entry.
    ///
    /// ```text
    /// dict:
    ///     | '{' [kvpair (',' kvpair)* [',']] '}'
    /// kvpair:
    ///     | expression ':' expression
    /// ```
    pub(super) fn parse_dict(&mut self) -> ParseExprResult {
        let mark = self.pos;

        if self.expect(TokenType::LBrace).is_some() {
            let mut keys = vec![];
            let mut values = vec![];
            loop {
                let before = self.pos;
                if let Ok(key) = self.parse_expression() {
                    if self.expect(TokenType::Colon).is_some()
                        && let Ok(value) = self.parse_expression()
                    {
                        keys.push(*key);
                        values.push(*value);
                        if self.expect(TokenType::Comma).is_some() {
                            continue;
                        }
                        break;
                    }
                    if self.peek().r#type != TokenType::Colon {
                        let span = self.peek().span;
                        self.hint(
                            span,
                            "a map entry is a key, ':', and a value, as in '{\"a\": 1}'",
                        );
                    }
                    self.pos = mark;
                    return Err(ParserError {
                        method: "parse_dict".to_string(),
                        token: self.peek().r#type.clone(),
                        span: self.peek().span,
                    });
                }
                self.pos = before;
                break;
            }
            if self.expect(TokenType::RBrace).is_some() {
                return Ok(Box::new(Expr::new(
                    ExprKind::Dict { keys, values },
                    self.span_from(mark),
                )));
            }
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_dict".to_string(),
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

    /// Parses what goes between a subscript's brackets: an index, such as `i` in `xs[i]`, or the
    /// bounds of a slice, such as `1:` in `xs[1:]` or `::-1` in `xs[::-1]`.
    ///
    /// ```text
    /// slice:
    ///     | [expression] ':' [expression] [':' [expression]]
    ///     | expression
    /// ```
    pub(super) fn parse_slice(&mut self) -> ParseExprResult {
        let mark = self.pos;
        let lower = self.parse_expression_optional();
        if self.expect(TokenType::Colon).is_none() {
            return match lower {
                Some(index) => Ok(index),
                None => Err(ParserError {
                    method: "parse_slice".to_string(),
                    token: self.peek().r#type.clone(),
                    span: self.peek().span,
                }),
            };
        }
        let upper = self.parse_expression_optional();
        let step = match self.expect(TokenType::Colon) {
            Some(_) => self.parse_expression_optional(),
            None => None,
        };
        Ok(Box::new(Expr::new(
            ExprKind::Slice { lower, upper, step },
            self.span_from(mark),
        )))
    }

    /// Parses an expression if one comes next, leaving the position where it was otherwise.
    fn parse_expression_optional(&mut self) -> Option<Box<Expr>> {
        let mark = self.pos;
        let expr = self.parse_expression().ok();
        if expr.is_none() {
            self.pos = mark;
        }
        expr
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
    pub(super) fn parse_arguments_optional(&mut self) -> (Vec<Expr>, Vec<Keyword>) {
        let mark = self.pos;
        self.parse_arguments().unwrap_or_else(|_| {
            self.pos = mark;
            (vec![], vec![])
        })
    }

    /// Parses the calls, subscripts, attributes, and `?`s that follow an atom, such as `(1)[0]` in
    /// `f(1)[0]`, `.len()` in `xs.len()`, or `?` in `f()?`.
    ///
    /// ```text
    /// ( '(' [arguments] ')' | '[' slices ']' | '.' NAME | '?' )*
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
                let (args, keywords) = arguments;
                results.push(PrimaryOp::Call(args, keywords, self.last_span()));
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

            // '?'
            if self.expect(TokenType::Operator("?".to_string())).is_some() {
                results.push(PrimaryOp::Try(self.last_span()));
                continue;
            }
            self.pos = mark;
            break;
        }
        results
    }

    /// Parses an atom followed by any calls, subscripts, attributes, or `?`s, such as `f(1)`,
    /// `a[0]`, `xs.len()`, or `f()?`.
    ///
    /// ```text
    /// primary:
    ///     | atom ( '(' [arguments] ')' | '[' slices ']' | '.' NAME | '?' )*
    /// ```
    pub(super) fn parse_primary(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // atom ( '(' [arguments] ')' | '[' slices ']' | '.' NAME | '?' )*
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
                    PrimaryOp::Call(args, keywords, end) => Box::new(Expr::new(
                        ExprKind::Call {
                            func: result,
                            args,
                            keywords,
                        },
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
                    PrimaryOp::Try(end) => {
                        Box::new(Expr::new(ExprKind::Try { value: result }, start.to(end)))
                    }
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

    /// Parses a primary raised to a power, such as `2 ** n`, or just a primary.
    ///
    /// The exponent is a factor, so `2 ** 3 ** 2` groups as `2 ** (3 ** 2)` and `2 ** -1` parses,
    /// while `-2 ** 2` is `-(2 ** 2)`, since the sign belongs to the enclosing factor.
    ///
    /// ```text
    /// power:
    ///     | primary '**' factor
    ///     | primary
    /// ```
    pub(super) fn parse_power(&mut self) -> ParseExprResult {
        let base = self.parse_primary()?;
        let mark = self.pos;

        // primary '**' factor
        if self.expect(TokenType::Operator("**".to_string())).is_some()
            && let Ok(exponent) = self.nested(Self::parse_factor)
        {
            let span = base.span.to(exponent.span);
            return Ok(Box::new(Expr::new(
                ExprKind::BinOp {
                    op: Operator::Power,
                    left: base,
                    right: exponent,
                },
                span,
            )));
        }
        self.pos = mark;

        // primary
        Ok(base)
    }

    /// Parses a power with optional unary signs, such as `-x` or `+5`.
    ///
    /// ```text
    /// factor:
    ///     | '+' factor
    ///     | '-' factor
    ///     | power
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

        // power
        self.parse_power()
    }

    /// Parses the repeated `*`, `/`, and `%` operations of a term, such as `* b % c` in
    /// `a * b % c`.
    ///
    /// ```text
    /// ('*'|'/'|'%' factor)*
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

            if self.expect(TokenType::Operator("%".to_string())).is_some()
                && let Ok(term) = self.parse_factor()
            {
                results.push((Operator::Modulo, *term));
                continue;
            }

            self.pos = mark;
            break;
        }
        results
    }

    /// Parses multiplication, division, and remainders, such as `a * b / c % d`.
    ///
    /// ```text
    /// term:
    ///     | factor ('*'|'/'|'%' factor)*
    /// ```
    pub(super) fn parse_term(&mut self) -> ParseExprResult {
        let mark = self.pos;

        // factor ('*'|'/'|'%' factor)*
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
    ///     | '==' | '!=' | '<' | '<=' | '>' | '>=' | 'in' | 'not' 'in'
    /// ```
    pub(super) fn parse_compare_op(&mut self) -> Option<CompOp> {
        let keyword = |word: &str| TokenType::Keyword(word.to_string());
        if self.expect_quietly(keyword("in")).is_some() {
            return Some(CompOp::In);
        }
        let mark = self.pos;
        if self.expect_quietly(keyword("not")).is_some() {
            if self.expect_quietly(keyword("in")).is_some() {
                return Some(CompOp::NotIn);
            }
            self.pos = mark;
        }
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

    /// Parses the expression of a statement, such as `x + 1` in `x = x + 1`.
    ///
    /// Values separated by commas, such as `1, 2` in `x = 1, 2`, are an error that says a tuple
    /// needs parentheses, rather than a tuple as in Python.
    ///
    /// ```text
    /// expressions:
    ///     | expression
    /// ```
    pub(super) fn parse_expressions(&mut self) -> ParseExprResult {
        self.parse_expressions_or(
            "values separated by commas need parentheses to make a tuple, as in '(a, b)'",
        )
    }

    /// Parses one expression like [`Self::parse_expressions`], noting `message` as the error for
    /// values separated by commas.
    pub(super) fn parse_expressions_or(&mut self, message: &str) -> ParseExprResult {
        let mark = self.pos;
        let first = self.parse_expression()?;
        if self.peek().r#type != TokenType::Comma {
            return Ok(first);
        }
        while self.expect(TokenType::Comma).is_some() {
            let before = self.pos;
            if self.parse_expression().is_err() {
                self.pos = before;
                break;
            }
        }
        let span = self.span_from(mark);
        self.hint(span, message);
        self.pos = mark;
        Err(ParserError {
            method: "parse_expressions".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }
}
