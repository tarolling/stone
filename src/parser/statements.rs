//! Statement rules, such as `if_stmt`, `function_def`, and `program`.

use super::Parser;
use crate::ast::{
    Arg, Arguments, Expr, Field, MatchCase, Operator, ParserError, Pattern, PatternKind, Stmt,
    StmtKind, TraitName, TypeExpr, TypeExprKind, TypeParam, Variant,
};
use crate::debug;
use crate::span::{FileId, Span};
use crate::token::TokenType;

impl Parser {
    /// Parses a `for` loop, such as `for i in range(3): print(i)`.
    ///
    /// ```text
    /// for_stmt:
    ///     | 'for' star_targets 'in' ~ expressions ':' block
    /// ```
    pub(super) fn parse_for_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'for' star_targets 'in' ~ expressions ':' block
        if self.expect(TokenType::Keyword("for".to_string())).is_some()
            && let Ok(target) = self.parse_star_targets()
            && self.expect(TokenType::Keyword("in".to_string())).is_some()
            && let Ok(iter) = self.parse_expressions()
            && self.expect_block_start().is_some()
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

    /// Parses a `while` loop, such as `while x: x = x - 1`.
    ///
    /// ```text
    /// while_stmt:
    ///     | 'while' expression ':' block
    /// ```
    pub(super) fn parse_while_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'while' expression ':' block
        if self
            .expect(TokenType::Keyword("while".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect_block_start().is_some()
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

    /// Parses the `else` branch of an `if` statement, such as `else: print(x)`.
    ///
    /// ```text
    /// else_block:
    ///     | 'else' ':' block
    /// ```
    pub(super) fn parse_else_block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mark = self.pos;

        // 'else' ':' block
        if self
            .expect(TokenType::Keyword("else".to_string()))
            .is_some()
            && self.expect_block_start().is_some()
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

    /// Parses an `elif` branch and any branches after it, such as `elif x: print(x)`.
    ///
    /// ```text
    /// elif_stmt:
    ///     | 'elif' expression ':' block elif_stmt
    ///     | 'elif' expression ':' block [else_block]
    /// ```
    pub(super) fn parse_elif_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // both alternatives share this prefix, so parse it once instead of backtracking
        if self
            .expect(TokenType::Keyword("elif".to_string()))
            .is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect_block_start().is_some()
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

    /// Parses an `if` statement with its `elif` and `else` branches, such as `if x: print(x)`.
    ///
    /// ```text
    /// if_stmt:
    ///     | 'if' expression ':' block elif_stmt
    ///     | 'if' expression ':' block [else_block]
    /// ```
    pub(super) fn parse_if_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // both alternatives share this prefix, so parse it once instead of backtracking
        if self.expect(TokenType::Keyword("if".to_string())).is_some()
            && let Ok(test) = self.parse_expression()
            && self.expect_block_start().is_some()
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

    /// Parses one function parameter, such as `a: int` in `fn f(a: int, b: int):`.
    ///
    /// ```text
    /// param:
    ///     | NAME [':' type]
    /// ```
    pub(super) fn parse_param(&mut self) -> Result<Arg, ParserError> {
        let mark = self.pos;

        // NAME [':' type]
        if let Some((arg, span)) = self.expect_name("a parameter name", false) {
            let annotation = if self.expect(TokenType::Colon).is_some() {
                self.parse_type().ok().map(|ty| Some(*ty))
            } else {
                Some(None)
            };
            if let Some(annotation) = annotation {
                return Ok(Arg {
                    arg,
                    span,
                    annotation,
                });
            }
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_param".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a type in a function's signature, such as `int`, `list[list[float]]`,
    /// `tuple[int, str]`, `option[int]`, or an enum such as `Shape` or `shapes.Shape`.
    ///
    /// ```text
    /// type:
    ///     | 'list' '[' type ']'
    ///     | 'tuple' '[' type (',' type)+ [','] ']'
    ///     | 'fn' '(' [','.type+ [',']] ')' ['->' type]
    ///     | NAME ('.' NAME)* ['[' ','.type+ [','] ']']
    /// ```
    pub(super) fn parse_type(&mut self) -> Result<Box<TypeExpr>, ParserError> {
        let mark = self.pos;
        let scalar = match &self.peek().r#type {
            TokenType::Name(name) => match name.as_str() {
                "int" => Some(TypeExprKind::Int),
                "float" => Some(TypeExprKind::Float),
                "bool" => Some(TypeExprKind::Bool),
                "str" => Some(TypeExprKind::Str),
                "bytes" => Some(TypeExprKind::Bytes),
                _ => None,
            },
            _ => None,
        };

        // NAME for int, float, bool, or str
        if let Some(kind) = scalar {
            self.pos += 1;
            return Ok(Box::new(TypeExpr {
                kind,
                span: self.span_from(mark),
            }));
        }

        if self.peek().r#type == TokenType::Keyword("none".to_string()) {
            let span = self.peek().span;
            self.hint(
                span,
                "'none' is not a type: a function without '->' returns no value, and 'option[T]' holds a value or none",
            );
        } else if self.peek().r#type == TokenType::Name("list".to_string()) {
            // 'list' '[' type ']'
            self.pos += 1;
            if self.expect(TokenType::LBracket).is_some()
                && let Ok(elem) = self.nested(Self::parse_type)
                && self.expect(TokenType::RBracket).is_some()
            {
                return Ok(Box::new(TypeExpr {
                    kind: TypeExprKind::List(elem),
                    span: self.span_from(mark),
                }));
            }
        } else if self.peek().r#type == TokenType::Name("map".to_string()) {
            // 'map' '[' type ',' type [','] ']'
            self.pos += 1;
            if self.expect(TokenType::LBracket).is_some()
                && let Some(mut elts) = self.parse_types()
            {
                if elts.len() != 2 && self.peek().r#type == TokenType::RBracket {
                    let span = self.tokens[mark].span.to(self.peek().span);
                    self.hint(
                        span,
                        "a map type needs a key type and a value type, as in 'map[str, int]'",
                    );
                } else if self.expect(TokenType::RBracket).is_some() {
                    let value = elts.pop().unwrap();
                    let key = elts.pop().unwrap();
                    return Ok(Box::new(TypeExpr {
                        kind: TypeExprKind::Map(Box::new(key), Box::new(value)),
                        span: self.span_from(mark),
                    }));
                }
            }
        } else if self.peek().r#type == TokenType::Name("tuple".to_string()) {
            // 'tuple' '[' type (',' type)+ [','] ']'
            self.pos += 1;
            if self.expect(TokenType::LBracket).is_some()
                && let Some(elts) = self.parse_types()
            {
                if elts.len() == 1 && self.peek().r#type == TokenType::RBracket {
                    let span = self.tokens[mark].span.to(self.peek().span);
                    self.hint(
                        span,
                        "a tuple type needs at least two types, as in 'tuple[int, str]'",
                    );
                } else if self.expect(TokenType::RBracket).is_some() {
                    return Ok(Box::new(TypeExpr {
                        kind: TypeExprKind::Tuple(elts),
                        span: self.span_from(mark),
                    }));
                }
            }
        } else if self.peek().r#type == TokenType::Keyword("fn".to_string()) {
            // 'fn' '(' [','.type+ [',']] ')' ['->' type]
            self.pos += 1;
            if self.expect(TokenType::LParen).is_some() {
                let params = match self.peek().r#type == TokenType::RParen {
                    true => Some(vec![]),
                    false => self.parse_types(),
                };
                if let Some(params) = params
                    && self.expect(TokenType::RParen).is_some()
                {
                    let ret = match self.expect(TokenType::Arrow) {
                        Some(_) => self.nested(Self::parse_type).ok().map(Some),
                        None => Some(None),
                    };
                    if let Some(ret) = ret {
                        return Ok(Box::new(TypeExpr {
                            kind: TypeExprKind::Function { params, ret },
                            span: self.span_from(mark),
                        }));
                    }
                }
            }
        } else if let Some((first, _)) = self.expect_name("a type", false) {
            // NAME ('.' NAME)* ['[' ','.type+ [','] ']']
            let mut name = first;
            loop {
                let before = self.pos;
                if self.expect(TokenType::Dot).is_some()
                    && let Some((part, _)) = self.expect_name("a type name", false)
                {
                    name = format!("{name}.{part}");
                    continue;
                }
                self.pos = before;
                break;
            }
            let args = if self.expect(TokenType::LBracket).is_some() {
                match self.parse_types() {
                    Some(args) if self.expect(TokenType::RBracket).is_some() => Some(args),
                    _ => None,
                }
            } else {
                Some(vec![])
            };
            if let Some(args) = args {
                return Ok(Box::new(TypeExpr {
                    kind: TypeExprKind::Named { name, args },
                    span: self.span_from(mark),
                }));
            }
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_type".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one or more types separated by commas, allowing a trailing comma, such as
    /// `int, str` in `tuple[int, str]`. Returns `None` if the first type fails.
    fn parse_types(&mut self) -> Option<Vec<TypeExpr>> {
        let first = self.nested(Self::parse_type).ok()?;
        let mut types = vec![*first];
        while self.expect(TokenType::Comma).is_some() {
            let before = self.pos;
            match self.nested(Self::parse_type) {
                Ok(ty) => types.push(*ty),
                Err(_) => {
                    // a trailing comma is allowed
                    self.pos = before;
                    break;
                }
            }
        }
        Some(types)
    }

    /// Parses an enum definition, such as `enum Shape:` followed by an indented line for each
    /// variant, like `Circle(float)` or `Empty`.
    ///
    /// ```text
    /// enum_def:
    ///     | ['pub'] 'enum' NAME [type_params] ':' NEWLINE INDENT variant+ DEDENT
    /// ```
    pub(super) fn parse_enum_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // ['pub'] 'enum' NAME [type_params] ':' NEWLINE INDENT variant+ DEDENT
        let public = self.expect(TokenType::Keyword("pub".to_string())).is_some();
        if self
            .expect(TokenType::Keyword("enum".to_string()))
            .is_some()
            && let Some((name, name_span)) = self.expect_name("an enum name", false)
            && let Ok(type_params) = self.parse_type_params("an enum's", "enum Tree[T]:")
            && self.expect_block_start().is_some()
            && self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(variants) = self.nested(Self::parse_variants)
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::EnumDef {
                    name,
                    name_span,
                    public,
                    type_params,
                    variants,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_enum_def".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a struct definition, such as `struct Point:` followed by an indented line for each
    /// field, like `x: float`.
    ///
    /// ```text
    /// struct_def:
    ///     | ['pub'] 'struct' NAME [type_params] ':' NEWLINE INDENT field+ DEDENT
    /// ```
    pub(super) fn parse_struct_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // ['pub'] 'struct' NAME [type_params] ':' NEWLINE INDENT field+ DEDENT
        let public = self.expect(TokenType::Keyword("pub".to_string())).is_some();
        if self
            .expect(TokenType::Keyword("struct".to_string()))
            .is_some()
            && let Some((name, name_span)) = self.expect_name("a struct name", false)
            && let Ok(type_params) = self.parse_type_params("a struct's", "struct Pair[A, B]:")
            && self.expect_block_start().is_some()
            && self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(fields) = self.nested(Self::parse_fields)
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::StructDef {
                    name,
                    name_span,
                    public,
                    type_params,
                    fields,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_struct_def".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses an `impl` block, such as `impl Point:` followed by an indented function for each
    /// method, like `fn norm(self) -> float: ret self.x`, or `impl Shape for Circle:`, which
    /// implements the trait `Shape` for `Circle`.
    ///
    /// ```text
    /// impl_def:
    ///     | 'impl' [trait_name 'for'] NAME ':' NEWLINE INDENT function_def+ DEDENT
    /// ```
    pub(super) fn parse_impl_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'impl' [trait_name 'for'] NAME ':' NEWLINE INDENT function_def+ DEDENT
        if self
            .expect(TokenType::Keyword("impl".to_string()))
            .is_some()
            && let Some(first) = self.parse_trait_name("a struct name")
            && let Some((trait_name, (name, name_span))) = self.parse_impl_target(first)
            && self.expect_block_start().is_some()
            && self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(methods) = self.nested(|parser| parser.parse_methods(false))
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::ImplDef {
                    name,
                    name_span,
                    trait_name,
                    methods,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_impl_def".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses what follows the first name of an `impl` block: nothing for `impl Point:`, whose
    /// name is the struct's, or `for` and the type for `impl Shape for Circle:`, whose first name
    /// is the trait's. A dotted first name is a trait's, so it must be followed by `for`.
    #[allow(clippy::type_complexity)]
    fn parse_impl_target(
        &mut self,
        first: TraitName,
    ) -> Option<(Option<TraitName>, (String, Span))> {
        if self.expect(TokenType::Keyword("for".to_string())).is_some() {
            let target = self.expect_name("a type name", false)?;
            return Some((Some(first), target));
        }
        if first.name.contains('.') {
            self.record_expected("'for'".to_string(), false);
            return None;
        }
        // `for` is optional here, so it is only expected if nothing else is
        self.record_expected("'for'".to_string(), true);
        Some((None, (first.name, first.span)))
    }

    /// Parses a trait's name, maybe through its module and with type arguments, such as
    /// `Shape`, `geo.Shape`, or `from[int]`, calling it `what` if the first name is missing.
    ///
    /// ```text
    /// trait_name:
    ///     | '.'.NAME+ ['[' ','.type+ [','] ']']
    /// ```
    fn parse_trait_name(&mut self, what: &str) -> Option<TraitName> {
        let (mut name, mut span) = self.expect_name(what, false)?;
        while self.peek().r#type == TokenType::Dot {
            let mark = self.pos;
            self.advance();
            let Some((next, next_span)) = self.expect_name("a name", false) else {
                self.pos = mark;
                break;
            };
            name = format!("{name}.{next}");
            span = span.to(next_span);
        }
        let mut args = vec![];
        if self.expect(TokenType::LBracket).is_some() {
            args = self.parse_types()?;
            self.expect(TokenType::RBracket)?;
        }
        Some(TraitName { name, span, args })
    }

    /// Parses a trait definition, such as `trait Shape:` followed by an indented function for
    /// each method, written without a body, as `fn area(self) -> float`, or with a default one.
    ///
    /// ```text
    /// trait_def:
    ///     | ['pub'] 'trait' NAME [type_params] ':' NEWLINE INDENT trait_method+ DEDENT
    /// ```
    pub(super) fn parse_trait_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // ['pub'] 'trait' NAME [type_params] ':' NEWLINE INDENT trait_method+ DEDENT
        let public = self.expect(TokenType::Keyword("pub".to_string())).is_some();
        if self
            .expect(TokenType::Keyword("trait".to_string()))
            .is_some()
            && let Some((name, name_span)) = self.expect_name("a trait name", false)
            && let Ok(type_params) = self.parse_type_params("a trait's", "trait from[T]:")
            && self.expect_block_start().is_some()
            && self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(methods) = self.nested(|parser| parser.parse_methods(true))
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::TraitDef {
                    name,
                    name_span,
                    public,
                    type_params,
                    methods,
                },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_trait_def".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the functions of an `impl` block, or of a trait if `in_trait`, where a function
    /// may have no body, up to the `Dedent` that ends them.
    ///
    /// ```text
    /// methods:
    ///     | function_def+
    /// trait_method:
    ///     | function_def
    ///     | 'fn' NAME [type_params] '(' [params] ')' ['->' type] NEWLINE
    /// ```
    fn parse_methods(&mut self, in_trait: bool) -> Result<Vec<Stmt>, ParserError> {
        let mut methods = vec![];
        loop {
            let token = self.peek().clone();
            if token.r#type == TokenType::Keyword("pub".to_string()) {
                let message = if in_trait {
                    "a trait's method needs no 'pub', since it is visible wherever the trait is"
                } else {
                    "a method needs no 'pub', since it is visible wherever its struct is"
                };
                self.hint(token.span, message);
                break;
            }
            if token.r#type != TokenType::Keyword("fn".to_string()) {
                self.record_expected("'fn'".to_string(), false);
                break;
            }
            let Ok(method) = self.parse_function_def_in(in_trait) else {
                break;
            };
            methods.push(*method);
            if self.peek().r#type == TokenType::Dedent {
                return Ok(methods);
            }
        }

        Err(ParserError {
            method: "parse_methods".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the fields of a struct, one per line, up to the `Dedent` that ends them.
    ///
    /// ```text
    /// field:
    ///     | NAME ':' type NEWLINE
    /// ```
    fn parse_fields(&mut self) -> Result<Vec<Field>, ParserError> {
        let mut fields = vec![];
        loop {
            let mark = self.pos;
            if let Some((name, span)) = self.expect_name("a field name", false)
                && self.expect(TokenType::Colon).is_some()
                && let Ok(annotation) = self.parse_type()
                && self.expect(TokenType::Newline).is_some()
            {
                fields.push(Field {
                    name,
                    span,
                    annotation: *annotation,
                });
            } else {
                self.pos = mark;
                break;
            }
            if self.peek().r#type == TokenType::Dedent {
                return Ok(fields);
            }
        }

        Err(ParserError {
            method: "parse_fields".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the variants of an enum, one per line, up to the `Dedent` that ends them.
    ///
    /// ```text
    /// variant:
    ///     | NAME ['(' ','.type+ [','] ')'] NEWLINE
    /// ```
    fn parse_variants(&mut self) -> Result<Vec<Variant>, ParserError> {
        let mut variants = vec![];
        loop {
            let mark = self.pos;
            // only the prelude names a variant `none`, which is a keyword everywhere else
            let none = TokenType::Keyword("none".to_string());
            let named = if self.peek().span.file == FileId::PRELUDE && self.peek().r#type == none {
                let token = self.advance();
                Some(("none".to_string(), token.span))
            } else {
                self.expect_name("a variant name", false)
            };
            let Some((name, span)) = named else {
                break;
            };
            let fields = if self.expect(TokenType::LParen).is_some() {
                if self.peek().r#type == TokenType::RParen {
                    self.hint(
                        span,
                        &format!(
                            "a variant without values is written without parentheses, as in '{name}'"
                        ),
                    );
                    None
                } else {
                    match self.parse_types() {
                        Some(fields) if self.expect(TokenType::RParen).is_some() => Some(fields),
                        _ => None,
                    }
                }
            } else {
                Some(vec![])
            };
            match fields {
                Some(fields) if self.expect(TokenType::Newline).is_some() => {
                    variants.push(Variant { name, span, fields });
                }
                _ => {
                    self.pos = mark;
                    break;
                }
            }
            if self.peek().r#type == TokenType::Dedent {
                return Ok(variants);
            }
        }

        Err(ParserError {
            method: "parse_variants".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a `match` statement, such as `match s:` followed by an indented `case` for each
    /// pattern, like `case Shape.Circle(r): print(r)`.
    ///
    /// ```text
    /// match_stmt:
    ///     | 'match' expression ':' NEWLINE INDENT case_block+ DEDENT
    /// case_block:
    ///     | 'case' pattern ':' block
    /// ```
    pub(super) fn parse_match_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // 'match' expression ':' NEWLINE INDENT case_block+ DEDENT
        if self
            .expect(TokenType::Keyword("match".to_string()))
            .is_some()
            && let Ok(subject) = self.parse_expression()
            && self.expect_block_start().is_some()
            && self.expect(TokenType::Newline).is_some()
            && self.expect(TokenType::Indent).is_some()
            && let Ok(cases) = self.nested(Self::parse_cases)
            && self.expect(TokenType::Dedent).is_some()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::Match { subject, cases },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_match_stmt".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the cases of a `match`, up to the `Dedent` that ends them.
    fn parse_cases(&mut self) -> Result<Vec<MatchCase>, ParserError> {
        let mut cases = vec![];
        loop {
            let mark = self.pos;
            if self
                .expect(TokenType::Keyword("case".to_string()))
                .is_some()
                && let Ok(pattern) = self.parse_pattern()
                && self.expect_block_start().is_some()
                && let Ok(body) = self.nested(Self::parse_block)
            {
                cases.push(MatchCase { pattern, body });
                if self.peek().r#type == TokenType::Dedent {
                    return Ok(cases);
                }
                continue;
            }
            self.pos = mark;
            break;
        }

        Err(ParserError {
            method: "parse_cases".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses a pattern after `case`, such as `Shape.Rect(w, _)`, `some(x)`, `none`, a name, or
    /// `_`.
    ///
    /// ```text
    /// pattern:
    ///     | 'none'
    ///     | NAME ('.' NAME)* ['(' ','.pattern+ [','] ')']
    /// ```
    pub(super) fn parse_pattern(&mut self) -> Result<Pattern, ParserError> {
        let mark = self.pos;

        // 'none'
        if let Some(token) = self.expect(TokenType::Keyword("none".to_string())) {
            return Ok(Pattern {
                kind: PatternKind::Variant {
                    path: vec![("none".to_string(), token.span)],
                    fields: vec![],
                },
                span: token.span,
            });
        }

        // NAME ('.' NAME)* ['(' ','.pattern+ [','] ')']
        if let Some(first) = self.expect_name("a pattern", false) {
            let mut path = vec![first];
            loop {
                let before = self.pos;
                if self.expect(TokenType::Dot).is_some()
                    && let Some(part) = self.expect_name("a variant name", false)
                {
                    path.push(part);
                    continue;
                }
                self.pos = before;
                break;
            }
            let called = self.expect(TokenType::LParen).is_some();
            let fields = if called {
                match self.parse_patterns() {
                    Some(fields) if self.expect(TokenType::RParen).is_some() => Some(fields),
                    _ => None,
                }
            } else {
                Some(vec![])
            };
            if let Some(fields) = fields {
                let kind = match path.as_slice() {
                    [(name, _)] if !called && name == "_" => PatternKind::Wildcard,
                    [(name, _)] if !called => PatternKind::Name(name.clone()),
                    _ => PatternKind::Variant { path, fields },
                };
                return Ok(Pattern {
                    kind,
                    span: self.span_from(mark),
                });
            }
        } else if matches!(
            self.peek().r#type,
            TokenType::Number(_) | TokenType::Float(_) | TokenType::String(_)
        ) || matches!(&self.peek().r#type, TokenType::Keyword(word) if word == "true" || word == "false")
            || matches!(&self.peek().r#type, TokenType::Operator(op) if op == "-")
        {
            let span = self.peek().span;
            self.hint(
                span,
                "a pattern is a variant such as 'Shape.Circle(r)', 'some(x)', or 'none', a name, or '_'",
            );
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_pattern".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one or more patterns separated by commas, allowing a trailing comma, such as
    /// `w, _` in `Shape.Rect(w, _)`. Returns `None` if the first pattern fails.
    fn parse_patterns(&mut self) -> Option<Vec<Pattern>> {
        let first = self.nested(Self::parse_pattern).ok()?;
        let mut patterns = vec![first];
        while self.expect(TokenType::Comma).is_some() {
            let before = self.pos;
            match self.nested(Self::parse_pattern) {
                Ok(pattern) => patterns.push(pattern),
                Err(_) => {
                    // a trailing comma is allowed
                    self.pos = before;
                    break;
                }
            }
        }
        Some(patterns)
    }

    /// Parses a comma-separated parameter list, such as `a: int, b: int` in
    /// `fn f(a: int, b: int):`.
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

    /// Parses function parameters if there are any, returning an empty list for `fn f():`.
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

    /// Parses a function's return type if it has one, such as `-> int` in
    /// `fn f(a: int) -> int:`, returning `None` for `fn f():`.
    ///
    /// ```text
    /// ['->' type]
    /// ```
    pub(super) fn parse_return_type(&mut self) -> Result<Option<TypeExpr>, ParserError> {
        if self.expect(TokenType::Arrow).is_none() {
            return Ok(None);
        }
        self.parse_type().map(|ty| Some(*ty))
    }

    /// Parses the type parameters of a generic function, struct, or enum if it has any, such as
    /// `[A, B]` in `fn swap[A, B](p: tuple[A, B]) -> tuple[B, A]:`, returning an empty list for
    /// `fn f():`. `owner` names what has them in the hint for empty brackets, such as
    /// `"a function's"`, and `example` shows them, such as `"fn first[T](xs: list[T]) -> T"`.
    ///
    /// A parameter may name traits its types must implement, as in `[T: Shape + Named]`.
    ///
    /// ```text
    /// type_params:
    ///     | '[' ','.type_param+ [','] ']'
    /// type_param:
    ///     | NAME [':' '+'.trait_name+]
    /// ```
    pub(super) fn parse_type_params(
        &mut self,
        owner: &str,
        example: &str,
    ) -> Result<Vec<TypeParam>, ParserError> {
        let open = self.peek().span;
        if self.expect(TokenType::LBracket).is_none() {
            return Ok(vec![]);
        }
        if self.peek().r#type == TokenType::RBracket {
            let span = open.to(self.peek().span);
            self.hint(
                span,
                &format!("{owner} type parameters need at least one name, as in '{example}'"),
            );
        } else if let Some(param) = self.parse_type_param() {
            let mut params = vec![param];
            while self.expect(TokenType::Comma).is_some() {
                if self.peek().r#type == TokenType::RBracket {
                    // a trailing comma is allowed
                    break;
                }
                match self.parse_type_param() {
                    Some(param) => params.push(param),
                    None => return Err(self.error_here("parse_type_params")),
                }
            }
            if self.expect(TokenType::RBracket).is_some() {
                return Ok(params);
            }
        }
        Err(ParserError {
            method: "parse_type_params".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses one type parameter and its bounds, such as `T` or `T: Shape + geo.Named`.
    fn parse_type_param(&mut self) -> Option<TypeParam> {
        let (name, span) = self.expect_name("a type parameter name", false)?;
        let mut bounds = vec![];
        if self.expect(TokenType::Colon).is_some() {
            bounds.push(self.parse_trait_name("a trait name")?);
            while self.expect(TokenType::Operator("+".to_string())).is_some() {
                bounds.push(self.parse_trait_name("a trait name")?);
            }
        }
        Some(TypeParam { name, span, bounds })
    }

    /// Parses a function definition, such as `fn add(a: int, b: int) -> int: ret a + b`,
    /// `pub fn f() -> int: ret 1`, or `fn first[T](xs: list[T]) -> T: ret xs[0]`.
    ///
    /// ```text
    /// function_def:
    ///     | ['pub'] 'fn' NAME [type_params] '(' [params] ')' ['->' type] ':' block
    /// ```
    pub(super) fn parse_function_def(&mut self) -> Result<Box<Stmt>, ParserError> {
        self.parse_function_def_in(false)
    }

    /// Parses a function definition, which in a trait (`in_trait`) may end its signature with a
    /// newline instead of a body, as `fn area(self) -> float` does, leaving `body` empty.
    fn parse_function_def_in(&mut self, in_trait: bool) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // ['pub'] 'fn' NAME [type_params] '(' [params] ')' ['->' type] ':' block
        let public = self.expect(TokenType::Keyword("pub".to_string())).is_some();
        if self.expect(TokenType::Keyword("fn".to_string())).is_some()
            && let Some((name, name_span)) = self.expect_name("a function name", false)
            && let Ok(type_params) =
                self.parse_type_params("a function's", "fn first[T](xs: list[T]) -> T")
            && self.expect(TokenType::LParen).is_some()
            && let args = self.parse_function_def_optional()
            && self.expect(TokenType::RParen).is_some()
            && let Ok(returns) = self.parse_return_type()
            && let Ok(body) = self.parse_function_body(in_trait)
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::FunctionDef {
                    name,
                    name_span,
                    public,
                    type_params,
                    args,
                    returns,
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

    /// Parses a function's `':' block`, or in a trait (`in_trait`) the newline of a signature
    /// without a body, giving an empty body.
    fn parse_function_body(&mut self, in_trait: bool) -> Result<Vec<Stmt>, ParserError> {
        if in_trait && self.peek().r#type == TokenType::Newline {
            self.advance();
            return Ok(vec![]);
        }
        if self.expect_block_start().is_none() {
            return Err(self.error_here("parse_function_def"));
        }
        self.parse_block()
    }

    /// Returns the error of the rule `method` failing at the current token.
    fn error_here(&self, method: &str) -> ParserError {
        ParserError {
            method: method.to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        }
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
        self.parse_expressions_or("to return several values, return a tuple, as in 'ret (a, b)'")
            .ok()
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

    /// Parses an augmented assignment, such as `x += 1` or `grid[i][j] *= 2`.
    ///
    /// ```text
    /// aug_assignment: single_target augassign expressions
    /// augassign: '+=' | '-=' | '*=' | '/=' | '%=' | '**='
    /// ```
    pub(super) fn parse_aug_assignment(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // single_target augassign expressions
        if let Ok(target) = self.parse_target_with_star_atom()
            && let Some(op) = self.parse_augassign()
            && let Ok(value) = self.parse_expressions()
        {
            return Ok(Box::new(Stmt::new(
                StmtKind::AugAssign { target, op, value },
                self.span_from(mark),
            )));
        }
        self.pos = mark;

        Err(ParserError {
            method: "parse_aug_assignment".to_string(),
            token: self.peek().r#type.clone(),
            span: self.peek().span,
        })
    }

    /// Parses the operator of an augmented assignment, such as `+=`, returning the arithmetic
    /// operator it applies (`Operator::Add`).
    ///
    /// ```text
    /// augassign: '+=' | '-=' | '*=' | '/=' | '%=' | '**='
    /// ```
    fn parse_augassign(&mut self) -> Option<Operator> {
        let TokenType::Operator(text) = &self.peek().r#type else {
            return None;
        };
        let op = match text.as_str() {
            "+=" => Operator::Add,
            "-=" => Operator::Subtract,
            "*=" => Operator::Multiply,
            "/=" => Operator::Divide,
            "%=" => Operator::Modulo,
            "**=" => Operator::Power,
            _ => return None,
        };
        self.pos += 1;
        Some(op)
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
    ///     | enum_def
    ///     | struct_def
    ///     | impl_def
    ///     | if_stmt
    ///     | for_stmt
    ///     | while_stmt
    ///     | match_stmt
    /// ```
    pub(super) fn parse_compound_stmt(&mut self) -> Result<Box<Stmt>, ParserError> {
        let mark = self.pos;

        // function_def
        if let Ok(stmt) = self.parse_function_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // enum_def
        if let Ok(stmt) = self.parse_enum_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // struct_def
        if let Ok(stmt) = self.parse_struct_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // impl_def
        if let Ok(stmt) = self.parse_impl_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // trait_def
        if let Ok(stmt) = self.parse_trait_def() {
            return Ok(stmt);
        }
        self.pos = mark;

        // match_stmt
        if let Ok(stmt) = self.parse_match_stmt() {
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

    /// Parses a statement without a block, such as `x = 1`, `x += 1`, `print(x)`, or `ret x`.
    ///
    /// ```text
    /// simple_stmt:
    ///     | assignment
    ///     | aug_assignment
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

        // aug_assignment
        if let Ok(stmt) = self.parse_aug_assignment() {
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

    /// Parses one statement of either kind, such as `x = 1` or `if x: print(x)`.
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
