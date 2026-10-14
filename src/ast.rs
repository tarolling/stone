//! Abstract syntax tree node definitions, modeled on `docs/grammar/stone.asdl`.
//!
//! For example, `x = 42` parses into a [`Stmt::Assign`] whose value is an [`Expr::Constant`].

////////////////////////////////////////////////////////////////
// syntax tree node definitions
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// built-in types from the asdl
////////////////////////////////////////////////////////////////

use std::{error::Error, fmt::Display};

use crate::span::Span;
use crate::token::TokenType;

type Identifier = String;
// `String` is built in

////////////////////////////////////////////////////////////////
// constant type definitions
////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// An arbitrary-precision integer that auto-promotes and never overflows, such as `42`.
    Int(i64),
    // signed integers
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    // unsigned integers
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    // pointer-sized integers
    ISize(isize),
    USize(usize),
    // floating point numbers
    Float(f64),
    F32(f32),
    F64(f64),
    Decimal(f64),
    // boolean
    Bool(bool),
    // strings and characters
    Char(char),
    Str(String),
    // none
    None,
}

impl Constant {
    pub fn type_name(&self) -> &str {
        match self {
            Constant::Int(_) => "int",
            Constant::I8(_) => "i8",
            Constant::I16(_) => "i16",
            Constant::I32(_) => "i32",
            Constant::I64(_) => "i64",
            Constant::I128(_) => "i128",
            Constant::U8(_) => "u8",
            Constant::U16(_) => "u16",
            Constant::U32(_) => "u32",
            Constant::U64(_) => "u64",
            Constant::U128(_) => "u128",
            Constant::ISize(_) => "isize",
            Constant::USize(_) => "usize",
            Constant::Float(_) => "float",
            Constant::F32(_) => "f32",
            Constant::F64(_) => "f64",
            Constant::Decimal(_) => "decimal",
            Constant::Bool(_) => "bool",
            Constant::Char(_) => "char",
            Constant::Str(_) => "str",
            Constant::None => "none",
        }
    }
}

////////////////////////////////////////////////////////////////
// custom types from the asdl
////////////////////////////////////////////////////////////////

/// A function parameter, the span of its name, and its type, such as `a: int` in
/// `fn f(a: int);`.
#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub arg: Identifier,
    pub span: Span,
    /// The parameter's type, which the checker requires but the parser accepts without, so
    /// that a missing one is a type error and editor features keep working.
    pub annotation: Option<TypeExpr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arguments {
    pub args: Vec<Arg>,
}

/// A comparison operator, such as `<` in `a < b`.
///
/// ```text
/// cmpop = Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

/// A unary operator, such as `-` in `-x`.
///
/// ```text
/// unaryop = Not | UnaryAdd | UnarySub
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,
    UnaryAdd,
    UnarySub,
}

/// A binary arithmetic operator, such as `+` in `a + b`.
///
/// ```text
/// operator = Add | Subtract | Multiply | Divide | Modulo | Power
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    /// `%`, the remainder of `/`, which takes the dividend's sign: `-7 % 2` is `-1`.
    Modulo,
    /// `**`, which raises an int or float base to an int exponent: `2 ** 10` is `1024`.
    Power,
}

/// A boolean operator, such as `and` in `a and b`.
///
/// ```text
/// boolop = And | Or
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

/// How a name or subscript is used: `x` in `print(x)` is loaded, and `x` in `x = 1` is stored.
///
/// ```text
/// expr_context = Load | Store
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum ExprContext {
    Load,
    Store,
}

/// An expression and the span of source text it was parsed from.
///
/// For example, `a + 12` in `x = a + 12` is a [`ExprKind::BinOp`] spanning cols 5 to 11.
#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Expr { kind, span }
    }
}

/// The kind of an [`Expr`], such as a call or a binary operation.
#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    /// A boolean operation, such as `a and b`.
    BoolOp { op: BoolOp, values: Vec<Expr> },
    /// A binary operation, such as `a + b`.
    BinOp {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// A unary operation, such as `-x`.
    UnaryOp { op: UnaryOp, operand: Box<Expr> },
    Compare {
        left: Box<Expr>,
        ops: Vec<CompOp>,
        comparators: Vec<Expr>,
    },
    /// A call, such as `f(1, 2)`, with any named values after the others, such as `x=1.0` in
    /// `Point(x=1.0, y=2.0)`, which only a struct takes.
    Call {
        func: Box<Expr>,
        args: Vec<Expr>,
        keywords: Vec<Keyword>,
    },
    /// A literal value, such as `42`, `"hi"`, or `true`.
    Constant {
        value: Box<Constant>,
        kind: Option<String>,
    },
    /// An index into a value, such as `a[0]`.
    Subscript {
        value: Box<Expr>,
        slice: Box<Expr>,
        ctx: ExprContext,
    },
    /// An attribute of a value: a struct's field, such as `p.x`, which an assignment can also
    /// change (ctx `Store`), or, as the function of a call, a method, such as `xs.len` in
    /// `xs.len()`.
    Attribute {
        value: Box<Expr>,
        attr: Identifier,
        ctx: ExprContext,
    },
    /// A variable reference, such as `x`.
    Name { id: Identifier, ctx: ExprContext },
    /// A list literal, such as `[1, 2, 3]`.
    List { elts: Vec<Expr>, ctx: ExprContext },
    /// A tuple of two or more values, such as `(1, "a")`, or, as an assignment or `for` target,
    /// names to unpack a tuple into, such as `q, r` in `q, r = divide(7, 2)`.
    Tuple { elts: Vec<Expr>, ctx: ExprContext },
    /// The bounds of a slice, such as `1:` in `xs[1:]`, which can only be a
    /// [`ExprKind::Subscript`]'s `slice`. A missing bound is `None`.
    Slice {
        lower: Option<Box<Expr>>,
        upper: Option<Box<Expr>>,
        step: Option<Box<Expr>>,
    },
}

/// A named value in a call, such as `x=1.0` in `Point(x=1.0, y=2.0)`.
#[derive(Debug, Clone, PartialEq)]
pub struct Keyword {
    pub arg: Identifier,
    /// The span of the name before `=`.
    pub arg_span: Span,
    pub value: Expr,
}

/// Returns the value of `expr` if it is an int literal, possibly negated.
///
/// For example, `8` gives `Some(8)`, `-4` gives `Some(-4)`, and `x` or `2 + 2` gives `None`.
pub fn constant_int(expr: &Expr) -> Option<i64> {
    match &expr.kind {
        ExprKind::Constant { value, .. } => match **value {
            Constant::Int(n) => Some(n),
            _ => None,
        },
        ExprKind::UnaryOp {
            op: UnaryOp::UnarySub,
            operand,
        } => constant_int(operand).map(i64::wrapping_neg),
        _ => None,
    }
}

/// Pushes the names a target assigns, in order: the name itself, or each name of a tuple target,
/// at any depth.
///
/// For example, `a, (b, c)` in `a, (b, c) = t` gives `a`, `b`, and `c`, and a subscript target
/// such as `xs[0]` gives nothing.
pub fn target_names<'a>(target: &'a Expr, out: &mut Vec<&'a Expr>) {
    match &target.kind {
        ExprKind::Name { .. } => out.push(target),
        ExprKind::Tuple { elts, .. } => elts.iter().for_each(|elt| target_names(elt, out)),
        _ => {}
    }
}

/// A statement and the span of source text it was parsed from.
///
/// For example, `x = 1` spans cols 1 to 6, and a compound statement ends at the last token of its
/// block.
#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span) -> Self {
        Stmt { kind, span }
    }
}

/// The kind of a [`Stmt`], such as an assignment or a loop.
#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    /// A function definition, such as `fn add(a: int, b: int) -> int; ret a + b`.
    FunctionDef {
        name: Identifier,
        /// The span of the function's name, which editor features point at.
        name_span: Span,
        /// Whether the definition starts with `pub`, so other modules may use it.
        public: bool,
        args: Arguments,
        /// The type after `->`, if any; a function without one returns no value.
        returns: Option<TypeExpr>,
        body: Vec<Stmt>,
    },

    /// A return statement, such as `ret x`.
    Return { value: Option<Box<Expr>> },
    /// An assignment, such as `x = 1` or `a = b = 1`.
    Assign {
        targets: Vec<Expr>,
        value: Box<Expr>,
    },
    /// An augmented assignment, such as `x += 1` or `grid[i][j] *= 2`, which means
    /// `target = target op value` except that the target's indexes are evaluated once: first the
    /// indexes, then the target's old value, then `value`.
    AugAssign {
        target: Box<Expr>,
        op: Operator,
        value: Box<Expr>,
    },
    /// A `for` loop, such as `for i in 10; print(i)`.
    For {
        target: Box<Expr>,
        iter: Box<Expr>,
        body: Vec<Stmt>,
    },
    /// A `while` loop, such as `while x; x = x - 1`.
    While { test: Box<Expr>, body: Vec<Stmt> },
    /// An `if` statement, where `elif` and `else` branches are nested in `orelse`.
    If {
        test: Box<Expr>,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    /// An expression used as a statement, such as `print(x)`.
    Expr { value: Box<Expr> },
    /// An import, such as `use geometry.shapes` or `use util.pad as lpad`, holding each dotted
    /// name and the optional name after `as`, each with its span.
    Use {
        path: Vec<(Identifier, Span)>,
        alias: Option<(Identifier, Span)>,
    },
    /// An enum definition, such as `enum Shape;` followed by an indented line for each variant,
    /// like `Circle(float)` or `Empty`.
    EnumDef {
        name: Identifier,
        /// The span of the enum's name, which editor features point at.
        name_span: Span,
        /// Whether the definition starts with `pub`, so other modules may use it.
        public: bool,
        variants: Vec<Variant>,
    },
    /// A struct definition, such as `struct Point;` followed by an indented line for each
    /// field, like `x: float`.
    StructDef {
        name: Identifier,
        /// The span of the struct's name, which editor features point at.
        name_span: Span,
        /// Whether the definition starts with `pub`, so other modules may use it.
        public: bool,
        fields: Vec<Field>,
    },
    /// A `match` statement, which runs the first case whose pattern matches `subject`, such as
    /// `match s;` followed by `case Shape.Circle(r); print(r)`.
    Match {
        subject: Box<Expr>,
        cases: Vec<MatchCase>,
    },
    /// A `break` statement.
    Break,
    /// A `cont` statement, which continues to the next loop iteration.
    Continue,
}

/// One variant of an enum, such as `Rect(float, float)`, with the types of its values in order.
#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub name: Identifier,
    /// The span of the variant's name.
    pub span: Span,
    pub fields: Vec<TypeExpr>,
}

/// One field of a struct, such as `x: float`.
#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Identifier,
    /// The span of the field's name.
    pub span: Span,
    pub annotation: TypeExpr,
}

/// One case of a `match` statement, such as `case some(x); print(x)`.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Vec<Stmt>,
}

/// A pattern after `case`, such as `Shape.Rect(w, _)`.
#[derive(Debug, Clone, PartialEq)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

/// The kind of a [`Pattern`].
#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    /// `_`, which matches anything and binds nothing.
    Wildcard,
    /// A name, which matches anything and binds it, such as `r` in `Shape.Circle(r)`.
    Name(Identifier),
    /// A variant and a pattern for each of its values: `path` is the variant's name with what
    /// qualifies it, such as `["Shape", "Circle"]` for `Shape.Circle(r)`, `["some"]` for
    /// `some(x)`, and `["none"]` for `none`.
    Variant {
        path: Vec<(Identifier, Span)>,
        fields: Vec<Pattern>,
    },
}

impl Pattern {
    /// Pushes the names the pattern binds, with their spans, in order.
    ///
    /// For example, `Shape.Rect(w, _)` gives `w`, and `some(Tree.Node(a, b))` gives `a` and `b`.
    pub fn names<'a>(&'a self, out: &mut Vec<(&'a str, Span)>) {
        match &self.kind {
            PatternKind::Wildcard => {}
            PatternKind::Name(name) => out.push((name, self.span)),
            PatternKind::Variant { fields, .. } => fields.iter().for_each(|f| f.names(out)),
        }
    }
}

impl Display for Pattern {
    /// Writes the pattern as it is written in source, such as `Shape.Rect(w, _)`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            PatternKind::Wildcard => write!(f, "_"),
            PatternKind::Name(name) => write!(f, "{name}"),
            PatternKind::Variant { path, fields } => {
                let names: Vec<&str> = path.iter().map(|(name, _)| name.as_str()).collect();
                write!(f, "{}", names.join("."))?;
                if !fields.is_empty() {
                    write!(f, "(")?;
                    for (i, field) in fields.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{field}")?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Mod {
    Module { body: Vec<Stmt> },
}

/// A call, subscript, or attribute that follows an atom, along with the span of its last token.
///
/// For example, `(1)` in `f(1)` is a `Call` whose span covers the `)`, and `.len` in `xs.len()` is
/// an `Attribute` whose span covers `len`.
pub enum PrimaryOp {
    Subscript(Box<Expr>, Span),
    Call(Vec<Expr>, Vec<Keyword>, Span),
    Attribute(Identifier, Span),
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub method: String,
    pub token: TokenType,
    pub span: Span,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {:?} on line {}, col {}",
            self.method, self.token, self.span.start.line, self.span.start.col
        )
    }
}

impl Error for ParserError {}

////////////////////////////////////////////////////////////////
// type annotations
////////////////////////////////////////////////////////////////

/// A type written in a function's signature, such as `list[int]` in `fn f(xs: list[int]);`.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeExpr {
    pub kind: TypeExprKind,
    pub span: Span,
}

/// The kind of a [`TypeExpr`], one of the types the checker knows.
#[derive(Debug, Clone, PartialEq)]
pub enum TypeExprKind {
    Int,
    Float,
    Bool,
    Str,
    /// A list of the given element type, such as `list[float]`.
    List(Box<TypeExpr>),
    /// A tuple of two or more types, such as `tuple[int, str]`.
    Tuple(Vec<TypeExpr>),
    /// Any other type by name, with its type arguments: a struct or enum such as `Shape` or
    /// `shapes.Shape`, or `option[int]`.
    Named {
        name: String,
        args: Vec<TypeExpr>,
    },
}

impl Display for TypeExpr {
    /// Writes the type as it is written in source, such as `list[list[int]]`.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            TypeExprKind::Int => write!(f, "int"),
            TypeExprKind::Float => write!(f, "float"),
            TypeExprKind::Bool => write!(f, "bool"),
            TypeExprKind::Str => write!(f, "str"),
            TypeExprKind::List(elem) => write!(f, "list[{elem}]"),
            TypeExprKind::Tuple(elts) => {
                write!(f, "tuple[")?;
                for (i, elt) in elts.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{elt}")?;
                }
                write!(f, "]")
            }
            TypeExprKind::Named { name, args } => {
                write!(f, "{name}")?;
                if !args.is_empty() {
                    write!(f, "[")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{arg}")?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            }
        }
    }
}
