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
#[allow(dead_code)] // mirrors the asdl, unused until int annotations land
type Int = i64;
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

/// A function parameter and the span of its name, such as `a` in `def f(a);`.
#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub arg: Identifier,
    pub span: Span,
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
#[derive(Debug, Clone, PartialEq)]
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
#[derive(Debug, Clone, PartialEq)]
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
/// expr_context = Load | Store | Delete
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum ExprContext {
    Load,
    Store,
    Delete,
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
    /// A function call, such as `f(1, 2)`.
    Call { func: Box<Expr>, args: Vec<Expr> },
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
    /// An attribute of a value, such as `xs.len` in `xs.len()`.
    ///
    /// Attributes are only meaningful as the function of a call, which calls a method.
    Attribute {
        value: Box<Expr>,
        attr: Identifier,
        ctx: ExprContext,
    },
    /// A variable reference, such as `x`.
    Name { id: Identifier, ctx: ExprContext },
    /// A list literal, such as `[1, 2, 3]`.
    List { elts: Vec<Expr>, ctx: ExprContext },
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
    /// A function definition, such as `def add(a, b); ret a + b`.
    FunctionDef {
        name: Identifier,
        /// The span of the function's name, which editor features point at.
        name_span: Span,
        /// Whether the definition starts with `pub`, so other modules may use it.
        public: bool,
        args: Arguments,
        body: Vec<Stmt>,
    },

    /// A return statement, such as `ret x`.
    Return { value: Option<Box<Expr>> },
    /// A delete statement that removes variables from scope.
    Delete { targets: Vec<Expr> },
    /// An assignment, such as `x = 1` or `a = b = 1`.
    Assign {
        targets: Vec<Expr>,
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
    /// A `break` statement.
    Break,
    /// A `cont` statement, which continues to the next loop iteration.
    Continue,
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
    Call(Vec<Expr>, Span),
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

pub enum Type {
    /// An arbitrary-precision integer that auto-promotes and never overflows, such as `42`.
    Int,
    // signed integers
    I8,
    I16,
    I32,
    I64,
    I128,
    // unsigned integers
    U8,
    U16,
    U32,
    U64,
    U128,
    // pointer-sized integers
    ISize,
    USize,
    // floating point numbers
    Float,
    F32,
    F64,
    Decimal,
    // boolean
    Bool,
    // strings and characters
    Char,
    String,
    // none
    None,
}

impl Type {
    pub fn as_str(&self) -> &str {
        match self {
            Type::Int => "int",
            Type::I8 => "i8",
            Type::I16 => "i16",
            Type::I32 => "i32",
            Type::I64 => "i64",
            Type::I128 => "i128",
            Type::U8 => "u8",
            Type::U16 => "u16",
            Type::U32 => "u32",
            Type::U64 => "u64",
            Type::U128 => "u128",
            Type::ISize => "isize",
            Type::USize => "usize",
            Type::Float => "float",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Decimal => "decimal",
            Type::Bool => "bool",
            Type::Char => "char",
            Type::String => "str",
            Type::None => "none",
        }
    }
}
