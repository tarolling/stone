////////////////////////////////////////////////////////////////
// AST Node Definitions
////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////
// ASDL Built-in Types
////////////////////////////////////////////////////////////////

use std::{error::Error, fmt::Display};

use crate::types::TokenType;

type Identifier = String;
type Int = i64;
// String already exists

////////////////////////////////////////////////////////////////
// Constant Type Definitions
////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Arbitrary precision integer; auto-promotes, never overflows
    Int(i64),
    // Signed integers
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    // Unsigned integers
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    // Pointer-sized integers
    ISize(isize),
    USize(usize),
    // Floating point numbers
    Float(f64),
    F32(f32),
    F64(f64),
    Decimal(f64),
    // Boolean
    Bool(bool),
    // Strings and characters
    Char(char),
    Str(String),
    // None
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
// ASDL Custom Types
////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub arg: Identifier,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arguments {
    pub args: Vec<Arg>,
}

/// AST Node - CompOp
///
/// cmpop = [`Eq`] | [`NotEq`] | [`Lt`] | [`LtE`] | [`Gt`] | [`GtE`]
#[derive(Debug, Clone, PartialEq)]
pub enum CompOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

/// AST Node - UnaryOp
///
/// unaryop = [`Invert`] | [`Not`] | [`UnaryAdd`] | [`UnarySub`]
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    UnaryAdd,
    UnarySub,
}

/// AST Node - Operator
///
/// operator = [`Add`] | [`Subtract`] | [`Multiply`] | [`Divide`]
#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
}

/// AST Node - BoolOp
///
/// boolop = [`And`] | [`Or`]
#[derive(Debug, Clone, PartialEq)]
pub enum BoolOp {
    And,
    Or,
}

/// AST Node - ExprContext
///
/// expr_context = [`Load`] | [`Store`] | [`Delete`]
#[derive(Debug, Clone, PartialEq)]
pub enum ExprContext {
    Load,
    Store,
    Delete,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Boolean operation
    BoolOp { op: BoolOp, values: Vec<Expr> },
    /// Binary operation
    BinOp {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    /// Unary operation
    UnaryOp { op: UnaryOp, operand: Box<Expr> },
    Compare {
        left: Box<Expr>,
        ops: Vec<CompOp>,
        comparators: Vec<Expr>,
    },
    /// Call
    Call { func: Box<Expr>, args: Vec<Expr> },
    /// Constant
    Constant {
        value: Box<Constant>,
        kind: Option<String>,
    },
    /// Subscript
    Subscript {
        value: Box<Expr>,
        slice: Box<Expr>,
        ctx: ExprContext,
    },
    /// Name
    Name { id: Identifier, ctx: ExprContext },
    /// List
    List { elts: Vec<Expr>, ctx: ExprContext },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Function definition
    FunctionDef {
        name: Identifier,
        args: Arguments,
        body: Vec<Stmt>,
    },

    /// Return statement
    Return { value: Option<Box<Expr>> },
    /// Delete statement
    Delete { targets: Vec<Expr> },
    /// Assignment
    Assign {
        targets: Vec<Expr>,
        value: Box<Expr>,
    },
    /// For loop
    For {
        target: Box<Expr>,
        iter: Box<Expr>,
        body: Vec<Stmt>,
    },
    /// While statement
    While { test: Box<Expr>, body: Vec<Stmt> },
    /// If statement
    If {
        test: Box<Expr>,
        body: Vec<Stmt>,
        orelse: Vec<Stmt>,
    },
    /// Expression statement
    Expr { value: Box<Expr> },
    /// Break
    Break,
    /// Continue
    Continue,
}

#[derive(Debug, PartialEq)]
pub enum Mod {
    Module { body: Vec<Stmt> },
}

// additional helper for parser
pub enum PrimaryOp {
    Subscript(Box<Expr>),
    Call(Vec<Expr>),
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub method: String,
    pub token: TokenType,
    pub line: usize,
    pub col: usize,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {:?} on line {}, col {}",
            self.method, self.token, self.line, self.col
        )
    }
}

impl Error for ParserError {}
