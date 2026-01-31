//! Stone Datatype Definitions

mod ast;
mod grammar;

pub use ast::*;
pub use grammar::*;

////////////////////////////////////////////////////////////////
// Type Annotation
////////////////////////////////////////////////////////////////

pub enum Type {
    /// Arbitrary precision integer; auto-promotes, never overflows
    Int,
    // Signed integers
    I8,
    I16,
    I32,
    I64,
    I128,
    // Unsigned integers
    U8,
    U16,
    U32,
    U64,
    U128,
    // Pointer-sized integers
    ISize,
    USize,
    // Floating point numbers
    Float,
    F32,
    F64,
    Decimal,
    // Boolean
    Bool,
    // Strings and characters
    Char,
    String,
    // None
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
