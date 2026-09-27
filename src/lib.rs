//! The stone programming language: a lexer, parser, tree-walking interpreter, and x86-64 compiler.
//!
//! For example, [`driver::interpret`] runs a source string and [`driver::compile`] turns one
//! into a native executable.

pub mod ast;
pub mod checker;
pub mod codegen;
pub mod driver;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod stdlib;
pub mod token;

/// Prints to stderr in debug builds only, so that program output on stdout stays clean.
///
/// For example, `debug!("{:?}", tokens)` dumps the token stream under `cargo run` but not under
/// `cargo run --release`.
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            eprintln!($($arg)*);
        }
    };
}
