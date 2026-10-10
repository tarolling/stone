//! The stone programming language: a lexer, parser, tree-walking interpreter, and x86-64 compiler.
//!
//! For example, [`driver::interpret`] runs a source string and [`driver::compile`] turns one
//! into a native executable.

pub mod ast;
pub mod checker;
pub mod codegen;
pub mod diagnostic;
pub mod driver;
pub mod interpreter;
pub mod last_use;
pub mod lexer;
pub mod parser;
pub mod project;
pub mod repl;
pub mod span;
pub mod stdlib;
pub mod token;

/// Prints to stderr in debug builds when the `STONE_DEBUG` environment variable is set, so that
/// program output and diagnostics stay clean by default.
///
/// For example, `STONE_DEBUG=1 cargo run -- run file.st` dumps the token stream and parser trace,
/// while plain `cargo run` and `cargo run --release` do not. It is also silent under `cargo fuzz`,
/// which builds with debug assertions but would slow to a crawl printing traces for every input.
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) && !cfg!(fuzzing) && $crate::debug_enabled() {
            eprintln!($($arg)*);
        }
    };
}

/// Returns whether `STONE_DEBUG` is set, reading the environment only once.
#[doc(hidden)]
pub fn debug_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("STONE_DEBUG").is_some())
}
