//! Pipelines that wire the lexer, parser, checker, and backends together.
//!
//! For example, `interpret("print(1)\n")` lexes, parses, checks, and evaluates the source, printing `1`.

use crate::ast::{Mod, Stmt};
use crate::checker::TypeChecker;
use crate::codegen::AssemblyGenerator;
use crate::codegen::x64::X64Generator;
use crate::debug;
use crate::interpreter::{Interpreter, Limits};
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::error::Error;
use std::io::Write;
use std::path::Path;

/// Resolves names to their definitions before type checking. Not implemented yet.
pub struct Resolver;

impl Resolver {
    pub fn new() -> Self {
        Self
    }

    pub fn resolve(&self, _ast: &[Stmt]) {
        todo!()
    }
}

impl Default for Resolver {
    fn default() -> Self {
        Self::new()
    }
}

/// Lexes and parses source code into a module.
///
/// For example, `parse("x = 42\n")` returns a module holding one assignment.
pub fn parse(source: &str) -> Result<Mod, Box<dyn Error>> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex()?;

    debug!("{:?}", tokens);

    let mut parser = Parser::new(&tokens);
    Ok(parser.parse()?)
}

/// Runs source code with the tree-walking interpreter.
///
/// For example, `interpret("print(1 + 2)\n")` prints `3`.
pub fn interpret(source: &str) -> Result<(), Box<dyn Error>> {
    let ast = parse(source)?;

    // resolve names
    // let resolver = Resolver::new();
    // resolver.resolve(&ast);

    let mut checker = TypeChecker::new();
    checker.check(&ast)?;

    let mut interpreter = Interpreter::new();
    interpreter.evaluate(&ast)
}

/// Runs source code with the tree-walking interpreter, printing to `out` and stopping the program
/// with an error once it exceeds `limits`.
///
/// For example, `interpret_with("print(1)\n", &mut buffer, limits)` appends `1\n` to `buffer`,
/// while `interpret_with("while 1;\n    x = 1\n", ...)` returns an error once fuel runs out.
pub fn interpret_with(
    source: &str,
    out: &mut impl Write,
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    let ast = parse(source)?;

    let mut checker = TypeChecker::new();
    checker.check(&ast)?;

    let mut interpreter = Interpreter::with_output(out, limits);
    interpreter.evaluate(&ast)
}

/// Compiles source code to a native x86-64 executable at `output`.
///
/// For example, `compile("print(1)\n", Path::new("build/out"))` writes `build/out.s` and links
/// `build/out`.
pub fn compile(source: &str, output: &Path) -> Result<(), Box<dyn Error>> {
    let ast = parse(source)?;

    let mut r#gen = X64Generator::new();
    r#gen.compile(&ast, output)?;
    Ok(())
}

/// Checks source code for errors without running it. Not implemented yet.
pub fn check(_source: &str) -> Result<(), Box<dyn Error>> {
    todo!()
}

/// Runs an interactive session over the given input. Not implemented yet.
pub fn repl(_source: &str) -> Result<(), Box<dyn Error>> {
    todo!()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_program() {
        let source = r#"
x = 42
y = x + 8
ret y
"#;

        let _ = interpret(source);
    }

    /// Interprets `source` with small limits and returns what it printed, or the error message.
    ///
    /// For example, `run_limited("print(1)\n")` returns `Ok("1\n")`.
    fn run_limited(source: &str) -> Result<String, String> {
        let mut out = Vec::new();
        let limits = Limits {
            fuel: 10_000,
            max_depth: 50,
        };
        interpret_with(source, &mut out, limits).map_err(|e| e.to_string())?;
        Ok(String::from_utf8(out).unwrap())
    }

    #[test]
    fn print_goes_to_the_given_writer() {
        assert_eq!(run_limited("print(1, 2)\n"), Ok("1 2\n".to_string()));
    }

    #[test]
    fn infinite_loop_runs_out_of_fuel() {
        assert!(run_limited("while 1;\n    x = 1\n").is_err());
    }

    #[test]
    fn unbounded_recursion_hits_the_depth_limit() {
        assert!(run_limited("def f(n);\n    ret f(n)\nf(1)\n").is_err());
    }

    #[test]
    fn deep_expressions_inside_recursion_hit_the_depth_limit() {
        let source = format!("def f(n);\n    ret {}f(n)\nf(1)\n", "-".repeat(40));
        assert!(run_limited(&source).is_err());
    }

    #[test]
    fn division_by_zero_is_an_error() {
        assert!(run_limited("print(1 / 0)\n").is_err());
    }

    #[test]
    fn dividing_the_minimum_by_negative_one_is_an_error() {
        // idiv traps on this in compiled code too
        assert!(run_limited("x = 0 - 9223372036854775807 - 1\nprint(x / -1)\n").is_err());
    }

    #[test]
    fn integer_overflow_wraps() {
        assert_eq!(
            run_limited("print(9223372036854775807 + 1)\n"),
            Ok("-9223372036854775808\n".to_string())
        );
    }

    #[test]
    fn ret_inside_if_returns_from_the_function() {
        let source = "def f(x);\n    if x;\n        ret 1\n    ret 0\nprint(f(5))\n";
        assert_eq!(run_limited(source), Ok("1\n".to_string()));
    }

    #[test]
    fn while_reevaluates_its_test() {
        let source = "i = 0\nwhile i - 2;\n    i = i + 1\nprint(i)\n";
        assert_eq!(run_limited(source), Ok("2\n".to_string()));
    }

    #[test]
    fn parameters_do_not_overwrite_caller_variables() {
        let source = "def f(a);\n    ret a\na = 7\nprint(f(1))\nprint(a)\n";
        assert_eq!(run_limited(source), Ok("1\n7\n".to_string()));
    }
}
