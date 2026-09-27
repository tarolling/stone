//! Pipelines that wire the lexer, parser, checker, and backends together.
//!
//! For example, `interpret("print(1)\n")` lexes, parses, checks, and evaluates the source, printing `1`.

use crate::ast::{Mod, Stmt};
use crate::checker::TypeChecker;
use crate::codegen::AssemblyGenerator;
use crate::codegen::x64::X64Generator;
use crate::debug;
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::error::Error;
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
    let tokens = lexer.lex();

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
}
