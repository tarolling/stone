use crate::checker::TypeChecker;
use crate::generators::X64Generator;
use crate::interpreter::Interpreter;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::types::Stmt;
use clap::{Parser as ClapParser, Subcommand};
use std::io::Read;

mod checker;
mod generators;
mod interpreter;
mod lexer;
mod parser;
mod stdlib;
mod types;

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        if cfg!(debug_assertions) {
            println!($($arg)*);
        }
    };
}

////////////////////////////////////////////////////////////////
// Argument Parsing
////////////////////////////////////////////////////////////////

/// The stone programming language executor
#[derive(ClapParser, Debug)]
#[command(author, version, about = "The stone programming language executor")]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// File to process (used with run mode if no subcommand specified)
    file: Option<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Run mode - execute a file
    Run {
        /// File to run
        file: String,
    },
    /// Build mode - compile a file
    Build {
        /// File to build
        file: String,
    },
    /// Check mode - check for errors
    Check {
        /// File to check
        file: String,
    },
}

/// Defines what mode to run with
enum ExecutionMode {
    Build(String),
    Check(String),
    Repl,
    Run(String),
}

////////////////////////////////////////////////////////////////
// Name Resolver
////////////////////////////////////////////////////////////////

struct Resolver;

impl Resolver {
    fn new() -> Self {
        Self
    }

    pub fn resolve(&self, _ast: &[Stmt]) {
        todo!()
    }
}

////////////////////////////////////////////////////////////////
// Execution Pipelines
////////////////////////////////////////////////////////////////

fn execute_compiler_pipeline(source: &str) {
    // Lex
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex();

    // Parse
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();

    match ast {
        Ok(ast) => {
            // Generate x64
            let mut r#gen = X64Generator::new();
            let _asm = r#gen.compile(&ast);
        }
        Err(e) => panic!("{}", e),
    }
}

fn execute_check_pipeline(_source: &str) {
    todo!()
}

fn execute_repl_pipeline(_source: &str) {
    todo!()
}

fn execute_interpreter_pipeline(source: &str) {
    // Lex
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex();

    debug!("{:?}", tokens);

    // Parse
    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();

    match ast {
        Ok(ast) => {
            // Resolve names
            // let resolver = Resolver::new();
            // resolver.resolve(&ast);

            // Type Check
            let mut checker = TypeChecker::new();
            let _ = checker.check(&ast);

            // Interpret
            let mut interpreter = Interpreter::new();
            let _ = interpreter.evaluate(&ast);
        }
        Err(e) => panic!("{}", e),
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let mode = match args.command {
        Some(Command::Run { file }) => ExecutionMode::Run(file),
        Some(Command::Build { file }) => ExecutionMode::Build(file),
        Some(Command::Check { file }) => ExecutionMode::Check(file),
        None => {
            if let Some(file) = args.file {
                ExecutionMode::Run(file)
            } else {
                ExecutionMode::Repl
            }
        }
    };

    match mode {
        ExecutionMode::Build(file) => {
            let mut source = String::new();
            let mut file = std::fs::File::open(&file)?;
            file.read_to_string(&mut source)?;
            execute_compiler_pipeline(&source);
        }
        ExecutionMode::Check(file) => {
            let mut source = String::new();
            let mut file = std::fs::File::open(&file)?;
            file.read_to_string(&mut source)?;
            execute_check_pipeline(&source)
        }
        ExecutionMode::Repl => {
            println!("REPL mode - type your code (press Ctrl+D to exit)");
            let mut source = String::new();
            std::io::stdin().read_to_string(&mut source)?;
            execute_repl_pipeline(&source);
        }
        ExecutionMode::Run(file) => {
            let mut source = String::new();
            let mut file = std::fs::File::open(&file)?;
            file.read_to_string(&mut source)?;
            execute_interpreter_pipeline(&source);
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_simple_program() {
        let source = r#"
x = 42
y = x + 8
ret y
"#;

        execute_interpreter_pipeline(source);
    }
}
