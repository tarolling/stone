use clap::{Parser as ClapParser, Subcommand};
use std::io::Read;
use std::path::PathBuf;
use stone::driver;

/// The stone programming language executor.
#[derive(ClapParser, Debug)]
#[command(author, version, about = "The stone programming language executor")]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// The file to run when no subcommand is given.
    file: Option<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Runs a file with the interpreter.
    Run {
        /// The file to run.
        file: String,
    },
    /// Compiles a file to a native executable.
    Build {
        /// The file to build.
        file: String,
        /// Where to write the executable. The assembly goes next to it with a `.s` extension.
        #[arg(short, long, default_value = "build/out")]
        output: PathBuf,
    },
    /// Checks a file for errors without running it.
    Check {
        /// The file to check.
        file: String,
    },
}

/// The mode stone runs in, chosen from the command-line arguments.
enum ExecutionMode {
    Build { file: String, output: PathBuf },
    Check(String),
    Repl,
    Run(String),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let mode = match args.command {
        Some(Command::Run { file }) => ExecutionMode::Run(file),
        Some(Command::Build { file, output }) => ExecutionMode::Build { file, output },
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
        ExecutionMode::Build { file, output } => {
            driver::compile(&std::fs::read_to_string(file)?, &output)?;
            println!("Compiled {}", output.display());
            Ok(())
        }
        ExecutionMode::Check(file) => driver::check(&std::fs::read_to_string(file)?),
        ExecutionMode::Repl => {
            println!("REPL mode - type your code (press Ctrl+D to exit)");
            let mut source = String::new();
            std::io::stdin().read_to_string(&mut source)?;
            driver::repl(&source)
        }
        ExecutionMode::Run(file) => driver::interpret(&std::fs::read_to_string(file)?),
    }
}
