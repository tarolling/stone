use clap::{Parser as ClapParser, Subcommand};
use std::error::Error;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use stone::ast::Mod;
use stone::diagnostic::{Diagnostic, Diagnostics, Severity};
use stone::driver;
use stone::interpreter::Limits;
use stone::project::FsSources;

#[cfg(feature = "self-update")]
mod update;

/// The stone programming language executor.
#[derive(ClapParser, Debug)]
#[command(author, version, about = "The stone programming language executor")]
#[command(args_conflicts_with_subcommands = true)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// The file to run when no subcommand is given.
    file: Option<String>,

    /// Arguments for the program, which it reads with `args()`.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true, requires = "file")]
    args: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Runs a file with the interpreter.
    Run {
        /// The file to run.
        file: String,
        /// Arguments for the program, which it reads with `args()`.
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
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
    /// Updates stone to the newest release.
    SelfUpdate {
        /// Only report whether a newer release exists.
        #[arg(long, conflicts_with = "version")]
        check: bool,
        /// The release to install instead of the newest, such as v0.1.0.
        #[arg(long, value_name = "TAG")]
        version: Option<String>,
    },
}

/// The mode stone runs in, chosen from the command-line arguments.
enum ExecutionMode {
    Build { file: String, output: PathBuf },
    Check(String),
    Repl,
    Run { file: String, args: Vec<String> },
    SelfUpdate { check: bool, tag: Option<String> },
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mode = match args.command {
        Some(Command::Run { file, args }) => ExecutionMode::Run { file, args },
        Some(Command::Build { file, output }) => ExecutionMode::Build { file, output },
        Some(Command::Check { file }) => ExecutionMode::Check(file),
        Some(Command::SelfUpdate { check, version }) => ExecutionMode::SelfUpdate {
            check,
            tag: version,
        },
        None => {
            if let Some(file) = args.file {
                ExecutionMode::Run {
                    file,
                    args: args.args,
                }
            } else {
                ExecutionMode::Repl
            }
        }
    };

    match mode {
        ExecutionMode::Build { file, output } => with_program(&file, |module| {
            driver::compile_module(module, &output)?;
            println!("Compiled {}", output.display());
            Ok(())
        }),
        ExecutionMode::Check(file) => {
            let Some(source) = read(&file) else {
                return ExitCode::FAILURE;
            };
            let (sources, diagnostics) =
                driver::check_program(Path::new(&file), &source, &FsSources);
            // warnings alone still pass
            let rendered: String = diagnostics.iter().map(|d| sources.render(d)).collect();
            eprint!("{rendered}");
            if diagnostics.iter().any(|d| d.severity == Severity::Error) {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        ExecutionMode::Repl => {
            println!("REPL mode - type your code (press Ctrl+D to exit)");
            let mut source = String::new();
            if let Err(e) = std::io::stdin().read_to_string(&mut source) {
                eprintln!("error: {e}");
                return ExitCode::FAILURE;
            }
            match driver::repl(&source) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => {
                    eprintln!("<stdin>: error: {e}");
                    ExitCode::FAILURE
                }
            }
        }
        ExecutionMode::Run { file, args } => with_program(&file, |module| {
            driver::run_module(
                module,
                &mut std::io::BufReader::new(std::io::stdin()),
                &mut std::io::stdout(),
                &args,
                Limits::DEFAULT,
            )
        }),
        ExecutionMode::SelfUpdate { check, tag } => self_update(check, tag.as_deref()),
    }
}

/// Runs `stone self-update`, printing any error to stderr and returning the exit code.
#[cfg(feature = "self-update")]
fn self_update(check: bool, tag: Option<&str>) -> ExitCode {
    match update::run(check, tag) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}

/// Explains how to upgrade a stone built without the `self-update` feature, such as one from
/// `cargo install`.
#[cfg(not(feature = "self-update"))]
fn self_update(_check: bool, _tag: Option<&str>) -> ExitCode {
    eprintln!(
        "error: this stone was built without self-update; upgrade it by rerunning install.sh \
         or `cargo install`, or build with `--features self-update`"
    );
    ExitCode::FAILURE
}

/// Reads `file`, printing why to stderr if it cannot be read.
fn read(file: &str) -> Option<String> {
    match std::fs::read_to_string(file) {
        Ok(source) => Some(source),
        Err(e) => {
            eprintln!("{file}: error: {e}");
            None
        }
    }
}

/// Loads, links, and checks the program whose entry file is `file`, then runs `action` on its
/// module, printing any error to stderr and returning the exit code.
///
/// Diagnostics are rendered with the path of the file they are in, the line, and a caret under
/// the problem, while other errors, such as runtime errors, are printed as `file: error: message`.
fn with_program(file: &str, action: impl FnOnce(&Mod) -> Result<(), Box<dyn Error>>) -> ExitCode {
    let Some(source) = read(file) else {
        return ExitCode::FAILURE;
    };
    let (sources, result) = driver::load(Path::new(file), &source, &FsSources);
    let err = match result {
        Ok(module) => match action(&module) {
            Ok(()) => return ExitCode::SUCCESS,
            Err(err) => err,
        },
        Err(diagnostics) => Box::new(diagnostics),
    };
    if let Some(Diagnostics(diagnostics)) = err.downcast_ref::<Diagnostics>() {
        let rendered: String = diagnostics.iter().map(|d| sources.render(d)).collect();
        eprint!("{rendered}");
    } else if let Some(diagnostic) = err.downcast_ref::<Diagnostic>() {
        eprint!("{}", sources.render(diagnostic));
    } else {
        eprintln!("{file}: error: {err}");
    }
    ExitCode::FAILURE
}
