use clap::{Parser as ClapParser, Subcommand};
use std::error::Error;
use std::io::Read;
use std::path::PathBuf;
use std::process::ExitCode;
use stone::diagnostic::{Diagnostic, Diagnostics, Severity};
use stone::driver;

#[cfg(feature = "self-update")]
mod update;

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
    Run(String),
    SelfUpdate { check: bool, tag: Option<String> },
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mode = match args.command {
        Some(Command::Run { file }) => ExecutionMode::Run(file),
        Some(Command::Build { file, output }) => ExecutionMode::Build { file, output },
        Some(Command::Check { file }) => ExecutionMode::Check(file),
        Some(Command::SelfUpdate { check, version }) => ExecutionMode::SelfUpdate {
            check,
            tag: version,
        },
        None => {
            if let Some(file) = args.file {
                ExecutionMode::Run(file)
            } else {
                ExecutionMode::Repl
            }
        }
    };

    match mode {
        ExecutionMode::Build { file, output } => with_source(&file, |source| {
            driver::compile(source, &output)?;
            println!("Compiled {}", output.display());
            Ok(())
        }),
        ExecutionMode::Check(file) => with_source(&file, |source| {
            let diagnostics = driver::check(source);
            if diagnostics.iter().any(|d| d.severity == Severity::Error) {
                return Err(Box::new(Diagnostics(diagnostics)));
            }
            // warnings alone still pass
            eprint!("{}", render_all(&file, source, &diagnostics));
            Ok(())
        }),
        ExecutionMode::Repl => {
            println!("REPL mode - type your code (press Ctrl+D to exit)");
            let mut source = String::new();
            if let Err(e) = std::io::stdin().read_to_string(&mut source) {
                eprintln!("error: {e}");
                return ExitCode::FAILURE;
            }
            with_source("<stdin>", |_| driver::repl(&source))
        }
        ExecutionMode::Run(file) => with_source(&file, driver::interpret),
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

/// Reads `file` and runs `action` on its contents, printing any error to stderr and returning
/// the exit code.
///
/// Diagnostics are rendered with the file name, line, and a caret under the problem, while other
/// errors, such as runtime errors, are printed as `file: error: message`.
fn with_source(file: &str, action: impl FnOnce(&str) -> Result<(), Box<dyn Error>>) -> ExitCode {
    let source = match std::fs::read_to_string(file) {
        Ok(source) => source,
        Err(e) => {
            eprintln!("{file}: error: {e}");
            return ExitCode::FAILURE;
        }
    };
    let Err(err) = action(&source) else {
        return ExitCode::SUCCESS;
    };
    if let Some(Diagnostics(diagnostics)) = err.downcast_ref::<Diagnostics>() {
        eprint!("{}", render_all(file, &source, diagnostics));
    } else if let Some(diagnostic) = err.downcast_ref::<Diagnostic>() {
        eprint!("{}", diagnostic.render(file, &source));
    } else {
        eprintln!("{file}: error: {err}");
    }
    ExitCode::FAILURE
}

/// Renders each diagnostic for a terminal, one after another.
fn render_all(file: &str, source: &str, diagnostics: &[Diagnostic]) -> String {
    diagnostics.iter().map(|d| d.render(file, source)).collect()
}
