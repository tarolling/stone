use clap::{Parser as ClapParser, Subcommand};
use std::error::Error;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use stone::ast::Mod;
use stone::codegen::Target;
use stone::diagnostic::{Diagnostic, Diagnostics, Severity};
use stone::driver;
use stone::interpreter::{Exit, Limits};
use stone::project::FsSources;

mod uninstall;
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
        /// The system to build for: `x86_64` or `aarch64` for Linux, or `aarch64-macos`.
        /// Defaults to this machine, and any can be built for from any machine.
        #[arg(long, value_name = "TARGET", value_parser = str::parse::<Target>)]
        target: Option<Target>,
    },
    /// Checks a file for errors without running it.
    Check {
        /// The file to check.
        file: String,
    },
    /// Updates stone to the newest release.
    Update {
        /// Only report whether a newer release exists.
        #[arg(long, conflicts_with = "version")]
        check: bool,
        /// The release to install instead of the newest, such as v0.1.0.
        #[arg(long, value_name = "TAG")]
        version: Option<String>,
    },
    /// Removes the stone binary.
    Uninstall {
        /// Remove it without asking first.
        #[arg(short, long)]
        yes: bool,
    },
}

/// The mode stone runs in, chosen from the command-line arguments.
enum ExecutionMode {
    Build {
        file: String,
        output: PathBuf,
        /// What to build for, or the host if not given.
        target: Option<Target>,
    },
    Check(String),
    Repl,
    Run {
        file: String,
        args: Vec<String>,
    },
    Uninstall {
        yes: bool,
    },
    Update {
        check: bool,
        tag: Option<String>,
    },
}

fn main() -> ExitCode {
    let args = Args::parse();

    let mode = match args.command {
        Some(Command::Run { file, args }) => ExecutionMode::Run { file, args },
        Some(Command::Build {
            file,
            output,
            target,
        }) => ExecutionMode::Build {
            file,
            output,
            target,
        },
        Some(Command::Check { file }) => ExecutionMode::Check(file),
        Some(Command::Update { check, version }) => ExecutionMode::Update {
            check,
            tag: version,
        },
        Some(Command::Uninstall { yes }) => ExecutionMode::Uninstall { yes },
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
        ExecutionMode::Build {
            file,
            output,
            target,
        } => with_program(&file, |module| {
            let target = target.map_or_else(Target::host, Ok)?;
            driver::compile_module(module, &output, target)?;
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
            // prompts only make sense for a person typing, so piped input runs quietly
            let prompts = std::io::stdin().is_terminal();
            if prompts {
                eprintln!("stone {}. Press Ctrl+D to exit.", env!("CARGO_PKG_VERSION"));
            }
            match driver::repl(
                &mut std::io::BufReader::new(std::io::stdin()),
                &mut std::io::stdout(),
                &mut std::io::stderr(),
                prompts,
                &FsSources,
            ) {
                Ok(()) => ExitCode::SUCCESS,
                Err(e) => match e.downcast_ref::<Exit>() {
                    Some(Exit(status)) => ExitCode::from(*status),
                    None => {
                        eprintln!("error: {e}");
                        ExitCode::FAILURE
                    }
                },
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
        ExecutionMode::Uninstall { yes } => match uninstall::run(yes) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("error: {e}");
                ExitCode::FAILURE
            }
        },
        ExecutionMode::Update { check, tag } => update(check, tag.as_deref()),
    }
}

/// Runs `stone update`, printing any error to stderr and returning the exit code.
#[cfg(feature = "self-update")]
fn update(check: bool, tag: Option<&str>) -> ExitCode {
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
fn update(_check: bool, _tag: Option<&str>) -> ExitCode {
    eprintln!(
        "error: this stone was built without `stone update`; upgrade it by rerunning \
         install.sh or `cargo install`, or build with `--features self-update`"
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
/// A program that ended with `os.exit` prints nothing more and exits with its status.
fn with_program(file: &str, action: impl FnOnce(&Mod) -> Result<(), Box<dyn Error>>) -> ExitCode {
    let Some(source) = read(file) else {
        return ExitCode::FAILURE;
    };
    let (sources, result) = driver::load(Path::new(file), &source, &FsSources);
    let err = match result {
        Ok((module, warnings)) => {
            let rendered: String = warnings.iter().map(|d| sources.render(d)).collect();
            eprint!("{rendered}");
            match action(&module) {
                Ok(()) => return ExitCode::SUCCESS,
                Err(err) => err,
            }
        }
        Err(diagnostics) => Box::new(diagnostics),
    };
    if let Some(Exit(status)) = err.downcast_ref::<Exit>() {
        return ExitCode::from(*status);
    }
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
