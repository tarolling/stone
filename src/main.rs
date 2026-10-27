use clap::{Parser as ClapParser, Subcommand};
use std::error::Error;
use std::io::{IsTerminal, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use stone::codegen::Target;
use stone::diagnostic::{Diagnostic, Diagnostics, Severity};
use stone::driver::{self, Checked};
use stone::format;
use stone::interpreter::{Exit, Limits};
use stone::project::{FsSources, SourceMap};

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
    /// Compiles files to native executables in the `build` directory next to each.
    Build {
        /// The files to build, each into an executable named after it (or, for a `main.stn`,
        /// after its directory).
        #[arg(required = true)]
        files: Vec<String>,
        /// Where to write the executable instead, which needs exactly one file and one target.
        /// The assembly goes next to it with a `.s` extension.
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// The system to build for: `x86_64-linux`, `aarch64-linux`, or `aarch64-macos`,
        /// optionally with a processor level such as `x86_64v3-linux`. Defaults to this machine,
        /// and any can be built for from any machine. Give it more than once to build for
        /// several; each target other than this machine builds into `build/<TARGET>/`.
        #[arg(long = "target", value_name = "TARGET", value_parser = str::parse::<Target>)]
        targets: Vec<Target>,
    },
    /// Removes the build directory that `stone build` made.
    Clean {
        /// A program's entry file or directory, whose `build` directory to remove. Defaults to
        /// the current directory.
        path: Option<PathBuf>,
    },
    /// Checks a file for errors without running it.
    Check {
        /// The file to check.
        file: String,
    },
    /// Rewrites files in the standard layout.
    Fmt {
        /// The files to format, directories to format every `.stn` file under, or `-` to format
        /// standard input to standard output. Defaults to the current directory.
        paths: Vec<PathBuf>,
        /// Change nothing, but list each file that is not formatted, and fail if there is one.
        #[arg(long)]
        check: bool,
    },
    /// Updates stone to the newest release.
    Update {
        /// Only report whether a newer release exists.
        #[arg(long, conflicts_with = "version")]
        check: bool,
        /// The release to install instead of the newest, such as v0.2.0.
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
        files: Vec<String>,
        output: Option<PathBuf>,
        /// What to build for, or the host if empty.
        targets: Vec<Target>,
    },
    Check(String),
    Clean(Option<PathBuf>),
    Fmt {
        paths: Vec<PathBuf>,
        check: bool,
    },
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
            files,
            output,
            targets,
        }) => ExecutionMode::Build {
            files,
            output,
            targets,
        },
        Some(Command::Check { file }) => ExecutionMode::Check(file),
        Some(Command::Clean { path }) => ExecutionMode::Clean(path),
        Some(Command::Fmt { paths, check }) => ExecutionMode::Fmt { paths, check },
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
            files,
            output,
            targets,
        } => build(&files, output.as_deref(), &targets),
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
        ExecutionMode::Clean(path) => clean(path.as_deref()),
        ExecutionMode::Fmt { paths, check } => fmt(&paths, check),
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
        ExecutionMode::Run { file, args } => with_program(&file, |checked, _| {
            driver::run_module(
                &checked.module,
                &checked.calls,
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

/// Runs `stone build`, compiling each of `files` for each of `targets` (the host if there are
/// none) into its build directory, or into `output` if given. A file that fails prints its errors
/// and the rest still build, but the exit code is then a failure.
///
/// For example, building `a.stn` and `b.stn` on an x86-64 Linux machine writes `build/a` and
/// `build/b`, and adding `--target aarch64-macos` also writes `build/aarch64-macos/a` and
/// `build/aarch64-macos/b`.
fn build(files: &[String], output: Option<&Path>, targets: &[Target]) -> ExitCode {
    let mut unique: Vec<Target> = Vec::new();
    for &target in targets {
        if !unique.contains(&target) {
            unique.push(target);
        }
    }
    if output.is_some() && (files.len() > 1 || unique.len() > 1) {
        eprintln!("error: -o names one executable, so it needs exactly one file and one target");
        return ExitCode::FAILURE;
    }
    let mut status = ExitCode::SUCCESS;
    for file in files {
        let code = with_program(file, |checked, sources| {
            let targets = if unique.is_empty() {
                vec![Target::host()?]
            } else {
                unique.clone()
            };
            let entry = Path::new(file);
            for target in targets {
                let path = match output {
                    Some(output) => output.to_path_buf(),
                    None => {
                        driver::prepare_build_dir(&driver::build_dir(entry))?;
                        driver::output_path(entry, target)
                    }
                };
                driver::compile_module(checked, sources, &path, target)?;
                println!("Compiled {}", path.display());
            }
            Ok(())
        });
        if code != ExitCode::SUCCESS {
            status = code;
        }
    }
    status
}

/// Runs `stone clean`, removing the build directory of the program in `path`, an entry file or a
/// directory, or in the current directory if there is none.
///
/// For example, `stone clean examples/basics.stn` and `stone clean examples` both remove
/// `examples/build`.
fn clean(path: Option<&Path>) -> ExitCode {
    let dir = match path {
        None => Path::new(""),
        Some(path) if path.is_dir() => path,
        Some(path) => path.parent().unwrap_or(Path::new("")),
    };
    let build = dir.join(driver::BUILD_DIR);
    match driver::clean(dir) {
        Ok(true) => println!("Removed {}", build.display()),
        Ok(false) => println!("nothing to clean in {}", build.display()),
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::FAILURE;
        }
    }
    ExitCode::SUCCESS
}

/// Runs `stone fmt`, rewriting each file of `paths` (every `.stn` file under a directory, and
/// the current directory if there are none) that is not in the standard layout, or with `check`
/// only listing them. `-` formats standard input to standard output. A file that cannot be read
/// or does not parse prints its errors and is left alone, and the exit code is then a failure.
///
/// For example, `stone fmt examples` rewrites `examples/basics.stn` if it needs it, and
/// `stone fmt --check examples` prints `examples/basics.stn` and fails instead.
fn fmt(paths: &[PathBuf], check: bool) -> ExitCode {
    let mut status = ExitCode::SUCCESS;
    let mut files = vec![];
    for path in paths {
        if path == Path::new("-") {
            if !fmt_stdin(check) {
                status = ExitCode::FAILURE;
            }
        } else if path.is_dir() {
            match driver::stone_files(path) {
                Ok(found) => files.extend(found),
                Err(e) => {
                    eprintln!("{}: error: {e}", path.display());
                    status = ExitCode::FAILURE;
                }
            }
        } else {
            files.push(path.clone());
        }
    }
    if paths.is_empty() {
        match driver::stone_files(Path::new(".")) {
            Ok(found) => files.extend(
                found
                    .iter()
                    .map(|f| f.strip_prefix(".").unwrap_or(f).into()),
            ),
            Err(e) => {
                eprintln!("error: {e}");
                status = ExitCode::FAILURE;
            }
        }
    }
    for file in files {
        let name = file.display().to_string();
        let Some(source) = read(&name) else {
            status = ExitCode::FAILURE;
            continue;
        };
        let formatted = match format::format(&source) {
            Ok(formatted) if formatted == source => continue,
            Ok(formatted) => formatted,
            Err(Diagnostics(diagnostics)) => {
                let rendered: String = diagnostics
                    .iter()
                    .map(|d| d.render(&name, &source))
                    .collect();
                eprint!("{rendered}");
                status = ExitCode::FAILURE;
                continue;
            }
        };
        if check {
            println!("{name}");
            status = ExitCode::FAILURE;
        } else if let Err(e) = std::fs::write(&file, formatted) {
            eprintln!("{name}: error: {e}");
            status = ExitCode::FAILURE;
        }
    }
    status
}

/// Formats standard input to standard output for `stone fmt -`, or with `check` writes nothing,
/// returning whether it succeeded (and, with `check`, was already formatted).
fn fmt_stdin(check: bool) -> bool {
    let mut source = String::new();
    if let Err(e) = std::io::stdin().read_to_string(&mut source) {
        eprintln!("<stdin>: error: {e}");
        return false;
    }
    match format::format(&source) {
        Ok(formatted) if check => formatted == source,
        Ok(formatted) => {
            print!("{formatted}");
            true
        }
        Err(Diagnostics(diagnostics)) => {
            let rendered: String = diagnostics
                .iter()
                .map(|d| d.render("<stdin>", &source))
                .collect();
            eprint!("{rendered}");
            false
        }
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
fn with_program(
    file: &str,
    action: impl FnOnce(&Checked, &SourceMap) -> Result<(), Box<dyn Error>>,
) -> ExitCode {
    let Some(source) = read(file) else {
        return ExitCode::FAILURE;
    };
    let (sources, result) = driver::load(Path::new(file), &source, &FsSources);
    let err = match result {
        Ok(checked) => {
            let rendered: String = checked.warnings.iter().map(|d| sources.render(d)).collect();
            eprint!("{rendered}");
            match action(&checked, &sources) {
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
