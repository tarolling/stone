use clap::error::{ContextKind, ContextValue, ErrorKind};
use clap::{CommandFactory, Parser as ClapParser, Subcommand};
use std::error::Error;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::ExitCode;
use stone::codegen::device::Device;
use stone::codegen::{Os, Subsystem, Target};
use stone::diagnostic::{Diagnostic, Diagnostics, Severity};
use stone::driver::{self, Checked, EntryUse};
use stone::format;
use stone::init;
use stone::interpreter::{Exit, Limits};
use stone::project::{DEFAULT_ENTRY, FsSources, SourceMap};
use stone::testing;

mod deps;
mod uninstall;
#[cfg(feature = "self-update")]
mod update;

/// The stone programming language executor.
#[derive(ClapParser, Debug)]
#[command(author, version, about = "The stone programming language executor")]
#[command(args_conflicts_with_subcommands = true, arg_required_else_help = true)]
struct Args {
    #[command(subcommand)]
    command: Option<Command>,

    /// The file to run when no subcommand is given.
    file: Option<String>,

    /// Arguments for the program, after `--`, which it reads with `args()`.
    #[arg(last = true, requires = "file")]
    args: Vec<String>,
}

#[derive(Subcommand, Debug)]
enum Command {
    /// Starts a project: a `stone.toml`, a `main.stn` to run, and a `.gitignore`.
    Init {
        /// The project's directory, made if there is none. Defaults to the current directory.
        path: Option<PathBuf>,
        /// Start a library, with a `lib.stn` for programs to use, instead of a program.
        #[arg(long)]
        lib: bool,
        /// The package's name, instead of the directory's.
        #[arg(long)]
        name: Option<String>,
        /// Do not make the directory a git repository, which stone init otherwise does when it
        /// is not in one already and git is installed.
        #[arg(long)]
        no_git: bool,
    },
    /// Runs a program with the interpreter.
    Run {
        /// Use only packages already downloaded.
        #[arg(long)]
        offline: bool,
        /// The file to run, or a directory to run the `main.stn` of. Defaults to the program the
        /// current directory is in.
        file: Option<String>,
        /// Arguments for the program, after `--`, which it reads with `args()`.
        #[arg(last = true)]
        args: Vec<String>,
    },
    /// Compiles programs to native executables in the `build` directory next to each.
    Build {
        /// The files to build, each into an executable named after it (or, for a `main.stn`,
        /// after its package, else its directory), or directories to build the `main.stn` of. Defaults to the
        /// program the current directory is in.
        files: Vec<String>,
        /// Where to write the executable instead, which needs exactly one file and one target.
        /// The assembly goes next to it with a `.s` extension.
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// The system to build for: `x86_64-linux`, `aarch64-linux`, `aarch64-macos`, or
        /// `x86_64-windows`, optionally with a processor level such as `x86_64v3-linux`. Defaults
        /// to this machine, and any can be built for from any machine. Give it more than once to build for
        /// several; each target other than this machine builds into `build/<TARGET>/`.
        #[arg(long = "target", value_name = "TARGET", value_parser = str::parse::<Target>)]
        targets: Vec<Target>,
        /// A GPU to write kernels for, named as its vendor names it, such as `sm_75`. Each
        /// piece of arithmetic on lists becomes a kernel in `<output>.<GPU>.ptx`. Windows
        /// programs run the kernels on an NVIDIA GPU when one is usable (`STONE_GPU=off` never,
        /// `require` always), and others compute on the processor. Give it more than once for
        /// several GPUs; only NVIDIA's are supported.
        #[arg(long = "gpu", value_name = "GPU", value_parser = str::parse::<Device>)]
        devices: Vec<Device>,
        /// Builds Windows programs as programs with windows, which start with no console, so
        /// opening one from Explorer opens no console, and what it prints goes nowhere unless
        /// its output is redirected. Needs a Windows target.
        #[arg(long)]
        windows_gui: bool,
        /// Use only packages already downloaded.
        #[arg(long)]
        offline: bool,
    },
    /// Removes the build directory that `stone build` made.
    Clean {
        /// A program's entry file or directory, whose `build` directory to remove. Defaults to
        /// the current directory.
        path: Option<PathBuf>,
    },
    /// Runs the test blocks (`test "description":`) of files, each test alone and without the
    /// file's top-level code.
    Test {
        /// The files to test, or directories to test every file with test blocks under.
        /// Defaults to the current directory.
        paths: Vec<PathBuf>,
        /// Compile each file and run its tests as native programs, instead of with the
        /// interpreter.
        #[arg(long)]
        build: bool,
        /// Run only the tests whose description contains this text.
        #[arg(long, value_name = "TEXT")]
        filter: Option<String>,
        /// How many files to test at once. Defaults to one per processor.
        #[arg(short, long, value_name = "N")]
        jobs: Option<usize>,
        /// Use only packages already downloaded.
        #[arg(long)]
        offline: bool,
    },
    /// Checks a program for errors without running it.
    Check {
        /// The file to check, or a directory to check the `main.stn` (or a library's `lib.stn`)
        /// of. Defaults to the program the current directory is in.
        file: Option<String>,
        /// Use only packages already downloaded.
        #[arg(long)]
        offline: bool,
    },
    /// Adds a package from a GitHub or GitLab release to the program in the current directory,
    /// naming it in `stone.toml` and downloading it.
    Add {
        /// The package, as `github:owner/repo` or `gitlab:group/project`, or a link to one,
        /// optionally followed by `@latest` (the default) or a version such as `@1.2`, which is
        /// the newest 1.2.x.
        package: String,
        /// The name the program uses it under, instead of the package's own.
        #[arg(long)]
        name: Option<String>,
        /// The host of a GitLab of its own, such as `gitlab.example.com`.
        #[arg(long)]
        host: Option<String>,
    },
    /// Removes a package from the program in the current directory.
    Remove {
        /// The name the program uses it under.
        name: String,
    },
    /// Downloads every package a program uses and brings its `stone.lock` up to date.
    Fetch {
        /// A program's entry file or directory. Defaults to the current directory.
        path: Option<PathBuf>,
        /// Fail if `stone.lock` would change, as in CI.
        #[arg(long)]
        locked: bool,
    },
    /// Shows the packages a program uses, and the packages they use.
    Tree {
        /// A program's entry file or directory. Defaults to the current directory.
        path: Option<PathBuf>,
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
    /// Explains an error code, such as E0005, with an example of the error and its fix.
    Explain {
        /// The code, as an error shows it, such as E0005.
        code: String,
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
        /// The GPUs to write kernels for.
        devices: Vec<Device>,
        /// Whether Windows executables start with no console.
        windows_gui: bool,
        offline: bool,
    },
    Check {
        file: Option<String>,
        offline: bool,
    },
    Init {
        path: Option<PathBuf>,
        options: init::Options,
    },
    Test {
        paths: Vec<PathBuf>,
        options: testing::Options,
        offline: bool,
    },
    Add {
        package: String,
        name: Option<String>,
        host: Option<String>,
    },
    Remove(String),
    Fetch {
        path: Option<PathBuf>,
        locked: bool,
    },
    Tree(Option<PathBuf>),
    Clean(Option<PathBuf>),
    Fmt {
        paths: Vec<PathBuf>,
        check: bool,
    },
    Run {
        file: Option<String>,
        args: Vec<String>,
        offline: bool,
    },
    Uninstall {
        yes: bool,
    },
    Explain(String),
    Update {
        check: bool,
        tag: Option<String>,
    },
}

fn main() -> ExitCode {
    let args = match Args::try_parse() {
        Ok(args) => args,
        Err(error) => with_dashes_tip(error).exit(),
    };

    let mode = match args.command {
        Some(Command::Run {
            file,
            args,
            offline,
        }) => ExecutionMode::Run {
            file,
            args,
            offline,
        },
        Some(Command::Build {
            files,
            output,
            targets,
            devices,
            windows_gui,
            offline,
        }) => ExecutionMode::Build {
            files,
            output,
            targets,
            devices,
            windows_gui,
            offline,
        },
        Some(Command::Check { file, offline }) => ExecutionMode::Check { file, offline },
        Some(Command::Init {
            path,
            lib,
            name,
            no_git,
        }) => ExecutionMode::Init {
            path,
            options: init::Options {
                lib,
                name,
                git: !no_git,
            },
        },
        Some(Command::Test {
            paths,
            build,
            filter,
            jobs,
            offline,
        }) => ExecutionMode::Test {
            paths,
            options: testing::Options {
                build,
                filter,
                jobs: jobs.unwrap_or_else(testing::default_jobs),
            },
            offline,
        },
        Some(Command::Add {
            package,
            name,
            host,
        }) => ExecutionMode::Add {
            package,
            name,
            host,
        },
        Some(Command::Remove { name }) => ExecutionMode::Remove(name),
        Some(Command::Fetch { path, locked }) => ExecutionMode::Fetch { path, locked },
        Some(Command::Tree { path }) => ExecutionMode::Tree(path),
        Some(Command::Clean { path }) => ExecutionMode::Clean(path),
        Some(Command::Fmt { paths, check }) => ExecutionMode::Fmt { paths, check },
        Some(Command::Update { check, version }) => ExecutionMode::Update {
            check,
            tag: version,
        },
        Some(Command::Uninstall { yes }) => ExecutionMode::Uninstall { yes },
        Some(Command::Explain { code }) => ExecutionMode::Explain(code),
        // clap prints the usage when there is neither a subcommand nor a file
        None => ExecutionMode::Run {
            file: args.file,
            args: args.args,
            offline: false,
        },
    };

    match mode {
        ExecutionMode::Build {
            files,
            output,
            targets,
            devices,
            windows_gui,
            offline,
        } => {
            let subsystem = match windows_gui {
                true => Subsystem::Gui,
                false => Subsystem::Console,
            };
            build(
                &files,
                output.as_deref(),
                &targets,
                &devices,
                subsystem,
                offline,
            )
        }
        ExecutionMode::Add {
            package,
            name,
            host,
        } => deps::add(&package, name.as_deref(), host.as_deref()),
        ExecutionMode::Remove(name) => deps::remove(&name),
        ExecutionMode::Fetch { path, locked } => deps::fetch(path.as_deref(), locked),
        ExecutionMode::Tree(path) => deps::tree(path.as_deref()),
        ExecutionMode::Init { path, options } => init(path.as_deref(), &options),
        ExecutionMode::Check { file, offline } => {
            let file = match entry(file.as_deref(), EntryUse::Any) {
                Ok(file) => file,
                Err(code) => return code,
            };
            let Some(source) = read(&file) else {
                return ExitCode::FAILURE;
            };
            deps::prepare(Path::new(&file), offline);
            let (sources, diagnostics) =
                driver::check_program(Path::new(&file), &source, &FsSources);
            // warnings alone still pass
            let rendered: String = diagnostics.iter().map(|d| sources.render(d)).collect();
            eprint!("{rendered}");
            eprint!("{}", stone::explain::hint(&diagnostics).unwrap_or_default());
            if diagnostics.iter().any(|d| d.severity == Severity::Error) {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            }
        }
        ExecutionMode::Test {
            paths,
            options,
            offline,
        } => test(&paths, &options, offline),
        ExecutionMode::Clean(path) => clean(path.as_deref()),
        ExecutionMode::Fmt { paths, check } => fmt(&paths, check),
        ExecutionMode::Run {
            file,
            args,
            offline,
        } => {
            let file = match entry(file.as_deref(), EntryUse::Program) {
                Ok(file) => file,
                Err(code) => return code,
            };
            run(&file, &args, offline)
        }
        ExecutionMode::Uninstall { yes } => match uninstall::run(yes) {
            Ok(()) => ExitCode::SUCCESS,
            Err(e) => {
                eprintln!("error: {e}");
                ExitCode::FAILURE
            }
        },
        ExecutionMode::Update { check, tag } => update(check, tag.as_deref()),
        ExecutionMode::Explain(code) => explain(&code),
    }
}

/// Runs `stone explain`, printing what the code means, or an error naming a close code if there
/// is no such code.
fn explain(code: &str) -> ExitCode {
    if let Some(found) = stone::explain::find(code) {
        print!("{}", stone::explain::text(found));
        return ExitCode::SUCCESS;
    }
    let ids = stone::explain::Code::ALL.iter().map(|c| c.id());
    match stone::diagnostic::similar(code, ids) {
        Some(like) => eprintln!("error: no error has the code '{code}'; did you mean '{like}'?"),
        None => eprintln!(
            "error: no error has the code '{code}'; codes go from E0001 to {}",
            stone::explain::Code::ALL.last().map_or("E0001", |c| c.id())
        ),
    }
    ExitCode::FAILURE
}

/// Adds a tip to an error about an argument clap did not expect after `stone run FILE` (or
/// `stone FILE`), since a program's arguments go after `--`.
///
/// For example, `stone run sum.stn 1 2` fails with `unexpected argument '1' found` and the tip
/// `to give '1' to the program, write it after '--', as in 'stone run sum.stn -- 1'`.
fn with_dashes_tip(mut error: clap::Error) -> clap::Error {
    let words: Vec<String> = std::env::args().skip(1).collect();
    let running = match words.first() {
        Some(word) => word == "run" || Args::command().find_subcommand(word).is_none(),
        None => false,
    };
    if error.kind() != ErrorKind::UnknownArgument || !running {
        return error;
    }
    let Some(ContextValue::String(arg)) = error.get(ContextKind::InvalidArg) else {
        return error;
    };
    let before: Vec<&str> = words
        .iter()
        .take_while(|word| *word != arg)
        .map(String::as_str)
        .collect();
    let tip = format!(
        "to give '{arg}' to the program, write it after '--', as in 'stone {} -- {arg}'",
        before.join(" ")
    );
    error.insert(
        ContextKind::Suggested,
        ContextValue::StyledStrs(vec![tip.into()]),
    );
    error
}

/// Runs `stone build`, compiling each of `files` for each of `targets` (the host if there are
/// none) into its build directory, or into `output` if given. A file that fails prints its errors
/// and the rest still build, but the exit code is then a failure.
///
/// For example, building `a.stn` and `b.stn` on an x86-64 Linux machine writes `build/a` and
/// `build/b`, and adding `--target aarch64-macos` also writes `build/aarch64-macos/a` and
/// `build/aarch64-macos/b`. Windows executables are programs of `subsystem`, and each GPU of
/// `devices` gets a `.ptx` file beside each executable.
fn build(
    files: &[String],
    output: Option<&Path>,
    targets: &[Target],
    devices: &[Device],
    subsystem: Subsystem,
    offline: bool,
) -> ExitCode {
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
    let windows = match unique.is_empty() {
        true => Target::host().is_ok_and(|host| host.os == Os::Windows),
        false => unique.iter().any(|target| target.os == Os::Windows),
    };
    if subsystem == Subsystem::Gui && !windows {
        eprintln!("error: --windows-gui needs a Windows target, such as --target x86_64-windows");
        return ExitCode::FAILURE;
    }
    for device in devices {
        if let Err(message) = device.ptx_version() {
            eprintln!("error: {message}");
            return ExitCode::FAILURE;
        }
    }
    let mut status = ExitCode::SUCCESS;
    let named: Vec<Option<&str>> = match files.is_empty() {
        true => vec![None],
        false => files.iter().map(|file| Some(file.as_str())).collect(),
    };
    for file in named {
        let file = match entry(file, EntryUse::Program) {
            Ok(file) => file,
            Err(code) => {
                status = code;
                continue;
            }
        };
        let file = &file;
        let code = with_program(file, offline, |checked, sources| {
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
                driver::compile_module(checked, sources, &path, target, subsystem, devices)?;
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

/// Runs `stone run`, interpreting the program whose entry file is `file` with `args` as its
/// arguments, after downloading the packages it uses unless `offline`.
fn run(file: &str, args: &[String], offline: bool) -> ExitCode {
    with_program(file, offline, |checked, sources| {
        // the interpreter runs only what the machine it runs on can
        if let Ok(host) = Target::host() {
            let unsupported = driver::unsupported(&checked.module, sources, host.os);
            if !unsupported.is_empty() {
                return Err(Box::new(Diagnostics(unsupported)));
            }
        }
        driver::run_module(
            &checked.module,
            &checked.calls,
            &mut std::io::BufReader::new(std::io::stdin()),
            &mut std::io::stdout(),
            args,
            Limits::DEFAULT,
        )
    })
}

/// Returns the entry file of the program `path` names, or of the program the current directory
/// is in if there is none (see [`driver::find_entry`]), printing why there is none.
///
/// For example, `entry(None, EntryUse::Program)` in a project made by `stone init` is
/// `main.stn`, and `entry(Some("sum.stn"), EntryUse::Program)` is `sum.stn`.
fn entry(path: Option<&str>, want: EntryUse) -> Result<String, ExitCode> {
    match driver::find_entry(path.map(Path::new), want) {
        Ok(file) => Ok(file.to_string_lossy().into_owned()),
        Err(e) => {
            eprintln!("error: {e}");
            Err(ExitCode::FAILURE)
        }
    }
}

/// Runs `stone init`, starting a project in `path` (the current directory if there is none) as
/// `options` say, and printing each file it wrote and how to go on.
///
/// For example, `stone init hello` prints `created hello/stone.toml` and the other files, then
/// `made the program hello; run it with `cd hello` and `stone run``.
fn init(path: Option<&Path>, options: &init::Options) -> ExitCode {
    let created = match init::init(path.unwrap_or(Path::new("")), options) {
        Ok(created) => created,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::FAILURE;
        }
    };
    for file in &created.files {
        println!("created {}", file.display());
    }
    if created.git {
        println!("created a git repository");
    }
    let (what, next) = match options.lib {
        true => ("library", "test it with"),
        false => ("program", "run it with"),
    };
    let command = match options.lib {
        true => "stone test",
        false => "stone run",
    };
    let name = &created.name;
    match path {
        Some(path) => println!(
            "made the {what} {name}; {next} `cd {}` and `{command}`",
            path.display()
        ),
        None => println!("made the {what} {name}; {next} `{command}`"),
    }
    ExitCode::SUCCESS
}

/// Runs `stone test`, testing every file of `paths` with test blocks (every such file under a
/// directory, and with no paths under the program the current directory is in, or the current
/// directory if it is in none) as `options` says, after
/// downloading the packages their programs use unless `offline`.
///
/// For example, `stone test geo` runs the tests of `geo/shapes.stn` and `geo/vec.stn`, printing
/// a line per test and a summary, and fails if any test failed or any file has errors.
fn test(paths: &[PathBuf], options: &testing::Options, offline: bool) -> ExitCode {
    // with no paths, the program the current directory is in, else the directory itself
    let paths = match paths.is_empty() {
        true => match driver::program_dir() {
            Some(root) if !root.as_os_str().is_empty() => vec![root],
            _ => vec![PathBuf::from(".")],
        },
        false => paths.to_vec(),
    };
    let (files, errors) = testing::test_files(&paths);
    let mut status = ExitCode::SUCCESS;
    for error in &errors {
        eprintln!("{error}");
        status = ExitCode::FAILURE;
    }
    let mut prepared = vec![];
    for file in &files {
        let root = driver::program_root(file, &FsSources)
            .unwrap_or_else(|| file.parent().unwrap_or(Path::new("")).to_path_buf());
        if !prepared.contains(&root) {
            deps::prepare(&root.join(DEFAULT_ENTRY), offline);
            prepared.push(root);
        }
    }
    match testing::run(&files, options, &mut std::io::stdout().lock()) {
        Ok(true) => status,
        Ok(false) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}

/// Runs `stone clean`, removing the build directory of the program in `path`, an entry file or a
/// directory, or in the current directory if there is none.
///
/// For example, `stone clean examples/basics.stn` and `stone clean examples` both remove
/// `examples/build`.
fn clean(path: Option<&Path>) -> ExitCode {
    let root = driver::program_dir().unwrap_or_default();
    let dir = match path {
        None => &root,
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
/// A program that ended with `os.exit` prints nothing more and exits with its status. The
/// packages it uses are downloaded first, unless `offline`.
fn with_program(
    file: &str,
    offline: bool,
    action: impl FnOnce(&Checked, &SourceMap) -> Result<(), Box<dyn Error>>,
) -> ExitCode {
    let Some(source) = read(file) else {
        return ExitCode::FAILURE;
    };
    deps::prepare(Path::new(file), offline);
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
        eprint!("{}", stone::explain::hint(diagnostics).unwrap_or_default());
    } else if let Some(diagnostic) = err.downcast_ref::<Diagnostic>() {
        eprint!("{}", sources.render(diagnostic));
        let hint = stone::explain::hint(std::slice::from_ref(diagnostic));
        eprint!("{}", hint.unwrap_or_default());
    } else {
        eprintln!("{file}: error: {err}");
    }
    ExitCode::FAILURE
}
