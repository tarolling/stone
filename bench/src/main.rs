//! Benchmarks stone's compiled output against C and Python.
//!
//! For example, `cargo run --release --manifest-path bench/Cargo.toml -- --runs 3 --filter fib`
//! builds `bench/programs/fib.st` with `stone build`, `fib.c` with `gcc -O2` and `gcc -O0`, checks
//! that all of them and `python3 fib.py` print `fib.out`, then prints a table of their median
//! times over three runs.

use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::{Command, ExitCode};
use std::time::{Duration, Instant};

use stone_bench::{Implementation, Program, Row, discover, format_table, median, min};

const USAGE: &str = "\
usage: stone-bench [--runs N] [--filter NAME] [--stone PATH] [--min]

  --runs N       timed runs of each implementation, after one untimed check (default 5)
  --filter NAME  only run benchmarks whose name contains NAME
  --stone PATH   use this stone binary instead of building target/release/stone
  --min          report the fastest run instead of the median";

/// A timing this short usually means gcc optimized the benchmark's work away.
const SUSPICIOUSLY_FAST: Duration = Duration::from_millis(10);

/// The command-line options.
struct Options {
    runs: usize,
    filter: Option<String>,
    stone: Option<PathBuf>,
    use_min: bool,
}

/// Parses the arguments after the program name.
///
/// For example, `["--runs", "3"]` gives 3 runs, no filter, a freshly built stone, and the median.
fn parse_args(mut args: impl Iterator<Item = String>) -> Result<Options, String> {
    let mut options = Options {
        runs: 5,
        filter: None,
        stone: None,
        use_min: false,
    };
    while let Some(arg) = args.next() {
        let mut value = || args.next().ok_or(format!("{arg} needs a value"));
        match arg.as_str() {
            "--runs" => {
                options.runs = value()?
                    .parse()
                    .ok()
                    .filter(|&runs| runs > 0)
                    .ok_or("--runs needs a positive number")?;
            }
            "--filter" => options.filter = Some(value()?),
            "--stone" => options.stone = Some(PathBuf::from(value()?)),
            "--min" => options.use_min = true,
            "-h" | "--help" => return Err(USAGE.into()),
            _ => return Err(format!("unknown argument {arg}\n{USAGE}")),
        }
    }
    Ok(options)
}

/// Runs `argv` to completion, failing with its stderr if it cannot start or exits unsuccessfully,
/// and returns its stdout.
fn run_command(argv: &[OsString]) -> Result<String, String> {
    let shown = argv
        .iter()
        .map(|arg| arg.to_string_lossy())
        .collect::<Vec<_>>()
        .join(" ");
    let output = Command::new(&argv[0])
        .args(&argv[1..])
        .output()
        .map_err(|e| format!("could not run `{shown}`: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "`{shown}` failed with {}\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// Builds the stone binary in release mode and returns its path.
fn build_stone(root: &Path) -> Result<PathBuf, String> {
    eprintln!("building stone in release mode");
    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let status = Command::new(cargo)
        .args(["build", "--release", "--bin", "stone"])
        .current_dir(root)
        .status()
        .map_err(|e| format!("could not run cargo: {e}"))?;
    if !status.success() {
        return Err(format!("building stone failed with {status}"));
    }
    Ok(root.join("target").join("release").join("stone"))
}

/// Builds `program` for every implementation that needs building, and returns the command that
/// runs each one, in [`Implementation::ALL`] order.
fn prepare(program: &Program, stone: &Path, out_dir: &Path) -> Result<Vec<Vec<OsString>>, String> {
    let mut commands = Vec::new();
    for implementation in Implementation::ALL {
        // stone build also writes <binary>.s next to the binary, e.g. collatz-stone.s
        let suffix = match implementation {
            Implementation::COptimized => "c-o2",
            Implementation::CUnoptimized => "c-o0",
            Implementation::Stone => "stone",
            Implementation::Python => "py",
        };
        let binary = out_dir.join(format!("{}-{suffix}", program.name));
        let build: Vec<OsString> = match implementation {
            Implementation::Stone => vec![
                stone.into(),
                "build".into(),
                program.file("st").into(),
                "-o".into(),
                binary.clone().into(),
            ],
            Implementation::COptimized | Implementation::CUnoptimized => {
                let level = if implementation == Implementation::COptimized {
                    "-O2"
                } else {
                    "-O0"
                };
                vec![
                    "gcc".into(),
                    level.into(),
                    "-fwrapv".into(),
                    program.file("c").into(),
                    "-o".into(),
                    binary.clone().into(),
                ]
            }
            Implementation::Python => {
                commands.push(vec!["python3".into(), program.file("py").into()]);
                continue;
            }
        };
        run_command(&build)?;
        commands.push(vec![binary.into()]);
    }
    Ok(commands)
}

/// Runs `argv` once untimed and then `runs` times timed, checking every run prints `expected`,
/// and returns the timed runs' wall times.
fn measure(
    argv: &[OsString],
    runs: usize,
    expected: &str,
    what: &str,
) -> Result<Vec<Duration>, String> {
    let mut times = Vec::with_capacity(runs);
    for run in 0..=runs {
        let start = Instant::now();
        let stdout = run_command(argv)?;
        let elapsed = start.elapsed();
        if stdout != expected {
            return Err(format!(
                "{what} printed {stdout:?}, but the .out file expects {expected:?}"
            ));
        }
        // the first run only checks the output and warms caches
        if run > 0 {
            times.push(elapsed);
        }
    }
    Ok(times)
}

fn run() -> Result<(), String> {
    let options = parse_args(std::env::args().skip(1))?;
    let bench_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
    let root = bench_dir.parent().ok_or("the bench crate has no parent")?;
    let out_dir = bench_dir.join("target").join("programs");
    std::fs::create_dir_all(&out_dir).map_err(|e| format!("{}: {e}", out_dir.display()))?;

    let programs: Vec<Program> = discover(&bench_dir.join("programs"))?
        .into_iter()
        .filter(|p| options.filter.as_ref().is_none_or(|f| p.name.contains(f)))
        .collect();
    if programs.is_empty() {
        return Err("no benchmarks match the filter".into());
    }
    let stone = match options.stone {
        Some(path) => path,
        None => build_stone(root)?,
    };

    let mut rows = Vec::new();
    for program in &programs {
        let commands = prepare(program, &stone, &out_dir)?;
        let expected = std::fs::read_to_string(program.file("out"))
            .map_err(|e| format!("{}: {e}", program.file("out").display()))?;
        let mut times = [Duration::ZERO; 4];
        for (i, implementation) in Implementation::ALL.into_iter().enumerate() {
            let what = format!("{} under {}", program.name, implementation.label());
            eprintln!("running {what}");
            let samples = measure(&commands[i], options.runs, &expected, &what)?;
            times[i] = if options.use_min {
                min(&samples)
            } else {
                median(&samples)
            };
            if times[i] < SUSPICIOUSLY_FAST {
                eprintln!(
                    "warning: {what} took {:?}, so its work may have been optimized away",
                    times[i]
                );
            }
        }
        rows.push(Row {
            name: program.name.clone(),
            times,
        });
    }

    let statistic = if options.use_min { "Fastest" } else { "Median" };
    println!("{statistic} wall time of {} runs.\n", options.runs);
    print!("{}", format_table(&rows));
    Ok(())
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(message) => {
            eprintln!("error: {message}");
            ExitCode::FAILURE
        }
    }
}
