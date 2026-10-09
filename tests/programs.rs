//! Golden-output tests that run every `.st` program in `examples/`, `tests/programs/`, and
//! `docs/examples/` (the programs the documentation shows) through both backends and compare stdout against the sibling `.out` file.
//!
//! For example, `examples/basics.st` must print exactly the contents of `examples/basics.out`
//! under both `stone run` and a binary produced by `stone build`.
//!
//! A program that should fail at runtime, such as one indexing past the end of a list, has a
//! sibling `.err` file too. It must then exit with an error under both backends, print the `.out`
//! text first, and print the `.err` text somewhere in its stderr.
//!
//! A program that reads input has a sibling `.in` file, which becomes its stdin (otherwise stdin
//! is empty), and one that reads `args()` has a sibling `.args` file holding one argument per
//! line, passed after the program under `stone run` and to the built binary. One that reads the
//! environment through `os.env` can have a sibling `.env` file holding one `NAME=value` per line,
//! which every backend sets on top of the environment the tests run in.
//!
//! A subdirectory of one of those directories is a program of several files, run from its
//! `main.st`, whose `.out`, `.err`, `.in`, `.args`, and `.env` files sit next to `main.st`. For example,
//! `tests/programs/modules/main.st` uses modules such as `tests/programs/modules/text.st`.
//!
//! The compiler is also tested for the other architecture, such as aarch64 on an x86-64 machine,
//! by cross-compiling each program with `stone build --target` and running it under qemu-user.
//! That needs the cross compiler (such as `aarch64-linux-gnu-gcc`) and qemu (such as
//! `qemu-aarch64`) on `PATH`, and is skipped with a note when either is missing. qemu finds the
//! target's libc through `QEMU_LD_PREFIX`, which defaults to `/usr/aarch64-linux-gnu`.
//!
//! A program can opt out of a backend by being listed in [`SKIPS`] along with the reason.

use std::fmt::Write as _;
use std::io::Write as _;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use stone::codegen::Architecture;

const PROGRAM_DIRS: [&str; 3] = ["examples", "tests/programs", "docs/examples"];

/// Programs a backend can't handle yet, as `(file stem, backend name, reason)`.
///
/// For example, `("printing", "build", "...")` skips `printing.st` under `stone build` only, and
/// `("printing", "build-aarch64", "...")` only when cross-compiling it for aarch64.
const SKIPS: &[(&str, &str, &str)] = &[];

/// A way of executing a stone program.
#[derive(Clone, Copy)]
enum Backend {
    Run,
    Build,
    /// `stone build --target` for an architecture other than the host's, run under qemu-user.
    Cross(Architecture),
}

impl Backend {
    fn name(self) -> &'static str {
        match self {
            Backend::Run => "run",
            Backend::Build => "build",
            Backend::Cross(Architecture::X64) => "build-x86_64",
            Backend::Cross(Architecture::Arm64) => "build-aarch64",
        }
    }
}

/// Returns the qemu-user emulator for `arch` and the directory it should find that
/// architecture's libc in.
///
/// For example, aarch64 is run by `qemu-aarch64` with libc from `/usr/aarch64-linux-gnu`, unless
/// `QEMU_LD_PREFIX` says otherwise.
fn emulator(arch: Architecture) -> (&'static str, String) {
    let (qemu, prefix) = match arch {
        Architecture::X64 => ("qemu-x86_64", "/usr/x86_64-linux-gnu"),
        Architecture::Arm64 => ("qemu-aarch64", "/usr/aarch64-linux-gnu"),
    };
    let prefix = std::env::var("QEMU_LD_PREFIX").unwrap_or_else(|_| prefix.to_string());
    (qemu, prefix)
}

/// Returns whether `tool` can be run, checking with `--version`.
fn runs(tool: &str) -> bool {
    Command::new(tool)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok()
}

/// Returns every `.st` file directly in [`PROGRAM_DIRS`] and the `main.st` of each of their
/// subdirectories, sorted so failures are reported in a stable order.
fn programs() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut programs = Vec::new();
    for dir in PROGRAM_DIRS {
        for entry in std::fs::read_dir(root.join(dir)).expect("program directory should exist") {
            let path = entry.unwrap().path();
            if path.is_dir() {
                let main = path.join("main.st");
                assert!(main.is_file(), "{} has no main.st", path.display());
                programs.push(main);
            } else if path.extension().is_some_and(|ext| ext == "st") {
                programs.push(path);
            }
        }
    }
    programs.sort();
    assert!(!programs.is_empty(), "no .st programs found");
    programs
}

/// Returns a program's name: its file stem, or for a program of several files, its directory's
/// name.
///
/// For example, `tests/programs/lists.st` is `lists` and `tests/programs/modules/main.st` is
/// `modules`.
fn name_of(program: &Path) -> &str {
    let path = if program
        .parent()
        .is_some_and(|dir| PROGRAM_DIRS.iter().all(|d| !dir.ends_with(d)))
    {
        program.parent().unwrap()
    } else {
        program
    };
    path.file_stem().and_then(|s| s.to_str()).unwrap_or("")
}

/// Returns the reason the program is skipped under the backend, if it is listed in [`SKIPS`].
fn skip_reason(program: &Path, backend: Backend) -> Option<&'static str> {
    let stem = name_of(program);
    SKIPS
        .iter()
        .find(|(name, skipped, _)| *name == stem && *skipped == backend.name())
        .map(|(_, _, reason)| *reason)
}

/// How a run of a program ended.
struct Outcome {
    stdout: String,
    /// The stderr of a run that exited with an error, or `None` if it succeeded.
    error: Option<String>,
}

/// Runs a command to completion with `input` as its stdin, failing only if it cannot be started.
fn outcome_of(command: &mut Command, input: &[u8]) -> Result<Outcome, String> {
    let mut child = command
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to spawn {command:?}: {e}"))?;
    let mut stdin = child.stdin.take().expect("stdin is piped");
    // a program may exit without reading everything, which closes the pipe early
    let _ = stdin.write_all(input);
    drop(stdin);
    let output = child
        .wait_with_output()
        .map_err(|e| format!("failed to wait for {command:?}: {e}"))?;
    Ok(Outcome {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        error: (!output.status.success())
            .then(|| String::from_utf8_lossy(&output.stderr).into_owned()),
    })
}

/// Executes the program with the given backend and returns how it ended.
fn execute(program: &Path, backend: Backend) -> Result<Outcome, String> {
    let stone = env!("CARGO_BIN_EXE_stone");
    let input = std::fs::read(program.with_extension("in")).unwrap_or_default();
    let args: Vec<String> = std::fs::read_to_string(program.with_extension("args"))
        .map(|text| text.lines().map(str::to_string).collect())
        .unwrap_or_default();
    let env: Vec<(String, String)> = std::fs::read_to_string(program.with_extension("env"))
        .map(|text| {
            text.lines()
                .filter_map(|line| line.split_once('='))
                .map(|(name, value)| (name.to_string(), value.to_string()))
                .collect()
        })
        .unwrap_or_default();
    match backend {
        Backend::Run => outcome_of(
            Command::new(stone)
                .arg("run")
                .arg(program)
                .args(&args)
                .envs(env),
            &input,
        ),
        Backend::Build | Backend::Cross(_) => {
            let exe = Path::new(env!("CARGO_TARGET_TMPDIR"))
                .join(backend.name())
                .join(name_of(program));
            let mut build = Command::new(stone);
            build.arg("build").arg(program).arg("-o").arg(&exe);
            if let Backend::Cross(arch) = backend {
                build.arg("--target").arg(arch.to_string());
            }
            let build = outcome_of(&mut build, &[])?;
            if let Some(stderr) = build.error {
                return Err(format!("build failed:\n{stderr}"));
            }
            let mut command = match backend {
                Backend::Cross(arch) => {
                    let (qemu, prefix) = emulator(arch);
                    let mut command = Command::new(qemu);
                    command.arg(&exe).env("QEMU_LD_PREFIX", prefix);
                    command
                }
                _ => Command::new(&exe),
            };
            // the binary reports any string or list still allocated when it ends
            outcome_of(
                command.args(&args).envs(env).env("STONE_LEAK_CHECK", "1"),
                &input,
            )
        }
    }
}

/// Runs every program with the backend and panics once, listing every mismatch.
fn check_all(backend: Backend) {
    let mut failures = String::new();
    for program in programs() {
        if let Some(reason) = skip_reason(&program, backend) {
            eprintln!(
                "skipped {} ({}): {reason}",
                program.display(),
                backend.name()
            );
            continue;
        }

        let expected_path = program.with_extension("out");
        let expected = std::fs::read_to_string(&expected_path)
            .unwrap_or_else(|_| panic!("missing expected output {}", expected_path.display()));

        let expected_error = std::fs::read_to_string(program.with_extension("err"))
            .ok()
            .map(|e| e.trim().to_string());

        let problem = match execute(&program, backend) {
            Err(e) => Some(e),
            Ok(outcome) if outcome.stdout != expected => Some(format!(
                "expected:\n{expected:?}\nactual:\n{:?}",
                outcome.stdout
            )),
            Ok(outcome) => match (outcome.error, &expected_error) {
                (None, None) => None,
                (Some(stderr), Some(want)) if stderr.contains(want.as_str()) => None,
                (Some(stderr), Some(want)) => Some(format!(
                    "expected stderr to contain {want:?}, got:\n{stderr}"
                )),
                (Some(stderr), None) => Some(format!("exited with an error:\n{stderr}")),
                (None, Some(want)) => Some(format!("expected an error containing {want:?}")),
            },
        };
        if let Some(problem) = problem {
            writeln!(
                failures,
                "--- {} ({})\n{problem}\n",
                program.display(),
                backend.name()
            )
            .unwrap();
        }
    }
    assert!(
        failures.is_empty(),
        "program output mismatches:\n{failures}"
    );
}

#[test]
fn interpreter_matches_expected() {
    check_all(Backend::Run);
}

#[test]
fn compiler_matches_expected() {
    check_all(Backend::Build);
}

#[test]
fn cross_compiler_matches_expected() {
    let Some(arch) = Architecture::ALL
        .into_iter()
        .find(|arch| *arch != Architecture::host())
    else {
        return;
    };
    let (qemu, _) = emulator(arch);
    for tool in [arch.linker(), qemu] {
        if !runs(tool) {
            eprintln!("skipped building for {arch}: {tool} was not found");
            return;
        }
    }
    check_all(Backend::Cross(arch));
}
