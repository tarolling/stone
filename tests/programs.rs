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
//! A program can opt out of a backend by being listed in [`SKIPS`] along with the reason.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

const PROGRAM_DIRS: [&str; 3] = ["examples", "tests/programs", "docs/examples"];

/// Programs a backend can't handle yet, as `(file stem, backend name, reason)`.
///
/// For example, `("printing", "build", "...")` skips `printing.st` under `stone build` only.
const SKIPS: &[(&str, &str, &str)] = &[];

/// A way of executing a stone program.
#[derive(Clone, Copy)]
enum Backend {
    Run,
    Build,
}

impl Backend {
    fn name(self) -> &'static str {
        match self {
            Backend::Run => "run",
            Backend::Build => "build",
        }
    }
}

/// Returns every `.st` file under [`PROGRAM_DIRS`], sorted so failures are reported in a stable order.
fn programs() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut programs = Vec::new();
    for dir in PROGRAM_DIRS {
        for entry in std::fs::read_dir(root.join(dir)).expect("program directory should exist") {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|ext| ext == "st") {
                programs.push(path);
            }
        }
    }
    programs.sort();
    assert!(!programs.is_empty(), "no .st programs found");
    programs
}

/// Returns the reason the program is skipped under the backend, if it is listed in [`SKIPS`].
fn skip_reason(program: &Path, backend: Backend) -> Option<&'static str> {
    let stem = program.file_stem()?.to_str()?;
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

/// Runs a command to completion, failing only if it cannot be started.
fn outcome_of(command: &mut Command) -> Result<Outcome, String> {
    let output = command
        .output()
        .map_err(|e| format!("failed to spawn {command:?}: {e}"))?;
    Ok(Outcome {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        error: (!output.status.success())
            .then(|| String::from_utf8_lossy(&output.stderr).into_owned()),
    })
}

/// Executes the program with the given backend and returns how it ended.
fn execute(program: &Path, backend: Backend) -> Result<Outcome, String> {
    let stone = env!("CARGO_BIN_EXE_stone");
    match backend {
        Backend::Run => outcome_of(Command::new(stone).arg("run").arg(program)),
        Backend::Build => {
            let name = program.file_stem().unwrap();
            let exe = Path::new(env!("CARGO_TARGET_TMPDIR")).join(name);
            let build = outcome_of(
                Command::new(stone)
                    .arg("build")
                    .arg(program)
                    .arg("-o")
                    .arg(&exe),
            )?;
            if let Some(stderr) = build.error {
                return Err(format!("build failed:\n{stderr}"));
            }
            outcome_of(&mut Command::new(&exe))
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
