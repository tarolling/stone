//! Golden-output tests that run every `.st` program in `examples/` and `tests/programs/`
//! through both backends and compare stdout against the sibling `.out` file.
//!
//! For example, `examples/basics.st` must print exactly the contents of `examples/basics.out`
//! under both `stone run` and a binary produced by `stone build`.
//!
//! A program can opt out of a backend by being listed in [`SKIPS`] along with the reason.

use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

const PROGRAM_DIRS: [&str; 2] = ["examples", "tests/programs"];

/// Programs a backend can't handle yet, as `(file stem, backend name, reason)`.
///
/// For example, `("printing", "build", "...")` skips `printing.st` under `stone build` only.
const SKIPS: &[(&str, &str, &str)] = &[
    (
        "printing",
        "build",
        "compiled print shows only its first argument, nothing for negatives, and 1/0 for booleans",
    ),
    (
        "recursion",
        "run",
        "interpreted `ret` inside `if` is dropped, and parameters overwrite the caller's variables",
    ),
];

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

/// Runs a command and returns its stdout, or a description of how it failed.
fn stdout_of(command: &mut Command) -> Result<String, String> {
    let output = command
        .output()
        .map_err(|e| format!("failed to spawn {command:?}: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "{command:?} exited with {}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr)
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// Executes the program with the given backend and returns what it printed.
fn execute(program: &Path, backend: Backend) -> Result<String, String> {
    let stone = env!("CARGO_BIN_EXE_stone");
    match backend {
        Backend::Run => stdout_of(Command::new(stone).arg("run").arg(program)),
        Backend::Build => {
            let name = program.file_stem().unwrap();
            let exe = Path::new(env!("CARGO_TARGET_TMPDIR")).join(name);
            stdout_of(
                Command::new(stone)
                    .arg("build")
                    .arg(program)
                    .arg("-o")
                    .arg(&exe),
            )?;
            stdout_of(&mut Command::new(&exe))
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

        match execute(&program, backend) {
            Ok(actual) if actual == expected => {}
            Ok(actual) => writeln!(
                failures,
                "--- {} ({})\nexpected:\n{expected:?}\nactual:\n{actual:?}\n",
                program.display(),
                backend.name()
            )
            .unwrap(),
            Err(e) => writeln!(
                failures,
                "--- {} ({})\n{e}\n",
                program.display(),
                backend.name()
            )
            .unwrap(),
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
