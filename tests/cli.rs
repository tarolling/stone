//! Tests for how the `stone` binary reports errors, run against the real executable.
//!
//! For example, `stone check` on a file with a syntax error must exit with a failure and print the
//! error with its file, line, and column.

use std::path::PathBuf;
use std::process::{Command, Output};

/// Writes `source` to a temporary `.st` file named after `name` and runs `stone <args> <file>`.
///
/// For example, `stone(&["check"], "ok", "x = 1\n")` checks a one-line program.
fn stone(args: &[&str], name: &str, source: &str) -> (Output, PathBuf) {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let file = dir.join(format!("{name}.st"));
    std::fs::write(&file, source).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(args)
        .arg(&file)
        .output()
        .expect("stone should run");
    (output, file)
}

#[test]
fn check_passes_a_valid_program() {
    let (output, _) = stone(&["check"], "valid", "x = 1\nprint(x)\n");
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
}

#[test]
fn check_reports_a_syntax_error_and_fails() {
    let (output, file) = stone(&["check"], "missing_semi", "if 1\n    x = 1\n");
    assert!(!output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!(
            "{}:1:5: error: expected ';', found end of line\n  |\n1 | if 1\n  |     ^\n",
            file.display()
        )
    );
}

#[test]
fn run_reports_a_syntax_error_without_running() {
    let (output, file) = stone(&["run"], "run_bad", "print(1)\nprint(2\n");
    assert!(!output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "");
    assert!(
        String::from_utf8_lossy(&output.stderr).starts_with(&format!(
            "{}:2:8: error: expected ')', found end of line\n",
            file.display()
        ))
    );
}

#[test]
fn run_reports_a_runtime_error_and_fails() {
    let (output, file) = stone(&["run"], "div_zero", "print(1 / 0)\n");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.starts_with(&format!("{}: error: ", file.display())),
        "{stderr}"
    );
}

#[cfg(not(feature = "self-update"))]
#[test]
fn self_update_without_feature_explains_how_to_upgrade() {
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("self-update")
        .output()
        .expect("stone should run");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("built without self-update"), "{stderr}");
    assert!(stderr.contains("install.sh"), "{stderr}");
}

/// Writes `source` to a temporary `.st` file named after `name` and runs `stone <before> <file>
/// <after>`, returning its stdout.
fn stone_stdout(before: &[&str], name: &str, source: &str, after: &[&str]) -> String {
    let file = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}.st"));
    std::fs::write(&file, source).unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(before)
        .arg(&file)
        .args(after)
        .output()
        .expect("stone should run");
    assert!(output.status.success(), "{output:?}");
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn run_passes_the_arguments_after_the_file_to_the_program() {
    let source = "print(args())\n";
    let expected = "['a', '-b', '--c', 'd e']\n";
    let after = ["a", "-b", "--c", "d e"];
    assert_eq!(stone_stdout(&["run"], "args_run", source, &after), expected);
    assert_eq!(stone_stdout(&[], "args_bare", source, &after), expected);
    assert_eq!(stone_stdout(&["run"], "args_none", source, &[]), "[]\n");
}

/// Writes each `(path, source)` file under a fresh temporary directory named after `name` and
/// returns the directory.
fn project(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name);
    let _ = std::fs::remove_dir_all(&dir);
    for (path, source) in files {
        let file = dir.join(path);
        std::fs::create_dir_all(file.parent().unwrap()).unwrap();
        std::fs::write(file, source).unwrap();
    }
    dir
}

#[test]
fn errors_in_an_imported_file_name_that_file() {
    let dir = project(
        "project_errors",
        &[
            ("main.st", "use geometry.vec\nprint(vec.zero())\n"),
            ("geometry/vec.st", "pub def zero();\n    ret 0 +\n"),
        ],
    );
    let expected = format!(
        "{}:2:12: error: expected an expression, found end of line\n  |\n2 |     ret 0 +\n  |            ^\n",
        dir.join("geometry/vec.st").display()
    );
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_stone"))
            .arg(command)
            .arg(dir.join("main.st"))
            .output()
            .expect("stone should run");
        assert!(!output.status.success(), "{command}");
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            expected,
            "{command}"
        );
    }
}

#[test]
fn run_and_build_load_imported_files() {
    let dir = project(
        "project_runs",
        &[
            ("main.st", "use util.twice\nprint(twice(21))\n"),
            ("util.st", "pub def twice(x);\n    ret x * 2\n"),
        ],
    );
    let main = dir.join("main.st");
    let run = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("run")
        .arg(&main)
        .output()
        .expect("stone should run");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n", "{run:?}");

    let exe = dir.join("out");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&main)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let built = Command::new(&exe).output().expect("program should run");
    assert_eq!(String::from_utf8_lossy(&built.stdout), "42\n");
}

#[test]
fn no_arguments_runs_a_quiet_session_over_piped_input() {
    use std::io::Write;
    use std::process::Stdio;

    let mut child = Command::new(env!("CARGO_BIN_EXE_stone"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("stone should run");
    let entries = "x = 2\nx * 3\n1 / 0\ndef f(n);\n    ret n + x\n\nf(1)\n";
    child
        .stdin
        .take()
        .unwrap()
        .write_all(entries.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success(), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "6\n3\n");
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "error: division by zero\n"
    );
}
