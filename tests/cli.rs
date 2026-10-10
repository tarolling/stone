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
fn update_without_feature_explains_how_to_upgrade() {
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("update")
        .output()
        .expect("stone should run");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("built without"), "{stderr}");
    assert!(stderr.contains("install.sh"), "{stderr}");
}

#[test]
fn self_update_is_not_a_command() {
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("self-update")
        .output()
        .expect("stone should run");
    assert!(!output.status.success());
    // with no such subcommand, `self-update` is read as a file to run
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.starts_with("self-update: error: "), "{stderr}");
}

/// Copies the stone binary to `<target tmp>/<dir>/stone` so a test can uninstall it, returning
/// the copy's path.
///
/// For example, `installed_copy("uninstall_yes/bin")` returns `.../uninstall_yes/bin/stone`.
fn installed_copy(dir: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(dir);
    std::fs::create_dir_all(&dir).unwrap();
    let copy = dir.join("stone");
    std::fs::copy(env!("CARGO_BIN_EXE_stone"), &copy).unwrap();
    copy
}

/// Runs `<exe> uninstall <args>` with no stdin and `CARGO_HOME` set to `cargo_home`.
fn uninstall(exe: &PathBuf, args: &[&str], cargo_home: &PathBuf) -> Output {
    Command::new(exe)
        .arg("uninstall")
        .args(args)
        .env("CARGO_HOME", cargo_home)
        .stdin(std::process::Stdio::null())
        .output()
        .expect("stone should run")
}

#[test]
fn uninstall_without_a_terminal_needs_yes() {
    let copy = installed_copy("uninstall_no_terminal/bin");
    let output = uninstall(&copy, &[], &copy.with_file_name("no-cargo"));
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("--yes"), "{stderr}");
    assert!(copy.exists());
}

#[test]
fn uninstall_yes_removes_the_binary() {
    let copy = installed_copy("uninstall_yes/bin");
    let output = uninstall(&copy, &["--yes"], &copy.with_file_name("no-cargo"));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(output.status.success(), "{stderr}");
    assert!(String::from_utf8_lossy(&output.stdout).contains("removed"));
    assert!(!copy.exists());
}

#[test]
fn uninstall_refuses_a_cargo_install() {
    let copy = installed_copy("uninstall_cargo/cargo/bin");
    let cargo_home = copy.parent().unwrap().parent().unwrap().to_path_buf();
    let output = uninstall(&copy, &["--yes"], &cargo_home);
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("cargo uninstall stone"), "{stderr}");
    assert!(copy.exists());
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

#[test]
fn build_rejects_an_unknown_target() {
    let (output, _) = stone(&["build", "--target", "sparc"], "sparc", "print(1)\n");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unknown target 'sparc' (expected x86_64 or aarch64)"),
        "{stderr}"
    );
}

#[test]
fn build_for_the_host_target_by_name_runs() {
    let host = if cfg!(target_arch = "aarch64") {
        "aarch64"
    } else {
        "x86_64"
    };
    let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("host_target");
    let exe_arg = exe.to_str().unwrap();
    let (output, _) = stone(
        &["build", "--target", host, "-o", exe_arg],
        "host_target",
        "print(6 * 7)\n",
    );
    assert!(output.status.success(), "{output:?}");
    let run = Command::new(&exe).output().expect("the binary should run");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n");
}

/// Writes `source` as `main.st` in a fresh temporary directory named after `name`, then runs it
/// from that directory both with `stone run` and as a binary from `stone build`, returning each
/// run's output.
fn run_and_build_in(name: &str, source: &str) -> (PathBuf, [Output; 2]) {
    let dir = project(name, &[("main.st", source)]);
    let run = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("run")
        .arg("main.st")
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "main.st", "-o", "out"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let built = Command::new(dir.join("out"))
        .current_dir(&dir)
        .env("STONE_LEAK_CHECK", "1")
        .output()
        .expect("the binary should run");
    (dir, [run, built])
}

#[test]
fn run_and_build_print_warnings_and_still_succeed() {
    let source = "def push(xs, x);\n    xs.append(x)\n\nys = [1]\npush(ys, 2)\nprint(ys)\n";
    let warning = "main.st:2:5: warning: changes to 'xs' are lost when 'push' returns";
    for command in ["run", "build"] {
        let dir = project(&format!("warns_{command}"), &[("main.st", source)]);
        let output = Command::new(env!("CARGO_BIN_EXE_stone"))
            .args([command, "main.st"])
            .current_dir(&dir)
            .output()
            .expect("stone should run");
        assert!(output.status.success(), "{output:?}");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.starts_with(warning), "{command}: {stderr}");
    }
}

#[test]
fn exit_ends_the_program_with_its_status() {
    let source = "use os\n\ndef stop(xs);\n    os.exit(3)\n\nprint(1)\nstop([\"a\"])\nprint(2)\n";
    let (_, outputs) = run_and_build_in("os_exit_status", source);
    for output in outputs {
        assert_eq!(output.status.code(), Some(3), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "1\n");
        assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    }
}

#[test]
fn exit_ends_a_session_with_its_status() {
    use std::io::Write;
    use std::process::Stdio;

    let mut child = Command::new(env!("CARGO_BIN_EXE_stone"))
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("stone should run");
    let entries = "use os\nprint(1)\nos.exit(5)\nprint(2)\n";
    child
        .stdin
        .take()
        .unwrap()
        .write_all(entries.as_bytes())
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert_eq!(output.status.code(), Some(5), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1\n");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
}

#[test]
fn cwd_is_the_directory_the_program_runs_in() {
    let (dir, outputs) = run_and_build_in("os_cwd", "use os\nprint(os.cwd())\n");
    let expected = format!("{}\n", dir.canonicalize().unwrap().display());
    for output in outputs {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}
