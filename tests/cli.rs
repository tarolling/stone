//! Tests for how the `stone` binary reports errors, run against the real executable.
//!
//! For example, `stone check` on a file with a syntax error must exit with a failure and print the
//! error with its file, line, and column.

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// Writes `source` to a temporary `.stn` file named after `name` and runs `stone <args> <file>`.
///
/// For example, `stone(&["check"], "ok", "x = 1\n")` checks a one-line program.
fn stone(args: &[&str], name: &str, source: &str) -> (Output, PathBuf) {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let file = dir.join(format!("{name}.stn"));
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
            "{}:1:5: error: expected ':', found end of line\n  |\n1 | if 1\n  |     ^\n",
            file.display()
        )
    );
}

#[test]
fn check_suggests_parentheses_for_a_tuple() {
    let (output, file) = stone(&["check"], "bare_tuple", "pair = 1, \"a\"\n");
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!(
            "{}:1:8: error: values separated by commas need parentheses to make a tuple, as in \
             '(a, b)'\n  |\n1 | pair = 1, \"a\"\n  |        ^^^^^^\n",
            file.display()
        )
    );
}

#[test]
fn fmt_rewrites_a_file_and_check_reports_it() {
    let (output, file) = stone(
        &["fmt", "--check"],
        "fmt_messy",
        "x=1\nif x==1:\n  print( x )\n",
    );
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        format!("{}\n", file.display())
    );
    // --check writes nothing
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "x=1\nif x==1:\n  print( x )\n"
    );

    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("fmt")
        .arg(&file)
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "");
    assert_eq!(
        std::fs::read_to_string(&file).unwrap(),
        "x = 1\nif x == 1:\n    print(x)\n"
    );

    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["fmt", "--check"])
        .arg(&file)
        .output()
        .unwrap();
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "");
}

#[test]
fn fmt_reports_a_syntax_error_and_formats_the_rest() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("fmt_dir");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(dir.join("build")).unwrap();
    std::fs::write(dir.join("bad.stn"), "x = \n").unwrap();
    std::fs::write(dir.join("good.stn"), "y=2\n").unwrap();
    // a build directory stone made is skipped
    std::fs::write(dir.join("build/.stone"), "").unwrap();
    std::fs::write(dir.join("build/skipped.stn"), "z=3\n").unwrap();
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("fmt")
        .arg(&dir)
        .output()
        .unwrap();
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.starts_with(&format!("{}:1:5: error: ", dir.join("bad.stn").display())),
        "{stderr}"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("bad.stn")).unwrap(),
        "x = \n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("good.stn")).unwrap(),
        "y = 2\n"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("build/skipped.stn")).unwrap(),
        "z=3\n"
    );
}

#[test]
fn fmt_formats_standard_input_to_standard_output() {
    use std::io::Write;
    let mut child = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["fmt", "-"])
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .spawn()
        .unwrap();
    child
        .stdin
        .take()
        .unwrap()
        .write_all(b"print( 1+2 )\n")
        .unwrap();
    let output = child.wait_with_output().unwrap();
    assert!(output.status.success());
    assert_eq!(String::from_utf8_lossy(&output.stdout), "print(1 + 2)\n");
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
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!(
            "{}:1:7: error: division by zero\n  |\n1 | print(1 / 0)\n  |       ^^^^^\n",
            file.display()
        )
    );
}

#[test]
fn built_programs_report_runtime_errors_as_run_does() {
    let source = "fn mean(xs: list[int]) -> int:\n    ret xs[0] / xs.len()\n\nprint(mean([4]))\nprint(mean([]))\n";
    let (run, file) = stone(&["run"], "mean_empty", source);
    let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("mean_empty");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&file)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let built = Command::new(&exe).output().expect("program should run");
    let expected = format!(
        "{}:2:9: error: list index out of range (index 0, length 0)\n",
        file.display()
    );
    for output in [&run, &built] {
        assert_eq!(output.status.code(), Some(1), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "4\n");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.starts_with(&expected), "{stderr}");
        assert!(
            stderr.ends_with("\n  |\n2 |     ret xs[0] / xs.len()\n  |         ^^^^^\n"),
            "{stderr}"
        );
    }
}

#[test]
fn built_programs_render_runtime_errors_byte_for_byte_as_run_does() {
    let source = "fn ratio(a: int, b: int) -> int:\n    ret a / b\n\nprint(ratio(6, 3))\nprint(ratio(1, 0))\n";
    let (run, file) = stone(&["run"], "ratio_zero", source);
    let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("ratio_zero");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&file)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let built = Command::new(&exe).output().expect("program should run");
    let expected = format!(
        "{}:2:9: error: division by zero\n  |\n2 |     ret a / b\n  |         ^^^^^\n",
        file.display()
    );
    assert_eq!(String::from_utf8_lossy(&run.stderr), expected);
    assert_eq!(String::from_utf8_lossy(&built.stderr), expected);
    assert_eq!(run.status.code(), Some(1));
    assert_eq!(built.status.code(), Some(1));
}

/// Runs `program` with `args` under `sh` with its stderr sent where its stdout goes, and returns
/// what both wrote, in the order they wrote it.
fn merged_output(program: &Path, args: &[&Path]) -> String {
    let output = Command::new("sh")
        .arg("-c")
        .arg("\"$0\" \"$@\" 2>&1")
        .arg(program)
        .args(args)
        .output()
        .expect("sh should run");
    assert!(output.status.success(), "{output:?}");
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn eprint_and_print_interleave_in_program_order_under_both_backends() {
    let source = "print(\"one\")\neprint(\"two\")\nprint(\"three\")\neprint(\"four\", 4)\n";
    let (run, file) = stone(&["check"], "interleaved", source);
    assert!(run.status.success(), "{run:?}");
    let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("interleaved");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&file)
        .arg("-o")
        .arg(&exe)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let expected = "one\ntwo\nthree\nfour 4\n";
    let stone = Path::new(env!("CARGO_BIN_EXE_stone"));
    assert_eq!(merged_output(stone, &[Path::new("run"), &file]), expected);
    assert_eq!(merged_output(&exe, &[]), expected);
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
///
/// A freshly copied binary can fail to start with `ETXTBSY` while another test's child, forked
/// before it execs, still holds the copy's write descriptor (rust-lang/rust#114554), so this
/// retries until that child has exec'd.
fn uninstall(exe: &PathBuf, args: &[&str], cargo_home: &PathBuf) -> Output {
    let mut attempts = 0;
    loop {
        let result = Command::new(exe)
            .arg("uninstall")
            .args(args)
            .env("CARGO_HOME", cargo_home)
            .stdin(std::process::Stdio::null())
            .output();
        match result {
            Err(e) if e.kind() == std::io::ErrorKind::ExecutableFileBusy && attempts < 100 => {
                attempts += 1;
                std::thread::sleep(std::time::Duration::from_millis(10));
            }
            result => return result.expect("stone should run"),
        }
    }
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

/// Writes `source` to a temporary `.stn` file named after `name` and runs `stone <before> <file>
/// <after>`, returning its stdout.
fn stone_stdout(before: &[&str], name: &str, source: &str, after: &[&str]) -> String {
    let file = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}.stn"));
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
            ("main.stn", "use geometry.vec\nprint(vec.zero())\n"),
            ("geometry/vec.stn", "pub fn zero():\n    ret 0 +\n"),
        ],
    );
    let expected = format!(
        "{}:2:12: error: expected an expression, found end of line\n  |\n2 |     ret 0 +\n  |            ^\n",
        dir.join("geometry/vec.stn").display()
    );
    for command in ["check", "run", "build"] {
        let output = Command::new(env!("CARGO_BIN_EXE_stone"))
            .arg(command)
            .arg(dir.join("main.stn"))
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
            ("main.stn", "use util.twice\nprint(twice(21))\n"),
            ("util.stn", "pub fn twice(x: int) -> int:\n    ret x * 2\n"),
        ],
    );
    let main = dir.join("main.stn");
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
    let entries = "x = 2\nx * 3\n1 / 0\nfn f(n: int) -> int:\n    ret n + x\n\nf(1)\n";
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
        "<stdin>:1:1: error: division by zero\n  |\n1 | 1 / 0\n  | ^^^^^\n"
    );
}

#[test]
fn build_rejects_an_unknown_target() {
    let (output, _) = stone(&["build", "--target", "sparc"], "sparc", "print(1)\n");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "unknown target 'sparc' (expected x86_64-linux, aarch64-linux, or aarch64-macos, \
             with an optional level such as x86_64v3-linux)"
        ),
        "{stderr}"
    );
}

/// Returns the `--target` name of the machine the tests run on.
///
/// For example, it is `x86_64-linux` on an x86-64 Linux machine.
fn host_target() -> &'static str {
    if cfg!(target_os = "macos") {
        "aarch64-macos"
    } else if cfg!(target_arch = "aarch64") {
        "aarch64-linux"
    } else {
        "x86_64-linux"
    }
}

#[test]
fn build_for_the_host_target_by_name_runs() {
    let host = host_target();
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

/// Writes `source` as `main.stn` in a fresh temporary directory named after `name`, then runs it
/// from that directory both with `stone run` and as a binary from `stone build`, returning each
/// run's output.
fn run_and_build_in(name: &str, source: &str) -> (PathBuf, [Output; 2]) {
    run_and_build_with(name, source, |_| {})
}

/// Does what [`run_and_build_in`] does, calling `setup` with the directory first, such as to
/// write files for the program to read.
fn run_and_build_with(
    name: &str,
    source: &str,
    setup: impl FnOnce(&Path),
) -> (PathBuf, [Output; 2]) {
    run_and_build_env(name, source, setup, &[])
}

/// Does what [`run_and_build_with`] does, running the program under both backends with the
/// environment variables `env` added.
fn run_and_build_env(
    name: &str,
    source: &str,
    setup: impl FnOnce(&Path),
    env: &[(&str, &str)],
) -> (PathBuf, [Output; 2]) {
    let dir = project(name, &[("main.stn", source)]);
    setup(&dir);
    let run = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("run")
        .arg("main.stn")
        .current_dir(&dir)
        .envs(env.iter().copied())
        .output()
        .expect("stone should run");
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "main.stn", "-o", "out"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let built = Command::new(dir.join("out"))
        .current_dir(&dir)
        .env("STONE_LEAK_CHECK", "1")
        .envs(env.iter().copied())
        .output()
        .expect("the binary should run");
    (dir, [run, built])
}

#[test]
fn run_and_build_print_warnings_and_still_succeed() {
    let source =
        "fn push(xs: list[int], x: int):\n    xs.append(x)\n\nys = [1]\npush(ys, 2)\nprint(ys)\n";
    let warning = "main.stn:2:5: warning: changes to 'xs' are lost when 'push' returns";
    for command in ["run", "build"] {
        let dir = project(&format!("warns_{command}"), &[("main.stn", source)]);
        let output = Command::new(env!("CARGO_BIN_EXE_stone"))
            .args([command, "main.stn"])
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
    let source =
        "use os\n\nfn stop(xs: list[str]):\n    os.exit(3)\n\nprint(1)\nstop([\"a\"])\nprint(2)\n";
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

#[test]
fn machine_values_are_the_same_in_both_backends() {
    // the golden programs cannot check these, since they differ from machine to machine
    let source = "use os\nprint(os.hostname(), os.cpu_count(), os.platform(), os.arch())\n";
    let (_, [run, built]) = run_and_build_in("os_machine", source);
    assert!(run.status.success(), "{run:?}");
    assert!(built.status.success(), "{built:?}");
    assert_eq!(
        String::from_utf8_lossy(&built.stdout),
        String::from_utf8_lossy(&run.stdout)
    );
}

#[test]
fn input_reads_lines_longer_than_any_buffer() {
    let source = "\
count = 0
longest = 0
total = 0
last = \"\"
while not eof():
    last = input().unwrap()
    count = count + 1
    total = total + last.len()
    if last.len() > longest:
        longest = last.len()
print(count, longest, total, last)
";
    let dir = project("input_long_lines", &[("main.stn", source)]);
    let mut input = "a".repeat(200_000) + "\n\n";
    for i in 0..30_000 {
        input.push_str(&format!("line {i}\n"));
    }
    input.push_str("last");
    std::fs::write(dir.join("input.txt"), &input).unwrap();
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "main.stn", "-o", "out"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    let total = 200_000
        + (0..30_000)
            .map(|i| format!("line {i}").len())
            .sum::<usize>()
        + 4;
    let expected = format!("30003 200000 {total} last\n");
    let mut run = Command::new(env!("CARGO_BIN_EXE_stone"));
    run.args(["run", "main.stn"]);
    let mut built = Command::new(dir.join("out"));
    built.env("STONE_LEAK_CHECK", "1");
    for command in [&mut run, &mut built] {
        let stdin = std::fs::File::open(dir.join("input.txt")).unwrap();
        let output = command
            .current_dir(&dir)
            .stdin(stdin)
            .output()
            .expect("the program should run");
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

/// Returns the next number of a xorshift generator, so tests can make many inputs that are the
/// same on every run.
fn xorshift(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

#[test]
fn compiled_floats_print_and_parse_like_the_interpreter() {
    let mut state = 0x2545_f491_4f6c_dd1d;
    let mut source = String::new();
    for i in 0..4000 {
        let random = xorshift(&mut state);
        // random bits, then powers of two, subnormals, and whole numbers, which are edge cases
        let bits = match i % 4 {
            0 => random,
            1 => (random % 2046 + 1) << 52,
            2 => random % (1 << 52),
            _ => (random % (1 << 53)) as f64 as u64 as f64 as u64,
        };
        let value = if i % 4 == 3 {
            (random % (1 << 53)) as f64
        } else {
            f64::from_bits(bits & !(1 << 63))
        };
        if !value.is_finite() || value == 0.0 {
            continue;
        }
        // every literal reads back as exactly the float it was written from
        source.push_str(&format!("print({value:e}, -{value:e}, {value:e} / 3.0)\n"));
    }
    for _ in 0..3000 {
        let mut text = String::new();
        for _ in 0..xorshift(&mut state) % 30 + 1 {
            text.push((b'0' + (xorshift(&mut state) % 10) as u8) as char);
        }
        let point = xorshift(&mut state) % (text.len() as u64 + 1);
        if point.is_multiple_of(2) {
            text.insert(point as usize, '.');
        }
        let exponent = xorshift(&mut state) % 700;
        text.push_str(&format!("e{}", exponent as i64 - 350));
        source.push_str(&format!("print(\"{text}\", float(\"{text}\"))\n"));
    }
    let (_, [run, built]) = run_and_build_in("floats_differential", &source);
    assert!(run.status.success(), "{run:?}");
    assert!(built.status.success(), "{built:?}");
    let expected = String::from_utf8_lossy(&run.stdout);
    let actual = String::from_utf8_lossy(&built.stdout);
    for (line, (want, got)) in expected.lines().zip(actual.lines()).enumerate() {
        assert_eq!(got, want, "line {}", line + 1);
    }
    assert_eq!(actual.lines().count(), expected.lines().count());
}

/// Returns the type of every program header of the ELF executable `bytes`, such as 3 for
/// `PT_INTERP`, which names the dynamic loader a program needs.
fn program_header_types(bytes: &[u8]) -> Vec<u32> {
    assert_eq!(&bytes[..4], b"\x7fELF");
    let read = |at: usize, size: usize| {
        let mut value = 0u64;
        for (i, byte) in bytes[at..at + size].iter().enumerate() {
            value |= (*byte as u64) << (8 * i);
        }
        value as usize
    };
    let (offset, size, count) = (read(0x20, 8), read(0x36, 2), read(0x38, 2));
    (0..count)
        .map(|i| read(offset + i * size, 4) as u32)
        .collect()
}

/// Checks that the executable `exe`, built for this machine, needs nothing but the system: on
/// Linux it is static, and on macOS it is signed and loads only `libSystem`.
fn assert_needs_only_the_system(exe: &Path) {
    if cfg!(target_os = "macos") {
        let verify = Command::new("codesign")
            .args(["--verify", "--strict"])
            .arg(exe)
            .output()
            .expect("codesign should run");
        assert!(verify.status.success(), "{verify:?}");
        let libraries = Command::new("otool")
            .arg("-L")
            .arg(exe)
            .output()
            .expect("otool should run");
        let libraries = String::from_utf8_lossy(&libraries.stdout);
        let listed: Vec<&str> = libraries.lines().skip(1).map(str::trim).collect();
        assert_eq!(listed.len(), 1, "{libraries}");
        assert!(
            listed[0].starts_with("/usr/lib/libSystem.B.dylib "),
            "{libraries}"
        );
    } else {
        let types = program_header_types(&std::fs::read(exe).unwrap());
        // no dynamic loader and no dynamic section, so nothing but the kernel runs before _start
        assert!(!types.contains(&3), "{types:?}");
        assert!(!types.contains(&2), "{types:?}");
    }
}

#[test]
fn built_programs_need_only_the_system_and_start_on_their_own() {
    let source = "use os\nprint(args(), os.env(\"STONE_TEST\"), os.has_env(\"HOME\"))\n";
    let dir = project("static_start", &[("main.stn", source)]);
    let build = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "main.stn", "-o", "out"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert!(build.status.success(), "{build:?}");
    assert_needs_only_the_system(&dir.join("out"));

    let empty = Command::new(dir.join("out"))
        .env_clear()
        .output()
        .expect("the binary should run");
    assert_eq!(String::from_utf8_lossy(&empty.stdout), "[] none false\n");
    let full = Command::new(dir.join("out"))
        .args(["a", "b c"])
        .env("STONE_TEST", "x=y")
        .output()
        .expect("the binary should run");
    let has_home = std::env::var_os("HOME").is_some();
    assert_eq!(
        String::from_utf8_lossy(&full.stdout),
        format!("['a', 'b c'] some('x=y') {has_home}\n")
    );
}

/// Runs `stone <args>` in `dir`.
///
/// For example, `stone_in(&dir, &["build", "main.stn"])` builds `dir/main.stn`.
fn stone_in(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(args)
        .current_dir(dir)
        .output()
        .expect("stone should run")
}

/// Runs the executable at `exe` and returns what it printed.
///
/// For example, running a build of `print(1)` returns `"1\n"`.
fn stdout_of(exe: &Path) -> String {
    let run = Command::new(exe).output().expect("the binary should run");
    assert!(run.status.success(), "{run:?}");
    String::from_utf8_lossy(&run.stdout).into_owned()
}

#[test]
fn build_defaults_to_the_projects_build_directory() {
    let dir = project("default_build", &[("app/prog.stn", "print(6 * 7)\n")]);
    // built from outside the project, it still lands next to the entry file
    let output = stone_in(&dir, &["build", "app/prog.stn"]);
    assert!(output.status.success(), "{output:?}");
    let build = dir.join("app/build");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        format!("Compiled {}\n", Path::new("app/build/prog").display())
    );
    assert_eq!(stdout_of(&build.join("prog")), "42\n");
    assert!(build.join("prog.s").is_file());
    assert!(build.join(".stone").is_file());
}

#[test]
fn build_names_main_after_its_directory() {
    let dir = project("named_proj", &[("main.stn", "print(1)\n")]);
    let output = stone_in(&dir, &["build", "main.stn"]);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(stdout_of(&dir.join("build/named_proj")), "1\n");
}

#[test]
fn build_puts_other_targets_in_their_own_directory() {
    let dir = project("several_targets", &[("p.stn", "print(2)\n")]);
    let host = host_target();
    let other = if host == "aarch64-macos" {
        "x86_64-linux"
    } else {
        "aarch64-macos"
    };
    let output = stone_in(
        &dir,
        &["build", "p.stn", "--target", host, "--target", other],
    );
    assert!(output.status.success(), "{output:?}");
    assert_eq!(stdout_of(&dir.join("build/p")), "2\n");
    assert!(dir.join("build").join(other).join("p").is_file());
    assert!(!dir.join("build").join(host).exists());
}

#[test]
fn build_takes_several_files_and_reports_each_failure() {
    let dir = project(
        "several_files",
        &[
            ("a.stn", "print(\"a\")\n"),
            ("bad.stn", "x = 1 + \"s\"\n"),
            ("b.stn", "print(\"b\")\n"),
        ],
    );
    let output = stone_in(&dir, &["build", "a.stn", "bad.stn", "b.stn"]);
    assert!(!output.status.success(), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("bad.stn:1:"),
        "{output:?}"
    );
    assert_eq!(stdout_of(&dir.join("build/a")), "a\n");
    assert_eq!(stdout_of(&dir.join("build/b")), "b\n");
    assert!(!dir.join("build/bad").exists());
}

#[test]
fn build_rejects_output_with_several_files_or_targets() {
    let dir = project(
        "output_conflict",
        &[("a.stn", "print(1)\n"), ("b.stn", "print(2)\n")],
    );
    let expected = "error: -o names one executable, so it needs exactly one file and one target\n";
    for args in [
        &["build", "a.stn", "b.stn", "-o", "out"][..],
        &[
            "build",
            "a.stn",
            "--target",
            "x86_64-linux",
            "--target",
            "aarch64-linux",
            "-o",
            "out",
        ],
    ] {
        let output = stone_in(&dir, args);
        assert!(!output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stderr), expected);
        assert!(!dir.join("out").exists());
    }
}

#[test]
fn clean_removes_the_build_directory() {
    let dir = project("clean_project", &[("app/main.stn", "print(1)\n")]);
    assert!(stone_in(&dir, &["build", "app/main.stn"]).status.success());
    assert!(dir.join("app/build/app").is_file());

    // a file names its program's directory
    let output = stone_in(&dir, &["clean", "app/main.stn"]);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        format!("Removed {}\n", Path::new("app/build").display())
    );
    assert!(!dir.join("app/build").exists());

    // with no path, the current directory
    let output = stone_in(&dir.join("app"), &["clean"]);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "nothing to clean in build\n"
    );
}

#[test]
fn clean_refuses_a_build_directory_stone_did_not_make() {
    let dir = project("clean_foreign", &[("build/notes.txt", "mine\n")]);
    let output = stone_in(&dir, &["clean"]);
    assert!(!output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "error: build was not made by stone, so it was left alone\n"
    );
    assert!(dir.join("build/notes.txt").is_file());
}

#[test]
fn printing_stops_at_an_enum_value_nested_too_deeply_in_both_backends() {
    // `N.Z` inside 9,999 `N.S`s is 10,000 levels, which prints, and one more does not
    let source = "enum N:\n    S(N)\n    Z\n\nn = N.Z\nfor i in range(9999):\n    n = N.S(n)\n\
                  print(n)\nn = N.S(n)\nprint(\"start\", n)\n";
    let (_, [run, built]) = run_and_build_in("print_too_deep", source);
    // a print that fails writes nothing of its line
    let expected = format!("{}N.Z{}\n", "N.S(".repeat(9999), ")".repeat(9999));
    for output in [&run, &built] {
        assert_eq!(output.status.code(), Some(1), "{output:?}");
        assert!(String::from_utf8_lossy(&output.stdout) == expected);
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            "main.stn:10:1: error: value is nested too deeply to print (more than 10000 levels)\n   \
             |\n10 | print(\"start\", n)\n   | ^^^^^^^^^^^^^^^^^\n"
        );
    }
}

#[test]
fn str_stops_at_a_value_nested_too_deeply_in_both_backends() {
    let source = "enum N:\n    S(N)\n    Z\n\nn = N.Z\nfor i in range(10000):\n    n = N.S(n)\n\
                  print(\"start\")\ns = str(n)\nprint(s.len())\n";
    let (_, [run, built]) = run_and_build_in("str_too_deep", source);
    for output in [&run, &built] {
        assert_eq!(output.status.code(), Some(1), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "start\n");
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            "main.stn:9:5: error: value is nested too deeply to print (more than 10000 levels)\n  \
             |\n9 | s = str(n)\n  |     ^^^^^^\n"
        );
    }
}

#[test]
fn printing_counts_struct_values_toward_the_depth_limit_in_both_backends() {
    // a leaf inside 9,999 trees is 10,000 levels, which prints, and one more does not
    let source = "struct T:\n    kids: list[T]\n\nt = T(kids=[])\nfor i in range(9999):\n    \
                  t = T(kids=[t])\nprint(t)\nt = T(kids=[t])\nprint(\"start\", t)\n";
    let (_, [run, built]) = run_and_build_in("print_struct_too_deep", source);
    let expected = format!(
        "{}T(kids=[]){}\n",
        "T(kids=[".repeat(9999),
        "])".repeat(9999)
    );
    for output in [&run, &built] {
        assert_eq!(output.status.code(), Some(1), "{output:?}");
        assert!(String::from_utf8_lossy(&output.stdout) == expected);
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            "main.stn:9:1: error: value is nested too deeply to print (more than 10000 levels)\n  \
             |\n9 | print(\"start\", t)\n  | ^^^^^^^^^^^^^^^^^\n"
        );
    }
}

#[test]
fn check_reports_a_mistake_inside_a_generic_function() {
    let (output, file) = stone(
        &["check"],
        "generic_add",
        "fn total[T](xs: list[T]) -> T:\n    ret xs[0] + xs[1]\nprint(total([1, 2]))\n",
    );
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!(
            "{}:2:9: error: '+' needs int, float, str, or bytes operands, found T\n  |\n2 |     ret \
             xs[0] + xs[1]\n  |         ^^^^^^^^^^^^^\n",
            file.display()
        )
    );
}

#[test]
fn check_reports_a_method_a_trait_needs() {
    let (output, file) = stone(
        &["check"],
        "trait_missing",
        "trait Shape:\n    fn area(self) -> float\nstruct Sq:\n    s: float\n\
         impl Shape for Sq:\n    fn side(self) -> float:\n        ret self.s\n",
    );
    assert_eq!(output.status.code(), Some(1));
    let file = file.display();
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        format!(
            "{file}:5:16: error: 'Sq' does not implement 'area' of 'Shape'\n  |\n5 | impl Shape \
             for Sq:\n  |                ^^\n{file}:6:8: error: 'side' is not a method of \
             'Shape', so it goes in an 'impl Sq:' block\n  |\n6 |     fn side(self) -> float:\n  \
             |        ^^^^\n"
        )
    );
}

/// Asserts that both runs succeeded and printed the same lines, naming the first that differs.
fn assert_same_output([run, built]: &[Output; 2]) {
    assert!(run.status.success(), "{run:?}");
    assert!(built.status.success(), "{built:?}");
    let expected = String::from_utf8_lossy(&run.stdout);
    let actual = String::from_utf8_lossy(&built.stdout);
    for (line, (want, got)) in expected.lines().zip(actual.lines()).enumerate() {
        assert_eq!(got, want, "line {}", line + 1);
    }
    assert_eq!(actual.lines().count(), expected.lines().count());
}

#[test]
fn compiled_files_are_text_exactly_when_the_interpreter_says() {
    // the edge cases of UTF-8, then random bytes that are mostly the start of a character or the
    // rest of one
    let mut files: Vec<Vec<u8>> = [
        &b""[..],
        b"plain",
        b"\0",
        b"a\0b",
        b"\xc0\x80",
        b"\xc1\xbf",
        b"\xc2\x80",
        b"\xdf\xbf",
        b"\xe0\x80\x80",
        b"\xe0\x9f\xbf",
        b"\xe0\xa0\x80",
        b"\xed\x9f\xbf",
        b"\xed\xa0\x80",
        b"\xef\xbf\xbf",
        b"\xf0\x80\x80\x80",
        b"\xf0\x8f\xbf\xbf",
        b"\xf0\x90\x80\x80",
        b"\xf4\x8f\xbf\xbf",
        b"\xf4\x90\x80\x80",
        b"\xf5\x80\x80\x80",
        b"\xff",
        b"\x80",
        b"\xe2\x82",
        b"\xe2\x82\xac",
        b"\xf0\x9f\x98",
        b"\xf0\x9f\x98\x80",
    ]
    .iter()
    .map(|bytes| bytes.to_vec())
    .collect();
    let mut state = 0x9e37_79b9_7f4a_7c15;
    let pieces: [&[u8]; 8] = [
        b"a",
        b"\n",
        b"\r\n",
        b"\xc3",
        b"\xa9",
        b"\xe2\x82",
        b"\xf0\x9f",
        b"\x80",
    ];
    for _ in 0..400 {
        let mut bytes = Vec::new();
        for _ in 0..xorshift(&mut state) % 8 {
            let random = xorshift(&mut state);
            match random % 3 {
                0 => bytes.push((random >> 8) as u8),
                _ => bytes.extend_from_slice(pieces[(random >> 8) as usize % pieces.len()]),
            }
        }
        files.push(bytes);
    }
    let source = "use fs\nfor name in fs.list_dir(\"data\").unwrap():\n    \
                  path = \"data/\" + name\n    text = fs.read(path)\n    \
                  lines = fs.read_lines(path)\n    \
                  print(name, text.is_ok(), text.unwrap_or(\"\").len(), \
                  lines.unwrap_or([]).len())\n";
    let (_, outputs) = run_and_build_with("fs_text_differential", source, |dir| {
        std::fs::create_dir(dir.join("data")).unwrap();
        for (i, bytes) in files.iter().enumerate() {
            std::fs::write(dir.join(format!("data/{i:03}")), bytes).unwrap();
        }
    });
    assert_same_output(&outputs);
    let text = String::from_utf8_lossy(&outputs[0].stdout).into_owned();
    // both kinds happen, so the check is not passing by refusing everything
    assert!(
        text.contains(" true ") && text.contains(" false "),
        "{text}"
    );
}

#[test]
fn large_files_and_directories_read_the_same_in_both_backends() {
    let source = "use fs\nlines = fs.read_lines(\"big.txt\").unwrap()\n\
                  print(lines.len(), lines[0], lines[lines.len() - 1])\n\
                  text = fs.read(\"big.txt\").unwrap()\nprint(text.len())\n\
                  print(fs.write(\"copy.txt\", text), fs.read(\"copy.txt\").unwrap() == text)\n\
                  names = fs.list_dir(\"many\").unwrap()\n\
                  print(names.len(), names[0], names[names.len() - 1])\n\
                  total = 0\nfor name in names:\n    total += name.len()\nprint(total)\n";
    let (_, outputs) = run_and_build_with("fs_large", source, |dir| {
        // more than 3 MiB, so the buffer doubles past the allocator's largest class
        let mut text = String::new();
        for i in 0..400_000 {
            text.push_str(&format!("line {i}\n"));
        }
        std::fs::write(dir.join("big.txt"), text).unwrap();
        // more entries than one getdents64 call returns, with names of many lengths
        std::fs::create_dir(dir.join("many")).unwrap();
        for i in 0..2000 {
            let name = format!("{i}-{}", "x".repeat(i % 37));
            std::fs::write(dir.join("many").join(name), "").unwrap();
        }
    });
    assert_same_output(&outputs);
    let text = String::from_utf8_lossy(&outputs[0].stdout).into_owned();
    assert!(text.starts_with("400000 line 0 line 399999\n"), "{text}");
}

#[test]
fn file_errors_are_worded_the_same_in_both_backends() {
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::PermissionsExt;
    let source = "use fs\nprint(fs.read(\"secret.txt\"))\nprint(fs.write(\"secret.txt\", \"x\"))\n\
                  print(fs.list_dir(\"locked\"))\nprint(fs.write(\"locked/new.txt\", \"x\"))\n\
                  print(fs.exists(\"locked/inside.txt\"))\nprint(fs.list_dir(\"odd\"))\n\
                  print(fs.read(\"loop\"))\nprint(fs.exists(\"dangling\"))\n";
    let (dir, outputs) = run_and_build_with("fs_errors", source, |dir| {
        std::fs::write(dir.join("secret.txt"), "hidden").unwrap();
        std::fs::create_dir(dir.join("locked")).unwrap();
        std::fs::write(dir.join("locked/inside.txt"), "").unwrap();
        for path in ["secret.txt", "locked"] {
            let permissions = std::fs::Permissions::from_mode(0o000);
            std::fs::set_permissions(dir.join(path), permissions).unwrap();
        }
        // a name that is not UTF-8, a link to itself, and a link to nothing
        std::fs::create_dir(dir.join("odd")).unwrap();
        // which macOS's file system refuses to create
        let name = std::ffi::OsStr::from_bytes(b"caf\xe9");
        let _ = std::fs::write(dir.join("odd").join(name), "");
        std::os::unix::fs::symlink("loop", dir.join("loop")).unwrap();
        std::os::unix::fs::symlink("nowhere", dir.join("dangling")).unwrap();
    });
    // root reads anything, so the permissions mean nothing there
    let root = std::fs::read(dir.join("secret.txt")).is_ok();
    for path in ["secret.txt", "locked"] {
        let permissions = std::fs::Permissions::from_mode(0o755);
        std::fs::set_permissions(dir.join(path), permissions).unwrap();
    }
    assert_same_output(&outputs);
    let text = String::from_utf8_lossy(&outputs[0].stdout).into_owned();
    let odd = match std::fs::read_dir(dir.join("odd")).unwrap().count() {
        0 => "ok([])",
        _ => "err(\"not valid UTF-8 text: 'odd'\")",
    };
    let mut expected = vec![
        "err(\"permission denied: 'secret.txt'\")".to_string(),
        "err(\"permission denied: 'secret.txt'\")".to_string(),
        "err(\"permission denied: 'locked'\")".to_string(),
        "err(\"permission denied: 'locked/new.txt'\")".to_string(),
        "false".to_string(),
        odd.to_string(),
        "err(\"too many levels of symbolic links: 'loop'\")".to_string(),
        "false".to_string(),
    ];
    if root {
        expected.drain(..5);
        assert!(text.ends_with(&(expected.join("\n") + "\n")), "{text}");
    } else {
        assert_eq!(text, expected.join("\n") + "\n");
    }
}

/// Encodes `name` as a DNS name, a length before each dot-separated label and a 0 at the end.
fn dns_name(name: &str) -> Vec<u8> {
    let mut out = vec![];
    for label in name.split('.') {
        out.push(label.len() as u8);
        out.extend_from_slice(label.as_bytes());
    }
    out.push(0);
    out
}

/// Returns a DNS server's reply to `query` from a fixed set of answers: the question's name and
/// type decide the reply, which may be malformed or never come, as the stone resolver must
/// handle.
fn dns_reply(query: &[u8]) -> Option<Vec<u8>> {
    let mut at = 12;
    let mut labels = vec![];
    while query[at] != 0 {
        let length = query[at] as usize;
        labels.push(String::from_utf8_lossy(&query[at + 1..at + 1 + length]).to_string());
        at += 1 + length;
    }
    let name = labels.join(".");
    let question = &query[12..at + 5];
    let kind = u16::from_be_bytes([query[at + 1], query[at + 2]]);
    let record = |rtype: u16, data: &[u8]| {
        let mut out = vec![0xc0, 12];
        out.extend_from_slice(&rtype.to_be_bytes());
        out.extend_from_slice(&[0, 1, 0, 0, 0, 60]);
        out.extend_from_slice(&(data.len() as u16).to_be_bytes());
        out.extend_from_slice(data);
        out
    };
    let v6 = [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7];
    let (rcode, answers): (u8, Vec<Vec<u8>>) = match (name.as_str(), kind) {
        ("alpha.test", 1) => (0, vec![record(1, &[10, 1, 2, 3])]),
        ("alpha.test", 28) => (0, vec![record(28, &v6)]),
        // another name for alpha.test, whose own record follows under that name
        ("www.test", _) => {
            let alias = record(5, &dns_name("alpha.test"));
            let mut target = dns_name("alpha.test");
            let data: &[u8] = if kind == 1 { &[10, 1, 2, 3] } else { &v6 };
            target.extend_from_slice(&kind.to_be_bytes());
            target.extend_from_slice(&[0, 1, 0, 0, 0, 60]);
            target.extend_from_slice(&(data.len() as u16).to_be_bytes());
            target.extend_from_slice(data);
            (0, vec![alias, target])
        }
        ("short.corp.test", 1) => (0, vec![record(1, &[10, 9, 9, 9])]),
        ("short.corp.test", _) => (0, vec![]),
        ("broken.test", _) => return Some(query[..5].to_vec()),
        ("refused.test", _) => (5, vec![]),
        ("slow.test", _) => return None,
        _ => (3, vec![]),
    };
    let mut reply = vec![
        query[0],
        query[1],
        0x81,
        0x80 | rcode,
        0,
        1,
        0,
        answers.len() as u8,
    ];
    reply.extend_from_slice(&[0, 0, 0, 0]);
    reply.extend_from_slice(question);
    for answer in answers {
        reply.extend_from_slice(&answer);
    }
    Some(reply)
}

#[test]
fn both_backends_resolve_names_through_dns_alike() {
    let server = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let port = server.local_addr().unwrap().port();
    std::thread::spawn(move || {
        let mut query = [0; 512];
        while let Ok((n, from)) = server.recv_from(&mut query) {
            if let Some(reply) = dns_reply(&query[..n]) {
                server.send_to(&reply, from).unwrap();
            }
        }
    });
    let source = "use net\nfor host in [\"alpha.test\", \"WWW.test.\", \"short\", \"missing.test\", \
                  \"broken.test\", \"refused.test\", \"slow.test\"]:\n    \
                  print(host, net.resolve(host))\n";
    let conf =
        format!("nameserver 127.0.0.1:{port}\nsearch corp.test\noptions timeout:1 attempts:1\n");
    let (_, [run, built]) = run_and_build_env(
        "net_dns",
        source,
        |dir| {
            std::fs::write(dir.join("resolv.conf"), &conf).unwrap();
            std::fs::write(dir.join("hosts"), "").unwrap();
        },
        &[
            ("STONE_RESOLV_CONF", "resolv.conf"),
            ("STONE_HOSTS", "hosts"),
        ],
    );
    let expected = "alpha.test ok(['10.1.2.3', '2001:db8::7'])\n\
                    WWW.test. ok(['10.1.2.3', '2001:db8::7'])\n\
                    short ok(['10.9.9.9'])\n\
                    missing.test err(\"unknown host: 'missing.test'\")\n\
                    broken.test err(\"bad DNS reply: '127.0.0.1'\")\n\
                    refused.test err(\"refused: '127.0.0.1'\")\n\
                    slow.test err(\"timed out: '127.0.0.1'\")\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

#[test]
fn both_backends_talk_over_a_unix_socket_alike() {
    let source = "use net\n\
                  s = net.connect_unix(\"echo.sock\").unwrap()\n\
                  s.send(\"ping\\n\").unwrap()\n\
                  print(s.read_line())\n\
                  s.send_bytes(b\"\\x00\\xff\\n\").unwrap()\n\
                  print(s.read_exact(3))\n";
    let (_, [run, built]) = run_and_build_env(
        "net_unix_echo",
        source,
        |dir| {
            let path = dir.join("echo.sock");
            let _ = std::fs::remove_file(&path);
            let server = std::os::unix::net::UnixListener::bind(&path).unwrap();
            std::thread::spawn(move || {
                for mut client in server.incoming().flatten() {
                    std::thread::spawn(move || {
                        let mut copy = client.try_clone().unwrap();
                        let _ = std::io::copy(&mut client, &mut copy);
                    });
                }
            });
        },
        &[],
    );
    let expected = "ok(some('ping'))\nok(b'\\x00\\xff\\n')\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

#[test]
fn both_backends_give_up_connecting_after_a_timeout() {
    // an address in a private range that nothing answers, so a connection waits until it times
    // out, unless this network refuses it at once
    let source = "use net\nprint(net.connect_timeout(\"10.255.255.1\", 80, 0.2))\n";
    let (_, [run, built]) = run_and_build_in("net_connect_timeout", source);
    let expected = "err(\"timed out: '10.255.255.1:80'\")\n";
    if String::from_utf8_lossy(&run.stdout) != expected {
        eprintln!(
            "skipping: this network does not leave 10.255.255.1 unanswered ({})",
            String::from_utf8_lossy(&run.stdout).trim()
        );
        return;
    }
    assert!(built.status.success(), "{built:?}");
    assert_eq!(String::from_utf8_lossy(&built.stdout), expected);
}

/// How the fake X server of [`fake_x_server`] describes itself.
#[derive(Clone, Copy)]
struct FakeDisplay {
    /// Whether it takes pixels most significant byte first.
    msb: bool,
    /// The depth of the root window, and the bits per pixel of its pixmap format.
    depth: u8,
    bits: u8,
    /// The most 4-byte units one request may hold.
    max_request: u16,
    /// The reason it refuses connections, if it does.
    refuse: Option<&'static str>,
    /// The events it sends once the window is mapped.
    script: FakeScript,
    /// Whether its setup reply is cut short after 20 bytes, its length saying so.
    short: bool,
    /// What it does with MIT-SHM.
    shm: FakeShm,
}

/// The events a fake X server sends once the window is mapped.
#[derive(Clone, Copy, PartialEq)]
enum FakeScript {
    /// [`fake_events`], all at once.
    All,
    /// [`paced_events`], a part after each frame.
    Paced,
    /// An exposure alone.
    Expose,
}

/// What a fake X server does with the MIT-SHM extension.
#[derive(Clone, Copy, PartialEq)]
enum FakeShm {
    /// It does not have it.
    Absent,
    /// It shows images from the memory it is sent.
    Working,
    /// It has it, but answers every attachment with an error.
    Refusing,
}

/// The major opcode and first event code the fake X server gives MIT-SHM.
const FAKE_SHM: (u8, u8) = (130, 70);

/// What the fake X server saw on one connection: the authorization's name and data, and every
/// request after the setup.
#[derive(Default, Debug)]
struct FakeConnection {
    auth: (Vec<u8>, Vec<u8>),
    requests: Vec<Vec<u8>>,
    /// Whether the client has closed the connection.
    closed: bool,
    /// The pixels of each ShmPutImage, as 4 bytes each by row.
    shm_images: Vec<Vec<u8>>,
}

/// C's `struct iovec`, for [`receive`].
#[repr(C)]
struct IoVec {
    base: *mut u8,
    len: usize,
}

/// Linux's `struct msghdr`, for [`receive`].
#[repr(C)]
struct MsgHdr {
    name: *mut u8,
    namelen: u32,
    iov: *mut IoVec,
    iovlen: usize,
    control: *mut u8,
    controllen: usize,
    flags: i32,
}

unsafe extern "C" {
    fn recvmsg(fd: i32, msg: *mut MsgHdr, flags: i32) -> isize;
}

/// Reads from `stream` until `buf` is full, as an X server does, adding any descriptor that
/// comes with the bytes to `fds`. On macOS, where clients never send one, it reads plainly.
fn receive(
    stream: &mut std::os::unix::net::UnixStream,
    buf: &mut [u8],
    fds: &mut Vec<std::fs::File>,
) -> std::io::Result<()> {
    use std::io::Read;
    use std::os::fd::{AsRawFd, FromRawFd};
    if !cfg!(target_os = "linux") {
        return stream.read_exact(buf);
    }
    let mut filled = 0;
    while filled < buf.len() {
        let mut control = [0u64; 3];
        let mut iov = IoVec {
            base: buf[filled..].as_mut_ptr(),
            len: buf.len() - filled,
        };
        let mut msg = MsgHdr {
            name: std::ptr::null_mut(),
            namelen: 0,
            iov: &mut iov,
            iovlen: 1,
            control: control.as_mut_ptr().cast(),
            controllen: 24,
            flags: 0,
        };
        // SAFETY: msg points at one iovec over the rest of `buf` and 24 bytes of control
        let n = unsafe { recvmsg(stream.as_raw_fd(), &mut msg, 0) };
        if n <= 0 {
            return Err(std::io::ErrorKind::UnexpectedEof.into());
        }
        if msg.controllen >= 20 && control[1] == 1 | 1 << 32 {
            // SAFETY: SCM_RIGHTS gave this process the descriptor, which nothing else owns
            fds.push(unsafe { std::fs::File::from_raw_fd(control[2] as u32 as i32) });
        }
        filled += n as usize;
    }
    Ok(())
}

/// Answers the MIT-SHM request `request` as a fake X server whose extension does `shm`, with
/// the descriptors the client sent in `fds`, the segments it attached in `segments`, and what
/// it shows recorded in `seen`.
fn fake_shm_request(
    stream: &mut std::os::unix::net::UnixStream,
    request: &[u8],
    shm: FakeShm,
    fds: &mut Vec<std::fs::File>,
    segments: &mut std::collections::HashMap<u32, std::fs::File>,
    seen: &std::sync::Mutex<FakeConnection>,
) -> std::io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::FileExt;
    let word = |at: usize| u32::from_le_bytes(request[at..at + 4].try_into().unwrap());
    let half = |at: usize| u16::from_le_bytes([request[at], request[at + 1]]) as usize;
    match request[1] {
        // ShmQueryVersion: 1.2, which has ShmAttachFd
        0 => {
            let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0];
            reply.resize(32, 0);
            stream.write_all(&reply)?;
        }
        // ShmDetach
        2 => {
            segments.remove(&word(4));
        }
        // ShmPutImage of the whole image, from its segment
        3 => {
            let (width, height) = (half(12), half(14));
            assert_eq!(
                (half(16), half(18), half(20), half(22)),
                (0, 0, width, height)
            );
            let mut pixels = vec![0; width * height * 4];
            segments[&word(32)].read_exact_at(&mut pixels, word(36) as u64)?;
            seen.lock().unwrap().shm_images.push(pixels);
            if request[30] == 1 {
                let completion = x11_event(
                    FAKE_SHM.1,
                    0,
                    &[
                        (4, word(4), 4),
                        (8, 3, 2),
                        (10, FAKE_SHM.0 as u32, 1),
                        (12, word(32), 4),
                    ],
                );
                stream.write_all(&completion)?;
            }
        }
        // ShmAttachFd, with the descriptor that came with it
        6 => {
            let fd = fds.remove(0);
            match shm {
                FakeShm::Refusing => {
                    // BadAccess
                    let error = x11_event(
                        0,
                        10,
                        &[(4, word(4), 4), (8, 6, 2), (10, FAKE_SHM.0 as u32, 1)],
                    );
                    stream.write_all(&error)?;
                }
                _ => {
                    segments.insert(word(4), fd);
                }
            }
        }
        minor => panic!("unexpected MIT-SHM request {minor}"),
    }
    Ok(())
}

/// The atoms the fake X server gives out, by name.
const FAKE_ATOMS: [(&str, u32); 4] = [
    ("WM_PROTOCOLS", 301),
    ("WM_DELETE_WINDOW", 302),
    ("_NET_WM_NAME", 303),
    ("UTF8_STRING", 304),
];

/// The fake keyboard's keysyms, two per keycode from 8: `a` on 38, `1` on 10, Escape on 9,
/// Shift on 50, the Greek alpha, a Unicode keysym, on 60, and the Cyrillic ef, a keysym of its
/// own (Cyrillic_ef and Cyrillic_EF), on 61.
fn fake_keysyms() -> Vec<u32> {
    let mut syms = vec![0; 248 * 2];
    let mut set = |keycode: usize, plain: u32, shifted: u32| {
        syms[(keycode - 8) * 2] = plain;
        syms[(keycode - 8) * 2 + 1] = shifted;
    };
    set(38, 0x61, 0x41);
    set(10, 0x31, 0x21);
    set(9, 0xff1b, 0);
    set(50, 0xffe1, 0);
    set(60, 0x0100_03b1, 0x0100_0391);
    set(61, 0x6c6, 0x6e6);
    syms
}

/// A 32-byte X11 event: `code`, `detail`, a sequence number of 0, then `fields`, each written at
/// its offset as two or four bytes, least significant first.
fn x11_event(code: u8, detail: u8, fields: &[(usize, u32, usize)]) -> [u8; 32] {
    let mut event = [0; 32];
    event[0] = code;
    event[1] = detail;
    for &(at, value, size) in fields {
        event[at..at + size].copy_from_slice(&value.to_le_bytes()[..size]);
    }
    event
}

/// A key or button event of `code` for `detail` at `time`, with the modifier `state`, at the
/// point `x`, `y` of the window.
fn x11_input(code: u8, detail: u8, time: u32, state: u32, x: i16, y: i16) -> [u8; 32] {
    x11_event(
        code,
        detail,
        &[
            (4, time, 4),
            (24, x as u16 as u32, 2),
            (26, y as u16 as u32, 2),
            (28, state, 2),
        ],
    )
}

/// The events the fake X server sends once the window `window` is mapped.
fn fake_events(window: u32) -> Vec<u8> {
    let expose = |count| x11_event(12, 0, &[(4, window, 4), (16, count, 2)]);
    let events = [
        expose(0),
        x11_event(
            22,
            0,
            &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
        ),
        x11_input(2, 38, 10, 0, 0, 0),
        // a held key repeats as a release and a press at the same time
        x11_input(3, 38, 20, 0, 0, 0),
        x11_input(2, 38, 20, 0, 0, 0),
        x11_input(3, 38, 30, 0, 0, 0),
        // Shift, Escape, Control, Caps Lock on a digit and a letter, Shift itself, and alpha
        x11_input(2, 38, 40, 1, 0, 0),
        x11_input(2, 9, 41, 0, 0, 0),
        x11_input(2, 38, 42, 4, 0, 0),
        x11_input(2, 10, 43, 2, 0, 0),
        x11_input(2, 38, 44, 2, 0, 0),
        x11_input(2, 50, 45, 0, 0, 0),
        x11_input(2, 60, 46, 0, 0, 0),
        // a legacy keysym, alone and with Caps Lock
        x11_input(2, 61, 47, 0, 0, 0),
        x11_input(2, 61, 48, 2, 0, 0),
        x11_input(4, 1, 50, 0, 10, 20),
        x11_input(5, 3, 51, 0, 11, -2),
        x11_input(4, 4, 52, 0, 0, 0),
        x11_input(5, 4, 53, 0, 0, 0),
        x11_input(4, 5, 54, 0, 0, 0),
        x11_input(6, 0, 55, 0, 5, 6),
        // a size that did not change, then an exposure in two parts
        x11_event(
            22,
            0,
            &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
        ),
        expose(1),
        expose(0),
        // a client message of another protocol, then the close button
        x11_event(33, 32, &[(4, window, 4), (8, 301, 4), (12, 999, 4)]),
        x11_event(33, 32, &[(4, window, 4), (8, 301, 4), (12, 302, 4)]),
    ];
    events.concat()
}

/// The part of its events a paced fake X server sends at stage `stage` for the window `window`:
/// an exposure once it is mapped (0), a new size and `a` after the first frame (1), `A` after
/// the first frame of the new size (2), and the close button after the next (3).
fn paced_events(window: u32, stage: usize) -> Vec<u8> {
    let events = match stage {
        0 => vec![x11_event(12, 0, &[(4, window, 4)])],
        1 => vec![
            x11_event(
                22,
                0,
                &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
            ),
            x11_input(2, 38, 1, 0, 0, 0),
        ],
        2 => vec![x11_input(2, 38, 2, 1, 0, 0)],
        3 => vec![x11_event(
            33,
            32,
            &[(4, window, 4), (8, 301, 4), (12, 302, 4)],
        )],
        _ => vec![],
    };
    events.concat()
}

/// The setup reply of the fake X server described by `display`, or its refusal.
fn fake_setup(display: FakeDisplay) -> Vec<u8> {
    if let Some(reason) = display.refuse {
        let mut padded = reason.as_bytes().to_vec();
        padded.resize(reason.len().div_ceil(4) * 4, 0);
        let mut reply = vec![0, reason.len() as u8, 11, 0, 0, 0];
        reply.extend((padded.len() as u16 / 4).to_le_bytes());
        reply.extend(padded);
        return reply;
    }
    let mut more = vec![];
    more.extend(1u32.to_le_bytes()); // release
    more.extend(0x0020_0000u32.to_le_bytes()); // resource id base
    more.extend(0x001f_ffffu32.to_le_bytes()); // resource id mask
    more.extend(0u32.to_le_bytes()); // motion buffer
    more.extend(8u16.to_le_bytes()); // vendor length
    more.extend(display.max_request.to_le_bytes());
    more.extend([1, 1, display.msb as u8, 0, 32, 32, 8, 255, 0, 0, 0, 0]);
    more.extend(b"fakestn!");
    more.extend([display.depth, display.bits, 32, 0, 0, 0, 0, 0]);
    // the screen
    more.extend(0x100u32.to_le_bytes());
    more.extend(0x20u32.to_le_bytes());
    more.extend(0xff_ffffu32.to_le_bytes());
    more.extend(0u32.to_le_bytes());
    more.extend(0u32.to_le_bytes());
    for value in [1920u16, 1080, 500, 300, 1, 1] {
        more.extend(value.to_le_bytes());
    }
    more.extend(0x21u32.to_le_bytes());
    more.extend([0, 0, display.depth, 1]);
    // its one depth, with one TrueColor visual
    more.extend([display.depth, 0, 1, 0, 0, 0, 0, 0]);
    more.extend(0x21u32.to_le_bytes());
    more.extend([4, 8, 0, 1]);
    let masks = match display.depth {
        16 => [0xf800u32, 0x7e0, 0x1f],
        _ => [0xff_0000, 0xff00, 0xff],
    };
    for mask in masks {
        more.extend(mask.to_le_bytes());
    }
    more.extend([0; 4]);
    if display.short {
        more.truncate(20);
    }
    let mut reply = vec![1, 0, 11, 0, 0, 0];
    reply.extend((more.len() as u16 / 4).to_le_bytes());
    reply.extend(more);
    reply
}

/// Serves one client of the fake X server on `stream`, recording what it sends in `seen`.
fn fake_x_client(
    mut stream: std::os::unix::net::UnixStream,
    display: FakeDisplay,
    seen: &std::sync::Mutex<FakeConnection>,
) -> std::io::Result<()> {
    use std::io::{Read, Write};
    let mut setup = [0; 12];
    stream.read_exact(&mut setup)?;
    let name_len = u16::from_le_bytes([setup[6], setup[7]]) as usize;
    let data_len = u16::from_le_bytes([setup[8], setup[9]]) as usize;
    let mut name = vec![0; name_len.div_ceil(4) * 4];
    let mut data = vec![0; data_len.div_ceil(4) * 4];
    stream.read_exact(&mut name)?;
    stream.read_exact(&mut data)?;
    name.truncate(name_len);
    data.truncate(data_len);
    seen.lock().unwrap().auth = (name, data);
    stream.write_all(&fake_setup(display))?;
    let mut window = 0;
    let mut stage = 0;
    let mut fds = vec![];
    let mut segments = std::collections::HashMap::new();
    loop {
        let mut head = [0; 4];
        if receive(&mut stream, &mut head, &mut fds).is_err() {
            seen.lock().unwrap().closed = true;
            return Ok(());
        }
        let units = u16::from_le_bytes([head[2], head[3]]) as usize;
        let mut request = head.to_vec();
        request.resize(units * 4, 0);
        receive(&mut stream, &mut request[4..], &mut fds)?;
        // recorded first, since answering it fails once the client has closed
        seen.lock().unwrap().requests.push(request.clone());
        let word = |at: usize| u32::from_le_bytes(request[at..at + 4].try_into().unwrap());
        match request[0] {
            1 => window = word(4),
            8 => match display.script {
                FakeScript::All => stream.write_all(&fake_events(window))?,
                FakeScript::Paced => stream.write_all(&paced_events(window, 0))?,
                FakeScript::Expose => stream.write_all(&x11_event(12, 0, &[(4, window, 4)]))?,
            },
            // a frame starts with the strip at the top, and after the first, only one as wide
            // as the new size moves the script on
            72 if display.script == FakeScript::Paced && request[18..20] == [0, 0] => {
                if stage == 0 || request[12..14] == 400u16.to_le_bytes() {
                    stage += 1;
                    stream.write_all(&paced_events(window, stage))?;
                }
            }
            16 => {
                let len = u16::from_le_bytes([request[4], request[5]]) as usize;
                let name = std::str::from_utf8(&request[8..8 + len]).unwrap();
                let atom = FAKE_ATOMS.iter().find(|(n, _)| *n == name).unwrap().1;
                let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0];
                reply.extend(atom.to_le_bytes());
                reply.resize(32, 0);
                stream.write_all(&reply)?;
            }
            // QueryExtension, which knows MIT-SHM alone
            98 => {
                let len = u16::from_le_bytes([request[4], request[5]]) as usize;
                let present = &request[8..8 + len] == b"MIT-SHM" && display.shm != FakeShm::Absent;
                let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0];
                if present {
                    reply.extend([1, FAKE_SHM.0, FAKE_SHM.1, 150]);
                }
                reply.resize(32, 0);
                stream.write_all(&reply)?;
            }
            // GetInputFocus, which clients ask to wait for what they sent before it
            43 => stream.write_all(&[[1].as_slice(), &[0; 31]].concat())?,
            op if op == FAKE_SHM.0 => fake_shm_request(
                &mut stream,
                &request,
                display.shm,
                &mut fds,
                &mut segments,
                seen,
            )?,
            101 => {
                let (first, count) = (request[4] as usize, request[5] as usize);
                let syms = fake_keysyms();
                let mut reply = vec![1, 2, 0, 0];
                reply.extend(((count * 2) as u32).to_le_bytes());
                reply.resize(32, 0);
                for sym in &syms[(first - 8) * 2..(first - 8 + count) * 2] {
                    reply.extend(sym.to_le_bytes());
                }
                stream.write_all(&reply)?;
            }
            _ => {}
        }
    }
}

/// Starts a fake X server described by `display`, listening on the Unix socket `path`, and
/// returns what it sees on each connection, in order.
fn fake_x_server(
    path: &Path,
    display: FakeDisplay,
) -> std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<FakeConnection>>>>> {
    use std::sync::{Arc, Mutex};
    let _ = std::fs::remove_file(path);
    let listener = std::os::unix::net::UnixListener::bind(path).unwrap();
    let connections: Arc<Mutex<Vec<Arc<Mutex<FakeConnection>>>>> = Arc::default();
    let all = connections.clone();
    std::thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            let seen = Arc::new(Mutex::new(FakeConnection::default()));
            all.lock().unwrap().push(seen.clone());
            std::thread::spawn(move || {
                // a client that closed before reading everything sent ends it with an error
                let _ = fake_x_client(stream, display, &seen);
                seen.lock().unwrap().closed = true;
            });
        }
    });
    connections
}

/// The pixels of every PutImage among `requests`, as 4 bytes each by row, `width` wide.
fn put_images(requests: &[Vec<u8>], width: usize) -> Vec<u8> {
    let mut image = vec![];
    for request in requests.iter().filter(|r| r[0] == 72) {
        assert_eq!(request[1], 2, "ZPixmap");
        assert_eq!(
            u16::from_le_bytes([request[12], request[13]]) as usize,
            width
        );
        let rows = u16::from_le_bytes([request[14], request[15]]) as usize;
        let y = u16::from_le_bytes([request[18], request[19]]) as usize;
        assert_eq!(image.len(), y * width * 4, "strips in order");
        image.extend(&request[24..24 + rows * width * 4]);
    }
    image
}

const WINDOW_PROGRAM: &str = r#"use gui

match gui.open("fake", 40, 30):
    case ok(w):
        print(w.size())
        done = false
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                if e == gui.event.close:
                    done = true
        print(w.size())
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        c.rect(5, 5, 10, 10, gui.rgb(250, 128, 7))
        w.show(c)
        w.set_title("done")
        w.close()
        es = w.events()
        print(w.is_open(), es)
    case err(reason):
        print(reason)
"#;

/// Waits until the client of each connection the fake X server has seen has closed it, and
/// returns them.
fn closed_connections(
    connections: &std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<FakeConnection>>>>,
) -> Vec<FakeConnection> {
    for _ in 0..500 {
        let all = connections.lock().unwrap();
        if all.iter().all(|c| c.lock().unwrap().closed) {
            return all
                .iter()
                .map(|c| std::mem::take(&mut *c.lock().unwrap()))
                .collect();
        }
        drop(all);
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    panic!("a client never closed its connection");
}

#[test]
fn both_backends_drive_an_x11_window_alike() {
    let expected = "(40, 30)\n\
                    event.redraw\n\
                    event.resize(400, 300)\n\
                    event.key_down('a')\n\
                    event.text('a')\n\
                    event.text('a')\n\
                    event.key_up('a')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_down('escape')\n\
                    event.key_down('a')\n\
                    event.key_down('1')\n\
                    event.text('1')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_down('shift')\n\
                    event.key_down('\u{3b1}')\n\
                    event.text('\u{3b1}')\n\
                    event.key_down('\u{444}')\n\
                    event.text('\u{444}')\n\
                    event.key_down('\u{444}')\n\
                    event.text('\u{424}')\n\
                    event.mouse_down('left', 10, 20)\n\
                    event.mouse_up('right', 11, -2)\n\
                    event.scroll(1)\n\
                    event.scroll(-1)\n\
                    event.mouse_move(5, 6)\n\
                    event.redraw\n\
                    event.close\n\
                    (400, 300)\n\
                    false []\n";
    for msb in [false, true] {
        let name = format!("gui_x11_{}", if msb { "msb" } else { "lsb" });
        let display = FakeDisplay {
            msb,
            depth: 24,
            bits: 32,
            // small, so the image goes in strips of a row each
            max_request: 64,
            refuse: None,
            script: FakeScript::All,
            short: false,
            shm: FakeShm::Absent,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            &name,
            WINDOW_PROGRAM,
            |dir| {
                connections = Some(fake_x_server(&dir.join("X0"), display));
                // an authority file with a cookie for another display, then one for this one
                let mut file = vec![];
                for (number, cookie) in [("1", &b"wrong"[..]), ("0", &b"secret_cookie"[..])] {
                    file.extend([1, 0, 0, 4]);
                    file.extend(b"host");
                    for field in [number.as_bytes(), b"MIT-MAGIC-COOKIE-1", cookie] {
                        file.extend((field.len() as u16).to_be_bytes());
                        file.extend(field);
                    }
                }
                std::fs::write(dir.join("Xauthority"), file).unwrap();
            },
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "Xauthority"),
            ],
        );
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
        }
        let seen = closed_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_eq!(interpreted.requests, compiled.requests);
        assert_eq!(
            interpreted.auth,
            (b"MIT-MAGIC-COOKIE-1".to_vec(), b"secret_cookie".to_vec())
        );
        // one strip of a row per request, and the colors in the server's byte order
        let requests = &interpreted.requests;
        assert_eq!(requests.iter().filter(|r| r[0] == 72).count(), 30);
        let image = put_images(requests, 40);
        assert_eq!(image.len(), 40 * 30 * 4);
        let pixel = |x: usize, y: usize| &image[(y * 40 + x) * 4..(y * 40 + x) * 4 + 4];
        let (background, square): (&[u8], &[u8]) = match msb {
            false => (&[3, 2, 1, 0], &[7, 128, 250, 0]),
            true => (&[0, 1, 2, 3], &[0, 250, 128, 7]),
        };
        assert_eq!(pixel(0, 0), background);
        assert_eq!(pixel(5, 5), square);
        assert_eq!(pixel(15, 15), background);
        // the last title, WM_NAME, holds the text alone
        let title = requests
            .iter()
            .rfind(|r| r[0] == 18 && r[8..12] == 39u32.to_le_bytes())
            .unwrap();
        assert_eq!(u32::from_le_bytes(title[20..24].try_into().unwrap()), 4);
        assert_eq!(&title[24..28], b"done");
    }
}

#[test]
fn both_backends_say_why_a_window_cannot_open() {
    let program = "use gui\n\
                   for size in [(10, 10), (0, 10), (40000, 1)]:\n    \
                   match gui.open(\"x\", size[0], size[1]):\n        \
                   case ok(_):\n            \
                   print(\"opened\")\n        \
                   case err(reason):\n            \
                   print(reason)\n";
    let refusing = FakeDisplay {
        msb: false,
        depth: 24,
        bits: 32,
        max_request: 65535,
        refuse: Some("No protocol specified"),
        script: FakeScript::All,
        short: false,
        shm: FakeShm::Absent,
    };
    let shallow = FakeDisplay {
        depth: 16,
        bits: 16,
        refuse: None,
        ..refusing
    };
    let short = FakeDisplay {
        refuse: None,
        short: true,
        ..refusing
    };
    let base = env!("CARGO_TARGET_TMPDIR");
    let cases = [
        (
            "gui_no_display",
            String::new(),
            None,
            "no display: neither WAYLAND_DISPLAY nor DISPLAY is set".to_string(),
        ),
        (
            "gui_bad_display",
            "nowhere".to_string(),
            None,
            "invalid display: 'nowhere'".to_string(),
        ),
        (
            "gui_bad_number",
            ":x".to_string(),
            None,
            "invalid display: ':x'".to_string(),
        ),
        (
            "gui_no_socket",
            format!("{base}/gui_no_socket/X0:0"),
            None,
            format!("no such file or directory: '{base}/gui_no_socket/X0'"),
        ),
        (
            "gui_refused",
            format!("{base}/gui_refused/X0:0"),
            Some(refusing),
            "the X server refused the connection: No protocol specified".to_string(),
        ),
        (
            "gui_shallow",
            format!("{base}/gui_shallow/X0:0"),
            Some(shallow),
            "unsupported display format: depth 16, 16 bits per pixel".to_string(),
        ),
        (
            "gui_short",
            format!("{base}/gui_short/X0:0"),
            Some(short),
            "the X server sent a setup reply that is cut short".to_string(),
        ),
    ];
    for (name, display, server, first) in cases {
        let (_, [run, built]) = run_and_build_env(
            name,
            program,
            |dir| {
                if let Some(server) = server {
                    fake_x_server(&dir.join("X0"), server);
                }
            },
            &[
                ("DISPLAY", &display),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        let expected =
            format!("{first}\ninvalid window size: 0x10\ninvalid window size: 40000x1\n");
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
    }
}

const APP_PROGRAM: &str = r#"use gui

struct typist:
    typed: str
    ticks: int
    size: tuple[int, int]

impl gui.app for typist:
    fn update(self, e: gui.event):
        match e:
            case gui.event.text(t):
                self.typed += t
            case gui.event.tick(seconds):
                if seconds >= 0.0:
                    self.ticks += 1
            case gui.event.resize(width, height):
                self.size = (width, height)
            case gui.event.close:
                print("close")
            case _:
                self.ticks += 0

    fn draw(self, c: gui.canvas) -> gui.canvas:
        c.fill(gui.rgb(1, 2, 3))
        c.text(0, 0, self.typed, gui.rgb(255, 255, 255))
        ret c

    // ten seconds of turns at most, so a broken run fails rather than waits forever
    fn done(self) -> bool:
        ret DONE or self.ticks > 600

match gui.run("app", 40, 30, typist(typed="", ticks=0, size=(40, 30))):
    case ok(state):
        print(state.typed, state.ticks > 0, state.size)
    case err(reason):
        print(reason)
"#;

#[test]
fn both_backends_run_an_app_alike() {
    // the server sends a new size and `a` after the first frame, `A` after the first of the new
    // size, so after `draw` has a canvas of that size, and the close button after the next
    let cases = [
        // done once `A` is typed, before the frame that would show it
        (
            "gui_app_done",
            "self.typed.endswith(\"A\")",
            "aA true (400, 300)\n",
        ),
        ("gui_app_closed", "false", "close\naA true (400, 300)\n"),
    ];
    for (name, done, expected) in cases {
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Paced,
            short: false,
            shm: FakeShm::Absent,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let (_, [run, built]) = run_and_build_env(
            name,
            &APP_PROGRAM.replace("DONE", done),
            |dir| {
                fake_x_server(&dir.join("X0"), display);
            },
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
    }
}

/// Starts `Xvfb` on a free display, if it is on PATH, returning it and the display's number.
fn start_xvfb() -> Option<(std::process::Child, String)> {
    use std::io::BufRead;
    let mut server = Command::new("Xvfb")
        .args([
            "-displayfd",
            "1",
            "-nolisten",
            "tcp",
            "-screen",
            "0",
            "320x240x24",
        ])
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;
    // it writes the display's number once it is ready
    let mut number = String::new();
    std::io::BufReader::new(server.stdout.take()?)
        .read_line(&mut number)
        .ok()?;
    match number.trim() {
        "" => {
            let _ = server.kill();
            None
        }
        n => Some((server, n.to_string())),
    }
}

#[test]
fn both_backends_draw_on_a_real_x_server() {
    let Some((mut server, number)) = start_xvfb() else {
        eprintln!("skipping: Xvfb is not on PATH");
        return;
    };
    let source = r#"use gui
use time

match gui.open("real", 64, 48):
    case ok(w):
        c = gui.blank(64, 48, gui.rgb(10, 20, 30))
        c.text(2, 2, "hi", gui.rgb(255, 255, 255))
        w.show(c)
        w.set_title("still real")
        // errors come back on their own time
        time.sleep(0.3)
        es = w.events()
        failures = 0
        for e in es:
            match e:
                case gui.event.failed(reason):
                    print(reason)
                    failures += 1
                case _:
                    cont
        print(w.is_open(), failures)
        w.close()
    case err(reason):
        print(reason)
"#;
    let display = format!(":{number}");
    let (_, [run, built]) = run_and_build_env(
        "gui_xvfb",
        source,
        |_| {},
        &[
            ("DISPLAY", &display),
            ("STONE_GUI", "x11"),
            ("XAUTHORITY", "missing"),
        ],
    );
    let _ = server.kill();
    let _ = server.wait();
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "true 0\n");
    }
}

#[test]
#[cfg(target_os = "linux")]
fn both_backends_show_images_through_shared_memory_alike() {
    let source = r#"use gui

match gui.open("shm", 40, 30):
    case ok(w):
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        c.rect(5, 5, 10, 10, gui.rgb(250, 128, 7))
        w.show(c)
        // drawn while the server may still be showing the first
        c.rect(0, 0, 2, 2, gui.rgb(255, 255, 255))
        w.show(c)
        // bigger than the memory shared so far
        w.show(gui.blank(60, 50, gui.rgb(9, 9, 9)))
        es = w.events()
        print(es)
        w.close()
    case err(reason):
        print(reason)
"#;
    for shm in [FakeShm::Working, FakeShm::Refusing] {
        let name = match shm {
            FakeShm::Working => "gui_shm",
            _ => "gui_shm_refused",
        };
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Expose,
            short: false,
            shm,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            name,
            source,
            |dir| connections = Some(fake_x_server(&dir.join("X0"), display)),
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                "[event.redraw]\n",
                "{name}"
            );
        }
        let seen = closed_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_eq!(interpreted.requests, compiled.requests, "{name}");
        assert_eq!(interpreted.shm_images, compiled.shm_images, "{name}");
        let count = |op: u8, minor: u8| {
            let requests = &interpreted.requests;
            requests
                .iter()
                .filter(|r| r[0] == op && r[1] == minor)
                .count()
        };
        let put_images = interpreted.requests.iter().filter(|r| r[0] == 72).count();
        let images = &interpreted.shm_images;
        match shm {
            FakeShm::Working => {
                // attached for the first image, then again, bigger, for the last
                assert_eq!((count(130, 6), count(130, 2), count(130, 3)), (2, 1, 3));
                assert_eq!(put_images, 0);
                let pixel = |image: &[u8], x: usize, y: usize| {
                    image[(y * 40 + x) * 4..(y * 40 + x) * 4 + 4].to_vec()
                };
                assert_eq!(images.len(), 3);
                assert_eq!(pixel(&images[0], 0, 0), [3, 2, 1, 0]);
                assert_eq!(pixel(&images[0], 5, 5), [7, 128, 250, 0]);
                assert_eq!(pixel(&images[1], 0, 0), [255, 255, 255, 0]);
                assert_eq!(images[2], [9, 9, 9, 0].repeat(60 * 50));
            }
            _ => {
                // the first attachment fails, and every image goes the plain way
                assert_eq!((count(130, 6), count(130, 3)), (1, 0));
                assert_eq!(put_images, 3);
                assert!(images.is_empty());
            }
        }
    }
}

/// How a fake Wayland compositor of [`fake_wayland_server`] behaves.
#[derive(Clone, Copy)]
struct FakeCompositor {
    /// Whether it offers `zxdg_decoration_manager_v1`, and so draws windows' frames itself.
    decorations: bool,
    /// What it sends after the app's first frame.
    script: WaylandScript,
    /// The keymap in `tests/keymaps` it sends the keyboard, and its repeats a second.
    keymap: &'static str,
    rate: u32,
}

/// What a fake compositor sends after the app's first frame.
#[derive(Clone, Copy, PartialEq)]
enum WaylandScript {
    /// A ping and a new size, then the close button after a frame of that size.
    Resize,
    /// Keys, the pointer on the content, then presses on our frame's edge, title, and close
    /// button.
    Input,
    /// A key held down.
    Hold,
}

/// Sends `data` on `stream` with the descriptor of `file`, as a compositor sends a keymap.
fn send_with_fd(
    stream: &std::os::unix::net::UnixStream,
    data: &[u8],
    file: &std::fs::File,
) -> std::io::Result<()> {
    use std::os::fd::AsRawFd;
    unsafe extern "C" {
        fn sendmsg(fd: i32, msg: *const MsgHdr, flags: i32) -> isize;
    }
    // a cmsghdr of 20 bytes, SOL_SOCKET, and SCM_RIGHTS, then the descriptor
    let mut control = [20u64, 1 | 1 << 32, file.as_raw_fd() as u64];
    let mut iov = IoVec {
        base: data.as_ptr().cast_mut(),
        len: data.len(),
    };
    let msg = MsgHdr {
        name: std::ptr::null_mut(),
        namelen: 0,
        iov: &mut iov,
        iovlen: 1,
        control: control.as_mut_ptr().cast(),
        controllen: 24,
        flags: 0,
    };
    // SAFETY: msg points at one iovec over `data` and 24 bytes of control, which outlive it
    match unsafe { sendmsg(stream.as_raw_fd(), &msg, 0) } {
        n if n as usize == data.len() => Ok(()),
        _ => Err(std::io::Error::last_os_error()),
    }
}

/// The 32-bit word of the fixed-point (24.8) number `value`.
fn fixed(value: f64) -> u32 {
    (value * 256.0) as i32 as u32
}

/// What the fake compositor saw on one connection: every request, as its bytes, and every
/// buffer committed, by the role of its surface (`frame`, `content`, or `cursor`), with its
/// width, height, and pixels (4 bytes each by row).
#[derive(Default, Debug)]
struct WaylandConnection {
    requests: Vec<Vec<u8>>,
    commits: Vec<(String, u32, u32, Vec<u8>)>,
    /// Whether the client has closed the connection.
    closed: bool,
}

/// The globals the fake compositor offers: name, interface, and version.
fn fake_globals(compositor: FakeCompositor) -> Vec<(u32, &'static str, u32)> {
    let mut globals = vec![
        (1, "wl_compositor", 5),
        (2, "wl_subcompositor", 1),
        (3, "wl_shm", 1),
        (4, "xdg_wm_base", 2),
        (5, "wl_seat", 8),
        (6, "wl_output", 3),
    ];
    if compositor.decorations {
        globals.push((7, "zxdg_decoration_manager_v1", 1));
    }
    globals
}

/// A Wayland message to `object` of `opcode` with the 32-bit arguments `args`.
fn wayland_message(object: u32, opcode: u16, args: &[u32]) -> Vec<u8> {
    let mut out = object.to_le_bytes().to_vec();
    out.extend((((8 + 4 * args.len()) as u32) << 16 | opcode as u32).to_le_bytes());
    for arg in args {
        out.extend(arg.to_le_bytes());
    }
    out
}

/// The 32-bit words of a Wayland string argument holding `text`: its length with a 0 after it,
/// then its bytes, padded to a multiple of four.
fn wayland_string(text: &str) -> Vec<u32> {
    let mut bytes = text.as_bytes().to_vec();
    bytes.push(0);
    let mut words = vec![bytes.len() as u32];
    bytes.resize(bytes.len().div_ceil(4) * 4, 0);
    words.extend(
        bytes
            .chunks(4)
            .map(|c| u32::from_le_bytes(c.try_into().unwrap())),
    );
    words
}

/// What the fake compositor knows of one connection's objects.
#[derive(Default)]
struct FakeObjects {
    /// The interface of each object, by id.
    kinds: std::collections::HashMap<u32, String>,
    /// Each pool's file, by id, and each buffer's pool, offset, width, height, and stride.
    pools: std::collections::HashMap<u32, std::fs::File>,
    buffers: std::collections::HashMap<u32, (u32, u32, u32, u32, u32)>,
    /// The role of each surface, the buffer attached to it, and the one it last committed.
    roles: std::collections::HashMap<u32, String>,
    attached: std::collections::HashMap<u32, u32>,
    shown: std::collections::HashMap<u32, u32>,
    toplevel: u32,
    xdg_surface: u32,
    decoration: u32,
    pointer: u32,
    keyboard: u32,
    /// How many content buffers were committed, and how far the script has gone.
    content_commits: usize,
    stage: usize,
}

/// Answers one request of a fake compositor, `message` to `object` of `opcode`, recording what
/// it shows in `seen`.
fn fake_wayland_request(
    stream: &mut std::os::unix::net::UnixStream,
    compositor: FakeCompositor,
    message: &[u8],
    fds: &mut Vec<std::fs::File>,
    objects: &mut FakeObjects,
    seen: &std::sync::Mutex<WaylandConnection>,
) -> std::io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::FileExt;
    let object = u32::from_le_bytes(message[0..4].try_into().unwrap());
    let opcode = u16::from_le_bytes([message[4], message[5]]);
    let arg = |i: usize| u32::from_le_bytes(message[8 + 4 * i..12 + 4 * i].try_into().unwrap());
    let kind = objects.kinds.get(&object).cloned().unwrap_or_default();
    let new = |objects: &mut FakeObjects, id: u32, kind: &str| {
        objects.kinds.insert(id, kind.to_string());
    };
    match (kind.as_str(), opcode) {
        ("wl_display", 0) => {
            // sync: done at once
            stream.write_all(&wayland_message(arg(0), 0, &[1]))?;
            stream.write_all(&wayland_message(1, 1, &[arg(0)]))?;
        }
        ("wl_display", 1) => {
            new(objects, arg(0), "wl_registry");
            for (name, interface, version) in fake_globals(compositor) {
                let mut args = vec![name];
                args.extend(wayland_string(interface));
                args.push(version);
                stream.write_all(&wayland_message(arg(0), 0, &args))?;
            }
        }
        ("wl_registry", 0) => {
            // bind(name, interface, version, id)
            let len = arg(1) as usize;
            let interface = std::str::from_utf8(&message[16..16 + len - 1])
                .unwrap()
                .to_string();
            let after = 3 + len.div_ceil(4);
            let id = arg(after);
            new(objects, id, &interface);
            if interface == "wl_seat" {
                // a pointer and a keyboard
                stream.write_all(&wayland_message(id, 0, &[3]))?;
            }
        }
        ("wl_compositor", 0) => new(objects, arg(0), "wl_surface"),
        ("wl_subcompositor", 1) => {
            new(objects, arg(0), "wl_subsurface");
            objects.roles.insert(arg(1), "content".to_string());
        }
        ("wl_shm", 0) => {
            new(objects, arg(0), "wl_shm_pool");
            objects.pools.insert(arg(0), fds.remove(0));
        }
        ("wl_shm_pool", 0) => {
            new(objects, arg(0), "wl_buffer");
            objects
                .buffers
                .insert(arg(0), (object, arg(1), arg(2), arg(3), arg(4)));
        }
        ("xdg_wm_base", 2) => {
            new(objects, arg(0), "xdg_surface");
            objects.xdg_surface = arg(0);
            objects
                .roles
                .entry(arg(1))
                .or_insert_with(|| "frame".to_string());
        }
        ("xdg_surface", 1) => {
            new(objects, arg(0), "xdg_toplevel");
            objects.toplevel = arg(0);
        }
        ("zxdg_decoration_manager_v1", 1) => {
            new(objects, arg(0), "zxdg_toplevel_decoration_v1");
            objects.decoration = arg(0);
        }
        ("wl_seat", 0) => {
            new(objects, arg(0), "wl_pointer");
            objects.pointer = arg(0);
        }
        ("wl_seat", 1) => {
            new(objects, arg(0), "wl_keyboard");
            objects.keyboard = arg(0);
            // the keymap, with a 0 after it as xkbcommon writes it, and the repeat rate
            let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/keymaps");
            let mut text = std::fs::read(path.join(compositor.keymap))?;
            text.push(0);
            let file = std::env::temp_dir().join(format!(
                "stone-keymap-{}-{}",
                std::process::id(),
                arg(0)
            ));
            std::fs::write(&file, &text)?;
            let opened = std::fs::File::open(&file)?;
            std::fs::remove_file(&file)?;
            send_with_fd(
                stream,
                &wayland_message(arg(0), 0, &[1, text.len() as u32]),
                &opened,
            )?;
            stream.write_all(&wayland_message(arg(0), 5, &[compositor.rate, 100]))?;
        }
        ("wl_pointer", 0) => {
            objects.roles.insert(arg(1), "cursor".to_string());
        }
        ("wl_surface", 1) => {
            objects.attached.insert(object, arg(0));
        }
        ("wl_surface", 6) => {
            let role = objects.roles.get(&object).cloned().unwrap_or_default();
            match objects.attached.remove(&object) {
                Some(buffer) if buffer != 0 => {
                    let (pool, offset, width, height, stride) = objects.buffers[&buffer];
                    let mut pixels = vec![];
                    for row in 0..height {
                        let mut line = vec![0; width as usize * 4];
                        let at = (offset + row * stride) as u64;
                        objects.pools[&pool].read_exact_at(&mut line, at)?;
                        pixels.extend(line);
                    }
                    seen.lock()
                        .unwrap()
                        .commits
                        .push((role.clone(), width, height, pixels));
                    let old = objects.shown.insert(object, buffer);
                    if let Some(old) = old.filter(|old| *old != buffer) {
                        stream.write_all(&wayland_message(old, 0, &[]))?;
                    }
                    if role == "content" || (role == "frame" && compositor.decorations) {
                        objects.content_commits += 1;
                        match compositor.script {
                            WaylandScript::Resize => fake_wayland_script(stream, objects, width)?,
                            script if objects.content_commits == 2 => {
                                stream.write_all(&fake_input(objects, script))?;
                            }
                            _ => {}
                        }
                    }
                }
                _ if role == "frame" && objects.stage == 0 => {
                    // the first commit, with no buffer: the first configure
                    objects.stage = 1;
                    if objects.decoration != 0 {
                        // server_side
                        stream.write_all(&wayland_message(objects.decoration, 0, &[2]))?;
                    }
                    // no size, and the activated state (an array of 4 bytes)
                    stream.write_all(&wayland_message(objects.toplevel, 0, &[0, 0, 4, 4]))?;
                    stream.write_all(&wayland_message(objects.xdg_surface, 0, &[10]))?;
                }
                _ => {}
            }
        }
        _ => {}
    }
    Ok(())
}

/// Moves the fake compositor's script on after a content buffer `width` pixels wide was
/// committed: after the app's first frame (the second, after the one `open` shows), a ping and a
/// new size of 300 by 200; after a frame of that width, the close button.
fn fake_wayland_script(
    stream: &mut std::os::unix::net::UnixStream,
    objects: &mut FakeObjects,
    width: u32,
) -> std::io::Result<()> {
    use std::io::Write;
    if objects.stage == 1 && objects.content_commits == 2 {
        objects.stage = 2;
        let mut out = wayland_message(objects.xdg_wm_base_id(), 0, &[77]);
        out.extend(wayland_message(objects.toplevel, 0, &[300, 200, 4, 4]));
        out.extend(wayland_message(objects.xdg_surface, 0, &[11]));
        stream.write_all(&out)?;
    } else if objects.stage == 2 && width == 300 {
        objects.stage = 3;
        stream.write_all(&wayland_message(objects.toplevel, 1, &[]))?;
    }
    Ok(())
}

/// The events of the script `script` (`Input` or `Hold`) for the window `objects` knows, its
/// content 40 by 30 in our frame.
fn fake_input(objects: &FakeObjects, script: WaylandScript) -> Vec<u8> {
    let (keyboard, pointer) = (objects.keyboard, objects.pointer);
    let surface_of = |role: &str| {
        objects
            .roles
            .iter()
            .find(|(_, r)| *r == role)
            .map_or(0, |(id, _)| *id)
    };
    let (content, frame) = (surface_of("content"), surface_of("frame"));
    let key = |code: u32, down: bool| wayland_message(keyboard, 3, &[9, 0, code, down as u32]);
    let mods =
        |depressed: u32, locked: u32| wayland_message(keyboard, 4, &[9, depressed, 0, locked, 0]);
    let mut out = wayland_message(keyboard, 1, &[9, frame, 0]);
    if script == WaylandScript::Hold {
        // `a` held down
        out.extend(key(30, true));
        return out;
    }
    // evdev codes: 30 a, 42 left Shift, 39 the key right of l, 44 the key right of left Shift,
    // and 1 Escape
    for part in [
        mods(0, 0),
        key(30, true),
        key(30, false),
        key(42, true),
        mods(1, 0),
        key(30, true),
        key(30, false),
        mods(0, 0),
        key(42, false),
        mods(0, 2),
        key(39, true),
        key(44, true),
        mods(0, 0),
        key(1, true),
        mods(4, 0),
        key(30, true),
        mods(0, 0),
    ] {
        out.extend(part);
    }
    // the pointer on the content: a move, a press and a release, a wheel step with its value,
    // and two steps' worth of a value alone
    for part in [
        wayland_message(pointer, 0, &[10, content, fixed(5.0), fixed(6.0)]),
        wayland_message(pointer, 2, &[0, fixed(10.5), fixed(20.25)]),
        wayland_message(pointer, 3, &[11, 0, 272, 1]),
        wayland_message(pointer, 3, &[12, 0, 273, 0]),
        wayland_message(pointer, 8, &[0, 1]),
        wayland_message(pointer, 4, &[0, 0, fixed(10.0)]),
        wayland_message(pointer, 5, &[]),
        wayland_message(pointer, 4, &[0, 0, fixed(-20.0)]),
        wayland_message(pointer, 5, &[]),
        wayland_message(pointer, 1, &[13, content]),
    ] {
        out.extend(part);
    }
    // our frame, 48 by 62: a press on its top left corner, then on the title bar, then on the
    // close button
    for (x, y, serial) in [(1.0, 1.0, 21), (10.0, 10.0, 22), (40.0, 10.0, 23)] {
        out.extend(wayland_message(
            pointer,
            0,
            &[serial, frame, fixed(x), fixed(y)],
        ));
        out.extend(wayland_message(pointer, 3, &[serial, 0, 272, 1]));
        out.extend(wayland_message(pointer, 3, &[serial, 0, 272, 0]));
        out.extend(wayland_message(pointer, 1, &[serial, frame]));
    }
    out
}

impl FakeObjects {
    /// The id the client bound `xdg_wm_base` to.
    fn xdg_wm_base_id(&self) -> u32 {
        self.kinds
            .iter()
            .find(|(_, kind)| *kind == "xdg_wm_base")
            .map_or(0, |(id, _)| *id)
    }
}

/// Starts a fake Wayland compositor behaving as `compositor`, listening on the Unix socket
/// `path`, and returns what it sees on each connection, in order.
fn fake_wayland_server(
    path: &Path,
    compositor: FakeCompositor,
) -> std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<WaylandConnection>>>>> {
    use std::sync::{Arc, Mutex};
    let _ = std::fs::remove_file(path);
    let listener = std::os::unix::net::UnixListener::bind(path).unwrap();
    let connections: Arc<Mutex<Vec<Arc<Mutex<WaylandConnection>>>>> = Arc::default();
    let all = connections.clone();
    std::thread::spawn(move || {
        for mut stream in listener.incoming().flatten() {
            let seen = Arc::new(Mutex::new(WaylandConnection::default()));
            all.lock().unwrap().push(seen.clone());
            std::thread::spawn(move || {
                let mut objects = FakeObjects::default();
                objects.kinds.insert(1, "wl_display".to_string());
                let mut fds = vec![];
                loop {
                    let mut head = [0; 8];
                    if receive(&mut stream, &mut head, &mut fds).is_err() {
                        break;
                    }
                    let size = u16::from_le_bytes([head[6], head[7]]) as usize;
                    let mut message = head.to_vec();
                    message.resize(size.max(8), 0);
                    if receive(&mut stream, &mut message[8..], &mut fds).is_err() {
                        break;
                    }
                    seen.lock().unwrap().requests.push(message.clone());
                    let answered = fake_wayland_request(
                        &mut stream,
                        compositor,
                        &message,
                        &mut fds,
                        &mut objects,
                        &seen,
                    );
                    if answered.is_err() {
                        break;
                    }
                }
                seen.lock().unwrap().closed = true;
            });
        }
    });
    connections
}

/// Waits until the client of each connection the fake compositor has seen has closed it, and
/// returns them.
fn closed_wayland_connections(
    connections: &std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<WaylandConnection>>>>,
) -> Vec<WaylandConnection> {
    for _ in 0..500 {
        let all = connections.lock().unwrap();
        if all.iter().all(|c| c.lock().unwrap().closed) {
            return all
                .iter()
                .map(|c| std::mem::take(&mut *c.lock().unwrap()))
                .collect();
        }
        drop(all);
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    panic!("a client never closed its connection");
}

const WAYLAND_PROGRAM: &str = r#"use gui

match gui.open("way", 40, 30):
    case ok(w):
        print(w.size())
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        done = false
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                match e:
                    case gui.event.redraw:
                        w.show(c)
                    case gui.event.resize(width, height):
                        c = gui.blank(width, height, gui.rgb(4, 5, 6))
                        w.show(c)
                    case gui.event.close:
                        done = true
                    case _:
                        cont
        w.close()
    case err(reason):
        print(reason)
"#;

#[test]
#[cfg(target_os = "linux")]
fn both_backends_drive_a_wayland_window_alike() {
    for decorations in [false, true] {
        let name = match decorations {
            false => "gui_wayland_ours",
            true => "gui_wayland_server",
        };
        let compositor = FakeCompositor {
            decorations,
            script: WaylandScript::Resize,
            keymap: "us.xkb",
            rate: 0,
        };
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            name,
            WAYLAND_PROGRAM,
            |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
            &[
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", ""),
            ],
        );
        // with our own frame, the title bar is part of the 300 by 200 the compositor asked for
        let height = if decorations { 200 } else { 176 };
        let expected =
            format!("(40, 30)\nevent.redraw\nevent.resize(300, {height})\nevent.close\n");
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
        let seen = closed_wayland_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_eq!(interpreted.requests, compiled.requests, "{name}");
        assert_eq!(interpreted.commits, compiled.commits, "{name}");
        let content: Vec<_> = interpreted
            .commits
            .iter()
            .filter(|(role, ..)| *role == if decorations { "frame" } else { "content" })
            .collect();
        // black when it opens, then the app's two frames
        let sizes: Vec<_> = content.iter().map(|(_, w, h, _)| (*w, *h)).collect();
        assert_eq!(sizes, [(40, 30), (40, 30), (300, height)], "{name}");
        assert_eq!(content[0].3, [0, 0, 0, 0].repeat(40 * 30));
        assert_eq!(content[1].3, [3, 2, 1, 0].repeat(40 * 30));
        assert_eq!(content[2].3, [6, 5, 4, 0].repeat(300 * height as usize));
        let frames: Vec<_> = interpreted
            .commits
            .iter()
            .filter(|(role, ..)| *role == "frame")
            .collect();
        if !decorations {
            // our frame: a transparent border of 4 around a title bar of 24 and the content
            let (_, width, height, pixels) = frames.last().unwrap();
            assert_eq!((*width, *height), (308, 208));
            let pixel = |x: u32, y: u32| pixels[((y * width + x) * 4) as usize..][..4].to_vec();
            assert_eq!(pixel(0, 0)[3], 0);
            assert_eq!(pixel(10, 10)[3], 255);
        }
    }
}

#[test]
fn both_backends_choose_a_display_alike() {
    let program = "use gui\n\
                   match gui.open(\"x\", 10, 10):\n    \
                   case ok(w):\n        \
                   print(\"opened\", w.size())\n    \
                   case err(reason):\n        \
                   print(reason)\n";
    let base = env!("CARGO_TARGET_TMPDIR");
    let x_server = format!("{base}/gui_choice_fallback/X0:0");
    // each case's name, its environment, and what it prints
    type Case<'a> = (&'a str, Vec<(&'a str, &'a str)>, &'a str);
    let cases: [Case; 7] = [
        (
            "gui_choice_none",
            vec![("DISPLAY", ""), ("WAYLAND_DISPLAY", ""), ("STONE_GUI", "")],
            "no display: neither WAYLAND_DISPLAY nor DISPLAY is set",
        ),
        (
            "gui_choice_bad",
            vec![("STONE_GUI", "metal")],
            "STONE_GUI is 'metal', but can only be 'wayland' or 'x11'",
        ),
        (
            "gui_choice_wayland",
            vec![("STONE_GUI", "wayland"), ("WAYLAND_DISPLAY", "")],
            "no display: WAYLAND_DISPLAY is not set",
        ),
        (
            "gui_choice_x11",
            vec![("STONE_GUI", "x11"), ("DISPLAY", "")],
            "no display: DISPLAY is not set",
        ),
        (
            "gui_choice_no_runtime",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", ""),
                ("DISPLAY", ""),
            ],
            "no display: XDG_RUNTIME_DIR is not set",
        ),
        (
            "gui_choice_no_socket",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", ""),
            ],
            "no such file or directory: './wayland-9'",
        ),
        // a compositor that is not there, then the X server
        (
            "gui_choice_fallback",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", &x_server),
                ("XAUTHORITY", "missing"),
            ],
            "opened (10, 10)",
        ),
    ];
    for (name, env, expected) in cases {
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Expose,
            short: false,
            shm: FakeShm::Absent,
        };
        let (_, [run, built]) = run_and_build_env(
            name,
            program,
            |dir| {
                fake_x_server(&dir.join("X0"), display);
            },
            &env,
        );
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                format!("{expected}\n"),
                "{name}"
            );
        }
    }
}

const INPUT_PROGRAM: &str = r#"use gui

match gui.open("keys", 40, 30):
    case ok(w):
        done = false
        typed = 0
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                match e:
                    case gui.event.redraw:
                        w.show(gui.blank(40, 30, gui.rgb(1, 2, 3)))
                    case gui.event.text(_):
                        typed += 1
                        done = HOLD and typed == 4
                    case gui.event.close:
                        done = true
                    case _:
                        cont
        w.close()
    case err(reason):
        print(reason)
"#;

#[test]
#[cfg(target_os = "linux")]
fn both_backends_take_wayland_input_alike() {
    let expected = "event.redraw\n\
                    event.key_down('a')\n\
                    event.text('a')\n\
                    event.key_up('a')\n\
                    event.key_down('shift')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_up('a')\n\
                    event.key_up('shift')\n\
                    event.key_down('\u{f6}')\n\
                    event.text('\u{d6}')\n\
                    event.key_down('y')\n\
                    event.text('Y')\n\
                    event.key_down('escape')\n\
                    event.key_down('a')\n\
                    event.mouse_move(10, 20)\n\
                    event.mouse_down('left', 10, 20)\n\
                    event.mouse_up('right', 10, 20)\n\
                    event.scroll(-1)\n\
                    event.scroll(1)\n\
                    event.scroll(1)\n\
                    event.close\n";
    let compositor = FakeCompositor {
        decorations: false,
        script: WaylandScript::Input,
        keymap: "de.xkb",
        rate: 0,
    };
    let mut connections = None;
    let (_, [run, built]) = run_and_build_env(
        "gui_wayland_input",
        &INPUT_PROGRAM.replace("HOLD", "false"),
        |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
        &[
            ("WAYLAND_DISPLAY", "wayland-9"),
            ("XDG_RUNTIME_DIR", "."),
            ("STONE_GUI", ""),
        ],
    );
    for output in [&run, &built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
    let seen = closed_wayland_connections(&connections.unwrap());
    let [interpreted, compiled] = &seen[..] else {
        panic!("expected two connections, not {}", seen.len());
    };
    assert_eq!(interpreted.requests, compiled.requests);
    // xdg_toplevel's resize from the top left corner (5) and move, each with its press's serial
    let toplevel_requests: Vec<_> = interpreted
        .requests
        .iter()
        .filter(|r| r.len() >= 16 && (r[4] == 5 || r[4] == 6) && r[6] as usize == r.len())
        .map(|r| {
            r[8..]
                .chunks(4)
                .map(|w| u32::from_le_bytes(w.try_into().unwrap()))
                .collect::<Vec<_>>()
        })
        .filter(|args: &Vec<u32>| args.len() >= 2 && (args[1] == 21 || args[1] == 22))
        .collect();
    assert_eq!(toplevel_requests.len(), 2, "{toplevel_requests:?}");
    assert_eq!(toplevel_requests[0][1..], [21, 5]);
    assert_eq!(toplevel_requests[1][1..], [22]);
}

#[test]
#[cfg(target_os = "linux")]
fn a_key_held_down_on_wayland_repeats_in_both_backends() {
    let compositor = FakeCompositor {
        decorations: false,
        script: WaylandScript::Hold,
        keymap: "us.xkb",
        rate: 50,
    };
    let (_, [run, built]) = run_and_build_env(
        "gui_wayland_hold",
        &INPUT_PROGRAM.replace("HOLD", "true"),
        |dir| {
            fake_wayland_server(&dir.join("wayland-9"), compositor);
        },
        &[
            ("WAYLAND_DISPLAY", "wayland-9"),
            ("XDG_RUNTIME_DIR", "."),
            ("STONE_GUI", ""),
        ],
    );
    let expected = "event.redraw\nevent.key_down('a')\nevent.text('a')\nevent.text('a')\n\
                    event.text('a')\nevent.text('a')\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

/// Starts a headless Weston, if `weston` is on PATH, in a runtime directory of its own, returning
/// it, the directory, and its socket's name.
fn start_weston(name: &str) -> Option<(std::process::Child, PathBuf, String)> {
    use std::os::unix::fs::PermissionsExt;
    let runtime = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}-runtime"));
    let _ = std::fs::remove_dir_all(&runtime);
    std::fs::create_dir_all(&runtime).ok()?;
    std::fs::set_permissions(&runtime, std::fs::Permissions::from_mode(0o700)).ok()?;
    let socket = "stone-test".to_string();
    let mut server = Command::new("weston")
        .args(["--backend=headless", "--renderer=pixman", "--shell=kiosk"])
        .arg(format!("--socket={socket}"))
        .env("XDG_RUNTIME_DIR", &runtime)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;
    for _ in 0..100 {
        if runtime.join(&socket).exists() {
            return Some((server, runtime, socket));
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    let _ = server.kill();
    None
}

#[test]
#[cfg(target_os = "linux")]
fn both_backends_draw_on_a_real_wayland_compositor() {
    let Some((mut server, runtime, socket)) = start_weston("gui_weston") else {
        eprintln!("skipping: weston is not on PATH");
        return;
    };
    let source = r#"use gui
use time

match gui.open("real", 64, 48):
    case ok(w):
        c = gui.blank(64, 48, gui.rgb(10, 20, 30))
        c.text(2, 2, "hi", gui.rgb(255, 255, 255))
        w.show(c)
        w.set_title("still real")
        time.sleep(0.3)
        failures = 0
        for _ in range(3):
            es = w.events()
            for e in es:
                match e:
                    case gui.event.failed(reason):
                        print(reason)
                        failures += 1
                    case gui.event.resize(width, height):
                        c = gui.blank(width, height, gui.rgb(10, 20, 30))
                    case _:
                        cont
            w.show(c)
            time.sleep(0.1)
        print(w.is_open(), failures)
        w.close()
    case err(reason):
        print(reason)
"#;
    let runtime = runtime.to_string_lossy().to_string();
    let (_, [run, built]) = run_and_build_env(
        "gui_weston",
        source,
        |_| {},
        &[
            ("STONE_GUI", "wayland"),
            ("WAYLAND_DISPLAY", &socket),
            ("XDG_RUNTIME_DIR", &runtime),
        ],
    );
    let _ = server.kill();
    let _ = server.wait();
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "true 0\n");
    }
}
