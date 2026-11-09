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

#[cfg(unix)]
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

#[cfg(unix)]
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

/// Copies the stone binary to `<target tmp>/<dir>/stone` (`stone.exe` on Windows) so a test can uninstall it, returning
/// the copy's path.
///
/// For example, `installed_copy("uninstall_yes/bin")` returns `.../uninstall_yes/bin/stone`.
fn installed_copy(dir: &str) -> PathBuf {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(dir);
    std::fs::create_dir_all(&dir).unwrap();
    let copy = dir.join(format!("stone{}", std::env::consts::EXE_SUFFIX));
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
fn run_passes_the_arguments_after_dashes_to_the_program() {
    let source = "print(args())\n";
    let expected = "['a', '-b', '--c', 'd e', '--']\n";
    let after = ["--", "a", "-b", "--c", "d e", "--"];
    assert_eq!(stone_stdout(&["run"], "args_run", source, &after), expected);
    assert_eq!(stone_stdout(&[], "args_bare", source, &after), expected);
    assert_eq!(stone_stdout(&["run"], "args_none", source, &[]), "[]\n");
}

#[test]
fn run_refuses_arguments_without_dashes() {
    let (output, _) = stone(&["run"], "args_undashed", "print(args())\n");
    assert!(output.status.success(), "{output:?}");
    let file = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("args_undashed.stn");
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("run")
        .arg(&file)
        .arg("a")
        .output()
        .expect("stone should run");
    assert!(!output.status.success(), "{output:?}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("unexpected argument 'a'"), "{stderr}");
    let tip = format!(
        "tip: to give 'a' to the program, write it after '--', as in 'stone run {} -- a'",
        file.display()
    );
    assert!(stderr.contains(&tip), "{stderr}");
}

#[test]
fn init_starts_a_program_that_runs_builds_checks_and_tests() {
    let dir = project("init_program", &[]);
    std::fs::create_dir_all(&dir).unwrap();
    let output = stone_in(&dir, &["init", "my-app", "--no-git"]);
    assert!(output.status.success(), "{output:?}");
    let app = dir.join("my-app");
    let created = |file: &str| Path::new("my-app").join(file).display().to_string();
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        format!(
            "created {}\ncreated {}\ncreated {}\nmade the program my_app; run it with \
             `cd my-app` and `stone run`\n",
            created("stone.toml"),
            created("main.stn"),
            created(".gitignore")
        )
    );
    assert!(!app.join(".git").exists());

    let run = stone_in(&app, &["run"]);
    assert!(run.status.success(), "{run:?}");
    assert_eq!(String::from_utf8_lossy(&run.stdout), "Hello, world!\n");
    // from a directory inside the project, the same program
    std::fs::create_dir_all(app.join("notes/old")).unwrap();
    let nested = stone_in(&app.join("notes/old"), &["run"]);
    assert!(nested.status.success(), "{nested:?}");
    assert_eq!(String::from_utf8_lossy(&nested.stdout), "Hello, world!\n");
    // and a directory names its program
    let named = stone_in(&dir, &["run", "my-app"]);
    assert_eq!(String::from_utf8_lossy(&named.stdout), "Hello, world!\n");

    let check = stone_in(&app.join("notes"), &["check"]);
    assert!(check.status.success(), "{check:?}");
    assert_eq!(String::from_utf8_lossy(&check.stderr), "");
    let fmt = stone_in(&app, &["fmt", "--check"]);
    assert!(fmt.status.success(), "{fmt:?}");
    // tests run from anywhere inside the project, as everything else does
    for dir in [app.clone(), app.join("notes/old")] {
        let test = stone_in(&dir, &["test"]);
        assert!(test.status.success(), "{test:?}");
        assert!(String::from_utf8_lossy(&test.stdout).contains("1 passed, 0 failed"));
    }

    let build = stone_in(&app.join("notes"), &["build"]);
    assert!(build.status.success(), "{build:?}");
    assert_eq!(
        String::from_utf8_lossy(&build.stdout),
        format!(
            "Compiled {}{}\n",
            Path::new("..").join("build").join("my_app").display(),
            std::env::consts::EXE_SUFFIX
        )
    );
    assert_eq!(stdout_of(&app.join("build/my_app")), "Hello, world!\n");
    let clean = stone_in(&app.join("notes"), &["clean"]);
    assert!(clean.status.success(), "{clean:?}");
    assert!(!app.join("build").exists());

    std::fs::write(app.join("main.stn"), "print(args())\n").unwrap();
    let args = stone_in(&app, &["run", "--", "a", "-b"]);
    assert_eq!(String::from_utf8_lossy(&args.stdout), "['a', '-b']\n");
}

#[test]
fn init_starts_a_library_that_checks_but_does_not_run() {
    let dir = project("init_library", &[]);
    let output = stone_in(
        &PathBuf::from(env!("CARGO_TARGET_TMPDIR")),
        &[
            "init",
            "init_library",
            "--lib",
            "--name",
            "greet",
            "--no-git",
        ],
    );
    assert!(output.status.success(), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stdout)
            .ends_with("made the library greet; test it with `cd init_library` and `stone test`\n")
    );
    let manifest = std::fs::read_to_string(dir.join("stone.toml")).unwrap();
    assert!(manifest.contains("name = \"greet\""), "{manifest}");
    let check = stone_in(&dir, &["check"]);
    assert!(check.status.success(), "{check:?}");
    assert_eq!(String::from_utf8_lossy(&check.stderr), "");
    let run = stone_in(&dir, &["run"]);
    assert!(!run.status.success(), "{run:?}");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "error: this directory is a library, with a lib.stn but no main.stn, so there is nothing \
         to run; check it with `stone check` or test it with `stone test`\n"
    );
}

#[test]
fn init_leaves_a_project_alone() {
    let dir = project("init_existing", &[("main.stn", "print(1)\n")]);
    let output = stone_in(&dir, &["init"]);
    assert!(!output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "error: main.stn already exists, so stone init left this directory alone\n"
    );
    assert!(!dir.join("stone.toml").exists());
}

#[test]
fn init_makes_a_git_repository_outside_one() {
    if Command::new("git").arg("--version").output().is_err() {
        eprintln!("skipping: git is not on PATH");
        return;
    }
    let dir = project("init_git", &[]);
    std::fs::create_dir_all(&dir).unwrap();
    // the temporary directory is inside stone's own repository, which git must not find
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["init", "repo"])
        .current_dir(&dir)
        .env("GIT_CEILING_DIRECTORIES", &dir)
        .output()
        .expect("stone should run");
    assert!(output.status.success(), "{output:?}");
    assert!(dir.join("repo/.git").is_dir());
    assert!(String::from_utf8_lossy(&output.stdout).contains("created a git repository"));
}

#[test]
fn run_outside_a_program_says_how_to_start_one() {
    let dir = project("no_program", &[]);
    std::fs::create_dir_all(&dir).unwrap();
    // nothing above the temporary directory is a program either
    for command in ["run", "build", "check"] {
        let output = stone_in(&dir, &[command]);
        assert!(!output.status.success(), "{output:?}");
        assert_eq!(
            String::from_utf8_lossy(&output.stderr),
            "error: there is no stone program here, since neither this directory nor any above \
             it holds a main.stn or a stone.toml; run `stone init` to start one, or name a file\n"
        );
    }
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
        dir.join("geometry").join("vec.stn").display()
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
fn no_arguments_prints_usage() {
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .output()
        .expect("stone should run");
    assert!(!output.status.success(), "{output:?}");
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("Usage:"),
        "{output:?}"
    );
}

#[test]
fn build_rejects_an_unknown_target() {
    let (output, _) = stone(&["build", "--target", "sparc"], "sparc", "print(1)\n");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "unknown target 'sparc' (expected x86_64-linux, aarch64-linux, aarch64-macos, or \
             x86_64-windows, with an optional level such as x86_64v3-linux)"
        ),
        "{stderr}"
    );
}

#[test]
fn build_writes_ptx_for_each_gpu_beside_the_program() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let exe = dir.join("gpu_kernels");
    let ptx = dir.join("gpu_kernels.sm_75.ptx");
    let _ = std::fs::remove_file(&ptx);
    let exe_arg = exe.to_str().unwrap();
    let (output, _) = stone(
        &["build", "--gpu", "sm_75", "--gpu", "sm_90", "-o", exe_arg],
        "gpu_kernels",
        "a = [1.0, 2.0]\nb = [0.5, 0.25]\nprint(a * 2.0 + b)\nprint([1, 2] * 3)\n",
    );
    assert!(output.status.success(), "{output:?}");
    let text = std::fs::read_to_string(&ptx).unwrap();
    assert!(text.contains(".target sm_75\n"), "{text}");
    assert!(text.contains(".visible .entry stone_k0("), "{text}");
    assert!(text.contains(".visible .entry stone_k1("), "{text}");
    assert!(text.contains("\tmul.rn.f64 \t"), "{text}");
    let newer = std::fs::read_to_string(dir.join("gpu_kernels.sm_90.ptx")).unwrap();
    assert!(newer.contains(".version 7.8\n.target sm_90\n"), "{newer}");
    // nothing launches kernels yet, so the program computes as before
    if cfg!(target_os = "linux") {
        let ran = Command::new(&exe).output().unwrap();
        assert_eq!(
            String::from_utf8_lossy(&ran.stdout),
            "[2.5, 4.25]\n[3, 6]\n"
        );
    }
}

/// Returns whether NVIDIA's `ptxas` runs, which a GPU build needs to write machine code.
fn has_ptxas() -> bool {
    Command::new("ptxas")
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

#[test]
fn build_compiles_the_kernels_of_each_gpu_to_a_cubin() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let exe = dir.join("gpu_cubin");
    let cubin = dir.join("gpu_cubin.sm_75.cubin");
    let _ = std::fs::remove_file(&cubin);
    let (output, _) = stone(
        &["build", "--gpu", "sm_75", "-o", exe.to_str().unwrap()],
        "gpu_cubin",
        "a = [1.0, 2.0]\nb = [0.5, 0.25]\nprint(a * 2.0 + b)\n",
    );
    assert!(output.status.success(), "{output:?}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    if has_ptxas() {
        let bytes = std::fs::read(&cubin).unwrap();
        assert_eq!(&bytes[..4], b"\x7fELF");
        assert!(!stderr.contains("warning"), "{stderr}");
    } else {
        // the program still builds and computes on the processor, and says why not on the GPU
        assert!(!cubin.exists());
        assert!(
            stderr.contains("warning: cannot run 'ptxas' to compile the kernels for sm_75"),
            "{stderr}"
        );
    }
}

/// The program of the GPU image tests: `stone_k0` is `a * 2.0 + b` and `stone_k1` divides, as the
/// kernels of the fixture cubin `two_kernels.sm_75.cubin` are.
#[cfg(target_os = "linux")]
const TWO_KERNEL_PROGRAM: &str =
    "a = [1.0, 2.0]\nb = [0.5, 0.25]\nprint(a * 2.0 + b)\nprint(a / b)\n";

/// Returns the image a GPU build embedded in the executable at `exe`.
#[cfg(target_os = "linux")]
fn embedded_image(exe: &Path) -> stone::codegen::gpu::image::Image {
    use stone::codegen::gpu::image;
    let bytes = std::fs::read(exe).unwrap();
    let at = bytes
        .windows(image::MAGIC.len())
        .position(|window| window == image::MAGIC)
        .expect("the executable holds an image");
    image::parse(&bytes[at..]).unwrap()
}

// only Linux programs carry the image so far
#[cfg(target_os = "linux")]
#[test]
fn build_embeds_the_machine_code_of_each_gpu_in_the_program() {
    use std::os::unix::fs::PermissionsExt;
    use stone::codegen::gpu::cubin;
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    // a `ptxas` that gives the fixture's cubin, so no CUDA toolkit is needed
    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/codegen/gpu/testdata");
    let cubin_file = fixture.join("two_kernels.sm_75.cubin");
    let fake = dir.join("fake_ptxas_for_the_image");
    std::fs::write(
        &fake,
        format!(
            "#!/bin/sh\nwhile [ \"$1\" != -o ]; do shift; done\ncp '{}' \"$2\"\n",
            cubin_file.display()
        ),
    )
    .unwrap();
    std::fs::set_permissions(&fake, std::fs::Permissions::from_mode(0o755)).unwrap();
    let file = dir.join("gpu_image.stn");
    std::fs::write(&file, TWO_KERNEL_PROGRAM).unwrap();
    let exe = dir.join("gpu_image");
    let _ = std::fs::remove_file(&exe);
    let build = || {
        Command::new(env!("CARGO_BIN_EXE_stone"))
            .args(["build", "--gpu", "sm_90", "--gpu", "sm_75", "-o"])
            .arg(&exe)
            .arg(&file)
            .env("STONE_PTXAS", &fake)
            .output()
            .unwrap()
    };
    let mut output = build();
    // another test may have a fork holding the script open for writing
    for _ in 0..20 {
        if !String::from_utf8_lossy(&output.stderr).contains("Text file busy") {
            break;
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
        output = build();
    }
    assert!(output.status.success(), "{output:?}");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");

    let image = embedded_image(&exe);
    assert_eq!(image.record_kernel, [0, 1]);
    let devices: Vec<(u32, bool)> = image.devices.iter().map(|d| (d.sm, d.specific)).collect();
    assert_eq!(devices, [(75, false), (90, false)]);
    let kernels = cubin::parse(&std::fs::read(&cubin_file).unwrap()).unwrap();
    for device in &image.devices {
        for (index, kernel) in device.kernels.iter().enumerate() {
            let expected = kernels
                .iter()
                .find(|k| k.name == format!("stone_k{index}"))
                .unwrap();
            assert_eq!(kernel.sass, expected.sass);
            assert_eq!(kernel.registers, expected.registers);
            assert_eq!(kernel.param_size, expected.param_size);
        }
    }
    assert!(!image.shapes[0].fail_flag);
    assert!(image.shapes[1].fail_flag);

    if cfg!(target_os = "linux") {
        let ran = Command::new(&exe).output().unwrap();
        assert_eq!(
            String::from_utf8_lossy(&ran.stdout),
            "[2.5, 4.25]\n[2.0, 8.0]\n"
        );
    }
}

#[cfg(target_os = "linux")]
#[test]
fn a_gpu_build_without_ptxas_embeds_an_image_with_no_devices() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let file = dir.join("gpu_image_empty.stn");
    std::fs::write(&file, TWO_KERNEL_PROGRAM).unwrap();
    let exe = dir.join("gpu_image_empty");
    let _ = std::fs::remove_file(&exe);
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "--gpu", "sm_75", "-o"])
        .arg(&exe)
        .arg(&file)
        .env("STONE_PTXAS", "/nonexistent/ptxas")
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("warning: cannot run '/nonexistent/ptxas'"),
        "{stderr}"
    );
    let image = embedded_image(&exe);
    assert_eq!(image.record_kernel, [0, 1]);
    assert!(image.devices.is_empty());
    let ran = Command::new(&exe).output().unwrap();
    assert_eq!(
        String::from_utf8_lossy(&ran.stdout),
        "[2.5, 4.25]\n[2.0, 8.0]\n"
    );
}

#[cfg(target_os = "linux")]
#[test]
fn a_build_without_gpus_embeds_no_image() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let file = dir.join("no_gpu_image.stn");
    std::fs::write(&file, TWO_KERNEL_PROGRAM).unwrap();
    let exe = dir.join("no_gpu_image");
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "-o"])
        .arg(&exe)
        .arg(&file)
        .output()
        .unwrap();
    assert!(output.status.success(), "{output:?}");
    let bytes = std::fs::read(&exe).unwrap();
    assert!(
        !bytes
            .windows(8)
            .any(|window| window == stone::codegen::gpu::image::MAGIC)
    );
}

#[test]
fn build_rejects_a_gpu_it_cannot_generate_code_for() {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let exe = dir.join("gpu_amd");
    let _ = std::fs::remove_file(&exe);
    let (output, _) = stone(
        &["build", "--gpu", "gfx1100", "-o", exe.to_str().unwrap()],
        "gpu_amd",
        "print(1)\n",
    );
    assert!(!output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "error: AMD GPUs (gfx1100) are not supported yet\n"
    );
    assert!(!exe.exists());
    let (output, _) = stone(&["build", "--gpu", "cuda"], "gpu_unknown", "print(1)\n");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("unknown GPU 'cuda'"), "{stderr}");
}

#[test]
fn windows_gui_needs_a_windows_target() {
    let (output, _) = stone(
        &["build", "--windows-gui", "--target", "aarch64-linux"],
        "windows_gui_linux",
        "print(1)\n",
    );
    assert!(!output.status.success());
    assert_eq!(
        String::from_utf8_lossy(&output.stderr),
        "error: --windows-gui needs a Windows target, such as --target x86_64-windows\n"
    );
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR"));
    let exe = dir.join("windows_gui.exe");
    let (output, _) = stone(
        &[
            "build",
            "--windows-gui",
            "--target",
            "x86_64-windows",
            "-o",
            exe.to_str().unwrap(),
        ],
        "windows_gui",
        "print(1)\n",
    );
    assert!(output.status.success(), "{output:?}");
    // the subsystem, at offset 68 of the optional header, after the PE signature and COFF header
    let bytes = std::fs::read(&exe).unwrap();
    let pe = u32::from_le_bytes(bytes[60..64].try_into().unwrap()) as usize;
    assert_eq!(u16::from_le_bytes([bytes[pe + 92], bytes[pe + 93]]), 2);
}

/// Returns the `--target` name of the machine the tests run on.
///
/// For example, it is `x86_64-linux` on an x86-64 Linux machine.
fn host_target() -> &'static str {
    if cfg!(target_os = "macos") {
        "aarch64-macos"
    } else if cfg!(windows) {
        "x86_64-windows"
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

/// Returns whether this machine has every feature of x86-64 `level` (2 to 4) that the check at
/// the start of a program built for it reads.
#[cfg(target_arch = "x86_64")]
fn host_reaches(level: u8) -> bool {
    use std::arch::is_x86_feature_detected as has;
    let v2 = has!("sse3")
        && has!("ssse3")
        && has!("sse4.1")
        && has!("sse4.2")
        && has!("popcnt")
        && has!("cmpxchg16b");
    let v3 = v2
        && has!("avx")
        && has!("avx2")
        && has!("bmi1")
        && has!("bmi2")
        && has!("f16c")
        && has!("fma")
        && has!("lzcnt")
        && has!("movbe");
    let v4 = v3
        && has!("avx512f")
        && has!("avx512bw")
        && has!("avx512cd")
        && has!("avx512dq")
        && has!("avx512vl");
    match level {
        2 => v2,
        3 => v3,
        _ => v4,
    }
}

#[cfg(target_arch = "x86_64")]
#[test]
fn a_program_built_for_a_level_runs_only_on_a_processor_that_reaches_it() {
    let system = host_target().split_once('-').unwrap().1;
    for level in 2..=4 {
        let name = format!("level_v{level}");
        let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(&name);
        let target = format!("x86_64v{level}-{system}");
        let (output, _) = stone(
            &["build", "--target", &target, "-o", exe.to_str().unwrap()],
            &name,
            "print(6 * 7)\n",
        );
        assert!(output.status.success(), "{output:?}");
        let run = Command::new(&exe).output().expect("the binary should run");
        if host_reaches(level) {
            assert_eq!(String::from_utf8_lossy(&run.stdout), "42\n", "{target}");
            assert_eq!(run.status.code(), Some(0), "{target}");
        } else {
            // the check runs before anything else, so nothing is printed
            assert_eq!(run.stdout, b"", "{target}");
            assert_eq!(
                String::from_utf8_lossy(&run.stderr),
                format!("error: this program needs an x86-64 v{level} processor\n"),
                "{target}"
            );
            assert_eq!(run.status.code(), Some(1), "{target}");
        }
    }
}

/// Builds `packed_widths` for `aarch64v9-linux` and runs it under qemu-aarch64 with SVE vectors
/// of 128, 512, and 2048 bits, which must all print what the interpreter does, since the loop
/// takes as many elements at a time as the processor's vectors hold, and on a processor without
/// SVE, which must stop at the startup check.
#[cfg(target_os = "linux")]
#[test]
fn sve_code_runs_alike_at_every_vector_length() {
    let qemu = "qemu-aarch64";
    if Command::new(qemu).arg("--version").output().is_err() {
        eprintln!("skipped running SVE code at several vector lengths: {qemu} was not found");
        return;
    }
    let programs = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/programs");
    let exe = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("sve_packed_widths");
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["build", "--target", "aarch64v9-linux", "-o"])
        .arg(&exe)
        .arg(programs.join("packed_widths.stn"))
        .output()
        .expect("stone should run");
    assert!(output.status.success(), "{output:?}");
    let expected = std::fs::read_to_string(programs.join("packed_widths.out")).unwrap();
    for bytes in [16, 64, 256] {
        let cpu = format!("max,sve-default-vector-length={bytes}");
        let run = Command::new(qemu)
            .args(["-cpu", &cpu])
            .arg(&exe)
            .env("STONE_LEAK_CHECK", "1")
            .output()
            .unwrap();
        assert!(run.status.success(), "{cpu}: {run:?}");
        assert_eq!(String::from_utf8_lossy(&run.stdout), expected, "{cpu}");
    }
    let run = Command::new(qemu)
        .args(["-cpu", "cortex-a72"])
        .arg(&exe)
        .output()
        .unwrap();
    assert_eq!(run.stdout, b"");
    assert_eq!(
        String::from_utf8_lossy(&run.stderr),
        "error: this program needs an aarch64 v9 processor with SVE2\n"
    );
    assert_eq!(run.status.code(), Some(1));
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
fn check_warns_about_unused_names_but_run_does_not() {
    let source = "use math\n\nfn area(w: int, h: int) -> int:\n    ret w * 2\n\nx = area(1, 2)\n";
    let dir = project("unused_names", &[("main.stn", source)]);
    let stone = |command: &str| {
        Command::new(env!("CARGO_BIN_EXE_stone"))
            .args([command, "main.stn"])
            .current_dir(&dir)
            .output()
            .expect("stone should run")
    };
    let check = stone("check");
    assert!(check.status.success(), "{check:?}");
    let stderr = String::from_utf8_lossy(&check.stderr);
    let warnings: Vec<&str> = stderr.lines().filter(|l| l.contains("warning")).collect();
    assert_eq!(
        warnings,
        [
            "main.stn:1:5: warning: 'math' is imported but never used",
            "main.stn:3:17: warning: parameter 'h' is never used; remove it or name it '_h'",
            "main.stn:6:1: warning: variable 'x' is never used; remove it or name it '_x'",
        ]
    );
    let run = stone("run");
    assert!(run.status.success(), "{run:?}");
    assert_eq!(String::from_utf8_lossy(&run.stderr), "");
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
fn cwd_is_the_directory_the_program_runs_in() {
    let (dir, outputs) = run_and_build_in("os_cwd", "use os\nprint(os.cwd())\n");
    let canonical = dir.canonicalize().unwrap().display().to_string();
    // Windows names a canonical path in its verbatim form, which the system's own cwd is not
    let expected = format!("{}\n", canonical.trim_start_matches(r"\\?\"));
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

#[test]
fn compiled_trig_matches_the_interpreter() {
    let mut state = 0x9e37_79b9_7f4a_7c15;
    let mut source = String::from("use math\n");
    // values live in every register across a sin that calls the prelude, which must keep them
    source.push_str(
        "fn spread(x: float, k: int) -> float:\n    \
         a = x * 1.5\n    b = x + 2.0\n    c = x - 3.0\n    d = x * x\n    e = a + b\n    \
         f = c * d\n    g = e - f\n    h = a * 0.25\n    i = b * c\n    j = d + 1.0\n    \
         m = k * 3\n    n = k + 7\n    o = k - 2\n    \
         s = math.sin(x * 1e300) + math.cos(x * 1e30)\n    \
         ret a + b + c + d + e + f + g + h + i + j + float(m + n + o) + s\n",
    );
    let pick = |state: &mut u64| {
        let random = xorshift(state);
        let unit = (random >> 11) as f64 / (1u64 << 53) as f64;
        let sign = if random & 1 == 0 { 1.0 } else { -1.0 };
        sign * match random % 6 {
            0 => f64::from_bits(random >> 2) % 1e300,
            1 => unit,
            2 => unit * 2000.0,
            3 => (unit * 1e6).round() * std::f64::consts::FRAC_PI_2 * (1.0 + 1e-15 * unit),
            4 => 2f64.powf(20.0 + 1000.0 * unit),
            _ => 2f64.powf(-60.0 * unit),
        }
    };
    // the arguments go in lists for one loop, since every call site is a copy of the code, and
    // calls of the same value share one, a sine and a cosine reducing it once
    source.push_str("xs = []\nys = []\n");
    for _ in 0..4000 {
        let (x, y) = (pick(&mut state), pick(&mut state));
        if x.is_finite() && y.is_finite() {
            source.push_str(&format!("xs.append({x:e})\nys.append({y:e})\n"));
        }
    }
    source.push_str(
        "for i in range(xs.len()):\n    x = xs[i]\n    y = ys[i]\n    u = x % 1.0\n    \
         print(math.sin(x), math.cos(x), math.tan(x), math.atan(x), math.atan2(y, x))\n    \
         print(math.asin(u), math.acos(u), math.atan2(x, u))\n    \
         print(math.cos(y) - math.sin(y) * math.cos(y), math.tan(y) + math.sin(y))\n    \
         if i % 100 == 0:\n        print(spread(u, i))\n",
    );
    let (_, [run, built]) = run_and_build_in("trig_differential", &source);
    assert!(run.status.success(), "{run:?}");
    assert!(built.status.success(), "{built:?}");
    let expected = String::from_utf8_lossy(&run.stdout);
    let actual = String::from_utf8_lossy(&built.stdout);
    for (line, (want, got)) in expected.lines().zip(actual.lines()).enumerate() {
        assert_eq!(got, want, "line {}", line + 1);
    }
    assert_eq!(actual.lines().count(), expected.lines().count());
}

/// Returns a random format spec that Python's `format` takes for a value of `kind`, `'i'` for
/// an int, `'f'` for a float, and `'s'` for a str, built from the parts of the mini-language in
/// order: fill and alignment, sign, `#`, `0`, width, grouping, precision, and type.
fn random_spec(state: &mut u64, kind: char) -> String {
    let mut pick = |n: u64| xorshift(state) % n;
    let types: &[&str] = match kind {
        'i' => &[
            "", "d", "b", "o", "x", "X", "e", "E", "f", "F", "g", "G", "%",
        ],
        'f' => &["", "", "e", "E", "f", "F", "g", "G", "%"],
        _ => &["", "s"],
    };
    let ty = types[pick(types.len() as u64) as usize];
    let radix = matches!(ty, "b" | "o" | "x" | "X");
    let mut spec = String::new();
    if pick(10) < 4 {
        if pick(2) == 0 {
            spec.push(['*', '0', '-', 'x', ' ', 'é', '#'][pick(7) as usize]);
        }
        let aligns = if kind == 's' { "<>^" } else { "<>^=" };
        spec.push(aligns.as_bytes()[pick(aligns.len() as u64) as usize] as char);
    }
    if kind != 's' && pick(10) < 3 {
        spec.push(['+', '-', ' '][pick(3) as usize]);
    }
    if kind != 's' && pick(10) < 2 {
        spec.push('#');
    }
    if pick(10) < 3 {
        spec.push('0');
    }
    if pick(10) < 6 {
        spec.push_str(&(pick(25) + 1).to_string());
    }
    if kind != 's' && pick(10) < 3 {
        spec.push(if radix || pick(2) == 0 { '_' } else { ',' });
    }
    // an int takes a precision only as a float, and a str only with no sign
    let float = matches!(ty, "e" | "E" | "f" | "F" | "g" | "G" | "%");
    if pick(10) < 6 && (kind != 'i' || float) {
        spec.push_str(&format!(".{}", pick(21)));
    }
    spec + ty
}

#[test]
fn format_specs_write_what_python_writes() {
    if Command::new("python3").arg("--version").output().is_err() {
        eprintln!("skipping: python3 is not on PATH");
        return;
    }
    let mut state = 0x853c_49e6_748f_ea9b;
    // values written the same way in stone and in Python, each reading back exactly
    let mut floats: Vec<String> = [
        "0.0",
        "-0.0",
        "inf",
        "-inf",
        "nan",
        "0.5",
        "2.5",
        "-1.5",
        "9.995",
        "0.125",
        "1e16",
        "5e-324",
        "1.7976931348623157e308",
        "0.05",
        "99.5",
        "2.675",
        "1234567.891",
    ]
    .iter()
    .map(|text| text.to_string())
    .collect();
    while floats.len() < 250 {
        let random = xorshift(&mut state);
        let unit = (random >> 11) as f64 / (1u64 << 53) as f64;
        let value = match random % 4 {
            0 => f64::from_bits(xorshift(&mut state)),
            1 => (unit - 0.5) * 2000.0,
            2 => ((unit - 0.5) * 200.0 * 1e4).round() / 1e4,
            _ => 10f64.powf(unit * 34.0 - 12.0),
        };
        if value.is_finite() {
            floats.push(format!("{value:e}"));
        }
    }
    let mut ints: Vec<String> = ["0", "-1", "1", "255", "-255", "9223372036854775807"]
        .iter()
        .map(|text| text.to_string())
        .collect();
    ints.push("-9223372036854775807 - 1".to_string());
    while ints.len() < 250 {
        let random = xorshift(&mut state);
        let value = match random % 2 {
            0 => random as i64,
            _ => (random % 200_001) as i64 - 100_000,
        };
        ints.push(value.to_string());
    }
    let texts: Vec<String> = (0..80)
        .map(|_| {
            let length = xorshift(&mut state) % 12;
            (0..length)
                .map(|_| ['a', 'b', 'é', ' ', 'x', 'y'][(xorshift(&mut state) % 6) as usize])
                .collect()
        })
        .collect();
    let specs: Vec<(char, Vec<String>)> = ['f', 'i', 's']
        .into_iter()
        .map(|kind| {
            let specs = (0..40).map(|_| random_spec(&mut state, kind)).collect();
            (kind, specs)
        })
        .collect();

    // a list of each kind, then a line of every spec per value, the same in both languages
    let list = |values: &[String], quote: bool| {
        let items: Vec<String> = values
            .iter()
            .map(|value| match (quote, value.as_str()) {
                (true, _) => format!("\"{value}\""),
                (_, "inf") => "1e308 * 10.0".to_string(),
                (_, "-inf") => "-(1e308 * 10.0)".to_string(),
                (_, "nan") => "1e308 * 10.0 - 1e308 * 10.0".to_string(),
                _ => value.clone(),
            })
            .collect();
        format!("[{}]", items.join(", "))
    };
    let lists = [list(&floats, false), list(&ints, false), list(&texts, true)];
    let mut source = String::new();
    let mut python = String::new();
    for ((kind, specs), values) in specs.iter().zip(&lists) {
        // a variable of its own for each kind, since a variable has one type
        let holes: Vec<String> = specs
            .iter()
            .map(|spec| format!("{{{kind}:{spec}}}"))
            .collect();
        let holes = holes.join("|");
        source.push_str(&format!(
            "for {kind} in {values}:\n    print(\"{holes}\")\n"
        ));
        python.push_str(&format!(
            "for {kind} in {values}:\n    print(f\"{holes}\")\n",
            values = if *kind == 'f' {
                format!("[float(x) for x in {values}]")
            } else {
                values.clone()
            }
        ));
    }
    let script = std::env::temp_dir().join("stone_format_specs.py");
    std::fs::write(&script, &python).unwrap();
    // Windows' python writes a pipe in the ANSI code page unless told otherwise
    let expected = Command::new("python3")
        .arg(&script)
        .env("PYTHONIOENCODING", "utf-8")
        .output()
        .expect("python3 should run");
    assert!(expected.status.success(), "{expected:?}");
    let expected = String::from_utf8_lossy(&expected.stdout);

    let (_, [run, built]) = run_and_build_in("format_specs_differential", &source);
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        let actual = String::from_utf8_lossy(&output.stdout);
        for (line, (want, got)) in expected.lines().zip(actual.lines()).enumerate() {
            assert_eq!(got, want, "line {}", line + 1);
        }
        assert_eq!(actual.lines().count(), expected.lines().count());
    }
}

/// Writes a random JSON value of at most `depth` more levels into `out`, with whitespace, escapes,
/// and number forms that `json.parse` and Python's `json.loads` read alike: no integer past 64
/// bits, no float that overflows, no `\u0000`, and no lone surrogate, which stone refuses.
fn random_json(state: &mut u64, depth: u32, out: &mut String) {
    let space = |state: &mut u64, out: &mut String| {
        for _ in 0..xorshift(state) % 3 {
            out.push([' ', '\t', ' '][(xorshift(state) % 3) as usize]);
        }
    };
    space(state, out);
    let kind = xorshift(state) % if depth == 0 { 5 } else { 7 };
    match kind {
        0 => out.push_str(["null", "true", "false"][(xorshift(state) % 3) as usize]),
        1 => {
            let random = xorshift(state);
            let value = match random % 4 {
                0 => random as i64,
                1 => i64::MIN,
                2 => i64::MAX,
                _ => (random % 2001) as i64 - 1000,
            };
            out.push_str(&value.to_string());
        }
        2 => {
            let random = xorshift(state);
            let value = match random % 3 {
                0 => f64::from_bits(xorshift(state)),
                1 => (random % 100_000) as f64 / 64.0 - 700.0,
                _ => 10f64.powi((random % 600) as i32 - 300),
            };
            if !value.is_finite() {
                out.push_str("-0.0");
            } else if random.is_multiple_of(2) {
                out.push_str(&format!("{value:e}"));
            } else {
                out.push_str(&format!("{value:?}"));
            }
        }
        3 => out.push_str(
            ["1e2", "-0", "0.5E-3", "12E+3", "-1.25e-400"][(xorshift(state) % 5) as usize],
        ),
        4 => {
            out.push('"');
            for _ in 0..xorshift(state) % 10 {
                let pieces = [
                    "a",
                    "Z",
                    " ",
                    "é",
                    "日本",
                    "😀",
                    "\\n",
                    "\\\"",
                    "\\\\",
                    "\\/",
                    "\\t",
                    "\\b",
                    "\\f",
                    "\\r",
                    "\\u0001",
                    "\\u001f",
                    "\\u00e9",
                    "\\u65e5",
                    "\\ud83d\\ude00",
                    "\\uFFFF",
                    "\u{7f}",
                    "{",
                    "}",
                ];
                out.push_str(pieces[(xorshift(state) % pieces.len() as u64) as usize]);
            }
            out.push('"');
        }
        5 => {
            out.push('[');
            let count = xorshift(state) % 5;
            for i in 0..count {
                if i > 0 {
                    out.push(',');
                }
                random_json(state, depth - 1, out);
            }
            space(state, out);
            out.push(']');
        }
        _ => {
            out.push('{');
            let count = xorshift(state) % 5;
            for i in 0..count {
                if i > 0 {
                    out.push(',');
                }
                space(state, out);
                // few keys, so that some repeat
                let key =
                    ["a", "b", "key", "é", "\\u0041", "\\n", ""][(xorshift(state) % 7) as usize];
                out.push_str(&format!("\"{key}\""));
                space(state, out);
                out.push(':');
                random_json(state, depth - 1, out);
            }
            space(state, out);
            out.push('}');
        }
    }
    space(state, out);
}

#[test]
fn json_reads_and_writes_what_python_does() {
    if Command::new("python3").arg("--version").output().is_err() {
        eprintln!("skipping: python3 is not on PATH");
        return;
    }
    let mut state = 0x9e37_79b9_7f4a_7c15;
    let documents: Vec<String> = (0..300)
        .map(|_| {
            let mut text = String::new();
            random_json(&mut state, 4, &mut text);
            text
        })
        .collect();
    let documents = documents.join("\n") + "\n";
    let python = "import json, sys\n\
                  for line in open('documents.txt', encoding='utf-8').read().splitlines():\n    \
                  v = json.loads(line)\n    \
                  print(json.dumps(v, separators=(',', ':'), ensure_ascii=False))\n    \
                  print(json.dumps(v, indent=2, ensure_ascii=False))\n";
    let source = "use fs\nuse json\n\
                  for line in fs.read(\"documents.txt\").unwrap().splitlines():\n    \
                  v = json.parse(line).unwrap()\n    \
                  print(json.write(v))\n    \
                  print(json.pretty(v, 2))\n";
    let (dir, [run, built]) = run_and_build_with("json_differential", source, |dir| {
        std::fs::write(dir.join("documents.txt"), &documents).unwrap();
        std::fs::write(dir.join("expected.py"), python).unwrap();
    });
    let expected = Command::new("python3")
        .arg("expected.py")
        .current_dir(&dir)
        .env("PYTHONIOENCODING", "utf-8")
        .output()
        .expect("python3 should run");
    assert!(expected.status.success(), "{expected:?}");
    let expected = String::from_utf8_lossy(&expected.stdout);
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        let actual = String::from_utf8_lossy(&output.stdout);
        for (line, (want, got)) in expected.lines().zip(actual.lines()).enumerate() {
            assert_eq!(got, want, "line {}", line + 1);
        }
        assert_eq!(actual.lines().count(), expected.lines().count());
    }
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

/// Returns the name of every DLL whose name the Windows executable `bytes` holds, such as
/// `KERNEL32.dll`, which its import directory names.
fn dll_names(bytes: &[u8]) -> Vec<String> {
    assert_eq!(&bytes[..2], b"MZ");
    let mut names = Vec::new();
    for end in 0..bytes.len().saturating_sub(4) {
        if bytes[end..end + 5].eq_ignore_ascii_case(b".dll\0") {
            let start = bytes[..end]
                .iter()
                .rposition(|b| !b.is_ascii_alphanumeric() && *b != b'_' && *b != b'-')
                .map_or(0, |i| i + 1);
            names.push(String::from_utf8_lossy(&bytes[start..end + 4]).into_owned());
        }
    }
    names
}

/// Checks that the executable `exe`, built for this machine, needs nothing but the system: on
/// Linux it is static, on macOS it is signed and loads only `libSystem`, and on Windows it
/// imports only from the system's own DLLs.
fn assert_needs_only_the_system(exe: &Path) {
    if cfg!(windows) {
        let names = dll_names(&std::fs::read(exe).unwrap());
        assert!(names.contains(&"KERNEL32.dll".to_string()), "{names:?}");
        for name in &names {
            assert!(
                name == "KERNEL32.dll" || name == "bcrypt.dll",
                "{name} is not the system's: {names:?}"
            );
        }
    } else if cfg!(target_os = "macos") {
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

/// Runs the executable at `exe`, named as `stone build` names it on this system (`exe.exe` on
/// Windows), and returns what it printed.
///
/// For example, running a build of `print(1)` returns `"1\n"`.
fn stdout_of(exe: &Path) -> String {
    let exe = exe.with_file_name(format!(
        "{}{}",
        exe.file_name().unwrap().to_str().unwrap(),
        std::env::consts::EXE_SUFFIX
    ));
    let run = Command::new(&exe).output().expect("the binary should run");
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
        format!(
            "Compiled {}{}\n",
            Path::new("app").join("build").join("prog").display(),
            std::env::consts::EXE_SUFFIX
        )
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
    assert!(
        dir.join(format!("app/build/app{}", std::env::consts::EXE_SUFFIX))
            .is_file()
    );

    // a file names its program's directory
    let output = stone_in(&dir, &["clean", "app/main.stn"]);
    assert!(output.status.success(), "{output:?}");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        format!("Removed {}\n", Path::new("app").join("build").display())
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
            "{}:2:9: error: '+' needs 'T' to implement 'add', since 'T' may be any type; bound \
             it, as in '[T: add[T]]'\n  |\n2 |     ret xs[0] + xs[1]\n  |         \
             ^^^^^^^^^^^^^\n",
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
         impl Shape for Sq:\n    fn side(self) -> float:\n        ret self.s\nprint(Sq(s=1.0))\n",
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

// symlinks, modes, and names that are not UTF-8 are Unix's
#[cfg(unix)]
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

/// Tests of what only Unix runs: networking, which Windows programs cannot use yet, and
/// windows drawn over X11 and Wayland, against fake servers and real ones.
#[cfg(unix)]
#[path = "cli/unix.rs"]
mod unix;

/// Tests of downloading packages from a fake GitHub, which need the `fetch` feature.
#[cfg(feature = "fetch")]
#[path = "cli/packages.rs"]
mod packages;

#[cfg(not(feature = "fetch"))]
#[test]
fn add_without_feature_explains_how_to_get_a_build_that_downloads() {
    let dir = project("package_no_fetch", &[("main.stn", "print(1)\n")]);
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["add", "github:alice/json"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains("cannot download packages"), "{stderr}");
    assert!(!dir.join("stone.toml").exists());
}

#[test]
fn tree_shows_the_packages_a_program_uses() {
    let dir = project(
        "package_tree",
        &[
            ("app/main.stn", "print(1)\n"),
            (
                "app/stone.toml",
                "[package]\nname = \"app\"\nversion = \"0.2.0\"\n\n[dependencies]\n\
                 geo = { path = \"../geo\" }\nshapes = { path = \"../shapes\" }\n",
            ),
            (
                "geo/stone.toml",
                "[package]\nname = \"geo\"\nversion = \"1.0.0\"\n\n[dependencies]\n\
                 nums = { path = \"../num\" }\n",
            ),
            (
                "shapes/stone.toml",
                "[package]\nname = \"shapes\"\n\n[dependencies]\nnum = { path = \"../num\" }\n",
            ),
            ("num/stone.toml", "[package]\nname = \"num\"\n"),
        ],
    );
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(["tree", "app"])
        .current_dir(&dir)
        .output()
        .expect("stone should run");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout).replace('\\', "/"),
        "app 0.2.0\n\
         ├── geo 1.0.0 (path:geo)\n\
         │   └── nums = num (path:num)\n\
         └── shapes (path:shapes)\n    \
         └── num (path:num) (*)\n"
    );
}

/// Tests of the `gui` module's Win32 windows, which run where Windows programs and PowerShell
/// do: on Windows, and under WSL.
#[path = "cli/windows_gui.rs"]
mod windows_gui;

/// Tests of Windows programs that start others, which run where Windows programs do: on
/// Windows, and under WSL.
#[path = "cli/windows.rs"]
mod windows;

#[test]
fn a_package_from_the_store_runs_and_is_locked() {
    let dir = project(
        "package_store",
        &[
            ("app/main.stn", "use mime\nprint(mime.v())\n"),
            (
                "app/stone.toml",
                "[dependencies]\nmime = { github = \"alice/mime\", version = \"1.2\" }\n",
            ),
            (
                "home/packages/github.com/alice/mime/1.2.0/stone.toml",
                "[package]\nname = \"mime\"\nversion = \"1.2.0\"\n",
            ),
            (
                "home/packages/github.com/alice/mime/1.2.0/lib.stn",
                "pub fn v() -> str:\n    ret \"1.2.0\"\n",
            ),
        ],
    );
    let run = |command: &str| {
        Command::new(env!("CARGO_BIN_EXE_stone"))
            .args([command, "app/main.stn"])
            .current_dir(&dir)
            .env("STONE_HOME", dir.join("home"))
            .output()
            .expect("stone should run")
    };
    let output = run("run");
    assert_eq!(String::from_utf8_lossy(&output.stderr), "");
    assert_eq!(String::from_utf8_lossy(&output.stdout), "1.2.0\n");
    let lock = std::fs::read_to_string(dir.join("app/stone.lock")).expect("lock written");
    assert!(lock.contains("name = \"mime\"\ngithub = \"alice/mime\"\nversion = \"1.2.0\"\n"));

    // a changed release no longer matches the lock
    std::fs::write(
        dir.join("home/packages/github.com/alice/mime/1.2.0/lib.stn"),
        "pub fn v() -> str:\n    ret \"changed\"\n",
    )
    .unwrap();
    let output = run("check");
    assert_eq!(output.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("error: mime 1.2.0 (github:alice/mime) does not hold what it held"),
        "{stderr}"
    );
    assert_eq!(
        std::fs::read_to_string(dir.join("app/stone.lock")).unwrap(),
        lock
    );
}

/// Copies the directory `from` and everything in it to `to`, replacing what was there.
fn copy_dir(from: &Path, to: &Path) {
    let _ = std::fs::remove_dir_all(to);
    std::fs::create_dir_all(to).unwrap();
    for entry in std::fs::read_dir(from).unwrap() {
        let path = entry.unwrap().path();
        let target = to.join(path.file_name().unwrap());
        if path.is_dir() {
            copy_dir(&path, &target);
        } else {
            std::fs::copy(&path, &target).unwrap();
        }
    }
}

/// Runs `stone test <args>` in a copy of `tests/testing` named after `name`, returning its exit
/// code and stdout.
fn stone_test(name: &str, args: &[&str]) -> (Option<i32>, String) {
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(name);
    copy_dir(Path::new("tests/testing"), &dir);
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("test")
        .args(args)
        .current_dir(&dir)
        .env("STONE_LEAK_CHECK", "1")
        .output()
        .expect("stone should run");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(stderr, "", "stone test wrote to stderr");
    // Windows writes paths with backslashes, and the fixtures' sources hold none
    let stdout = String::from_utf8(output.stdout).unwrap();
    let stdout = match cfg!(windows) {
        true => stdout.replace('\\', "/"),
        false => stdout,
    };
    (output.status.code(), stdout)
}

#[test]
fn stone_test_reports_every_test_alike_under_both_backends() {
    let expected = std::fs::read_to_string("tests/testing/report.txt").unwrap();
    assert_eq!(stone_test("testing-run", &[]), (Some(1), expected.clone()));
    assert_eq!(
        stone_test("testing-build", &["--build", "-j", "2"]),
        (Some(1), expected)
    );
}

#[test]
fn stone_test_runs_only_what_its_paths_and_filter_pick() {
    assert_eq!(
        stone_test(
            "testing-filter",
            &["app", "shapes.stn", "--filter", "parses a point"]
        ),
        (
            Some(1),
            "\
shapes.stn
  ok      parses a point
  FAILED  parses a point without a comma

failures:

shapes.stn: parses a point without a comma
shapes.stn:8:47: error: list index out of range (index 1, length 1)
  |
8 |     ret Point(x=int(parts[0]).unwrap(), y=int(parts[1]).unwrap())
  |                                               ^^^^^^^^

1 passed, 1 failed, 3 filtered out in 1 file
"
            .to_string()
        )
    );
    assert_eq!(
        stone_test("testing-passing", &["app"]),
        (
            Some(0),
            "\
app/geo/vec.stn
  ok      dot of two vectors
  ok      dot of nothing

2 passed, 0 failed in 1 file
"
            .to_string()
        )
    );
    assert_eq!(
        stone_test("testing-none", &["plain.stn"]),
        (Some(0), "no tests found\n".to_string())
    );
}
