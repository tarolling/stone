//! `stone test`: finds the files with test blocks, runs each test, and reports the results.
//!
//! Each file is linked as the entry of the program it belongs to (see
//! [`driver::program_root`]), so its `use`s resolve as the program's do, and checked. Then each
//! of its tests runs alone, with none of the file's top-level statements: under the interpreter
//! by default, or, with [`Options::build`], as a run of the file's build for `stone test` (see
//! [`driver::compile_tests`]) with `STONE_TEST` naming the test. A test passes if it ends without
//! a runtime error, `os.exit(0)` included.
//!
//! For example, a file `shapes.stn` with a passing and a failing test reports
//!
//! ```text
//! shapes.stn
//!   ok      parses a point
//!   FAILED  area of a circle
//!
//! failures:
//!
//! shapes.stn: area of a circle
//! shapes.stn:12:12: error: assertion failed: 3.14 == 3.0
//!    |
//! 12 |     assert area(c) == 3.0
//!    |            ^^^^^^^^^^^^^^
//!
//! 1 passed, 1 failed in 1 file
//! ```

use crate::codegen::Target;
use crate::codegen::ir::lower::TEST_VARIABLE;
use crate::diagnostic::{Diagnostic, Diagnostics, Severity};
use crate::driver::{self, Checked};
use crate::interpreter::{Exit, Limits};
use crate::lexer::Lexer;
use crate::project::{self, FsSources, SourceMap, Sources};
use crate::token::TokenType;
use std::error::Error;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::mpsc;

/// How `stone test` runs tests.
#[derive(Debug, Clone, Default)]
pub struct Options {
    /// Whether each file is compiled and its tests run as native programs, rather than with the
    /// interpreter.
    pub build: bool,
    /// Text that a test's description must contain for it to run, if any.
    pub filter: Option<String>,
    /// How many files are tested at once, at least 1.
    pub jobs: usize,
}

/// How one test ended.
#[derive(Debug, Clone, PartialEq)]
pub enum Outcome {
    Passed,
    /// The test failed, with what it printed, its runtime error last, as a terminal would show
    /// a run of it.
    Failed(String),
}

/// What became of one test of a file.
#[derive(Debug, Clone, PartialEq)]
pub struct TestReport {
    /// The test's description, as in `test "parses a point":`.
    pub description: String,
    pub outcome: Outcome,
}

/// What became of the tests of one file.
#[derive(Debug, Clone, PartialEq)]
pub struct FileReport {
    /// The file's path, as it was found.
    pub path: String,
    /// The errors that kept the file from being checked or built, rendered, if any, when none of
    /// its tests ran.
    pub errors: Option<String>,
    /// Every test that ran, in the file's order.
    pub tests: Vec<TestReport>,
    /// How many tests the filter left out.
    pub filtered: usize,
}

impl FileReport {
    /// Returns whether every test of the file that ran passed and nothing kept any from running.
    pub fn passed(&self) -> bool {
        self.errors.is_none() && self.tests.iter().all(|t| t.outcome == Outcome::Passed)
    }
}

/// Returns whether `source` has a test block, `test` followed by a string at the start of a
/// top-level line, judging by its tokens alone, without parsing it.
///
/// For example, `has_tests("test \"adds\":\n    assert 1 + 1 == 2\n")` is true, while a file
/// that only defines `fn test(n: int)` has none.
pub fn has_tests(source: &str) -> bool {
    let (tokens, _) = Lexer::new(source).lex_recovering();
    let mut depth = 0usize;
    let mut line_start = true;
    for (i, token) in tokens.iter().enumerate() {
        match &token.r#type {
            TokenType::Indent => depth += 1,
            TokenType::Dedent => depth = depth.saturating_sub(1),
            TokenType::Newline => line_start = true,
            TokenType::Name(name) if line_start && depth == 0 && name == "test" => {
                let next = tokens.get(i + 1).map(|t| &t.r#type);
                if matches!(next, Some(TokenType::String(_) | TokenType::StringStart(_))) {
                    return true;
                }
                line_start = false;
            }
            _ => line_start = false,
        }
    }
    false
}

/// Returns the files of `paths` that have test blocks (see [`has_tests`]), in order: each path
/// is a file, or a directory whose `.stn` files are all looked at (see [`driver::stone_files`]).
/// A path that cannot be read gives an error message instead.
///
/// For example, `test_files(&["tests".into()])` gives every file under `tests` with a test block.
pub fn test_files(paths: &[PathBuf]) -> (Vec<PathBuf>, Vec<String>) {
    let (mut files, mut errors) = (vec![], vec![]);
    for path in paths {
        let found = match path.is_dir() {
            true => match driver::stone_files(path) {
                Ok(found) => found,
                Err(e) => {
                    errors.push(format!("{}: error: {e}", path.display()));
                    continue;
                }
            },
            false => vec![path.clone()],
        };
        for file in found {
            match std::fs::read_to_string(&file) {
                Ok(source) if has_tests(&source) => {
                    // `./a.stn` reads better as `a.stn`
                    let file = file
                        .strip_prefix(".")
                        .map(Path::to_path_buf)
                        .unwrap_or(file);
                    if !files.contains(&file) {
                        files.push(file);
                    }
                }
                Ok(_) => {}
                Err(e) => errors.push(format!("{}: error: {e}", file.display())),
            }
        }
    }
    (files, errors)
}

/// Links the file at `path`, holding `source`, as the entry of the program it belongs to, so its
/// `use`s resolve from the program's root, as they do when the program runs.
pub fn link_file(path: &Path, source: &str, sources: &dyn Sources) -> project::Linked {
    match driver::program_root(path, sources) {
        Some(root) => project::link_in(&root, path, source, sources),
        None => project::link(path, source, sources),
    }
}

/// Tests the file at `path`, reading it and the modules it uses from `sources`: links and checks
/// it, then runs each of its tests that `options` lets through.
///
/// For example, a file whose only test passes gives a report of one [`Outcome::Passed`], and a
/// file with a type error gives a report with the rendered error and no tests.
pub fn test_file(path: &Path, options: &Options, sources: &dyn Sources) -> FileReport {
    let mut report = FileReport {
        path: path.display().to_string(),
        errors: None,
        tests: vec![],
        filtered: 0,
    };
    let Some(source) = sources.read(path) else {
        report.errors = Some(format!("{}: error: cannot read the file\n", report.path));
        return report;
    };
    let (map, checked) = driver::check_linked(link_file(path, &source, sources), sources);
    let checked = match checked {
        Ok(checked) => checked,
        Err(diagnostics) => {
            report.errors = Some(render_errors(&map, &diagnostics.0));
            return report;
        }
    };
    let (chosen, filtered): (Vec<_>, Vec<_>) = checked.tests.iter().partition(|test| {
        options
            .filter
            .as_ref()
            .is_none_or(|text| test.description.contains(text.as_str()))
    });
    report.filtered = filtered.len();
    if chosen.is_empty() {
        return report;
    }
    let runner = match options.build {
        true => match Runner::build(path, &checked, &map, sources) {
            Ok(runner) => runner,
            Err(error) => {
                report.errors = Some(error);
                return report;
            }
        },
        false => {
            // the interpreter runs only what the machine it runs on can
            if let Ok(host) = Target::host() {
                let unsupported = driver::unsupported(&checked.module, &map, host.os);
                if !unsupported.is_empty() {
                    report.errors = Some(render_errors(&map, &unsupported));
                    return report;
                }
            }
            Runner::Interpreter
        }
    };
    for test in chosen {
        let outcome = runner.run(&checked, &map, &test.function);
        report.tests.push(TestReport {
            description: test.description.clone(),
            outcome,
        });
    }
    report
}

/// Renders the errors among `diagnostics`, each with its location and source line.
fn render_errors(map: &SourceMap, diagnostics: &[Diagnostic]) -> String {
    diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .map(|d| map.render(d))
        .collect()
}

/// How a file's tests run.
enum Runner {
    /// With the interpreter, each in a fresh one.
    Interpreter,
    /// As runs of the file's build for `stone test`, the executable at this path.
    Build(PathBuf),
}

impl Runner {
    /// Compiles the file at `path` into a build for `stone test`, under `build/test/` of its
    /// program's root (see [`test_build_path`]).
    fn build(
        path: &Path,
        checked: &Checked,
        map: &SourceMap,
        sources: &dyn Sources,
    ) -> Result<Runner, String> {
        let target = Target::host().map_err(|e| format!("{}: error: {e}\n", path.display()))?;
        let output = test_build_path(path, target, sources);
        let root = driver::program_root(path, sources)
            .unwrap_or_else(|| path.parent().unwrap_or(Path::new("")).to_path_buf());
        let built = driver::prepare_build_dir(&root.join(driver::BUILD_DIR))
            .and_then(|()| match output.parent() {
                Some(dir) => std::fs::create_dir_all(dir),
                None => Ok(()),
            })
            .map_err(|e| -> Box<dyn Error> { e.into() })
            .and_then(|()| driver::compile_tests(checked, map, &output, target));
        match built {
            Ok(()) => Ok(Runner::Build(output)),
            Err(error) => Err(match error.downcast_ref::<Diagnostics>() {
                Some(diagnostics) => render_errors(map, &diagnostics.0),
                None => format!("{}: error: {error}\n", path.display()),
            }),
        }
    }

    /// Runs the test function `function` of the checked file.
    fn run(&self, checked: &Checked, map: &SourceMap, function: &str) -> Outcome {
        match self {
            Runner::Interpreter => interpret(checked, map, function),
            Runner::Build(executable) => run_built(executable, function),
        }
    }
}

/// Returns where the build for `stone test` of the file at `path` goes: under `build/test/` in
/// its program's root (or its own directory), at its path from there.
///
/// For example, `app/geo/shapes.stn` in the program `app` builds to
/// `app/build/test/geo/shapes`, with `.exe` added for Windows.
pub fn test_build_path(path: &Path, target: Target, sources: &dyn Sources) -> PathBuf {
    let dir = path.parent().unwrap_or(Path::new(""));
    let root = driver::program_root(path, sources).unwrap_or_else(|| dir.to_path_buf());
    let mut output = root
        .join(driver::BUILD_DIR)
        .join("test")
        .join(path.strip_prefix(&root).unwrap_or(path).with_extension(""));
    if target.os == crate::codegen::Os::Windows {
        output.set_extension("exe");
    }
    output
}

/// Runs the test function `function` of the checked file with the interpreter.
fn interpret(checked: &Checked, map: &SourceMap, function: &str) -> Outcome {
    let (mut out, mut errors) = (vec![], vec![]);
    let result = driver::run_test(
        &checked.module,
        &checked.calls,
        function,
        &mut out,
        &mut errors,
        Limits::DEFAULT,
    );
    let Err(error) = result else {
        return Outcome::Passed;
    };
    let reason = match error.downcast_ref::<Diagnostic>() {
        Some(diagnostic) => map.render(diagnostic),
        None => match error.downcast_ref::<Exit>() {
            Some(Exit(status)) => exited(&errors, Some(i32::from(*status))),
            None => format!("error: {error}\n"),
        },
    };
    Outcome::Failed(failure_text(&out, &errors, &reason))
}

/// Runs the build for `stone test` at `executable` with `STONE_TEST` naming `function`, giving
/// it no arguments and no input.
fn run_built(executable: &Path, function: &str) -> Outcome {
    let done = Command::new(executable)
        .env(TEST_VARIABLE, function)
        .stdin(Stdio::null())
        .output();
    let done = match done {
        Ok(done) => done,
        Err(e) => {
            return Outcome::Failed(format!(
                "error: could not run {}: {e}\n",
                executable.display()
            ));
        }
    };
    if done.status.success() {
        return Outcome::Passed;
    }
    let reason = exited(&done.stderr, done.status.code());
    Outcome::Failed(failure_text(&done.stdout, &done.stderr, &reason))
}

/// Returns the reason a failed test gives for its exit `status`, or nothing if it wrote to
/// stderr, which says why itself, as a runtime error does.
fn exited(errors: &[u8], status: Option<i32>) -> String {
    match (errors.is_empty(), status) {
        (false, _) => String::new(),
        (true, Some(status)) => format!("error: the test exited with status {status}\n"),
        (true, None) => "error: the test was stopped by a signal\n".to_string(),
    }
}

/// Joins what a failed test wrote to stdout and stderr, each ending its line, and the `reason`
/// it failed.
fn failure_text(out: &[u8], errors: &[u8], reason: &str) -> String {
    let mut text = String::new();
    for part in [out, errors] {
        let part = String::from_utf8_lossy(part);
        text.push_str(&part);
        if !part.is_empty() && !part.ends_with('\n') {
            text.push('\n');
        }
    }
    text.push_str(reason);
    text
}

/// Writes the lines of the report of one file: its path, then each test that ran with whether it
/// passed, or that it could not be tested.
///
/// For example, a file `a.stn` whose test `adds` passed writes `a.stn\n  ok      adds\n`.
pub fn file_lines(report: &FileReport) -> String {
    let mut text = format!("{}\n", report.path);
    if report.errors.is_some() {
        text.push_str("  FAILED  (the file has errors)\n");
    }
    for test in &report.tests {
        let status = match test.outcome {
            Outcome::Passed => "ok",
            Outcome::Failed(_) => "FAILED",
        };
        text.push_str(&format!("  {status:<6}  {}\n", test.description));
    }
    text
}

/// Returns the end of the report of `reports`: each failure with why it failed, then how many
/// tests passed and failed in how many files.
///
/// For example, one file whose two tests passed gives `\n2 passed, 0 failed in 1 file\n`.
pub fn summary(reports: &[FileReport]) -> String {
    let mut failures = String::new();
    for report in reports {
        if let Some(errors) = &report.errors {
            failures.push_str(&format!("\n{}\n{errors}", report.path));
        }
        for test in &report.tests {
            if let Outcome::Failed(why) = &test.outcome {
                failures.push_str(&format!("\n{}: {}\n{why}", report.path, test.description));
            }
        }
    }
    let mut text = String::new();
    if !failures.is_empty() {
        text.push_str("\nfailures:\n");
        text.push_str(&failures);
    }
    let count = |outcome: fn(&Outcome) -> bool| {
        reports
            .iter()
            .flat_map(|r| &r.tests)
            .filter(|t| outcome(&t.outcome))
            .count()
    };
    let passed = count(|o| *o == Outcome::Passed);
    let failed = count(|o| *o != Outcome::Passed);
    let tested: Vec<&FileReport> = reports
        .iter()
        .filter(|r| r.errors.is_some() || !r.tests.is_empty())
        .collect();
    let plural = |n: usize, one: &str, many: &str| match n {
        1 => format!("1 {one}"),
        n => format!("{n} {many}"),
    };
    text.push_str(&format!("\n{passed} passed, {failed} failed"));
    let filtered: usize = reports.iter().map(|r| r.filtered).sum();
    if filtered > 0 {
        text.push_str(&format!(", {filtered} filtered out"));
    }
    text.push_str(&format!(" in {}", plural(tested.len(), "file", "files")));
    let broken = reports.iter().filter(|r| r.errors.is_some()).count();
    if broken > 0 {
        let have = if broken == 1 { "has" } else { "have" };
        text.push_str(&format!(
            ", of which {} {have} errors",
            plural(broken, "file", "files")
        ));
    }
    text.push('\n');
    text
}

/// Tests `files`, several at once as `options` allows, reading them from the file system, and
/// writes the report to `out` as it goes, each file's lines as soon as it and every file before
/// it are done. Returns whether every test passed and every file could be tested.
///
/// With no test to run at all, it writes `no tests found` and succeeds.
pub fn run(files: &[PathBuf], options: &Options, out: &mut impl Write) -> std::io::Result<bool> {
    let next = AtomicUsize::new(0);
    let (send, receive) = mpsc::channel();
    let mut reports: Vec<Option<FileReport>> = vec![None; files.len()];
    let workers = options.jobs.clamp(1, files.len().max(1));
    let mut written = 0;
    let mut shown = 0;
    std::thread::scope(|scope| -> std::io::Result<()> {
        for _ in 0..workers {
            let send = send.clone();
            let next = &next;
            // checking and linking recurse as deeply as the interpreter may
            std::thread::Builder::new()
                .stack_size(Limits::STACK_SIZE)
                .spawn_scoped(scope, move || {
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(file) = files.get(i) else {
                            break;
                        };
                        let report = test_file(file, options, &FsSources);
                        if send.send((i, report)).is_err() {
                            break;
                        }
                    }
                })?;
        }
        drop(send);
        for (i, report) in receive {
            reports[i] = Some(report);
            while let Some(Some(report)) = reports.get(written) {
                written += 1;
                if report.errors.is_none() && report.tests.is_empty() {
                    continue;
                }
                if shown > 0 {
                    writeln!(out)?;
                }
                shown += 1;
                write!(out, "{}", file_lines(report))?;
                out.flush()?;
            }
        }
        Ok(())
    })?;
    let reports: Vec<FileReport> = reports.into_iter().flatten().collect();
    if shown == 0 {
        writeln!(out, "no tests found")?;
        return Ok(true);
    }
    write!(out, "{}", summary(&reports))?;
    Ok(reports.iter().all(FileReport::passed))
}

/// Returns how many files to test at once by default: one per processor.
pub fn default_jobs() -> usize {
    std::thread::available_parallelism().map_or(1, |n| n.get())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::project::MapSources;

    #[test]
    fn a_file_has_tests_when_a_top_level_line_opens_one() {
        assert!(has_tests("test \"adds\":\n    assert 1 + 1 == 2\n"));
        assert!(has_tests("x = 1\ntest \"a {x}\":\n    ret\n"));
        assert!(!has_tests(
            "fn test(n: int) -> int:\n    ret n\nprint(test(1))\n"
        ));
        assert!(!has_tests("fn f():\n    test \"nested\":\n        ret\n"));
        assert!(!has_tests("print(\"test\")\n"));
    }

    fn sources(files: &[(&str, &str)]) -> MapSources {
        let mut sources = MapSources::default();
        for (path, source) in files {
            sources.insert(*path, *source);
        }
        sources
    }

    #[test]
    fn each_test_runs_alone_and_a_failure_shows_its_output_and_error() {
        let files = sources(&[(
            "shapes.stn",
            "\
print(\"top-level code\")

fn area(r: float) -> float:
    ret 3.0 * r * r

test \"area of a unit circle\":
    assert area(1.0) == 3.0

test \"area of a circle\":
    print(\"computing\")
    assert area(2.0) == 13.0
",
        )]);
        let report = test_file(Path::new("shapes.stn"), &Options::default(), &files);
        assert_eq!(report.errors, None);
        assert_eq!(
            report.tests,
            [
                TestReport {
                    description: "area of a unit circle".to_string(),
                    outcome: Outcome::Passed,
                },
                TestReport {
                    description: "area of a circle".to_string(),
                    outcome: Outcome::Failed(
                        "\
computing
shapes.stn:11:12: error: assertion failed: 12.0 == 13.0
   |
11 |     assert area(2.0) == 13.0
   |            ^^^^^^^^^^^^^^^^^
"
                        .to_string()
                    ),
                },
            ]
        );
        assert_eq!(
            file_lines(&report),
            "shapes.stn\n  ok      area of a unit circle\n  FAILED  area of a circle\n"
        );
        assert_eq!(
            summary(std::slice::from_ref(&report)),
            "\
\nfailures:

shapes.stn: area of a circle
computing
shapes.stn:11:12: error: assertion failed: 12.0 == 13.0
   |
11 |     assert area(2.0) == 13.0
   |            ^^^^^^^^^^^^^^^^^

1 passed, 1 failed in 1 file
"
        );
    }

    #[test]
    fn a_module_is_tested_in_its_program_and_a_filter_picks_tests() {
        let files = sources(&[
            ("app/main.stn", "use geo.vec\nprint(vec.dot([1], [2]))\n"),
            ("app/geo/units.stn", "pub fn scale() -> int:\n    ret 2\n"),
            (
                "app/geo/vec.stn",
                "\
use geo.units

pub fn dot(a: list[int], b: list[int]) -> int:
    total = 0
    for i in range(a.len()):
        total += a[i] * b[i] * units.scale()
    ret total

test \"dot of one\":
    assert dot([1], [3]) == 6

test \"dot of two\":
    os_status = 0
    assert dot([1, 1], [1, 1]) == 4 + os_status
",
            ),
        ]);
        let options = Options {
            filter: Some("one".to_string()),
            ..Options::default()
        };
        let report = test_file(Path::new("app/geo/vec.stn"), &options, &files);
        assert_eq!(report.errors, None);
        assert_eq!(report.filtered, 1);
        assert_eq!(
            report.tests,
            [TestReport {
                description: "dot of one".to_string(),
                outcome: Outcome::Passed,
            }]
        );
        assert_eq!(
            summary(&[report]),
            "\n1 passed, 0 failed, 1 filtered out in 1 file\n"
        );
    }

    #[test]
    fn a_file_with_errors_runs_no_test_and_says_why() {
        let files = sources(&[(
            "limits.stn",
            "\
LIMIT = 10

fn clamp(n: int) -> int:
    ret n.min(LIMIT)

test \"clamps\":
    assert clamp(20) == 10
",
        )]);
        let report = test_file(Path::new("limits.stn"), &Options::default(), &files);
        assert_eq!(report.tests, []);
        let errors = report.errors.clone().unwrap();
        assert!(
            errors.starts_with(
                "limits.stn:6:6: error[E0041]: this test reads the global 'LIMIT' (through 'clamp'), \
                 but stone test runs no top-level code"
            ),
            "{errors}"
        );
        assert_eq!(
            file_lines(&report),
            "limits.stn\n  FAILED  (the file has errors)\n"
        );
        assert!(
            summary(&[report])
                .ends_with("\n0 passed, 0 failed in 1 file, of which 1 file has errors\n")
        );
    }

    #[test]
    fn a_test_that_exits_passes_only_with_status_zero() {
        let files = sources(&[(
            "exits.stn",
            "\
use os

test \"exits with 0\":
    os.exit(0)
    assert false

test \"exits with 3\":
    print(\"leaving\")
    os.exit(3)
",
        )]);
        let report = test_file(Path::new("exits.stn"), &Options::default(), &files);
        let outcomes: Vec<&Outcome> = report.tests.iter().map(|t| &t.outcome).collect();
        assert_eq!(
            outcomes,
            [
                &Outcome::Passed,
                &Outcome::Failed("leaving\nerror: the test exited with status 3\n".to_string())
            ]
        );
    }

    #[test]
    fn a_test_build_goes_in_the_programs_build_directory() {
        let files = sources(&[("app/main.stn", ""), ("app/geo/shapes.stn", "")]);
        assert_eq!(
            test_build_path(Path::new("app/geo/shapes.stn"), Target::X64_LINUX, &files),
            Path::new("app/build/test/geo/shapes")
        );
        assert_eq!(
            test_build_path(Path::new("app/main.stn"), Target::X64_WINDOWS, &files),
            Path::new("app/build/test/main.exe")
        );
        assert_eq!(
            test_build_path(Path::new("solo/one.stn"), Target::X64_LINUX, &files),
            Path::new("solo/build/test/one")
        );
    }
}
