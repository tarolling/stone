//! Shared pieces of the benchmark runner: finding benchmark programs, summarizing timings, and
//! formatting the results table.
//!
//! Each benchmark is a set of files in `bench/programs/` sharing a stem, such as `fib.stn`,
//! `fib.c`, `fib.rs`, `fib.py`, and `fib.out`, where the `.out` file holds what all of them must
//! print. A benchmark may also have a `.args` file, such as `fib.args`, whose lines are the
//! command-line arguments every version runs with, a tuned Rust version, such as
//! `matmul.fast.rs`, and, for arithmetic on lists, a numpy version, such as `vector.np.py`.

use std::path::{Path, PathBuf};
use std::time::Duration;

/// The file extensions every benchmark needs besides its `.stn` source.
pub const SIBLINGS: [&str; 4] = ["c", "rs", "py", "out"];

/// One benchmark, written once in each language.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    /// The shared file stem, such as `fib`.
    pub name: String,
    /// The directory holding the program's files.
    pub dir: PathBuf,
}

impl Program {
    /// Returns the path of the program's file with the given extension.
    ///
    /// For example, `fib` in `bench/programs` with `"c"` gives `bench/programs/fib.c`.
    pub fn file(&self, extension: &str) -> PathBuf {
        self.dir.join(format!("{}.{extension}", self.name))
    }

    /// Returns whether the program has a numpy version, `<name>.np.py`.
    pub fn has_numpy(&self) -> bool {
        self.file("np.py").is_file()
    }

    /// Returns whether the program has a tuned Rust version, `<name>.fast.rs`, written the way an
    /// expert would for speed (arenas, flat arrays, unchecked access behind a proof), where
    /// `<name>.rs` is idiomatic safe Rust.
    pub fn has_fast_rust(&self) -> bool {
        self.file("fast.rs").is_file()
    }

    /// Returns the arguments every version of the program runs with, one per line of
    /// `<name>.args`, or none if there is no such file.
    ///
    /// For example, a `fib.args` holding `35` gives `["35"]`.
    pub fn args(&self) -> Result<Vec<String>, String> {
        let path = self.file("args");
        if !path.is_file() {
            return Ok(Vec::new());
        }
        let text =
            std::fs::read_to_string(&path).map_err(|e| format!("{}: {e}", path.display()))?;
        Ok(text.lines().map(str::to_string).collect())
    }
}

/// Returns every benchmark in `dir`, sorted by name, or an error naming the first missing file.
///
/// For example, a directory holding `fib.stn`, `fib.c`, `fib.rs`, `fib.py`, and `fib.out` gives
/// one `Program` named `fib`, and removing `fib.py` makes it an error.
pub fn discover(dir: &Path) -> Result<Vec<Program>, String> {
    let entries = std::fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))?;
    let mut programs = Vec::new();
    for entry in entries {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.extension().is_none_or(|ext| ext != "stn") {
            continue;
        }
        let Some(name) = path.file_stem().and_then(|stem| stem.to_str()) else {
            continue;
        };
        let program = Program {
            name: name.to_string(),
            dir: dir.to_path_buf(),
        };
        for extension in SIBLINGS {
            let sibling = program.file(extension);
            if !sibling.is_file() {
                return Err(format!("{} is missing", sibling.display()));
            }
        }
        programs.push(program);
    }
    programs.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(programs)
}

/// Returns how many checks inside loops the `stone stats total` line of `stderr` reports, as a
/// build with `STONE_STATS` set writes it.
///
/// For example, `stone stats total: checks=12/3 index=5/2 ...` gives `Some(3)`.
pub fn checks_in_loops(stderr: &str) -> Option<usize> {
    let line = stderr
        .lines()
        .find(|line| line.starts_with("stone stats total: "))?;
    let counts = line.split_whitespace().find_map(|word| word.strip_prefix("checks="))?;
    counts.split_once('/')?.1.parse().ok()
}

/// The flags every Rust version is built with: full optimization in one codegen unit, aborting on
/// panic, and the baseline processor, as stone builds for by default.
pub const RUSTC_FLAGS: [&str; 8] = [
    "--edition",
    "2024",
    "-C",
    "opt-level=3",
    "-C",
    "codegen-units=1",
    "-C",
    "panic=abort",
];

/// Returns the smallest of `times`, or zero if there are none.
///
/// For example, the minimum of 3 ms, 1 ms, and 2 ms is 1 ms.
pub fn min(times: &[Duration]) -> Duration {
    times.iter().copied().min().unwrap_or_default()
}

/// Returns the median of `times`, averaging the middle two when there is an even number, or zero
/// if there are none.
///
/// For example, the median of 3 ms, 1 ms, and 2 ms is 2 ms, and of 1 ms and 4 ms is 2.5 ms.
pub fn median(times: &[Duration]) -> Duration {
    let mut sorted = times.to_vec();
    sorted.sort();
    let middle = sorted.len() / 2;
    match sorted.len() {
        0 => Duration::ZERO,
        len if len % 2 == 1 => sorted[middle],
        _ => (sorted[middle - 1] + sorted[middle]) / 2,
    }
}

/// Returns the geometric mean of `values`, which suits ratios since it treats 2x faster and 2x
/// slower symmetrically.
///
/// For example, the geometric mean of 2 and 8 is 4.
pub fn geometric_mean(values: &[f64]) -> f64 {
    let log_sum: f64 = values.iter().map(|v| v.ln()).sum();
    (log_sum / values.len() as f64).exp()
}

/// A way of running a benchmark, in the order the results table lists them.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Implementation {
    /// The C version built with `gcc -O2`.
    COptimized,
    /// The C version built with `gcc -O0`.
    CUnoptimized,
    /// The stone version built with `stone build`.
    Stone,
    /// The Python version run with `python3`.
    Python,
}

impl Implementation {
    /// Every implementation, in table order.
    pub const ALL: [Implementation; 4] = [
        Implementation::COptimized,
        Implementation::CUnoptimized,
        Implementation::Stone,
        Implementation::Python,
    ];

    /// Returns the name used in the table and in error messages, such as `C -O2`.
    pub fn label(self) -> &'static str {
        match self {
            Implementation::COptimized => "C -O2",
            Implementation::CUnoptimized => "C -O0",
            Implementation::Stone => "stone",
            Implementation::Python => "Python",
        }
    }
}

/// One benchmark's summarized time under each implementation, indexed like
/// [`Implementation::ALL`].
#[derive(Debug, Clone, PartialEq)]
pub struct Row {
    /// The benchmark's name, such as `fib`.
    pub name: String,
    /// The summarized time for each implementation, in [`Implementation::ALL`] order.
    pub times: [Duration; 4],
    /// The summarized time of the numpy version, if the benchmark has one and numpy ran.
    pub numpy: Option<Duration>,
    /// The summarized time of the stone version under `stone run`, the interpreter, if it ran.
    pub interpreter: Option<Duration>,
    /// The summarized time of the idiomatic Rust version, if rustc was found.
    pub rust: Option<Duration>,
    /// The summarized time of the tuned Rust version, if the benchmark has one and rustc was
    /// found.
    pub rust_fast: Option<Duration>,
    /// How many checks the stone version still makes inside loops, counted in its optimized IR
    /// (see `stone::codegen::ir::stats`), if they were asked for.
    pub checks: Option<usize>,
}

impl Row {
    /// Returns a row of `times` with nothing optional measured.
    pub fn new(name: &str, times: [Duration; 4]) -> Row {
        Row {
            name: name.to_string(),
            times,
            numpy: None,
            interpreter: None,
            rust: None,
            rust_fast: None,
            checks: None,
        }
    }

    /// Returns the fastest Rust time: the tuned version's if there is one, else the idiomatic
    /// one's.
    pub fn best_rust(&self) -> Option<Duration> {
        match (self.rust, self.rust_fast) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (a, b) => a.or(b),
        }
    }
}

impl Row {
    /// Returns the time for `implementation`.
    pub fn time(&self, implementation: Implementation) -> Duration {
        let index = Implementation::ALL
            .iter()
            .position(|&i| i == implementation)
            .expect("every implementation is in ALL");
        self.times[index]
    }
}

/// Formats `rows` as a Markdown table of times in milliseconds, with the ratios of stone to
/// C -O2 and of Python to stone, and a final row holding the geometric mean of each ratio. If any
/// row has a numpy time, the table also has its time and the ratio of numpy to stone, left blank
/// for the rows without one. If any row has a Rust time, the table has the idiomatic and tuned
/// Rust times and the ratios of stone to idiomatic Rust and to the best Rust. If any row has a
/// count of checks, the table has it. If any row has an interpreter time, the table ends with it
/// and the ratio of the interpreter to Python, how many times slower `stone run` is.
///
/// For example, a `fib` row of 10, 30, 50, and 400 ms gives
/// `| fib | 10.0 | 30.0 | 50.0 | 400.0 | 5.0x | 8.0x |`.
pub fn format_table(rows: &[Row]) -> String {
    let with_numpy = rows.iter().any(|row| row.numpy.is_some());
    let with_rust = rows.iter().any(|row| row.rust.is_some());
    let with_checks = rows.iter().any(|row| row.checks.is_some());
    let with_interpreter = rows.iter().any(|row| row.interpreter.is_some());
    let mut header = vec!["program".to_string()];
    for implementation in Implementation::ALL {
        header.push(format!("{} (ms)", implementation.label()));
    }
    if with_numpy {
        header.push("numpy (ms)".into());
    }
    if with_rust {
        header.extend(["Rust (ms)".into(), "Rust fast (ms)".into()]);
    }
    header.push("stone / C -O2".into());
    if with_rust {
        header.extend(["stone / Rust".into(), "stone / best Rust".into()]);
    }
    header.push("Python / stone".into());
    if with_numpy {
        header.push("numpy / stone".into());
    }
    if with_checks {
        header.push("checks in loops".into());
    }
    if with_interpreter {
        header.extend(["stone run (ms)".into(), "stone run / Python".into()]);
    }
    let mut table = format!("| {} |\n|---|", header.join(" | "));
    table.push_str(&"---:|".repeat(header.len() - 1));
    table.push('\n');

    let ms = |time: Duration| format!("{:.1}", time.as_secs_f64() * 1_000.0);
    let optional = |time: Option<Duration>| time.map(ms).unwrap_or_default();
    let ratio = |r: Option<f64>| r.map(|r| format!("{r:.1}x")).unwrap_or_default();
    let mut means: Vec<Vec<f64>> = vec![Vec::new(); header.len()];
    for row in rows {
        let stone = row.time(Implementation::Stone).as_secs_f64();
        let python_time = row.time(Implementation::Python).as_secs_f64();
        // each cell, and the ratio it adds to its column's geometric mean
        let mut cells: Vec<(String, Option<f64>)> = vec![(row.name.clone(), None)];
        for time in row.times {
            cells.push((ms(time), None));
        }
        let numpy = row.numpy.map(|time| time.as_secs_f64() / stone);
        if with_numpy {
            cells.push((optional(row.numpy), None));
        }
        if with_rust {
            cells.push((optional(row.rust), None));
            cells.push((optional(row.rust_fast), None));
        }
        let c = stone / row.time(Implementation::COptimized).as_secs_f64();
        cells.push((ratio(Some(c)), Some(c)));
        if with_rust {
            let rust = row.rust.map(|time| stone / time.as_secs_f64());
            let best = row.best_rust().map(|time| stone / time.as_secs_f64());
            cells.push((ratio(rust), rust));
            cells.push((ratio(best), best));
        }
        let python = python_time / stone;
        cells.push((ratio(Some(python)), Some(python)));
        if with_numpy {
            cells.push((ratio(numpy), numpy));
        }
        if with_checks {
            cells.push((row.checks.map(|n| n.to_string()).unwrap_or_default(), None));
        }
        if with_interpreter {
            let interpreter = row.interpreter.map(|time| time.as_secs_f64() / python_time);
            cells.push((optional(row.interpreter), None));
            cells.push((ratio(interpreter), interpreter));
        }
        for (column, (_, value)) in cells.iter().enumerate() {
            means[column].extend(*value);
        }
        table.push_str(&table_line(cells.iter().map(|(text, _)| text.as_str())));
    }
    let mut last = vec!["geometric mean".to_string()];
    for values in &means[1..] {
        last.push(if values.is_empty() {
            String::new()
        } else {
            format!("{:.1}x", geometric_mean(values))
        });
    }
    table.push_str(&table_line(last.iter().map(String::as_str)));
    table
}

/// Returns one line of a Markdown table holding `cells`, writing an empty cell as `| |`.
///
/// For example, the cells `fib`, ``, and `2.0x` give `| fib | | 2.0x |`.
fn table_line<'a>(cells: impl Iterator<Item = &'a str>) -> String {
    let mut line = String::from("|");
    for cell in cells {
        if cell.is_empty() {
            line.push_str(" |");
        } else {
            line.push_str(&format!(" {cell} |"));
        }
    }
    line.push('\n');
    line
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ms(millis: u64) -> Duration {
        Duration::from_millis(millis)
    }

    /// Creates an empty scratch directory for one test under the crate's target directory.
    fn scratch(test: &str) -> PathBuf {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("target")
            .join("test-scratch")
            .join(test);
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn touch(dir: &Path, names: &[&str]) {
        for name in names {
            std::fs::write(dir.join(name), "").unwrap();
        }
    }

    #[test]
    fn min_picks_the_smallest() {
        assert_eq!(min(&[ms(3), ms(1), ms(2)]), ms(1));
        assert_eq!(min(&[]), Duration::ZERO);
    }

    #[test]
    fn median_of_odd_and_even_counts() {
        assert_eq!(median(&[ms(3), ms(1), ms(2)]), ms(2));
        assert_eq!(median(&[ms(4), ms(1)]), Duration::from_micros(2_500));
        assert_eq!(median(&[]), Duration::ZERO);
    }

    #[test]
    fn geometric_mean_of_ratios() {
        assert!((geometric_mean(&[2.0, 8.0]) - 4.0).abs() < 1e-9);
        assert!((geometric_mean(&[5.0]) - 5.0).abs() < 1e-9);
    }

    #[test]
    fn discover_finds_complete_programs_in_order() {
        let dir = scratch("discover_complete");
        touch(
            &dir,
            &[
                "sieve.stn",
                "sieve.c",
                "sieve.rs",
                "sieve.py",
                "sieve.out",
                "fib.stn",
                "fib.c",
                "fib.rs",
                "fib.py",
                "fib.out",
                "notes.txt",
            ],
        );
        let names: Vec<String> = discover(&dir)
            .unwrap()
            .into_iter()
            .map(|p| p.name)
            .collect();
        assert_eq!(names, ["fib", "sieve"]);
    }

    #[test]
    fn discover_reports_a_missing_sibling() {
        let dir = scratch("discover_missing");
        touch(&dir, &["fib.stn", "fib.c", "fib.rs", "fib.out"]);
        let error = discover(&dir).unwrap_err();
        assert!(error.contains("fib.py"), "{error}");
    }

    #[test]
    fn table_lists_times_ratios_and_geometric_means() {
        let rows = [
            Row {
                name: "fib".into(),
                times: [ms(10), ms(30), ms(50), ms(400)],
                numpy: None,
                interpreter: None,
                rust: None,
                rust_fast: None,
                checks: None,
            },
            Row {
                name: "sieve".into(),
                times: [ms(20), ms(40), ms(400), ms(1_600)],
                numpy: None,
                interpreter: None,
                rust: None,
                rust_fast: None,
                checks: None,
            },
        ];
        let expected = "\
| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | stone / C -O2 | Python / stone |
|---|---:|---:|---:|---:|---:|---:|
| fib | 10.0 | 30.0 | 50.0 | 400.0 | 5.0x | 8.0x |
| sieve | 20.0 | 40.0 | 400.0 | 1600.0 | 20.0x | 4.0x |
| geometric mean | | | | | 10.0x | 5.7x |
";
        assert_eq!(format_table(&rows), expected);
    }

    #[test]
    fn table_adds_numpy_columns_when_a_benchmark_has_numpy_times() {
        let rows = [
            Row {
                name: "fib".into(),
                times: [ms(10), ms(30), ms(50), ms(400)],
                numpy: None,
                interpreter: None,
                rust: None,
                rust_fast: None,
                checks: None,
            },
            Row {
                name: "vector".into(),
                times: [ms(20), ms(40), ms(25), ms(1_600)],
                numpy: Some(ms(50)),
                interpreter: None,
                rust: None,
                rust_fast: None,
                checks: None,
            },
        ];
        let expected = "\
| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | numpy (ms) | stone / C -O2 | Python / stone | numpy / stone |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| fib | 10.0 | 30.0 | 50.0 | 400.0 | | 5.0x | 8.0x | |
| vector | 20.0 | 40.0 | 25.0 | 1600.0 | 50.0 | 1.2x | 64.0x | 2.0x |
| geometric mean | | | | | | 2.5x | 22.6x | 2.0x |
";
        assert_eq!(format_table(&rows), expected);
    }

    #[test]
    fn table_adds_interpreter_columns_when_the_interpreter_ran() {
        let rows = [Row {
            name: "fib".into(),
            times: [ms(10), ms(30), ms(50), ms(400)],
            numpy: None,
            interpreter: Some(ms(2_000)),
            rust: None,
            rust_fast: None,
            checks: None,
        }];
        let expected = "\
| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | stone / C -O2 | Python / stone | stone run (ms) | stone run / Python |
|---|---:|---:|---:|---:|---:|---:|---:|---:|
| fib | 10.0 | 30.0 | 50.0 | 400.0 | 5.0x | 8.0x | 2000.0 | 5.0x |
| geometric mean | | | | | 5.0x | 8.0x | | 5.0x |
";
        assert_eq!(format_table(&rows), expected);
    }

    #[test]
    fn table_adds_rust_and_checks_columns_when_they_were_measured() {
        let mut fib = Row::new("fib", [ms(10), ms(30), ms(50), ms(400)]);
        fib.rust = Some(ms(20));
        fib.checks = Some(2);
        let mut matmul = Row::new("matmul", [ms(20), ms(40), ms(80), ms(1_600)]);
        matmul.rust = Some(ms(40));
        matmul.rust_fast = Some(ms(20));
        matmul.checks = Some(0);
        let expected = "\
| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | Rust (ms) | Rust fast (ms) | stone / C -O2 | stone / Rust | stone / best Rust | Python / stone | checks in loops |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| fib | 10.0 | 30.0 | 50.0 | 400.0 | 20.0 | | 5.0x | 2.5x | 2.5x | 8.0x | 2 |
| matmul | 20.0 | 40.0 | 80.0 | 1600.0 | 40.0 | 20.0 | 4.0x | 2.0x | 4.0x | 20.0x | 0 |
| geometric mean | | | | | | | 4.5x | 2.2x | 3.2x | 12.6x | |
";
        assert_eq!(format_table(&[fib, matmul]), expected);
    }

    #[test]
    fn checks_in_loops_come_from_the_total_line() {
        let stderr = "stone stats f: checks=4/4 index=4/4\n\
                      stone stats total: checks=12/3 index=5/2 division=0/0\nCompiled out\n";
        assert_eq!(checks_in_loops(stderr), Some(3));
        assert_eq!(checks_in_loops("Compiled out\n"), None);
    }

    #[test]
    fn a_tuned_rust_version_is_optional() {
        let dir = scratch("discover_fast_rust");
        touch(&dir, &["fib.stn", "fib.c", "fib.rs", "fib.py", "fib.out"]);
        touch(
            &dir,
            &["sort.stn", "sort.c", "sort.rs", "sort.fast.rs", "sort.py", "sort.out"],
        );
        let programs = discover(&dir).unwrap();
        let fast: Vec<bool> = programs.iter().map(Program::has_fast_rust).collect();
        assert_eq!(fast, [false, true]);
    }

    #[test]
    fn a_numpy_version_is_optional() {
        let dir = scratch("discover_numpy");
        touch(
            &dir,
            &["fib.stn", "fib.c", "fib.rs", "fib.py", "fib.out", "vector.stn"],
        );
        touch(
            &dir,
            &["vector.c", "vector.rs", "vector.py", "vector.np.py", "vector.out"],
        );
        let programs = discover(&dir).unwrap();
        let numpy: Vec<bool> = programs.iter().map(Program::has_numpy).collect();
        assert_eq!(numpy, [false, true]);
    }

    #[test]
    fn args_are_read_one_per_line() {
        let dir = scratch("args_lines");
        touch(&dir, &["fib.stn", "fib.c", "fib.rs", "fib.py", "fib.out"]);
        std::fs::write(dir.join("fib.args"), "35\n10\n").unwrap();
        let programs = discover(&dir).unwrap();
        assert_eq!(programs[0].args().unwrap(), ["35", "10"]);
    }

    #[test]
    fn args_are_optional() {
        let dir = scratch("args_optional");
        touch(&dir, &["fib.stn", "fib.c", "fib.rs", "fib.py", "fib.out"]);
        let programs = discover(&dir).unwrap();
        assert_eq!(programs[0].args().unwrap(), Vec::<String>::new());
    }

    /// Every benchmark must stay valid stone, so a language change that breaks one fails here
    /// rather than partway through a long benchmark run.
    #[test]
    fn every_benchmark_passes_the_checker() {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("programs");
        let programs = discover(&dir).unwrap();
        assert!(!programs.is_empty(), "no benchmarks found");
        for program in programs {
            let source = std::fs::read_to_string(program.file("stn")).unwrap();
            let diagnostics = stone::driver::check(&source);
            assert!(diagnostics.is_empty(), "{}: {diagnostics:?}", program.name);
        }
    }
}
