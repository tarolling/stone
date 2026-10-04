//! Shared pieces of the benchmark runner: finding benchmark programs, summarizing timings, and
//! formatting the results table.
//!
//! Each benchmark is a set of files in `bench/programs/` sharing a stem, such as `fib.st`,
//! `fib.c`, `fib.py`, and `fib.out`, where the `.out` file holds what all three must print.

use std::path::{Path, PathBuf};
use std::time::Duration;

/// The file extensions every benchmark needs besides its `.st` source.
pub const SIBLINGS: [&str; 3] = ["c", "py", "out"];

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
}

/// Returns every benchmark in `dir`, sorted by name, or an error naming the first missing file.
///
/// For example, a directory holding `fib.st`, `fib.c`, `fib.py`, and `fib.out` gives one
/// `Program` named `fib`, and removing `fib.py` makes it an error.
pub fn discover(dir: &Path) -> Result<Vec<Program>, String> {
    let entries = std::fs::read_dir(dir).map_err(|e| format!("{}: {e}", dir.display()))?;
    let mut programs = Vec::new();
    for entry in entries {
        let path = entry.map_err(|e| e.to_string())?.path();
        if path.extension().is_none_or(|ext| ext != "st") {
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
/// C -O2 and of Python to stone, and a final row holding the geometric mean of each ratio.
///
/// For example, a `fib` row of 10, 30, 50, and 400 ms gives
/// `| fib | 10.0 | 30.0 | 50.0 | 400.0 | 5.0x | 8.0x |`.
pub fn format_table(rows: &[Row]) -> String {
    let mut table = String::from("| program |");
    for implementation in Implementation::ALL {
        table.push_str(&format!(" {} (ms) |", implementation.label()));
    }
    table.push_str(" stone / C -O2 | Python / stone |\n");
    table.push_str("|---|");
    table.push_str(&"---:|".repeat(Implementation::ALL.len() + 2));
    table.push('\n');

    let mut stone_vs_c = Vec::new();
    let mut python_vs_stone = Vec::new();
    for row in rows {
        table.push_str(&format!("| {} |", row.name));
        for time in row.times {
            table.push_str(&format!(" {:.1} |", time.as_secs_f64() * 1_000.0));
        }
        let stone = row.time(Implementation::Stone).as_secs_f64();
        let c = stone / row.time(Implementation::COptimized).as_secs_f64();
        let python = row.time(Implementation::Python).as_secs_f64() / stone;
        table.push_str(&format!(" {c:.1}x | {python:.1}x |\n"));
        stone_vs_c.push(c);
        python_vs_stone.push(python);
    }
    table.push_str("| geometric mean |");
    table.push_str(&" |".repeat(Implementation::ALL.len()));
    table.push_str(&format!(
        " {:.1}x | {:.1}x |\n",
        geometric_mean(&stone_vs_c),
        geometric_mean(&python_vs_stone)
    ));
    table
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
                "sieve.st",
                "sieve.c",
                "sieve.py",
                "sieve.out",
                "fib.st",
                "fib.c",
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
        touch(&dir, &["fib.st", "fib.c", "fib.out"]);
        let error = discover(&dir).unwrap_err();
        assert!(error.contains("fib.py"), "{error}");
    }

    #[test]
    fn table_lists_times_ratios_and_geometric_means() {
        let rows = [
            Row {
                name: "fib".into(),
                times: [ms(10), ms(30), ms(50), ms(400)],
            },
            Row {
                name: "sieve".into(),
                times: [ms(20), ms(40), ms(400), ms(1_600)],
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

    /// Every benchmark must stay valid stone, so a language change that breaks one fails here
    /// rather than partway through a long benchmark run.
    #[test]
    fn every_benchmark_passes_the_checker() {
        let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("programs");
        let programs = discover(&dir).unwrap();
        assert!(!programs.is_empty(), "no benchmarks found");
        for program in programs {
            let source = std::fs::read_to_string(program.file("st")).unwrap();
            let diagnostics = stone::driver::check(&source);
            assert!(diagnostics.is_empty(), "{}: {diagnostics:?}", program.name);
        }
    }
}
