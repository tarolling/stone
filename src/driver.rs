//! Pipelines that wire the lexer, parser, checker, and backends together.
//!
//! For example, `interpret("print(1)\n")` lexes, parses, checks, and evaluates the source, printing `1`.

use crate::ast::Mod;
use crate::checker::{Analysis, TypeChecker};
use crate::codegen::Target;
use crate::debug;
use crate::diagnostic::{Diagnostic, Diagnostics, Severity};
use crate::interpreter::{Exit, Interpreter, Limits};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::project::{self, DEFAULT_ENTRY, Linked, MapSources, SourceMap, Sources};
use crate::repl;
use std::error::Error;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};

/// Lexes and parses source code into a module.
///
/// For example, `parse("x = 42\n")` returns a module holding one assignment, and `parse("x = \n")`
/// returns a diagnostic saying an expression was expected.
pub fn parse(source: &str) -> Result<Mod, Diagnostic> {
    let mut lexer = Lexer::new(source);
    let tokens = lexer.lex()?;

    debug!("{:?}", tokens);

    let mut parser = Parser::new(&tokens);
    parser.parse()
}

/// Parses, links, and checks the program with this entry source, given as a single string with
/// no other files, returning the module only if no errors were found.
///
/// For example, `checked("x = \n")` fails with the syntax error, while a program that parses
/// returns its module.
fn checked(source: &str) -> Result<Mod, Diagnostics> {
    load(Path::new(DEFAULT_ENTRY), source, &MapSources::default())
        .1
        .map(|(module, _)| module)
}

/// Loads, links, and checks the program whose entry file is at `entry` and holds `source`,
/// reading the modules it uses from `sources`. Returns the program's files, for rendering
/// diagnostics, and its linked module with any warnings if no errors were found.
///
/// For example, loading `main.st` holding `use util` without a `util.st` fails with
/// `no module named 'util'`.
pub fn load(
    entry: &Path,
    source: &str,
    sources: &dyn Sources,
) -> (SourceMap, Result<(Mod, Vec<Diagnostic>), Diagnostics>) {
    let Linked {
        module,
        sources,
        diagnostics,
        ..
    } = project::link(entry, source, sources);
    if has_errors(&diagnostics) {
        return (sources, Err(Diagnostics(diagnostics)));
    }
    let diagnostics = TypeChecker::new().check(&module);
    if has_errors(&diagnostics) {
        return (sources, Err(Diagnostics(diagnostics)));
    }
    (sources, Ok((module, diagnostics)))
}

fn has_errors(diagnostics: &[Diagnostic]) -> bool {
    diagnostics.iter().any(|d| d.severity == Severity::Error)
}

/// Checks a linked program, returning everything learned about it for editor tooling, with any
/// syntax or import error as one of its diagnostics.
///
/// Syntax and import errors do not stop the analysis: the statements around them are still
/// checked, so an editor can hover and jump to definitions in the rest of the program. Type
/// errors are left out until the others are fixed, since a statement that failed to parse or an
/// import that failed to resolve makes names look undefined.
pub fn analyze_linked(linked: &Linked) -> Analysis {
    let mut analysis = TypeChecker::new().analyze(&linked.module);
    if !linked.diagnostics.is_empty() {
        analysis.diagnostics = linked.diagnostics.clone();
    }
    analysis
}

/// Parses and checks source code, returning everything learned about it for editor tooling,
/// with any syntax error as one of its diagnostics. See [`analyze_linked`].
///
/// For example, `analyze("x = 1\n")` has a symbol for `x` of type `int`, and `analyze("x = \n")`
/// has only the syntax error.
pub fn analyze(source: &str) -> Analysis {
    analyze_linked(&project::link(
        Path::new(DEFAULT_ENTRY),
        source,
        &MapSources::default(),
    ))
}

/// Checks source code for errors without running it, returning every problem found.
///
/// For example, `check("x = 1\n")` returns nothing, and `check("if x\n")` returns one error
/// pointing at the end of the first line.
pub fn check(source: &str) -> Vec<Diagnostic> {
    analyze(source).diagnostics
}

/// Checks the program whose entry file is at `entry` and holds `source` without running it,
/// returning its files and every problem found in any of them.
///
/// For example, checking a `main.st` that uses a `util.st` with a syntax error returns that error,
/// whose span is in `util.st`.
pub fn check_program(
    entry: &Path,
    source: &str,
    sources: &dyn Sources,
) -> (SourceMap, Vec<Diagnostic>) {
    let linked = project::link(entry, source, sources);
    let diagnostics = analyze_linked(&linked).diagnostics;
    (linked.sources, diagnostics)
}

/// Runs source code with the tree-walking interpreter.
///
/// For example, `interpret("print(1 + 2)\n", &[])` prints `3`. `input` reads stdin, and `args`
/// returns `args`.
pub fn interpret(source: &str, args: &[String]) -> Result<(), Box<dyn Error>> {
    interpret_with_io(
        source,
        &mut std::io::BufReader::new(std::io::stdin()),
        &mut std::io::stdout(),
        args,
        Limits::DEFAULT,
    )
}

/// Runs source code with the tree-walking interpreter, printing to `out` and stopping the program
/// with an error once it exceeds `limits`.
///
/// For example, `interpret_with("print(1)\n", &mut buffer, limits)` appends `1\n` to `buffer`,
/// while `interpret_with("while 1;\n    x = 1\n", ...)` returns an error once fuel runs out.
pub fn interpret_with(
    source: &str,
    out: &mut (impl Write + Send),
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    interpret_with_io(source, &mut std::io::empty(), out, &[], limits)
}

/// Runs source code with the tree-walking interpreter, reading `input` for `input` and `eof`,
/// printing to `out`, giving `args` to `args`, and stopping the program once it exceeds `limits`.
///
/// For example, `interpret_with_io("print(input())\n", &mut "hi\n".as_bytes(), &mut buffer, &[],
/// limits)` appends `hi\n` to `buffer`.
pub fn interpret_with_io(
    source: &str,
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    args: &[String],
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    let ast = checked(source)?;
    run_module(&ast, input, out, args, limits)
}

/// Runs a checked module, such as one from [`load`], with the tree-walking interpreter. It reads
/// `input` for `input` and `eof`, prints to `out`, gives `args` to `args`, and stops the program
/// once it exceeds `limits`.
///
/// For example, running the module of `print(1)` appends `1\n` to `out`.
///
/// A program that calls `os.exit` with a status other than 0 ends with the error [`Exit`], which
/// callers report by exiting with that status, while `os.exit(0)` ends it successfully.
pub fn run_module(
    ast: &Mod,
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    args: &[String],
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    // a thread of its own, since deep recursion needs more stack than the main thread has
    let result = std::thread::scope(|scope| {
        let thread = std::thread::Builder::new()
            .stack_size(Limits::STACK_SIZE)
            .spawn_scoped(scope, || {
                let result = Interpreter::with_output(out, limits)
                    .with_input(input)
                    .with_args(args.to_vec())
                    .evaluate(ast);
                Ending::of(result)
            })
            .map_err(|e| Ending::Error(format!("could not start the interpreter: {e}")))?;
        thread
            .join()
            .unwrap_or_else(|_| Err(Ending::Error("the interpreter panicked".to_string())))
    });
    result.map_err(Ending::into_error)
}

/// How an interpreter thread stopped early, in a form that can leave the thread, since the
/// interpreter's errors are not `Send`.
enum Ending {
    /// A runtime error, by its message.
    Error(String),
    /// `os.exit` with a status other than 0.
    Exit(Exit),
}

impl Ending {
    /// Turns what a program's run returned into how it ended, treating `os.exit(0)` as success.
    fn of(result: Result<(), Box<dyn Error>>) -> Result<(), Ending> {
        let Err(error) = result else {
            return Ok(());
        };
        match error.downcast::<Exit>() {
            Ok(exit) if exit.0 == 0 => Ok(()),
            Ok(exit) => Err(Ending::Exit(*exit)),
            Err(error) => Err(Ending::Error(error.to_string())),
        }
    }

    fn into_error(self) -> Box<dyn Error> {
        match self {
            Ending::Error(message) => message.into(),
            Ending::Exit(exit) => Box::new(exit),
        }
    }
}

/// Compiles source code to a native executable for the machine stone runs on, at `output`.
///
/// For example, `compile("print(1)\n", Path::new("build/out"))` writes `build/out.s` and links
/// `build/out`.
pub fn compile(source: &str, output: &Path) -> Result<(), Box<dyn Error>> {
    compile_for(source, output, Target::host()?)
}

/// Compiles source code to a native executable for `target` at `output`. Any target can be
/// built for on any machine, since stone assembles and links the program itself.
///
/// For example, `compile_for("print(1)\n", Path::new("build/out"), Target::ARM64_LINUX)` writes
/// arm64 assembly to `build/out.s` and links it into an arm64 Linux `build/out`.
pub fn compile_for(source: &str, output: &Path, target: Target) -> Result<(), Box<dyn Error>> {
    compile_module(&checked(source)?, output, target)
}

/// Compiles a checked module, such as one from [`load`], to a native executable for `target` at
/// `output`.
///
/// For example, compiling for [`Target::ARM64_MACOS`] on an x86-64 Linux machine writes an arm64
/// macOS executable, with no cross toolchain installed.
pub fn compile_module(ast: &Mod, output: &Path, target: Target) -> Result<(), Box<dyn Error>> {
    target.generator().compile(ast, output)?;
    Ok(())
}

/// The name of the directory, next to a program's entry file, that `stone build` writes to.
pub const BUILD_DIR: &str = "build";

/// The file that marks a build directory as stone's, so `stone clean` never deletes a `build`
/// directory something else made.
pub const MARKER: &str = ".stone";

/// Returns the build directory of the program whose entry file is `entry`: `build` next to it,
/// in the same directory that is the root of the program's modules.
///
/// For example, `build_dir(Path::new("examples/basics.st"))` is `examples/build`, and
/// `build_dir(Path::new("main.st"))` is `build`.
pub fn build_dir(entry: &Path) -> PathBuf {
    entry.parent().unwrap_or(Path::new("")).join(BUILD_DIR)
}

/// Returns the name of the executable built from the entry file `entry`: the file's stem, or,
/// for a `main.st`, the name of the directory it is in, as a Cargo package is named.
///
/// For example, `examples/basics.st` is named `basics`, `tests/programs/modules/main.st` is
/// named `modules`, and `/main.st`, whose directory has no name, is named `main`.
pub fn program_name(entry: &Path) -> String {
    let stem = entry.file_stem().unwrap_or_default().to_string_lossy();
    if entry.file_name().and_then(|name| name.to_str()) != Some(DEFAULT_ENTRY) {
        return stem.into_owned();
    }
    // `main.st` alone, or `sub/../main.st`, names a directory only once resolved
    let parent = match entry.parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent,
        _ => Path::new("."),
    };
    std::fs::canonicalize(parent)
        .ok()
        .and_then(|dir| {
            dir.file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| stem.into_owned())
}

/// Returns where `stone build` writes the executable for `target` built from the entry file
/// `entry`: in the program's [`build_dir`] for the machine stone runs on, or in a directory
/// named after the target inside it for any other target, so builds for different targets
/// never overwrite each other.
///
/// For example, on an x86-64 Linux machine, `examples/basics.st` builds to
/// `examples/build/basics`, and for [`Target::ARM64_MACOS`] to
/// `examples/build/aarch64-macos/basics`.
pub fn output_path(entry: &Path, target: Target) -> PathBuf {
    let mut path = build_dir(entry);
    // an Intel Mac has no host target, so everything it builds is for another machine
    if Target::host().ok() != Some(target) {
        path.push(target.to_string());
    }
    path.push(program_name(entry));
    path
}

/// Creates the build directory `build`, if it does not exist yet, with the [`MARKER`] that lets
/// [`clean`] delete it.
///
/// For example, `prepare_build_dir(Path::new("build"))` creates `build/.stone`.
pub fn prepare_build_dir(build: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(build)?;
    let marker = build.join(MARKER);
    if !marker.exists() {
        std::fs::write(
            marker,
            "stone build made this directory, and stone clean deletes it.\n",
        )?;
    }
    Ok(())
}

/// Deletes the build directory in `dir`, returning whether there was one. A `build` directory
/// without the [`MARKER`] is an error and stays, since stone did not make it.
///
/// For example, after `stone build examples/basics.st`, `clean(Path::new("examples"))` deletes
/// `examples/build` and returns `Ok(true)`, and a second call returns `Ok(false)`.
pub fn clean(dir: &Path) -> std::io::Result<bool> {
    let build = dir.join(BUILD_DIR);
    if std::fs::symlink_metadata(&build).is_err() {
        return Ok(false);
    }
    if !build.join(MARKER).is_file() {
        return Err(std::io::Error::other(format!(
            "{} was not made by stone, so it was left alone",
            build.display()
        )));
    }
    std::fs::remove_dir_all(&build)?;
    Ok(true)
}

/// Runs an interactive session that reads entries from `input`, prints program output and
/// expression values to `out`, and prints errors, plus prompts if `prompts` is set, to `err`.
/// Modules named by `use` are read from `sources`. See [`crate::repl`].
///
/// For example, a session over the input `x = 1\nx + 1\n` prints `2` to `out`. An entry that
/// calls `os.exit` ends the session, with the error [`Exit`] unless its status is 0.
pub fn repl(
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    err: &mut (impl Write + Send),
    prompts: bool,
    sources: &(dyn Sources + Sync),
) -> Result<(), Box<dyn Error>> {
    // a thread of its own, since deep recursion needs more stack than the main thread has
    let result = std::thread::scope(|scope| {
        let thread = std::thread::Builder::new()
            .stack_size(Limits::STACK_SIZE)
            .spawn_scoped(scope, || {
                let mut interpreter =
                    Interpreter::with_output(out, Limits::DEFAULT).with_input(input);
                match repl::run(&mut interpreter, err, prompts, sources) {
                    Ok(0) => Ok(()),
                    Ok(status) => Err(Ending::Exit(Exit(status))),
                    Err(e) => Err(Ending::Error(e.to_string())),
                }
            })
            .map_err(|e| Ending::Error(format!("could not start the interpreter: {e}")))?;
        thread
            .join()
            .unwrap_or_else(|_| Err(Ending::Error("the interpreter panicked".to_string())))
    });
    result.map_err(Ending::into_error)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_program() {
        let source = r#"
x = 42
y = x + 8
ret y
"#;

        let _ = interpret(source, &[]);
    }

    /// Interprets `source` with small limits and returns what it printed, or the error message.
    ///
    /// For example, `run_limited("print(1)\n")` returns `Ok("1\n")`.
    fn run_limited(source: &str) -> Result<String, String> {
        let mut out = Vec::new();
        let limits = Limits {
            fuel: 10_000,
            max_depth: 50,
            max_calls: 20,
        };
        interpret_with(source, &mut out, limits).map_err(|e| e.to_string())?;
        Ok(String::from_utf8(out).unwrap())
    }

    #[test]
    fn print_goes_to_the_given_writer() {
        assert_eq!(run_limited("print(1, 2)\n"), Ok("1 2\n".to_string()));
    }

    #[test]
    fn infinite_loop_runs_out_of_fuel() {
        assert!(run_limited("while 1;\n    x = 1\n").is_err());
    }

    #[test]
    fn unbounded_recursion_hits_the_depth_limit() {
        assert!(run_limited("def f(n);\n    ret f(n)\nf(1)\n").is_err());
    }

    #[test]
    fn deep_expressions_inside_recursion_hit_the_depth_limit() {
        let source = format!("def f(n);\n    ret {}f(n)\nf(1)\n", "-".repeat(40));
        assert!(run_limited(&source).is_err());
    }

    #[test]
    fn division_by_zero_is_an_error() {
        assert!(run_limited("print(1 / 0)\n").is_err());
    }

    #[test]
    fn dividing_the_minimum_by_negative_one_is_an_error() {
        // idiv traps on this in compiled code too
        assert!(run_limited("x = 0 - 9223372036854775807 - 1\nprint(x / -1)\n").is_err());
    }

    #[test]
    fn integer_overflow_wraps() {
        assert_eq!(
            run_limited("print(9223372036854775807 + 1)\n"),
            Ok("-9223372036854775808\n".to_string())
        );
    }

    #[test]
    fn ret_inside_if_returns_from_the_function() {
        let source = "def f(x);\n    if x;\n        ret 1\n    ret 0\nprint(f(5))\n";
        assert_eq!(run_limited(source), Ok("1\n".to_string()));
    }

    #[test]
    fn while_reevaluates_its_test() {
        let source = "i = 0\nwhile i - 2;\n    i = i + 1\nprint(i)\n";
        assert_eq!(run_limited(source), Ok("2\n".to_string()));
    }

    #[test]
    fn check_accepts_a_valid_program() {
        assert_eq!(check("x = 1\nprint(x)\n"), vec![]);
    }

    #[test]
    fn check_reports_a_syntax_error_with_a_caret() {
        let source = "x = 1\nif x\n    y = 1\n";
        let rendered: Vec<String> = check(source)
            .iter()
            .map(|d| d.render("main.st", source))
            .collect();
        assert_eq!(
            rendered,
            ["main.st:2:5: error: expected ';', found end of line\n  |\n2 | if x\n  |     ^\n"]
        );
    }

    #[test]
    fn check_reports_a_lex_error_under_the_whole_literal() {
        let source = "x = 99999999999999999999\n";
        let rendered: Vec<String> = check(source)
            .iter()
            .map(|d| d.render("big.st", source))
            .collect();
        assert_eq!(
            rendered,
            [concat!(
                "big.st:1:5: error: integer literal 99999999999999999999 is too large\n",
                "  |\n",
                "1 | x = 99999999999999999999\n",
                "  |     ^^^^^^^^^^^^^^^^^^^^\n",
            )]
        );
    }

    #[test]
    fn analyze_reports_syntax_errors_as_diagnostics() {
        let analysis = analyze("x = \n");
        assert_eq!(analysis.diagnostics.len(), 1);
        assert_eq!(
            analysis.diagnostics[0].message,
            "expected an expression, found end of line"
        );
    }

    #[test]
    fn analyze_keeps_what_parses_around_syntax_errors() {
        let analysis = analyze("x = 1\ny = \nz = x + 1\n");
        let messages: Vec<&str> = analysis
            .diagnostics
            .iter()
            .map(|d| d.message.as_str())
            .collect();
        assert_eq!(messages, ["expected an expression, found end of line"]);
        let names: Vec<&str> = analysis.symbols.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(names, ["x", "z"]);
    }

    #[test]
    fn analyze_hides_type_errors_while_there_are_syntax_errors() {
        // `f` failed to parse, so calling it would otherwise be an undefined function
        let analysis = analyze("def f(;\n    ret 1\nprint(f())\n");
        assert_eq!(analysis.diagnostics.len(), 1);
    }

    #[test]
    fn check_reports_every_syntax_error() {
        assert_eq!(check("x = \ny = \n").len(), 2);
    }

    #[test]
    fn analyze_returns_types_and_symbols() {
        let analysis = analyze("x = 1\n");
        assert_eq!(analysis.diagnostics, []);
        assert_eq!(analysis.symbols[0].name, "x");
    }

    #[test]
    fn parameters_do_not_overwrite_caller_variables() {
        let source = "def f(a);\n    ret a\na = 7\nprint(f(1))\nprint(a)\n";
        assert_eq!(run_limited(source), Ok("1\n7\n".to_string()));
    }

    /// Interprets `source` reading `input` as stdin with `args`, returning what it printed or the
    /// error message.
    ///
    /// For example, `run_io("print(input())\n", "a\n", &[])` returns `Ok("a\n")`.
    fn run_io(source: &str, input: &str, args: &[&str]) -> Result<String, String> {
        let mut out = Vec::new();
        let args: Vec<String> = args.iter().map(|a| a.to_string()).collect();
        interpret_with_io(
            source,
            &mut input.as_bytes(),
            &mut out,
            &args,
            Limits::DEFAULT,
        )
        .map_err(|e| e.to_string())?;
        Ok(String::from_utf8(out).unwrap())
    }

    #[test]
    fn os_functions_run_in_the_interpreter() {
        let platform = crate::stdlib::os::platform();
        assert_eq!(
            run_io(
                "use os\nprint(os.platform(), os.has_env(\"PATH\"), os.env(\"STONE_UNSET\") == \"\")\n",
                "",
                &[]
            ),
            Ok(format!("{platform} true true\n"))
        );
        let source = "use os\nprint(os.pid() > 0, os.cpu_count() > 0, os.time() > 0.0)\n\
                      print(os.clock() >= 0.0, os.hostname() == os.hostname(), os.cwd() != \"\")\n";
        assert_eq!(
            run_io(source, "", &[]),
            Ok("true true true\ntrue true true\n".to_string())
        );
    }

    #[test]
    fn exit_with_status_zero_ends_the_program_successfully() {
        let source =
            "use os\n\ndef stop(xs);\n    os.exit(0)\n\nprint(1)\nstop([\"a\"])\nprint(2)\n";
        assert_eq!(run_io(source, "", &[]), Ok("1\n".to_string()));
        // only the low 8 bits are the status
        let source = "use os\nprint(1)\nos.exit(256)\nprint(2)\n";
        assert_eq!(run_io(source, "", &[]), Ok("1\n".to_string()));
    }

    #[test]
    fn exit_with_another_status_is_an_exit_error() {
        let mut out = Vec::new();
        let error = interpret_with_io(
            "use os\nprint(1)\nos.exit(-1)\nprint(2)\n",
            &mut std::io::empty(),
            &mut out,
            &[],
            Limits::DEFAULT,
        )
        .unwrap_err();
        assert_eq!(error.downcast_ref::<Exit>(), Some(&Exit(255)));
        assert_eq!(out, b"1\n");
    }

    #[test]
    fn input_reads_lines_until_eof() {
        let source =
            "while not eof();\n    print(\"[\" + input() + \"]\")\nprint(input() == \"\")\n";
        assert_eq!(
            run_io(source, "a\n\nb c\r\nlast", &[]),
            Ok("[a]\n[]\n[b c\r]\n[last]\ntrue\n".to_string())
        );
    }

    #[test]
    fn input_writes_its_prompt_first() {
        assert_eq!(
            run_io("x = input(\"name? \")\nprint(x)\n", "Ada\n", &[]),
            Ok("name? Ada\n".to_string())
        );
    }

    #[test]
    fn interpret_with_has_no_input() {
        assert_eq!(
            run_limited("print(eof(), input() == \"\")\n"),
            Ok("true true\n".to_string())
        );
    }

    #[test]
    fn args_are_the_given_arguments() {
        assert_eq!(
            run_io("print(args())\n", "", &["a", "-b", "c d"]),
            Ok("['a', '-b', 'c d']\n".to_string())
        );
        assert_eq!(run_io("print(args())\n", "", &[]), Ok("[]\n".to_string()));
    }

    #[test]
    fn strs_parse_and_convert() {
        let source =
            "print(int(\" 42 \") + 1, float(\"2.5\") * 2.0, str(1.0) + str(true) + str(-3))\n";
        assert_eq!(run_limited(source), Ok("43 5.0 1.0true-3\n".to_string()));
        assert_eq!(
            run_limited("print(int(\"x\"))\n"),
            Err("invalid literal for int() with base 10: 'x'".to_string())
        );
        assert_eq!(
            run_limited("print(float(\"\"))\n"),
            Err("could not convert string to float: ''".to_string())
        );
    }

    /// Makes an empty directory named `name` under the system's temporary directory, unique to
    /// this process.
    ///
    /// For example, `scratch("clean")` returns `/tmp/stone-driver-1234/clean`.
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("stone-driver-{}", std::process::id()))
            .join(name);
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn the_build_directory_sits_next_to_the_entry_file() {
        assert_eq!(
            build_dir(Path::new("examples/basics.st")),
            Path::new("examples/build")
        );
        assert_eq!(build_dir(Path::new("main.st")), Path::new("build"));
        assert_eq!(build_dir(Path::new("/abs/p.st")), Path::new("/abs/build"));
    }

    #[test]
    fn programs_are_named_after_their_entry_file() {
        assert_eq!(program_name(Path::new("examples/basics.st")), "basics");
        let dir = scratch("modules");
        std::fs::write(dir.join("main.st"), "").unwrap();
        assert_eq!(program_name(&dir.join("main.st")), "modules");
        // a relative main.st takes the name of the directory it resolves to
        let nested = dir.join("sub").join("main.st");
        std::fs::create_dir_all(nested.parent().unwrap()).unwrap();
        std::fs::write(&nested, "").unwrap();
        assert_eq!(program_name(&dir.join("sub/../main.st")), "modules");
        assert_eq!(program_name(Path::new("/main.st")), "main");
    }

    #[test]
    fn other_targets_build_into_directories_of_their_own() {
        let host = Target::host().unwrap();
        assert_eq!(
            output_path(Path::new("examples/basics.st"), host),
            Path::new("examples/build/basics")
        );
        let other = if host == Target::ARM64_MACOS {
            Target::X64_LINUX
        } else {
            Target::ARM64_MACOS
        };
        assert_eq!(
            output_path(Path::new("examples/basics.st"), other),
            Path::new("examples/build")
                .join(other.to_string())
                .join("basics")
        );
        let leveled: Target = "x86_64v3-linux".parse().unwrap();
        assert_eq!(
            output_path(Path::new("p.st"), leveled),
            Path::new("build/x86_64v3-linux/p")
        );
    }

    #[test]
    fn clean_removes_only_a_build_directory_stone_made() {
        let dir = scratch("clean");
        assert!(!clean(&dir).unwrap());

        prepare_build_dir(&dir.join("build")).unwrap();
        assert!(dir.join("build").join(MARKER).exists());
        std::fs::create_dir_all(dir.join("build/aarch64-macos")).unwrap();
        std::fs::write(dir.join("build/aarch64-macos/p"), "").unwrap();
        assert!(clean(&dir).unwrap());
        assert!(!dir.join("build").exists());

        std::fs::create_dir_all(dir.join("build")).unwrap();
        std::fs::write(dir.join("build/keep.txt"), "mine").unwrap();
        let err = clean(&dir).unwrap_err();
        assert!(err.to_string().contains("not made by stone"), "{err}");
        assert!(dir.join("build/keep.txt").exists());
    }

    #[test]
    fn strip_and_split_work_on_strs() {
        let source = "s = \" a,b  c \"\nprint(s.strip(), s.split(), s.split(\",\"))\n";
        assert_eq!(
            run_limited(source),
            Ok("a,b  c ['a,b', 'c'] [' a', 'b  c ']\n".to_string())
        );
        assert_eq!(
            run_limited("print(\"a\".split(\"\"))\n"),
            Err("empty separator".to_string())
        );
    }
}
