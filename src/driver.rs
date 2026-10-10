//! Pipelines that wire the lexer, parser, checker, and backends together.
//!
//! For example, `interpret("print(1)\n")` lexes, parses, checks, and evaluates the source, printing `1`.

use crate::ast::Mod;
use crate::checker::{Analysis, TypeChecker};
use crate::codegen::Architecture;
use crate::debug;
use crate::diagnostic::{Diagnostic, Diagnostics, Severity};
use crate::interpreter::{Exit, Interpreter, Limits};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::project::{self, DEFAULT_ENTRY, Linked, MapSources, SourceMap, Sources};
use crate::repl;
use std::error::Error;
use std::io::{BufRead, Write};
use std::path::Path;

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
    compile_for(source, output, Architecture::host())
}

/// Compiles source code to a native executable for `arch` at `output`, which needs that
/// architecture's [`Architecture::linker`].
///
/// For example, `compile_for("print(1)\n", Path::new("build/out"), Architecture::Arm64)` writes
/// arm64 assembly to `build/out.s` and links it into an arm64 `build/out`.
pub fn compile_for(source: &str, output: &Path, arch: Architecture) -> Result<(), Box<dyn Error>> {
    compile_module(&checked(source)?, output, arch)
}

/// Compiles a checked module, such as one from [`load`], to a native executable for `arch` at
/// `output`.
///
/// For example, compiling for [`Architecture::Arm64`] on an x86-64 machine links with
/// `aarch64-linux-gnu-gcc`.
pub fn compile_module(ast: &Mod, output: &Path, arch: Architecture) -> Result<(), Box<dyn Error>> {
    arch.generator().compile(ast, output)?;
    Ok(())
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
