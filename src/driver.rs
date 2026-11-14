//! Pipelines that wire the lexer, parser, checker, and backends together.
//!
//! For example, `interpret("print(1)\n")` lexes, parses, checks, and evaluates the source, printing `1`.

use crate::ast::{Expr, ExprKind, Mod};
use crate::checker::{Analysis, Calls, TypeChecker};
use crate::codegen::device::Device;
use crate::codegen::gpu::compile::{CompileError, compile_cubin};
use crate::codegen::gpu::{cubin, image, ptx};
use crate::codegen::{self, Os, Subsystem, Target};
use crate::debug;
use crate::diagnostic::{Diagnostic, Diagnostics, Severity};
use crate::interpreter::{Exit, Interpreter, Limits};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::project::{self, DEFAULT_ENTRY, Linked, MapSources, SourceMap, Sources};
use std::error::Error;
use std::io::{BufRead, Write};
use std::path::{Path, PathBuf};
use std::rc::Rc;

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

/// Parses source code and links it, with no other files, into a module that holds the prelude,
/// so `option`, `some`, and `none` work in it as in any program. Returns the syntax and import
/// errors, if any.
///
/// For example, `parse_program("x = some(1)\n")` returns a module whose second statement assigns
/// `option.some(1)`, after the prelude's `option` enum.
pub fn parse_program(source: &str) -> Result<Mod, Diagnostics> {
    let linked = project::link(Path::new(DEFAULT_ENTRY), source, &MapSources::default());
    if has_errors(&linked.diagnostics) {
        return Err(Diagnostics(linked.diagnostics));
    }
    Ok(linked.module)
}

/// Parses, links, and checks the program with this entry source, given as a single string with
/// no other files, returning the module only if no errors were found.
///
/// For example, `checked("x = \n")` fails with the syntax error, while a program that parses
/// returns it checked.
fn checked(source: &str) -> Result<Checked, Diagnostics> {
    load(Path::new(DEFAULT_ENTRY), source, &MapSources::default()).1
}

/// A linked program that passed the checker.
#[derive(Debug)]
pub struct Checked {
    pub module: Mod,
    /// The checker's warnings, which `run` and `build` print before going on.
    pub warnings: Vec<Diagnostic>,
    /// How every call of a function of an `impl` block was resolved, and every function value
    /// and call of one, which the interpreter needs.
    pub calls: Calls,
    /// Everything the checker learned about the module, which compiling it reuses.
    pub analysis: Rc<Analysis>,
    /// The test blocks of the entry file, in order, which `stone test` runs.
    pub tests: Vec<project::Test>,
}

/// Loads, links, and checks the program whose entry file is at `entry` and holds `source`,
/// reading the modules it uses from `sources`. Returns the program's files, for rendering
/// diagnostics, and the checked program if no errors were found.
///
/// For example, loading `main.stn` holding `use util` without a `util.stn` fails with
/// `no module named 'util'`.
pub fn load(
    entry: &Path,
    source: &str,
    sources: &dyn Sources,
) -> (SourceMap, Result<Checked, Diagnostics>) {
    check_linked(project::link(entry, source, sources), sources)
}

/// Checks a program that linking gave, as [`load`] does, writing its `stone.lock` through
/// `sources` if it should change.
///
/// For example, a module of a program linked as its entry with [`project::link_in`], as
/// `stone test` links each file, is checked as its own program.
pub fn check_linked(
    linked: Linked,
    sources: &dyn Sources,
) -> (SourceMap, Result<Checked, Diagnostics>) {
    let lock_error = write_lock(&linked, sources);
    let Linked {
        module,
        sources,
        mut diagnostics,
        tests,
        lints,
        ..
    } = linked;
    diagnostics.extend(lock_error);
    if has_errors(&diagnostics) {
        return (sources, Err(Diagnostics(diagnostics)));
    }
    let mut analysis = TypeChecker::new().analyze(&module);
    add_lints(&module, &mut analysis, &sources);
    let found = std::mem::take(&mut analysis.diagnostics);
    analysis.diagnostics = crate::lint::apply(found, &lints, &sources);
    if has_errors(&analysis.diagnostics) {
        return (sources, Err(Diagnostics(analysis.diagnostics)));
    }
    let checked = Checked {
        module,
        calls: analysis.calls(),
        warnings: analysis.diagnostics.clone(),
        analysis: Rc::new(analysis),
        tests,
    };
    (sources, Ok(checked))
}

/// Writes the `stone.lock` that linking found should change, if the program linked without
/// errors, returning the error if it cannot be written.
fn write_lock(linked: &Linked, sources: &dyn Sources) -> Option<Diagnostic> {
    let (path, text) = linked.lock.as_ref()?;
    if has_errors(&linked.diagnostics) {
        return None;
    }
    let error = sources.write(path, text).err()?;
    Some(
        crate::explain::Code::Manifest
            .error(crate::span::Span::empty(crate::span::Pos::new(1, 1)), error),
    )
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
    } else {
        add_lints(&linked.module, &mut analysis, &linked.sources);
    }
    analysis
}

/// Adds the warnings of the lints [`crate::lint::find`] reports to the checker's `analysis` of
/// `module`, if it found no errors, keeping its diagnostics sorted by file and position.
fn add_lints(module: &Mod, analysis: &mut Analysis, sources: &SourceMap) {
    if has_errors(&analysis.diagnostics) {
        return;
    }
    let found = crate::lint::find(module, analysis, sources);
    analysis.diagnostics.extend(found);
    // stable, so the checker's own diagnostics stay first at a position
    analysis
        .diagnostics
        .sort_by_key(|d| (d.span.file, d.span.start));
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
/// returning its files and every problem found in any of them, including a warning for each
/// thing it defines and never uses ([`crate::unused`]).
///
/// For example, checking a `main.stn` that uses a `util.stn` with a syntax error returns that error,
/// whose span is in `util.stn`.
pub fn check_program(
    entry: &Path,
    source: &str,
    sources: &dyn Sources,
) -> (SourceMap, Vec<Diagnostic>) {
    let linked = project::link(entry, source, sources);
    let analysis = analyze_linked(&linked);
    let mut diagnostics = crate::unused::find(&linked, &analysis);
    diagnostics.extend(write_lock(&linked, sources));
    diagnostics.extend(analysis.diagnostics);
    // a program the host cannot run is an error where it runs
    if let Ok(host) = Target::host() {
        diagnostics.extend(unsupported(&linked.module, &linked.sources, host.os));
    }
    let mut diagnostics = crate::lint::apply(diagnostics, &linked.lints, &linked.sources);
    // stable, so a warning stays before an error at the same place
    diagnostics.sort_by_key(|d| (d.span.file, d.span.start));
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
/// while `interpret_with("while true:\n    x = 1\n", ...)` returns an error once fuel runs out.
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
/// For example, `interpret_with_io("print(input().unwrap())\n", &mut "hi\n".as_bytes(), &mut buffer, &[],
/// limits)` appends `hi\n` to `buffer`.
pub fn interpret_with_io(
    source: &str,
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    args: &[String],
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    let checked = checked(source)?;
    run_module(&checked.module, &checked.calls, input, out, args, limits)
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
    calls: &Calls,
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    args: &[String],
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    on_interpreter_thread(|| {
        Interpreter::with_output(out, limits)
            .with_input(input)
            .with_args(args.to_vec())
            .with_calls(calls.clone())
            .evaluate(ast)
    })
}

/// Runs a checked module as [`run_module`] does, but writes `eprint`'s lines to `errors` rather
/// than to standard error, so a caller such as the MCP server can capture both streams.
///
/// For example, running the module of `eprint(1)` appends `1\n` to `errors` and nothing to `out`.
pub fn run_module_with_errors(
    ast: &Mod,
    calls: &Calls,
    input: &mut (impl BufRead + Send),
    out: &mut (impl Write + Send),
    errors: &mut (impl Write + Send),
    args: &[String],
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    on_interpreter_thread(|| {
        Interpreter::with_output(out, limits)
            .with_errors(errors)
            .with_input(input)
            .with_args(args.to_vec())
            .with_calls(calls.clone())
            .evaluate(ast)
    })
}

/// Runs the test function `function` of a checked module, such as `test.4.1`, with the
/// tree-walking interpreter, without any of the module's top-level statements: it prints to `out`,
/// writes `eprint`'s lines to `errors`, reads no input, gets no arguments, and stops once it
/// exceeds `limits`.
///
/// For example, running the test of `test "one":` plus `print(1)` appends `1\n` to `out`, and
/// one whose `assert` fails returns that runtime error. `os.exit(0)` passes, and any other status
/// ends with the error [`Exit`].
pub fn run_test(
    ast: &Mod,
    calls: &Calls,
    function: &str,
    out: &mut (impl Write + Send),
    errors: &mut (impl Write + Send),
    limits: Limits,
) -> Result<(), Box<dyn Error>> {
    on_interpreter_thread(|| {
        Interpreter::with_output(out, limits)
            .with_errors(errors)
            .with_calls(calls.clone())
            .run_function(ast, function)
    })
}

/// Runs `run` on a thread with [`Limits::STACK_SIZE`] of stack, since deep recursion needs more
/// than the main thread has, and turns how it ended into an error that can leave the thread.
fn on_interpreter_thread(
    run: impl FnOnce() -> Result<(), Box<dyn Error>> + Send,
) -> Result<(), Box<dyn Error>> {
    let result = std::thread::scope(|scope| {
        let thread = std::thread::Builder::new()
            .stack_size(Limits::STACK_SIZE)
            .spawn_scoped(scope, || Ending::of(run()))
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
    /// A runtime error at a position in the program.
    Located(Diagnostic),
    /// Any other error, by its message, such as running out of fuel.
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
        let error = match error.downcast::<Exit>() {
            Ok(exit) if exit.0 == 0 => return Ok(()),
            Ok(exit) => return Err(Ending::Exit(*exit)),
            Err(error) => error,
        };
        match error.downcast::<Diagnostic>() {
            Ok(diagnostic) => Err(Ending::Located(*diagnostic)),
            Err(error) => Err(Ending::Error(error.to_string())),
        }
    }

    fn into_error(self) -> Box<dyn Error> {
        match self {
            Ending::Located(diagnostic) => Box::new(diagnostic),
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
    let (sources, loaded) = load(Path::new(DEFAULT_ENTRY), source, &MapSources::default());
    compile_module(&loaded?, &sources, output, target, Subsystem::Console, &[])
}

/// Compiles a checked module, such as one from [`load`], to a native executable for `target` at
/// `output`, which on Windows is a program of `subsystem`, and writes the kernels of its
/// arithmetic on lists for each of `devices` beside it ([`ptx_path`]). Nothing launches them yet.
///
/// For example, compiling for [`Target::ARM64_MACOS`] on an x86-64 Linux machine writes an arm64
/// macOS executable, with no cross toolchain installed.
pub fn compile_module(
    checked: &Checked,
    sources: &SourceMap,
    output: &Path,
    target: Target,
    subsystem: Subsystem,
    devices: &[Device],
) -> Result<(), Box<dyn Error>> {
    compile_with(checked, sources, output, target, subsystem, devices, None)
}

/// Compiles a checked module, as [`compile_module`] does, into a build for `stone test`: an
/// executable that runs none of the module's top-level statements, but the one test function of
/// [`Checked::tests`] whose linked name the `STONE_TEST` environment variable holds.
///
/// For example, running the build of a file whose test block at 4:1 prints `1` with
/// `STONE_TEST=test.4.1` prints `1`, and running it with no such test fails with
/// `error: STONE_TEST names no test of this program`.
pub fn compile_tests(
    checked: &Checked,
    sources: &SourceMap,
    output: &Path,
    target: Target,
) -> Result<(), Box<dyn Error>> {
    let tests = checked.tests.iter().map(|t| t.function.clone()).collect();
    compile_with(
        checked,
        sources,
        output,
        target,
        Subsystem::Console,
        &[],
        Some(tests),
    )
}

/// Compiles a checked module as [`compile_module`] does, into a build for `stone test` if
/// `tests` lists its test functions.
fn compile_with(
    checked: &Checked,
    sources: &SourceMap,
    output: &Path,
    target: Target,
    subsystem: Subsystem,
    devices: &[Device],
    tests: Option<Vec<String>>,
) -> Result<(), Box<dyn Error>> {
    let unsupported = unsupported(&checked.module, sources, target.os);
    if !unsupported.is_empty() {
        return Err(Box::new(Diagnostics(unsupported)));
    }
    for device in devices {
        device.ptx_version()?;
    }
    let mut generator = target.generator();
    generator.set_sources(sources.clone());
    generator.set_analysis(Rc::clone(&checked.analysis));
    // Linux and Windows programs launch kernels; others compute everything on the processor
    let carry = match target.os {
        Os::Linux => Some(image::Carry::Sass),
        Os::Windows => Some(image::Carry::Modules),
        Os::MacOs => None,
    };
    generator.set_gpu_launch(!devices.is_empty() && carry.is_some());
    if let Some(tests) = tests {
        generator.set_tests(tests);
    }
    let mut assembly = generator.assemble(&checked.module)?;
    if !devices.is_empty() {
        let numbered = codegen::gpu::numbered(generator.device_kernels())?;
        let mut embedded = Vec::new();
        for &device in devices {
            let text = ptx::module(&numbered.kernels, device)?;
            std::fs::write(ptx_path(output, device), &text)?;
            let cubin = match compile_cubin(&text, device) {
                Ok(bytes) => {
                    let kernels = cubin::parse(&bytes)?;
                    std::fs::write(cubin_path(output, device), &bytes)?;
                    Some(image::Cubin { bytes, kernels })
                }
                Err(CompileError::Missing(message)) => {
                    match carry {
                        // the driver compiles the PTX the program carries
                        Some(image::Carry::Modules) => eprintln!(
                            "warning: {message}, so the GPU's driver compiles the PTX when the \
                             program runs"
                        ),
                        // without machine code the program computes on the processor
                        _ => eprintln!("warning: {message}"),
                    }
                    None
                }
                Err(error) => return Err(Box::new(error)),
            };
            let code = image::DeviceCode {
                device,
                cubin,
                ptx: text,
            };
            match carry {
                Some(image::Carry::Sass) if code.cubin.is_some() => {
                    match embeddable(&code, &numbered) {
                        Ok(()) => embedded.push(code),
                        // the program still computes on the processor, as it did before
                        Err(reason) => eprintln!("warning: {reason}"),
                    }
                }
                Some(image::Carry::Modules) => embedded.push(code),
                _ => {}
            }
        }
        if let Some(carry) = carry {
            let image = image::build(&numbered.record_kernel, &numbered.shapes, &embedded, carry)?;
            assembly.push_str(&image::assembly(&image));
        }
    }
    codegen::link(&assembly, output, target, subsystem)?;
    Ok(())
}

/// Returns whether a Linux program can carry the machine code of `code`, which has a cubin,
/// in its image, else why not.
///
/// For example, a cubin whose kernels keep a table of constants in `.nv.constant2` cannot be
/// loaded without placing that section, which stone does not do, so its device's code is left
/// out of the image and the program computes on the processor on those GPUs.
fn embeddable(
    code: &image::DeviceCode,
    numbered: &codegen::gpu::kernel::Numbered,
) -> Result<(), String> {
    let Some(cubin) = &code.cubin else {
        return Err(format!("there is no cubin for {}", code.device));
    };
    let unplaceable = cubin::unplaceable_sections(&cubin.bytes)?;
    if !unplaceable.is_empty() {
        return Err(format!(
            "the kernels for {} need {}, which a program cannot load itself, so it computes on \
             the processor there",
            code.device,
            unplaceable.join(", ")
        ));
    }
    image::build(
        &numbered.record_kernel,
        &numbered.shapes,
        std::slice::from_ref(code),
        image::Carry::Sass,
    )
    .map_err(|reason| format!("{reason}, so the program computes on the processor there"))?;
    Ok(())
}

/// Returns where `stone build` writes the PTX for `device` of the executable at `output`, which
/// is named as its assembly is, with the device before `.ptx`.
///
/// For example, `ptx_path(Path::new("build/basics"), sm_75)` is `build/basics.sm_75.ptx`.
pub fn ptx_path(output: &Path, device: Device) -> PathBuf {
    output.with_extension(format!("{device}.ptx"))
}

/// Returns where `stone build` writes the machine code `ptxas` makes for `device` of the
/// executable at `output`, beside its PTX.
///
/// For example, `cubin_path(Path::new("build/basics"), sm_75)` is `build/basics.sm_75.cubin`.
pub fn cubin_path(output: &Path, device: Device) -> PathBuf {
    output.with_extension(format!("{device}.cubin"))
}

/// The builtin functions a Windows program cannot use yet, by linked name.
const WINDOWS_UNSUPPORTED: [&str; 1] = ["net.connect_unix"];

/// Returns an error for each place the program's own files name a builtin function that
/// compiled code and the interpreter do not support yet on `os`: on Windows,
/// `net.connect_unix`. The modules' own files are not the program's, so they name nothing, and
/// `gui`, which uses `net.connect_unix` elsewhere, opens Win32 windows there.
///
/// For example, `net.connect_unix("/tmp/x")` in a program for Windows is the error
/// `net.connect_unix is not supported on Windows yet` at `net.connect_unix`.
pub fn unsupported(module: &Mod, sources: &SourceMap, os: Os) -> Vec<Diagnostic> {
    if os != Os::Windows {
        return Vec::new();
    }
    let mut errors = Vec::new();
    let mut found = |expr: &Expr| {
        let ExprKind::Name { id, .. } = &expr.kind else {
            return;
        };
        if WINDOWS_UNSUPPORTED.contains(&id.as_str()) {
            errors.push(
                crate::explain::Code::Unsupported
                    .error(expr.span, format!("{id} is not supported on Windows yet")),
            );
        }
    };
    let Mod::Module { body } = module;
    let mut blocks = vec![&body[..]];
    while let Some(block) = blocks.pop() {
        for stmt in block {
            if sources.file(stmt.span.file).is_builtin() {
                continue;
            }
            for expr in stmt.exprs() {
                expr.visit(&mut found);
            }
            blocks.extend(stmt.blocks());
        }
    }
    errors.sort_by_key(|d| (d.span.file, d.span.start));
    errors
}

/// The name of the directory, next to a program's entry file, that `stone build` writes to.
pub const BUILD_DIR: &str = "build";

/// The file that marks a build directory as stone's, so `stone clean` never deletes a `build`
/// directory something else made.
pub const MARKER: &str = ".stone";

/// Returns the root of the program the file at `path` belongs to: the nearest directory at or
/// above the file's own that holds a `main.stn` or a `stone.toml`, or `None` if none does, when
/// the file is a program of its own.
///
/// For example, with `app/main.stn` and `app/geo/shapes.stn`, the root of `app/geo/shapes.stn`
/// is `app`, so linking it there resolves `use geo.vec` as the program does.
pub fn program_root(path: &Path, sources: &dyn Sources) -> Option<PathBuf> {
    let dir = path.parent().unwrap_or(Path::new(""));
    dir.ancestors()
        .find(|d| is_root(d, sources))
        .map(Path::to_path_buf)
}

/// Returns whether `dir` is the root of a program: whether it holds a `main.stn` or a
/// `stone.toml`.
fn is_root(dir: &Path, sources: &dyn Sources) -> bool {
    let holds = |name: &str| sources.read(&dir.join(name)).is_some();
    holds(DEFAULT_ENTRY) || holds(crate::package::MANIFEST)
}

/// Returns the root of the program the directory `start` is in: the nearest directory at or
/// above it that holds a `main.stn` or a `stone.toml`, or `None` if there is none.
///
/// For example, in a project made by `stone init app`, `find_root(Path::new("/home/me/app/src"))`
/// is `/home/me/app`.
pub fn find_root(start: &Path) -> Option<PathBuf> {
    start
        .ancestors()
        .find(|d| is_root(d, &project::FsSources))
        .map(Path::to_path_buf)
}

/// Returns the root of the program the current directory is in (see [`find_root`]), written
/// relative to the current directory, or `None` if there is none.
///
/// For example, run in `app/src` of a project `app`, `program_dir()` is `Some("..")`, and run in
/// `app` itself, `Some("")`.
pub fn program_dir() -> Option<PathBuf> {
    relative_root(&std::env::current_dir().ok()?)
}

/// Returns the root of the program `cwd` is in, written relative to `cwd` as `..`s, or `None` if
/// there is none.
fn relative_root(cwd: &Path) -> Option<PathBuf> {
    let root = find_root(cwd)?;
    let depth = cwd
        .strip_prefix(&root)
        .map_or(0, |rest| rest.components().count());
    Some((0..depth).map(|_| Path::new("..")).collect())
}

/// What a command wants of the entry file [`find_entry`] finds.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryUse {
    /// A program to run or build, so a `main.stn`.
    Program,
    /// Anything to check, so a library's `lib.stn` too.
    Any,
}

/// The error [`find_entry`] gives when no path is named and no program is found.
pub const NO_PROGRAM: &str = "there is no stone program here, since neither this directory nor \
                              any above it holds a main.stn or a stone.toml; run `stone init` to \
                              start one, or name a file";

/// Returns the entry file a command such as `stone run` uses, given the path it was named, if
/// any (see [`find_entry_in`]).
pub fn find_entry(path: Option<&Path>, want: EntryUse) -> Result<PathBuf, String> {
    let cwd =
        std::env::current_dir().map_err(|e| format!("cannot read the current directory: {e}"))?;
    find_entry_in(&cwd, path, want)
}

/// Returns the entry file a command run in `cwd` uses, given the path it was named, if any. A
/// file is its own entry. A directory's entry is its `main.stn`, or, for [`EntryUse::Any`], its
/// `lib.stn` if it is a library. Without a path, the entry is that of the program's root (see
/// [`find_root`]) from `cwd`, written relative to `cwd`, so errors and the build directory read
/// as if it had been named.
///
/// For example, run in `app/src` of a project `app`, `find_entry_in(cwd, None,
/// EntryUse::Program)` is `../main.stn`, and `find_entry_in(cwd, Some(Path::new("sum.stn")),
/// EntryUse::Program)` is `sum.stn`.
pub fn find_entry_in(cwd: &Path, path: Option<&Path>, want: EntryUse) -> Result<PathBuf, String> {
    let dir = match path {
        Some(path) if !cwd.join(path).is_dir() => return Ok(path.to_path_buf()),
        Some(dir) => dir.to_path_buf(),
        None => relative_root(cwd).ok_or(NO_PROGRAM)?,
    };
    let shown = match dir.as_os_str().is_empty() {
        true => "this directory".to_string(),
        false => dir.display().to_string(),
    };
    let main = dir.join(DEFAULT_ENTRY);
    if cwd.join(&main).is_file() {
        return Ok(main);
    }
    let lib = dir.join(project::LIB_ENTRY);
    match (want, cwd.join(&lib).is_file()) {
        (EntryUse::Any, true) => Ok(lib),
        (EntryUse::Program, true) => Err(format!(
            "{shown} is a library, with a {} but no {DEFAULT_ENTRY}, so there is nothing to run; \
             check it with `stone check` or test it with `stone test`",
            project::LIB_ENTRY
        )),
        (_, false) => Err(format!("there is no {DEFAULT_ENTRY} in {shown}")),
    }
}

/// Returns the build directory of the program whose entry file is `entry`: `build` next to it,
/// in the same directory that is the root of the program's modules.
///
/// For example, `build_dir(Path::new("examples/basics.stn"))` is `examples/build`, and
/// `build_dir(Path::new("main.stn"))` is `build`.
pub fn build_dir(entry: &Path) -> PathBuf {
    entry.parent().unwrap_or(Path::new("")).join(BUILD_DIR)
}

/// Returns the name of the executable built from the entry file `entry`: the file's stem, or,
/// for a `main.stn`, the name its project's `stone.toml` gives the package, else the name of the
/// directory it is in, as Cargo names a package's executable.
///
/// For example, `examples/basics.stn` is named `basics`, `tests/programs/modules/main.stn` is
/// named `modules`, the `main.stn` of a directory `my-app` whose `stone.toml` holds
/// `name = "my_app"` is named `my_app`, and `/main.stn`, whose directory has no name, is named
/// `main`.
pub fn program_name(entry: &Path) -> String {
    let stem = entry.file_stem().unwrap_or_default().to_string_lossy();
    if entry.file_name().and_then(|name| name.to_str()) != Some(DEFAULT_ENTRY) {
        return stem.into_owned();
    }
    // `main.stn` alone, or `sub/../main.stn`, names a directory only once resolved
    let parent = match entry.parent() {
        Some(parent) if !parent.as_os_str().is_empty() => parent,
        _ => Path::new("."),
    };
    if let Some(name) = package_name(parent) {
        return name;
    }
    std::fs::canonicalize(parent)
        .ok()
        .and_then(|dir| {
            dir.file_name()
                .map(|name| name.to_string_lossy().into_owned())
        })
        .unwrap_or_else(|| stem.into_owned())
}

/// Returns the name the `stone.toml` in `dir` gives its package, or `None` if there is no
/// manifest, it names no package, or it has errors, which loading the program reports.
///
/// For example, a `stone.toml` holding `[package]` and `name = "hello"` gives `Some("hello")`.
fn package_name(dir: &Path) -> Option<String> {
    let text = std::fs::read_to_string(dir.join(crate::package::MANIFEST)).ok()?;
    let (manifest, errors) = crate::package::Manifest::parse(&text, crate::span::FileId(0));
    match errors.is_empty() {
        true => manifest.name.map(|(name, _)| name),
        false => None,
    }
}

/// Returns where `stone build` writes the executable for `target` built from the entry file
/// `entry`: in the program's [`build_dir`] for the machine stone runs on, or in a directory
/// named after the target inside it for any other target, so builds for different targets
/// never overwrite each other.
///
/// For example, on an x86-64 Linux machine, `examples/basics.stn` builds to
/// `examples/build/basics`, for [`Target::ARM64_MACOS`] to
/// `examples/build/aarch64-macos/basics`, and for [`Target::X64_WINDOWS`] to
/// `examples/build/x86_64-windows/basics.exe`, since Windows runs a program by its extension.
pub fn output_path(entry: &Path, target: Target) -> PathBuf {
    let mut path = build_dir(entry);
    // an Intel Mac has no host target, so everything it builds is for another machine
    if Target::host().ok() != Some(target) {
        path.push(target.to_string());
    }
    let mut name = program_name(entry);
    if target.os == Os::Windows {
        name.push_str(".exe");
    }
    path.push(name);
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
/// For example, after `stone build examples/basics.stn`, `clean(Path::new("examples"))` deletes
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

/// Returns every `.stn` file in `dir` and the directories under it, sorted, skipping hidden
/// directories (such as `.git`) and build directories that stone made (those with the
/// [`MARKER`]), which is what `stone fmt` formats when given a directory.
///
/// For example, a directory holding `main.stn`, `geo/shapes.stn`, and `build/` after a build
/// gives `main.stn` and `geo/shapes.stn`.
pub fn stone_files(dir: &Path) -> std::io::Result<Vec<PathBuf>> {
    let mut files = vec![];
    let mut pending = vec![dir.to_path_buf()];
    while let Some(dir) = pending.pop() {
        for entry in std::fs::read_dir(&dir)? {
            let path = entry?.path();
            let hidden = path
                .file_name()
                .is_some_and(|name| name.to_string_lossy().starts_with('.'));
            if path.is_dir() {
                if !hidden && !path.join(MARKER).is_file() {
                    pending.push(path);
                }
            } else if path.extension().is_some_and(|ext| ext == "stn") {
                files.push(path);
            }
        }
    }
    files.sort();
    Ok(files)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::Type;
    use crate::codegen::ir::lower::TEST_VARIABLE;
    use crate::span::FileId;

    #[test]
    fn windows_programs_cannot_use_unix_sockets_yet() {
        let source = "\
use net
use os
use gui
fn f() -> result[output, str]:
    ret os.run(\"ls\", [])
print(f().is_ok(), os.pid() > 0)
s = net.connect(\"localhost\", 80)
c = gui.canvas(width=1, height=1, pixels=[0])
u = net.connect_unix(\"/tmp/x\")
";
        let (sources, checked) = load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.unwrap();
        assert!(unsupported(&checked.module, &sources, Os::Linux).is_empty());
        let errors: Vec<String> = unsupported(&checked.module, &sources, Os::Windows)
            .iter()
            .map(|d| sources.render(d).lines().next().unwrap().to_string())
            .collect();
        assert_eq!(
            errors,
            ["main.stn:9:9: error[E0046]: net.connect_unix is not supported on Windows yet"]
        );
        let output = std::env::temp_dir().join(format!("stone-net-{}.exe", std::process::id()));
        let built = compile_module(
            &checked,
            &sources,
            &output,
            Target::X64_WINDOWS,
            Subsystem::Console,
            &[],
        );
        assert!(built.is_err());
        assert!(!output.exists());
    }

    #[test]
    fn a_test_runs_alone_without_the_top_level_code() {
        let source = "\
print(\"top-level code\")

fn double(x: int) -> int:
    ret x * 2

test \"doubles\":
    print(double(2))
    assert double(2) == 4

test \"doubles wrong\":
    eprint(\"checking\")
    assert double(2) == 5
";
        let (sources, checked) = load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.unwrap();
        let names: Vec<(&str, &str)> = checked
            .tests
            .iter()
            .map(|t| (t.description.as_str(), t.function.as_str()))
            .collect();
        assert_eq!(
            names,
            [("doubles", "test.6.1"), ("doubles wrong", "test.10.1")]
        );
        let run = |function: &str| {
            let (mut out, mut errors) = (vec![], vec![]);
            let result = run_test(
                &checked.module,
                &checked.calls,
                function,
                &mut out,
                &mut errors,
                Limits::DEFAULT,
            );
            let error = result.err().map(|e| match e.downcast_ref::<Diagnostic>() {
                Some(d) => sources.render(d).lines().next().unwrap().to_string(),
                None => e.to_string(),
            });
            (
                String::from_utf8(out).unwrap(),
                String::from_utf8(errors).unwrap(),
                error,
            )
        };
        assert_eq!(run("test.6.1"), ("4\n".to_string(), String::new(), None));
        assert_eq!(
            run("test.10.1"),
            (
                String::new(),
                "checking\n".to_string(),
                Some("main.stn:12:12: error: assertion failed: 4 == 5".to_string())
            )
        );
    }

    #[test]
    fn a_test_build_runs_the_test_stone_test_names() {
        let Ok(target) = Target::host() else {
            return;
        };
        let source = "\
print(\"top-level code\")

test \"passes\":
    print(1)

test \"fails\":
    xs = [1, 2]
    assert xs == [1, 3]
";
        let (sources, checked) = load(Path::new("main.stn"), source, &MapSources::default());
        let checked = checked.unwrap();
        let dir = std::env::temp_dir().join(format!("stone-test-build-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let output = dir.join("main");
        compile_tests(&checked, &sources, &output, target).unwrap();
        let run = |name: Option<&str>| {
            let mut command = std::process::Command::new(&output);
            command.env_remove(TEST_VARIABLE);
            if let Some(name) = name {
                command.env(TEST_VARIABLE, name);
            }
            let done = command.output().unwrap();
            (
                done.status.code(),
                String::from_utf8(done.stdout).unwrap(),
                String::from_utf8(done.stderr).unwrap(),
            )
        };
        assert_eq!(
            run(Some("test.3.1")),
            (Some(0), "1\n".to_string(), String::new())
        );
        let (status, out, errors) = run(Some("test.6.1"));
        assert_eq!((status, out.as_str()), (Some(1), ""));
        assert!(
            errors.starts_with("main.stn:8:12: error: assertion failed: [1, 2] == [1, 3]"),
            "{errors}"
        );
        assert_eq!(
            run(None),
            (
                Some(1),
                String::new(),
                "error: STONE_TEST names no test of this program\n".to_string()
            )
        );
        std::fs::remove_dir_all(&dir).unwrap();
    }

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
        assert!(run_limited("while true:\n    x = 1\n").is_err());
    }

    #[test]
    fn unbounded_recursion_hits_the_depth_limit() {
        assert!(run_limited("fn f(n: int) -> int:\n    ret f(n)\nf(1)\n").is_err());
    }

    #[test]
    fn deep_expressions_inside_recursion_hit_the_depth_limit() {
        let source = format!("fn f(n):\n    ret {}f(n)\nf(1)\n", "-".repeat(40));
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
        let source =
            "fn f(x: int) -> int:\n    if x != 0:\n        ret 1\n    ret 0\nprint(f(5))\n";
        assert_eq!(run_limited(source), Ok("1\n".to_string()));
    }

    #[test]
    fn while_reevaluates_its_test() {
        let source = "i = 0\nwhile i != 2:\n    i = i + 1\nprint(i)\n";
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
            .map(|d| d.render("main.stn", source))
            .collect();
        assert_eq!(
            rendered,
            [
                "main.stn:2:5: error[E0001]: expected ':', found end of line\n  |\n2 | if x\n  |     ^\n"
            ]
        );
    }

    #[test]
    fn check_reports_a_lex_error_under_the_whole_literal() {
        let source = "x = 99999999999999999999\n";
        let rendered: Vec<String> = check(source)
            .iter()
            .map(|d| d.render("big.stn", source))
            .collect();
        assert_eq!(
            rendered,
            [concat!(
                "big.stn:1:5: error[E0002]: integer literal 99999999999999999999 is too large\n",
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
        let names: Vec<&str> = analysis
            .symbols
            .iter()
            .filter(|s| s.span.file == FileId(0))
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, ["x", "z"]);
    }

    #[test]
    fn analyze_keeps_what_lexes_around_lexing_errors() {
        // an unfinished value in a string, as while it is being typed
        let analysis = analyze("x = [1]\ny = \"{x.\nz = x\nw = \"}\"\nv = \n");
        let messages: Vec<(&str, usize)> = analysis
            .diagnostics
            .iter()
            .map(|d| (d.message.as_str(), d.span.start.line))
            .collect();
        assert_eq!(
            messages,
            [
                ("unterminated string", 2),
                ("a '}' in a string is written '}}'", 4),
                ("expected an expression, found end of line", 5),
            ]
        );
        let names: Vec<&str> = analysis
            .symbols
            .iter()
            .filter(|s| s.span.file == FileId(0))
            .map(|s| s.name.as_str())
            .collect();
        assert_eq!(names, ["x", "z"]);
    }

    #[test]
    fn analyze_hides_type_errors_while_there_are_syntax_errors() {
        // `f` failed to parse, so calling it would otherwise be an undefined function
        let analysis = analyze("fn f(:\n    ret 1\nprint(f())\n");
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
        let x = analysis.symbols.iter().find(|s| s.span.file == FileId(0));
        assert_eq!(x.map(|s| s.name.as_str()), Some("x"));
        assert_eq!(x.map(|s| &s.ty), Some(&Type::Int));
    }

    #[test]
    fn parameters_do_not_overwrite_caller_variables() {
        let source = "fn f(a: int) -> int:\n    ret a\na = 7\nprint(f(1))\nprint(a)\n";
        assert_eq!(run_limited(source), Ok("1\n7\n".to_string()));
    }

    /// Interprets `source` reading `input` as stdin with `args`, returning what it printed or the
    /// error message.
    ///
    /// For example, `run_io("print(input().unwrap())\n", "a\n", &[])` returns `Ok("a\n")`.
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
                "use os\nprint(os.platform(), os.has_env(\"PATH\"), os.env(\"STONE_UNSET\").is_none())\n",
                "",
                &[]
            ),
            Ok(format!("{platform} true true\n"))
        );
        let source = "use os\nuse time\nprint(os.pid() > 0, os.cpu_count() > 0, time.now() > 0.0)\n\
                      print(time.clock() >= 0.0, os.hostname() == os.hostname(), os.cwd() != \"\")\n";
        assert_eq!(
            run_io(source, "", &[]),
            Ok("true true true\ntrue true true\n".to_string())
        );
    }

    #[test]
    fn exit_with_status_zero_ends_the_program_successfully() {
        let source = "use os\n\nfn stop(xs: list[str]):\n    os.exit(0)\n\nprint(1)\nstop([\"a\"])\nprint(2)\n";
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
            "while not eof():\n    print(\"[\" + input().unwrap() + \"]\")\nprint(input())\n";
        assert_eq!(
            run_io(source, "a\n\nb c\r\nlast", &[]),
            Ok("[a]\n[]\n[b c\r]\n[last]\nerr('end of input')\n".to_string())
        );
    }

    #[test]
    fn input_writes_its_prompt_first() {
        assert_eq!(
            run_io("x = input(\"name? \")\nprint(x)\n", "Ada\n", &[]),
            Ok("name? ok('Ada')\n".to_string())
        );
    }

    #[test]
    fn interpret_with_has_no_input() {
        assert_eq!(
            run_limited("print(eof(), input())\n"),
            Ok("true err('end of input')\n".to_string())
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
        let source = "print(int(\" 42 \").unwrap() + 1, float(\"2.5\").unwrap() * 2.0, str(1.0) + str(true) + str(-3))\n";
        assert_eq!(run_limited(source), Ok("43 5.0 1.0true-3\n".to_string()));
        assert_eq!(
            run_limited("print(int(\"x\").unwrap())\n"),
            Err(
                "1:7: error: cannot unwrap err: invalid literal for int() with base 10: 'x'"
                    .to_string()
            )
        );
        assert_eq!(
            run_limited("print(float(\"\").unwrap())\n"),
            Err("1:7: error: cannot unwrap err: could not convert string to float: ''".to_string())
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
    fn a_named_file_is_its_own_entry() {
        let dir = scratch("entry_file");
        let found = find_entry_in(&dir, Some(Path::new("sum.stn")), EntryUse::Program);
        assert_eq!(found, Ok(PathBuf::from("sum.stn")));
    }

    #[test]
    fn a_directory_runs_its_main_and_checks_its_lib() {
        let dir = scratch("entry_dir");
        std::fs::create_dir_all(dir.join("app")).unwrap();
        std::fs::write(dir.join("app/main.stn"), "").unwrap();
        std::fs::create_dir_all(dir.join("geo")).unwrap();
        std::fs::write(dir.join("geo/lib.stn"), "").unwrap();
        let app = Some(Path::new("app"));
        assert_eq!(
            find_entry_in(&dir, app, EntryUse::Program),
            Ok(PathBuf::from("app/main.stn"))
        );
        let geo = Some(Path::new("geo"));
        assert_eq!(
            find_entry_in(&dir, geo, EntryUse::Any),
            Ok(PathBuf::from("geo/lib.stn"))
        );
        assert_eq!(
            find_entry_in(&dir, geo, EntryUse::Program),
            Err(
                "geo is a library, with a lib.stn but no main.stn, so there is nothing to run; \
                 check it with `stone check` or test it with `stone test`"
                    .to_string()
            )
        );
        std::fs::create_dir_all(dir.join("empty")).unwrap();
        assert_eq!(
            find_entry_in(&dir, Some(Path::new("empty")), EntryUse::Any),
            Err("there is no main.stn in empty".to_string())
        );
    }

    #[test]
    fn without_a_path_the_nearest_program_above_is_found() {
        let dir = scratch("entry_found");
        std::fs::write(dir.join("stone.toml"), "").unwrap();
        std::fs::write(dir.join("main.stn"), "").unwrap();
        std::fs::create_dir_all(dir.join("a/b")).unwrap();
        assert_eq!(
            find_entry_in(&dir, None, EntryUse::Program),
            Ok(PathBuf::from("main.stn"))
        );
        assert_eq!(
            find_entry_in(&dir.join("a/b"), None, EntryUse::Program),
            Ok(PathBuf::from("../../main.stn"))
        );
        assert_eq!(find_root(&dir.join("a/b")), Some(dir.clone()));
        // a library's root has no main.stn, so only checking finds an entry
        std::fs::remove_file(dir.join("main.stn")).unwrap();
        std::fs::write(dir.join("lib.stn"), "").unwrap();
        assert_eq!(
            find_entry_in(&dir.join("a"), None, EntryUse::Any),
            Ok(PathBuf::from("../lib.stn"))
        );
        assert!(find_entry_in(&dir, None, EntryUse::Program).is_err());
    }

    #[test]
    fn without_a_path_or_a_program_above_there_is_no_entry() {
        // the system's root directory holds no stone program
        let root = Path::new("/");
        assert_eq!(find_root(root), None);
        assert_eq!(
            find_entry_in(root, None, EntryUse::Program),
            Err(NO_PROGRAM.to_string())
        );
    }

    #[test]
    fn the_build_directory_sits_next_to_the_entry_file() {
        assert_eq!(
            build_dir(Path::new("examples/basics.stn")),
            Path::new("examples/build")
        );
        assert_eq!(build_dir(Path::new("main.stn")), Path::new("build"));
        assert_eq!(build_dir(Path::new("/abs/p.stn")), Path::new("/abs/build"));
    }

    #[test]
    fn programs_are_named_after_their_entry_file() {
        assert_eq!(program_name(Path::new("examples/basics.stn")), "basics");
        let dir = scratch("modules");
        std::fs::write(dir.join("main.stn"), "").unwrap();
        assert_eq!(program_name(&dir.join("main.stn")), "modules");
        // a relative main.stn takes the name of the directory it resolves to
        let nested = dir.join("sub").join("main.stn");
        std::fs::create_dir_all(nested.parent().unwrap()).unwrap();
        std::fs::write(&nested, "").unwrap();
        assert_eq!(program_name(&dir.join("sub/../main.stn")), "modules");
        assert_eq!(program_name(Path::new("/main.stn")), "main");
    }

    #[test]
    fn a_projects_main_is_named_after_its_package() {
        let dir = scratch("my-app");
        std::fs::write(dir.join("main.stn"), "").unwrap();
        std::fs::write(dir.join("tool.stn"), "").unwrap();
        std::fs::write(dir.join("stone.toml"), "[package]\nname = \"my_app\"\n").unwrap();
        assert_eq!(program_name(&dir.join("main.stn")), "my_app");
        // only the project's entry file takes the package's name
        assert_eq!(program_name(&dir.join("tool.stn")), "tool");
        // a manifest without a name, or one that does not parse, leaves the directory's
        std::fs::write(dir.join("stone.toml"), "[dependencies]\n").unwrap();
        assert_eq!(program_name(&dir.join("main.stn")), "my-app");
        std::fs::write(dir.join("stone.toml"), "[package\n").unwrap();
        assert_eq!(program_name(&dir.join("main.stn")), "my-app");
    }

    #[test]
    fn other_targets_build_into_directories_of_their_own() {
        let host = Target::host().unwrap();
        assert_eq!(
            output_path(Path::new("examples/basics.stn"), host),
            Path::new(&format!(
                "examples/build/basics{}",
                std::env::consts::EXE_SUFFIX
            ))
        );
        let other = if host == Target::ARM64_MACOS {
            Target::X64_LINUX
        } else {
            Target::ARM64_MACOS
        };
        assert_eq!(
            output_path(Path::new("examples/basics.stn"), other),
            Path::new("examples/build")
                .join(other.to_string())
                .join("basics")
        );
        let leveled: Target = "x86_64v3-linux".parse().unwrap();
        assert_eq!(
            output_path(Path::new("p.stn"), leveled),
            Path::new("build/x86_64v3-linux/p")
        );
    }

    #[test]
    fn stone_files_skips_hidden_and_build_directories() {
        let dir = scratch("stone-files");
        for file in [
            "b.stn",
            "a.stn",
            "notes.txt",
            "lib/c.stn",
            ".git/d.stn",
            "build/e.stn",
        ] {
            let path = dir.join(file);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, "").unwrap();
        }
        let names = |files: Vec<PathBuf>| -> Vec<String> {
            files
                .iter()
                // windows writes its own separator
                .map(|f| {
                    f.strip_prefix(&dir)
                        .unwrap()
                        .display()
                        .to_string()
                        .replace('\\', "/")
                })
                .collect()
        };
        // a build directory is skipped only once stone has marked it as its own
        assert_eq!(
            names(stone_files(&dir).unwrap()),
            ["a.stn", "b.stn", "build/e.stn", "lib/c.stn"]
        );
        prepare_build_dir(&dir.join("build")).unwrap();
        assert_eq!(
            names(stone_files(&dir).unwrap()),
            ["a.stn", "b.stn", "lib/c.stn"]
        );
        assert!(stone_files(&dir.join("missing")).is_err());
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
            Err("1:7: error: empty separator".to_string())
        );
    }
}
