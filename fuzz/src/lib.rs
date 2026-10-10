//! Shared pieces of the stone fuzz targets: interpreter limits, a grammar-aware program
//! generator, and the differential check that compares `stone run` with `stone build`.
//!
//! For example, [`generate`] turns fuzzer bytes into a program such as
//! `def f0(p0);\n    ret p0 + 1\nprint(f0(2))\n`, and [`differential`] runs it through both backends.

use arbitrary::{Result, Unstructured};
use std::fs::File;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};
use stone::codegen::Architecture;
use stone::interpreter::Limits;

/// Limits for fuzzing the interpreter, small enough that every input finishes quickly.
///
/// For example, `while 1;` runs out of fuel after 10,000 iterations instead of hanging.
pub const LIMITS: Limits = Limits {
    fuel: 10_000,
    max_depth: 200,
    max_calls: stone::stdlib::MAX_CALL_DEPTH,
};

/// How long a compiled program may run before the differential check reports a hang.
const RUN_TIMEOUT: Duration = Duration::from_secs(5);

/// Most statements in one block, keeping generated programs small enough to run quickly.
const MAX_BLOCK_LEN: usize = 4;

/// Deepest nesting of blocks and expressions the generator produces.
const MAX_NESTING: usize = 4;

/// Most parameters a generated function takes, past the six the compiler passes in registers.
const MAX_PARAMS: usize = 8;

/// Generates a stone program from fuzzer bytes, staying inside the subset that both backends
/// implement the same way.
///
/// Every program passes the checker. Variables hold ints or floats, except top-level lists of
/// ints that always have at least two elements, so indexes from -2 to 1 are always in range.
/// Strings and booleans appear in `print` and in conditions. Functions never read globals, which may not be
/// assigned yet when they run, and every loop is bounded: `while` loops by a counter, `for` loops
/// by a small `range` or by a list that nothing appends to while it is iterated.
///
/// For example, some input bytes produce:
///
/// ```text
/// def f0(p0, p1);
///     l0 = p0 * p1
///     ret l0 - 3
/// g0 = f0(4, 2)
/// print(g0)
/// ```
pub fn generate(u: &mut Unstructured) -> Result<String> {
    let mut generator = Generator::default();

    for index in 0..u.int_in_range(0..=3)? {
        generator.function(u, index)?;
    }

    let mut scope = Scope::default();
    for _ in 0..u.int_in_range(0..=MAX_BLOCK_LEN)? {
        generator.statement(u, &mut scope, 0, 0)?;
    }
    // always print something, so a program cannot pass by printing nothing in both backends
    let value = generator.expression(u, &scope, 0)?;
    generator.line(0, &format!("print({value})"));

    Ok(generator.source)
}

/// Names visible at one point in a generated program.
#[derive(Clone, Default)]
struct Scope {
    /// Int variables that can be read and assigned.
    variables: Vec<String>,
    /// Float variables that can be read and assigned.
    floats: Vec<String>,
    /// Lists of ints with at least two elements, which only top-level code uses.
    lists: Vec<String>,
    /// Loop counters, which can be read but never assigned so every loop terminates.
    counters: Vec<String>,
    /// Whether this scope is a function body, so names are local and `ret` is allowed.
    in_function: bool,
    /// Whether this point is inside a `while` loop, so `break` and `cont` are allowed.
    in_loop: bool,
}

impl Scope {
    fn readable(&self) -> Vec<&String> {
        self.variables.iter().chain(&self.counters).collect()
    }
}

/// Emits source text for [`generate`].
///
/// Once the fuzzer's bytes run out, `Unstructured` returns the low end of every range and `false`
/// for every choice, so each choice here is arranged for those defaults to end the recursion.
#[derive(Default)]
struct Generator {
    source: String,
    /// Functions defined so far, as `(name, parameter count)`.
    functions: Vec<(String, usize)>,
    /// Counter for fresh variable names, so names never collide across scopes.
    next_name: usize,
}

impl Generator {
    fn line(&mut self, indent: usize, text: &str) {
        self.source.push_str(&"    ".repeat(indent));
        self.source.push_str(text);
        self.source.push('\n');
    }

    fn fresh(&mut self, prefix: &str) -> String {
        let name = format!("{prefix}{}", self.next_name);
        self.next_name += 1;
        name
    }

    /// Emits a top-level function that only reads its parameters and its own locals.
    fn function(&mut self, u: &mut Unstructured, index: usize) -> Result<()> {
        let name = format!("f{index}");
        let params: Vec<String> = (0..u.int_in_range(0..=MAX_PARAMS)?)
            .map(|_| self.fresh("p"))
            .collect();
        self.line(0, &format!("def {name}({});", params.join(", ")));

        // registering before the body allows recursion, which fuel keeps bounded
        self.functions.push((name, params.len()));

        let mut scope = Scope {
            variables: params,
            in_function: true,
            ..Scope::default()
        };
        for _ in 0..u.int_in_range(0..=MAX_BLOCK_LEN)? {
            self.statement(u, &mut scope, 1, 1)?;
        }
        // an explicit final ret, since a missing one returns none in the interpreter but leaves
        // whatever is in rax in compiled code
        let value = self.expression(u, &scope, 0)?;
        self.line(1, &format!("ret {value}"));
        Ok(())
    }

    /// Emits one statement at `indent`, adding any variable it assigns to `scope`.
    fn statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let nested = nesting < MAX_NESTING;
        match u.int_in_range(0..=11)? {
            0 | 1 => {
                let value = self.expression(u, scope, 0)?;
                let target = if !scope.variables.is_empty() && u.arbitrary()? {
                    u.choose(&scope.variables)?.clone()
                } else {
                    let prefix = if scope.in_function { "l" } else { "g" };
                    let name = self.fresh(prefix);
                    scope.variables.push(name.clone());
                    name
                };
                self.line(indent, &format!("{target} = {value}"));
            }
            2 => self.float_statement(u, scope, indent)?,
            3 | 4 => {
                let value = self.expression(u, scope, 0)?;
                if u.arbitrary()? {
                    let test = self.comparison(u, scope)?;
                    let label = self.fresh("s");
                    self.line(indent, &format!("print(\"{label}\", {value}, {test})"));
                } else {
                    self.line(indent, &format!("print({value})"));
                }
            }
            5 if nested => self.if_statement(u, scope, indent, nesting)?,
            6 if nested => {
                if u.arbitrary()? {
                    self.while_statement(u, scope, indent, nesting)?
                } else {
                    self.for_statement(u, scope, indent, nesting)?
                }
            }
            7 if scope.in_loop => {
                let keyword = if u.arbitrary()? { "break" } else { "cont" };
                self.line(indent, keyword);
            }
            8 if scope.in_function => {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("ret {value}"));
            }
            9 if !self.functions.is_empty() => {
                let call = self.call(u, scope, 0)?;
                self.line(indent, &call);
            }
            10 | 11 if !scope.in_function => self.list_statement(u, scope, indent)?,
            _ => {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("print({value})"));
            }
        }
        Ok(())
    }

    /// Emits a statement that makes, changes, or prints a list, or a `print` of several values.
    fn list_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
    ) -> Result<()> {
        if scope.lists.is_empty() || u.int_in_range(0..=4)? == 0 {
            let name = self.fresh("list");
            let mut items = vec![];
            for _ in 0..u.int_in_range(2..=4)? {
                items.push(self.expression(u, scope, 0)?);
            }
            self.line(indent, &format!("{name} = [{}]", items.join(", ")));
            if u.arbitrary()? {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("{name}.append({value})"));
            }
            scope.lists.push(name);
            return Ok(());
        }
        let list = u.choose(&scope.lists)?.clone();
        match u.int_in_range(0..=4)? {
            0 | 4 => {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("{list}.append({value})"));
            }
            1 => {
                let index = u.int_in_range(-2..=1)?;
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("{list}[{index}] = {value}"));
            }
            2 => self.line(indent, &format!("print({list}, {list}.len())")),
            _ => {
                let value = self.expression(u, scope, 0)?;
                let test = self.comparison(u, scope)?;
                self.line(indent, &format!("print(\"{list}\", {value}, {test})"));
            }
        }
        Ok(())
    }

    /// Emits a statement that assigns a float variable or prints floats, such as `g0 = 2.5 * g1`
    /// or `print("s1", g0, g0 < 1e300)`.
    fn float_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
    ) -> Result<()> {
        let value = self.float_expression(u, scope, 0)?;
        if u.arbitrary()? {
            let target = if !scope.floats.is_empty() && u.arbitrary()? {
                u.choose(&scope.floats)?.clone()
            } else {
                let prefix = if scope.in_function { "l" } else { "g" };
                let name = self.fresh(prefix);
                scope.floats.push(name.clone());
                name
            };
            self.line(indent, &format!("{target} = {value}"));
        } else {
            let op = *u.choose(&["==", "!=", "<", "<=", ">", ">="])?;
            let operand = self.float_expression(u, scope, 0)?;
            let label = self.fresh("s");
            self.line(
                indent,
                &format!("print(\"{label}\", {value}, {value} {op} {operand})"),
            );
        }
        Ok(())
    }

    /// Emits a block of one or more statements whose new variables stay inside the block, since
    /// they may be unassigned when the block does not run.
    fn block(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let mut inner = scope.clone();
        for _ in 0..u.int_in_range(1..=MAX_BLOCK_LEN)? {
            self.statement(u, &mut inner, indent, nesting)?;
        }
        Ok(())
    }

    fn if_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let test = self.condition(u, scope)?;
        self.line(indent, &format!("if {test};"));
        self.block(u, scope, indent + 1, nesting + 1)?;

        for _ in 0..u.int_in_range(0..=2)? {
            let test = self.condition(u, scope)?;
            self.line(indent, &format!("elif {test};"));
            self.block(u, scope, indent + 1, nesting + 1)?;
        }

        if u.arbitrary()? {
            self.line(indent, "else;");
            self.block(u, scope, indent + 1, nesting + 1)?;
        }
        Ok(())
    }

    /// Emits a loop that runs a fixed number of times, such as
    /// `c0 = 0` then `while c0 < 3;` with `c0 = c0 + 1` as the first statement of its body.
    fn while_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let counter = self.fresh("c");
        let bound = u.int_in_range(0..=5)?;
        self.line(indent, &format!("{counter} = 0"));
        self.line(indent, &format!("while {counter} < {bound};"));
        // incrementing first means `cont` cannot skip it
        self.line(indent + 1, &format!("{counter} = {counter} + 1"));

        let mut inner = scope.clone();
        inner.counters.push(counter);
        inner.in_loop = true;
        self.block(u, &inner, indent + 1, nesting + 1)
    }

    /// Emits a `for` loop over a small range, or at the top level, over a list whose body never
    /// appends to it.
    fn for_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let variable = self.fresh("c");
        let mut inner = scope.clone();
        if !scope.lists.is_empty() && u.arbitrary()? {
            let list = u.choose(&scope.lists)?.clone();
            self.line(indent, &format!("for {variable} in {list};"));
            inner.lists.retain(|l| *l != list);
        } else {
            let start = u.int_in_range(-2..=2)?;
            let end = u.int_in_range(0..=5)?;
            self.line(indent, &format!("for {variable} in range({start}, {end});"));
        }
        // the variable is read-only in the body, like a `while` counter
        inner.counters.push(variable);
        inner.in_loop = true;
        self.block(u, &inner, indent + 1, nesting + 1)
    }

    /// Generates an `if` or `elif` test: an int, a comparison, or a negation of either.
    fn condition(&mut self, u: &mut Unstructured, scope: &Scope) -> Result<String> {
        let test = if u.arbitrary()? {
            self.comparison(u, scope)?
        } else {
            self.expression(u, scope, 0)?
        };
        // parenthesized, since `not a and b` would mix a bool with an int
        Ok(if u.arbitrary()? {
            format!("not ({test})")
        } else {
            test
        })
    }

    /// Generates a comparison, possibly chained, such as `a < b <= c`.
    fn comparison(&mut self, u: &mut Unstructured, scope: &Scope) -> Result<String> {
        let mut text = self.sum(u, scope, 0)?;
        for _ in 0..u.int_in_range(1..=2)? {
            let op = *u.choose(&["==", "!=", "<", "<=", ">", ">="])?;
            let operand = self.sum(u, scope, 0)?;
            text = format!("{text} {op} {operand}");
        }
        Ok(text)
    }

    /// Generates an integer-valued expression following the grammar's precedence levels.
    ///
    /// ```text
    /// expression: sum ('or' sum)* | sum ('and' sum)*
    /// sum:        term (('+' | '-') term)*
    /// term:       factor (('*' | '/' | '%') factor)*
    /// factor:     '-' factor | primary ['**' exponent]
    /// ```
    fn expression(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<String> {
        let mut text = self.sum(u, scope, nesting)?;
        if u.int_in_range(0..=7)? == 7 {
            let op = if u.arbitrary()? { "and" } else { "or" };
            for _ in 0..u.int_in_range(1..=2)? {
                let operand = self.sum(u, scope, nesting)?;
                text = format!("{text} {op} {operand}");
            }
        }
        Ok(text)
    }

    fn sum(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        let mut text = self.term(u, scope, nesting)?;
        for _ in 0..u.int_in_range(0..=2)? {
            let op = if u.arbitrary()? { "+" } else { "-" };
            let operand = self.term(u, scope, nesting)?;
            text = format!("{text} {op} {operand}");
        }
        Ok(text)
    }

    fn term(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        let mut text = self.factor(u, scope, nesting)?;
        for _ in 0..u.int_in_range(0..=1)? {
            let op = u.choose(&["*", "/", "%"])?;
            let operand = self.factor(u, scope, nesting)?;
            text = format!("{text} {op} {operand}");
        }
        Ok(text)
    }

    fn factor(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        if nesting < MAX_NESTING && u.int_in_range(0..=7)? == 7 {
            let operand = self.factor(u, scope, nesting + 1)?;
            return Ok(format!("-{operand}"));
        }
        let base = self.primary(u, scope, nesting)?;
        if nesting < MAX_NESTING && u.int_in_range(0..=7)? == 7 {
            let exponent = self.exponent(u, scope, nesting + 1)?;
            return Ok(format!("{base} ** {exponent}"));
        }
        Ok(base)
    }

    /// Generates the int exponent of a `**`, usually a small literal, but sometimes an
    /// expression, which can be huge or negative.
    fn exponent(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        if u.int_in_range(0..=3)? == 3 {
            return self.factor(u, scope, nesting);
        }
        Ok(u.int_in_range(0..=4)?.to_string())
    }

    fn primary(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        let readable = scope.readable();
        match u.int_in_range(0..=7)? {
            6 if !scope.lists.is_empty() => {
                let list = u.choose(&scope.lists)?;
                Ok(format!("{list}[{}]", u.int_in_range(-2..=1)?))
            }
            7 if !scope.lists.is_empty() => Ok(format!("{}.len()", u.choose(&scope.lists)?)),
            4 if nesting < MAX_NESTING => {
                let inner = self.expression(u, scope, nesting + 1)?;
                Ok(format!("({inner})"))
            }
            0 | 1 if !readable.is_empty() => Ok(u.choose(&readable)?.to_string()),
            2 if nesting < MAX_NESTING && !self.functions.is_empty() => {
                self.call(u, scope, nesting + 1)
            }
            3 => Ok(u.int_in_range(0..=i64::MAX)?.to_string()),
            5 if nesting < MAX_NESTING && !scope.floats.is_empty() && u.arbitrary()? => {
                // fails for nan and huge floats, which the differential check skips
                let inner = self.float_expression(u, scope, nesting + 1)?;
                Ok(format!("int({inner})"))
            }
            _ => Ok(u.int_in_range(0..=12)?.to_string()),
        }
    }

    /// Generates a float-valued expression, with the same precedence levels as [`expression`]
    /// but no `and` or `or`, since floats are not conditions.
    ///
    /// [`expression`]: Generator::expression
    fn float_expression(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<String> {
        let mut text = self.float_term(u, scope, nesting)?;
        for _ in 0..u.int_in_range(0..=2)? {
            let op = if u.arbitrary()? { "+" } else { "-" };
            let operand = self.float_term(u, scope, nesting)?;
            text = format!("{text} {op} {operand}");
        }
        Ok(text)
    }

    fn float_term(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<String> {
        let mut text = self.float_factor(u, scope, nesting)?;
        for _ in 0..u.int_in_range(0..=1)? {
            let op = u.choose(&["*", "/", "%"])?;
            let operand = self.float_factor(u, scope, nesting)?;
            text = format!("{text} {op} {operand}");
        }
        Ok(text)
    }

    fn float_factor(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<String> {
        if nesting < MAX_NESTING && u.int_in_range(0..=7)? == 7 {
            let operand = self.float_factor(u, scope, nesting + 1)?;
            return Ok(format!("-{operand}"));
        }
        let base = self.float_primary(u, scope, nesting)?;
        if nesting < MAX_NESTING && u.int_in_range(0..=7)? == 7 {
            let exponent = self.exponent(u, scope, nesting + 1)?;
            return Ok(format!("{base} ** {exponent}"));
        }
        Ok(base)
    }

    fn float_primary(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<String> {
        match u.int_in_range(0..=5)? {
            0 | 1 if !scope.floats.is_empty() => Ok(u.choose(&scope.floats)?.clone()),
            2 if nesting < MAX_NESTING => {
                let inner = self.float_expression(u, scope, nesting + 1)?;
                Ok(format!("({inner})"))
            }
            3 if nesting < MAX_NESTING => {
                let inner = self.sum(u, scope, nesting + 1)?;
                Ok(format!("float({inner})"))
            }
            // values that print in e notation, round awkwardly, or overflow to inf when combined
            4 => Ok(u
                .choose(&[
                    "0.1", "1e16", "1e300", "1e-300", "5e-324", "1e-05", "0.3", "2.5e-7",
                ])?
                .to_string()),
            _ => Ok(format!(
                "{}.{}",
                u.int_in_range(0..=99)?,
                u.int_in_range(0..=99)?
            )),
        }
    }

    /// Generates a call to a function defined earlier, with the right number of arguments.
    fn call(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        let (name, arity) = u.choose(&self.functions)?.clone();
        let mut args = Vec::with_capacity(arity);
        for _ in 0..arity {
            args.push(self.expression(u, scope, nesting)?);
        }
        Ok(format!("{name}({})", args.join(", ")))
    }
}

/// Interprets `source` under [`LIMITS`], returning its output or `None` if it failed.
///
/// For example, `interpret("print(1)\n")` returns `Some("1\n")`, and a program that divides by
/// zero returns `None`.
pub fn interpret(source: &str) -> Option<String> {
    let mut out = Vec::new();
    stone::driver::interpret_with(source, &mut out, LIMITS).ok()?;
    String::from_utf8(out).ok()
}

/// Returns the architecture to compile for: the one `STONE_FUZZ_TARGET` names, such as
/// `aarch64`, or the host's.
fn target() -> Architecture {
    std::env::var("STONE_FUZZ_TARGET")
        .ok()
        .and_then(|name| name.parse().ok())
        .unwrap_or_else(Architecture::host)
}

/// Returns the command that runs the binary `exe` built for `arch`: the binary itself on its own
/// architecture, and qemu-user otherwise, which needs nothing else, since compiled programs are
/// static and use no libc.
fn runner(exe: &Path, arch: Architecture) -> Command {
    if arch == Architecture::host() {
        return Command::new(exe);
    }
    let qemu = match arch {
        Architecture::X64 => "qemu-x86_64",
        Architecture::Arm64 => "qemu-aarch64",
    };
    let mut command = Command::new(qemu);
    command.arg(exe);
    command
}

/// Compiles `source` with `stone build`, runs the binary, and returns its stdout.
///
/// For example, `run_compiled("print(1)\n")` returns `Ok("1\n")`. A compile error, crash, hang,
/// or string or list the program never freed is returned as `Err` with a description. Setting
/// `STONE_FUZZ_TARGET=aarch64` on an x86-64 machine (or `x86_64` on an arm64 one) builds for
/// that architecture instead and runs the binary under qemu-user.
pub fn run_compiled(source: &str) -> std::result::Result<String, String> {
    static NEXT: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join("stone-fuzz");
    std::fs::create_dir_all(&dir).map_err(|e| e.to_string())?;
    let stem = format!(
        "{}-{}",
        std::process::id(),
        NEXT.fetch_add(1, Ordering::Relaxed)
    );
    let exe: PathBuf = dir.join(&stem);
    let stdout_path = dir.join(format!("{stem}.stdout"));

    let result = (|| {
        let arch = target();
        stone::driver::compile_for(source, &exe, arch)
            .map_err(|e| format!("compile failed: {e}"))?;

        // stdout goes to a file, so a chatty program cannot block on a full pipe
        let stdout = File::create(&stdout_path).map_err(|e| e.to_string())?;
        // empty stdin, like the input `interpret` gives the interpreter
        let mut child = runner(&exe, arch)
            .env("STONE_LEAK_CHECK", "1")
            .stdin(Stdio::null())
            .stdout(Stdio::from(stdout))
            .stderr(Stdio::null())
            .spawn()
            .map_err(|e| format!("failed to run binary: {e}"))?;

        let deadline = Instant::now() + RUN_TIMEOUT;
        let status = loop {
            if let Some(status) = child.try_wait().map_err(|e| e.to_string())? {
                break status;
            }
            if Instant::now() > deadline {
                let _ = child.kill();
                let _ = child.wait();
                return Err(format!("binary ran longer than {RUN_TIMEOUT:?}"));
            }
            std::thread::sleep(Duration::from_millis(2));
        };
        if !status.success() {
            return Err(format!("binary exited with {status}"));
        }
        std::fs::read_to_string(&stdout_path).map_err(|e| e.to_string())
    })();

    for path in [exe.clone(), exe.with_extension("s"), stdout_path] {
        let _ = std::fs::remove_file(path);
    }
    result
}

/// Runs `source` through both backends and panics if the compiled program's output differs from
/// the interpreter's.
///
/// Programs the interpreter rejects, such as ones that run out of fuel or divide by zero, are
/// skipped.
pub fn differential(source: &str) {
    let Some(expected) = interpret(source) else {
        return;
    };

    match run_compiled(source) {
        Ok(actual) => assert_eq!(
            actual, expected,
            "compiled output differs from the interpreter for:\n{source}"
        ),
        Err(e) => panic!("{e} for a program the interpreter ran:\n{source}"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Produces deterministic pseudo-random bytes, so the tests below need no extra crates.
    ///
    /// For example, `bytes(1, 4)` always returns the same four bytes.
    fn bytes(seed: u64, len: usize) -> Vec<u8> {
        // xorshift64
        let mut state = seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1;
        (0..len)
            .map(|_| {
                state ^= state << 13;
                state ^= state >> 7;
                state ^= state << 17;
                state as u8
            })
            .collect()
    }

    #[test]
    fn generated_programs_parse() {
        for seed in 0..2_000 {
            let data = bytes(seed, 512);
            let source = generate(&mut Unstructured::new(&data)).unwrap();
            if let Err(e) = stone::driver::parse(&source) {
                panic!("generated program failed to parse ({e}):\n{source}");
            }
        }
    }

    #[test]
    fn generated_programs_type_check() {
        for seed in 0..2_000 {
            let data = bytes(seed, 512);
            let source = generate(&mut Unstructured::new(&data)).unwrap();
            let diagnostics = stone::driver::check(&source);
            if let Some(first) = diagnostics.first() {
                panic!("generated program failed to check ({first}):\n{source}");
            }
        }
    }

    #[test]
    fn generated_programs_assemble_for_every_target() {
        // only assembles and links in memory, so it can cover many more programs than run
        for seed in 0..2_000 {
            let data = bytes(seed, 512);
            let source = generate(&mut Unstructured::new(&data)).unwrap();
            let sources = stone::project::MapSources::default();
            let (_, loaded) = stone::driver::load(Path::new("main.st"), &source, &sources);
            let Ok((module, _)) = loaded else {
                continue;
            };
            for arch in Architecture::ALL {
                let text = arch.generator().assemble(&module).unwrap();
                let linked = stone::codegen::asm::assemble(&text, arch)
                    .and_then(|object| stone::codegen::elf::link(&object, arch));
                if let Err(e) = linked {
                    panic!("{arch} assembly of a generated program failed ({e}):\n{source}");
                }
            }
        }
    }

    #[test]
    fn generated_programs_agree_across_backends() {
        // builds and runs each program, so keep the count small enough for every test run
        for seed in 0..150 {
            let data = bytes(seed, 512);
            let source = generate(&mut Unstructured::new(&data)).unwrap();
            differential(&source);
        }
    }

    #[test]
    fn generated_programs_cover_the_language() {
        let sources: Vec<String> = (0..500)
            .map(|seed| generate(&mut Unstructured::new(&bytes(seed, 512))).unwrap())
            .collect();
        for feature in [
            "for ",
            " in range(",
            ".append(",
            ".len()",
            "[",
            " < ",
            "not (",
            "print(\"",
            "def ",
            ".",
            "float(",
            "int(",
            " % ",
            " ** ",
        ] {
            let count = sources.iter().filter(|s| s.contains(feature)).count();
            assert!(count >= 25, "only {count} of 500 programs use {feature:?}");
        }
    }

    #[test]
    fn generated_programs_are_mostly_comparable() {
        let comparable = (0..500)
            .filter(|&seed| {
                let data = bytes(seed, 512);
                let source = generate(&mut Unstructured::new(&data)).unwrap();
                interpret(&source).is_some()
            })
            .count();
        // the differential target skips the rest, so a low count means wasted fuzzing time
        assert!(
            comparable > 250,
            "only {comparable} of 500 generated programs can be compared"
        );
    }
}
