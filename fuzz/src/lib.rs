//! Shared pieces of the stone fuzz targets: interpreter limits, a grammar-aware program
//! generator, and the differential check that compares `stone run` with `stone build`.
//!
//! For example, [`generate`] turns fuzzer bytes into a program such as
//! `def f0(p0);\n    ret p0 + 1\nprint(f0(2))\n`, and [`differential`] runs it through both backends.

use arbitrary::{Result, Unstructured};
use std::fs::File;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};
use stone::interpreter::Limits;

/// Limits for fuzzing the interpreter, small enough that every input finishes quickly.
///
/// For example, `while 1;` runs out of fuel after 10,000 iterations instead of hanging.
pub const LIMITS: Limits = Limits {
    fuel: 10_000,
    max_depth: 200,
};

/// Largest integer the compiled `print` can show, since it treats larger values as string
/// pointers.
///
/// For example, `print(4095)` works in both backends, while `print(4096)` crashes a compiled
/// binary.
const MAX_PRINTABLE: i64 = 4095;

/// How long a compiled program may run before the differential check reports a hang.
const RUN_TIMEOUT: Duration = Duration::from_secs(5);

/// Most statements in one block, keeping generated programs small enough to run quickly.
const MAX_BLOCK_LEN: usize = 4;

/// Deepest nesting of blocks and expressions the generator produces.
const MAX_NESTING: usize = 4;

/// Most parameters a generated function takes, matching the compiler's register arguments.
const MAX_PARAMS: usize = 6;

/// Generates a stone program from fuzzer bytes, staying inside the subset that both backends
/// implement the same way.
///
/// The subset avoids constructs where the backends are known to differ: no strings, booleans, or
/// `none` values, no nested functions, no globals read from functions, `print` with exactly one
/// argument, and `break`/`cont` only inside loops. Every `while` loop is bounded by a counter.
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
    /// Variables that can be read and assigned.
    variables: Vec<String>,
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
        match u.int_in_range(0..=9)? {
            0..=2 => {
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
            3 | 4 => {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("print({value})"));
            }
            5 if nested => self.if_statement(u, scope, indent, nesting)?,
            6 if nested => self.while_statement(u, scope, indent, nesting)?,
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
            _ => {
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("print({value})"));
            }
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
    /// `c0 = 0` then `while c0 - 3;` with `c0 = c0 + 1` as the first statement of its body.
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
        self.line(indent, &format!("while {counter} - {bound};"));
        // incrementing first means `cont` cannot skip it
        self.line(indent + 1, &format!("{counter} = {counter} + 1"));

        let mut inner = scope.clone();
        inner.counters.push(counter);
        inner.in_loop = true;
        self.block(u, &inner, indent + 1, nesting + 1)
    }

    /// Generates an `if` or `elif` test, which unlike other expressions may use `not`.
    fn condition(&mut self, u: &mut Unstructured, scope: &Scope) -> Result<String> {
        let value = self.expression(u, scope, 0)?;
        Ok(if u.arbitrary()? {
            format!("not {value}")
        } else {
            value
        })
    }

    /// Generates an integer-valued expression following the grammar's precedence levels, since
    /// stone has no parentheses for grouping.
    ///
    /// ```text
    /// expression: sum ('or' sum)* | sum ('and' sum)*
    /// sum:        term (('+' | '-') term)*
    /// term:       factor (('*' | '/') factor)*
    /// factor:     '-' factor | primary
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
            let op = if u.arbitrary()? { "*" } else { "/" };
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
        self.primary(u, scope, nesting)
    }

    fn primary(&mut self, u: &mut Unstructured, scope: &Scope, nesting: usize) -> Result<String> {
        let readable = scope.readable();
        match u.int_in_range(0..=5)? {
            0 | 1 if !readable.is_empty() => Ok(u.choose(&readable)?.to_string()),
            2 if nesting < MAX_NESTING && !self.functions.is_empty() => {
                self.call(u, scope, nesting + 1)
            }
            3 => Ok(u.int_in_range(0..=i64::MAX)?.to_string()),
            _ => Ok(u.int_in_range(0..=12)?.to_string()),
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

/// Reports whether every printed line is an integer the compiled `print` can show.
///
/// For example, `"1\n2\n"` qualifies, while `"-1\n"`, `"5000\n"`, and `"true\n"` do not.
fn printable_by_compiled_code(output: &str) -> bool {
    output.lines().all(|line| {
        line.parse::<i64>()
            .is_ok_and(|n| (0..=MAX_PRINTABLE).contains(&n))
    })
}

/// Compiles `source` with `stone build`, runs the binary, and returns its stdout.
///
/// For example, `run_compiled("print(1)\n")` returns `Ok("1\n")`. A compile error, crash, or hang
/// is returned as `Err` with a description.
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
        stone::driver::compile(source, &exe).map_err(|e| format!("compile failed: {e}"))?;

        // stdout goes to a file, so a chatty program cannot block on a full pipe
        let stdout = File::create(&stdout_path).map_err(|e| e.to_string())?;
        let mut child = Command::new(&exe)
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
/// skipped, as are programs whose output the compiled `print` cannot show.
pub fn differential(source: &str) {
    let Some(expected) = interpret(source) else {
        return;
    };
    if !printable_by_compiled_code(&expected) {
        return;
    }

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
    fn generated_programs_are_mostly_comparable() {
        let comparable = (0..500)
            .filter(|&seed| {
                let data = bytes(seed, 512);
                let source = generate(&mut Unstructured::new(&data)).unwrap();
                interpret(&source).is_some_and(|out| printable_by_compiled_code(&out))
            })
            .count();
        // the differential target skips the rest, so a low count means wasted fuzzing time
        assert!(
            comparable > 250,
            "only {comparable} of 500 generated programs can be compared"
        );
    }

    #[test]
    fn printable_output() {
        assert!(printable_by_compiled_code("0\n4095\n"));
        assert!(!printable_by_compiled_code("-1\n"));
        assert!(!printable_by_compiled_code("4096\n"));
        assert!(!printable_by_compiled_code("true\n"));
    }
}
