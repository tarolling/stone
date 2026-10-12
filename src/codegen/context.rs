//! Bookkeeping that every backend shares: the checked and lowered program, labels, interned
//! string literals, runtime failures, and which runtime routines the program needs.
//!
//! For example, both backends ask [`Context::fail_label`] for `division by zero`, get the same
//! label back every time, and later emit one stub behind it that prints the message.

use crate::ast::{Expr, ExprKind, Mod, Stmt, StmtKind};
use crate::checker::{Symbol, Type, TypeChecker};
use crate::codegen::AssemblyGenerator;
use crate::codegen::ir::lower::lower;
use crate::codegen::ir::{Callee, Inst, Program};
use crate::diagnostic::Severity;
use crate::project::SourceMap;
use crate::span::Span;
use std::collections::{HashMap, HashSet};

/// Runtime routines for list methods and slices, which `builtins::list_methods` emits in each
/// backend for the ones a program calls.
pub const LIST_ROUTINES: &[&str] = &[
    "stone.list_slice",
    "stone.list_pop",
    "stone.list_insert",
    "stone.list_extend",
    "stone.list_reuse",
];

/// The runtime routines behind string methods, indexes, slices, and ordering, which each
/// backend's `builtins::string_methods` emits when the program calls them.
pub const STRING_METHOD_ROUTINES: &[&str] = &[
    "stone.str_strip",
    "stone.str_split_ws",
    "stone.str_split",
    "stone.str_at",
    "stone.str_slice_of",
    "stone.str_cmp",
    "stone.str_join",
    "stone.str_position",
    "stone.str_starts",
    "stone.str_ends",
    "stone.str_char",
];

/// Runtime routines that need the string runtime, even in a program where no expression is a
/// `str`, such as one that only prints `args()`.
const STRING_ROUTINES: &[&str] = &[
    "stone.parse_int",
    "stone.parse_float",
    "stone.str_strip",
    "stone.str_split_ws",
    "stone.str_split",
    "stone.str_at",
    "stone.str_slice_of",
    "stone.str_cmp",
    "stone.str_join",
    "stone.str_position",
    "stone.str_starts",
    "stone.str_ends",
    "stone.str_char",
    "stone.input",
    "stone.args",
    "stone.os_env",
    "stone.os_platform",
    "stone.os_arch",
    "stone.os_hostname",
    "stone.os_cwd",
];

/// The runtime routine behind each function of a builtin module that calls the runtime, by its
/// linked name. Each module's routines share a prefix, such as `stone.os_` or `stone.time_`.
///
/// For example, `os.env("HOME")` lowers to a call of `stone.os_env`. The `math` module lowers
/// to plain IR instead, so it has none.
pub const MODULE_ROUTINES: [(&str, &str); 14] = [
    ("os.env", "stone.os_env"),
    ("os.has_env", "stone.os_has_env"),
    ("os.platform", "stone.os_platform"),
    ("os.arch", "stone.os_arch"),
    ("os.hostname", "stone.os_hostname"),
    ("os.cpu_count", "stone.os_cpu_count"),
    ("os.pid", "stone.os_pid"),
    ("os.cwd", "stone.os_cwd"),
    ("os.exit", "stone.os_exit"),
    ("random.seed", "stone.random_seed"),
    ("random.random", "stone.random_float"),
    ("time.now", "stone.time_now"),
    ("time.clock", "stone.time_clock"),
    ("time.sleep", "stone.time_sleep"),
];

/// Returns the runtime routine behind the function of a builtin module with the linked name
/// `name`, such as `stone.os_pid` for `os.pid`.
pub fn module_routine(name: &str) -> Option<&'static str> {
    MODULE_ROUTINES
        .iter()
        .find(|(function, _)| *function == name)
        .map(|(_, label)| *label)
}

/// Runtime routines that can stop the program with a runtime error of their own, so a call to one
/// first records where it is, for the error to point at.
///
/// For example, `int("x")` calls `stone.parse_int`, which fails with
/// `invalid literal for int() with base 10: 'x'` at the call.
pub const FALLIBLE_ROUTINES: &[&str] = &[
    "stone.parse_int",
    "stone.parse_float",
    "stone.str_split",
    "stone.str_at",
    "stone.str_slice_of",
    "stone.os_cwd",
    "stone.time_sleep",
];

/// A runtime error the code can jump to, from [`Context::fail_label`] or [`Context::fail_at`].
///
/// A message with holes, such as `stdlib::INDEX_RANGE`, is printed with the ints the code put in
/// the backend's two fail value registers before jumping, or that `setup` puts there, written
/// where the holes are.
#[derive(Clone, Debug, PartialEq)]
pub struct Failure {
    pub label: String,
    pub message: String,
    /// The label of the site the error points at, from [`Context::site_label`], if it has one.
    pub site: Option<String>,
    /// Assembly lines that run first, from [`Context::fail_at_after`], such as lines that work
    /// out the values a message's holes need from the registers at the jump.
    pub setup: Vec<String>,
}

impl Failure {
    /// Returns the text around the message's two holes, or `None` for a message without any.
    ///
    /// For example, `list index out of range (index {}, length {})` gives
    /// `["list index out of range (index ", ", length ", ")"]`.
    pub fn pieces(&self) -> Option<Vec<&str>> {
        let pieces: Vec<&str> = self.message.split("{}").collect();
        (pieces.len() > 1).then_some(pieces)
    }
}

/// Where a runtime error points, rendered when the program is compiled: the location, such as
/// `main.stn:3:7: `, and the excerpt that follows the message, both escaped for a `.string`
/// directive. A backend emits the two strings one after the other behind `label`, so the
/// excerpt starts right after the location's terminating zero.
#[derive(Clone, Debug, PartialEq)]
pub struct Site {
    pub label: String,
    pub location: String,
    pub excerpt: String,
}

/// The reference count of a string literal or other static object, which is never freed: no
/// program can release it `1 << 62` times.
pub const IMMORTAL: i64 = 1 << 62;

/// The smallest block the allocator hands out is `1 << SMALLEST_CLASS` bytes, counting its
/// 8-byte header, so a block on a free list has room for the link to the next one.
pub const SMALLEST_CLASS: i64 = 5;

/// Blocks of up to `1 << LARGEST_CLASS` bytes come from free lists, one per power of two, and
/// larger ones get a mapping of their own, which is returned to the system when freed.
///
/// For example, a 100-byte string is a 128-byte block of class 7, while a list of 10,000
/// elements stores them in a mapping of 80,008 bytes rounded up to whole pages.
pub const LARGEST_CLASS: i64 = 16;

/// The size of each region the allocator maps to cut small blocks from.
pub const HEAP_REGION: i64 = 1 << 20;

/// The size of the buffer compiled programs read stdin into, one `read` syscall at a time.
pub const STDIN_BUFFER: i64 = 1 << 16;

/// The file that lists the processors that are online, as ranges such as `0-3,6`, which
/// `os.cpu_count` reads the way glibc's `sysconf(_SC_NPROCESSORS_ONLN)` does.
pub const CPU_ONLINE: &str = "/sys/devices/system/cpu/online";

/// The size of the buffer `os.cwd` asks the kernel to write the path into, which is the most
/// the `getcwd` syscall can return.
pub const PATH_BUFFER: i64 = 4096;

/// The size of the `utsname` structure the `uname` syscall fills: six fields of 65 bytes, rounded
/// up to keep the stack aligned.
pub const UTSNAME_SIZE: i64 = 400;

/// Where the host name starts in the `utsname` structure, after the system's name.
pub const UTSNAME_FIELD: i64 = 65;

/// How many 64-bit limbs each bignum of the float runtime has room for. The largest is a
/// parsed number's divisor, at most `10^1125`, about 3,740 bits, lined up with its dividend.
pub const BIG_LIMBS: i64 = 64;

/// How many significant digits `float` keeps when it reads a number. A float halfway between
/// two others never needs more than 767, so any digits past these only matter in whether
/// they are all 0, which the runtime remembers as one more digit of 1.
pub const MAX_DIGITS: i64 = 800;

/// The largest exponent `float` reads, such as the `400` of `1e400`. Any larger one gives the
/// same result, infinity or 0, and stopping there keeps the arithmetic from overflowing.
pub const EXPONENT_LIMIT: i64 = 100_000;

/// What the leak check writes before the number of objects never freed.
pub const LEAK_PREFIX: &str = "error: ";

/// What the leak check writes after the number of objects never freed, before a newline.
pub const LEAK_SUFFIX: &str = " objects were never freed";

/// The message the program stops with when the system has no memory left to map.
pub const OUT_OF_MEMORY: &str = "error: out of memory\\n";

/// The length in bytes of [`OUT_OF_MEMORY`] once assembled, where `\\n` is one newline.
pub const OUT_OF_MEMORY_LENGTH: usize = OUT_OF_MEMORY.len() - 1;

/// Emits the string `text` so that it lives as long as the program, preceded by the header
/// every string has: its length in bytes at `[label - 16]` and its reference count at
/// `[label - 8]`, so measuring, retaining, and releasing it works like any other string's.
///
/// For example, `immortal_string(gen, ".Lstone_empty", "")` emits the empty string `stone.input`
/// returns at the end of the input. The caller picks the section, which must be writable.
pub fn immortal_string(r#gen: &mut dyn AssemblyGenerator, label: &str, text: &str) {
    r#gen.emit("\t.balign\t8");
    r#gen.emit(&format!("\t.quad\t{}", text.len()));
    r#gen.emit(&format!("\t.quad\t{IMMORTAL}"));
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\t.string \"{}\"", escape(text)));
}

/// What a backend knows about the program it is compiling, apart from the assembly it writes.
#[derive(Default)]
pub struct Context {
    label_count: usize,
    /// The lowered module, produced by [`Context::lower`].
    pub program: Program,
    string_literals: HashMap<String, String>,
    /// The type of every expression, keyed by span, from the checker.
    types: HashMap<Span, Type>,
    /// Every function and variable, with its type, from the checker.
    symbols: Vec<Symbol>,
    /// List types that `print` needs a printer for, emitted after the code that uses them.
    list_printers: Vec<Type>,
    /// Whether `print` needs `stone.print_float`, emitted after the code that uses it.
    pub prints_floats: bool,
    /// Runtime errors the code can jump to, emitted after `main`.
    failures: Vec<Failure>,
    /// The program's files, which runtime errors are rendered from, if the driver gave them.
    sources: Option<SourceMap>,
    /// Every place a runtime error can point at, by span.
    sites: Vec<(Span, Site)>,
    /// Whether runtime routines build messages for `stone.fail` themselves, so it is needed even
    /// without a [`Context::fail_label`].
    pub needs_fail: bool,
    /// Every runtime routine the lowered program calls, such as `stone.input`.
    runtime: HashSet<&'static str>,
    /// Whether the program has strings or lists, which need the memory runtime, and `main` checks
    /// for leaks before it returns.
    pub counts_references: bool,
}

impl Context {
    /// Runs the checker over `module` and keeps the types and symbols that lowering needs,
    /// returning the first error if the program is invalid.
    ///
    /// For example, checking `x = 1 + "a"` fails with `1:9: error: expected int, found str`.
    pub fn check(&mut self, module: &Mod) -> Result<(), String> {
        // code generation depends on the checker's types, so it only accepts valid programs
        let analysis = TypeChecker::new().analyze(module);
        if let Some(error) = analysis
            .diagnostics
            .iter()
            .find(|d| d.severity == Severity::Error)
        {
            return Err(error.to_string());
        }
        self.types = analysis.types;
        self.symbols = analysis.symbols;
        Ok(())
    }

    /// Lowers the checked `module` to IR, then notes every runtime routine it calls and whether it
    /// counts references.
    pub fn lower(&mut self, module: &Mod) -> Result<(), String> {
        self.program = lower(module, &self.types, &self.symbols)?;
        self.runtime = self
            .program
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.insts)
            .filter_map(|inst| match inst {
                Inst::Call {
                    callee: Callee::Runtime(label),
                    ..
                } => Some(*label),
                _ => None,
            })
            .collect();
        self.counts_references = self.needs_strings() || self.needs_lists();
        Ok(())
    }

    /// Returns a new local label starting with `prefix`.
    ///
    /// For example, the first `new_label("block")` is `.Lblock_0`.
    pub fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_count);
        self.label_count += 1;
        label
    }

    /// Returns the label of the string literal `content`, the same one for every use.
    pub fn intern_string(&mut self, content: &str) -> String {
        // reuse an existing label
        if let Some(label) = self.string_literals.get(content) {
            return label.clone();
        }

        // new label for this string
        let label = self.new_label("str");
        self.string_literals
            .insert(content.to_string(), label.clone());
        label
    }

    /// Returns a label that stops the program with `message`, the way the interpreter reports the
    /// same runtime error.
    ///
    /// For example, jumping to `self.fail_label("division by zero")` prints
    /// `error: division by zero` to stderr and exits with status 1.
    ///
    /// The error points at the site a fallible routine's caller recorded, if any, so runtime
    /// routines use this label; generated code uses [`Context::fail_at`].
    pub fn fail_label(&mut self, message: &str) -> String {
        self.failure(message, None, Vec::new())
    }

    /// Returns a label that stops the program with `message`, pointing at `span` the way the
    /// interpreter reports the same runtime error there.
    ///
    /// For example, jumping to the label for `division by zero` at the span of `1 / n` on line 3
    /// of `main.stn` prints `main.stn:3:7: error: division by zero`, then the line with carets
    /// under `1 / n`. Without sources (see [`Context::set_sources`]) it prints only the message.
    pub fn fail_at(&mut self, message: &str, span: Span) -> String {
        self.fail_at_after(message, span, Vec::new())
    }

    /// Returns a label that runs the assembly lines `setup`, then stops the program with
    /// `message` pointing at `span`, as [`Context::fail_at`] does.
    ///
    /// For example, an inline list index check that jumps here with the length still in memory
    /// passes lines that load it into a fail value register, for `(index {}, length {})`.
    pub fn fail_at_after(&mut self, message: &str, span: Span, setup: Vec<String>) -> String {
        let site = self.site_label(span);
        self.failure(message, site, setup)
    }

    fn failure(&mut self, message: &str, site: Option<String>, setup: Vec<String>) -> String {
        if let Some(failure) = self
            .failures
            .iter()
            .find(|f| f.message == message && f.site == site && f.setup == setup)
        {
            return failure.label.clone();
        }
        let label = self.new_label("fail");
        self.failures.push(Failure {
            label: label.clone(),
            message: message.to_string(),
            site,
            setup,
        });
        label
    }

    /// Returns every failure asked for, or `None` if no code can fail, so `stone.fail` need not
    /// be emitted.
    pub fn failures(&self) -> Option<Vec<Failure>> {
        (!self.failures.is_empty() || self.needs_fail).then(|| self.failures.clone())
    }

    /// Gives the program's files, so runtime errors point at their place in them.
    pub fn set_sources(&mut self, sources: SourceMap) {
        self.sources = Some(sources);
    }

    /// Returns the label of the site record for `span`, the same one for every use, or `None`
    /// without sources or for a span that is in no source, such as code `main` adds.
    pub fn site_label(&mut self, span: Span) -> Option<String> {
        let sources = self.sources.as_ref()?;
        if span.start.line == 0 {
            return None;
        }
        if let Some((_, site)) = self.sites.iter().find(|(s, _)| *s == span) {
            return Some(site.label.clone());
        }
        let (location, excerpt) = sources.site(span);
        let label = self.new_label("site");
        self.sites.push((
            span,
            Site {
                label: label.clone(),
                location: escape(&location),
                excerpt: escape(&excerpt),
            },
        ));
        Some(label)
    }

    /// Returns every site record asked for, in the order they were asked for.
    pub fn sites(&self) -> Vec<Site> {
        self.sites.iter().map(|(_, site)| site.clone()).collect()
    }

    /// Returns the routine that prints a value of type `ty`, quoting strings inside lists.
    ///
    /// For example, `list[int]` is printed by `stone.print_list_int`, which is emitted later.
    pub fn print_routine(&mut self, ty: &Type, nested: bool) -> Result<String, String> {
        Ok(match ty {
            Type::Int => "stone.print_int".to_string(),
            Type::Float => {
                self.prints_floats = true;
                "stone.print_float".to_string()
            }
            Type::Bool => "stone.print_bool".to_string(),
            Type::Str if nested => "stone.print_str_quoted".to_string(),
            Type::Str => "stone.print_str".to_string(),
            Type::None => "stone.print_none".to_string(),
            Type::List(_) => {
                if !self.list_printers.contains(ty) {
                    self.list_printers.push(ty.clone());
                }
                format!("stone.print_{}", mangle(ty))
            }
            // lowering prints a tuple, and a list holding one, a value at a time
            Type::Tuple(_) => return Err("a tuple is printed a value at a time".to_string()),
            Type::Function { .. } => return Err("functions cannot be printed".to_string()),
        })
    }

    /// Returns every list printer `print` asked for, as `(label, element routine)`, including
    /// the printers their elements need.
    ///
    /// For example, printing a `list[list[int]]` needs `stone.print_list_list_int`, which calls
    /// `stone.print_list_int` for each element, which calls `stone.print_int`.
    pub fn list_printers(&mut self) -> Result<Vec<(String, String)>, String> {
        // printing a nested list registers the printer for its elements, so work until none are new
        let mut printers = Vec::new();
        let mut emitted = 0;
        while emitted < self.list_printers.len() {
            let ty = self.list_printers[emitted].clone();
            emitted += 1;
            let Type::List(elem) = &ty else {
                continue;
            };
            let element = self.print_routine(elem, true)?;
            printers.push((format!("stone.print_{}", mangle(&ty)), element));
        }
        Ok(printers)
    }

    /// Returns whether the program needs the string runtime.
    pub fn needs_strings(&self) -> bool {
        self.types.values().any(contains_str) || self.uses(STRING_ROUTINES)
    }

    /// Returns whether the program needs the list runtime.
    pub fn needs_lists(&self) -> bool {
        self.types.values().any(contains_list)
            || self.uses(&["stone.args", "stone.str_split_ws", "stone.str_split"])
    }

    /// Returns whether the program reads its environment: through `os.env` or `os.has_env`, or
    /// through the leak check every program with strings or lists ends with, which looks for
    /// `STONE_LEAK_CHECK`.
    pub fn needs_env(&self) -> bool {
        self.counts_references || self.uses(&["stone.os_env", "stone.os_has_env"])
    }

    /// Returns the runtime routines of a builtin module that the program calls, by the prefix
    /// their labels share, such as `stone.os_pid` for `stone.os_`.
    pub fn module_routines(&self, prefix: &str) -> Vec<&'static str> {
        MODULE_ROUTINES
            .iter()
            .map(|(_, label)| *label)
            .filter(|label| label.starts_with(prefix) && self.uses(&[label]))
            .collect()
    }

    /// Returns the failure label for `message` that the runtime routine `routine` jumps to, if
    /// the program calls it, or an empty string, so programs that cannot fail need no
    /// `stone.fail`.
    ///
    /// For example, `failure_of("stone.os_cwd", CWD_FAILURE)` is a label only in a program that
    /// calls `os.cwd`.
    pub fn failure_of(&mut self, routine: &str, message: &str) -> String {
        if self.uses(&[routine]) {
            self.fail_label(message)
        } else {
            String::new()
        }
    }

    /// Returns whether the program calls any of the runtime routines in `labels`.
    pub fn uses(&self, labels: &[&str]) -> bool {
        labels.iter().any(|label| self.runtime.contains(label))
    }

    /// Returns whether the program needs the `print` routines: it calls `print`, or `input`, which
    /// prints its prompt, or `str` of a float, which shares `stone.print_float`'s code.
    pub fn needs_print(&self, module: &Mod) -> bool {
        let mut calls = HashSet::new();
        match module {
            Mod::Module { body } => {
                for stmt in body {
                    collect_calls_from_stmt(stmt, &mut calls);
                }
            }
        }
        calls.contains("print") || self.uses(&["stone.input", "stone.str_float"])
    }

    /// Returns every interned string literal as `(label, text)`, sorted by label so the output
    /// is stable, for [`immortal_string`] to emit.
    pub fn string_literals(&self) -> Vec<(String, String)> {
        let mut literals: Vec<(String, String)> = self
            .string_literals
            .iter()
            .map(|(content, label)| (label.clone(), content.clone()))
            .collect();
        literals.sort();
        literals
    }
}

/// Escapes `content` for a `.string` directive. Other control characters are written as three
/// octal digits, which GNU as reads, since its `\x` would also take any hex digits that follow.
///
/// For example, `a"b` followed by a newline becomes `a\"b\n`, and the escape character
/// (`\x1b`) becomes `\033`.
pub fn escape(content: &str) -> String {
    let mut escaped = String::with_capacity(content.len());
    for ch in content.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\t' => escaped.push_str("\\t"),
            '\r' => escaped.push_str("\\r"),
            '\0'..='\x1f' | '\x7f' => escaped.push_str(&format!("\\{:03o}", ch as u32)),
            _ => escaped.push(ch),
        }
    }
    escaped
}

/// Returns the assembly symbol for a global variable.
///
/// The prefix keeps variables from colliding with functions or registers, so `global_label("rax")`
/// is `g.rax`.
pub fn global_label(name: &str) -> String {
    format!("g.{name}")
}

/// Returns the assembly symbol for a user-defined function.
///
/// The prefix keeps stone functions from colliding with `main` or with C library functions, so
/// `function_label("exit")` is `fn.exit`.
pub fn function_label(name: &str) -> String {
    format!("fn.{name}")
}

/// Returns whether a value of type `ty` involves a list, which means the list runtime is needed.
fn contains_list(ty: &Type) -> bool {
    match ty {
        Type::List(_) => true,
        Type::Tuple(fields) => fields.iter().any(contains_list),
        Type::Function { params, ret } => params.iter().any(contains_list) || contains_list(ret),
        _ => false,
    }
}

/// Returns whether a value of type `ty` is or holds a string, outside of lists.
///
/// For example, `str` and `tuple[int, str]` do, and `list[str]` does not.
fn contains_str(ty: &Type) -> bool {
    match ty {
        Type::Str => true,
        Type::Tuple(fields) => fields.iter().any(contains_str),
        _ => false,
    }
}

/// Spells a type as part of an assembly symbol.
///
/// For example, `list[list[str]]` becomes `list_list_str`.
fn mangle(ty: &Type) -> String {
    match ty {
        Type::List(elem) => format!("list_{}", mangle(elem)),
        other => other.to_string(),
    }
}

/// Adds the name of every builtin that `stmt` calls to `calls`.
fn collect_calls_from_stmt(stmt: &Stmt, calls: &mut HashSet<String>) {
    match &stmt.kind {
        StmtKind::Expr { value } => collect_calls_from_expr(value, calls),
        StmtKind::Assign { targets, value } => {
            for target in targets {
                collect_calls_from_expr(target, calls);
            }
            collect_calls_from_expr(value, calls);
        }
        StmtKind::AugAssign { target, value, .. } => {
            collect_calls_from_expr(target, calls);
            collect_calls_from_expr(value, calls);
        }
        StmtKind::Return { value } => {
            if let Some(v) = value {
                collect_calls_from_expr(v, calls);
            }
        }
        StmtKind::If { test, body, orelse } => {
            collect_calls_from_expr(test, calls);
            for s in body.iter().chain(orelse) {
                collect_calls_from_stmt(s, calls);
            }
        }
        StmtKind::While { test, body } => {
            collect_calls_from_expr(test, calls);
            for s in body {
                collect_calls_from_stmt(s, calls);
            }
        }
        StmtKind::For { target, iter, body } => {
            collect_calls_from_expr(target, calls);
            collect_calls_from_expr(iter, calls);
            for s in body {
                collect_calls_from_stmt(s, calls);
            }
        }
        StmtKind::FunctionDef { body, .. } => {
            for s in body {
                collect_calls_from_stmt(s, calls);
            }
        }
        StmtKind::Break | StmtKind::Continue | StmtKind::Use { .. } => {}
    }
}

/// Adds the name of every builtin that `expr` calls to `calls`.
fn collect_calls_from_expr(expr: &Expr, calls: &mut HashSet<String>) {
    match &expr.kind {
        ExprKind::Call { func, args } => {
            if let ExprKind::Name { id, .. } = &func.kind
                && crate::stdlib::is_builtin(id)
            {
                calls.insert(id.clone());
            }

            // arguments too
            collect_calls_from_expr(func, calls);
            for arg in args {
                collect_calls_from_expr(arg, calls);
            }
        }
        ExprKind::BinOp { left, right, .. } => {
            collect_calls_from_expr(left, calls);
            collect_calls_from_expr(right, calls);
        }
        ExprKind::UnaryOp { operand, .. } => collect_calls_from_expr(operand, calls),
        ExprKind::BoolOp { values, .. } => {
            for val in values {
                collect_calls_from_expr(val, calls);
            }
        }
        ExprKind::Compare {
            left, comparators, ..
        } => {
            collect_calls_from_expr(left, calls);
            for comp in comparators {
                collect_calls_from_expr(comp, calls);
            }
        }
        ExprKind::Subscript { value, slice, .. } => {
            collect_calls_from_expr(value, calls);
            collect_calls_from_expr(slice, calls);
        }
        ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
            for elt in elts {
                collect_calls_from_expr(elt, calls);
            }
        }
        ExprKind::Attribute { value, .. } => collect_calls_from_expr(value, calls),
        ExprKind::Slice { lower, upper, step } => {
            for bound in [lower, upper, step].into_iter().flatten() {
                collect_calls_from_expr(bound, calls);
            }
        }
        ExprKind::Constant { .. } | ExprKind::Name { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn escape_writes_control_bytes_in_octal() {
        assert_eq!(escape("a\"b\\\n\t\r"), "a\\\"b\\\\\\n\\t\\r");
        // three digits always, so a digit after the escape stays a digit
        assert_eq!(escape("\x01\x1b7\x7f"), "\\001\\0337\\177");
        assert_eq!(escape("é"), "é");
    }
}
