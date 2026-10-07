//! The x86-64 backend, which compiles a stone AST to GNU assembler source in Intel syntax.
//!
//! The AST is first lowered to [`ir`](crate::codegen::ir), then each function's vregs are
//! assigned registers by linear scan and emitted (see `x64/emit.rs`). For example, `x = 42` in a
//! function becomes `mov` of 42 into whichever register holds `x`.

pub mod builtins;
mod emit;

use crate::ast::{Expr, ExprKind, Mod, Stmt, StmtKind};
use crate::checker::{Type, TypeChecker};
use crate::codegen::ir::lower::lower;
use crate::codegen::ir::{Callee, Inst, Program};
use crate::codegen::x64::builtins::print;
use crate::codegen::{Architecture, AssemblyGenerator};
use crate::span::Span;
use crate::stdlib::BUILTINS;
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// Runtime routines that need the string runtime, even in a program where no expression is a
/// `str`, such as one that only prints `args()`.
const STRING_ROUTINES: &[&str] = &[
    "stone.parse_int",
    "stone.parse_float",
    "stone.str_strip",
    "stone.str_split_ws",
    "stone.str_split",
];

/// Registers that carry arguments under the System V ABI, in order.
///
/// Calls between stone functions pass the first six arguments in these registers, and push any
/// past the sixth on the stack, first argument deepest. So with `n` arguments, argument `i >= 6`
/// is at `[rbp + 16 + 8 * (n - 1 - i)]` in the callee.
const ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Code generator for x86-64 that emits GNU assembler source in Intel syntax.
///
/// For example, `a + b` with `a` and `b` in registers becomes a `mov` and an `add`.
pub struct X64Generator {
    output: String,
    label_count: usize,
    /// The lowered module, produced by [`AssemblyGenerator::scan`].
    program: Program,
    string_literals: HashMap<String, String>,
    /// The type of every expression, keyed by span, from the checker.
    types: HashMap<Span, Type>,
    /// List types that `print` needs a printer for, emitted after the code that uses them.
    list_printers: Vec<Type>,
    /// Whether `print` needs `stone.print_float`, emitted after the code that uses it.
    prints_floats: bool,
    /// Runtime errors the code can jump to, as `(label, message)`, emitted after `main`.
    failures: Vec<(String, String)>,
    /// Whether runtime routines build messages for `stone.fail` themselves, so it is needed even
    /// without a [`X64Generator::fail_label`].
    needs_fail: bool,
    /// Every runtime routine the lowered program calls, such as `stone.input`.
    runtime: HashSet<&'static str>,
}

impl AssemblyGenerator for X64Generator {
    fn compile(&mut self, module: &Mod, output: &Path) -> std::io::Result<()> {
        let text = self.assemble(module).map_err(std::io::Error::other)?;

        let assembly = output.with_extension("s");
        if let Some(dir) = output.parent() {
            std::fs::create_dir_all(dir)?;
        }
        std::fs::write(&assembly, text)?;

        // assemble and link here for now
        let status = std::process::Command::new("gcc")
            .arg("-g")
            .arg("-no-pie")
            .arg("-o")
            .arg(output)
            .arg(&assembly)
            .status()?;

        if !status.success() {
            return Err(std::io::Error::other(format!("gcc failed with {status}")));
        }

        Ok(())
    }

    fn scan(&mut self, module: &Mod) -> Result<(), String> {
        self.program = lower(module, &self.types)?;
        Ok(())
    }

    fn generate(&mut self, module: &Mod) -> Result<(), String> {
        self.emit("\t.intel_syntax noprefix");
        self.emit("\t.text");

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

        // only emit the stdlib functions that are actually called
        let stdlib_calls = self.collect_stdlib_calls(module);
        self.emit_stdlib(stdlib_calls);

        // stone functions first, then main, which the lowering puts last
        for function in std::mem::take(&mut self.program.functions) {
            self.emit_function(&function)?;
        }

        self.emit_io_runtime();
        self.emit_failures();
        self.emit_list_runtime()?;
        if self.prints_floats || self.uses(&["stone.str_float"]) {
            builtins::float_runtime(self);
        }
        if self.types.values().any(|ty| *ty == Type::Str) || self.uses(STRING_ROUTINES) {
            builtins::string_runtime(self);
        }

        self.emit_rodata();
        self.emit_globals();

        // suppress the linker's executable stack warning
        self.emit("\t.section\t.note.GNU-stack,\"\",@progbits");

        Ok(())
    }

    fn emit(&mut self, code: &str) {
        self.output.push_str(code);
        self.output.push('\n');
    }

    fn architecture(&self) -> Architecture {
        Architecture::X64
    }
}

impl Default for X64Generator {
    fn default() -> Self {
        Self::new()
    }
}

impl X64Generator {
    /// Runs both compilation passes and returns the assembly text, without assembling or linking.
    ///
    /// For example, assembling the module for `print(1)` returns text containing `main:` and
    /// `call stone.print_int`.
    pub fn assemble(&mut self, module: &Mod) -> Result<String, String> {
        // code generation depends on the checker's types, so it only accepts valid programs
        let analysis = TypeChecker::new().analyze(module);
        if let Some(error) = analysis.diagnostics.first() {
            return Err(error.to_string());
        }
        self.types = analysis.types;

        // first pass lowers to IR, and the second allocates registers and emits
        self.scan(module)?;
        self.generate(module)?;
        Ok(self.output.clone())
    }

    pub fn new() -> Self {
        X64Generator {
            output: String::new(),
            label_count: 0,
            program: Program::default(),
            string_literals: HashMap::new(),
            types: HashMap::new(),
            list_printers: Vec::new(),
            prints_floats: false,
            failures: Vec::new(),
            needs_fail: false,
            runtime: HashSet::new(),
        }
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_count);
        self.label_count += 1;
        label
    }

    fn intern_string(&mut self, content: &str) -> String {
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
    fn fail_label(&mut self, message: &str) -> String {
        if let Some((label, _)) = self.failures.iter().find(|(_, m)| m == message) {
            return label.clone();
        }
        let label = self.new_label("fail");
        self.failures.push((label.clone(), message.to_string()));
        label
    }

    /// Emits the code behind every [`X64Generator::fail_label`], plus `stone.fail`, which writes
    /// `error: `, the string in `rdi`, and a newline to stderr, then exits with status 1.
    fn emit_failures(&mut self) {
        if self.failures.is_empty() && !self.needs_fail {
            return;
        }
        for (label, message) in self.failures.clone() {
            let text = self.intern_string(&message);
            self.emit(&format!("{label}:"));
            self.emit(&format!("\tlea\trdi, [rip + {text}]"));
            self.emit("\tjmp\tstone.fail");
        }
        let prefix = self.intern_string("error: ");
        let newline = self.intern_string("\n");
        self.emit("stone.fail:");
        self.emit("\tmov\tr12, rdi");
        for text in [prefix, "r12".to_string(), newline] {
            if text == "r12" {
                self.emit("\tmov\trsi, r12");
            } else {
                self.emit(&format!("\tlea\trsi, [rip + {text}]"));
            }
            // strlen, then write to stderr
            self.emit("\txor\trdx, rdx");
            let length = self.new_label("fail_length");
            let write = self.new_label("fail_write");
            self.emit(&format!("{length}:"));
            self.emit("\tcmp\tbyte ptr [rsi + rdx], 0");
            self.emit(&format!("\tje\t{write}"));
            self.emit("\tinc\trdx");
            self.emit(&format!("\tjmp\t{length}"));
            self.emit(&format!("{write}:"));
            self.emit("\tmov\trax, 1"); // sys_write
            self.emit("\tmov\trdi, 2"); // stderr
            self.emit("\tsyscall");
        }
        self.emit("\tmov\trax, 231"); // sys_exit_group
        self.emit("\tmov\trdi, 1");
        self.emit("\tsyscall");
    }

    /// Returns the routine that prints a value of type `ty`, quoting strings inside lists.
    ///
    /// For example, `list[int]` is printed by `stone.print_list_int`, which is emitted later.
    fn print_routine(&mut self, ty: &Type, nested: bool) -> Result<String, String> {
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
            Type::Function { .. } => return Err("functions cannot be printed".to_string()),
        })
    }

    /// Returns whether the program calls any of the runtime routines in `labels`.
    fn uses(&self, labels: &[&str]) -> bool {
        labels.iter().any(|label| self.runtime.contains(label))
    }

    /// Emits the routines behind input, `args`, parsing, `str`, and the string methods that the
    /// program calls, before the failures, since some of them fail through `stone.fail`.
    fn emit_io_runtime(&mut self) {
        if self.uses(&["stone.input", "stone.eof"]) {
            builtins::io_runtime(self);
        }
        if self.uses(&["stone.args"]) {
            builtins::args_runtime(self);
        }
        if self.uses(&["stone.parse_int", "stone.parse_float"]) {
            builtins::parse_runtime(self);
            self.needs_fail = true;
        }
        if self.uses(&["stone.str_int", "stone.str_bool"]) {
            builtins::conversion_runtime(self);
        }
        if self.uses(&["stone.str_strip", "stone.str_split_ws", "stone.str_split"]) {
            let empty_separator = self.fail_label("empty separator");
            builtins::string_methods(self, &empty_separator);
        }
    }

    /// Emits the list runtime and every list printer `print` asked for, if the program uses lists.
    fn emit_list_runtime(&mut self) -> Result<(), String> {
        let needed = self.uses(&["stone.args", "stone.str_split_ws", "stone.str_split"]);
        if !needed && !self.types.values().any(contains_list) {
            return Ok(());
        }
        builtins::list_runtime(self);

        // printing a nested list registers the printer for its elements, so work until none are new
        let mut emitted = 0;
        while emitted < self.list_printers.len() {
            let ty = self.list_printers[emitted].clone();
            emitted += 1;
            let Type::List(elem) = &ty else {
                continue;
            };
            let element = self.print_routine(elem, true)?;
            builtins::print_list(self, &format!("stone.print_{}", mangle(&ty)), &element);
        }
        Ok(())
    }

    /// Returns the standard library functions that the module calls.
    ///
    /// For example, a program that only calls `print` returns `["print"]`, so `len` is never emitted.
    fn collect_stdlib_calls(&self, module: &Mod) -> Vec<String> {
        let mut calls = HashSet::new();

        match module {
            Mod::Module { body } => {
                for stmt in body {
                    self.collect_calls_from_stmt(stmt, &mut calls);
                }
            }
        }

        calls.into_iter().collect()
    }

    fn collect_calls_from_stmt(&self, stmt: &Stmt, calls: &mut HashSet<String>) {
        match &stmt.kind {
            StmtKind::Expr { value } => self.collect_calls_from_expr(value, calls),
            StmtKind::Assign { targets, value } => {
                for target in targets {
                    self.collect_calls_from_expr(target, calls);
                }
                self.collect_calls_from_expr(value, calls);
            }
            StmtKind::Return { value } => {
                if let Some(v) = value {
                    self.collect_calls_from_expr(v, calls);
                }
            }
            StmtKind::If { test, body, orelse } => {
                self.collect_calls_from_expr(test, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
                for s in orelse {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            StmtKind::While { test, body } => {
                self.collect_calls_from_expr(test, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            StmtKind::For { target, iter, body } => {
                self.collect_calls_from_expr(target, calls);
                self.collect_calls_from_expr(iter, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            StmtKind::FunctionDef { body, .. } => {
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            StmtKind::Delete { targets } => {
                for target in targets {
                    self.collect_calls_from_expr(target, calls);
                }
            }
            StmtKind::Break | StmtKind::Continue | StmtKind::Use { .. } => {}
        }
    }

    fn collect_calls_from_expr(&self, expr: &Expr, calls: &mut HashSet<String>) {
        match &expr.kind {
            ExprKind::Call { func, args } => {
                // stdlib call
                if let ExprKind::Name { id, .. } = &func.kind
                    && self.is_stdlib_function(id)
                {
                    calls.insert(id.clone());
                }

                // arguments too
                self.collect_calls_from_expr(func, calls);
                for arg in args {
                    self.collect_calls_from_expr(arg, calls);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.collect_calls_from_expr(left, calls);
                self.collect_calls_from_expr(right, calls);
            }
            ExprKind::UnaryOp { operand, .. } => {
                self.collect_calls_from_expr(operand, calls);
            }
            ExprKind::BoolOp { values, .. } => {
                for val in values {
                    self.collect_calls_from_expr(val, calls);
                }
            }
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.collect_calls_from_expr(left, calls);
                for comp in comparators {
                    self.collect_calls_from_expr(comp, calls);
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                self.collect_calls_from_expr(value, calls);
                self.collect_calls_from_expr(slice, calls);
            }
            ExprKind::List { elts, .. } => {
                for elt in elts {
                    self.collect_calls_from_expr(elt, calls);
                }
            }
            ExprKind::Attribute { value, .. } => self.collect_calls_from_expr(value, calls),
            ExprKind::Constant { .. } | ExprKind::Name { .. } => {}
        }
    }

    #[inline(always)]
    fn is_stdlib_function(&self, name: &str) -> bool {
        BUILTINS.contains(&name)
    }

    /// Emits the `print` routines if the program prints. The list and string runtimes are emitted
    /// separately, based on the types the program uses.
    fn emit_stdlib(&mut self, calls: Vec<String>) {
        // input prints its prompt, and str of a float shares print_float's code
        if calls.iter().any(|call| call == "print")
            || self.uses(&["stone.input", "stone.str_float"])
        {
            self.emit("\t# Standard Library Functions");
            print(self);
        }
    }

    fn emit_rodata(&mut self) {
        if self.string_literals.is_empty() {
            return;
        }

        self.emit("");
        self.emit("\t.section\t.rodata");

        for (content, label) in &self.string_literals.clone() {
            self.emit(&format!("{}:", label));

            // escape special characters for assembly
            let escaped = content
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\t", "\\t")
                .replace("\r", "\\r");

            self.emit(&format!("\t.string \"{}\"", escaped));
        }

        self.emit("");
    }

    /// Emits the `.bss` data: the count of active calls, and a zeroed 8-byte slot for each global
    /// variable, plus a flag that is set once the global is assigned.
    ///
    /// For example, `total = 1` at the top level produces `g.total` and `g.total.set`.
    fn emit_globals(&mut self) {
        self.emit("\t.bss");
        self.emit("\t.p2align\t3");
        self.emit("stone.call_depth:");
        self.emit("\t.zero\t8");
        if self.uses(&["stone.args"]) {
            // main saves its argc and argv here for args()
            self.emit("stone.argc:");
            self.emit("\t.zero\t8");
            self.emit("stone.argv:");
            self.emit("\t.zero\t8");
        }
        for name in self.program.globals.clone() {
            let label = global_label(&name);
            self.emit(&format!("{label}:"));
            self.emit("\t.zero\t8");
            self.emit(&format!("{label}.set:"));
            self.emit("\t.zero\t8");
        }
        self.emit("\t.text");
    }
}

/// Returns the assembly symbol for a global variable.
///
/// The prefix keeps variables from colliding with functions or registers, so `global_label("rax")`
/// is `g.rax`.
fn global_label(name: &str) -> String {
    format!("g.{name}")
}

/// Returns the assembly symbol for a user-defined function.
///
/// The prefix keeps stone functions from colliding with `main` or with C library functions, so
/// `function_label("exit")` is `fn.exit`.
fn function_label(name: &str) -> String {
    format!("fn.{name}")
}

/// Returns whether a value of type `ty` involves a list, which means the list runtime is needed.
fn contains_list(ty: &Type) -> bool {
    match ty {
        Type::List(_) => true,
        Type::Function { params, ret } => params.iter().any(contains_list) || contains_list(ret),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::parse;

    /// Parses and assembles `source` without invoking gcc.
    fn assemble(source: &str) -> Result<String, String> {
        let module = parse(source).map_err(|e| e.to_string())?;
        X64Generator::new().assemble(&module)
    }

    #[test]
    fn assemble_returns_assembly_text() {
        let assembly = assemble("print(1)\n").unwrap();
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("\tcall\tstone.print_int"));
    }

    #[test]
    fn invalid_programs_are_rejected_before_code_generation() {
        assert_eq!(
            assemble("x = 1 + \"a\"\n"),
            Err("1:9: error: expected int, found str".to_string())
        );
    }

    #[test]
    fn more_than_six_parameters_assemble() {
        assert!(
            assemble("def f(a, b, c, d, e, g, h);\n    ret h\nprint(f(1, 2, 3, 4, 5, 6, 7))\n")
                .is_ok()
        );
    }

    #[test]
    fn print_takes_any_number_of_arguments() {
        assert!(assemble("print(1, 2, 3, 4, 5, 6, 7)\n").is_ok());
    }

    #[test]
    fn division_by_a_power_of_two_shifts_instead_of_dividing() {
        let assembly = assemble("x = 7\nprint(x / 8, x / -2)\n").unwrap();
        assert!(!assembly.contains("idiv"), "{assembly}");
        assert!(assembly.contains("\tsar\trax, 3"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
    }

    #[test]
    fn division_by_another_nonzero_constant_skips_the_checks() {
        let assembly = assemble("x = 7\nprint(x / 10, x / -3)\n").unwrap();
        assert!(assembly.contains("\tidiv\trcx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        assert!(!assembly.contains("integer overflow"), "{assembly}");
    }

    #[test]
    fn modulo_by_a_safe_constant_skips_the_checks() {
        let assembly = assemble("x = 7\nprint(x % 10, x % -8)\n").unwrap();
        assert!(assembly.contains("\tidiv\trcx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        let checked = assemble("x = 7\ny = 3\nprint(x % y)\n").unwrap();
        assert!(checked.contains("division by zero"), "{checked}");
        assert!(!checked.contains("integer overflow"), "{checked}");
    }

    #[test]
    fn powers_and_float_remainders_do_not_call_libc() {
        let assembly =
            assemble("x = 2.5\nn = 3\nprint(x ** n, x ** -2, x % 0.75, n ** n, n ** 2)\n").unwrap();
        assert!(!assembly.contains("\tcall\tpow"), "{assembly}");
        assert!(!assembly.contains("\tcall\tfmod"), "{assembly}");
        assert!(assembly.contains("\tfprem"), "{assembly}");
        // only the exponent that is not a literal is checked
        assert_eq!(assembly.matches("\tjs\t").count(), 1, "{assembly}");
    }

    #[test]
    fn list_indexing_checks_bounds_inline() {
        let assembly = assemble("xs = [1, 2]\nxs[0] = xs[-1]\nprint(xs[1])\n").unwrap();
        assert!(!assembly.contains("stone.list_slot"), "{assembly}");
        assert!(assembly.contains("list index out of range"), "{assembly}");
    }

    #[test]
    fn float_arithmetic_uses_sse() {
        let assembly = assemble("a = 1.5\nb = -a * 2.0 + a / 0.5\nprint(a < b)\n").unwrap();
        for instruction in [
            "\tmulsd\t",
            "\taddsd\t",
            "\tdivsd\t",
            "\tucomisd\t",
            "\tbtc\t",
        ] {
            assert!(assembly.contains(instruction), "{instruction}");
        }
        // 1.5 is loaded by its bits
        assert!(assembly.contains("\tmov\trax, 4609434218613702656"));
        // even a literal float divisor is checked, unlike an int one
        assert!(assembly.contains("division by zero"));
    }

    #[test]
    fn float_printing_is_only_emitted_when_needed() {
        let assembly = assemble("print([1.5])\n").unwrap();
        assert!(assembly.contains("stone.print_float:"));
        assert!(assembly.contains("\tcall\tsnprintf"));
        let assembly = assemble("x = 1.5\nprint(1)\n").unwrap();
        assert!(!assembly.contains("stone.print_float:"));
    }

    #[test]
    fn converting_a_float_to_an_int_is_checked() {
        let assembly = assemble("print(int(2.5), float(2))\n").unwrap();
        assert!(assembly.contains("\tcvttsd2si\trax, xmm0"));
        assert!(assembly.contains("\tcvtsi2sd\txmm0, rax"));
        assert!(assembly.contains("cannot convert float to int"));
    }

    /// Returns the assembly of stone function `name`, from its label to the next function's.
    fn function_text(assembly: &str, name: &str) -> String {
        let start = assembly
            .find(&format!("{}:\n", function_label(name)))
            .unwrap();
        let rest = &assembly[start..];
        let end = ["\nfn.", "\n\t.globl\tmain"]
            .iter()
            .filter_map(|next| rest.find(next))
            .min()
            .unwrap_or(rest.len());
        rest[..end].to_string()
    }

    const LCG: &str = "def lcg(rounds);\n    x = 1\n    total = 0\n    i = 0\n    \
                       while i < rounds;\n        x = x * 1103515245 + 12345\n        \
                       x = x - (x / 2147483648) * 2147483648\n        \
                       total = total + x / 65536\n        i = i + 1\n    ret total\n\
                       print(lcg(20))\n";

    const FIB: &str = "def fib(n);\n    if n < 2;\n        ret n\n    \
                       ret fib(n - 1) + fib(n - 2)\nprint(fib(10))\n";

    #[test]
    fn a_call_free_loop_keeps_its_variables_in_registers() {
        let lcg = function_text(&assemble(LCG).unwrap(), "lcg");
        assert!(!lcg.contains("[rbp -"), "{lcg}");
        assert!(!lcg.contains("\tpush\trax"), "{lcg}");
        assert!(!lcg.contains("idiv"), "{lcg}");
    }

    #[test]
    fn functions_save_exactly_the_callee_saved_registers_they_use() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        let mut saved = 0;
        for reg in ["rbx", "r12", "r13", "r14", "r15"] {
            let used = fib.contains(&format!(", {reg}")) || fib.contains(&format!("\t{reg},"));
            let pushed = fib.contains(&format!("\tpush\t{reg}\n"));
            let popped = fib.contains(&format!("\tpop\t{reg}\n"));
            assert_eq!(used, pushed, "{reg} in\n{fib}");
            assert_eq!(used, popped, "{reg} in\n{fib}");
            saved += used as usize;
        }
        // n and the first call's result both live across a call
        assert!(saved >= 2, "{fib}");
    }

    #[test]
    fn calls_pass_arguments_in_registers() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        assert!(!fib.contains("[rsp"), "{fib}");
    }

    #[test]
    fn arguments_are_computed_in_their_argument_registers() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        assert!(fib.contains("\tsub\trdi, 1\n\tcall\tfn.fib"), "{fib}");
        assert!(fib.contains("\tsub\trdi, 2\n\tcall\tfn.fib"), "{fib}");
    }

    #[test]
    fn no_value_is_stored_and_immediately_reloaded() {
        let assembly = assemble(FIB).unwrap() + &assemble(LCG).unwrap();
        let lines: Vec<&str> = assembly.lines().collect();
        for pair in lines.windows(2) {
            if let (Some(store), Some(load)) = (
                pair[0].strip_prefix("\tmov\t"),
                pair[1].strip_prefix("\tmov\t"),
            ) && let (Some((a, b)), Some((c, d))) =
                (store.split_once(", "), load.split_once(", "))
            {
                assert!(!(a == d && b == c), "{}\n{}", pair[0], pair[1]);
            }
        }
    }

    #[test]
    fn commutative_operations_write_their_right_operand_in_place() {
        // the product's destination is the register of its right operand
        let source = "def f(a, b);\n    t = 0\n    for k in range(a);\n        \
                      t = t + a * (b + k)\n    ret t\nprint(f(3, 4))\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(!f.contains("\timul\trax"), "{f}");
    }

    #[test]
    fn loop_tests_compare_and_jump_without_a_bool() {
        let source = "def f(n);\n    i = 0\n    while i < n;\n        i = i + 1\n    ret i\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(f.contains("\tcmp\t"), "{f}");
        assert!(f.contains("\tjge\t") || f.contains("\tjl\t"), "{f}");
        assert!(!f.contains("\tset"), "{f}");
    }

    #[test]
    fn division_by_a_variable_zero_or_minus_one_keeps_the_checks() {
        for divisor in ["y", "0", "-1"] {
            let source = format!("x = 7\ny = 2\nprint(x / {divisor})\n");
            let assembly = assemble(&source).unwrap();
            assert!(assembly.contains("division by zero"), "{divisor}");
            assert!(
                assembly.contains("integer overflow in division"),
                "{divisor}"
            );
        }
    }

    #[test]
    fn io_and_string_routines_are_only_emitted_when_used() {
        let plain = assemble("print(1)\n").unwrap();
        for label in [
            "stone.input:",
            "stone.eof:",
            "stone.args:",
            "stone.argc",
            "stone.parse_int:",
            "stone.str_int:",
            "stone.str_split:",
            "stone.format_float:",
        ] {
            assert!(!plain.contains(label), "{label} in {plain}");
        }

        let reading = assemble("while not eof();\n    x = input(\"> \")\n").unwrap();
        assert!(reading.contains("stone.input:"), "{reading}");
        assert!(reading.contains("stone.print_str:"), "{reading}");
        assert!(!reading.contains("stone.args:"), "{reading}");

        let arguments = assemble("x = args()\n").unwrap();
        assert!(arguments.contains("stone.args:"), "{arguments}");
        assert!(arguments.contains("[rip + stone.argc], edi"), "{arguments}");

        let parsing = assemble("x = int(\"1\")\n").unwrap();
        assert!(parsing.contains("stone.parse_int:"), "{parsing}");
        assert!(parsing.contains("stone.fail:"), "{parsing}");

        let converting = assemble("x = str(1.5)\n").unwrap();
        assert!(converting.contains("stone.str_float:"), "{converting}");
        assert!(converting.contains("stone.format_float:"), "{converting}");

        let splitting = assemble("x = \"a b\".split(\" \")\n").unwrap();
        assert!(splitting.contains("stone.str_split:"), "{splitting}");
        assert!(splitting.contains("empty separator"), "{splitting}");
    }
}
