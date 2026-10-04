//! Assembly code generators that compile a stone AST to native code.
//!
//! For example, `x = 42` compiles to `mov rax, 42` followed by a store into `x`'s stack slot.

pub mod builtins;

use crate::ast::{
    Arg, BoolOp, CompOp, Constant, Expr, ExprContext, ExprKind, Mod, Operator, Stmt, StmtKind,
    UnaryOp,
};
use crate::checker::{Type, TypeChecker, range_args};
use crate::codegen::x64::builtins::print;
use crate::codegen::{Architecture, AssemblyGenerator};
use crate::span::{Pos, Span};
use crate::stdlib::BUILTINS;
use crate::stdlib::MAX_CALL_DEPTH;
use std::collections::{HashMap, HashSet};
use std::path::Path;

/// Registers that carry arguments under the System V ABI, in order.
///
/// Calls between stone functions pass the first six arguments in these registers and also leave
/// every argument on the stack, where the callee finds any past the sixth. The first argument is
/// deepest, so with `n` arguments, argument `i` is at `[rbp + 16 + 8 * (n - 1 - i)]` in the
/// callee.
const ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Code generator for x86-64 that emits GNU assembler source in Intel syntax.
///
/// For example, `1 + 2` becomes a `mov`, `push`, `mov`, `pop`, and `add rax, rbx` sequence.
pub struct X64Generator {
    output: String,
    label_count: usize,
    stack_offset: i32,
    env: CompilerEnv,
    current_function: Option<String>,
    break_labels: Vec<String>,
    continue_labels: Vec<String>,
    string_literals: HashMap<String, String>,
    /// The type of every expression, keyed by span, from the checker.
    types: HashMap<Span, Type>,
    /// List types that `print` needs a printer for, emitted after the code that uses them.
    list_printers: Vec<Type>,
    /// Whether `print` needs `stone.print_float`, emitted after the code that uses it.
    prints_floats: bool,
    /// Runtime errors the code can jump to, as `(label, message)`, emitted after `main`.
    failures: Vec<(String, String)>,
}

#[derive(Default)]
struct CompilerEnv {
    /// Names of global variables, each stored in `.bss` under [`global_label`].
    globals: Vec<String>,
    /// Stack of local scopes, each mapping a variable name to its stack offset.
    scopes: Vec<HashMap<String, i32>>,
    functions: HashMap<String, FunctionInfo>,
}

struct FunctionInfo {
    #[allow(dead_code)] // recorded by the scan pass, not read yet
    args: Vec<String>,
    locals: HashMap<String, i32>,
    stack_size: i32,
    #[allow(dead_code)] // recorded by the scan pass, not read yet
    label_prefix: String,
}

#[allow(dead_code)] // planned return type of the scan pass
struct ScanResult {
    max_stack_needed: i32,
    string_literals: HashMap<String, String>,
    function_vars: HashMap<String, Vec<String>>,
    control_flow_depth: usize,
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
        match module {
            Mod::Module { body } => {
                for stmt in body {
                    self.scan_stmt(stmt);
                }
            }
        }

        Ok(())
    }

    fn generate(&mut self, module: &Mod) -> Result<(), String> {
        self.emit("\t.intel_syntax noprefix");
        self.emit("\t.text");

        // only emit the stdlib functions that are actually called
        let stdlib_calls = self.collect_stdlib_calls(module);

        self.emit_stdlib(stdlib_calls);

        match module {
            Mod::Module { body } => {
                let mut top_level_stmts = Vec::new();

                for stmt in body {
                    match &stmt.kind {
                        StmtKind::FunctionDef { .. } => self.gen_stmt(stmt)?,
                        _ => top_level_stmts.push(stmt),
                    }
                }

                // the entry point runs the top-level statements, even if there are none
                self.emit("\t.globl main");
                self.emit("main:");
                self.emit("\tpush\trbp");
                self.emit("\tmov\trbp, rsp");
                // keep rsp 16-byte aligned, as it is at every other call site
                self.emit("\tsub\trsp, 16");

                for stmt in top_level_stmts {
                    self.gen_stmt(stmt)?;
                }

                self.emit("\txor\trax, rax"); // return 0
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");

                self.emit_failures();
                self.emit_list_runtime()?;
                if self.prints_floats {
                    builtins::print_float(self);
                }
                if self.types.values().any(|ty| *ty == Type::Str) {
                    builtins::string_runtime(self);
                }
            }
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
    /// `call print`.
    pub fn assemble(&mut self, module: &Mod) -> Result<String, String> {
        // code generation depends on the checker's types, so it only accepts valid programs
        let analysis = TypeChecker::new().analyze(module);
        if let Some(error) = analysis.diagnostics.first() {
            return Err(error.to_string());
        }
        self.types = analysis.types;

        // first pass: stack allocations, string literals, etc.
        self.scan(module)?;
        self.generate(module)?;
        Ok(self.output.clone())
    }

    pub fn new() -> Self {
        X64Generator {
            output: String::new(),
            label_count: 0,
            stack_offset: 0,
            env: CompilerEnv::default(),
            current_function: None,
            break_labels: Vec::new(),
            continue_labels: Vec::new(),
            string_literals: HashMap::new(),
            types: HashMap::new(),
            list_printers: Vec::new(),
            prints_floats: false,
            failures: Vec::new(),
        }
    }

    fn enter_scope(&mut self) {
        self.env.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.env.scopes.pop();
    }

    fn define_var(&mut self, name: &str) {
        if let Some(scope) = self.env.scopes.last_mut() {
            // local variable
            self.stack_offset += 8;
            scope.insert(name.to_string(), self.stack_offset);
        } else {
            self.env.globals.push(name.to_string());
        }
    }

    /// Allocates storage for `name` in the current scope unless it already has some.
    ///
    /// For example, scanning `x = 1` then `x = 2` gives `x` a single slot rather than two.
    fn declare_var(&mut self, name: &str) {
        let exists = match self.env.scopes.last() {
            Some(scope) => scope.contains_key(name),
            None => self.env.globals.iter().any(|g| g == name),
        };
        if !exists {
            self.define_var(name);
        }
    }

    /// Finds where `name` is stored: the current function's stack frame, then the globals, the
    /// same lexical scoping the checker and interpreter use.
    fn lookup_var(&self, name: &str) -> Option<Slot> {
        if let Some(&offset) = self.env.scopes.last().and_then(|scope| scope.get(name)) {
            return Some(Slot::Stack(offset));
        }
        self.env
            .globals
            .iter()
            .any(|g| g == name)
            .then(|| Slot::Global(global_label(name)))
    }

    fn scan_function(&mut self, name: &str, args: &[Arg], body: &[Stmt]) {
        let saved_offset = self.stack_offset;
        self.stack_offset = 0;
        self.enter_scope();

        let mut func_info = FunctionInfo {
            args: args.iter().map(|a| a.arg.clone()).collect(),
            locals: HashMap::new(),
            stack_size: 0,
            label_prefix: name.to_string(),
        };

        // define args in scope
        for arg in args {
            self.define_var(&arg.arg);
        }

        // scan body for all local vars
        for stmt in body {
            self.scan_stmt(stmt);
        }

        func_info.stack_size = self.stack_offset;
        func_info.locals = self.env.scopes.last().unwrap().clone();

        self.env.functions.insert(name.to_string(), func_info);
        self.exit_scope();
        self.stack_offset = saved_offset;
    }

    /// Returns the stack slot of a variable that the scan pass has already allocated.
    ///
    /// Fails if the variable was never allocated, which means `scan` missed it.
    fn slot_of(&self, name: &str) -> Result<Slot, String> {
        self.lookup_var(name)
            .ok_or_else(|| format!("variable '{}' was not allocated during scan", name))
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

    fn scan_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                self.scan_function(name, &args.args, body);
            }

            StmtKind::Assign { targets, value } => {
                self.scan_expr(value);
                for target in targets {
                    if let ExprKind::Name { id, .. } = &target.kind {
                        self.declare_var(id);
                    }
                    self.scan_expr(target);
                }
            }

            StmtKind::While { test, body } => {
                self.scan_expr(test);
                for s in body {
                    self.scan_stmt(s);
                }
            }

            StmtKind::If { test, body, orelse } => {
                self.scan_expr(test);
                for s in body.iter().chain(orelse) {
                    self.scan_stmt(s);
                }
            }

            StmtKind::For { target, iter, body } => {
                if let ExprKind::Name { id, .. } = &target.kind {
                    self.declare_var(id);
                }
                let (next, end) = for_slots(stmt);
                self.declare_var(&next);
                self.declare_var(&end);
                self.scan_expr(iter);
                for s in body {
                    self.scan_stmt(s);
                }
            }

            StmtKind::Return { value } => {
                if let Some(v) = value {
                    self.scan_expr(v);
                }
            }

            StmtKind::Expr { value } => {
                self.scan_expr(value);
            }

            StmtKind::Delete { targets } => {
                for t in targets {
                    self.scan_expr(t);
                }
            }

            StmtKind::Break | StmtKind::Continue => {}
        }
    }

    fn scan_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Constant { value, .. } => {
                if let Constant::Str(s) = &**value {
                    self.intern_string(s);
                }
            }

            ExprKind::BinOp { left, right, .. } => {
                self.scan_expr(left);
                self.scan_expr(right);
            }

            ExprKind::UnaryOp { operand, .. } => {
                self.scan_expr(operand);
            }

            ExprKind::BoolOp { values, .. } => {
                for v in values {
                    self.scan_expr(v);
                }
            }

            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.scan_expr(left);
                for c in comparators {
                    self.scan_expr(c);
                }
            }

            ExprKind::Call { func, args } => {
                self.scan_expr(func);
                for arg in args {
                    self.scan_expr(arg);
                }
            }

            ExprKind::Subscript { value, slice, .. } => {
                self.scan_expr(value);
                self.scan_expr(slice);
            }

            ExprKind::List { elts, .. } => {
                for e in elts {
                    self.scan_expr(e);
                }
            }

            ExprKind::Name { .. } => {}
        }
    }

    fn gen_constant(&mut self, value: &Constant) -> Result<(), String> {
        match value {
            Constant::Int(n) => {
                self.emit(&format!("\tmov\trax, {}", n));
            }
            Constant::Float(x) => {
                // floats live in general registers as their bits until an instruction needs them
                self.emit(&format!("\tmov\trax, {}", x.to_bits() as i64));
            }
            Constant::Bool(b) => {
                self.emit(&format!("\tmov\trax, {}", if *b { 1 } else { 0 }));
            }
            Constant::Str(s) => {
                let label = self.intern_string(s);
                self.emit(&format!("\tlea\trax, [rip + {}]", label));
            }
            Constant::None => {
                self.emit("\txor\trax, rax"); // 0
            }
            other => return Err(format!("unsupported constant value {:?}", other)),
        }
        Ok(())
    }

    fn gen_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match &expr.kind {
            ExprKind::Constant { value, .. } => {
                self.gen_constant(value)?;
            }

            ExprKind::Name { id, ctx } => {
                if let Some(slot) = self.lookup_var(id) {
                    match ctx {
                        ExprContext::Load => {
                            // top-level code is checked to assign before reading, but a
                            // function can run before a global it reads is assigned
                            if let Slot::Global(label) = &slot
                                && self.current_function.is_some()
                            {
                                let fail = self
                                    .fail_label(&format!("'{id}' is used before it is assigned"));
                                self.emit(&format!("\tcmp\tQWORD PTR [rip + {label}.set], 0"));
                                self.emit(&format!("\tje\t{fail}"));
                            }
                            self.emit(&format!("\tmov\trax, {}", slot));
                        }
                        ExprContext::Store => self.store_rax(&slot),
                        ExprContext::Delete => {
                            // zero the slot
                            self.emit(&format!("\tmov\t{}, 0", slot));
                        }
                    }
                } else {
                    // undefined variable, possibly a runtime error
                    self.emit(&format!("\t# Error: undefined variable '{}'", id));
                    self.emit("\txor\trax, rax");
                }
            }

            ExprKind::BinOp { op, left, right } => {
                if self.type_of(expr)? == Type::Str {
                    // only `+` applies to strings
                    self.gen_expr(left)?;
                    self.emit("\tpush\trax");
                    self.gen_expr(right)?;
                    self.emit("\tpush\trax");
                    self.runtime_call("stone.str_concat", 2);
                    return Ok(());
                }

                if self.type_of(expr)? == Type::Float {
                    self.gen_expr(left)?;
                    self.emit("\tpush\trax");
                    self.gen_expr(right)?;
                    self.emit("\tmov\trbx, rax");
                    self.emit("\tpop\trax");
                    self.gen_float_binop(op);
                    return Ok(());
                }

                if *op == Operator::Divide
                    && let Some(divisor) = constant_int(right)
                    && divisor != 0
                    && divisor != -1
                {
                    // a literal has no side effects, so evaluating it last changes nothing
                    self.gen_expr(left)?;
                    self.gen_divide_by_constant(divisor);
                    return Ok(());
                }

                // left to right, like the interpreter
                self.gen_expr(left)?;
                self.emit("\tpush\trax");
                self.gen_expr(right)?;
                self.emit("\tmov\trbx, rax");
                self.emit("\tpop\trax");

                match op {
                    Operator::Add => self.emit("\tadd\trax, rbx"),
                    Operator::Subtract => self.emit("\tsub\trax, rbx"),
                    Operator::Multiply => self.emit("\timul\trax, rbx"),
                    Operator::Divide => {
                        // idiv traps on both of these, so report them like the interpreter
                        let by_zero = self.fail_label("division by zero");
                        let overflow = self.fail_label("integer overflow in division");
                        let divide = self.new_label("divide");
                        self.emit("\ttest\trbx, rbx");
                        self.emit(&format!("\tjz\t{by_zero}"));
                        self.emit("\tcmp\trbx, -1");
                        self.emit(&format!("\tjne\t{divide}"));
                        // only the minimum overflows when negated
                        self.emit("\tmov\trcx, rax");
                        self.emit("\tneg\trcx");
                        self.emit(&format!("\tjo\t{overflow}"));
                        self.emit(&format!("{divide}:"));
                        // x64 division: rax = rdx:rax / rbx
                        self.emit("\tcqo"); // sign-extend rax into rdx
                        self.emit("\tidiv\trbx");
                    }
                }
            }

            ExprKind::BoolOp { op, values } => {
                if values.is_empty() {
                    return Ok(());
                }

                match op {
                    BoolOp::And => {
                        let end_label = self.new_label("and_end");

                        for (i, val) in values.iter().enumerate() {
                            self.gen_expr(val)?;
                            if i < values.len() - 1 {
                                self.emit("\ttest\trax, rax");
                                self.emit(&format!("\tjz\t{}", end_label));
                            }
                        }
                        self.emit(&format!("{}:", end_label));
                    }
                    BoolOp::Or => {
                        let end_label = self.new_label("or_end");

                        for (i, val) in values.iter().enumerate() {
                            self.gen_expr(val)?;
                            if i < values.len() - 1 {
                                self.emit("\ttest\trax, rax");
                                self.emit(&format!("\tjnz\t{}", end_label));
                            }
                        }
                        self.emit(&format!("{}:", end_label));
                    }
                }
            }

            ExprKind::UnaryOp { op, operand } => {
                self.gen_expr(operand)?;
                match op {
                    UnaryOp::Not => {
                        self.emit("\ttest\trax, rax");
                        self.emit("\tsetz\tal");
                        self.emit("\tmovzx\trax, al");
                    }
                    UnaryOp::UnaryAdd => {
                        // no-op
                    }
                    UnaryOp::UnarySub if self.type_of(operand)? == Type::Float => {
                        // flip the sign bit, which also negates 0.0 and nan like the interpreter
                        self.emit("\tbtc\trax, 63");
                    }
                    UnaryOp::UnarySub => {
                        self.emit("\tneg\trax");
                    }
                }
            }

            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => {
                // a < b < c means a < b and b < c, with b evaluated once and c skipped once false
                let end_label = self.new_label("cmp_end");
                self.gen_expr(left)?;
                for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
                    self.emit("\tpush\trax");
                    self.gen_expr(comparator)?;
                    self.emit("\tmov\trbx, rax");
                    self.emit("\tpop\trax");
                    if self.type_of(comparator)? == Type::Str {
                        // strings compare by contents, and only with == and !=
                        self.emit("\tpush\trbx"); // the next link's left side
                        self.emit("\tpush\trax");
                        self.emit("\tpush\trbx");
                        self.runtime_call("stone.str_eq", 2);
                        self.emit("\tpop\trbx");
                        if matches!(op, CompOp::NotEqual) {
                            self.emit("\txor\trax, 1");
                        }
                    } else if self.type_of(comparator)? == Type::Float {
                        self.gen_float_compare(op);
                    } else {
                        self.emit("\tcmp\trax, rbx");
                        self.emit(&format!("\t{}\tal", set_instruction(op)));
                        self.emit("\tmovzx\trax, al");
                    }
                    if i + 1 < ops.len() {
                        self.emit("\ttest\trax, rax");
                        self.emit(&format!("\tjz\t{}", end_label));
                        // the right side becomes the next link's left side
                        self.emit("\tmov\trax, rbx");
                    }
                }
                self.emit(&format!("{}:", end_label));
            }

            ExprKind::Call { func, args } => {
                if let ExprKind::Name { id, .. } = &func.kind {
                    match id.as_str() {
                        "print" => return self.gen_print(args),
                        "len" if matches!(self.type_of(&args[0])?, Type::List(_)) => {
                            self.gen_expr(&args[0])?;
                            self.emit("\tmov\trax, QWORD PTR [rax]");
                            return Ok(());
                        }
                        "len" => {
                            self.gen_expr(&args[0])?;
                            self.emit("\tpush\trax");
                            self.runtime_call("stone.str_len", 1);
                            return Ok(());
                        }
                        "float" => {
                            self.gen_expr(&args[0])?;
                            if self.type_of(&args[0])? == Type::Int {
                                self.emit("\tcvtsi2sd\txmm0, rax");
                                self.emit("\tmovq\trax, xmm0");
                            }
                            return Ok(());
                        }
                        "int" => {
                            self.gen_expr(&args[0])?;
                            if self.type_of(&args[0])? == Type::Float {
                                self.gen_float_to_int();
                            }
                            return Ok(());
                        }
                        "append" => {
                            for arg in args {
                                self.gen_expr(arg)?;
                                self.emit("\tpush\trax");
                            }
                            self.runtime_call("stone.list_append", args.len());
                            return Ok(());
                        }
                        _ => {}
                    }
                }
                // evaluate every argument first, so nothing can clobber a loaded register, such
                // as the rdx that division overwrites
                for arg in args {
                    self.gen_expr(arg)?;
                    self.emit("\tpush\trax");
                }
                for (i, reg) in ARG_REGS.iter().take(args.len()).enumerate() {
                    let depth = 8 * (args.len() - 1 - i);
                    self.emit(&format!("\tmov\t{reg}, QWORD PTR [rsp + {depth}]"));
                }

                if let ExprKind::Name { id, .. } = &func.kind {
                    self.emit(&format!("\tcall\t{}", function_label(id)));
                }
                if !args.is_empty() {
                    self.emit(&format!("\tadd\trsp, {}", 8 * args.len()));
                }
            }

            ExprKind::Subscript { value, slice, .. } => {
                self.gen_list_slot(value, slice)?;
                self.emit("\tmov\trax, QWORD PTR [rax]");
            }

            ExprKind::List { elts, .. } => {
                for elt in elts {
                    self.gen_expr(elt)?;
                    self.emit("\tpush\trax");
                }
                self.emit(&format!("\tpush\t{}", elts.len()));
                self.runtime_call("stone.list_new", 1);
                // r10 and r11 carry no arguments, so filling the list leaves those intact
                self.emit("\tmov\tr10, QWORD PTR [rax + 16]");
                for i in (0..elts.len()).rev() {
                    self.emit("\tpop\tr11");
                    self.emit(&format!("\tmov\tQWORD PTR [r10 + {}], r11", 8 * i));
                }
            }
        }
        Ok(())
    }

    /// Generates `print(a, b, ...)`: evaluates every argument first, like the interpreter, then
    /// writes each with the routine for its type, separated by spaces and ending with a newline.
    /// Divides `rax` by a constant that is neither 0 nor -1, rounding toward zero like `idiv`.
    ///
    /// Neither `idiv` trap can happen for such a divisor, so no checks are emitted, and a power
    /// of two becomes shifts. For example, dividing by `8` emits a bias for negative dividends
    /// and then `sar rax, 3`, and dividing by `10` emits a plain `idiv`.
    fn gen_divide_by_constant(&mut self, divisor: i64) {
        let magnitude = divisor.unsigned_abs();
        if !magnitude.is_power_of_two() {
            self.emit(&format!("\tmov\trbx, {divisor}"));
            self.emit("\tcqo"); // sign-extend rax into rdx
            self.emit("\tidiv\trbx");
            return;
        }

        let shift = magnitude.trailing_zeros();
        if shift > 0 {
            // an arithmetic shift rounds down, so first add 2^shift - 1 to a negative dividend
            self.emit("\tmov\trcx, rax");
            self.emit("\tsar\trcx, 63"); // all ones if negative
            self.emit(&format!("\tshr\trcx, {}", 64 - shift));
            self.emit("\tadd\trax, rcx");
            self.emit(&format!("\tsar\trax, {shift}"));
        }
        if divisor < 0 {
            // the quotient is never the minimum here, so negating it cannot overflow
            self.emit("\tneg\trax");
        }
    }

    /// Applies `op` to the floats whose bits are in `rax` and `rbx`, leaving the result's bits in
    /// `rax`.
    ///
    /// Dividing by 0.0 or -0.0 exits with `division by zero`, as in the interpreter, rather than
    /// giving inf or nan.
    fn gen_float_binop(&mut self, op: &Operator) {
        self.emit("\tmovq\txmm0, rax");
        self.emit("\tmovq\txmm1, rbx");
        let instruction = match op {
            Operator::Add => "addsd",
            Operator::Subtract => "subsd",
            Operator::Multiply => "mulsd",
            Operator::Divide => {
                let by_zero = self.fail_label("division by zero");
                let divide = self.new_label("divide");
                self.emit("\txorpd\txmm2, xmm2");
                self.emit("\tucomisd\txmm1, xmm2");
                // a nan divisor also sets ZF, but PF tells it apart
                self.emit(&format!("\tjp\t{divide}"));
                self.emit(&format!("\tje\t{by_zero}"));
                self.emit(&format!("{divide}:"));
                "divsd"
            }
        };
        self.emit(&format!("\t{instruction}\txmm0, xmm1"));
        self.emit("\tmovq\trax, xmm0");
    }

    /// Compares the floats whose bits are in `rax` and `rbx` with `op`, leaving 1 or 0 in `rax`.
    ///
    /// `ucomisd` sets ZF, PF, and CF when either side is nan, so `a < b` is computed as `b > a`
    /// with `seta`, which is false for nan, and `==` also checks that PF is clear.
    fn gen_float_compare(&mut self, op: &CompOp) {
        self.emit("\tmovq\txmm0, rax");
        self.emit("\tmovq\txmm1, rbx");
        match op {
            CompOp::LessThan | CompOp::LessThanEqual => {
                self.emit("\tucomisd\txmm1, xmm0");
            }
            _ => self.emit("\tucomisd\txmm0, xmm1"),
        }
        match op {
            CompOp::LessThan | CompOp::GreaterThan => self.emit("\tseta\tal"),
            CompOp::LessThanEqual | CompOp::GreaterThanEqual => self.emit("\tsetae\tal"),
            CompOp::Equal => {
                self.emit("\tsete\tal");
                self.emit("\tsetnp\tcl");
                self.emit("\tand\tal, cl");
            }
            CompOp::NotEqual => {
                self.emit("\tsetne\tal");
                self.emit("\tsetp\tcl");
                self.emit("\tor\tal, cl");
            }
        }
        self.emit("\tmovzx\trax, al");
    }

    /// Converts the float whose bits are in `rax` to an int, dropping any fraction.
    ///
    /// `cvttsd2si` gives the minimum int for nan and for anything out of range, so that result
    /// exits with an error unless the float really was -2^63.
    fn gen_float_to_int(&mut self) {
        let fail = self.fail_label("cannot convert float to int (nan or out of range)");
        let done = self.new_label("to_int");
        self.emit("\tmovq\txmm0, rax");
        self.emit("\tmov\trcx, rax");
        self.emit("\tcvttsd2si\trax, xmm0");
        self.emit(&format!("\tmov\trdx, {}", i64::MIN));
        self.emit("\tcmp\trax, rdx");
        self.emit(&format!("\tjne\t{done}"));
        self.emit(&format!(
            "\tmov\trdx, {}",
            (i64::MIN as f64).to_bits() as i64
        ));
        self.emit("\tcmp\trcx, rdx");
        self.emit(&format!("\tjne\t{fail}"));
        self.emit(&format!("{done}:"));
    }

    fn gen_print(&mut self, args: &[Expr]) -> Result<(), String> {
        for arg in args {
            self.gen_expr(arg)?;
            self.emit("\tpush\trax");
        }
        for (i, arg) in args.iter().enumerate() {
            let ty = self.type_of(arg)?;
            let routine = self.print_routine(&ty, false)?;
            // the first argument was pushed first, so it is deepest
            let depth = 8 * (args.len() - 1 - i);
            self.emit(&format!("\tmov\trdi, QWORD PTR [rsp + {depth}]"));
            self.emit(&format!("\tcall\t{routine}"));
            if i + 1 < args.len() {
                self.emit("\tmov\trdi, 32"); // ' '
                self.emit("\tcall\tstone.print_char");
            }
        }
        self.emit("\tmov\trdi, 10"); // newline
        self.emit("\tcall\tstone.print_char");
        if !args.is_empty() {
            self.emit(&format!("\tadd\trsp, {}", 8 * args.len()));
        }
        self.emit("\txor\trax, rax"); // print returns none
        Ok(())
    }

    /// Generates `for x in items;`, walking the list by index and rereading its length every
    /// iteration, so a list that grows while it is iterated keeps going, like in the interpreter.
    ///
    /// `index` holds the next position and `list` holds the list, both hidden variables.
    fn gen_for_list(
        &mut self,
        iter: &Expr,
        body: &[Stmt],
        target: &Slot,
        index: &Slot,
        list: &Slot,
    ) -> Result<(), String> {
        self.gen_expr(iter)?;
        self.emit(&format!("\tmov\t{list}, rax"));
        self.emit(&format!("\tmov\t{index}, 0"));

        let start_label = self.new_label("for_start");
        let end_label = self.new_label("for_end");
        self.break_labels.push(end_label.clone());
        self.continue_labels.push(start_label.clone());

        self.emit(&format!("{start_label}:"));
        self.emit(&format!("\tmov\trax, {index}"));
        self.emit(&format!("\tmov\trbx, {list}"));
        self.emit("\tcmp\trax, QWORD PTR [rbx]");
        self.emit(&format!("\tjge\t{end_label}"));
        self.emit("\tmov\trbx, QWORD PTR [rbx + 16]");
        self.emit("\tmov\trax, QWORD PTR [rbx + rax * 8]");
        self.store_rax(target);
        self.emit(&format!("\tinc\t{index}"));

        for stmt in body {
            self.gen_stmt(stmt)?;
        }

        self.emit(&format!("\tjmp\t{start_label}"));
        self.emit(&format!("{end_label}:"));
        self.break_labels.pop();
        self.continue_labels.pop();
        Ok(())
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
        if self.failures.is_empty() {
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

    /// Stores `rax` in `slot`, marking a global as assigned so functions can read it.
    fn store_rax(&mut self, slot: &Slot) {
        self.emit(&format!("\tmov\t{slot}, rax"));
        if let Slot::Global(label) = slot {
            self.emit(&format!("\tmov\tQWORD PTR [rip + {label}.set], 1"));
        }
    }

    /// Returns the type the checker inferred for `expr`.
    fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        self.types
            .get(&expr.span)
            .cloned()
            .ok_or_else(|| format!("expression at {:?} has no type", expr.span.start))
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

    /// Calls a runtime routine with `count` arguments, which the caller pushed left to right.
    ///
    /// Arguments only go into registers right before a call, so no register holds a value that
    /// the routine could clobber. For example, after pushing a list and a value,
    /// `self.runtime_call("stone.list_append", 2)` pops the value into `rsi` and the list into
    /// `rdi`, then calls the routine.
    fn runtime_call(&mut self, label: &str, count: usize) {
        for reg in ARG_REGS.iter().take(count).rev() {
            self.emit(&format!("\tpop\t{reg}"));
        }
        self.emit(&format!("\tcall\t{label}"));
    }

    /// Leaves the address of `value[slice]` in `rax`, counting negative indexes from the end, and
    /// exits with an error if the index is out of range.
    ///
    /// A list is a pointer to a `{len, cap, data}` header (see [`builtins::list_runtime`]), so for
    /// example `xs[-1]` with `xs` of length 3 checks that `-1 + 3` is below 3 and leaves the
    /// address `data + 2 * 8`.
    fn gen_list_slot(&mut self, value: &Expr, slice: &Expr) -> Result<(), String> {
        self.gen_expr(value)?;
        self.emit("\tpush\trax");
        self.gen_expr(slice)?;
        self.emit("\tpop\trdi");
        let out_of_range = self.fail_label("list index out of range");
        let check = self.new_label("index_check");
        self.emit("\ttest\trax, rax");
        self.emit(&format!("\tjns\t{check}"));
        self.emit("\tadd\trax, QWORD PTR [rdi]"); // negative indexes count from the end
        self.emit(&format!("{check}:"));
        // unsigned, so an index still negative after adjusting is out of range too
        self.emit("\tcmp\trax, QWORD PTR [rdi]");
        self.emit(&format!("\tjae\t{out_of_range}"));
        self.emit("\tmov\trdi, QWORD PTR [rdi + 16]");
        self.emit("\tlea\trax, [rdi + rax * 8]");
        Ok(())
    }

    /// Emits the list runtime and every list printer `print` asked for, if the program uses lists.
    fn emit_list_runtime(&mut self) -> Result<(), String> {
        if !self.types.values().any(contains_list) {
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

    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                self.gen_expr(value)?;

                for target in targets {
                    match &target.kind {
                        ExprKind::Name { id, .. } => {
                            let slot = self.slot_of(id)?;
                            self.store_rax(&slot);
                        }
                        ExprKind::Subscript { value, slice, .. } => {
                            // the value is evaluated first, like the interpreter
                            self.emit("\tpush\trax");
                            self.gen_list_slot(value, slice)?;
                            self.emit("\tpop\trbx");
                            self.emit("\tmov\tQWORD PTR [rax], rbx");
                            // later targets of `a = b[0] = 1` store the same value
                            self.emit("\tmov\trax, rbx");
                        }
                        _ => {}
                    }
                }
            }
            StmtKind::Return { value } => {
                if let Some(val) = value {
                    self.gen_expr(val)?;
                }

                if self.current_function.is_some() {
                    self.emit("\tdec\tQWORD PTR [rip + stone.call_depth]");
                } else {
                    // `ret` at the top level ends the program normally
                    self.emit("\txor\trax, rax");
                }
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");
            }

            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                let func_info = self
                    .env
                    .functions
                    .get(name)
                    .ok_or_else(|| format!("function '{}' was not scanned", name))?;
                let locals = func_info.locals.clone();
                let stack_size = align16(func_info.stack_size);
                self.current_function = Some(name.clone());

                // locals and their offsets were already laid out by the scan pass
                self.env.scopes.push(locals);

                self.emit(&format!("{}:", function_label(name)));

                // function prologue
                self.emit("\tpush\trbp");
                self.emit("\tmov\trbp, rsp");

                if stack_size > 0 {
                    self.emit(&format!("\tsub\trsp, {}", stack_size));
                }

                // the same limit as the interpreter, rather than overflowing the stack
                let too_deep = self.fail_label(&format!(
                    "recursion is too deep (more than {MAX_CALL_DEPTH} nested calls)"
                ));
                self.emit("\tinc\tQWORD PTR [rip + stone.call_depth]");
                self.emit(&format!(
                    "\tcmp\tQWORD PTR [rip + stone.call_depth], {MAX_CALL_DEPTH}"
                ));
                self.emit(&format!("\tjg\t{too_deep}"));

                // copy the arguments into the parameters' slots, from the caller's stack past six
                let count = args.args.len();
                for (i, arg) in args.args.iter().enumerate() {
                    let slot = self.slot_of(&arg.arg)?;
                    if let Some(reg) = ARG_REGS.get(i) {
                        self.emit(&format!("\tmov\t{slot}, {reg}"));
                    } else {
                        let offset = 16 + 8 * (count - 1 - i);
                        self.emit(&format!("\tmov\trax, QWORD PTR [rbp + {offset}]"));
                        self.emit(&format!("\tmov\t{slot}, rax"));
                    }
                }

                for stmt in body {
                    self.gen_stmt(stmt)?;
                }

                // falling off the end returns none
                self.emit("\tdec\tQWORD PTR [rip + stone.call_depth]");
                self.emit("\txor\trax, rax");
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");

                // restore state
                self.exit_scope();
                self.current_function = None;
            }

            StmtKind::While { test, body } => {
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                self.emit(&format!("{}:", start_label));

                // test condition
                self.gen_expr(test)?;
                self.emit("\ttest\trax, rax");
                self.emit(&format!("\tjz\t{}", end_label));

                // loop body
                for stmt in body {
                    self.gen_stmt(stmt)?;
                }

                self.emit(&format!("\tjmp\t{}", start_label));
                self.emit(&format!("{}:", end_label));

                self.break_labels.pop();
                self.continue_labels.pop();
            }

            StmtKind::If { test, body, orelse } => {
                let else_label = self.new_label("if_else");
                let end_label = self.new_label("if_end");

                // test condition
                self.gen_expr(test)?;
                self.emit("\ttest\trax, rax");

                if orelse.is_empty() {
                    self.emit(&format!("\tjz\t{}", end_label));

                    for stmt in body {
                        self.gen_stmt(stmt)?;
                    }

                    self.emit(&format!("{}:", end_label));
                } else {
                    self.emit(&format!("\tjz\t{}", else_label));

                    for stmt in body {
                        self.gen_stmt(stmt)?;
                    }

                    self.emit(&format!("\tjmp\t{}", end_label));
                    self.emit(&format!("{}:", else_label));

                    for stmt in orelse {
                        self.gen_stmt(stmt)?;
                    }

                    self.emit(&format!("{}:", end_label));
                }
            }

            StmtKind::For { target, iter, body } => {
                let ExprKind::Name { id, .. } = &target.kind else {
                    return Err("a 'for' loop's variable must be a name".to_string());
                };
                let target = self.slot_of(id)?;
                let (next, end) = for_slots(stmt);
                let (next, end) = (self.slot_of(&next)?, self.slot_of(&end)?);
                let Some(args) = range_args(iter) else {
                    return self.gen_for_list(iter, body, &target, &next, &end);
                };

                // the bounds are evaluated once, left to right
                match args {
                    [limit] => {
                        self.emit(&format!("\tmov\t{}, 0", next));
                        self.gen_expr(limit)?;
                        self.emit(&format!("\tmov\t{}, rax", end));
                    }
                    [start, limit] => {
                        self.gen_expr(start)?;
                        self.emit(&format!("\tmov\t{}, rax", next));
                        self.gen_expr(limit)?;
                        self.emit(&format!("\tmov\t{}, rax", end));
                    }
                    _ => return Err("range() takes 1 or 2 arguments".to_string()),
                }

                let start_label = self.new_label("for_start");
                let end_label = self.new_label("for_end");
                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                // a separate counter, so assigning the variable cannot change the iteration
                self.emit(&format!("{}:", start_label));
                self.emit(&format!("\tmov\trax, {}", next));
                self.emit(&format!("\tcmp\trax, {}", end));
                self.emit(&format!("\tjge\t{}", end_label));
                self.store_rax(&target);
                self.emit(&format!("\tinc\t{}", next));

                for stmt in body {
                    self.gen_stmt(stmt)?;
                }

                self.emit(&format!("\tjmp\t{}", start_label));
                self.emit(&format!("{}:", end_label));

                self.break_labels.pop();
                self.continue_labels.pop();
            }

            StmtKind::Expr { value } => {
                self.gen_expr(value)?;
            }

            StmtKind::Break => {
                if let Some(label) = self.break_labels.last() {
                    self.emit(&format!("\tjmp\t{}", label));
                }
            }

            StmtKind::Continue => {
                if let Some(label) = self.continue_labels.last() {
                    self.emit(&format!("\tjmp\t{}", label));
                }
            }

            StmtKind::Delete { targets } => {
                for target in targets {
                    if let ExprKind::Name { id, .. } = &target.kind
                        && let Some(slot) = self.lookup_var(id)
                    {
                        self.emit(&format!("\tmov\t{}, 0", slot));
                    }
                }
            }
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
            StmtKind::Break | StmtKind::Continue => {}
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
        if calls.iter().any(|call| call == "print") {
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
        for name in self.env.globals.clone() {
            let label = global_label(&name);
            self.emit(&format!("{label}:"));
            self.emit("\t.zero\t8");
            self.emit(&format!("{label}.set:"));
            self.emit("\t.zero\t8");
        }
        self.emit("\t.text");
    }
}

/// Where a variable lives: a slot in the current stack frame, or a global in `.bss`.
///
/// Its `Display` is the memory operand, such as `QWORD PTR [rbp - 8]` or
/// `QWORD PTR [rip + g.total]`.
enum Slot {
    Stack(i32),
    Global(String),
}

impl std::fmt::Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Slot::Stack(offset) => write!(f, "QWORD PTR [rbp - {offset}]"),
            Slot::Global(label) => write!(f, "QWORD PTR [rip + {label}]"),
        }
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

/// Returns the names of the hidden variables a `for` loop counts with: the next value, and the
/// end of the range.
///
/// They are named after the loop's position, and the dots keep them from colliding with any
/// stone name. For example, a loop on line 3, col 1 counts with `for.next.3.1` and `for.end.3.1`.
fn for_slots(stmt: &Stmt) -> (String, String) {
    let Pos { line, col } = stmt.span.start;
    (
        format!("for.next.{line}.{col}"),
        format!("for.end.{line}.{col}"),
    )
}

/// Returns the value of `expr` if it is an int literal, possibly negated.
///
/// For example, `8` gives `Some(8)`, `-4` gives `Some(-4)`, and `x` or `2 + 2` gives `None`.
fn constant_int(expr: &Expr) -> Option<i64> {
    match &expr.kind {
        ExprKind::Constant { value, .. } => match **value {
            Constant::Int(n) => Some(n),
            _ => None,
        },
        ExprKind::UnaryOp {
            op: UnaryOp::UnarySub,
            operand,
        } => constant_int(operand).map(i64::wrapping_neg),
        _ => None,
    }
}

/// Returns the `setcc` instruction that sets `al` when a signed `cmp` satisfies `op`.
///
/// For example, `set_instruction(&CompOp::LessThanEqual)` returns `"setle"`.
fn set_instruction(op: &CompOp) -> &'static str {
    match op {
        CompOp::Equal => "sete",
        CompOp::NotEqual => "setne",
        CompOp::LessThan => "setl",
        CompOp::LessThanEqual => "setle",
        CompOp::GreaterThan => "setg",
        CompOp::GreaterThanEqual => "setge",
    }
}

/// Rounds a frame size up to the 16-byte stack alignment required by the System V ABI.
///
/// For example, `align16(20)` returns `32` and `align16(32)` returns `32`.
fn align16(size: i32) -> i32 {
    (size + 15) & !15
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
        assert!(assembly.contains("\tidiv\trbx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        assert!(!assembly.contains("integer overflow"), "{assembly}");
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
}
