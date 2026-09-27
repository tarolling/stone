//! Assembly code generators that compile a stone AST to native code.
//!
//! For example, `x = 42` compiles to `mov rax, 42` followed by a store into `x`'s stack slot.

pub mod builtins;

use crate::ast::{Arg, BoolOp, Constant, Expr, ExprContext, Mod, Operator, Stmt, UnaryOp};
use crate::codegen::x64::builtins::{len, print};
use crate::codegen::{Architecture, AssemblyGenerator};
use crate::stdlib::BUILTINS;
use std::collections::{HashMap, HashSet};
use std::path::Path;

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
}

#[derive(Default)]
struct CompilerEnv {
    /// Stack offsets of global variables, which live in `main`'s frame.
    globals: HashMap<String, i32>,
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
        // first pass: stack allocations, string literals, etc.
        self.scan(module).map_err(std::io::Error::other)?;
        self.generate(module).map_err(std::io::Error::other)?;

        let assembly = output.with_extension("s");
        if let Some(dir) = output.parent() {
            std::fs::create_dir_all(dir)?;
        }
        std::fs::write(&assembly, &self.output)?;

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
                let mut has_main = false;
                let mut top_level_stmts = Vec::new();

                for stmt in body {
                    match stmt {
                        Stmt::FunctionDef { name, .. } => {
                            if name == "main" {
                                has_main = true;
                            }
                            self.gen_stmt(stmt);
                        }
                        _ => {
                            top_level_stmts.push(stmt);
                        }
                    }
                }

                if !has_main && !top_level_stmts.is_empty() {
                    self.emit("\t.globl main");
                    self.emit("main:");
                    self.emit("\tpush\trbp");
                    self.emit("\tmov\trbp, rsp");

                    // globals live in main's frame, sized by the scan pass
                    let globals_size = align16(self.stack_offset);
                    if globals_size > 0 {
                        self.emit(&format!("\tsub\trsp, {}", globals_size));
                    }

                    for stmt in top_level_stmts {
                        self.gen_stmt(stmt);
                    }

                    self.emit("\txor\trax, rax"); // return 0
                    self.emit("\tmov\trsp, rbp");
                    self.emit("\tpop\trbp");
                    self.emit("\tret");
                }
            }
        }

        self.emit_rodata();

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
        }
    }

    fn enter_scope(&mut self) {
        self.env.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.env.scopes.pop();
    }

    fn define_var(&mut self, name: &str) -> i32 {
        if let Some(scope) = self.env.scopes.last_mut() {
            // local variable
            self.stack_offset += 8;
            scope.insert(name.to_string(), self.stack_offset);
            self.stack_offset
        } else {
            // global variable
            self.stack_offset += 8;
            self.env.globals.insert(name.to_string(), self.stack_offset);
            self.stack_offset
        }
    }

    /// Returns the existing stack slot for `name` in the current scope, or allocates a new one.
    ///
    /// For example, scanning `x = 1` then `x = 2` gives `x` a single slot rather than two.
    fn declare_var(&mut self, name: &str) -> i32 {
        let existing = match self.env.scopes.last() {
            Some(scope) => scope.get(name),
            None => self.env.globals.get(name),
        };

        match existing {
            Some(&offset) => offset,
            None => self.define_var(name),
        }
    }

    fn lookup_var(&self, name: &str) -> Option<i32> {
        // innermost to outermost, like the interpreter
        for scope in self.env.scopes.iter().rev() {
            if let Some(&offset) = scope.get(name) {
                return Some(offset);
            }
        }

        // globals are rbp-relative to main's frame, unreachable from functions for now
        if !self.env.scopes.is_empty() {
            return None;
        }

        self.env.globals.get(name).copied()
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
    /// Panics if the variable was never allocated, which means `scan` missed it.
    fn slot_of(&self, name: &str) -> i32 {
        self.lookup_var(name)
            .unwrap_or_else(|| panic!("variable '{}' was not allocated during scan", name))
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
        match stmt {
            Stmt::FunctionDef { name, args, body } => {
                self.scan_function(name, &args.args, body);
            }

            Stmt::Assign { targets, value } => {
                self.scan_expr(value);
                for target in targets {
                    if let Expr::Name { id, .. } = target {
                        self.declare_var(id);
                    }
                    self.scan_expr(target);
                }
            }

            Stmt::While { test, body } => {
                self.scan_expr(test);
                for s in body {
                    self.scan_stmt(s);
                }
            }

            Stmt::If { test, body, orelse } => {
                self.scan_expr(test);
                for s in body.iter().chain(orelse) {
                    self.scan_stmt(s);
                }
            }

            Stmt::For { target, iter, body } => {
                if let Expr::Name { id, .. } = &**target {
                    self.declare_var(id);
                }
                self.scan_expr(iter);
                for s in body {
                    self.scan_stmt(s);
                }
            }

            Stmt::Return { value } => {
                if let Some(v) = value {
                    self.scan_expr(v);
                }
            }

            Stmt::Expr { value } => {
                self.scan_expr(value);
            }

            Stmt::Delete { targets } => {
                for t in targets {
                    self.scan_expr(t);
                }
            }

            Stmt::Break | Stmt::Continue => {}
        }
    }

    fn scan_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Constant { value, .. } => {
                if let Constant::Str(s) = &**value {
                    self.intern_string(s);
                }
            }

            Expr::BinOp { left, right, .. } => {
                self.scan_expr(left);
                self.scan_expr(right);
            }

            Expr::UnaryOp { operand, .. } => {
                self.scan_expr(operand);
            }

            Expr::BoolOp { values, .. } => {
                for v in values {
                    self.scan_expr(v);
                }
            }

            Expr::Compare {
                left, comparators, ..
            } => {
                self.scan_expr(left);
                for c in comparators {
                    self.scan_expr(c);
                }
            }

            Expr::Call { func, args } => {
                self.scan_expr(func);
                for arg in args {
                    self.scan_expr(arg);
                }
            }

            Expr::Subscript { value, slice, .. } => {
                self.scan_expr(value);
                self.scan_expr(slice);
            }

            Expr::List { elts, .. } => {
                for e in elts {
                    self.scan_expr(e);
                }
            }

            Expr::Name { .. } => {}
        }
    }

    fn gen_constant(&mut self, value: &Constant) {
        match value {
            Constant::Int(n) => {
                self.emit(&format!("\tmov\trax, {}", n));
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
            _ => panic!("unsupported constant value"),
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Constant { value, .. } => {
                self.gen_constant(value);
            }

            Expr::Name { id, ctx } => {
                if let Some(offset) = self.lookup_var(id) {
                    match ctx {
                        ExprContext::Load => {
                            self.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", offset));
                        }
                        ExprContext::Store => {
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", offset));
                        }
                        ExprContext::Delete => {
                            // zero the slot
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));
                        }
                    }
                } else {
                    // undefined variable, possibly a runtime error
                    self.emit(&format!("\t# Error: undefined variable '{}'", id));
                    self.emit("\txor\trax, rax");
                }
            }

            Expr::BinOp { op, left, right } => {
                // evaluate right, push it
                self.gen_expr(right);
                self.emit("\tpush\trax");

                // evaluate left
                self.gen_expr(left);

                // pop right into rbx
                self.emit("\tpop\trbx");

                match op {
                    Operator::Add => self.emit("\tadd\trax, rbx"),
                    Operator::Subtract => self.emit("\tsub\trax, rbx"),
                    Operator::Multiply => self.emit("\timul\trax, rbx"),
                    Operator::Divide => {
                        // x64 division: rax = rdx:rax / rbx
                        self.emit("\txor\trdx, rdx"); // clear rdx
                        self.emit("\tidiv\trbx");
                    }
                }
            }

            Expr::BoolOp { op, values } => {
                if values.is_empty() {
                    return;
                }

                match op {
                    BoolOp::And => {
                        let end_label = self.new_label("and_end");

                        for (i, val) in values.iter().enumerate() {
                            self.gen_expr(val);
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
                            self.gen_expr(val);
                            if i < values.len() - 1 {
                                self.emit("\ttest\trax, rax");
                                self.emit(&format!("\tjnz\t{}", end_label));
                            }
                        }
                        self.emit(&format!("{}:", end_label));
                    }
                }
            }

            Expr::UnaryOp { op, operand } => {
                self.gen_expr(operand);
                match op {
                    UnaryOp::Not => {
                        self.emit("\ttest\trax, rax");
                        self.emit("\tsetz\tal");
                        self.emit("\tmovzx\trax, al");
                    }
                    UnaryOp::UnaryAdd => {
                        // no-op
                    }
                    UnaryOp::UnarySub => {
                        self.emit("\tneg\trax");
                    }
                }
            }

            Expr::Compare {
                left,
                ops: _,
                comparators,
            } => {
                // simplified: single comparison only
                if !comparators.is_empty() {
                    self.gen_expr(left);
                    self.emit("\tpush\trax");
                    self.gen_expr(&comparators[0]);
                    self.emit("\tmov\trbx, rax");
                    self.emit("\tpop\trax");
                    self.emit("\tcmp\trax, rbx");

                    // equality only for now
                    self.emit("\tsete\tal");
                    self.emit("\tmovzx\trax, al");
                }
            }

            Expr::Call { func, args } => {
                // save caller-saved registers
                self.emit("\tpush\trdi");
                self.emit("\tpush\trsi");
                self.emit("\tpush\trdx");
                self.emit("\tpush\trcx");
                self.emit("\tpush\tr8");
                self.emit("\tpush\tr9");

                // pass arguments (System V AMD64 ABI: rdi, rsi, rdx, rcx, r8, r9)
                let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                for (i, arg) in args.iter().enumerate() {
                    self.gen_expr(arg);
                    if i < arg_regs.len() {
                        self.emit(&format!("\tmov\t{}, rax", arg_regs[i]));
                    } else {
                        // push to stack for additional args
                        self.emit("\tpush\trax");
                    }
                }

                // call the function
                if let Expr::Name { id, .. } = &**func {
                    self.emit(&format!("\tcall\t{}", id));
                }

                // restore caller-saved registers
                self.emit("\tpop\tr9");
                self.emit("\tpop\tr8");
                self.emit("\tpop\trcx");
                self.emit("\tpop\trdx");
                self.emit("\tpop\trsi");
                self.emit("\tpop\trdi");
            }

            Expr::Subscript { value, slice, .. } => {
                // simplified array access, value is the base address
                self.gen_expr(slice);
                self.emit("\timul\trax, 8"); // scale by 8 bytes
                self.emit("\tpush\trax");

                self.gen_expr(value);
                self.emit("\tpop\trbx");
                self.emit("\tadd\trax, rbx");
                self.emit("\tmov\trax, QWORD PTR [rax]");
            }

            Expr::List { elts, .. } => {
                // simplified: evaluate elements only, real lists need heap allocation
                if !elts.is_empty() {
                    for elt in elts {
                        self.gen_expr(elt);
                    }
                }
            }
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Assign { targets, value } => {
                self.gen_expr(value);

                for target in targets {
                    match target {
                        Expr::Name { id, .. } => {
                            let offset = self.slot_of(id);
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", offset));
                        }
                        Expr::Subscript { value, slice, .. } => {
                            // store to array element
                            self.emit("\tpush\trax"); // save value

                            self.gen_expr(slice);
                            self.emit("\timul\trax, 8");
                            self.emit("\tpush\trax");

                            self.gen_expr(value);
                            self.emit("\tpop\trbx");
                            self.emit("\tadd\trax, rbx");

                            self.emit("\tpop\trbx"); // restore value
                            self.emit("\tmov\tQWORD PTR [rax], rbx");
                        }
                        _ => {}
                    }
                }
            }
            Stmt::Return { value } => {
                if let Some(val) = value {
                    self.gen_expr(val);
                }

                // function epilogue
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");
            }

            Stmt::FunctionDef { name, args, body } => {
                let func_info = &self.env.functions[name];
                let locals = func_info.locals.clone();
                let stack_size = align16(func_info.stack_size);
                self.current_function = Some(name.clone());

                // locals and their offsets were already laid out by the scan pass
                self.env.scopes.push(locals);

                // function label
                if name == "main" {
                    self.emit("\t.globl main");
                }
                self.emit(&format!("{}:", name));

                // function prologue
                self.emit("\tpush\trbp");
                self.emit("\tmov\trbp, rsp");

                if stack_size > 0 {
                    self.emit(&format!("\tsub\trsp, {}", stack_size));
                }

                // save arguments to local variables
                let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (i, arg) in args.args.iter().enumerate() {
                    let offset = self.slot_of(&arg.arg);
                    if i < arg_regs.len() {
                        self.emit(&format!(
                            "\tmov\tQWORD PTR [rbp - {}], {}",
                            offset, arg_regs[i]
                        ));
                    }
                }

                for stmt in body {
                    self.gen_stmt(stmt);
                }

                // default return if no explicit return
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");

                // restore state
                self.exit_scope();
                self.current_function = None;
            }

            Stmt::While { test, body } => {
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                self.emit(&format!("{}:", start_label));

                // test condition
                self.gen_expr(test);
                self.emit("\ttest\trax, rax");
                self.emit(&format!("\tjz\t{}", end_label));

                // loop body
                for stmt in body {
                    self.gen_stmt(stmt);
                }

                self.emit(&format!("\tjmp\t{}", start_label));
                self.emit(&format!("{}:", end_label));

                self.break_labels.pop();
                self.continue_labels.pop();
            }

            Stmt::If { test, body, orelse } => {
                let else_label = self.new_label("if_else");
                let end_label = self.new_label("if_end");

                // test condition
                self.gen_expr(test);
                self.emit("\ttest\trax, rax");

                if orelse.is_empty() {
                    self.emit(&format!("\tjz\t{}", end_label));

                    for stmt in body {
                        self.gen_stmt(stmt);
                    }

                    self.emit(&format!("{}:", end_label));
                } else {
                    self.emit(&format!("\tjz\t{}", else_label));

                    for stmt in body {
                        self.gen_stmt(stmt);
                    }

                    self.emit(&format!("\tjmp\t{}", end_label));
                    self.emit(&format!("{}:", else_label));

                    for stmt in orelse {
                        self.gen_stmt(stmt);
                    }

                    self.emit(&format!("{}:", end_label));
                }
            }

            Stmt::For { target, iter, body } => {
                // simplified: iter evaluates to a count
                let start_label = self.new_label("for_start");
                let end_label = self.new_label("for_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                // initialize counter
                if let Expr::Name { id, .. } = &**target {
                    let offset = self.slot_of(id);
                    self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));

                    // get limit
                    self.gen_expr(iter);
                    self.emit("\tpush\trax");

                    self.emit(&format!("{}:", start_label));

                    // check condition
                    self.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", offset));
                    self.emit("\tpop\trbx");
                    self.emit("\tpush\trbx");
                    self.emit("\tcmp\trax, rbx");
                    self.emit(&format!("\tjge\t{}", end_label));

                    // body
                    for stmt in body {
                        self.gen_stmt(stmt);
                    }

                    // increment
                    self.emit(&format!("\tinc\tQWORD PTR [rbp - {}]", offset));
                    self.emit(&format!("\tjmp\t{}", start_label));

                    self.emit(&format!("{}:", end_label));
                    self.emit("\tpop\trbx"); // clean up limit
                }

                self.break_labels.pop();
                self.continue_labels.pop();
            }

            Stmt::Expr { value } => {
                self.gen_expr(value);
            }

            Stmt::Break => {
                if let Some(label) = self.break_labels.last() {
                    self.emit(&format!("\tjmp\t{}", label));
                }
            }

            Stmt::Continue => {
                if let Some(label) = self.continue_labels.last() {
                    self.emit(&format!("\tjmp\t{}", label));
                }
            }

            Stmt::Delete { targets } => {
                for target in targets {
                    if let Expr::Name { id, .. } = target
                        && let Some(offset) = self.lookup_var(id)
                    {
                        self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));
                    }
                }
            }
        }
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
        match stmt {
            Stmt::Expr { value } => self.collect_calls_from_expr(value, calls),
            Stmt::Assign { targets, value } => {
                for target in targets {
                    self.collect_calls_from_expr(target, calls);
                }
                self.collect_calls_from_expr(value, calls);
            }
            Stmt::Return { value } => {
                if let Some(v) = value {
                    self.collect_calls_from_expr(v, calls);
                }
            }
            Stmt::If { test, body, orelse } => {
                self.collect_calls_from_expr(test, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
                for s in orelse {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::While { test, body } => {
                self.collect_calls_from_expr(test, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::For { target, iter, body } => {
                self.collect_calls_from_expr(target, calls);
                self.collect_calls_from_expr(iter, calls);
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::FunctionDef { body, .. } => {
                for s in body {
                    self.collect_calls_from_stmt(s, calls);
                }
            }
            Stmt::Delete { targets } => {
                for target in targets {
                    self.collect_calls_from_expr(target, calls);
                }
            }
            Stmt::Break | Stmt::Continue => {}
        }
    }

    fn collect_calls_from_expr(&self, expr: &Expr, calls: &mut HashSet<String>) {
        match expr {
            Expr::Call { func, args } => {
                // stdlib call
                if let Expr::Name { id, .. } = &**func
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
            Expr::BinOp { left, right, .. } => {
                self.collect_calls_from_expr(left, calls);
                self.collect_calls_from_expr(right, calls);
            }
            Expr::UnaryOp { operand, .. } => {
                self.collect_calls_from_expr(operand, calls);
            }
            Expr::BoolOp { values, .. } => {
                for val in values {
                    self.collect_calls_from_expr(val, calls);
                }
            }
            Expr::Compare {
                left, comparators, ..
            } => {
                self.collect_calls_from_expr(left, calls);
                for comp in comparators {
                    self.collect_calls_from_expr(comp, calls);
                }
            }
            Expr::Subscript { value, slice, .. } => {
                self.collect_calls_from_expr(value, calls);
                self.collect_calls_from_expr(slice, calls);
            }
            Expr::List { elts, .. } => {
                for elt in elts {
                    self.collect_calls_from_expr(elt, calls);
                }
            }
            Expr::Constant { .. } | Expr::Name { .. } => {}
        }
    }

    #[inline(always)]
    fn is_stdlib_function(&self, name: &str) -> bool {
        BUILTINS.contains(&name)
    }

    fn emit_stdlib(&mut self, calls: Vec<String>) {
        if calls.is_empty() {
            return;
        }

        self.emit("\t# Standard Library Functions");

        for func in calls {
            match func.as_str() {
                "print" => print(self),
                "len" => len(self),
                _ => {}
            }
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
}

/// Rounds a frame size up to the 16-byte stack alignment required by the System V ABI.
///
/// For example, `align16(20)` returns `32` and `align16(32)` returns `32`.
fn align16(size: i32) -> i32 {
    (size + 15) & !15
}
