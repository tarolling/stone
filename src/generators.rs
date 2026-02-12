//! Assembly Code Generators

pub mod common;

use crate::generators::common::{Architecture, AssemblyGenerator};
use crate::stdlib::BUILTINS;
use crate::stdlib::x64::builtins::{len, print};
use crate::types::{BoolOp, Constant, Expr, ExprContext, Mod, Operator, Stmt, UnaryOp};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

/// x64 Code Generator
///
/// Only produces Intel syntax for assembly
pub struct X64Generator {
    output: String,
    label_count: usize,
    stack_offset: i32,
    vars: HashMap<String, i32>,
    current_function: Option<String>,
    break_labels: Vec<String>,
    continue_labels: Vec<String>,
    string_literals: HashMap<String, String>,
}

impl AssemblyGenerator for X64Generator {
    fn emit(&mut self, code: &str) {
        self.output.push_str(code);
        self.output.push('\n');
    }

    fn architecture(&self) -> Architecture {
        Architecture::X64
    }
}

impl X64Generator {
    pub fn new() -> Self {
        X64Generator {
            output: String::new(),
            label_count: 0,
            stack_offset: 0,
            vars: HashMap::new(),
            current_function: None,
            break_labels: Vec::new(),
            continue_labels: Vec::new(),
            string_literals: HashMap::new(),
        }
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_count);
        self.label_count += 1;
        label
    }

    fn intern_string(&mut self, content: &str) -> String {
        // Check if we already have this string
        if let Some(label) = self.string_literals.get(content) {
            return label.clone();
        }

        // Create new label for this string
        let label = self.new_label("str");
        self.string_literals
            .insert(content.to_string(), label.clone());
        label
    }

    fn allocate_var(&mut self, name: &str) -> i32 {
        if let Some(&offset) = self.vars.get(name) {
            offset
        } else {
            self.stack_offset += 8;
            self.vars.insert(name.to_string(), self.stack_offset);
            self.stack_offset
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
                if let Some(&offset) = self.vars.get(id) {
                    match ctx {
                        ExprContext::Load => {
                            self.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", offset));
                        }
                        ExprContext::Store => {
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", offset));
                        }
                        ExprContext::Delete => {
                            // Could clear memory or just leave it
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));
                        }
                    }
                } else {
                    // Variable not found - could be a runtime error
                    self.emit(&format!("\t# Error: undefined variable '{}'", id));
                    self.emit("\txor\trax, rax");
                }
            }

            Expr::BinOp { op, left, right } => {
                // Evaluate right, push it
                self.gen_expr(right);
                self.emit("\tpush\trax");

                // Evaluate left
                self.gen_expr(left);

                // Pop right into rbx
                self.emit("\tpop\trbx");

                match op {
                    Operator::Add => self.emit("\tadd\trax, rbx"),
                    Operator::Subtract => self.emit("\tsub\trax, rbx"),
                    Operator::Multiply => self.emit("\timul\trax, rbx"),
                    Operator::Divide => {
                        // x64 division: rax = rdx:rax / rbx
                        self.emit("\txor\trdx, rdx"); // Clear rdx
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
                        // No-op
                    }
                    UnaryOp::UnarySub => {
                        self.emit("\tneg\trax");
                    }
                }
            }

            Expr::Compare {
                left,
                ops,
                comparators,
            } => {
                // Simplified: only handle single comparison
                if !comparators.is_empty() {
                    self.gen_expr(left);
                    self.emit("\tpush\trax");
                    self.gen_expr(&comparators[0]);
                    self.emit("\tmov\trbx, rax");
                    self.emit("\tpop\trax");
                    self.emit("\tcmp\trax, rbx");

                    // For now, assume equals comparison
                    self.emit("\tsete\tal");
                    self.emit("\tmovzx\trax, al");
                }
            }

            Expr::Call { func, args } => {
                // Save caller-saved registers
                self.emit("\tpush\trdi");
                self.emit("\tpush\trsi");
                self.emit("\tpush\trdx");
                self.emit("\tpush\trcx");
                self.emit("\tpush\tr8");
                self.emit("\tpush\tr9");

                // Pass arguments (System V AMD64 ABI: rdi, rsi, rdx, rcx, r8, r9)
                let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                for (i, arg) in args.iter().enumerate() {
                    self.gen_expr(arg);
                    if i < arg_regs.len() {
                        self.emit(&format!("\tmov\t{}, rax", arg_regs[i]));
                    } else {
                        // Push to stack for additional args
                        self.emit("\tpush\trax");
                    }
                }

                // Call the function
                if let Expr::Name { id, .. } = &**func {
                    self.emit(&format!("\tcall\t{}", id));
                }

                // Restore caller-saved registers
                self.emit("\tpop\tr9");
                self.emit("\tpop\tr8");
                self.emit("\tpop\trcx");
                self.emit("\tpop\trdx");
                self.emit("\tpop\trsi");
                self.emit("\tpop\trdi");
            }

            Expr::Subscript { value, slice, .. } => {
                // Simplified array access: assume value is base address
                self.gen_expr(slice);
                self.emit("\timul\trax, 8"); // Scale by 8 bytes
                self.emit("\tpush\trax");

                self.gen_expr(value);
                self.emit("\tpop\trbx");
                self.emit("\tadd\trax, rbx");
                self.emit("\tmov\trax, QWORD PTR [rax]");
            }

            Expr::List { elts, .. } => {
                // Simplified: just evaluate elements (would need heap allocation in real impl)
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
                            let offset = self.allocate_var(id);
                            self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", offset));
                        }
                        Expr::Subscript { value, slice, .. } => {
                            // Store to array element
                            self.emit("\tpush\trax"); // Save value

                            self.gen_expr(slice);
                            self.emit("\timul\trax, 8");
                            self.emit("\tpush\trax");

                            self.gen_expr(value);
                            self.emit("\tpop\trbx");
                            self.emit("\tadd\trax, rbx");

                            self.emit("\tpop\trbx"); // Restore value
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

                // Function epilogue
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");
            }

            Stmt::FunctionDef { name, args, body } => {
                self.current_function = Some(name.clone());
                let saved_vars = self.vars.clone();
                let saved_offset = self.stack_offset;
                self.vars.clear();
                self.stack_offset = 0;

                // Function label
                if name == "main" {
                    self.emit("\t.globl main");
                }
                self.emit(&format!("{}:", name));

                // Function prologue
                self.emit("\tpush\trbp");
                self.emit("\tmov\trbp, rsp");

                // Save arguments to local variables
                let arg_regs = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for (i, arg) in args.args.iter().enumerate() {
                    let offset = self.allocate_var(&arg.arg);
                    if i < arg_regs.len() {
                        self.emit(&format!(
                            "\tmov\tQWORD PTR [rbp - {}], {}",
                            offset, arg_regs[i]
                        ));
                    }
                }

                // generate body first
                let original_output = self.output.clone();
                for stmt in body {
                    self.gen_stmt(stmt);
                }
                let body_output = self.output[original_output.len()..].to_string();
                self.output = original_output;

                // NOW allocate stack space for locals
                if self.stack_offset > 0 {
                    self.emit(&format!("\tsub\trsp, {}", self.stack_offset));
                }

                self.output.push_str(&body_output);

                // Default return if no explicit return
                self.emit("\tmov\trsp, rbp");
                self.emit("\tpop\trbp");
                self.emit("\tret");

                // Restore state
                self.vars = saved_vars;
                self.stack_offset = saved_offset;
                self.current_function = None;
            }

            Stmt::While { test, body } => {
                let start_label = self.new_label("while_start");
                let end_label = self.new_label("while_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                self.emit(&format!("{}:", start_label));

                // Test condition
                self.gen_expr(test);
                self.emit("\ttest\trax, rax");
                self.emit(&format!("\tjz\t{}", end_label));

                // Loop body
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

                // Test condition
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
                // Simplified: assume iter evaluates to a count
                let start_label = self.new_label("for_start");
                let end_label = self.new_label("for_end");

                self.break_labels.push(end_label.clone());
                self.continue_labels.push(start_label.clone());

                // Initialize counter
                if let Expr::Name { id, .. } = &**target {
                    let offset = self.allocate_var(id);
                    self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));

                    // Get limit
                    self.gen_expr(iter);
                    self.emit("\tpush\trax");

                    self.emit(&format!("{}:", start_label));

                    // Check condition
                    self.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", offset));
                    self.emit("\tpop\trbx");
                    self.emit("\tpush\trbx");
                    self.emit("\tcmp\trax, rbx");
                    self.emit(&format!("\tjge\t{}", end_label));

                    // Body
                    for stmt in body {
                        self.gen_stmt(stmt);
                    }

                    // Increment
                    self.emit(&format!("\tinc\tQWORD PTR [rbp - {}]", offset));
                    self.emit(&format!("\tjmp\t{}", start_label));

                    self.emit(&format!("{}:", end_label));
                    self.emit("\tpop\trbx"); // Clean up limit
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
                        && let Some(&offset) = self.vars.get(id)
                    {
                        self.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", offset));
                    }
                }
            }
        }
    }

    /// scan AST for stdlib function calls
    fn collect_stdlib_calls(&self, module: &Mod) -> Vec<String> {
        let mut calls = std::collections::HashSet::new();

        match module {
            Mod::Module { body } => {
                for stmt in body {
                    self.collect_calls_from_stmt(stmt, &mut calls);
                }
            }
        }

        calls.into_iter().collect()
    }

    fn collect_calls_from_stmt(&self, stmt: &Stmt, calls: &mut std::collections::HashSet<String>) {
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

    fn collect_calls_from_expr(&self, expr: &Expr, calls: &mut std::collections::HashSet<String>) {
        match expr {
            Expr::Call { func, args } => {
                // Check if it's a stdlib function
                if let Expr::Name { id, .. } = &**func
                    && self.is_stdlib_function(id)
                {
                    calls.insert(id.clone());
                }

                // Check arguments too
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

            // Escape special characters for assembly
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

    pub fn generate(&mut self, module: &Mod) -> String {
        self.emit("\t.intel_syntax noprefix");
        self.emit("\t.text");

        // go find all of the standard lib calls first; we only bring in what we need
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

                    for stmt in top_level_stmts {
                        self.gen_stmt(stmt);
                    }

                    self.emit("\txor\trax, rax"); // Return 0
                    self.emit("\tmov\trsp, rbp");
                    self.emit("\tpop\trbp");
                    self.emit("\tret");
                }
            }
        }

        self.emit_rodata();

        // supress executable stack warning from linker
        self.emit("\t.section\t.note.GNU-stack,\"\",@progbits");

        match std::fs::create_dir_all("build") {
            Ok(_) => {
                let mut file = File::create("build/out.s").unwrap();
                let _ = file.write_all(self.output.clone().as_bytes());
            }
            Err(_) => panic!("Unable to write to build file"),
        }

        self.output.clone()
    }

    pub fn compile(&mut self, module: &Mod) -> Result<(), String> {
        self.generate(module);

        // let assembler =
        //     find_assembler().ok_or("No assembler found. Please install 'as', 'nasm', or 'yasm'")?;
        // let linker =
        //     find_linker().ok_or("No linker found. Please install 'gcc', 'clang', or 'lld'")?;

        // assemble and link here for now
        let output = std::process::Command::new("gcc")
            .arg("-g")
            .arg("-o")
            .arg("build/out")
            .arg("build/out.s")
            .arg("-no-pie")
            .status()
            .expect("gcc should succeed");

        if !output.success() {
            eprintln!("Compilation failed");
        }

        Ok(())
    }
}
