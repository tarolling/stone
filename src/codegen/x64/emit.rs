//! Instruction selection from register-allocated [`ir`](crate::codegen::ir) to x86-64.
//!
//! Each function's vregs are assigned by [`linear_scan`] to [`CALLEE_SAVED`] or [`CALLER_SAVED`]
//! registers, or to spill slots below the saved registers in its frame. `rax`, `rcx`, `rdx`, and `xmm0` to `xmm2` are
//! never allocated: instructions use them as scratch, such as `rdx:rax` for `idiv`, and an operand
//! that is in memory or too wide for an immediate is loaded into one first. For example,
//! `v2 = add v0, v1` with `v0` in `rbx`, `v1` in `r12`, and `v2` in `r13` becomes
//! `mov r13, rbx` then `add r13, r12`.

use super::{ARG_REGS, X64Generator};
use crate::codegen::context::{FALLIBLE_ROUTINES, function_label, global_label, variant_label};
use crate::codegen::ir::liveness::analyze;
use crate::codegen::ir::{
    BinOp, BlockId, Callee, Cond, FIELDS_OFFSET, Function, Inst, Kernel, Operand, RcKind,
    Terminator, VReg, variable_of,
};
use crate::codegen::regalloc::{Location, hints, linear_scan, parallel_moves};
use crate::codegen::{AssemblyGenerator, magic_signed};
use crate::stdlib::{INDEX_RANGE, MAX_CALL_DEPTH};
use std::collections::HashMap;
use std::fmt;

/// Registers that calls preserve, which are the only ones that can hold a value across a call.
const CALLEE_SAVED: [&str; 5] = ["rbx", "r12", "r13", "r14", "r15"];

/// Registers that calls clobber, which hold values that do not live across one.
const CALLER_SAVED: [&str; 6] = ["rsi", "rdi", "r8", "r9", "r10", "r11"];

/// The registers a stone function returns its first values in. A tuple's values past these go in
/// an area the caller reserves just above the arguments it passes on the stack, so the callee
/// writes result `j` at `[rbp + 16 + 8 * (stack arguments + j - 3)]`. None of them is ever
/// allocated, so moving them into their destinations cannot overwrite one another.
const RESULT_REGS: [&str; 3] = ["rax", "rdx", "rcx"];

/// The registers that hold the ints a runtime error's message is filled with when code jumps to
/// a failure whose message has holes, such as the index and the length of
/// `list index out of range (index {}, length {})`. They are never allocated.
pub(super) const FAIL_VALUES: [&str; 2] = ["rcx", "rdx"];

/// Where a value is while generating code: a register, a slot in the frame, or an immediate.
///
/// Its `Display` is the operand text, such as `rbx`, `QWORD PTR [rbp - 16]`, or `42`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Value {
    Reg(&'static str),
    /// A spill slot at `[rbp - offset]`.
    Stack(i32),
    Imm(i64),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Reg(reg) => write!(f, "{reg}"),
            Value::Stack(offset) => write!(f, "QWORD PTR [rbp - {offset}]"),
            Value::Imm(n) => write!(f, "{n}"),
        }
    }
}

/// The vector registers, by number.
const XMM: [&str; 16] = [
    "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10",
    "xmm11", "xmm12", "xmm13", "xmm14", "xmm15",
];

/// The registers of a packed loop while it is emitted.
struct Packing {
    /// The vector registers holding nothing, taken from the end.
    free: Vec<&'static str>,
    /// Each number in the kernel and the register holding it in both halves.
    numbers: Vec<(Operand, &'static str)>,
    /// The register holding the sign bit in both halves, for negating floats.
    mask: Option<&'static str>,
    float: bool,
    /// The label after the loop.
    stop: String,
}

impl Packing {
    fn take(&mut self) -> Result<&'static str, String> {
        self.free
            .pop()
            .ok_or_else(|| "packed arithmetic needs too many registers".to_string())
    }

    fn give(&mut self, reg: &'static str) {
        self.free.push(reg);
    }
}

/// Adds every number in `kernel` to `out`, left to right.
fn number_operands(kernel: &Kernel, out: &mut Vec<Operand>) {
    match kernel {
        Kernel::Number(value) => out.push(*value),
        Kernel::Element(_) => {}
        Kernel::Binary(_, lhs, rhs) => {
            number_operands(lhs, out);
            number_operands(rhs, out);
        }
        Kernel::Neg(operand) => number_operands(operand, out),
    }
}

/// Returns whether `kernel` negates anything.
fn has_neg(kernel: &Kernel) -> bool {
    match kernel {
        Kernel::Number(_) | Kernel::Element(_) => false,
        Kernel::Binary(_, lhs, rhs) => has_neg(lhs) || has_neg(rhs),
        Kernel::Neg(_) => true,
    }
}

/// Returns whether `n` fits the sign-extended 32-bit immediate most instructions take.
fn fits_i32(n: i64) -> bool {
    i32::try_from(n).is_ok()
}

/// The suffix of the conditional jump or `set` instruction for a signed int comparison.
///
/// For example, `cc(Cond::Le)` is `"le"`, as in `jle` and `setle`.
fn cc(cond: Cond) -> &'static str {
    match cond {
        Cond::Eq => "e",
        Cond::Ne => "ne",
        Cond::Lt => "l",
        Cond::Le => "le",
        Cond::Gt => "g",
        Cond::Ge => "ge",
    }
}

/// A function being emitted: where each vreg lives, and its labels.
struct Frame {
    locations: HashMap<VReg, Location<&'static str>>,
    saved: usize,
    blocks: Vec<String>,
    epilogue: String,
    is_main: bool,
    /// How many parameters arrive on the stack, above which the caller reserves room for the
    /// results past [`RESULT_REGS`].
    stack_params: usize,
}

impl Frame {
    fn value(&self, operand: Operand) -> Result<Value, String> {
        match operand {
            Operand::Imm(n) => Ok(Value::Imm(n)),
            Operand::Reg(reg) => match self.locations.get(&reg) {
                Some(Location::Reg(name)) => Ok(Value::Reg(name)),
                Some(Location::Stack(slot)) => Ok(Value::Stack(8 * (self.saved + slot + 1) as i32)),
                None => Err(format!("{reg} was never allocated")),
            },
        }
    }

    fn reg(&self, reg: VReg) -> Result<Value, String> {
        self.value(Operand::Reg(reg))
    }
}

impl X64Generator {
    /// Returns a label that stops the program with `message`, pointing at the instruction being
    /// emitted.
    fn fail_here(&mut self, message: &str) -> String {
        self.ctx.fail_at(message, self.span)
    }

    /// Allocates registers for `function` and emits it, from its label through its epilogue.
    pub(super) fn emit_function(&mut self, function: &Function) -> Result<(), String> {
        let liveness = analyze(function);
        let allocation = linear_scan(
            &liveness.intervals,
            &CALLEE_SAVED,
            &CALLER_SAVED,
            &hints(function, &ARG_REGS),
        );
        crate::debug!("{function}");
        crate::debug!("allocation: {:?}", allocation.locations);

        let blocks = (0..function.blocks.len())
            .map(|_| self.ctx.new_label("block"))
            .collect();
        let frame = Frame {
            locations: allocation.locations,
            saved: allocation.used_callee_saved.len(),
            blocks,
            epilogue: self.ctx.new_label("return"),
            is_main: function.name.is_none(),
            stack_params: function.params.len().saturating_sub(ARG_REGS.len()),
        };

        match &function.name {
            Some(name) if function.helper => self.emit(&format!("{name}:")),
            Some(name) => self.emit(&format!("{}:", function_label(name))),
            None => {
                self.emit("\t.globl\tmain");
                self.emit("main:");
            }
        }
        self.emit("\tpush\trbp");
        self.emit("\tmov\trbp, rsp");
        if frame.is_main && self.ctx.uses(&["stone.args"]) {
            // argc is a C int, so only its low half is set
            self.emit("\tmov\tDWORD PTR [rip + stone.argc], edi");
            self.emit("\tmov\tQWORD PTR [rip + stone.argv], rsi");
        }
        if frame.is_main && self.ctx.needs_env() {
            self.emit("\tmov\tQWORD PTR [rip + stone.envp], rdx");
        }
        for reg in &allocation.used_callee_saved {
            self.emit(&format!("\tpush\t{reg}"));
        }
        if allocation.spill_slots > 0 {
            self.emit(&format!("\tsub\trsp, {}", 8 * allocation.spill_slots));
        }

        if !frame.is_main && !function.helper {
            // the same limit as the interpreter, rather than overflowing the stack
            // each call checks the depth first, so the error points at the call
            self.emit("\tinc\tQWORD PTR [rip + stone.call_depth]");
        }

        // arguments arrive in ARG_REGS and, past the sixth, on the caller's stack
        let needed = |param: &VReg| liveness.live_at_entry[param.0 as usize];
        let mut moves = Vec::new();
        for (param, reg) in function.params.iter().zip(ARG_REGS) {
            if needed(param) {
                moves.push((frame.reg(*param)?, Value::Reg(reg)));
            }
        }
        for (dst, src) in parallel_moves(&moves, Value::Reg("rax")) {
            self.mov(dst, src);
        }
        let count = function.params.len();
        for (i, param) in function.params.iter().enumerate().skip(ARG_REGS.len()) {
            if needed(param) {
                let target = self.target(frame.reg(*param)?, None);
                let offset = 16 + 8 * (count - 1 - i);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [rbp + {offset}]"));
                self.store(frame.reg(*param)?, target);
            }
        }

        for (i, block) in function.blocks.iter().enumerate() {
            self.emit(&format!("{}:", frame.blocks[i]));
            for (inst, span) in block.insts.iter().zip(&block.spans) {
                self.span = *span;
                self.emit_inst(&frame, inst)?;
            }
            self.span = block.term_span;
            self.emit_terminator(&frame, &block.term, BlockId(i + 1))?;
        }

        self.emit(&format!("{}:", frame.epilogue));
        if frame.is_main && self.ctx.counts_references {
            self.emit("\tcall\tstone.leak_check");
        }
        if !frame.is_main && !function.helper {
            self.emit("\tdec\tQWORD PTR [rip + stone.call_depth]");
        }
        if allocation.used_callee_saved.is_empty() {
            self.emit("\tmov\trsp, rbp");
        } else {
            let saved = allocation.used_callee_saved.len();
            self.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved));
            for reg in allocation.used_callee_saved.iter().rev() {
                self.emit(&format!("\tpop\t{reg}"));
            }
        }
        self.emit("\tpop\trbp");
        self.emit("\tret");
        Ok(())
    }

    /// Returns the register to compute a result for `dst` in: `dst` itself when it is a register
    /// that `avoid` does not need, and `rax` otherwise.
    fn target(&self, dst: Value, avoid: Option<Value>) -> &'static str {
        match dst {
            Value::Reg(reg) if avoid != Some(dst) => reg,
            _ => "rax",
        }
    }

    /// Moves `src` into `dst`, going through `rax` when both are in memory or the immediate is too
    /// wide to store directly.
    /// Emits a [`Inst::Packed`] loop with SSE2, two elements at a time in `xmm3` to `xmm15`, which
    /// hold no vregs. `rcx` counts the elements done, `rdx` holds the number of whole pairs' worth,
    /// and `rax` holds a pointer that was spilled. Each number is copied to both halves of a
    /// register once, before the loop.
    fn packed(
        &mut self,
        frame: &Frame,
        done: VReg,
        float: bool,
        kernel: &Kernel,
        out: Operand,
        len: Operand,
    ) -> Result<(), String> {
        let mut packing = Packing {
            free: (3..16).rev().map(|n| XMM[n]).collect(),
            numbers: Vec::new(),
            mask: None,
            float,
            stop: self.ctx.new_label("packed_done"),
        };
        let mut numbers = Vec::new();
        number_operands(kernel, &mut numbers);
        for number in numbers {
            if packing.numbers.iter().any(|(n, _)| *n == number) {
                continue;
            }
            let reg = packing.take()?;
            match frame.value(number)? {
                Value::Reg(gpr) => self.emit(&format!("\tmovq\t{reg}, {gpr}")),
                value => {
                    self.load("rax", value);
                    self.emit(&format!("\tmovq\t{reg}, rax"));
                }
            }
            self.emit(&format!("\tpunpcklqdq\t{reg}, {reg}"));
            packing.numbers.push((number, reg));
        }
        if float && has_neg(kernel) {
            // flipping the sign bit of each half negates it
            let reg = packing.take()?;
            self.load("rax", Value::Imm(i64::MIN));
            self.emit(&format!("\tmovq\t{reg}, rax"));
            self.emit(&format!("\tpunpcklqdq\t{reg}, {reg}"));
            packing.mask = Some(reg);
        }
        let len = frame.value(len)?;
        self.load("rdx", len);
        self.emit("\tand\trdx, -2");
        self.emit("\txor\tecx, ecx");
        self.emit("\ttest\trdx, rdx");
        self.emit(&format!("\tjz\t{}", packing.stop));
        let top = self.ctx.new_label("packed");
        self.emit(&format!("{top}:"));
        let (result, _) = self.packed_node(frame, &mut packing, kernel)?;
        let base = self.base(frame.value(out)?, "rax");
        self.emit(&format!(
            "\tmovupd\tXMMWORD PTR [{base} + rcx * 8], {result}"
        ));
        self.emit("\tadd\trcx, 2");
        self.emit("\tcmp\trcx, rdx");
        self.emit(&format!("\tjl\t{top}"));
        self.emit(&format!("{}:", packing.stop));
        let done = frame.reg(done)?;
        self.store(done, "rcx");
        Ok(())
    }

    /// Emits the instructions for one node of a packed kernel inside the loop, returning the
    /// register holding its two results and whether the node owns that register, which a
    /// broadcast number does not.
    fn packed_node(
        &mut self,
        frame: &Frame,
        packing: &mut Packing,
        node: &Kernel,
    ) -> Result<(&'static str, bool), String> {
        match node {
            Kernel::Number(value) => {
                let reg = packing
                    .numbers
                    .iter()
                    .find(|(n, _)| n == value)
                    .map(|(_, reg)| *reg)
                    .ok_or("a packed number was not broadcast")?;
                Ok((reg, false))
            }
            Kernel::Element(data) => {
                let reg = packing.take()?;
                let base = self.base(frame.value(*data)?, "rax");
                self.emit(&format!("\tmovupd\t{reg}, XMMWORD PTR [{base} + rcx * 8]"));
                Ok((reg, true))
            }
            Kernel::Binary(op, lhs, rhs) => {
                let (lhs, lhs_owned) = self.packed_node(frame, packing, lhs)?;
                let (rhs, rhs_owned) = self.packed_node(frame, packing, rhs)?;
                let dst = if lhs_owned {
                    lhs
                } else {
                    let reg = packing.take()?;
                    self.emit(&format!("\tmovapd\t{reg}, {lhs}"));
                    reg
                };
                let mnemonic = match (op, packing.float) {
                    (BinOp::Add, true) => "addpd",
                    (BinOp::Sub, true) => "subpd",
                    (BinOp::Mul, true) => "mulpd",
                    (BinOp::Div, true) => {
                        // stop before a pair with a zero divisor, for the single loop to report
                        let zero = packing.take()?;
                        self.emit(&format!("\txorpd\t{zero}, {zero}"));
                        self.emit(&format!("\tcmppd\t{zero}, {rhs}, 0"));
                        self.emit(&format!("\tmovmskpd\teax, {zero}"));
                        self.emit("\ttest\teax, eax");
                        self.emit(&format!("\tjnz\t{}", packing.stop));
                        packing.give(zero);
                        "divpd"
                    }
                    (BinOp::Add, false) => "paddq",
                    (BinOp::Sub, false) => "psubq",
                    _ => return Err(format!("{op:?} cannot be packed")),
                };
                self.emit(&format!("\t{mnemonic}\t{dst}, {rhs}"));
                if rhs_owned {
                    packing.give(rhs);
                }
                Ok((dst, true))
            }
            Kernel::Neg(operand) => {
                let (src, owned) = self.packed_node(frame, packing, operand)?;
                if packing.float {
                    let mask = packing.mask.ok_or("a packed negation has no sign mask")?;
                    let dst = if owned {
                        src
                    } else {
                        let reg = packing.take()?;
                        self.emit(&format!("\tmovapd\t{reg}, {src}"));
                        reg
                    };
                    self.emit(&format!("\txorpd\t{dst}, {mask}"));
                    Ok((dst, true))
                } else {
                    // 0 - x
                    let dst = packing.take()?;
                    self.emit(&format!("\tpxor\t{dst}, {dst}"));
                    self.emit(&format!("\tpsubq\t{dst}, {src}"));
                    if owned {
                        packing.give(src);
                    }
                    Ok((dst, true))
                }
            }
        }
    }

    fn mov(&mut self, dst: Value, src: Value) {
        if dst == src {
            return;
        }
        match (dst, src) {
            (Value::Reg(reg), _) => self.emit(&format!("\tmov\t{reg}, {src}")),
            (_, Value::Reg(_)) => self.emit(&format!("\tmov\t{dst}, {src}")),
            (_, Value::Imm(n)) if fits_i32(n) => self.emit(&format!("\tmov\t{dst}, {n}")),
            _ => {
                self.emit(&format!("\tmov\trax, {src}"));
                self.emit(&format!("\tmov\t{dst}, rax"));
            }
        }
    }

    /// Loads `src` into register `reg`, unless it is already there.
    fn load(&mut self, reg: &'static str, src: Value) {
        self.mov(Value::Reg(reg), src);
    }

    /// Stores register `reg` into `dst`, unless `dst` is that register.
    fn store(&mut self, dst: Value, reg: &'static str) {
        self.mov(dst, Value::Reg(reg));
    }

    /// Returns `value` as the source operand of an arithmetic or compare instruction, which takes
    /// a register, memory, or a 32-bit immediate, loading a wider immediate into `scratch` first.
    fn source(&mut self, value: Value, scratch: &'static str) -> String {
        match value {
            Value::Imm(n) if !fits_i32(n) => {
                self.load(scratch, value);
                scratch.to_string()
            }
            _ => value.to_string(),
        }
    }

    /// Returns a register holding the list pointer `list`, loading it into `scratch` if needed.
    fn base(&mut self, list: Value, scratch: &'static str) -> &'static str {
        match list {
            Value::Reg(reg) => reg,
            _ => {
                self.load(scratch, list);
                scratch
            }
        }
    }

    /// Moves a float's bits from `value` into an `xmm` register.
    fn load_xmm(&mut self, xmm: &str, value: Value) {
        match value {
            Value::Imm(_) => {
                self.load("rax", value);
                self.emit(&format!("\tmovq\t{xmm}, rax"));
            }
            _ => self.emit(&format!("\tmovq\t{xmm}, {value}")),
        }
    }

    /// Emits `cmp lhs, rhs` for an int comparison, loading `lhs` into `rax` when `cmp` cannot
    /// take it as its first operand.
    fn compare_ints(&mut self, lhs: Value, rhs: Value) {
        let lhs = match (lhs, rhs) {
            (Value::Imm(_), _) | (Value::Stack(_), Value::Stack(_)) => {
                self.load("rax", lhs);
                "rax".to_string()
            }
            _ => lhs.to_string(),
        };
        let rhs = self.source(rhs, "rcx");
        self.emit(&format!("\tcmp\t{lhs}, {rhs}"));
    }

    /// Compares the floats in `xmm0` and `xmm1` with `ucomisd`, ordered so that `jcc` with the
    /// returned `(true, false)` suffixes jumps when the comparison does or does not hold.
    ///
    /// `ucomisd` sets ZF, PF, and CF when either side is nan, so `a < b` is computed as `b > a`
    /// with `ja`, which is false for nan. `==` and `!=` also need PF, which the caller checks.
    fn compare_floats(&mut self, cond: Cond) -> (&'static str, &'static str) {
        match cond {
            Cond::Lt | Cond::Le => self.emit("\tucomisd\txmm1, xmm0"),
            _ => self.emit("\tucomisd\txmm0, xmm1"),
        }
        match cond {
            Cond::Lt | Cond::Gt => ("a", "be"),
            Cond::Le | Cond::Ge => ("ae", "b"),
            Cond::Eq => ("e", "ne"),
            Cond::Ne => ("ne", "e"),
        }
    }

    /// Jumps to `then` when flag condition `yes` holds and to `otherwise` when `no` does, leaving
    /// out a jump to `next`, the block that follows.
    fn branch(
        &mut self,
        frame: &Frame,
        (yes, no): (&str, &str),
        then: BlockId,
        otherwise: BlockId,
        next: BlockId,
    ) {
        if then == next {
            self.emit(&format!("\tj{no}\t{}", frame.blocks[otherwise.0]));
        } else {
            self.emit(&format!("\tj{yes}\t{}", frame.blocks[then.0]));
            if otherwise != next {
                self.emit(&format!("\tjmp\t{}", frame.blocks[otherwise.0]));
            }
        }
    }

    fn emit_terminator(
        &mut self,
        frame: &Frame,
        term: &Terminator,
        next: BlockId,
    ) -> Result<(), String> {
        match term {
            Terminator::Jump(target) => {
                if *target != next {
                    self.emit(&format!("\tjmp\t{}", frame.blocks[target.0]));
                }
            }
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => match frame.value(*cond)? {
                Value::Imm(n) => {
                    let target = if n != 0 { then } else { otherwise };
                    if *target != next {
                        self.emit(&format!("\tjmp\t{}", frame.blocks[target.0]));
                    }
                }
                value => {
                    match value {
                        Value::Reg(reg) => self.emit(&format!("\ttest\t{reg}, {reg}")),
                        _ => self.emit(&format!("\tcmp\t{value}, 0")),
                    }
                    self.branch(frame, ("ne", "e"), *then, *otherwise, next);
                }
            },
            Terminator::CmpBranch {
                cond,
                float,
                lhs,
                rhs,
                then,
                otherwise,
            } => {
                let (lhs, rhs) = (frame.value(*lhs)?, frame.value(*rhs)?);
                if *float {
                    self.load_xmm("xmm0", lhs);
                    self.load_xmm("xmm1", rhs);
                    let flags = self.compare_floats(*cond);
                    // a nan operand sets PF, which makes == false and != true
                    match cond {
                        Cond::Eq => self.emit(&format!("\tjp\t{}", frame.blocks[otherwise.0])),
                        Cond::Ne => self.emit(&format!("\tjp\t{}", frame.blocks[then.0])),
                        _ => {}
                    }
                    self.branch(frame, flags, *then, *otherwise, next);
                } else {
                    // an immediate can only be compared second
                    let (cond, lhs, rhs) = match (lhs, rhs) {
                        (Value::Imm(_), Value::Reg(_) | Value::Stack(_)) => (cond.swap(), rhs, lhs),
                        _ => (*cond, lhs, rhs),
                    };
                    self.compare_ints(lhs, rhs);
                    self.branch(
                        frame,
                        (cc(cond), cc(cond.negate())),
                        *then,
                        *otherwise,
                        next,
                    );
                }
            }
            Terminator::Fail(message, values) => {
                if values.len() > FAIL_VALUES.len() {
                    return Err(format!("too many values for the runtime error '{message}'"));
                }
                for (&reg, value) in FAIL_VALUES.iter().zip(values) {
                    let value = frame.value(*value)?;
                    self.load(reg, value);
                }
                let label = self.fail_here(message);
                self.emit(&format!("\tjmp\t{label}"));
            }
            Terminator::Return(values) => {
                let values = values
                    .iter()
                    .map(|value| frame.value(*value))
                    .collect::<Result<Vec<_>, _>>()?;
                // results past the registers go to the caller's area, through rdx, before any
                // result register is set
                for (j, value) in values.iter().enumerate().skip(RESULT_REGS.len()) {
                    let offset = 16 + 8 * (frame.stack_params + j - RESULT_REGS.len());
                    self.store_qword(&format!("[rbp + {offset}]"), *value);
                }
                for (reg, value) in RESULT_REGS.iter().zip(&values).skip(1) {
                    self.load(reg, *value);
                }
                match values.first() {
                    None | Some(Value::Imm(0)) => self.emit("\txor\trax, rax"),
                    Some(value) => self.load("rax", *value),
                }
                if next.0 < frame.blocks.len() {
                    self.emit(&format!("\tjmp\t{}", frame.epilogue));
                }
            }
        }
        Ok(())
    }

    fn emit_inst(&mut self, frame: &Frame, inst: &Inst) -> Result<(), String> {
        match inst {
            Inst::DepthCheck => self.depth_check(),

            Inst::ElementSlot {
                dst,
                list,
                index,
                width,
            } => {
                let (dst, list, index) =
                    (frame.reg(*dst)?, frame.value(*list)?, frame.value(*index)?);
                self.element_slot(list, index, *width as i64);
                self.store(dst, "rcx");
            }

            Inst::ListData { dst, list } => {
                let (dst, list) = (frame.reg(*dst)?, frame.value(*list)?);
                let base = self.base(list, "rax");
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [{base} + 16]"));
                self.store(dst, target);
            }

            Inst::Packed {
                done,
                float,
                kernel,
                out,
                len,
            } => self.packed(frame, *done, *float, kernel, *out, *len)?,

            Inst::Copy { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.mov(dst, src);
            }

            Inst::Binary {
                op: op @ (BinOp::Div | BinOp::Rem),
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                // idiv leaves the quotient in rax and the remainder in rdx
                let result = if *op == BinOp::Div { "rax" } else { "rdx" };
                self.load("rax", lhs);
                match rhs {
                    Value::Imm(divisor) if divisor != 0 && divisor != -1 => {
                        if *op == BinOp::Div {
                            self.divide_by_constant(divisor);
                        } else {
                            self.remainder_by_constant(divisor);
                        }
                    }
                    _ => {
                        self.load("rcx", rhs);
                        // idiv traps on both of these, so report them like the interpreter
                        let by_zero = self.fail_here("division by zero");
                        let divide = self.ctx.new_label("divide");
                        let done = self.ctx.new_label("divided");
                        self.emit("\ttest\trcx, rcx");
                        self.emit(&format!("\tjz\t{by_zero}"));
                        self.emit("\tcmp\trcx, -1");
                        self.emit(&format!("\tjne\t{divide}"));
                        if *op == BinOp::Div {
                            let overflow = self.fail_here("integer overflow in division");
                            // only the minimum overflows when negated
                            self.emit("\tmov\trdx, rax");
                            self.emit("\tneg\trdx");
                            self.emit(&format!("\tjo\t{overflow}"));
                        } else {
                            // anything % -1 is 0, including the minimum, where idiv would trap
                            self.emit("\txor\tedx, edx");
                            self.emit(&format!("\tjmp\t{done}"));
                        }
                        self.emit(&format!("{divide}:"));
                        self.emit("\tcqo"); // sign-extend rax into rdx
                        self.emit("\tidiv\trcx");
                        self.emit(&format!("{done}:"));
                    }
                }
                self.store(dst, result);
            }

            Inst::Binary {
                op: BinOp::Pow,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                self.load("rcx", lhs);
                self.load("rdx", rhs);
                if !matches!(rhs, Value::Imm(n) if n >= 0) {
                    let negative = self.fail_here("negative exponent");
                    self.emit("\ttest\trdx, rdx");
                    self.emit(&format!("\tjs\t{negative}"));
                }
                self.emit("\tmov\teax, 1");
                self.power_loop("imul\trax, rcx", "imul\trcx, rcx");
                self.store(dst, "rax");
            }

            Inst::Binary { op, dst, lhs, rhs } => {
                let (dst, mut lhs, mut rhs) =
                    (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                // `a + b` is `b + a`, so the operand already in the destination can go first
                if matches!(op, BinOp::Add | BinOp::Mul)
                    && (rhs == dst || matches!(lhs, Value::Imm(_)))
                {
                    std::mem::swap(&mut lhs, &mut rhs);
                }
                let target = self.target(dst, Some(rhs));
                self.load(target, lhs);
                match (op, rhs) {
                    (BinOp::Mul, Value::Imm(n)) if fits_i32(n) => {
                        self.emit(&format!("\timul\t{target}, {target}, {n}"))
                    }
                    _ => {
                        let name = match op {
                            BinOp::Add => "add",
                            BinOp::Sub => "sub",
                            BinOp::Mul => "imul",
                            BinOp::Div | BinOp::Rem | BinOp::Pow => {
                                return Err(format!("{op:?} is emitted separately"));
                            }
                        };
                        let rhs = self.source(rhs, "rcx");
                        self.emit(&format!("\t{name}\t{target}, {rhs}"));
                    }
                }
                self.store(dst, target);
            }

            Inst::FloatBinary {
                op: BinOp::Rem,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                self.load_xmm("xmm1", rhs);
                self.fail_on_float_zero("xmm1");
                // x87's fprem gives the exact remainder with the dividend's sign, as Rust's `%`
                // does, but only reduces the exponent by up to 63 per step, so repeat it until
                // C2 (bit 2 of ah) says the remainder is complete
                self.load("rcx", rhs);
                self.load("rdx", lhs);
                self.emit("\tpush\trcx");
                self.emit("\tpush\trdx");
                self.emit("\tfld\tQWORD PTR [rsp + 8]");
                self.emit("\tfld\tQWORD PTR [rsp]");
                let reduce = self.ctx.new_label("fprem");
                self.emit(&format!("{reduce}:"));
                self.emit("\tfprem");
                self.emit("\tfnstsw\tax");
                self.emit("\ttest\tah, 4");
                self.emit(&format!("\tjnz\t{reduce}"));
                // pop both x87 registers, leaving its stack empty
                self.emit("\tfstp\tQWORD PTR [rsp]");
                self.emit("\tfstp\tst(0)");
                self.emit("\tpop\trax");
                self.emit("\tadd\trsp, 8");
                self.store(dst, "rax");
            }

            Inst::FloatBinary {
                op: BinOp::Pow,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                // the same operations as `stdlib::float_pow`: square and multiply by the
                // exponent's magnitude, then take the reciprocal if it was negative
                self.load_xmm("xmm1", lhs);
                self.emit(&format!("\tmov\trax, {}", 1.0f64.to_bits() as i64));
                self.emit("\tmovq\txmm0, rax");
                let negative = match rhs {
                    Value::Imm(n) => {
                        // the magnitude of the minimum is 2^63, which shr reads correctly
                        self.load("rdx", Value::Imm(n.unsigned_abs() as i64));
                        Some(n < 0)
                    }
                    _ => {
                        self.load("rdx", rhs);
                        self.emit("\tmov\trcx, rdx");
                        let positive = self.ctx.new_label("positive");
                        self.emit("\ttest\trdx, rdx");
                        self.emit(&format!("\tjns\t{positive}"));
                        self.emit("\tneg\trdx");
                        self.emit(&format!("{positive}:"));
                        None
                    }
                };
                self.power_loop("mulsd\txmm0, xmm1", "mulsd\txmm1, xmm1");
                let reciprocal = |r#gen: &mut Self| {
                    // rax still holds 1.0
                    r#gen.emit("\tmovq\txmm1, rax");
                    r#gen.emit("\tdivsd\txmm1, xmm0");
                    r#gen.emit("\tmovapd\txmm0, xmm1");
                };
                match negative {
                    Some(true) => reciprocal(self),
                    Some(false) => {}
                    None => {
                        let done = self.ctx.new_label("powered");
                        self.emit("\ttest\trcx, rcx");
                        self.emit(&format!("\tjns\t{done}"));
                        reciprocal(self);
                        self.emit(&format!("{done}:"));
                    }
                }
                self.emit(&format!("\tmovq\t{dst}, xmm0"));
            }

            Inst::FloatBinary { op, dst, lhs, rhs } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                self.load_xmm("xmm0", lhs);
                self.load_xmm("xmm1", rhs);
                let instruction = match op {
                    BinOp::Add => "addsd",
                    BinOp::Sub => "subsd",
                    BinOp::Mul => "mulsd",
                    BinOp::Div => {
                        self.fail_on_float_zero("xmm1");
                        "divsd"
                    }
                    BinOp::Rem | BinOp::Pow => {
                        return Err(format!("float {op:?} is emitted separately"));
                    }
                };
                self.emit(&format!("\t{instruction}\txmm0, xmm1"));
                self.emit(&format!("\tmovq\t{dst}, xmm0"));
            }

            Inst::Neg { dst, src } | Inst::FloatNeg { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                let target = self.target(dst, None);
                self.load(target, src);
                if matches!(inst, Inst::Neg { .. }) {
                    self.emit(&format!("\tneg\t{target}"));
                } else {
                    // flip the sign bit, which also negates 0.0 and nan like the interpreter
                    self.emit(&format!("\tbtc\t{target}, 63"));
                }
                self.store(dst, target);
            }

            Inst::FloatSqrt { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.load_xmm("xmm0", src);
                self.emit("\tsqrtsd\txmm0, xmm0");
                self.emit(&format!("\tmovq\t{dst}, xmm0"));
            }

            Inst::Not { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.load("rax", src);
                self.emit("\ttest\trax, rax");
                self.emit("\tsetz\tal");
                self.emit("\tmovzx\trax, al");
                self.store(dst, "rax");
            }

            Inst::Compare {
                cond,
                float,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                if *float {
                    self.load_xmm("xmm0", lhs);
                    self.load_xmm("xmm1", rhs);
                    let (yes, _) = self.compare_floats(*cond);
                    self.emit(&format!("\tset{yes}\tal"));
                    match cond {
                        Cond::Eq => {
                            self.emit("\tsetnp\tcl");
                            self.emit("\tand\tal, cl");
                        }
                        Cond::Ne => {
                            self.emit("\tsetp\tcl");
                            self.emit("\tor\tal, cl");
                        }
                        _ => {}
                    }
                } else {
                    self.compare_ints(lhs, rhs);
                    self.emit(&format!("\tset{}\tal", cc(*cond)));
                }
                self.emit("\tmovzx\trax, al");
                self.store(dst, "rax");
            }

            Inst::LoadGlobal { dst, name, checked } => {
                let dst = frame.reg(*dst)?;
                let label = global_label(name);
                if *checked {
                    // a tuple's values are its own globals, all assigned together
                    let name = variable_of(name);
                    let fail = self.fail_here(&format!("'{name}' is used before it is assigned"));
                    self.emit(&format!("\tcmp\tQWORD PTR [rip + {label}.set], 0"));
                    self.emit(&format!("\tje\t{fail}"));
                }
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [rip + {label}]"));
                self.store(dst, target);
            }

            Inst::StoreGlobal { name, src } => {
                let label = global_label(name);
                match frame.value(*src)? {
                    Value::Reg(reg) => {
                        self.emit(&format!("\tmov\tQWORD PTR [rip + {label}], {reg}"))
                    }
                    Value::Imm(n) if fits_i32(n) => {
                        self.emit(&format!("\tmov\tQWORD PTR [rip + {label}], {n}"))
                    }
                    value => {
                        self.load("rax", value);
                        self.emit(&format!("\tmov\tQWORD PTR [rip + {label}], rax"));
                    }
                }
                self.emit(&format!("\tmov\tQWORD PTR [rip + {label}.set], 1"));
            }

            Inst::StrAddr { dst, text } => {
                let dst = frame.reg(*dst)?;
                let label = self.ctx.intern_string(text);
                let target = self.target(dst, None);
                self.emit(&format!("\tlea\t{target}, [rip + {label}]"));
                self.store(dst, target);
            }

            Inst::EnumTag { dst, value } => {
                let (dst, value) = (frame.reg(*dst)?, frame.value(*value)?);
                let base = self.base(value, "rax");
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [{base}]"));
                self.store(dst, target);
            }

            Inst::FieldGet { dst, value, slot } => {
                let (dst, value) = (frame.reg(*dst)?, frame.value(*value)?);
                let base = self.base(value, "rax");
                let target = self.target(dst, None);
                let offset = FIELDS_OFFSET + 8 * *slot as i64;
                self.emit(&format!("\tmov\t{target}, QWORD PTR [{base} + {offset}]"));
                self.store(dst, target);
            }

            Inst::FieldInit { value, slot, src } => {
                let (value, src) = (frame.value(*value)?, frame.value(*src)?);
                let base = self.base(value, "rax");
                let offset = FIELDS_OFFSET + 8 * *slot as i64;
                self.store_qword(&format!("[{base} + {offset}]"), src);
            }

            Inst::VariantStatic { dst, tag } => {
                let dst = frame.reg(*dst)?;
                let target = self.target(dst, None);
                self.emit(&format!("\tlea\t{target}, [rip + {}]", variant_label(*tag)));
                self.store(dst, target);
            }

            Inst::FunctionAddress { dst, label } => {
                let dst = frame.reg(*dst)?;
                let target = self.target(dst, None);
                self.emit(&format!("\tlea\t{target}, [rip + {label}]"));
                self.store(dst, target);
            }

            Inst::ListLen { dst, list } => {
                let (dst, list) = (frame.reg(*dst)?, frame.value(*list)?);
                let base = self.base(list, "rax");
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [{base}]"));
                self.store(dst, target);
            }

            Inst::ListLoad { dst, list, index } => {
                let (dst, list, index) =
                    (frame.reg(*dst)?, frame.value(*list)?, frame.value(*index)?);
                let slot = self.list_slot(list, index, true);
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR {slot}"));
                self.store(dst, target);
            }

            Inst::ListGet {
                dst,
                list,
                index,
                offset,
            } => {
                let (dst, list, index) =
                    (frame.reg(*dst)?, frame.value(*list)?, frame.value(*index)?);
                let slot = self.known_slot(list, index, *offset);
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR {slot}"));
                self.store(dst, target);
            }

            Inst::ListStore {
                old,
                list,
                index,
                value,
            } => {
                let (list, index, value) = (
                    frame.value(*list)?,
                    frame.value(*index)?,
                    frame.value(*value)?,
                );
                let slot = self.list_slot(list, index, true);
                // the slot needs rax and rcx and the value may need rdx, so the old element waits
                // in xmm0, since `old` may share a register with `value`
                if old.is_some() {
                    self.emit(&format!("\tmovq\txmm0, QWORD PTR {slot}"));
                }
                self.store_qword(&slot, value);
                if let Some(old) = old {
                    let old = frame.reg(*old)?;
                    self.emit(&format!("\tmovq\t{old}, xmm0"));
                }
            }

            Inst::ListSet {
                list,
                index,
                value,
                offset,
            } => {
                let (list, index, value) = (
                    frame.value(*list)?,
                    frame.value(*index)?,
                    frame.value(*value)?,
                );
                let slot = self.known_slot(list, index, *offset);
                self.store_qword(&slot, value);
            }

            Inst::Retain { src } => match frame.value(*src)? {
                // none, or a variable never assigned
                Value::Imm(0) => {}
                Value::Imm(_) => return Err("only a pointer can be retained".to_string()),
                value => {
                    let base = self.base(value, "rax");
                    let skip = self.ctx.new_label("retained");
                    self.emit(&format!("\ttest\t{base}, {base}"));
                    self.emit(&format!("\tjz\t{skip}"));
                    self.emit(&format!("\tinc\tQWORD PTR [{base} - 8]"));
                    self.emit(&format!("{skip}:"));
                }
            },

            Inst::Release { src, kind } => match frame.value(*src)? {
                Value::Imm(0) => {}
                Value::Imm(_) => return Err("only a pointer can be released".to_string()),
                value => {
                    let base = self.base(value, "rax");
                    let skip = self.ctx.new_label("released");
                    self.emit(&format!("\ttest\t{base}, {base}"));
                    self.emit(&format!("\tjz\t{skip}"));
                    self.emit(&format!("\tdec\tQWORD PTR [{base} - 8]"));
                    self.emit(&format!("\tjnz\t{skip}"));
                    // the last reference is gone, and the free routines take it in rax
                    self.load("rax", Value::Reg(base));
                    let routine = match kind {
                        RcKind::Str => "stone.free_str",
                        RcKind::List => "stone.free_list",
                        RcKind::Enum => "stone.free_enum",
                    };
                    self.emit(&format!("\tcall\t{routine}"));
                    self.emit(&format!("{skip}:"));
                }
            },

            Inst::ListUnique {
                dst,
                src,
                maybe_null,
            } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                // a list nothing else refers to is changed in place, and any other is copied
                // first by `stone.list_copy`, which takes and returns it in rax
                self.load("rax", src);
                let skip = self.ctx.new_label("unique");
                if *maybe_null {
                    self.emit("\ttest\trax, rax");
                    self.emit(&format!("\tjz\t{skip}"));
                }
                self.emit("\tcmp\tQWORD PTR [rax - 8], 1");
                self.emit(&format!("\tje\t{skip}"));
                self.emit("\tcall\tstone.list_copy");
                self.emit(&format!("{skip}:"));
                self.store(dst, "rax");
            }

            Inst::ListInit { list, index, value } => {
                let (list, value) = (frame.value(*list)?, frame.value(*value)?);
                let slot = self.list_slot(list, Value::Imm(*index as i64), false);
                self.store_qword(&slot, value);
            }

            Inst::IntToFloat { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                let src = match src {
                    Value::Imm(_) => {
                        self.load("rax", src);
                        "rax".to_string()
                    }
                    _ => src.to_string(),
                };
                self.emit(&format!("\tcvtsi2sd\txmm0, {src}"));
                self.emit(&format!("\tmovq\t{dst}, xmm0"));
            }

            Inst::FloatToInt { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.load("rax", src);
                self.gen_float_to_int();
                self.store(dst, "rax");
            }

            Inst::Call { dsts, callee, args } => {
                let extra_results = dsts.len().saturating_sub(RESULT_REGS.len());
                if dsts.len() > 1 && !matches!(callee, Callee::User(_) | Callee::Value(_)) {
                    return Err(format!("{callee} returns one value"));
                }
                if let Callee::User(_) | Callee::Value(_) = callee {
                    self.depth_check();
                }
                let mut values = Vec::new();
                for arg in args {
                    values.push(frame.value(*arg)?);
                }
                // room for the results past the registers, above the arguments
                if extra_results > 0 {
                    self.emit(&format!("\tsub\trsp, {}", 8 * extra_results));
                }
                // arguments past the sixth go on the stack, the first deepest
                let extra = values.len().saturating_sub(ARG_REGS.len());
                for value in values.iter().skip(ARG_REGS.len()) {
                    match value {
                        Value::Imm(n) if !fits_i32(*n) => {
                            self.load("rax", *value);
                            self.emit("\tpush\trax");
                        }
                        Value::Imm(n) => self.emit(&format!("\tpush\t{n}")),
                        _ => self.emit(&format!("\tpush\t{value}")),
                    }
                }
                let mut moves: Vec<(Value, Value)> = ARG_REGS
                    .iter()
                    .zip(&values)
                    .map(|(reg, value)| (Value::Reg(reg), *value))
                    .collect();
                // a function value is called through r11, which holds no argument
                if let Callee::Value(function) = callee {
                    moves.push((Value::Reg("r11"), frame.value(*function)?));
                }
                for (to, from) in parallel_moves(&moves, Value::Reg("rax")) {
                    self.mov(to, from);
                }
                let label = match callee {
                    Callee::User(name) => function_label(name),
                    Callee::Runtime(label) => label.to_string(),
                    Callee::Print(ty) => self.ctx.print_routine(ty, false)?,
                    Callee::Helper(label) => label.clone(),
                    Callee::Value(_) => "r11".to_string(),
                };
                // the routine's own runtime errors point at this call
                if (FALLIBLE_ROUTINES.contains(&label.as_str())
                    || matches!(callee, Callee::Helper(_)))
                    && let Some(site) = self.ctx.site_label(self.span)
                {
                    self.record_site(&site);
                }
                self.emit(&format!("\tcall\t{label}"));
                if extra > 0 {
                    self.emit(&format!("\tadd\trsp, {}", 8 * extra));
                }
                for (dst, reg) in dsts.iter().zip(RESULT_REGS) {
                    self.store(frame.reg(*dst)?, reg);
                }
                for (j, dst) in dsts.iter().enumerate().skip(RESULT_REGS.len()) {
                    let offset = 8 * (j - RESULT_REGS.len());
                    self.emit(&format!("\tmov\trax, QWORD PTR [rsp + {offset}]"));
                    self.store(frame.reg(*dst)?, "rax");
                }
                if extra_results > 0 {
                    self.emit(&format!("\tadd\trsp, {}", 8 * extra_results));
                }
            }
        }
        Ok(())
    }

    /// Writes `value` to the memory operand `slot`, through `rdx` if it is in memory itself or
    /// too wide an immediate.
    fn store_qword(&mut self, slot: &str, value: Value) {
        match value {
            Value::Reg(reg) => self.emit(&format!("\tmov\tQWORD PTR {slot}, {reg}")),
            Value::Imm(n) if fits_i32(n) => self.emit(&format!("\tmov\tQWORD PTR {slot}, {n}")),
            _ => {
                self.load("rdx", value);
                self.emit(&format!("\tmov\tQWORD PTR {slot}, rdx"));
            }
        }
    }

    /// Returns the memory operand for element `index` of `list`, without its size prefix.
    ///
    /// When `checked`, a negative index counts from the end, and an index still out of range
    /// exits with an error. A list is a pointer to a `{len, cap, data, kinds, width}` header (see
    /// [`super::builtins::list_runtime`]), so for example `xs[-1]` with `xs` of length 3 checks
    /// that `-1 + 3` is below 3 and returns `[rax + rcx * 8]` with `rax` holding `data` and `rcx`
    /// holding 2. This uses `rax` and `rcx`.
    fn list_slot(&mut self, list: Value, index: Value, checked: bool) -> String {
        let base = self.base(list, "rax");
        let constant = match index {
            Value::Imm(n) if (0..1 << 28).contains(&n) => Some(n),
            _ => None,
        };
        if checked {
            // the error reports the index as written and the length, in FAIL_VALUES
            let mut setup = vec![format!("\tmov\trdx, QWORD PTR [{base}]")];
            match constant {
                Some(n) => setup.push(format!("\tmov\trcx, {n}")),
                None => {
                    // a negative index was counted from the end, and only a negative one is
                    // still negative after that, so the length comes off again
                    setup.push("\tmov\trax, rcx".to_string());
                    setup.push("\tsar\trax, 63".to_string());
                    setup.push("\tand\trax, rdx".to_string());
                    setup.push("\tsub\trcx, rax".to_string());
                }
            }
            let out_of_range = self.ctx.fail_at_after(INDEX_RANGE, self.span, setup);
            match constant {
                Some(n) => {
                    // the length is never negative, so a constant index is in range below it
                    self.emit(&format!("\tcmp\tQWORD PTR [{base}], {n}"));
                    self.emit(&format!("\tjle\t{out_of_range}"));
                }
                None => {
                    let check = self.ctx.new_label("index_check");
                    self.load("rcx", index);
                    self.emit("\ttest\trcx, rcx");
                    self.emit(&format!("\tjns\t{check}"));
                    // negative indexes count from the end
                    self.emit(&format!("\tadd\trcx, QWORD PTR [{base}]"));
                    self.emit(&format!("{check}:"));
                    // unsigned, so an index still negative after adjusting is out of range too
                    self.emit(&format!("\tcmp\trcx, QWORD PTR [{base}]"));
                    self.emit(&format!("\tjae\t{out_of_range}"));
                }
            }
        } else if constant.is_none() {
            self.load("rcx", index);
        }
        self.emit(&format!("\tmov\trax, QWORD PTR [{base} + 16]"));
        match constant {
            Some(n) => format!("[rax + {}]", 8 * n),
            None => "[rax + rcx * 8]".to_string(),
        }
    }

    /// Jumps to the `recursion is too deep` failure if a call made here would nest more calls
    /// than the interpreter allows, rather than overflowing the stack.
    fn depth_check(&mut self) {
        let too_deep = self.fail_here(&format!(
            "recursion is too deep (more than {MAX_CALL_DEPTH} nested calls)"
        ));
        self.emit(&format!(
            "\tcmp\tQWORD PTR [rip + stone.call_depth], {MAX_CALL_DEPTH}"
        ));
        self.emit(&format!("\tjge\t{too_deep}"));
    }

    /// Leaves in `rcx` the slot where element `index` of `list` starts, whose elements are
    /// `width` slots each, counting a negative index from the end and jumping to the
    /// `INDEX_RANGE` failure unless that is an element. This uses `rcx` and `rdx`, and `rax` for a
    /// list in memory.
    ///
    /// An index from 0 is checked against the slot count before it is multiplied, so the product
    /// cannot overflow, and then the slot it starts at is. A negative one must not be below minus
    /// the slot count, and then the slot count plus the index times the width must not be
    /// negative. For example, with 7 slots per element and 14 slots, index 1 starts at slot 7 and
    /// index -3 fails.
    fn element_slot(&mut self, list: Value, index: Value, width: i64) {
        let base = self.base(list, "rax");
        // the error reports the index as written and the number of elements, in FAIL_VALUES, and
        // stone.fail never returns, so its setup may use any register
        let mut setup = vec![format!("\tmov\trax, QWORD PTR [{base}]")];
        if width > 1 {
            setup.push("\tcqo".to_string());
            setup.push(format!("\tmov\trcx, {width}"));
            setup.push("\tidiv\trcx".to_string());
        }
        setup.push("\tmov\trdx, rax".to_string());
        setup.push(format!("\tmov\trcx, {index}"));
        let out_of_range = self.ctx.fail_at_after(INDEX_RANGE, self.span, setup);
        let done = self.ctx.new_label("slot_done");
        self.load("rcx", index);
        self.emit(&format!("\tmov\trdx, QWORD PTR [{base}]"));
        if !matches!(index, Value::Imm(n) if n >= 0) {
            let from_start = self.ctx.new_label("slot_from_start");
            self.emit("\ttest\trcx, rcx");
            self.emit(&format!("\tjns\t{from_start}"));
            // negative indexes count from the end
            self.emit("\tadd\trcx, rdx");
            self.emit(&format!("\tjs\t{out_of_range}"));
            if width > 1 {
                self.emit("\tsub\trcx, rdx");
                self.emit(&format!("\timul\trcx, rcx, {width}"));
                self.emit("\tadd\trcx, rdx");
                self.emit(&format!("\tjs\t{out_of_range}"));
            }
            self.emit(&format!("\tjmp\t{done}"));
            self.emit(&format!("{from_start}:"));
        }
        self.emit("\tcmp\trcx, rdx");
        self.emit(&format!("\tjge\t{out_of_range}"));
        if width > 1 {
            self.emit(&format!("\timul\trcx, rcx, {width}"));
            self.emit("\tcmp\trcx, rdx");
            self.emit(&format!("\tjge\t{out_of_range}"));
        }
        self.emit(&format!("{done}:"));
    }

    /// Returns the memory operand for slot `index + offset` of `list`, already known to be in
    /// range, without its size prefix, using an index in a register as it is. This uses `rax`,
    /// and `rcx` for an index in memory.
    ///
    /// For example, slot `r12 + 3` of a list in `rbx` is `[rax + r12 * 8 + 24]` after
    /// `mov rax, QWORD PTR [rbx + 16]`.
    fn known_slot(&mut self, list: Value, index: Value, offset: usize) -> String {
        let base = self.base(list, "rax");
        let index = match index {
            Value::Imm(n) => {
                self.emit(&format!("\tmov\trax, QWORD PTR [{base} + 16]"));
                return format!("[rax + {}]", 8 * (n + offset as i64));
            }
            Value::Reg(reg) => reg,
            Value::Stack(_) => {
                self.load("rcx", index);
                "rcx"
            }
        };
        self.emit(&format!("\tmov\trax, QWORD PTR [{base} + 16]"));
        match offset {
            0 => format!("[rax + {index} * 8]"),
            n => format!("[rax + {index} * 8 + {}]", 8 * n),
        }
    }

    /// Jumps to the `division by zero` failure if the float divisor in `xmm` is 0.0 or -0.0, as
    /// in the interpreter, rather than letting `/` or `%` give inf or nan. Clobbers `xmm2`.
    fn fail_on_float_zero(&mut self, xmm: &str) {
        let by_zero = self.fail_here("division by zero");
        let nonzero = self.ctx.new_label("nonzero");
        self.emit("\txorpd\txmm2, xmm2");
        self.emit(&format!("\tucomisd\t{xmm}, xmm2"));
        // a nan divisor also sets ZF, but PF tells it apart
        self.emit(&format!("\tjp\t{nonzero}"));
        self.emit(&format!("\tje\t{by_zero}"));
        self.emit(&format!("{nonzero}:"));
    }

    /// Emits the loop that raises a base to the exponent in `rdx` by squaring and multiplying,
    /// as `stdlib::int_pow` and `stdlib::float_pow` do. `multiply` folds the base into the
    /// result and `square` squares the base, and `rdx` ends at 0.
    ///
    /// For example, `power_loop("imul\trax, rcx", "imul\trcx, rcx")` leaves `rcx` to the power
    /// `rdx` in `rax`, if `rax` started at 1.
    fn power_loop(&mut self, multiply: &str, square: &str) {
        let top = self.ctx.new_label("pow");
        let skip = self.ctx.new_label("pow_skip");
        let done = self.ctx.new_label("pow_done");
        self.emit(&format!("{top}:"));
        self.emit("\ttest\trdx, rdx");
        self.emit(&format!("\tjz\t{done}"));
        self.emit("\ttest\tdl, 1");
        self.emit(&format!("\tjz\t{skip}"));
        self.emit(&format!("\t{multiply}"));
        self.emit(&format!("{skip}:"));
        self.emit(&format!("\t{square}"));
        self.emit("\tshr\trdx, 1");
        self.emit(&format!("\tjmp\t{top}"));
        self.emit(&format!("{done}:"));
    }

    /// Divides `rax` by a constant that is neither 0 nor -1, rounding toward zero like `idiv`.
    ///
    /// Neither `idiv` trap can happen for such a divisor, so no checks are emitted. A power of
    /// two becomes shifts, and any other divisor a multiplication by its magic number
    /// (`magic_quotient`). For example, dividing by `8` emits a bias for negative dividends and
    /// then `sar rax, 3`, and dividing by `10` emits `imul rdx` and shifts. Uses `rcx` and `rdx`.
    fn divide_by_constant(&mut self, divisor: i64) {
        let magnitude = divisor.unsigned_abs();
        if !magnitude.is_power_of_two() {
            self.magic_quotient(divisor);
            return;
        }

        let shift = magnitude.trailing_zeros();
        if shift > 0 {
            // an arithmetic shift rounds down, so first add 2^shift - 1 to a negative dividend
            self.emit("\tmov\trdx, rax");
            self.emit("\tsar\trdx, 63"); // all ones if negative
            self.emit(&format!("\tshr\trdx, {}", 64 - shift));
            self.emit("\tadd\trax, rdx");
            self.emit(&format!("\tsar\trax, {shift}"));
        }
        if divisor < 0 {
            // the quotient is never the minimum here, so negating it cannot overflow
            self.emit("\tneg\trax");
        }
    }

    /// Leaves in `rdx` the remainder of `rax` divided by a constant that is neither 0 nor -1,
    /// taking the dividend's sign like `idiv`.
    ///
    /// A power of two becomes shifts: the quotient rounded toward zero, as `divide_by_constant`
    /// computes it, shifted back and subtracted. The sign of the divisor never matters. For
    /// example, `x % 8` emits `sar rcx, 3`, `shl rcx, 3`, and `sub rdx, rcx`, and `x % 10`
    /// subtracts ten times the quotient `magic_quotient` gives.
    fn remainder_by_constant(&mut self, divisor: i64) {
        let magnitude = divisor.unsigned_abs();
        if !magnitude.is_power_of_two() {
            self.magic_quotient(divisor);
            // the product is no bigger than the dividend, so it cannot overflow
            if fits_i32(divisor) {
                self.emit(&format!("\timul\trax, rax, {divisor}"));
            } else {
                self.emit(&format!("\tmov\trdx, {divisor}"));
                self.emit("\timul\trax, rdx");
            }
            self.emit("\tmov\trdx, rcx");
            self.emit("\tsub\trdx, rax");
            return;
        }

        let shift = magnitude.trailing_zeros();
        if shift == 0 {
            self.emit("\txor\tedx, edx");
            return;
        }
        // an arithmetic shift rounds down, so first add 2^shift - 1 to a negative dividend
        self.emit("\tmov\trcx, rax");
        self.emit("\tsar\trcx, 63"); // all ones if negative
        self.emit(&format!("\tshr\trcx, {}", 64 - shift));
        self.emit("\tadd\trcx, rax");
        self.emit(&format!("\tsar\trcx, {shift}"));
        self.emit(&format!("\tshl\trcx, {shift}"));
        self.emit("\tmov\trdx, rax");
        self.emit("\tsub\trdx, rcx");
    }

    /// Divides `rax` by a constant whose magnitude is at least 3 and not a power of two, rounding
    /// toward zero like `idiv`, by multiplying by the divisor's magic number from
    /// [`magic_signed`]. Leaves the quotient in `rax` and the dividend in `rcx`, and uses `rdx`.
    ///
    /// For example, dividing by `7` takes the high half of the product with
    /// `5270498306774157605`, shifts it right by 1, and adds 1 if that is negative.
    fn magic_quotient(&mut self, divisor: i64) {
        let (multiplier, shift) = magic_signed(divisor);
        self.emit("\tmov\trcx, rax");
        self.emit(&format!("\tmov\trdx, {multiplier}"));
        self.emit("\timul\trdx"); // the high half of the product goes to rdx
        if divisor > 0 && multiplier < 0 {
            self.emit("\tadd\trdx, rcx");
        } else if divisor < 0 && multiplier > 0 {
            self.emit("\tsub\trdx, rcx");
        }
        if shift > 0 {
            self.emit(&format!("\tsar\trdx, {shift}"));
        }
        // a negative quotient rounds toward zero
        self.emit("\tmov\trax, rdx");
        self.emit("\tshr\trax, 63");
        self.emit("\tadd\trax, rdx");
    }

    /// Converts the float whose bits are in `rax` to an int in `rax`, dropping any fraction.
    ///
    /// `cvttsd2si` gives the minimum int for nan and for anything out of range, so that result
    /// exits with an error unless the float really was -2^63.
    fn gen_float_to_int(&mut self) {
        let fail = self.fail_here("cannot convert float to int (nan or out of range)");
        let done = self.ctx.new_label("to_int");
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
}
