//! Instruction selection from register-allocated [`ir`](crate::codegen::ir) to x86-64.
//!
//! Each function's vregs are assigned by [`linear_scan`] to [`CALLEE_SAVED`] or [`CALLER_SAVED`]
//! registers, or to spill slots below the saved registers in its frame. `rax`, `rcx`, `rdx`, and `xmm0` to `xmm2` are
//! never allocated: instructions use them as scratch, such as `rdx:rax` for `idiv`, and an operand
//! that is in memory or too wide for an immediate is loaded into one first. For example,
//! `v2 = add v0, v1` with `v0` in `rbx`, `v1` in `r12`, and `v2` in `r13` becomes
//! `mov r13, rbx` then `add r13, r12`.

use super::{ARG_REGS, X64Generator};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{function_label, global_label};
use crate::codegen::ir::liveness::analyze;
use crate::codegen::ir::{
    BinOp, BlockId, Callee, Cond, Function, Inst, Operand, RcKind, Terminator, VReg,
};
use crate::codegen::regalloc::{Location, hints, linear_scan, parallel_moves};
use crate::stdlib::MAX_CALL_DEPTH;
use std::collections::HashMap;
use std::fmt;

/// Registers that calls preserve, which are the only ones that can hold a value across a call.
const CALLEE_SAVED: [&str; 5] = ["rbx", "r12", "r13", "r14", "r15"];

/// Registers that calls clobber, which hold values that do not live across one.
const CALLER_SAVED: [&str; 6] = ["rsi", "rdi", "r8", "r9", "r10", "r11"];

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
        };

        match &function.name {
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
        for reg in &allocation.used_callee_saved {
            self.emit(&format!("\tpush\t{reg}"));
        }
        if allocation.spill_slots > 0 {
            self.emit(&format!("\tsub\trsp, {}", 8 * allocation.spill_slots));
        }

        if !frame.is_main {
            // the same limit as the interpreter, rather than overflowing the stack
            let too_deep = self.ctx.fail_label(&format!(
                "recursion is too deep (more than {MAX_CALL_DEPTH} nested calls)"
            ));
            self.emit("\tinc\tQWORD PTR [rip + stone.call_depth]");
            self.emit(&format!(
                "\tcmp\tQWORD PTR [rip + stone.call_depth], {MAX_CALL_DEPTH}"
            ));
            self.emit(&format!("\tjg\t{too_deep}"));
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
            for inst in &block.insts {
                self.emit_inst(&frame, inst)?;
            }
            self.emit_terminator(&frame, &block.term, BlockId(i + 1))?;
        }

        self.emit(&format!("{}:", frame.epilogue));
        if frame.is_main && self.ctx.counts_references {
            self.emit("\tcall\tstone.leak_check");
        }
        if !frame.is_main {
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
            Terminator::Return(value) => {
                match value.map(|v| frame.value(v)).transpose()? {
                    None | Some(Value::Imm(0)) => self.emit("\txor\trax, rax"),
                    Some(value) => self.load("rax", value),
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
                            self.emit(&format!("\tmov\trcx, {divisor}"));
                            self.emit("\tcqo"); // sign-extend rax into rdx
                            self.emit("\tidiv\trcx");
                        }
                    }
                    _ => {
                        self.load("rcx", rhs);
                        // idiv traps on both of these, so report them like the interpreter
                        let by_zero = self.ctx.fail_label("division by zero");
                        let divide = self.ctx.new_label("divide");
                        let done = self.ctx.new_label("divided");
                        self.emit("\ttest\trcx, rcx");
                        self.emit(&format!("\tjz\t{by_zero}"));
                        self.emit("\tcmp\trcx, -1");
                        self.emit(&format!("\tjne\t{divide}"));
                        if *op == BinOp::Div {
                            let overflow = self.ctx.fail_label("integer overflow in division");
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
                    let negative = self.ctx.fail_label("negative exponent");
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
                    let fail = self
                        .ctx
                        .fail_label(&format!("'{name}' is used before it is assigned"));
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

            Inst::ListLen { dst, list } => {
                let (dst, list) = (frame.reg(*dst)?, frame.value(*list)?);
                let base = self.base(list, "rax");
                let target = self.target(dst, None);
                self.emit(&format!("\tmov\t{target}, QWORD PTR [{base}]"));
                self.store(dst, target);
            }

            Inst::ListLoad { dst, list, index } | Inst::ListGet { dst, list, index } => {
                let checked = matches!(inst, Inst::ListLoad { .. });
                let (dst, list, index) =
                    (frame.reg(*dst)?, frame.value(*list)?, frame.value(*index)?);
                let slot = self.list_slot(list, index, checked);
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
                    };
                    self.emit(&format!("\tcall\t{routine}"));
                    self.emit(&format!("{skip}:"));
                }
            },

            Inst::ListUnique { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                // a list nothing else refers to is changed in place, and any other is copied
                // first by `stone.list_copy`, which takes and returns it in rax
                self.load("rax", src);
                let skip = self.ctx.new_label("unique");
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

            Inst::Call { dst, callee, args } => {
                let mut values = Vec::new();
                for arg in args {
                    values.push(frame.value(*arg)?);
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
                let moves: Vec<(Value, Value)> = ARG_REGS
                    .iter()
                    .zip(&values)
                    .map(|(reg, value)| (Value::Reg(reg), *value))
                    .collect();
                for (to, from) in parallel_moves(&moves, Value::Reg("rax")) {
                    self.mov(to, from);
                }
                let label = match callee {
                    Callee::User(name) => function_label(name),
                    Callee::Runtime(label) => label.to_string(),
                    Callee::Print(ty) => self.ctx.print_routine(ty, false)?,
                };
                self.emit(&format!("\tcall\t{label}"));
                if extra > 0 {
                    self.emit(&format!("\tadd\trsp, {}", 8 * extra));
                }
                if let Some(dst) = dst {
                    self.store(frame.reg(*dst)?, "rax");
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
    /// exits with an error. A list is a pointer to a `{len, cap, data}` header (see
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
            let out_of_range = self.ctx.fail_label("list index out of range");
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

    /// Jumps to the `division by zero` failure if the float divisor in `xmm` is 0.0 or -0.0, as
    /// in the interpreter, rather than letting `/` or `%` give inf or nan. Clobbers `xmm2`.
    fn fail_on_float_zero(&mut self, xmm: &str) {
        let by_zero = self.ctx.fail_label("division by zero");
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
    /// Neither `idiv` trap can happen for such a divisor, so no checks are emitted, and a power
    /// of two becomes shifts. For example, dividing by `8` emits a bias for negative dividends
    /// and then `sar rax, 3`, and dividing by `10` emits a plain `idiv`.
    fn divide_by_constant(&mut self, divisor: i64) {
        let magnitude = divisor.unsigned_abs();
        if !magnitude.is_power_of_two() {
            self.emit(&format!("\tmov\trcx, {divisor}"));
            self.emit("\tcqo"); // sign-extend rax into rdx
            self.emit("\tidiv\trcx");
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

    /// Converts the float whose bits are in `rax` to an int in `rax`, dropping any fraction.
    ///
    /// `cvttsd2si` gives the minimum int for nan and for anything out of range, so that result
    /// exits with an error unless the float really was -2^63.
    fn gen_float_to_int(&mut self) {
        let fail = self
            .ctx
            .fail_label("cannot convert float to int (nan or out of range)");
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
