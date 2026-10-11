//! Instruction selection from register-allocated [`ir`](crate::codegen::ir) to arm64.
//!
//! Each function's vregs are assigned by [`linear_scan`] to [`CALLEE_SAVED`] or [`CALLER_SAVED`]
//! registers, or to spill slots in its frame. `x8` to `x11`, `x16`, `x17`, and `d0` to `d2` are
//! never allocated: instructions use them as scratch, and an operand that is in memory or is an
//! immediate an instruction cannot encode is loaded into one first. For example,
//! `v2 = add v0, v1` with `v0` in `x19`, `v1` in `x20`, and `v2` in `x21` becomes
//! `add x21, x19, x20`.
//!
//! A frame holds, from `x29` down: the saved `x29` and `x30` (at `x29` itself), the callee-saved
//! registers the function uses, its spill slots, and the arguments past the eighth of any call it
//! makes, so `sp` never moves in the body and every slot is addressed from it.

use super::{ARG_REGS, Arm64Generator, pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{function_label, global_label};
use crate::codegen::ir::liveness::analyze;
use crate::codegen::ir::{
    BinOp, BlockId, Callee, Cond, Function, Inst, Operand, RcKind, Terminator, VReg,
};
use crate::codegen::regalloc::{Location, hints, linear_scan, parallel_moves};
use crate::stdlib::MAX_CALL_DEPTH;
use std::collections::HashMap;

/// Registers that calls preserve, which are the only ones that can hold a value across a call.
const CALLEE_SAVED: [&str; 10] = [
    "x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28",
];

/// Registers that calls clobber, which hold values that do not live across one. The free
/// routines and `stone.fmod` preserve all of them (see `builtins.rs`).
pub(super) const CALLER_SAVED: [&str; 12] = [
    "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x12", "x13", "x14", "x15",
];

/// The largest offset an 8-byte `ldr` or `str` can add to its base register.
const MAX_SCALED_OFFSET: i64 = 8 * 4095;

/// Where a value is while generating code: a register, a slot in the frame, or an immediate.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Value {
    Reg(&'static str),
    /// A spill slot at `[sp, #offset]`.
    Stack(i64),
    Imm(i64),
}

/// Returns whether `n` is an immediate that `add`, `sub`, and `cmp` can take: 12 bits, optionally
/// shifted left by 12.
///
/// For example, `4095` and `4096` fit, and `4097` does not.
fn fits_arith(n: i64) -> bool {
    (0..1 << 12).contains(&n) || (n & 0xfff == 0 && (0..1 << 24).contains(&n))
}

/// The condition suffix for a signed int comparison, as in `b.lt` and `cset x0, lt`.
fn cc(cond: Cond) -> &'static str {
    match cond {
        Cond::Eq => "eq",
        Cond::Ne => "ne",
        Cond::Lt => "lt",
        Cond::Le => "le",
        Cond::Gt => "gt",
        Cond::Ge => "ge",
    }
}

/// The condition suffixes that hold after `fcmp` when a float comparison does and does not hold.
///
/// Comparing with nan sets the flags to unordered, under which `mi`, `ls`, `gt`, `ge`, and `eq`
/// are false and `ne` is true, just as in the interpreter. For example, `float_cc(Cond::Lt)` is
/// `("mi", "pl")`.
fn float_cc(cond: Cond) -> (&'static str, &'static str) {
    match cond {
        Cond::Lt => ("mi", "pl"),
        Cond::Le => ("ls", "hi"),
        Cond::Gt => ("gt", "le"),
        Cond::Ge => ("ge", "lt"),
        Cond::Eq => ("eq", "ne"),
        Cond::Ne => ("ne", "eq"),
    }
}

/// A function being emitted: where each vreg lives, and its labels.
struct Frame {
    locations: HashMap<VReg, Location<&'static str>>,
    /// The bytes at the bottom of the frame for arguments past the eighth, below the spill slots.
    outgoing: i64,
    blocks: Vec<String>,
    epilogue: String,
}

impl Frame {
    fn value(&self, operand: Operand) -> Result<Value, String> {
        match operand {
            Operand::Imm(n) => Ok(Value::Imm(n)),
            Operand::Reg(reg) => match self.locations.get(&reg) {
                Some(Location::Reg(name)) => Ok(Value::Reg(name)),
                Some(Location::Stack(slot)) => Ok(Value::Stack(self.outgoing + 8 * *slot as i64)),
                None => Err(format!("{reg} was never allocated")),
            },
        }
    }

    fn reg(&self, reg: VReg) -> Result<Value, String> {
        self.value(Operand::Reg(reg))
    }
}

/// Rounds `bytes` up to the 16-byte alignment the stack pointer must keep.
///
/// For example, `align16(8)` is `16` and `align16(32)` is `32`.
fn align16(bytes: i64) -> i64 {
    (bytes + 15) & !15
}

impl Arm64Generator {
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

        let stack_args = function
            .blocks
            .iter()
            .flat_map(|block| &block.insts)
            .filter_map(|inst| match inst {
                Inst::Call { args, .. } => Some(args.len().saturating_sub(ARG_REGS.len())),
                _ => None,
            })
            .max()
            .unwrap_or(0);
        let outgoing = align16(8 * stack_args as i64);
        let locals = outgoing + align16(8 * allocation.spill_slots as i64);

        let blocks = (0..function.blocks.len())
            .map(|_| self.ctx.new_label("block"))
            .collect();
        let frame = Frame {
            locations: allocation.locations,
            outgoing,
            blocks,
            epilogue: self.ctx.new_label("return"),
        };
        let is_main = function.name.is_none();

        match &function.name {
            Some(name) => self.emit(&format!("{}:", function_label(name))),
            None => {
                self.emit("\t.globl\tmain");
                self.emit("main:");
            }
        }
        push_frame(self, &allocation.used_callee_saved);
        if locals > 0 {
            self.adjust_sp("sub", locals);
        }
        if is_main && self.ctx.uses(&["stone.args"]) {
            // argc is a C int, so only its low half is set
            self.emit("\tadrp\tx16, stone.argc");
            self.emit("\tstr\tw0, [x16, :lo12:stone.argc]");
            self.emit("\tadrp\tx16, stone.argv");
            self.emit("\tstr\tx1, [x16, :lo12:stone.argv]");
        }
        if is_main && self.ctx.needs_env() {
            self.emit("\tadrp\tx16, stone.envp");
            self.emit("\tstr\tx2, [x16, :lo12:stone.envp]");
        }

        if !is_main {
            // the same limit as the interpreter, rather than overflowing the stack
            let too_deep = self.ctx.fail_label(&format!(
                "recursion is too deep (more than {MAX_CALL_DEPTH} nested calls)"
            ));
            self.emit("\tadrp\tx16, stone.call_depth");
            self.emit("\tldr\tx17, [x16, :lo12:stone.call_depth]");
            self.emit("\tadd\tx17, x17, #1");
            self.emit("\tstr\tx17, [x16, :lo12:stone.call_depth]");
            self.compare_with(Value::Reg("x17"), MAX_CALL_DEPTH as i64);
            self.emit(&format!("\tb.gt\t{too_deep}"));
        }

        // arguments arrive in ARG_REGS and, past the eighth, at the bottom of the caller's frame
        let needed = |param: &VReg| liveness.live_at_entry[param.0 as usize];
        let mut moves = Vec::new();
        for (param, reg) in function.params.iter().zip(ARG_REGS) {
            if needed(param) {
                moves.push((frame.reg(*param)?, Value::Reg(reg)));
            }
        }
        for (dst, src) in parallel_moves(&moves, Value::Reg("x9")) {
            self.mov(dst, src);
        }
        for (i, param) in function.params.iter().enumerate().skip(ARG_REGS.len()) {
            if needed(param) {
                let dst = frame.reg(*param)?;
                let target = self.target(dst);
                let offset = 16 + 8 * (i - ARG_REGS.len());
                self.emit(&format!("\tldr\t{target}, [x29, #{offset}]"));
                self.store(dst, target);
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
        if is_main && self.ctx.counts_references {
            self.emit("\tbl\tstone.leak_check");
        }
        if !is_main {
            self.emit("\tadrp\tx16, stone.call_depth");
            self.emit("\tldr\tx17, [x16, :lo12:stone.call_depth]");
            self.emit("\tsub\tx17, x17, #1");
            self.emit("\tstr\tx17, [x16, :lo12:stone.call_depth]");
        }
        pop_frame(self, &allocation.used_callee_saved);
        Ok(())
    }

    /// Moves `sp` down (`sub`) or up (`add`) by `bytes`, a multiple of 16, through `x16` when the
    /// amount is too large for an immediate.
    fn adjust_sp(&mut self, op: &str, bytes: i64) {
        if fits_arith(bytes) {
            self.emit(&format!("\t{op}\tsp, sp, #{bytes}"));
        } else {
            self.mov_imm("x16", bytes);
            self.emit(&format!("\t{op}\tsp, sp, x16"));
        }
    }

    /// Returns the memory operand for the spill slot at `offset` bytes above `sp`, computing its
    /// address in `x8` first when the offset is too large for `ldr` and `str`.
    fn slot(&mut self, offset: i64) -> String {
        if offset <= MAX_SCALED_OFFSET {
            format!("[sp, #{offset}]")
        } else {
            self.mov_imm("x8", offset);
            self.emit("\tadd\tx8, sp, x8");
            "[x8]".to_string()
        }
    }

    /// Puts the 64-bit constant `n` in `reg`, with one `mov` when it fits 16 bits, and otherwise
    /// a `movz` and a `movk` for each other halfword that is not zero.
    ///
    /// For example, `mov_imm("x9", 65536)` emits `movz x9, #0` and `movk x9, #1, lsl #16`.
    fn mov_imm(&mut self, reg: &str, n: i64) {
        if (-65536..65536).contains(&n) {
            self.emit(&format!("\tmov\t{reg}, #{n}"));
            return;
        }
        let bits = n as u64;
        self.emit(&format!("\tmovz\t{reg}, #{}", bits & 0xffff));
        for shift in [16, 32, 48] {
            let half = (bits >> shift) & 0xffff;
            if half != 0 {
                self.emit(&format!("\tmovk\t{reg}, #{half}, lsl #{shift}"));
            }
        }
    }

    /// Returns the register to compute a result for `dst` in: `dst` itself when it is a register,
    /// and `x9` otherwise.
    fn target(&self, dst: Value) -> &'static str {
        match dst {
            Value::Reg(reg) => reg,
            _ => "x9",
        }
    }

    /// Loads `src` into register `reg`, unless it is already there.
    fn load(&mut self, reg: &'static str, src: Value) {
        match src {
            Value::Reg(from) if from == reg => {}
            Value::Reg(from) => self.emit(&format!("\tmov\t{reg}, {from}")),
            Value::Stack(offset) => {
                let slot = self.slot(offset);
                self.emit(&format!("\tldr\t{reg}, {slot}"));
            }
            Value::Imm(n) => self.mov_imm(reg, n),
        }
    }

    /// Stores register `reg` into `dst`, unless `dst` is that register.
    fn store(&mut self, dst: Value, reg: &'static str) {
        match dst {
            Value::Reg(to) if to == reg => {}
            Value::Reg(to) => self.emit(&format!("\tmov\t{to}, {reg}")),
            Value::Stack(offset) => {
                let slot = self.slot(offset);
                self.emit(&format!("\tstr\t{reg}, {slot}"));
            }
            Value::Imm(_) => {}
        }
    }

    /// Moves `src` into `dst`, going through `x16` when `dst` is in memory and `src` is not in a
    /// register.
    fn mov(&mut self, dst: Value, src: Value) {
        if dst == src {
            return;
        }
        match (dst, src) {
            (Value::Reg(reg), _) => self.load(reg, src),
            (_, Value::Reg(reg)) => self.store(dst, reg),
            _ => {
                let reg = self.operand(src, "x16");
                self.store(dst, reg);
            }
        }
    }

    /// Returns a register holding `value`, loading it into `scratch` unless it is already in one.
    /// Zero is `xzr`, so this is only for operands, never for addresses.
    fn operand(&mut self, value: Value, scratch: &'static str) -> &'static str {
        match value {
            Value::Reg(reg) => reg,
            Value::Imm(0) => "xzr",
            _ => {
                self.load(scratch, value);
                scratch
            }
        }
    }

    /// Returns a register holding `value`, loading it into `scratch` if needed, even for zero, so
    /// it works as an address or as the first operand of an instruction with an immediate.
    fn base(&mut self, value: Value, scratch: &'static str) -> &'static str {
        match value {
            Value::Reg(reg) => reg,
            _ => {
                self.load(scratch, value);
                scratch
            }
        }
    }

    /// Moves a float's bits from `value` into the `d` register `float`.
    fn load_float(&mut self, float: &str, value: Value) {
        match value {
            Value::Stack(offset) => {
                let slot = self.slot(offset);
                self.emit(&format!("\tldr\t{float}, {slot}"));
            }
            _ => {
                let reg = self.operand(value, "x16");
                self.emit(&format!("\tfmov\t{float}, {reg}"));
            }
        }
    }

    /// Moves a float's bits from the `d` register `float` into `dst`.
    fn store_float(&mut self, dst: Value, float: &str) {
        match dst {
            Value::Reg(reg) => self.emit(&format!("\tfmov\t{reg}, {float}")),
            Value::Stack(offset) => {
                let slot = self.slot(offset);
                self.emit(&format!("\tstr\t{float}, {slot}"));
            }
            Value::Imm(_) => {}
        }
    }

    /// Emits a comparison of `lhs` with the constant `n`, as `cmp` or `cmn` when the constant
    /// fits, and through `x11` otherwise. `lhs` is never `xzr` here, since the immediate forms
    /// read register 31 as `sp`.
    fn compare_with(&mut self, lhs: Value, n: i64) {
        let lhs = self.base(lhs, "x10");
        if fits_arith(n) {
            self.emit(&format!("\tcmp\t{lhs}, #{n}"));
        } else if n != i64::MIN && fits_arith(-n) {
            self.emit(&format!("\tcmn\t{lhs}, #{}", -n));
        } else {
            self.mov_imm("x11", n);
            self.emit(&format!("\tcmp\t{lhs}, x11"));
        }
    }

    /// Emits a signed comparison of `lhs` with `rhs`, swapping them when only `lhs` is an
    /// immediate, and returns the condition that then says whether `cond` held.
    fn compare_ints(&mut self, cond: Cond, lhs: Value, rhs: Value) -> Cond {
        let (cond, lhs, rhs) = match (lhs, rhs) {
            (Value::Imm(_), Value::Reg(_) | Value::Stack(_)) => (cond.swap(), rhs, lhs),
            _ => (cond, lhs, rhs),
        };
        match rhs {
            Value::Imm(n) => self.compare_with(lhs, n),
            _ => {
                let lhs = self.operand(lhs, "x10");
                let rhs = self.operand(rhs, "x11");
                self.emit(&format!("\tcmp\t{lhs}, {rhs}"));
            }
        }
        cond
    }

    /// Emits `fcmp` of the floats `lhs` and `rhs`, through `d0` and `d1`.
    fn compare_floats(&mut self, lhs: Value, rhs: Value) {
        self.load_float("d0", lhs);
        self.load_float("d1", rhs);
        self.emit("\tfcmp\td0, d1");
    }

    /// Jumps to `then` when condition `yes` holds and to `otherwise` when `no` does, leaving out
    /// a jump to `next`, the block that follows.
    fn branch(
        &mut self,
        frame: &Frame,
        (yes, no): (&str, &str),
        then: BlockId,
        otherwise: BlockId,
        next: BlockId,
    ) {
        if then == next {
            self.emit(&format!("\tb.{no}\t{}", frame.blocks[otherwise.0]));
        } else {
            self.emit(&format!("\tb.{yes}\t{}", frame.blocks[then.0]));
            if otherwise != next {
                self.emit(&format!("\tb\t{}", frame.blocks[otherwise.0]));
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
                    self.emit(&format!("\tb\t{}", frame.blocks[target.0]));
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
                        self.emit(&format!("\tb\t{}", frame.blocks[target.0]));
                    }
                }
                value => {
                    let reg = self.operand(value, "x10");
                    if *then == next {
                        self.emit(&format!("\tcbz\t{reg}, {}", frame.blocks[otherwise.0]));
                    } else {
                        self.emit(&format!("\tcbnz\t{reg}, {}", frame.blocks[then.0]));
                        if *otherwise != next {
                            self.emit(&format!("\tb\t{}", frame.blocks[otherwise.0]));
                        }
                    }
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
                    self.compare_floats(lhs, rhs);
                    self.branch(frame, float_cc(*cond), *then, *otherwise, next);
                } else {
                    let cond = self.compare_ints(*cond, lhs, rhs);
                    self.branch(
                        frame,
                        (cc(cond), cc(cond.negate())),
                        *then,
                        *otherwise,
                        next,
                    );
                }
            }
            Terminator::Fail(message) => {
                let label = self.ctx.fail_label(message);
                self.emit(&format!("\tb\t{label}"));
            }
            Terminator::Return(value) => {
                match value.map(|v| frame.value(v)).transpose()? {
                    None => self.emit("\tmov\tx0, #0"),
                    Some(value) => self.load("x0", value),
                }
                if next.0 < frame.blocks.len() {
                    self.emit(&format!("\tb\t{}", frame.epilogue));
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
                let dividend = self.operand(lhs, "x10");
                match rhs {
                    Value::Imm(divisor) if divisor != 0 && divisor != -1 => {
                        if *op == BinOp::Div {
                            self.divide_by_constant(dividend, divisor);
                        } else {
                            self.mov_imm("x11", divisor);
                            self.emit(&format!("\tsdiv\tx9, {dividend}, x11"));
                            self.emit(&format!("\tmsub\tx9, x9, x11, {dividend}"));
                        }
                    }
                    _ => {
                        let divisor = self.base(rhs, "x11");
                        // sdiv gives 0 for both of these rather than failing, so report them
                        // like the interpreter
                        let by_zero = self.ctx.fail_label("division by zero");
                        let divide = self.ctx.new_label("divide");
                        let done = self.ctx.new_label("divided");
                        self.emit(&format!("\tcbz\t{divisor}, {by_zero}"));
                        self.emit(&format!("\tcmn\t{divisor}, #1"));
                        self.emit(&format!("\tb.ne\t{divide}"));
                        if *op == BinOp::Div {
                            let overflow = self.ctx.fail_label("integer overflow in division");
                            // only the minimum overflows when negated
                            self.emit(&format!("\tnegs\tx9, {dividend}"));
                            self.emit(&format!("\tb.vs\t{overflow}"));
                        } else {
                            // anything % -1 is 0, including the minimum
                            self.emit("\tmov\tx9, #0");
                        }
                        self.emit(&format!("\tb\t{done}"));
                        self.emit(&format!("{divide}:"));
                        self.emit(&format!("\tsdiv\tx9, {dividend}, {divisor}"));
                        if *op == BinOp::Rem {
                            self.emit(&format!("\tmsub\tx9, x9, {divisor}, {dividend}"));
                        }
                        self.emit(&format!("{done}:"));
                    }
                }
                self.store(dst, "x9");
            }

            Inst::Binary {
                op: BinOp::Pow,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                self.load("x10", lhs);
                self.load("x11", rhs);
                if !matches!(rhs, Value::Imm(n) if n >= 0) {
                    let negative = self.ctx.fail_label("negative exponent");
                    self.emit("\tcmp\tx11, #0");
                    self.emit(&format!("\tb.lt\t{negative}"));
                }
                self.emit("\tmov\tx9, #1");
                self.power_loop("mul\tx9, x9, x10", "mul\tx10, x10, x10");
                self.store(dst, "x9");
            }

            Inst::Binary { op, dst, lhs, rhs } => {
                let (dst, mut lhs, mut rhs) =
                    (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                // `a + b` is `b + a`, so an immediate can go second, where `add` takes it
                if matches!(op, BinOp::Add | BinOp::Mul) && matches!(lhs, Value::Imm(_)) {
                    std::mem::swap(&mut lhs, &mut rhs);
                }
                let target = self.target(dst);
                // not xzr, which the immediate forms would read as sp
                let left = self.base(lhs, "x10");
                let name = match op {
                    BinOp::Add => "add",
                    BinOp::Sub => "sub",
                    BinOp::Mul => "mul",
                    BinOp::Div | BinOp::Rem | BinOp::Pow => {
                        return Err(format!("{op:?} is emitted separately"));
                    }
                };
                match (op, rhs) {
                    (BinOp::Add | BinOp::Sub, Value::Imm(n)) if fits_arith(n) => {
                        self.emit(&format!("\t{name}\t{target}, {left}, #{n}"));
                    }
                    (BinOp::Add | BinOp::Sub, Value::Imm(n)) if n != i64::MIN && fits_arith(-n) => {
                        let flipped = if *op == BinOp::Add { "sub" } else { "add" };
                        self.emit(&format!("\t{flipped}\t{target}, {left}, #{}", -n));
                    }
                    _ => {
                        let right = self.operand(rhs, "x11");
                        self.emit(&format!("\t{name}\t{target}, {left}, {right}"));
                    }
                }
                self.store(dst, target);
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
                self.load_float("d1", lhs);
                self.emit("\tfmov\td0, #1.0");
                let negative = match rhs {
                    Value::Imm(n) => {
                        // the magnitude of the minimum is 2^63, which lsr reads correctly
                        self.mov_imm("x11", n.unsigned_abs() as i64);
                        Some(n < 0)
                    }
                    _ => {
                        self.load("x10", rhs);
                        self.emit("\tcmp\tx10, #0");
                        self.emit("\tcneg\tx11, x10, lt");
                        None
                    }
                };
                self.power_loop("fmul\td0, d0, d1", "fmul\td1, d1, d1");
                let reciprocal = |r#gen: &mut Self| {
                    r#gen.emit("\tfmov\td1, #1.0");
                    r#gen.emit("\tfdiv\td0, d1, d0");
                };
                match negative {
                    Some(true) => reciprocal(self),
                    Some(false) => {}
                    None => {
                        let done = self.ctx.new_label("powered");
                        self.emit("\tcmp\tx10, #0");
                        self.emit(&format!("\tb.ge\t{done}"));
                        reciprocal(self);
                        self.emit(&format!("{done}:"));
                    }
                }
                self.store_float(dst, "d0");
            }

            Inst::FloatBinary { op, dst, lhs, rhs } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                self.load_float("d0", lhs);
                self.load_float("d1", rhs);
                match op {
                    BinOp::Add => self.emit("\tfadd\td0, d0, d1"),
                    BinOp::Sub => self.emit("\tfsub\td0, d0, d1"),
                    BinOp::Mul => self.emit("\tfmul\td0, d0, d1"),
                    BinOp::Div => {
                        self.fail_on_float_zero("d1");
                        self.emit("\tfdiv\td0, d0, d1");
                    }
                    BinOp::Rem => {
                        // fmod is exact, with the dividend's sign, as Rust's `%` is
                        self.fail_on_float_zero("d1");
                        self.uses_fmod = true;
                        self.emit("\tbl\tstone.fmod");
                    }
                    BinOp::Pow => return Err("float Pow is emitted separately".to_string()),
                }
                self.store_float(dst, "d0");
            }

            Inst::Neg { dst, src } | Inst::FloatNeg { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                let target = self.target(dst);
                let src = self.operand(src, "x10");
                if matches!(inst, Inst::Neg { .. }) {
                    self.emit(&format!("\tneg\t{target}, {src}"));
                } else {
                    // flip the sign bit, which also negates 0.0 and nan like the interpreter
                    self.emit(&format!("\teor\t{target}, {src}, #0x8000000000000000"));
                }
                self.store(dst, target);
            }

            Inst::Not { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                let target = self.target(dst);
                self.compare_with(src, 0);
                self.emit(&format!("\tcset\t{target}, eq"));
                self.store(dst, target);
            }

            Inst::Compare {
                cond,
                float,
                dst,
                lhs,
                rhs,
            } => {
                let (dst, lhs, rhs) = (frame.reg(*dst)?, frame.value(*lhs)?, frame.value(*rhs)?);
                let holds = if *float {
                    self.compare_floats(lhs, rhs);
                    float_cc(*cond).0
                } else {
                    cc(self.compare_ints(*cond, lhs, rhs))
                };
                let target = self.target(dst);
                self.emit(&format!("\tcset\t{target}, {holds}"));
                self.store(dst, target);
            }

            Inst::LoadGlobal { dst, name, checked } => {
                let dst = frame.reg(*dst)?;
                let label = global_label(name);
                if *checked {
                    let fail = self
                        .ctx
                        .fail_label(&format!("'{name}' is used before it is assigned"));
                    self.emit(&format!("\tadrp\tx16, {label}.set"));
                    self.emit(&format!("\tldr\tx17, [x16, :lo12:{label}.set]"));
                    self.emit(&format!("\tcbz\tx17, {fail}"));
                }
                let target = self.target(dst);
                self.emit(&format!("\tadrp\tx16, {label}"));
                self.emit(&format!("\tldr\t{target}, [x16, :lo12:{label}]"));
                self.store(dst, target);
            }

            Inst::StoreGlobal { name, src } => {
                let label = global_label(name);
                let src = frame.value(*src)?;
                let reg = self.operand(src, "x10");
                self.emit(&format!("\tadrp\tx16, {label}"));
                self.emit(&format!("\tstr\t{reg}, [x16, :lo12:{label}]"));
                self.emit(&format!("\tadrp\tx16, {label}.set"));
                self.emit("\tmov\tx17, #1");
                self.emit(&format!("\tstr\tx17, [x16, :lo12:{label}.set]"));
            }

            Inst::StrAddr { dst, text } => {
                let dst = frame.reg(*dst)?;
                let label = self.ctx.intern_string(text);
                let target = self.target(dst);
                super::address(self, target, &label);
                self.store(dst, target);
            }

            Inst::ListLen { dst, list } => {
                let (dst, list) = (frame.reg(*dst)?, frame.value(*list)?);
                let base = self.base(list, "x10");
                let target = self.target(dst);
                self.emit(&format!("\tldr\t{target}, [{base}]"));
                self.store(dst, target);
            }

            Inst::ListLoad { dst, list, index } | Inst::ListGet { dst, list, index } => {
                let checked = matches!(inst, Inst::ListLoad { .. });
                let (dst, list, index) =
                    (frame.reg(*dst)?, frame.value(*list)?, frame.value(*index)?);
                let slot = self.list_slot(list, index, checked);
                let target = self.target(dst);
                self.emit(&format!("\tldr\t{target}, {slot}"));
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
                // `old` may share a register with `value`, so the old element waits in x9
                if old.is_some() {
                    self.emit(&format!("\tldr\tx9, {slot}"));
                }
                let reg = self.operand(value, "x16");
                self.emit(&format!("\tstr\t{reg}, {slot}"));
                if let Some(old) = old {
                    self.store(frame.reg(*old)?, "x9");
                }
            }

            Inst::ListInit { list, index, value } => {
                let (list, value) = (frame.value(*list)?, frame.value(*value)?);
                let slot = self.list_slot(list, Value::Imm(*index as i64), false);
                let reg = self.operand(value, "x16");
                self.emit(&format!("\tstr\t{reg}, {slot}"));
            }

            Inst::Retain { src } => match frame.value(*src)? {
                // none, or a variable never assigned
                Value::Imm(0) => {}
                Value::Imm(_) => return Err("only a pointer can be retained".to_string()),
                value => {
                    let base = self.base(value, "x10");
                    let skip = self.ctx.new_label("retained");
                    self.emit(&format!("\tcbz\t{base}, {skip}"));
                    self.emit(&format!("\tldur\tx16, [{base}, #-8]"));
                    self.emit("\tadd\tx16, x16, #1");
                    self.emit(&format!("\tstur\tx16, [{base}, #-8]"));
                    self.emit(&format!("{skip}:"));
                }
            },

            Inst::Release { src, kind } => match frame.value(*src)? {
                Value::Imm(0) => {}
                Value::Imm(_) => return Err("only a pointer can be released".to_string()),
                value => {
                    let base = self.base(value, "x10");
                    let skip = self.ctx.new_label("released");
                    self.emit(&format!("\tcbz\t{base}, {skip}"));
                    self.emit(&format!("\tldur\tx16, [{base}, #-8]"));
                    self.emit("\tsubs\tx16, x16, #1");
                    self.emit(&format!("\tstur\tx16, [{base}, #-8]"));
                    self.emit(&format!("\tb.ne\t{skip}"));
                    // the last reference is gone, and the free routines take it in x9
                    self.load("x9", Value::Reg(base));
                    let routine = match kind {
                        RcKind::Str => "stone.free_str",
                        RcKind::List => "stone.free_list",
                    };
                    self.emit(&format!("\tbl\t{routine}"));
                    self.emit(&format!("{skip}:"));
                }
            },

            Inst::ListUnique { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                // a list nothing else refers to is changed in place, and any other is copied
                // first by `stone.list_copy`, which takes and returns it in x9
                self.load("x9", src);
                let skip = self.ctx.new_label("unique");
                self.emit("\tldur\tx16, [x9, #-8]");
                self.emit("\tcmp\tx16, #1");
                self.emit(&format!("\tb.eq\t{skip}"));
                self.emit("\tbl\tstone.list_copy");
                self.emit(&format!("{skip}:"));
                self.store(dst, "x9");
            }

            Inst::FloatSqrt { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.load_float("d0", src);
                self.emit("\tfsqrt\td0, d0");
                self.store_float(dst, "d0");
            }

            Inst::IntToFloat { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                let src = self.operand(src, "x10");
                self.emit(&format!("\tscvtf\td0, {src}"));
                self.store_float(dst, "d0");
            }

            Inst::FloatToInt { dst, src } => {
                let (dst, src) = (frame.reg(*dst)?, frame.value(*src)?);
                self.load_float("d0", src);
                self.float_to_int();
                let target = self.target(dst);
                self.emit(&format!("\tfcvtzs\t{target}, d0"));
                self.store(dst, target);
            }

            Inst::Call { dst, callee, args } => {
                let mut values = Vec::new();
                for arg in args {
                    values.push(frame.value(*arg)?);
                }
                // arguments past the eighth go at the bottom of the frame, the first lowest
                for (k, value) in values.iter().skip(ARG_REGS.len()).enumerate() {
                    let reg = self.operand(*value, "x16");
                    self.emit(&format!("\tstr\t{reg}, [sp, #{}]", 8 * k));
                }
                let moves: Vec<(Value, Value)> = ARG_REGS
                    .iter()
                    .zip(&values)
                    .map(|(reg, value)| (Value::Reg(reg), *value))
                    .collect();
                for (to, from) in parallel_moves(&moves, Value::Reg("x9")) {
                    self.mov(to, from);
                }
                let label = match callee {
                    Callee::User(name) => function_label(name),
                    Callee::Runtime(label) => label.to_string(),
                    Callee::Print(ty) => self.ctx.print_routine(ty, false)?,
                };
                self.emit(&format!("\tbl\t{label}"));
                if let Some(dst) = dst {
                    self.store(frame.reg(*dst)?, "x0");
                }
            }
        }
        Ok(())
    }

    /// Returns the memory operand for element `index` of `list`.
    ///
    /// When `checked`, a negative index counts from the end, and an index still out of range
    /// exits with an error. A list is a pointer to a `{len, cap, data, elem}` header (see
    /// [`super::builtins::list_runtime`]), so for example `xs[-1]` with `xs` of length 3 checks
    /// that `-1 + 3` is below 3 and returns `[x17, x11, lsl #3]` with `x17` holding `data` and
    /// `x11` holding 2. This uses `x10`, `x11`, `x16`, and `x17`.
    fn list_slot(&mut self, list: Value, index: Value, checked: bool) -> String {
        let base = self.base(list, "x10");
        let constant = match index {
            Value::Imm(n) if (0..4096).contains(&n) => Some(n),
            _ => None,
        };
        if checked {
            let out_of_range = self.ctx.fail_label("list index out of range");
            self.emit(&format!("\tldr\tx16, [{base}]"));
            match constant {
                Some(n) => {
                    // the length is never negative, so a constant index is in range below it
                    self.emit(&format!("\tcmp\tx16, #{n}"));
                    self.emit(&format!("\tb.le\t{out_of_range}"));
                }
                None => {
                    let check = self.ctx.new_label("index_check");
                    self.load("x11", index);
                    self.emit("\tcmp\tx11, #0");
                    self.emit(&format!("\tb.ge\t{check}"));
                    // negative indexes count from the end
                    self.emit("\tadd\tx11, x11, x16");
                    self.emit(&format!("{check}:"));
                    // unsigned, so an index still negative after adjusting is out of range too
                    self.emit("\tcmp\tx11, x16");
                    self.emit(&format!("\tb.hs\t{out_of_range}"));
                }
            }
        } else if constant.is_none() {
            self.load("x11", index);
        }
        self.emit(&format!("\tldr\tx17, [{base}, #16]"));
        match constant {
            Some(n) => format!("[x17, #{}]", 8 * n),
            None => "[x17, x11, lsl #3]".to_string(),
        }
    }

    /// Jumps to the `division by zero` failure if the float divisor in `float` is 0.0 or -0.0, as
    /// in the interpreter, rather than letting `/` or `%` give inf or nan. A nan divisor compares
    /// unordered, which is not equal, so it is let through.
    fn fail_on_float_zero(&mut self, float: &str) {
        let by_zero = self.ctx.fail_label("division by zero");
        self.emit(&format!("\tfcmp\t{float}, #0.0"));
        self.emit(&format!("\tb.eq\t{by_zero}"));
    }

    /// Emits the loop that raises a base to the exponent in `x11` by squaring and multiplying,
    /// as `stdlib::int_pow` and `stdlib::float_pow` do. `multiply` folds the base into the
    /// result and `square` squares the base, and `x11` ends at 0.
    ///
    /// For example, `power_loop("mul\tx9, x9, x10", "mul\tx10, x10, x10")` leaves `x10` to the
    /// power `x11` in `x9`, if `x9` started at 1.
    fn power_loop(&mut self, multiply: &str, square: &str) {
        let top = self.ctx.new_label("pow");
        let skip = self.ctx.new_label("pow_skip");
        let done = self.ctx.new_label("pow_done");
        self.emit(&format!("{top}:"));
        self.emit(&format!("\tcbz\tx11, {done}"));
        self.emit(&format!("\ttbz\tx11, #0, {skip}"));
        self.emit(&format!("\t{multiply}"));
        self.emit(&format!("{skip}:"));
        self.emit(&format!("\t{square}"));
        self.emit("\tlsr\tx11, x11, #1");
        self.emit(&format!("\tb\t{top}"));
        self.emit(&format!("{done}:"));
    }

    /// Divides `dividend` by a constant that is neither 0 nor -1, rounding toward zero, leaving
    /// the quotient in `x9`.
    ///
    /// Neither overflow nor a zero divisor can happen for such a divisor, so no checks are
    /// emitted, and a power of two becomes shifts. For example, dividing by `8` adds a bias of 7
    /// to a negative dividend and then shifts right by 3, and dividing by `10` emits `sdiv`.
    fn divide_by_constant(&mut self, dividend: &str, divisor: i64) {
        let magnitude = divisor.unsigned_abs();
        if !magnitude.is_power_of_two() {
            self.mov_imm("x11", divisor);
            self.emit(&format!("\tsdiv\tx9, {dividend}, x11"));
            return;
        }

        let shift = magnitude.trailing_zeros();
        if shift > 0 {
            // an arithmetic shift rounds down, so first add 2^shift - 1 to a negative dividend
            self.emit(&format!("\tasr\tx9, {dividend}, #63")); // all ones if negative
            self.emit(&format!("\tadd\tx9, {dividend}, x9, lsr #{}", 64 - shift));
            self.emit(&format!("\tasr\tx9, x9, #{shift}"));
        } else {
            self.emit(&format!("\tmov\tx9, {dividend}"));
        }
        if divisor < 0 {
            // the quotient is never the minimum here, so negating it cannot overflow
            self.emit("\tneg\tx9, x9");
        }
    }

    /// Jumps to the conversion failure unless the float in `d0` truncates to an int, which is
    /// when it is a number from -2^63 up to but not including 2^63. `fcvtzs` would instead give 0
    /// for nan and the nearest int for anything out of range. Clobbers `d1` and `x16`.
    fn float_to_int(&mut self) {
        let fail = self
            .ctx
            .fail_label("cannot convert float to int (nan or out of range)");
        self.emit("\tfcmp\td0, d0");
        self.emit(&format!("\tb.vs\t{fail}")); // nan
        self.mov_imm("x16", (2f64.powi(63)).to_bits() as i64);
        self.emit("\tfmov\td1, x16");
        self.emit("\tfcmp\td0, d1");
        self.emit(&format!("\tb.ge\t{fail}"));
        self.mov_imm("x16", (-(2f64.powi(63))).to_bits() as i64);
        self.emit("\tfmov\td1, x16");
        self.emit("\tfcmp\td0, d1");
        self.emit(&format!("\tb.lt\t{fail}"));
    }
}
