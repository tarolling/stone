//! Inlines calls of small stone functions that call no stone function themselves, so a call such
//! as `bodies[i].drift(dt)` passes nothing and returns nothing, and the optimizer (see
//! [`super::opt`]) sees the callee's work next to its caller's.
//!
//! A callee's IR already says who owns each value, so splicing it in is a matter of renaming:
//! its parameters become copies of the arguments, its vregs and blocks new ones of the caller,
//! and each `ret` copies the results to the call's destinations and goes on after the call. The
//! call is replaced by a [`Inst::DepthCheck`], so a call one level past the limit still fails as
//! it would have, and since the callee calls no stone function, nothing deeper can tell the
//! difference. Every runtime error in the callee keeps the callee's span.

use super::{Block, BlockId, Callee, Function, Inst, Operand, Program, Terminator, VReg};
use std::collections::HashMap;

/// The most instructions a function may have to be inlined.
const MAX_INSTS: usize = 48;

/// The most blocks a function may have to be inlined.
const MAX_BLOCKS: usize = 8;

/// Inlines every call of a small leaf function in `program`, innermost first, so that a function
/// whose calls were all inlined can be inlined in turn.
pub fn inline(program: &mut Program) {
    loop {
        let leaves: HashMap<String, Function> = program
            .functions
            .iter()
            .filter(|function| is_inlinable(function))
            .filter_map(|function| Some((function.name.clone()?, function.clone())))
            .collect();
        let mut changed = false;
        for function in &mut program.functions {
            while let Some((b, i, callee)) = find_call(function, &leaves) {
                splice(function, b, i, callee);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

/// Returns whether `function` is small, calls no stone function (so it cannot recurse), and is
/// one the program defines, not one lowering made.
fn is_inlinable(function: &Function) -> bool {
    let insts: usize = function.blocks.iter().map(|block| block.insts.len()).sum();
    function.name.is_some()
        && !function.helper
        && function.blocks.len() <= MAX_BLOCKS
        && insts <= MAX_INSTS
        && !function
            .blocks
            .iter()
            .flat_map(|block| &block.insts)
            .any(|inst| {
                // reduce_pio2 calls the prelude, and its depth check must stay a call deeper
                matches!(
                    inst,
                    Inst::Call {
                        callee: Callee::User(_) | Callee::Value(_),
                        ..
                    } | Inst::ReducePio2 { .. }
                )
            })
}

/// Returns the first call in `function` of one of `leaves` other than itself, by its block and
/// position, with the callee.
fn find_call<'a>(
    function: &Function,
    leaves: &'a HashMap<String, Function>,
) -> Option<(usize, usize, &'a Function)> {
    for (b, block) in function.blocks.iter().enumerate() {
        for (i, inst) in block.insts.iter().enumerate() {
            if let Inst::Call {
                callee: Callee::User(name),
                ..
            } = inst
                && function.name.as_ref() != Some(name)
                && let Some(callee) = leaves.get(name)
            {
                return Some((b, i, callee));
            }
        }
    }
    None
}

/// Replaces the call at position `i` of block `b` of `function` with the body of `callee`.
///
/// The block is split at the call: its first part checks the call depth, copies the arguments to
/// the callee's parameters, and goes to the callee's first block, which comes right after it,
/// followed by the callee's other blocks and then the rest of the split block.
fn splice(function: &mut Function, b: usize, i: usize, callee: &Function) {
    let Inst::Call { dsts, args, .. } = function.blocks[b].insts[i].clone() else {
        unreachable!("only a call is inlined");
    };
    let call_span = function.blocks[b].spans[i];
    let first_vreg = function.vreg_count;
    function.vreg_count += callee.vreg_count;
    let rename = |VReg(n): VReg| VReg(first_vreg + n);
    let rename_operand = |operand: Operand| match operand {
        Operand::Reg(reg) => Operand::Reg(rename(reg)),
        imm => imm,
    };
    let inlined = callee.blocks.len();
    // the caller's blocks after the split one move down past the callee's and the rest of it
    let move_block = |BlockId(n): BlockId| match n > b {
        true => BlockId(n + inlined + 1),
        false => BlockId(n),
    };
    let rest = BlockId(b + inlined + 1);

    let depth = function.blocks[b].loop_depth;
    let split = &mut function.blocks[b];
    let after_insts = split.insts.split_off(i + 1);
    let after_spans = split.spans.split_off(i + 1);
    split.insts.pop();
    split.spans.pop();
    let after_term = std::mem::replace(&mut split.term, Terminator::Jump(BlockId(b + 1)));
    let after_term_span = split.term_span;
    split.insts.push(Inst::DepthCheck);
    split.spans.push(call_span);
    for (&param, &arg) in callee.params.iter().zip(&args) {
        split.insts.push(Inst::Copy {
            dst: rename(param),
            src: arg,
        });
        split.spans.push(call_span);
    }

    let mut body = Vec::with_capacity(inlined + 1);
    for block in &callee.blocks {
        let mut insts: Vec<Inst> = block
            .insts
            .iter()
            .map(|inst| rename_inst(inst, &rename, &rename_operand))
            .collect();
        let mut spans = block.spans.clone();
        let into_callee = |BlockId(n): BlockId| BlockId(b + 1 + n);
        let term = match &block.term {
            Terminator::Return(values) => {
                for (&dst, &value) in dsts.iter().zip(values) {
                    insts.push(Inst::Copy {
                        dst,
                        src: rename_operand(value),
                    });
                    spans.push(block.term_span);
                }
                Terminator::Jump(rest)
            }
            term => retarget(term, into_callee, rename_operand),
        };
        body.push(Block {
            insts,
            spans,
            term,
            term_span: block.term_span,
            loop_depth: block.loop_depth + depth,
        });
    }
    body.push(Block {
        insts: after_insts,
        spans: after_spans,
        term: after_term,
        term_span: after_term_span,
        loop_depth: depth,
    });

    for block in &mut function.blocks {
        block.term = retarget(&block.term, move_block, |operand| operand);
    }
    function.blocks[b].term = Terminator::Jump(BlockId(b + 1));
    // the rest of the split block keeps its old targets, which moved like every other block's
    let last = body.len() - 1;
    body[last].term = retarget(&body[last].term, move_block, |operand| operand);
    function.blocks.splice(b + 1..b + 1, body);
}

/// Returns `term` with its targets passed through `block` and its operands through `operand`.
pub(super) fn retarget(
    term: &Terminator,
    block: impl Fn(BlockId) -> BlockId,
    operand: impl Fn(Operand) -> Operand,
) -> Terminator {
    match term {
        Terminator::Jump(target) => Terminator::Jump(block(*target)),
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => Terminator::Branch {
            cond: operand(*cond),
            then: block(*then),
            otherwise: block(*otherwise),
        },
        Terminator::CmpBranch {
            cond,
            float,
            lhs,
            rhs,
            then,
            otherwise,
        } => Terminator::CmpBranch {
            cond: *cond,
            float: *float,
            lhs: operand(*lhs),
            rhs: operand(*rhs),
            then: block(*then),
            otherwise: block(*otherwise),
        },
        Terminator::TestBranch {
            value,
            mask,
            then,
            otherwise,
        } => Terminator::TestBranch {
            value: operand(*value),
            mask: *mask,
            then: block(*then),
            otherwise: block(*otherwise),
        },
        Terminator::Return(values) => {
            Terminator::Return(values.iter().map(|value| operand(*value)).collect())
        }
        Terminator::Fail(message, values) => Terminator::Fail(
            message.clone(),
            values.iter().map(|value| operand(*value)).collect(),
        ),
    }
}

/// Returns `inst` with every vreg it writes renamed by `reg` and every operand by `operand`.
pub(super) fn rename_inst(
    inst: &Inst,
    reg: &impl Fn(VReg) -> VReg,
    operand: &impl Fn(Operand) -> Operand,
) -> Inst {
    use super::Kernel;
    fn kernel(k: &Kernel, operand: &impl Fn(Operand) -> Operand) -> Kernel {
        match k {
            Kernel::Element(value) => Kernel::Element(operand(*value)),
            Kernel::Number(value) => Kernel::Number(operand(*value)),
            Kernel::Binary(op, lhs, rhs) => Kernel::Binary(
                *op,
                Box::new(kernel(lhs, operand)),
                Box::new(kernel(rhs, operand)),
            ),
            Kernel::Neg(value) => Kernel::Neg(Box::new(kernel(value, operand))),
        }
    }
    let o = operand;
    match inst.clone() {
        Inst::Copy { dst, src } => Inst::Copy {
            dst: reg(dst),
            src: o(src),
        },
        Inst::Binary { op, dst, lhs, rhs } => Inst::Binary {
            op,
            dst: reg(dst),
            lhs: o(lhs),
            rhs: o(rhs),
        },
        Inst::FloatBinary { op, dst, lhs, rhs } => Inst::FloatBinary {
            op,
            dst: reg(dst),
            lhs: o(lhs),
            rhs: o(rhs),
        },
        Inst::Neg { dst, src } => Inst::Neg {
            dst: reg(dst),
            src: o(src),
        },
        Inst::FloatNeg { dst, src } => Inst::FloatNeg {
            dst: reg(dst),
            src: o(src),
        },
        Inst::FloatAbs { dst, src } => Inst::FloatAbs {
            dst: reg(dst),
            src: o(src),
        },
        Inst::FloatSqrt { dst, src } => Inst::FloatSqrt {
            dst: reg(dst),
            src: o(src),
        },
        Inst::Trig { func, dst, args } => Inst::Trig {
            func,
            dst: reg(dst),
            args: args.into_iter().map(o).collect(),
        },
        Inst::SinCos { sin, cos, src } => Inst::SinCos {
            sin: reg(sin),
            cos: reg(cos),
            src: o(src),
        },
        Inst::ReducePio2 { n, hi, lo, src } => Inst::ReducePio2 {
            n: reg(n),
            hi: reg(hi),
            lo: reg(lo),
            src: o(src),
        },
        Inst::Not { dst, src } => Inst::Not {
            dst: reg(dst),
            src: o(src),
        },
        Inst::Compare {
            cond,
            float,
            dst,
            lhs,
            rhs,
        } => Inst::Compare {
            cond,
            float,
            dst: reg(dst),
            lhs: o(lhs),
            rhs: o(rhs),
        },
        Inst::LoadGlobal { dst, name, checked } => Inst::LoadGlobal {
            dst: reg(dst),
            name,
            checked,
        },
        Inst::StoreGlobal { name, src } => Inst::StoreGlobal { name, src: o(src) },
        Inst::StrAddr { dst, text } => Inst::StrAddr {
            dst: reg(dst),
            text,
        },
        Inst::BytesAddr { dst, bytes } => Inst::BytesAddr {
            dst: reg(dst),
            bytes,
        },
        Inst::ListLen { dst, list } => Inst::ListLen {
            dst: reg(dst),
            list: o(list),
        },
        Inst::StrLen { dst, text } => Inst::StrLen {
            dst: reg(dst),
            text: o(text),
        },
        Inst::StrByte { dst, text, index } => Inst::StrByte {
            dst: reg(dst),
            text: o(text),
            index: o(index),
        },
        Inst::ListLoad { dst, list, index } => Inst::ListLoad {
            dst: reg(dst),
            list: o(list),
            index: o(index),
        },
        Inst::ListStore {
            old,
            list,
            index,
            value,
        } => Inst::ListStore {
            old: old.map(reg),
            list: o(list),
            index: o(index),
            value: o(value),
        },
        Inst::ListGet {
            dst,
            list,
            index,
            offset,
        } => Inst::ListGet {
            dst: reg(dst),
            list: o(list),
            index: o(index),
            offset,
        },
        Inst::ListSet {
            list,
            index,
            value,
            offset,
        } => Inst::ListSet {
            list: o(list),
            index: o(index),
            value: o(value),
            offset,
        },
        Inst::ElementSlot {
            dst,
            list,
            index,
            width,
        } => Inst::ElementSlot {
            dst: reg(dst),
            list: o(list),
            index: o(index),
            width,
        },
        Inst::ListData { dst, list } => Inst::ListData {
            dst: reg(dst),
            list: o(list),
        },
        Inst::DataGet {
            dst,
            data,
            index,
            offset,
        } => Inst::DataGet {
            dst: reg(dst),
            data: o(data),
            index: o(index),
            offset,
        },
        Inst::DataSet {
            data,
            index,
            value,
            offset,
        } => Inst::DataSet {
            data: o(data),
            index: o(index),
            value: o(value),
            offset,
        },
        Inst::Packed {
            done,
            float,
            kernel: k,
            out,
            len,
        } => Inst::Packed {
            done: reg(done),
            float,
            kernel: kernel(&k, o),
            out: o(out),
            len: o(len),
        },
        Inst::ListUnique {
            dst,
            src,
            maybe_null,
        } => Inst::ListUnique {
            dst: reg(dst),
            src: o(src),
            maybe_null,
        },
        Inst::ListInit { list, index, value } => Inst::ListInit {
            list: o(list),
            index,
            value: o(value),
        },
        Inst::IntToFloat { dst, src } => Inst::IntToFloat {
            dst: reg(dst),
            src: o(src),
        },
        Inst::FloatToInt { dst, src, checked } => Inst::FloatToInt {
            dst: reg(dst),
            src: o(src),
            checked,
        },
        Inst::EnumTag { dst, value } => Inst::EnumTag {
            dst: reg(dst),
            value: o(value),
        },
        Inst::FieldGet { dst, value, slot } => Inst::FieldGet {
            dst: reg(dst),
            value: o(value),
            slot,
        },
        Inst::FieldInit { value, slot, src } => Inst::FieldInit {
            value: o(value),
            slot,
            src: o(src),
        },
        Inst::VariantStatic { dst, tag } => Inst::VariantStatic { dst: reg(dst), tag },
        Inst::Retain { src } => Inst::Retain { src: o(src) },
        Inst::Release { src, kind } => Inst::Release { src: o(src), kind },
        Inst::FunctionAddress { dst, label } => Inst::FunctionAddress {
            dst: reg(dst),
            label,
        },
        Inst::FunctionStatic { dst, label } => Inst::FunctionStatic {
            dst: reg(dst),
            label,
        },
        Inst::DepthCheck => Inst::DepthCheck,
        Inst::Call { dsts, callee, args } => Inst::Call {
            dsts: dsts.into_iter().map(reg).collect(),
            callee: match callee {
                Callee::Value(function) => Callee::Value(o(function)),
                callee => callee,
            },
            args: args.into_iter().map(o).collect(),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeChecker;
    use crate::codegen::ir::lower::lower;
    use crate::driver::parse_program;

    /// Parses, checks, lowers, and inlines `source`, returning the function named `name`, or
    /// `main` for `None`.
    fn inlined(source: &str, name: Option<&str>) -> Function {
        let module = parse_program(source).unwrap();
        let analysis = TypeChecker::new().analyze(&module);
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let mut program = lower(&module, &analysis).unwrap();
        inline(&mut program);
        program
            .functions
            .into_iter()
            .find(|f| f.name.as_deref() == name)
            .unwrap()
    }

    fn calls(function: &Function, name: &str) -> usize {
        function
            .blocks
            .iter()
            .flat_map(|block| &block.insts)
            .filter(|inst| matches!(inst, Inst::Call { callee: Callee::User(n), .. } if n == name))
            .count()
    }

    #[test]
    fn a_small_leaf_is_spliced_in_with_a_depth_check() {
        let f = inlined(
            "fn sq(x: int) -> int:\n    ret x * x\nfn f(a: int) -> int:\n    ret sq(a) + sq(3)\n",
            Some("f"),
        );
        assert_eq!(calls(&f, "sq"), 0, "{f}");
        let text = f.to_string();
        assert_eq!(text.matches("depth_check").count(), 2, "{text}");
        assert_eq!(text.matches("mul").count(), 2, "{text}");
    }

    #[test]
    fn recursive_and_calling_functions_are_not_inlined() {
        let source = "fn fact(n: int) -> int:\n    if n < 2:\n        ret 1\n    ret n * fact(n - 1)\n\
                      fn twice(n: int) -> int:\n    ret fact(n) + fact(n)\n\
                      fn f(n: int) -> int:\n    ret twice(n)\n";
        let f = inlined(source, Some("f"));
        assert_eq!(calls(&f, "twice"), 1, "{f}");
        let fact = inlined(source, Some("fact"));
        assert_eq!(calls(&fact, "fact"), 1, "{fact}");
    }

    #[test]
    fn a_function_whose_calls_were_inlined_is_inlined_in_turn() {
        let f = inlined(
            "fn sq(x: int) -> int:\n    ret x * x\nfn quad(x: int) -> int:\n    ret sq(sq(x))\n\
             fn f(a: int) -> int:\n    ret quad(a)\n",
            Some("f"),
        );
        assert_eq!(calls(&f, "quad") + calls(&f, "sq"), 0, "{f}");
    }

    #[test]
    fn a_method_that_changes_self_returns_its_fields_through_copies() {
        let source = "struct P:\n    x: float\n    y: float\nimpl P:\n    fn shift(self, d: float):\n        \
                      self.x += d\n        self.y -= d\n\
                      fn f(ps: list[P]) -> list[P]:\n    ps[0].shift(1.0)\n    ret ps\n";
        let f = inlined(source, Some("f"));
        assert_eq!(calls(&f, "P.shift"), 0, "{f}");
        let text = f.to_string();
        assert!(text.contains("fadd") && text.contains("fsub"), "{text}");
    }
}
