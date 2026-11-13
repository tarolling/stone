//! Constant propagation and folding: an operation of constants becomes its result, as the
//! interpreter computes it, and a branch on constants becomes a jump, so code that can never run
//! is never reached.
//!
//! For example, `v2 = mul 3, 4` then `v0 = add v2, 1` becomes `v2 = copy 12` then
//! `v0 = copy 13`, and `br_gt 1, 2 -> b1, b2` becomes `jmp b2`.

use super::{BlockId, Function, Inst, Operand, Rounding, Terminator, VReg, predecessors};
use crate::codegen::ir::inline::{rename_inst, retarget};
use crate::codegen::ir::{BinOp, Cond};
use std::collections::HashMap;

/// The constant each vreg is known to hold at a point.
type Constants = HashMap<VReg, i64>;

/// Folds every operation of constants in `function` and every branch on them, and returns
/// whether it changed anything.
///
/// The constants a block starts with are those every block before it that can go to it ends
/// with, found by iterating to a fixed point from the guess that no block is reached but the
/// entry, so a block that only a folded branch leads to adds nothing, as in sparse conditional
/// constant propagation.
pub(super) fn fold_constants(function: &mut Function) -> bool {
    let preds = predecessors(function);
    let count = function.blocks.len();
    let mut outs: Vec<Option<Constants>> = vec![None; count];
    let entry = |b: usize, outs: &[Option<Constants>], function: &Function| -> Option<Constants> {
        if b == 0 {
            return Some(Constants::new());
        }
        let mut known: Option<Constants> = None;
        for &p in &preds[b] {
            let Some(out) = &outs[p] else { continue };
            if !successors(&function.blocks[p].term, out).contains(&BlockId(b)) {
                continue;
            }
            known = Some(match known {
                None => out.clone(),
                Some(mut known) => {
                    known.retain(|reg, value| out.get(reg) == Some(value));
                    known
                }
            });
        }
        known
    };
    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..count {
            let Some(mut constants) = entry(b, &outs, function) else {
                continue;
            };
            for inst in &function.blocks[b].insts {
                let inst = folded(inst, &constants);
                record(&mut constants, &inst);
            }
            if outs[b].as_ref() != Some(&constants) {
                outs[b] = Some(constants);
                changed = true;
            }
        }
    }
    let mut folded_any = false;
    for b in 0..count {
        // a block never reached stays as it is, for the cleanup of the graph to drop
        let Some(mut constants) = entry(b, &outs, function) else {
            continue;
        };
        let block = &mut function.blocks[b];
        for inst in &mut block.insts {
            let new = folded(inst, &constants);
            record(&mut constants, &new);
            if new != *inst {
                *inst = new;
                folded_any = true;
            }
        }
        let term = folded_term(&block.term, &constants);
        if term != block.term {
            block.term = term;
            folded_any = true;
        }
    }
    folded_any
}

/// Updates `constants` for running `inst`.
fn record(constants: &mut Constants, inst: &Inst) {
    for def in inst.defs() {
        constants.remove(&def);
    }
    if let Inst::Copy {
        dst,
        src: Operand::Imm(value),
    } = *inst
    {
        constants.insert(dst, value);
    }
}

/// Returns `operand` with a vreg known to hold a constant replaced by the constant.
fn known(operand: Operand, constants: &Constants) -> Operand {
    match operand {
        Operand::Reg(reg) => constants
            .get(&reg)
            .map_or(operand, |&value| Operand::Imm(value)),
        imm => imm,
    }
}

/// Returns `inst` computed, when its operands are known, or with the constants it reads written
/// in, when both backends take a constant there; otherwise `inst` itself.
fn folded(inst: &Inst, constants: &Constants) -> Inst {
    let substitutable = matches!(
        inst,
        Inst::Copy { .. }
            | Inst::Binary { .. }
            | Inst::FloatBinary { .. }
            | Inst::Neg { .. }
            | Inst::FloatNeg { .. }
            | Inst::FloatAbs { .. }
            | Inst::FloatSqrt { .. }
            | Inst::Not { .. }
            | Inst::Compare { .. }
            | Inst::IntToFloat { .. }
            | Inst::FloatToInt { checked: false, .. }
    );
    if !substitutable {
        return inst.clone();
    }
    let new = rename_inst(inst, &|reg| reg, &|operand| known(operand, constants));
    if let Some(value) = evaluate(&new) {
        return Inst::Copy {
            dst: new.defs()[0],
            src: Operand::Imm(value),
        };
    }
    // lowering writes literals into these, so both backends take a constant anywhere in them
    match new {
        Inst::Copy { .. }
        | Inst::Binary { .. }
        | Inst::FloatBinary { .. }
        | Inst::Compare { .. } => new,
        _ => inst.clone(),
    }
}

/// Returns `term` as a jump when constants decide where it goes, else with the constants it
/// reads written in where both backends take one.
fn folded_term(term: &Terminator, constants: &Constants) -> Terminator {
    if let Some(target) = decided(term, constants) {
        return Terminator::Jump(target);
    }
    match term {
        Terminator::CmpBranch { .. } | Terminator::Return(_) => {
            retarget(term, |b| b, |operand| known(operand, constants))
        }
        _ => term.clone(),
    }
}

/// Returns the blocks `term` can go to, given `constants`.
fn successors(term: &Terminator, constants: &Constants) -> Vec<BlockId> {
    match decided(term, constants) {
        Some(target) => vec![target],
        None => term.successors(),
    }
}

/// Returns where a branch goes when constants decide it.
fn decided(term: &Terminator, constants: &Constants) -> Option<BlockId> {
    let value = |operand: Operand| match known(operand, constants) {
        Operand::Imm(value) => Some(value),
        Operand::Reg(_) => None,
    };
    let (taken, then, otherwise) = match *term {
        Terminator::Branch {
            cond,
            then,
            otherwise,
        } => (value(cond)? != 0, then, otherwise),
        Terminator::CmpBranch {
            cond,
            float,
            lhs,
            rhs,
            then,
            otherwise,
        } => (
            holds(cond, float, value(lhs)?, value(rhs)?),
            then,
            otherwise,
        ),
        Terminator::TestBranch {
            value: tested,
            mask,
            then,
            otherwise,
        } => (value(tested)? & mask != 0, then, otherwise),
        _ => return None,
    };
    Some(if taken { then } else { otherwise })
}

/// Returns whether `cond` holds of `lhs` and `rhs`, as ints or as the bits of floats, where every
/// ordered comparison with nan is false.
fn holds(cond: Cond, float: bool, lhs: i64, rhs: i64) -> bool {
    if float {
        let (a, b) = (f64::from_bits(lhs as u64), f64::from_bits(rhs as u64));
        match cond {
            Cond::Eq => a == b,
            Cond::Ne => a != b,
            Cond::Lt => a < b,
            Cond::Le => a <= b,
            Cond::Gt => a > b,
            Cond::Ge => a >= b,
        }
    } else {
        match cond {
            Cond::Eq => lhs == rhs,
            Cond::Ne => lhs != rhs,
            Cond::Lt => lhs < rhs,
            Cond::Le => lhs <= rhs,
            Cond::Gt => lhs > rhs,
            Cond::Ge => lhs >= rhs,
        }
    }
}

/// Returns what `inst` writes when every operand is a constant and computing it can neither fail
/// nor give nan, exactly as the interpreter computes it.
///
/// For example, `v1 = div 7, -2` gives -3, while `v1 = div 1, 0` gives `None`, so the program
/// still stops there, and so does `v1 = fsub inf, inf`, whose nan's sign bit the processor picks.
pub(super) fn evaluate(inst: &Inst) -> Option<i64> {
    let float = |bits: i64| f64::from_bits(bits as u64);
    let bits = |value: f64| (!value.is_nan()).then_some(value.to_bits() as i64);
    match *inst {
        Inst::Copy {
            src: Operand::Imm(value),
            ..
        } => Some(value),
        Inst::Binary {
            op,
            lhs: Operand::Imm(a),
            rhs: Operand::Imm(b),
            ..
        } => match op {
            BinOp::Add => Some(a.wrapping_add(b)),
            BinOp::Sub => Some(a.wrapping_sub(b)),
            BinOp::Mul => Some(a.wrapping_mul(b)),
            BinOp::Div if b == 0 || (a == i64::MIN && b == -1) => None,
            BinOp::Div => Some(a / b),
            BinOp::Rem if b == 0 => None,
            BinOp::Rem if b == -1 => Some(0),
            BinOp::Rem => Some(a % b),
            BinOp::Pow if b < 0 => None,
            BinOp::Pow => Some(crate::stdlib::int_pow(a, b)),
            BinOp::DivUnchecked => None,
            BinOp::And => Some(a & b),
            BinOp::Or => Some(a | b),
            BinOp::Xor => Some(a ^ b),
            // the count's low 6 bits, as the processors take it
            BinOp::Shl => Some(a.wrapping_shl(b as u32)),
            BinOp::Shr => Some(a.wrapping_shr(b as u32)),
        },
        Inst::FloatBinary {
            op,
            lhs: Operand::Imm(a),
            rhs: Operand::Imm(b),
            ..
        } => {
            let (a, b) = (float(a), float(b));
            match op {
                BinOp::Add => bits(a + b),
                BinOp::Sub => bits(a - b),
                BinOp::Mul => bits(a * b),
                BinOp::Div if b == 0.0 => None,
                BinOp::Div | BinOp::DivUnchecked => bits(a / b),
                // the remainder and power take their own routines, not folded here, and floats
                // have no bitwise operations
                BinOp::Rem
                | BinOp::Pow
                | BinOp::And
                | BinOp::Or
                | BinOp::Xor
                | BinOp::Shl
                | BinOp::Shr => None,
            }
        }
        Inst::Neg {
            src: Operand::Imm(a),
            ..
        } => Some(a.wrapping_neg()),
        Inst::FloatNeg {
            src: Operand::Imm(a),
            ..
        } => Some(a ^ i64::MIN),
        Inst::FloatAbs {
            src: Operand::Imm(a),
            ..
        } => Some(a & i64::MAX),
        Inst::FloatSqrt {
            src: Operand::Imm(a),
            ..
        } => bits(float(a).sqrt()),
        Inst::Not {
            src: Operand::Imm(a),
            ..
        } => Some((a == 0) as i64),
        Inst::Compare {
            cond,
            float,
            lhs: Operand::Imm(a),
            rhs: Operand::Imm(b),
            ..
        } => Some(holds(cond, float, a, b) as i64),
        Inst::IntToFloat {
            src: Operand::Imm(a),
            ..
        } => Some((a as f64).to_bits() as i64),
        Inst::FloatToInt {
            src: Operand::Imm(a),
            checked: false,
            rounding,
            ..
        } => {
            let value = float(a);
            let whole = match rounding {
                Rounding::Zero => value.trunc(),
                Rounding::Down => value.floor(),
            };
            // -2^63 <= whole < 2^63, which nan never is
            let limit = 2f64.powi(63);
            (-limit..limit).contains(&whole).then_some(whole as i64)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn binary(op: BinOp, a: i64, b: i64) -> Option<i64> {
        evaluate(&Inst::Binary {
            op,
            dst: VReg(0),
            lhs: Operand::Imm(a),
            rhs: Operand::Imm(b),
        })
    }

    fn float_binary(op: BinOp, a: f64, b: f64) -> Option<f64> {
        evaluate(&Inst::FloatBinary {
            op,
            dst: VReg(0),
            lhs: Operand::Imm(a.to_bits() as i64),
            rhs: Operand::Imm(b.to_bits() as i64),
        })
        .map(|bits| f64::from_bits(bits as u64))
    }

    #[test]
    fn ints_fold_as_the_interpreter_computes_them() {
        assert_eq!(binary(BinOp::Add, i64::MAX, 1), Some(i64::MIN));
        assert_eq!(binary(BinOp::Mul, i64::MAX, 2), Some(-2));
        assert_eq!(binary(BinOp::Div, -7, 2), Some(-3));
        assert_eq!(binary(BinOp::Rem, -7, 2), Some(-1));
        assert_eq!(binary(BinOp::Rem, i64::MIN, -1), Some(0));
        assert_eq!(binary(BinOp::Pow, 2, 64), Some(0));
        assert_eq!(binary(BinOp::Pow, 3, 4), Some(81));
    }

    #[test]
    fn what_would_fail_is_left_to_fail() {
        assert_eq!(binary(BinOp::Div, 1, 0), None);
        assert_eq!(binary(BinOp::Rem, 1, 0), None);
        assert_eq!(binary(BinOp::Div, i64::MIN, -1), None);
        assert_eq!(binary(BinOp::Pow, 2, -1), None);
        assert_eq!(float_binary(BinOp::Div, 1.0, 0.0), None);
        assert_eq!(float_binary(BinOp::Div, 1.0, -0.0), None);
    }

    #[test]
    fn floats_fold_unless_they_give_nan() {
        assert_eq!(float_binary(BinOp::Add, 0.1, 0.2), Some(0.1 + 0.2));
        assert_eq!(float_binary(BinOp::Div, 1.0, 3.0), Some(1.0 / 3.0));
        assert_eq!(
            float_binary(BinOp::DivUnchecked, 1.0, 0.0),
            Some(f64::INFINITY)
        );
        assert_eq!(float_binary(BinOp::Sub, f64::INFINITY, f64::INFINITY), None);
        assert_eq!(float_binary(BinOp::Rem, 5.0, 3.0), None);
    }

    #[test]
    fn conversions_fold_only_in_range() {
        let to_int = |value: f64, rounding| {
            evaluate(&Inst::FloatToInt {
                dst: VReg(0),
                src: Operand::Imm(value.to_bits() as i64),
                checked: false,
                rounding,
            })
        };
        assert_eq!(to_int(-2.5, Rounding::Zero), Some(-2));
        assert_eq!(to_int(-2.5, Rounding::Down), Some(-3));
        assert_eq!(to_int(9.3e18, Rounding::Zero), None);
        assert_eq!(to_int(f64::NAN, Rounding::Zero), None);
        assert_eq!(to_int(-(2f64.powi(63)), Rounding::Zero), Some(i64::MIN));
    }

    #[test]
    fn float_comparisons_with_nan_are_false_but_not_equal() {
        let nan = f64::NAN.to_bits() as i64;
        assert!(!holds(Cond::Eq, true, nan, nan));
        assert!(holds(Cond::Ne, true, nan, nan));
        assert!(!holds(Cond::Lt, true, nan, 0));
        assert!(holds(Cond::Lt, false, -1, 0));
    }
}
