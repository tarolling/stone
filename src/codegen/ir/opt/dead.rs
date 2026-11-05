//! Removal of work nothing needs: instructions whose results are never read, or read only by
//! other such instructions, and whole loops that change nothing anything after them reads and
//! are sure to end.
//!
//! For example, in `z = 0` followed by `for i in range(n): z += i` with `z` never read again,
//! the additions only feed each other, so they go, and then the loop only counts, so it goes too.

use super::{BinOp, BlockId, Cond, Function, Inst, Loop, Operand, Terminator, VReg};
use super::{dominators, is_removable, loops, predecessors};
use crate::codegen::ir::inline::retarget;
use crate::codegen::ir::liveness::{Set, block_liveness};
use std::collections::{HashMap, HashSet};

/// Removes every instruction whose removal changes nothing but the vregs it writes, when nothing
/// needs what it writes, and returns whether it removed any.
///
/// A vreg is needed when an instruction that cannot be removed, or a terminator, reads it, or an
/// instruction writing a needed vreg reads it, so a cycle of additions that nothing else reads
/// goes as a whole. Then a write that nothing reads before the next write of the same vreg goes
/// too, since a vreg written in two places may be needed after one and not after the other.
pub(super) fn remove_dead(function: &mut Function) -> bool {
    let mut needed: HashSet<VReg> = HashSet::new();
    let mut writers: HashMap<VReg, Vec<Vec<VReg>>> = HashMap::new();
    for block in &function.blocks {
        for inst in &block.insts {
            if is_removable(inst) {
                for def in inst.defs() {
                    writers.entry(def).or_default().push(inst.uses());
                }
            } else {
                needed.extend(inst.uses());
            }
        }
        needed.extend(block.term.uses());
    }
    let mut work: Vec<VReg> = needed.iter().copied().collect();
    while let Some(reg) = work.pop() {
        for uses in writers.get(&reg).into_iter().flatten() {
            for &used in uses {
                if needed.insert(used) {
                    work.push(used);
                }
            }
        }
    }
    let (_, live_out) = block_liveness(function);
    let mut removed = false;
    for (block, live_out) in function.blocks.iter_mut().zip(live_out) {
        let mut live: Set = live_out;
        for reg in block.term.uses() {
            live.insert(reg);
        }
        // walk backward, keeping what is live after each instruction
        let mut keep = vec![true; block.insts.len()];
        for (i, inst) in block.insts.iter().enumerate().rev() {
            let defs = inst.defs();
            if is_removable(inst)
                && defs
                    .iter()
                    .all(|def| !needed.contains(def) || !live.contains(*def))
            {
                keep[i] = false;
                continue;
            }
            for def in defs {
                live.remove(def);
            }
            for reg in inst.uses() {
                live.insert(reg);
            }
        }
        if keep.iter().all(|k| *k) {
            continue;
        }
        removed = true;
        let (insts, spans) = (
            std::mem::take(&mut block.insts),
            std::mem::take(&mut block.spans),
        );
        for ((inst, span), kept) in insts.into_iter().zip(spans).zip(keep) {
            if kept {
                block.insts.push(inst);
                block.spans.push(span);
            }
        }
    }
    removed
}

/// Removes one loop that does nothing anything after it needs and is sure to end, by sending
/// every way into it to where it leaves, and returns whether it did.
///
/// A loop qualifies when every instruction in it [`is_removable`], it only branches (no `ret` or
/// `fail`), every way out goes to the same block, no vreg it writes is live there, it holds no
/// other loop, and it counts: one vreg goes up or down by exactly 1 once a turn, and a test it
/// runs every turn stays in the loop only while the vreg is below (or above) a bound the loop
/// never changes (or only reads again as the length of a list it never writes), or differs from
/// it. Going by 1, the vreg reaches the bound, wrapping around if
/// it must, so the loop ends. `<=` and `>=` qualify only against a constant other than the
/// largest and smallest int, which the vreg could never pass. Every `for` loop over `range`
/// counts up by 1 this way.
pub(super) fn remove_dead_loops(function: &mut Function) -> bool {
    let preds = predecessors(function);
    let (live_in, _) = block_liveness(function);
    let idom = dominators(function, &preds);
    let dominates = |a: usize, mut b: usize| loop {
        if a == b {
            return true;
        }
        match idom[b] {
            Some(up) if up != b => b = up,
            _ => return false,
        }
    };
    for l in loops(function, &preds) {
        let Some(exit) = dead_loop_exit(function, &l, &live_in, &dominates) else {
            continue;
        };
        for &p in &preds[l.header] {
            if l.body[p] {
                continue;
            }
            let block = &mut function.blocks[p];
            block.term = retarget(
                &block.term,
                |b| {
                    if b == BlockId(l.header) {
                        BlockId(exit)
                    } else {
                        b
                    }
                },
                |o| o,
            );
        }
        return true;
    }
    false
}

/// Returns the one block `l` leaves to, when the loop can be removed as [`remove_dead_loops`]
/// describes.
fn dead_loop_exit(
    function: &Function,
    l: &Loop,
    live_in: &[Set],
    dominates: &impl Fn(usize, usize) -> bool,
) -> Option<usize> {
    let blocks: Vec<usize> = l.blocks().collect();
    if !l.insts(function).all(is_removable) {
        return None;
    }
    let mut exit = None;
    for &b in &blocks {
        let term = &function.blocks[b].term;
        if matches!(term, Terminator::Return(_) | Terminator::Fail(..)) {
            return None;
        }
        for BlockId(succ) in term.successors() {
            if l.body[succ] {
                // a way back to a block other than the header is an inner loop
                if succ != l.header && dominates(succ, b) {
                    return None;
                }
            } else if *exit.get_or_insert(succ) != succ {
                return None;
            }
        }
    }
    let exit = exit?;
    let written: HashSet<VReg> = l.insts(function).flat_map(|inst| inst.defs()).collect();
    if written.iter().any(|&reg| live_in[exit].contains(reg)) {
        return None;
    }
    let latches: Vec<usize> = blocks
        .iter()
        .copied()
        .filter(|&b| {
            function.blocks[b]
                .term
                .successors()
                .contains(&BlockId(l.header))
        })
        .collect();
    let every_turn = |b: usize| latches.iter().all(|&latch| dominates(b, latch));
    // a vreg the loop changes by `step` once a turn, and nothing else writes
    let step_of = |reg: VReg| -> Option<i64> {
        let mut step = None;
        for &b in &blocks {
            for inst in &function.blocks[b].insts {
                if !inst.defs().contains(&reg) {
                    continue;
                }
                let by = match *inst {
                    Inst::Binary {
                        op: BinOp::Add,
                        lhs: Operand::Reg(r),
                        rhs: Operand::Imm(k),
                        ..
                    }
                    | Inst::Binary {
                        op: BinOp::Add,
                        lhs: Operand::Imm(k),
                        rhs: Operand::Reg(r),
                        ..
                    } if r == reg => k,
                    Inst::Binary {
                        op: BinOp::Sub,
                        lhs: Operand::Reg(r),
                        rhs: Operand::Imm(k),
                        ..
                    } if r == reg => k.wrapping_neg(),
                    _ => return None,
                };
                if step.is_some() || (by != 1 && by != -1) || !every_turn(b) {
                    return None;
                }
                step = Some(by);
            }
        }
        step
    };
    let ends = blocks.iter().any(|&b| {
        let Terminator::CmpBranch {
            cond,
            float: false,
            lhs,
            rhs,
            then,
            otherwise,
        } = function.blocks[b].term
        else {
            return false;
        };
        if !every_turn(b) {
            return false;
        }
        // the condition under which the loop goes on
        let stays = match (l.body[then.0], l.body[otherwise.0]) {
            (true, false) => cond,
            (false, true) => cond.negate(),
            _ => return false,
        };
        // nothing in the loop stores or calls, so a length it reads again stays the same
        let fixed = |o: Operand| match o {
            Operand::Imm(_) => true,
            Operand::Reg(reg) => {
                !written.contains(&reg)
                    || l.insts(function)
                        .filter(|inst| inst.defs().contains(&reg))
                        .all(|inst| match *inst {
                            Inst::ListLen {
                                list: Operand::Reg(list),
                                ..
                            }
                            | Inst::StrLen {
                                text: Operand::Reg(list),
                                ..
                            } => !written.contains(&list),
                            _ => false,
                        })
            }
        };
        let (counter, stays, bound) = match (lhs, rhs) {
            (Operand::Reg(c), bound) if written.contains(&c) && fixed(bound) => (c, stays, bound),
            (bound, Operand::Reg(c)) if written.contains(&c) && fixed(bound) => {
                (c, stays.swap(), bound)
            }
            _ => return false,
        };
        let Some(step) = step_of(counter) else {
            return false;
        };
        match (stays, step, bound) {
            (Cond::Ne, _, _) | (Cond::Lt, 1, _) | (Cond::Gt, -1, _) => true,
            (Cond::Le, 1, Operand::Imm(k)) => k != i64::MAX,
            (Cond::Ge, -1, Operand::Imm(k)) => k != i64::MIN,
            _ => false,
        }
    });
    ends.then_some(exit)
}
