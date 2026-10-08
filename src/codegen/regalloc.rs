//! Linear-scan register allocation over [`liveness`](crate::codegen::ir::liveness) intervals, and
//! ordering of parallel moves.
//!
//! Both are independent of the target: registers are any `Copy + Eq` type, such as the x64 or
//! arm64 backend's register names.

use crate::codegen::ir::liveness::Interval;
use crate::codegen::ir::{BinOp, Function, Inst, Operand, VReg};
use std::collections::HashMap;

/// Where a vreg lives for its whole interval: a register, or a numbered spill slot in memory.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Location<R> {
    Reg(R),
    Stack(usize),
}

/// A register a vreg would like, so that a move can be left out.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Hint<R> {
    /// A specific register, such as the argument register a value is passed in.
    Reg(R),
    /// Whatever register another vreg got, such as the source of a copy.
    Like(VReg),
}

/// The result of [`linear_scan`].
#[derive(Clone, Debug, PartialEq)]
pub struct Allocation<R> {
    pub locations: HashMap<VReg, Location<R>>,
    /// How many spill slots were handed out, numbered from 0.
    pub spill_slots: usize,
    /// The callee-saved registers the allocation uses, which the function must save and restore,
    /// in the order they were given.
    pub used_callee_saved: Vec<R>,
}

/// Returns the registers each vreg would like, so that values are computed where they are needed.
///
/// A parameter would like the register its argument arrives in, and a call argument the register
/// it is passed in. A copy and its source, and the result of a two-address instruction and its
/// first input, would like to share a register, so the move between them disappears. For example,
/// in `v1 = sub v0, 1; call f(v1)`, `v1` would like the first of `arg_regs`, such as `rdi` on
/// x86-64.
pub fn hints<R: Copy>(function: &Function, arg_regs: &[R]) -> HashMap<VReg, Vec<Hint<R>>> {
    let mut hints: HashMap<VReg, Vec<Hint<R>>> = HashMap::new();
    let mut add = |reg: VReg, hint| hints.entry(reg).or_default().push(hint);
    for (param, reg) in function.params.iter().zip(arg_regs) {
        add(*param, Hint::Reg(*reg));
    }
    for inst in function.blocks.iter().flat_map(|block| &block.insts) {
        match inst {
            Inst::Call { args, .. } => {
                for (arg, reg) in args.iter().zip(arg_regs) {
                    if let Operand::Reg(arg) = arg {
                        add(*arg, Hint::Reg(*reg));
                    }
                }
            }
            Inst::Copy {
                dst,
                src: Operand::Reg(src),
            } => {
                add(*dst, Hint::Like(*src));
                add(*src, Hint::Like(*dst));
            }
            Inst::Binary {
                op: BinOp::Add | BinOp::Sub | BinOp::Mul,
                dst,
                lhs,
                rhs,
            } => {
                if let Operand::Reg(lhs) = lhs {
                    add(*dst, Hint::Like(*lhs));
                }
                if let Operand::Reg(rhs) = rhs {
                    add(*dst, Hint::Like(*rhs));
                }
            }
            Inst::Neg {
                dst,
                src: Operand::Reg(src),
            }
            | Inst::FloatNeg {
                dst,
                src: Operand::Reg(src),
            } => {
                add(*dst, Hint::Like(*src));
            }
            _ => {}
        }
    }
    // a fixed register saves a move at the call or entry, so it comes before a shared one
    for list in hints.values_mut() {
        list.sort_by_key(|hint| matches!(hint, Hint::Like(_)));
    }
    hints
}

/// Assigns each interval a register or a spill slot, in the style of Poletto and Sarkar.
///
/// Intervals that cross a call may only use `callee_saved` registers. Others try `caller_saved`
/// first, then `callee_saved`. When no allowed register is free, whichever of the new interval and
/// the lightest active interval holding an allowed register weighs less goes to memory for its
/// whole life. For example, with one callee-saved register and two intervals that both cross a
/// call and overlap, the heavier one gets the register and the other gets spill slot 0.
///
/// A free register that `hints` lists for the vreg is taken before any other, in the order
/// listed, as long as the vreg may use it.
///
/// `intervals` must be sorted by start, as [`analyze`](crate::codegen::ir::liveness::analyze)
/// returns them.
pub fn linear_scan<R: Copy + Eq>(
    intervals: &[Interval],
    callee_saved: &[R],
    caller_saved: &[R],
    hints: &HashMap<VReg, Vec<Hint<R>>>,
) -> Allocation<R> {
    let mut locations = HashMap::new();
    let mut spill_slots = 0;
    // (index into intervals, register), for intervals in registers that have not ended
    let mut active: Vec<(usize, R)> = Vec::new();
    let mut used = vec![false; callee_saved.len()];

    let mut spill = |locations: &mut HashMap<VReg, Location<R>>, vreg: VReg| {
        locations.insert(vreg, Location::Stack(spill_slots));
        spill_slots += 1;
    };

    for (i, current) in intervals.iter().enumerate() {
        active.retain(|&(j, _)| intervals[j].end >= current.start);

        let allowed: Vec<R> = if current.crosses_call {
            callee_saved.to_vec()
        } else {
            caller_saved.iter().chain(callee_saved).copied().collect()
        };

        let is_free = |reg: &R| allowed.contains(reg) && !active.iter().any(|(_, r)| r == reg);
        let hinted = hints
            .get(&current.vreg)
            .into_iter()
            .flatten()
            .find_map(|hint| {
                let reg = match hint {
                    Hint::Reg(reg) => *reg,
                    Hint::Like(other) => match locations.get(other) {
                        Some(Location::Reg(reg)) => *reg,
                        _ => return None,
                    },
                };
                is_free(&reg).then_some(reg)
            });
        let free = hinted.or_else(|| allowed.iter().find(|reg| is_free(reg)).copied());
        let reg = match free {
            Some(reg) => Some(reg),
            None => {
                let victim = active
                    .iter()
                    .enumerate()
                    .filter(|(_, (_, reg))| allowed.contains(reg))
                    .min_by(|(_, (a, _)), (_, (b, _))| {
                        intervals[*a].weight.total_cmp(&intervals[*b].weight)
                    })
                    .map(|(slot, &(j, reg))| (slot, j, reg));
                match victim {
                    Some((slot, j, reg)) if intervals[j].weight < current.weight => {
                        active.remove(slot);
                        spill(&mut locations, intervals[j].vreg);
                        Some(reg)
                    }
                    _ => None,
                }
            }
        };

        match reg {
            Some(reg) => {
                if let Some(k) = callee_saved.iter().position(|r| *r == reg) {
                    used[k] = true;
                }
                locations.insert(current.vreg, Location::Reg(reg));
                active.push((i, reg));
            }
            None => spill(&mut locations, current.vreg),
        }
    }

    let used_callee_saved = callee_saved
        .iter()
        .zip(&used)
        .filter(|(_, used)| **used)
        .map(|(reg, _)| *reg)
        .collect();
    Allocation {
        locations,
        spill_slots,
        used_callee_saved,
    }
}

/// Orders moves that conceptually happen at once, `(destination, source)`, into a sequence that
/// never overwrites a source before reading it, breaking cycles through `temp`.
///
/// Destinations must be distinct, and `temp` must be neither a destination nor a source. For
/// example, swapping `a` and `b` with `[(a, b), (b, a)]` becomes `[(t, a), (a, b), (b, t)]`.
pub fn parallel_moves<L: Copy + Eq>(moves: &[(L, L)], temp: L) -> Vec<(L, L)> {
    let mut pending: Vec<(L, L)> = moves.iter().copied().filter(|(d, s)| d != s).collect();
    let mut sequence = Vec::new();
    while !pending.is_empty() {
        // a move is safe once no other pending move still reads its destination
        let ready = pending
            .iter()
            .position(|(dst, _)| !pending.iter().any(|(_, src)| src == dst));
        match ready {
            Some(i) => sequence.push(pending.remove(i)),
            None => {
                // only cycles remain, so save one destination and read it from temp instead
                let (dst, _) = pending[0];
                sequence.push((temp, dst));
                for (_, src) in pending.iter_mut() {
                    if *src == dst {
                        *src = temp;
                    }
                }
            }
        }
    }
    sequence
}

#[cfg(test)]
mod tests {
    use super::*;

    fn interval(vreg: u32, start: u32, end: u32, weight: f64, crosses_call: bool) -> Interval {
        Interval {
            vreg: VReg(vreg),
            start,
            end,
            weight,
            crosses_call,
        }
    }

    #[test]
    fn intervals_that_cross_calls_only_get_callee_saved_registers() {
        let intervals = [
            interval(0, 0, 10, 1.0, true),
            interval(1, 0, 10, 1.0, false),
        ];
        let allocation = linear_scan(&intervals, &["rbx"], &["rsi"], &HashMap::new());
        assert_eq!(allocation.locations[&VReg(0)], Location::Reg("rbx"));
        assert_eq!(allocation.locations[&VReg(1)], Location::Reg("rsi"));
        assert_eq!(allocation.used_callee_saved, ["rbx"]);
    }

    #[test]
    fn the_lighter_interval_is_spilled() {
        let intervals = [
            interval(0, 0, 20, 1.0, true),
            interval(1, 2, 10, 100.0, true),
            interval(2, 4, 8, 0.5, true),
        ];
        let allocation = linear_scan(&intervals, &["rbx"], &["rsi"], &HashMap::new());
        assert_eq!(allocation.locations[&VReg(0)], Location::Stack(0));
        assert_eq!(allocation.locations[&VReg(1)], Location::Reg("rbx"));
        assert_eq!(allocation.locations[&VReg(2)], Location::Stack(1));
        assert_eq!(allocation.spill_slots, 2);
    }

    #[test]
    fn a_result_reuses_the_register_of_an_input_it_reads_last() {
        // v0 is last read at 2, and v1 is written at 3
        let intervals = [interval(0, 0, 2, 1.0, false), interval(1, 3, 4, 1.0, false)];
        let allocation = linear_scan(&intervals, &[], &["rsi"], &HashMap::new());
        assert_eq!(allocation.locations[&VReg(0)], Location::Reg("rsi"));
        assert_eq!(allocation.locations[&VReg(1)], Location::Reg("rsi"));
    }

    #[test]
    fn running_out_of_registers_spills_instead_of_failing() {
        let intervals: Vec<Interval> = (0..20).map(|i| interval(i, i, 100, 1.0, true)).collect();
        let allocation = linear_scan::<&str>(&intervals, &[], &[], &HashMap::new());
        assert_eq!(allocation.spill_slots, 20);
        assert!(allocation.used_callee_saved.is_empty());
    }

    #[test]
    fn a_hinted_register_is_preferred_when_free() {
        let intervals = [interval(0, 0, 4, 1.0, false)];
        let hints = HashMap::from([(VReg(0), vec![Hint::Reg("rdi")])]);
        let allocation = linear_scan(&intervals, &["rbx"], &["rsi", "rdi"], &hints);
        assert_eq!(allocation.locations[&VReg(0)], Location::Reg("rdi"));
    }

    #[test]
    fn a_hint_that_is_not_allowed_or_not_free_is_ignored() {
        let intervals = [interval(0, 0, 4, 1.0, true), interval(1, 1, 4, 1.0, false)];
        let hints = HashMap::from([
            (VReg(0), vec![Hint::Reg("rdi")]),
            (VReg(1), vec![Hint::Reg("rbx")]),
        ]);
        let allocation = linear_scan(&intervals, &["rbx"], &["rsi", "rdi"], &hints);
        assert_eq!(allocation.locations[&VReg(0)], Location::Reg("rbx"));
        assert_eq!(allocation.locations[&VReg(1)], Location::Reg("rsi"));
    }

    #[test]
    fn a_copy_takes_the_register_its_source_frees() {
        // v1 = copy v0, where v0 dies at the copy
        let intervals = [interval(0, 0, 2, 1.0, false), interval(1, 3, 6, 1.0, false)];
        let hints = HashMap::from([
            (VReg(0), vec![Hint::Reg("rdi")]),
            (VReg(1), vec![Hint::Like(VReg(0))]),
        ]);
        let allocation = linear_scan(&intervals, &[], &["rsi", "rdi"], &hints);
        assert_eq!(allocation.locations[&VReg(1)], Location::Reg("rdi"));
    }

    #[test]
    fn a_swap_goes_through_the_temporary() {
        assert_eq!(
            parallel_moves(&[("rdi", "rsi"), ("rsi", "rdi")], "rax"),
            [("rax", "rdi"), ("rdi", "rsi"), ("rsi", "rax")]
        );
    }

    #[test]
    fn a_chain_moves_each_value_before_overwriting_it() {
        // rdi <- rsi <- rdx, which needs rsi moved into rdi first
        assert_eq!(
            parallel_moves(&[("rsi", "rdx"), ("rdi", "rsi"), ("rcx", "rcx")], "rax"),
            [("rdi", "rsi"), ("rsi", "rdx")]
        );
    }

    #[test]
    fn two_cycles_both_resolve() {
        let moves = [("a", "b"), ("b", "a"), ("c", "d"), ("d", "c"), ("e", "a")];
        let sequence = parallel_moves(&moves, "t");
        // simulate the moves on registers holding their own names
        let mut values: HashMap<&str, &str> = ["a", "b", "c", "d", "e", "t"]
            .iter()
            .map(|r| (*r, *r))
            .collect();
        for (dst, src) in &sequence {
            let value = values[src];
            values.insert(dst, value);
        }
        for (dst, src) in moves {
            assert_eq!(values[dst], src, "{sequence:?}");
        }
    }
}
