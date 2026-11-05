//! Removal of index checks a loop's own test proves: in `for i in range(xs.len()): s += xs[i]`,
//! `i` is never negative and always below the length, so `xs[i]` reads its slot unchecked.
//!
//! When the loop's bound is not the list's length, as in `for k in range(n): s += a[k] * b[k]`,
//! the loop is copied: one test before it compares `n` with each list's length once, and the
//! copy, with no checks, runs when `n` is at most every length, the loop as it was otherwise, so
//! a bound past a length still fails at the same index, after the same turns, as it would have.

use super::{
    BlockId, Function, Inst, Loop, Operand, Terminator, VReg, dominators, loops, never_negative,
    place_before, predecessors,
};
use crate::codegen::ir::inline::{rename_inst, retarget};
use crate::codegen::ir::liveness::block_liveness;
use crate::codegen::ir::{Block, Cond};
use std::collections::{HashMap, HashSet};

/// The most instructions a loop may have to be copied, so copies cost little code.
const MAX_COPIED_INSTS: usize = 64;

/// Removes the index checks one loop's test proves, as the module describes, and returns whether
/// it changed anything, so the caller runs it again until it does not.
pub(super) fn remove_range_checks(function: &mut Function) -> bool {
    let preds = predecessors(function);
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
    let natural = never_negative(function);
    for l in loops(function, &preds) {
        let Some(preheader) = l.preheader(function, &preds) else {
            continue;
        };
        // a call may change any list's length
        if l.insts(function).any(Inst::is_call) {
            continue;
        }
        let Some(test) = Test::of(&function.blocks[l.header].term, &l) else {
            continue;
        };
        let entry = test.entry.0;
        let fixed = |o: Operand| match o {
            Operand::Imm(_) => true,
            Operand::Reg(reg) => !l.writes(function, reg),
        };
        if entry == l.header
            || preds[entry] != [l.header]
            || !natural.contains(&test.counter)
            || !fixed(test.bound)
        {
            continue;
        }
        let latches: Vec<usize> = l
            .blocks()
            .filter(|&b| {
                function.blocks[b]
                    .term
                    .successors()
                    .contains(&BlockId(l.header))
            })
            .collect();
        let candidates = proven_reads(function, &l, &test, &dominates, &fixed);
        // reads by an index below the length the loop's bound was read as
        let length = bound_length(function, preheader, &test, &l);
        let (by_length, by_bound): (Vec<Candidate>, Vec<Candidate>) =
            candidates.into_iter().partition(|c| Some(c.list) == length);
        if !by_length.is_empty() {
            for c in &by_length {
                uncheck(&mut function.blocks[c.block].insts[c.index]);
            }
            return true;
        }
        // a read that runs on every turn of a loop that runs at all shows the list is there to
        // measure before the loop
        let every_turn: Vec<Candidate> = by_bound
            .into_iter()
            .filter(|c| latches.iter().all(|&latch| dominates(c.block, latch)))
            .collect();
        let small = l.insts(function).count() <= MAX_COPIED_INSTS;
        let innermost = l.blocks().all(|b| {
            function.blocks[b]
                .term
                .successors()
                .iter()
                .all(|s| !l.body[s.0] || s.0 == l.header || !dominates(s.0, b))
        });
        if every_turn.is_empty() || !small || !innermost {
            continue;
        }
        version(function, &l, preheader, &test, &every_turn);
        return true;
    }
    false
}

/// A loop's test: it goes on to `entry` while `counter < bound`, and leaves otherwise.
pub(super) struct Test {
    pub(super) counter: VReg,
    pub(super) bound: Operand,
    pub(super) entry: BlockId,
}

impl Test {
    /// Returns the test `term` makes, if it stays in `l` exactly while an int vreg is below a
    /// bound, in either order of operands and either order of ways.
    pub(super) fn of(term: &Terminator, l: &Loop) -> Option<Test> {
        let Terminator::CmpBranch {
            cond,
            float: false,
            lhs,
            rhs,
            then,
            otherwise,
        } = *term
        else {
            return None;
        };
        let (stays, entry) = match (l.body[then.0], l.body[otherwise.0]) {
            (true, false) => (cond, then),
            (false, true) => (cond.negate(), otherwise),
            _ => return None,
        };
        let (counter, bound) = match (stays, lhs, rhs) {
            (Cond::Lt, Operand::Reg(counter), bound) | (Cond::Gt, bound, Operand::Reg(counter)) => {
                (counter, bound)
            }
            _ => return None,
        };
        Some(Test {
            counter,
            bound,
            entry,
        })
    }
}

/// A checked read or store at `index` of `block`, of `list`, by an index the test proves is from
/// 0 to below its bound.
struct Candidate {
    block: usize,
    index: usize,
    list: VReg,
}

/// Returns the checked reads and stores in `l` whose index the test proves is from 0 to below its
/// bound, of lists the loop never writes but to make them unique, which keeps their lengths.
///
/// The counter is never negative, and below the bound on the way into the test's block, until
/// that block writes it. A vreg the block copies the counter into before writing it holds that
/// value wherever the copy comes first, if nothing else in the loop writes it, as the variable of
/// a `for` loop is, since the loop counts with a vreg of its own.
fn proven_reads(
    function: &Function,
    l: &Loop,
    test: &Test,
    dominates: &impl Fn(usize, usize) -> bool,
    fixed: &impl Fn(Operand) -> bool,
) -> Vec<Candidate> {
    let entry = test.entry.0;
    let writes_in_loop = |reg: VReg| {
        l.insts(function)
            .filter(|inst| inst.defs().contains(&reg))
            .count()
    };
    // the vregs that hold the counter's value from the test, and where each starts to
    let mut copies: HashMap<VReg, usize> = HashMap::new();
    let mut counter_until = function.blocks[entry].insts.len();
    for (i, inst) in function.blocks[entry].insts.iter().enumerate() {
        if inst.defs().contains(&test.counter) {
            counter_until = i;
            break;
        }
        if let Inst::Copy {
            dst,
            src: Operand::Reg(src),
        } = *inst
            && src == test.counter
            && writes_in_loop(dst) == 1
            && !function.params.contains(&dst)
        {
            copies.insert(dst, i);
        }
    }
    let proven = |block: usize, at: usize, index: Operand| -> bool {
        let Operand::Reg(reg) = index else {
            return false;
        };
        if reg == test.counter {
            return block == entry && at < counter_until;
        }
        match copies.get(&reg) {
            Some(&from) => {
                (block == entry && at > from) || (block != entry && dominates(entry, block))
            }
            None => false,
        }
    };
    let mut found = Vec::new();
    for b in l.blocks() {
        for (i, inst) in function.blocks[b].insts.iter().enumerate() {
            let (list, index) = match *inst {
                Inst::ListLoad { list, index, .. } => (list, index),
                Inst::ListStore {
                    old: None,
                    list,
                    index,
                    ..
                } => (list, index),
                _ => continue,
            };
            let Operand::Reg(list) = list else { continue };
            if fixed(Operand::Reg(list)) && proven(b, i, index) {
                found.push(Candidate {
                    block: b,
                    index: i,
                    list,
                });
            }
        }
    }
    found
}

/// Returns the list whose length the loop's bound is, when the block before the loop reads it as
/// the length last, with nothing after that could change either.
fn bound_length(function: &Function, preheader: usize, test: &Test, l: &Loop) -> Option<VReg> {
    let Operand::Reg(bound) = test.bound else {
        return None;
    };
    let insts = &function.blocks[preheader].insts;
    let at = insts
        .iter()
        .rposition(|inst| inst.defs().contains(&bound))?;
    let Inst::ListLen {
        list: Operand::Reg(list),
        ..
    } = insts[at]
    else {
        return None;
    };
    let after = &insts[at + 1..];
    let changes = |inst: &Inst| {
        inst.is_call()
            || inst.defs().contains(&bound)
            || (inst.defs().contains(&list)
                && !matches!(inst, Inst::ListUnique { dst, src: Operand::Reg(src), .. } if dst == src))
    };
    if after.iter().any(changes) || l.writes(function, list) {
        return None;
    }
    Some(list)
}

/// Turns a checked read or store into one of a slot known to be in range.
fn uncheck(inst: &mut Inst) {
    *inst = match *inst {
        Inst::ListLoad { dst, list, index } => Inst::ListGet {
            dst,
            list,
            index,
            offset: 0,
        },
        Inst::ListStore {
            old: None,
            list,
            index,
            value,
        } => Inst::ListSet {
            list,
            index,
            value,
            offset: 0,
        },
        _ => return,
    };
}

/// Copies `l` with `candidates` unchecked in the copy, and makes the way in from `preheader` test
/// the loop's bound against each candidate's list's length first: a bound at most every length
/// runs the copy, and anything else the loop as it was. The loop's own test comes first, so a
/// loop that never runs measures nothing.
///
/// The copy writes new vregs for what the loop only uses within a turn, and sits with the tests
/// right before the loop, so that neither loop's values live across the code between them.
fn version(
    function: &mut Function,
    l: &Loop,
    preheader: usize,
    test: &Test,
    candidates: &[Candidate],
) {
    let blocks: Vec<usize> = l.blocks().collect();
    // what the loop writes and nothing reads before writing it in the same turn or after it
    let (live_in, _) = block_liveness(function);
    let exits: Vec<usize> = blocks
        .iter()
        .flat_map(|&b| function.blocks[b].term.successors())
        .map(|BlockId(t)| t)
        .filter(|&t| !l.body[t])
        .collect();
    let mut fresh: HashMap<VReg, VReg> = HashMap::new();
    for inst in l.insts(function) {
        for def in inst.defs() {
            let local = !live_in[l.header].contains(def)
                && exits.iter().all(|&e| !live_in[e].contains(def))
                && !function.params.contains(&def);
            if local && !fresh.contains_key(&def) {
                fresh.insert(def, VReg(function.vreg_count + fresh.len() as u32));
            }
        }
    }
    function.vreg_count += fresh.len() as u32;
    // the lengths to compare, one block each, the last going into the copy
    let mut lists: Vec<VReg> = Vec::new();
    for c in candidates {
        if !lists.contains(&c.list) {
            lists.push(c.list);
        }
    }
    let base = function.blocks.len();
    let first_copy = base + lists.len();
    let copy_of: HashMap<usize, usize> = blocks
        .iter()
        .enumerate()
        .map(|(i, &b)| (b, first_copy + i))
        .collect();

    let span = function.blocks[l.header].term_span;
    let depth = function.blocks[preheader].loop_depth;
    for (i, &list) in lists.iter().enumerate() {
        let length = VReg(function.vreg_count);
        function.vreg_count += 1;
        let pass = if i + 1 == lists.len() {
            BlockId(copy_of[&l.header])
        } else {
            BlockId(base + i + 1)
        };
        function.blocks.push(Block {
            insts: vec![Inst::ListLen {
                dst: length,
                list: Operand::Reg(list),
            }],
            spans: vec![span],
            term: Terminator::CmpBranch {
                cond: Cond::Le,
                float: false,
                lhs: test.bound,
                rhs: Operand::Reg(length),
                then: pass,
                otherwise: BlockId(l.header),
            },
            term_span: span,
            loop_depth: depth,
        });
    }

    let unchecked: HashSet<(usize, usize)> =
        candidates.iter().map(|c| (c.block, c.index)).collect();
    let reg = |r: VReg| fresh.get(&r).copied().unwrap_or(r);
    let operand = |o: Operand| match o {
        Operand::Reg(r) => Operand::Reg(reg(r)),
        imm => imm,
    };
    for &b in &blocks {
        let mut block = function.blocks[b].clone();
        for (i, inst) in block.insts.iter_mut().enumerate() {
            if unchecked.contains(&(b, i)) {
                uncheck(inst);
            }
            *inst = rename_inst(inst, &reg, &operand);
        }
        block.term = retarget(
            &block.term,
            |BlockId(t)| BlockId(copy_of.get(&t).copied().unwrap_or(t)),
            operand,
        );
        function.blocks.push(block);
    }

    // the first turn's test, which measures only for a loop that runs
    let header_term = function.blocks[l.header].term.clone();
    let entry = test.entry;
    function.blocks[preheader].term = retarget(
        &header_term,
        |b| if b == entry { BlockId(base) } else { b },
        |o| o,
    );
    let placements: Vec<(usize, usize)> = (base..function.blocks.len())
        .map(|b| (b, l.header))
        .collect();
    place_before(function, &placements);
}
