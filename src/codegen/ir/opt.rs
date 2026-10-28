//! Optimizations over lowered IR that remove work the lowering of each statement on its own
//! repeats: checking an index into a list again, reading an element again, and making a list
//! unique again.
//!
//! Lowering checks every index and makes every list it changes unique, statement by statement, so
//! `bodies[i].vx -= dx * m` followed by `bodies[i].vy -= dy * m` checks `i` and the uniqueness of
//! `bodies` twice, and does it again on every turn of a loop. [`optimize`] runs these passes over
//! each function:
//!
//! 1. A `list_unique` of a variable inside a loop that cannot make the list shared again moves
//!    before the loop, as a `list_unique_or_null`, since the variable may be null when the loop
//!    never runs. Copying a shared list early is invisible, since lists are values.
//! 2. An `element_slot`, or a `list_load` in a loop that changes no list, whose list and index the
//!    loop never changes, and which every turn runs before anything that could fail or call out,
//!    runs once before the loop, behind a copy of the loop's test, so it fails exactly when the
//!    first turn would, and not at all for a loop that never runs. The ones inside the loop
//!    become copies of its result.
//! 3. Within a function, an `element_slot` or `list_load` already done on every path to it
//!    becomes a copy of the earlier result, and a `list_unique` of a list already unique is
//!    dropped, as long as nothing between them could change the list or share it.
//! 4. A slot that is a constant past another, such as a field of an element at `add v4, 3`, is
//!    read and written as an offset from it (`list_get v0, v4 + 3`), and work whose result
//!    nothing reads is dropped.
//! 5. Each loop tests at the bottom as well as on entry (see [`rotate_loops`]), and then a list
//!    the loop reads and writes without moving its elements has its elements' address loaded
//!    once on the way in (see [`hoist_data`]).
//!
//! The IR is not SSA, so every fact is about vregs and dies when any vreg it names is written.

use super::inline::{rename_inst, retarget};
use super::{BinOp, Block, BlockId, Cond, Function, Inst, Operand, Program, Terminator, VReg};
use std::collections::{HashMap, HashSet};

/// Runs every optimization over each function of `program`.
pub fn optimize(program: &mut Program) {
    for function in &mut program.functions {
        while forward_copies(function) {}
        test_remainders(function);
        while hoist_unique(function) {}
        while hoist_checks(function) {}
        eliminate(function);
        fold_offsets(function);
        remove_dead(function);
        rotate_loops(function);
        hoist_data(function);
    }
}

/// Something known to hold at a point of a function.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Fact {
    /// `slot` holds where element `index` of `list` starts, already checked.
    Slot {
        list: VReg,
        index: Operand,
        width: usize,
        slot: VReg,
    },
    /// Nothing but this vreg refers to its list.
    Unique(VReg),
    /// `value` holds element `index` of `list`, already checked and read.
    Loaded {
        list: VReg,
        index: Operand,
        value: VReg,
    },
}

impl Fact {
    fn mentions(&self, reg: VReg) -> bool {
        match self {
            Fact::Slot {
                list, index, slot, ..
            } => *list == reg || *slot == reg || *index == Operand::Reg(reg),
            Fact::Unique(list) => *list == reg,
            Fact::Loaded { list, index, value } => {
                *list == reg || *value == reg || *index == Operand::Reg(reg)
            }
        }
    }
}

/// Returns whether `inst` makes the list in `list` unique, where it already is.
fn is_unique_of_itself(inst: &Inst) -> Option<VReg> {
    match inst {
        Inst::ListUnique { dst, src, .. } if *src == Operand::Reg(*dst) => Some(*dst),
        _ => None,
    }
}

/// Updates `facts` for running `inst`.
///
/// A call may change any list's length, since the runtime's list routines are calls. A retain
/// may share any list, since its vreg may hold the same list as another. Making a list unique
/// keeps what is known of its elements, since a copy has the same length.
fn transfer(facts: &mut HashSet<Fact>, inst: &Inst) {
    // what was read from a list may have changed, or been freed
    let changes_elements = matches!(
        inst,
        Inst::ListStore { .. }
            | Inst::ListSet { .. }
            | Inst::DataSet { .. }
            | Inst::ListInit { .. }
            | Inst::Packed { .. }
            | Inst::FieldInit { .. }
            | Inst::ListUnique { .. }
            | Inst::Release { .. }
    );
    if changes_elements {
        facts.retain(|fact| !matches!(fact, Fact::Loaded { .. }));
    }
    match inst {
        Inst::Call { .. } => facts.clear(),
        Inst::Retain { .. } => facts.retain(|fact| !matches!(fact, Fact::Unique(_))),
        Inst::Release {
            src: Operand::Reg(reg),
            ..
        } => facts.retain(|fact| !fact.mentions(*reg)),
        _ => {}
    }
    if let Some(list) = is_unique_of_itself(inst) {
        facts.insert(Fact::Unique(list));
        return;
    }
    for def in inst.defs() {
        facts.retain(|fact| !fact.mentions(def));
    }
    if let Inst::ElementSlot {
        dst,
        list: Operand::Reg(list),
        index,
        width,
    } = inst
        && dst != list
        && *index != Operand::Reg(*dst)
    {
        facts.insert(Fact::Slot {
            list: *list,
            index: *index,
            width: *width,
            slot: *dst,
        });
    }
    if let Inst::ListLoad {
        dst,
        list: Operand::Reg(list),
        index,
    } = inst
        && dst != list
        && *index != Operand::Reg(*dst)
    {
        facts.insert(Fact::Loaded {
            list: *list,
            index: *index,
            value: *dst,
        });
    }
}

/// Returns the value an earlier read of the same element left in `facts`, if any.
fn loaded_value(facts: &HashSet<Fact>, list: VReg, index: Operand) -> Option<VReg> {
    facts.iter().find_map(|fact| match fact {
        Fact::Loaded {
            list: l,
            index: i,
            value,
        } if *l == list && *i == index => Some(*value),
        _ => None,
    })
}

/// Returns the slot an earlier check of the same element left in `facts`, if any.
fn checked_slot(facts: &HashSet<Fact>, list: VReg, index: Operand, width: usize) -> Option<VReg> {
    facts.iter().find_map(|fact| match fact {
        Fact::Slot {
            list: l,
            index: i,
            width: w,
            slot,
        } if *l == list && *i == index && *w == width => Some(*slot),
        _ => None,
    })
}

/// Returns the blocks that can come right before each block.
fn predecessors(function: &Function) -> Vec<Vec<usize>> {
    let mut preds = vec![Vec::new(); function.blocks.len()];
    for (i, block) in function.blocks.iter().enumerate() {
        for BlockId(succ) in block.term.successors() {
            if !preds[succ].contains(&i) {
                preds[succ].push(i);
            }
        }
    }
    preds
}

/// Removes repeated index checks and uniqueness checks, through the facts available on entry to
/// each block: those that hold at the end of every block before it, found by iterating to a fixed
/// point from the optimistic guess that every fact holds everywhere but the entry.
fn eliminate(function: &mut Function) {
    let preds = predecessors(function);
    let count = function.blocks.len();
    // None stands for every fact, before a block has been reached
    let mut outs: Vec<Option<HashSet<Fact>>> = vec![None; count];
    // the facts on entry to a block, or None while no block before it has been reached, which
    // stands for every fact
    let entry_facts = |b: usize, outs: &[Option<HashSet<Fact>>]| -> Option<HashSet<Fact>> {
        if b == 0 {
            return Some(HashSet::new());
        }
        let mut known: Option<HashSet<Fact>> = None;
        for &p in &preds[b] {
            if let Some(out) = &outs[p] {
                known = Some(match known {
                    None => out.clone(),
                    Some(known) => known.intersection(out).cloned().collect(),
                });
            }
        }
        known
    };
    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..count {
            let Some(mut facts) = entry_facts(b, &outs) else {
                continue;
            };
            for inst in &function.blocks[b].insts {
                transfer(&mut facts, inst);
            }
            if outs[b].as_ref() != Some(&facts) {
                outs[b] = Some(facts);
                changed = true;
            }
        }
    }
    for b in 0..count {
        // a block never reached keeps every check
        let mut facts = entry_facts(b, &outs).unwrap_or_default();
        let block = &mut function.blocks[b];
        let (insts, spans) = (
            std::mem::take(&mut block.insts),
            std::mem::take(&mut block.spans),
        );
        for (inst, span) in insts.into_iter().zip(spans) {
            let inst = match inst {
                Inst::ElementSlot {
                    dst,
                    list: Operand::Reg(list),
                    index,
                    width,
                } => match checked_slot(&facts, list, index, width) {
                    Some(slot) => Inst::Copy {
                        dst,
                        src: Operand::Reg(slot),
                    },
                    None => Inst::ElementSlot {
                        dst,
                        list: Operand::Reg(list),
                        index,
                        width,
                    },
                },
                Inst::ListLoad {
                    dst,
                    list: Operand::Reg(list),
                    index,
                } => match loaded_value(&facts, list, index) {
                    Some(value) => Inst::Copy {
                        dst,
                        src: Operand::Reg(value),
                    },
                    None => Inst::ListLoad {
                        dst,
                        list: Operand::Reg(list),
                        index,
                    },
                },
                // the list is already unique, so this would do nothing
                inst if is_unique_of_itself(&inst)
                    .is_some_and(|list| facts.contains(&Fact::Unique(list))) =>
                {
                    continue;
                }
                inst => inst,
            };
            transfer(&mut facts, &inst);
            block.insts.push(inst);
            block.spans.push(span);
        }
    }
}

/// Reads and writes each slot that a block computes as a constant past another vreg, or as a copy
/// of one, as an offset from that vreg, as long as neither is written in between.
///
/// For example, `v5 = add v4, 3` then `v6 = list_get v0, v5` becomes `v6 = list_get v0, v4 + 3`,
/// which the backends address directly, and `v5` is left for [`remove_dead`] if nothing else
/// reads it.
fn fold_offsets(function: &mut Function) {
    for block in &mut function.blocks {
        // each vreg known to be another plus a constant
        let mut past: HashMap<VReg, (VReg, usize)> = HashMap::new();
        for inst in &mut block.insts {
            if let Inst::ListGet { index, offset, .. } | Inst::ListSet { index, offset, .. } = inst
                && let Operand::Reg(reg) = index
                && let Some(&(base, extra)) = past.get(reg)
            {
                *index = Operand::Reg(base);
                *offset += extra;
            }
            for def in inst.defs() {
                past.remove(&def);
                past.retain(|_, (base, _)| *base != def);
            }
            let found = match inst {
                Inst::Binary {
                    op: BinOp::Add,
                    dst,
                    lhs: Operand::Reg(base),
                    rhs: Operand::Imm(k),
                } if (0..=64).contains(k) => Some((*dst, *base, *k as usize)),
                Inst::Copy {
                    dst,
                    src: Operand::Reg(base),
                } => Some((*dst, *base, 0)),
                _ => None,
            };
            if let Some((dst, base, k)) = found
                && dst != base
            {
                let (base, k) = past.get(&base).map_or((base, k), |&(b, j)| (b, j + k));
                past.insert(dst, (base, k));
            }
        }
    }
}

/// Returns whether removing `inst` changes nothing but the vregs it writes: it never fails, calls
/// out, or changes memory or a count.
fn is_removable(inst: &Inst) -> bool {
    match inst {
        Inst::Binary { op, .. } | Inst::FloatBinary { op, .. } => {
            matches!(op, BinOp::Add | BinOp::Sub | BinOp::Mul)
        }
        Inst::Copy { .. }
        | Inst::Neg { .. }
        | Inst::FloatNeg { .. }
        | Inst::Not { .. }
        | Inst::Compare { .. }
        | Inst::IntToFloat { .. }
        | Inst::ListLen { .. }
        | Inst::StrLen { .. }
        | Inst::StrByte { .. }
        | Inst::ListGet { .. }
        | Inst::DataGet { .. }
        | Inst::ListData { .. }
        | Inst::StrAddr { .. }
        | Inst::BytesAddr { .. }
        | Inst::VariantStatic { .. }
        | Inst::FunctionAddress { .. }
        | Inst::FunctionStatic { .. }
        | Inst::EnumTag { .. }
        | Inst::FieldGet { .. }
        | Inst::LoadGlobal { checked: false, .. } => true,
        _ => false,
    }
}

/// Removes the instructions whose results nothing reads, when removing them changes nothing else,
/// until none is left.
fn remove_dead(function: &mut Function) {
    loop {
        let mut used: HashSet<VReg> = HashSet::new();
        for block in &function.blocks {
            for inst in &block.insts {
                used.extend(inst.uses());
            }
            used.extend(block.term.uses());
        }
        let mut removed = false;
        for block in &mut function.blocks {
            let (insts, spans) = (
                std::mem::take(&mut block.insts),
                std::mem::take(&mut block.spans),
            );
            for (inst, span) in insts.into_iter().zip(spans) {
                let dead = is_removable(&inst) && inst.defs().iter().all(|def| !used.contains(def));
                if dead {
                    removed = true;
                } else {
                    block.insts.push(inst);
                    block.spans.push(span);
                }
            }
        }
        if !removed {
            break;
        }
    }
}

/// A natural loop: its header, which every way into it goes through, and every block in it.
struct Loop {
    header: usize,
    body: Vec<bool>,
}

impl Loop {
    fn blocks(&self) -> impl Iterator<Item = usize> + '_ {
        self.body
            .iter()
            .enumerate()
            .filter(|(_, inside)| **inside)
            .map(|(b, _)| b)
    }

    fn insts<'a>(&'a self, function: &'a Function) -> impl Iterator<Item = &'a Inst> + 'a {
        self.blocks()
            .flat_map(move |b| function.blocks[b].insts.iter())
    }

    /// Returns the one block outside the loop that goes to its header, if it goes nowhere else.
    fn preheader(&self, function: &Function, preds: &[Vec<usize>]) -> Option<usize> {
        let outside: Vec<usize> = preds[self.header]
            .iter()
            .copied()
            .filter(|p| !self.body[*p])
            .collect();
        match outside.as_slice() {
            [p] if function.blocks[*p].term == Terminator::Jump(BlockId(self.header)) => Some(*p),
            _ => None,
        }
    }

    /// Returns whether the loop writes `reg` other than by making the list in it unique.
    fn writes(&self, function: &Function, reg: VReg) -> bool {
        self.insts(function)
            .any(|inst| is_unique_of_itself(inst).is_none() && inst.defs().contains(&reg))
    }
}

/// Returns the immediate dominator of each block reachable from the entry, by the algorithm of
/// Cooper, Harvey, and Kennedy, over the blocks in reverse postorder.
fn dominators(function: &Function, preds: &[Vec<usize>]) -> Vec<Option<usize>> {
    let count = function.blocks.len();
    let mut order = Vec::new();
    let mut seen = vec![false; count];
    // an explicit stack of (block, next successor), for a postorder without recursion
    let mut stack = vec![(0, 0)];
    seen[0] = true;
    while let Some((b, next)) = stack.pop() {
        let succs = function.blocks[b].term.successors();
        if let Some(&BlockId(succ)) = succs.get(next) {
            stack.push((b, next + 1));
            if !seen[succ] {
                seen[succ] = true;
                stack.push((succ, 0));
            }
        } else {
            order.push(b);
        }
    }
    order.reverse();
    let mut rank = vec![usize::MAX; count];
    for (i, &b) in order.iter().enumerate() {
        rank[b] = i;
    }
    let mut idom: Vec<Option<usize>> = vec![None; count];
    idom[0] = Some(0);
    let intersect = |idom: &[Option<usize>], mut a: usize, mut b: usize| {
        while a != b {
            while rank[a] > rank[b] {
                a = idom[a].expect("a processed block has a dominator");
            }
            while rank[b] > rank[a] {
                b = idom[b].expect("a processed block has a dominator");
            }
        }
        a
    };
    let mut changed = true;
    while changed {
        changed = false;
        for &b in order.iter().skip(1) {
            let mut new = None;
            for &p in &preds[b] {
                if idom[p].is_none() {
                    continue;
                }
                new = Some(match new {
                    None => p,
                    Some(other) => intersect(&idom, p, other),
                });
            }
            if new.is_some() && idom[b] != new {
                idom[b] = new;
                changed = true;
            }
        }
    }
    idom
}

/// Returns every natural loop of `function`, innermost first, with the loops that share a header
/// merged.
fn loops(function: &Function, preds: &[Vec<usize>]) -> Vec<Loop> {
    let idom = dominators(function, preds);
    let dominates = |a: usize, mut b: usize| loop {
        if a == b {
            return true;
        }
        match idom[b] {
            Some(up) if up != b => b = up,
            _ => return false,
        }
    };
    let count = function.blocks.len();
    let mut found: Vec<Loop> = Vec::new();
    for (tail, block) in function.blocks.iter().enumerate() {
        if idom[tail].is_none() {
            continue;
        }
        for BlockId(header) in block.term.successors() {
            if !dominates(header, tail) {
                continue;
            }
            let index = match found.iter().position(|l| l.header == header) {
                Some(index) => index,
                None => {
                    let mut body = vec![false; count];
                    body[header] = true;
                    found.push(Loop { header, body });
                    found.len() - 1
                }
            };
            // every block that reaches the tail without going through the header
            let mut stack = vec![tail];
            while let Some(b) = stack.pop() {
                if found[index].body[b] {
                    continue;
                }
                found[index].body[b] = true;
                stack.extend(preds[b].iter().copied());
            }
        }
    }
    found.sort_by_key(|l| l.body.iter().filter(|inside| **inside).count());
    found
}

/// Moves the `list_unique` of a variable out of one loop that never makes it shared again, before
/// the loop, and returns whether it did.
///
/// Inside such a loop, nothing but making the list unique writes the variable, and nothing calls
/// out, retains a value (which may be the same list), or releases the variable, so once the list
/// is unique it stays unique for the whole loop.
fn hoist_unique(function: &mut Function) -> bool {
    let preds = predecessors(function);
    for l in loops(function, &preds) {
        let Some(preheader) = l.preheader(function, &preds) else {
            continue;
        };
        let shares = |inst: &Inst, list: VReg| match inst {
            Inst::Call { .. } | Inst::Retain { .. } => true,
            Inst::Release { src, .. } => *src == Operand::Reg(list),
            _ => false,
        };
        let candidates: Vec<VReg> = l.insts(function).filter_map(is_unique_of_itself).collect();
        let Some(list) = candidates.into_iter().find(|&list| {
            !l.writes(function, list) && !l.insts(function).any(|inst| shares(inst, list))
        }) else {
            continue;
        };
        let mut span = None;
        for b in l.blocks().collect::<Vec<_>>() {
            let block = &mut function.blocks[b];
            let mut i = 0;
            while i < block.insts.len() {
                if is_unique_of_itself(&block.insts[i]) == Some(list) {
                    block.insts.remove(i);
                    span = Some(block.spans.remove(i));
                } else {
                    i += 1;
                }
            }
        }
        let block = &mut function.blocks[preheader];
        block.insts.push(Inst::ListUnique {
            dst: list,
            src: Operand::Reg(list),
            maybe_null: true,
        });
        block.spans.push(span.unwrap_or(block.term_span));
        return true;
    }
    false
}

/// Copies the test of each loop whose header does nothing but test to the end of each block that
/// jumps back to it, so a turn of the loop takes one branch, back to its first block, rather than
/// a jump to the test and a branch past it, as gcc lays loops out. The header then runs only on
/// entry.
///
/// For example, `b1: br_lt v2, v4 -> b2, b3` with `b2` ending in `jmp b1` becomes `b2` ending in
/// `br_lt v2, v4 -> b2, b3`.
fn rotate_loops(function: &mut Function) {
    let preds = predecessors(function);
    for l in loops(function, &preds) {
        let header = &function.blocks[l.header];
        let tests_only = header.insts.len() <= 4 && header.insts.iter().all(is_pure);
        let leaves = match &header.term {
            Terminator::Branch {
                then, otherwise, ..
            }
            | Terminator::CmpBranch {
                then, otherwise, ..
            }
            | Terminator::TestBranch {
                then, otherwise, ..
            } => l.body[then.0] != l.body[otherwise.0],
            _ => false,
        };
        if !tests_only || !leaves {
            continue;
        }
        let (insts, spans, term, term_span) = (
            header.insts.clone(),
            header.spans.clone(),
            header.term.clone(),
            header.term_span,
        );
        for &latch in &preds[l.header] {
            let block = &mut function.blocks[latch];
            if !l.body[latch] || block.term != Terminator::Jump(BlockId(l.header)) {
                continue;
            }
            block.insts.extend(insts.iter().cloned());
            block.spans.extend(spans.iter().copied());
            block.term = term.clone();
            block.term_span = term_span;
        }
    }
}

/// Loads, once on the way into each loop that [`rotate_loops`] rotated, the address of the
/// elements of each list the loop reads or writes at slots known to be in range, and reads and
/// writes them through it (`data_get`, `data_set`), rather than reloading it from the list for
/// every element.
///
/// The address stays right as long as the loop never writes the variable holding the list,
/// calls nothing (appending, the only way a list's elements move, is a call), makes no list
/// unique (which may copy it), and releases nothing (which could free it). A new block on the
/// loop's one way in loads the addresses, which after rotation runs once each time the loop
/// starts, and only if it runs at all.
fn hoist_data(function: &mut Function) {
    let preds = predecessors(function);
    for l in loops(function, &preds) {
        // the one way in, which a rotated loop takes once each time it starts
        let outside: Vec<usize> = preds[l.header]
            .iter()
            .copied()
            .filter(|p| !l.body[*p])
            .collect();
        let [before] = outside[..] else {
            continue;
        };
        let entry = BlockId(l.header);
        let moves = l.insts(function).any(|inst| {
            matches!(
                inst,
                Inst::Call { .. } | Inst::ListUnique { .. } | Inst::Release { .. }
            )
        });
        if moves {
            continue;
        }
        let mut lists: Vec<VReg> = Vec::new();
        for inst in l.insts(function) {
            if let Inst::ListGet {
                list: Operand::Reg(list),
                ..
            }
            | Inst::ListSet {
                list: Operand::Reg(list),
                ..
            } = inst
                && !lists.contains(list)
                && !l.writes(function, *list)
                && !l.insts(function).any(|inst| inst.defs().contains(list))
            {
                lists.push(*list);
            }
        }
        if lists.is_empty() {
            continue;
        }
        let mut addresses = HashMap::new();
        let mut insts = Vec::new();
        for &list in &lists {
            let data = VReg(function.vreg_count);
            function.vreg_count += 1;
            addresses.insert(list, data);
            insts.push(Inst::ListData {
                dst: data,
                list: Operand::Reg(list),
            });
        }
        for b in l.blocks().collect::<Vec<_>>() {
            for inst in &mut function.blocks[b].insts {
                *inst = match inst.clone() {
                    Inst::ListGet {
                        dst,
                        list: Operand::Reg(list),
                        index,
                        offset,
                    } if addresses.contains_key(&list) => Inst::DataGet {
                        dst,
                        data: Operand::Reg(addresses[&list]),
                        index,
                        offset,
                    },
                    Inst::ListSet {
                        list: Operand::Reg(list),
                        index,
                        value,
                        offset,
                    } if addresses.contains_key(&list) => Inst::DataSet {
                        data: Operand::Reg(addresses[&list]),
                        index,
                        value,
                        offset,
                    },
                    inst => inst,
                };
            }
        }
        // it runs once each time the loop starts, one level outside it
        let span = function.blocks[before].term_span;
        let depth = function.blocks[l.header].loop_depth.saturating_sub(1);
        let way_in = BlockId(function.blocks.len());
        function.blocks.push(Block {
            spans: vec![span; insts.len()],
            insts,
            term: Terminator::Jump(entry),
            term_span: span,
            loop_depth: depth,
        });
        let term = &function.blocks[before].term;
        let moved = |b: BlockId| if b == entry { way_in } else { b };
        function.blocks[before].term = retarget(term, moved, |operand| operand);
    }
}

/// A place in a function: a block and the index of an instruction in it, where the block's
/// terminator comes after every instruction.
type Point = (usize, usize);

/// Reads `src` in place of each copy of it that holds the same value wherever it is read, and
/// returns whether it did, removing the copies.
///
/// A copy `c = copy v` qualifies when nothing else writes `c`, `v` is a parameter nothing writes
/// or is written only once, before the copy on every way to it, and every read of `c` comes after
/// the copy on every way to it. Then `v` cannot change between the copy and any read of `c`, since
/// a way from a later write of `v` to that read would make a way from the entry to the read that
/// misses the copy. This is what a `for` loop's bound is, in `for i in range(n):` when nothing
/// assigns `n`, so the loop compares with `n` itself, and nested loops over `range(n)` keep one
/// register for it rather than one each.
fn forward_copies(function: &mut Function) -> bool {
    let preds = predecessors(function);
    let idom = dominators(function, &preds);
    // whether block a dominates block b, which an unreachable b never is
    let dominates = |a: usize, mut b: usize| loop {
        if a == b {
            return true;
        }
        match idom[b] {
            Some(up) if up != b => b = up,
            _ => return false,
        }
    };
    let before = |(ab, ai): Point, (bb, bi): Point| {
        if ab == bb { ai < bi } else { dominates(ab, bb) }
    };
    let mut writes: HashMap<VReg, Vec<Point>> = HashMap::new();
    let mut reads: HashMap<VReg, Vec<Point>> = HashMap::new();
    for (b, block) in function.blocks.iter().enumerate() {
        for (i, inst) in block.insts.iter().enumerate() {
            for def in inst.defs() {
                writes.entry(def).or_default().push((b, i));
            }
            for reg in inst.uses() {
                reads.entry(reg).or_default().push((b, i));
            }
        }
        for reg in block.term.uses() {
            reads.entry(reg).or_default().push((b, block.insts.len()));
        }
    }
    let mut forwarded: HashMap<VReg, VReg> = HashMap::new();
    for (b, block) in function.blocks.iter().enumerate() {
        if idom[b].is_none() {
            continue;
        }
        for (i, inst) in block.insts.iter().enumerate() {
            let Inst::Copy {
                dst,
                src: Operand::Reg(src),
            } = *inst
            else {
                continue;
            };
            let copy = (b, i);
            let written_once = |reg: VReg| writes.get(&reg).map_or(&[][..], Vec::as_slice);
            let source_fixed = match written_once(src) {
                [] => function.params.contains(&src),
                [write] => !function.params.contains(&src) && before(*write, copy),
                _ => false,
            };
            let qualifies = dst != src
                && source_fixed
                && !function.params.contains(&dst)
                && written_once(dst) == [copy]
                && reads
                    .get(&dst)
                    .is_none_or(|points| points.iter().all(|&read| before(copy, read)));
            if qualifies {
                forwarded.insert(dst, src);
                // one copy at a time, since forwarding changes what the others read
                break;
            }
        }
        if !forwarded.is_empty() {
            break;
        }
    }
    if forwarded.is_empty() {
        return false;
    }
    let rename = |operand: Operand| match operand {
        Operand::Reg(reg) => Operand::Reg(forwarded.get(&reg).copied().unwrap_or(reg)),
        imm => imm,
    };
    for block in &mut function.blocks {
        let (insts, spans) = (
            std::mem::take(&mut block.insts),
            std::mem::take(&mut block.spans),
        );
        for (inst, span) in insts.into_iter().zip(spans) {
            if matches!(inst, Inst::Copy { dst, .. } if forwarded.contains_key(&dst)) {
                continue;
            }
            block.insts.push(rename_inst(&inst, &|reg| reg, &rename));
            block.spans.push(span);
        }
        block.term = retarget(&block.term, |b| b, rename);
    }
    true
}

/// Turns a remainder by a power of two that is only compared with 0 into a test of the
/// dividend's low bits.
///
/// `%` gives the dividend's sign, so `x % 8` is 0 exactly when the low three bits of `x` are, for
/// a negative `x` too, and `v1 = rem v0, 8` followed by `br_eq v1, 0 -> b1, b2` becomes
/// `br_test v0, 7 -> b2, b1` when nothing else reads `v1`. A remainder by a constant other than 0
/// never fails, so dropping it changes nothing else. `x % 2 == 1` stays, since `-3 % 2` is -1.
fn test_remainders(function: &mut Function) {
    let mut reads: HashMap<VReg, usize> = HashMap::new();
    for block in &function.blocks {
        let insts = block.insts.iter().flat_map(Inst::uses);
        for reg in insts.chain(block.term.uses()) {
            *reads.entry(reg).or_default() += 1;
        }
    }
    for block in &mut function.blocks {
        let Terminator::CmpBranch {
            cond: cond @ (Cond::Eq | Cond::Ne),
            float: false,
            lhs,
            rhs,
            then,
            otherwise,
        } = block.term
        else {
            continue;
        };
        let tested = match (lhs, rhs) {
            (Operand::Reg(reg), Operand::Imm(0)) | (Operand::Imm(0), Operand::Reg(reg)) => reg,
            _ => continue,
        };
        if reads.get(&tested) != Some(&1) {
            continue;
        }
        let Some(at) = block
            .insts
            .iter()
            .rposition(|inst| inst.defs().contains(&tested))
        else {
            continue;
        };
        let Inst::Binary {
            op: BinOp::Rem,
            lhs: Operand::Reg(dividend),
            rhs: Operand::Imm(divisor),
            ..
        } = block.insts[at]
        else {
            continue;
        };
        let magnitude = divisor.unsigned_abs();
        // the dividend is read at the end of the block now, so it must not change before then
        let changed = block.insts[at + 1..]
            .iter()
            .any(|inst| inst.defs().contains(&dividend));
        if !magnitude.is_power_of_two() || magnitude == 1 || changed {
            continue;
        }
        block.insts.remove(at);
        block.spans.remove(at);
        // a set bit means the remainder is not 0
        let (then, otherwise) = match cond {
            Cond::Eq => (otherwise, then),
            _ => (then, otherwise),
        };
        block.term = Terminator::TestBranch {
            value: Operand::Reg(dividend),
            mask: (magnitude - 1) as i64,
            then,
            otherwise,
        };
    }
}

/// The largest constant an index may grow by and still count as never negative, so that wrapping
/// past `i64::MAX` would take at least 2^59 additions.
const NATURAL_STEP: i64 = 16;

/// Returns the vregs that never hold an index a list would count from its end, so that both
/// backends can check them against the length alone.
///
/// A vreg qualifies when it is not a parameter and every write of it is a constant from 0, a list's
/// length or a string's, a copy of a qualifying vreg, or a qualifying vreg plus a constant from 0 to
/// [`NATURAL_STEP`], which takes in every counter of a `for` loop over `range` with a start from 0
/// and a small step. Such a value is never negative unless an addition wrapped past `i64::MAX`,
/// which lands it near `i64::MIN`, where it stays out of range even counted from the end, so
/// skipping that step changes nothing. For example, `i` in `for i in range(n): t += xs[i]`
/// qualifies, while `i - 1` and a parameter do not.
pub fn never_negative(function: &Function) -> HashSet<VReg> {
    let mut writes: HashMap<VReg, Vec<&Inst>> = HashMap::new();
    for inst in function.blocks.iter().flat_map(|block| &block.insts) {
        for def in inst.defs() {
            writes.entry(def).or_default().push(inst);
        }
    }
    let mut natural: HashSet<VReg> = writes
        .keys()
        .copied()
        .filter(|reg| !function.params.contains(reg))
        .collect();
    // the largest set every write keeps, found by dropping vregs until none needs dropping
    loop {
        let dropped: Vec<VReg> = natural
            .iter()
            .copied()
            .filter(|reg| {
                let from = |operand: &Operand| match operand {
                    Operand::Imm(n) => *n >= 0,
                    Operand::Reg(src) => natural.contains(src),
                };
                !writes[reg].iter().all(|inst| match inst {
                    Inst::Copy { src, .. } => from(src),
                    Inst::ListLen { .. } | Inst::StrLen { .. } | Inst::StrByte { .. } => true,
                    Inst::Binary {
                        op: BinOp::Add,
                        lhs,
                        rhs,
                        ..
                    } => match (lhs, rhs) {
                        (Operand::Reg(reg), Operand::Imm(n))
                        | (Operand::Imm(n), Operand::Reg(reg)) => {
                            (0..=NATURAL_STEP).contains(n) && natural.contains(reg)
                        }
                        _ => false,
                    },
                    _ => false,
                })
            })
            .collect();
        if dropped.is_empty() {
            return natural;
        }
        for reg in dropped {
            natural.remove(&reg);
        }
    }
}

/// Returns whether `inst` may stop the program or call out, which a check must not move before.
fn may_fail(inst: &Inst) -> bool {
    match inst {
        Inst::Binary {
            op: BinOp::Div | BinOp::Rem,
            rhs: Operand::Imm(divisor),
            ..
        } => *divisor == 0 || *divisor == -1,
        Inst::Binary {
            op: BinOp::Div | BinOp::Rem | BinOp::Pow,
            ..
        }
        | Inst::FloatBinary {
            op: BinOp::Div | BinOp::Rem | BinOp::Pow,
            ..
        }
        | Inst::FloatToInt { .. }
        | Inst::ListLoad { .. }
        | Inst::ListStore { .. }
        | Inst::ElementSlot { .. }
        | Inst::LoadGlobal { checked: true, .. }
        | Inst::DepthCheck
        | Inst::Call { .. } => true,
        _ => false,
    }
}

/// Returns whether running `inst` twice is the same as running it once: it reads but changes
/// nothing but the vregs it writes, and never fails.
fn is_pure(inst: &Inst) -> bool {
    !may_fail(inst)
        && !matches!(
            inst,
            Inst::StoreGlobal { .. }
                | Inst::ListSet { .. }
                | Inst::DataSet { .. }
                | Inst::ListInit { .. }
                | Inst::FieldInit { .. }
                | Inst::Retain { .. }
                | Inst::Release { .. }
                | Inst::ListUnique { .. }
                | Inst::Packed { .. }
        )
}

/// Checks before one loop the indexes that every turn of it checks first, and returns whether it
/// did.
///
/// A check qualifies when the loop calls nothing (so no list changes length), never writes its
/// list (other than to make it unique, which keeps its length) or its index, and every turn
/// reaches it from the loop's test without passing anything that could fail first. A checked read
/// of an element (`list_load`) qualifies the same way in a loop that changes no list, since it
/// then reads the same value every turn, as `a[i]` does in `for k in range(n): s += a[i][k]`. A
/// new block runs the checks between a copy of the test, for the loop's first turn, and the loop,
/// and the checks in the loop become copies of their results there.
fn hoist_checks(function: &mut Function) -> bool {
    let preds = predecessors(function);
    for l in loops(function, &preds) {
        let Some(preheader) = l.preheader(function, &preds) else {
            continue;
        };
        if l.insts(function).any(Inst::is_call) {
            continue;
        }
        let header = &function.blocks[l.header];
        if !header.insts.iter().all(is_pure) {
            continue;
        }
        let (entry, test) = match &header.term {
            Terminator::Branch {
                then, otherwise, ..
            }
            | Terminator::CmpBranch {
                then, otherwise, ..
            }
            | Terminator::TestBranch {
                then, otherwise, ..
            } => match (l.body[then.0], l.body[otherwise.0]) {
                (true, false) => (then.0, header.term.clone()),
                (false, true) => (otherwise.0, header.term.clone()),
                _ => continue,
            },
            _ => continue,
        };
        if entry == l.header {
            continue;
        }
        let invariant = |operand: &Operand| match operand {
            Operand::Reg(reg) => !l.writes(function, *reg),
            Operand::Imm(_) => true,
        };
        // an element read in a loop that changes no list reads the same value every turn
        let stores = l.insts(function).any(|inst| {
            matches!(
                inst,
                Inst::ListStore { .. }
                    | Inst::ListSet { .. }
                    | Inst::DataSet { .. }
                    | Inst::ListInit { .. }
                    | Inst::Packed { .. }
                    | Inst::FieldInit { .. }
            )
        });
        // the checks every turn makes before anything that could fail, by block and position
        let mut hoisted: Vec<(usize, usize)> = Vec::new();
        let mut block = entry;
        'walk: loop {
            for (i, inst) in function.blocks[block].insts.iter().enumerate() {
                match inst {
                    Inst::ElementSlot { list, index, .. }
                        if matches!(list, Operand::Reg(_))
                            && invariant(list)
                            && invariant(index) =>
                    {
                        hoisted.push((block, i));
                    }
                    Inst::ListLoad { list, index, .. }
                        if !stores
                            && matches!(list, Operand::Reg(_))
                            && invariant(list)
                            && invariant(index) =>
                    {
                        hoisted.push((block, i));
                    }
                    inst if may_fail(inst) => break 'walk,
                    _ => {}
                }
            }
            match function.blocks[block].term {
                Terminator::Jump(BlockId(next))
                    if l.body[next] && next != l.header && preds[next] == [block] =>
                {
                    block = next;
                }
                _ => break,
            }
        }
        if hoisted.is_empty() {
            continue;
        }

        let guard = function.blocks.len();
        let mut checks = Vec::new();
        let mut spans = Vec::new();
        for &(b, i) in &hoisted {
            let slot = VReg(function.vreg_count);
            function.vreg_count += 1;
            let mut check = function.blocks[b].insts[i].clone();
            let dst = match &mut check {
                Inst::ElementSlot { dst, .. } | Inst::ListLoad { dst, .. } => {
                    std::mem::replace(dst, slot)
                }
                _ => unreachable!("only checks and checked reads are hoisted"),
            };
            checks.push(check);
            spans.push(function.blocks[b].spans[i]);
            function.blocks[b].insts[i] = Inst::Copy {
                dst,
                src: Operand::Reg(slot),
            };
        }
        let before = &function.blocks[preheader];
        function.blocks.push(Block {
            insts: checks,
            spans,
            term: Terminator::Jump(BlockId(l.header)),
            term_span: before.term_span,
            loop_depth: before.loop_depth,
        });
        // the first turn's test, which goes to the checks rather than into the loop
        let guarded = |b: BlockId| if b.0 == entry { BlockId(guard) } else { b };
        let test = retarget(&test, guarded, |operand| operand);
        let (header_insts, header_spans, header_span) = {
            let header = &function.blocks[l.header];
            (header.insts.clone(), header.spans.clone(), header.term_span)
        };
        let before = &mut function.blocks[preheader];
        before.insts.extend(header_insts);
        before.spans.extend(header_spans);
        before.term = test;
        before.term_span = header_span;
        return true;
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeChecker;
    use crate::codegen::ir::lower::lower;
    use crate::driver::parse_program;

    const POINT: &str = "struct P:\n    x: float\n    y: float\n";

    /// Parses, checks, lowers, and optimizes `source`, returning the function named `name`.
    fn optimized(source: &str, name: &str) -> Function {
        let module = parse_program(source).unwrap();
        let analysis = TypeChecker::new().analyze(&module);
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let mut program = lower(&module, &analysis).unwrap();
        optimize(&mut program);
        program
            .functions
            .into_iter()
            .find(|f| f.name.as_deref() == Some(name))
            .unwrap()
    }

    /// Returns how many instructions of `function`'s blocks inside a loop satisfy `pick`, and how
    /// many outside one do.
    fn count(function: &Function, pick: impl Fn(&Inst) -> bool) -> (usize, usize) {
        let (mut inside, mut outside) = (0, 0);
        for block in &function.blocks {
            let n = block.insts.iter().filter(|inst| pick(inst)).count();
            if block.loop_depth > 0 {
                inside += n;
            } else {
                outside += n;
            }
        }
        (inside, outside)
    }

    fn is_check(inst: &Inst) -> bool {
        matches!(inst, Inst::ElementSlot { .. })
    }

    fn is_unique(inst: &Inst) -> bool {
        matches!(inst, Inst::ListUnique { .. })
    }

    #[test]
    fn an_element_checked_again_reuses_the_first_check() {
        let f = optimized(
            &format!("{POINT}fn f(ps: list[P], i: int) -> float:\n    ret ps[i].x + ps[i].y\n"),
            "f",
        );
        assert_eq!(count(&f, is_check), (0, 1), "{f}");
    }

    #[test]
    fn a_call_between_two_checks_keeps_both() {
        let f = optimized(
            &format!(
                "{POINT}fn g() -> float:\n    ret 1.0\n\
                 fn f(ps: list[P], i: int) -> float:\n    ret ps[i].x + g() + ps[i].y\n"
            ),
            "f",
        );
        assert_eq!(count(&f, is_check), (0, 2), "{f}");
    }

    #[test]
    fn changes_in_a_loop_make_the_list_unique_once_before_it() {
        let f = optimized(
            &format!(
                "{POINT}fn f(ps: list[P], n: int) -> list[P]:\n    for i in range(n):\n        \
                 ps[0].x += 1.0\n        ps[0].y -= 1.0\n    ret ps\n"
            ),
            "f",
        );
        assert_eq!(count(&f, is_unique), (0, 1), "{f}");
        assert!(f.to_string().contains("list_unique_or_null"), "{f}");
    }

    #[test]
    fn a_loop_that_appends_or_shares_keeps_its_uniqueness_checks() {
        for body in ["ps.append(ps[0])", "qs = ps\n        print(qs.len())"] {
            let f = optimized(
                &format!(
                    "{POINT}fn f(ps: list[P], n: int) -> list[P]:\n    for i in range(n):\n        \
                     ps[0].x += 1.0\n        {body}\n    ret ps\n"
                ),
                "f",
            );
            assert!(count(&f, is_unique).0 >= 1, "{f}");
        }
    }

    #[test]
    fn an_invariant_index_is_checked_before_the_loop() {
        let f = optimized(
            &format!(
                "{POINT}fn f(ps: list[P], i: int, n: int) -> float:\n    t = 0.0\n    \
                 for j in range(n):\n        t += ps[i].x * ps[j].y\n    ret t\n"
            ),
            "f",
        );
        // ps[j] stays in the loop, and ps[i] is checked once before it
        assert_eq!(count(&f, is_check), (1, 1), "{f}");
    }

    #[test]
    fn a_hoisted_check_also_replaces_later_checks_in_the_loop() {
        let f = optimized(
            &format!(
                "{POINT}fn f(ps: list[P], i: int, n: int) -> float:\n    t = 0.0\n    \
                 for j in range(n):\n        t += ps[i].x * ps[j].y + ps[i].y\n    ret t\n"
            ),
            "f",
        );
        assert_eq!(count(&f, is_check), (1, 1), "{f}");
    }

    #[test]
    fn fields_of_an_element_are_offsets_from_its_first_slot() {
        let f = optimized(
            &format!("{POINT}fn f(ps: list[P], i: int) -> float:\n    ret ps[i].x + ps[i].y\n"),
            "f",
        );
        let text = f.to_string();
        assert!(text.contains("list_get v0, v2 + 1"), "{text}");
        assert!(!text.contains(" = add "), "{text}");
    }

    #[test]
    fn a_loop_tests_again_at_the_end_of_each_turn() {
        let f = optimized(
            "fn f(n: int) -> int:\n    t = 0\n    for i in range(n):\n        t += i\n    ret t\n",
            "f",
        );
        // the body branches back to itself instead of jumping to the header
        let body = f
            .blocks
            .iter()
            .enumerate()
            .find(|(_, block)| block.loop_depth > 0 && !block.insts.is_empty())
            .unwrap();
        assert!(
            matches!(&body.1.term, Terminator::CmpBranch { then, .. } if then.0 == body.0),
            "{f}"
        );
    }

    #[test]
    fn an_invariant_row_is_read_once_before_the_loop() {
        let f = optimized(
            "fn f(a: list[list[int]], b: list[list[int]], i: int, j: int, n: int) -> int:\n    \
             s = 0\n    for k in range(n):\n        s += a[i][k] * b[k][j]\n    ret s\n",
            "f",
        );
        let loads = |inst: &Inst| matches!(inst, Inst::ListLoad { .. });
        // a[i] once before the loop; a[i][k], b[k], and b[k][j] each turn
        assert_eq!(count(&f, loads), (3, 1), "{f}");
    }

    #[test]
    fn a_row_read_twice_is_read_once_unless_a_store_comes_between() {
        let twice = optimized(
            "fn f(a: list[list[int]], i: int) -> int:\n    ret a[i][0] + a[i][1]\n",
            "f",
        );
        let loads = |inst: &Inst| matches!(inst, Inst::ListLoad { .. });
        assert_eq!(count(&twice, loads), (0, 3), "{twice}");
        let stored = optimized(
            "fn f(a: list[list[int]], i: int) -> list[list[int]]:\n    \
             a[i][0] = a[i][1]\n    a[i][1] = a[i][0]\n    ret a\n",
            "f",
        );
        assert!(count(&stored, loads).1 >= 2, "{stored}");
    }

    #[test]
    fn a_loop_reads_elements_through_their_address_loaded_on_the_way_in() {
        let f = optimized(
            &format!(
                "{POINT}fn f(ps: list[P], n: int) -> list[P]:\n    for i in range(n):\n        \
                 ps[i].x += ps[i].y\n    ret ps\n"
            ),
            "f",
        );
        let data = |inst: &Inst| matches!(inst, Inst::ListData { .. });
        let get = |inst: &Inst| matches!(inst, Inst::ListGet { .. } | Inst::ListSet { .. });
        assert_eq!(count(&f, data), (0, 1), "{f}");
        assert_eq!(count(&f, get).0, 0, "{f}");
    }

    #[test]
    fn a_loop_bound_that_never_changes_is_not_copied() {
        let f = optimized(
            "fn f(n: int) -> int:\n    t = 0\n    for i in range(n):\n        \
             for j in range(n):\n            t += i * j\n    ret t\n",
            "f",
        );
        let n = f.params[0];
        let copies =
            |inst: &Inst| matches!(inst, Inst::Copy { src, .. } if *src == Operand::Reg(n));
        assert_eq!(count(&f, copies), (0, 0), "{f}");
    }

    #[test]
    fn a_copy_of_a_variable_assigned_again_stays() {
        let f = optimized(
            "fn f(n: int) -> int:\n    t = 0\n    for i in range(n):\n        n -= 1\n        \
             t += i\n    ret t\n",
            "f",
        );
        let n = f.params[0];
        let copies =
            |inst: &Inst| matches!(inst, Inst::Copy { src, .. } if *src == Operand::Reg(n));
        assert_eq!(count(&f, copies), (0, 1), "{f}");
    }

    /// Returns the index of every `list_load` in `function`, in layout order.
    fn load_indexes(function: &Function) -> Vec<Operand> {
        let blocks = function.blocks.iter();
        let insts = blocks.flat_map(|block| &block.insts);
        insts
            .filter_map(|inst| match inst {
                Inst::ListLoad { index, .. } => Some(*index),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn a_range_counter_is_never_negative() {
        let f = optimized(
            "fn f(xs: list[int], n: int) -> int:\n    t = 0\n    for i in range(n):\n        \
             t += xs[i]\n    for j in range(2, n):\n        t += xs[j]\n    ret t\n",
            "f",
        );
        let natural = never_negative(&f);
        let indexes = load_indexes(&f);
        assert_eq!(indexes.len(), 2, "{f}");
        for index in indexes {
            assert!(
                matches!(index, Operand::Reg(v) if natural.contains(&v)),
                "{f}"
            );
        }
    }

    #[test]
    fn an_index_that_may_be_negative_is_not() {
        let f = optimized(
            "fn f(xs: list[int], n: int, k: int) -> int:\n    t = xs[k]\n    \
             for i in range(n):\n        j = i - 1\n        t += xs[j]\n    m = 0\n    \
             while m < n:\n        t += xs[m]\n        m += 1000\n    ret t\n",
            "f",
        );
        let natural = never_negative(&f);
        let indexes = load_indexes(&f);
        assert_eq!(indexes.len(), 3, "{f}");
        for index in indexes {
            assert!(
                !matches!(index, Operand::Reg(v) if natural.contains(&v)),
                "{f}"
            );
        }
    }

    fn is_remainder(inst: &Inst) -> bool {
        matches!(inst, Inst::Binary { op: BinOp::Rem, .. })
    }

    /// Returns the terminators of `function` that test bits, as `(mask, then, otherwise)`.
    fn bit_tests(function: &Function) -> Vec<(i64, BlockId, BlockId)> {
        let terms = function.blocks.iter().map(|block| &block.term);
        terms
            .filter_map(|term| match term {
                Terminator::TestBranch {
                    mask,
                    then,
                    otherwise,
                    ..
                } => Some((*mask, *then, *otherwise)),
                _ => None,
            })
            .collect()
    }

    #[test]
    fn a_remainder_by_a_power_of_two_compared_with_zero_tests_bits() {
        let f = optimized(
            "fn f(n: int) -> int:\n    if n % 8 == 0:\n        ret 1\n    \
             if n % -2 != 0:\n        ret 2\n    ret 3\n",
            "f",
        );
        assert_eq!(count(&f, is_remainder), (0, 0), "{f}");
        let tests = bit_tests(&f);
        assert_eq!(tests.len(), 2, "{f}");
        // == 0 goes on when no bit is set, != 0 when one is
        let (mask, then, otherwise) = tests[0];
        assert_eq!(mask, 7, "{f}");
        assert!(
            matches!(f.blocks[otherwise.0].term, Terminator::Return(_)),
            "{f}"
        );
        assert!(
            !matches!(f.blocks[then.0].term, Terminator::Return(_)),
            "{f}"
        );
        let (mask, then, _) = tests[1];
        assert_eq!(mask, 1, "{f}");
        assert!(
            matches!(f.blocks[then.0].term, Terminator::Return(_)),
            "{f}"
        );
    }

    #[test]
    fn other_remainders_stay() {
        let f = optimized(
            "fn f(n: int) -> int:\n    if n % 2 == 1:\n        ret 1\n    \
             if n % 6 == 0:\n        ret 2\n    r = n % 4\n    if r == 0:\n        ret r\n    \
             ret 3\n",
            "f",
        );
        assert_eq!(count(&f, is_remainder), (0, 3), "{f}");
        assert!(bit_tests(&f).is_empty(), "{f}");
    }

    #[test]
    fn a_check_after_something_that_may_fail_stays_in_the_loop() {
        let f = optimized(
            &format!(
                "{POINT}fn f(ps: list[P], i: int, n: int) -> float:\n    t = 0.0\n    \
                 for j in range(n):\n        t += 1.0 / float(j)\n        t += ps[i].x\n    ret t\n"
            ),
            "f",
        );
        assert_eq!(count(&f, is_check), (1, 0), "{f}");
    }
}
