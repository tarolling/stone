//! Optimizations over lowered IR that remove work the lowering of each statement on its own
//! repeats: checking an index into a list again, and making a list unique again.
//!
//! Lowering checks every index and makes every list it changes unique, statement by statement, so
//! `bodies[i].vx -= dx * m` followed by `bodies[i].vy -= dy * m` checks `i` and the uniqueness of
//! `bodies` twice, and does it again on every turn of a loop. [`optimize`] runs three passes over
//! each function:
//!
//! 1. A `list_unique` of a variable inside a loop that cannot make the list shared again moves
//!    before the loop, as a `list_unique_or_null`, since the variable may be null when the loop
//!    never runs. Copying a shared list early is invisible, since lists are values.
//! 2. An `element_slot` whose list and index the loop never changes, which every turn runs before
//!    anything that could fail or call out, is checked once before the loop, behind a copy of the
//!    loop's test, so it fails exactly when the first turn would, and not at all for a loop that
//!    never runs. The checks inside the loop become copies of its result.
//! 3. Within a function, an `element_slot` of a list and an index already checked on every path to
//!    it becomes a copy of the earlier result, and a `list_unique` of a list already unique is
//!    dropped, as long as nothing between them could change the list's length or share it.
//!
//! Then a slot that is a constant past another, such as a field of an element at
//! `add v4, 3`, is read and written as an offset from it (`list_get v0, v4 + 3`), and work whose
//! result nothing reads is dropped.
//!
//! The IR is not SSA, so every fact is about vregs and dies when any vreg it names is written.

use super::{BinOp, Block, BlockId, Function, Inst, Operand, Program, Terminator, VReg};
use std::collections::{HashMap, HashSet};

/// Runs every optimization over each function of `program`.
pub fn optimize(program: &mut Program) {
    for function in &mut program.functions {
        while hoist_unique(function) {}
        while hoist_checks(function) {}
        eliminate(function);
        fold_offsets(function);
        remove_dead(function);
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
}

impl Fact {
    fn mentions(&self, reg: VReg) -> bool {
        match self {
            Fact::Slot {
                list, index, slot, ..
            } => *list == reg || *slot == reg || *index == Operand::Reg(reg),
            Fact::Unique(list) => *list == reg,
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
        | Inst::ListGet { .. }
        | Inst::ListData { .. }
        | Inst::StrAddr { .. }
        | Inst::VariantStatic { .. }
        | Inst::FunctionAddress { .. }
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
/// reaches it from the loop's test without passing anything that could fail first. A new block
/// runs the checks between a copy of the test, for the loop's first turn, and the loop, and the
/// checks in the loop become copies of their results there.
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
            let Inst::ElementSlot {
                dst,
                list,
                index,
                width,
            } = function.blocks[b].insts[i].clone()
            else {
                unreachable!("only checks are hoisted");
            };
            let slot = VReg(function.vreg_count);
            function.vreg_count += 1;
            checks.push(Inst::ElementSlot {
                dst: slot,
                list,
                index,
                width,
            });
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
        let retarget = |b: BlockId| if b.0 == entry { BlockId(guard) } else { b };
        let test = match test {
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => Terminator::Branch {
                cond,
                then: retarget(then),
                otherwise: retarget(otherwise),
            },
            Terminator::CmpBranch {
                cond,
                float,
                lhs,
                rhs,
                then,
                otherwise,
            } => Terminator::CmpBranch {
                cond,
                float,
                lhs,
                rhs,
                then: retarget(then),
                otherwise: retarget(otherwise),
            },
            _ => unreachable!("the test branches"),
        };
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
