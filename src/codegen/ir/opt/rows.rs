//! One guard before a loop nest that reads rows of a list it never changes, so that the facts
//! pass can prove every such read in range: in
//!
//! ```text
//! for k in range(n):
//!     total += b[k][j]
//! ```
//!
//! inside `for j in range(n)`, with `b` a parameter the function never writes, `b` and its rows
//! cannot change while the function runs (see [`super::facts::frozen`]). So before the nest, one
//! test compares `n` with `b`'s length and an [`Inst::MinRowLen`] measures the shortest of its
//! first `n` rows: when both are at least `n`, a copy of the nest runs, in which the facts pass
//! proves `b[k]` and `b[k][j]` in range; otherwise the nest as it was runs, so a short row still
//! fails at the same index, after the same turns, as it would have.
//!
//! The guard costs one pass over the rows it measures, only as many as the nest reads.

use super::ranges::Test;
use super::{
    BlockId, Function, Inst, Loop, Operand, VReg, facts, loops, place_before, predecessors,
};
use crate::codegen::ir::inline::{rename_inst, retarget};
use crate::codegen::ir::liveness::block_liveness;
use crate::codegen::ir::{Block, Cond, Terminator};
use std::collections::{HashMap, HashSet};

/// The most instructions a loop nest may have to be copied, so copies cost little code.
const MAX_NEST_INSTS: usize = 200;

/// A list whose rows a nest reads: element `index < rows` of `list`, then element `index <
/// columns` of that row, by loop counters whose bounds `rows` and `columns` are.
#[derive(Clone, Copy, PartialEq)]
struct Rows {
    list: VReg,
    rows: Operand,
    columns: Operand,
}

/// Guards one loop nest, as the module describes, and returns whether it did, so the caller runs
/// it again until it does not.
pub(super) fn guard_rows(function: &mut Function) -> bool {
    let frozen = facts::frozen(function);
    if frozen.is_empty() {
        return false;
    }
    // a list measured once is not measured again, so the copies are not guarded in turn
    let measured: HashSet<VReg> = function
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter_map(|inst| match inst {
            Inst::MinRowLen {
                list: Operand::Reg(list),
                ..
            } => Some(*list),
            _ => None,
        })
        .collect();
    let preds = predecessors(function);
    let all = loops(function, &preds);
    for nest in &all {
        let outermost = all
            .iter()
            .all(|other| other.header == nest.header || !other.body[nest.header]);
        if !outermost || nest.insts(function).count() > MAX_NEST_INSTS {
            continue;
        }
        let Some(preheader) = nest.preheader(function, &preds) else {
            continue;
        };
        let guards = reads_of_rows(function, nest, &all, &frozen, &measured);
        if guards.is_empty() {
            continue;
        }
        guard(function, nest, preheader, &guards);
        return true;
    }
    false
}

/// Returns the lists of lists, frozen and not yet measured, whose rows `nest` reads by loop
/// counters (or copies of them) whose bounds the nest never writes, one per list.
fn reads_of_rows(
    function: &Function,
    nest: &Loop,
    all: &[Loop],
    frozen: &HashSet<VReg>,
    measured: &HashSet<VReg>,
) -> Vec<Rows> {
    let fixed = |o: Operand| match o {
        Operand::Imm(_) => true,
        Operand::Reg(reg) => !nest.writes(function, reg),
    };
    let writes = |reg: VReg| {
        nest.insts(function)
            .filter(|inst| inst.defs().contains(&reg))
            .count()
    };
    // the bound of each counter of a loop in the nest
    let mut bound_of: HashMap<VReg, Operand> = HashMap::new();
    for l in all.iter().filter(|l| l.blocks().all(|b| nest.body[b])) {
        if let Some(test) = Test::of(&function.blocks[l.header].term, l)
            && fixed(test.bound)
        {
            bound_of.insert(test.counter, test.bound);
        }
    }
    // a copy of a counter, as a `for` loop's variable is, is below the same bound
    let copies: Vec<(VReg, Operand)> = nest
        .insts(function)
        .filter_map(|inst| match *inst {
            Inst::Copy {
                dst,
                src: Operand::Reg(counter),
            } if !bound_of.contains_key(&dst) && writes(dst) == 1 => {
                bound_of.get(&counter).map(|&bound| (dst, bound))
            }
            _ => None,
        })
        .collect();
    bound_of.extend(copies);
    // each row the nest reads, by the list it comes from and its index's bound
    let mut row_of: HashMap<VReg, (VReg, Operand)> = HashMap::new();
    for inst in nest.insts(function) {
        if let Inst::ListLoad {
            dst,
            list: Operand::Reg(list),
            index: Operand::Reg(index),
        } = *inst
            && frozen.contains(&list)
            && !measured.contains(&list)
            && writes(dst) == 1
            && let Some(&rows) = bound_of.get(&index)
        {
            row_of.insert(dst, (list, rows));
        }
    }
    // a row put in a variable of its own, as `row = b[k]` does
    let copies: Vec<(VReg, (VReg, Operand))> = nest
        .insts(function)
        .filter_map(|inst| match *inst {
            Inst::Copy {
                dst,
                src: Operand::Reg(src),
            } if writes(dst) == 1 && !row_of.contains_key(&dst) => {
                row_of.get(&src).map(|&row| (dst, row))
            }
            _ => None,
        })
        .collect();
    row_of.extend(copies);
    let mut guards: Vec<Rows> = Vec::new();
    for inst in nest.insts(function) {
        if let Inst::ListLoad {
            list: Operand::Reg(row),
            index: Operand::Reg(index),
            ..
        } = *inst
            && let Some(&(list, rows)) = row_of.get(&row)
            && let Some(&columns) = bound_of.get(&index)
            && guards.iter().all(|g| g.list != list)
        {
            guards.push(Rows {
                list,
                rows,
                columns,
            });
        }
    }
    guards
}

/// Copies `nest` and makes the way in from `preheader` test, for each of `guards`, that the
/// list has at least `rows` elements and that the first `rows` are at least `columns` long: the
/// copy runs when every test passes, and the nest as it was otherwise.
///
/// The copy writes new vregs for what the nest only uses within a turn, and sits with the tests
/// right before the nest, so that neither nest's values live across the code between them.
fn guard(function: &mut Function, nest: &Loop, preheader: usize, guards: &[Rows]) {
    let blocks: Vec<usize> = nest.blocks().collect();
    let (live_in, _) = block_liveness(function);
    let exits: Vec<usize> = blocks
        .iter()
        .flat_map(|&b| function.blocks[b].term.successors())
        .map(|BlockId(t)| t)
        .filter(|&t| !nest.body[t])
        .collect();
    let mut fresh: HashMap<VReg, VReg> = HashMap::new();
    for inst in nest.insts(function) {
        for def in inst.defs() {
            let local = !live_in[nest.header].contains(def)
                && exits.iter().all(|&e| !live_in[e].contains(def))
                && !function.params.contains(&def);
            if local && !fresh.contains_key(&def) {
                fresh.insert(def, VReg(function.vreg_count + fresh.len() as u32));
            }
        }
    }
    function.vreg_count += fresh.len() as u32;

    // two tests per list, the last going into the copy
    let base = function.blocks.len();
    let first_copy = base + 2 * guards.len();
    let copy_of: HashMap<usize, usize> = blocks
        .iter()
        .enumerate()
        .map(|(i, &b)| (b, first_copy + i))
        .collect();
    let span = function.blocks[nest.header].term_span;
    let depth = function.blocks[preheader].loop_depth;
    let header = BlockId(nest.header);
    for (i, g) in guards.iter().enumerate() {
        let length = VReg(function.vreg_count);
        let shortest = VReg(function.vreg_count + 1);
        function.vreg_count += 2;
        let next = if i + 1 == guards.len() {
            BlockId(copy_of[&nest.header])
        } else {
            BlockId(base + 2 * i + 2)
        };
        let test = |lhs: Operand, rhs: VReg, then: BlockId| Terminator::CmpBranch {
            cond: Cond::Le,
            float: false,
            lhs,
            rhs: Operand::Reg(rhs),
            then,
            otherwise: header,
        };
        function.blocks.push(Block {
            insts: vec![Inst::ListLen {
                dst: length,
                list: Operand::Reg(g.list),
            }],
            spans: vec![span],
            term: test(g.rows, length, BlockId(base + 2 * i + 1)),
            term_span: span,
            loop_depth: depth,
        });
        // the rows measured are elements, since the test before showed there are as many
        function.blocks.push(Block {
            insts: vec![Inst::MinRowLen {
                dst: shortest,
                list: Operand::Reg(g.list),
                count: g.rows,
            }],
            spans: vec![span],
            term: test(g.columns, shortest, next),
            term_span: span,
            loop_depth: depth,
        });
    }

    let reg = |r: VReg| fresh.get(&r).copied().unwrap_or(r);
    let operand = |o: Operand| match o {
        Operand::Reg(r) => Operand::Reg(reg(r)),
        imm => imm,
    };
    for &b in &blocks {
        let mut block = function.blocks[b].clone();
        for inst in block.insts.iter_mut() {
            *inst = rename_inst(inst, &reg, &operand);
        }
        block.term = retarget(
            &block.term,
            |BlockId(t)| BlockId(copy_of.get(&t).copied().unwrap_or(t)),
            operand,
        );
        function.blocks.push(block);
    }
    function.blocks[preheader].term = Terminator::Jump(BlockId(base));
    let placements: Vec<(usize, usize)> = (base..function.blocks.len())
        .map(|b| (b, nest.header))
        .collect();
    place_before(function, &placements);
}
