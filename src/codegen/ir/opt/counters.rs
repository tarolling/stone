//! One vreg for each `for` loop's count. Lowering counts with a vreg of its own, which the body
//! cannot assign, and copies it into the loop's variable before adding 1, as
//! `v2 = copy v5` then `v5 = add v5, 1`, so both are live through the body, and every turn moves
//! one into the other. When nothing needs them apart, the body reads the count itself, and the
//! addition moves to the end of each turn.

use super::ranges::Test;
use super::{BinOp, BlockId, Function, Inst, Operand, Terminator, VReg, loops, predecessors};
use crate::codegen::ir::inline::{rename_inst, retarget};
use crate::codegen::ir::liveness::block_liveness;

/// Makes one loop read its count where it read the copy of it, adding 1 at the end of each turn
/// rather than at the start, and returns whether it did.
///
/// For example, `b2: v2 = copy v5; v5 = add v5, 1; v6 = mul v2, v2; jmp b1` becomes
/// `b2: v6 = mul v5, v5; v5 = add v5, 1; jmp b1`. A loop qualifies when its test goes on while
/// the count is below a bound, the block it goes on to copies the count and then adds 1 to it,
/// nothing else in the loop writes either vreg or reads the count, the copy is read only after it
/// is made, neither is live where the loop leaves (a `break` would see the count unchanged), and
/// every way back to the test is a jump, where the addition then goes.
pub(super) fn coalesce_counters(function: &mut Function) -> bool {
    let preds = predecessors(function);
    let (live_in, _) = block_liveness(function);
    for l in loops(function, &preds) {
        let Some(test) = Test::of(&function.blocks[l.header].term, &l) else {
            continue;
        };
        let (entry, counter) = (test.entry.0, test.counter);
        if entry == l.header || preds[entry] != [l.header] {
            continue;
        }
        let insts = &function.blocks[entry].insts;
        let Some(add) = insts.iter().position(|inst| inst.defs().contains(&counter)) else {
            continue;
        };
        let adds_one = matches!(
            insts[add],
            Inst::Binary {
                op: BinOp::Add,
                lhs: Operand::Reg(r),
                rhs: Operand::Imm(1),
                ..
            } if r == counter
        );
        let Some((copy, var)) = insts[..add]
            .iter()
            .enumerate()
            .find_map(|(i, inst)| match *inst {
                Inst::Copy {
                    dst,
                    src: Operand::Reg(src),
                } if src == counter && dst != counter => Some((i, dst)),
                _ => None,
            })
        else {
            continue;
        };
        let writes = |reg: VReg| {
            l.insts(function)
                .filter(|inst| inst.defs().contains(&reg))
                .count()
        };
        if !adds_one || writes(counter) != 1 || writes(var) != 1 || function.params.contains(&var) {
            continue;
        }
        // the count is read only by the test, the copy, and the addition
        let mut counter_reads = 0;
        for b in l.blocks() {
            let block = &function.blocks[b];
            counter_reads += block
                .insts
                .iter()
                .flat_map(|inst| inst.uses())
                .chain(block.term.uses())
                .filter(|&reg| reg == counter)
                .count();
        }
        let header = &function.blocks[l.header];
        let header_reads_var = header
            .insts
            .iter()
            .flat_map(|inst| inst.uses())
            .chain(header.term.uses())
            .any(|reg| reg == var);
        let header_counter_reads = header
            .insts
            .iter()
            .flat_map(|inst| inst.uses())
            .chain(header.term.uses())
            .filter(|&reg| reg == counter)
            .count();
        let read_before_copy = insts[..copy].iter().any(|inst| inst.uses().contains(&var));
        if counter_reads != header_counter_reads + 2 || header_reads_var || read_before_copy {
            continue;
        }
        let mut exits = Vec::new();
        let mut latches = Vec::new();
        for b in l.blocks() {
            for BlockId(succ) in function.blocks[b].term.successors() {
                if !l.body[succ] {
                    exits.push(succ);
                } else if succ == l.header {
                    latches.push(b);
                }
            }
        }
        let live_after = exits
            .iter()
            .any(|&e| live_in[e].contains(var) || live_in[e].contains(counter));
        let jumps_back = latches
            .iter()
            .all(|&b| function.blocks[b].term == Terminator::Jump(BlockId(l.header)));
        if live_after || !jumps_back {
            continue;
        }

        let block = &mut function.blocks[entry];
        let increment = block.insts.remove(add);
        let span = block.spans.remove(add);
        block.insts.remove(copy);
        block.spans.remove(copy);
        let rename = |o: Operand| match o {
            Operand::Reg(reg) if reg == var => Operand::Reg(counter),
            o => o,
        };
        for b in l.blocks().collect::<Vec<_>>() {
            let block = &mut function.blocks[b];
            for inst in &mut block.insts {
                *inst = rename_inst(inst, &|reg| reg, &rename);
            }
            block.term = retarget(&block.term, |t| t, rename);
        }
        for latch in latches {
            let block = &mut function.blocks[latch];
            block.insts.push(increment.clone());
            block.spans.push(span);
        }
        return true;
    }
    false
}
