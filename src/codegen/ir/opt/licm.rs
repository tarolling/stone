//! Loop-invariant code motion: arithmetic a loop computes from values it never changes runs once
//! before the loop rather than on every turn.
//!
//! For example, in `for i in range(n): s += a * b + i`, `a * b` is computed once before the loop.

use super::{Function, Inst, Operand, VReg, is_arithmetic, loops, predecessors};
use std::collections::{HashMap, HashSet};

/// Moves every instruction that [`is_arithmetic`] and reads only constants and vregs a loop never
/// writes out of the loop, to the end of the block before it, innermost loops first so that work
/// moves out as far as it can, and returns whether it moved any.
///
/// Such an instruction can neither fail nor see a different value on any turn, so computing it
/// once, even for a loop that never runs, changes nothing else. When nothing else in the function
/// writes its vreg, the instruction itself moves; otherwise a new vreg takes the result before the
/// loop, and a copy of it stays where the instruction was.
pub(super) fn hoist_invariants(function: &mut Function) -> bool {
    let preds = predecessors(function);
    let mut writes: HashMap<VReg, usize> = HashMap::new();
    for block in &function.blocks {
        for inst in &block.insts {
            for def in inst.defs() {
                *writes.entry(def).or_default() += 1;
            }
        }
    }
    let mut moved = false;
    for l in loops(function, &preds) {
        let Some(preheader) = l.preheader(function, &preds) else {
            continue;
        };
        let blocks: Vec<usize> = l.blocks().collect();
        let mut written: HashSet<VReg> = l.insts(function).flat_map(|inst| inst.defs()).collect();
        // a parameter's value on entry is written by no instruction, but is not invariant if one
        // in the loop writes it, which `written` already says
        let invariant = |o: &Operand, written: &HashSet<VReg>| match o {
            Operand::Imm(_) => true,
            Operand::Reg(reg) => !written.contains(reg),
        };
        let mut progress = true;
        while progress {
            progress = false;
            for &b in &blocks {
                let mut i = 0;
                while i < function.blocks[b].insts.len() {
                    let inst = &function.blocks[b].insts[i];
                    let operands = inst.operands();
                    if !is_arithmetic(inst) || !operands.iter().all(|o| invariant(o, &written)) {
                        i += 1;
                        continue;
                    }
                    let dst = inst.defs()[0];
                    let span = function.blocks[b].spans[i];
                    if writes.get(&dst) == Some(&1) && !function.params.contains(&dst) {
                        let inst = function.blocks[b].insts.remove(i);
                        function.blocks[b].spans.remove(i);
                        let pre = &mut function.blocks[preheader];
                        pre.insts.push(inst);
                        pre.spans.push(span);
                        written.remove(&dst);
                    } else {
                        let temp = VReg(function.vreg_count);
                        function.vreg_count += 1;
                        let hoisted = crate::codegen::ir::inline::rename_inst(
                            inst,
                            &|reg| if reg == dst { temp } else { reg },
                            &|o| o,
                        );
                        function.blocks[b].insts[i] = Inst::Copy {
                            dst,
                            src: Operand::Reg(temp),
                        };
                        let pre = &mut function.blocks[preheader];
                        pre.insts.push(hoisted);
                        pre.spans.push(span);
                        writes.insert(temp, 1);
                        i += 1;
                    }
                    moved = true;
                    progress = true;
                }
            }
        }
    }
    moved
}
