//! Cleanup of the graph of blocks after other passes fold branches and empty blocks: a branch
//! whose two ways lead to the same block becomes a jump, a way to an empty block that only jumps
//! on goes straight to where it leads, a block that is the only way into the next absorbs it,
//! and blocks nothing reaches are dropped.

use super::{BlockId, Function, Terminator, dominators, predecessors};
use crate::codegen::ir::inline::retarget;

/// Cleans up the graph of `function`'s blocks, as the module describes, and returns whether it
/// changed anything.
///
/// For example, `b0: br v1 -> b1, b2`, `b1: jmp b2`, and `b2: ret 0` become `b0: ret 0`: the
/// branch goes to `b2` both ways, so it is a jump, and `b0` then absorbs `b2`.
pub(super) fn simplify_cfg(function: &mut Function) -> bool {
    let mut changed = false;
    loop {
        let round = same_way_branches(function)
            | skip_empty_blocks(function)
            | merge_blocks(function)
            | drop_unreachable(function);
        if !round {
            return changed;
        }
        changed = true;
    }
}

/// Turns each branch whose ways lead to the same block into a jump.
fn same_way_branches(function: &mut Function) -> bool {
    let mut changed = false;
    for block in &mut function.blocks {
        let target = match block.term {
            Terminator::Branch {
                then, otherwise, ..
            }
            | Terminator::CmpBranch {
                then, otherwise, ..
            }
            | Terminator::TestBranch {
                then, otherwise, ..
            } if then == otherwise => then,
            _ => continue,
        };
        block.term = Terminator::Jump(target);
        changed = true;
    }
    changed
}

/// Sends each way into a block that does nothing but jump straight to where it jumps, unless that
/// is a loop's header, whose one way in from outside the loop other passes put work before.
fn skip_empty_blocks(function: &mut Function) -> bool {
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
    let is_header = |b: usize| preds[b].iter().any(|&p| dominates(b, p));
    let count = function.blocks.len();
    // where a way into each block can go instead
    let mut forward: Vec<usize> = (0..count).collect();
    for (b, slot) in forward.iter_mut().enumerate() {
        let mut target = b;
        // a chain of empty blocks is followed at most once around, so a cycle of them stays
        for _ in 0..count {
            let block = &function.blocks[target];
            match block.term {
                Terminator::Jump(BlockId(next))
                    if block.insts.is_empty() && next != target && !is_header(next) =>
                {
                    target = next
                }
                _ => break,
            }
        }
        *slot = target;
    }
    let mut changed = false;
    for block in &mut function.blocks {
        let term = retarget(&block.term, |BlockId(b)| BlockId(forward[b]), |o| o);
        if term != block.term {
            block.term = term;
            changed = true;
        }
    }
    changed
}

/// Appends to each block that ends in a jump the block it jumps to, when it is the only way into
/// that block and that block is not the entry.
fn merge_blocks(function: &mut Function) -> bool {
    let mut preds = predecessors(function);
    let mut changed = false;
    for b in 0..function.blocks.len() {
        // a block keeps absorbing while it ends in a jump to a block only it leads to
        while let Terminator::Jump(BlockId(next)) = function.blocks[b].term {
            if next == b || next == 0 || preds[next] != [b] {
                break;
            }
            let term_span = function.blocks[b].term_span;
            let absorbed = std::mem::replace(
                &mut function.blocks[next],
                crate::codegen::ir::Block {
                    insts: Vec::new(),
                    spans: Vec::new(),
                    // nothing reaches it any more, so it returns nothing and is dropped
                    term: Terminator::Return(Vec::new()),
                    term_span,
                    loop_depth: 0,
                },
            );
            for BlockId(succ) in absorbed.term.successors() {
                for p in &mut preds[succ] {
                    if *p == next {
                        *p = b;
                    }
                }
                preds[succ].sort_unstable();
                preds[succ].dedup();
            }
            preds[next].clear();
            let block = &mut function.blocks[b];
            block.insts.extend(absorbed.insts);
            block.spans.extend(absorbed.spans);
            block.term = absorbed.term;
            block.term_span = absorbed.term_span;
            changed = true;
        }
    }
    changed
}

/// Drops the blocks the entry never reaches, keeping the others in their order.
fn drop_unreachable(function: &mut Function) -> bool {
    let count = function.blocks.len();
    let mut reached = vec![false; count];
    let mut stack = vec![0];
    while let Some(b) = stack.pop() {
        if reached[b] {
            continue;
        }
        reached[b] = true;
        stack.extend(function.blocks[b].term.successors().iter().map(|s| s.0));
    }
    if reached.iter().all(|r| *r) {
        return false;
    }
    let mut renumber = vec![usize::MAX; count];
    let mut next = 0;
    for (b, slot) in renumber.iter_mut().enumerate() {
        if reached[b] {
            *slot = next;
            next += 1;
        }
    }
    let blocks = std::mem::take(&mut function.blocks);
    for (b, mut block) in blocks.into_iter().enumerate() {
        if reached[b] {
            block.term = retarget(&block.term, |BlockId(t)| BlockId(renumber[t]), |o| o);
            function.blocks.push(block);
        }
    }
    true
}
