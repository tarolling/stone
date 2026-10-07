//! Liveness analysis and live intervals for register allocation.
//!
//! Positions number the blocks in layout order. Each block gets one position for its start, then
//! each instruction and the terminator get two: an even one where they read their inputs and the
//! odd one after it where they write their result. So a vreg whose last use is an instruction ends
//! before that instruction's result begins, and the two can share a register. For example, in
//! `b0: v1 = add v0, 1; ret v1`, the block starts at 0, the `add` reads at 2 and writes at 3, and
//! the `ret` reads at 4, so `v0` lives over `[0, 2]` and `v1` over `[3, 4]`.

use super::{Function, VReg};

/// The span of positions over which a vreg holds a value, with no holes.
#[derive(Clone, Debug, PartialEq)]
pub struct Interval {
    pub vreg: VReg,
    pub start: u32,
    pub end: u32,
    /// How costly keeping the vreg in memory would be: each read or write counts
    /// `10^loop_depth`, with the depth capped at 4.
    pub weight: f64,
    /// Whether a call happens while the vreg holds a value it needs afterward, so a register
    /// that calls clobber cannot hold it.
    pub crosses_call: bool,
}

/// The result of [`analyze`].
#[derive(Clone, Debug, PartialEq)]
pub struct Liveness {
    /// One interval per vreg that is ever read or written, sorted by start and then vreg.
    pub intervals: Vec<Interval>,
    /// Whether each vreg holds a value on entry, which for a parameter means its argument is
    /// needed. Indexed by vreg number.
    pub live_at_entry: Vec<bool>,
}

/// A fixed-size set of vregs.
#[derive(Clone, PartialEq)]
struct Set(Vec<u64>);

impl Set {
    fn new(size: u32) -> Set {
        Set(vec![0; (size as usize).div_ceil(64)])
    }

    fn insert(&mut self, reg: VReg) {
        self.0[reg.0 as usize / 64] |= 1 << (reg.0 % 64);
    }

    fn remove(&mut self, reg: VReg) {
        self.0[reg.0 as usize / 64] &= !(1 << (reg.0 % 64));
    }

    fn contains(&self, reg: VReg) -> bool {
        self.0[reg.0 as usize / 64] & (1 << (reg.0 % 64)) != 0
    }

    /// Adds every member of `other`, returning whether anything was new.
    fn union(&mut self, other: &Set) -> bool {
        let mut changed = false;
        for (word, extra) in self.0.iter_mut().zip(&other.0) {
            let merged = *word | extra;
            changed |= merged != *word;
            *word = merged;
        }
        changed
    }

    fn iter(&self) -> impl Iterator<Item = VReg> + '_ {
        self.0.iter().enumerate().flat_map(|(i, &word)| {
            (0..64)
                .filter(move |bit| word & (1 << bit) != 0)
                .map(move |bit| VReg((i * 64 + bit) as u32))
        })
    }
}

/// Computes which vregs are live where in `function`, and turns that into intervals.
pub fn analyze(function: &Function) -> Liveness {
    let count = function.vreg_count;
    let blocks = &function.blocks;

    // what each block reads before writing, and what it writes
    let mut reads = Vec::new();
    let mut writes = Vec::new();
    for block in blocks {
        let mut read = Set::new(count);
        let mut written = Set::new(count);
        for inst in &block.insts {
            for reg in inst.uses() {
                if !written.contains(reg) {
                    read.insert(reg);
                }
            }
            if let Some(reg) = inst.def() {
                written.insert(reg);
            }
        }
        for reg in block.term.uses() {
            if !written.contains(reg) {
                read.insert(reg);
            }
        }
        reads.push(read);
        writes.push(written);
    }

    // live-in = reads + (live-out - writes), iterated backward to a fixed point
    let mut live_in = vec![Set::new(count); blocks.len()];
    let mut live_out = vec![Set::new(count); blocks.len()];
    let mut changed = true;
    while changed {
        changed = false;
        for b in (0..blocks.len()).rev() {
            for succ in blocks[b].term.successors() {
                let succ_in = live_in[succ.0].clone();
                changed |= live_out[b].union(&succ_in);
            }
            let mut input = live_out[b].clone();
            for (word, written) in input.0.iter_mut().zip(&writes[b].0) {
                *word &= !written;
            }
            input.union(&reads[b]);
            changed |= live_in[b].union(&input);
        }
    }

    let mut start = vec![u32::MAX; count as usize];
    let mut end = vec![0u32; count as usize];
    let mut weight = vec![0f64; count as usize];
    let mut extend = |reg: VReg, pos: u32| {
        let i = reg.0 as usize;
        start[i] = start[i].min(pos);
        end[i] = end[i].max(pos);
    };

    let mut calls = Vec::new();
    let mut slot = 0u32;
    for (b, block) in blocks.iter().enumerate() {
        let block_start = 2 * slot;
        slot += 1;
        let first = slot;
        slot += block.insts.len() as u32 + 1;
        let block_end = 2 * (slot - 1) + 1;
        let cost = 10f64.powi(block.loop_depth.min(4) as i32);

        // walk backward from what is live out, so each def ends a range that started above it
        let mut live = live_out[b].clone();
        for reg in live.iter() {
            extend(reg, block_end);
        }
        let term_pos = 2 * (first + block.insts.len() as u32);
        for reg in block.term.uses() {
            extend(reg, term_pos);
            weight[reg.0 as usize] += cost;
            live.insert(reg);
        }
        for (i, inst) in block.insts.iter().enumerate().rev() {
            let use_pos = 2 * (first + i as u32);
            if inst.is_call() {
                calls.push(use_pos);
            }
            if let Some(reg) = inst.def() {
                extend(reg, use_pos + 1);
                weight[reg.0 as usize] += cost;
                live.remove(reg);
            }
            for reg in inst.uses() {
                extend(reg, use_pos);
                weight[reg.0 as usize] += cost;
                live.insert(reg);
            }
        }
        for reg in live.iter() {
            extend(reg, block_start);
        }
    }
    calls.sort_unstable();

    let mut intervals: Vec<Interval> = (0..count)
        .filter(|&i| start[i as usize] != u32::MAX)
        .map(|i| {
            let (s, e) = (start[i as usize], end[i as usize]);
            // the first call after the start, which must also be before the end's write slot
            let next_call = calls.partition_point(|&call| call <= s);
            let crosses_call = calls.get(next_call).is_some_and(|&call| e > call + 1);
            Interval {
                vreg: VReg(i),
                start: s,
                end: e,
                weight: weight[i as usize],
                crosses_call,
            }
        })
        .collect();
    intervals.sort_by_key(|interval| (interval.start, interval.vreg));

    let entry = live_in.first();
    let live_at_entry = (0..count)
        .map(|i| entry.is_some_and(|set| set.contains(VReg(i))))
        .collect();

    Liveness {
        intervals,
        live_at_entry,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeChecker;
    use crate::codegen::ir::lower::lower;
    use crate::driver::parse;

    /// Lowers `source` and analyzes its first function.
    fn liveness(source: &str) -> Liveness {
        let module = parse(source).unwrap();
        let analysis = TypeChecker::new().analyze(&module);
        assert!(
            analysis.diagnostics.is_empty(),
            "{:?}",
            analysis.diagnostics
        );
        let program = lower(&module, &analysis.types, &analysis.symbols).unwrap();
        analyze(&program.functions[0])
    }

    fn span(liveness: &Liveness, reg: u32) -> (u32, u32) {
        let interval = liveness
            .intervals
            .iter()
            .find(|i| i.vreg == VReg(reg))
            .unwrap();
        (interval.start, interval.end)
    }

    fn crosses(liveness: &Liveness, reg: u32) -> bool {
        liveness
            .intervals
            .iter()
            .find(|i| i.vreg == VReg(reg))
            .unwrap()
            .crosses_call
    }

    #[test]
    fn a_loop_carried_variable_lives_over_the_whole_loop() {
        // b0 starts at 0: copy 2/3, jmp 4/5; b1 at 6: br 8/9; b2 at 10: add 12/13, jmp 14/15;
        // b3 at 16: ret 18/19
        let liveness =
            liveness("def f(n);\n    i = 0\n    while i < n;\n        i = i + 1\n    ret i\n");
        assert_eq!(span(&liveness, 0), (0, 15));
        assert_eq!(span(&liveness, 1), (3, 18));
        assert!(liveness.live_at_entry[0]);
        assert!(!liveness.live_at_entry[1]);
    }

    #[test]
    fn a_temporary_ends_where_its_last_reader_reads() {
        // mul 2/3, add 4/5, ret 6/7
        let liveness = liveness("def f(a, b);\n    ret a * b + 1\n");
        assert_eq!(span(&liveness, 0), (0, 2));
        assert_eq!(span(&liveness, 2), (3, 4));
        assert_eq!(span(&liveness, 3), (5, 6));
    }

    #[test]
    fn only_values_needed_after_a_call_cross_it() {
        let liveness = liveness("def f(a, b);\n    ret g(a) + b\ndef g(x);\n    ret x\n");
        assert!(
            !crosses(&liveness, 0),
            "the argument is last read by the call"
        );
        assert!(crosses(&liveness, 1), "b is read after the call");
        assert!(!crosses(&liveness, 2), "the result is written by the call");
    }

    #[test]
    fn a_value_live_into_a_loop_crosses_a_call_at_the_top_of_the_loop() {
        let source = "def f(n);\n    t = 0\n    while 1;\n        print(t)\n        t = t + n\n";
        let liveness = liveness(source);
        assert!(crosses(&liveness, 0));
        assert!(crosses(&liveness, 1));
    }

    #[test]
    fn deeper_loops_weigh_more() {
        let liveness = liveness(
            "def f(n);\n    a = 0\n    b = 0\n    for i in range(n);\n        b = b + 1\n    ret a + b\n",
        );
        let weight = |reg: u32| {
            liveness
                .intervals
                .iter()
                .find(|i| i.vreg == VReg(reg))
                .unwrap()
                .weight
        };
        assert!(
            weight(2) > 10.0 * weight(1),
            "{} vs {}",
            weight(2),
            weight(1)
        );
    }
}
