//! Counts of the work an optimized program still does at run time to stay safe, and that C would
//! not do: index and division checks, checks of the call depth, other failures it tests for,
//! uniqueness checks, reference counting, and allocation.
//!
//! Each optimization that proves a check unnecessary should lower these counts, so they say what
//! a change removed without timing anything. `STONE_STATS` makes `stone build` print them for
//! each function and in total (see [`Stats::line`]), and the benchmark runner's `--checks` shows
//! the totals beside the times.

use super::{BinOp, Callee, Function, Inst, Operand, Program, Terminator};
use std::fmt;

/// How many of something a function has, and how many of those are inside a loop, where each
/// runs once per turn.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Count {
    pub all: usize,
    pub in_loops: usize,
}

impl Count {
    fn add(&mut self, in_loop: bool) {
        self.all += 1;
        if in_loop {
            self.in_loops += 1;
        }
    }

    fn merge(&mut self, other: Count) {
        self.all += other.all;
        self.in_loops += other.in_loops;
    }
}

impl fmt::Display for Count {
    /// Writes the count and how many are in loops, as `4/2`.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.all, self.in_loops)
    }
}

/// What a function, or a whole program, still does to stay safe.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Stats {
    /// Checked list reads and stores, and checked element slots.
    pub index: Count,
    /// Int divisions, remainders, and powers that test their operands, and float divisions and
    /// remainders that test for zero.
    pub division: Count,
    /// Checks of the call depth: one before each call of a stone function or a function value,
    /// and each that stands for an inlined call.
    pub depth: Count,
    /// Ways to stop the program that lowering wrote out as blocks, such as `math.sqrt`'s domain
    /// check, an `unwrap`, or a shift's range check.
    pub failure: Count,
    /// Uniqueness checks of a list before it changes.
    pub unique: Count,
    /// Retains and releases.
    pub counting: Count,
    /// Calls of runtime routines that allocate (see [`ALLOCATING`]).
    pub allocation: Count,
    /// Reads of a global that check it was assigned.
    pub global: Count,
}

/// The runtime routines that always allocate a new object.
pub const ALLOCATING: &[&str] = &[
    "stone.alloc",
    "stone.list_new",
    "stone.list_new_wide",
    "stone.list_copy",
    "stone.list_slice",
    "stone.enum_new",
    "stone.str_new",
    "stone.str_concat",
    "stone.str_slice",
    "stone.str_int",
    "stone.str_float",
];

impl Stats {
    /// Counts what `function` still does.
    ///
    /// For example, the IR of `fn f(xs: list[int], i: int) -> int: ret xs[i]` has one index check
    /// outside any loop, so its `index` is `1/0`.
    pub fn of(function: &Function) -> Stats {
        let mut stats = Stats::default();
        for block in &function.blocks {
            let in_loop = block.loop_depth > 0;
            for inst in &block.insts {
                let count = match inst {
                    Inst::ListLoad { .. } | Inst::ListStore { .. } | Inst::ElementSlot { .. } => {
                        Some(&mut stats.index)
                    }
                    Inst::Binary {
                        op: BinOp::Div | BinOp::Rem,
                        rhs: Operand::Imm(divisor),
                        ..
                    } => (*divisor == 0 || *divisor == -1).then_some(&mut stats.division),
                    Inst::Binary {
                        op: BinOp::Pow,
                        rhs: Operand::Imm(exponent),
                        ..
                    } => (*exponent < 0).then_some(&mut stats.division),
                    Inst::Binary {
                        op: BinOp::Div | BinOp::Rem | BinOp::Pow,
                        ..
                    }
                    | Inst::FloatBinary {
                        op: BinOp::Div | BinOp::Rem,
                        ..
                    } => Some(&mut stats.division),
                    Inst::DepthCheck
                    | Inst::Call {
                        callee: Callee::User(_) | Callee::Value(_),
                        ..
                    } => Some(&mut stats.depth),
                    Inst::ListUnique { .. } => Some(&mut stats.unique),
                    Inst::Retain { .. } | Inst::Release { .. } => Some(&mut stats.counting),
                    Inst::Call {
                        callee: Callee::Runtime(label),
                        ..
                    } => ALLOCATING.contains(label).then_some(&mut stats.allocation),
                    Inst::LoadGlobal { checked: true, .. } => Some(&mut stats.global),
                    _ => None,
                };
                if let Some(count) = count {
                    count.add(in_loop);
                }
            }
            if matches!(block.term, Terminator::Fail(..)) {
                stats.failure.add(in_loop);
            }
        }
        stats
    }

    /// Counts what every function of `program` still does, together.
    pub fn of_program(program: &Program) -> Stats {
        let mut total = Stats::default();
        for function in &program.functions {
            total.merge(Stats::of(function));
        }
        total
    }

    /// Returns the checks of every kind, which stop the program when they fail, together.
    pub fn checks(&self) -> Count {
        let mut checks = Count::default();
        for count in [
            self.index,
            self.division,
            self.depth,
            self.failure,
            self.global,
        ] {
            checks.merge(count);
        }
        checks
    }

    fn merge(&mut self, other: Stats) {
        self.index.merge(other.index);
        self.division.merge(other.division);
        self.depth.merge(other.depth);
        self.failure.merge(other.failure);
        self.unique.merge(other.unique);
        self.counting.merge(other.counting);
        self.allocation.merge(other.allocation);
        self.global.merge(other.global);
    }

    /// Returns the line `STONE_STATS` prints for these counts of `name`, each as all of them and
    /// those in loops, such as
    /// `stone stats fn.f: checks=3/1 index=1/1 division=0/0 depth=2/0 failure=0/0 global=0/0
    /// unique=0/0 counting=0/0 allocation=0/0`.
    pub fn line(&self, name: &str) -> String {
        format!(
            "stone stats {name}: checks={} index={} division={} depth={} failure={} global={} \
             unique={} counting={} allocation={}",
            self.checks(),
            self.index,
            self.division,
            self.depth,
            self.failure,
            self.global,
            self.unique,
            self.counting,
            self.allocation,
        )
    }
}

/// Returns the lines `STONE_STATS` prints for `program`: one per function that has anything to
/// count, in order, then the total.
pub fn report(program: &Program) -> Vec<String> {
    let mut lines: Vec<String> = program
        .functions
        .iter()
        .filter_map(|function| {
            let stats = Stats::of(function);
            let name = function.name.as_deref().unwrap_or("main");
            (stats != Stats::default()).then(|| stats.line(name))
        })
        .collect();
    lines.push(Stats::of_program(program).line("total"));
    lines
}

/// Returns whether `STONE_STATS` asks for the counts, set to anything but `0`.
pub fn enabled() -> bool {
    std::env::var_os("STONE_STATS").is_some_and(|v| v != "0")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeChecker;
    use crate::codegen::ir::lower::lower;
    use crate::codegen::ir::opt::optimize;
    use crate::driver::parse_program;

    fn stats(source: &str, name: &str) -> Stats {
        let module = parse_program(source).unwrap();
        let analysis = TypeChecker::new().analyze(&module);
        let mut program = lower(&module, &analysis).unwrap();
        optimize(&mut program);
        let function = program
            .functions
            .iter()
            .find(|f| f.name.as_deref() == Some(name))
            .unwrap();
        Stats::of(function)
    }

    #[test]
    fn a_read_by_any_index_is_one_check() {
        let s = stats("fn f(xs: list[int], i: int) -> int:\n    ret xs[i]\n", "f");
        assert_eq!(
            s.index,
            Count {
                all: 1,
                in_loops: 0
            }
        );
        assert_eq!(s.division, Count::default());
    }

    #[test]
    fn a_read_a_loop_proves_in_range_is_no_check() {
        let s = stats(
            "fn f(xs: list[int]) -> int:\n    t = 0\n    for i in range(xs.len()):\n        \
             t += xs[i]\n    ret t\n",
            "f",
        );
        assert_eq!(s.index, Count::default());
    }

    #[test]
    fn division_by_a_variable_is_a_check_and_by_a_constant_is_not() {
        let s = stats(
            "fn f(a: int, b: int) -> int:\n    for i in range(a):\n        a = a / b + a % 7\n    \
             ret a\n",
            "f",
        );
        assert_eq!(
            s.division,
            Count {
                all: 1,
                in_loops: 1
            }
        );
    }

    #[test]
    fn a_recursive_call_checks_the_depth() {
        let s = stats(
            "fn fib(n: int) -> int:\n    if n < 2:\n        ret n\n    \
             ret fib(n - 1) + fib(n - 2)\n",
            "fib",
        );
        assert_eq!(s.depth.all, 2);
        assert_eq!(s.checks().all, 2);
    }

    #[test]
    fn the_line_writes_every_count() {
        let mut s = Stats::default();
        s.index.add(true);
        s.failure.add(false);
        assert_eq!(
            s.line("fn.f"),
            "stone stats fn.f: checks=2/1 index=1/1 division=0/0 depth=0/0 failure=1/0 \
             global=0/0 unique=0/0 counting=0/0 allocation=0/0"
        );
    }
}
