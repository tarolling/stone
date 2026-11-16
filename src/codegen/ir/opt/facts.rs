//! Removal of checks that facts about values prove never fail: an index from 0 to below its
//! list's length, a branch to a failure whose comparison never holds, such as `math.sqrt`'s check
//! that a sum of squares is negative, and a float division by a constant other than zero.
//!
//! The facts are a small, decidable kind, inferred and never written: for ints, bounds of the form
//! `x - y <= c`, where `x` and `y` are vregs, zero, or the number of elements of a list (see
//! [`Term`]), and for floats, which vregs are never below zero. They come from constants, copies,
//! adding a constant, lengths, the bounds of other arithmetic, and the comparisons a branch makes
//! on each of its ways, and flow forward through the function to a fixed point, so a `while` loop's
//! test proves what a `range` does: in `while m < n:`, `m < n` holds in the loop's body.
//!
//! In `for j in range(i + 1, n)`, with `n = bodies.len()`, the counter is below the list's number
//! of elements and never negative, so `bodies[j]` reads its slots unchecked, and in
//! `math.sqrt(dx * dx + dy * dy)` the argument is a sum of squares, which is never below zero
//! (it may be nan, but nan is not below zero either), so the domain check goes.
//!
//! Every fact about a list's length dies at a call, which may change any list, and every fact
//! about a vreg when it is written, except that adding a constant to a vreg shifts the facts
//! about it. Ints wrap, so a fact derived through arithmetic needs bounds that show the
//! arithmetic cannot overflow, and any vreg is between `i64::MIN` and `i64::MAX` and any number of
//! elements between 0 and [`MAX_LEN`]. Loops reach a fixed point because a fact whose bound at a
//! block keeps changing is dropped there after a few rounds.
//!
//! With [`Options::verify`], each removed check stays as [`Inst::Never`]s, so a wrong proof
//! fails the program with `compiler bug` instead of reading out of bounds.

use super::{
    BinOp, Block, BlockId, Cond, Function, Inst, Operand, Options, Terminator, VReg, never_outside,
    predecessors,
};
use crate::codegen::ir::Callee;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// More elements than any list can hold, far more than memory allows, so that adding a small
/// constant to a number of elements never overflows.
pub(super) const MAX_LEN: i128 = 1 << 60;

/// How many times a block's facts may change before a fact whose bound still changes is dropped
/// there, which ends the iteration around loops.
const WIDEN_AFTER: usize = 3;

/// How many bounds a point may know, beyond which the analysis gives up on a function rather than
/// slow the build down.
const MAX_BOUNDS: usize = 400;

/// Returns whether a bound of `c` says more than every int or number of elements being within
/// its range, so that one only implied is worth keeping.
fn useful(c: i128) -> bool {
    c.abs() < MAX_LEN
}

/// Something an int bound is about.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Term {
    Zero,
    Reg(VReg),
    /// The number of elements of the list in the vreg, whose elements are this many slots each.
    Len(VReg, usize),
    /// The fewest slots of the lists that are the first elements of the list in the vreg, as
    /// many as [`State::rows`] says, which an [`Inst::MinRowLen`] measured.
    Rows(VReg),
}

impl Term {
    /// Returns the term and constant an operand is: a vreg plus 0, or zero plus the constant.
    fn of(operand: Operand) -> (Term, i128) {
        match operand {
            Operand::Reg(reg) => (Term::Reg(reg), 0),
            Operand::Imm(n) => (Term::Zero, n as i128),
        }
    }

    fn mentions(self, reg: VReg) -> bool {
        matches!(self, Term::Reg(r) | Term::Len(r, _) | Term::Rows(r) if r == reg)
    }

    /// Returns the list whose length or rows the term is about, if it is about one.
    fn list(self) -> Option<VReg> {
        match self {
            Term::Len(r, _) | Term::Rows(r) => Some(r),
            _ => None,
        }
    }
}

/// What is known at a point of a function.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
struct State {
    /// `x - y <= c` for each `(x, y)` and its `c`.
    bounds: HashMap<(Term, Term), i128>,
    /// The vregs holding floats that are never below zero: at least zero, `-0.0`, or nan.
    not_negative: HashSet<VReg>,
    /// For each list a [`Term::Rows`] is about, how many of its first elements were measured.
    rows: HashMap<VReg, Operand>,
    /// The function's lists that nothing changes while it runs (see [`frozen`]), the same at
    /// every point.
    frozen: Rc<HashSet<VReg>>,
}

impl State {
    /// Notes that `x - y <= c`.
    fn add(&mut self, x: Term, y: Term, c: i128) {
        if x == y {
            return;
        }
        let bound = self.bounds.entry((x, y)).or_insert(c);
        *bound = (*bound).min(c);
    }

    /// Notes that `x == y + c`.
    fn equal(&mut self, x: Term, y: Term, c: i128) {
        self.add(x, y, c);
        self.add(y, x, -c);
    }

    /// Forgets everything about `reg`, which is about to be written.
    fn kill(&mut self, reg: VReg) {
        self.bounds
            .retain(|(x, y), _| !x.mentions(reg) && !y.mentions(reg));
        self.not_negative.remove(&reg);
        self.rows
            .retain(|&list, count| list != reg && *count != Operand::Reg(reg));
    }

    /// Forgets every list's number of elements and rows, which a call may change, except those
    /// of frozen lists when `keep_frozen`.
    fn kill_lengths(&mut self, keep_frozen: bool) {
        let frozen = Rc::clone(&self.frozen);
        let kept = |t: &Term| t.list().is_none_or(|r| keep_frozen && frozen.contains(&r));
        self.bounds.retain(|(x, y), _| kept(x) && kept(y));
        self.rows
            .retain(|list, _| keep_frozen && frozen.contains(list));
    }

    /// Forgets what is known of the rows of every list but frozen ones, since a store may put a
    /// new row in a list another vreg also holds.
    fn kill_rows(&mut self) {
        let frozen = Rc::clone(&self.frozen);
        let kept = |t: &Term| !matches!(t, Term::Rows(r) if !frozen.contains(r));
        self.bounds.retain(|(x, y), _| kept(x) && kept(y));
        self.rows.retain(|list, _| frozen.contains(list));
    }

    /// Returns whether a call leaves frozen lists' lengths and rows alone: a stone function,
    /// which changes only what it owns, and nothing frozen is ever given away, or a runtime
    /// routine on lists, which changes only its first argument, made unique first.
    fn keeps_frozen(&self, callee: &Callee, args: &[Operand]) -> bool {
        match callee {
            Callee::User(_) | Callee::Helper(_) | Callee::Value(_) | Callee::Print(_) => true,
            Callee::Runtime(name) => {
                name.starts_with("stone.list_")
                    && !matches!(args.first(), Some(Operand::Reg(r)) if self.frozen.contains(r))
            }
        }
    }

    /// Returns the least `c` the bounds prove `from - to <= c` for, or `None` if they prove
    /// none, or contradict each other, as they only can where the code never runs.
    fn shortest(&self, from: Term, to: Term) -> Option<i128> {
        if from == to {
            return Some(0);
        }
        self.distances(from, to, false)?
            .into_iter()
            .find_map(|(t, d)| (t == to).then_some(d))
    }

    /// Returns, for each term the bounds mention (and `also`), the least `c` they prove
    /// `source - term <= c` for, or with `reversed`, `term - source <= c`, leaving out terms
    /// they prove nothing about; or `None` if the bounds contradict each other.
    fn distances(&self, source: Term, also: Term, reversed: bool) -> Option<Vec<(Term, i128)>> {
        let mut nodes: Vec<Term> = vec![Term::Zero, source, also];
        for &(x, y) in self.bounds.keys() {
            nodes.push(x);
            nodes.push(y);
        }
        nodes.sort_unstable();
        nodes.dedup();
        let index = |t: Term| nodes.binary_search(&t).expect("every term is a node");
        let mut edges: Vec<(usize, usize, i128)> = self
            .bounds
            .iter()
            .map(|(&(x, y), &c)| (index(x), index(y), c))
            .collect();
        // every int is within i64, and every number of elements within 0 and MAX_LEN
        let zero = index(Term::Zero);
        for (i, &t) in nodes.iter().enumerate() {
            match t {
                Term::Zero => {}
                Term::Reg(_) => {
                    edges.push((i, zero, i64::MAX as i128));
                    edges.push((zero, i, -(i64::MIN as i128)));
                }
                Term::Len(..) | Term::Rows(_) => {
                    edges.push((i, zero, MAX_LEN));
                    edges.push((zero, i, 0));
                }
            }
        }
        if reversed {
            for edge in &mut edges {
                *edge = (edge.1, edge.0, edge.2);
            }
        }
        let mut distance: Vec<Option<i128>> = vec![None; nodes.len()];
        distance[index(source)] = Some(0);
        for round in 0..=nodes.len() {
            let mut changed = false;
            for &(x, y, c) in &edges {
                if let Some(d) = distance[x]
                    && distance[y].is_none_or(|old| d + c < old)
                {
                    distance[y] = Some(d + c);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
            if round == nodes.len() {
                // a cycle below zero: the bounds contradict each other
                return None;
            }
        }
        Some(
            nodes
                .into_iter()
                .zip(distance)
                .filter_map(|(t, d)| Some((t, d?)))
                .collect(),
        )
    }

    /// Returns whether the bounds can all hold at once, which they cannot only where the code
    /// never runs, as on the way out of a branch whose test the bounds already decide.
    ///
    /// For example, `x - y <= -1` and `y - x <= 0` (`x < y` and `y <= x`) cannot.
    fn consistent(&self) -> bool {
        // from a source that reaches every term at 0, distances settle unless a cycle is negative
        let mut distance: HashMap<Term, i128> = HashMap::new();
        for &(x, y) in self.bounds.keys() {
            distance.insert(x, 0);
            distance.insert(y, 0);
        }
        for _ in 0..=distance.len() {
            let mut changed = false;
            for (&(x, y), &c) in &self.bounds {
                let through = distance[&x] + c;
                if through < distance[&y] {
                    distance.insert(y, through);
                    changed = true;
                }
            }
            if !changed {
                return true;
            }
        }
        false
    }

    /// Returns whether the bounds prove `x - y <= c`.
    fn holds(&self, x: Term, y: Term, c: i128) -> bool {
        self.shortest(x, y).is_some_and(|d| d <= c)
    }

    /// Returns the bounds proven for `t`, lowest and highest.
    fn range(&self, t: Term) -> (i128, i128) {
        let high = self.shortest(t, Term::Zero).unwrap_or(i64::MAX as i128);
        let low = self
            .shortest(Term::Zero, t)
            .map_or(i64::MIN as i128, |d| -d);
        (low, high)
    }

    /// Notes that `lhs` compares with `rhs` as `cond`, as ints.
    fn assume(&mut self, cond: Cond, lhs: Operand, rhs: Operand) {
        let ((a, ka), (b, kb)) = (Term::of(lhs), Term::of(rhs));
        // a + ka against b + kb
        match cond {
            Cond::Lt => self.add(a, b, kb - ka - 1),
            Cond::Le => self.add(a, b, kb - ka),
            Cond::Gt => self.add(b, a, ka - kb - 1),
            Cond::Ge => self.add(b, a, ka - kb),
            Cond::Eq => self.equal(a, b, kb - ka),
            Cond::Ne => {}
        }
    }

    /// Returns whether `lhs` always compares with `rhs` as `cond` (`Some(true)`), never does
    /// (`Some(false)`), or either may happen, as ints.
    fn decides(&self, cond: Cond, lhs: Operand, rhs: Operand) -> Option<bool> {
        let ((a, ka), (b, kb)) = (Term::of(lhs), Term::of(rhs));
        let lt = || self.holds(a, b, kb - ka - 1);
        let le = || self.holds(a, b, kb - ka);
        let gt = || self.holds(b, a, ka - kb - 1);
        let ge = || self.holds(b, a, ka - kb);
        match cond {
            Cond::Lt if lt() => Some(true),
            Cond::Lt if ge() => Some(false),
            Cond::Le if le() => Some(true),
            Cond::Le if gt() => Some(false),
            Cond::Gt if gt() => Some(true),
            Cond::Gt if le() => Some(false),
            Cond::Ge if ge() => Some(true),
            Cond::Ge if lt() => Some(false),
            Cond::Eq if le() && ge() => Some(true),
            Cond::Eq if lt() || gt() => Some(false),
            Cond::Ne if lt() || gt() => Some(true),
            Cond::Ne if le() && ge() => Some(false),
            _ => None,
        }
    }

    /// Returns whether the float `operand` is never below zero.
    fn not_negative(&self, operand: Operand) -> bool {
        match operand {
            Operand::Reg(reg) => self.not_negative.contains(&reg),
            Operand::Imm(bits) => f64::from_bits(bits as u64) >= 0.0,
        }
    }

    /// Returns whether a float comparison `lhs` `cond` `rhs` never holds, for a `lhs` never
    /// below zero against a constant.
    fn never_float(&self, cond: Cond, lhs: Operand, rhs: Operand) -> bool {
        let Operand::Imm(bits) = rhs else {
            return false;
        };
        let bound = f64::from_bits(bits as u64);
        self.not_negative(lhs)
            && match cond {
                // -0.0 is the least such value, and nan compares false
                Cond::Lt => bound <= 0.0,
                Cond::Le => bound < 0.0,
                _ => false,
            }
    }

    /// Returns the list whose number of elements `reg` is, if a bound says so exactly.
    fn length_of(&self, reg: VReg) -> Option<VReg> {
        self.bounds.iter().find_map(|(&(x, y), &c)| match (x, y) {
            (Term::Reg(r), Term::Len(list, 1))
                if r == reg && c == 0 && self.bounds.get(&(y, x)) == Some(&0) =>
            {
                Some(list)
            }
            _ => None,
        })
    }

    /// Notes the bounds `reg` gets from an operation whose exact result lies within `low` and
    /// `high`, if both are within an int's range: past either, the result may wrap around to
    /// anything.
    fn bound(&mut self, reg: VReg, low: i128, high: i128) {
        if low >= i64::MIN as i128 && high <= i64::MAX as i128 {
            self.add(Term::Reg(reg), Term::Zero, high);
            self.add(Term::Zero, Term::Reg(reg), -low);
        }
    }

    /// Updates the facts past `inst`.
    fn step(&mut self, inst: &Inst) {
        match *inst {
            Inst::Copy { dst, src } => {
                let (t, k) = Term::of(src);
                if t == Term::Reg(dst) {
                    return;
                }
                let not_negative = self.not_negative(src);
                self.kill(dst);
                self.equal(Term::Reg(dst), t, k);
                // a copy of a list holds the same list, so its length is the same
                if let Term::Reg(src) = t {
                    let renamed = |t: Term| match t {
                        Term::Len(r, width) if r == src => Term::Len(dst, width),
                        t => t,
                    };
                    let lengths: Vec<_> = self
                        .bounds
                        .iter()
                        .filter(|((x, y), _)| {
                            matches!(x, Term::Len(r, _) if *r == src)
                                || matches!(y, Term::Len(r, _) if *r == src)
                        })
                        .map(|(&(x, y), &c)| (renamed(x), renamed(y), c))
                        .collect();
                    for (x, y, c) in lengths {
                        self.add(x, y, c);
                    }
                }
                if not_negative {
                    self.not_negative.insert(dst);
                }
            }
            Inst::Binary {
                op: op @ (BinOp::Add | BinOp::Sub),
                dst,
                lhs,
                rhs,
            } => {
                let shift = match (op, lhs, rhs) {
                    (BinOp::Add, Operand::Reg(a), Operand::Imm(c))
                    | (BinOp::Add, Operand::Imm(c), Operand::Reg(a)) => Some((a, c as i128)),
                    (BinOp::Sub, Operand::Reg(a), Operand::Imm(c)) => Some((a, -(c as i128))),
                    _ => None,
                };
                match shift {
                    Some((a, c)) => self.shift(dst, a, c),
                    None => {
                        let (l1, h1) = self.range(Term::of(lhs).0);
                        let (l2, h2) = self.range(Term::of(rhs).0);
                        let (k1, k2) = (Term::of(lhs).1, Term::of(rhs).1);
                        let (low, high) = match op {
                            BinOp::Add => (l1 + k1 + l2 + k2, h1 + k1 + h2 + k2),
                            _ => (l1 + k1 - (h2 + k2), h1 + k1 - (l2 + k2)),
                        };
                        self.kill(dst);
                        self.bound(dst, low, high);
                    }
                }
            }
            Inst::Binary { op, dst, lhs, rhs } => {
                let value = |o: Operand| match Term::of(o) {
                    (Term::Zero, k) => (k, k),
                    (t, _) => self.range(t),
                };
                let ((l1, h1), (l2, h2)) = (value(lhs), value(rhs));
                let length = match (op, lhs, rhs) {
                    (BinOp::Div, Operand::Reg(a), Operand::Imm(w)) if w >= 1 => {
                        self.length_of(a).map(|list| (list, w as usize))
                    }
                    _ => None,
                };
                let range = match (op, rhs) {
                    (BinOp::Mul, _) => {
                        let corners = [l1 * l2, l1 * h2, h1 * l2, h1 * h2];
                        let fits = |n: &i128| (i64::MIN as i128..=i64::MAX as i128).contains(n);
                        corners.iter().all(fits).then(|| {
                            (
                                *corners.iter().min().unwrap(),
                                *corners.iter().max().unwrap(),
                            )
                        })
                    }
                    (BinOp::Div, Operand::Imm(w)) if w >= 1 => {
                        Some((l1 / w as i128, h1 / w as i128))
                    }
                    (BinOp::Rem, Operand::Imm(m)) if m >= 1 => {
                        let m = m as i128 - 1;
                        Some(if l1 >= 0 { (0, h1.min(m)) } else { (-m, m) })
                    }
                    (BinOp::And, _) if l1 >= 0 || l2 >= 0 => {
                        let high = if l1 >= 0 && l2 >= 0 {
                            h1.min(h2)
                        } else if l1 >= 0 {
                            h1
                        } else {
                            h2
                        };
                        Some((0, high))
                    }
                    (BinOp::Shr, Operand::Imm(s)) if l1 >= 0 && (0..64).contains(&s) => {
                        Some((0, h1 >> s))
                    }
                    _ => None,
                };
                self.kill(dst);
                if let Some((list, width)) = length {
                    self.equal(Term::Reg(dst), Term::Len(list, width), 0);
                }
                if let Some((low, high)) = range {
                    self.bound(dst, low, high);
                }
            }
            Inst::Neg { dst, src } => {
                let (low, high) = match Term::of(src) {
                    (Term::Zero, k) => (k, k),
                    (t, _) => self.range(t),
                };
                self.kill(dst);
                if low > i64::MIN as i128 {
                    self.bound(dst, -high, -low);
                }
            }
            Inst::ListLen {
                dst,
                list: Operand::Reg(list),
            } => {
                self.kill(dst);
                self.equal(Term::Reg(dst), Term::Len(list, 1), 0);
            }
            // a list made unique keeps its elements, and so its length
            Inst::ListUnique {
                dst,
                src: Operand::Reg(src),
                ..
            } if dst == src => {}
            // a new list has as many slots as asked for
            Inst::Call {
                ref dsts,
                callee: Callee::Runtime("stone.list_new"),
                ref args,
            } if dsts.len() == 1 && !args.is_empty() => {
                let dst = dsts[0];
                self.kill_lengths(true);
                self.kill(dst);
                let (t, k) = Term::of(args[0]);
                if !matches!(t, Term::Reg(r) if r == dst) {
                    self.equal(Term::Len(dst, 1), t, k);
                }
            }
            // appending adds one slot to its list and changes no other, though another vreg may
            // hold the same list
            Inst::Call {
                ref dsts,
                callee: Callee::Runtime("stone.list_append"),
                ref args,
            } if dsts.is_empty() && matches!(args.first(), Some(Operand::Reg(_))) => {
                let Some(&Operand::Reg(list)) = args.first() else {
                    unreachable!("the guard matched a vreg");
                };
                // what is known of the length against every other term, implied bounds
                // included, since the way into a loop of appends often knows `len == 0` and
                // `i == 0` but not `len == i`
                let len = Term::Len(list, 1);
                let other = |t: &Term| t != &len && t.list().is_none();
                let mut kept = Vec::new();
                if let (Some(out), Some(back)) = (
                    self.distances(len, len, false),
                    self.distances(len, len, true),
                ) {
                    for (t, d) in out {
                        if other(&t) && useful(d) {
                            kept.push((len, t, d + 1));
                        }
                    }
                    for (t, d) in back {
                        if other(&t) && useful(d) {
                            kept.push((t, len, d - 1));
                        }
                    }
                }
                let keep = self.keeps_frozen(&Callee::Runtime("stone.list_append"), args);
                self.kill_lengths(keep);
                for (x, y, c) in kept {
                    self.add(x, y, c);
                }
            }
            Inst::Call {
                ref dsts,
                ref callee,
                ref args,
            } => {
                let keep = self.keeps_frozen(callee, args);
                self.kill_lengths(keep);
                for &dst in dsts {
                    self.kill(dst);
                }
            }
            // the guard before a loop nest measured the first `count` rows
            Inst::MinRowLen {
                dst,
                list: Operand::Reg(list),
                count,
            } if count != Operand::Reg(dst) && list != dst => {
                self.kill(dst);
                self.bounds
                    .retain(|(x, y), _| *x != Term::Rows(list) && *y != Term::Rows(list));
                self.equal(Term::Reg(dst), Term::Rows(list), 0);
                self.rows.insert(list, count);
            }
            // a row read from below the measured count is at least as long as the shortest
            Inst::ListLoad {
                dst,
                list: Operand::Reg(list),
                index,
            }
            | Inst::ListGet {
                dst,
                list: Operand::Reg(list),
                index,
                offset: 0,
            } if list != dst => {
                let measured = self.rows.get(&list).copied().is_some_and(|count| {
                    let (t, k) = Term::of(index);
                    let (c, m) = Term::of(count);
                    // 0 <= index + k < count + m
                    self.holds(Term::Zero, t, k) && self.holds(t, c, m - k - 1)
                });
                self.kill(dst);
                if measured {
                    self.add(Term::Rows(list), Term::Len(dst, 1), 0);
                }
            }
            Inst::ListStore { .. }
            | Inst::ListSet { .. }
            | Inst::DataSet { .. }
            | Inst::ListInit { .. } => {
                self.kill_rows();
                for dst in inst.defs() {
                    self.kill(dst);
                }
            }
            Inst::FloatBinary { op, dst, lhs, rhs } => {
                let not_negative = match op {
                    BinOp::Mul => lhs == rhs || (self.not_negative(lhs) && self.not_negative(rhs)),
                    BinOp::Add => self.not_negative(lhs) && self.not_negative(rhs),
                    _ => false,
                };
                self.kill(dst);
                if not_negative {
                    self.not_negative.insert(dst);
                }
            }
            Inst::FloatSqrt { dst, .. } | Inst::FloatAbs { dst, .. } => {
                self.kill(dst);
                self.not_negative.insert(dst);
            }
            Inst::IntToFloat { dst, src } => {
                let not_negative = match Term::of(src) {
                    (Term::Zero, k) => k >= 0,
                    (t, _) => self.range(t).0 >= 0,
                };
                self.kill(dst);
                if not_negative {
                    self.not_negative.insert(dst);
                }
            }
            _ => {
                for dst in inst.defs() {
                    self.kill(dst);
                }
            }
        }
    }

    /// Updates the facts past `dst = a + c`, shifting those about `a` when `dst` is `a`, as
    /// long as the addition cannot overflow.
    fn shift(&mut self, dst: VReg, a: VReg, c: i128) {
        let (low, high) = self.range(Term::Reg(a));
        let safe = high + c <= i64::MAX as i128 && low + c >= i64::MIN as i128;
        if dst != a {
            self.kill(dst);
            if safe {
                self.equal(Term::Reg(dst), Term::Reg(a), c);
            }
            return;
        }
        if !safe {
            self.kill(dst);
            return;
        }
        let old = std::mem::take(&mut self.bounds);
        for ((x, y), bound) in old {
            let bound = if x == Term::Reg(a) {
                bound + c
            } else if y == Term::Reg(a) {
                bound - c
            } else {
                bound
            };
            self.bounds.insert((x, y), bound);
        }
        self.not_negative.remove(&a);
    }

    /// Returns what holds both here and in `other`: each bound one knows and the other knows or
    /// implies, at the weaker of the two, and each float both know is never below zero. A bound
    /// only implied on one side is kept, since around a loop the way in often knows `x == 0` and
    /// `y == 0` where the way back knows `x == y` outright.
    fn meet(&self, other: &State) -> State {
        let implied = |state: &State, &(x, y): &(Term, Term)| {
            state
                .bounds
                .get(&(x, y))
                .copied()
                .or_else(|| state.shortest(x, y).filter(|&c| useful(c)))
        };
        let mut bounds: HashMap<(Term, Term), i128> = HashMap::new();
        for (key, &c) in &self.bounds {
            if let Some(d) = implied(other, key) {
                bounds.insert(*key, c.max(d));
            }
        }
        for (key, &d) in &other.bounds {
            if !bounds.contains_key(key)
                && let Some(c) = implied(self, key)
            {
                bounds.insert(*key, c.max(d));
            }
        }
        let not_negative = self
            .not_negative
            .intersection(&other.not_negative)
            .copied()
            .collect();
        let rows = self
            .rows
            .iter()
            .filter(|(list, count)| other.rows.get(list) == Some(count))
            .map(|(&list, &count)| (list, count))
            .collect();
        State {
            bounds,
            not_negative,
            rows,
            frozen: Rc::clone(&self.frozen),
        }
    }
}

/// The facts on each way out of a block, given those at its end, leaving out a way the facts
/// rule out.
fn ways_out(state: &State, term: &Terminator) -> Vec<(BlockId, State)> {
    match *term {
        Terminator::CmpBranch {
            cond,
            float,
            lhs,
            rhs,
            then,
            otherwise,
        } if then != otherwise => {
            let (mut taken, mut not_taken) = (state.clone(), state.clone());
            if float {
                // a float compared with a constant at least zero, either way, is never below zero
                let floor = matches!(rhs, Operand::Imm(bits) if f64::from_bits(bits as u64) >= 0.0);
                if let (Operand::Reg(reg), true) = (lhs, floor) {
                    match cond {
                        Cond::Gt | Cond::Ge | Cond::Eq => {
                            taken.not_negative.insert(reg);
                        }
                        Cond::Lt => {
                            not_taken.not_negative.insert(reg);
                        }
                        _ => {}
                    }
                }
            } else {
                taken.assume(cond, lhs, rhs);
                not_taken.assume(cond.negate(), lhs, rhs);
            }
            // a way the bounds rule out is not taken, so far as they know
            [(then, taken), (otherwise, not_taken)]
                .into_iter()
                .filter(|(_, state)| state.consistent())
                .collect()
        }
        _ => term
            .successors()
            .into_iter()
            .map(|b| (b, state.clone()))
            .collect(),
    }
}

/// Returns the facts on entry to each block, `None` for a block nothing reaches, or nothing if
/// the function is too big to analyze.
fn analyze(function: &Function) -> Option<Vec<Option<State>>> {
    let count = function.blocks.len();
    let preds = predecessors(function);
    let mut entry: Vec<Option<State>> = vec![None; count];
    entry[0] = Some(State {
        frozen: Rc::new(frozen(function)),
        ..State::default()
    });
    // the facts on each way from a block to another
    let mut edges: HashMap<(usize, usize), State> = HashMap::new();
    let mut changes = vec![0; count];
    let mut changed = true;
    while changed {
        changed = false;
        for b in 0..count {
            if b != 0 {
                let mut incoming = preds[b].iter().filter_map(|&p| edges.get(&(p, b)));
                let Some(first) = incoming.next() else {
                    continue;
                };
                let mut state = incoming.fold(first.clone(), |acc, s| acc.meet(s));
                if let Some(old) = &entry[b] {
                    if *old == state {
                        continue;
                    }
                    changes[b] += 1;
                    if changes[b] > WIDEN_AFTER {
                        // drop what still changes, so loops end
                        state.bounds.retain(|key, c| old.bounds.get(key) == Some(c));
                        state.not_negative.retain(|r| old.not_negative.contains(r));
                    }
                }
                entry[b] = Some(state);
            }
            let mut state = entry[b].clone().expect("a reached block has facts");
            for inst in &function.blocks[b].insts {
                state.step(inst);
            }
            if state.bounds.len() > MAX_BOUNDS {
                return None;
            }
            for (BlockId(to), out) in ways_out(&state, &function.blocks[b].term) {
                if edges.get(&(b, to)) != Some(&out) {
                    edges.insert((b, to), out);
                    changed = true;
                }
            }
        }
    }
    Some(entry)
}

/// Returns the lists that nothing changes while the function runs: the parameters it never
/// writes, and what it only ever copies or loads from such a list, or sets to no list at all.
///
/// stone has value semantics, so a parameter the function never writes is one it borrows: its
/// caller holds the list, and every list inside it, which nothing can change in place, since a
/// change makes a list unique first, copying one that anything else also holds. A runtime
/// routine changes only the list it is given first, which lowering made unique.
///
/// For example, in `fn f(b: list[list[int]])`, `b` and every `row = b[k]` are frozen.
pub(super) fn frozen(function: &Function) -> HashSet<VReg> {
    let mut defs: HashMap<VReg, Vec<&Inst>> = HashMap::new();
    for block in &function.blocks {
        for inst in &block.insts {
            for dst in inst.defs() {
                defs.entry(dst).or_default().push(inst);
            }
        }
    }
    let mut frozen: HashSet<VReg> = function
        .params
        .iter()
        .copied()
        .filter(|p| !defs.contains_key(p))
        .collect();
    loop {
        let more: Vec<VReg> = defs
            .iter()
            .filter(|(reg, _)| !frozen.contains(reg))
            .filter(|(_, insts)| {
                insts.iter().all(|inst| match inst {
                    Inst::Copy {
                        src: Operand::Reg(r),
                        ..
                    }
                    | Inst::ListLoad {
                        list: Operand::Reg(r),
                        ..
                    }
                    | Inst::ListGet {
                        list: Operand::Reg(r),
                        ..
                    } => frozen.contains(r),
                    // a variable starts out holding no list, which nothing can change either
                    Inst::Copy {
                        src: Operand::Imm(0),
                        ..
                    } => true,
                    _ => false,
                })
            })
            .map(|(&reg, _)| reg)
            .collect();
        if more.is_empty() {
            return frozen;
        }
        frozen.extend(more);
    }
}

/// Returns whether `index` is proven to be an element of `list`, whose elements are `width` slots
/// each: from 0 to below their number.
fn in_range(state: &State, list: Operand, index: Operand, width: usize) -> bool {
    let Operand::Reg(list) = list else {
        return false;
    };
    let (t, k) = Term::of(index);
    state.holds(Term::Zero, t, k) && state.holds(t, Term::Len(list, width), -1 - k)
}

/// A change [`remove_proven_checks`] makes to one instruction or terminator.
enum Rewrite {
    /// The instruction at this index becomes this, with these before it when verifying.
    Inst(usize, Inst, Vec<Inst>),
    /// The terminator becomes a jump to the block, with these at the block's end when
    /// verifying.
    Jump(BlockId, Vec<Inst>),
}

/// Removes every check the facts prove never fails, as the module describes, and returns whether
/// it removed any.
pub(super) fn remove_proven_checks(function: &mut Function, options: Options) -> bool {
    let Some(entry) = analyze(function) else {
        return false;
    };
    let mut rewrites: Vec<(usize, Rewrite)> = Vec::new();
    for (b, state) in entry.into_iter().enumerate() {
        let Some(mut state) = state else { continue };
        let block = &function.blocks[b];
        let mut found = Vec::new();
        for (i, inst) in block.insts.iter().enumerate() {
            let proven = match *inst {
                Inst::ListLoad { dst, list, index } if in_range(&state, list, index, 1) => Some((
                    Inst::ListGet {
                        dst,
                        list,
                        index,
                        offset: 0,
                    },
                    list,
                    index,
                    1,
                )),
                Inst::ListStore {
                    old: None,
                    list,
                    index,
                    value,
                } if in_range(&state, list, index, 1) => Some((
                    Inst::ListSet {
                        list,
                        index,
                        value,
                        offset: 0,
                    },
                    list,
                    index,
                    1,
                )),
                Inst::ElementSlot {
                    dst,
                    list,
                    index,
                    width,
                } if in_range(&state, list, index, width) => {
                    let slot = if width == 1 {
                        Inst::Copy { dst, src: index }
                    } else {
                        Inst::Binary {
                            op: BinOp::Mul,
                            dst,
                            lhs: index,
                            rhs: Operand::Imm(width as i64),
                        }
                    };
                    Some((slot, list, index, width))
                }
                _ => None,
            };
            if let Some((unchecked, list, index, width)) = proven {
                found.push((i, unchecked, list, index, width));
            }
            // a float divided by a constant other than zero cannot be divided by zero
            if let Inst::FloatBinary {
                op: BinOp::Div,
                dst,
                lhs,
                rhs: Operand::Imm(bits),
            } = *inst
                && f64::from_bits(bits as u64) != 0.0
            {
                rewrites.push((
                    b,
                    Rewrite::Inst(
                        i,
                        Inst::FloatBinary {
                            op: BinOp::DivUnchecked,
                            dst,
                            lhs,
                            rhs: Operand::Imm(bits),
                        },
                        vec![],
                    ),
                ));
            }
            state.step(inst);
        }
        for (i, unchecked, list, index, width) in found {
            let checks = match options.verify {
                true => never_outside(function, list, index, width, "facts"),
                false => vec![],
            };
            rewrites.push((b, Rewrite::Inst(i, unchecked, checks)));
        }
        if let Terminator::CmpBranch {
            cond,
            float,
            lhs,
            rhs,
            then,
            otherwise,
        } = function.blocks[b].term
            && then != otherwise
        {
            let decided = if float {
                state.never_float(cond, lhs, rhs).then_some(false)
            } else {
                state.decides(cond, lhs, rhs)
            };
            if let Some(holds) = decided {
                // the way the comparison never takes, as a comparison that would take it
                let (kept, never) = match holds {
                    true => (then, cond.negate()),
                    false => (otherwise, cond),
                };
                let checks = match options.verify {
                    true => vec![Inst::Never {
                        cond: never,
                        float,
                        lhs,
                        rhs,
                        what: "facts",
                    }],
                    false => vec![],
                };
                rewrites.push((b, Rewrite::Jump(kept, checks)));
            }
        }
    }
    let changed = !rewrites.is_empty();
    // later instructions first, so that what verifying puts before one leaves the rest in place
    rewrites.sort_by_key(|(b, rewrite)| {
        let at = match rewrite {
            Rewrite::Inst(i, ..) => *i,
            Rewrite::Jump(..) => usize::MAX,
        };
        std::cmp::Reverse((*b, at))
    });
    for (b, rewrite) in rewrites {
        let block: &mut Block = &mut function.blocks[b];
        match rewrite {
            Rewrite::Inst(i, inst, checks) => {
                block.insts[i] = inst;
                let span = block.spans[i];
                let count = checks.len();
                block.insts.splice(i..i, checks);
                block.spans.splice(i..i, std::iter::repeat_n(span, count));
            }
            Rewrite::Jump(target, checks) => {
                let span = block.term_span;
                block.spans.extend(std::iter::repeat_n(span, checks.len()));
                block.insts.extend(checks);
                block.term = Terminator::Jump(target);
            }
        }
    }
    changed
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(n: u32) -> Term {
        Term::Reg(VReg(n))
    }

    #[test]
    fn adding_a_constant_shifts_what_is_known() {
        let mut state = State::default();
        state.equal(r(80), Term::Zero, 0);
        state.add(r(80), r(15), -1);
        state.step(&Inst::Binary {
            op: BinOp::Add,
            dst: VReg(80),
            lhs: Operand::Reg(VReg(80)),
            rhs: Operand::Imm(1),
        });
        assert_eq!(state.range(r(80)), (1, 1), "{:?}", state.bounds);
        assert!(state.holds(r(80), r(15), 0), "{:?}", state.bounds);
    }

    #[test]
    fn a_way_the_bounds_rule_out_is_not_taken() {
        let mut state = State::default();
        state.equal(r(1), Term::Zero, 0);
        // `br_lt v1, 0`: v1 is 0, so only the way where it is not below 0 is taken
        let term = Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(VReg(1)),
            rhs: Operand::Imm(0),
            then: BlockId(1),
            otherwise: BlockId(2),
        };
        let ways: Vec<BlockId> = ways_out(&state, &term)
            .into_iter()
            .map(|(b, _)| b)
            .collect();
        assert_eq!(ways, [BlockId(2)]);
    }

    #[test]
    fn a_sum_of_squares_is_never_below_zero() {
        let mut state = State::default();
        let mul = |dst, x| Inst::FloatBinary {
            op: BinOp::Mul,
            dst: VReg(dst),
            lhs: Operand::Reg(VReg(x)),
            rhs: Operand::Reg(VReg(x)),
        };
        state.step(&mul(3, 1));
        state.step(&mul(4, 2));
        state.step(&Inst::FloatBinary {
            op: BinOp::Add,
            dst: VReg(5),
            lhs: Operand::Reg(VReg(3)),
            rhs: Operand::Reg(VReg(4)),
        });
        assert!(state.never_float(Cond::Lt, Operand::Reg(VReg(5)), Operand::Imm(0)));
        // a difference may be below zero
        state.step(&Inst::FloatBinary {
            op: BinOp::Sub,
            dst: VReg(6),
            lhs: Operand::Reg(VReg(3)),
            rhs: Operand::Reg(VReg(4)),
        });
        assert!(!state.never_float(Cond::Lt, Operand::Reg(VReg(6)), Operand::Imm(0)));
    }

    #[test]
    fn a_sum_that_may_overflow_has_no_bounds() {
        let mut state = State::default();
        state.add(Term::Zero, r(1), 0);
        state.add(Term::Zero, r(2), -1);
        // v1 >= 0 and v2 >= 1, but v1 + v2 may wrap around below zero
        state.step(&Inst::Binary {
            op: BinOp::Add,
            dst: VReg(3),
            lhs: Operand::Reg(VReg(1)),
            rhs: Operand::Reg(VReg(2)),
        });
        assert!(!state.holds(Term::Zero, r(3), 0), "{:?}", state.bounds);
    }

    #[test]
    fn a_call_forgets_every_length() {
        let mut state = State::default();
        state.step(&Inst::ListLen {
            dst: VReg(1),
            list: Operand::Reg(VReg(0)),
        });
        state.add(r(2), r(1), -1);
        assert!(state.holds(r(2), Term::Len(VReg(0), 1), -1));
        state.step(&Inst::Call {
            dsts: vec![],
            callee: Callee::User("f".to_string()),
            args: vec![Operand::Reg(VReg(0))],
        });
        assert!(!state.holds(r(2), Term::Len(VReg(0), 1), -1));
        assert!(state.holds(r(2), r(1), -1));
    }

    #[test]
    fn a_list_built_by_appends_is_as_long_as_its_appends() {
        let mut state = State::default();
        // v6 = call stone.list_new(0, 0); v1 = copy v6; v7 = copy 0
        state.step(&Inst::Call {
            dsts: vec![VReg(6)],
            callee: Callee::Runtime("stone.list_new"),
            args: vec![Operand::Imm(0), Operand::Imm(0)],
        });
        state.step(&Inst::Copy {
            dst: VReg(1),
            src: Operand::Reg(VReg(6)),
        });
        state.step(&Inst::Copy {
            dst: VReg(7),
            src: Operand::Imm(0),
        });
        assert!(
            state.holds(Term::Len(VReg(1), 1), r(7), 0),
            "{:?}",
            state.bounds
        );
        // v7 = add v7, 1; call stone.list_append(v1, 1)
        state.step(&Inst::Binary {
            op: BinOp::Add,
            dst: VReg(7),
            lhs: Operand::Reg(VReg(7)),
            rhs: Operand::Imm(1),
        });
        state.step(&Inst::Call {
            dsts: vec![],
            callee: Callee::Runtime("stone.list_append"),
            args: vec![Operand::Reg(VReg(1)), Operand::Imm(1)],
        });
        let len = Term::Len(VReg(1), 1);
        assert!(
            state.holds(len, r(7), 0) && state.holds(r(7), len, 0),
            "{:?}",
            state.bounds
        );
        // v6 holds the same list, which grew too, so what was known of its length is forgotten
        assert!(
            !state.holds(Term::Len(VReg(6), 1), Term::Zero, 0),
            "{:?}",
            state.bounds
        );
    }

    #[test]
    fn meeting_keeps_a_bound_one_side_only_implies() {
        // one way knows v1 == 0 and v2 == 0, the other v1 == v2 outright
        let mut zero = State::default();
        zero.equal(r(1), Term::Zero, 0);
        zero.equal(r(2), Term::Zero, 0);
        let mut same = State::default();
        same.equal(r(1), r(2), 0);
        for met in [zero.meet(&same), same.meet(&zero)] {
            assert!(
                met.holds(r(1), r(2), 0) && met.holds(r(2), r(1), 0),
                "{:?}",
                met.bounds
            );
        }
    }

    /// Returns facts in which `lists` are frozen.
    fn frozen_state(lists: &[u32]) -> State {
        State {
            frozen: Rc::new(lists.iter().map(|&n| VReg(n)).collect()),
            ..State::default()
        }
    }

    #[test]
    fn a_frozen_list_keeps_its_length_across_calls() {
        let mut state = frozen_state(&[0]);
        state.step(&Inst::ListLen {
            dst: VReg(1),
            list: Operand::Reg(VReg(0)),
        });
        state.add(r(2), r(1), -1);
        // appending to another list, or calling a stone function, changes no frozen list
        for callee in [
            Callee::Runtime("stone.list_append"),
            Callee::User("f".to_string()),
        ] {
            state.step(&Inst::Call {
                dsts: vec![],
                callee,
                args: vec![Operand::Reg(VReg(5)), Operand::Imm(1)],
            });
            assert!(
                state.holds(r(2), Term::Len(VReg(0), 1), -1),
                "{:?}",
                state.bounds
            );
        }
        // a routine not on lists may change anything
        state.step(&Inst::Call {
            dsts: vec![],
            callee: Callee::Runtime("stone.map_compact"),
            args: vec![Operand::Reg(VReg(5))],
        });
        assert!(
            !state.holds(r(2), Term::Len(VReg(0), 1), -1),
            "{:?}",
            state.bounds
        );
    }

    #[test]
    fn a_row_read_below_the_measured_count_is_at_least_the_shortest() {
        let mut state = frozen_state(&[0]);
        // v3 = min_row_len v0, v2, and the guard took the way where v4 <= v3
        state.step(&Inst::MinRowLen {
            dst: VReg(3),
            list: Operand::Reg(VReg(0)),
            count: Operand::Reg(VReg(2)),
        });
        state.assume(Cond::Le, Operand::Reg(VReg(4)), Operand::Reg(VReg(3)));
        // 0 <= v5 < v2 and 0 <= v6 < v4
        for (index, bound) in [(5, 2), (6, 4)] {
            state.assume(Cond::Ge, Operand::Reg(VReg(index)), Operand::Imm(0));
            state.assume(
                Cond::Lt,
                Operand::Reg(VReg(index)),
                Operand::Reg(VReg(bound)),
            );
        }
        let load = |dst, index| Inst::ListLoad {
            dst: VReg(dst),
            list: Operand::Reg(VReg(0)),
            index: Operand::Reg(VReg(index)),
        };
        state.step(&load(7, 5));
        assert!(in_range(
            &state,
            Operand::Reg(VReg(7)),
            Operand::Reg(VReg(6)),
            1
        ));
        // a row from past the measured ones may be shorter
        state.step(&load(8, 4));
        assert!(!in_range(
            &state,
            Operand::Reg(VReg(8)),
            Operand::Reg(VReg(6)),
            1
        ));
        // a store may put a shorter row in a list that is not frozen
        let mut thawed = state.clone();
        thawed.frozen = Rc::new(HashSet::new());
        thawed.step(&Inst::ListStore {
            old: None,
            list: Operand::Reg(VReg(9)),
            index: Operand::Imm(0),
            value: Operand::Imm(0),
        });
        thawed.step(&load(10, 5));
        assert!(!in_range(
            &thawed,
            Operand::Reg(VReg(10)),
            Operand::Reg(VReg(6)),
            1
        ));
    }
}
