//! The trigonometric functions of the `math` module in compiled code.
//!
//! Lowering writes each call as one [`Inst::Trig`], such as `v3 = sin v2`. [`share`] then
//! computes each value once, turning a repeated call into a copy and a `sin` and a `cos` of the
//! same value into one [`Inst::SinCos`], which reduces it once, as `sincos` does in C. [`expand`]
//! then replaces each with inline code, before inlining and optimizing see the function, so the
//! floats around a `math.sin` keep their registers.
//!
//! Each function branches as its interpreter version in `stdlib::math::trig` does, and the code
//! between the branches comes from the same generic functions there, through [`Emit`], so both
//! backends do the same float operations in the same order. Only an argument of `sin`, `cos`, or
//! `tan` of at least 2^20 * pi/2 leaves the inline code, for the prelude's `rem_pio2_large`
//! behind [`Inst::ReducePio2`], in a cold block with the other rare cases (nan, infinities, and
//! atan2's extreme magnitudes), which come after the common ones. They stay before the code after
//! the call rather than at the end of the function, since each vreg's live interval runs, with no
//! holes, from its first to its last mention in layout order, so a cold block far away would keep
//! every value it mentions in a register through everything in between.

use super::inline::retarget;
use super::opt::predecessors;
use super::{BinOp, Block, BlockId, Cond, Function, Inst, Operand, Terminator, TrigFunc, VReg};
use crate::stdlib::math::DOMAIN_ERROR;
use crate::stdlib::math::trig::{self, Arith};
use std::collections::{HashMap, HashSet};
use std::f64::consts::{FRAC_PI_2, PI};

/// A value known to be in a vreg where a fact holds: `func` of `args` is in `dst`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct Known {
    func: TrigFunc,
    args: Vec<Operand>,
    dst: VReg,
}

/// Returns the values `inst` computes.
fn computed(inst: &Inst) -> Vec<Known> {
    match inst {
        Inst::Trig { func, dst, args } => vec![Known {
            func: *func,
            args: args.clone(),
            dst: *dst,
        }],
        Inst::SinCos { sin, cos, src } => [(TrigFunc::Sin, *sin), (TrigFunc::Cos, *cos)]
            .into_iter()
            .map(|(func, dst)| Known {
                func,
                args: vec![*src],
                dst,
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Updates the values known after `inst`: what it writes kills every value it held or was of,
/// and what it computes becomes known.
fn transfer(known: &mut HashSet<Known>, inst: &Inst) {
    let defs = inst.defs();
    let mentions = |value: &Known| {
        defs.contains(&value.dst)
            || value
                .args
                .iter()
                .any(|arg| matches!(arg, Operand::Reg(reg) if defs.contains(reg)))
    };
    known.retain(|value| !mentions(value));
    known.extend(computed(inst));
}

/// Returns the values known on entry to each block, `None` for a block nothing reaches, found
/// by iterating to a fixed point as `opt::eliminate` does.
fn known_on_entry(function: &Function) -> Vec<Option<HashSet<Known>>> {
    let preds = predecessors(function);
    let count = function.blocks.len();
    let mut outs: Vec<Option<HashSet<Known>>> = vec![None; count];
    let entry = |b: usize, outs: &[Option<HashSet<Known>>]| -> Option<HashSet<Known>> {
        if b == 0 {
            return Some(HashSet::new());
        }
        let mut known: Option<HashSet<Known>> = None;
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
            let Some(mut known) = entry(b, &outs) else {
                continue;
            };
            for inst in &function.blocks[b].insts {
                transfer(&mut known, inst);
            }
            if outs[b].as_ref() != Some(&known) {
                outs[b] = Some(known);
                changed = true;
            }
        }
    }
    (0..count).map(|b| entry(b, &outs)).collect()
}

/// Returns the function whose value a `sin` or `cos` of the same argument shares a reduction
/// with.
fn partner(func: TrigFunc) -> Option<TrigFunc> {
    match func {
        TrigFunc::Sin => Some(TrigFunc::Cos),
        TrigFunc::Cos => Some(TrigFunc::Sin),
        _ => None,
    }
}

/// Computes each trigonometric value of `function` once: a call whose value an earlier one
/// already computed, on every way to it, with its argument unchanged since, becomes a copy of
/// that value, and a `sin` or `cos` whose partner was computed so becomes a copy of a value the
/// earlier call, now an [`Inst::SinCos`], computes as well.
///
/// For example, `v3 = sin v2` then `v4 = cos v2` become `v3, v5 = sincos v2` then
/// `v4 = copy v5`. The call kept is always the earlier one, at its own span, so a failure (an
/// infinite argument, or calls nested too deeply in the prelude's reduction) happens where the
/// interpreter's first call fails.
pub fn share(function: &mut Function) {
    while share_once(function) {}
}

/// One round of [`share`]: every call made a copy, or, when there is none, every pair made one
/// [`Inst::SinCos`], returning whether anything changed.
fn share_once(function: &mut Function) -> bool {
    let calls = function
        .blocks
        .iter()
        .flat_map(|block| &block.insts)
        .filter(|inst| matches!(inst, Inst::Trig { .. }))
        .count();
    if calls < 2 {
        return false;
    }
    let mut defined: HashMap<VReg, Vec<(usize, usize)>> = HashMap::new();
    for (b, block) in function.blocks.iter().enumerate() {
        for (i, inst) in block.insts.iter().enumerate() {
            for dst in inst.defs() {
                defined.entry(dst).or_default().push((b, i));
            }
        }
    }
    // the one call that writes `dst`, if one does
    let call_of = |dst: VReg| match defined.get(&dst).map(Vec::as_slice) {
        Some(&[at]) => Some(at),
        _ => None,
    };
    let entries = known_on_entry(function);
    let mut copies = Vec::new();
    let mut pairs = Vec::new();
    for (b, block) in function.blocks.iter().enumerate() {
        let Some(mut known) = entries[b].clone() else {
            continue;
        };
        for (i, inst) in block.insts.iter().enumerate() {
            if let Inst::Trig { func, args, dst } = inst {
                let find = |func: TrigFunc| {
                    known
                        .iter()
                        .find(|value| value.func == func && value.args == *args)
                        .map(|value| value.dst)
                };
                if let Some(value) = find(*func) {
                    copies.push((b, i, *dst, value));
                } else if let Some(at) = partner(*func).and_then(find).and_then(call_of)
                    && matches!(function.blocks[at.0].insts[at.1], Inst::Trig { .. })
                {
                    pairs.push(((b, i), *dst, at));
                }
            }
            transfer(&mut known, inst);
        }
    }
    if !copies.is_empty() {
        for (b, i, dst, value) in copies {
            function.blocks[b].insts[i] = Inst::Copy {
                dst,
                src: Operand::Reg(value),
            };
        }
        return true;
    }
    // a call that becomes a copy cannot also take its partner's value
    let sources: HashSet<(usize, usize)> = pairs.iter().map(|&(source, ..)| source).collect();
    let mut partners: HashMap<(usize, usize), VReg> = HashMap::new();
    let mut changed = false;
    for ((b, i), dst, at) in pairs {
        if sources.contains(&at) {
            continue;
        }
        let value = *partners.entry(at).or_insert_with(|| {
            let reg = VReg(function.vreg_count);
            function.vreg_count += 1;
            reg
        });
        function.blocks[b].insts[i] = Inst::Copy {
            dst,
            src: Operand::Reg(value),
        };
        changed = true;
    }
    for ((b, i), value) in partners {
        let Inst::Trig { func, dst, args } = &function.blocks[b].insts[i] else {
            continue;
        };
        let (sin, cos) = match func {
            TrigFunc::Sin => (*dst, value),
            _ => (value, *dst),
        };
        function.blocks[b].insts[i] = Inst::SinCos {
            sin,
            cos,
            src: args[0],
        };
    }
    changed
}

/// Replaces every [`Inst::Trig`] and [`Inst::SinCos`] in `function` with inline code: the
/// common cases in blocks right after the call's, then the rare ones in cold blocks, then the
/// rest of the call's block.
pub fn expand(function: &mut Function) {
    loop {
        let found = function.blocks.iter().enumerate().find_map(|(b, block)| {
            let i = block
                .insts
                .iter()
                .position(|inst| matches!(inst, Inst::Trig { .. } | Inst::SinCos { .. }))?;
            Some((b, i))
        });
        let Some((b, i)) = found else {
            return;
        };
        expand_at(function, b, i);
    }
}

/// Replaces the call at instruction `i` of block `b` with inline code.
fn expand_at(function: &mut Function, b: usize, i: usize) {
    let inst = function.blocks[b].insts[i].clone();
    let mut builder = Builder::new(function.vreg_count);
    let (entry, done) = (builder.new_block(), builder.new_block());
    builder.start(entry);
    match inst {
        Inst::Trig { func, dst, args } => match func {
            TrigFunc::Sin | TrigFunc::Cos | TrigFunc::Tan => {
                builder.circular(func, args[0], dst, done)
            }
            TrigFunc::Asin => builder.asin(args[0], dst, done),
            TrigFunc::Acos => builder.acos(args[0], dst, done),
            TrigFunc::Atan => builder.atan(args[0], dst, done),
            TrigFunc::Atan2 => builder.atan2(args[0], args[1], dst, done),
        },
        Inst::SinCos { sin, cos, src } => builder.sincos(src, sin, cos, done),
        _ => unreachable!("only a trigonometric call is expanded"),
    }
    builder.start(done);
    function.vreg_count = builder.vreg_count;
    splice(function, b, i, builder, entry, done);
}

/// Puts `builder`'s blocks in `function` in place of instruction `i` of block `b`: the block
/// ends there with a jump to `entry`, and after it come the hot blocks in the order they were
/// started, then the cold ones, then `done`, the last hot block, which gets the rest of the block.
fn splice(
    function: &mut Function,
    b: usize,
    i: usize,
    builder: Builder,
    entry: BlockId,
    done: BlockId,
) {
    let Builder {
        blocks, hot, cold, ..
    } = builder;
    let span = function.blocks[b].spans[i];
    let depth = function.blocks[b].loop_depth;
    let order: Vec<BlockId> = hot
        .iter()
        .filter(|&&id| id != done)
        .chain(&cold)
        .chain([&done])
        .copied()
        .collect();
    let inserted = order.len();
    let mut place = vec![BlockId(usize::MAX); blocks.len()];
    for (k, &BlockId(id)) in order.iter().enumerate() {
        place[id] = BlockId(b + 1 + k);
    }
    // the function's blocks after the split one move down past the hot ones
    let move_block = |BlockId(n): BlockId| match n > b {
        true => BlockId(n + inserted),
        false => BlockId(n),
    };

    let split = &mut function.blocks[b];
    let rest_insts = split.insts.split_off(i + 1);
    let rest_spans = split.spans.split_off(i + 1);
    split.insts.pop();
    split.spans.pop();
    let rest_term = std::mem::replace(&mut split.term, Terminator::Jump(BlockId(b)));
    let rest_term_span = split.term_span;
    for block in &mut function.blocks {
        block.term = retarget(&block.term, move_block, |operand| operand);
    }
    function.blocks[b].term = Terminator::Jump(place[entry.0]);

    let mut pending: Vec<Option<Pending>> = blocks.into_iter().map(Some).collect();
    let mut take = |BlockId(id): BlockId| -> Block {
        let block = pending[id].take().expect("each block is placed once");
        let count = block.insts.len();
        if BlockId(id) == done {
            let mut insts = block.insts;
            insts.extend(rest_insts.iter().cloned());
            let mut spans = vec![span; count];
            spans.extend(rest_spans.iter().copied());
            return Block {
                insts,
                spans,
                term: retarget(&rest_term, move_block, |operand| operand),
                term_span: rest_term_span,
                loop_depth: depth,
            };
        }
        let term = block.term.expect("every block but the last is ended");
        Block {
            insts: block.insts,
            spans: vec![span; count],
            term: retarget(&term, |BlockId(n)| place[n], |operand| operand),
            term_span: span,
            loop_depth: depth,
        }
    };
    let placed: Vec<Block> = order.iter().map(|&id| take(id)).collect();
    function.blocks.splice(b + 1..b + 1, placed);
}

/// A block the [`Builder`] is still writing.
struct Pending {
    insts: Vec<Inst>,
    term: Option<Terminator>,
}

/// Writes the blocks of one expanded call, numbered from 0 in the order they are made, as
/// lowering writes a function's: [`Builder::start`] places a block next among the hot ones, and
/// [`Builder::start_cold`] among the cold ones, and a block left without a terminator goes on
/// into the one started next.
struct Builder {
    blocks: Vec<Pending>,
    hot: Vec<BlockId>,
    cold: Vec<BlockId>,
    current: Option<BlockId>,
    vreg_count: u32,
}

impl Builder {
    fn new(vreg_count: u32) -> Builder {
        Builder {
            blocks: Vec::new(),
            hot: Vec::new(),
            cold: Vec::new(),
            current: None,
            vreg_count,
        }
    }

    fn fresh(&mut self) -> VReg {
        let reg = VReg(self.vreg_count);
        self.vreg_count += 1;
        reg
    }

    fn new_block(&mut self) -> BlockId {
        self.blocks.push(Pending {
            insts: Vec::new(),
            term: None,
        });
        BlockId(self.blocks.len() - 1)
    }

    /// Makes `block` the current block, placing it next among the hot blocks.
    fn start(&mut self, block: BlockId) {
        self.go_on(block);
        self.hot.push(block);
    }

    /// Makes `block` the current block, placing it next among the cold blocks.
    fn start_cold(&mut self, block: BlockId) {
        self.go_on(block);
        self.cold.push(block);
    }

    /// Ends the current block, if it has no terminator yet, with a jump to `block`, which
    /// becomes the current one.
    fn go_on(&mut self, block: BlockId) {
        if let Some(current) = self.current
            && self.blocks[current.0].term.is_none()
        {
            self.blocks[current.0].term = Some(Terminator::Jump(block));
        }
        self.current = Some(block);
    }

    fn push(&mut self, inst: Inst) {
        let current = self.current.expect("a block is started");
        self.blocks[current.0].insts.push(inst);
    }

    fn terminate(&mut self, term: Terminator) {
        let current = self.current.expect("a block is started");
        self.blocks[current.0].term = Some(term);
    }

    fn jump(&mut self, target: BlockId) {
        self.terminate(Terminator::Jump(target));
    }

    /// Branches to `then` if the floats `lhs` and `rhs` compare as `cond`, else to `otherwise`.
    fn float_branch(
        &mut self,
        cond: Cond,
        lhs: Operand,
        rhs: Operand,
        then: BlockId,
        otherwise: BlockId,
    ) {
        self.terminate(Terminator::CmpBranch {
            cond,
            float: true,
            lhs,
            rhs,
            then,
            otherwise,
        });
    }

    /// Branches to `then` if any bit of `mask` is set in the int `value`, else to `otherwise`.
    fn test_branch(&mut self, value: Operand, mask: i64, then: BlockId, otherwise: BlockId) {
        self.terminate(Terminator::TestBranch {
            value,
            mask,
            then,
            otherwise,
        });
    }

    /// Branches to `then` if the float `x` is below 0, else to `otherwise`.
    fn sign_branch(&mut self, x: Operand, then: BlockId, otherwise: BlockId) {
        self.float_branch(Cond::Lt, x, number(0.0), then, otherwise);
    }

    /// Goes on in a new block if the magnitude `ax` is below `bound`, which no nan is, else to
    /// `outside`.
    fn below(&mut self, ax: Operand, bound: f64, outside: BlockId) {
        let inside = self.new_block();
        self.float_branch(Cond::Lt, ax, number(bound), inside, outside);
        self.start(inside);
    }

    fn copy_to(&mut self, dst: VReg, src: Operand) {
        self.push(Inst::Copy { dst, src });
    }

    fn negate(&mut self, dst: VReg, src: Operand) {
        self.push(Inst::FloatNeg { dst, src });
    }

    /// Writes `value` to `result` and goes to `done`.
    fn give(&mut self, result: VReg, value: Operand, done: BlockId) {
        self.copy_to(result, value);
        self.jump(done);
    }

    /// Writes `value` to `result`, negated if the float `x` is below 0, and goes to `done`.
    fn give_signed(&mut self, x: Operand, result: VReg, value: Operand, done: BlockId) {
        let (negative, positive) = (self.new_block(), self.new_block());
        self.sign_branch(x, negative, positive);
        self.start(negative);
        self.negate(result, value);
        self.jump(done);
        self.start(positive);
        self.give(result, value, done);
    }

    /// Writes `trunc(n) + offset` to `q` for the float `n`, a whole number of quarter turns
    /// small enough to fit: only its two low bits matter, which give the quarter turns mod 4 in
    /// two's complement even for a negative `n`.
    fn quarters(&mut self, n: Operand, offset: i64, q: VReg) {
        self.push(Inst::FloatToInt {
            dst: q,
            src: n,
            checked: false,
        });
        if offset != 0 {
            self.push(Inst::Binary {
                op: BinOp::Add,
                dst: q,
                lhs: Operand::Reg(q),
                rhs: Operand::Imm(offset),
            });
        }
    }

    /// Reduces `x`, whose magnitude `ax` is at least pi/4 or nan, by pi/2 as `trig::rem_pio2`
    /// does, writing `y0`, `y1`, and the quarter turns plus `offset` to `q`, then going to
    /// `quad`: inline below 2^20 * pi/2, with musl's second and third rounds when the ones
    /// before cancelled, and through the prelude beyond. An infinity fails, and a nan goes to a
    /// cold block that `nan` ends. The magnitude is taken again from `x` where it is needed
    /// later, which costs one instruction, rather than kept in a register throughout.
    fn reduce(
        &mut self,
        x: Operand,
        ax: Operand,
        offset: i64,
        (q, y0, y1): (VReg, VReg, VReg),
        quad: BlockId,
        nan: impl FnOnce(&mut Self),
    ) {
        let cold = self.new_block();
        self.below(ax, trig::MEDIUM, cold);
        let (n, r, w, first) = trig::reduce_first(&mut Emit(self), x);
        let (r_var, w_var) = (self.fresh(), self.fresh());
        self.copy_to(r_var, r);
        self.copy_to(w_var, w);
        self.copy_to(y0, first);
        let tail = self.new_block();
        for (scale, part, rest) in trig::ROUNDS {
            let ax = Emit(self).abs(x);
            let (ay, bound) = trig::round_test(&mut Emit(self), ax, Operand::Reg(y0), scale);
            let more = self.new_block();
            self.float_branch(Cond::Lt, ay, bound, more, tail);
            self.start(more);
            let (r, w, more_y0) =
                trig::reduce_round(&mut Emit(self), n, Operand::Reg(r_var), part, rest);
            self.copy_to(r_var, r);
            self.copy_to(w_var, w);
            self.copy_to(y0, more_y0);
        }
        self.start(tail);
        let rest = trig::reduce_tail(
            &mut Emit(self),
            Operand::Reg(r_var),
            Operand::Reg(y0),
            Operand::Reg(w_var),
        );
        self.copy_to(y1, rest);
        self.quarters(n, offset, q);
        self.jump(quad);

        // nan, infinities, and arguments too large to reduce inline
        self.start_cold(cold);
        let (finite, not_number) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, x, x, finite, not_number);
        self.start_cold(not_number);
        nan(self);
        self.start_cold(finite);
        let ax = Emit(self).abs(x);
        let (infinite, large) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, ax, number(f64::INFINITY), infinite, large);
        self.start_cold(infinite);
        self.terminate(Terminator::Fail(DOMAIN_ERROR.to_string(), Vec::new()));
        self.start_cold(large);
        let (turns, hi, lo) = (self.fresh(), self.fresh(), self.fresh());
        self.push(Inst::DepthCheck);
        self.push(Inst::ReducePio2 {
            n: turns,
            hi,
            lo,
            src: ax,
        });
        self.copy_to(y0, Operand::Reg(hi));
        self.copy_to(y1, Operand::Reg(lo));
        let (flip, counted) = (self.new_block(), self.new_block());
        self.sign_branch(x, flip, counted);
        self.start_cold(flip);
        self.negate(y0, Operand::Reg(hi));
        self.negate(y1, Operand::Reg(lo));
        self.negate(turns, Operand::Reg(turns));
        self.start_cold(counted);
        self.quarters(Operand::Reg(turns), offset, q);
        self.jump(quad);
    }

    /// Expands `sin`, `cos`, or `tan` of `x`, as `trig::sin`, `trig::cos`, and `trig::tan`
    /// compute them: the argument reduced by pi/2 to `y0 + y1` after `q` quarter turns, then a
    /// kernel.
    fn circular(&mut self, func: TrigFunc, x: Operand, result: VReg, done: BlockId) {
        let cos = func == TrigFunc::Cos;
        let (q, y0, y1) = (self.fresh(), self.fresh(), self.fresh());
        let (wide, quad) = (self.new_block(), self.new_block());
        let ax = Emit(self).abs(x);

        // below pi/4, after a tiny argument, which gives itself or 1
        self.below(ax, trig::PIO4_BOUND, wide);
        let tiny = match func {
            TrigFunc::Cos => trig::COS_TINY,
            TrigFunc::Tan => trig::TAN_TINY,
            _ => trig::SIN_TINY,
        };
        let (small, kernel) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(tiny), small, kernel);
        self.start(small);
        self.give(result, if cos { number(1.0) } else { x }, done);
        self.start(kernel);
        self.copy_to(y0, x);
        self.copy_to(y1, number(0.0));
        self.copy_to(q, Operand::Imm(cos as i64));
        self.jump(quad);

        // reduced, a quarter turn more for cos than for sin
        self.start(wide);
        self.reduce(x, ax, cos as i64, (q, y0, y1), quad, |this| {
            this.give(result, x, done)
        });

        // the kernels
        self.start(quad);
        let (y0, y1, q) = (Operand::Reg(y0), Operand::Reg(y1), Operand::Reg(q));
        if func == TrigFunc::Tan {
            self.tan_kernel(y0, y1, q, result, done);
        } else {
            self.quadrant(q, y0, y1, result, done);
        }
    }

    /// Expands `trig`'s `quadrant`: sin(y0 + y1) after `q` quarter turns, the sine's or the
    /// cosine's kernel by whether `q` is odd, negated when its second bit is set.
    fn quadrant(&mut self, q: Operand, y0: Operand, y1: Operand, result: VReg, done: BlockId) {
        let (sine, cosine, sign) = (self.new_block(), self.new_block(), self.new_block());
        self.test_branch(q, 1, cosine, sine);
        self.start(sine);
        let value = trig::sin_kernel(&mut Emit(self), y0, y1);
        self.give(result, value, sign);
        self.start(cosine);
        let value = trig::cos_kernel(&mut Emit(self), y0, y1);
        self.copy_to(result, value);
        self.start(sign);
        let flip = self.new_block();
        self.test_branch(q, 2, flip, done);
        self.start(flip);
        self.negate(result, Operand::Reg(result));
        self.jump(done);
    }

    /// Expands `trig`'s `tan_kernel`: tan(y0 + y1), or -1/tan(y0 + y1) if `q` is odd.
    fn tan_kernel(&mut self, y0: Operand, y1: Operand, q: Operand, result: VReg, done: BlockId) {
        let (big, normal) = (self.new_block(), self.new_block());
        let ay0 = Emit(self).abs(y0);
        self.float_branch(Cond::Ge, ay0, number(trig::TAN_BIG), big, normal);

        // from pi/4 back, on the argument without its sign, which is never 0 here
        self.start(big);
        let y = self.fresh();
        let (negative, positive, fold) = (self.new_block(), self.new_block(), self.new_block());
        self.sign_branch(y0, negative, positive);
        self.start(negative);
        self.negate(y, y1);
        self.jump(fold);
        self.start(positive);
        self.copy_to(y, y1);
        self.start(fold);
        let folded = trig::tan_fold(&mut Emit(self), ay0, Operand::Reg(y));
        let (w, r) = trig::tan_poly(&mut Emit(self), folded, number(0.0));
        let sign = self.fresh();
        let (even, odd, unfold) = (self.new_block(), self.new_block(), self.new_block());
        self.test_branch(q, 1, odd, even);
        self.start(even);
        self.copy_to(sign, number(1.0));
        self.jump(unfold);
        self.start(odd);
        self.copy_to(sign, number(-1.0));
        self.start(unfold);
        let value = trig::tan_unfold(&mut Emit(self), folded, w, r, Operand::Reg(sign));
        self.give_signed(y0, result, value, done);

        self.start(normal);
        let (w, r) = trig::tan_poly(&mut Emit(self), y0, y1);
        let (even, reciprocal) = (self.new_block(), self.new_block());
        self.test_branch(q, 1, reciprocal, even);
        self.start(even);
        self.give(result, w, done);
        self.start(reciprocal);
        let value = trig::tan_reciprocal(&mut Emit(self), y0, w, r);
        self.give(result, value, done);
    }

    /// Expands `sin` and `cos` of `x` together, with the same bits as `trig::sin` and
    /// `trig::cos` give: one reduction, both kernels, and each result picked by its own quarter
    /// turns, the cosine's one more than the sine's.
    fn sincos(&mut self, x: Operand, sin: VReg, cos: VReg, done: BlockId) {
        let (q, y0, y1) = (self.fresh(), self.fresh(), self.fresh());
        let (wide, quad) = (self.new_block(), self.new_block());
        let ax = Emit(self).abs(x);
        self.below(ax, trig::PIO4_BOUND, wide);
        self.copy_to(y0, x);
        self.copy_to(y1, number(0.0));
        self.copy_to(q, Operand::Imm(0));
        self.jump(quad);
        self.start(wide);
        self.reduce(x, ax, 0, (q, y0, y1), quad, |this| {
            this.copy_to(sin, x);
            this.give(cos, x, done);
        });

        // the sine's kernel and the cosine's, swapped and negated by the quarter turns
        self.start(quad);
        let (y0, y1, q) = (Operand::Reg(y0), Operand::Reg(y1), Operand::Reg(q));
        let s = trig::sin_kernel(&mut Emit(self), y0, y1);
        let c = trig::cos_kernel(&mut Emit(self), y0, y1);
        let (even, odd, sign) = (self.new_block(), self.new_block(), self.new_block());
        self.test_branch(q, 1, odd, even);
        self.start(even);
        self.copy_to(sin, s);
        self.copy_to(cos, c);
        self.jump(sign);
        self.start(odd);
        self.copy_to(sin, c);
        self.negate(cos, s);
        self.start(sign);
        let (flip, signed) = (self.new_block(), self.new_block());
        self.test_branch(q, 2, flip, signed);
        self.start(flip);
        self.negate(sin, Operand::Reg(sin));
        self.negate(cos, Operand::Reg(cos));
        self.start(signed);

        // a tiny argument, which only the first case gives, has a sine of itself, and a cosine
        // of 1 if tinier still
        let ax = Emit(self).abs(x);
        let fix = self.new_block();
        self.float_branch(Cond::Lt, ax, number(trig::SIN_TINY), fix, done);
        self.start_cold(fix);
        self.copy_to(sin, x);
        let one = self.new_block();
        self.float_branch(Cond::Lt, ax, number(trig::COS_TINY), one, done);
        self.start_cold(one);
        self.give(cos, number(1.0), done);
    }

    /// Ends the cases of `asin` and `acos` for `|x|` of 1 or more, or nan, in a cold block:
    /// `one` gives the result for `|x| == 1`, nan gives itself, and anything else fails.
    fn domain_edge(
        &mut self,
        x: Operand,
        ax: Operand,
        result: VReg,
        done: BlockId,
        edge: BlockId,
        one: impl FnOnce(&mut Self),
    ) {
        self.start_cold(edge);
        let (unit, other) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, ax, number(1.0), unit, other);
        self.start_cold(unit);
        one(self);
        self.start_cold(other);
        let (outside, nan) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, x, x, outside, nan);
        self.start_cold(nan);
        self.give(result, x, done);
        self.start_cold(outside);
        self.terminate(Terminator::Fail(DOMAIN_ERROR.to_string(), Vec::new()));
    }

    /// Expands `trig::asin`.
    fn asin(&mut self, x: Operand, result: VReg, done: BlockId) {
        let ax = Emit(self).abs(x);
        let edge = self.new_block();
        self.below(ax, 1.0, edge);
        let (small, half) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(0.5), small, half);
        self.start(small);
        let (tiny, polynomial) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(trig::ASIN_TINY), tiny, polynomial);
        self.start(tiny);
        self.give(result, x, done);
        self.start(polynomial);
        let value = trig::asin_small(&mut Emit(self), x);
        self.give(result, value, done);

        self.start(half);
        let (z, s, r) = trig::half_turn(&mut Emit(self), ax);
        let magnitude = self.fresh();
        let (near, middle, sign) = (self.new_block(), self.new_block(), self.new_block());
        self.float_branch(Cond::Ge, ax, number(trig::ASIN_NEAR_ONE), near, middle);
        self.start(near);
        let value = trig::asin_near_one(&mut Emit(self), s, r);
        self.give(magnitude, value, sign);
        self.start(middle);
        let value = trig::asin_middle(&mut Emit(self), z, s, r);
        self.copy_to(magnitude, value);
        self.start(sign);
        self.give_signed(x, result, Operand::Reg(magnitude), done);

        self.domain_edge(x, ax, result, done, edge, |this| {
            let value = trig::asin_one(&mut Emit(this), x);
            this.give(result, value, done);
        });
    }

    /// Expands `trig::acos`.
    fn acos(&mut self, x: Operand, result: VReg, done: BlockId) {
        let ax = Emit(self).abs(x);
        let edge = self.new_block();
        self.below(ax, 1.0, edge);
        let (small, half) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(0.5), small, half);
        self.start(small);
        let (tiny, polynomial) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(trig::ACOS_TINY), tiny, polynomial);
        self.start(tiny);
        let value = trig::acos_tiny(&mut Emit(self));
        self.give(result, value, done);
        self.start(polynomial);
        let value = trig::acos_small(&mut Emit(self), x);
        self.give(result, value, done);

        // the half turn of |x|, which is -x for a negative x
        self.start(half);
        let (z, s, r) = trig::half_turn(&mut Emit(self), ax);
        let (negative, positive) = (self.new_block(), self.new_block());
        self.sign_branch(x, negative, positive);
        self.start(negative);
        let value = trig::acos_negative(&mut Emit(self), s, r);
        self.give(result, value, done);
        self.start(positive);
        let value = trig::acos_positive(&mut Emit(self), z, s, r);
        self.give(result, value, done);

        self.domain_edge(x, ax, result, done, edge, |this| {
            let (minus_one, one) = (this.new_block(), this.new_block());
            this.sign_branch(x, minus_one, one);
            this.start_cold(minus_one);
            let value = trig::acos_minus_one(&mut Emit(this));
            this.give(result, value, done);
            this.start_cold(one);
            this.give(result, number(0.0), done);
        });
    }

    /// Expands `trig::atan`, writing atan(x) to `result` and going to `done`.
    fn atan(&mut self, x: Operand, result: VReg, done: BlockId) {
        let ax = Emit(self).abs(x);
        let huge = self.new_block();
        self.below(ax, trig::ATAN_HUGE, huge);
        let (small, parts) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(0.4375), small, parts);
        self.start(small);
        let (tiny, polynomial) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(trig::TAN_TINY), tiny, polynomial);
        self.start(tiny);
        self.give(result, x, done);
        self.start(polynomial);
        let value = trig::atan_small(&mut Emit(self), x, number(0.0));
        self.give(result, value, done);

        self.start(parts);
        let (magnitude, sign) = (self.fresh(), self.new_block());
        self.atan_parts(ax, number(0.0), magnitude, sign);
        self.start(sign);
        self.give_signed(x, result, Operand::Reg(magnitude), done);

        // at least 2^66, or nan
        self.start_cold(huge);
        let (big, nan) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, x, x, big, nan);
        self.start_cold(nan);
        self.give(result, x, done);
        self.start_cold(big);
        let value = trig::atan_huge(&mut Emit(self));
        self.give_signed(x, result, value, done);
    }

    /// Writes `atan(ax) + c` to `result` for an `ax` from 0.4375 to 2^66, by the part of the
    /// range `trig::atan_part` picks, and goes to `done`. The polynomial is emitted once, with
    /// the part's argument and constants in vregs.
    fn atan_parts(&mut self, ax: Operand, c: Operand, result: VReg, done: BlockId) {
        let (t, hi, lo) = (self.fresh(), self.fresh(), self.fresh());
        let finish = self.new_block();
        let ids: Vec<BlockId> = (0..4).map(|_| self.new_block()).collect();
        let (low, high) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, ax, number(1.1875), low, high);
        self.start(low);
        self.float_branch(Cond::Lt, ax, number(0.6875), ids[0], ids[1]);
        self.start(high);
        self.float_branch(Cond::Lt, ax, number(2.4375), ids[2], ids[3]);
        for (id, &block) in ids.iter().enumerate() {
            self.start(block);
            let argument = trig::atan_argument(&mut Emit(self), ax, id);
            self.copy_to(t, argument);
            self.copy_to(hi, number(trig::ATAN_HI[id]));
            self.copy_to(lo, number(trig::ATAN_LO[id]));
            self.jump(finish);
        }
        self.start(finish);
        let value = trig::atan_finish_with(
            &mut Emit(self),
            Operand::Reg(t),
            Operand::Reg(hi),
            Operand::Reg(lo),
            c,
        );
        self.give(result, value, done);
    }

    /// Writes `negative` to `result` if the float `y` is below 0, else `positive`, and goes to
    /// `done`, in cold blocks.
    fn give_by_sign(
        &mut self,
        y: Operand,
        negative: f64,
        positive: f64,
        result: VReg,
        done: BlockId,
    ) {
        let (minus, plus) = (self.new_block(), self.new_block());
        self.sign_branch(y, minus, plus);
        self.start_cold(minus);
        self.give(result, number(negative), done);
        self.start_cold(plus);
        self.give(result, number(positive), done);
    }

    /// Expands `trig::atan2` of `y` and `x`. Its signs are compared as floats, which is the sign
    /// bit's test for every value but zero: zeros only matter when `y` is one, where the sign of
    /// `1 / v` (an infinity for a zero) tells `-0.0` from `0.0`.
    fn atan2(&mut self, y: Operand, x: Operand, result: VReg, done: BlockId) {
        let (numbers, nan) = (self.new_block(), self.new_block());
        let x_number = self.new_block();
        self.float_branch(Cond::Eq, x, x, x_number, nan);
        self.start(x_number);
        self.float_branch(Cond::Eq, y, y, numbers, nan);
        self.start_cold(nan);
        let value = Emit(self).add(x, y);
        self.give(result, value, done);

        self.start(numbers);
        let ax = Emit(self).abs(x);
        let ay = Emit(self).abs(y);

        // y == 0: y itself to the right, else pi or -pi by y's sign
        let (y_zero, y_other) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, y, number(0.0), y_zero, y_other);
        self.start_cold(y_zero);
        let (left, maybe, right) = (self.new_block(), self.new_block(), self.new_block());
        self.sign_branch(x, left, maybe);
        self.start_cold(maybe);
        let inverse = Emit(self).div(number(1.0), x);
        self.sign_branch(inverse, left, right);
        self.start_cold(right);
        self.give(result, y, done);
        self.start_cold(left);
        let inverse = Emit(self).div(number(1.0), y);
        self.give_by_sign(inverse, -PI, PI, result, done);

        // x == 0, with y not
        self.start(y_other);
        let (x_zero, x_other) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Eq, x, number(0.0), x_zero, x_other);
        self.start_cold(x_zero);
        self.give_by_sign(y, -FRAC_PI_2, FRAC_PI_2, result, done);

        // x infinite, in cold blocks after the rest
        self.start(x_other);
        let x_infinite = self.new_block();
        self.below(ax, f64::INFINITY, x_infinite);
        let (y_finite, both) = (self.new_block(), self.new_block());
        let ratio = number(trig::ATAN2_RATIO);
        let scaled = Emit(self).mul(ax, ratio);
        let (vertical, not_above) = (self.new_block(), self.new_block());
        // |y/x| > 2^64
        self.float_branch(Cond::Gt, ay, scaled, vertical, not_above);
        self.start(not_above);
        self.below(ay, f64::INFINITY, vertical);
        let (flat, quotient, zero) = (self.new_block(), self.new_block(), self.new_block());
        let z = self.fresh();
        let after = self.new_block();
        // |y/x| < 2^-64 with x < 0 makes z 0, else z is atan(|y/x|)
        self.sign_branch(x, flat, quotient);
        self.start(flat);
        let scaled = Emit(self).mul(ay, ratio);
        self.float_branch(Cond::Lt, scaled, ax, zero, quotient);
        self.start_cold(zero);
        self.give(z, number(0.0), after);
        self.start(quotient);
        self.atan_quotient(ay, ax, z, after);

        // by the signs: z, -z, pi - (z - pi_lo), or (z - pi_lo) - pi
        self.start(after);
        let z = Operand::Reg(z);
        let (right, left) = (self.new_block(), self.new_block());
        self.sign_branch(x, left, right);
        self.start(right);
        self.give_signed(y, result, z, done);
        self.start(left);
        let (down, up) = (self.new_block(), self.new_block());
        self.sign_branch(y, down, up);
        self.start(up);
        let value = trig::atan2_left(&mut Emit(self), z, false);
        self.give(result, value, done);
        self.start(down);
        let value = trig::atan2_left(&mut Emit(self), z, true);
        self.give(result, value, done);

        self.start_cold(vertical);
        self.give_by_sign(y, -FRAC_PI_2, FRAC_PI_2, result, done);

        // x infinite: y infinite too gives a multiple of pi/4, else 0 or pi, with |y| taken
        // again so that it need not stay in a register through everything before
        self.start_cold(x_infinite);
        let ay = Emit(self).abs(y);
        self.float_branch(Cond::Eq, ay, number(f64::INFINITY), both, y_finite);
        self.start_cold(both);
        let (right, left) = (self.new_block(), self.new_block());
        self.sign_branch(x, left, right);
        self.start_cold(right);
        let (minus, plus) = (trig::atan2_infinities(1), trig::atan2_infinities(0));
        self.give_by_sign(y, minus, plus, result, done);
        self.start_cold(left);
        let (minus, plus) = (trig::atan2_infinities(3), trig::atan2_infinities(2));
        self.give_by_sign(y, minus, plus, result, done);
        self.start_cold(y_finite);
        let (right, left) = (self.new_block(), self.new_block());
        self.sign_branch(x, left, right);
        self.start_cold(right);
        let (minus, plus) = (self.new_block(), self.new_block());
        self.sign_branch(y, minus, plus);
        self.start_cold(minus);
        self.give(result, number(-0.0), done);
        self.start_cold(plus);
        self.give(result, number(0.0), done);
        self.start_cold(left);
        self.give_by_sign(y, -PI, PI, result, done);
    }

    /// Writes `atan(ay / ax)` to `z` and goes to `after`, as `trig`'s `atan_quotient` computes
    /// it: from the rounded quotient and its `quotient_correction`, with both magnitudes scaled
    /// first when `trig::atan2_scale` says so. The scaling blocks come right after their tests,
    /// though they are rare, so that the scaled magnitudes are only live up to the correction.
    fn atan_quotient(&mut self, ay: Operand, ax: Operand, z: VReg, after: BlockId) {
        let q = Emit(self).div(ay, ax);
        let correct = self.new_block();
        let (tiny, rest) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, q, number(trig::TAN_TINY), tiny, rest);
        self.start(tiny);
        self.give(z, q, after);
        self.start(rest);
        let (sy, sx) = (self.fresh(), self.fresh());
        let (down, not_big, up, plain) = (
            self.new_block(),
            self.new_block(),
            self.new_block(),
            self.new_block(),
        );
        self.float_branch(Cond::Gt, ax, number(trig::ATAN2_BIG), down, not_big);
        self.start(not_big);
        self.float_branch(Cond::Lt, ax, number(1.0 / trig::ATAN2_BIG), up, plain);
        self.start(plain);
        self.copy_to(sy, ay);
        self.copy_to(sx, ax);
        self.jump(correct);
        for (block, scale) in [(down, 1.0 / trig::ATAN2_FACTOR), (up, trig::ATAN2_FACTOR)] {
            self.start(block);
            let scaled = Emit(self).mul(ay, number(scale));
            self.copy_to(sy, scaled);
            let scaled = Emit(self).mul(ax, number(scale));
            self.copy_to(sx, scaled);
            self.jump(correct);
        }
        self.start(correct);
        let c = trig::quotient_correction(&mut Emit(self), Operand::Reg(sy), Operand::Reg(sx), q);
        let (small, parts) = (self.new_block(), self.new_block());
        self.float_branch(Cond::Lt, q, number(0.4375), small, parts);
        self.start(small);
        let value = trig::atan_small(&mut Emit(self), q, c);
        self.give(z, value, after);
        self.start(parts);
        self.atan_parts(q, c, z, after);
    }
}

/// [`Arith`] over a [`Builder`]: each operation becomes an instruction in the current block,
/// except one of two immediates, which it computes as the interpreter would, to the same bits.
struct Emit<'b>(&'b mut Builder);

impl Emit<'_> {
    fn binary(&mut self, op: BinOp, lhs: Operand, rhs: Operand) -> Operand {
        // x - 0.0 is x, a -0.0 and a nan included
        if op == BinOp::Sub && rhs == number(0.0) {
            return lhs;
        }
        if let (Operand::Imm(a), Operand::Imm(b)) = (lhs, rhs) {
            let (a, b) = (float_of(a), float_of(b));
            return number(match op {
                BinOp::Add => a + b,
                BinOp::Sub => a - b,
                BinOp::Mul => a * b,
                _ => a / b,
            });
        }
        let dst = self.0.fresh();
        self.0.push(Inst::FloatBinary { op, dst, lhs, rhs });
        Operand::Reg(dst)
    }

    /// Writes the result of an operation of one float to a new vreg, or computes it at once for
    /// an immediate.
    fn unary(
        &mut self,
        a: Operand,
        now: fn(f64) -> f64,
        inst: fn(VReg, Operand) -> Inst,
    ) -> Operand {
        if let Operand::Imm(bits) = a {
            return number(now(float_of(bits)));
        }
        let dst = self.0.fresh();
        self.0.push(inst(dst, a));
        Operand::Reg(dst)
    }
}

impl Arith for Emit<'_> {
    type Value = Operand;
    fn num(&mut self, x: f64) -> Operand {
        number(x)
    }
    fn add(&mut self, a: Operand, b: Operand) -> Operand {
        self.binary(BinOp::Add, a, b)
    }
    fn sub(&mut self, a: Operand, b: Operand) -> Operand {
        self.binary(BinOp::Sub, a, b)
    }
    fn mul(&mut self, a: Operand, b: Operand) -> Operand {
        self.binary(BinOp::Mul, a, b)
    }
    fn div(&mut self, a: Operand, b: Operand) -> Operand {
        self.binary(BinOp::DivUnchecked, a, b)
    }
    fn neg(&mut self, a: Operand) -> Operand {
        self.unary(a, |x| -x, |dst, src| Inst::FloatNeg { dst, src })
    }
    fn abs(&mut self, a: Operand) -> Operand {
        self.unary(a, f64::abs, |dst, src| Inst::FloatAbs { dst, src })
    }
    fn sqrt(&mut self, a: Operand) -> Operand {
        self.unary(a, f64::sqrt, |dst, src| Inst::FloatSqrt { dst, src })
    }
}

/// Returns the float `x` as an immediate of its bits.
fn number(x: f64) -> Operand {
    Operand::Imm(x.to_bits() as i64)
}

/// Returns the float whose bits are `bits`.
fn float_of(bits: i64) -> f64 {
    f64::from_bits(bits as u64)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checker::TypeChecker;
    use crate::codegen::ir::lower::lower;

    /// Links, checks, and lowers `source`, then runs `pass` on its first function and returns it.
    fn first(source: &str, pass: impl Fn(&mut Function)) -> Function {
        let sources = crate::project::MapSources::default();
        let path = std::path::Path::new("main.stn");
        let (_, module) = crate::driver::load(path, source, &sources);
        let module = module.unwrap().module;
        let analysis = TypeChecker::new().analyze(&module);
        let mut program = lower(&module, &analysis).unwrap();
        let mut function = program.functions.remove(0);
        pass(&mut function);
        function
    }

    #[test]
    fn a_sine_and_a_cosine_share_one_reduction() {
        let source = "use math\nfn f(x: float) -> float:\n    ret math.sin(x) * math.cos(x) + math.sin(x)\nprint(f(1.0))\n";
        assert_eq!(
            first(source, share).to_string(),
            "fn f(v0):\n\
             b0:\n  v1, v6 = sincos v0\n  v2 = copy v6\n  v3 = fmul v1, v2\n  v4 = copy v1\n  \
             v5 = fadd v3, v4\n  ret v5\n"
        );
    }

    #[test]
    fn an_argument_written_between_calls_is_not_shared() {
        let source = "use math\nfn f(x: float) -> float:\n    a = math.sin(x)\n    x = x + 1.0\n    ret a + math.cos(x)\nprint(f(1.0))\n";
        assert_eq!(
            first(source, share).to_string(),
            "fn f(v0):\n\
             b0:\n  v2 = sin v0\n  v1 = copy v2\n  v0 = fadd v0, 4607182418800017408\n  \
             v3 = cos v0\n  v4 = fadd v1, v3\n  ret v4\n"
        );
    }

    #[test]
    fn calls_on_every_branch_share_the_one_before() {
        let source = "use math\nfn f(x: float, b: bool) -> float:\n    s = math.sin(x)\n    if b:\n        ret s + math.cos(x)\n    ret s - math.cos(x)\nprint(f(1.0, true))\n";
        assert_eq!(
            first(source, share).to_string(),
            "fn f(v0, v1):\n\
             b0:\n  v3, v8 = sincos v0\n  v2 = copy v3\n  br v1 -> b1, b2\n\
             b1:\n  v4 = copy v8\n  v5 = fadd v2, v4\n  ret v5\n\
             b2:\n  v6 = copy v8\n  v7 = fsub v2, v6\n  ret v7\n"
        );
    }

    #[test]
    fn expanding_leaves_one_reduction_through_the_prelude_per_call() {
        let source = "use math\nfn f(x: float) -> float:\n    ret math.sin(x) + math.cos(x) + math.tan(x) + math.atan2(x, 2.0)\nprint(f(1.0))\n";
        let function = first(source, |function| {
            share(function);
            expand(function);
        });
        let insts: Vec<&Inst> = function.blocks.iter().flat_map(|b| &b.insts).collect();
        assert!(
            !insts
                .iter()
                .any(|inst| matches!(inst, Inst::Trig { .. } | Inst::SinCos { .. }))
        );
        // one for the sine and cosine together, and one for the tangent
        let reductions = function
            .blocks
            .iter()
            .flat_map(|block| block.insts.windows(2))
            .filter(|pair| matches!(pair, [Inst::DepthCheck, Inst::ReducePio2 { .. }]))
            .count();
        assert_eq!(reductions, 2);
        let count = function.blocks.len();
        for block in &function.blocks {
            assert_eq!(block.insts.len(), block.spans.len());
            assert!(block.term.successors().iter().all(|&BlockId(b)| b < count));
        }
        // the code after the calls is the last block, which returns
        assert!(matches!(
            function.blocks.last().unwrap().term,
            Terminator::Return(_)
        ));
    }
}
