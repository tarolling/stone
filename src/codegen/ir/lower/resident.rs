//! Regions of code whose lists stay on a GPU between kernels.
//!
//! Arithmetic on lists that a GPU computes copies its lists to the device and its result back,
//! which costs about what computing on the processor does. A *region* lets lists stay: a run of
//! statements that compute on lists (`b = a * 2.0` then `c = b + a`, a *chain*), or a loop whose
//! body does nothing else with them (`for _ in range(k): x = x * 0.99 + v`). Each variable a
//! region's arithmetic names gets a slot, and each launch in the region tells the runtime, per
//! list it takes, the slot of the variable that holds it, so a list is copied to the device once
//! and a result stays there. `gpu_plan.begin` opens the region, deciding from its costs whether
//! to keep lists at all, and `gpu_plan.end` copies back each list the program reads after it.
//!
//! The processor must never read or change a list while the device holds a newer copy, so a
//! region holds, besides its arithmetic, only statements that name none of its variables (in a
//! chain, none it has named so far), cannot leave it but through its end (no `ret` or `?`, and
//! in a chain no `break` or `cont`), and, in top-level code, call nothing, since a function may
//! read a global. A statement of arithmetic whose launch lowering does not make (which should
//! not happen) gives the lists back with `gpu_plan.flush` instead.

use super::{Lowerer, Os};
use crate::ast::{Expr, ExprContext, ExprKind, Operator, Stmt, StmtKind, UnaryOp};
use crate::checker::Type;
use crate::codegen::context::function_label;
use crate::codegen::ir::{BlockId, Callee, Inst, Operand};
use crate::span::Span;
use crate::stdlib::gpu::PLAN_MODULE;
use std::collections::{HashMap, HashSet};

/// The most slots a region may have, which a descriptor's 6 bits of slot number allow.
const MAX_SLOTS: usize = 62;
/// The most lists a launch's descriptor describes, one byte each.
const MAX_LISTS: usize = 8;
/// A descriptor byte's bit that says the launch is the last to read a list.
const DROP: i64 = 64;

/// The region being lowered.
pub(super) struct Region {
    /// The slot of each variable the region's arithmetic names.
    slots: HashMap<String, usize>,
    /// The statement of arithmetic being lowered, if any: its variable and its span.
    current: Option<(String, Span)>,
    /// Whether the current statement's arithmetic was launched.
    launched: bool,
}

/// A statement of arithmetic on lists that a region can keep on the device.
struct Device<'a> {
    /// The variable it assigns.
    target: String,
    /// The variables whose lists it reads, the target's first for `x += ...`.
    reads: Vec<(String, &'a Expr)>,
    /// The variables its numbers name, which the processor reads.
    scalars: HashSet<String>,
}

impl Lowerer<'_> {
    /// Lowers the statements of a block, keeping the lists of each region in it on the GPU.
    pub(super) fn stmts(&mut self, body: &[Stmt]) -> Result<(), String> {
        // a block inside a region, such as a loop region's body, is part of it
        if self.region.is_some() {
            return body.iter().try_for_each(|stmt| self.region_stmt(stmt));
        }
        if !self.keeps_lists() {
            for stmt in body {
                self.stmt(stmt)?;
            }
            return Ok(());
        }
        let mut i = 0;
        while i < body.len() {
            if let Some(vars) = self.loop_region(&body[i]) {
                self.region(&body[i..=i], vars)?;
                i += 1;
            } else if let Some((end, vars)) = self.chain(body, i) {
                self.region(&body[i..end], vars)?;
                i = end;
            } else {
                self.stmt(&body[i])?;
                i += 1;
            }
        }
        Ok(())
    }

    /// Returns whether a block may hold a region: in a Linux build that launches kernels (whose
    /// planner is stone code), outside any other region.
    fn keeps_lists(&self) -> bool {
        self.context.gpu_launch.launches() && self.context.os == Os::Linux && self.region.is_none()
    }

    /// Returns `stmt` as a statement of arithmetic a region can keep: `x = <arithmetic>` or
    /// `x op= <arithmetic>` on lists of ints or floats, whose lists are variables and whose
    /// operations a GPU computes.
    fn device<'a>(&self, stmt: &'a Stmt) -> Option<Device<'a>> {
        let (target, value, op) = match &stmt.kind {
            StmtKind::Assign { targets, value } if targets.len() == 1 => (&targets[0], value, None),
            StmtKind::AugAssign { target, op, value } => (&**target, value, Some(*op)),
            _ => return None,
        };
        let ExprKind::Name { id, .. } = &target.kind else {
            return None;
        };
        let float = match self.span_type(target.span)? {
            Type::List(elem) if *elem == Type::Float => true,
            Type::List(elem) if *elem == Type::Int => false,
            _ => return None,
        };
        let mut device = Device {
            target: id.clone(),
            reads: Vec::new(),
            scalars: HashSet::new(),
        };
        match op {
            Some(op) if allowed(op, float) => device.reads.push((id.clone(), target)),
            Some(_) => return None,
            // a plain copy or a literal is not arithmetic
            None if !matches!(
                value.kind,
                ExprKind::BinOp { .. } | ExprKind::UnaryOp { .. }
            ) =>
            {
                return None;
            }
            None => {}
        }
        if !self.device_expr(value, float, &mut device) || device.reads.is_empty() {
            return None;
        }
        Some(device)
    }

    /// Adds what `expr`, arithmetic on lists of floats if `float` and of ints otherwise, reads to
    /// `device`, and returns whether a GPU computes it all.
    fn device_expr<'a>(&self, expr: &'a Expr, float: bool, device: &mut Device<'a>) -> bool {
        match self.span_type(expr.span) {
            Some(Type::List(elem)) if (*elem == Type::Float) == float => match &expr.kind {
                ExprKind::Name { id, .. } => {
                    device.reads.push((id.clone(), expr));
                    true
                }
                ExprKind::BinOp { op, left, right } => {
                    allowed(*op, float)
                        && self.device_expr(left, float, device)
                        && self.device_expr(right, float, device)
                }
                ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd | UnaryOp::UnarySub,
                    operand,
                } => self.device_expr(operand, float, device),
                ExprKind::UnaryOp {
                    op: UnaryOp::Invert,
                    operand,
                } => !float && self.device_expr(operand, float, device),
                _ => false,
            },
            Some(Type::Int | Type::Float) => {
                let (mut calls, mut lists) = (false, false);
                expr.visit(&mut |inner| {
                    match &inner.kind {
                        ExprKind::Name { id, .. } => {
                            device.scalars.insert(id.clone());
                        }
                        ExprKind::Call { .. } => calls = true,
                        _ => {}
                    }
                    // arithmetic on lists in a number would launch with the statement's slots
                    lists |= matches!(self.span_type(inner.span), Some(Type::List(_)));
                });
                // in top-level code a function may read a global the region holds
                !lists && !(calls && self.name.is_none())
            }
            _ => false,
        }
    }

    /// Returns whether `stmt` may sit in a region whose variables are `vars` without the
    /// processor touching their lists: it names none of them, cannot leave the region (nor, if
    /// `jumps`, jump out of the loop it is in), and in top-level code calls nothing.
    fn neutral(&self, stmt: &Stmt, vars: &HashSet<String>, jumps: bool) -> bool {
        let mut fine = true;
        let top = self.name.is_none();
        visit(stmt, &mut |stmt| match &stmt.kind {
            StmtKind::Return { .. } | StmtKind::FunctionDef { .. } => fine = false,
            StmtKind::Break | StmtKind::Continue if jumps => fine = false,
            _ => {
                for expr in stmt.exprs() {
                    expr.visit(&mut |inner| match &inner.kind {
                        ExprKind::Name { id, .. } if vars.contains(id) => fine = false,
                        ExprKind::Try { .. } => fine = false,
                        ExprKind::Call { .. } if top => fine = false,
                        _ => {}
                    });
                }
            }
        });
        fine
    }

    /// Returns where the chain starting at `body[start]` ends and its variables, if one does:
    /// statements of arithmetic and statements that name none of the variables so far, ending
    /// at the last statement of arithmetic, with at least two of them and one reading what an
    /// earlier one wrote.
    fn chain(&self, body: &[Stmt], start: usize) -> Option<(usize, Vec<String>)> {
        self.device(&body[start])?;
        let mut vars = HashSet::new();
        let mut order = Vec::new();
        let mut written = HashSet::new();
        let (mut shares, mut count, mut end) = (false, 0, start);
        for (index, stmt) in body.iter().enumerate().skip(start) {
            if let Some(device) = self.device(stmt) {
                if device.scalars.iter().any(|name| vars.contains(name)) {
                    break;
                }
                shares |= device.reads.iter().any(|(name, _)| written.contains(name));
                for name in device
                    .reads
                    .iter()
                    .map(|(name, _)| name)
                    .chain([&device.target])
                {
                    if vars.insert(name.clone()) {
                        order.push(name.clone());
                    }
                }
                written.insert(device.target);
                count += 1;
                end = index + 1;
            } else if !self.neutral(stmt, &vars, true) {
                break;
            }
        }
        (count >= 2 && shares && order.len() <= MAX_SLOTS).then_some((end, order))
    }

    /// Returns the variables of `stmt` if it is a loop whose lists a region can keep: one whose
    /// body, nested blocks included, does arithmetic on lists and nothing else that names their
    /// variables, and that it can leave only through its end or a `break`.
    fn loop_region(&self, stmt: &Stmt) -> Option<Vec<String>> {
        let (heads, body): (Vec<&Expr>, &[Stmt]) = match &stmt.kind {
            StmtKind::For { target, iter, body } => (vec![target, iter], body),
            StmtKind::While { test, body } => (vec![test], body),
            _ => return None,
        };
        let mut order = Vec::new();
        let mut vars = HashSet::new();
        for inner in body {
            visit(inner, &mut |stmt| {
                if let Some(device) = self.device(stmt) {
                    for (name, _) in device.reads {
                        if vars.insert(name.clone()) {
                            order.push(name);
                        }
                    }
                    if vars.insert(device.target.clone()) {
                        order.push(device.target);
                    }
                }
            });
        }
        if order.is_empty() || order.len() > MAX_SLOTS {
            return None;
        }
        let mut names = HashSet::new();
        for head in heads {
            head.visit(&mut |inner| {
                if let ExprKind::Name { id, .. } = &inner.kind {
                    names.insert(id.clone());
                }
            });
        }
        if names.iter().any(|name| vars.contains(name)) || !self.loop_body_fits(body, &vars) {
            return None;
        }
        Some(order)
    }

    /// Returns whether every statement of `body` is arithmetic a region keeps, a `break` or
    /// `cont`, a block whose head names none of `vars` and whose statements fit in turn, or a
    /// statement `neutral` allows.
    fn loop_body_fits(&self, body: &[Stmt], vars: &HashSet<String>) -> bool {
        body.iter().all(|stmt| {
            if let Some(device) = self.device(stmt) {
                return !device.scalars.iter().any(|name| vars.contains(name));
            }
            let quiet = |exprs: Vec<&Expr>| {
                let mut quiet = true;
                for expr in exprs {
                    expr.visit(&mut |inner| match &inner.kind {
                        ExprKind::Name { id, .. } if vars.contains(id) => quiet = false,
                        ExprKind::Try { .. } => quiet = false,
                        ExprKind::Call { .. } if self.name.is_none() => quiet = false,
                        _ => {}
                    });
                }
                quiet
            };
            match &stmt.kind {
                StmtKind::Break | StmtKind::Continue => true,
                StmtKind::If { body, orelse, .. } => {
                    quiet(stmt.exprs())
                        && self.loop_body_fits(body, vars)
                        && self.loop_body_fits(orelse, vars)
                }
                StmtKind::For { body, .. } | StmtKind::While { body, .. } => {
                    quiet(stmt.exprs()) && self.loop_body_fits(body, vars)
                }
                _ => self.neutral(stmt, vars, false),
            }
        })
    }

    /// Lowers `stmts`, a region whose variables are `vars`, between `gpu_plan.begin` and
    /// `gpu_plan.end`.
    fn region(&mut self, stmts: &[Stmt], vars: Vec<String>) -> Result<(), String> {
        // the first list the region reads gives its length
        let mut first = None;
        let mut inputs = HashSet::new();
        let mut written = HashSet::new();
        for stmt in stmts {
            visit(stmt, &mut |stmt| {
                if let Some(device) = self.device(stmt) {
                    if first.is_none() {
                        first = device.reads.first().map(|(name, _)| name.clone());
                    }
                    for (name, _) in &device.reads {
                        if !written.contains(name) {
                            inputs.insert(name.clone());
                        }
                    }
                    written.insert(device.target);
                }
            });
        }
        let Some(first) = first else {
            for stmt in stmts {
                self.stmt(stmt)?;
            }
            return Ok(());
        };
        let last = stmts.last().expect("a region has statements").span;
        let live = self.live_after.get(&last).cloned().unwrap_or_default();
        let outputs = vars.iter().filter(|name| live.contains(*name)).count();
        let mask = vars
            .iter()
            .enumerate()
            .filter(|(_, name)| live.contains(*name))
            .fold(0i64, |mask, (slot, _)| mask | 1 << slot);
        let trips = match &stmts[0].kind {
            StmtKind::For { iter, .. } => self.trips(iter)?,
            StmtKind::While { .. } => Operand::Imm(-1),
            _ => Operand::Imm(1),
        };
        let list = self.expr(&Expr {
            kind: ExprKind::Name {
                id: first,
                ctx: ExprContext::Load,
            },
            span: Span::default(),
        })?;
        let len = self.fresh();
        self.push(Inst::ListLen { dst: len, list });
        let opened = self.fresh();
        let records = self.context.device_kernels.borrow().len() as i64;
        self.push(Inst::Call {
            dsts: vec![opened],
            callee: plan("begin"),
            args: vec![
                Operand::Imm(records),
                Operand::Imm(records),
                Operand::Imm(inputs.len() as i64),
                Operand::Imm(outputs as i64),
                Operand::Reg(len),
                trips,
                Operand::Imm(vars.len() as i64),
            ],
        });
        let begin = self.current_inst();
        self.region = Some(Region {
            slots: vars
                .iter()
                .enumerate()
                .map(|(slot, name)| (name.clone(), slot))
                .collect(),
            current: None,
            launched: false,
        });
        let lowered = stmts.iter().try_for_each(|stmt| self.region_stmt(stmt));
        self.region = None;
        lowered?;
        // the region's kernels are those recorded while lowering it
        let last_record = self.context.device_kernels.borrow().len() as i64;
        if let Some((block, index)) = begin
            && let Inst::Call { args, .. } = &mut self.blocks[block.0].insts[index]
        {
            args[1] = Operand::Imm(last_record);
        }
        self.push(Inst::Call {
            dsts: vec![],
            callee: plan("end"),
            args: vec![Operand::Imm(mask)],
        });
        Ok(())
    }

    /// Lowers a statement of a region, telling each launch of its arithmetic which slots its
    /// lists have, and giving the lists back to the processor if its arithmetic was not
    /// launched after all.
    fn region_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        let device = self.device(stmt).map(|device| device.target);
        let Some(target) = device else {
            // a loop lowers its body through `stmts`, which reaches the arithmetic in it
            return self.stmt(stmt);
        };
        if let Some(region) = &mut self.region {
            region.current = Some((target, stmt.span));
            region.launched = false;
        }
        let lowered = self.stmt(stmt);
        let launched = self.region.as_mut().is_some_and(|region| {
            region.current = None;
            region.launched
        });
        lowered?;
        if !launched {
            self.push(Inst::Call {
                dsts: vec![],
                callee: plan("flush"),
                args: vec![],
            });
        }
        Ok(())
    }

    /// Returns how many turns the loop over `iter` takes, if a `range` of names and constants
    /// gives it, else -1.
    fn trips(&mut self, iter: &Expr) -> Result<Operand, String> {
        let ExprKind::Call { func, args, .. } = &iter.kind else {
            return Ok(Operand::Imm(-1));
        };
        let simple =
            |expr: &Expr| matches!(expr.kind, ExprKind::Name { .. } | ExprKind::Constant { .. });
        if !matches!(&func.kind, ExprKind::Name { id, .. } if id == "range")
            || !args.iter().all(simple)
        {
            return Ok(Operand::Imm(-1));
        }
        match &args[..] {
            [limit] => self.expr(limit),
            [start, limit] => {
                let (start, limit) = (self.expr(start)?, self.expr(limit)?);
                let dst = self.fresh();
                self.push(Inst::Binary {
                    op: crate::codegen::ir::BinOp::Sub,
                    dst,
                    lhs: limit,
                    rhs: start,
                });
                Ok(Operand::Reg(dst))
            }
            _ => Ok(Operand::Imm(-1)),
        }
    }

    /// Returns where the last instruction pushed is, if any.
    fn current_inst(&self) -> Option<(BlockId, usize)> {
        let block = self.current?;
        let index = self.blocks[block.0].insts.len().checked_sub(1)?;
        Some((block, index))
    }

    /// Returns a launch's descriptor in the current region, given the variable each list the
    /// kernel takes (after the result) holds: per list, the result's first, a byte of its
    /// variable's slot plus 1, with [`DROP`] on the last read of a variable, or 0 outside one.
    /// It also notes the statement's arithmetic was launched.
    pub(super) fn residency(&mut self, lists: &[Option<String>]) -> i64 {
        let Some(region) = &mut self.region else {
            return 0;
        };
        let Some((target, span)) = region.current.clone() else {
            return 0;
        };
        region.launched = true;
        let live = self.live_after.get(&span).cloned().unwrap_or_default();
        let region = self.region.as_ref().expect("in a region");
        let byte = |name: &str| region.slots.get(name).map_or(0, |&slot| slot as i64 + 1);
        // a result no one reads after the statement is copied back, not kept
        let mut bytes = vec![if live.contains(&target) {
            byte(&target)
        } else {
            0
        }];
        for name in lists {
            bytes.push(match name {
                Some(name) => {
                    let slot = byte(name);
                    let last = !live.contains(name) && *name != target;
                    if slot != 0 && last { slot | DROP } else { slot }
                }
                None => 0,
            });
        }
        if bytes.len() > MAX_LISTS {
            // a list past the descriptor's last byte would be read from the processor's copy
            if let Some(region) = &mut self.region {
                region.launched = false;
            }
            return 0;
        }
        bytes
            .iter()
            .enumerate()
            .fold(0, |descriptor, (index, byte)| {
                descriptor | byte << (8 * index)
            })
    }
}

/// Returns a call of the planner's function `name`, which lowering makes after the program's.
fn plan(name: &str) -> Callee {
    Callee::Helper(function_label(&format!("{PLAN_MODULE}.{name}")))
}

/// Returns whether a GPU computes `op` on lists of floats if `float` and of ints otherwise.
fn allowed(op: Operator, float: bool) -> bool {
    match op {
        Operator::Add | Operator::Subtract | Operator::Multiply => true,
        Operator::Divide => float,
        Operator::BitAnd | Operator::BitOr | Operator::BitXor => !float,
        _ => false,
    }
}

/// Calls `f` on `stmt` and on every statement in the blocks it holds.
fn visit<'a>(stmt: &'a Stmt, f: &mut impl FnMut(&'a Stmt)) {
    f(stmt);
    match &stmt.kind {
        StmtKind::If { body, orelse, .. } => {
            body.iter().chain(orelse).for_each(|stmt| visit(stmt, f));
        }
        StmtKind::For { body, .. } | StmtKind::While { body, .. } => {
            body.iter().for_each(|stmt| visit(stmt, f));
        }
        StmtKind::Match { cases, .. } => {
            for case in cases {
                case.body.iter().for_each(|stmt| visit(stmt, f));
            }
        }
        _ => {}
    }
}
