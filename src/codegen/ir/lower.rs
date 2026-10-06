//! Lowers a checked AST to [`ir`](super) functions.
//!
//! Names resolve the way the checker resolves them: a function's parameters and assigned names
//! are its locals, each one vreg, and every name assigned at the top level is a global that lives
//! in memory. Evaluation is left to right, like the interpreter, so runtime errors happen in the
//! same order. For example, `x = x + 1` in a function lowers to `v0 = add v0, 1`.

#[cfg(test)]
mod tests;

use super::{
    BinOp, Block, BlockId, Callee, Cond, Function, Inst, Operand, Program, Terminator, VReg,
};
use crate::ast::{Arg, BoolOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp};
use crate::checker::{Type, collect_assigned, range_args};
use crate::span::Span;
use std::collections::{HashMap, HashSet};

/// Lowers every function in `module`, then its top-level code as `main`, using the types the
/// checker inferred.
///
/// Fails only on input the checker rejects, such as a constant type no backend supports.
pub fn lower(module: &Mod, types: &HashMap<Span, Type>) -> Result<Program, String> {
    let Mod::Module { body } = module;

    let mut assigned = Vec::new();
    collect_assigned(body, &mut assigned);
    let globals: Vec<String> = assigned.into_iter().map(|(name, _)| name).collect();
    let global_set: HashSet<String> = globals.iter().cloned().collect();

    let mut functions = Vec::new();
    let mut top_level = Vec::new();
    for stmt in body {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                let lowerer = Lowerer::new(types, &global_set, Some(name.clone()));
                functions.push(lowerer.function(&args.args, body)?);
            }
            _ => top_level.push(stmt.clone()),
        }
    }
    functions.push(Lowerer::new(types, &global_set, None).function(&[], &top_level)?);

    Ok(Program { functions, globals })
}

/// What a `for` loop walks: a range up to an end, or a list.
#[derive(Clone, Copy)]
enum Iterable {
    Range(Operand),
    List(Operand),
}

/// A block being built, which may not have a terminator yet.
struct PendingBlock {
    insts: Vec<Inst>,
    term: Option<Terminator>,
    loop_depth: u32,
}

/// Lowers one function body.
struct Lowerer<'a> {
    types: &'a HashMap<Span, Type>,
    globals: &'a HashSet<String>,
    name: Option<String>,
    /// Each local's vreg. Empty in `main`, where assigned names are globals.
    locals: HashMap<String, VReg>,
    vreg_count: u32,
    blocks: Vec<PendingBlock>,
    /// Block ids in the order they were started, which becomes the layout order.
    order: Vec<BlockId>,
    /// The block instructions go to, or `None` after a terminator, until the next block starts.
    current: Option<BlockId>,
    /// Each enclosing loop's `(break, continue)` targets, innermost last.
    loops: Vec<(BlockId, BlockId)>,
    loop_depth: u32,
}

impl<'a> Lowerer<'a> {
    fn new(
        types: &'a HashMap<Span, Type>,
        globals: &'a HashSet<String>,
        name: Option<String>,
    ) -> Self {
        Lowerer {
            types,
            globals,
            name,
            locals: HashMap::new(),
            vreg_count: 0,
            blocks: Vec::new(),
            order: Vec::new(),
            current: None,
            loops: Vec::new(),
            loop_depth: 0,
        }
    }

    fn function(mut self, args: &[Arg], body: &[Stmt]) -> Result<Function, String> {
        let mut params = Vec::new();
        if self.name.is_some() {
            for arg in args {
                let reg = self.local(&arg.arg);
                params.push(reg);
            }
            let mut assigned = Vec::new();
            collect_assigned(body, &mut assigned);
            for (name, _) in assigned {
                self.local(&name);
            }
        }

        let entry = self.new_block();
        self.start(entry);
        for stmt in body {
            self.stmt(stmt)?;
        }
        // falling off the end returns none, and main exits with 0
        if self.current.is_some() {
            self.terminate(Terminator::Return(Some(Operand::Imm(0))));
        }

        Ok(Function {
            name: self.name.clone(),
            params,
            blocks: self.layout(),
            vreg_count: self.vreg_count,
        })
    }

    /// Returns the vreg of local `name`, creating it the first time.
    fn local(&mut self, name: &str) -> VReg {
        if let Some(reg) = self.locals.get(name) {
            return *reg;
        }
        let reg = self.fresh();
        self.locals.insert(name.to_string(), reg);
        reg
    }

    fn fresh(&mut self) -> VReg {
        let reg = VReg(self.vreg_count);
        self.vreg_count += 1;
        reg
    }

    /// Returns `into` if the caller wants the result there, or else a fresh vreg.
    fn dst(&mut self, into: Option<VReg>) -> VReg {
        into.unwrap_or_else(|| self.fresh())
    }

    /// Moves `value` into `into` when the caller asked for a destination, and returns where the
    /// result is.
    fn finish(&mut self, value: Operand, into: Option<VReg>) -> Operand {
        match into {
            Some(dst) => {
                if value != Operand::Reg(dst) {
                    self.push(Inst::Copy { dst, src: value });
                }
                Operand::Reg(dst)
            }
            None => value,
        }
    }

    /// Returns whether `value` is a local variable's vreg, which a later assignment can change.
    fn is_variable(&self, value: Operand) -> bool {
        matches!(value, Operand::Reg(reg) if self.locals.values().any(|&local| local == reg))
    }

    /// Returns `value` in a vreg that nothing else writes, copying a local variable so that
    /// reassigning the variable cannot change it.
    fn snapshot(&mut self, value: Operand) -> Operand {
        if self.is_variable(value) {
            let copy = self.fresh();
            self.push(Inst::Copy {
                dst: copy,
                src: value,
            });
            Operand::Reg(copy)
        } else {
            value
        }
    }

    fn type_of(&self, expr: &Expr) -> Result<Type, String> {
        self.types
            .get(&expr.span)
            .cloned()
            .ok_or_else(|| format!("expression at {:?} has no type", expr.span.start))
    }

    fn new_block(&mut self) -> BlockId {
        self.blocks.push(PendingBlock {
            insts: Vec::new(),
            term: None,
            loop_depth: 0,
        });
        BlockId(self.blocks.len() - 1)
    }

    /// Makes `block` the current block, placing it next in layout order. If the previous block
    /// has no terminator yet, control falls through into `block`.
    fn start(&mut self, block: BlockId) {
        if self.current.is_some() {
            self.terminate(Terminator::Jump(block));
        }
        self.blocks[block.0].loop_depth = self.loop_depth;
        self.order.push(block);
        self.current = Some(block);
    }

    fn push(&mut self, inst: Inst) {
        // code after `ret`, `break`, or `cont` is unreachable, but still lowered
        let block = match self.current {
            Some(block) => block,
            None => {
                let block = self.new_block();
                self.start(block);
                block
            }
        };
        self.blocks[block.0].insts.push(inst);
    }

    fn terminate(&mut self, term: Terminator) {
        let block = match self.current.take() {
            Some(block) => block,
            None => {
                let block = self.new_block();
                self.start(block);
                self.current = None;
                block
            }
        };
        self.blocks[block.0].term = Some(term);
    }

    fn jump(&mut self, target: BlockId) {
        self.terminate(Terminator::Jump(target));
    }

    /// Renumbers the blocks into the order they were started.
    fn layout(&mut self) -> Vec<Block> {
        // a block nothing started can only be an unused target, so it goes last
        for id in 0..self.blocks.len() {
            if !self.order.contains(&BlockId(id)) {
                self.order.push(BlockId(id));
            }
        }
        let mut position = vec![0; self.blocks.len()];
        for (pos, id) in self.order.iter().enumerate() {
            position[id.0] = pos;
        }
        let renumber = |id: BlockId| BlockId(position[id.0]);

        let mut pending: Vec<Option<PendingBlock>> = std::mem::take(&mut self.blocks)
            .into_iter()
            .map(Some)
            .collect();
        self.order
            .iter()
            .map(|id| {
                let block = pending[id.0].take().expect("each block is placed once");
                let term = match block
                    .term
                    .unwrap_or(Terminator::Return(Some(Operand::Imm(0))))
                {
                    Terminator::Jump(target) => Terminator::Jump(renumber(target)),
                    Terminator::Branch {
                        cond,
                        then,
                        otherwise,
                    } => Terminator::Branch {
                        cond,
                        then: renumber(then),
                        otherwise: renumber(otherwise),
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
                        then: renumber(then),
                        otherwise: renumber(otherwise),
                    },
                    ret @ Terminator::Return(_) => ret,
                };
                Block {
                    insts: block.insts,
                    term,
                    loop_depth: block.loop_depth,
                }
            })
            .collect()
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                if let [target] = targets.as_slice()
                    && let ExprKind::Name { id, .. } = &target.kind
                    && let Some(&local) = self.locals.get(id)
                {
                    self.expr_into(value, Some(local))?;
                    return Ok(());
                }
                let value = self.expr(value)?;
                for target in targets {
                    self.assign(target, value)?;
                }
            }

            StmtKind::Expr { value } => {
                self.expr(value)?;
            }

            StmtKind::Return { value } => {
                let value = match value {
                    Some(value) => self.expr(value)?,
                    None => Operand::Imm(0),
                };
                // `ret` at the top level ends the program normally
                let value = if self.name.is_some() {
                    value
                } else {
                    Operand::Imm(0)
                };
                self.terminate(Terminator::Return(Some(value)));
            }

            StmtKind::If { test, body, orelse } => {
                let then = self.new_block();
                let end = self.new_block();
                let otherwise = if orelse.is_empty() {
                    end
                } else {
                    self.new_block()
                };
                self.cond(test, then, otherwise)?;
                self.start(then);
                for stmt in body {
                    self.stmt(stmt)?;
                }
                if !orelse.is_empty() {
                    if self.current.is_some() {
                        self.jump(end);
                    }
                    self.start(otherwise);
                    for stmt in orelse {
                        self.stmt(stmt)?;
                    }
                }
                self.start(end);
            }

            StmtKind::While { test, body } => {
                let header = self.new_block();
                let loop_body = self.new_block();
                let exit = self.new_block();
                self.loop_depth += 1;
                self.start(header);
                self.cond(test, loop_body, exit)?;
                self.start(loop_body);
                self.loops.push((exit, header));
                for stmt in body {
                    self.stmt(stmt)?;
                }
                self.loops.pop();
                if self.current.is_some() {
                    self.jump(header);
                }
                self.loop_depth -= 1;
                self.start(exit);
            }

            StmtKind::For { target, iter, body } => self.for_loop(target, iter, body)?,

            StmtKind::Break => {
                if let Some(&(exit, _)) = self.loops.last() {
                    self.jump(exit);
                }
            }

            StmtKind::Continue => {
                if let Some(&(_, header)) = self.loops.last() {
                    self.jump(header);
                }
            }

            StmtKind::Delete { targets } => {
                for target in targets {
                    if let ExprKind::Name { id, .. } = &target.kind {
                        if let Some(&local) = self.locals.get(id) {
                            self.push(Inst::Copy {
                                dst: local,
                                src: Operand::Imm(0),
                            });
                        } else if self.globals.contains(id) {
                            // zeroed, but still counted as assigned
                            self.push(Inst::StoreGlobal {
                                name: id.clone(),
                                src: Operand::Imm(0),
                                mark_set: false,
                            });
                        }
                    }
                }
            }

            StmtKind::FunctionDef { .. } => {
                return Err("functions must be defined at the top level".to_string());
            }
        }
        Ok(())
    }

    /// Stores `value` in an assignment target: a local, a global, or a list element.
    fn assign(&mut self, target: &Expr, value: Operand) -> Result<(), String> {
        match &target.kind {
            ExprKind::Name { id, .. } => {
                if let Some(&local) = self.locals.get(id) {
                    if value != Operand::Reg(local) {
                        self.push(Inst::Copy {
                            dst: local,
                            src: value,
                        });
                    }
                } else if self.globals.contains(id) {
                    self.push(Inst::StoreGlobal {
                        name: id.clone(),
                        src: value,
                        mark_set: true,
                    });
                }
            }
            ExprKind::Subscript {
                value: list, slice, ..
            } => {
                // the value was evaluated first, like the interpreter
                let list = self.expr(list)?;
                let index = self.expr(slice)?;
                self.push(Inst::ListStore { list, index, value });
            }
            _ => {}
        }
        Ok(())
    }

    /// Lowers `for target in iter;`, counting with hidden vregs that the body cannot assign, so
    /// assigning the loop variable never changes the iteration.
    ///
    /// A range's bounds are evaluated once, left to right. A list's length is reread every
    /// iteration, so a list that grows while it is iterated keeps going, like in the interpreter.
    fn for_loop(&mut self, target: &Expr, iter: &Expr, body: &[Stmt]) -> Result<(), String> {
        let header = self.new_block();
        let loop_body = self.new_block();
        let exit = self.new_block();

        let next = self.fresh();
        let iterable = match range_args(iter) {
            Some([limit]) => {
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                let end = self.expr(limit)?;
                Iterable::Range(self.snapshot(end))
            }
            Some([start, limit]) => {
                self.expr_into(start, Some(next))?;
                let end = self.expr(limit)?;
                Iterable::Range(self.snapshot(end))
            }
            Some(_) => return Err("range() takes 1 or 2 arguments".to_string()),
            None => {
                let list = self.expr(iter)?;
                let list = self.snapshot(list);
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                Iterable::List(list)
            }
        };

        self.loop_depth += 1;
        self.start(header);
        let end = match iterable {
            Iterable::List(list) => {
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                Operand::Reg(len)
            }
            Iterable::Range(end) => end,
        };
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(next),
            rhs: end,
            then: loop_body,
            otherwise: exit,
        });

        self.start(loop_body);
        let local = match &target.kind {
            ExprKind::Name { id, .. } => self.locals.get(id).copied(),
            _ => return Err("a 'for' loop's variable must be a name".to_string()),
        };
        let value = match iterable {
            Iterable::List(list) => {
                let dst = self.dst(local);
                self.push(Inst::ListGet {
                    dst,
                    list,
                    index: Operand::Reg(next),
                });
                Operand::Reg(dst)
            }
            Iterable::Range(_) => Operand::Reg(next),
        };
        self.assign(target, value)?;
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: Operand::Imm(1),
        });

        self.loops.push((exit, header));
        for stmt in body {
            self.stmt(stmt)?;
        }
        self.loops.pop();
        if self.current.is_some() {
            self.jump(header);
        }
        self.loop_depth -= 1;
        self.start(exit);
        Ok(())
    }

    /// Lowers a test, going to `then` if it is truthy and to `otherwise` if not, and ends the
    /// current block.
    ///
    /// Comparisons branch on the flags directly, and `and`, `or`, and `not` become branches, so
    /// `if a < b and c;` never materializes a bool.
    fn cond(&mut self, test: &Expr, then: BlockId, otherwise: BlockId) -> Result<(), String> {
        match &test.kind {
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } if comparators
                .iter()
                .all(|c| self.type_of(c).is_ok_and(|ty| ty != Type::Str)) =>
            {
                // a < b < c means a < b and b < c, with b evaluated once and c skipped once false
                let mut lhs = self.expr(left)?;
                for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
                    let rhs = self.expr(comparator)?;
                    let float = self.type_of(comparator)? == Type::Float;
                    let next = if i + 1 < ops.len() {
                        self.new_block()
                    } else {
                        then
                    };
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::from(op),
                        float,
                        lhs,
                        rhs,
                        then: next,
                        otherwise,
                    });
                    if next != then {
                        self.start(next);
                    }
                    lhs = rhs;
                }
            }

            ExprKind::BoolOp { op, values } if !values.is_empty() => {
                let (last, rest) = values.split_last().expect("values is not empty");
                for value in rest {
                    let next = self.new_block();
                    match op {
                        BoolOp::And => self.cond(value, next, otherwise)?,
                        BoolOp::Or => self.cond(value, then, next)?,
                    }
                    self.start(next);
                }
                self.cond(last, then, otherwise)?;
            }

            ExprKind::UnaryOp {
                op: UnaryOp::Not,
                operand,
            } => self.cond(operand, otherwise, then)?,

            ExprKind::Constant { value, .. }
                if matches!(**value, Constant::Int(_) | Constant::Bool(_)) =>
            {
                let truthy = matches!(**value, Constant::Int(n) if n != 0)
                    || matches!(**value, Constant::Bool(true));
                self.jump(if truthy { then } else { otherwise });
            }

            _ => {
                let cond = self.expr(test)?;
                self.terminate(Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                });
            }
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> Result<Operand, String> {
        self.expr_into(expr, None)
    }

    /// Lowers `expr` and returns where its value is. With `into`, the value ends up in that vreg,
    /// and instructions that read all their inputs before writing write it directly.
    fn expr_into(&mut self, expr: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        match &expr.kind {
            ExprKind::Constant { value, .. } => {
                let value = match &**value {
                    Constant::Int(n) => Operand::Imm(*n),
                    Constant::Float(x) => Operand::Imm(x.to_bits() as i64),
                    Constant::Bool(b) => Operand::Imm(*b as i64),
                    Constant::None => Operand::Imm(0),
                    Constant::Str(s) => {
                        let dst = self.dst(into);
                        self.push(Inst::StrAddr {
                            dst,
                            text: s.clone(),
                        });
                        Operand::Reg(dst)
                    }
                    other => return Err(format!("unsupported constant value {:?}", other)),
                };
                Ok(self.finish(value, into))
            }

            ExprKind::Name { id, .. } => {
                if let Some(&local) = self.locals.get(id) {
                    return Ok(self.finish(Operand::Reg(local), into));
                }
                if self.globals.contains(id) {
                    let dst = self.dst(into);
                    // top-level code is checked to assign before reading, but a function can run
                    // before a global it reads is assigned
                    self.push(Inst::LoadGlobal {
                        dst,
                        name: id.clone(),
                        checked: self.name.is_some(),
                    });
                    return Ok(Operand::Reg(dst));
                }
                Ok(self.finish(Operand::Imm(0), into))
            }

            ExprKind::BinOp { op, left, right } => {
                let ty = self.type_of(expr)?;
                let lhs = self.expr(left)?;
                let rhs = self.expr(right)?;
                let dst = self.dst(into);
                let op = match op {
                    Operator::Add => BinOp::Add,
                    Operator::Subtract => BinOp::Sub,
                    Operator::Multiply => BinOp::Mul,
                    Operator::Divide => BinOp::Div,
                    Operator::Modulo => BinOp::Rem,
                    Operator::Power => BinOp::Pow,
                };
                self.push(match ty {
                    // only `+` applies to strings
                    Type::Str => Inst::Call {
                        dst: Some(dst),
                        callee: Callee::Runtime("stone.str_concat"),
                        args: vec![lhs, rhs],
                    },
                    Type::Float => Inst::FloatBinary { op, dst, lhs, rhs },
                    _ => Inst::Binary { op, dst, lhs, rhs },
                });
                Ok(Operand::Reg(dst))
            }

            ExprKind::UnaryOp { op, operand } => {
                if *op == UnaryOp::UnaryAdd {
                    let value = self.expr(operand)?;
                    return Ok(self.finish(value, into));
                }
                let float = self.type_of(operand)? == Type::Float;
                if *op == UnaryOp::UnarySub {
                    if let Some(n) = constant_int(expr) {
                        return Ok(self.finish(Operand::Imm(n), into));
                    }
                    if let ExprKind::Constant { value, .. } = &operand.kind
                        && let Constant::Float(x) = **value
                    {
                        // the same bits flipping the sign bit gives
                        return Ok(self.finish(Operand::Imm((-x).to_bits() as i64), into));
                    }
                }
                let src = self.expr(operand)?;
                let dst = self.dst(into);
                self.push(match op {
                    UnaryOp::Not => Inst::Not { dst, src },
                    _ if float => Inst::FloatNeg { dst, src },
                    _ => Inst::Neg { dst, src },
                });
                Ok(Operand::Reg(dst))
            }

            ExprKind::BoolOp { values, .. } if values.is_empty() => {
                Ok(self.finish(Operand::Imm(0), into))
            }

            ExprKind::BoolOp { op, values } => {
                // a fresh vreg, since a later value may read the variable being assigned
                let result = self.fresh();
                let end = self.new_block();
                for (i, value) in values.iter().enumerate() {
                    self.expr_into(value, Some(result))?;
                    if i + 1 < values.len() {
                        let next = self.new_block();
                        let (then, otherwise) = match op {
                            BoolOp::And => (next, end),
                            BoolOp::Or => (end, next),
                        };
                        self.terminate(Terminator::Branch {
                            cond: Operand::Reg(result),
                            then,
                            otherwise,
                        });
                        self.start(next);
                    }
                }
                self.start(end);
                Ok(self.finish(Operand::Reg(result), into))
            }

            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => self.compare(left, ops, comparators, into),

            ExprKind::Call { func, args } => {
                if let ExprKind::Attribute { value, attr, .. } = &func.kind {
                    return self.method(value, attr, args, into);
                }
                let ExprKind::Name { id, .. } = &func.kind else {
                    return Err("only functions can be called, by name".to_string());
                };
                self.call(id, args, into)
            }

            ExprKind::Attribute { attr, .. } => {
                Err(format!("'{attr}' is a method, so it can only be called"))
            }

            ExprKind::Subscript { value, slice, .. } => {
                let list = self.expr(value)?;
                let index = self.expr(slice)?;
                let dst = self.dst(into);
                self.push(Inst::ListLoad { dst, list, index });
                Ok(Operand::Reg(dst))
            }

            ExprKind::List { elts, .. } => {
                let mut values = Vec::new();
                for elt in elts {
                    values.push(self.expr(elt)?);
                }
                // the elements are already evaluated, so only one that is the target itself
                // needs the list built elsewhere
                let list = match into {
                    Some(dst) if !values.contains(&Operand::Reg(dst)) => dst,
                    _ => self.fresh(),
                };
                self.push(Inst::Call {
                    dst: Some(list),
                    callee: Callee::Runtime("stone.list_new"),
                    args: vec![Operand::Imm(values.len() as i64)],
                });
                for (index, value) in values.into_iter().enumerate() {
                    self.push(Inst::ListInit {
                        list: Operand::Reg(list),
                        index,
                        value,
                    });
                }
                Ok(self.finish(Operand::Reg(list), into))
            }
        }
    }

    /// Lowers a comparison used as a value, which is 1 or 0.
    fn compare(
        &mut self,
        left: &Expr,
        ops: &[crate::ast::CompOp],
        comparators: &[Expr],
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        // a single link can write its destination directly, but a chain writes it more than once
        let result = if ops.len() == 1 {
            self.dst(into)
        } else {
            self.fresh()
        };
        let end = (ops.len() > 1).then(|| self.new_block());
        let mut lhs = self.expr(left)?;
        for (i, (op, comparator)) in ops.iter().zip(comparators).enumerate() {
            let rhs = self.expr(comparator)?;
            let cond = Cond::from(op);
            match self.type_of(comparator)? {
                // strings compare by contents, and only with == and !=
                Type::Str if cond == Cond::Ne => {
                    let equal = self.fresh();
                    self.push(Inst::Call {
                        dst: Some(equal),
                        callee: Callee::Runtime("stone.str_eq"),
                        args: vec![lhs, rhs],
                    });
                    self.push(Inst::Not {
                        dst: result,
                        src: Operand::Reg(equal),
                    });
                }
                Type::Str => self.push(Inst::Call {
                    dst: Some(result),
                    callee: Callee::Runtime("stone.str_eq"),
                    args: vec![lhs, rhs],
                }),
                ty => self.push(Inst::Compare {
                    cond,
                    float: ty == Type::Float,
                    dst: result,
                    lhs,
                    rhs,
                }),
            }
            if let Some(end) = end
                && i + 1 < ops.len()
            {
                let next = self.new_block();
                self.terminate(Terminator::Branch {
                    cond: Operand::Reg(result),
                    then: next,
                    otherwise: end,
                });
                self.start(next);
            }
            lhs = rhs;
        }
        if let Some(end) = end {
            self.start(end);
        }
        Ok(self.finish(Operand::Reg(result), into))
    }

    /// Lowers the method call `receiver.name(args)`, evaluating the receiver before the arguments
    /// like the interpreter.
    ///
    /// For example, `xs.len()` on a list becomes `v1 = len v0`, and on a string it calls
    /// `stone.str_len`.
    fn method(
        &mut self,
        receiver: &Expr,
        name: &str,
        args: &[Expr],
        into: Option<VReg>,
    ) -> Result<Operand, String> {
        match name {
            "len" if matches!(self.type_of(receiver)?, Type::List(_)) => {
                let list = self.expr(receiver)?;
                let dst = self.dst(into);
                self.push(Inst::ListLen { dst, list });
                Ok(Operand::Reg(dst))
            }
            "len" => {
                let text = self.expr(receiver)?;
                let dst = self.dst(into);
                self.push(Inst::Call {
                    dst: Some(dst),
                    callee: Callee::Runtime("stone.str_len"),
                    args: vec![text],
                });
                Ok(Operand::Reg(dst))
            }
            "append" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.list_append"),
                    args: values,
                });
                // append returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            _ => Err(format!("there is no method '{name}'")),
        }
    }

    /// Lowers a call to a builtin or a stone function.
    fn call(&mut self, name: &str, args: &[Expr], into: Option<VReg>) -> Result<Operand, String> {
        match name {
            "print" => {
                // every argument is evaluated before anything prints, like the interpreter
                let mut values = Vec::new();
                for arg in args {
                    values.push((self.expr(arg)?, self.type_of(arg)?));
                }
                let count = values.len();
                for (i, (value, ty)) in values.into_iter().enumerate() {
                    self.push(Inst::Call {
                        dst: None,
                        callee: Callee::Print(ty),
                        args: vec![value],
                    });
                    if i + 1 < count {
                        self.print_char(' ');
                    }
                }
                self.print_char('\n');
                // print returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "float" | "int" => {
                let from = self.type_of(&args[0])?;
                let src = self.expr(&args[0])?;
                let converts = match name {
                    "float" => from == Type::Int,
                    _ => from == Type::Float,
                };
                if !converts {
                    return Ok(self.finish(src, into));
                }
                let dst = self.dst(into);
                self.push(match name {
                    "float" => Inst::IntToFloat { dst, src },
                    _ => Inst::FloatToInt { dst, src },
                });
                Ok(Operand::Reg(dst))
            }
            _ => {
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let dst = self.dst(into);
                self.push(Inst::Call {
                    dst: Some(dst),
                    callee: Callee::User(name.to_string()),
                    args: values,
                });
                Ok(Operand::Reg(dst))
            }
        }
    }

    fn print_char(&mut self, c: char) {
        self.push(Inst::Call {
            dst: None,
            callee: Callee::Runtime("stone.print_char"),
            args: vec![Operand::Imm(c as i64)],
        });
    }
}

/// Returns the value of `expr` if it is an int literal, possibly negated.
///
/// For example, `8` gives `Some(8)`, `-4` gives `Some(-4)`, and `x` or `2 + 2` gives `None`.
pub fn constant_int(expr: &Expr) -> Option<i64> {
    match &expr.kind {
        ExprKind::Constant { value, .. } => match **value {
            Constant::Int(n) => Some(n),
            _ => None,
        },
        ExprKind::UnaryOp {
            op: UnaryOp::UnarySub,
            operand,
        } => constant_int(operand).map(i64::wrapping_neg),
        _ => None,
    }
}
