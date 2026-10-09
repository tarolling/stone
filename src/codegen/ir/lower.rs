//! Lowers a checked AST to [`ir`](super) functions.
//!
//! Names resolve the way the checker resolves them: a function's parameters and assigned names
//! are its locals, each one vreg, and every name assigned at the top level is a global that lives
//! in memory. Evaluation is left to right, like the interpreter, so runtime errors happen in the
//! same order. For example, `x = x + 1` in a function lowers to `v0 = add v0, 1`.
//!
//! Strings and lists are counted (see [`RcKind`]), and lowering keeps every count exact:
//!
//! - every variable and list slot owns one reference to the value it holds
//! - an expression's value is either borrowed, like a variable's or a literal's, or a temporary
//!   that owns a reference, like a call's result or a list element (which is retained when read,
//!   since a later call could replace it in the list)
//! - whatever uses a temporary releases it right after, and storing a value moves a temporary's
//!   reference or retains a borrowed value
//! - a parameter is borrowed from the caller, unless the function assigns it, which retains it
//! - returning releases every local, and every list a `for` loop is walking
//! - the end of `main` releases every global, so nothing is left when the program ends
//!
//! Globals can be borrowed because only top-level statements assign them, and a statement's
//! assignment happens after everything it evaluates. For example, `s = s + "x"` in a function
//! lowers to `v2 = call stone.str_concat(v0, v1)`, then `release_str v0`, then `v0 = copy v2`.

#[cfg(test)]
mod tests;

use super::{
    BinOp, Block, BlockId, Callee, Cond, Function, Inst, Operand, Program, RcKind, Terminator, VReg,
};
use crate::ast::{Arg, BoolOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp};
use crate::checker::{Symbol, SymbolKind, Type, collect_assigned, range_args};
use crate::codegen::context::os_routine;
use crate::span::Span;
use std::collections::{HashMap, HashSet};

/// The type of each variable, keyed by the function it belongs to (`None` for globals) and name.
type Variables = HashMap<(Option<String>, String), Type>;

/// Lowers every function in `module`, then its top-level code as `main`, using the types the
/// checker inferred for every expression and for every variable in `symbols`.
///
/// Fails only on input the checker rejects, such as a constant type no backend supports.
pub fn lower(
    module: &Mod,
    types: &HashMap<Span, Type>,
    symbols: &[Symbol],
) -> Result<Program, String> {
    let Mod::Module { body } = module;

    let mut assigned = Vec::new();
    collect_assigned(body, &mut assigned);
    let globals: Vec<String> = assigned.into_iter().map(|(name, _)| name).collect();

    let variables: Variables = symbols
        .iter()
        .filter(|symbol| symbol.kind != SymbolKind::Function)
        .map(|symbol| {
            (
                (symbol.scope.clone(), symbol.name.clone()),
                symbol.ty.clone(),
            )
        })
        .collect();
    let context = Context {
        types,
        variables: &variables,
        globals: &globals,
        global_set: globals.iter().cloned().collect(),
    };

    let mut functions = Vec::new();
    let mut top_level = Vec::new();
    for stmt in body {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                let lowerer = Lowerer::new(&context, Some(name.clone()));
                functions.push(lowerer.function(&args.args, body)?);
            }
            _ => top_level.push(stmt.clone()),
        }
    }
    functions.push(Lowerer::new(&context, None).function(&[], &top_level)?);

    Ok(Program { functions, globals })
}

/// What lowering each function shares.
struct Context<'a> {
    types: &'a HashMap<Span, Type>,
    variables: &'a Variables,
    /// Every global, in the order they are first assigned.
    globals: &'a [String],
    global_set: HashSet<String>,
}

/// What a `for` loop walks: a range up to an end, or a list.
#[derive(Clone, Copy)]
enum Iterable {
    Range(Operand),
    List(Operand),
}

/// A loop that `break` and `cont` can leave.
struct Loop {
    exit: BlockId,
    header: BlockId,
    /// The list a `for` loop walks, which it holds a reference to until it ends.
    list: Option<Operand>,
}

/// A block being built, which may not have a terminator yet.
struct PendingBlock {
    insts: Vec<Inst>,
    term: Option<Terminator>,
    loop_depth: u32,
}

/// Lowers one function body.
struct Lowerer<'a> {
    context: &'a Context<'a>,
    name: Option<String>,
    /// Each local's vreg. Empty in `main`, where assigned names are globals.
    locals: HashMap<String, VReg>,
    /// The locals that own a reference, which are released when the function returns.
    counted_locals: Vec<(VReg, RcKind)>,
    /// Temporaries that own a reference, which whatever uses them must give away or release.
    owned: HashMap<VReg, RcKind>,
    vreg_count: u32,
    blocks: Vec<PendingBlock>,
    /// Block ids in the order they were started, which becomes the layout order.
    order: Vec<BlockId>,
    /// The block instructions go to, or `None` after a terminator, until the next block starts.
    current: Option<BlockId>,
    /// Each enclosing loop, innermost last.
    loops: Vec<Loop>,
    loop_depth: u32,
}

impl<'a> Lowerer<'a> {
    fn new(context: &'a Context<'a>, name: Option<String>) -> Self {
        Lowerer {
            context,
            name,
            locals: HashMap::new(),
            counted_locals: Vec::new(),
            owned: HashMap::new(),
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
        let mut assigned = Vec::new();
        if self.name.is_some() {
            for arg in args {
                let reg = self.local(&arg.arg);
                params.push(reg);
            }
            collect_assigned(body, &mut assigned);
            for (name, _) in &assigned {
                self.local(name);
            }
        }

        let entry = self.new_block();
        self.start(entry);
        // a parameter the function assigns needs a reference of its own to release, and other
        // counted locals start null, since they may not be assigned on every path to a return
        let names = args
            .iter()
            .map(|arg| &arg.arg)
            .chain(assigned.iter().map(|(name, _)| name));
        let mut seen = HashSet::new();
        for name in names {
            let reg = self.locals[name];
            let Some(kind) = self.variable_kind(name) else {
                continue;
            };
            if !seen.insert(reg) {
                continue;
            }
            if !params.contains(&reg) {
                self.push(Inst::Copy {
                    dst: reg,
                    src: Operand::Imm(0),
                });
            } else if assigned.iter().any(|(assigned, _)| assigned == name) {
                self.push(Inst::Retain {
                    src: Operand::Reg(reg),
                });
            } else {
                continue;
            }
            self.counted_locals.push((reg, kind));
        }

        for stmt in body {
            self.stmt(stmt)?;
        }
        // falling off the end returns none, and main exits with 0
        if self.current.is_some() {
            self.leave(Operand::Imm(0));
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

    /// Returns how variable `name` of this function, or global `name` in `main`, is counted.
    fn variable_kind(&self, name: &str) -> Option<RcKind> {
        self.context
            .variables
            .get(&(self.name.clone(), name.to_string()))
            .and_then(RcKind::of)
    }

    /// Returns how global `name` is counted.
    fn global_kind(&self, name: &str) -> Option<RcKind> {
        self.context
            .variables
            .get(&(None, name.to_string()))
            .and_then(RcKind::of)
    }

    /// Returns how the value of `expr` is counted.
    fn counted(&self, expr: &Expr) -> Option<RcKind> {
        self.context.types.get(&expr.span).and_then(RcKind::of)
    }

    /// Returns how the elements of the list `expr` are counted.
    fn element_kind(&self, expr: &Expr) -> Option<RcKind> {
        match self.context.types.get(&expr.span) {
            Some(Type::List(elem)) => RcKind::of(elem),
            _ => None,
        }
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
    /// result is. A temporary's reference moves with it.
    fn finish(&mut self, value: Operand, into: Option<VReg>) -> Operand {
        match into {
            Some(dst) => {
                if value != Operand::Reg(dst) {
                    self.push(Inst::Copy { dst, src: value });
                    if let Operand::Reg(src) = value
                        && let Some(kind) = self.owned.remove(&src)
                    {
                        self.owned.insert(dst, kind);
                    }
                }
                Operand::Reg(dst)
            }
            None => value,
        }
    }

    /// Records that `value`, a fresh result, owns a reference if it is counted as `kind`.
    fn own(&mut self, value: Operand, kind: Option<RcKind>) {
        if let (Operand::Reg(reg), Some(kind)) = (value, kind) {
            self.owned.insert(reg, kind);
        }
    }

    /// Releases `value` if it is a temporary that owns a reference, once nothing needs it.
    fn release_temp(&mut self, value: Operand) {
        if let Operand::Reg(reg) = value
            && let Some(kind) = self.owned.remove(&reg)
        {
            self.push(Inst::Release { src: value, kind });
        }
    }

    /// Makes `value`, counted as `kind`, carry a reference for whatever stores it: a temporary
    /// gives up its own, and anything else is retained.
    fn take(&mut self, value: Operand, kind: Option<RcKind>) {
        if kind.is_none() {
            return;
        }
        if let Operand::Reg(reg) = value
            && self.owned.remove(&reg).is_some()
        {
            return;
        }
        self.push(Inst::Retain { src: value });
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
        self.context
            .types
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

    /// Returns `value` from the function, first releasing the lists of the loops it is in and
    /// the references its locals own, except one that `value` is, which the caller takes. `main`
    /// releases every global instead and exits with 0.
    fn leave(&mut self, value: Operand) {
        let lists: Vec<Operand> = self.loops.iter().rev().filter_map(|l| l.list).collect();
        for list in lists {
            self.push(Inst::Release {
                src: list,
                kind: RcKind::List,
            });
        }
        if self.name.is_some() {
            for (reg, kind) in self.counted_locals.clone() {
                if value == Operand::Reg(reg) {
                    continue;
                }
                self.push(Inst::Release {
                    src: Operand::Reg(reg),
                    kind,
                });
            }
            self.terminate(Terminator::Return(Some(value)));
            return;
        }
        for name in self.context.globals {
            let Some(kind) = self.global_kind(name) else {
                continue;
            };
            let global = self.fresh();
            self.push(Inst::LoadGlobal {
                dst: global,
                name: name.clone(),
                checked: false,
            });
            self.push(Inst::Release {
                src: Operand::Reg(global),
                kind,
            });
        }
        self.terminate(Terminator::Return(Some(Operand::Imm(0))));
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
                let kind = self.counted(value);
                if kind.is_none()
                    && let [target] = targets.as_slice()
                    && let ExprKind::Name { id, .. } = &target.kind
                    && let Some(&local) = self.locals.get(id)
                {
                    self.expr_into(value, Some(local))?;
                    return Ok(());
                }
                let value = self.expr(value)?;
                // every target gets a reference of its own, the last one the temporary's
                for (i, target) in targets.iter().enumerate() {
                    if i + 1 < targets.len() {
                        if kind.is_some() {
                            self.push(Inst::Retain { src: value });
                        }
                    } else {
                        self.take(value, kind);
                    }
                    self.assign(target, value, kind)?;
                }
            }

            StmtKind::Expr { value } => {
                let value = self.expr(value)?;
                self.release_temp(value);
            }

            StmtKind::Return { value } => {
                let value = match value {
                    // `ret` at the top level ends the program normally
                    Some(value) if self.name.is_none() => {
                        let result = self.expr(value)?;
                        self.release_temp(result);
                        Operand::Imm(0)
                    }
                    Some(value) => {
                        let result = self.expr(value)?;
                        // a counted local gives its own reference to the caller
                        if !self
                            .counted_locals
                            .iter()
                            .any(|(reg, _)| result == Operand::Reg(*reg))
                        {
                            self.take(result, self.counted(value));
                        }
                        result
                    }
                    None => Operand::Imm(0),
                };
                self.leave(value);
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
                self.loops.push(Loop {
                    exit,
                    header,
                    list: None,
                });
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
                if let Some(exit) = self.loops.last().map(|l| l.exit) {
                    self.jump(exit);
                }
            }

            StmtKind::Continue => {
                if let Some(header) = self.loops.last().map(|l| l.header) {
                    self.jump(header);
                }
            }

            StmtKind::FunctionDef { .. } => {
                return Err("functions must be defined at the top level".to_string());
            }
            StmtKind::Use { .. } => {
                return Err("use is only allowed at the top level of a file".to_string());
            }
        }
        Ok(())
    }

    /// Stores `value` in an assignment target: a local, a global, or a list element. A value
    /// counted as `kind` already carries the reference the target takes, and the target's old
    /// value is released once it is replaced.
    fn assign(
        &mut self,
        target: &Expr,
        value: Operand,
        kind: Option<RcKind>,
    ) -> Result<(), String> {
        match &target.kind {
            ExprKind::Name { id, .. } => {
                if let Some(&local) = self.locals.get(id) {
                    if let Some(kind) = kind {
                        self.push(Inst::Release {
                            src: Operand::Reg(local),
                            kind,
                        });
                    }
                    if value != Operand::Reg(local) {
                        self.push(Inst::Copy {
                            dst: local,
                            src: value,
                        });
                    }
                } else if self.context.global_set.contains(id) {
                    self.store_global(id, value, kind);
                }
            }
            ExprKind::Subscript {
                value: list, slice, ..
            } => {
                // the value was evaluated first, like the interpreter
                let list = self.list_operand(list, &[slice])?;
                let index = self.expr(slice)?;
                let old = kind.map(|_| self.fresh());
                self.push(Inst::ListStore {
                    old,
                    list,
                    index,
                    value,
                });
                if let (Some(old), Some(kind)) = (old, kind) {
                    self.push(Inst::Release {
                        src: Operand::Reg(old),
                        kind,
                    });
                }
                self.release_temp(list);
            }
            _ => {}
        }
        Ok(())
    }

    /// Writes global `name`, releasing its old value if it is counted as `kind`.
    fn store_global(&mut self, name: &str, value: Operand, kind: Option<RcKind>) {
        let old = kind.map(|_| self.fresh());
        if let Some(old) = old {
            self.push(Inst::LoadGlobal {
                dst: old,
                name: name.to_string(),
                checked: false,
            });
        }
        self.push(Inst::StoreGlobal {
            name: name.to_string(),
            src: value,
        });
        if let (Some(old), Some(kind)) = (old, kind) {
            self.push(Inst::Release {
                src: Operand::Reg(old),
                kind,
            });
        }
    }

    /// Lowers `for target in iter;`, counting with hidden vregs that the body cannot assign, so
    /// assigning the loop variable never changes the iteration.
    ///
    /// A range's bounds are evaluated once, left to right. A list's length is reread every
    /// iteration, so a list that grows while it is iterated keeps going, like in the interpreter.
    /// The loop holds a reference to the list until it ends, so reassigning the variable it came
    /// from cannot free it.
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
                self.take(list, Some(RcKind::List));
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                Iterable::List(list)
            }
        };
        let (list, elem) = match iterable {
            Iterable::List(list) => (Some(list), self.element_kind(iter)),
            Iterable::Range(_) => (None, None),
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
                // a counted element is retained for the variable, which releases its old value
                let dst = match elem {
                    Some(_) => self.fresh(),
                    None => self.dst(local),
                };
                self.push(Inst::ListGet {
                    dst,
                    list,
                    index: Operand::Reg(next),
                });
                if elem.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                }
                Operand::Reg(dst)
            }
            Iterable::Range(_) => Operand::Reg(next),
        };
        self.assign(target, value, elem)?;
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: Operand::Imm(1),
        });

        self.loops.push(Loop { exit, header, list });
        for stmt in body {
            self.stmt(stmt)?;
        }
        self.loops.pop();
        if self.current.is_some() {
            self.jump(header);
        }
        self.loop_depth -= 1;
        self.start(exit);
        if let Some(list) = list {
            self.push(Inst::Release {
                src: list,
                kind: RcKind::List,
            });
        }
        Ok(())
    }

    /// Lowers a test, going to `then` if it is truthy and to `otherwise` if not, and ends the
    /// current block.
    ///
    /// Comparisons branch on the flags directly, and `and`, `or`, and `not` become branches, so
    /// `if a < b and c;` never materializes a bool.
    fn cond(&mut self, test: &Expr, then: BlockId, otherwise: BlockId) -> Result<(), String> {
        match &test.kind {
            // strings are compared by a call, which compare handles with their references
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
    ///
    /// A counted value comes back borrowed, or as a temporary in [`Lowerer::owned`] that the
    /// caller must release or give away.
    fn expr_into(&mut self, expr: &Expr, into: Option<VReg>) -> Result<Operand, String> {
        let kind = self.counted(expr);
        if into.is_some() && kind.is_some() {
            // computed apart, so the destination's old value is still there to release
            let value = self.expr(expr)?;
            return Ok(self.finish(value, into));
        }
        match &expr.kind {
            ExprKind::Constant { value, .. } => {
                let value = match &**value {
                    Constant::Int(n) => Operand::Imm(*n),
                    Constant::Float(x) => Operand::Imm(x.to_bits() as i64),
                    Constant::Bool(b) => Operand::Imm(*b as i64),
                    Constant::None => Operand::Imm(0),
                    Constant::Str(s) => {
                        // literals are never freed, so they are borrowed
                        let dst = self.fresh();
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
                if self.context.global_set.contains(id) {
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
                self.release_temp(lhs);
                self.release_temp(rhs);
                self.own(Operand::Reg(dst), kind);
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
                    return self.method(value, attr, args, into, kind);
                }
                let ExprKind::Name { id, .. } = &func.kind else {
                    return Err("only functions can be called, by name".to_string());
                };
                self.call(id, args, into, kind)
            }

            ExprKind::Attribute { attr, .. } => {
                Err(format!("'{attr}' is a method, so it can only be called"))
            }

            ExprKind::Subscript { value, slice, .. } => {
                let list = self.list_operand(value, &[slice])?;
                let index = self.expr(slice)?;
                let dst = self.dst(into);
                self.push(Inst::ListLoad { dst, list, index });
                // a later call could replace the element in the list, so it gets a reference
                if kind.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                    self.own(Operand::Reg(dst), kind);
                }
                self.release_temp(list);
                Ok(Operand::Reg(dst))
            }

            ExprKind::List { elts, .. } => {
                let elem = self.element_kind(expr);
                let mut values = Vec::new();
                for elt in elts {
                    values.push(self.expr(elt)?);
                }
                for value in &values {
                    self.take(*value, elem);
                }
                // the list's elem field, as `builtins::list_runtime` describes it
                let code = match elem {
                    None => 0,
                    Some(RcKind::Str) => 1,
                    Some(RcKind::List) => 2,
                };
                let list = self.fresh();
                self.push(Inst::Call {
                    dst: Some(list),
                    callee: Callee::Runtime("stone.list_new"),
                    args: vec![Operand::Imm(values.len() as i64), Operand::Imm(code)],
                });
                for (index, value) in values.into_iter().enumerate() {
                    self.push(Inst::ListInit {
                        list: Operand::Reg(list),
                        index,
                        value,
                    });
                }
                self.own(Operand::Reg(list), kind);
                Ok(Operand::Reg(list))
            }
        }
    }

    /// Lowers `list`, the list an operation reads once `before` are evaluated.
    ///
    /// An element of another list is borrowed rather than retained when `before` makes no calls,
    /// since nothing else can change a list until the operation reads it, so `a[i][k]` adds no
    /// reference to the row `a[i]`. The list it came from must be borrowed too, since releasing
    /// a temporary like the result of `f()` in `f()[0][1]` could free the row with it.
    fn list_operand(&mut self, list: &Expr, before: &[&Expr]) -> Result<Operand, String> {
        let ExprKind::Subscript { value, slice, .. } = &list.kind else {
            return self.expr(list);
        };
        if before.iter().any(|expr| has_call(expr)) {
            return self.expr(list);
        }
        let outer = self.list_operand(value, &[slice])?;
        let index = self.expr(slice)?;
        let dst = self.fresh();
        self.push(Inst::ListLoad {
            dst,
            list: outer,
            index,
        });
        if matches!(outer, Operand::Reg(reg) if self.owned.contains_key(&reg)) {
            self.push(Inst::Retain {
                src: Operand::Reg(dst),
            });
            self.own(Operand::Reg(dst), Some(RcKind::List));
            self.release_temp(outer);
        }
        Ok(Operand::Reg(dst))
    }

    /// Lowers a comparison used as a value, which is 1 or 0. Each counted operand is released
    /// once the links that read it are done, including when a chain stops early.
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
            self.release_temp(lhs);
            match end {
                Some(end) if i + 1 < ops.len() => {
                    // the next link reads rhs, so stopping here releases it on the way out
                    let owned = match rhs {
                        Operand::Reg(reg) => self.owned.get(&reg).copied(),
                        Operand::Imm(_) => None,
                    };
                    let next = self.new_block();
                    let stop = match owned {
                        Some(_) => self.new_block(),
                        None => end,
                    };
                    self.terminate(Terminator::Branch {
                        cond: Operand::Reg(result),
                        then: next,
                        otherwise: stop,
                    });
                    if let Some(kind) = owned {
                        self.start(stop);
                        self.push(Inst::Release { src: rhs, kind });
                        self.jump(end);
                    }
                    self.start(next);
                }
                _ => self.release_temp(rhs),
            }
            lhs = rhs;
        }
        if let Some(end) = end {
            self.start(end);
        }
        Ok(self.finish(Operand::Reg(result), into))
    }

    /// Lowers the method call `receiver.name(args)`, evaluating the receiver before the arguments
    /// like the interpreter. Its result is counted as `kind`.
    ///
    /// For example, `xs.len()` on a list becomes `v1 = len v0`, and on a string it calls
    /// `stone.str_len`.
    fn method(
        &mut self,
        receiver: &Expr,
        name: &str,
        args: &[Expr],
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        match name {
            "len" if matches!(self.type_of(receiver)?, Type::List(_)) => {
                let list = self.list_operand(receiver, &[])?;
                let dst = self.dst(into);
                self.push(Inst::ListLen { dst, list });
                self.release_temp(list);
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
                self.release_temp(text);
                Ok(Operand::Reg(dst))
            }
            "append" => {
                let elem = self.element_kind(receiver);
                let list = self.list_operand(receiver, &args.iter().collect::<Vec<_>>())?;
                let mut values = vec![list];
                for arg in args {
                    let value = self.expr(arg)?;
                    // the list keeps a reference to what it holds
                    self.take(value, elem);
                    values.push(value);
                }
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.list_append"),
                    args: values,
                });
                self.release_temp(list);
                // append returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "strip" | "split" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let label = match (name, values.len()) {
                    ("strip", _) => "stone.str_strip",
                    (_, 1) => "stone.str_split_ws",
                    _ => "stone.str_split",
                };
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
            }
            _ => Err(format!("there is no method '{name}'")),
        }
    }

    /// Calls the runtime routine `label` with `args`, returning its result.
    fn runtime_call(
        &mut self,
        label: &'static str,
        args: Vec<Operand>,
        into: Option<VReg>,
    ) -> Operand {
        let dst = self.dst(into);
        self.push(Inst::Call {
            dst: Some(dst),
            callee: Callee::Runtime(label),
            args,
        });
        Operand::Reg(dst)
    }

    /// Lowers a call to a builtin or a stone function, whose result is counted as `kind`. The
    /// call borrows its arguments, and temporaries among them are released after it.
    fn call(
        &mut self,
        name: &str,
        args: &[Expr],
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        match name {
            "print" => {
                // every argument is evaluated before anything prints, like the interpreter
                let mut values = Vec::new();
                for arg in args {
                    values.push((self.expr(arg)?, self.type_of(arg)?));
                }
                let count = values.len();
                for (i, (value, ty)) in values.iter().cloned().enumerate() {
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
                for (value, _) in values {
                    self.release_temp(value);
                }
                // print returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "float" | "int" | "str" => {
                let from = self.type_of(&args[0])?;
                let src = self.expr(&args[0])?;
                let runtime = match (name, &from) {
                    ("int", Type::Str) => Some("stone.parse_int"),
                    ("float", Type::Str) => Some("stone.parse_float"),
                    ("str", Type::Int) => Some("stone.str_int"),
                    ("str", Type::Float) => Some("stone.str_float"),
                    ("str", Type::Bool) => Some("stone.str_bool"),
                    // strings never change, so str of a str can share it
                    ("str", _) => return Ok(self.finish(src, into)),
                    _ => None,
                };
                if let Some(label) = runtime {
                    let result = self.runtime_call(label, vec![src], into);
                    self.release_temp(src);
                    self.own(result, kind);
                    return Ok(result);
                }
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
            "input" => {
                // no prompt is a null pointer
                let prompt = match args {
                    [prompt] => self.expr(prompt)?,
                    _ => Operand::Imm(0),
                };
                let result = self.runtime_call("stone.input", vec![prompt], into);
                self.release_temp(prompt);
                self.own(result, kind);
                Ok(result)
            }
            "eof" => Ok(self.runtime_call("stone.eof", vec![], into)),
            "args" => {
                let result = self.runtime_call("stone.args", vec![], into);
                self.own(result, kind);
                Ok(result)
            }
            "os.exit" => {
                let code = self.expr(&args[0])?;
                // it never returns, but the call is a statement like any other
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.os_exit"),
                    args: vec![code],
                });
                Ok(self.finish(Operand::Imm(0), into))
            }
            _ if let Some(label) = os_routine(name) => {
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let result = self.runtime_call(label, values.clone(), into);
                for value in values {
                    self.release_temp(value);
                }
                self.own(result, kind);
                Ok(result)
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
                    args: values.clone(),
                });
                for value in values {
                    self.release_temp(value);
                }
                self.own(Operand::Reg(dst), kind);
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

/// Returns whether evaluating `expr` can call a function or method, which is the only way code
/// can change a list while an expression is being evaluated.
///
/// For example, `i + 1` and `a[i]` make no calls, but `f(i)` and `xs.len()` do.
fn has_call(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Call { .. } => true,
        ExprKind::Constant { .. } | ExprKind::Name { .. } => false,
        ExprKind::BinOp { left, right, .. } => has_call(left) || has_call(right),
        ExprKind::UnaryOp { operand, .. } => has_call(operand),
        ExprKind::BoolOp { values, .. } | ExprKind::List { elts: values, .. } => {
            values.iter().any(has_call)
        }
        ExprKind::Compare {
            left, comparators, ..
        } => has_call(left) || comparators.iter().any(has_call),
        ExprKind::Subscript { value, slice, .. } => has_call(value) || has_call(slice),
        ExprKind::Attribute { value, .. } => has_call(value),
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
