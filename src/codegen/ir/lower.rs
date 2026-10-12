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
//! - a parameter is borrowed from the caller, unless the function assigns or changes it: then
//!   the function owns it, and the caller gives it a reference, a temporary's own, a retained
//!   one, or a variable's own when nothing reads the variable after the call (`xs = f(xs)` or
//!   `ret f(xs)`), which moves the value in so the function can change it in place
//! - lists are values, so before a statement changes one (`xs[i] = v` or `xs.append(v)`), each
//!   list from the variable to it is made unique (`list_unique`): copied if anything else refers
//!   to it, so the change shows through no other variable
//! - returning releases every local, and every list a `for` loop is walking
//! - the end of `main` releases every global, so nothing is left when the program ends
//!
//! Globals can be borrowed because only top-level statements assign them, and a statement's
//! assignment happens after everything it evaluates. For example, `s = s + "x"` in a function
//! lowers to `v2 = call stone.str_concat(v0, v1)`, then `release_str v0`, then `v0 = copy v2`.

#[cfg(test)]
mod tests;

use super::{
    BinOp, Block, BlockId, Callee, Cond, Function, Inst, Kernel, Operand, Program, RcKind,
    Terminator, VReg,
};
use crate::ast::{Arg, BoolOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp};
use crate::checker::{
    Symbol, SymbolKind, Type, collect_assigned, collect_changed, place, place_spans, range_args,
};
use crate::codegen::context::module_routine;
use crate::last_use::{LiveAfter, global_reads, live_after};
use crate::span::Span;
use crate::stdlib::{
    INDEX_RANGE, LENGTHS_DIFFER, POP_EMPTY, POP_RANGE, SLICE_STEP_ZERO, STRING_INDEX_RANGE, math,
    random,
};
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
    let global_set: HashSet<String> = globals.iter().cloned().collect();
    let context = Context {
        types,
        variables: &variables,
        globals: &globals,
        owned_params: owned_params(body),
        global_reads: global_reads(body, &global_set),
        global_set,
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
    /// For each function, whether it owns each parameter, which it does if it assigns or changes
    /// it. A caller gives an owned parameter a reference of its own.
    owned_params: HashMap<String, Vec<bool>>,
    /// For each function, the globals it or any function it calls may read, which a caller must
    /// not move into it.
    global_reads: HashMap<String, HashSet<String>>,
}

/// What a `for` loop walks: a range up to an end, or a list.
#[derive(Clone, Copy)]
enum Iterable {
    Range(Operand),
    List(Operand),
    /// A string and its length, which cannot change while the loop holds it.
    Str(Operand, Operand),
}

/// A loop that `break` and `cont` can leave.
struct Loop {
    exit: BlockId,
    header: BlockId,
    /// The list or string a `for` loop walks, which it holds a reference to until it ends.
    held: Option<(Operand, RcKind)>,
}

/// A block being built, which may not have a terminator yet.
struct PendingBlock {
    insts: Vec<Inst>,
    spans: Vec<Span>,
    term: Option<Terminator>,
    term_span: Span,
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
    /// The variables whose value the next call to a stone function, or elementwise arithmetic,
    /// may move into it, since nothing reads them after the call: the target of `x = f(...)`, or
    /// every counted local for `ret f(...)`.
    movable: Vec<String>,
    /// The locals and globals a call moved, whose old value the assignment or return that follows
    /// must not release.
    moved_locals: HashSet<VReg>,
    moved_globals: HashSet<String>,
    /// The variables still read after each statement of the body being lowered.
    live_after: LiveAfter,
    /// The variables the expression statement being lowered uses for the last time, which an
    /// `append` it makes may move into the list.
    statement_last_uses: Vec<String>,
    /// The innermost statement or expression being lowered, which each instruction records as
    /// where a runtime error at it points.
    span: Span,
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
            movable: Vec::new(),
            moved_locals: HashSet::new(),
            moved_globals: HashSet::new(),
            live_after: LiveAfter::new(),
            statement_last_uses: Vec::new(),
            span: Span::default(),
        }
    }

    fn function(mut self, args: &[Arg], body: &[Stmt]) -> Result<Function, String> {
        let mut params = Vec::new();
        let mut assigned = Vec::new();
        let mut changed = HashSet::new();
        if self.name.is_some() {
            for arg in args {
                let reg = self.local(&arg.arg);
                params.push(reg);
            }
            collect_assigned(body, &mut assigned);
            collect_changed(body, &mut changed);
            for (name, _) in &assigned {
                self.local(name);
            }
        }

        self.live_after = live_after(body, &self.context.global_reads);
        let entry = self.new_block();
        self.start(entry);
        // the caller gives a parameter the function assigns or changes a reference of its own to
        // release, and other counted locals start null, since they may not be assigned on every
        // path
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
            } else if !assigned.iter().any(|(assigned, _)| assigned == name)
                && !changed.contains(name)
            {
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
            spans: Vec::new(),
            term: None,
            term_span: Span::default(),
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
        self.blocks[block.0].spans.push(self.span);
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
        self.blocks[block.0].term_span = self.span;
    }

    fn jump(&mut self, target: BlockId) {
        self.terminate(Terminator::Jump(target));
    }

    /// Returns `value` from the function, first releasing the lists and strings of the loops it
    /// is in and the references its locals own, except one that `value` is, which the caller
    /// takes. `main` releases every global instead and exits with 0.
    fn leave(&mut self, value: Operand) {
        let held: Vec<(Operand, RcKind)> = self.loops.iter().rev().filter_map(|l| l.held).collect();
        for (src, kind) in held {
            self.push(Inst::Release { src, kind });
        }
        if self.name.is_some() {
            let moved = std::mem::take(&mut self.moved_locals);
            for (reg, kind) in self.counted_locals.clone() {
                // a local moved into a call no longer holds a reference
                if value == Operand::Reg(reg) || moved.contains(&reg) {
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
                    term @ (Terminator::Return(_) | Terminator::Fail(_)) => term,
                };
                Block {
                    insts: block.insts,
                    spans: block.spans,
                    term,
                    term_span: block.term_span,
                    loop_depth: block.loop_depth,
                }
            })
            .collect()
    }

    /// Lowers `stmt`, whose instructions point at it unless an expression inside it is more
    /// precise.
    fn stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        let outer = std::mem::replace(&mut self.span, stmt.span);
        let result = self.stmt_unspanned(stmt);
        self.span = outer;
        result
    }

    fn stmt_unspanned(&mut self, stmt: &Stmt) -> Result<(), String> {
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                let last_uses = self.last_uses(stmt);
                // a call can also take a target's old value, which the assignment replaces, but
                // not what a target's indexes read after it
                if (self.is_stone_call(value) || self.is_elementwise(value))
                    && targets
                        .iter()
                        .all(|target| matches!(target.kind, ExprKind::Name { .. }))
                {
                    self.movable = last_uses.clone();
                    for target in targets {
                        if let ExprKind::Name { id, .. } = &target.kind
                            && self.is_counted_variable(id)
                        {
                            self.movable.push(id.clone());
                        }
                    }
                }
                let kind = self.counted(value);
                if kind.is_none()
                    && let [target] = targets.as_slice()
                    && let ExprKind::Name { id, .. } = &target.kind
                    && let Some(&local) = self.locals.get(id)
                {
                    self.expr_into(value, Some(local))?;
                } else {
                    let result = self.expr(value)?;
                    // every target gets a reference of its own, the last one the temporary's
                    for (i, target) in targets.iter().enumerate() {
                        if i + 1 < targets.len() {
                            if kind.is_some() {
                                self.push(Inst::Retain { src: result });
                            }
                        } else {
                            self.take_from(value, result, kind, &last_uses);
                        }
                        self.assign(target, result, kind)?;
                    }
                }
                self.clear_moved();
            }

            StmtKind::Expr { value } => {
                let last_uses = self.last_uses(stmt);
                if self.is_stone_call(value) {
                    self.movable = last_uses.clone();
                }
                // `xs.append(row)` can take `row` at its last use
                self.statement_last_uses = last_uses;
                let result = self.expr(value)?;
                self.statement_last_uses.clear();
                self.release_temp(result);
                self.clear_moved();
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
                        // returning releases every local, so a call can take any of them
                        if self.is_stone_call(value) || self.is_elementwise(value) {
                            self.movable = self.last_uses(stmt);
                        }
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
                    held: None,
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
                    // a value moved into the call that computed the new one is not ours to release
                    if let Some(kind) = kind
                        && !self.moved_locals.remove(&local)
                    {
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
                    let kind = kind.filter(|_| !self.moved_globals.remove(id));
                    self.store_global(id, value, kind);
                }
            }
            ExprKind::Subscript {
                value: list, slice, ..
            } => {
                // the value was evaluated first, then the indexes, like the interpreter
                let indexes = self.place_indexes(list)?;
                let index = self.expr(slice)?;
                let list = self.unique_place(list, &indexes)?;
                let old = kind.map(|_| self.fresh());
                let outer = std::mem::replace(&mut self.span, target.span);
                self.push(Inst::ListStore {
                    old,
                    list,
                    index,
                    value,
                });
                self.span = outer;
                if let (Some(old), Some(kind)) = (old, kind) {
                    self.push(Inst::Release {
                        src: Operand::Reg(old),
                        kind,
                    });
                }
            }
            _ => {}
        }
        Ok(())
    }

    /// Evaluates the indexes that lead from a variable to `list`, which a statement is about to
    /// change, outermost first. For example, `grid[i][j]` evaluates `i` and `j`.
    fn place_indexes(&mut self, list: &Expr) -> Result<Vec<Operand>, String> {
        let Some((_, indexes)) = place(list) else {
            return Err("only a list in a variable can be changed".to_string());
        };
        indexes.into_iter().map(|index| self.expr(index)).collect()
    }

    /// Walks from the variable `list` starts from through `indexes`, from
    /// [`Lowerer::place_indexes`], and returns `list` once nothing else refers to it, borrowed
    /// from whatever holds it.
    ///
    /// Each list on the way is made unique and written back where it came from, since making it
    /// unique may copy it. For example, `grid[0].append(1)` in a function lowers to
    /// `v0 = list_unique v0`, `v1 = list_load v0, 0`, `v1 = list_unique v1`, and
    /// `list_store v0, 0, v1` before the append. Only `main` changes globals.
    fn unique_place(&mut self, list: &Expr, indexes: &[Operand]) -> Result<Operand, String> {
        let Some((root, _)) = place(list) else {
            return Err("only a list in a variable can be changed".to_string());
        };
        let mut current = if let Some(&local) = self.locals.get(root) {
            self.push(Inst::ListUnique {
                dst: local,
                src: Operand::Reg(local),
            });
            Operand::Reg(local)
        } else if self.name.is_none() && self.context.global_set.contains(root) {
            let loaded = self.fresh();
            self.push(Inst::LoadGlobal {
                dst: loaded,
                name: root.to_string(),
                checked: false,
            });
            let unique = self.fresh();
            self.push(Inst::ListUnique {
                dst: unique,
                src: Operand::Reg(loaded),
            });
            self.push(Inst::StoreGlobal {
                name: root.to_string(),
                src: Operand::Reg(unique),
            });
            Operand::Reg(unique)
        } else {
            return Err(format!("a function cannot change the global '{root}'"));
        };
        // an index out of range points at its own subscript
        let outer = self.span;
        for (&index, span) in indexes.iter().zip(place_spans(list)) {
            self.span = span;
            let row = self.fresh();
            self.push(Inst::ListLoad {
                dst: row,
                list: current,
                index,
            });
            self.push(Inst::ListUnique {
                dst: row,
                src: Operand::Reg(row),
            });
            // the slot's reference moves to the unique row, which may be a copy
            self.push(Inst::ListStore {
                old: None,
                list: current,
                index,
                value: Operand::Reg(row),
            });
            current = Operand::Reg(row);
        }
        self.span = outer;
        Ok(current)
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
    /// A string steps by whole characters: `stone.str_char` returns the one at the position, and
    /// its length is the step. The loop holds a reference to the list or string until it ends, so
    /// reassigning the variable it came from cannot free it.
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
            None if self.type_of(iter)? == Type::Str => {
                let text = self.expr(iter)?;
                let text = self.snapshot(text);
                self.take(text, Some(RcKind::Str));
                let len = self.runtime_call("stone.str_len", vec![text], None);
                self.push(Inst::Copy {
                    dst: next,
                    src: Operand::Imm(0),
                });
                Iterable::Str(text, len)
            }
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
        let (held, elem) = match iterable {
            Iterable::List(list) => (Some((list, RcKind::List)), self.element_kind(iter)),
            Iterable::Str(text, _) => (Some((text, RcKind::Str)), Some(RcKind::Str)),
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
            Iterable::Str(_, len) => len,
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
            Iterable::Str(..) => Operand::Reg(self.fresh()),
            Iterable::Range(_) => Operand::Reg(next),
        };
        let step = match iterable {
            Iterable::Str(text, _) => {
                // the character is a new reference, which the variable takes
                let Operand::Reg(dst) = value else {
                    unreachable!("a character is a register")
                };
                self.push(Inst::Call {
                    dst: Some(dst),
                    callee: Callee::Runtime("stone.str_char"),
                    args: vec![text, Operand::Reg(next)],
                });
                self.runtime_call("stone.str_len", vec![value], None)
            }
            _ => Operand::Imm(1),
        };
        self.assign(target, value, elem)?;
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: next,
            lhs: Operand::Reg(next),
            rhs: step,
        });

        self.loops.push(Loop { exit, header, held });
        for stmt in body {
            self.stmt(stmt)?;
        }
        self.loops.pop();
        if self.current.is_some() {
            self.jump(header);
        }
        self.loop_depth -= 1;
        self.start(exit);
        if let Some((src, kind)) = held {
            self.push(Inst::Release { src, kind });
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
        let outer = std::mem::replace(&mut self.span, expr.span);
        let result = self.expr_into_unspanned(expr, into);
        self.span = outer;
        result
    }

    fn expr_into_unspanned(&mut self, expr: &Expr, into: Option<VReg>) -> Result<Operand, String> {
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

            ExprKind::BinOp { .. } | ExprKind::UnaryOp { .. } if self.is_elementwise(expr) => {
                self.elementwise(expr, kind)
            }

            ExprKind::BinOp { op, left, right } => {
                let ty = self.type_of(expr)?;
                let lhs = self.expr(left)?;
                let rhs = self.expr(right)?;
                let dst = self.dst(into);
                let op = bin_op(*op);
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
                let string = self.type_of(value)? == Type::Str;
                if let ExprKind::Slice { lower, upper, step } = &slice.kind {
                    let label = match string {
                        true => "stone.str_slice_of",
                        false => "stone.list_slice",
                    };
                    return self.slice(value, [lower, upper, step], label, into, kind);
                }
                if string {
                    return self.string_index(value, slice, into, kind);
                }
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

            ExprKind::Slice { .. } => Err("a slice can only appear in brackets".to_string()),
        }
    }

    /// Lowers arithmetic on lists to one loop over their elements, with no list for any
    /// intermediate result and no index checks, as the interpreter's `elementwise` defines it:
    /// every operand that is not itself arithmetic on lists is evaluated once, left to right, then
    /// the lists' lengths are checked, then the whole expression is computed for each element in
    /// turn.
    ///
    /// The result goes in a new list, unless an operand is a variable in [`Lowerer::movable`] with
    /// the result's element type, which moves into `stone.list_reuse`: when nothing else refers
    /// to its list, the loop writes the result over it, so `a = a * 2 + b` allocates nothing.
    fn elementwise(&mut self, expr: &Expr, kind: Option<RcKind>) -> Result<Operand, String> {
        // taken first, so that no call among the operands moves what the loop reads
        let movable = std::mem::take(&mut self.movable);
        let tree = self.fused(expr)?;
        let float = matches!(self.type_of(expr)?, Type::List(elem) if *elem == Type::Float);
        let mut lists = Vec::new();
        tree.lists(&mut lists);
        // a list used twice, as in `a * a`, is checked and read once
        let mut seen = HashSet::new();
        lists.retain(|leaf| seen.insert(leaf.value));
        let first = lists
            .first()
            .ok_or("arithmetic on lists has no list")?
            .value;
        let len = self.fresh();
        self.push(Inst::ListLen {
            dst: len,
            list: first,
        });
        for leaf in &lists[1..] {
            let other = self.fresh();
            self.push(Inst::ListLen {
                dst: other,
                list: leaf.value,
            });
            self.fail_unless(
                Cond::Eq,
                Operand::Reg(len),
                Operand::Reg(other),
                LENGTHS_DIFFER,
            );
        }
        let reused = lists.iter().find(|leaf| {
            leaf.float == float
                && leaf
                    .name
                    .as_ref()
                    .is_some_and(|name| movable.contains(name))
        });
        let result = self.fresh();
        let call = match reused {
            Some(leaf) if self.mark_moved(leaf.name.as_deref().unwrap_or_default()) => Inst::Call {
                dst: Some(result),
                callee: Callee::Runtime("stone.list_reuse"),
                args: vec![leaf.value, Operand::Reg(len)],
            },
            _ => Inst::Call {
                dst: Some(result),
                callee: Callee::Runtime("stone.list_new"),
                args: vec![Operand::Reg(len), Operand::Imm(0)],
            },
        };
        self.push(call);
        let temporaries: Vec<Operand> = lists.iter().map(|leaf| leaf.value).collect();

        // pairs of elements first, when the operations allow, then one at a time from where that
        // stopped
        let mut start = Operand::Imm(0);
        if tree.packable(float) && tree.size() <= MAX_PACKED_SIZE {
            let mut data = HashMap::new();
            for leaf in &lists {
                let dst = self.fresh();
                self.push(Inst::ListData {
                    dst,
                    list: leaf.value,
                });
                data.insert(leaf.value, Operand::Reg(dst));
            }
            let out = self.fresh();
            self.push(Inst::ListData {
                dst: out,
                list: Operand::Reg(result),
            });
            let done = self.fresh();
            self.push(Inst::Packed {
                done,
                float,
                kernel: tree.kernel(&data),
                out: Operand::Reg(out),
                len: Operand::Reg(len),
            });
            start = Operand::Reg(done);
        }
        let index = self.fresh();
        self.push(Inst::Copy {
            dst: index,
            src: start,
        });
        let (header, body, exit) = (self.new_block(), self.new_block(), self.new_block());
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(index),
            rhs: Operand::Reg(len),
            then: body,
            otherwise: exit,
        });
        self.start(body);
        let value = self.element(&tree, Operand::Reg(index), &mut HashMap::new());
        self.push(Inst::ListSet {
            list: Operand::Reg(result),
            index: Operand::Reg(index),
            value,
        });
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: index,
            lhs: Operand::Reg(index),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        for value in temporaries {
            self.release_temp(value);
        }
        self.own(Operand::Reg(result), kind);
        Ok(Operand::Reg(result))
    }

    /// Evaluates the operands of the arithmetic on lists `expr`, left to right, into a tree that
    /// [`Lowerer::element`] computes one element of. An operand that is a number, including
    /// arithmetic on numbers such as `2 + 3` in `a * (2 + 3)`, is computed here, once.
    fn fused(&mut self, expr: &Expr) -> Result<Fused, String> {
        let ty = self.type_of(expr)?;
        if let Type::List(elem) = &ty {
            let float = **elem == Type::Float;
            match &expr.kind {
                ExprKind::BinOp { op, left, right } => {
                    let lhs = self.fused(left)?;
                    let rhs = self.fused(right)?;
                    return Ok(Fused::Binary {
                        op: bin_op(*op),
                        float,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    });
                }
                ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd,
                    operand,
                } => return self.fused(operand),
                ExprKind::UnaryOp {
                    op: UnaryOp::UnarySub,
                    operand,
                } => {
                    let operand = self.fused(operand)?;
                    return Ok(Fused::Neg {
                        float,
                        operand: Box::new(operand),
                    });
                }
                _ => {}
            }
        }
        let value = self.expr(expr)?;
        Ok(match ty {
            Type::List(elem) => Fused::List(FusedList {
                value,
                float: *elem == Type::Float,
                name: match &expr.kind {
                    ExprKind::Name { id, .. } => Some(id.clone()),
                    _ => None,
                },
            }),
            _ => Fused::Number(value),
        })
    }

    /// Computes the element at `index` of `tree`, from [`Lowerer::fused`], whose lists all have
    /// more than `index` elements, with the same instructions as arithmetic on single numbers.
    /// `loaded` holds the element already read from each list.
    fn element(
        &mut self,
        tree: &Fused,
        index: Operand,
        loaded: &mut HashMap<Operand, Operand>,
    ) -> Operand {
        match tree {
            Fused::Number(value) => *value,
            Fused::List(leaf) => {
                if let Some(&element) = loaded.get(&leaf.value) {
                    return element;
                }
                let dst = self.fresh();
                self.push(Inst::ListGet {
                    dst,
                    list: leaf.value,
                    index,
                });
                loaded.insert(leaf.value, Operand::Reg(dst));
                Operand::Reg(dst)
            }
            Fused::Binary {
                op,
                float,
                lhs,
                rhs,
            } => {
                let lhs = self.element(lhs, index, loaded);
                let rhs = self.element(rhs, index, loaded);
                let dst = self.fresh();
                self.push(match float {
                    true => Inst::FloatBinary {
                        op: *op,
                        dst,
                        lhs,
                        rhs,
                    },
                    false => Inst::Binary {
                        op: *op,
                        dst,
                        lhs,
                        rhs,
                    },
                });
                Operand::Reg(dst)
            }
            Fused::Neg { float, operand } => {
                let src = self.element(operand, index, loaded);
                let dst = self.fresh();
                self.push(match float {
                    true => Inst::FloatNeg { dst, src },
                    false => Inst::Neg { dst, src },
                });
                Operand::Reg(dst)
            }
        }
    }

    /// Stops the program with the runtime error `message` unless `lhs cond rhs` holds for ints.
    fn fail_unless(&mut self, cond: Cond, lhs: Operand, rhs: Operand, message: &str) {
        let (holds, fails) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond,
            float: false,
            lhs,
            rhs,
            then: holds,
            otherwise: fails,
        });
        self.start(fails);
        self.terminate(Terminator::Fail(message.to_string()));
        self.start(holds);
    }

    /// Returns where `index` points in a list of `len` elements, counting a negative index from
    /// the end, and stops the program with `message` unless that is an element, or with `end`,
    /// an element or the end of the list.
    ///
    /// For example, `xs.insert(-1, x)` puts `x` at position `len - 1`, and `xs.insert(len, x)` at
    /// the end, but `xs.pop(len)` fails.
    fn list_position(&mut self, index: Operand, len: Operand, end: bool, message: &str) -> Operand {
        let position = self.fresh();
        self.push(Inst::Copy {
            dst: position,
            src: index,
        });
        let (negative, check) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(position),
            rhs: Operand::Imm(0),
            then: negative,
            otherwise: check,
        });
        self.start(negative);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: position,
            lhs: Operand::Reg(position),
            rhs: len,
        });
        self.start(check);
        self.fail_unless(Cond::Ge, Operand::Reg(position), Operand::Imm(0), message);
        let below = if end { Cond::Le } else { Cond::Lt };
        self.fail_unless(below, Operand::Reg(position), len, message);
        Operand::Reg(position)
    }

    /// Lowers the string index `text[index]`, evaluating the string and then the index, into a
    /// check that the index is in range followed by a call to `stone.str_at`, which fails if the
    /// byte there is part of a longer character.
    ///
    /// For example, `s[i]` becomes `v2 = call stone.str_len(v0)`, the steps of
    /// [`Lowerer::list_position`], and `v4 = call stone.str_at(v0, v3)`.
    fn string_index(
        &mut self,
        text: &Expr,
        index: &Expr,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let text = self.list_operand(text, &[index])?;
        let index = self.expr(index)?;
        let len = self.runtime_call("stone.str_len", vec![text], None);
        let position = self.list_position(index, len, false, STRING_INDEX_RANGE);
        let result = self.runtime_call("stone.str_at", vec![text, position], into);
        self.release_temp(text);
        self.own(result, kind);
        Ok(result)
    }

    /// Lowers the slice `list[lower:upper:step]` of a list or a string, evaluating it and then
    /// each bound that is there, then failing on a step of 0, into a call to `label`:
    /// `stone.list_slice` or `stone.str_slice_of`. A missing bound is passed as 0 with its bit set
    /// in the last argument: 1 for `lower`, 2 for `upper`, and 4 for `step`.
    ///
    /// For example, `xs[1:]` becomes `v1 = call stone.list_slice(v0, 1, 0, 0, 6)`.
    fn slice(
        &mut self,
        list: &Expr,
        bounds: [&Option<Box<Expr>>; 3],
        label: &'static str,
        into: Option<VReg>,
        kind: Option<RcKind>,
    ) -> Result<Operand, String> {
        let present: Vec<&Expr> = bounds.iter().filter_map(|b| b.as_deref()).collect();
        let list = self.list_operand(list, &present)?;
        let mut args = vec![list];
        let mut omitted = 0;
        for (bit, bound) in bounds.into_iter().enumerate() {
            match bound {
                Some(bound) => args.push(self.expr(bound)?),
                None => {
                    args.push(Operand::Imm(0));
                    omitted |= 1 << bit;
                }
            }
        }
        if let Some(step) = bounds[2]
            && constant_int(step).is_none_or(|n| n == 0)
        {
            let (zero, nonzero) = (self.new_block(), self.new_block());
            self.terminate(Terminator::CmpBranch {
                cond: Cond::Eq,
                float: false,
                lhs: args[3],
                rhs: Operand::Imm(0),
                then: zero,
                otherwise: nonzero,
            });
            self.start(zero);
            self.terminate(Terminator::Fail(SLICE_STEP_ZERO.to_string()));
            self.start(nonzero);
        }
        args.push(Operand::Imm(omitted));
        let result = self.runtime_call(label, args, into);
        self.release_temp(list);
        self.own(result, kind);
        Ok(result)
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
        // a slice is a new list, which the operation must release
        if matches!(slice.kind, ExprKind::Slice { .. }) || before.iter().any(|expr| has_call(expr))
        {
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
                // strings compare by contents
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
                Type::Str if cond == Cond::Eq => self.push(Inst::Call {
                    dst: Some(result),
                    callee: Callee::Runtime("stone.str_eq"),
                    args: vec![lhs, rhs],
                }),
                // ordered by a call that returns -1, 0, or 1
                Type::Str => {
                    let order = self.runtime_call("stone.str_cmp", vec![lhs, rhs], None);
                    self.push(Inst::Compare {
                        cond,
                        float: false,
                        dst: result,
                        lhs: order,
                        rhs: Operand::Imm(0),
                    });
                }
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
                // the receiver's indexes, then the arguments, then the change
                let elem = self.element_kind(receiver);
                let indexes = self.place_indexes(receiver)?;
                let mut values = Vec::new();
                for arg in args {
                    let value = self.expr(arg)?;
                    // the list keeps a reference to what it holds
                    let last_uses = std::mem::take(&mut self.statement_last_uses);
                    self.take_from(arg, value, elem, &last_uses);
                    self.statement_last_uses = last_uses;
                    values.push(value);
                }
                let list = self.unique_place(receiver, &indexes)?;
                values.insert(0, list);
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.list_append"),
                    args: values,
                });
                // append returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            "pop" => {
                // the receiver's indexes, then the index, then the change
                let indexes = self.place_indexes(receiver)?;
                let index = match args.first() {
                    Some(arg) => Some(self.expr(arg)?),
                    None => None,
                };
                let list = self.unique_place(receiver, &indexes)?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                self.fail_unless(Cond::Ne, Operand::Reg(len), Operand::Imm(0), POP_EMPTY);
                let position = match index {
                    Some(index) => self.list_position(index, Operand::Reg(len), false, POP_RANGE),
                    None => {
                        let last = self.fresh();
                        self.push(Inst::Binary {
                            op: BinOp::Sub,
                            dst: last,
                            lhs: Operand::Reg(len),
                            rhs: Operand::Imm(1),
                        });
                        Operand::Reg(last)
                    }
                };
                // the list's reference to the element moves to the result
                let result = self.runtime_call("stone.list_pop", vec![list, position], into);
                self.own(result, kind);
                Ok(result)
            }
            "insert" => {
                let elem = self.element_kind(receiver);
                let indexes = self.place_indexes(receiver)?;
                let [index, item] = args else {
                    return Err("insert() takes an index and a value".to_string());
                };
                let index = self.expr(index)?;
                let value = self.expr(item)?;
                // the list keeps a reference to what it holds
                let last_uses = std::mem::take(&mut self.statement_last_uses);
                self.take_from(item, value, elem, &last_uses);
                self.statement_last_uses = last_uses;
                let list = self.unique_place(receiver, &indexes)?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                let position = self.list_position(index, Operand::Reg(len), true, INDEX_RANGE);
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.list_insert"),
                    args: vec![list, position, value],
                });
                Ok(self.finish(Operand::Imm(0), into))
            }
            "extend" => {
                let indexes = self.place_indexes(receiver)?;
                let [other] = args else {
                    return Err("extend() takes a list".to_string());
                };
                // the elements are retained, so the call only borrows the other list
                let other = self.expr(other)?;
                let list = self.unique_place(receiver, &indexes)?;
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.list_extend"),
                    args: vec![list, other],
                });
                self.release_temp(other);
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
            "join" | "find" | "startswith" | "endswith" => {
                let mut values = vec![self.expr(receiver)?];
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let label = match name {
                    "join" => "stone.str_join",
                    "find" => "stone.str_position",
                    "startswith" => "stone.str_starts",
                    _ => "stone.str_ends",
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

    /// Lowers a call to a function of the `math` module, which never calls the runtime: each
    /// becomes compares and branches that do what `stdlib::math` does, in the same order.
    ///
    /// For example, `math.abs(x)` of an int fails if `x` is the smallest int, then negates it if
    /// it is negative, and `math.max(xs)` loops over the list, keeping an element only when it is
    /// greater than the one kept so far.
    fn math(&mut self, name: &str, args: &[Expr], into: Option<VReg>) -> Result<Operand, String> {
        let float = match args {
            [arg] => match self.type_of(arg)? {
                Type::List(elem) => *elem == Type::Float,
                ty => ty == Type::Float,
            },
            [first, ..] => self.type_of(first)? == Type::Float,
            [] => return Err(format!("'{name}' takes an argument")),
        };
        match name {
            "math.abs" => {
                let x = self.expr(&args[0])?;
                // the result is written last, since `x` may be the variable it is assigned to
                let result = self.fresh();
                let (negate, keep, done) = (self.new_block(), self.new_block(), self.new_block());
                if float {
                    // `0.0 - x` rather than flipping the sign, so -0.0 becomes 0.0
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Le,
                        float: true,
                        lhs: x,
                        rhs: Operand::Imm(0),
                        then: negate,
                        otherwise: keep,
                    });
                    self.start(negate);
                    self.push(Inst::FloatBinary {
                        op: BinOp::Sub,
                        dst: result,
                        lhs: Operand::Imm(0),
                        rhs: x,
                    });
                } else {
                    let (overflow, check) = (self.new_block(), self.new_block());
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Eq,
                        float: false,
                        lhs: x,
                        rhs: Operand::Imm(i64::MIN),
                        then: overflow,
                        otherwise: check,
                    });
                    self.start(overflow);
                    self.terminate(Terminator::Fail(math::ABS_OVERFLOW.to_string()));
                    self.start(check);
                    self.terminate(Terminator::CmpBranch {
                        cond: Cond::Lt,
                        float: false,
                        lhs: x,
                        rhs: Operand::Imm(0),
                        then: negate,
                        otherwise: keep,
                    });
                    self.start(negate);
                    self.push(Inst::Neg {
                        dst: result,
                        src: x,
                    });
                }
                self.jump(done);
                self.start(keep);
                self.push(Inst::Copy {
                    dst: result,
                    src: x,
                });
                self.start(done);
                Ok(self.finish(Operand::Reg(result), into))
            }
            "math.min" | "math.max" => {
                // a later value replaces the kept one only if it is strictly better, as in Python
                let better = if name == "math.max" {
                    Cond::Gt
                } else {
                    Cond::Lt
                };
                let result = self.fresh();
                if let [list] = args {
                    let empty = if name == "math.max" {
                        math::MAX_OF_EMPTY
                    } else {
                        math::MIN_OF_EMPTY
                    };
                    self.fold_list(list, better, float, empty, result)?;
                } else {
                    let mut values = Vec::new();
                    for arg in args {
                        values.push(self.expr(arg)?);
                    }
                    self.push(Inst::Copy {
                        dst: result,
                        src: values[0],
                    });
                    for &value in &values[1..] {
                        let (replace, next) = (self.new_block(), self.new_block());
                        self.terminate(Terminator::CmpBranch {
                            cond: better,
                            float,
                            lhs: value,
                            rhs: Operand::Reg(result),
                            then: replace,
                            otherwise: next,
                        });
                        self.start(replace);
                        self.push(Inst::Copy {
                            dst: result,
                            src: value,
                        });
                        self.start(next);
                    }
                }
                Ok(self.finish(Operand::Reg(result), into))
            }
            "math.sqrt" => {
                let mut x = self.expr(&args[0])?;
                if !float {
                    let converted = self.fresh();
                    self.push(Inst::IntToFloat {
                        dst: converted,
                        src: x,
                    });
                    x = Operand::Reg(converted);
                }
                let (negative, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Lt,
                    float: true,
                    lhs: x,
                    rhs: Operand::Imm(0),
                    then: negative,
                    otherwise: ok,
                });
                self.start(negative);
                self.terminate(Terminator::Fail(math::DOMAIN_ERROR.to_string()));
                self.start(ok);
                let dst = self.dst(into);
                self.push(Inst::FloatSqrt { dst, src: x });
                Ok(Operand::Reg(dst))
            }
            "math.floor" => {
                let x = self.expr(&args[0])?;
                if !float {
                    return Ok(self.finish(x, into));
                }
                // truncate toward zero, which fails on nan or overflow, then step down if that
                // went up, which only happens for a negative number with a fraction
                let truncated = self.fresh();
                self.push(Inst::FloatToInt {
                    dst: truncated,
                    src: x,
                });
                let back = self.fresh();
                self.push(Inst::IntToFloat {
                    dst: back,
                    src: Operand::Reg(truncated),
                });
                let (down, done) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Lt,
                    float: true,
                    lhs: x,
                    rhs: Operand::Reg(back),
                    then: down,
                    otherwise: done,
                });
                self.start(down);
                self.push(Inst::Binary {
                    op: BinOp::Sub,
                    dst: truncated,
                    lhs: Operand::Reg(truncated),
                    rhs: Operand::Imm(1),
                });
                self.start(done);
                Ok(self.finish(Operand::Reg(truncated), into))
            }
            _ => Err(format!("there is no function '{name}'")),
        }
    }

    /// Folds the numbers in the list `list` into `result` for `math.min` or `math.max`, keeping
    /// an element only when it compares `better` than the one kept so far, and failing with
    /// `empty` for an empty list.
    fn fold_list(
        &mut self,
        list: &Expr,
        better: Cond,
        float: bool,
        empty: &str,
        result: VReg,
    ) -> Result<(), String> {
        let list = self.list_operand(list, &[])?;
        let len = self.fresh();
        self.push(Inst::ListLen { dst: len, list });
        let (fail, first) = (self.new_block(), self.new_block());
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Eq,
            float: false,
            lhs: Operand::Reg(len),
            rhs: Operand::Imm(0),
            then: fail,
            otherwise: first,
        });
        self.start(fail);
        self.terminate(Terminator::Fail(empty.to_string()));
        self.start(first);
        self.push(Inst::ListGet {
            dst: result,
            list,
            index: Operand::Imm(0),
        });
        let index = self.fresh();
        self.push(Inst::Copy {
            dst: index,
            src: Operand::Imm(1),
        });

        let header = self.new_block();
        let body = self.new_block();
        let replace = self.new_block();
        let next = self.new_block();
        let exit = self.new_block();
        self.loop_depth += 1;
        self.start(header);
        self.terminate(Terminator::CmpBranch {
            cond: Cond::Lt,
            float: false,
            lhs: Operand::Reg(index),
            rhs: Operand::Reg(len),
            then: body,
            otherwise: exit,
        });
        self.start(body);
        let element = self.fresh();
        self.push(Inst::ListGet {
            dst: element,
            list,
            index: Operand::Reg(index),
        });
        self.terminate(Terminator::CmpBranch {
            cond: better,
            float,
            lhs: Operand::Reg(element),
            rhs: Operand::Reg(result),
            then: replace,
            otherwise: next,
        });
        self.start(replace);
        self.push(Inst::Copy {
            dst: result,
            src: Operand::Reg(element),
        });
        self.start(next);
        self.push(Inst::Binary {
            op: BinOp::Add,
            dst: index,
            lhs: Operand::Reg(index),
            rhs: Operand::Imm(1),
        });
        self.jump(header);
        self.loop_depth -= 1;
        self.start(exit);
        self.release_temp(list);
        Ok(())
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
            _ if name.starts_with("math.") => self.math(name, args, into),
            "random.randint" => {
                let low = self.expr(&args[0])?;
                let high = self.expr(&args[1])?;
                let (empty, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Gt,
                    float: false,
                    lhs: low,
                    rhs: high,
                    then: empty,
                    otherwise: ok,
                });
                self.start(empty);
                self.terminate(Terminator::Fail(random::EMPTY_RANGE.to_string()));
                self.start(ok);
                // wraps to 0, which the runtime takes as 2^64, for the whole range of ints
                let span = self.fresh();
                self.push(Inst::Binary {
                    op: BinOp::Sub,
                    dst: span,
                    lhs: high,
                    rhs: low,
                });
                self.push(Inst::Binary {
                    op: BinOp::Add,
                    dst: span,
                    lhs: Operand::Reg(span),
                    rhs: Operand::Imm(1),
                });
                let offset =
                    self.runtime_call("stone.random_below", vec![Operand::Reg(span)], None);
                let dst = self.dst(into);
                self.push(Inst::Binary {
                    op: BinOp::Add,
                    dst,
                    lhs: low,
                    rhs: offset,
                });
                Ok(Operand::Reg(dst))
            }
            "random.choice" => {
                let list = self.list_operand(&args[0], &[])?;
                let len = self.fresh();
                self.push(Inst::ListLen { dst: len, list });
                let (empty, ok) = (self.new_block(), self.new_block());
                self.terminate(Terminator::CmpBranch {
                    cond: Cond::Eq,
                    float: false,
                    lhs: Operand::Reg(len),
                    rhs: Operand::Imm(0),
                    then: empty,
                    otherwise: ok,
                });
                self.start(empty);
                self.terminate(Terminator::Fail(random::EMPTY_CHOICE.to_string()));
                self.start(ok);
                let index = self.runtime_call("stone.random_below", vec![Operand::Reg(len)], None);
                let dst = self.dst(into);
                self.push(Inst::ListGet { dst, list, index });
                // the element gets a reference of its own, as when a subscript reads it
                if kind.is_some() {
                    self.push(Inst::Retain {
                        src: Operand::Reg(dst),
                    });
                    self.own(Operand::Reg(dst), kind);
                }
                self.release_temp(list);
                Ok(Operand::Reg(dst))
            }
            "time.sleep" => {
                let mut seconds = self.expr(&args[0])?;
                if self.type_of(&args[0])? == Type::Int {
                    let converted = self.fresh();
                    self.push(Inst::IntToFloat {
                        dst: converted,
                        src: seconds,
                    });
                    seconds = Operand::Reg(converted);
                }
                self.push(Inst::Call {
                    dst: None,
                    callee: Callee::Runtime("stone.time_sleep"),
                    args: vec![seconds],
                });
                // sleep returns none
                Ok(self.finish(Operand::Imm(0), into))
            }
            _ if let Some(label) = module_routine(name) => {
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
                // taken first, so that no call among the arguments moves what a later one reads
                let movable = std::mem::take(&mut self.movable);
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.expr(arg)?);
                }
                let given = self.give_references(name, args, &values, &movable);
                let dst = self.dst(into);
                self.push(Inst::Call {
                    dst: Some(dst),
                    callee: Callee::User(name.to_string()),
                    args: values.clone(),
                });
                for (value, given) in values.into_iter().zip(given) {
                    if !given {
                        self.release_temp(value);
                    }
                }
                self.own(Operand::Reg(dst), kind);
                Ok(Operand::Reg(dst))
            }
        }
    }

    /// Gives a reference to each argument of the stone function `name` that it owns, once every
    /// argument is evaluated, and returns which arguments got one.
    ///
    /// A temporary gives its own. A variable in `movable` gives its own too, which moves its
    /// value into the call, unless it is a global the function may read, or it was already given
    /// to another parameter of the same call. Anything else is retained. For example, in
    /// `xs = add(xs, 1)` the list moves into `add`, which can then append to it in place, while
    /// `ys = add(xs, 1)` retains it, so `add` copies it before appending.
    fn give_references(
        &mut self,
        name: &str,
        args: &[Expr],
        values: &[Operand],
        movable: &[String],
    ) -> Vec<bool> {
        let owned = self.context.owned_params.get(name);
        let mut given = vec![false; values.len()];
        for (i, (arg, &value)) in args.iter().zip(values).enumerate() {
            if owned.is_none_or(|owned| owned.get(i) != Some(&true)) || self.counted(arg).is_none()
            {
                continue;
            }
            given[i] = true;
            if let Operand::Reg(reg) = value
                && self.owned.remove(&reg).is_some()
            {
                continue;
            }
            // a global the function may read must keep its own copy while it runs
            if let ExprKind::Name { id, .. } = &arg.kind
                && movable.contains(id)
                && (self.locals.contains_key(id) || !self.context.global_reads[name].contains(id))
                && self.mark_moved(id)
            {
                continue;
            }
            self.push(Inst::Retain { src: value });
        }
        given
    }

    /// Returns the counted variables this function owns, or the counted globals in `main`, that
    /// nothing reads after `stmt`, so that `stmt` may move them instead of copying them.
    ///
    /// For example, in `ys = add(xs, 1)` followed by `print(ys)`, this is `xs`.
    fn last_uses(&self, stmt: &Stmt) -> Vec<String> {
        let live = self.live_after.get(&stmt.span);
        let mut names: Vec<String> = match &self.name {
            Some(_) => self
                .locals
                .keys()
                .filter(|name| self.is_counted_variable(name))
                .cloned()
                .collect(),
            None => self
                .context
                .globals
                .iter()
                .filter(|name| self.global_kind(name).is_some())
                .cloned()
                .collect(),
        };
        names.retain(|name| live.is_none_or(|live| !live.contains(name)));
        names.sort();
        names
    }

    /// Returns whether `name` is a variable this code owns a counted reference in: a counted
    /// local of this function, or a counted global in `main`.
    fn is_counted_variable(&self, name: &str) -> bool {
        match self.locals.get(name) {
            Some(reg) => self.counted_locals.iter().any(|(local, _)| local == reg),
            None => self.name.is_none() && self.global_kind(name).is_some(),
        }
    }

    /// Notes that the variable `name` gave its reference away, returning false if it already
    /// had.
    fn mark_moved(&mut self, name: &str) -> bool {
        match self.locals.get(name) {
            Some(&local) => self.moved_locals.insert(local),
            None => self.moved_globals.insert(name.to_string()),
        }
    }

    /// Like [`Lowerer::take`], but a variable in `last_uses` gives its own reference instead of
    /// being retained, so that `ys = xs` at the last use of `xs` leaves one reference, not two.
    fn take_from(
        &mut self,
        expr: &Expr,
        value: Operand,
        kind: Option<RcKind>,
        last_uses: &[String],
    ) {
        if kind.is_some()
            && let ExprKind::Name { id, .. } = &expr.kind
            && last_uses.contains(id)
            && self.mark_moved(id)
        {
            return;
        }
        self.take(value, kind);
    }

    /// Leaves null in each variable that gave its reference away and that its statement did not
    /// assign again, so releasing it later does nothing.
    fn clear_moved(&mut self) {
        let mut locals: Vec<VReg> = self.moved_locals.drain().collect();
        locals.sort_by_key(|reg| reg.0);
        for local in locals {
            self.push(Inst::Copy {
                dst: local,
                src: Operand::Imm(0),
            });
        }
        let mut globals: Vec<String> = self.moved_globals.drain().collect();
        globals.sort();
        for name in globals {
            self.push(Inst::StoreGlobal {
                name,
                src: Operand::Imm(0),
            });
        }
    }

    /// Returns whether `expr` is arithmetic on lists, which [`Lowerer::elementwise`] lowers.
    fn is_elementwise(&self, expr: &Expr) -> bool {
        matches!(
            expr.kind,
            ExprKind::BinOp { .. }
                | ExprKind::UnaryOp {
                    op: UnaryOp::UnaryAdd | UnaryOp::UnarySub,
                    ..
                }
        ) && matches!(self.context.types.get(&expr.span), Some(Type::List(_)))
    }

    /// Returns whether `expr` is a call to a stone function, rather than a builtin or a method.
    fn is_stone_call(&self, expr: &Expr) -> bool {
        matches!(&expr.kind, ExprKind::Call { func, .. }
            if matches!(&func.kind, ExprKind::Name { id, .. }
                if self.context.owned_params.contains_key(id)))
    }

    fn print_char(&mut self, c: char) {
        self.push(Inst::Call {
            dst: None,
            callee: Callee::Runtime("stone.print_char"),
            args: vec![Operand::Imm(c as i64)],
        });
    }
}

/// Returns, for each function in `body`, whether it owns each of its parameters, which it does if
/// it assigns or changes the parameter.
///
/// For example, `fn add(xs: list[int], x: int) -> list[int]; xs.append(x); ret xs` owns `xs`
/// but not `x`.
fn owned_params(body: &[Stmt]) -> HashMap<String, Vec<bool>> {
    let mut owned = HashMap::new();
    for stmt in body {
        if let StmtKind::FunctionDef {
            name, args, body, ..
        } = &stmt.kind
        {
            let mut assigned = Vec::new();
            collect_assigned(body, &mut assigned);
            let mut changed = HashSet::new();
            collect_changed(body, &mut changed);
            let params = args
                .args
                .iter()
                .map(|arg| {
                    changed.contains(&arg.arg) || assigned.iter().any(|(name, _)| *name == arg.arg)
                })
                .collect();
            owned.insert(name.clone(), params);
        }
    }
    owned
}

/// Arithmetic on lists with its operands evaluated, from [`Lowerer::fused`].
enum Fused {
    /// A number, the same for every element.
    Number(Operand),
    List(FusedList),
    /// An operation on ints, or on floats if `float`.
    Binary {
        op: BinOp,
        float: bool,
        lhs: Box<Fused>,
        rhs: Box<Fused>,
    },
    Neg {
        float: bool,
        operand: Box<Fused>,
    },
}

/// A list operand of arithmetic on lists, read one element at a time.
struct FusedList {
    value: Operand,
    /// Whether its elements are floats.
    float: bool,
    /// The variable it is, if any, which can give its list to the result.
    name: Option<String>,
}

/// The most nodes a [`Fused`] tree may have to be computed in pairs, so that its numbers, its
/// intermediate results, and the few registers a packed loop needs besides fit the 13 vector
/// registers x64 has free.
const MAX_PACKED_SIZE: usize = 10;

impl Fused {
    /// Returns whether every operation in the tree can be computed two elements at a time with
    /// packed instructions, on floats if `float` and ints otherwise: adding, subtracting, and
    /// negating either, and multiplying and dividing floats.
    fn packable(&self, float: bool) -> bool {
        match self {
            Fused::Number(_) => true,
            Fused::List(leaf) => leaf.float == float,
            Fused::Binary {
                op,
                float: node,
                lhs,
                rhs,
            } => {
                let packs = match op {
                    BinOp::Add | BinOp::Sub => true,
                    BinOp::Mul | BinOp::Div => float,
                    BinOp::Rem | BinOp::Pow => false,
                };
                packs && *node == float && lhs.packable(float) && rhs.packable(float)
            }
            Fused::Neg {
                float: node,
                operand,
            } => *node == float && operand.packable(float),
        }
    }

    /// Returns how many nodes the tree has.
    fn size(&self) -> usize {
        match self {
            Fused::Number(_) | Fused::List(_) => 1,
            Fused::Binary { lhs, rhs, .. } => 1 + lhs.size() + rhs.size(),
            Fused::Neg { operand, .. } => 1 + operand.size(),
        }
    }

    /// Returns the tree as a kernel for [`Inst::Packed`], reading each list through its data
    /// pointer in `data`.
    fn kernel(&self, data: &HashMap<Operand, Operand>) -> Kernel {
        match self {
            Fused::Number(value) => Kernel::Number(*value),
            Fused::List(leaf) => Kernel::Element(data[&leaf.value]),
            Fused::Binary { op, lhs, rhs, .. } => {
                Kernel::Binary(*op, Box::new(lhs.kernel(data)), Box::new(rhs.kernel(data)))
            }
            Fused::Neg { operand, .. } => Kernel::Neg(Box::new(operand.kernel(data))),
        }
    }

    /// Adds every list in the tree to `out`, left to right.
    fn lists<'a>(&'a self, out: &mut Vec<&'a FusedList>) {
        match self {
            Fused::Number(_) => {}
            Fused::List(leaf) => out.push(leaf),
            Fused::Binary { lhs, rhs, .. } => {
                lhs.lists(out);
                rhs.lists(out);
            }
            Fused::Neg { operand, .. } => operand.lists(out),
        }
    }
}

/// Returns the IR operation for an arithmetic operator.
///
/// For example, `bin_op(Operator::Modulo)` is `BinOp::Rem`.
fn bin_op(op: Operator) -> BinOp {
    match op {
        Operator::Add => BinOp::Add,
        Operator::Subtract => BinOp::Sub,
        Operator::Multiply => BinOp::Mul,
        Operator::Divide => BinOp::Div,
        Operator::Modulo => BinOp::Rem,
        Operator::Power => BinOp::Pow,
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
        ExprKind::Slice { lower, upper, step } => [lower, upper, step]
            .into_iter()
            .flatten()
            .any(|b| has_call(b)),
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
