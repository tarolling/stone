//! Tree-walking interpreter that evaluates a stone AST directly.
//!
//! For example, running `x = 42` followed by `print(x)` prints `42`.

use crate::ast::{
    BoolOp, CompOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp,
};
use crate::checker::{collect_assigned, place, range_args};
use crate::last_use::{GlobalReads, global_reads, last_uses};
use crate::span::Span;
use crate::stdlib::{self, MAX_CALL_DEPTH, os};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::rc::Rc;

type EvalResult<T> = Result<T, Box<dyn std::error::Error>>;

/// The variables each statement of a body uses for the last time, which the interpreter drops
/// after the statement so that a list nothing else holds can change in place.
#[derive(Default)]
struct LastUses {
    by_span: HashMap<Span, Vec<String>>,
    /// Whether some statement starting on each line uses a variable for the last time, which
    /// rules out most statements without hashing their span.
    lines: Vec<bool>,
}

impl LastUses {
    /// Finds the last uses in `body`, a function's or the program's, given what each function
    /// reads.
    fn new(body: &[Stmt], reads: &GlobalReads) -> Self {
        let by_span = last_uses(body, reads);
        let mut lines = Vec::new();
        for span in by_span.keys() {
            let line = span.start.line;
            if lines.len() <= line {
                lines.resize(line + 1, false);
            }
            lines[line] = true;
        }
        LastUses { by_span, lines }
    }

    /// Returns the variables the statement at `span` uses for the last time, if any.
    fn get(&self, span: Span) -> Option<&Vec<String>> {
        if !self.lines.get(span.start.line).copied().unwrap_or(false) {
            return None;
        }
        self.by_span.get(&span)
    }
}

/// A user-defined function.
///
/// For example, `def add(a, b); ret a + b` has the parameters `["a", "b"]`.
struct Function {
    params: Vec<String>,
    body: Vec<Stmt>,
    /// The variables each statement of the body uses for the last time.
    last_uses: Rc<LastUses>,
}

/// A value a running program works with.
///
/// Lists are values: after `b = a`, appending to `b` leaves `a` unchanged. They share their
/// elements through an `Rc` until one is changed, which copies it first if anything else still
/// holds it ([`Rc::make_mut`]), the same copy on write compiled code does.
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(Rc<str>),
    None,
    List(Rc<Vec<Value>>),
}

/// Makes a new list of strings, such as the pieces `split` returns.
fn str_list(items: Vec<&str>) -> Value {
    Value::list(items.into_iter().map(|s| Value::Str(s.into())).collect())
}

impl Value {
    fn list(items: Vec<Value>) -> Value {
        Value::List(Rc::new(items))
    }

    /// Formats the value the way `print` shows it, with strings inside lists quoted.
    ///
    /// For example, `[1, 2]` prints as `[1, 2]` and `["a"]` as `['a']`.
    fn display(&self, nested: bool) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Float(x) => stdlib::format_float(*x),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) if nested => format!("'{s}'"),
            Value::Str(s) => s.to_string(),
            Value::None => "none".to_string(),
            Value::List(items) => {
                let items: Vec<String> = items.iter().map(|v| v.display(true)).collect();
                format!("[{}]", items.join(", "))
            }
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Int(i) => *i != 0,
            Value::Float(x) => *x != 0.0,
            Value::Bool(b) => *b,
            Value::Str(s) => !s.is_empty(),
            Value::None => false,
            Value::List(items) => !items.is_empty(),
        }
    }

    /// Returns whether two values are equal, comparing lists by identity as compiled code does.
    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::None, Value::None) => true,
            (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }

    fn as_int(&self) -> EvalResult<i64> {
        match self {
            Value::Int(i) => Ok(*i),
            other => Err(format!("expected an int, found {}", other.display(true)).into()),
        }
    }

    /// Orders two numbers, or returns `None` if either is nan.
    fn compare(&self, other: &Value) -> EvalResult<Option<Ordering>> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => Ok(Some(a.cmp(b))),
            (Value::Float(a), Value::Float(b)) => Ok(a.partial_cmp(b)),
            (a, b) => {
                Err(format!("cannot order {} and {}", a.display(true), b.display(true)).into())
            }
        }
    }

    fn as_list(&self) -> EvalResult<&Rc<Vec<Value>>> {
        match self {
            Value::List(items) => Ok(items),
            other => Err(format!("expected a list, found {}", other.display(true)).into()),
        }
    }

    /// Returns whether this is a list that another value shares, so changing it copies it.
    fn is_shared(&self) -> bool {
        matches!(self, Value::List(items) if Rc::strong_count(items) > 1)
    }

    /// Returns the elements of a list for changing, copying them first if another value still
    /// shares them, so the change shows through no other variable.
    fn as_list_mut(&mut self) -> EvalResult<&mut Vec<Value>> {
        match self {
            Value::List(items) => Ok(Rc::make_mut(items)),
            other => Err(format!("expected a list, found {}", other.display(true)).into()),
        }
    }
}

impl From<&Constant> for Value {
    fn from(constant: &Constant) -> Self {
        match constant {
            Constant::Bool(b) => Value::Bool(*b),
            Constant::Str(s) => Value::Str(s.as_str().into()),
            Constant::Char(c) => Value::Str(c.to_string().into()),
            Constant::None => Value::None,
            Constant::Int(i) | Constant::I64(i) => Value::Int(*i),
            Constant::Float(x) | Constant::F64(x) => Value::Float(*x),
            // other literal kinds are not lexed yet
            _ => Value::None,
        }
    }
}

/// Converts `index` into a position in a list of length `len`, counting negative indexes from the
/// end like Python.
///
/// For example, index `-1` of a 3-element list is position 2, and index 3 is an error.
fn list_position(index: i64, len: usize) -> EvalResult<usize> {
    let len = len as i64;
    let position = if index < 0 { index + len } else { index };
    if (0..len).contains(&position) {
        Ok(position as usize)
    } else {
        Err(format!("list index out of range (index {index}, length {len})").into())
    }
}

/// The error `os.exit` stops a program with, holding the exit status, which is the low 8 bits of
/// the code it was given, as the system keeps them.
///
/// For example, `os.exit(3)` stops with `Exit(3)` and `os.exit(-1)` with `Exit(255)`. It is not
/// a failure: callers print nothing for it and exit with its status, which may be 0.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Exit(pub u8);

impl std::fmt::Display for Exit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "the program exited with status {}", self.0)
    }
}

impl std::error::Error for Exit {}

pub enum ControlFlow {
    None,
    Return(Value),
    Break,
    Continue,
}

/// Bounds on how much work a program may do before the interpreter stops it with an error.
///
/// For example, `Limits { fuel: 1_000, max_depth: 200, max_calls: 50 }` allows at most 1,000 loop
/// iterations and calls combined, with statements, expressions, and calls nested at most 200 deep
/// in total, and at most 50 calls active at once.
#[derive(Clone, Copy, Debug)]
pub struct Limits {
    /// Loop iterations and function calls allowed in total.
    pub fuel: u64,
    /// Deepest nesting of statement and expression evaluation allowed, counting across calls.
    ///
    /// This bounds the interpreter's own recursion, and so its stack use. A recursive stone
    /// function uses several levels per call, so `def f(n); ret f(n - 1)` uses about three.
    pub max_depth: usize,
    /// Most function calls active at once, which is part of the language, so compiled code
    /// enforces the same limit.
    pub max_calls: usize,
}

impl Limits {
    /// No practical limit on fuel, the language's [`MAX_CALL_DEPTH`], and a nesting limit with
    /// room for that many calls, which needs [`Limits::STACK_SIZE`] of stack.
    pub const DEFAULT: Limits = Limits {
        fuel: u64::MAX,
        max_depth: 50_000,
        max_calls: MAX_CALL_DEPTH,
    };

    /// Stack to run the interpreter on under [`Limits::DEFAULT`].
    ///
    /// Each level of nesting takes up to about 5 KiB in debug builds, so 50,000 levels needs about
    /// 250 MiB. Only the pages actually used are ever allocated.
    pub const STACK_SIZE: usize = 256 * 1024 * 1024;
}

pub struct Interpreter<'out> {
    // global variables
    globals: HashMap<String, Value>,
    /// Stack of local scopes, with the innermost scope last.
    scopes: Vec<HashMap<String, Value>>,
    /// User-defined functions by name.
    functions: HashMap<String, Rc<Function>>,
    /// The variables each statement of the running body uses for the last time, innermost call
    /// last. A variable is dropped after its last use, so a list that nothing else holds can be
    /// changed in place.
    last_uses: Vec<Rc<LastUses>>,
    /// For each function, the globals it may read, which must not move into a call to it.
    global_reads: GlobalReads,
    /// Whether globals can move at their last use, which is never in an interactive session,
    /// since a later entry may read them.
    moves_globals: bool,
    /// The call a statement makes as its value, by span, and the variables it may take its
    /// arguments from, since nothing reads them after it.
    top_call: Option<(Span, Vec<String>)>,
    /// Where `print` writes, which is stdout unless a caller captures it.
    out: Box<dyn Write + 'out>,
    /// Where `input` and `eof` read, which is empty unless a caller gives one.
    input: Box<dyn BufRead + 'out>,
    /// What `args` returns.
    args: Vec<String>,
    limits: Limits,
    /// Fuel spent so far, compared against [`Limits::fuel`].
    fuel_used: u64,
    /// Current nesting of `eval_stmt` and `eval_expr`, compared against [`Limits::max_depth`].
    depth: usize,
    /// Function calls currently active, compared against [`Limits::max_calls`].
    calls: usize,
    /// How many times a change copied a list that something else still shared, which tests use
    /// to check that values move rather than being copied.
    copies: u64,
}

impl Default for Interpreter<'static> {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter<'static> {
    /// Creates an interpreter that prints to stdout with [`Limits::DEFAULT`].
    pub fn new() -> Self {
        Interpreter::with_output(std::io::stdout(), Limits::DEFAULT)
    }
}

impl<'out> Interpreter<'out> {
    /// Creates an interpreter that prints to `out` and stops programs that exceed `limits`.
    ///
    /// For example, `Interpreter::with_output(&mut buffer, limits)` collects printed lines in a
    /// `Vec<u8>` named `buffer`.
    pub fn with_output(out: impl Write + 'out, limits: Limits) -> Self {
        Self {
            globals: HashMap::new(),
            scopes: vec![],
            functions: HashMap::new(),
            last_uses: vec![],
            global_reads: GlobalReads::new(),
            moves_globals: false,
            top_call: None,
            out: Box::new(out),
            input: Box::new(std::io::empty()),
            args: vec![],
            limits,
            fuel_used: 0,
            depth: 0,
            calls: 0,
            copies: 0,
        }
    }

    /// Makes `input` and `eof` read from `input` instead of an empty stream.
    ///
    /// For example, `Interpreter::with_output(out, limits).with_input(std::io::stdin().lock())`
    /// reads the program's stdin.
    pub fn with_input(mut self, input: impl BufRead + 'out) -> Self {
        self.input = Box::new(input);
        self
    }

    /// Makes `args` return `args` instead of an empty list.
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.args = args;
        self
    }

    /// Reads one line for `input`, without its newline, or `""` at the end of the input.
    ///
    /// A read error counts as the end of the input, and bytes that are not UTF-8 become U+FFFD.
    fn read_line(&mut self) -> String {
        let mut line = Vec::new();
        if self.input.read_until(b'\n', &mut line).is_err() {
            return String::new();
        }
        if line.last() == Some(&b'\n') {
            line.pop();
        }
        String::from_utf8_lossy(&line).into_owned()
    }

    /// Returns whether `input` has nothing left, which is what `eof` returns.
    fn at_eof(&mut self) -> bool {
        self.input.fill_buf().map_or(true, |buf| buf.is_empty())
    }

    /// Spends one unit of fuel, failing once the budget in [`Limits::fuel`] is used up.
    fn burn_fuel(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        self.fuel_used += 1;
        if self.fuel_used > self.limits.fuel {
            return Err("program ran out of fuel".into());
        }
        Ok(())
    }

    /// Runs `eval` one level deeper, failing once nesting exceeds [`Limits::max_depth`].
    ///
    /// For example, `self.nested(|this| this.eval_expr_unguarded(expr))` evaluates `expr` one
    /// level deeper.
    fn nested<T>(
        &mut self,
        eval: impl FnOnce(&mut Self) -> Result<T, Box<dyn std::error::Error>>,
    ) -> Result<T, Box<dyn std::error::Error>> {
        if self.depth >= self.limits.max_depth {
            return Err(format!("nesting exceeded {} levels", self.limits.max_depth).into());
        }
        self.depth += 1;
        let result = eval(self);
        self.depth -= 1;
        result
    }

    /// Runs statements in order until one of them breaks, continues, or returns.
    ///
    /// For example, the body of `if x; ret 1` yields `ControlFlow::Return(Int(1))` when `x` is
    /// truthy, which the enclosing function then returns.
    fn eval_block(&mut self, body: &[Stmt]) -> Result<ControlFlow, Box<dyn std::error::Error>> {
        for stmt in body {
            let flow = self.eval_stmt(stmt)?;
            if !matches!(flow, ControlFlow::None) {
                return Ok(flow);
            }
        }
        Ok(ControlFlow::None)
    }

    pub fn evaluate(&mut self, module: &Mod) -> Result<(), Box<dyn std::error::Error>> {
        match module {
            Mod::Module { body } => {
                let mut assigned = vec![];
                collect_assigned(body, &mut assigned);
                let globals = assigned.into_iter().map(|(name, _)| name).collect();
                self.global_reads = global_reads(body, &globals);
                self.moves_globals = true;
                self.last_uses = vec![Rc::new(LastUses::new(body, &self.global_reads))];
                // functions can be called before their definition
                for stmt in body {
                    if let StmtKind::FunctionDef { .. } = stmt.kind {
                        self.eval_stmt(stmt)?;
                    }
                }
                for stmt in body {
                    if let ControlFlow::Return(_) = self.eval_stmt(stmt)? {
                        break; // top-level return
                    }
                }
            }
        }

        Ok(())
    }

    /// Runs one entry of an interactive session, keeping the globals and functions of earlier
    /// entries, and prints the value of each top-level expression statement unless it is `none`.
    ///
    /// For example, after an entry `x = 1`, running the entry `x + 1` prints `2`, and running
    /// `"hi"` prints `'hi'`, quoted as it would be inside a list.
    pub fn run_entry(&mut self, body: &[Stmt]) -> EvalResult<()> {
        // a later entry may read any global, so none moves
        self.moves_globals = false;
        self.last_uses = vec![Rc::default()];
        // functions can be called before their definition
        for stmt in body {
            if let StmtKind::FunctionDef { .. } = stmt.kind {
                self.eval_stmt(stmt)?;
            }
        }
        for stmt in body {
            match &stmt.kind {
                StmtKind::FunctionDef { .. } => {}
                StmtKind::Expr { value } => {
                    let value = self.eval_expr(value)?;
                    if !matches!(value, Value::None) {
                        writeln!(self.out, "{}", value.display(true))?;
                    }
                }
                _ => {
                    if let ControlFlow::Return(_) = self.eval_stmt(stmt)? {
                        break; // top-level return
                    }
                }
            }
        }
        Ok(())
    }

    /// Returns the reader `input` and `eof` read from, so an interactive session can read its
    /// entries from the same place.
    pub fn input(&mut self) -> &mut (dyn BufRead + 'out) {
        &mut *self.input
    }

    /// Returns the writer `print` writes to.
    pub fn output(&mut self) -> &mut (dyn Write + 'out) {
        &mut *self.out
    }

    /// Executes one statement and reports whether it breaks, continues, or returns.
    ///
    /// ```text
    /// stmt = FunctionDef(identifier name, arguments args, stmt* body, expr? returns)
    ///      | Return(expr? value)
    ///      | Assign(expr* targets, expr value)
    ///      | For(expr target, expr iter, stmt* body, stmt* orelse)
    ///      | While(expr test, stmt* body, stmt* orelse)
    ///      | If(expr test, stmt* body, stmt* orelse)
    ///      | Expr(expr value)
    ///      | Break | Continue
    /// ```
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, Box<dyn std::error::Error>> {
        self.nested(|this| {
            let flow = this.eval_stmt_unguarded(stmt)?;
            // returning drops the whole scope anyway
            if !matches!(stmt.kind, StmtKind::Return { .. }) {
                this.drop_last_uses(stmt.span);
            }
            Ok(flow)
        })
    }

    /// Notes `value`, the value of the statement at `span`, if it calls a stone function, with
    /// the variables the call may take its arguments from: those the statement uses for the last
    /// time, and `targets`, the names it assigns.
    ///
    /// For example, `xs = add(xs, 1)` may take `xs`, since the assignment replaces it.
    fn note_top_call(&mut self, span: Span, value: &Expr, targets: &[Expr]) {
        let ExprKind::Call { func, .. } = &value.kind else {
            return;
        };
        let ExprKind::Name { id, .. } = &func.kind else {
            return;
        };
        if !self.functions.contains_key(id) {
            return;
        }
        let mut names = self
            .last_uses
            .last()
            .and_then(|uses| uses.get(span))
            .cloned()
            .unwrap_or_default();
        for target in targets {
            if let ExprKind::Name { id, .. } = &target.kind {
                names.push(id.clone());
            }
        }
        // a call claims only its own entry, by span, so an unclaimed one can stay
        self.top_call = Some((value.span, names));
    }

    /// Drops the variables the statement at `span` used for the last time, so the values they
    /// held are no longer shared.
    fn drop_last_uses(&mut self, span: Span) {
        // most statements use nothing for the last time, so only a hit is cloned
        let Some(names) = self
            .last_uses
            .last()
            .and_then(|uses| uses.get(span))
            .cloned()
        else {
            return;
        };
        for name in &names {
            self.drop_variable(name, None);
        }
    }

    /// Drops the variable `name` of the running body if it holds a list, the only value a
    /// change can copy: a local of the running function, or a global at the top level, unless
    /// globals cannot move or the function `callee` may read it.
    fn drop_variable(&mut self, name: &str, callee: Option<&str>) {
        let holds_list =
            |scope: &HashMap<String, Value>| matches!(scope.get(name), Some(Value::List(_)));
        match self.scopes.last_mut() {
            Some(scope) => {
                if holds_list(scope) {
                    scope.remove(name);
                }
            }
            None if self.moves_globals
                && holds_list(&self.globals)
                && callee.is_none_or(|callee| {
                    self.global_reads
                        .get(callee)
                        .is_none_or(|reads| !reads.contains(name))
                }) =>
            {
                self.globals.remove(name);
            }
            None => {}
        }
    }

    fn eval_stmt_unguarded(
        &mut self,
        stmt: &Stmt,
    ) -> Result<ControlFlow, Box<dyn std::error::Error>> {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                let function = Function {
                    params: args.args.iter().map(|arg| arg.arg.clone()).collect(),
                    last_uses: Rc::new(LastUses::new(body, &self.global_reads)),
                    body: body.clone(),
                };
                self.functions.insert(name.clone(), Rc::new(function));
                Ok(ControlFlow::None)
            }
            StmtKind::Return { value } => {
                let val = if let Some(expr) = value {
                    self.note_top_call(stmt.span, expr, &[]);
                    self.eval_expr(expr)?
                } else {
                    Value::None
                };
                Ok(ControlFlow::Return(val))
            }
            StmtKind::Assign { targets, value } => {
                // a target's indexes are read after the call
                if targets
                    .iter()
                    .all(|target| matches!(target.kind, ExprKind::Name { .. }))
                {
                    self.note_top_call(stmt.span, value, targets);
                }
                let rhs = self.eval_expr(value)?;

                for target in targets {
                    match &target.kind {
                        ExprKind::Name { id, .. } => {
                            self.set_var(id, &rhs)?;
                        }
                        ExprKind::Subscript { value, slice, .. } => {
                            let (root, indexes) = self.eval_place(value)?;
                            let index = self.eval_expr(slice)?.as_int()?;
                            let items = self.place_list(root, &indexes)?;
                            let position = list_position(index, items.len())?;
                            items[position] = rhs.clone();
                        }
                        _ => return Err("Invalid assignment target".into()),
                    }
                }
                Ok(ControlFlow::None)
            }
            StmtKind::For { target, iter, body } => {
                let ExprKind::Name { id, .. } = &target.kind else {
                    return Err("a 'for' loop's variable must be a name".into());
                };
                if let Some(args) = range_args(iter) {
                    // the bounds are evaluated once, left to right
                    let mut bounds = Vec::with_capacity(args.len());
                    for arg in args {
                        bounds.push(self.eval_expr(arg)?.as_int()?);
                    }
                    let (start, end) = match bounds[..] {
                        [end] => (0, end),
                        [start, end] => (start, end),
                        _ => return Err("range() takes 1 or 2 arguments".into()),
                    };
                    for i in start..end {
                        self.set_var(id, &Value::Int(i))?;
                        if let Some(flow) = self.run_iteration(body)? {
                            return Ok(flow);
                        }
                    }
                    return Ok(ControlFlow::None);
                }

                // the loop holds the list as it was, so changing the variable copies it first
                let list = self.eval_expr(iter)?;
                let items = list.as_list()?.clone();
                for item in items.iter() {
                    self.set_var(id, item)?;
                    if let Some(flow) = self.run_iteration(body)? {
                        return Ok(flow);
                    }
                }
                Ok(ControlFlow::None)
            }
            StmtKind::While { test, body } => {
                loop {
                    if !self.eval_expr(test)?.is_truthy() {
                        break;
                    }
                    if let Some(flow) = self.run_iteration(body)? {
                        return Ok(flow);
                    }
                }
                Ok(ControlFlow::None)
            }
            StmtKind::If { test, body, orelse } => {
                if self.eval_expr(test)?.is_truthy() {
                    self.eval_block(body)
                } else {
                    self.eval_block(orelse)
                }
            }
            StmtKind::Expr { value } => {
                self.note_top_call(stmt.span, value, &[]);
                self.eval_expr(value)?;
                Ok(ControlFlow::None)
            }
            StmtKind::Break => Ok(ControlFlow::Break),
            StmtKind::Continue => Ok(ControlFlow::Continue),
            StmtKind::Use { .. } => Err("use is only allowed at the top level of a file".into()),
        }
    }

    /// Evaluates one expression to a constant, so `1 + 2` evaluates to `Int(3)`.
    ///
    /// ```text
    /// expr = BoolOp(boolop op, expr* values)
    ///      | BinOp(expr left, operator op, expr right)
    ///      | UnaryOp(unaryop op, expr operand)
    ///      | Compare(expr left, cmpop* ops, expr* comparators)
    ///      | Call(expr func, expr* args)
    ///      | Constant(constant value, string? kind)
    ///      | Name(identifier id, expr_context ctx)
    ///      | List(expr* elts, expr_context ctx)
    /// ```
    /// Runs one iteration of a loop body, spending fuel, and returns how the loop should end, or
    /// `None` to keep looping.
    ///
    /// For example, a `break` ends the loop normally with `ControlFlow::None`, while a `ret`
    /// passes its `ControlFlow::Return` on to the enclosing function.
    fn run_iteration(&mut self, body: &[Stmt]) -> EvalResult<Option<ControlFlow>> {
        self.burn_fuel()?;
        Ok(match self.eval_block(body)? {
            ControlFlow::Break => Some(ControlFlow::None),
            ControlFlow::Return(value) => Some(ControlFlow::Return(value)),
            ControlFlow::Continue | ControlFlow::None => None,
        })
    }

    fn eval_expr(&mut self, expr: &Expr) -> EvalResult<Value> {
        self.nested(|this| this.eval_expr_unguarded(expr))
    }

    fn eval_expr_unguarded(&mut self, expr: &Expr) -> EvalResult<Value> {
        match &expr.kind {
            // short-circuiting
            ExprKind::BoolOp { op, values } => {
                let mut result = self.eval_expr(&values[0])?;
                for value in &values[1..] {
                    let done = match op {
                        BoolOp::And => !result.is_truthy(),
                        BoolOp::Or => result.is_truthy(),
                    };
                    if done {
                        return Ok(result);
                    }
                    result = self.eval_expr(value)?;
                }
                Ok(result)
            }
            ExprKind::BinOp { op, left, right } => {
                let lhs = self.eval_expr(left)?;
                let rhs = self.eval_expr(right)?;

                match (lhs, rhs) {
                    // the exponent is always an int, even for a float base
                    (Value::Int(_), Value::Int(r)) if *op == Operator::Power && r < 0 => {
                        Err("negative exponent".into())
                    }
                    (Value::Int(l), Value::Int(r)) if *op == Operator::Power => {
                        Ok(Value::Int(stdlib::int_pow(l, r)))
                    }
                    (Value::Float(l), Value::Int(r)) if *op == Operator::Power => {
                        Ok(Value::Float(stdlib::float_pow(l, r)))
                    }
                    (Value::Int(l), Value::Int(r)) => match op {
                        // wrap like the compiled add, sub, and imul do
                        Operator::Add => Ok(Value::Int(l.wrapping_add(r))),
                        Operator::Subtract => Ok(Value::Int(l.wrapping_sub(r))),
                        Operator::Multiply => Ok(Value::Int(l.wrapping_mul(r))),
                        // the same errors compiled code checks for before idiv
                        Operator::Divide if r == 0 => Err("division by zero".into()),
                        Operator::Divide => l
                            .checked_div(r)
                            .map(Value::Int)
                            .ok_or_else(|| "integer overflow in division".into()),
                        Operator::Modulo if r == 0 => Err("division by zero".into()),
                        // `MIN % -1` is 0, which compiled code also gives
                        Operator::Modulo => Ok(Value::Int(l.wrapping_rem(r))),
                        Operator::Power => Err("Type mismatch in binary operation".into()),
                    },
                    (Value::Float(l), Value::Float(r)) => match op {
                        Operator::Add => Ok(Value::Float(l + r)),
                        Operator::Subtract => Ok(Value::Float(l - r)),
                        Operator::Multiply => Ok(Value::Float(l * r)),
                        // like Python, rather than giving inf or nan
                        Operator::Divide if r == 0.0 => Err("division by zero".into()),
                        Operator::Divide => Ok(Value::Float(l / r)),
                        Operator::Modulo if r == 0.0 => Err("division by zero".into()),
                        Operator::Modulo => Ok(Value::Float(l % r)),
                        Operator::Power => Err("Type mismatch in binary operation".into()),
                    },
                    (Value::Str(l), Value::Str(r)) if matches!(op, Operator::Add) => {
                        Ok(Value::Str(format!("{l}{r}").into()))
                    }
                    _ => Err("Type mismatch in binary operation".into()),
                }
            }
            ExprKind::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                match op {
                    UnaryOp::Not => Ok(Value::Bool(!val.is_truthy())),
                    UnaryOp::UnaryAdd => Ok(val),
                    UnaryOp::UnarySub => match val {
                        Value::Float(x) => Ok(Value::Float(-x)),
                        other => Ok(Value::Int(other.as_int()?.wrapping_neg())),
                    },
                }
            }
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => {
                let mut current = self.eval_expr(left)?;
                for (op, comparator) in ops.iter().zip(comparators) {
                    let next = self.eval_expr(comparator)?;
                    let holds = match op {
                        CompOp::Equal => current.equals(&next),
                        CompOp::NotEqual => !current.equals(&next),
                        CompOp::LessThan => current.compare(&next)?.is_some_and(Ordering::is_lt),
                        CompOp::LessThanEqual => {
                            current.compare(&next)?.is_some_and(Ordering::is_le)
                        }
                        CompOp::GreaterThan => current.compare(&next)?.is_some_and(Ordering::is_gt),
                        CompOp::GreaterThanEqual => {
                            current.compare(&next)?.is_some_and(Ordering::is_ge)
                        }
                    };
                    if !holds {
                        return Ok(Value::Bool(false));
                    }
                    current = next;
                }
                Ok(Value::Bool(true))
            }
            ExprKind::Call { func, args } => {
                if let ExprKind::Attribute { value, attr, .. } = &func.kind
                    && stdlib::changes_receiver(attr)
                {
                    // the receiver's indexes, then the arguments, then the change
                    let (root, indexes) = self.eval_place(value)?;
                    let mut values = Vec::with_capacity(args.len());
                    for arg in args {
                        values.push(self.eval_expr(arg)?);
                    }
                    let items = self.place_list(root, &indexes)?;
                    return match (attr.as_str(), &values[..]) {
                        ("append", [item]) => {
                            items.push(item.clone());
                            Ok(Value::None)
                        }
                        _ => Err("append() is called on a list, with one value".into()),
                    };
                }
                if let ExprKind::Attribute { value, attr, .. } = &func.kind {
                    // the receiver is evaluated before the arguments
                    let receiver = self.eval_expr(value)?;
                    let mut values = Vec::with_capacity(args.len());
                    for arg in args {
                        values.push(self.eval_expr(arg)?);
                    }
                    return self.call_method(attr, receiver, values);
                }
                let ExprKind::Name { id, .. } = &func.kind else {
                    return Err("only functions can be called, by name".into());
                };
                // claimed first, since a call among the arguments runs statements of its own
                let movable = self.top_call.take_if(|(span, _)| *span == expr.span);
                // arguments are evaluated left to right in the caller's scope
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr(arg)?);
                }
                // an argument nothing reads after its statement's call moves into it
                if let Some((_, names)) = movable {
                    for arg in args {
                        if let ExprKind::Name { id: name, .. } = &arg.kind
                            && names.contains(name)
                        {
                            self.drop_variable(name, Some(id));
                        }
                    }
                }
                self.call(id, values)
            }
            ExprKind::Attribute { attr, .. } => {
                Err(format!("'{attr}' is a method, so it can only be called").into())
            }
            ExprKind::Constant { value, kind: _ } => Ok(Value::from(&**value)),
            ExprKind::Subscript { value, slice, .. } => {
                let list = self.eval_expr(value)?;
                let index = self.eval_expr(slice)?.as_int()?;
                let items = list.as_list()?;
                let position = list_position(index, items.len())?;
                Ok(items[position].clone())
            }
            ExprKind::Name { id, ctx: _ } => self.get_var(id),
            ExprKind::List { elts, ctx: _ } => {
                let mut items = Vec::with_capacity(elts.len());
                for elt in elts {
                    items.push(self.eval_expr(elt)?);
                }
                Ok(Value::list(items))
            }
        }
    }

    /// Calls the builtin method `name` on an already evaluated receiver and arguments.
    ///
    /// For example, `call_method("len", Value::Str("abc".into()), vec![])` returns `Value::Int(3)`.
    fn call_method(&mut self, name: &str, receiver: Value, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &receiver, &args[..]) {
            // len counts bytes, like the compiled strlen
            ("len", Value::Str(s), []) => Ok(Value::Int(s.len() as i64)),
            ("len", Value::List(items), []) => Ok(Value::Int(items.len() as i64)),
            ("len", _, _) => Err("len() is called on a str or list, with no arguments".into()),
            ("strip", Value::Str(s), []) => Ok(Value::Str(stdlib::strip(s).into())),
            ("strip", _, _) => Err("strip() is called on a str, with no arguments".into()),
            ("split", Value::Str(s), []) => Ok(str_list(stdlib::split_whitespace(s))),
            ("split", Value::Str(s), [Value::Str(separator)]) => {
                Ok(str_list(stdlib::split(s, separator)?))
            }
            ("split", _, _) => Err("split() is called on a str, with at most one str".into()),
            _ => Err(format!("there is no method '{name}'").into()),
        }
    }

    /// Calls the builtin or user-defined function named `name` with already evaluated arguments.
    fn call(&mut self, name: &str, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &args[..]) {
            ("print", _) => {
                let parts: Vec<String> = args.iter().map(|v| v.display(false)).collect();
                writeln!(self.out, "{}", parts.join(" "))?;
                return Ok(Value::None);
            }
            ("float", [Value::Int(i)]) => return Ok(Value::Float(*i as f64)),
            ("float", [Value::Float(x)]) => return Ok(Value::Float(*x)),
            ("float", [Value::Str(s)]) => return Ok(Value::Float(stdlib::parse_float(s)?)),
            ("float", _) => return Err("float() takes one int, float, or str".into()),
            ("int", [Value::Int(i)]) => return Ok(Value::Int(*i)),
            // -2^63 fits in an int but 2^63 does not, and nan fails both checks
            ("int", [Value::Float(x)]) if (i64::MIN as f64..-(i64::MIN as f64)).contains(x) => {
                return Ok(Value::Int(*x as i64));
            }
            ("int", [Value::Float(_)]) => {
                return Err("cannot convert float to int (nan or out of range)".into());
            }
            ("int", [Value::Str(s)]) => return Ok(Value::Int(stdlib::parse_int(s)?)),
            ("int", _) => return Err("int() takes one int, float, or str".into()),
            ("str", [value]) => return Ok(Value::Str(value.display(false).into())),
            ("input", [] | [Value::Str(_)]) => {
                if let [prompt] = &args[..] {
                    write!(self.out, "{}", prompt.display(false))?;
                }
                // a prompt, or earlier output a pipe buffered, shows before waiting for input
                self.out.flush()?;
                return Ok(Value::Str(self.read_line().into()));
            }
            ("eof", []) => {
                self.out.flush()?;
                return Ok(Value::Bool(self.at_eof()));
            }
            ("args", []) => {
                let args = self.args.iter().map(|a| a.as_str()).collect();
                return Ok(str_list(args));
            }
            ("os.env", [Value::Str(name)]) => return Ok(Value::Str(os::env(name).into())),
            ("os.has_env", [Value::Str(name)]) => return Ok(Value::Bool(os::has_env(name))),
            ("os.platform", []) => return Ok(Value::Str(os::platform().into())),
            ("os.arch", []) => return Ok(Value::Str(os::arch().into())),
            ("os.hostname", []) => return Ok(Value::Str(os::hostname().into())),
            ("os.cpu_count", []) => return Ok(Value::Int(os::cpu_count())),
            ("os.pid", []) => return Ok(Value::Int(os::pid())),
            ("os.cwd", []) => return Ok(Value::Str(os::cwd()?.into())),
            ("os.exit", [Value::Int(code)]) => {
                self.out.flush()?;
                return Err(Box::new(Exit(*code as u8)));
            }
            ("os.time", []) => return Ok(Value::Float(os::time())),
            ("os.clock", []) => return Ok(Value::Float(os::clock())),
            _ => {}
        }

        let Some(function) = self.functions.get(name).cloned() else {
            return Err(format!("Function '{name}' not found").into());
        };
        if function.params.len() != args.len() {
            let expected = function.params.len();
            return Err(format!("Function {name} expects {expected} args").into());
        }

        self.burn_fuel()?;
        if self.calls >= self.limits.max_calls {
            return Err(format!(
                "recursion is too deep (more than {} nested calls)",
                self.limits.max_calls
            )
            .into());
        }

        // bind parameters directly so they shadow globals of the same name
        self.calls += 1;
        self.last_uses.push(function.last_uses.clone());
        self.enter_scope();
        let scope = self.scopes.last_mut().expect("scope was just entered");
        for (param, value) in function.params.iter().zip(args) {
            scope.insert(param.clone(), value);
        }
        let flow = self.eval_block(&function.body);
        self.exit_scope();
        self.last_uses.pop();
        self.calls -= 1;

        Ok(match flow? {
            ControlFlow::Return(value) => value,
            _ => Value::None,
        })
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    /// Assigns a variable, which is local inside a function and global otherwise, like Python.
    ///
    /// For example, `x = 2` inside a function leaves a global `x` unchanged.
    fn set_var(&mut self, name: &str, value: &Value) -> EvalResult<Value> {
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        scope.insert(name.to_string(), value.clone());
        Ok(value.clone())
    }

    /// Reads a variable from the current function's scope, then from the globals.
    ///
    /// A function never sees its caller's locals, so scoping is lexical.
    fn get_var(&self, name: &str) -> EvalResult<Value> {
        self.scopes
            .last()
            .and_then(|scope| scope.get(name))
            .or_else(|| self.globals.get(name))
            .cloned()
            .ok_or_else(|| format!("'{name}' is used before it is assigned").into())
    }

    /// Evaluates the indexes of `list`, a variable or an element of one that a statement is about
    /// to change, and returns the variable's name with the indexes, outermost first.
    ///
    /// For example, `grid[i][j]` gives `"grid"` and the values of `i` and `j`. Nothing is checked
    /// against the lists yet: [`Interpreter::place_list`] does that once the change is ready.
    fn eval_place<'e>(&mut self, list: &'e Expr) -> EvalResult<(&'e str, Vec<i64>)> {
        let Some((root, exprs)) = place(list) else {
            return Err("only a list in a variable can be changed".into());
        };
        let mut indexes = Vec::with_capacity(exprs.len());
        for index in exprs {
            indexes.push(self.eval_expr(index)?.as_int()?);
        }
        Ok((root, indexes))
    }

    /// Walks from the variable `root` through `indexes` to the list a statement changes, and
    /// returns its elements for changing.
    ///
    /// Every list on the way that another value still shares is copied first, so the change shows
    /// through no other variable. The checker allows a function to change only its own variables,
    /// so `root` is in the function's scope, or in the globals at the top level.
    fn place_list(&mut self, root: &str, indexes: &[i64]) -> EvalResult<&mut Vec<Value>> {
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        let mut value = scope
            .get_mut(root)
            .ok_or_else(|| format!("'{root}' is used before it is assigned"))?;
        for &index in indexes {
            self.copies += value.is_shared() as u64;
            let items = value.as_list_mut()?;
            let position = list_position(index, items.len())?;
            value = &mut items[position];
        }
        self.copies += value.is_shared() as u64;
        value.as_list_mut()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::parse;

    /// Runs `source` and returns how many times a change copied a list that something else
    /// still shared.
    fn copies(source: &str) -> u64 {
        let module = parse(source).unwrap();
        let mut out = Vec::new();
        let mut interpreter = Interpreter::with_output(&mut out, Limits::DEFAULT);
        interpreter.evaluate(&module).unwrap();
        interpreter.copies
    }

    /// A function that appends to its parameter and returns it.
    const ADD: &str = "def add(xs, x);\n    xs.append(x)\n    ret xs\n";

    #[test]
    fn a_list_assigned_the_result_of_a_call_is_changed_in_place() {
        let source =
            format!("{ADD}xs = []\nfor i in range(100);\n    xs = add(xs, i)\nprint(xs)\n");
        assert_eq!(copies(&source), 0);
        let source = format!(
            "{ADD}def fill(xs, n);\n    if n == 0;\n        ret xs\n    xs.append(n)\n    \
             ret fill(xs, n - 1)\nprint(fill([], 50))\n"
        );
        assert_eq!(copies(&source), 0);
        // a call among the arguments runs statements of its own first
        let source = format!(
            "{ADD}def one();\n    n = 1\n    ret n\nxs = []\nfor i in range(10);\n    \
             xs = add(xs, one())\nprint(xs)\n"
        );
        assert_eq!(copies(&source), 0);
    }

    #[test]
    fn a_list_moves_at_its_last_use() {
        assert_eq!(
            copies(&format!("{ADD}xs = [1]\nys = add(xs, 2)\nprint(ys)\n")),
            0
        );
        assert_eq!(copies("xs = [1]\nys = xs\nys.append(2)\nprint(ys)\n"), 0);
        assert_eq!(
            copies("rows = []\nrow = [1]\nrows.append(row)\nrows[0].append(2)\nprint(rows)\n"),
            0
        );
        let source = "def f(n);\n    xs = [n]\n    ys = xs\n    ys.append(1)\n    ret ys\n\
                      print(f(0))\n";
        assert_eq!(copies(source), 0);
    }

    #[test]
    fn a_list_read_later_is_copied() {
        assert_eq!(
            copies(&format!("{ADD}xs = [1]\nys = add(xs, 2)\nprint(xs, ys)\n")),
            1
        );
        assert_eq!(
            copies("xs = [1]\nys = xs\nys.append(2)\nprint(xs, ys)\n"),
            1
        );
        // the function reads the global it is given, so it must not see the change
        let source = "def peek(ys);\n    ys.append(xs[0])\n    ret ys\nxs = [1]\nxs = peek(xs)\n\
                      print(xs)\n";
        assert_eq!(copies(source), 1);
    }
}
