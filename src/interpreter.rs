//! Tree-walking interpreter that evaluates a stone AST directly.
//!
//! For example, running `x = 42` followed by `print(x)` prints `42`.

use crate::ast::{
    BoolOp, CompOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp,
};
use crate::checker::range_args;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

type EvalResult<T> = Result<T, Box<dyn std::error::Error>>;

/// A value a running program works with.
///
/// Lists are shared rather than copied, like in Python, so after `b = a`, appending to `b` also
/// changes `a`.
#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Str(Rc<str>),
    None,
    List(Rc<RefCell<Vec<Value>>>),
}

impl Value {
    fn list(items: Vec<Value>) -> Value {
        Value::List(Rc::new(RefCell::new(items)))
    }

    /// Formats the value the way `print` shows it, with strings inside lists quoted.
    ///
    /// For example, `[1, 2]` prints as `[1, 2]` and `["a"]` as `['a']`.
    fn display(&self, nested: bool) -> String {
        match self {
            Value::Int(i) => i.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Str(s) if nested => format!("'{s}'"),
            Value::Str(s) => s.to_string(),
            Value::None => "none".to_string(),
            Value::List(items) => {
                let items: Vec<String> = items.borrow().iter().map(|v| v.display(true)).collect();
                format!("[{}]", items.join(", "))
            }
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Int(i) => *i != 0,
            Value::Bool(b) => *b,
            Value::Str(s) => !s.is_empty(),
            Value::None => false,
            Value::List(items) => !items.borrow().is_empty(),
        }
    }

    /// Returns whether two values are equal, comparing lists by identity as compiled code does.
    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
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

    fn as_list(&self) -> EvalResult<&Rc<RefCell<Vec<Value>>>> {
        match self {
            Value::List(items) => Ok(items),
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

pub enum ControlFlow {
    None,
    Return(Value),
    Break,
    Continue,
}

/// Bounds on how much work a program may do before the interpreter stops it with an error.
///
/// For example, `Limits { fuel: 1_000, max_depth: 200 }` allows at most 1,000 loop iterations
/// and calls combined, with statements, expressions, and calls nested at most 200 deep in total.
#[derive(Clone, Copy, Debug)]
pub struct Limits {
    /// Loop iterations and function calls allowed in total.
    pub fuel: u64,
    /// Deepest nesting of statement and expression evaluation allowed, counting across calls.
    ///
    /// This bounds the interpreter's own recursion, and so its stack use. A recursive stone
    /// function uses several levels per call, so `def f(n); ret f(n - 1)` uses about three.
    pub max_depth: usize,
}

impl Limits {
    /// No practical limit on fuel, and a depth that keeps evaluation on the main thread's stack.
    ///
    /// Each level takes up to about 5 KiB of stack in debug builds, so 1,000 levels stays inside
    /// the main thread's 8 MiB.
    pub const DEFAULT: Limits = Limits {
        fuel: u64::MAX,
        max_depth: 1_000,
    };
}

pub struct Interpreter<'out> {
    // global variables
    globals: HashMap<String, Value>,
    /// Stack of local scopes, with the innermost scope last.
    scopes: Vec<HashMap<String, Value>>,
    /// User-defined functions, mapping each name to its parameters and body.
    ///
    /// For example, `def add(a, b); ret a + b` is stored as `"add" -> (["a", "b"], body)`.
    functions: HashMap<String, (Vec<String>, Rc<Vec<Stmt>>)>,
    /// Where `print` writes, which is stdout unless a caller captures it.
    out: Box<dyn Write + 'out>,
    limits: Limits,
    /// Fuel spent so far, compared against [`Limits::fuel`].
    fuel_used: u64,
    /// Current nesting of `eval_stmt` and `eval_expr`, compared against [`Limits::max_depth`].
    depth: usize,
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
            out: Box::new(out),
            limits,
            fuel_used: 0,
            depth: 0,
        }
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

    /// Executes one statement and reports whether it breaks, continues, or returns.
    ///
    /// ```text
    /// stmt = FunctionDef(identifier name, arguments args, stmt* body, expr? returns)
    ///      | Return(expr? value)
    ///      | Delete(expr* targets)
    ///      | Assign(expr* targets, expr value)
    ///      | For(expr target, expr iter, stmt* body, stmt* orelse)
    ///      | While(expr test, stmt* body, stmt* orelse)
    ///      | If(expr test, stmt* body, stmt* orelse)
    ///      | Expr(expr value)
    ///      | Break | Continue
    /// ```
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, Box<dyn std::error::Error>> {
        self.nested(|this| this.eval_stmt_unguarded(stmt))
    }

    fn eval_stmt_unguarded(
        &mut self,
        stmt: &Stmt,
    ) -> Result<ControlFlow, Box<dyn std::error::Error>> {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => {
                let param_names: Vec<String> =
                    args.args.iter().map(|arg| arg.arg.clone()).collect();
                self.functions
                    .insert(name.clone(), (param_names, Rc::new(body.clone())));
                Ok(ControlFlow::None)
            }
            StmtKind::Return { value } => {
                let val = if let Some(expr) = value {
                    self.eval_expr(expr)?
                } else {
                    Value::None
                };
                Ok(ControlFlow::Return(val))
            }
            StmtKind::Delete { targets } => {
                for target in targets {
                    if let ExprKind::Name { id, .. } = &target.kind {
                        self.delete_var(id)?;
                    }
                }
                Ok(ControlFlow::None)
            }
            StmtKind::Assign { targets, value } => {
                let rhs = self.eval_expr(value)?;

                for target in targets {
                    match &target.kind {
                        ExprKind::Name { id, .. } => {
                            self.set_var(id, &rhs)?;
                        }
                        ExprKind::Subscript { value, slice, .. } => {
                            let list = self.eval_expr(value)?;
                            let index = self.eval_expr(slice)?.as_int()?;
                            let mut items = list.as_list()?.borrow_mut();
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

                // like Python, a list that grows while it is iterated keeps going
                let list = self.eval_expr(iter)?;
                let items = list.as_list()?.clone();
                let mut position = 0;
                loop {
                    let Some(item) = items.borrow().get(position).cloned() else {
                        break;
                    };
                    position += 1;
                    self.set_var(id, &item)?;
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
                self.eval_expr(value)?;
                Ok(ControlFlow::None)
            }
            StmtKind::Break => Ok(ControlFlow::Break),
            StmtKind::Continue => Ok(ControlFlow::Continue),
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
                    (Value::Int(l), Value::Int(r)) => match op {
                        // wrap like the compiled add, sub, and imul do
                        Operator::Add => Ok(Value::Int(l.wrapping_add(r))),
                        Operator::Subtract => Ok(Value::Int(l.wrapping_sub(r))),
                        Operator::Multiply => Ok(Value::Int(l.wrapping_mul(r))),
                        // idiv traps on both of these, so report them instead
                        Operator::Divide => l
                            .checked_div(r)
                            .map(Value::Int)
                            .ok_or_else(|| "division by zero or overflow".into()),
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
                    UnaryOp::UnarySub => Ok(Value::Int(val.as_int()?.wrapping_neg())),
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
                        CompOp::LessThan => current.as_int()? < next.as_int()?,
                        CompOp::LessThanEqual => current.as_int()? <= next.as_int()?,
                        CompOp::GreaterThan => current.as_int()? > next.as_int()?,
                        CompOp::GreaterThanEqual => current.as_int()? >= next.as_int()?,
                    };
                    if !holds {
                        return Ok(Value::Bool(false));
                    }
                    current = next;
                }
                Ok(Value::Bool(true))
            }
            ExprKind::Call { func, args } => {
                let ExprKind::Name { id, .. } = &func.kind else {
                    return Err("only functions can be called, by name".into());
                };
                // arguments are evaluated left to right in the caller's scope
                let mut values = Vec::with_capacity(args.len());
                for arg in args {
                    values.push(self.eval_expr(arg)?);
                }
                self.call(id, values)
            }
            ExprKind::Constant { value, kind: _ } => Ok(Value::from(&**value)),
            ExprKind::Subscript { value, slice, .. } => {
                let list = self.eval_expr(value)?;
                let index = self.eval_expr(slice)?.as_int()?;
                let items = list.as_list()?.borrow();
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

    /// Calls the builtin or user-defined function named `name` with already evaluated arguments.
    fn call(&mut self, name: &str, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &args[..]) {
            ("print", _) => {
                let parts: Vec<String> = args.iter().map(|v| v.display(false)).collect();
                writeln!(self.out, "{}", parts.join(" "))?;
                return Ok(Value::None);
            }
            // len counts bytes, like the compiled strlen
            ("len", [Value::Str(s)]) => return Ok(Value::Int(s.len() as i64)),
            ("len", [Value::List(items)]) => return Ok(Value::Int(items.borrow().len() as i64)),
            ("len", _) => return Err("len() takes one str or list".into()),
            ("append", [Value::List(items), item]) => {
                items.borrow_mut().push(item.clone());
                return Ok(Value::None);
            }
            ("append", _) => return Err("append() takes a list and a value".into()),
            _ => {}
        }

        let Some((params, body)) = self.functions.get(name) else {
            return Err(format!("Function '{name}' not found").into());
        };
        let (params, body) = (params.clone(), body.clone());
        if params.len() != args.len() {
            return Err(format!("Function {} expects {} args", name, params.len()).into());
        }

        self.burn_fuel()?;

        // bind parameters directly so they shadow globals of the same name
        self.enter_scope();
        let scope = self.scopes.last_mut().expect("scope was just entered");
        for (param, value) in params.into_iter().zip(args) {
            scope.insert(param, value);
        }
        let flow = self.eval_block(&body);
        self.exit_scope();

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
            .ok_or_else(|| format!("Variable '{}' not found", name).into())
    }

    fn delete_var(&mut self, name: &str) -> EvalResult<()> {
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        if scope.remove(name).is_some() {
            return Ok(());
        }
        Err(format!("Variable '{}' not found", name).into())
    }
}
