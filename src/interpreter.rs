//! Tree-walking interpreter that evaluates a stone AST directly.
//!
//! For example, running `x = 42` followed by `print(x)` prints `42`.

use crate::ast::{BoolOp, CompOp, Constant, Expr, Mod, Operator, Stmt, UnaryOp};
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

pub enum ControlFlow {
    None,
    Return(Constant),
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
    globals: HashMap<String, Constant>,
    /// Stack of local scopes, with the innermost scope last.
    scopes: Vec<HashMap<String, Constant>>,
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
        match stmt {
            Stmt::FunctionDef { name, args, body } => {
                let param_names: Vec<String> =
                    args.args.iter().map(|arg| arg.arg.clone()).collect();
                self.functions
                    .insert(name.clone(), (param_names, Rc::new(body.clone())));
                Ok(ControlFlow::None)
            }
            Stmt::Return { value } => {
                let val = if let Some(expr) = value {
                    self.eval_expr(expr)?
                } else {
                    Constant::None
                };
                Ok(ControlFlow::Return(val))
            }
            Stmt::Delete { targets } => {
                for target in targets {
                    if let Expr::Name { id, .. } = target {
                        self.delete_var(id)?;
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::Assign { targets, value } => {
                let rhs = self.eval_expr(value)?;

                for target in targets {
                    match target {
                        Expr::Name { id, .. } => {
                            self.set_var(id, &rhs)?;
                        }
                        _ => return Err("Invalid assignment target".into()),
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::For {
                target: _,
                iter: _,
                body: _,
            } => Ok(ControlFlow::None),
            Stmt::While { test, body } => {
                loop {
                    let value = self.eval_expr(test)?;
                    if !self.is_truthy(&value) {
                        break;
                    }
                    self.burn_fuel()?;
                    match self.eval_block(body)? {
                        ControlFlow::Break => break,
                        ControlFlow::Return(value) => return Ok(ControlFlow::Return(value)),
                        ControlFlow::Continue | ControlFlow::None => {}
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::If { test, body, orelse } => {
                let test = self.eval_expr(test)?;
                if self.is_truthy(&test) {
                    self.eval_block(body)
                } else {
                    self.eval_block(orelse)
                }
            }
            Stmt::Expr { value } => {
                self.eval_expr(value)?;
                Ok(ControlFlow::None)
            }
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
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
    fn eval_expr(&mut self, expr: &Expr) -> Result<Constant, Box<dyn std::error::Error>> {
        self.nested(|this| this.eval_expr_unguarded(expr))
    }

    fn eval_expr_unguarded(&mut self, expr: &Expr) -> Result<Constant, Box<dyn std::error::Error>> {
        match expr {
            // short-circuiting
            Expr::BoolOp { op, values } => {
                let mut result = self.eval_expr(&values[0])?;

                for value in &values[1..] {
                    match op {
                        BoolOp::And => {
                            if !self.is_truthy(&result) {
                                return Ok(result);
                            }
                            result = self.eval_expr(value)?;
                        }
                        BoolOp::Or => {
                            if self.is_truthy(&result) {
                                return Ok(result);
                            }
                            result = self.eval_expr(value)?;
                        }
                    }
                }
                Ok(result)
            }
            Expr::BinOp { op, left, right } => {
                let lhs = self.eval_expr(left)?;
                let rhs = self.eval_expr(right)?;

                match (lhs, rhs) {
                    (Constant::Int(l), Constant::Int(r)) => match op {
                        // wrap like the compiled add, sub, and imul do
                        Operator::Add => Ok(Constant::Int(l.wrapping_add(r))),
                        Operator::Subtract => Ok(Constant::Int(l.wrapping_sub(r))),
                        Operator::Multiply => Ok(Constant::Int(l.wrapping_mul(r))),
                        // idiv traps on both of these, so report them instead
                        Operator::Divide => l
                            .checked_div(r)
                            .map(Constant::Int)
                            .ok_or_else(|| "division by zero or overflow".into()),
                    },
                    (Constant::Float(l), Constant::Float(r)) => match op {
                        Operator::Add => Ok(Constant::Float(l + r)),
                        Operator::Subtract => Ok(Constant::Float(l - r)),
                        Operator::Multiply => Ok(Constant::Float(l * r)),
                        Operator::Divide => Ok(Constant::Float(l / r)),
                    },
                    (Constant::Str(l), Constant::Str(r)) if matches!(op, Operator::Add) => {
                        Ok(Constant::Str(format!("{}{}", l, r)))
                    }
                    _ => Err("Type mismatch in binary operation".into()),
                }
            }
            Expr::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;

                match op {
                    UnaryOp::Not => Ok(Constant::Bool(!self.is_truthy(&val))),
                    UnaryOp::UnaryAdd => Ok(val),
                    UnaryOp::UnarySub => match val {
                        Constant::Int(i) => Ok(Constant::Int(i.wrapping_neg())),
                        Constant::Float(f) => Ok(Constant::Float(-f)),
                        _ => Err("Cannot negate non-numeric value".into()),
                    },
                }
            }
            Expr::Compare {
                left,
                ops,
                comparators,
            } => {
                let mut current = self.eval_expr(left)?;

                for (op, comparator) in ops.iter().zip(comparators.iter()) {
                    let next = self.eval_expr(comparator)?;

                    let result = match op {
                        CompOp::Equal => current == next,
                        CompOp::NotEqual => current != next,
                        CompOp::LessThan => self.compare_lt(&current, &next)?,
                        CompOp::LessThanEqual => self.compare_lte(&current, &next)?,
                        CompOp::GreaterThan => self.compare_gt(&current, &next)?,
                        CompOp::GreaterThanEqual => self.compare_gte(&current, &next)?,
                    };

                    if !result {
                        return Ok(Constant::Bool(false));
                    }
                    current = next;
                }
                Ok(Constant::Bool(true))
            }
            Expr::Call { func, args } => {
                if let Expr::Name { id, .. } = &**func {
                    // built-in functions
                    match id.as_str() {
                        "print" => {
                            let mut parts = Vec::with_capacity(args.len());
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                parts.push(self.to_string(&val));
                            }
                            writeln!(self.out, "{}", parts.join(" "))?;
                            return Ok(Constant::None);
                        }
                        "len" => {
                            if args.len() != 1 {
                                return Err("len() takes exactly 1 argument".into());
                            }
                            let val = self.eval_expr(&args[0])?;
                            match val {
                                Constant::Str(ref s) => return Ok(Constant::Int(s.len() as i64)),
                                _ => return Err("len() requires list or string".into()),
                            }
                        }
                        _ => {}
                    }

                    // user-defined functions
                    if let Some((params, body_rc)) = self.functions.get(id) {
                        let params = params.clone();
                        let body = body_rc.clone();

                        if params.len() != args.len() {
                            return Err(
                                format!("Function {} expects {} args", id, params.len()).into()
                            );
                        }

                        // evaluate arguments in the caller's scope
                        let mut values = Vec::with_capacity(args.len());
                        for arg in args {
                            values.push(self.eval_expr(arg)?);
                        }

                        self.burn_fuel()?;

                        // bind parameters directly so they shadow globals of the same name
                        self.enter_scope();
                        let scope = self.scopes.last_mut().expect("scope was just entered");
                        for (param, value) in params.into_iter().zip(values) {
                            scope.insert(param, value);
                        }

                        let flow = self.eval_block(&body);
                        self.exit_scope();

                        return Ok(match flow? {
                            ControlFlow::Return(value) => value,
                            _ => Constant::None,
                        });
                    }
                }
                Err("Function not found".into())
            }
            Expr::Constant { value, kind: _ } => Ok(*value.clone()),
            Expr::Subscript {
                value: _,
                slice: _,
                ctx: _,
            } => Err("subscript not supported yet".into()),
            Expr::Name { id, ctx: _ } => self.get_var(id),
            Expr::List { elts: _, ctx: _ } => Err("list not supported yet".into()),
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn set_var(
        &mut self,
        name: &str,
        value: &Constant,
    ) -> Result<Constant, Box<dyn std::error::Error>> {
        // search existing scopes
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value.clone());
                return Ok(value.clone());
            }
        }

        // check globals
        if self.globals.contains_key(name) {
            self.globals.insert(name.to_string(), value.clone());
            return Ok(value.clone());
        }

        // create in current scope or globals
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value.clone());
        } else {
            self.globals.insert(name.to_string(), value.clone());
        }

        Ok(value.clone())
    }

    fn get_var(&self, name: &str) -> Result<Constant, Box<dyn std::error::Error>> {
        // search scopes
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Ok(var.clone());
            }
        }

        // search globals
        if let Some(var) = self.globals.get(name) {
            return Ok(var.clone());
        }

        Err(format!("Variable '{}' not found", name).into())
    }

    fn delete_var(&mut self, name: &str) -> Result<(), Box<dyn std::error::Error>> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.remove(name).is_some() {
                return Ok(());
            }
        }

        if self.globals.remove(name).is_some() {
            return Ok(());
        }

        Err(format!("Variable '{}' not found", name).into())
    }

    fn is_truthy(&self, val: &Constant) -> bool {
        match val {
            Constant::Bool(b) => *b,
            Constant::None => false,
            Constant::Int(i) => *i != 0,
            Constant::Float(f) => *f != 0.0,
            Constant::Str(s) => !s.is_empty(),
            _ => false,
        }
    }

    // comparison methods

    fn compare_lt(&self, a: &Constant, b: &Constant) -> Result<bool, Box<dyn std::error::Error>> {
        match (a, b) {
            (Constant::Int(x), Constant::Int(y)) => Ok(x < y),
            (Constant::Float(x), Constant::Float(y)) => Ok(x < y),
            _ => Err("Cannot compare these types".into()),
        }
    }

    fn compare_lte(&self, a: &Constant, b: &Constant) -> Result<bool, Box<dyn std::error::Error>> {
        match (a, b) {
            (Constant::Int(x), Constant::Int(y)) => Ok(x <= y),
            (Constant::Float(x), Constant::Float(y)) => Ok(x <= y),
            _ => Err("Cannot compare these types".into()),
        }
    }

    fn compare_gt(&self, a: &Constant, b: &Constant) -> Result<bool, Box<dyn std::error::Error>> {
        match (a, b) {
            (Constant::Int(x), Constant::Int(y)) => Ok(x > y),
            (Constant::Float(x), Constant::Float(y)) => Ok(x > y),
            _ => Err("Cannot compare these types".into()),
        }
    }

    fn compare_gte(&self, a: &Constant, b: &Constant) -> Result<bool, Box<dyn std::error::Error>> {
        match (a, b) {
            (Constant::Int(x), Constant::Int(y)) => Ok(x >= y),
            (Constant::Float(x), Constant::Float(y)) => Ok(x >= y),
            _ => Err("Cannot compare these types".into()),
        }
    }

    fn to_string(&self, value: &Constant) -> String {
        match value {
            Constant::None => "none".to_string(),
            Constant::Bool(b) => if *b { "true" } else { "false" }.to_string(),
            Constant::Char(c) => c.to_string(),
            Constant::Int(i) => i.to_string(),
            Constant::Float(f) => f.to_string(),
            Constant::Str(s) => s.clone(),
            _ => "UNIMPLEMENTED".to_string(),
        }
    }
}
