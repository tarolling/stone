//! Stone Interpreter

use crate::types::{BoolOp, CompOp, Constant, Expr, Mod, Operator, Stmt, UnaryOp};
use std::collections::HashMap;
use std::rc::Rc;

pub enum ControlFlow {
    None,
    Return(Constant),
    Break,
    Continue,
}

pub struct Interpreter {
    // Global variables
    globals: HashMap<String, Constant>,
    /// Stack of current scopes
    scopes: Vec<HashMap<String, Constant>>,
    /// list of functions (name -> (params, body))
    functions: HashMap<String, (Vec<String>, Rc<Vec<Stmt>>)>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            scopes: vec![],
            functions: HashMap::new(),
        }
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

    ///     stmt =
    ///
    ///           | FunctionDef(identifier name, arguments args, stmt* body, expr? returns)
    ///           | Return(expr? value)
    ///           | Delete(expr* targets)
    ///           | Assign(expr* targets, expr value)
    ///           | For(expr target, expr iter, stmt* body, stmt* orelse)
    ///           | While(expr test, stmt* body, stmt* orelse)
    ///           | If(expr test, stmt* body, stmt* orelse)
    ///           | Expr(expr value)
    ///           | Break | Continue
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, Box<dyn std::error::Error>> {
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
                let test = self.eval_expr(test)?;
                while self.is_truthy(&test) {
                    for stmt in body {
                        self.eval_stmt(stmt)?;
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::If { test, body, orelse } => {
                let test = self.eval_expr(test)?;
                if self.is_truthy(&test) {
                    for stmt in body {
                        self.eval_stmt(stmt)?;
                    }
                } else {
                    for stmt in orelse {
                        self.eval_stmt(stmt)?;
                    }
                }
                Ok(ControlFlow::None)
            }
            Stmt::Expr { value } => {
                self.eval_expr(value)?;
                Ok(ControlFlow::None)
            }
            Stmt::Break => Ok(ControlFlow::Break),
            Stmt::Continue => Ok(ControlFlow::Continue),
        }
    }

    /// expr = BoolOp(boolop op, expr* values)
    ///  | BinOp(expr left, operator op, expr right)
    ///  | UnaryOp(unaryop op, expr operand)
    ///  | Compare(expr left, cmpop* ops, expr* comparators)
    ///  | Call(expr func, expr* args)
    ///  | Constant(constant value, string? kind)
    ///  | Name(identifier id, expr_context ctx)
    ///  | List(expr* elts, expr_context ctx)
    fn eval_expr(&mut self, expr: &Expr) -> Result<Constant, Box<dyn std::error::Error>> {
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
                        Operator::Add => Ok(Constant::Int(l + r)),
                        Operator::Subtract => Ok(Constant::Int(l - r)),
                        Operator::Multiply => Ok(Constant::Int(l * r)),
                        Operator::Divide => Ok(Constant::Int(l / r)),
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
                        Constant::Int(i) => Ok(Constant::Int(-i)),
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
                    // Built-in functions
                    match id.as_str() {
                        "print" => {
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                print!("{} ", self.to_string(&val));
                            }
                            println!();
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

                    // User-defined functions
                    if let Some((params, body_rc)) = self.functions.get(id) {
                        let params = params.clone();
                        let body = body_rc.clone();

                        if params.len() != args.len() {
                            return Err(
                                format!("Function {} expects {} args", id, params.len()).into()
                            );
                        }

                        self.enter_scope();

                        for (param, arg) in params.iter().zip(args.iter()) {
                            let val = self.eval_expr(arg)?;
                            self.set_var(param, &val)?;
                        }

                        let mut result = Constant::None;
                        for stmt in body.iter() {
                            if let ControlFlow::Return(val) = self.eval_stmt(stmt)? {
                                result = val;
                                break;
                            }
                        }

                        self.exit_scope();
                        return Ok(result);
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
        // Search existing scopes
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value.clone());
                return Ok(value.clone());
            }
        }

        // Check globals
        if self.globals.contains_key(name) {
            self.globals.insert(name.to_string(), value.clone());
            return Ok(value.clone());
        }

        // Create in current scope or globals
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), value.clone());
        } else {
            self.globals.insert(name.to_string(), value.clone());
        }

        Ok(value.clone())
    }

    fn get_var(&self, name: &str) -> Result<Constant, Box<dyn std::error::Error>> {
        // Search scopes
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Ok(var.clone());
            }
        }

        // Search globals
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

    // COMPARISON METHODS

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
