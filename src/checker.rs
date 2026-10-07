//! Type checker and name resolver that validates a stone AST before it runs.
//!
//! Types are inferred, never written. Every expression gets one of `int`, `bool`, `str`, `none`,
//! or `list[T]`, and every variable keeps a single type for its whole life. Functions are
//! monomorphic: each parameter and return type is inferred from the function's body and every call
//! to it, so after `def add(a, b); ret a + b` and `add(1, 2)`, `add` is `def(int, int) -> int`.
//!
//! Scoping follows Python. Inside a function, parameters and every name the function assigns are
//! local, and other names refer to top-level variables or functions. Functions can only be
//! defined at the top level, and can be called before their definition.
//!
//! The checker only accepts programs that the interpreter and the compiler run the same way. For
//! example, conditions must be `int` or `bool`, because compiled code tests them for zero.

use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::ast::{CompOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, UnaryOp};
use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{BUILTINS, METHODS};

#[cfg(test)]
mod tests;

/// The type of a value or function, such as `int` or `def(int) -> bool`.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    None,
    List(Box<Type>),
    Function { params: Vec<Type>, ret: Box<Type> },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::None => write!(f, "none"),
            Type::List(elem) => write!(f, "list[{elem}]"),
            Type::Function { params, ret } => {
                let params: Vec<String> = params.iter().map(Type::to_string).collect();
                write!(f, "def({}) -> {ret}", params.join(", "))
            }
        }
    }
}

/// What a [`Symbol`] names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    Parameter,
    /// A variable assigned inside a function.
    Local,
    /// A variable assigned at the top level.
    Global,
}

/// A named thing a program defines, such as a function or a variable.
///
/// For example, `x = 1` at the top level defines a [`SymbolKind::Global`] named `x` of type `int`,
/// whose span is the `x` of its first assignment.
#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
    /// Where the symbol is defined: a function's name, a parameter, or a first assignment.
    pub span: Span,
    pub ty: Type,
    /// The function a parameter or local belongs to, or `None` for top-level symbols.
    pub scope: Option<String>,
}

/// One place a symbol's name appears, including where it is defined.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Reference {
    pub span: Span,
    /// The index of the symbol in [`Analysis::symbols`].
    pub symbol: usize,
}

/// Everything the checker learned about a module.
#[derive(Debug, Default)]
pub struct Analysis {
    /// Problems found, sorted by file and position.
    pub diagnostics: Vec<Diagnostic>,
    /// The type of every expression, keyed by the expression's span.
    pub types: HashMap<Span, Type>,
    pub symbols: Vec<Symbol>,
    /// Every appearance of every symbol, sorted by file and position.
    pub references: Vec<Reference>,
    /// The span of each function's whole definition, from `def` to the end of its body.
    pub functions: HashMap<String, Span>,
}

impl Analysis {
    /// Returns the reference at `pos` in `file`, if the position is on a symbol's name.
    ///
    /// For example, in `x = add(1, 2)`, any position from the `a` of `add` to just past its `d`
    /// finds the reference to `add`.
    pub fn reference_at(&self, file: FileId, pos: Pos) -> Option<&Reference> {
        self.references
            .iter()
            .find(|r| r.span.file == file && r.span.contains(pos))
    }

    /// Returns every reference to the symbol at index `symbol`, including its definition, in
    /// source order.
    pub fn references_to(&self, symbol: usize) -> impl Iterator<Item = &Reference> {
        self.references.iter().filter(move |r| r.symbol == symbol)
    }

    /// Returns the symbols that code at `pos` in `file` can refer to by their own names: the
    /// file's top-level functions and globals, plus the parameters and locals of the function `pos`
    /// is inside.
    pub fn visible_at(&self, file: FileId, pos: Pos) -> impl Iterator<Item = &Symbol> {
        let function = self
            .functions
            .iter()
            .find(|(_, span)| span.file == file && span.contains(pos))
            .map(|(name, _)| name.as_str());
        self.symbols.iter().filter(move |s| match &s.scope {
            None => s.span.file == file,
            Some(scope) => Some(scope.as_str()) == function,
        })
    }
}

pub struct TypeChecker;

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        Self
    }

    /// Returns every problem found in the module, or nothing if it is valid.
    pub fn check(&mut self, ast: &Mod) -> Vec<Diagnostic> {
        self.analyze(ast).diagnostics
    }

    /// Infers the types of the module and resolves its names, reporting any problems found.
    ///
    /// For example, analyzing `x = 1` gives a global symbol `x` of type `int` and no diagnostics.
    pub fn analyze(&mut self, ast: &Mod) -> Analysis {
        let Mod::Module { body } = ast;
        let mut inference = Inference::default();
        inference.check_module(body);
        inference.finish()
    }
}

/// A type during inference, which may still contain unsolved type variables.
#[derive(Debug, Clone, PartialEq)]
enum Ty {
    Int,
    Float,
    Bool,
    Str,
    None,
    List(Box<Ty>),
    /// An unknown type, identified by its index in [`Inference::vars`].
    Var(usize),
}

/// A requirement that a type turn out to be one of a few kinds, checked once inference is done
/// because the type may still be unknown when the requirement is found.
///
/// For example, `a + b` requires `int` or `str`, and `if x` requires `int` or `bool`.
struct Constraint {
    ty: Ty,
    /// The allowed kinds of type, where the first is the default if the type is never pinned down.
    allowed: &'static [Kind],
    span: Span,
    /// The message, with `{}` standing for the type that was found.
    message: &'static str,
}

/// A kind of type, ignoring what a list holds, for [`Constraint`]s.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Int,
    Float,
    Bool,
    Str,
    None,
    List,
}

impl Kind {
    fn of(ty: &Ty) -> Option<Kind> {
        match ty {
            Ty::Int => Some(Kind::Int),
            Ty::Float => Some(Kind::Float),
            Ty::Bool => Some(Kind::Bool),
            Ty::Str => Some(Kind::Str),
            Ty::None => Some(Kind::None),
            Ty::List(_) => Some(Kind::List),
            Ty::Var(_) => None,
        }
    }
}

/// The kinds arithmetic and ordering comparisons accept, with `int` the default.
const NUMBERS: &[Kind] = &[Kind::Int, Kind::Float];
/// What `int` and `float` convert from.
const CONVERTIBLE: &[Kind] = &[Kind::Int, Kind::Float, Kind::Str];
/// What `str` converts from.
const PRINTABLE: &[Kind] = &[Kind::Int, Kind::Float, Kind::Bool, Kind::Str];

/// A function's inferred signature.
struct FunctionInfo {
    symbol: usize,
    params: Vec<Ty>,
    ret: Ty,
}

/// The function whose body is being checked.
struct FunctionScope {
    name: String,
    /// Parameters and locals, mapped to their symbols.
    locals: HashMap<String, usize>,
    ret: Ty,
}

/// State for one run of type inference over a module.
#[derive(Default)]
struct Inference {
    /// The solution for each type variable, if known.
    vars: Vec<Option<Ty>>,
    diagnostics: Vec<Diagnostic>,
    constraints: Vec<Constraint>,
    expr_types: Vec<(Span, Ty)>,
    symbols: Vec<Symbol>,
    /// The type of each symbol in `symbols`, resolved into [`Symbol::ty`] at the end.
    symbol_types: Vec<Ty>,
    references: Vec<Reference>,
    functions: HashMap<String, FunctionInfo>,
    globals: HashMap<String, usize>,
    function: Option<FunctionScope>,
    loop_depth: usize,
    function_spans: HashMap<String, Span>,
}

/// What a name refers to where it is used.
enum Resolved {
    Variable(usize),
    Function(String),
    Builtin,
    Undefined,
}

impl Inference {
    fn fresh(&mut self) -> Ty {
        self.vars.push(None);
        Ty::Var(self.vars.len() - 1)
    }

    fn error(&mut self, span: Span, message: impl Into<String>) {
        self.diagnostics.push(Diagnostic::error(span, message));
    }

    fn add_symbol(&mut self, name: &str, kind: SymbolKind, span: Span, ty: Ty) -> usize {
        let scope = match kind {
            SymbolKind::Parameter | SymbolKind::Local => {
                self.function.as_ref().map(|f| f.name.clone())
            }
            SymbolKind::Function | SymbolKind::Global => None,
        };
        self.symbols.push(Symbol {
            name: name.to_string(),
            kind,
            span,
            // placeholder until `finish` resolves `symbol_types`
            ty: Type::None,
            scope,
        });
        self.symbol_types.push(ty);
        self.references.push(Reference {
            span,
            symbol: self.symbols.len() - 1,
        });
        self.symbols.len() - 1
    }

    fn add_reference(&mut self, span: Span, symbol: usize) {
        self.references.push(Reference { span, symbol });
    }

    ////////////////////////////////////////////////////////////////
    // unification
    ////////////////////////////////////////////////////////////////

    /// Follows solved type variables until reaching a concrete type or an unsolved variable.
    fn shallow(&self, ty: &Ty) -> Ty {
        let mut ty = ty.clone();
        while let Ty::Var(v) = ty {
            match &self.vars[v] {
                Some(solved) => ty = solved.clone(),
                None => break,
            }
        }
        ty
    }

    /// Returns `ty` with every solved variable replaced by its solution.
    fn resolve(&self, ty: &Ty) -> Ty {
        match self.shallow(ty) {
            Ty::List(elem) => Ty::List(Box::new(self.resolve(&elem))),
            other => other,
        }
    }

    fn occurs(&self, var: usize, ty: &Ty) -> bool {
        match self.shallow(ty) {
            Ty::Var(v) => v == var,
            Ty::List(elem) => self.occurs(var, &elem),
            _ => false,
        }
    }

    /// Makes two types equal by solving type variables, or fails if they cannot be.
    ///
    /// For example, unifying `list[?1]` with `list[int]` solves `?1` as `int`, while unifying
    /// `int` with `str` fails.
    fn unify(&mut self, a: &Ty, b: &Ty) -> bool {
        match (self.shallow(a), self.shallow(b)) {
            (Ty::Var(x), Ty::Var(y)) if x == y => true,
            (Ty::Var(x), other) | (other, Ty::Var(x)) => {
                if self.occurs(x, &other) {
                    return false;
                }
                self.vars[x] = Some(other);
                true
            }
            (Ty::List(x), Ty::List(y)) => self.unify(&x, &y),
            (x, y) => x == y,
        }
    }

    /// Unifies the type that was found with the type that was expected, reporting
    /// `expected X, found Y` at `span` if they differ.
    fn expect(&mut self, expected: &Ty, found: &Ty, span: Span) {
        if !self.unify(expected, found) {
            let message = format!(
                "expected {}, found {}",
                self.show(expected),
                self.show(found)
            );
            self.error(span, message);
        }
    }

    /// Formats a type for a message, showing unsolved variables as `unknown`.
    fn show(&self, ty: &Ty) -> String {
        match self.resolve(ty) {
            Ty::Int => "int".to_string(),
            Ty::Float => "float".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::Str => "str".to_string(),
            Ty::None => "none".to_string(),
            Ty::List(elem) => format!("list[{}]", self.show(&elem)),
            Ty::Var(_) => "unknown".to_string(),
        }
    }

    fn require(&mut self, ty: Ty, allowed: &'static [Kind], span: Span, message: &'static str) {
        self.constraints.push(Constraint {
            ty,
            allowed,
            span,
            message,
        });
    }

    /// Requires `ty` to be `int` or `bool`, as conditions and the operands of `not`, `and`, and
    /// `or` must be.
    fn require_condition(&mut self, ty: Ty, span: Span) {
        self.require(
            ty,
            &[Kind::Int, Kind::Bool],
            span,
            "a condition must be int or bool, found {}",
        );
    }

    ////////////////////////////////////////////////////////////////
    // declarations
    ////////////////////////////////////////////////////////////////

    fn check_module(&mut self, body: &[Stmt]) {
        // functions and globals are visible everywhere, so declare them all first
        for stmt in body {
            if let StmtKind::FunctionDef {
                name,
                name_span,
                args,
                ..
            } = &stmt.kind
            {
                self.declare_function(name, *name_span, args.args.len());
                self.function_spans.entry(name.clone()).or_insert(stmt.span);
            }
        }
        let mut assigned = vec![];
        collect_assigned(body, &mut assigned);
        for (name, span) in assigned {
            if !self.globals.contains_key(&name)
                && !self.functions.contains_key(&name)
                && !BUILTINS.contains(&name.as_str())
            {
                let ty = self.fresh();
                let symbol = self.add_symbol(&name, SymbolKind::Global, span, ty);
                self.globals.insert(name, symbol);
            }
        }

        for stmt in body {
            self.check_stmt(stmt);
        }

        // top-level code must assign globals before reading them
        let globals = self.globals.keys().cloned().collect();
        AssignmentCheck::new(globals, &mut self.diagnostics).block(body, Some(HashSet::new()));
        for stmt in body {
            if let StmtKind::FunctionDef { args, body, .. } = &stmt.kind {
                let params: HashSet<String> = args.args.iter().map(|a| a.arg.clone()).collect();
                let mut locals = params.clone();
                let mut assigned = vec![];
                collect_assigned(body, &mut assigned);
                locals.extend(assigned.into_iter().map(|(name, _)| name));
                AssignmentCheck::new(locals, &mut self.diagnostics).block(body, Some(params));
            }
        }
    }

    fn declare_function(&mut self, name: &str, name_span: Span, arity: usize) {
        if self.functions.contains_key(name) {
            self.error(name_span, format!("function '{name}' is already defined"));
            return;
        }
        if BUILTINS.contains(&name) {
            self.error(name_span, format!("cannot redefine builtin '{name}'"));
            return;
        }
        let params = (0..arity).map(|_| self.fresh()).collect();
        let ret = self.fresh();
        // the symbol's type is rebuilt from `params` and `ret` in `finish`
        let symbol = self.add_symbol(name, SymbolKind::Function, name_span, Ty::None);
        self.functions.insert(
            name.to_string(),
            FunctionInfo {
                symbol,
                params,
                ret,
            },
        );
    }

    ////////////////////////////////////////////////////////////////
    // statements
    ////////////////////////////////////////////////////////////////

    fn check_block(&mut self, body: &[Stmt]) {
        for stmt in body {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name,
                name_span,
                args,
                body,
                ..
            } => {
                if self.function.is_some() || self.loop_depth > 0 {
                    self.error(*name_span, "functions can only be defined at the top level");
                    return;
                }
                self.check_function(name, *name_span, args, body);
            }
            StmtKind::Return { value } => {
                let found = match value {
                    Some(value) => self.infer(value),
                    None => Ty::None,
                };
                if let Some(function) = &self.function {
                    let (name, ret) = (function.name.clone(), function.ret.clone());
                    if !self.unify(&ret, &found) {
                        let span = value.as_ref().map_or(stmt.span, |v| v.span);
                        let message = format!(
                            "'{name}' returns {}, but this is {}",
                            self.show(&ret),
                            self.show(&found)
                        );
                        self.error(span, message);
                    }
                }
            }
            StmtKind::Delete { targets } => {
                for target in targets {
                    self.infer(target);
                }
            }
            StmtKind::Assign { targets, value } => {
                let found = self.infer(value);
                for target in targets {
                    self.check_target(target, &found, value.span);
                }
            }
            StmtKind::For { target, iter, body } => {
                let elem = match range_args(iter) {
                    Some(args) => self.check_range(iter, args),
                    None => {
                        let elem = self.fresh();
                        let iter_ty = self.infer(iter);
                        self.expect(&Ty::List(Box::new(elem.clone())), &iter_ty, iter.span);
                        elem
                    }
                };
                if matches!(target.kind, ExprKind::Name { .. }) {
                    self.check_target(target, &elem, target.span);
                } else {
                    self.error(target.span, "a 'for' loop's variable must be a name");
                }
                self.check_loop_body(body);
            }
            StmtKind::While { test, body } => {
                let ty = self.infer(test);
                self.require_condition(ty, test.span);
                self.check_loop_body(body);
            }
            StmtKind::If { test, body, orelse } => {
                let ty = self.infer(test);
                self.require_condition(ty, test.span);
                self.check_block(body);
                self.check_block(orelse);
            }
            StmtKind::Expr { value } => {
                self.infer(value);
            }
            StmtKind::Break => {
                if self.loop_depth == 0 {
                    self.error(stmt.span, "'break' outside a loop");
                }
            }
            StmtKind::Continue => {
                if self.loop_depth == 0 {
                    self.error(stmt.span, "'cont' outside a loop");
                }
            }
            // the linker removes every top-level `use`, so any left over is nested
            StmtKind::Use { .. } => {
                self.error(stmt.span, "use is only allowed at the top level of a file");
            }
        }
    }

    /// Checks the `range(...)` of a `for` loop, whose one or two bounds must be ints, and returns
    /// the type of the loop variable.
    fn check_range(&mut self, iter: &Expr, args: &[Expr]) -> Ty {
        if let ExprKind::Call { func, .. } = &iter.kind {
            self.expr_types.push((func.span, Ty::None));
        }
        for arg in args {
            let ty = self.infer(arg);
            self.expect(&Ty::Int, &ty, arg.span);
        }
        if !(1..=2).contains(&args.len()) {
            let verb = if args.len() == 1 { "was" } else { "were" };
            self.error(
                iter.span,
                format!(
                    "'range' takes 1 or 2 arguments, but {} {verb} given",
                    args.len()
                ),
            );
        }
        Ty::Int
    }

    fn check_loop_body(&mut self, body: &[Stmt]) {
        self.loop_depth += 1;
        self.check_block(body);
        self.loop_depth -= 1;
    }

    fn check_function(
        &mut self,
        name: &str,
        name_span: Span,
        args: &crate::ast::Arguments,
        body: &[Stmt],
    ) {
        let Some(info) = self.functions.get(name) else {
            return;
        };
        // a redefinition was already reported, so skip its body rather than check it twice
        if self.symbols[info.symbol].span != name_span {
            return;
        }
        let (params, ret) = (info.params.clone(), info.ret.clone());

        self.function = Some(FunctionScope {
            name: name.to_string(),
            locals: HashMap::new(),
            ret: ret.clone(),
        });
        for (arg, ty) in args.args.iter().zip(params) {
            if self.local(&arg.arg).is_some() {
                self.error(arg.span, format!("duplicate parameter '{}'", arg.arg));
                continue;
            }
            let symbol = self.add_symbol(&arg.arg, SymbolKind::Parameter, arg.span, ty);
            self.set_local(&arg.arg, symbol);
        }
        let mut assigned = vec![];
        collect_assigned(body, &mut assigned);
        for (local, span) in assigned {
            if self.local(&local).is_none() {
                let ty = self.fresh();
                let symbol = self.add_symbol(&local, SymbolKind::Local, span, ty);
                self.set_local(&local, symbol);
            }
        }

        let saved_loops = std::mem::take(&mut self.loop_depth);
        self.check_block(body);
        self.loop_depth = saved_loops;

        // falling off the end returns none, which must agree with every `ret`
        if !always_exits(body) && !self.unify(&ret, &Ty::None) {
            self.error(
                name_span,
                format!("'{name}' does not return a value on every path"),
            );
        }
        self.function = None;
    }

    fn local(&self, name: &str) -> Option<usize> {
        self.function.as_ref()?.locals.get(name).copied()
    }

    fn set_local(&mut self, name: &str, symbol: usize) {
        if let Some(function) = &mut self.function {
            function.locals.insert(name.to_string(), symbol);
        }
    }

    /// Checks one assignment target against the type of the value assigned to it.
    fn check_target(&mut self, target: &Expr, found: &Ty, value_span: Span) {
        match &target.kind {
            ExprKind::Name { id, .. } => {
                let symbol = match self.resolve_name(id) {
                    Resolved::Variable(symbol) => symbol,
                    Resolved::Function(_) => {
                        self.error(target.span, format!("cannot assign to function '{id}'"));
                        return;
                    }
                    Resolved::Builtin => {
                        self.error(target.span, format!("cannot assign to builtin '{id}'"));
                        return;
                    }
                    // unreachable, since every assigned name is declared up front
                    Resolved::Undefined => return,
                };
                if self.symbols[symbol].span != target.span {
                    self.add_reference(target.span, symbol);
                }
                let expected = self.symbol_types[symbol].clone();
                self.expr_types.push((target.span, expected.clone()));
                if !self.unify(&expected, found) {
                    let message = format!(
                        "cannot assign {} to '{id}', which is {}",
                        self.show(found),
                        self.show(&expected)
                    );
                    self.error(value_span, message);
                }
            }
            ExprKind::Subscript { value, slice, .. } => {
                let elem = self.subscript(value, slice);
                self.expect(&elem, found, value_span);
            }
            _ => self.error(target.span, "cannot assign to this expression"),
        }
    }

    /// Finds what `name` refers to from the current scope.
    fn resolve_name(&self, name: &str) -> Resolved {
        if let Some(symbol) = self.local(name) {
            return Resolved::Variable(symbol);
        }
        if let Some(&symbol) = self.globals.get(name) {
            return Resolved::Variable(symbol);
        }
        if self.functions.contains_key(name) {
            return Resolved::Function(name.to_string());
        }
        if BUILTINS.contains(&name) {
            return Resolved::Builtin;
        }
        Resolved::Undefined
    }

    ////////////////////////////////////////////////////////////////
    // expressions
    ////////////////////////////////////////////////////////////////

    /// Infers the type of an expression, recording it by span.
    fn infer(&mut self, expr: &Expr) -> Ty {
        let ty = self.infer_kind(expr);
        self.expr_types.push((expr.span, ty.clone()));
        ty
    }

    fn infer_kind(&mut self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Constant { value, .. } => match **value {
                Constant::Bool(_) => Ty::Bool,
                Constant::Str(_) | Constant::Char(_) => Ty::Str,
                Constant::None => Ty::None,
                Constant::Float(_) | Constant::F32(_) | Constant::F64(_) | Constant::Decimal(_) => {
                    Ty::Float
                }
                _ => Ty::Int,
            },
            ExprKind::Name { id, .. } => match self.resolve_name(id) {
                Resolved::Variable(symbol) => {
                    self.add_reference(expr.span, symbol);
                    self.symbol_types[symbol].clone()
                }
                Resolved::Function(name) => {
                    let symbol = self.functions[&name].symbol;
                    self.add_reference(expr.span, symbol);
                    self.error(
                        expr.span,
                        format!("'{id}' is a function, so it can only be called"),
                    );
                    self.fresh()
                }
                Resolved::Builtin => {
                    self.error(
                        expr.span,
                        format!("'{id}' is a function, so it can only be called"),
                    );
                    self.fresh()
                }
                Resolved::Undefined => {
                    self.error(expr.span, format!("undefined name '{id}'"));
                    self.fresh()
                }
            },
            ExprKind::BinOp {
                op: Operator::Power,
                left,
                right,
            } => {
                let l = self.infer(left);
                let r = self.infer(right);
                // an unknown exponent, such as a parameter, becomes an int
                if matches!(self.resolve(&r), Ty::Var(_)) {
                    self.expect(&Ty::Int, &r, right.span);
                }
                self.require(
                    l.clone(),
                    NUMBERS,
                    expr.span,
                    "'**' needs an int or float base, found {}",
                );
                self.require(
                    r,
                    &[Kind::Int],
                    right.span,
                    "'**' needs an int exponent, found {}",
                );
                l
            }
            ExprKind::BinOp { op, left, right } => {
                let l = self.infer(left);
                let r = self.infer(right);
                self.expect(&l, &r, right.span);
                let (allowed, message): (&'static [Kind], _) = match op {
                    Operator::Add => (
                        &[Kind::Int, Kind::Float, Kind::Str],
                        "'+' needs int, float, or str operands, found {}",
                    ),
                    Operator::Subtract => (NUMBERS, "'-' needs int or float operands, found {}"),
                    Operator::Multiply => (NUMBERS, "'*' needs int or float operands, found {}"),
                    Operator::Divide => (NUMBERS, "'/' needs int or float operands, found {}"),
                    Operator::Modulo => (NUMBERS, "'%' needs int or float operands, found {}"),
                    Operator::Power => unreachable!("matched above"),
                };
                self.require(l.clone(), allowed, expr.span, message);
                l
            }
            ExprKind::UnaryOp { op, operand } => {
                let ty = self.infer(operand);
                match op {
                    UnaryOp::Not => {
                        self.require_condition(ty, operand.span);
                        Ty::Bool
                    }
                    UnaryOp::UnaryAdd | UnaryOp::UnarySub => {
                        let message = match op {
                            UnaryOp::UnaryAdd => "'+' needs an int or float operand, found {}",
                            _ => "'-' needs an int or float operand, found {}",
                        };
                        self.require(ty.clone(), NUMBERS, operand.span, message);
                        ty
                    }
                }
            }
            ExprKind::BoolOp { op: _, values } => {
                // `a or b` evaluates to one of its operands, so they share a type
                let first = self.infer(&values[0]);
                self.require_condition(first.clone(), values[0].span);
                for value in &values[1..] {
                    let ty = self.infer(value);
                    self.expect(&first, &ty, value.span);
                }
                first
            }
            ExprKind::Compare {
                left,
                ops,
                comparators,
            } => {
                let mut prev = self.infer(left);
                for (op, comparator) in ops.iter().zip(comparators) {
                    let ty = self.infer(comparator);
                    self.expect(&prev, &ty, comparator.span);
                    let (allowed, message): (&'static [Kind], _) = match op {
                        // compiled code compares lists by address, so only compare values
                        CompOp::Equal | CompOp::NotEqual => (
                            &[Kind::Int, Kind::Float, Kind::Bool, Kind::Str, Kind::None],
                            "'==' and '!=' cannot compare {}",
                        ),
                        CompOp::LessThan => (NUMBERS, "'<' needs int or float operands, found {}"),
                        CompOp::LessThanEqual => {
                            (NUMBERS, "'<=' needs int or float operands, found {}")
                        }
                        CompOp::GreaterThan => {
                            (NUMBERS, "'>' needs int or float operands, found {}")
                        }
                        CompOp::GreaterThanEqual => {
                            (NUMBERS, "'>=' needs int or float operands, found {}")
                        }
                    };
                    self.require(ty.clone(), allowed, expr.span, message);
                    prev = ty;
                }
                Ty::Bool
            }
            ExprKind::Call { func, args } => self.infer_call(expr, func, args),
            ExprKind::Attribute { value, attr, .. } => {
                self.infer(value);
                self.error(
                    expr.span,
                    format!("'{attr}' is a method, so it can only be called"),
                );
                self.fresh()
            }
            ExprKind::Subscript { value, slice, .. } => self.subscript(value, slice),
            ExprKind::List { elts, .. } => {
                let elem = self.fresh();
                for elt in elts {
                    let ty = self.infer(elt);
                    self.expect(&elem, &ty, elt.span);
                }
                Ty::List(Box::new(elem))
            }
        }
    }

    /// Infers `value[slice]`, which must index a list with an int.
    fn subscript(&mut self, value: &Expr, slice: &Expr) -> Ty {
        let elem = self.fresh();
        let value_ty = self.infer(value);
        self.expect(&Ty::List(Box::new(elem.clone())), &value_ty, value.span);
        let index = self.infer(slice);
        self.expect(&Ty::Int, &index, slice.span);
        elem
    }

    fn infer_call(&mut self, call: &Expr, func: &Expr, args: &[Expr]) -> Ty {
        if let ExprKind::Attribute { value, attr, .. } = &func.kind {
            return self.infer_method(call, func, value, attr, args);
        }
        let ExprKind::Name { id, .. } = &func.kind else {
            self.error(func.span, "only functions can be called, by name");
            for arg in args {
                self.infer(arg);
            }
            return self.fresh();
        };
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();

        match self.resolve_name(id) {
            Resolved::Function(name) => {
                let info = &self.functions[&name];
                let (symbol, params, ret) = (info.symbol, info.params.clone(), info.ret.clone());
                self.add_reference(func.span, symbol);
                self.expr_types.push((func.span, Ty::None));
                if params.len() != args.len() {
                    self.arity_error(call.span, id, &params.len().to_string(), args.len());
                    return ret;
                }
                for ((param, arg_ty), arg) in params.iter().zip(&arg_types).zip(args) {
                    self.expect(param, arg_ty, arg.span);
                }
                ret
            }
            Resolved::Builtin => self.infer_builtin(call, id, args, &arg_types),
            Resolved::Variable(symbol) => {
                self.add_reference(func.span, symbol);
                self.error(func.span, format!("'{id}' is not a function"));
                self.fresh()
            }
            Resolved::Undefined if METHODS.contains(&id.as_str()) => {
                let args = if id == "len" { "" } else { "..." };
                self.error(
                    func.span,
                    format!("'{id}' is a method, so call it as value.{id}({args})"),
                );
                self.fresh()
            }
            Resolved::Undefined => {
                self.error(func.span, format!("undefined function '{id}'"));
                self.fresh()
            }
        }
    }

    /// Infers the method call `receiver.name(args)`, evaluating the receiver before the arguments.
    ///
    /// For example, `xs.append(1)` requires `xs` to be a `list[int]` and has type `none`.
    fn infer_method(
        &mut self,
        call: &Expr,
        func: &Expr,
        receiver: &Expr,
        name: &str,
        args: &[Expr],
    ) -> Ty {
        let receiver_ty = self.infer(receiver);
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
        match name {
            "len" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                self.require(
                    receiver_ty,
                    &[Kind::Str, Kind::List],
                    receiver.span,
                    "'len' needs a str or list, found {}",
                );
                Ty::Int
            }
            "append" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                    return Ty::None;
                }
                let elem = self.fresh();
                let list = Ty::List(Box::new(elem.clone()));
                if self.unify(&list, &receiver_ty) {
                    self.expect(&elem, &arg_types[0], args[0].span);
                } else {
                    // the element pins down the message, as in `expected list[int]`
                    self.unify(&elem, &arg_types[0]);
                    self.expect(&list, &receiver_ty, receiver.span);
                }
                Ty::None
            }
            "strip" | "split" => {
                let message = match name {
                    "strip" => "'strip' needs a str, found {}",
                    _ => "'split' needs a str, found {}",
                };
                self.require(receiver_ty, &[Kind::Str], receiver.span, message);
                match (name, args) {
                    ("strip", []) | ("split", []) => {}
                    ("split", [separator]) => self.expect(&Ty::Str, &arg_types[0], separator.span),
                    ("strip", _) => self.arity_error(call.span, name, "0", args.len()),
                    _ => self.arity_error(call.span, name, "0 or 1", args.len()),
                }
                if name == "strip" {
                    Ty::Str
                } else {
                    Ty::List(Box::new(Ty::Str))
                }
            }
            _ => {
                // the name is the last token of `func`
                let end = func.span.end;
                let start = Pos::new(end.line, end.col - name.chars().count());
                self.error(
                    Span::new(start, end).in_file(func.span.file),
                    format!("there is no method '{name}'"),
                );
                self.fresh()
            }
        }
    }

    fn infer_builtin(&mut self, call: &Expr, name: &str, args: &[Expr], arg_types: &[Ty]) -> Ty {
        match name {
            "print" => Ty::None,
            "int" | "float" | "str" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                } else {
                    let (allowed, message) = match name {
                        "int" => (CONVERTIBLE, "'int' needs an int, float, or str, found {}"),
                        "float" => (CONVERTIBLE, "'float' needs an int, float, or str, found {}"),
                        _ => (
                            PRINTABLE,
                            "'str' needs an int, float, bool, or str, found {}",
                        ),
                    };
                    self.require(arg_types[0].clone(), allowed, args[0].span, message);
                }
                match name {
                    "int" => Ty::Int,
                    "float" => Ty::Float,
                    _ => Ty::Str,
                }
            }
            "input" => {
                match args {
                    [] => {}
                    [prompt] => self.expect(&Ty::Str, &arg_types[0], prompt.span),
                    _ => self.arity_error(call.span, name, "0 or 1", args.len()),
                }
                Ty::Str
            }
            "eof" | "args" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                if name == "eof" {
                    Ty::Bool
                } else {
                    Ty::List(Box::new(Ty::Str))
                }
            }
            "range" => {
                self.error(
                    call.span,
                    "'range' can only be used as the iterable of a 'for' loop",
                );
                Ty::List(Box::new(Ty::Int))
            }
            _ => unreachable!("builtin '{name}' has no type rule"),
        }
    }

    /// Reports a call to `name` with `given` arguments when it takes `expected`, such as `1` or
    /// `0 or 1`.
    fn arity_error(&mut self, span: Span, name: &str, expected: &str, given: usize) {
        let plural = if expected == "1" { "" } else { "s" };
        let verb = if given == 1 { "was" } else { "were" };
        self.error(
            span,
            format!("'{name}' takes {expected} argument{plural}, but {given} {verb} given"),
        );
    }

    ////////////////////////////////////////////////////////////////
    // results
    ////////////////////////////////////////////////////////////////

    /// Checks the deferred constraints, fills in types nothing pinned down, and builds the
    /// [`Analysis`].
    fn finish(mut self) -> Analysis {
        for constraint in std::mem::take(&mut self.constraints) {
            let ty = self.shallow(&constraint.ty);
            let Some(kind) = Kind::of(&ty) else {
                // never pinned down, so take the default
                let default = match constraint.allowed[0] {
                    Kind::Int => Ty::Int,
                    Kind::Float => Ty::Float,
                    Kind::Bool => Ty::Bool,
                    Kind::Str => Ty::Str,
                    Kind::None => Ty::None,
                    Kind::List => Ty::List(Box::new(self.fresh())),
                };
                self.unify(&ty, &default);
                continue;
            };
            if !constraint.allowed.contains(&kind) {
                let message = constraint.message.replace("{}", &self.show(&ty));
                self.error(constraint.span, message);
            }
        }

        let mut analysis = Analysis::default();
        for (span, ty) in std::mem::take(&mut self.expr_types) {
            let ty = self.finalize(&ty);
            analysis.types.insert(span, ty);
        }
        for (i, symbol) in self.symbols.iter().enumerate() {
            let ty = if symbol.kind == SymbolKind::Function {
                let info = &self.functions[&symbol.name];
                Type::Function {
                    params: info.params.iter().map(|p| self.finalize(p)).collect(),
                    ret: Box::new(self.finalize(&info.ret)),
                }
            } else {
                self.finalize(&self.symbol_types[i])
            };
            analysis.symbols.push(Symbol {
                ty,
                ..symbol.clone()
            });
        }
        analysis.references = self.references;
        analysis
            .references
            .sort_by_key(|r| (r.span.file, r.span.start));
        analysis.functions = self.function_spans;
        self.diagnostics
            .sort_by_key(|d| (d.span.file, d.span.start));
        analysis.diagnostics = self.diagnostics;
        analysis
    }

    /// Converts an inference type to a final [`Type`], treating anything still unknown as `int`.
    ///
    /// For example, the parameter of `def ignore(a); ret 0`, which is never called, is `int`.
    fn finalize(&self, ty: &Ty) -> Type {
        match self.resolve(ty) {
            Ty::Int | Ty::Var(_) => Type::Int,
            Ty::Float => Type::Float,
            Ty::Bool => Type::Bool,
            Ty::Str => Type::Str,
            Ty::None => Type::None,
            Ty::List(elem) => Type::List(Box::new(self.finalize(&elem))),
        }
    }
}

/// The names definitely assigned at some point in a program, or `None` if that point can never be
/// reached, such as right after a `ret`.
type Assigned = Option<HashSet<String>>;

/// Checks that variables are assigned on every path before they are read, since the interpreter
/// stops with an error on such a read while compiled code would read a leftover value.
///
/// For example, after `if c; x = 1`, reading `x` is an error, but after `if c; x = 1` followed by
/// `else; x = 2` it is fine.
struct AssignmentCheck<'a> {
    /// The variables this check covers: the globals for top-level code, or a function's locals.
    tracked: HashSet<String>,
    /// Variables already reported, so each is reported once.
    reported: HashSet<String>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl<'a> AssignmentCheck<'a> {
    fn new(tracked: HashSet<String>, diagnostics: &'a mut Vec<Diagnostic>) -> Self {
        AssignmentCheck {
            tracked,
            reported: HashSet::new(),
            diagnostics,
        }
    }

    /// Checks a block that starts with `state` and returns the state after it.
    fn block(&mut self, body: &[Stmt], mut state: Assigned) -> Assigned {
        for stmt in body {
            state = self.stmt(stmt, state);
        }
        state
    }

    fn stmt(&mut self, stmt: &Stmt, state: Assigned) -> Assigned {
        // unreachable code cannot read anything
        let mut assigned = state?;
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                self.reads(value, &assigned);
                for target in targets {
                    match &target.kind {
                        ExprKind::Name { id, .. } => {
                            assigned.insert(id.clone());
                        }
                        _ => self.reads(target, &assigned),
                    }
                }
                Some(assigned)
            }
            StmtKind::Expr { value } => {
                self.reads(value, &assigned);
                Some(assigned)
            }
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.reads(value, &assigned);
                }
                None
            }
            StmtKind::Delete { targets } => {
                for target in targets {
                    self.reads(target, &assigned);
                }
                Some(assigned)
            }
            StmtKind::If { test, body, orelse } => {
                self.reads(test, &assigned);
                let then = self.block(body, Some(assigned.clone()));
                let otherwise = self.block(orelse, Some(assigned));
                match (then, otherwise) {
                    (None, other) | (other, None) => other,
                    (Some(a), Some(b)) => Some(a.intersection(&b).cloned().collect()),
                }
            }
            StmtKind::While { test, body } => {
                self.reads(test, &assigned);
                // the body might not run, so what it assigns does not count afterward
                self.block(body, Some(assigned.clone()));
                if is_always_true(test) && !breaks(body) {
                    None
                } else {
                    Some(assigned)
                }
            }
            StmtKind::For { target, iter, body } => {
                self.reads(iter, &assigned);
                let mut inside = assigned.clone();
                if let ExprKind::Name { id, .. } = &target.kind {
                    inside.insert(id.clone());
                }
                self.block(body, Some(inside));
                Some(assigned)
            }
            StmtKind::Break | StmtKind::Continue => None,
            StmtKind::FunctionDef { .. } | StmtKind::Use { .. } => Some(assigned),
        }
    }

    /// Reports every tracked variable `expr` reads that is not assigned yet.
    fn reads(&mut self, expr: &Expr, assigned: &HashSet<String>) {
        match &expr.kind {
            ExprKind::Name { id, .. } => {
                if self.tracked.contains(id)
                    && !assigned.contains(id)
                    && self.reported.insert(id.clone())
                {
                    self.diagnostics.push(Diagnostic::error(
                        expr.span,
                        format!("'{id}' might be used before it is assigned"),
                    ));
                }
            }
            ExprKind::BoolOp { values, .. } => {
                for value in values {
                    self.reads(value, assigned);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.reads(left, assigned);
                self.reads(right, assigned);
            }
            ExprKind::UnaryOp { operand, .. } => self.reads(operand, assigned),
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.reads(left, assigned);
                for comparator in comparators {
                    self.reads(comparator, assigned);
                }
            }
            ExprKind::Call { func, args } => {
                // a called name is a function, but a method's receiver is read
                if let ExprKind::Attribute { .. } = &func.kind {
                    self.reads(func, assigned);
                }
                for arg in args {
                    self.reads(arg, assigned);
                }
            }
            ExprKind::Attribute { value, .. } => self.reads(value, assigned),
            ExprKind::Subscript { value, slice, .. } => {
                self.reads(value, assigned);
                self.reads(slice, assigned);
            }
            ExprKind::List { elts, .. } => {
                for elt in elts {
                    self.reads(elt, assigned);
                }
            }
            ExprKind::Constant { .. } => {}
        }
    }
}

/// Returns the arguments of `iter` if it is a call to `range`, as in `for i in range(3);`.
///
/// Both backends count through a `range` directly rather than building a list, so it only has
/// meaning as a `for` loop's iterable.
pub fn range_args(iter: &Expr) -> Option<&[Expr]> {
    match &iter.kind {
        ExprKind::Call { func, args } => match &func.kind {
            ExprKind::Name { id, .. } if id == "range" => Some(args),
            _ => None,
        },
        _ => None,
    }
}

/// Collects every name assigned in `body`, with the span of its first assignment, without
/// looking inside function definitions.
///
/// For example, for `x = 1` followed by `if x; y = x`, this collects `x` and then `y`.
pub(crate) fn collect_assigned(body: &[Stmt], out: &mut Vec<(String, Span)>) {
    let add = |target: &Expr, out: &mut Vec<(String, Span)>| {
        if let ExprKind::Name { id, .. } = &target.kind
            && !out.iter().any(|(name, _)| name == id)
        {
            out.push((id.clone(), target.span));
        }
    };
    for stmt in body {
        match &stmt.kind {
            StmtKind::Assign { targets, .. } => {
                for target in targets {
                    add(target, out);
                }
            }
            StmtKind::For { target, body, .. } => {
                add(target, out);
                collect_assigned(body, out);
            }
            StmtKind::While { body, .. } => collect_assigned(body, out),
            StmtKind::If { body, orelse, .. } => {
                collect_assigned(body, out);
                collect_assigned(orelse, out);
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Return { .. }
            | StmtKind::Delete { .. }
            | StmtKind::Expr { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }
}

/// Returns whether running `body` can never reach its end, because every path returns or loops
/// forever.
///
/// For example, `if x; ret 1` followed by `else; ret 2` always exits, and so does `while 1;` with no
/// `break`, but `if x; ret 1` alone does not.
fn always_exits(body: &[Stmt]) -> bool {
    body.iter().any(|stmt| match &stmt.kind {
        StmtKind::Return { .. } => true,
        StmtKind::If { body, orelse, .. } => always_exits(body) && always_exits(orelse),
        StmtKind::While { test, body } => is_always_true(test) && !breaks(body),
        _ => false,
    })
}

/// Returns whether `test` is a literal that is always truthy, like `1` or `true`.
fn is_always_true(test: &Expr) -> bool {
    match &test.kind {
        ExprKind::Constant { value, .. } => match **value {
            Constant::Bool(b) => b,
            Constant::Int(n) => n != 0,
            _ => false,
        },
        _ => false,
    }
}

/// Returns whether `body` contains a `break` for the loop it belongs to, not counting `break`s of
/// loops nested inside it.
fn breaks(body: &[Stmt]) -> bool {
    body.iter().any(|stmt| match &stmt.kind {
        StmtKind::Break => true,
        StmtKind::If { body, orelse, .. } => breaks(body) || breaks(orelse),
        _ => false,
    })
}
