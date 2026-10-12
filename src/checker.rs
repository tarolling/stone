//! Type checker and name resolver that validates a stone AST before it runs.
//!
//! Function signatures are written, and every other type is inferred. Every expression gets one of
//! `int`, `float`, `bool`, `str`, `none`, or `list[T]`, and every variable keeps a single type for
//! its whole life. Each parameter and return type comes from the function's signature, so
//! `fn add(a: int, b: int) -> int` is `fn(int, int) -> int`, and a function without `->` returns
//! `none`. Functions are monomorphic, and every call is checked against the signature.
//!
//! Scoping follows Python. Inside a function, parameters and every name the function assigns are
//! local, and other names refer to top-level variables or functions. Functions can only be
//! defined at the top level, and can be called before their definition.
//!
//! Lists are values: assigning one or passing it to a function behaves as a copy. So a change,
//! `xs[i] = v` or `xs.append(v)`, is a statement of its own that changes a variable or an element
//! of one, and a function changes only its own variables, never a global.
//!
//! The checker only accepts programs that the interpreter and the compiler run the same way. For
//! example, conditions must be `int` or `bool`, because compiled code tests them for zero.

use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::ast::{
    Arguments, CompOp, Constant, Expr, ExprKind, Mod, Operator, Stmt, StmtKind, TypeExpr,
    TypeExprKind, UnaryOp,
};
use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{BUILTINS, METHODS, changes_receiver, is_builtin};

#[cfg(test)]
mod tests;

/// The type of a value or function, such as `int` or `fn(int) -> bool`.
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
                write!(f, "fn({}) -> {ret}", params.join(", "))
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
    /// The span of each function's whole definition, from `fn` to the end of its body.
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

/// An arithmetic operation whose operands may be lists, checked once its operands are known to be
/// lists or not, since `xs * k` takes a list and a number while `a * b` takes two numbers.
///
/// For example, after `rows = []`, nothing is known about `rows[0]` in `y = rows[0] * 2.0` until
/// `rows.append([1.5])` is checked, which makes the result `list[float]`.
struct Arithmetic {
    op: ArithmeticOp,
    left: Ty,
    /// The right operand, or `None` for a unary operation.
    right: Option<Ty>,
    result: Ty,
    /// The whole expression.
    span: Span,
    /// The right operand, or the only one.
    operand_span: Span,
    /// Whether `left` or `right` is a list literal of one element multiplied, as in `[0] * n`,
    /// which Python reads as repeating it.
    literal_left: bool,
    literal_right: bool,
}

/// What an unknown operand of arithmetic can still turn out to be, which unification enforces so
/// that a wrong value is reported where it is first used rather than once the operand is known.
///
/// For example, after `xs = []`, `xs[0] * 2` makes the element an int or a list of ints, so a
/// later `xs.append("a")` is an error at `"a"`.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Bound {
    /// The number, or a list of it, as `n` is in `n * 2`, where the number is `int` or `float`.
    NumberOrList(NumberTy),
    /// Any number or list of numbers, as `a` is in `a * b`, or also a string for `+`.
    Numeric { strings: bool },
}

/// A number type, for [`Bound::NumberOrList`].
#[derive(Clone, Copy, Debug, PartialEq)]
enum NumberTy {
    Int,
    Float,
}

impl NumberTy {
    fn of(ty: &Ty) -> Option<NumberTy> {
        match ty {
            Ty::Int => Some(NumberTy::Int),
            Ty::Float => Some(NumberTy::Float),
            _ => None,
        }
    }

    fn ty(self) -> Ty {
        match self {
            NumberTy::Int => Ty::Int,
            NumberTy::Float => Ty::Float,
        }
    }
}

impl Bound {
    /// Returns what both bounds allow, if anything.
    fn meet(self, other: Bound) -> Option<Bound> {
        match (self, other) {
            (Bound::NumberOrList(a), Bound::NumberOrList(b)) => (a == b).then_some(self),
            (bound @ Bound::NumberOrList(_), Bound::Numeric { .. })
            | (Bound::Numeric { .. }, bound @ Bound::NumberOrList(_)) => Some(bound),
            (Bound::Numeric { strings: a }, Bound::Numeric { strings: b }) => {
                Some(Bound::Numeric { strings: a && b })
            }
        }
    }

    /// The type an unknown with this bound is shown as and defaults to.
    fn default(self) -> Ty {
        match self {
            Bound::NumberOrList(number) => number.ty(),
            Bound::Numeric { .. } => Ty::Int,
        }
    }
}

/// The operator of an [`Arithmetic`] operation.
#[derive(Clone, Copy, PartialEq)]
enum ArithmeticOp {
    Binary(Operator),
    Unary(UnaryOp),
}

impl ArithmeticOp {
    /// The operator as written, such as `"**"`.
    fn symbol(self) -> &'static str {
        match self {
            ArithmeticOp::Binary(Operator::Add) | ArithmeticOp::Unary(UnaryOp::UnaryAdd) => "+",
            ArithmeticOp::Binary(Operator::Subtract) | ArithmeticOp::Unary(UnaryOp::UnarySub) => {
                "-"
            }
            ArithmeticOp::Binary(Operator::Multiply) => "*",
            ArithmeticOp::Binary(Operator::Divide) => "/",
            ArithmeticOp::Binary(Operator::Modulo) => "%",
            ArithmeticOp::Binary(Operator::Power) => "**",
            ArithmeticOp::Unary(UnaryOp::Not) => "not",
        }
    }
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

/// The kinds arithmetic accepts, with `int` the default.
const NUMBERS: &[Kind] = &[Kind::Int, Kind::Float];
/// The kinds ordering comparisons accept: numbers, and strings, which compare byte by byte.
const ORDERED: &[Kind] = &[Kind::Int, Kind::Float, Kind::Str];
/// What `int` and `float` convert from.
const CONVERTIBLE: &[Kind] = &[Kind::Int, Kind::Float, Kind::Str];
/// What the `math` functions take.
const NUMERIC: &[Kind] = &[Kind::Int, Kind::Float];
/// What `str` converts from.
const PRINTABLE: &[Kind] = &[Kind::Int, Kind::Float, Kind::Bool, Kind::Str];

/// A function's inferred signature.
struct FunctionInfo {
    symbol: usize,
    params: Vec<Ty>,
    ret: Ty,
    /// Whether the definition writes its return type, rather than returning `none` by default.
    ret_written: bool,
}

/// The function whose body is being checked.
struct FunctionScope {
    name: String,
    /// Parameters and locals, mapped to their symbols.
    locals: HashMap<String, usize>,
    ret: Ty,
    ret_written: bool,
}

/// State for one run of type inference over a module.
#[derive(Default)]
struct Inference {
    /// The solution for each type variable, if known.
    vars: Vec<Option<Ty>>,
    diagnostics: Vec<Diagnostic>,
    constraints: Vec<Constraint>,
    /// Arithmetic whose operands were not yet known to be lists or numbers when it was found.
    arithmetic: Vec<Arithmetic>,
    /// What each type variable that is an operand of such arithmetic can still be.
    bounds: HashMap<usize, Bound>,
    expr_types: Vec<(Span, Ty)>,
    symbols: Vec<Symbol>,
    /// The type of each symbol in `symbols`, resolved into [`Symbol::ty`] at the end.
    symbol_types: Vec<Ty>,
    references: Vec<Reference>,
    functions: HashMap<String, FunctionInfo>,
    /// Each parameter written without a type, with its span and the type inferred for it, which
    /// `finish` reports once inference can suggest a type.
    untyped_params: Vec<(String, Span, Ty)>,
    /// Each value returned by a function without a return type, with the function's name, which
    /// `finish` reports once inference can suggest a type.
    untyped_returns: Vec<(String, Span, Ty)>,
    globals: HashMap<String, usize>,
    function: Option<FunctionScope>,
    loop_depth: usize,
    function_spans: HashMap<String, Span>,
    /// The span of the call an expression statement makes, if it is one, which is the only place
    /// a method that changes its receiver may be called.
    statement_call: Option<Span>,
    /// The span of the call that is the whole value of an assignment, if it is one, where `pop`,
    /// which changes its receiver and returns a value, may also be called.
    assigned_call: Option<Span>,
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
        // the name in `x += 1` is read and assigned, but is one reference
        if self
            .references
            .last()
            .is_some_and(|last| last.span == span && last.symbol == symbol)
        {
            return;
        }
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
                if let Some(bound) = self.bounds.get(&x).copied()
                    && !self.admits(bound, &other)
                {
                    return false;
                }
                self.vars[x] = Some(other);
                true
            }
            (Ty::List(x), Ty::List(y)) => self.unify(&x, &y),
            (x, y) => x == y,
        }
    }

    /// Returns whether a type variable with `bound` can become `ty`, passing the bound on if `ty`
    /// is another variable.
    fn admits(&mut self, bound: Bound, ty: &Ty) -> bool {
        match ty {
            Ty::Var(y) => match self.bounds.get(y) {
                None => {
                    self.bounds.insert(*y, bound);
                    true
                }
                Some(other) => match bound.meet(*other) {
                    Some(met) => {
                        self.bounds.insert(*y, met);
                        true
                    }
                    None => false,
                },
            },
            Ty::List(elem) => match bound {
                Bound::NumberOrList(number) => self.unify(elem, &number.ty()),
                Bound::Numeric { .. } => {
                    matches!(self.shallow(elem), Ty::Int | Ty::Float | Ty::Var(_))
                }
            },
            Ty::Str => bound == Bound::Numeric { strings: true },
            ty => match bound {
                Bound::NumberOrList(number) => *ty == number.ty(),
                Bound::Numeric { .. } => NumberTy::of(ty).is_some(),
            },
        }
    }

    /// Notes what the unknown `operand` of arithmetic can be, given the other operand, if any.
    fn bound_operand(&mut self, operand: &Ty, other: Option<&Ty>, strings: bool) {
        let Ty::Var(x) = self.shallow(operand) else {
            return;
        };
        let bound = match other.map(|other| self.shallow(other)) {
            Some(Ty::List(elem)) => match NumberTy::of(&self.shallow(&elem)) {
                Some(number) => Bound::NumberOrList(number),
                None => Bound::Numeric { strings: false },
            },
            Some(other) => match NumberTy::of(&other) {
                Some(number) => Bound::NumberOrList(number),
                None => Bound::Numeric { strings },
            },
            None => Bound::Numeric { strings },
        };
        let bound = match self.bounds.get(&x) {
            Some(old) => old.meet(bound).unwrap_or(*old),
            None => bound,
        };
        self.bounds.insert(x, bound);
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
            Ty::Var(x) => match self.bounds.get(&x) {
                Some(bound) => self.show(&bound.default()),
                None => "unknown".to_string(),
            },
        }
    }

    /// Checks `op` now if its operands are known to be lists or numbers, and otherwise once they
    /// are, in [`Inference::finish`].
    fn arithmetic(&mut self, op: Arithmetic) {
        if self.solve_arithmetic(&op, false) {
            return;
        }
        let strings = op.op == ArithmeticOp::Binary(Operator::Add);
        match &op.right {
            Some(_) if op.op == ArithmeticOp::Binary(Operator::Power) => {
                self.bound_operand(&op.left, None, false);
            }
            Some(right) => {
                self.bound_operand(&op.left, Some(right), strings);
                self.bound_operand(right, Some(&op.left), strings);
            }
            None => self.bound_operand(&op.left, None, false),
        }
        self.arithmetic.push(op);
    }

    /// Checks the arithmetic `op` and gives its result a type, returning whether it could. Without
    /// `force`, it waits while an operand is unknown, since it could still turn out to be a list
    /// or a number. With `force`, an operand that is still unknown is a number: the other
    /// operand's element if that is a list, or else the same type as the other operand, unless the
    /// result is known to be a list.
    ///
    /// A list operand works element by element, so `[1, 2] * 2` is `list[int]` and `[1.5] + [2.5]`
    /// is `list[float]`, and its elements must be int or float.
    fn solve_arithmetic(&mut self, op: &Arithmetic, force: bool) -> bool {
        let symbol = op.op.symbol();
        let left = self.shallow(&op.left);
        let Some(right) = &op.right else {
            // `+x` or `-x`
            match &left {
                Ty::Var(_) if !force => return false,
                Ty::List(elem) => self.require_elements((**elem).clone(), symbol, op.operand_span),
                _ => {
                    let message = match symbol {
                        "+" => "'+' needs an int or float operand, found {}",
                        _ => "'-' needs an int or float operand, found {}",
                    };
                    self.require(left.clone(), NUMBERS, op.operand_span, message);
                }
            }
            self.unify(&op.result, &left);
            return true;
        };
        let right = self.shallow(right);
        if op.op == ArithmeticOp::Binary(Operator::Power) {
            return self.solve_power(op, left, right, force);
        }
        // a string, bool, or none is never a list, so an unknown other operand has its type
        let scalar = |ty: &Ty| matches!(ty, Ty::Str | Ty::Bool | Ty::None);
        match (&left, &right) {
            (Ty::Var(_), other) | (other, Ty::Var(_)) if !force && !scalar(other) => false,
            (Ty::List(_), Ty::List(_)) => {
                if self.unify(&left, &right) {
                    let Ty::List(elem) = &left else {
                        unreachable!()
                    };
                    self.require_elements((**elem).clone(), symbol, op.span);
                } else {
                    self.mismatch(&left, &right, op.operand_span);
                }
                self.unify(&op.result, &left);
                true
            }
            (Ty::List(elem), Ty::Var(_)) | (Ty::Var(_), Ty::List(elem)) => {
                // an unknown number for each element, whose type may still be unknown too, so
                // this settles here rather than solving again
                let (list, unknown) = match left {
                    Ty::List(_) => (&left, &right),
                    _ => (&right, &left),
                };
                self.unify(unknown, elem);
                self.require_elements((**elem).clone(), symbol, op.span);
                self.unify(&op.result, list);
                true
            }
            (Ty::List(elem), number) | (number, Ty::List(elem)) => {
                let list_on_left = matches!(left, Ty::List(_));
                let (expected, found) = match list_on_left {
                    true => ((**elem).clone(), number.clone()),
                    false => (number.clone(), (**elem).clone()),
                };
                if self.unify(&expected, &found) {
                    self.require_elements(expected, symbol, op.span);
                    let literal = match list_on_left {
                        true => op.literal_left,
                        false => op.literal_right,
                    };
                    if literal && self.shallow(number) == Ty::Int {
                        self.diagnostics.push(Diagnostic::warning(
                            op.span,
                            "'*' multiplies each element of a list, so this is a list of one \
                             element, not copies of it as in Python; build a longer list with \
                             'append' in a loop",
                        ));
                    }
                } else {
                    self.mismatch(&expected, &found, op.operand_span);
                }
                let list = if list_on_left { &left } else { &right };
                self.unify(&op.result, list);
                true
            }
            (Ty::Var(_), _) | (_, Ty::Var(_))
                if matches!(self.shallow(&op.result), Ty::List(_)) =>
            {
                // used as a list, so an unknown operand is the list, after which this solves
                // as arithmetic on a list
                let result = op.result.clone();
                for operand in [&left, &right] {
                    if matches!(operand, Ty::Var(_)) && !self.unify(operand, &result) {
                        self.mismatch(&result, operand, op.span);
                        return true;
                    }
                }
                self.solve_arithmetic(op, force)
            }
            _ => {
                // two numbers, or strings for `+`
                let ArithmeticOp::Binary(operator) = op.op else {
                    unreachable!("unary operations are handled above")
                };
                self.expect(&left, &right, op.operand_span);
                let (allowed, message): (&'static [Kind], _) = match operator {
                    Operator::Add => (
                        &[Kind::Int, Kind::Float, Kind::Str],
                        "'+' needs int, float, or str operands, found {}",
                    ),
                    Operator::Subtract => (NUMBERS, "'-' needs int or float operands, found {}"),
                    Operator::Multiply => (NUMBERS, "'*' needs int or float operands, found {}"),
                    Operator::Divide => (NUMBERS, "'/' needs int or float operands, found {}"),
                    Operator::Modulo => (NUMBERS, "'%' needs int or float operands, found {}"),
                    Operator::Power => unreachable!("handled by solve_power"),
                };
                self.require(left.clone(), allowed, op.span, message);
                self.unify(&op.result, &left);
                true
            }
        }
    }

    /// Checks `base ** exponent` for [`Inference::solve_arithmetic`]. The exponent is an int, or
    /// a list of ints, and the base an int or float, or a list of them, and the result is a list
    /// if either is. An exponent that is unknown when the operation is found is an int.
    fn solve_power(&mut self, op: &Arithmetic, base: Ty, exponent: Ty, force: bool) -> bool {
        // an unknown exponent, such as a parameter, becomes an int right away, so a mistaken
        // argument is reported where the function is called
        if matches!(exponent, Ty::Var(_)) && !matches!(self.shallow(&op.result), Ty::List(_)) {
            self.unify(&exponent, &Ty::Int);
        }
        let exponent = self.shallow(&exponent);
        if !force && matches!(base, Ty::Var(_)) {
            return false;
        }
        let exponent_list = match &exponent {
            Ty::List(elem) => {
                if !self.unify(elem, &Ty::Int) {
                    let message =
                        format!("'**' needs an int exponent, found {}", self.show(&exponent));
                    self.error(op.operand_span, message);
                }
                true
            }
            Ty::Var(_) if matches!(self.shallow(&op.result), Ty::List(_)) && base == Ty::Int => {
                self.unify(&exponent, &Ty::List(Box::new(Ty::Int)));
                true
            }
            _ => {
                self.require(
                    exponent,
                    &[Kind::Int],
                    op.operand_span,
                    "'**' needs an int exponent, found {}",
                );
                false
            }
        };
        let base = match base {
            Ty::Var(_) if matches!(self.shallow(&op.result), Ty::List(_)) && !exponent_list => {
                self.unify(&base, &op.result);
                self.shallow(&base)
            }
            base => base,
        };
        match &base {
            Ty::List(elem) => {
                self.require_elements((**elem).clone(), "**", op.span);
                self.unify(&op.result, &base);
            }
            _ => {
                self.require(
                    base.clone(),
                    NUMBERS,
                    op.span,
                    "'**' needs an int or float base, found {}",
                );
                let result = match exponent_list {
                    true => Ty::List(Box::new(base)),
                    false => base,
                };
                self.unify(&op.result, &result);
            }
        }
        true
    }

    /// Requires the elements of a list that `symbol` works on element by element to be numbers.
    fn require_elements(&mut self, elem: Ty, symbol: &str, span: Span) {
        let message = match symbol {
            "+" => "'+' on lists needs int or float elements, found {}",
            "-" => "'-' on lists needs int or float elements, found {}",
            "*" => "'*' on lists needs int or float elements, found {}",
            "/" => "'/' on lists needs int or float elements, found {}",
            "%" => "'%' on lists needs int or float elements, found {}",
            _ => "'**' on lists needs int or float elements, found {}",
        };
        self.require(elem, NUMBERS, span, message);
    }

    /// Reports that `found` should have been `expected`, as [`Inference::expect`] does.
    fn mismatch(&mut self, expected: &Ty, found: &Ty, span: Span) {
        let message = format!(
            "expected {}, found {}",
            self.show(expected),
            self.show(found)
        );
        self.error(span, message);
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
                returns,
                ..
            } = &stmt.kind
            {
                self.declare_function(name, *name_span, args, returns.as_ref());
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

    /// Declares a function with the types its signature writes, so calls anywhere in the program
    /// are checked against them.
    ///
    /// For example, `fn half(x: float) -> float;` declares `half` as `fn(float) -> float`. A
    /// parameter without a type gets an unknown one, so checking goes on, and is reported in
    /// `finish`; a function without `->` returns `none`.
    fn declare_function(
        &mut self,
        name: &str,
        name_span: Span,
        args: &Arguments,
        returns: Option<&TypeExpr>,
    ) {
        if self.functions.contains_key(name) {
            self.error(name_span, format!("function '{name}' is already defined"));
            return;
        }
        if BUILTINS.contains(&name) {
            self.error(name_span, format!("cannot redefine builtin '{name}'"));
            return;
        }
        let mut params = vec![];
        for arg in &args.args {
            let ty = match &arg.annotation {
                Some(annotation) => Self::written(annotation),
                None => {
                    let ty = self.fresh();
                    self.untyped_params
                        .push((arg.arg.clone(), arg.span, ty.clone()));
                    ty
                }
            };
            params.push(ty);
        }
        let ret = returns.map_or(Ty::None, Self::written);
        // the symbol's type is rebuilt from `params` and `ret` in `finish`
        let symbol = self.add_symbol(name, SymbolKind::Function, name_span, Ty::None);
        self.functions.insert(
            name.to_string(),
            FunctionInfo {
                symbol,
                params,
                ret,
                ret_written: returns.is_some(),
            },
        );
    }

    ////////////////////////////////////////////////////////////////
    // statements
    ////////////////////////////////////////////////////////////////

    /// Converts a type written in a signature to an inference type.
    ///
    /// For example, `list[float]` becomes `Ty::List(Box::new(Ty::Float))`.
    fn written(annotation: &TypeExpr) -> Ty {
        match &annotation.kind {
            TypeExprKind::Int => Ty::Int,
            TypeExprKind::Float => Ty::Float,
            TypeExprKind::Bool => Ty::Bool,
            TypeExprKind::Str => Ty::Str,
            TypeExprKind::None => Ty::None,
            TypeExprKind::List(elem) => Ty::List(Box::new(Self::written(elem))),
        }
    }

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
                    let ret_written = function.ret_written;
                    if !self.unify(&ret, &found) {
                        let span = value.as_ref().map_or(stmt.span, |v| v.span);
                        if ret_written {
                            let message = format!(
                                "'{name}' returns {}, but this is {}",
                                self.show(&ret),
                                self.show(&found)
                            );
                            self.error(span, message);
                        } else {
                            self.untyped_returns.push((name, span, found));
                        }
                    }
                }
            }
            StmtKind::Assign { targets, value } => {
                self.assigned_call = Some(value.span);
                let found = self.infer(value);
                self.assigned_call = None;
                for target in targets {
                    self.check_target(target, &found, value.span);
                }
            }
            StmtKind::AugAssign { target, op, value } => {
                // the old value comes first, then the value, as `target = target op value`
                // reads them, but a subscript's indexes are inferred once
                let old = match &target.kind {
                    ExprKind::Name { id, .. }
                        if !matches!(self.resolve_name(id), Resolved::Variable(_)) =>
                    {
                        // a function or builtin, which `check_target` reports once
                        let found = self.infer(value);
                        self.check_target(target, &found, value.span);
                        return;
                    }
                    ExprKind::Name { .. } => self.infer(target),
                    ExprKind::Subscript {
                        value: list, slice, ..
                    } => {
                        let (elem, string) = self.subscript_of(list, slice);
                        if string {
                            self.error(target.span, STRING_CHANGE);
                            return;
                        }
                        if matches!(slice.kind, ExprKind::Slice { .. }) {
                            self.error(target.span, SLICE_CHANGE);
                            return;
                        }
                        self.expr_types.push((target.span, elem.clone()));
                        elem
                    }
                    _ => {
                        self.error(target.span, "cannot assign to this expression");
                        return;
                    }
                };
                let found = self.infer(value);
                let result = self.fresh();
                // the operation's type, which no expression spans, for lowering
                self.expr_types.push((stmt.span, result.clone()));
                self.arithmetic(Arithmetic {
                    op: ArithmeticOp::Binary(*op),
                    left: old.clone(),
                    right: Some(found),
                    result: result.clone(),
                    span: stmt.span,
                    operand_span: value.span,
                    literal_left: false,
                    literal_right: false,
                });
                match &target.kind {
                    ExprKind::Subscript { value: list, .. } => {
                        self.expect(&old, &result, value.span);
                        self.check_change(list);
                    }
                    _ => self.check_target(target, &result, value.span),
                }
            }
            StmtKind::For { target, iter, body } => {
                let elem = match range_args(iter) {
                    Some(args) => self.check_range(iter, args),
                    None => {
                        let iter_ty = self.infer(iter);
                        // a string gives its characters, and anything else must be a list
                        if self.shallow(&iter_ty) == Ty::Str {
                            Ty::Str
                        } else {
                            let elem = self.fresh();
                            self.expect(&Ty::List(Box::new(elem.clone())), &iter_ty, iter.span);
                            elem
                        }
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
                self.statement_call = Some(value.span);
                self.infer(value);
                self.statement_call = None;
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
        let ret_written = info.ret_written;

        self.function = Some(FunctionScope {
            name: name.to_string(),
            locals: HashMap::new(),
            ret: ret.clone(),
            ret_written,
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

        for arg in &args.args {
            if let Some(span) = lost_change(body, &arg.arg) {
                let param = &arg.arg;
                self.diagnostics.push(Diagnostic::warning(
                    span,
                    format!(
                        "changes to '{param}' are lost when '{name}' returns, since parameters \
                         are copies; return it and assign the result where '{name}' is called"
                    ),
                ));
            }
        }

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
                let (elem, string) = self.subscript_of(value, slice);
                if string {
                    self.error(target.span, STRING_CHANGE);
                    return;
                }
                if matches!(slice.kind, ExprKind::Slice { .. }) {
                    self.error(target.span, SLICE_CHANGE);
                    return;
                }
                self.expect(&elem, found, value_span);
                self.check_change(value);
            }
            _ => self.error(target.span, "cannot assign to this expression"),
        }
    }

    /// Checks that `list`, which a statement changes, is a variable or an element of one, since a
    /// change to any other list would be lost, and that a function changes only its own variables.
    ///
    /// For example, `f().append(1)` and a function's `names.append(x)` for a global `names` are
    /// errors, while `grid[0].append(1)` is fine.
    fn check_change(&mut self, list: &Expr) {
        if has_slice(list) {
            self.error(list.span, SLICE_CHANGE);
            return;
        }
        let root = place_root(list);
        let ExprKind::Name { id, .. } = &root.kind else {
            self.error(
                root.span,
                "this list is not stored in a variable, so changing it has no effect",
            );
            return;
        };
        if self.function.is_some() && self.local(id).is_none() && self.globals.contains_key(id) {
            self.error(
                root.span,
                format!(
                    "a function cannot change the global '{id}'; pass it as an argument and \
                     return the changed value"
                ),
            );
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
        if is_builtin(name) {
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
            ExprKind::BinOp { op, left, right } => {
                let l = self.infer(left);
                let r = self.infer(right);
                let one_element = |expr: &Expr| {
                    *op == Operator::Multiply
                        && matches!(&expr.kind, ExprKind::List { elts, .. } if elts.len() == 1)
                };
                let result = self.fresh();
                self.arithmetic(Arithmetic {
                    op: ArithmeticOp::Binary(*op),
                    left: l,
                    right: Some(r),
                    result: result.clone(),
                    span: expr.span,
                    operand_span: right.span,
                    literal_left: one_element(left),
                    literal_right: one_element(right),
                });
                result
            }
            ExprKind::UnaryOp { op, operand } => {
                let ty = self.infer(operand);
                match op {
                    UnaryOp::Not => {
                        self.require_condition(ty, operand.span);
                        Ty::Bool
                    }
                    UnaryOp::UnaryAdd | UnaryOp::UnarySub => {
                        let result = self.fresh();
                        self.arithmetic(Arithmetic {
                            op: ArithmeticOp::Unary(*op),
                            left: ty,
                            right: None,
                            result: result.clone(),
                            span: expr.span,
                            operand_span: operand.span,
                            literal_left: false,
                            literal_right: false,
                        });
                        result
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
                        CompOp::LessThan => {
                            (ORDERED, "'<' needs int, float, or str operands, found {}")
                        }
                        CompOp::LessThanEqual => {
                            (ORDERED, "'<=' needs int, float, or str operands, found {}")
                        }
                        CompOp::GreaterThan => {
                            (ORDERED, "'>' needs int, float, or str operands, found {}")
                        }
                        CompOp::GreaterThanEqual => {
                            (ORDERED, "'>=' needs int, float, or str operands, found {}")
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
            // the parser only makes a slice inside a subscript, which `subscript` handles
            ExprKind::Slice { .. } => {
                self.error(
                    expr.span,
                    "a slice can only appear in brackets, as in 'xs[1:]'",
                );
                self.fresh()
            }
        }
    }

    /// Infers `value[slice]`, which must index a list or a string with an int, or slice it. An
    /// index gives an element of a list, or a string of one byte, and a slice gives a value of the
    /// same type. A value not yet known to be a string is taken to be a list.
    ///
    /// For example, `xs[0]` of a `list[int]` is `int`, and `s[1:]` of a `str` is `str`.
    fn subscript(&mut self, value: &Expr, slice: &Expr) -> Ty {
        self.subscript_of(value, slice).0
    }

    /// Infers `value[slice]` as [`Inference::subscript`] does, also returning whether `value` is
    /// a string.
    fn subscript_of(&mut self, value: &Expr, slice: &Expr) -> (Ty, bool) {
        let value_ty = self.infer(value);
        let string = self.shallow(&value_ty) == Ty::Str;
        let (sliced, elem) = match string {
            true => (Ty::Str, Ty::Str),
            false => {
                let elem = self.fresh();
                let list = Ty::List(Box::new(elem.clone()));
                self.expect(&list, &value_ty, value.span);
                (list, elem)
            }
        };
        if let ExprKind::Slice { lower, upper, step } = &slice.kind {
            for bound in [lower, upper, step].into_iter().flatten() {
                let ty = self.infer(bound);
                self.expect(&Ty::Int, &ty, bound.span);
            }
            return (sliced, string);
        }
        let index = self.infer(slice);
        self.expect(&Ty::Int, &index, slice.span);
        (elem, string)
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

    /// Requires `receiver` to be a list that can hold `item`, as `append` and `insert` do.
    fn add_element(&mut self, receiver: &Expr, receiver_ty: &Ty, item: &Expr, item_ty: &Ty) {
        let elem = self.fresh();
        let list = Ty::List(Box::new(elem.clone()));
        if self.unify(&list, receiver_ty) {
            self.expect(&elem, item_ty, item.span);
        } else {
            // the element pins down the message, as in `expected list[int]`
            self.unify(&elem, item_ty);
            self.expect(&list, receiver_ty, receiver.span);
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
        if changes_receiver(name) {
            // inside an expression, a borrowed copy of the receiver could see the change
            if name == "pop" {
                if ![self.statement_call, self.assigned_call].contains(&Some(call.span)) {
                    self.error(
                        call.span,
                        "'pop' changes a list, so it can only be a statement of its own or the \
                         value of an assignment, as in 'x = xs.pop()'",
                    );
                }
            } else if self.statement_call != Some(call.span) {
                self.error(
                    call.span,
                    format!(
                        "'{name}' changes a list and returns none, so it can only be a statement \
                         of its own"
                    ),
                );
            }
            self.check_change(receiver);
        }
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
                self.add_element(receiver, &receiver_ty, &args[0], &arg_types[0]);
                Ty::None
            }
            "pop" => {
                match (args, &arg_types[..]) {
                    ([], _) => {}
                    ([index], [ty]) => self.expect(&Ty::Int, ty, index.span),
                    _ => self.arity_error(call.span, name, "0 or 1", args.len()),
                }
                let elem = self.fresh();
                self.expect(
                    &Ty::List(Box::new(elem.clone())),
                    &receiver_ty,
                    receiver.span,
                );
                elem
            }
            "insert" => {
                if args.len() != 2 {
                    self.arity_error(call.span, name, "2", args.len());
                    return Ty::None;
                }
                self.expect(&Ty::Int, &arg_types[0], args[0].span);
                self.add_element(receiver, &receiver_ty, &args[1], &arg_types[1]);
                Ty::None
            }
            "extend" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                    return Ty::None;
                }
                let list = Ty::List(Box::new(self.fresh()));
                if self.unify(&list, &receiver_ty) {
                    self.expect(&list, &arg_types[0], args[0].span);
                } else {
                    // the argument pins down the message, as in `expected list[int]`
                    self.unify(&list, &arg_types[0]);
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
            "join" | "find" | "startswith" | "endswith" => {
                let (message, arg, result) = match name {
                    "join" => (
                        "'join' needs a str, found {}",
                        Ty::List(Box::new(Ty::Str)),
                        Ty::Str,
                    ),
                    "find" => ("'find' needs a str, found {}", Ty::Str, Ty::Int),
                    "startswith" => ("'startswith' needs a str, found {}", Ty::Str, Ty::Bool),
                    _ => ("'endswith' needs a str, found {}", Ty::Str, Ty::Bool),
                };
                self.require(receiver_ty, &[Kind::Str], receiver.span, message);
                match args {
                    [only] => self.expect(&arg, &arg_types[0], only.span),
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                result
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
            "os.env" | "os.has_env" | "os.exit" => {
                let param = if name == "os.exit" { Ty::Int } else { Ty::Str };
                match args {
                    [arg] => self.expect(&param, &arg_types[0], arg.span),
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                match name {
                    "os.env" => Ty::Str,
                    "os.has_env" => Ty::Bool,
                    _ => Ty::None,
                }
            }
            "os.platform" | "os.arch" | "os.hostname" | "os.cpu_count" | "os.pid" | "os.cwd"
            | "time.now" | "time.clock" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                match name {
                    "os.cpu_count" | "os.pid" => Ty::Int,
                    "time.now" | "time.clock" => Ty::Float,
                    _ => Ty::Str,
                }
            }
            "random.random" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                Ty::Float
            }
            "random.seed" => {
                match args {
                    [arg] => self.expect(&Ty::Int, &arg_types[0], arg.span),
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                Ty::None
            }
            "random.randint" => {
                if args.len() == 2 {
                    for (arg, ty) in args.iter().zip(arg_types) {
                        self.expect(&Ty::Int, ty, arg.span);
                    }
                } else {
                    self.arity_error(call.span, name, "2", args.len());
                }
                Ty::Int
            }
            "random.choice" => {
                let elem = self.fresh();
                match args {
                    [list] => {
                        self.require(
                            arg_types[0].clone(),
                            &[Kind::List],
                            list.span,
                            "'random.choice' needs a list, found {}",
                        );
                        self.unify(&Ty::List(Box::new(elem.clone())), &arg_types[0]);
                    }
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                elem
            }
            "time.sleep" => {
                match args {
                    [arg] => self.require(
                        arg_types[0].clone(),
                        NUMERIC,
                        arg.span,
                        "'time.sleep' needs an int or float, found {}",
                    ),
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                Ty::None
            }
            "math.abs" | "math.sqrt" | "math.floor" => {
                let [arg] = args else {
                    self.arity_error(call.span, name, "1", args.len());
                    return match name {
                        "math.sqrt" => Ty::Float,
                        "math.floor" => Ty::Int,
                        _ => self.fresh(),
                    };
                };
                let message = match name {
                    "math.abs" => "'math.abs' needs an int or float, found {}",
                    "math.sqrt" => "'math.sqrt' needs an int or float, found {}",
                    _ => "'math.floor' needs an int or float, found {}",
                };
                self.require(arg_types[0].clone(), NUMERIC, arg.span, message);
                match name {
                    "math.sqrt" => Ty::Float,
                    "math.floor" => Ty::Int,
                    _ => arg_types[0].clone(),
                }
            }
            "math.min" | "math.max" => match args {
                [] => {
                    self.arity_error(call.span, name, "1 or more", 0);
                    self.fresh()
                }
                [list] => {
                    self.require(
                        arg_types[0].clone(),
                        &[Kind::List],
                        list.span,
                        "one argument must be a list of numbers, found {}",
                    );
                    let elem = self.fresh();
                    if self.unify(&Ty::List(Box::new(elem.clone())), &arg_types[0]) {
                        self.require(
                            elem.clone(),
                            NUMERIC,
                            list.span,
                            "the list must hold int or float, found list[{}]",
                        );
                    }
                    elem
                }
                [first, ..] => {
                    let message = match name {
                        "math.min" => "'math.min' needs ints or floats, found {}",
                        _ => "'math.max' needs ints or floats, found {}",
                    };
                    self.require(arg_types[0].clone(), NUMERIC, first.span, message);
                    for (arg, ty) in args.iter().zip(arg_types).skip(1) {
                        self.expect(&arg_types[0], ty, arg.span);
                    }
                    arg_types[0].clone()
                }
            },
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
        // solving one operation can pin down another's operands, so force one only when none can
        // be solved otherwise
        let mut pending = std::mem::take(&mut self.arithmetic);
        while !pending.is_empty() {
            let before = pending.len();
            pending.retain(|op| !self.solve_arithmetic(op, false));
            if pending.len() == before {
                let op = pending.remove(0);
                self.solve_arithmetic(&op, true);
            }
        }
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
        // suggest the type inference found, which is what the program already relies on
        for (name, span, ty) in std::mem::take(&mut self.untyped_params) {
            let ty = self.finalize(&ty);
            self.error(
                span,
                format!("parameter '{name}' needs a type, such as '{name}: {ty}'"),
            );
        }
        for (name, span, ty) in std::mem::take(&mut self.untyped_returns) {
            let ty = self.finalize(&ty);
            self.error(
                span,
                format!(
                    "'{name}' has no return type, so it cannot return {ty}; add '-> {ty}' after \
                     its parameters"
                ),
            );
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
    /// For example, the elements of `xs` in `xs = []`, which nothing else uses, are `int`.
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
            // the target's old value is read first
            StmtKind::AugAssign { target, value, .. } => {
                self.reads(target, &assigned);
                self.reads(value, &assigned);
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
            ExprKind::Slice { lower, upper, step } => {
                for bound in [lower, upper, step].into_iter().flatten() {
                    self.reads(bound, assigned);
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
            StmtKind::AugAssign { target, .. } => add(target, out),
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
            | StmtKind::Expr { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }
}

/// Returns the expression the place `expr` starts from, following its subscripts.
///
/// For example, the root of `grid[i][j]` is `grid`, and the root of `f()[0]` is `f()`.
/// What the checker reports for a change to a slice, such as `xs[1:].append(2)`.
const SLICE_CHANGE: &str = "a slice is a new list, so changing it has no effect";
const STRING_CHANGE: &str =
    "a string cannot be changed; build a new one, as in 's = s[:i] + c + s[i + 1:]'";

/// Returns whether the list `expr`, which a statement changes, is a slice or an element of one.
///
/// For example, `xs[1:]` and `grid[:2][0]` are, but `grid[0]` is not.
fn has_slice(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Subscript { value, slice, .. } => {
            matches!(slice.kind, ExprKind::Slice { .. }) || has_slice(value)
        }
        _ => false,
    }
}

pub(crate) fn place_root(expr: &Expr) -> &Expr {
    match &expr.kind {
        ExprKind::Subscript { value, .. } => place_root(value),
        _ => expr,
    }
}

/// Splits a list that a statement changes into the variable it starts from and the indexes that
/// lead from that variable to it, outermost first, or returns `None` if it is not in a variable.
///
/// For example, `grid[i][j]` gives `grid` and `[i, j]`, `xs` gives `xs` and no indexes, and
/// `f()[0]` gives `None`.
pub(crate) fn place(expr: &Expr) -> Option<(&str, Vec<&Expr>)> {
    match &expr.kind {
        ExprKind::Name { id, .. } => Some((id, vec![])),
        ExprKind::Subscript { value, slice, .. } => {
            let (root, mut indexes) = place(value)?;
            indexes.push(slice);
            Some((root, indexes))
        }
        _ => None,
    }
}

/// Returns the span of each subscript in `list`, a variable or an element of one, outermost
/// index first, matching the indexes [`place`] returns. A runtime error for an index out of
/// range points at its subscript.
///
/// For example, `grid[i][j]` gives the spans of `grid[i]` and `grid[i][j]`.
pub(crate) fn place_spans(list: &Expr) -> Vec<Span> {
    match &list.kind {
        ExprKind::Subscript { value, .. } => {
            let mut spans = place_spans(value);
            spans.push(list.span);
            spans
        }
        _ => vec![],
    }
}

/// Returns the receiver of `expr` if it calls a method that changes its receiver.
///
/// For example, for `grid[0].append(1)` this returns `grid[0]`, and for `xs.len()` it returns
/// `None`.
pub(crate) fn changed_receiver(expr: &Expr) -> Option<&Expr> {
    let ExprKind::Call { func, .. } = &expr.kind else {
        return None;
    };
    match &func.kind {
        ExprKind::Attribute { value, attr, .. } if changes_receiver(attr) => Some(value),
        _ => None,
    }
}

/// Collects the name of every variable that `body` changes in place, through `xs[i] = v` or
/// `xs.append(v)`, without looking inside function definitions.
///
/// For example, for `grid[0][1] = 2` followed by `if x; ys.append(1)`, this collects `grid` and
/// `ys`.
pub(crate) fn collect_changed(body: &[Stmt], out: &mut HashSet<String>) {
    let add = |list: &Expr, out: &mut HashSet<String>| {
        if let Some((root, _)) = place(list) {
            out.insert(root.to_string());
        }
    };
    for stmt in body {
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                for target in targets {
                    if let ExprKind::Subscript { value, .. } = &target.kind {
                        add(value, out);
                    }
                }
                if let Some(receiver) = changed_receiver(value) {
                    add(receiver, out);
                }
            }
            StmtKind::AugAssign { target, .. } => {
                if let ExprKind::Subscript { value, .. } = &target.kind {
                    add(value, out);
                }
            }
            StmtKind::Expr { value } => {
                if let Some(receiver) = changed_receiver(value) {
                    add(receiver, out);
                }
            }
            StmtKind::For { body, .. } | StmtKind::While { body, .. } => collect_changed(body, out),
            StmtKind::If { body, orelse, .. } => {
                collect_changed(body, out);
                collect_changed(orelse, out);
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Return { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }
}

/// Returns where `body` first changes the parameter `name` if it never reads it anywhere else,
/// since the caller cannot see the change and nothing else uses it.
///
/// For example, the change in `fn push(xs: list[int], x: int); xs.append(x)` is lost, but a
/// later `ret xs` or `print(xs)` uses it.
fn lost_change(body: &[Stmt], name: &str) -> Option<Span> {
    let mut usage = ParameterUse {
        name,
        changed: None,
        read: false,
    };
    usage.block(body);
    if usage.read { None } else { usage.changed }
}

/// How a function body uses one of its parameters, for [`lost_change`].
struct ParameterUse<'a> {
    name: &'a str,
    /// Where the body first changes the parameter in place.
    changed: Option<Span>,
    /// Whether the body reads the parameter anywhere other than to change it.
    read: bool,
}

impl ParameterUse<'_> {
    fn block(&mut self, body: &[Stmt]) {
        for stmt in body {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Assign { targets, value } => {
                self.change_or_expr(value);
                for target in targets {
                    match &target.kind {
                        ExprKind::Name { .. } => {}
                        ExprKind::Subscript { value, slice, .. } => {
                            self.change(value);
                            self.expr(slice);
                        }
                        _ => self.expr(target),
                    }
                }
            }
            // reading an element only to change it still loses the change
            StmtKind::AugAssign { target, value, .. } => {
                match &target.kind {
                    ExprKind::Subscript { value, slice, .. } => {
                        self.change(value);
                        self.expr(slice);
                    }
                    _ => self.expr(target),
                }
                self.expr(value);
            }
            StmtKind::Expr { value } => self.change_or_expr(value),
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            StmtKind::If { test, body, orelse } => {
                self.expr(test);
                self.block(body);
                self.block(orelse);
            }
            StmtKind::While { test, body } => {
                self.expr(test);
                self.block(body);
            }
            StmtKind::For { iter, body, .. } => {
                self.expr(iter);
                self.block(body);
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    /// Notes the change `value` makes if it calls a method that changes its receiver, as in
    /// `xs.append(x)` or `x = xs.pop()`, and otherwise what it reads.
    fn change_or_expr(&mut self, value: &Expr) {
        match (changed_receiver(value), &value.kind) {
            (Some(receiver), ExprKind::Call { args, .. }) => {
                self.change(receiver);
                for arg in args {
                    self.expr(arg);
                }
            }
            _ => self.expr(value),
        }
    }

    /// Notes a change to `list`, which changes the parameter if `list` starts from it.
    fn change(&mut self, list: &Expr) {
        match place(list) {
            Some((root, indexes)) if root == self.name => {
                self.changed.get_or_insert(place_root(list).span);
                for index in indexes {
                    self.expr(index);
                }
            }
            _ => self.expr(list),
        }
    }

    /// Notes whether `expr` reads the parameter.
    fn expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Name { id, .. } => self.read |= id == self.name,
            ExprKind::BoolOp { values, .. } => values.iter().for_each(|value| self.expr(value)),
            ExprKind::BinOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::UnaryOp { operand, .. } => self.expr(operand),
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.expr(left);
                comparators.iter().for_each(|value| self.expr(value));
            }
            ExprKind::Call { func, args } => {
                // a called name is a function, but a method's receiver is read
                if let ExprKind::Attribute { .. } = &func.kind {
                    self.expr(func);
                }
                args.iter().for_each(|arg| self.expr(arg));
            }
            ExprKind::Attribute { value, .. } => self.expr(value),
            ExprKind::Subscript { value, slice, .. } => {
                self.expr(value);
                self.expr(slice);
            }
            ExprKind::List { elts, .. } => elts.iter().for_each(|elt| self.expr(elt)),
            ExprKind::Slice { lower, upper, step } => [lower, upper, step]
                .into_iter()
                .flatten()
                .for_each(|bound| self.expr(bound)),
            ExprKind::Constant { .. } => {}
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
