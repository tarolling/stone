//! Type checker and name resolver that validates a stone AST before it runs.
//!
//! Function signatures are written, and every other type is inferred. Every expression gets one of
//! `int`, `float`, `bool`, `str`, `list[T]`, `tuple[...]`, an enum, or `option[T]`, and every
//! variable keeps a single type for its whole life. Each parameter and return type comes from the
//! function's signature, so `fn add(a: int, b: int) -> int` is `fn(int, int) -> int`, and a
//! function without `->` returns no value, so its call can only be a statement. Functions are
//! monomorphic, and every call is checked against the signature.
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
//! example, conditions must be `bool`, since no other value is truthy.

use std::collections::{HashMap, HashSet};
use std::fmt::Display;

use crate::ast::{
    Arguments, CompOp, Constant, Expr, ExprKind, MatchCase, Mod, Operator, Pattern, PatternKind,
    Stmt, StmtKind, TypeExpr, TypeExprKind, UnaryOp, Variant, constant_int, target_names,
};
use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{BUILTINS, MAX_ELEMENT_VALUES, METHODS, changes_receiver, is_builtin};

#[cfg(test)]
mod tests;

/// The type of a value or function, such as `int` or `fn(int) -> bool`.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    /// No value, which is what a function without `->` returns. No variable can hold it.
    Unit,
    List(Box<Type>),
    /// A tuple of two or more fields, such as `tuple[int, str]`.
    Tuple(Vec<Type>),
    /// An enum by its linked name, with its type arguments: `Shape`, or `option[int]`, the enum
    /// [`OPTION`] built into the language.
    Enum {
        name: String,
        args: Vec<Type>,
    },
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
}

impl Type {
    /// Returns the values a value of this type is made of once nested tuples are flattened, in
    /// order: the type itself unless it is a tuple.
    ///
    /// For example, `tuple[int, tuple[str, float]]` has the leaves `int`, `str`, and `float`, and
    /// `list[tuple[int, int]]` is one leaf.
    pub fn leaves(&self) -> Vec<Type> {
        match self {
            Type::Tuple(fields) => fields.iter().flat_map(Type::leaves).collect(),
            other => vec![other.clone()],
        }
    }

    /// Returns the positions among [`Type::leaves`] of field `k` of this tuple type.
    ///
    /// For example, field 1 of `tuple[tuple[int, str], float, bool]` is leaf 2, and field 0 is
    /// leaves 0 and 1.
    pub fn field_leaves(&self, k: usize) -> std::ops::Range<usize> {
        let Type::Tuple(fields) = self else {
            return 0..1;
        };
        let start: usize = fields[..k].iter().map(|f| f.leaves().len()).sum();
        start..start + fields[k].leaves().len()
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Str => write!(f, "str"),
            Type::Unit => write!(f, "no value"),
            Type::List(elem) => write!(f, "list[{elem}]"),
            Type::Tuple(fields) => {
                let fields: Vec<String> = fields.iter().map(Type::to_string).collect();
                write!(f, "tuple[{}]", fields.join(", "))
            }
            Type::Enum { name, args } if args.is_empty() => write!(f, "{name}"),
            Type::Enum { name, args } => {
                let args: Vec<String> = args.iter().map(Type::to_string).collect();
                write!(f, "{name}[{}]", args.join(", "))
            }
            Type::Function { params, ret } => {
                let params: Vec<String> = params.iter().map(Type::to_string).collect();
                match **ret {
                    Type::Unit => write!(f, "fn({})", params.join(", ")),
                    _ => write!(f, "fn({}) -> {ret}", params.join(", ")),
                }
            }
        }
    }
}

/// The name of the enum built into the language, whose values are `none` and `some(x)`.
pub const OPTION: &str = "option";

/// The variants of every enum a program defines, keyed by linked name, each with the types of its
/// values in order, as in `Circle` holding `float`. [`variants`] also gives `option`'s.
pub type Enums = HashMap<String, Vec<(String, Vec<Type>)>>;

/// Returns the variants of the enum type `ty`, each with the types of its values, in the order
/// their tags number them, or nothing if `ty` is not an enum.
///
/// For example, `option[int]` gives `none` with no values, then `some` with an `int`.
pub fn variants(enums: &Enums, ty: &Type) -> Vec<(String, Vec<Type>)> {
    match ty {
        Type::Enum { name, args } if name == OPTION => vec![
            ("none".to_string(), vec![]),
            ("some".to_string(), args.clone()),
        ],
        Type::Enum { name, .. } => enums.get(name).cloned().unwrap_or_default(),
        _ => vec![],
    }
}

/// Returns how a value of the enum type `ty`'s variant `name` prints before its values: the
/// enum's own name and the variant's, or the variant's alone for `option`.
///
/// For example, `Circle` of `geometry.Shape` prints as `Shape.Circle`, and `some` as `some`.
pub fn variant_display(ty: &Type, name: &str) -> String {
    match ty {
        Type::Enum {
            name: enum_name, ..
        } if enum_name != OPTION => {
            let short = enum_name.rsplit('.').next().unwrap_or(enum_name);
            format!("{short}.{name}")
        }
        _ => name.to_string(),
    }
}

/// What a [`Symbol`] names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    /// An enum, such as `Shape` in `enum Shape;`.
    Enum,
    /// A variant of an enum, such as `Circle`, whose [`Symbol::scope`] is the enum's name.
    Variant,
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
    /// The variants of every enum the program defines.
    pub enums: Enums,
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
            _ if s.kind == SymbolKind::Variant => false,
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
    Unit,
    List(Box<Ty>),
    Tuple(Vec<Ty>),
    Enum(String, Vec<Ty>),
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

/// A kind of type, ignoring what a list or tuple holds, for [`Constraint`]s.
#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Int,
    Float,
    Bool,
    Str,
    Unit,
    List,
    Tuple,
    Enum,
}

impl Kind {
    fn of(ty: &Ty) -> Option<Kind> {
        match ty {
            Ty::Int => Some(Kind::Int),
            Ty::Float => Some(Kind::Float),
            Ty::Bool => Some(Kind::Bool),
            Ty::Str => Some(Kind::Str),
            Ty::Unit => Some(Kind::Unit),
            Ty::List(_) => Some(Kind::List),
            Ty::Tuple(_) => Some(Kind::Tuple),
            Ty::Enum(..) => Some(Kind::Enum),
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
/// The names of the types built into the language, which no enum may take.
const BUILTIN_TYPES: &[&str] = &["int", "float", "bool", "str", "list", "tuple", OPTION];
/// What `==` and `!=` compare. Compiled code compares lists by address, so only values compare.
const EQUATABLE: &[Kind] = &[Kind::Int, Kind::Float, Kind::Bool, Kind::Str];

/// An enum a program defines: its symbol and its variants, each with its symbol and the types of
/// its values.
struct EnumInfo {
    symbol: usize,
    variants: Vec<VariantInfo>,
}

struct VariantInfo {
    name: String,
    symbol: usize,
    fields: Vec<Ty>,
}

/// A `match` statement, kept so `finish` can check that its cases cover every value once the
/// subject's type is known.
struct PendingMatch {
    subject: Ty,
    subject_span: Span,
    patterns: Vec<Pattern>,
}

/// A function's inferred signature.
struct FunctionInfo {
    symbol: usize,
    params: Vec<Ty>,
    ret: Ty,
    /// Whether the definition writes its return type, rather than returning no value by default.
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
    /// The span of every tuple that is indexed, such as `t` in `t[0]`, so that a change through
    /// one, such as `t[0].append(1)`, can be reported.
    indexed_tuples: HashSet<Span>,
    /// Each list literal and the type of the list, whose elements `finish` checks are not too
    /// wide to store.
    list_literals: Vec<(Span, Ty)>,
    /// Constant indexes into a global a function reads before the code assigning it is checked,
    /// which `finish` settles once the global is known to be a tuple or a list.
    deferred_indexes: Vec<DeferredIndex>,
    /// Every enum the program defines, by name.
    enums: HashMap<String, EnumInfo>,
    /// The span of every enum definition declared at the top level, so a nested one is reported.
    enum_spans: HashSet<Span>,
    /// Every `match`, whose cases `finish` checks.
    matches: Vec<PendingMatch>,
}

/// `value[slice]`, with a constant `slice`, where `value` is a global not yet known to be a tuple
/// or a list, whose element or field is `result`.
struct DeferredIndex {
    value: Span,
    value_ty: Ty,
    slice: Expr,
    result: Ty,
}

/// What [`Inference::subscript_of`] indexed.
#[derive(Clone, Copy, PartialEq)]
enum Indexed {
    List,
    Str,
    Tuple,
}

/// What a name refers to where it is used.
enum Resolved {
    Variable(usize),
    Function(String),
    Enum(String),
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
            SymbolKind::Function | SymbolKind::Enum | SymbolKind::Global => None,
            // set by the caller
            SymbolKind::Variant => None,
        };
        self.symbols.push(Symbol {
            name: name.to_string(),
            kind,
            span,
            // placeholder until `finish` resolves `symbol_types`
            ty: Type::Unit,
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
            Ty::Tuple(fields) => Ty::Tuple(fields.iter().map(|f| self.resolve(f)).collect()),
            Ty::Enum(name, args) => Ty::Enum(name, args.iter().map(|a| self.resolve(a)).collect()),
            other => other,
        }
    }

    fn occurs(&self, var: usize, ty: &Ty) -> bool {
        match self.shallow(ty) {
            Ty::Var(v) => v == var,
            Ty::List(elem) => self.occurs(var, &elem),
            Ty::Tuple(fields) | Ty::Enum(_, fields) => fields.iter().any(|f| self.occurs(var, f)),
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
            (Ty::Tuple(xs), Ty::Tuple(ys)) => {
                // unify every field, so each is as solved as it can be even if one differs
                xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip(&ys)
                        .fold(true, |same, (x, y)| self.unify(x, y) && same)
            }
            (Ty::Enum(a, xs), Ty::Enum(b, ys)) => {
                a == b
                    && xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip(&ys)
                        .fold(true, |same, (x, y)| self.unify(x, y) && same)
            }
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
            Ty::Unit => "no value".to_string(),
            Ty::List(elem) => format!("list[{}]", self.show(&elem)),
            Ty::Tuple(fields) => {
                let fields: Vec<String> = fields.iter().map(|f| self.show(f)).collect();
                format!("tuple[{}]", fields.join(", "))
            }
            Ty::Enum(name, args) if args.is_empty() => name,
            Ty::Enum(name, args) => {
                let args: Vec<String> = args.iter().map(|a| self.show(a)).collect();
                format!("{name}[{}]", args.join(", "))
            }
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
        let scalar = |ty: &Ty| {
            matches!(
                ty,
                Ty::Str | Ty::Bool | Ty::Unit | Ty::Tuple(_) | Ty::Enum(..)
            )
        };
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
                // a tuple or enum is never an operand, whatever the other one is
                if let Some(tuple) = [&left, &right]
                    .into_iter()
                    .find(|ty| matches!(ty, Ty::Tuple(_) | Ty::Enum(..)))
                {
                    self.require(tuple.clone(), allowed, op.span, message);
                    self.unify(&op.result, &left);
                    return true;
                }
                self.expect(&left, &right, op.operand_span);
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

    /// Requires `ty` to be `bool`, as conditions and the operands of `not`, `and`, and `or` must
    /// be, so `if 1 or 0;` is an error while `if n != 0;` is fine.
    fn require_condition(&mut self, ty: Ty, span: Span) {
        self.require(
            ty,
            &[Kind::Bool],
            span,
            "a condition must be bool, found {}",
        );
    }

    ////////////////////////////////////////////////////////////////
    // declarations
    ////////////////////////////////////////////////////////////////

    fn check_module(&mut self, body: &[Stmt]) {
        // enums, functions, and globals are visible everywhere, so declare them all first, enums
        // before the signatures that name them
        self.declare_enums(body);
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
                && !self.enums.contains_key(&name)
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
    /// `finish`; a function without `->` returns no value.
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
        if self.enums.contains_key(name) {
            self.error(name_span, format!("'{name}' is already defined as an enum"));
            return;
        }
        let mut params = vec![];
        for arg in &args.args {
            let ty = match &arg.annotation {
                Some(annotation) => self.written(annotation),
                None => {
                    let ty = self.fresh();
                    self.untyped_params
                        .push((arg.arg.clone(), arg.span, ty.clone()));
                    ty
                }
            };
            params.push(ty);
        }
        let ret = returns.map_or(Ty::Unit, |returns| self.written(returns));
        let written = args.args.iter().filter_map(|arg| arg.annotation.as_ref());
        for annotation in written.chain(returns) {
            self.check_written_width(annotation);
        }
        // the symbol's type is rebuilt from `params` and `ret` in `finish`
        let symbol = self.add_symbol(name, SymbolKind::Function, name_span, Ty::Unit);
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

    /// Declares every enum defined at the top level of `body`: first their names, so variants can
    /// hold any enum, including their own, then their variants.
    ///
    /// For example, `enum Tree;` with variants `Leaf(int)` and `Node(Tree, Tree)` declares `Tree`
    /// with a variant `Node` that holds two `Tree`s.
    fn declare_enums(&mut self, body: &[Stmt]) {
        let mut declared = vec![];
        for stmt in body {
            let StmtKind::EnumDef {
                name,
                name_span,
                variants,
                ..
            } = &stmt.kind
            else {
                continue;
            };
            self.enum_spans.insert(stmt.span);
            if self.enums.contains_key(name) {
                self.error(*name_span, format!("enum '{name}' is already defined"));
                continue;
            }
            if BUILTINS.contains(&name.as_str()) || BUILTIN_TYPES.contains(&name.as_str()) {
                self.error(*name_span, format!("cannot redefine builtin '{name}'"));
                continue;
            }
            let ty = Ty::Enum(name.clone(), vec![]);
            let symbol = self.add_symbol(name, SymbolKind::Enum, *name_span, ty);
            self.enums.insert(
                name.clone(),
                EnumInfo {
                    symbol,
                    variants: vec![],
                },
            );
            declared.push((name, variants));
        }
        for (name, variants) in declared {
            let mut infos: Vec<VariantInfo> = vec![];
            for variant in variants {
                if infos.iter().any(|info| info.name == variant.name) {
                    self.error(
                        variant.span,
                        format!("'{name}' already has a variant '{}'", variant.name),
                    );
                    continue;
                }
                let fields: Vec<Ty> = variant.fields.iter().map(|f| self.written(f)).collect();
                for field in &variant.fields {
                    self.check_written_width(field);
                }
                self.check_variant_width(name, variant, &fields);
                let symbol = self.add_symbol(
                    &variant.name,
                    SymbolKind::Variant,
                    variant.span,
                    Ty::Enum(name.clone(), vec![]),
                );
                self.symbols[symbol].scope = Some(name.clone());
                infos.push(VariantInfo {
                    name: variant.name.clone(),
                    symbol,
                    fields,
                });
            }
            if let Some(info) = self.enums.get_mut(name) {
                info.variants = infos;
            }
        }
    }

    /// Reports a variant whose values, counting each value of its tuples, are more than an enum
    /// value can hold.
    fn check_variant_width(&mut self, name: &str, variant: &Variant, fields: &[Ty]) {
        let count: usize = fields.iter().map(|f| self.values(f)).sum();
        if count > MAX_ELEMENT_VALUES {
            self.error(
                variant.span,
                format!(
                    "a variant can hold at most {MAX_ELEMENT_VALUES} values, but '{name}.{}' \
                     holds {count}",
                    variant.name
                ),
            );
        }
    }

    /// Counts the values a value of type `ty` is made of, counting each value of its tuples.
    fn values(&self, ty: &Ty) -> usize {
        match self.shallow(ty) {
            Ty::Tuple(fields) => fields.iter().map(|f| self.values(f)).sum(),
            _ => 1,
        }
    }

    /// Reports a list type written in `annotation` whose elements hold too many values to store.
    fn check_written_width(&mut self, annotation: &TypeExpr) {
        match &annotation.kind {
            TypeExprKind::List(elem) => {
                let elem_ty = self.written_type(elem, &mut vec![]);
                self.check_element_width(&elem_ty, annotation.span);
                self.check_written_width(elem);
            }
            TypeExprKind::Tuple(fields) => fields.iter().for_each(|f| self.check_written_width(f)),
            TypeExprKind::Named { args, .. } => {
                args.iter().for_each(|a| self.check_written_width(a))
            }
            _ => {}
        }
    }

    /// Reports at `span` if a list element of type `elem` holds more values than
    /// [`MAX_ELEMENT_VALUES`], counting each field of its tuples.
    fn check_element_width(&mut self, elem: &Ty, span: Span) {
        let count = self.values(elem);
        if count > MAX_ELEMENT_VALUES {
            let message = format!(
                "a list element can hold at most {MAX_ELEMENT_VALUES} values, but {} holds {count}",
                self.show(elem)
            );
            self.error(span, message);
        }
    }

    ////////////////////////////////////////////////////////////////
    // statements
    ////////////////////////////////////////////////////////////////

    /// Converts a type written in a signature to an inference type, reporting a name that is not
    /// a type.
    ///
    /// For example, `list[float]` becomes `Ty::List(Box::new(Ty::Float))`.
    fn written(&mut self, annotation: &TypeExpr) -> Ty {
        let mut errors = vec![];
        let ty = self.written_type(annotation, &mut errors);
        for (span, message) in errors {
            self.error(span, message);
        }
        ty
    }

    /// Converts a type written in a signature as [`Inference::written`] does, pushing its errors
    /// to `errors`. A name that is not a type becomes an unknown type, so checking goes on.
    fn written_type(&mut self, annotation: &TypeExpr, errors: &mut Vec<(Span, String)>) -> Ty {
        match &annotation.kind {
            TypeExprKind::Int => Ty::Int,
            TypeExprKind::Float => Ty::Float,
            TypeExprKind::Bool => Ty::Bool,
            TypeExprKind::Str => Ty::Str,
            TypeExprKind::List(elem) => Ty::List(Box::new(self.written_type(elem, errors))),
            TypeExprKind::Tuple(fields) => Ty::Tuple(
                fields
                    .iter()
                    .map(|f| self.written_type(f, errors))
                    .collect(),
            ),
            TypeExprKind::Named { name, args } => {
                let args: Vec<Ty> = args.iter().map(|a| self.written_type(a, errors)).collect();
                let takes = if name == OPTION {
                    1
                } else if let Some(info) = self.enums.get(name) {
                    // a type that takes no types is only its name
                    self.add_reference(annotation.span, info.symbol);
                    0
                } else {
                    errors.push((annotation.span, format!("there is no type '{name}'")));
                    return self.fresh();
                };
                if args.len() != takes {
                    let message = match takes {
                        1 => format!("'{name}' takes one type, as in '{name}[int]'"),
                        _ => format!("'{name}' takes no types"),
                    };
                    errors.push((annotation.span, message));
                    return self.fresh();
                }
                Ty::Enum(name.clone(), args)
            }
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
                    None => Ty::Unit,
                };
                if let Some(function) = &self.function {
                    let (name, ret) = (function.name.clone(), function.ret.clone());
                    let ret_written = function.ret_written;
                    if !self.unify(&ret, &found) {
                        let span = value.as_ref().map_or(stmt.span, |v| v.span);
                        if value.is_none() {
                            let message = format!(
                                "'{name}' returns {}, so 'ret' needs a value",
                                self.show(&ret)
                            );
                            self.error(span, message);
                        } else if ret_written {
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
                        let (elem, indexed) = self.subscript_of(list, slice);
                        if let Some(message) = indexed.change_error() {
                            self.error(target.span, message);
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
                        } else if let Ty::Tuple(_) = self.shallow(&iter_ty) {
                            self.error(
                                iter.span,
                                "a tuple cannot be iterated; unpack it, or use a list",
                            );
                            self.fresh()
                        } else {
                            let elem = self.fresh();
                            self.expect(&Ty::List(Box::new(elem.clone())), &iter_ty, iter.span);
                            elem
                        }
                    }
                };
                if matches!(target.kind, ExprKind::Name { .. } | ExprKind::Tuple { .. }) {
                    self.check_target(target, &elem, target.span);
                } else {
                    self.error(
                        target.span,
                        "a 'for' loop's variable must be a name, or names to unpack each element \
                         into, as in 'for a, b in pairs'",
                    );
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
                // the one place a call that returns no value can be
                let ty = self.infer_kind(value);
                self.expr_types.push((value.span, ty));
                self.statement_call = None;
            }
            StmtKind::EnumDef { name_span, .. } => {
                if !self.enum_spans.contains(&stmt.span) {
                    self.error(*name_span, "enums can only be defined at the top level");
                }
            }
            StmtKind::Match { subject, cases } => self.check_match(subject, cases),
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
            self.expr_types.push((func.span, Ty::Unit));
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
            // assigning an enum's name is reported where it happens
            if self.local(&local).is_none() && !self.enums.contains_key(&local) {
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

        // falling off the end returns no value, which must agree with every `ret`
        if !always_exits(body) && !self.unify(&ret, &Ty::Unit) {
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
                    Resolved::Enum(_) => {
                        self.error(target.span, format!("cannot assign to enum '{id}'"));
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
                let (elem, indexed) = self.subscript_of(value, slice);
                if let Some(message) = indexed.change_error() {
                    self.error(target.span, message);
                    return;
                }
                if matches!(slice.kind, ExprKind::Slice { .. }) {
                    self.error(target.span, SLICE_CHANGE);
                    return;
                }
                self.expect(&elem, found, value_span);
                self.check_change(value);
            }
            ExprKind::Tuple { .. } => {
                if self.check_unpacking_names(target) {
                    self.unpack(target, found, value_span);
                } else {
                    // still declare each name, so the error does not lead to others
                    let mut names = Vec::new();
                    target_names(target, &mut names);
                    for name in names {
                        let unknown = self.fresh();
                        self.check_target(name, &unknown, value_span);
                    }
                }
            }
            _ => self.error(target.span, "cannot assign to this expression"),
        }
    }

    /// Checks that the tuple target `target` holds only names and nested tuples of names, each
    /// name once, reporting the first that does not and returning whether all do.
    ///
    /// For example, `a, (b, c)` is fine, while `a, xs[0]` and `a, a` are errors.
    fn check_unpacking_names(&mut self, target: &Expr) -> bool {
        fn first_other(target: &Expr) -> Option<&Expr> {
            match &target.kind {
                ExprKind::Name { .. } => None,
                ExprKind::Tuple { elts, .. } => elts.iter().find_map(first_other),
                _ => Some(target),
            }
        }
        if let Some(other) = first_other(target) {
            self.error(
                other.span,
                "only names can be unpacked into, as in 'a, b = t'; assign an element on a line of \
                 its own",
            );
            return false;
        }
        let mut names = Vec::new();
        target_names(target, &mut names);
        for (i, name) in names.iter().enumerate() {
            let ExprKind::Name { id, .. } = &name.kind else {
                continue;
            };
            let repeated = names[..i].iter().any(
                |earlier| matches!(&earlier.kind, ExprKind::Name { id: other, .. } if other == id),
            );
            if repeated {
                self.error(name.span, format!("'{id}' is unpacked into more than once"));
                return false;
            }
        }
        true
    }

    /// Assigns the fields of `found` to the names of the tuple target `target`, at any depth.
    ///
    /// For example, `q, r = (7, "x")` makes `q` an `int` and `r` a `str`, and unpacking a value not
    /// yet known, such as `xs.pop()` of an empty list, makes it a tuple.
    fn unpack(&mut self, target: &Expr, found: &Ty, value_span: Span) {
        let ExprKind::Tuple { elts, .. } = &target.kind else {
            self.check_target(target, found, value_span);
            return;
        };
        // lowering sizes each nested target from its type
        self.expr_types.push((target.span, found.clone()));
        let fields: Vec<Ty> = elts.iter().map(|_| self.fresh()).collect();
        let fits = match self.shallow(found) {
            Ty::Tuple(have) if have.len() != elts.len() => {
                let message = format!(
                    "cannot unpack {} into {} names; it has {} values",
                    self.show(found),
                    elts.len(),
                    have.len()
                );
                self.error(target.span, message);
                false
            }
            Ty::Tuple(_) | Ty::Var(_) => self.unify(&Ty::Tuple(fields.clone()), found),
            _ => {
                let message = format!(
                    "cannot unpack {}; only a tuple can be unpacked, as in 'a, b = (1, 2)'",
                    self.show(found)
                );
                self.error(target.span, message);
                false
            }
        };
        for (elt, field) in elts.iter().zip(fields) {
            // after an error, each name is still declared, with a type nothing pins down
            let field = if fits { field } else { self.fresh() };
            self.unpack(elt, &field, value_span);
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
        // a list inside a tuple, as in `t[0].append(1)`, cannot change
        let mut node = list;
        while let ExprKind::Subscript { value, .. } = &node.kind {
            if self.indexed_tuples.contains(&value.span) {
                self.error(node.span, TUPLE_CHANGE);
                return;
            }
            node = value;
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
        if self.enums.contains_key(name) {
            return Resolved::Enum(name.to_string());
        }
        if is_builtin(name) {
            return Resolved::Builtin;
        }
        Resolved::Undefined
    }

    ////////////////////////////////////////////////////////////////
    // expressions
    ////////////////////////////////////////////////////////////////

    /// Infers the type of an expression used as a value, recording it by span. A call that
    /// returns no value is reported, since only a statement can be one.
    fn infer(&mut self, expr: &Expr) -> Ty {
        let ty = self.infer_kind(expr);
        self.expr_types.push((expr.span, ty.clone()));
        if self.shallow(&ty) == Ty::Unit {
            if changed_receiver(expr).is_some() {
                // a method that changes a list already said it can only be a statement
                return self.fresh();
            }
            let called = match &expr.kind {
                ExprKind::Call { func, .. } => match &func.kind {
                    ExprKind::Name { id, .. } => Some(id.as_str()),
                    ExprKind::Attribute { attr, .. } => Some(attr.as_str()),
                    _ => None,
                },
                _ => None,
            };
            let message = match called {
                Some(name) => format!(
                    "'{name}' returns no value, so it can only be called as a statement of its \
                     own"
                ),
                None => "this has no value".to_string(),
            };
            self.error(expr.span, message);
            return self.fresh();
        }
        ty
    }

    /// Returns the span of the name `attr` at the end of `func`, the attribute `value.attr`.
    fn attr_span(func: &Expr, attr: &str) -> Span {
        let end = func.span.end;
        let start = Pos::new(end.line, end.col.saturating_sub(attr.chars().count()));
        Span::new(start, end).in_file(func.span.file)
    }

    /// Reports a use of the enum `id` as a value, which only its variants are.
    fn enum_as_value(&mut self, span: Span, id: &str) {
        let example = self
            .enums
            .get(id)
            .and_then(|info| info.variants.first())
            .map_or_else(String::new, |v| format!(", as in '{id}.{}'", v.name));
        if let Some(info) = self.enums.get(id) {
            let symbol = info.symbol;
            self.add_reference(span, symbol);
        }
        self.error(
            span,
            format!("'{id}' is an enum, so a value of it is one of its variants{example}"),
        );
    }

    /// Infers the variant `enum_name.variant` of an enum the program defines, written as `func`
    /// (whose value names the enum), with the values `args` if it is called, and returns the
    /// enum's type.
    ///
    /// For example, `Shape.Circle(1.0)` is a `Shape` if `Circle` holds one `float`, and
    /// `Shape.Empty` is one if `Empty` holds nothing.
    fn infer_variant(
        &mut self,
        expr: &Expr,
        func: &Expr,
        enum_name: &str,
        variant: &str,
        args: Option<&[Expr]>,
    ) -> Ty {
        let arg_types: Vec<Ty> = args
            .unwrap_or_default()
            .iter()
            .map(|arg| self.infer(arg))
            .collect();
        let enum_ty = Ty::Enum(enum_name.to_string(), vec![]);
        let ExprKind::Attribute { value, .. } = &func.kind else {
            return enum_ty;
        };
        let info = &self.enums[enum_name];
        let enum_symbol = info.symbol;
        let found = info
            .variants
            .iter()
            .find(|v| v.name == variant)
            .map(|v| (v.symbol, v.fields.clone()));
        self.add_reference(value.span, enum_symbol);
        let attr_span = Self::attr_span(func, variant);
        let Some((symbol, fields)) = found else {
            self.error(
                attr_span,
                format!("'{enum_name}' has no variant '{variant}'"),
            );
            return enum_ty;
        };
        self.add_reference(attr_span, symbol);
        let written = format!("{enum_name}.{variant}");
        match args {
            None if !fields.is_empty() => {
                let count = values_phrase(fields.len());
                self.error(
                    expr.span,
                    format!(
                        "'{written}' holds {count}, so make one with a call, as in '{written}(...)'"
                    ),
                );
            }
            Some(_) if fields.is_empty() => {
                self.error(
                    expr.span,
                    format!(
                        "'{written}' holds no values, so it is written without parentheses, as \
                         '{written}'"
                    ),
                );
            }
            Some(args) if args.len() != fields.len() => {
                let verb = if args.len() == 1 { "was" } else { "were" };
                self.error(
                    expr.span,
                    format!(
                        "'{written}' holds {}, but {} {verb} given",
                        values_phrase(fields.len()),
                        args.len()
                    ),
                );
            }
            Some(args) => {
                for ((field, arg_ty), arg) in fields.iter().zip(&arg_types).zip(args) {
                    self.expect(field, arg_ty, arg.span);
                }
            }
            None => {}
        }
        enum_ty
    }

    /// Returns the name of the enum `value` names, if it is one, as in `Shape` in `Shape.Circle`.
    fn enum_named(&self, value: &Expr) -> Option<String> {
        let ExprKind::Name { id, .. } = &value.kind else {
            return None;
        };
        match self.resolve_name(id) {
            Resolved::Enum(name) => Some(name),
            _ => None,
        }
    }

    fn infer_kind(&mut self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Constant { value, .. } => match **value {
                Constant::Bool(_) => Ty::Bool,
                Constant::Str(_) | Constant::Char(_) => Ty::Str,
                Constant::None => Ty::Enum(OPTION.to_string(), vec![self.fresh()]),
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
                Resolved::Enum(_) => {
                    self.enum_as_value(expr.span, id);
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
                for value in values {
                    let ty = self.infer(value);
                    self.require_condition(ty, value.span);
                }
                Ty::Bool
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
                        CompOp::Equal | CompOp::NotEqual => {
                            (EQUATABLE, "'==' and '!=' cannot compare {}")
                        }
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
                if let Some(name) = self.enum_named(value) {
                    return self.infer_variant(expr, expr, &name, attr, None);
                }
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
                let list = Ty::List(Box::new(elem));
                self.list_literals.push((expr.span, list.clone()));
                list
            }
            ExprKind::Tuple { elts, .. } => {
                Ty::Tuple(elts.iter().map(|elt| self.infer(elt)).collect())
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

    /// Infers `value[slice]`, which must index a list or a string with an int, or slice it, or
    /// index a tuple with a constant. An index gives an element of a list, a string of one byte,
    /// or a field of a tuple, and a slice gives a value of the same type. A value not yet known to
    /// be a string or a tuple is taken to be a list.
    ///
    /// For example, `xs[0]` of a `list[int]` is `int`, `s[1:]` of a `str` is `str`, and `t[-1]`
    /// of a `tuple[int, str]` is `str`.
    fn subscript(&mut self, value: &Expr, slice: &Expr) -> Ty {
        self.subscript_of(value, slice).0
    }

    /// Infers `value[slice]` as [`Inference::subscript`] does, also returning what it indexes.
    fn subscript_of(&mut self, value: &Expr, slice: &Expr) -> (Ty, Indexed) {
        let value_ty = self.infer(value);
        if let Ty::Var(_) = self.shallow(&value_ty)
            && self.is_global_in_function(value)
            && constant_int(slice).is_some()
        {
            // a function checked before the global's assignment, which may make it a tuple
            let index = self.infer(slice);
            self.expect(&Ty::Int, &index, slice.span);
            let result = self.fresh();
            self.deferred_indexes.push(DeferredIndex {
                value: value.span,
                value_ty,
                slice: slice.clone(),
                result: result.clone(),
            });
            return (result, Indexed::List);
        }
        let (sliced, elem, indexed) = match self.shallow(&value_ty) {
            Ty::Str => (Ty::Str, Ty::Str, Indexed::Str),
            Ty::Tuple(fields) => {
                return (self.tuple_field(value.span, &fields, slice), Indexed::Tuple);
            }
            _ => {
                let elem = self.fresh();
                let list = Ty::List(Box::new(elem.clone()));
                self.expect(&list, &value_ty, value.span);
                (list, elem, Indexed::List)
            }
        };
        if let ExprKind::Slice { lower, upper, step } = &slice.kind {
            for bound in [lower, upper, step].into_iter().flatten() {
                let ty = self.infer(bound);
                self.expect(&Ty::Int, &ty, bound.span);
            }
            return (sliced, indexed);
        }
        let index = self.infer(slice);
        self.expect(&Ty::Int, &index, slice.span);
        (elem, indexed)
    }

    /// Returns whether `value` names a global from inside a function.
    fn is_global_in_function(&self, value: &Expr) -> bool {
        let ExprKind::Name { id, .. } = &value.kind else {
            return false;
        };
        self.function.is_some() && self.local(id).is_none() && self.globals.contains_key(id)
    }

    /// Settles a constant index into a global that was not known when it was found: a field if
    /// the global turned out to be a tuple, and otherwise an element of a list.
    fn settle_index(&mut self, deferred: DeferredIndex) {
        match self.shallow(&deferred.value_ty) {
            Ty::Tuple(fields) => {
                let field = self.tuple_field(deferred.value, &fields, &deferred.slice);
                self.expect(&field, &deferred.result, deferred.slice.span);
            }
            _ => {
                let list = Ty::List(Box::new(deferred.result));
                self.expect(&list, &deferred.value_ty, deferred.value);
            }
        }
    }

    /// Infers field `slice` of the tuple at `value`, whose fields are `fields`. The index must be
    /// an int written in the code, counting from the end if it is negative, so the field's type is
    /// known.
    ///
    /// For example, `t[1]` and `t[-1]` of a `tuple[int, str]` are `str`, while `t[i]` and `t[2]`
    /// are errors.
    fn tuple_field(&mut self, value: Span, fields: &[Ty], slice: &Expr) -> Ty {
        self.indexed_tuples.insert(value);
        if matches!(slice.kind, ExprKind::Slice { .. }) {
            self.error(
                slice.span,
                "a tuple cannot be sliced; unpack it to name its values, as in 'a, b, c = t'",
            );
            return self.fresh();
        }
        let index = self.infer(slice);
        self.expect(&Ty::Int, &index, slice.span);
        let Some(k) = constant_int(slice) else {
            self.error(
                slice.span,
                "a tuple index must be a number written in the code, such as 't[0]'; unpack the \
                 tuple to name its values, as in 'a, b = t'",
            );
            return self.fresh();
        };
        let len = fields.len() as i64;
        let position = if k < 0 { k.wrapping_add(len) } else { k };
        if !(0..len).contains(&position) {
            let message = format!(
                "index {k} is out of range for {}, which has {len} values",
                self.show(&Ty::Tuple(fields.to_vec()))
            );
            self.error(slice.span, message);
            return self.fresh();
        }
        fields[position as usize].clone()
    }

    fn infer_call(&mut self, call: &Expr, func: &Expr, args: &[Expr]) -> Ty {
        if let ExprKind::Attribute { value, attr, .. } = &func.kind {
            if let Some(name) = self.enum_named(value) {
                return self.infer_variant(call, func, &name, attr, Some(args));
            }
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
                self.expr_types.push((func.span, Ty::Unit));
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
            Resolved::Enum(_) => {
                self.enum_as_value(func.span, id);
                self.fresh()
            }
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

    /// Checks a `match` statement: each case's pattern against the subject's type, binding its
    /// names, then the case's body. Whether the cases cover every value is checked in `finish`,
    /// once the subject's type is known.
    fn check_match(&mut self, subject: &Expr, cases: &[MatchCase]) {
        let ty = self.infer(subject);
        for case in cases {
            let mut bound = vec![];
            self.check_pattern(&case.pattern, &ty, &mut bound);
            self.check_block(&case.body);
        }
        self.matches.push(PendingMatch {
            subject: ty,
            subject_span: subject.span,
            patterns: cases.iter().map(|case| case.pattern.clone()).collect(),
        });
    }

    /// Checks that `pattern` can match a value of type `expected`, binding each name in it to a
    /// variable of the type of the value it matches. `bound` holds the names bound so far.
    ///
    /// For example, `Shape.Rect(w, h)` against a `Shape` makes `w` and `h` floats if `Rect` holds
    /// two floats.
    fn check_pattern(&mut self, pattern: &Pattern, expected: &Ty, bound: &mut Vec<String>) {
        self.expr_types.push((pattern.span, expected.clone()));
        match &pattern.kind {
            PatternKind::Wildcard => {}
            PatternKind::Name(id) => {
                if bound.contains(id) {
                    self.error(
                        pattern.span,
                        format!("'{id}' is bound more than once in this pattern"),
                    );
                    return;
                }
                bound.push(id.clone());
                if let Ty::Enum(name, _) = self.shallow(expected)
                    && let Some(info) = self.enums.get(&name)
                    && info.variants.iter().any(|v| v.name == *id)
                {
                    let short = name.rsplit('.').next().unwrap_or(&name);
                    self.error(
                        pattern.span,
                        format!(
                            "'{id}' here is a new variable that matches any value; to match the \
                             variant, write '{short}.{id}'"
                        ),
                    );
                    return;
                }
                let target = Expr::new(
                    ExprKind::Name {
                        id: id.clone(),
                        ctx: crate::ast::ExprContext::Store,
                    },
                    pattern.span,
                );
                self.check_target(&target, expected, pattern.span);
            }
            PatternKind::Variant { path, fields } => {
                let Some((enum_ty, written, field_types)) = self.variant_of_pattern(path) else {
                    for field in fields {
                        let unknown = self.fresh();
                        self.check_pattern(field, &unknown, bound);
                    }
                    return;
                };
                if !self.unify(expected, &enum_ty) {
                    let message = format!(
                        "this pattern matches {}, but the value is {}",
                        self.show(&enum_ty),
                        self.show(expected)
                    );
                    self.error(pattern.span, message);
                }
                if fields.len() != field_types.len() {
                    let message = match field_types.len() {
                        0 => format!(
                            "'{written}' holds no values, so its pattern is written without \
                             parentheses, as '{written}'"
                        ),
                        n => format!(
                            "'{written}' holds {}, but this pattern has {}",
                            values_phrase(n),
                            fields.len()
                        ),
                    };
                    self.error(pattern.span, message);
                }
                for (i, field) in fields.iter().enumerate() {
                    let field_ty = field_types.get(i).cloned().unwrap_or_else(|| self.fresh());
                    self.check_pattern(field, &field_ty, bound);
                }
            }
        }
    }

    /// Resolves the variant a pattern names, as `path`, to its enum's type, the variant as
    /// written, and the types of its values, reporting a path that names no variant.
    ///
    /// For example, `["Shape", "Circle"]` gives `Shape`, `Shape.Circle`, and `[float]`, and
    /// `["some"]` gives `option[T]`, `some`, and `[T]` for a new unknown `T`.
    fn variant_of_pattern(&mut self, path: &[(String, Span)]) -> Option<(Ty, String, Vec<Ty>)> {
        match path {
            [(name, _)] if name == "none" => {
                let elem = self.fresh();
                Some((
                    Ty::Enum(OPTION.to_string(), vec![elem]),
                    name.clone(),
                    vec![],
                ))
            }
            [(name, _)] if name == "some" => {
                let elem = self.fresh();
                let option = Ty::Enum(OPTION.to_string(), vec![elem.clone()]);
                Some((option, name.clone(), vec![elem]))
            }
            [(name, span)] => {
                self.error(
                    *span,
                    format!(
                        "'{name}' is not a variant; write a variant with its enum, as in \
                         'Shape.Circle(r)', or 'some(x)'"
                    ),
                );
                None
            }
            [(enum_name, enum_span), (variant, variant_span)] => {
                let Resolved::Enum(name) = self.resolve_name(enum_name) else {
                    self.error(*enum_span, format!("'{enum_name}' is not an enum"));
                    return None;
                };
                let info = &self.enums[&name];
                let enum_symbol = info.symbol;
                let found = info
                    .variants
                    .iter()
                    .find(|v| v.name == *variant)
                    .map(|v| (v.symbol, v.fields.clone()));
                self.add_reference(*enum_span, enum_symbol);
                let Some((symbol, fields)) = found else {
                    self.error(
                        *variant_span,
                        format!("'{enum_name}' has no variant '{variant}'"),
                    );
                    return None;
                };
                self.add_reference(*variant_span, symbol);
                let written = format!("{enum_name}.{variant}");
                Some((Ty::Enum(name, vec![]), written, fields))
            }
            _ => {
                let names: Vec<&str> = path.iter().map(|(name, _)| name.as_str()).collect();
                let span = path[0].1.to(path[path.len() - 1].1);
                self.error(span, format!("'{}' is not a variant", names.join(".")));
                None
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
                    return Ty::Unit;
                }
                self.add_element(receiver, &receiver_ty, &args[0], &arg_types[0]);
                Ty::Unit
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
                    return Ty::Unit;
                }
                self.expect(&Ty::Int, &arg_types[0], args[0].span);
                self.add_element(receiver, &receiver_ty, &args[1], &arg_types[1]);
                Ty::Unit
            }
            "extend" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                    return Ty::Unit;
                }
                let list = Ty::List(Box::new(self.fresh()));
                if self.unify(&list, &receiver_ty) {
                    self.expect(&list, &arg_types[0], args[0].span);
                } else {
                    // the argument pins down the message, as in `expected list[int]`
                    self.unify(&list, &arg_types[0]);
                    self.expect(&list, &receiver_ty, receiver.span);
                }
                Ty::Unit
            }
            "is_some" | "is_none" | "unwrap" | "unwrap_or" => {
                let elem = self.fresh();
                let option = Ty::Enum(OPTION.to_string(), vec![elem.clone()]);
                if !self.unify(&option, &receiver_ty) {
                    let message = format!(
                        "'{name}' needs an option, found {}",
                        self.show(&receiver_ty)
                    );
                    self.error(receiver.span, message);
                }
                match (name, args) {
                    ("unwrap_or", [default]) => self.expect(&elem, &arg_types[0], default.span),
                    ("unwrap_or", _) => self.arity_error(call.span, name, "1", args.len()),
                    (_, []) => {}
                    _ => self.arity_error(call.span, name, "0", args.len()),
                }
                match name {
                    "is_some" | "is_none" => Ty::Bool,
                    _ => elem,
                }
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
            "print" => Ty::Unit,
            "some" => match arg_types {
                [value] => Ty::Enum(OPTION.to_string(), vec![value.clone()]),
                _ => {
                    self.arity_error(call.span, name, "1", args.len());
                    Ty::Enum(OPTION.to_string(), vec![self.fresh()])
                }
            },
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
                    _ => Ty::Unit,
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
                Ty::Unit
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
                Ty::Unit
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
        for deferred in std::mem::take(&mut self.deferred_indexes) {
            self.settle_index(deferred);
        }
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
        for (span, list) in std::mem::take(&mut self.list_literals) {
            if let Ty::List(elem) = self.resolve(&list) {
                self.check_element_width(&elem, span);
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
                    Kind::Unit => Ty::Unit,
                    Kind::List => Ty::List(Box::new(self.fresh())),
                    // no constraint allows a tuple or enum first, and nothing tells which
                    Kind::Tuple | Kind::Enum => continue,
                };
                self.unify(&ty, &default);
                continue;
            };
            if !constraint.allowed.contains(&kind) {
                let mut message = constraint.message.replace("{}", &self.show(&ty));
                if kind == Kind::Enum && constraint.allowed == EQUATABLE {
                    message.push_str(
                        "; tell its variants apart with 'match', or an option's with 'is_some()' \
                         and 'is_none()'",
                    );
                }
                self.error(constraint.span, message);
            }
        }
        for pending in std::mem::take(&mut self.matches) {
            self.check_cases(pending);
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
            let ty = match symbol.kind {
                SymbolKind::Function => {
                    let info = &self.functions[&symbol.name];
                    Type::Function {
                        params: info.params.iter().map(|p| self.finalize(p)).collect(),
                        ret: Box::new(self.finalize(&info.ret)),
                    }
                }
                SymbolKind::Variant => {
                    let enum_name = symbol.scope.clone().unwrap_or_default();
                    let fields = self.enums[&enum_name]
                        .variants
                        .iter()
                        .find(|v| v.symbol == i)
                        .map(|v| v.fields.iter().map(|f| self.finalize(f)).collect())
                        .unwrap_or_default();
                    Type::Function {
                        params: fields,
                        ret: Box::new(Type::Enum {
                            name: enum_name,
                            args: vec![],
                        }),
                    }
                }
                _ => self.finalize(&self.symbol_types[i]),
            };
            analysis.symbols.push(Symbol {
                ty,
                ..symbol.clone()
            });
        }
        for (name, info) in &self.enums {
            let variants = info
                .variants
                .iter()
                .map(|v| {
                    let fields = v.fields.iter().map(|f| self.finalize(f)).collect();
                    (v.name.clone(), fields)
                })
                .collect();
            analysis.enums.insert(name.clone(), variants);
        }
        analysis.references = self.references;
        analysis
            .references
            .sort_by_key(|r| (r.span.file, r.span.start));
        // a written type can be read more than once
        analysis.references.dedup();
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
            Ty::Unit => Type::Unit,
            Ty::List(elem) => Type::List(Box::new(self.finalize(&elem))),
            Ty::Tuple(fields) => Type::Tuple(fields.iter().map(|f| self.finalize(f)).collect()),
            Ty::Enum(name, args) => Type::Enum {
                name,
                args: args.iter().map(|a| self.finalize(a)).collect(),
            },
        }
    }

    ////////////////////////////////////////////////////////////////
    // match cases
    ////////////////////////////////////////////////////////////////

    /// Checks that the cases of a `match` cover every value of its subject, reporting one they
    /// miss, and warns about a case that can never run because the cases before it match every
    /// value it would.
    ///
    /// This is the usefulness check of Maranget's "Warnings for pattern matching": a case is
    /// reachable if some value matches it and no case before it.
    fn check_cases(&mut self, pending: PendingMatch) {
        let subject = self.resolve(&pending.subject);
        if !matches!(subject, Ty::Enum(..)) {
            let message = format!(
                "'match' needs an enum or option value, found {}",
                self.show(&subject)
            );
            self.error(pending.subject_span, message);
            return;
        }
        let mut rows: Vec<Vec<Space>> = vec![];
        for pattern in &pending.patterns {
            // a pattern that names no variant was reported already
            let Some(space) = self.space(pattern, &subject) else {
                return;
            };
            if !self.useful(
                &rows,
                std::slice::from_ref(&space),
                std::slice::from_ref(&subject),
            ) {
                self.diagnostics.push(Diagnostic::warning(
                    pattern.span,
                    "this case never runs, since the cases before it match every value it would",
                ));
            }
            rows.push(vec![space]);
        }
        if let Some(witness) = self.missing(&rows, std::slice::from_ref(&subject)) {
            let missed = &witness[0];
            self.error(
                pending.subject_span,
                format!("'match' does not cover {missed}; add 'case {missed};' or 'case _;'"),
            );
        }
    }

    /// Returns the variants of the enum type `ty`, each with the types of its values, or `None` if
    /// `ty` is not an enum.
    fn ty_variants(&self, ty: &Ty) -> Option<Vec<(String, Vec<Ty>)>> {
        match self.resolve(ty) {
            Ty::Enum(name, args) if name == OPTION => Some(vec![
                ("none".to_string(), vec![]),
                ("some".to_string(), args),
            ]),
            Ty::Enum(name, _) => self.enums.get(&name).map(|info| {
                info.variants
                    .iter()
                    .map(|v| (v.name.clone(), v.fields.clone()))
                    .collect()
            }),
            _ => None,
        }
    }

    /// Converts `pattern`, which matches a value of type `ty`, to the [`Space`] of values it
    /// matches, or `None` if it names a variant `ty` does not have.
    fn space(&self, pattern: &Pattern, ty: &Ty) -> Option<Space> {
        match &pattern.kind {
            PatternKind::Wildcard | PatternKind::Name(_) => Some(Space::Any),
            PatternKind::Variant { path, fields } => {
                let variant = &path.last()?.0;
                let variants = self.ty_variants(ty)?;
                let tag = variants.iter().position(|(name, _)| name == variant)?;
                let field_types = &variants[tag].1;
                if field_types.len() != fields.len() {
                    return None;
                }
                let fields = fields
                    .iter()
                    .zip(field_types)
                    .map(|(field, ty)| self.space(field, ty))
                    .collect::<Option<Vec<_>>>()?;
                Some(Space::Variant(tag, fields))
            }
        }
    }

    /// Returns whether some values of types `types` match the patterns `row` but none of the
    /// rows of `rows`.
    fn useful(&self, rows: &[Vec<Space>], row: &[Space], types: &[Ty]) -> bool {
        let Some((ty, rest)) = types.split_first() else {
            return rows.is_empty();
        };
        let variants = self.ty_variants(ty);
        match &row[0] {
            Space::Variant(tag, fields) => {
                let field_types = variants.map(|v| v[*tag].1.clone()).unwrap_or_default();
                let specialized = specialize(rows, *tag, fields.len());
                let row: Vec<Space> = fields.iter().chain(&row[1..]).cloned().collect();
                let types: Vec<Ty> = field_types
                    .into_iter()
                    .chain(rest.iter().cloned())
                    .collect();
                self.useful(&specialized, &row, &types)
            }
            Space::Any => match variants {
                Some(variants) if covers_all(rows, variants.len()) => {
                    variants.iter().enumerate().any(|(tag, (_, field_types))| {
                        let specialized = specialize(rows, tag, field_types.len());
                        let row: Vec<Space> = std::iter::repeat_n(Space::Any, field_types.len())
                            .chain(row[1..].iter().cloned())
                            .collect();
                        let types: Vec<Ty> = field_types.iter().chain(rest).cloned().collect();
                        self.useful(&specialized, &row, &types)
                    })
                }
                _ => self.useful(&default_rows(rows), &row[1..], rest),
            },
        }
    }

    /// Returns patterns, one per type of `types` and as written in stone, for values that match
    /// none of `rows`, or `None` if every value matches one of them.
    ///
    /// For example, rows for `Shape.Circle(_)` alone, against a `Shape`, give
    /// `["Shape.Rect(_, _)"]` if `Rect` is the next variant.
    fn missing(&self, rows: &[Vec<Space>], types: &[Ty]) -> Option<Vec<String>> {
        let Some((ty, rest)) = types.split_first() else {
            return rows.is_empty().then(Vec::new);
        };
        let variants = self.ty_variants(ty);
        match variants {
            Some(variants) if covers_all(rows, variants.len()) => {
                for (tag, (name, field_types)) in variants.iter().enumerate() {
                    let specialized = specialize(rows, tag, field_types.len());
                    let types: Vec<Ty> = field_types.iter().chain(rest).cloned().collect();
                    if let Some(witness) = self.missing(&specialized, &types) {
                        let (fields, after) = witness.split_at(field_types.len());
                        let mut out = vec![self.written_variant(ty, name, fields)];
                        out.extend(after.iter().cloned());
                        return Some(out);
                    }
                }
                None
            }
            _ => {
                let after = self.missing(&default_rows(rows), rest)?;
                let used = used_tags(rows);
                let first = match variants {
                    Some(variants) if !used.is_empty() => {
                        let (name, field_types) = variants
                            .iter()
                            .enumerate()
                            .find(|(tag, _)| !used.contains(tag))
                            .map(|(_, variant)| variant)?;
                        let wild = vec!["_".to_string(); field_types.len()];
                        self.written_variant(ty, name, &wild)
                    }
                    _ => "_".to_string(),
                };
                let mut out = vec![first];
                out.extend(after);
                Some(out)
            }
        }
    }

    /// Writes the variant `name` of the enum type `ty` with the patterns `fields`, as a pattern is
    /// written, such as `Shape.Rect(_, _)` or `none`.
    fn written_variant(&self, ty: &Ty, name: &str, fields: &[String]) -> String {
        let prefix = match self.resolve(ty) {
            Ty::Enum(enum_name, _) if enum_name != OPTION => format!("{enum_name}.{name}"),
            _ => name.to_string(),
        };
        if fields.is_empty() {
            prefix
        } else {
            format!("{prefix}({})", fields.join(", "))
        }
    }
}

/// The values a pattern matches, for checking `match` cases: any value, or a variant, by its
/// tag, whose values match the given spaces.
#[derive(Clone)]
enum Space {
    Any,
    Variant(usize, Vec<Space>),
}

/// Returns the tags of the variants that start the rows of `rows`.
fn used_tags(rows: &[Vec<Space>]) -> HashSet<usize> {
    rows.iter()
        .filter_map(|row| match &row[0] {
            Space::Variant(tag, _) => Some(*tag),
            Space::Any => None,
        })
        .collect()
}

/// Returns whether the rows of `rows` start with every one of an enum's `count` variants.
fn covers_all(rows: &[Vec<Space>], count: usize) -> bool {
    let used = used_tags(rows);
    !used.is_empty() && used.len() == count
}

/// Returns the rows of `rows` that match the variant `tag`, which holds `arity` values, with
/// their first pattern replaced by a pattern for each of those values.
fn specialize(rows: &[Vec<Space>], tag: usize, arity: usize) -> Vec<Vec<Space>> {
    rows.iter()
        .filter_map(|row| {
            let head: Vec<Space> = match &row[0] {
                Space::Variant(t, fields) if *t == tag => fields.clone(),
                Space::Variant(..) => return None,
                Space::Any => vec![Space::Any; arity],
            };
            Some(head.into_iter().chain(row[1..].iter().cloned()).collect())
        })
        .collect()
}

/// Returns the rows of `rows` that match any value first, without their first pattern.
fn default_rows(rows: &[Vec<Space>]) -> Vec<Vec<Space>> {
    rows.iter()
        .filter(|row| matches!(row[0], Space::Any))
        .map(|row| row[1..].to_vec())
        .collect()
}

/// Writes how many values a variant holds, such as `1 value` or `2 values`.
fn values_phrase(count: usize) -> String {
    match count {
        1 => "1 value".to_string(),
        n => format!("{n} values"),
    }
}

impl Inference {}

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
                        ExprKind::Name { .. } | ExprKind::Tuple { .. } => {
                            let mut names = Vec::new();
                            target_names(target, &mut names);
                            for name in names {
                                if let ExprKind::Name { id, .. } = &name.kind {
                                    assigned.insert(id.clone());
                                }
                            }
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
                let mut names = Vec::new();
                target_names(target, &mut names);
                for name in names {
                    if let ExprKind::Name { id, .. } = &name.kind {
                        inside.insert(id.clone());
                    }
                }
                self.block(body, Some(inside));
                Some(assigned)
            }
            StmtKind::Match { subject, cases } => {
                self.reads(subject, &assigned);
                // the cases cover every value, so one of them always runs
                let mut after: Assigned = None;
                for case in cases {
                    let mut inside = assigned.clone();
                    let mut names = vec![];
                    case.pattern.names(&mut names);
                    inside.extend(names.into_iter().map(|(name, _)| name.to_string()));
                    let end = self.block(&case.body, Some(inside));
                    after = match (after, end) {
                        (None, other) | (other, None) => other,
                        (Some(a), Some(b)) => Some(a.intersection(&b).cloned().collect()),
                    };
                }
                after
            }
            StmtKind::Break | StmtKind::Continue => None,
            StmtKind::FunctionDef { .. } | StmtKind::Use { .. } | StmtKind::EnumDef { .. } => {
                Some(assigned)
            }
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
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
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
        let mut names = Vec::new();
        target_names(target, &mut names);
        for name in names {
            if let ExprKind::Name { id, .. } = &name.kind
                && !out.iter().any(|(seen, _)| seen == id)
            {
                out.push((id.clone(), name.span));
            }
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
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    let mut names = vec![];
                    case.pattern.names(&mut names);
                    for (name, span) in names {
                        if !out.iter().any(|(seen, _)| seen == name) {
                            out.push((name.to_string(), span));
                        }
                    }
                    collect_assigned(&case.body, out);
                }
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
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

const TUPLE_CHANGE: &str = "a tuple cannot be changed; build a new one, as in 't = (v, t[1])'";

impl Indexed {
    /// Returns the error for changing an element of what was indexed, if it cannot be changed.
    fn change_error(self) -> Option<&'static str> {
        match self {
            Indexed::List => None,
            Indexed::Str => Some(STRING_CHANGE),
            Indexed::Tuple => Some(TUPLE_CHANGE),
        }
    }
}

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
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    collect_changed(&case.body, out);
                }
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
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
                        ExprKind::Name { .. } | ExprKind::Tuple { .. } => {}
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
            StmtKind::Match { subject, cases } => {
                self.expr(subject);
                for case in cases {
                    self.block(&case.body);
                }
            }
            StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
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
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
                elts.iter().for_each(|elt| self.expr(elt))
            }
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
        // the cases cover every value, so one of them always runs
        StmtKind::Match { cases, .. } => cases.iter().all(|case| always_exits(&case.body)),
        _ => false,
    })
}

/// Returns whether `test` is the literal `true`.
fn is_always_true(test: &Expr) -> bool {
    matches!(&test.kind, ExprKind::Constant { value, .. } if matches!(**value, Constant::Bool(true)))
}

/// Returns whether `body` contains a `break` for the loop it belongs to, not counting `break`s of
/// loops nested inside it.
fn breaks(body: &[Stmt]) -> bool {
    body.iter().any(|stmt| match &stmt.kind {
        StmtKind::Break => true,
        StmtKind::If { body, orelse, .. } => breaks(body) || breaks(orelse),
        StmtKind::Match { cases, .. } => cases.iter().any(|case| breaks(&case.body)),
        _ => false,
    })
}
