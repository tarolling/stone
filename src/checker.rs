//! Type checker and name resolver that validates a stone AST before it runs.
//!
//! Function signatures are written, and every other type is inferred. Every expression gets one of
//! `int`, `float`, `bool`, `str`, `list[T]`, `tuple[...]`, a struct, an enum, or `option[T]`, and every
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
    Arguments, CompOp, Constant, Expr, ExprKind, Field, Keyword, MatchCase, Mod, Operator, Pattern,
    PatternKind, Stmt, StmtKind, TraitName, TypeExpr, TypeExprKind, TypeParam, UnaryOp, Variant,
    constant_int, target_names,
};
use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{
    MAX_ELEMENT_VALUES, METHODS, OUTPUT, changes_receiver, is_builtin, is_reserved,
};

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
    /// A map from keys of the first type to values of the second, such as `map[str, int]`.
    Map(Box<Type>, Box<Type>),
    /// A tuple of two or more fields, such as `tuple[int, str]`.
    Tuple(Vec<Type>),
    /// An enum by its linked name, with its type arguments: `Shape`, or `option[int]`, the
    /// prelude's enum [`OPTION`].
    Enum {
        name: String,
        args: Vec<Type>,
    },
    /// A struct by its linked name, with its type arguments: `Point`, `geometry.Point`, or
    /// `Pair[int, str]`, whose fields are in [`Analysis::structs`].
    Struct {
        name: String,
        args: Vec<Type>,
    },
    /// A type parameter of the generic function the type appears in, such as `T` in
    /// `fn first[T](xs: list[T]) -> T:`, which each call fills in with a type of its own.
    Param(String),
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
    },
}

impl Type {
    /// Returns the values a value of this type is made of once nested tuples and structs, whose
    /// fields are in `structs`, are flattened, in order: the type itself unless it is a tuple or
    /// a struct.
    ///
    /// For example, `tuple[int, tuple[str, float]]` has the leaves `int`, `str`, and `float`, and
    /// `list[tuple[int, int]]` is one leaf.
    pub fn leaves(&self, structs: &Structs) -> Vec<Type> {
        match self {
            Type::Tuple(fields) => fields.iter().flat_map(|f| f.leaves(structs)).collect(),
            Type::Struct { .. } => self
                .struct_fields(structs)
                .iter()
                .flat_map(|(_, f)| f.leaves(structs))
                .collect(),
            other => vec![other.clone()],
        }
    }

    /// Returns whether a value of this type is flattened into [`Type::leaves`] of its own: a
    /// tuple or a struct.
    pub fn is_flat(&self) -> bool {
        matches!(self, Type::Tuple(_) | Type::Struct { .. })
    }

    /// Returns the fields of this struct type, in order, each with its type once the struct's
    /// type arguments are filled in, or nothing if it is not a struct.
    ///
    /// For example, `Pair[int, str]`, with fields `a: A` and `b: B`, has `a: int` and `b: str`.
    pub fn struct_fields(&self, structs: &Structs) -> Vec<(String, Type)> {
        let Type::Struct { name, args } = self else {
            return vec![];
        };
        let Some(info) = structs.get(name) else {
            return vec![];
        };
        let substitution = substitution(&info.params, args);
        info.fields
            .iter()
            .map(|(field, ty)| (field.clone(), ty.substitute(&substitution)))
            .collect()
    }

    /// Returns this type with each type parameter that `substitution` names replaced by its
    /// type.
    ///
    /// For example, substituting `str` for `T` in `option[list[T]]` gives `option[list[str]]`.
    pub fn substitute(&self, substitution: &HashMap<String, Type>) -> Type {
        if substitution.is_empty() {
            return self.clone();
        }
        let each = |types: &[Type]| types.iter().map(|t| t.substitute(substitution)).collect();
        match self {
            Type::Param(name) => substitution
                .get(name)
                .cloned()
                .unwrap_or_else(|| self.clone()),
            Type::List(elem) => Type::List(Box::new(elem.substitute(substitution))),
            Type::Map(key, value) => Type::Map(
                Box::new(key.substitute(substitution)),
                Box::new(value.substitute(substitution)),
            ),
            Type::Tuple(fields) => Type::Tuple(each(fields)),
            Type::Enum { name, args } => Type::Enum {
                name: name.clone(),
                args: each(args),
            },
            Type::Struct { name, args } => Type::Struct {
                name: name.clone(),
                args: each(args),
            },
            Type::Function { params, ret } => Type::Function {
                params: each(params),
                ret: Box::new(ret.substitute(substitution)),
            },
            Type::Int | Type::Float | Type::Bool | Type::Str | Type::Unit => self.clone(),
        }
    }

    /// Returns the positions among [`Type::leaves`] of field `k` of this tuple type.
    ///
    /// For example, field 1 of `tuple[tuple[int, str], float, bool]` is leaf 2, and field 0 is
    /// leaves 0 and 1.
    pub fn field_leaves(&self, k: usize, structs: &Structs) -> std::ops::Range<usize> {
        let Type::Tuple(fields) = self else {
            return 0..1;
        };
        let start: usize = fields[..k].iter().map(|f| f.leaves(structs).len()).sum();
        start..start + fields[k].leaves(structs).len()
    }

    /// Returns the positions among [`Type::leaves`] of the field `name` of this struct type, and
    /// the field's type, or `None` if it has no such field.
    ///
    /// For example, `y` of `Point`, with fields `x: float` and `y: float`, is leaf 1, a `float`.
    pub fn struct_field(
        &self,
        name: &str,
        structs: &Structs,
    ) -> Option<(std::ops::Range<usize>, Type)> {
        let mut start = 0;
        for (field, ty) in self.struct_fields(structs) {
            let width = ty.leaves(structs).len();
            if field == name {
                return Some((start..start + width, ty));
            }
            start += width;
        }
        None
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
            Type::Map(key, value) => write!(f, "map[{key}, {value}]"),
            Type::Tuple(fields) => {
                let fields: Vec<String> = fields.iter().map(Type::to_string).collect();
                write!(f, "tuple[{}]", fields.join(", "))
            }
            Type::Enum { name, args } if args.is_empty() => write!(f, "{name}"),
            Type::Enum { name, args } => {
                let args: Vec<String> = args.iter().map(Type::to_string).collect();
                write!(f, "{name}[{}]", args.join(", "))
            }
            Type::Struct { name, args } if args.is_empty() => write!(f, "{name}"),
            Type::Struct { name, args } => {
                let args: Vec<String> = args.iter().map(Type::to_string).collect();
                write!(f, "{name}[{}]", args.join(", "))
            }
            Type::Param(name) => write!(f, "{name}"),
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

/// The name of the prelude's enum, whose values are `none` and `some(x)`, and which builtins such
/// as `find` and `os.env` return.
pub const OPTION: &str = "option";

/// The name of the prelude's enum whose values are `less`, `equal`, and `greater`, which `cmp`
/// returns.
pub const ORDERING: &str = "ordering";

/// The name of the prelude's enum whose values are `ok(x)` and `err(e)`, which `parse_int` and
/// `parse_float` return, and which `?` unwraps.
pub const RESULT: &str = "result";

/// The name of the prelude's trait of values `==` compares, which every type it compares part by
/// part implements, and a struct may implement itself.
pub const EQ: &str = "eq";

/// The name of the prelude's trait of ordered values, which `int`, `float`, and `str` implement,
/// and a struct may too.
pub const ORD: &str = "ord";

/// The name of the prelude's trait of values that `print`, `str`, and interpolation write as
/// text, which every type implements, and a struct may implement itself.
pub const DISPLAY: &str = "display";

/// Returns whether the variants of the enum `name` are written and printed alone, as the
/// prelude's are: `some(1)`, not `option.some(1)`.
///
/// For example, `prints_bare("option")` is true and `prints_bare("Shape")` is false.
pub fn prints_bare(name: &str) -> bool {
    name == OPTION || name == ORDERING || name == RESULT
}

/// Returns whether `value`, the expression before a dot, may name an enum rather than a value:
/// any name but a written one of a prelude enum, which is always a variable, since the prelude's
/// variants are written alone and linking writes the enum's name before them
/// ([`Expr::is_unwritten`]).
///
/// For example, `Shape` in `Shape.Circle` may, and so may the unwritten `result` in
/// `result.ok(1)`, but a written `result` in `result.append(x)` is a variable.
pub fn may_name_enum(value: &Expr) -> bool {
    match &value.kind {
        ExprKind::Name { id, .. } => !prints_bare(id) || value.is_unwritten(),
        _ => false,
    }
}

/// Maps each of `params`, a generic type's or function's type parameters, to the type at the
/// same position of `args`.
pub fn substitution(params: &[String], args: &[Type]) -> HashMap<String, Type> {
    params.iter().cloned().zip(args.iter().cloned()).collect()
}

/// An enum a program defines: its type parameters, such as `T` in `enum Tree[T]:`, and its
/// variants, each with the types of its values in order, as in `Circle` holding `float`.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct EnumDef {
    pub params: Vec<String>,
    pub variants: Vec<(String, Vec<Type>)>,
}

/// Every enum a program defines, keyed by linked name, including the prelude's `option`.
pub type Enums = HashMap<String, EnumDef>;

/// Returns the variants of the enum type `ty`, each with the types of its values once its type
/// arguments are filled in, in the order their tags number them, or nothing if `ty` is not an
/// enum.
///
/// For example, `option[int]` gives `none` with no values, then `some` with an `int`.
pub fn variants(enums: &Enums, ty: &Type) -> Vec<(String, Vec<Type>)> {
    match ty {
        Type::Enum { name, args } => {
            let Some(info) = enums.get(name) else {
                return vec![];
            };
            let substitution = substitution(&info.params, args);
            info.variants
                .iter()
                .map(|(variant, fields)| {
                    let fields = fields.iter().map(|f| f.substitute(&substitution)).collect();
                    (variant.clone(), fields)
                })
                .collect()
        }
        _ => vec![],
    }
}

/// Returns how a value of the enum type `ty`'s variant `name` prints before its values: the
/// enum's own name and the variant's, or the variant's alone for the prelude's `option`
/// ([`prints_bare`]).
///
/// For example, `Circle` of `geometry.Shape` prints as `Shape.Circle`, and `some` as `some`.
pub fn variant_display(ty: &Type, name: &str) -> String {
    match ty {
        Type::Enum {
            name: enum_name, ..
        } if !prints_bare(enum_name) => {
            let short = enum_name.rsplit('.').next().unwrap_or(enum_name);
            format!("{short}.{name}")
        }
        _ => name.to_string(),
    }
}

/// A struct a program defines: its type parameters, such as `A` and `B` in `struct Pair[A, B]:`,
/// and its fields, each with its type, in the order they are written, as in `Point` holding
/// `x: float` and `y: float`. [`Type::struct_fields`] fills in a struct type's arguments.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct StructDef {
    pub params: Vec<String>,
    pub fields: Vec<(String, Type)>,
}

/// Every struct a program defines, keyed by linked name.
pub type Structs = HashMap<String, StructDef>;

/// Returns how the struct `name` prints before its fields: its own name, without the module
/// it is in.
///
/// For example, `geometry.Point` prints as `Point`.
pub fn struct_display(name: &str) -> &str {
    name.rsplit('.').next().unwrap_or(name)
}

/// How a call of a function of an `impl` block was resolved, which [`Methods`] holds by the
/// call's span.
///
/// For example, `p.shift(1.0)` calls `Point.shift` with `p` as `self`, changing `p` if `shift`
/// changes `self`, and `Point.origin()` calls `Point.origin` without a receiver.
#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    /// The function's linked name: its struct's linked name, a dot, and its own name, such as
    /// `geo.Point.shift`.
    pub function: String,
    /// Whether the call passes the value before the dot as `self`, rather than calling an
    /// associated function on the struct.
    pub receiver: bool,
    /// Whether the method changes `self`, and so the receiver, in place.
    pub changes: bool,
    /// For a call through a bound, as `x.area()` with `x` of type `T` in
    /// `fn f[T: Shape](x: T):`, the function each type that implements the trait calls, by the
    /// type's linked name (`int` for a builtin type), and `function` is the trait's method, such
    /// as `Shape.area`. A function named by its trait is the trait's default method, called
    /// with the type as `Self`. `None` for a call whose function is known.
    pub implementations: Option<HashMap<String, String>>,
}

impl Method {
    /// Returns the function a call through a bound calls for a receiver of `ty`, and the type
    /// arguments of its copy: the struct's own for a method of its `impl` block, or `ty` for
    /// `Self` of the trait's default method. Returns `function` and no types for a call whose
    /// function is already known.
    ///
    /// For example, `x.area()` calls `Circle.area` for a `Circle`, and `Shape.half` with
    /// `Circle` for `Self` if `Circle` keeps the default of `half`.
    pub fn resolve(&self, ty: &Type) -> Option<(String, Vec<Type>)> {
        let Some(implementations) = &self.implementations else {
            return Some((self.function.clone(), vec![]));
        };
        let (key, args) = match ty {
            Type::Struct { name, args } => (name.as_str(), args.clone()),
            other => (type_key(other)?, vec![]),
        };
        let function = implementations.get(key)?;
        let (owner, _) = self.function.rsplit_once('.')?;
        if function
            .rsplit_once('.')
            .is_some_and(|(trait_, _)| trait_ == owner)
        {
            return Some((function.clone(), vec![ty.clone()]));
        }
        Some((function.clone(), args))
    }
}

/// Returns the name a builtin type is known by among the types that implement traits, such as
/// `int`, or `None` for a type that cannot implement one.
pub fn type_key(ty: &Type) -> Option<&'static str> {
    match ty {
        Type::Int => Some("int"),
        Type::Float => Some("float"),
        Type::Bool => Some("bool"),
        Type::Str => Some("str"),
        _ => None,
    }
}

/// Every call of a function of an `impl` block, keyed by the call's span.
pub type Methods = HashMap<Span, Method>;

/// The type arguments a call of a generic function fills its type parameters with, which
/// [`Analysis::instances`] holds by the call's span.
///
/// For example, `first(["a"])` calls `first` with `str` for `T`. Inside another generic
/// function, an argument may be that function's own type parameter, as `T` in `first(xs)`.
#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
    /// The function's linked name, such as `util.first`.
    pub function: String,
    /// A type for each of its type parameters, in order.
    pub args: Vec<Type>,
}

/// What a [`Symbol`] names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Function,
    /// An enum, such as `Shape` in `enum Shape:`.
    Enum,
    /// A variant of an enum, such as `Circle`, whose [`Symbol::scope`] is the enum's name.
    Variant,
    /// A struct, such as `Point` in `struct Point:`.
    Struct,
    /// A field of a struct, such as `x` in `x: float`, whose [`Symbol::scope`] is the struct's
    /// name.
    Field,
    /// A function of an `impl` block, such as `norm` in `fn norm(self) -> float:`, whose
    /// [`Symbol::scope`] is the struct's name, or of a trait, whose scope is the trait's name.
    Method,
    /// A trait, such as `Shape` in `trait Shape:`.
    Trait,
    /// A type parameter of a generic function, such as `T` in `fn first[T](xs: list[T]) -> T:`,
    /// whose [`Symbol::scope`] is the function's name.
    TypeParam,
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
    /// The fields of every struct the program defines.
    pub structs: Structs,
    /// How every call of a function of an `impl` block was resolved.
    pub methods: Methods,
    /// The type arguments of every call of a generic function, by the call's span.
    pub instances: HashMap<Span, Instance>,
    /// The names of each generic function's type parameters, in order, by its linked name.
    pub type_params: HashMap<String, Vec<String>>,
    /// The traits that bound each type parameter that has bounds, by the index of its symbol
    /// in [`Analysis::symbols`], such as `Shape` for `T` in `fn total[T: Shape]`.
    pub bounds: HashMap<usize, Vec<String>>,
    /// The traits each type implements, by the type's linked name (`int` for a builtin type),
    /// sorted.
    pub implemented: HashMap<String, Vec<String>>,
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
            _ if matches!(
                s.kind,
                SymbolKind::Variant
                    | SymbolKind::Field
                    | SymbolKind::Method
                    | SymbolKind::TypeParam
            ) =>
            {
                false
            }
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
    Map(Box<Ty>, Box<Ty>),
    Tuple(Vec<Ty>),
    Enum(String, Vec<Ty>),
    /// A struct by its linked name, with its type arguments.
    Struct(String, Vec<Ty>),
    /// An unknown type, identified by its index in [`Inference::vars`].
    Var(usize),
    /// A type parameter of a generic function, identified by its index in
    /// [`Inference::type_params`]. Inside the function it is an opaque type that unifies only with
    /// itself, and each call replaces it with a fresh variable.
    Param(usize),
}

/// Returns the builtin type a trait's `impl` block names by `key`, such as `int`, if it is one.
fn builtin_key_type(key: &str) -> Option<Ty> {
    match key {
        "int" => Some(Ty::Int),
        "float" => Some(Ty::Float),
        "bool" => Some(Ty::Bool),
        "str" => Some(Ty::Str),
        _ => None,
    }
}

/// Returns the name a type is known by among the types that implement traits: a struct's linked
/// name, or a builtin type's, such as `int`.
fn ty_key(ty: &Ty) -> Option<String> {
    match ty {
        Ty::Struct(name, _) => Some(name.clone()),
        Ty::Int => Some("int".to_string()),
        Ty::Float => Some("float".to_string()),
        Ty::Bool => Some("bool".to_string()),
        Ty::Str => Some("str".to_string()),
        _ => None,
    }
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
    /// Whether a type that implements `ord` is allowed too, as for `<`.
    ord: bool,
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
    Map,
    Tuple,
    Enum,
    Struct,
    /// A type parameter, which no constraint admits, since it may be any type.
    Param,
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
            Ty::Map(..) => Some(Kind::Map),
            Ty::Tuple(_) => Some(Kind::Tuple),
            Ty::Enum(..) => Some(Kind::Enum),
            Ty::Struct(..) => Some(Kind::Struct),
            Ty::Param(_) => Some(Kind::Param),
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
/// The names of the types built into the language, which no enum or struct may take.
const BUILTIN_TYPES: &[&str] = &[
    "int", "float", "bool", "str", "list", "tuple", OPTION, RESULT, "Self",
];
/// The message for a map key of another kind.
const MAP_KEY: &str = "a map key must be int, str, or bool, found {}";

/// A list or map inside a type, which keeps `==` from comparing it, and the innermost struct
/// field that holds it, if any, as the struct's name and the field's.
///
/// For example, in `tuple[int, Bag]`, where `Bag` has a field `items: list[int]`, it is
/// `list[int]` in `Bag.items`.
struct Uncomparable {
    ty: Ty,
    field: Option<(String, String)>,
}

/// An enum a program defines: its symbol, its type parameters, and its variants, each with its
/// symbol and the types of its values, which may hold the type parameters.
struct EnumInfo {
    symbol: usize,
    type_params: Vec<usize>,
    variants: Vec<VariantInfo>,
}

struct VariantInfo {
    name: String,
    symbol: usize,
    fields: Vec<Ty>,
}

/// A struct a program defines: its symbol, its type parameters, and its fields, each with its
/// symbol and type, which may hold the type parameters.
struct StructInfo {
    symbol: usize,
    type_params: Vec<usize>,
    fields: Vec<FieldInfo>,
}

struct FieldInfo {
    name: String,
    symbol: usize,
    ty: Ty,
}

/// A function of an `impl` block, whose signature is in [`Inference::functions`] under its
/// struct's name, a dot, and its own name.
///
/// A trait's default method the type keeps is one too, whose function is the trait's.
struct MethodInfo {
    symbol: usize,
    /// Whether its first parameter is `self`, so it is called on a value rather than the struct.
    has_self: bool,
    /// Whether it changes `self`, found by [`Inference::find_changing_methods`].
    changes: bool,
    /// The function it calls, such as `Point.norm`, or `Shape.half` for a default method.
    function: String,
    /// Whether it is a trait's default method, called with the type as `Self`.
    default: bool,
}

/// A trait a program defines: its symbol, its type parameter `Self`, the type that implements
/// it, and its methods, whose signatures are in [`Inference::functions`] under the trait's name,
/// a dot, and their own names.
struct TraitInfo {
    symbol: usize,
    self_param: usize,
    methods: Vec<TraitMethodInfo>,
}

struct TraitMethodInfo {
    name: String,
    symbol: usize,
    /// Whether it has a default body, which a type gets unless it defines the method itself.
    has_body: bool,
    /// Whether it changes `self`: a default that does, found by
    /// [`Inference::find_changing_methods`]. A method without a body never does.
    changes: bool,
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
    /// The type parameters of a generic function, as indexes into [`Inference::type_params`],
    /// which `params` and `ret` hold as [`Ty::Param`].
    type_params: Vec<usize>,
    params: Vec<Ty>,
    ret: Ty,
    /// Whether the definition writes its return type, rather than returning no value by default.
    ret_written: bool,
}

/// How a value is held inside another, for counting the values it takes up.
#[derive(Clone, Copy, PartialEq)]
enum Held {
    /// An element of a list.
    Element,
    /// The value of a map's entry, stored after its key.
    Entry,
    /// A value of a generic enum, whose widest variant counts.
    Variants,
}

/// A call of a generic function and the type arguments it fills the type parameters with.
struct PendingInstance {
    span: Span,
    /// The function the call is in, or `None` at the top level.
    caller: Option<String>,
    /// The generic function called.
    function: String,
    args: Vec<Ty>,
}

/// A type parameter of a generic function, such as `T` in `fn first[T](xs: list[T]) -> T:`.
struct TypeParamInfo {
    name: String,
    symbol: usize,
    /// The traits its types implement, by linked name, such as `Shape` in `[T: Shape]`.
    bounds: Vec<String>,
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
    /// Every type parameter of every generic function, which [`Ty::Param`] indexes.
    type_params: Vec<TypeParamInfo>,
    /// The type parameters of the generic function whose signature or body is being checked, by
    /// name.
    scope_params: HashMap<String, usize>,
    /// Every call of a generic function, which `finish` checks and gives lowering as
    /// [`Analysis::instances`].
    instances: Vec<PendingInstance>,
    diagnostics: Vec<Diagnostic>,
    constraints: Vec<Constraint>,
    /// The type each `==` and `!=` compares (or `in` or `contains` compares a list's elements
    /// by), with the comparison's span and its operators as an error names them, such as
    /// `'==' and '!='`, checked once inference is done for a list or map anywhere inside it.
    equalities: Vec<(Ty, Span, &'static str)>,
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
    /// The span of the call that is the whole value of an assignment, if it is one, where a call
    /// that changes its receiver may be whatever else the statement uses its receiver for.
    assigned_call: Option<Span>,
    /// The calls in the statement being checked that change their receiver, which
    /// [`Inference::check_stmt`] checks once it has seen them all.
    changing_calls: Vec<ChangingCall>,
    /// The span of every tuple that is indexed, such as `t` in `t[0]`, so that a change through
    /// one, such as `t[0].append(1)`, can be reported.
    indexed_tuples: HashSet<Span>,
    /// The span of every map that is indexed, such as `m` in `m[k]`, so that a change through an
    /// entry, such as `m[k].append(1)`, can be reported.
    indexed_maps: HashSet<Span>,
    /// Each map literal and the type of the map, whose entries `finish` checks are not too wide
    /// to store.
    map_literals: Vec<(Span, Ty)>,
    /// Each list literal and the type of the list, whose elements `finish` checks are not too
    /// wide to store.
    list_literals: Vec<(Span, Ty)>,
    /// Constant indexes into a global a function reads before the code assigning it is checked,
    /// which `finish` settles once the global is known to be a tuple or a list.
    deferred_indexes: Vec<DeferredIndex>,
    /// Tests of whether a container holds a value whose container was not yet known, which
    /// `finish` settles.
    deferred_memberships: Vec<Membership>,
    /// Every enum the program defines, by name.
    enums: HashMap<String, EnumInfo>,
    /// The span of every enum definition declared at the top level, so a nested one is reported.
    enum_spans: HashSet<Span>,
    /// Every struct the program defines, by name.
    structs: HashMap<String, StructInfo>,
    /// The span of every struct definition declared at the top level, so a nested one is
    /// reported.
    struct_spans: HashSet<Span>,
    /// The span of every `impl` block at the top level, so a nested one is reported.
    impl_spans: HashSet<Span>,
    /// The functions of every struct's `impl` blocks, and the methods of the traits it
    /// implements, by the type's linked name (`int` for a builtin type) and function name.
    struct_methods: HashMap<String, HashMap<String, MethodInfo>>,
    /// Every trait the program defines, by name.
    traits: HashMap<String, TraitInfo>,
    /// The span of every trait definition declared at the top level, so a nested one is
    /// reported.
    trait_spans: HashSet<Span>,
    /// The methods of every implementation of a trait, by the trait's and the type's linked
    /// names, each mapped to the function it calls.
    impls: HashMap<(String, String), HashMap<String, String>>,
    /// The type `Self` names in the `impl` block whose signatures are being read.
    self_type: Option<Ty>,
    /// How each call of a function of an `impl` block was resolved, by the call's span.
    methods: Methods,
    /// Methods called on a global a function reads before the code assigning it is checked,
    /// which `finish` settles once the global's type is known.
    deferred_methods: Vec<DeferredMethod>,
    /// Fields read from a global a function reads before the code assigning it is checked,
    /// which `finish` settles once the global's type is known.
    deferred_fields: Vec<DeferredField>,
    /// Every `match`, whose cases `finish` checks.
    matches: Vec<PendingMatch>,
}

/// `value[slice]`, with a constant `slice`, where `value` is a global not yet known to be a tuple
/// or a list, whose element or field is `result`.
struct DeferredIndex {
    value: Span,
    value_ty: Ty,
    slice: Expr,
    /// The type of the index, an int unless the global turns out to be a map.
    index_ty: Ty,
    result: Ty,
}

/// A test of whether a container holds a value, `needle in container` or
/// `container.contains(needle)`, whose container was not yet known to be a map, a list, or a
/// string when it was found, as a global a function reads before the code assigning it is
/// checked may not be.
struct Membership {
    /// The span of the whole test, where an error comparing a list's elements points.
    span: Span,
    container: Span,
    container_ty: Ty,
    needle: Span,
    needle_ty: Ty,
    /// The test as written: `in`, `not in`, or `contains`.
    name: &'static str,
}

/// `value.attr`, where `value` is a global not yet known to be a struct, whose field is
/// `result`.
struct DeferredField {
    value_ty: Ty,
    attr: String,
    attr_span: Span,
    result: Ty,
}

/// `receiver.name(args)`, where `receiver` is not yet known to be a struct, as a global a function
/// reads before the code assigning it is checked may not be, whose result is `result`.
struct DeferredMethod {
    call: Span,
    receiver: Expr,
    /// Whether changing the receiver would change a global from inside a function.
    global: bool,
    /// Whether the call is a statement of its own, or the whole value of an assignment, where a
    /// method that changes its receiver may be called.
    statement: bool,
    assigned: bool,
    value_ty: Ty,
    name: String,
    name_span: Span,
    /// The type and span of each argument.
    args: Vec<(Ty, Span)>,
    result: Ty,
    /// Whether its statement has been checked, filling in `conflict`.
    checked: bool,
    /// Where else its statement uses the receiver, which is an error if the method turns out to
    /// change it and the call is inside an expression.
    conflict: Option<Conflict>,
}

/// A call that changes its receiver, in the statement being checked.
struct ChangingCall {
    /// The name of the method, for errors.
    name: String,
    /// The span of the variable the receiver starts from, which another call that changes its
    /// receiver may share.
    root: Span,
    /// The variable the receiver starts from, then the fields after it up to the first index,
    /// such as `self` and `items` for `self.items[0]`, which is what the call may change.
    path: Vec<String>,
    /// Whether the call is inside an expression, rather than a statement of its own or the whole
    /// value of an assignment, so nothing else in the statement may use what it changes.
    inner: bool,
}

/// A use of what a call changes elsewhere in the same statement, such as `xs` in
/// `print(xs, xs.pop())`.
#[derive(Clone)]
struct Conflict {
    /// What the call changes, such as `xs` or `self.items`.
    changed: String,
    /// What the statement also uses, which overlaps it.
    used: String,
    span: Span,
}

/// What [`Inference::subscript_of`] indexed.
#[derive(Clone, Copy, PartialEq)]
enum Indexed {
    List,
    Str,
    Tuple,
    Map,
}

/// What a name refers to where it is used.
enum Resolved {
    Variable(usize),
    Function(String),
    Enum(String),
    Struct(String),
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
            SymbolKind::Function
            | SymbolKind::Enum
            | SymbolKind::Struct
            | SymbolKind::Trait
            | SymbolKind::Global => None,
            // set by the caller
            SymbolKind::Variant
            | SymbolKind::Field
            | SymbolKind::Method
            | SymbolKind::TypeParam => None,
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
        // a name linking wrote, such as `option` in `some(1)`, has no text to find
        if span.start == span.end {
            return;
        }
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
            Ty::Map(key, value) => {
                Ty::Map(Box::new(self.resolve(&key)), Box::new(self.resolve(&value)))
            }
            Ty::Tuple(fields) => Ty::Tuple(fields.iter().map(|f| self.resolve(f)).collect()),
            Ty::Enum(name, args) => Ty::Enum(name, args.iter().map(|a| self.resolve(a)).collect()),
            Ty::Struct(name, args) => {
                Ty::Struct(name, args.iter().map(|a| self.resolve(a)).collect())
            }
            other => other,
        }
    }

    fn occurs(&self, var: usize, ty: &Ty) -> bool {
        match self.shallow(ty) {
            Ty::Var(v) => v == var,
            Ty::List(elem) => self.occurs(var, &elem),
            Ty::Map(key, value) => self.occurs(var, &key) || self.occurs(var, &value),
            Ty::Tuple(fields) | Ty::Enum(_, fields) | Ty::Struct(_, fields) => {
                fields.iter().any(|f| self.occurs(var, f))
            }
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
            (Ty::Map(xk, xv), Ty::Map(yk, yv)) => {
                let keys = self.unify(&xk, &yk);
                self.unify(&xv, &yv) && keys
            }
            (Ty::Tuple(xs), Ty::Tuple(ys)) => {
                // unify every field, so each is as solved as it can be even if one differs
                xs.len() == ys.len()
                    && xs
                        .iter()
                        .zip(&ys)
                        .fold(true, |same, (x, y)| self.unify(x, y) && same)
            }
            (Ty::Enum(a, xs), Ty::Enum(b, ys)) | (Ty::Struct(a, xs), Ty::Struct(b, ys)) => {
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
            Ty::Map(key, value) => format!("map[{}, {}]", self.show(&key), self.show(&value)),
            Ty::Tuple(fields) => {
                let fields: Vec<String> = fields.iter().map(|f| self.show(f)).collect();
                format!("tuple[{}]", fields.join(", "))
            }
            Ty::Enum(name, args) if args.is_empty() => name,
            Ty::Enum(name, args) => {
                let args: Vec<String> = args.iter().map(|a| self.show(a)).collect();
                format!("{name}[{}]", args.join(", "))
            }
            Ty::Struct(name, args) if args.is_empty() => name,
            Ty::Struct(name, args) => {
                let args: Vec<String> = args.iter().map(|a| self.show(a)).collect();
                format!("{name}[{}]", args.join(", "))
            }
            Ty::Param(p) => self.type_params[p].name.clone(),
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
        // a type parameter may be any type, so it is no operand, whatever the other one is
        if let Some(param) = [&left, &right]
            .into_iter()
            .find(|t| matches!(t, Ty::Param(_)))
        {
            let message = match symbol {
                "+" => "'+' needs int, float, or str operands, found {}",
                "-" => "'-' needs int or float operands, found {}",
                "*" => "'*' needs int or float operands, found {}",
                "/" => "'/' needs int or float operands, found {}",
                _ => "'%' needs int or float operands, found {}",
            };
            self.require(param.clone(), NUMBERS, op.span, message);
            return true;
        }
        // a string, bool, or none is never a list, so an unknown other operand has its type
        let scalar = |ty: &Ty| {
            matches!(
                ty,
                Ty::Str
                    | Ty::Bool
                    | Ty::Unit
                    | Ty::Tuple(_)
                    | Ty::Enum(..)
                    | Ty::Struct(..)
                    | Ty::Param(_)
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
                // a tuple, enum, or struct is never an operand, whatever the other one is
                if let Some(tuple) = [&left, &right]
                    .into_iter()
                    .find(|ty| matches!(ty, Ty::Tuple(_) | Ty::Enum(..) | Ty::Struct(..)))
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
            ord: false,
        });
    }

    /// Requires `ty` to be ordered, as `<` and `sort` need: `int`, `float`, `str`, or a type
    /// that implements `ord`.
    fn require_ordered(&mut self, ty: Ty, span: Span, message: &'static str) {
        self.constraints.push(Constraint {
            ty,
            allowed: ORDERED,
            span,
            message,
            ord: true,
        });
    }

    /// Requires `ty` to be `bool`, as conditions and the operands of `not`, `and`, and `or` must
    /// be, so `if 1 or 0:` is an error while `if n != 0:` is fine.
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
        // structs, enums, functions, and globals are visible everywhere, so declare them all
        // first, types before the signatures that name them
        self.declare_types(body);
        self.declare_traits(body);
        self.declare_methods(body);
        self.find_changing_methods(body);
        for stmt in body {
            if let StmtKind::FunctionDef {
                name,
                name_span,
                type_params,
                args,
                returns,
                ..
            } = &stmt.kind
            {
                self.declare_function(name, *name_span, type_params, args, returns.as_ref());
                self.function_spans.entry(name.clone()).or_insert(stmt.span);
            }
        }
        let mut assigned = vec![];
        collect_assigned(body, &mut assigned);
        for (name, span) in assigned {
            if !self.globals.contains_key(&name)
                && !self.functions.contains_key(&name)
                && !self.value_enum(&name)
                && !self.value_struct(&name)
                && !is_reserved(&name)
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
        let methods = body.iter().flat_map(|stmt| match &stmt.kind {
            StmtKind::ImplDef { methods, .. } => methods.as_slice(),
            _ => &[],
        });
        for stmt in body.iter().chain(methods) {
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
    /// For example, `fn half(x: float) -> float:` declares `half` as `fn(float) -> float`. A
    /// parameter without a type gets an unknown one, so checking goes on, and is reported in
    /// `finish`; a function without `->` returns no value.
    fn declare_function(
        &mut self,
        name: &str,
        name_span: Span,
        type_params: &[TypeParam],
        args: &Arguments,
        returns: Option<&TypeExpr>,
    ) {
        if self.functions.contains_key(name) {
            self.error(name_span, format!("function '{name}' is already defined"));
            return;
        }
        if is_reserved(name) {
            self.error(name_span, format!("cannot redefine builtin '{name}'"));
            return;
        }
        if self.value_enum(name) {
            self.error(name_span, format!("'{name}' is already defined as an enum"));
            return;
        }
        if self.structs.contains_key(name) {
            self.error(
                name_span,
                format!("'{name}' is already defined as a struct"),
            );
            return;
        }
        if self.traits.contains_key(name) {
            self.error(name_span, format!("'{name}' is already defined as a trait"));
            return;
        }
        self.scope_params.clear();
        let type_params = self.declare_type_params(name, type_params, true);
        let (params, ret) = self.signature(args, returns, None);
        self.scope_params.clear();
        // the symbol's type is rebuilt from `params` and `ret` in `finish`
        let symbol = self.add_symbol(name, SymbolKind::Function, name_span, Ty::Unit);
        self.functions.insert(
            name.to_string(),
            FunctionInfo {
                symbol,
                type_params,
                params,
                ret,
                ret_written: returns.is_some(),
            },
        );
    }

    /// Declares the type parameters of `owner`, a generic function, struct, or enum, adding them
    /// to [`Inference::scope_params`] for the types it writes, and returns their indexes into
    /// [`Inference::type_params`]. A name that is already a type, or already a type parameter in
    /// scope, is reported and left out.
    ///
    /// For example, `[T]` in `fn first[T](xs: list[T]) -> T:` declares one parameter `T`. Only
    /// a function's parameters may have bounds (`bounded`), each of which must be a trait.
    fn declare_type_params(
        &mut self,
        owner: &str,
        params: &[TypeParam],
        bounded: bool,
    ) -> Vec<usize> {
        let mut declared = vec![];
        for param in params {
            let name = &param.name;
            // the prelude's parameters cannot clash with a program's types, which it never sees
            if param.span.file != FileId::PRELUDE
                && (BUILTIN_TYPES.contains(&name.as_str())
                    || name == "map"
                    || self.enums.contains_key(name)
                    || self.structs.contains_key(name))
            {
                self.error(param.span, format!("'{name}' is already a type"));
                continue;
            }
            if self.scope_params.contains_key(name) {
                self.error(
                    param.span,
                    format!("'{owner}' already has a type parameter '{name}'"),
                );
                continue;
            }
            let symbol = self.add_symbol(name, SymbolKind::TypeParam, param.span, Ty::Unit);
            self.symbols[symbol].scope = Some(owner.to_string());
            let bounds = self.bounds(&param.bounds, bounded);
            self.type_params.push(TypeParamInfo {
                name: name.clone(),
                symbol,
                bounds,
            });
            let index = self.type_params.len() - 1;
            self.symbol_types[symbol] = Ty::Param(index);
            self.scope_params.insert(name.clone(), index);
            declared.push(index);
        }
        declared
    }

    /// Returns the traits `bounds` name, reporting any that is no trait, and every one unless
    /// they bound a function's type parameter (`bounded`).
    fn bounds(&mut self, bounds: &[TraitName], bounded: bool) -> Vec<String> {
        let mut traits = vec![];
        for bound in bounds {
            let name = &bound.name;
            let short = struct_display(name);
            if !bounded {
                self.error(
                    bound.span,
                    "only a function's type parameters can have bounds for now",
                );
                continue;
            }
            match self.traits.get(name) {
                Some(info) => {
                    self.add_reference(bound.span, info.symbol);
                    if !traits.contains(name) {
                        traits.push(name.clone());
                    }
                }
                None if self.structs.contains_key(name) => {
                    self.error(bound.span, format!("'{short}' is a struct, not a trait"));
                }
                None if self.enums.contains_key(name) => {
                    self.error(bound.span, format!("'{short}' is an enum, not a trait"));
                }
                None => self.error(bound.span, format!("there is no trait '{short}'")),
            }
        }
        traits
    }

    /// Returns `ty` with each of the type parameters `params` replaced by the type at the same
    /// position of `args`.
    ///
    /// For example, substituting `int` for `T` in `list[T]` gives `list[int]`.
    fn substitute(ty: &Ty, params: &[usize], args: &[Ty]) -> Ty {
        let each = |tys: &[Ty]| {
            tys.iter()
                .map(|t| Self::substitute(t, params, args))
                .collect()
        };
        match ty {
            Ty::Param(p) => match params.iter().position(|q| q == p) {
                Some(i) => args.get(i).cloned().unwrap_or_else(|| ty.clone()),
                None => ty.clone(),
            },
            Ty::List(elem) => Ty::List(Box::new(Self::substitute(elem, params, args))),
            Ty::Map(key, value) => Ty::Map(
                Box::new(Self::substitute(key, params, args)),
                Box::new(Self::substitute(value, params, args)),
            ),
            Ty::Tuple(fields) => Ty::Tuple(each(fields)),
            Ty::Enum(name, fields) => Ty::Enum(name.clone(), each(fields)),
            Ty::Struct(name, fields) => Ty::Struct(name.clone(), each(fields)),
            other => other.clone(),
        }
    }

    /// Returns the parameter types and result type of the function `function` for the call at
    /// `call`, with its type parameters, if it is generic or a method of a generic struct,
    /// filled in by `type_args`, which the call records for lowering.
    ///
    /// For example, `first` of `fn first[T](xs: list[T]) -> T:` with `str` for `T` takes a
    /// `list[str]` and gives a `str`.
    fn instantiate(&mut self, call: Span, function: &str, type_args: Vec<Ty>) -> (Vec<Ty>, Ty) {
        let info = &self.functions[function];
        let (params, ret) = (info.params.clone(), info.ret.clone());
        let type_params = info.type_params.clone();
        if type_params.is_empty() {
            return (params, ret);
        }
        let params = params
            .iter()
            .map(|p| Self::substitute(p, &type_params, &type_args))
            .collect();
        let ret = Self::substitute(&ret, &type_params, &type_args);
        self.instances.push(PendingInstance {
            span: call,
            caller: self.function.as_ref().map(|f| f.name.clone()),
            function: function.to_string(),
            args: type_args,
        });
        (params, ret)
    }

    /// Returns the parameter types and return type a signature writes. In a function of the
    /// `impl` block of a struct of type `this`, a first parameter `self` is that struct.
    ///
    /// For example, `(x: float) -> float` gives `[float]` and `float`. A parameter without a type
    /// gets an unknown one, so checking goes on, and is reported in `finish`.
    fn signature(
        &mut self,
        args: &Arguments,
        returns: Option<&TypeExpr>,
        this: Option<&Ty>,
    ) -> (Vec<Ty>, Ty) {
        let mut params = vec![];
        for (i, arg) in args.args.iter().enumerate() {
            let ty = match (&arg.annotation, this) {
                (annotation, Some(this)) if arg.arg == "self" && i == 0 => {
                    if annotation.is_some() {
                        let name = match this {
                            Ty::Struct(name, _) => struct_display(name).to_string(),
                            _ => String::new(),
                        };
                        self.error(
                            arg.span,
                            format!(
                                "'self' needs no type, since it is the '{name}' the method is \
                                 called on"
                            ),
                        );
                    }
                    this.clone()
                }
                (annotation, _) if arg.arg == "self" => {
                    self.error(
                        arg.span,
                        "'self' can only be the first parameter of a method",
                    );
                    match annotation {
                        Some(annotation) => self.written(annotation),
                        None => self.fresh(),
                    }
                }
                (Some(annotation), _) => self.written(annotation),
                (None, _) => {
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
        (params, ret)
    }

    /// Declares the functions of every `impl` block at the top level of `body`, each under its
    /// struct's name, a dot, and its own name, so calls anywhere in the program are checked
    /// against them.
    ///
    /// For example, `impl Point:` holding `fn norm(self) -> float:` declares `Point.norm` as
    /// `fn(Point) -> float`, called as `p.norm()`. An `impl` block of a trait declares its
    /// functions the same way, checks them against the trait's, and gives the type the trait's
    /// default methods it does not define.
    fn declare_methods(&mut self, body: &[Stmt]) {
        // every block's own functions first, so a default never takes the name of one
        let mut implemented = vec![];
        for stmt in body {
            let StmtKind::ImplDef {
                name: owner,
                name_span,
                trait_name,
                methods,
            } = &stmt.kind
            else {
                continue;
            };
            self.impl_spans.insert(stmt.span);
            // the linker reports an impl block for anything but a struct of its file, or a
            // builtin type in a trait's
            let Some(this) = self.impl_type(owner, trait_name.is_some()) else {
                continue;
            };
            let trait_ = match trait_name {
                Some(trait_name) => match self.impl_trait(owner, *name_span, trait_name) {
                    Some(trait_) => Some(trait_),
                    None => continue,
                },
                None => None,
            };
            let mut declared = HashMap::new();
            for method in methods {
                if let Some(name) = self.declare_method(owner, &this, method, trait_.as_deref()) {
                    declared.insert(name.clone(), format!("{owner}.{name}"));
                }
            }
            if let Some(trait_) = trait_ {
                self.report_missing_methods(owner, *name_span, &trait_, &declared);
                implemented.push((trait_, owner.clone(), *name_span, declared));
            }
        }
        for (trait_, owner, span, mut declared) in implemented {
            let defaults: Vec<(String, usize)> = self.traits[&trait_]
                .methods
                .iter()
                .filter(|m| m.has_body && !declared.contains_key(&m.name))
                .map(|m| (m.name.clone(), m.symbol))
                .collect();
            for (name, symbol) in defaults {
                if self
                    .struct_methods
                    .get(&owner)
                    .is_some_and(|methods| methods.contains_key(&name))
                    || self.struct_field_ty(&owner, &name).is_some()
                {
                    self.error(
                        span,
                        format!(
                            "'{}' already has a method or field '{name}', so it cannot get the \
                             default of '{}'",
                            struct_display(&owner),
                            struct_display(&trait_)
                        ),
                    );
                    continue;
                }
                let function = format!("{trait_}.{name}");
                self.struct_methods
                    .entry(owner.clone())
                    .or_default()
                    .insert(
                        name.clone(),
                        MethodInfo {
                            symbol,
                            has_self: true,
                            changes: false,
                            function: function.clone(),
                            default: true,
                        },
                    );
                declared.insert(name, function);
            }
            self.impls.insert((trait_, owner), declared);
        }
    }

    /// Returns the type `Self` is in an `impl` block for `owner`: the struct with its type
    /// parameters as its type arguments, or a builtin type, which only a trait's block may be
    /// for. Returns `None` for anything else, which the linker reported.
    fn impl_type(&self, owner: &str, of_trait: bool) -> Option<Ty> {
        if self.structs.contains_key(owner) {
            return Some(self.own_type(owner));
        }
        if !of_trait {
            return None;
        }
        builtin_key_type(owner)
    }

    /// Finds the trait an `impl` block for `owner` names, reporting a name that is no trait and
    /// a second block implementing the same trait for the same type.
    fn impl_trait(
        &mut self,
        owner: &str,
        owner_span: Span,
        trait_name: &TraitName,
    ) -> Option<String> {
        let name = &trait_name.name;
        let Some(info) = self.traits.get(name) else {
            let short = struct_display(name);
            let message = if self.structs.contains_key(name) {
                format!("'{short}' is a struct, not a trait")
            } else if self.enums.contains_key(name) {
                format!("'{short}' is an enum, not a trait")
            } else {
                format!("there is no trait '{short}'")
            };
            self.error(trait_name.span, message);
            return None;
        };
        self.add_reference(trait_name.span, info.symbol);
        if (name == EQ || name == ORD || name == DISPLAY) && !self.structs.contains_key(owner) {
            self.error(owner_span, format!("{owner} already implements '{name}'"));
            return None;
        }
        let key = (name.clone(), owner.to_string());
        if self.impls.contains_key(&key) {
            self.error(
                owner_span,
                format!(
                    "'{}' already implements '{}'",
                    struct_display(owner),
                    struct_display(name)
                ),
            );
            return None;
        }
        // filled in once every block's functions are declared
        self.impls.insert(key, HashMap::new());
        Some(name.clone())
    }

    /// Declares `method`, a function of an `impl` block for `owner`, whose `self` is `this`,
    /// returning its name, or reports why it cannot be declared. In a block implementing
    /// `trait_`, it must be one of the trait's methods, with the same signature.
    fn declare_method(
        &mut self,
        owner: &str,
        this: &Ty,
        method: &Stmt,
        trait_: Option<&str>,
    ) -> Option<String> {
        let StmtKind::FunctionDef {
            name,
            name_span,
            type_params,
            args,
            returns,
            ..
        } = &method.kind
        else {
            return None;
        };
        // a generic struct's type parameters are its methods' too
        let struct_params = self
            .structs
            .get(owner)
            .map(|info| info.type_params.clone())
            .unwrap_or_default();
        self.scope_params = struct_params
            .iter()
            .map(|&p| (self.type_params[p].name.clone(), p))
            .collect();
        if let Some(param) = type_params.first() {
            self.error(param.span, "a method cannot have type parameters for now");
            // declared anyway, so its signature has no further errors
            self.declare_type_params(&format!("{owner}.{name}"), type_params, false);
        }
        let short = struct_display(owner);
        let trait_method =
            trait_.map(|trait_| self.traits[trait_].methods.iter().any(|m| &m.name == name));
        let builtin_trait = match name.as_str() {
            "eq" => Some(EQ),
            "cmp" => Some(ORD),
            "display" => Some(DISPLAY),
            _ => None,
        };
        let error = if let Some(builtin) = builtin_trait
            && trait_.is_none()
        {
            Some(format!(
                "'{name}' is the method of the trait '{builtin}', so it goes in an \
                 'impl {builtin} for {short}:' block"
            ))
        } else if trait_method == Some(false) {
            let trait_ = struct_display(trait_.unwrap_or_default());
            Some(match self.structs.contains_key(owner) {
                true => format!(
                    "'{name}' is not a method of '{trait_}', so it goes in an 'impl {short}:' block"
                ),
                false => format!("'{name}' is not a method of '{trait_}'"),
            })
        } else if self
            .struct_methods
            .get(owner)
            .is_some_and(|methods| methods.contains_key(name))
        {
            Some(format!("'{short}' already has a method '{name}'"))
        } else if self.struct_field_ty(owner, name).is_some() {
            Some(format!(
                "'{name}' is a field of '{short}', so a method cannot have its name"
            ))
        } else if !self.structs.contains_key(owner) && METHODS.contains(&name.as_str()) {
            Some(format!(
                "'{name}' is a builtin method, so a method cannot have its name"
            ))
        } else {
            None
        };
        if let Some(message) = error {
            self.error(*name_span, message);
            self.scope_params.clear();
            return None;
        }
        self.self_type = Some(this.clone());
        let (params, ret) = self.signature(args, returns.as_ref(), Some(this));
        self.self_type = None;
        self.scope_params.clear();
        if let Some(trait_) = trait_ {
            self.check_implemented_signature(trait_, name, *name_span, this, (&params, &ret));
        }
        let has_self = args.args.first().is_some_and(|arg| arg.arg == "self");
        // the symbol's type is rebuilt from `params` and `ret` in `finish`
        let symbol = self.add_symbol(name, SymbolKind::Method, *name_span, Ty::Unit);
        self.symbols[symbol].scope = Some(owner.to_string());
        let function = format!("{owner}.{name}");
        self.functions.insert(
            function.clone(),
            FunctionInfo {
                symbol,
                type_params: struct_params,
                params,
                ret,
                ret_written: returns.is_some(),
            },
        );
        self.function_spans.insert(function.clone(), method.span);
        self.struct_methods
            .entry(owner.to_string())
            .or_default()
            .insert(
                name.clone(),
                MethodInfo {
                    symbol,
                    has_self,
                    changes: false,
                    function,
                    default: false,
                },
            );
        Some(name.clone())
    }

    /// Reports a function of an `impl` block of `trait_` whose signature differs from the
    /// trait's method of the same name, with `this` for `Self`.
    fn check_implemented_signature(
        &mut self,
        trait_: &str,
        name: &str,
        name_span: Span,
        this: &Ty,
        (params, ret): (&[Ty], &Ty),
    ) {
        let info = &self.functions[&format!("{trait_}.{name}")];
        let (expected, expected_ret) = (info.params.clone(), info.ret.clone());
        let self_param = self.traits[trait_].self_param;
        let fill = |ty: &Ty| Self::substitute(ty, &[self_param], std::slice::from_ref(this));
        let same = |a: &Ty, b: &Ty| a == b || matches!(b, Ty::Var(_));
        let matches = expected.len() == params.len()
            && expected.iter().zip(params).all(|(e, p)| same(&fill(e), p))
            && same(&fill(&expected_ret), ret);
        if matches {
            return;
        }
        let rendered: Vec<String> = expected
            .iter()
            .enumerate()
            .map(|(i, ty)| match i {
                0 => "self".to_string(),
                _ => self.show(ty),
            })
            .collect();
        let returns = match expected_ret {
            Ty::Unit => String::new(),
            ty => format!(" -> {}", self.show(&ty)),
        };
        self.error(
            name_span,
            format!(
                "'{name}' must take and return what '{}' says, as in 'fn {name}({}){returns}'",
                struct_display(trait_),
                rendered.join(", ")
            ),
        );
    }

    /// Reports each method of `trait_` without a default that the `impl` block for `owner`
    /// does not define.
    fn report_missing_methods(
        &mut self,
        owner: &str,
        span: Span,
        trait_: &str,
        declared: &HashMap<String, String>,
    ) {
        let missing: Vec<String> = self.traits[trait_]
            .methods
            .iter()
            .filter(|m| !m.has_body && !declared.contains_key(&m.name))
            .map(|m| format!("'{}'", m.name))
            .collect();
        let list = match missing.as_slice() {
            [] => return,
            [one] => one.clone(),
            [rest @ .., last] => format!("{} and {last}", rest.join(", ")),
        };
        self.error(
            span,
            format!(
                "'{}' does not implement {list} of '{}'",
                struct_display(owner),
                struct_display(trait_)
            ),
        );
    }

    /// Declares the methods of every trait defined at the top level of `body`, each under the
    /// trait's name, a dot, and its own name, as a function generic over the trait's `Self`.
    ///
    /// For example, `trait Shape:` holding `fn area(self) -> float` declares `Shape.area` as
    /// `fn[Self](Self) -> float`.
    fn declare_traits(&mut self, body: &[Stmt]) {
        for stmt in body {
            let StmtKind::TraitDef {
                name: owner,
                name_span,
                methods,
                ..
            } = &stmt.kind
            else {
                continue;
            };
            let Some(info) = self.traits.get(owner) else {
                continue;
            };
            // a trait defined twice was reported, and only its first definition counts
            if self.symbols[info.symbol].span != *name_span {
                continue;
            }
            let self_param = info.self_param;
            let mut infos: Vec<TraitMethodInfo> = vec![];
            for method in methods {
                let StmtKind::FunctionDef {
                    name,
                    name_span,
                    type_params,
                    args,
                    returns,
                    body,
                    ..
                } = &method.kind
                else {
                    continue;
                };
                if infos.iter().any(|m| &m.name == name) {
                    self.error(
                        *name_span,
                        format!("'{}' already has a method '{name}'", struct_display(owner)),
                    );
                    continue;
                }
                if let Some(param) = type_params.first() {
                    self.error(
                        param.span,
                        "a trait's method cannot have type parameters for now",
                    );
                    continue;
                }
                if args.args.first().is_none_or(|arg| arg.arg != "self") {
                    self.error(
                        *name_span,
                        format!(
                            "a trait's method takes 'self' first, as in 'fn {name}(self)', since \
                             it is called on a value"
                        ),
                    );
                    continue;
                }
                self.scope_params = HashMap::from([("Self".to_string(), self_param)]);
                let (params, ret) =
                    self.signature(args, returns.as_ref(), Some(&Ty::Param(self_param)));
                self.scope_params.clear();
                let symbol = self.add_symbol(name, SymbolKind::Method, *name_span, Ty::Unit);
                self.symbols[symbol].scope = Some(owner.clone());
                let function = format!("{owner}.{name}");
                // a default's parameters are declared with its body, and the others' here, for
                // editors to name
                if body.is_empty() {
                    for (arg, ty) in args.args.iter().zip(&params) {
                        let param =
                            self.add_symbol(&arg.arg, SymbolKind::Parameter, arg.span, ty.clone());
                        self.symbols[param].scope = Some(function.clone());
                    }
                }
                self.functions.insert(
                    function.clone(),
                    FunctionInfo {
                        symbol,
                        type_params: vec![self_param],
                        params,
                        ret,
                        ret_written: returns.is_some(),
                    },
                );
                if !body.is_empty() {
                    self.function_spans.insert(function, method.span);
                }
                infos.push(TraitMethodInfo {
                    name: name.clone(),
                    symbol,
                    has_body: !body.is_empty(),
                    changes: false,
                });
            }
            if let Some(info) = self.traits.get_mut(owner) {
                info.methods = infos;
            }
        }
    }

    /// Declares the name of every trait defined at the top level of `body`, with its type
    /// parameter `Self`, which is bounded by the trait itself.
    fn declare_trait_names(&mut self, body: &[Stmt]) {
        for stmt in body {
            let StmtKind::TraitDef {
                name, name_span, ..
            } = &stmt.kind
            else {
                continue;
            };
            self.trait_spans.insert(stmt.span);
            let error = if self.traits.contains_key(name) {
                Some(format!("trait '{name}' is already defined"))
            } else if self.enums.contains_key(name) {
                Some(format!("'{name}' is already defined as an enum"))
            } else if self.structs.contains_key(name) {
                Some(format!("'{name}' is already defined as a struct"))
            } else if name_span.file != FileId::PRELUDE
                && (is_reserved(name) || BUILTIN_TYPES.contains(&name.as_str()))
            {
                Some(format!("cannot redefine builtin '{name}'"))
            } else {
                None
            };
            if let Some(message) = error {
                self.error(*name_span, message);
                continue;
            }
            let symbol = self.add_symbol(name, SymbolKind::Trait, *name_span, Ty::Unit);
            // `Self` refers to the trait wherever it is written
            self.type_params.push(TypeParamInfo {
                name: "Self".to_string(),
                symbol,
                bounds: vec![name.clone()],
            });
            self.traits.insert(
                name.clone(),
                TraitInfo {
                    symbol,
                    self_param: self.type_params.len() - 1,
                    methods: vec![],
                },
            );
        }
    }

    /// Finds which methods change `self`: those that assign it or a place in it, call a builtin
    /// method that changes such a place, as `self.items.append(x)` does, or call a method that
    /// changes `self` on such a place. Repeats until no more are found, since methods can call
    /// each other in any order. The type of a place in `self` comes from the fields' written
    /// types alone, so this needs nothing inferred.
    ///
    /// For example, in `fn push(self, x: int): self.items.append(x)`, `push` changes `self`, and
    /// so does `fn twice(self): self.push(1): self.push(2)`.
    fn find_changing_methods(&mut self, body: &[Stmt]) {
        let mut methods = vec![];
        let mut defaults = vec![];
        for stmt in body {
            match &stmt.kind {
                StmtKind::ImplDef {
                    name: owner,
                    methods: defs,
                    ..
                } => {
                    for def in defs {
                        // a method defined twice was reported, and only its first definition
                        // counts
                        if let StmtKind::FunctionDef {
                            name,
                            name_span,
                            body,
                            ..
                        } = &def.kind
                            && let Some(info) =
                                self.struct_methods.get(owner).and_then(|m| m.get(name))
                            && self.symbols[info.symbol].span == *name_span
                            && info.has_self
                        {
                            methods.push((owner, name, body));
                        }
                    }
                }
                StmtKind::TraitDef {
                    name: owner,
                    methods: defs,
                    ..
                } => {
                    for def in defs {
                        if let StmtKind::FunctionDef {
                            name,
                            name_span,
                            body,
                            ..
                        } = &def.kind
                            && let Some(info) = self.traits.get(owner)
                            && let Some(i) = info.methods.iter().position(|m| {
                                m.name == *name && self.symbols[m.symbol].span == *name_span
                            })
                            && info.methods[i].has_body
                        {
                            defaults.push((owner, i, body));
                        }
                    }
                }
                _ => {}
            }
        }
        loop {
            let mut found = false;
            for &(owner, name, body) in &methods {
                if !self.struct_methods[owner][name].changes && self.changes_self(owner, body) {
                    if let Some(info) = self
                        .struct_methods
                        .get_mut(owner)
                        .and_then(|methods| methods.get_mut(name))
                    {
                        info.changes = true;
                    }
                    found = true;
                }
            }
            for &(owner, i, body) in &defaults {
                if !self.traits[owner].methods[i].changes && self.changes_self(owner, body) {
                    if let Some(info) = self.traits.get_mut(owner) {
                        info.methods[i].changes = true;
                    }
                    found = true;
                }
            }
            // a type's method changes `self` wherever its trait's does, so calls through a
            // bound and on the type itself agree
            let forced: Vec<(String, String)> = self
                .impls
                .iter()
                .flat_map(|((trait_, owner), implemented)| {
                    let info = &self.traits[trait_];
                    implemented.keys().filter_map(move |name| {
                        let changes = info.methods.iter().any(|m| &m.name == name && m.changes);
                        changes.then(|| (owner.clone(), name.clone()))
                    })
                })
                .collect();
            for (owner, name) in forced {
                if let Some(info) = self
                    .struct_methods
                    .get_mut(&owner)
                    .and_then(|methods| methods.get_mut(&name))
                    && !info.changes
                {
                    info.changes = true;
                    found = true;
                }
            }
            if !found {
                break;
            }
        }
        // a method that changes `self` where the trait's does not would change a receiver
        // that code calling it through a bound only borrows
        let mut reported = vec![];
        for ((trait_, owner), implemented) in &self.impls {
            for name in implemented.keys() {
                let changes = self.traits[trait_]
                    .methods
                    .iter()
                    .any(|m| &m.name == name && m.changes);
                if let Some(info) = self.struct_methods.get(owner).and_then(|m| m.get(name))
                    && info.changes
                    && !info.default
                    && !changes
                {
                    reported.push((
                        self.symbols[info.symbol].span,
                        format!(
                            "'{name}' cannot change 'self', since '{name}' of '{}' does not",
                            struct_display(trait_)
                        ),
                    ));
                }
            }
        }
        reported.sort_by_key(|(span, _)| (span.file, span.start));
        for (span, message) in reported {
            self.error(span, message);
        }
    }

    /// Returns whether `body`, a method of the struct `owner`, changes `self`, given the methods
    /// found to change it so far.
    fn changes_self(&self, owner: &str, body: &[Stmt]) -> bool {
        let mut assigned = vec![];
        collect_assigned(body, &mut assigned);
        if assigned.iter().any(|(name, _)| name == "self") {
            return true;
        }
        self.block_changes_self(owner, body)
    }

    fn block_changes_self(&self, owner: &str, body: &[Stmt]) -> bool {
        let in_self = |place_expr: &Expr| place(place_expr).is_some_and(|(root, _)| root == "self");
        body.iter().any(|stmt| {
            // a call that changes its receiver may be anywhere in the statement's own expressions
            let mut calls = false;
            for expr in stmt.exprs() {
                expr.visit(&mut |expr| calls |= self.call_changes_self(owner, expr));
            }
            calls
                || match &stmt.kind {
                    StmtKind::Assign { targets, .. } => targets.iter().any(|target| {
                        matches!(
                            target.kind,
                            ExprKind::Subscript { .. } | ExprKind::Attribute { .. }
                        ) && in_self(target)
                    }),
                    StmtKind::AugAssign { target, .. } => {
                        matches!(
                            target.kind,
                            ExprKind::Subscript { .. } | ExprKind::Attribute { .. }
                        ) && in_self(target)
                    }
                    StmtKind::For { body, .. } | StmtKind::While { body, .. } => {
                        self.block_changes_self(owner, body)
                    }
                    StmtKind::If { body, orelse, .. } => {
                        self.block_changes_self(owner, body)
                            || self.block_changes_self(owner, orelse)
                    }
                    StmtKind::Match { cases, .. } => cases
                        .iter()
                        .any(|case| self.block_changes_self(owner, &case.body)),
                    _ => false,
                }
        })
    }

    /// Returns whether `expr`, in a method of the struct `owner`, calls a method that changes
    /// its receiver on `self` or a place in it.
    fn call_changes_self(&self, owner: &str, expr: &Expr) -> bool {
        let ExprKind::Call { func, .. } = &expr.kind else {
            return false;
        };
        let ExprKind::Attribute { value, attr, .. } = &func.kind else {
            return false;
        };
        let Some(("self", steps)) = place(value) else {
            return false;
        };
        // the place's type, from the fields' written types
        let mut ty = Some(match self.traits.get(owner) {
            Some(info) => Ty::Param(info.self_param),
            None => builtin_key_type(owner).unwrap_or_else(|| self.own_type(owner)),
        });
        for step in steps {
            ty = match (step, ty) {
                (Step::Field(field), Some(Ty::Struct(name, args))) => {
                    self.field_type(&name, &args, field)
                }
                (Step::Index(_), Some(Ty::List(elem))) => Some(*elem),
                _ => None,
            };
        }
        match ty {
            Some(Ty::Param(param)) => self.type_params[param].bounds.iter().any(|trait_| {
                self.traits[trait_]
                    .methods
                    .iter()
                    .any(|m| m.name == *attr && m.changes)
            }),
            Some(ty)
                if let Some(method) = ty_key(&ty).and_then(|key| {
                    self.struct_methods
                        .get(&key)
                        .and_then(|methods| methods.get(attr.as_str()))
                }) =>
            {
                method.changes
            }
            Some(Ty::Struct(..)) => false,
            _ => changes_receiver(attr),
        }
    }

    /// Declares every enum and struct defined at the top level of `body`: first their names, so
    /// variants and fields can hold any of them, including their own, then their variants and
    /// fields.
    ///
    /// For example, `enum Tree:` with variants `Leaf(int)` and `Node(Tree, Tree)` declares `Tree`
    /// with a variant `Node` that holds two `Tree`s.
    fn declare_types(&mut self, body: &[Stmt]) {
        let enums = self.declare_enum_names(body);
        let structs = self.declare_struct_names(body);
        self.declare_trait_names(body);
        // after every name, so a type parameter named like a type is reported
        for (name, params, _) in &enums {
            let declared = self.declare_type_params(name, params, false);
            self.scope_params.clear();
            let ty = Ty::Enum(
                name.to_string(),
                declared.iter().map(|&p| Ty::Param(p)).collect(),
            );
            if let Some(info) = self.enums.get_mut(*name) {
                self.symbol_types[info.symbol] = ty;
                info.type_params = declared;
            }
        }
        for (name, params, _) in &structs {
            let declared = self.declare_type_params(name, params, false);
            self.scope_params.clear();
            let ty = Ty::Struct(
                name.to_string(),
                declared.iter().map(|&p| Ty::Param(p)).collect(),
            );
            if let Some(info) = self.structs.get_mut(*name) {
                self.symbol_types[info.symbol] = ty;
                info.type_params = declared;
            }
        }
        self.declare_variants(enums);
        self.declare_fields(structs);
        self.check_growing_types();
    }

    /// Reports a generic struct or enum that holds itself, directly or through other types,
    /// with type arguments that nest its own type parameter, as `X(Bad[list[T]])` does in
    /// `enum Bad[T]:`, since a value of `Bad[int]` could then hold a `Bad[list[int]]`, which
    /// could hold a `Bad[list[list[int]]]`, and so on, so its types would never end.
    fn check_growing_types(&mut self) {
        // each type parameter is given to the ones its fields and variants fill in
        let mut edges: Vec<(usize, usize, bool, Span)> = vec![];
        let mut defs: Vec<(Vec<(Ty, Span)>, String)> = vec![];
        for (name, info) in &self.structs {
            let fields = info
                .fields
                .iter()
                .map(|f| (f.ty.clone(), self.symbols[f.symbol].span))
                .collect();
            defs.push((fields, name.clone()));
        }
        for (name, info) in &self.enums {
            let fields = info
                .variants
                .iter()
                .flat_map(|v| {
                    let span = self.symbols[v.symbol].span;
                    v.fields.iter().map(move |f| (f.clone(), span))
                })
                .collect();
            defs.push((fields, name.clone()));
        }
        for (fields, _) in &defs {
            for (ty, span) in fields {
                let mut stack = vec![ty.clone()];
                while let Some(ty) = stack.pop() {
                    match ty {
                        Ty::Struct(name, args) | Ty::Enum(name, args) => {
                            let params = self
                                .structs
                                .get(&name)
                                .map(|info| info.type_params.clone())
                                .or_else(|| self.enums.get(&name).map(|i| i.type_params.clone()))
                                .unwrap_or_default();
                            for (&q, arg) in params.iter().zip(&args) {
                                let mut found = vec![];
                                self.params_in(arg, &mut found);
                                for p in found {
                                    edges.push((p, q, *arg != Ty::Param(p), *span));
                                }
                            }
                            stack.extend(args);
                        }
                        Ty::List(elem) => stack.push(*elem),
                        Ty::Map(key, value) => {
                            stack.push(*key);
                            stack.push(*value);
                        }
                        Ty::Tuple(fields) => stack.extend(fields),
                        _ => {}
                    }
                }
            }
        }
        let reaches = |from: usize, to: usize| {
            let mut seen = HashSet::from([from]);
            let mut stack = vec![from];
            while let Some(p) = stack.pop() {
                if p == to {
                    return true;
                }
                for &(a, b, _, _) in &edges {
                    if a == p && seen.insert(b) {
                        stack.push(b);
                    }
                }
            }
            false
        };
        let mut reported = vec![];
        for &(p, q, grows, span) in &edges {
            if grows && reaches(q, p) && !reported.contains(&span) {
                reported.push(span);
                let owner = self.symbols[self.type_params[q].symbol]
                    .scope
                    .clone()
                    .unwrap_or_default();
                let param = self.type_params[q].name.clone();
                self.error(
                    span,
                    format!(
                        "this gives '{owner}' a type that nests '{}' for '{param}', so its values \
                         could hold ever larger types",
                        self.type_params[p].name
                    ),
                );
            }
        }
    }

    /// Puts the type parameters of the generic struct or enum `name` in
    /// [`Inference::scope_params`], for the types its fields or variants write.
    fn enter_type_params(&mut self, params: &[usize]) {
        self.scope_params = params
            .iter()
            .map(|&p| (self.type_params[p].name.clone(), p))
            .collect();
    }

    /// Returns the type the struct or enum `name` has inside its own definition and `impl`
    /// blocks: itself with its type parameters as its type arguments, such as `Pair[A, B]`.
    fn own_type(&self, name: &str) -> Ty {
        if let Some(info) = self.structs.get(name) {
            return Ty::Struct(
                name.to_string(),
                info.type_params.iter().map(|&p| Ty::Param(p)).collect(),
            );
        }
        let params = self
            .enums
            .get(name)
            .map(|info| info.type_params.iter().map(|&p| Ty::Param(p)).collect())
            .unwrap_or_default();
        Ty::Enum(name.to_string(), params)
    }

    /// Returns the type of field `field` of the struct type `name[args]`, with its type
    /// arguments filled in, if it has the field.
    ///
    /// For example, field `a` of `Pair[int, str]`, written `a: A`, is an `int`.
    fn field_type(&self, name: &str, args: &[Ty], field: &str) -> Option<Ty> {
        let info = self.structs.get(name)?;
        let ty = &info.fields.iter().find(|f| f.name == field)?.ty;
        Some(Self::substitute(ty, &info.type_params, args))
    }

    /// Returns the types of every field of the struct type `name[args]`, in order, with its
    /// type arguments filled in.
    fn field_types(&self, name: &str, args: &[Ty]) -> Vec<Ty> {
        self.structs.get(name).map_or_else(Vec::new, |info| {
            info.fields
                .iter()
                .map(|f| Self::substitute(&f.ty, &info.type_params, args))
                .collect()
        })
    }

    /// Returns a fresh type variable for each type parameter of the struct or enum `name`, which
    /// a value of it whose type arguments are not known yet has.
    fn fresh_args(&mut self, name: &str) -> Vec<Ty> {
        let count = match self.structs.get(name) {
            Some(info) => info.type_params.len(),
            None => self
                .enums
                .get(name)
                .map_or(0, |info| info.type_params.len()),
        };
        (0..count).map(|_| self.fresh()).collect()
    }

    /// Declares the name of every enum defined at the top level of `body`, returning each with
    /// its type parameters and variants.
    #[allow(clippy::type_complexity)]
    fn declare_enum_names<'a>(
        &mut self,
        body: &'a [Stmt],
    ) -> Vec<(&'a String, &'a Vec<TypeParam>, &'a Vec<Variant>)> {
        let mut declared = vec![];
        for stmt in body {
            let StmtKind::EnumDef {
                name,
                name_span,
                type_params,
                variants,
                ..
            } = &stmt.kind
            else {
                continue;
            };
            self.enum_spans.insert(stmt.span);
            if name_span.file != FileId::PRELUDE
                && (is_reserved(name) || BUILTIN_TYPES.contains(&name.as_str()))
            {
                self.error(*name_span, format!("cannot redefine builtin '{name}'"));
                continue;
            }
            if self.enums.contains_key(name) {
                self.error(*name_span, format!("enum '{name}' is already defined"));
                continue;
            }
            let ty = Ty::Enum(name.clone(), vec![]);
            let symbol = self.add_symbol(name, SymbolKind::Enum, *name_span, ty);
            self.enums.insert(
                name.clone(),
                EnumInfo {
                    symbol,
                    type_params: vec![],
                    variants: vec![],
                },
            );
            declared.push((name, type_params, variants));
        }
        declared
    }

    /// Declares the variants of each enum in `declared`, from [`Inference::declare_enum_names`].
    fn declare_variants(&mut self, declared: Vec<(&String, &Vec<TypeParam>, &Vec<Variant>)>) {
        for (name, _, variants) in declared {
            let params = self.enums[name].type_params.clone();
            self.enter_type_params(&params);
            let own = self.own_type(name);
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
                    own.clone(),
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
            self.scope_params.clear();
        }
    }

    /// Declares the name of every struct defined at the top level of `body`, returning each with
    /// its type parameters and fields.
    #[allow(clippy::type_complexity)]
    fn declare_struct_names<'a>(
        &mut self,
        body: &'a [Stmt],
    ) -> Vec<(&'a String, &'a Vec<TypeParam>, &'a Vec<Field>)> {
        let mut declared = vec![];
        for stmt in body {
            let StmtKind::StructDef {
                name,
                name_span,
                type_params,
                fields,
                ..
            } = &stmt.kind
            else {
                continue;
            };
            self.struct_spans.insert(stmt.span);
            if self.structs.contains_key(name) {
                self.error(*name_span, format!("struct '{name}' is already defined"));
                continue;
            }
            if self.enums.contains_key(name) {
                self.error(
                    *name_span,
                    format!("'{name}' is already defined as an enum"),
                );
                continue;
            }
            if is_reserved(name) || BUILTIN_TYPES.contains(&name.as_str()) {
                self.error(*name_span, format!("cannot redefine builtin '{name}'"));
                continue;
            }
            let ty = Ty::Struct(name.clone(), vec![]);
            let symbol = self.add_symbol(name, SymbolKind::Struct, *name_span, ty);
            self.structs.insert(
                name.clone(),
                StructInfo {
                    symbol,
                    type_params: vec![],
                    fields: vec![],
                },
            );
            declared.push((name, type_params, fields));
        }
        declared
    }

    /// Declares the fields of each struct in `declared`, from
    /// [`Inference::declare_struct_names`], then reports any struct that holds itself without a
    /// list, option, or enum in between, since such a value would never end.
    fn declare_fields(&mut self, declared: Vec<(&String, &Vec<TypeParam>, &Vec<Field>)>) {
        for (name, _, fields) in &declared {
            let params = self.structs[*name].type_params.clone();
            self.enter_type_params(&params);
            let mut infos: Vec<FieldInfo> = vec![];
            for field in fields.iter() {
                if infos.iter().any(|info| info.name == field.name) {
                    self.error(
                        field.span,
                        format!("'{name}' already has a field '{}'", field.name),
                    );
                    continue;
                }
                let ty = self.written(&field.annotation);
                self.check_written_width(&field.annotation);
                let symbol =
                    self.add_symbol(&field.name, SymbolKind::Field, field.span, ty.clone());
                self.symbols[symbol].scope = Some(name.to_string());
                infos.push(FieldInfo {
                    name: field.name.clone(),
                    symbol,
                    ty,
                });
            }
            if let Some(info) = self.structs.get_mut(*name) {
                info.fields = infos;
            }
            self.scope_params.clear();
        }
        for (name, _, fields) in declared {
            for field in fields {
                let Some(ty) = self.struct_field_ty(name, &field.name) else {
                    continue;
                };
                if !self.reaches(&ty, name, &mut HashSet::new(), 0) {
                    continue;
                }
                self.error(
                    field.span,
                    format!(
                        "'{name}' holds itself through '{}', so a value of it would never end; \
                         hold it in a list or an option, as in '{}: option[{name}]'",
                        field.name, field.name
                    ),
                );
                // so nothing that follows the struct's fields goes around forever
                let unknown = self.fresh();
                if let Some(info) = self.structs.get_mut(name)
                    && let Some(field) = info.fields.iter_mut().find(|f| f.name == field.name)
                {
                    field.ty = unknown;
                }
            }
        }
    }

    /// Returns the type of field `field` of the struct `name`, if it has one.
    fn struct_field_ty(&self, name: &str, field: &str) -> Option<Ty> {
        let info = self.structs.get(name)?;
        let field = info.fields.iter().find(|f| f.name == field)?;
        Some(field.ty.clone())
    }

    /// Returns whether a value of type `ty` holds a value of the struct `target` directly, as a
    /// field of its own or of the tuples and structs it holds, rather than in a list or an enum.
    /// `seen` holds the struct types already looked through, and `depth` how many, so a type
    /// that grows with every step, which `check_growing_types` reports, ends too.
    ///
    /// For example, with `struct Wrap[T]:` holding `x: T`, a `Wrap[S]` reaches `S`.
    fn reaches(&self, ty: &Ty, target: &str, seen: &mut HashSet<String>, depth: usize) -> bool {
        match self.shallow(ty) {
            Ty::Struct(name, _) if name == target => true,
            Ty::Struct(name, args) => {
                let shown = self.show(&Ty::Struct(name.clone(), args.clone()));
                if depth > MAX_ELEMENT_VALUES || !seen.insert(shown) {
                    return false;
                }
                let fields = self.field_types(&name, &args);
                fields
                    .iter()
                    .any(|f| self.reaches(f, target, seen, depth + 1))
            }
            Ty::Tuple(fields) => fields
                .iter()
                .any(|f| self.reaches(f, target, seen, depth + 1)),
            _ => false,
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

    /// Counts the values a value of type `ty` is made of, counting each value of its tuples and
    /// structs.
    fn values(&self, ty: &Ty) -> usize {
        match self.shallow(ty) {
            Ty::Tuple(fields) => fields.iter().map(|f| self.values(f)).sum(),
            Ty::Struct(name, args) if self.structs.contains_key(&name) => self
                .field_types(&name, &args)
                .iter()
                .map(|f| self.values(f))
                .sum(),
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
            TypeExprKind::Map(key, value) => {
                let value_ty = self.written_type(value, &mut vec![]);
                self.check_entry_width(&value_ty, annotation.span);
                self.check_written_width(key);
                self.check_written_width(value);
            }
            TypeExprKind::Named { args, .. } => {
                args.iter().for_each(|a| self.check_written_width(a))
            }
            _ => {}
        }
    }

    /// Reports at `span` if an entry of a map whose values are of type `value` holds more values
    /// than [`MAX_ELEMENT_VALUES`], counting its key and each field of its tuples and structs.
    fn check_entry_width(&mut self, value: &Ty, span: Span) {
        let count = 1 + self.values(value);
        if count > MAX_ELEMENT_VALUES {
            let message = format!(
                "a map entry can hold at most {MAX_ELEMENT_VALUES} values, but a key and {} hold \
                 {count}",
                self.show(value)
            );
            self.error(span, message);
        }
    }

    /// Reports at `span` if a list element of type `elem` holds more values than
    /// [`MAX_ELEMENT_VALUES`], counting each field of its tuples and structs.
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
            TypeExprKind::Map(key, value) => {
                let key_ty = self.written_type(key, errors);
                self.require(key_ty.clone(), MAP_KEYS, key.span, MAP_KEY);
                Ty::Map(Box::new(key_ty), Box::new(self.written_type(value, errors)))
            }
            TypeExprKind::Tuple(fields) => Ty::Tuple(
                fields
                    .iter()
                    .map(|f| self.written_type(f, errors))
                    .collect(),
            ),
            TypeExprKind::Named { name, args } => {
                let args: Vec<Ty> = args.iter().map(|a| self.written_type(a, errors)).collect();
                if let Some(&param) = self.scope_params.get(name) {
                    self.add_reference(annotation.span, self.type_params[param].symbol);
                    if !args.is_empty() {
                        errors.push((annotation.span, format!("'{name}' takes no types")));
                        return self.fresh();
                    }
                    return Ty::Param(param);
                }
                if name == "Self" {
                    return match self.self_type.clone() {
                        Some(ty) if args.is_empty() => ty,
                        Some(_) => {
                            errors.push((annotation.span, "'Self' takes no types".to_string()));
                            self.fresh()
                        }
                        None => {
                            errors.push((
                                annotation.span,
                                "'Self' is only a type in a trait or an impl block".to_string(),
                            ));
                            self.fresh()
                        }
                    };
                }
                if let Some(info) = self.traits.get(name) {
                    let symbol = info.symbol;
                    self.add_reference(annotation.span, symbol);
                    let short = struct_display(name);
                    errors.push((
                        annotation.span,
                        format!(
                            "'{short}' is a trait, not a type; to take any type that implements \
                             it, write a type parameter, as in 'fn f[T: {short}](x: T)'"
                        ),
                    ));
                    return self.fresh();
                }
                let found = if let Some(info) = self.enums.get(name) {
                    Some((Some(info.symbol), info.type_params.len(), false))
                } else {
                    self.structs
                        .get(name)
                        .map(|info| (Some(info.symbol), info.type_params.len(), true))
                };
                let Some((symbol, takes, is_struct)) = found else {
                    errors.push((annotation.span, format!("there is no type '{name}'")));
                    return self.fresh();
                };
                if let Some(symbol) = symbol {
                    self.add_reference(annotation.span, symbol);
                }
                if args.len() != takes {
                    // a type that takes no types is only its name
                    let message = match takes {
                        0 => format!("'{name}' takes no types"),
                        1 => format!("'{name}' takes one type, as in '{name}[int]'"),
                        n => {
                            let example = vec!["int"; n].join(", ");
                            format!("'{name}' takes {n} types, as in '{name}[{example}]'")
                        }
                    };
                    errors.push((annotation.span, message));
                    return self.fresh();
                }
                match is_struct {
                    true => Ty::Struct(name.clone(), args),
                    false => Ty::Enum(name.clone(), args),
                }
            }
        }
    }

    fn check_block(&mut self, body: &[Stmt]) {
        for stmt in body {
            self.check_stmt(stmt);
        }
    }

    /// Checks `stmt`, then that a call inside one of its expressions that changes its receiver,
    /// such as `xs.pop()` in `print(xs.pop())`, is the statement's only use of what it changes,
    /// since a borrowed copy of it elsewhere in the statement could see the change.
    fn check_stmt(&mut self, stmt: &Stmt) {
        let outer = std::mem::take(&mut self.changing_calls);
        let deferred = self.deferred_methods.len();
        self.check_stmt_kind(stmt);
        // the statements of a nested block checked their own calls
        let calls = std::mem::replace(&mut self.changing_calls, outer);
        let mut mentions = vec![];
        for expr in stmt.exprs() {
            place_mentions(expr, &mut mentions);
        }
        let exempt: HashSet<Span> = calls.iter().map(|call| call.root).collect();
        let mut reported = HashSet::new();
        for call in calls.iter().filter(|call| call.inner) {
            if let Some(conflict) = first_conflict(&mentions, &call.path, &exempt)
                && reported.insert(conflict.span)
            {
                self.error(conflict.span, conflict_message(&call.name, &conflict));
            }
        }
        for method in &mut self.deferred_methods[deferred..] {
            if method.checked {
                continue;
            }
            method.checked = true;
            if let Some((root, path)) = changed_path(&method.receiver) {
                let mut exempt = exempt.clone();
                exempt.insert(root);
                method.conflict = first_conflict(&mentions, &path, &exempt);
            }
        }
    }

    fn check_stmt_kind(&mut self, stmt: &Stmt) {
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
                self.assigned_call = Some(value.without_try().span);
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
                    ExprKind::Attribute {
                        value: base, attr, ..
                    } => {
                        let field = self.field_of(target, base, attr);
                        self.expr_types.push((target.span, field.clone()));
                        field
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
                    ExprKind::Attribute { .. } => {
                        self.expect(&old, &result, value.span);
                        self.check_change(target);
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
                        } else if let Ty::Map(key, _) = self.shallow(&iter_ty) {
                            if matches!(target.kind, ExprKind::Tuple { .. }) {
                                self.error(target.span, MAP_PAIRS);
                                // still declare each name, so the error does not lead to others
                                let mut names = Vec::new();
                                target_names(target, &mut names);
                                for name in names {
                                    let unknown = self.fresh();
                                    self.check_target(name, &unknown, target.span);
                                }
                                self.check_loop_body(body);
                                return;
                            }
                            *key
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
                self.statement_call = Some(value.without_try().span);
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
            StmtKind::StructDef { name_span, .. } => {
                if !self.struct_spans.contains(&stmt.span) {
                    self.error(*name_span, "structs can only be defined at the top level");
                }
            }
            StmtKind::ImplDef {
                name,
                name_span,
                methods,
                ..
            } => {
                if !self.impl_spans.contains(&stmt.span) {
                    self.error(*name_span, "impl blocks can only be at the top level");
                    return;
                }
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name: method,
                        name_span,
                        args,
                        body,
                        ..
                    } = &method.kind
                    {
                        self.check_function(&format!("{name}.{method}"), *name_span, args, body);
                    }
                }
            }
            StmtKind::TraitDef {
                name,
                name_span,
                methods,
                ..
            } => {
                if !self.trait_spans.contains(&stmt.span) {
                    self.error(*name_span, "traits can only be defined at the top level");
                    return;
                }
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name: method,
                        name_span,
                        args,
                        body,
                        ..
                    } = &method.kind
                        && !body.is_empty()
                    {
                        self.check_function(&format!("{name}.{method}"), *name_span, args, body);
                    }
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
            // assigning an enum's or struct's name is reported where it happens
            if self.local(&local).is_none()
                && !self.value_enum(&local)
                && !self.value_struct(&local)
            {
                let ty = self.fresh();
                let symbol = self.add_symbol(&local, SymbolKind::Local, span, ty);
                self.set_local(&local, symbol);
            }
        }

        let saved_loops = std::mem::take(&mut self.loop_depth);
        self.check_block(body);
        self.loop_depth = saved_loops;

        // a method's changes to `self` are its receiver's
        for arg in args.args.iter().filter(|arg| arg.arg != "self") {
            if let Some(span) = lost_change(body, &arg.arg, &self.methods) {
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
                    Resolved::Struct(_) => {
                        self.error(target.span, format!("cannot assign to struct '{id}'"));
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
            ExprKind::Attribute { value, attr, .. } => {
                let field = self.field_of(target, value, attr);
                self.expr_types.push((target.span, field.clone()));
                self.expect(&field, found, value_span);
                self.check_change(target);
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

    /// Checks that `list`, which a statement changes, is a variable, an element of one, or a
    /// field of one, since a change to any other value would be lost, and that a function changes
    /// only its own variables.
    ///
    /// For example, `f().append(1)` and a function's `names.append(x)` for a global `names` are
    /// errors, while `grid[0].append(1)` and `p.items.append(1)` are fine.
    fn check_change(&mut self, list: &Expr) {
        self.check_change_of(list, false);
    }

    /// Checks a change like [`Inference::check_change`] does, to a struct if `is_struct`, which
    /// errors call a value rather than a list.
    fn check_change_of(&mut self, list: &Expr, is_struct: bool) {
        let global = self.changes_global(list);
        self.check_change_with(list, is_struct, global);
    }

    /// Returns whether a change to `list` would change a global from inside a function.
    fn changes_global(&self, list: &Expr) -> bool {
        match &place_root(list).kind {
            ExprKind::Name { id, .. } => {
                self.function.is_some() && self.local(id).is_none() && self.globals.contains_key(id)
            }
            _ => false,
        }
    }

    /// Checks a change like [`Inference::check_change_of`] does, given whether it changes a
    /// global from inside a function, `global`, which `finish` cannot find out itself.
    fn check_change_with(&mut self, list: &Expr, is_struct: bool, global: bool) {
        if has_slice(list) {
            self.error(list.span, SLICE_CHANGE);
            return;
        }
        // a value inside a tuple, as in `t[0].append(1)`, cannot change
        let mut node = list;
        let mut field = is_struct;
        loop {
            match &node.kind {
                ExprKind::Subscript { value, .. } => {
                    if self.indexed_tuples.contains(&value.span) {
                        self.error(node.span, TUPLE_CHANGE);
                        return;
                    }
                    node = value;
                }
                ExprKind::Attribute { value, .. } => {
                    field = true;
                    node = value;
                }
                _ => break,
            }
        }
        let root = place_root(list);
        let ExprKind::Name { id, .. } = &root.kind else {
            let message = match field {
                true => "this value is not stored in a variable, so changing it has no effect",
                false => "this list is not stored in a variable, so changing it has no effect",
            };
            self.error(root.span, message);
            return;
        };
        if global {
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
        if self.structs.contains_key(name) {
            return Resolved::Struct(name.to_string());
        }
        // a prelude variant is only ever read as `option.some`, but no variable may take its name
        if is_builtin(name) || is_reserved(name) {
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
            if changed_receiver(expr, &self.methods).is_some() {
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

    /// Reports a use of the struct `id` as a value, which only a call of it makes.
    fn struct_as_value(&mut self, span: Span, id: &str) {
        if let Some(info) = self.structs.get(id) {
            let symbol = info.symbol;
            self.add_reference(span, symbol);
        }
        let example = self.struct_example(id);
        self.error(
            span,
            format!("'{id}' is a struct, so a value of it is made with a call, as in '{example}'"),
        );
    }

    /// Writes a call that makes a value of the struct `id`, naming each field, such as
    /// `Point(x=..., y=...)`.
    fn struct_example(&self, id: &str) -> String {
        let fields: Vec<String> = self.structs.get(id).map_or_else(Vec::new, |info| {
            info.fields
                .iter()
                .map(|f| format!("{}=...", f.name))
                .collect()
        });
        format!("{id}({})", fields.join(", "))
    }

    /// Infers `Name(field=value, ...)`, which makes a value of the struct `name` from a value for
    /// each of its fields, given in any order, and returns the struct's type. Each value is
    /// inferred in the order it is written, which is the order both backends evaluate them in.
    ///
    /// For example, `Point(y=2.0, x=1.0)` is a `Point` if both of its fields are floats.
    fn infer_struct(
        &mut self,
        call: &Expr,
        func: &Expr,
        name: &str,
        args: &[Expr],
        keywords: &[Keyword],
    ) -> Ty {
        let symbol = self.structs[name].symbol;
        self.add_reference(func.span, symbol);
        self.expr_types.push((func.span, Ty::Unit));
        for arg in args {
            self.infer(arg);
        }
        // each value fills in the struct's type parameters with types of its own
        let type_args = self.fresh_args(name);
        let struct_ty = Ty::Struct(name.to_string(), type_args.clone());
        let type_params = self.structs[name].type_params.clone();
        let mut given: Vec<&str> = vec![];
        for keyword in keywords {
            let found = self.infer(&keyword.value);
            let field = self.structs[name]
                .fields
                .iter()
                .find(|f| f.name == keyword.arg)
                .map(|f| (f.symbol, Self::substitute(&f.ty, &type_params, &type_args)));
            let Some((field_symbol, ty)) = field else {
                self.error(
                    keyword.arg_span,
                    format!("'{name}' has no field '{}'", keyword.arg),
                );
                continue;
            };
            self.add_reference(keyword.arg_span, field_symbol);
            if given.contains(&keyword.arg.as_str()) {
                self.error(
                    keyword.arg_span,
                    format!("field '{}' is given more than once", keyword.arg),
                );
                continue;
            }
            given.push(&keyword.arg);
            self.expect(&ty, &found, keyword.value.span);
        }
        if !args.is_empty() {
            let example = self.struct_example(name);
            self.error(
                call.span,
                format!("a struct's values are named, as in '{example}'"),
            );
            return struct_ty;
        }
        let missing: Vec<String> = self.structs[name]
            .fields
            .iter()
            .filter(|f| !given.contains(&f.name.as_str()))
            .map(|f| format!("'{}'", f.name))
            .collect();
        if !missing.is_empty() {
            let (noun, names) = match &missing[..] {
                [only] => ("field", only.clone()),
                [rest @ .., last] => ("fields", format!("{} and {last}", rest.join(", "))),
                [] => unreachable!(),
            };
            self.error(call.span, format!("missing {noun} {names} for '{name}'"));
        }
        struct_ty
    }

    /// Infers `base.attr`, the field `attr` of a struct, written as `expr`, and returns the
    /// field's type.
    ///
    /// For example, `p.x` is a `float` if `p` is a `Point` whose `x` is a `float`.
    fn field_of(&mut self, expr: &Expr, base: &Expr, attr: &str) -> Ty {
        let base_ty = self.infer(base);
        let attr_span = Self::attr_span(expr, attr);
        match self.shallow(&base_ty) {
            Ty::Struct(name, args) => self.struct_field(&name, &args, attr, attr_span),
            Ty::Var(_) if self.is_global_in_function(base) => {
                // a function checked before the global's assignment, which makes it a struct
                let result = self.fresh();
                self.deferred_fields.push(DeferredField {
                    value_ty: base_ty,
                    attr: attr.to_string(),
                    attr_span,
                    result: result.clone(),
                });
                result
            }
            Ty::Var(_) => {
                self.error(
                    base.span,
                    format!(
                        "the type of this value must be known before its field '{attr}' is read"
                    ),
                );
                self.fresh()
            }
            other => {
                if METHODS.contains(&attr) {
                    self.error(
                        expr.span,
                        format!("'{attr}' is a method, so it can only be called"),
                    );
                } else {
                    let message = format!("{} has no field '{attr}'", self.show(&other));
                    self.error(attr_span, message);
                }
                self.fresh()
            }
        }
    }

    /// Returns the type of field `attr` of the struct type `name[args]`, recording the reference
    /// to it at `attr_span`, or reports that there is no such field.
    fn struct_field(&mut self, name: &str, args: &[Ty], attr: &str, attr_span: Span) -> Ty {
        let field = self.structs.get(name).and_then(|info| {
            info.fields
                .iter()
                .find(|f| f.name == attr)
                .map(|f| (f.symbol, Self::substitute(&f.ty, &info.type_params, args)))
        });
        match field {
            Some((symbol, ty)) => {
                self.add_reference(attr_span, symbol);
                ty
            }
            None => {
                let is_method = self
                    .struct_methods
                    .get(name)
                    .is_some_and(|methods| methods.contains_key(attr));
                let message = if METHODS.contains(&attr) || is_method {
                    format!("'{attr}' is a method, so it can only be called")
                } else {
                    format!("'{name}' has no field '{attr}'")
                };
                self.error(attr_span, message);
                self.fresh()
            }
        }
    }

    /// Settles a field read from a global that was not known when it was found.
    fn settle_field(&mut self, deferred: DeferredField) {
        match self.shallow(&deferred.value_ty) {
            Ty::Struct(name, args) => {
                let field = self.struct_field(&name, &args, &deferred.attr, deferred.attr_span);
                self.expect(&field, &deferred.result, deferred.attr_span);
            }
            other => {
                let message = format!("{} has no field '{}'", self.show(&other), deferred.attr);
                self.error(deferred.attr_span, message);
            }
        }
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
        // each value fills in the enum's type parameters with types of its own
        let type_args = self.fresh_args(enum_name);
        let enum_ty = Ty::Enum(enum_name.to_string(), type_args.clone());
        let ExprKind::Attribute { value, .. } = &func.kind else {
            return enum_ty;
        };
        let info = &self.enums[enum_name];
        let enum_symbol = info.symbol;
        let type_params = info.type_params.clone();
        let found = info.variants.iter().find(|v| v.name == variant).map(|v| {
            let fields = v
                .fields
                .iter()
                .map(|f| Self::substitute(f, &type_params, &type_args))
                .collect::<Vec<Ty>>();
            (v.symbol, fields)
        });
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
        let written = if prints_bare(enum_name) {
            variant.to_string()
        } else {
            format!("{enum_name}.{variant}")
        };
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

    /// Returns whether `name` is an enum whose name no variable may take: any but the prelude's,
    /// whose variants are written alone, so that a value may be named `result`.
    fn value_enum(&self, name: &str) -> bool {
        self.enums.contains_key(name) && !prints_bare(name)
    }

    /// Returns whether `name` is a struct whose name no variable may take: any but the prelude's,
    /// so that a value may be named `output`. A function still may not take it, since a call of
    /// the name makes the struct.
    fn value_struct(&self, name: &str) -> bool {
        self.structs
            .get(name)
            .is_some_and(|info| self.symbols[info.symbol].span.file != FileId::PRELUDE)
    }

    /// Returns the name of the enum `value` names, if it is one, as in `Shape` in `Shape.Circle`.
    fn enum_named(&self, value: &Expr) -> Option<String> {
        let ExprKind::Name { id, .. } = &value.kind else {
            return None;
        };
        if value.is_unwritten() && self.enums.contains_key(id) {
            return Some(id.clone());
        }
        match self.resolve_name(id) {
            Resolved::Enum(name) => Some(name),
            _ => None,
        }
    }

    /// Returns the name of the struct `value` names, if it is one, as in `Point` in
    /// `Point.origin()`.
    fn struct_named(&self, value: &Expr) -> Option<String> {
        let ExprKind::Name { id, .. } = &value.kind else {
            return None;
        };
        match self.resolve_name(id) {
            Resolved::Struct(name) => Some(name),
            _ => None,
        }
    }

    /// Infers a call of the associated function `name` of the struct `owner`, written as `func`
    /// (whose value names the struct), and returns its result's type.
    ///
    /// For example, `Point.origin()` is a `Point` if `impl Point:` holds
    /// `fn origin() -> Point:`.
    fn infer_associated(
        &mut self,
        call: &Expr,
        func: &Expr,
        value: &Expr,
        owner: &str,
        name: &str,
        args: &[Expr],
    ) -> Ty {
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
        let name_span = Self::attr_span(func, name);
        let symbol = self.structs[owner].symbol;
        self.add_reference(value.span, symbol);
        self.expr_types.push((func.span, Ty::Unit));
        let method = self
            .struct_methods
            .get(owner)
            .and_then(|methods| methods.get(name))
            .map(|method| (method.symbol, method.has_self));
        let short = struct_display(owner);
        // a trait's default method always has `self`, so this is a function of the struct's
        let symbol = match method {
            None => {
                self.error(name_span, format!("'{short}' has no function '{name}'"));
                return self.fresh();
            }
            Some((_, true)) => {
                self.error(
                    name_span,
                    format!("'{name}' is a method, so call it on a value, as in 'value.{name}()'"),
                );
                return self.fresh();
            }
            Some((symbol, false)) => symbol,
        };
        self.add_reference(name_span, symbol);
        let function = format!("{owner}.{name}");
        let type_args = self.fresh_args(owner);
        let (params, ret) = self.instantiate(call.span, &function, type_args);
        self.methods.insert(
            call.span,
            Method {
                function,
                receiver: false,
                changes: false,
                implementations: None,
            },
        );
        if params.len() != args.len() {
            self.arity_error(call.span, name, &params.len().to_string(), args.len());
            return ret;
        }
        for ((param, arg_ty), arg) in params.iter().zip(&arg_types).zip(args) {
            self.expect(param, arg_ty, arg.span);
        }
        ret
    }

    /// Infers a call of the method `name` on `receiver`, a value of the struct `owner`, and
    /// returns its result's type. A method that changes `self` changes the receiver, so the call
    /// must be a statement of its own or the value of an assignment, and the receiver a variable
    /// or a place in one, as for `append`.
    ///
    /// For example, `p.norm()` is a `float` if `impl Point:` holds `fn norm(self) -> float:`.
    fn infer_struct_method(
        &mut self,
        call: &Expr,
        func: &Expr,
        receiver: &Expr,
        (owner, type_args): (&str, &[Ty]),
        name: &str,
        args: &[Expr],
    ) -> Ty {
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
        let name_span = Self::attr_span(func, name);
        let short = struct_display(owner);
        let method = self
            .struct_methods
            .get(owner)
            .and_then(|methods| methods.get(name))
            .map(|method| (method.symbol, method.has_self, method.changes));
        let (symbol, changes) = match method {
            None => {
                let mut message = match builtin_key_type(owner) {
                    Some(_) => format!("{owner} has no method '{name}'"),
                    None => format!("'{short}' has no method '{name}'"),
                };
                if self.struct_field_ty(owner, name).is_some() {
                    message.push_str(&format!(
                        "; '{name}' is a field of '{short}', so read it without parentheses"
                    ));
                }
                self.error(name_span, message);
                return self.fresh();
            }
            Some((_, false, _)) => {
                self.error(
                    name_span,
                    format!(
                        "'{name}' has no 'self', so call it on the struct, as in \
                         '{short}.{name}()'"
                    ),
                );
                return self.fresh();
            }
            Some((symbol, true, changes)) => (symbol, changes),
        };
        self.add_reference(name_span, symbol);
        let (function, type_args) = self.method_function(owner, type_args, name);
        let (mut params, ret) = self.instantiate(call.span, &function, type_args);
        params.remove(0);
        self.methods.insert(
            call.span,
            Method {
                function,
                receiver: true,
                changes,
                implementations: None,
            },
        );
        self.check_method_args(call, name, &params, &arg_types, args);
        if changes {
            self.check_changing_call(call.span, receiver, name, &ret);
            self.check_change_of(receiver, true);
        }
        ret
    }

    /// Checks the arguments of a call of the method `name`, whose parameters after `self` are
    /// `params`.
    fn check_method_args(
        &mut self,
        call: &Expr,
        name: &str,
        params: &[Ty],
        arg_types: &[Ty],
        args: &[Expr],
    ) {
        if params.len() != args.len() {
            self.arity_error(call.span, name, &params.len().to_string(), args.len());
        } else {
            for ((param, arg_ty), arg) in params.iter().zip(arg_types).zip(args) {
                self.expect(param, arg_ty, arg.span);
            }
        }
    }

    /// Reports the call at `call` of `name`, a method that changes `receiver` and returns `ret`,
    /// unless it is a statement of its own or returns a value, and notes it for
    /// [`Inference::check_stmt`], which checks that a call inside an expression is its
    /// statement's only use of what it changes.
    fn check_changing_call(&mut self, call: Span, receiver: &Expr, name: &str, ret: &Ty) {
        if *ret == Ty::Unit && self.statement_call != Some(call) {
            self.error(
                call,
                format!(
                    "'{name}' changes its receiver and returns no value, so it can only be a \
                     statement of its own"
                ),
            );
        }
        self.note_changing_call(call, receiver, name, *ret != Ty::Unit);
    }

    /// Notes the call at `call` of `name`, which changes `receiver`, for
    /// [`Inference::check_stmt`]: whether it `returns` a value and is inside an expression,
    /// rather than a statement of its own or the whole value of an assignment.
    fn note_changing_call(&mut self, call: Span, receiver: &Expr, name: &str, returns: bool) {
        // a receiver that is not a variable was reported
        if let Some((root, path)) = changed_path(receiver) {
            let whole = [self.statement_call, self.assigned_call].contains(&Some(call));
            self.changing_calls.push(ChangingCall {
                name: name.to_string(),
                root,
                path,
                inner: returns && !whole,
            });
        }
    }

    /// Returns the function a call of the method `name` on a value of `owner[type_args]`
    /// calls, and the type arguments of its copy: the struct's for one of its own methods, or
    /// the value's type, as `Self`, for a trait's default method.
    fn method_function(&self, owner: &str, type_args: &[Ty], name: &str) -> (String, Vec<Ty>) {
        let info = &self.struct_methods[owner][name];
        if !info.default {
            return (info.function.clone(), type_args.to_vec());
        }
        let this = builtin_key_type(owner)
            .unwrap_or_else(|| Ty::Struct(owner.to_string(), type_args.to_vec()));
        (info.function.clone(), vec![this])
    }

    /// Infers a call of the method `name` on `receiver`, whose type is the type parameter
    /// `param`, through the one trait among its bounds that has the method, and returns its
    /// result's type. Each copy of the function calls the method of its own type.
    ///
    /// For example, in `fn f[T: Shape](x: T) -> float: ret x.area()`, `x.area()` calls
    /// `Shape.area` with `T` as `Self`.
    #[allow(clippy::too_many_arguments)]
    fn infer_trait_call(
        &mut self,
        call: &Expr,
        func: &Expr,
        receiver: &Expr,
        param: usize,
        name: &str,
        args: &[Expr],
        arg_types: &[Ty],
    ) -> Ty {
        let name_span = Self::attr_span(func, name);
        let shown = self.type_params[param].name.clone();
        let traits: Vec<String> = self.type_params[param]
            .bounds
            .iter()
            .filter(|t| self.traits[*t].methods.iter().any(|m| m.name == name))
            .cloned()
            .collect();
        let trait_ = match traits.as_slice() {
            [trait_] => trait_.clone(),
            [] => {
                let mut message = format!(
                    "values of type '{shown}' have no method '{name}', since '{shown}' may be any \
                     type"
                );
                let mut having: Vec<&String> = self
                    .traits
                    .iter()
                    .filter(|(_, info)| info.methods.iter().any(|m| m.name == name))
                    .map(|(trait_, _)| trait_)
                    .collect();
                having.sort();
                if let Some(trait_) = having.first() {
                    let short = struct_display(trait_);
                    message.push_str(&format!(
                        " that implements a trait with it, as in '[{shown}: {short}]'"
                    ));
                }
                self.error(name_span, message);
                return self.fresh();
            }
            [first, second, ..] => {
                self.error(
                    name_span,
                    format!(
                        "'{name}' is a method of both '{}' and '{}', which bound '{shown}', so \
                         this call could mean either",
                        struct_display(first),
                        struct_display(second)
                    ),
                );
                return self.fresh();
            }
        };
        let info = self.traits[&trait_]
            .methods
            .iter()
            .find(|m| m.name == name)
            .map(|m| (m.symbol, m.changes));
        let Some((symbol, changes)) = info else {
            return self.fresh();
        };
        self.add_reference(name_span, symbol);
        let function = format!("{trait_}.{name}");
        let (mut params, ret) = self.instantiate(call.span, &function, vec![Ty::Param(param)]);
        params.remove(0);
        let implementations = self
            .impls
            .iter()
            .filter(|((t, _), _)| *t == trait_)
            .filter_map(|((_, ty), methods)| Some((ty.clone(), methods.get(name)?.clone())))
            .collect();
        self.methods.insert(
            call.span,
            Method {
                function,
                receiver: true,
                changes,
                implementations: Some(implementations),
            },
        );
        self.check_method_args(call, name, &params, arg_types, args);
        if changes {
            self.check_changing_call(call.span, receiver, name, &ret);
            self.check_change_of(receiver, true);
        }
        ret
    }

    /// Settles a method called on a value whose type was not known when it was found.
    fn settle_method(&mut self, deferred: DeferredMethod) {
        let name = &deferred.name;
        let (owner, type_args) = match self.shallow(&deferred.value_ty) {
            Ty::Struct(owner, args) => (owner, args),
            ty if ty_key(&ty).is_some_and(|key| self.struct_methods.contains_key(&key)) => {
                (ty_key(&ty).unwrap_or_default(), vec![])
            }
            Ty::Var(_) => {
                self.error(
                    deferred.receiver.span,
                    format!(
                        "the type of this value must be known before its method '{name}' is called"
                    ),
                );
                return;
            }
            other => {
                let message = format!("{} has no method '{}'", self.show(&other), deferred.name);
                self.error(deferred.name_span, message);
                return;
            }
        };
        let method = self
            .struct_methods
            .get(&owner)
            .and_then(|methods| methods.get(name))
            .map(|method| (method.symbol, method.has_self, method.changes));
        let Some((symbol, true, changes)) = method else {
            let message = format!("'{}' has no method '{name}'", struct_display(&owner));
            self.error(deferred.name_span, message);
            return;
        };
        self.add_reference(deferred.name_span, symbol);
        let (function, type_args) = self.method_function(&owner, &type_args, name);
        let (mut params, ret) = self.instantiate(deferred.call, &function, type_args);
        params.remove(0);
        if changes {
            if ret == Ty::Unit && !deferred.statement {
                self.error(
                    deferred.call,
                    format!(
                        "'{name}' changes its receiver and returns no value, so it can only be a \
                         statement of its own"
                    ),
                );
            } else if !deferred.statement
                && !deferred.assigned
                && let Some(conflict) = &deferred.conflict
            {
                self.error(conflict.span, conflict_message(name, conflict));
            }
            self.check_change_with(&deferred.receiver, true, deferred.global);
        } else if ret == Ty::Unit && !deferred.statement {
            self.error(
                deferred.call,
                format!(
                    "'{name}' returns no value, so it can only be called as a statement of its own"
                ),
            );
        }
        self.methods.insert(
            deferred.call,
            Method {
                function,
                receiver: true,
                changes,
                implementations: None,
            },
        );
        if params.len() != deferred.args.len() {
            self.arity_error(
                deferred.call,
                name,
                &params.len().to_string(),
                deferred.args.len(),
            );
        } else {
            for (param, (arg_ty, span)) in params.iter().zip(&deferred.args) {
                self.expect(param, arg_ty, *span);
            }
        }
        self.expect(&ret, &deferred.result, deferred.call);
    }

    /// Infers `value?`, the value an `ok` or a `some` holds, checking that `value` is a result
    /// whose error the function it is in returns, or an option in a function that returns one.
    fn infer_try(&mut self, expr: &Expr, value: &Expr) -> Ty {
        let found = self.infer(value);
        let inner = self.fresh();
        let Some(function) = &self.function else {
            let message = "'?' can only return from a function that returns a result or an option";
            self.error(expr.span, message.to_string());
            return inner;
        };
        let (name, ret) = (function.name.clone(), self.shallow(&function.ret));
        let found_enum = match self.shallow(&found) {
            Ty::Enum(enum_name, _) => Some(enum_name),
            _ => None,
        };
        match &ret {
            Ty::Enum(enum_name, args) if enum_name == RESULT => {
                let error = self.fresh();
                let result = Ty::Enum(RESULT.to_string(), vec![inner.clone(), error.clone()]);
                if !self.unify(&result, &found) {
                    let message = match found_enum.as_deref() {
                        Some(OPTION) => format!(
                            "'?' on an option needs '{name}' to return an option, but it returns {}",
                            self.show(&ret)
                        ),
                        _ => format!("'?' needs a result, found {}", self.show(&found)),
                    };
                    self.error(expr.span, message);
                } else if let Some(returned) = args.get(1)
                    && !self.unify(returned, &error)
                {
                    let message = format!(
                        "'{name}' returns {}, so '?' needs an error of type {}, found {}",
                        self.show(&ret),
                        self.show(returned),
                        self.show(&error)
                    );
                    self.error(expr.span, message);
                }
            }
            Ty::Enum(enum_name, _) if enum_name == OPTION => {
                let option = Ty::Enum(OPTION.to_string(), vec![inner.clone()]);
                if !self.unify(&option, &found) {
                    let message = match found_enum.as_deref() {
                        Some(RESULT) => format!(
                            "'?' on a result needs '{name}' to return a result, but it returns {}; \
                             add '.ok()' before '?' to drop the error",
                            self.show(&ret)
                        ),
                        _ => format!("'?' needs an option, found {}", self.show(&found)),
                    };
                    self.error(expr.span, message);
                }
            }
            _ => {
                let returns = match ret {
                    Ty::Unit => "no value".to_string(),
                    _ => self.show(&ret),
                };
                let message = format!(
                    "'?' returns from '{name}', which returns {returns}, not a result or an option"
                );
                self.error(expr.span, message);
            }
        }
        inner
    }

    fn infer_kind(&mut self, expr: &Expr) -> Ty {
        match &expr.kind {
            ExprKind::Try { value } => self.infer_try(expr, value),
            // any value is written as text, as `print` writes it
            ExprKind::JoinedStr { values } => {
                for value in values {
                    self.infer(value);
                }
                Ty::Str
            }
            ExprKind::Constant { value, .. } => match **value {
                Constant::Bool(_) => Ty::Bool,
                Constant::Str(_) | Constant::Char(_) => Ty::Str,
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
                Resolved::Struct(_) => {
                    self.struct_as_value(expr.span, id);
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
            } if ops.iter().any(CompOp::is_membership) => {
                let needle_ty = self.infer(left);
                let container_ty = self.infer(&comparators[0]);
                let name = match ops.iter().find(|op| op.is_membership()) {
                    Some(CompOp::NotIn) => "not in",
                    _ => "in",
                };
                if ops.len() > 1 {
                    for comparator in &comparators[1..] {
                        self.infer(comparator);
                    }
                    let message = format!(
                        "'{name}' cannot be chained with other comparisons; add parentheses, as \
                         in '(x {name} xs) == b'"
                    );
                    self.error(expr.span, message);
                    return Ty::Bool;
                }
                self.membership(Membership {
                    span: expr.span,
                    container: comparators[0].span,
                    container_ty,
                    needle: left.span,
                    needle_ty,
                    name,
                });
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
                    let message = match op {
                        CompOp::Equal | CompOp::NotEqual => {
                            self.equalities
                                .push((ty.clone(), expr.span, "'==' and '!='"));
                            prev = ty;
                            continue;
                        }
                        // tests of membership are the arm above's
                        CompOp::In | CompOp::NotIn => continue,
                        CompOp::LessThan => {
                            "'<' needs int, float, str, or a type that implements 'ord', found {}"
                        }
                        CompOp::LessThanEqual => {
                            "'<=' needs int, float, str, or a type that implements 'ord', found {}"
                        }
                        CompOp::GreaterThan => {
                            "'>' needs int, float, str, or a type that implements 'ord', found {}"
                        }
                        CompOp::GreaterThanEqual => {
                            "'>=' needs int, float, str, or a type that implements 'ord', found {}"
                        }
                    };
                    self.require_ordered(ty.clone(), expr.span, message);
                    prev = ty;
                }
                Ty::Bool
            }
            ExprKind::Call {
                func,
                args,
                keywords,
            } => self.infer_call(expr, func, args, keywords),
            ExprKind::Attribute { value, attr, .. } => {
                if let Some(name) = self.enum_named(value) {
                    return self.infer_variant(expr, expr, &name, attr, None);
                }
                self.field_of(expr, value, attr)
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
            ExprKind::Dict { keys, values } => {
                let key_ty = self.fresh();
                let value_ty = self.fresh();
                for (key, value) in keys.iter().zip(values) {
                    let ty = self.infer(key);
                    self.expect(&key_ty, &ty, key.span);
                    let ty = self.infer(value);
                    self.expect(&value_ty, &ty, value.span);
                }
                let key_span = keys.first().map_or(expr.span, |key| key.span);
                self.require(key_ty.clone(), MAP_KEYS, key_span, MAP_KEY);
                let map = Ty::Map(Box::new(key_ty), Box::new(value_ty));
                self.map_literals.push((expr.span, map.clone()));
                map
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
            && !matches!(slice.kind, ExprKind::Slice { .. })
        {
            // a function checked before the global's assignment, which may make it a tuple, or,
            // for an index not known to be an int, a map
            let index_ty = self.infer(slice);
            if constant_int(slice).is_some() || self.shallow(&index_ty) != Ty::Int {
                let result = self.fresh();
                self.deferred_indexes.push(DeferredIndex {
                    value: value.span,
                    value_ty,
                    slice: slice.clone(),
                    index_ty,
                    result: result.clone(),
                });
                return (result, Indexed::List);
            }
            let elem = self.fresh();
            self.expect(&Ty::List(Box::new(elem.clone())), &value_ty, value.span);
            return (elem, Indexed::List);
        }
        let (sliced, elem, indexed) = match self.shallow(&value_ty) {
            Ty::Str => (Ty::Str, Ty::Str, Indexed::Str),
            Ty::Tuple(fields) => {
                return (self.tuple_field(value.span, &fields, slice), Indexed::Tuple);
            }
            Ty::Map(key, entry) => {
                self.indexed_maps.insert(value.span);
                if matches!(slice.kind, ExprKind::Slice { .. }) {
                    self.error(slice.span, "a map cannot be sliced");
                    return (*entry, Indexed::Map);
                }
                let index = self.infer(slice);
                self.expect(&key, &index, slice.span);
                return (*entry, Indexed::Map);
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

    /// Checks a test of whether a container holds a value: a map's key, a list's element, which
    /// `==` compares with the value, or a string's text. A container not yet known waits for
    /// `finish`, which [`Inference::settle_membership`] settles.
    ///
    /// For example, `2 in xs` needs `xs` to be a `list[int]` once `xs` is known to be a list, and
    /// `"a" in m` a `map[str, V]`.
    fn membership(&mut self, test: Membership) {
        if let Ty::Var(_) = self.shallow(&test.container_ty) {
            self.deferred_memberships.push(test);
            return;
        }
        self.settle_membership(test);
    }

    /// Settles a test of whether a container holds a value, taking a container that is still
    /// not known to be a list, as a subscript does.
    fn settle_membership(&mut self, test: Membership) {
        match self.shallow(&test.container_ty) {
            Ty::Map(key, _) => self.expect(&key, &test.needle_ty, test.needle),
            Ty::Str => self.expect(&Ty::Str, &test.needle_ty, test.needle),
            Ty::List(elem) => self.compare_elements(*elem, &test),
            Ty::Var(_) => {
                let elem = self.fresh();
                let list = Ty::List(Box::new(elem.clone()));
                self.expect(&list, &test.container_ty, test.container);
                self.compare_elements(elem, &test);
            }
            other => {
                let message = format!(
                    "'{}' needs a str, list, or map, found {}",
                    test.name,
                    self.show(&other)
                );
                self.error(test.container, message);
            }
        }
    }

    /// Checks that a list of `elem` can hold the value `test` looks for, and that `==` can
    /// compare the two.
    fn compare_elements(&mut self, elem: Ty, test: &Membership) {
        self.expect(&elem, &test.needle_ty, test.needle);
        let what = match test.name {
            "contains" => "'contains'",
            _ => "'in' and 'not in'",
        };
        self.equalities.push((elem, test.span, what));
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
            Ty::Map(key, value) => {
                self.indexed_maps.insert(deferred.value);
                self.expect(&key, &deferred.index_ty, deferred.slice.span);
                self.expect(&value, &deferred.result, deferred.slice.span);
            }
            _ => {
                self.expect(&Ty::Int, &deferred.index_ty, deferred.slice.span);
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

    fn infer_call(&mut self, call: &Expr, func: &Expr, args: &[Expr], keywords: &[Keyword]) -> Ty {
        if let ExprKind::Name { id, .. } = &func.kind
            && let Resolved::Struct(name) = self.resolve_name(id)
        {
            return self.infer_struct(call, func, &name, args, keywords);
        }
        if let Some(keyword) = keywords.first() {
            // a struct's values are the only named ones, so nothing else is checked
            for arg in args {
                self.infer(arg);
            }
            for keyword in keywords {
                self.infer(&keyword.value);
            }
            self.error(
                keyword.arg_span,
                "only a struct is made with named values; pass this value without its name",
            );
            return self.fresh();
        }
        if let ExprKind::Attribute { value, attr, .. } = &func.kind {
            if let Some(name) = self.enum_named(value) {
                return self.infer_variant(call, func, &name, attr, Some(args));
            }
            if let Some(name) = self.struct_named(value) {
                return self.infer_associated(call, func, value, &name, attr, args);
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
                let (symbol, count) = {
                    let info = &self.functions[&name];
                    (info.symbol, info.type_params.len())
                };
                // each call fills in the type parameters with types of its own
                let type_args = (0..count).map(|_| self.fresh()).collect();
                let (params, ret) = self.instantiate(call.span, &name, type_args);
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
            // made by `infer_struct`, above
            Resolved::Struct(_) => self.fresh(),
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
                // the subject fills in the type arguments, as `some(x)` fills in `option`'s
                let type_args = self.fresh_args(&name);
                let info = &self.enums[&name];
                let enum_symbol = info.symbol;
                let type_params = info.type_params.clone();
                let found = info.variants.iter().find(|v| v.name == *variant).map(|v| {
                    let fields = v
                        .fields
                        .iter()
                        .map(|f| Self::substitute(f, &type_params, &type_args))
                        .collect::<Vec<Ty>>();
                    (v.symbol, fields)
                });
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
                Some((Ty::Enum(name, type_args), written, fields))
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
        match self.shallow(&receiver_ty) {
            // a struct that does not implement `eq` or `display` itself compares or is written
            // part by part
            Ty::Struct(owner, _)
                if (name == "eq" || name == DISPLAY)
                    && !self
                        .struct_methods
                        .get(&owner)
                        .is_some_and(|methods| methods.contains_key(name)) => {}
            Ty::Struct(owner, type_args) => {
                return self.infer_struct_method(
                    call,
                    func,
                    receiver,
                    (&owner, &type_args),
                    name,
                    args,
                );
            }
            // every value is written as text, whatever its bounds
            Ty::Param(_) if name == DISPLAY => {}
            Ty::Param(param) => {
                let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
                return self.infer_trait_call(call, func, receiver, param, name, args, &arg_types);
            }
            ty if ty_key(&ty).is_some_and(|key| {
                self.struct_methods
                    .get(&key)
                    .is_some_and(|methods| methods.contains_key(name))
            }) =>
            {
                let key = ty_key(&ty).unwrap_or_default();
                return self.infer_struct_method(call, func, receiver, (&key, &[]), name, args);
            }
            Ty::Var(_) if !METHODS.contains(&name) => {
                // as a global a function checked before the global's assignment reads, which
                // makes it a struct
                let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
                let result = self.fresh();
                self.deferred_methods.push(DeferredMethod {
                    call: call.span,
                    receiver: receiver.clone(),
                    global: self.changes_global(receiver),
                    statement: self.statement_call == Some(call.span),
                    assigned: self.assigned_call == Some(call.span),
                    value_ty: receiver_ty,
                    name: name.to_string(),
                    name_span: Self::attr_span(func, name),
                    args: arg_types
                        .into_iter()
                        .zip(args.iter().map(|a| a.span))
                        .collect(),
                    result: result.clone(),
                    checked: false,
                    conflict: None,
                });
                return result;
            }
            _ => {}
        }
        let arg_types: Vec<Ty> = args.iter().map(|arg| self.infer(arg)).collect();
        if changes_receiver(name) {
            // `pop` returns what it takes out, and the others return nothing
            self.note_changing_call(call.span, receiver, name, name == "pop");
            if name != "pop" && self.statement_call != Some(call.span) {
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
                    &[Kind::Str, Kind::List, Kind::Map],
                    receiver.span,
                    "'len' needs a str, list, or map, found {}",
                );
                Ty::Int
            }
            "contains" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                    return Ty::Bool;
                }
                self.membership(Membership {
                    span: call.span,
                    container: receiver.span,
                    container_ty: receiver_ty.clone(),
                    needle: args[0].span,
                    needle_ty: arg_types[0].clone(),
                    name: "contains",
                });
                Ty::Bool
            }
            "get" => {
                let key = self.fresh();
                let value = self.fresh();
                let map = Ty::Map(Box::new(key.clone()), Box::new(value.clone()));
                self.expect(&map, &receiver_ty, receiver.span);
                if args.len() == 1 {
                    self.expect(&key, &arg_types[0], args[0].span);
                } else {
                    self.arity_error(call.span, name, "1", args.len());
                }
                Ty::Enum(OPTION.to_string(), vec![value])
            }
            "append" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                    return Ty::Unit;
                }
                self.add_element(receiver, &receiver_ty, &args[0], &arg_types[0]);
                Ty::Unit
            }
            "pop" if matches!(self.shallow(&receiver_ty), Ty::Map(..)) => {
                let Ty::Map(key, value) = self.shallow(&receiver_ty) else {
                    unreachable!("the receiver is a map")
                };
                match (args, &arg_types[..]) {
                    ([arg], [ty]) => self.expect(&key, ty, arg.span),
                    _ => self.arity_error(call.span, name, "1", args.len()),
                }
                Ty::Enum(OPTION.to_string(), vec![*value])
            }
            "keys" | "values" | "items" => {
                let key = self.fresh();
                let value = self.fresh();
                let map = Ty::Map(Box::new(key.clone()), Box::new(value.clone()));
                self.expect(&map, &receiver_ty, receiver.span);
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                let elem = match name {
                    "keys" => key,
                    "values" => value,
                    _ => Ty::Tuple(vec![key, value]),
                };
                Ty::List(Box::new(elem))
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
                // without an index, an empty list gives none
                match args {
                    [] => Ty::Enum(OPTION.to_string(), vec![elem]),
                    _ => elem,
                }
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
            "cmp" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                } else {
                    self.expect(&receiver_ty, &arg_types[0], args[0].span);
                }
                self.require_ordered(
                    receiver_ty.clone(),
                    receiver.span,
                    "'cmp' needs int, float, str, or a type that implements 'ord', found {}",
                );
                Ty::Enum(ORDERING.to_string(), vec![])
            }
            "display" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                Ty::Str
            }
            "eq" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                } else {
                    self.expect(&receiver_ty, &arg_types[0], args[0].span);
                }
                self.equalities
                    .push((receiver_ty.clone(), call.span, "'==' and '!='"));
                Ty::Bool
            }
            "sort" => {
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                let elem = self.fresh();
                self.expect(
                    &Ty::List(Box::new(elem.clone())),
                    &receiver_ty,
                    receiver.span,
                );
                self.require_ordered(
                    elem,
                    receiver.span,
                    "'sort' needs a list of int, float, str, or a type that implements 'ord', \
                     found a list of {}",
                );
                Ty::Unit
            }
            "unwrap" | "unwrap_or" | "is_ok" | "is_err" | "unwrap_err" | "ok" | "err"
                if matches!(self.shallow(&receiver_ty), Ty::Enum(ref enum_name, _) if enum_name == RESULT)
                    || !matches!(name, "unwrap" | "unwrap_or") =>
            {
                let value = self.fresh();
                let error = self.fresh();
                let result = Ty::Enum(RESULT.to_string(), vec![value.clone(), error.clone()]);
                if !self.unify(&result, &receiver_ty) {
                    let message =
                        format!("'{name}' needs a result, found {}", self.show(&receiver_ty));
                    self.error(receiver.span, message);
                }
                match (name, args) {
                    ("unwrap_or", [default]) => self.expect(&value, &arg_types[0], default.span),
                    ("unwrap_or", _) => self.arity_error(call.span, name, "1", args.len()),
                    (_, []) => {}
                    _ => self.arity_error(call.span, name, "0", args.len()),
                }
                match name {
                    "is_ok" | "is_err" => Ty::Bool,
                    "unwrap_err" => error,
                    "ok" => Ty::Enum(OPTION.to_string(), vec![value]),
                    "err" => Ty::Enum(OPTION.to_string(), vec![error]),
                    _ => value,
                }
            }
            "parse_int" | "parse_float" => {
                let message = match name {
                    "parse_int" => "'parse_int' needs a str, found {}",
                    _ => "'parse_float' needs a str, found {}",
                };
                self.require(receiver_ty, &[Kind::Str], receiver.span, message);
                if !args.is_empty() {
                    self.arity_error(call.span, name, "0", args.len());
                }
                let value = if name == "parse_int" {
                    Ty::Int
                } else {
                    Ty::Float
                };
                Ty::Enum(RESULT.to_string(), vec![value, Ty::Str])
            }
            "is_some" | "is_none" | "unwrap" | "unwrap_or" => {
                let elem = self.fresh();
                let option = Ty::Enum(OPTION.to_string(), vec![elem.clone()]);
                if !self.unify(&option, &receiver_ty) {
                    let message = match name {
                        "unwrap" | "unwrap_or" => format!(
                            "'{name}' needs an option or a result, found {}",
                            self.show(&receiver_ty)
                        ),
                        _ => format!(
                            "'{name}' needs an option, found {}",
                            self.show(&receiver_ty)
                        ),
                    };
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
                    "find" => (
                        "'find' needs a str, found {}",
                        Ty::Str,
                        Ty::Enum(OPTION.to_string(), vec![Ty::Int]),
                    ),
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
                // a struct's methods are found by `infer_struct_method`
                let message = format!("there is no method '{name}'");
                self.error(Span::new(start, end).in_file(func.span.file), message);
                self.fresh()
            }
        }
    }

    fn infer_builtin(&mut self, call: &Expr, name: &str, args: &[Expr], arg_types: &[Ty]) -> Ty {
        match name {
            "print" => Ty::Unit,
            "int" | "float" | "str" => {
                if args.len() != 1 {
                    self.arity_error(call.span, name, "1", args.len());
                } else {
                    // `str` writes any value as `print` does
                    let message = match name {
                        "int" => Some("'int' needs an int, float, or str, found {}"),
                        "float" => Some("'float' needs an int, float, or str, found {}"),
                        _ => None,
                    };
                    if let Some(message) = message {
                        self.require(arg_types[0].clone(), CONVERTIBLE, args[0].span, message);
                    }
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
                    "os.env" => Ty::Enum(OPTION.to_string(), vec![Ty::Str]),
                    "os.has_env" => Ty::Bool,
                    _ => Ty::Unit,
                }
            }
            "os.run" => {
                match args {
                    [cmd, list] => {
                        self.expect(&Ty::Str, &arg_types[0], cmd.span);
                        self.expect(&Ty::List(Box::new(Ty::Str)), &arg_types[1], list.span);
                    }
                    _ => self.arity_error(call.span, name, "2", args.len()),
                }
                Ty::Enum(
                    RESULT.to_string(),
                    vec![Ty::Struct(OUTPUT.to_string(), vec![]), Ty::Str],
                )
            }
            "fs.read" | "fs.read_lines" | "fs.list_dir" | "fs.exists" | "fs.remove"
            | "fs.make_dir" | "fs.write" | "fs.append" => {
                let params = match name {
                    "fs.write" | "fs.append" => 2,
                    _ => 1,
                };
                if args.len() == params {
                    for (arg, ty) in args.iter().zip(arg_types) {
                        self.expect(&Ty::Str, ty, arg.span);
                    }
                } else {
                    self.arity_error(call.span, name, &params.to_string(), args.len());
                }
                let value = match name {
                    "fs.exists" => return Ty::Bool,
                    "fs.read_lines" | "fs.list_dir" => Ty::List(Box::new(Ty::Str)),
                    "fs.write" | "fs.append" => Ty::Int,
                    _ => Ty::Str,
                };
                Ty::Enum(RESULT.to_string(), vec![value, Ty::Str])
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
    /// Checks that `==` and `!=` can compare values of type `ty`: numbers, bools, strings, and
    /// tuples, structs, and enums of them, but no list or map, even inside another value, since
    /// compiled code would compare them by address.
    ///
    /// For example, `(1, "a") == t` is fine, while `(1, [2]) == t` is an error naming `list[int]`.
    fn check_equality(&mut self, ty: &Ty, span: Span, what: &str) {
        let ty = self.shallow(ty);
        match ty {
            // never pinned down, so take the default
            Ty::Var(_) => {
                self.unify(&ty, &Ty::Int);
            }
            Ty::List(_) | Ty::Map(..) | Ty::Unit => {
                let message = format!("{what} cannot compare {}", self.show(&ty));
                self.error(span, message);
            }
            Ty::Param(p) if self.type_params[p].bounds.iter().any(|b| b == EQ) => {}
            Ty::Param(_) => {
                let name = self.show(&ty);
                let message = format!(
                    "{what} cannot compare {name}, since '{name}' may be any type; to compare \
                     its values, bound it by 'eq', as in '[{name}: eq]'"
                );
                self.error(span, message);
            }
            _ => {
                let Some(found) = self.uncomparable(&ty, &mut HashSet::new()) else {
                    return;
                };
                let holder = match &found.field {
                    Some((owner, field)) => format!("field '{}.{field}'", struct_display(owner)),
                    None => "it".to_string(),
                };
                let message = format!(
                    "{what} cannot compare {}, since {holder} holds {}",
                    self.show(&ty),
                    self.show(&found.ty)
                );
                self.error(span, message);
            }
        }
    }

    /// Finds a list or map inside `ty`, looking into tuples, the fields of structs, and the values
    /// of every variant of enums, each enum and struct once, by its name in `seen`.
    fn uncomparable(&self, ty: &Ty, seen: &mut HashSet<String>) -> Option<Uncomparable> {
        match self.shallow(ty) {
            Ty::Param(p) if self.type_params[p].bounds.iter().any(|b| b == EQ) => None,
            // a struct that implements `eq` compares however it says
            Ty::Struct(name, _) if self.impls.contains_key(&(EQ.to_string(), name.clone())) => None,
            // a type parameter may be a list or a map
            ty @ (Ty::List(_) | Ty::Map(..) | Ty::Param(_)) => {
                Some(Uncomparable { ty, field: None })
            }
            Ty::Tuple(fields) => fields
                .iter()
                .find_map(|field| self.uncomparable(field, seen)),
            Ty::Enum(name, args) => {
                let info = self.enums.get(&name)?;
                if !seen.insert(self.show(&Ty::Enum(name.clone(), args.clone()))) {
                    return None;
                }
                info.variants
                    .iter()
                    .flat_map(|variant| &variant.fields)
                    .find_map(|field| {
                        let field = Self::substitute(field, &info.type_params, &args);
                        self.uncomparable(&field, seen)
                    })
            }
            Ty::Struct(name, args) => {
                let info = self.structs.get(&name)?;
                if !seen.insert(self.show(&Ty::Struct(name.clone(), args.clone()))) {
                    return None;
                }
                info.fields.iter().find_map(|field| {
                    let ty = Self::substitute(&field.ty, &info.type_params, &args);
                    let mut found = self.uncomparable(&ty, seen)?;
                    found
                        .field
                        .get_or_insert_with(|| (name.clone(), field.name.clone()));
                    Some(found)
                })
            }
            _ => None,
        }
    }

    /// Returns the type parameters that appear in `ty`, in order, each once.
    ///
    /// For example, `tuple[A, list[B], A]` holds `A` and `B`.
    fn params_in(&self, ty: &Ty, found: &mut Vec<usize>) {
        match self.shallow(ty) {
            Ty::Param(p) if !found.contains(&p) => found.push(p),
            Ty::List(elem) => self.params_in(&elem, found),
            Ty::Map(key, value) => {
                self.params_in(&key, found);
                self.params_in(&value, found);
            }
            Ty::Tuple(fields) | Ty::Enum(_, fields) => {
                fields.iter().for_each(|f| self.params_in(f, found))
            }
            _ => {}
        }
    }

    /// Checks every call of a generic function: that no call leads back to itself with ever
    /// larger types, which would need infinitely many copies of the function, as `f([x])` does
    /// inside `fn f[T](x: T):`, and that no list element or map entry holds too many values
    /// once the type parameters are filled in, which only the types a call gives can tell.
    fn check_instances(&mut self) {
        let instances: Vec<(Span, Option<String>, String, Vec<Ty>)> = self
            .instances
            .iter()
            .map(|i| {
                let args = i.args.iter().map(|a| self.resolve(a)).collect();
                (i.span, i.caller.clone(), i.function.clone(), args)
            })
            .collect();
        if instances.is_empty() {
            return;
        }
        self.check_bounds(&instances);

        // each type parameter is given to the ones its calls fill in, growing if nested
        let mut edges: Vec<(usize, usize, bool, usize)> = vec![];
        for (i, (_, _, function, args)) in instances.iter().enumerate() {
            let params = self.functions[function].type_params.clone();
            for (&q, arg) in params.iter().zip(args) {
                let mut found = vec![];
                self.params_in(arg, &mut found);
                for p in found {
                    edges.push((p, q, *arg != Ty::Param(p), i));
                }
            }
        }
        let reaches = |from: usize, to: usize| {
            let mut seen = HashSet::from([from]);
            let mut stack = vec![from];
            while let Some(p) = stack.pop() {
                if p == to {
                    return true;
                }
                for &(a, b, _, _) in &edges {
                    if a == p && seen.insert(b) {
                        stack.push(b);
                    }
                }
            }
            false
        };
        let mut growing = vec![];
        for &(p, q, grows, i) in &edges {
            if grows && reaches(q, p) && !growing.contains(&i) {
                growing.push(i);
            }
        }
        for &i in &growing {
            let (span, _, function, args) = &instances[i];
            let params = self.functions[function].type_params.clone();
            let (param, arg) = params
                .iter()
                .zip(args)
                .find(|(_, arg)| !matches!(arg, Ty::Param(_)))
                .map(|(&q, arg)| (self.type_params[q].name.clone(), self.show(arg)))
                .unwrap_or_default();
            let short = function.rsplit('.').next().unwrap_or(function);
            self.error(
                *span,
                format!(
                    "this call gives '{short}' {arg} for '{param}' and leads back to itself, so \
                     '{short}' would need a copy for ever larger types"
                ),
            );
        }
        if !growing.is_empty() {
            return;
        }

        // the list elements, map values, and enum values each generic function's code makes
        let mut held: HashMap<String, Vec<(Ty, Held)>> = HashMap::new();
        let spans: Vec<(String, Span)> = self
            .functions
            .iter()
            .filter(|(_, info)| !info.type_params.is_empty())
            .filter_map(|(name, _)| Some((name.clone(), *self.function_spans.get(name)?)))
            .collect();
        for (span, ty) in &self.expr_types {
            let Some((function, _)) = spans.iter().find(|(_, f)| f.encloses(*span)) else {
                continue;
            };
            let mut found = vec![];
            self.stored(&self.resolve(ty), &mut found, &mut HashSet::new());
            found.retain(|(ty, _)| {
                let mut params = vec![];
                self.params_in(ty, &mut params);
                !params.is_empty()
            });
            held.entry(function.clone()).or_default().extend(found);
        }

        // fill them in from each call outside a generic function's own type parameters
        let mut seen = HashSet::new();
        let mut reported = HashSet::new();
        let mut pending: Vec<(String, Vec<Ty>, Span)> = instances
            .iter()
            .filter(|(_, _, _, args)| {
                let mut found = vec![];
                args.iter().for_each(|a| self.params_in(a, &mut found));
                found.is_empty()
            })
            .map(|(span, _, function, args)| (function.clone(), args.clone(), *span))
            .collect();
        while let Some((function, args, outer)) = pending.pop() {
            // a trait's method is the type's own for a known type
            let (function, args) = self
                .implementation_of(&function, &args)
                .unwrap_or((function, args));
            if !seen.insert((function.clone(), self.show(&Ty::Tuple(args.clone())))) {
                continue;
            }
            let params = self.functions[&function].type_params.clone();
            for (ty, kind) in held.get(&function).cloned().unwrap_or_default() {
                let ty = Self::substitute(&ty, &params, &args);
                let (what, count) = self.held_width(&ty, kind);
                if count > MAX_ELEMENT_VALUES && reported.insert(outer) {
                    let short = function.rsplit('.').next().unwrap_or(&function);
                    self.error(
                        outer,
                        format!(
                            "this call makes '{short}' store {what}, which holds {count} \
                             values, but at most {MAX_ELEMENT_VALUES} fit"
                        ),
                    );
                }
            }
            for (_, caller, callee, inner) in &instances {
                if caller.as_deref() == Some(function.as_str()) {
                    let inner = inner
                        .iter()
                        .map(|a| Self::substitute(a, &params, &args))
                        .collect();
                    pending.push((callee.clone(), inner, outer));
                }
            }
        }
    }

    /// Reports each call of a generic function whose type arguments do not implement the
    /// traits that bound its type parameters, as `total([1])` does for `fn total[T: Shape]`
    /// unless `int` implements `Shape`. Inside another generic function, a type argument that
    /// is its own type parameter must have the bound itself.
    fn check_bounds(&mut self, instances: &[(Span, Option<String>, String, Vec<Ty>)]) {
        for (span, _, function, args) in instances {
            let params = self.functions[function].type_params.clone();
            for (&q, arg) in params.iter().zip(args) {
                for bound in self.type_params[q].bounds.clone() {
                    if self.satisfies(arg, &bound) {
                        continue;
                    }
                    let short = function.rsplit('.').next().unwrap_or(function);
                    let message = format!(
                        "'{}' does not implement '{}', which '{short}' needs for '{}'",
                        self.show(arg),
                        struct_display(&bound),
                        self.type_params[q].name
                    );
                    self.error(*span, message);
                }
            }
        }
    }

    /// Returns whether values of type `ty` have the methods of `trait_`: a type parameter if
    /// it is bounded by the trait, and any other type if it implements it.
    fn satisfies(&self, ty: &Ty, trait_: &str) -> bool {
        match self.shallow(ty) {
            // every value is written as text
            ty if trait_ == DISPLAY => !matches!(ty, Ty::Unit),
            Ty::Param(p) => self.type_params[p].bounds.iter().any(|b| b == trait_),
            // every type `==` compares part by part
            ty if trait_ == EQ => {
                !matches!(ty, Ty::List(_) | Ty::Map(..) | Ty::Unit)
                    && self.uncomparable(&ty, &mut HashSet::new()).is_none()
            }
            Ty::Int | Ty::Float | Ty::Str if trait_ == ORD => true,
            ty => {
                ty_key(&ty).is_some_and(|key| self.impls.contains_key(&(trait_.to_string(), key)))
            }
        }
    }

    /// Returns the function that the trait method `function` is for a known type `args[0]`,
    /// as `Circle.area` is for `Shape.area`, with the type arguments of its copy, or `None` if
    /// `function` is not a trait's method or the type is not known.
    fn implementation_of(&self, function: &str, args: &[Ty]) -> Option<(String, Vec<Ty>)> {
        let (trait_, name) = function.rsplit_once('.')?;
        if !self.traits.contains_key(trait_) {
            return None;
        }
        let [this] = args else {
            return None;
        };
        let key = ty_key(this)?;
        let implemented = self.impls.get(&(trait_.to_string(), key))?.get(name)?;
        if implemented
            .rsplit_once('.')
            .is_some_and(|(owner, _)| owner == trait_)
        {
            return Some((implemented.clone(), args.to_vec()));
        }
        let type_args = match this {
            Ty::Struct(_, type_args) => type_args.clone(),
            _ => vec![],
        };
        Some((implemented.clone(), type_args))
    }

    /// Lists the list elements, map values, and values of generic enums that a value of type
    /// `ty` holds, looking into tuples, structs, and enums, each struct and enum type once.
    ///
    /// For example, a `tuple[list[int], Box[str]]` holds list elements of `int` and the values
    /// of `Box[str]`.
    fn stored(&self, ty: &Ty, out: &mut Vec<(Ty, Held)>, seen: &mut HashSet<String>) {
        match self.shallow(ty) {
            Ty::List(elem) => {
                out.push(((*elem).clone(), Held::Element));
                self.stored(&elem, out, seen);
            }
            Ty::Map(key, value) => {
                out.push(((*value).clone(), Held::Entry));
                self.stored(&key, out, seen);
                self.stored(&value, out, seen);
            }
            Ty::Tuple(fields) => fields.iter().for_each(|f| self.stored(f, out, seen)),
            ty @ Ty::Enum(..) => {
                if !seen.insert(self.show(&ty)) {
                    return;
                }
                out.push((ty.clone(), Held::Variants));
                for (_, fields) in self.ty_variants(&ty).unwrap_or_default() {
                    fields.iter().for_each(|f| self.stored(f, out, seen));
                }
            }
            Ty::Struct(name, args) => {
                if !seen.insert(self.show(&Ty::Struct(name.clone(), args.clone()))) {
                    return;
                }
                for field in self.field_types(&name, &args) {
                    self.stored(&field, out, seen);
                }
            }
            _ => {}
        }
    }

    /// Describes what a value of type `ty`, held as `kind`, stores and how many values that is,
    /// counting each value of tuples and structs: one element of a list, a map's key and value,
    /// or the widest variant of an enum.
    fn held_width(&self, ty: &Ty, kind: Held) -> (String, usize) {
        match kind {
            Held::Element => (
                format!("a list element of {}", self.show(ty)),
                self.values(ty),
            ),
            Held::Entry => (
                format!("a map entry of a key and {}", self.show(ty)),
                1 + self.values(ty),
            ),
            Held::Variants => {
                let widest = self
                    .ty_variants(ty)
                    .unwrap_or_default()
                    .into_iter()
                    .map(|(variant, fields)| {
                        let count: usize = fields.iter().map(|f| self.values(f)).sum();
                        (variant, count)
                    })
                    .max_by_key(|(_, count)| *count);
                let Some((variant, count)) = widest else {
                    return (String::new(), 0);
                };
                (
                    format!("a value of {} variant '{variant}'", self.show(ty)),
                    count,
                )
            }
        }
    }

    /// Reports a value of a generic enum, made outside generic functions, whose variant holds
    /// more values than an enum value can once its type arguments are filled in.
    ///
    /// For example, with `enum Box[T]:` holding `Full(T)`, `Box.Full(t)` for a tuple `t` of 40
    /// values is too wide, though `Full(T)` alone is not.
    fn check_variant_widths(&mut self) {
        let generic: Vec<Span> = self
            .functions
            .iter()
            .filter(|(_, info)| !info.type_params.is_empty())
            .filter_map(|(name, _)| self.function_spans.get(name).copied())
            .collect();
        let mut seen = HashSet::new();
        let mut errors = vec![];
        for (span, ty) in &self.expr_types {
            if generic.iter().any(|f| f.encloses(*span)) {
                continue;
            }
            let mut found = vec![];
            self.stored(&self.resolve(ty), &mut found, &mut HashSet::new());
            for (ty, kind) in found {
                if kind != Held::Variants || !seen.insert(self.show(&ty)) {
                    continue;
                }
                let (what, count) = self.held_width(&ty, kind);
                if count > MAX_ELEMENT_VALUES {
                    errors.push((
                        *span,
                        format!(
                            "this makes {what}, which holds {count} values, but at most \
                             {MAX_ELEMENT_VALUES} fit"
                        ),
                    ));
                }
            }
        }
        errors.sort_by_key(|(span, _)| (span.file, span.start));
        for (span, message) in errors {
            self.error(span, message);
        }
    }

    /// Reports a global whose type holds a type parameter, which a generic function gave it by
    /// reading it, since outside that function the type parameter means nothing.
    ///
    /// For example, after `g = []`, `fn f[T]() -> T: ret g[0]` makes `g` a `list[T]`.
    fn check_captured_params(&mut self) {
        let mut globals: Vec<(&String, &usize)> = self.globals.iter().collect();
        globals.sort_by_key(|(_, symbol)| **symbol);
        let mut errors = vec![];
        for (name, &symbol) in globals {
            let mut found = vec![];
            self.params_in(&self.symbol_types[symbol], &mut found);
            if let Some(&p) = found.first() {
                let param = &self.type_params[p];
                let function = self.symbols[param.symbol].scope.clone().unwrap_or_default();
                errors.push((
                    self.symbols[symbol].span,
                    format!(
                        "'{name}' holds a value of type {}, the type parameter of '{function}', \
                         which means nothing outside '{function}'",
                        param.name
                    ),
                ));
            }
        }
        for (span, message) in errors {
            self.error(span, message);
        }
    }

    fn finish(mut self) -> Analysis {
        for deferred in std::mem::take(&mut self.deferred_indexes) {
            self.settle_index(deferred);
        }
        for deferred in std::mem::take(&mut self.deferred_fields) {
            self.settle_field(deferred);
        }
        // a method's receiver may be a deferred field, as in `g.inner.get()`
        for deferred in std::mem::take(&mut self.deferred_methods) {
            self.settle_method(deferred);
        }
        // and so may a container, as in `k in g.inner`
        for deferred in std::mem::take(&mut self.deferred_memberships) {
            self.settle_membership(deferred);
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
        for (span, map) in std::mem::take(&mut self.map_literals) {
            if let Ty::Map(_, value) = self.resolve(&map) {
                self.check_entry_width(&value, span);
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
                    // no constraint allows a map, tuple, enum, or struct first, and nothing tells
                    // which
                    Kind::Map | Kind::Tuple | Kind::Enum | Kind::Struct | Kind::Param => continue,
                };
                self.unify(&ty, &default);
                continue;
            };
            if !constraint.allowed.contains(&kind) && !(constraint.ord && self.satisfies(&ty, ORD))
            {
                let message = constraint.message.replace("{}", &self.show(&ty));
                self.error(constraint.span, message);
            }
        }
        for (ty, span, what) in std::mem::take(&mut self.equalities) {
            self.check_equality(&ty, span, what);
        }
        self.check_instances();
        self.check_variant_widths();
        self.check_captured_params();
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
                SymbolKind::Function | SymbolKind::Method => {
                    let info = match (&symbol.kind, &symbol.scope) {
                        (SymbolKind::Method, Some(owner)) => {
                            &self.functions[&format!("{owner}.{}", symbol.name)]
                        }
                        _ => &self.functions[&symbol.name],
                    };
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
                        ret: Box::new(self.finalize(&self.own_type(&enum_name))),
                    }
                }
                SymbolKind::Field => {
                    let struct_name = symbol.scope.clone().unwrap_or_default();
                    self.structs[&struct_name]
                        .fields
                        .iter()
                        .find(|f| f.symbol == i)
                        .map_or(Type::Int, |f| self.finalize(&f.ty))
                }
                _ => self.finalize(&self.symbol_types[i]),
            };
            analysis.symbols.push(Symbol {
                ty,
                ..symbol.clone()
            });
        }
        let names = |params: &[usize]| -> Vec<String> {
            params
                .iter()
                .map(|&p| self.type_params[p].name.clone())
                .collect()
        };
        for (name, info) in &self.enums {
            let variants = info
                .variants
                .iter()
                .map(|v| {
                    let fields = v.fields.iter().map(|f| self.finalize(f)).collect();
                    (v.name.clone(), fields)
                })
                .collect();
            let params = names(&info.type_params);
            if !params.is_empty() {
                analysis.type_params.insert(name.clone(), params.clone());
            }
            analysis
                .enums
                .insert(name.clone(), EnumDef { params, variants });
        }
        for (name, info) in &self.structs {
            let fields = info
                .fields
                .iter()
                .map(|f| (f.name.clone(), self.finalize(&f.ty)))
                .collect();
            let params = names(&info.type_params);
            if !params.is_empty() {
                analysis.type_params.insert(name.clone(), params.clone());
            }
            analysis
                .structs
                .insert(name.clone(), StructDef { params, fields });
        }
        for instance in std::mem::take(&mut self.instances) {
            let args = instance.args.iter().map(|a| self.finalize(a)).collect();
            analysis.instances.insert(
                instance.span,
                Instance {
                    function: instance.function,
                    args,
                },
            );
        }
        for (trait_, owner) in self.impls.keys() {
            analysis
                .implemented
                .entry(owner.clone())
                .or_default()
                .push(trait_.clone());
        }
        analysis
            .implemented
            .values_mut()
            .for_each(|traits| traits.sort());
        for info in &self.type_params {
            if !info.bounds.is_empty() && self.symbols[info.symbol].kind == SymbolKind::TypeParam {
                analysis.bounds.insert(info.symbol, info.bounds.clone());
            }
        }
        for (name, info) in &self.functions {
            if !info.type_params.is_empty() {
                let names = info
                    .type_params
                    .iter()
                    .map(|&p| self.type_params[p].name.clone())
                    .collect();
                analysis.type_params.insert(name.clone(), names);
            }
        }
        analysis.references = self.references;
        analysis
            .references
            .sort_by_key(|r| (r.span.file, r.span.start));
        // a written type can be read more than once
        analysis.references.dedup();
        analysis.functions = self.function_spans;
        analysis.methods = self.methods;
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
            Ty::Map(key, value) => Type::Map(
                Box::new(self.finalize(&key)),
                Box::new(self.finalize(&value)),
            ),
            Ty::Tuple(fields) => Type::Tuple(fields.iter().map(|f| self.finalize(f)).collect()),
            Ty::Enum(name, args) => Type::Enum {
                name,
                args: args.iter().map(|a| self.finalize(a)).collect(),
            },
            Ty::Struct(name, args) => Type::Struct {
                name,
                args: args.iter().map(|a| self.finalize(a)).collect(),
            },
            Ty::Param(p) => Type::Param(self.type_params[p].name.clone()),
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
                format!("'match' does not cover {missed}; add 'case {missed}:' or 'case _:'"),
            );
        }
    }

    /// Returns the variants of the enum type `ty`, each with the types of its values, or `None` if
    /// `ty` is not an enum.
    fn ty_variants(&self, ty: &Ty) -> Option<Vec<(String, Vec<Ty>)>> {
        match self.resolve(ty) {
            Ty::Enum(name, args) => self.enums.get(&name).map(|info| {
                info.variants
                    .iter()
                    .map(|v| {
                        let fields = v
                            .fields
                            .iter()
                            .map(|f| Self::substitute(f, &info.type_params, &args))
                            .collect();
                        (v.name.clone(), fields)
                    })
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
            Ty::Enum(enum_name, _) if !prints_bare(&enum_name) => format!("{enum_name}.{name}"),
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
/// For example, after `if c: x = 1`, reading `x` is an error, but after `if c: x = 1` followed by
/// `else: x = 2` it is fine.
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
            StmtKind::FunctionDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. } => Some(assigned),
        }
    }

    /// Reports every tracked variable `expr` reads that is not assigned yet.
    fn reads(&mut self, expr: &Expr, assigned: &HashSet<String>) {
        match &expr.kind {
            ExprKind::Name { .. } if expr.is_unwritten() => {}
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
            ExprKind::BoolOp { values, .. } | ExprKind::JoinedStr { values } => {
                for value in values {
                    self.reads(value, assigned);
                }
            }
            ExprKind::BinOp { left, right, .. } => {
                self.reads(left, assigned);
                self.reads(right, assigned);
            }
            ExprKind::UnaryOp { operand, .. } | ExprKind::Try { value: operand } => {
                self.reads(operand, assigned)
            }
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.reads(left, assigned);
                for comparator in comparators {
                    self.reads(comparator, assigned);
                }
            }
            ExprKind::Call {
                func,
                args,
                keywords,
            } => {
                // a called name is a function, but a method's receiver is read
                if let ExprKind::Attribute { .. } = &func.kind {
                    self.reads(func, assigned);
                }
                for arg in args {
                    self.reads(arg, assigned);
                }
                for keyword in keywords {
                    self.reads(&keyword.value, assigned);
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
            ExprKind::Dict { keys, values } => {
                for (key, value) in keys.iter().zip(values) {
                    self.reads(key, assigned);
                    self.reads(value, assigned);
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

/// Returns the arguments of `iter` if it is a call to `range`, as in `for i in range(3):`.
///
/// Both backends count through a `range` directly rather than building a list, so it only has
/// meaning as a `for` loop's iterable.
pub fn range_args(iter: &Expr) -> Option<&[Expr]> {
    match &iter.kind {
        ExprKind::Call { func, args, .. } => match &func.kind {
            ExprKind::Name { id, .. } if id == "range" => Some(args),
            _ => None,
        },
        _ => None,
    }
}

/// Collects every name assigned in `body`, with the span of its first assignment, without
/// looking inside function definitions.
///
/// For example, for `x = 1` followed by `if x: y = x`, this collects `x` and then `y`.
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
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Return { .. }
            | StmtKind::Expr { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }
}

/// What the checker reports for a change to a slice, such as `xs[1:].append(2)`.
const SLICE_CHANGE: &str = "a slice is a new list, so changing it has no effect";
const STRING_CHANGE: &str =
    "a string cannot be changed; build a new one, as in 's = s[:i] + c + s[i + 1:]'";

const TUPLE_CHANGE: &str = "a tuple cannot be changed; build a new one, as in 't = (v, t[1])'";
/// The error for a `for` loop that unpacks a map's elements, which are its keys.
const MAP_PAIRS: &str = "a map gives its keys; iterate 'm.items()' for its keys and values";
/// The kinds a map's keys may be, which hash and compare exactly.
const MAP_KEYS: &[Kind] = &[Kind::Int, Kind::Str, Kind::Bool];

impl Indexed {
    /// Returns the error for changing an element of what was indexed, if it cannot be changed.
    fn change_error(self) -> Option<&'static str> {
        match self {
            Indexed::List => None,
            Indexed::Str => Some(STRING_CHANGE),
            Indexed::Tuple => Some(TUPLE_CHANGE),
            Indexed::Map => None,
        }
    }
}

/// Returns whether the value `expr`, which a statement changes, is a slice or inside one.
///
/// For example, `xs[1:]` and `grid[:2][0]` are, but `grid[0]` is not.
fn has_slice(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::Subscript { value, slice, .. } => {
            matches!(slice.kind, ExprKind::Slice { .. }) || has_slice(value)
        }
        ExprKind::Attribute { value, .. } => has_slice(value),
        _ => false,
    }
}

/// Returns the span of the variable that `receiver`, a place a call changes, starts from, and
/// that variable followed by the fields after it up to the first index, which is what the call
/// may change.
///
/// For example, `self.items[0]` gives `self`'s span and `["self", "items"]`, and `f().x` gives
/// `None`.
fn changed_path(receiver: &Expr) -> Option<(Span, Vec<String>)> {
    let (root, steps) = place(receiver)?;
    Some((place_root(receiver).span, field_path(root, &steps)))
}

/// Returns `root` followed by the fields at the start of `steps`, up to the first index.
fn field_path(root: &str, steps: &[Step]) -> Vec<String> {
    let fields = steps.iter().map_while(|step| match step {
        Step::Field(field) => Some(field.to_string()),
        Step::Index(_) => None,
    });
    [root.to_string()].into_iter().chain(fields).collect()
}

/// Pushes each variable `expr` uses, in order, as the span of the variable and the path
/// [`field_path`] gives for the place it is read through, leaving out a method's name and a
/// called function's.
///
/// For example, `f(p.x, xs[i].len())` gives `p` with `["p", "x"]`, `xs` with `["xs"]`, and `i`
/// with `["i"]`.
fn place_mentions(expr: &Expr, out: &mut Vec<(Span, Vec<String>)>) {
    match &expr.kind {
        ExprKind::Name { .. } | ExprKind::Subscript { .. } | ExprKind::Attribute { .. }
            if let Some((root, steps)) = place(expr) =>
        {
            out.push((place_root(expr).span, field_path(root, &steps)));
            for step in steps {
                if let Step::Index(index) = step {
                    place_mentions(index, out);
                }
            }
        }
        ExprKind::Call {
            func,
            args,
            keywords,
        } => {
            match &func.kind {
                ExprKind::Name { .. } => {}
                ExprKind::Attribute { value, .. } => place_mentions(value, out),
                _ => place_mentions(func, out),
            }
            for arg in args
                .iter()
                .chain(keywords.iter().map(|keyword| &keyword.value))
            {
                place_mentions(arg, out);
            }
        }
        _ => {
            for child in expr.children() {
                place_mentions(child, out);
            }
        }
    }
}

/// Returns the first of `mentions`, from [`place_mentions`], that overlaps `changed`, a path
/// that [`changed_path`] gave, because one path starts with the other, leaving out the variables
/// at `exempt`, those other calls change.
///
/// For example, a change to `self.items` overlaps `self` and `self.items.len()`, but not
/// `self.offset`.
fn first_conflict(
    mentions: &[(Span, Vec<String>)],
    changed: &[String],
    exempt: &HashSet<Span>,
) -> Option<Conflict> {
    let (span, used) = mentions.iter().find(|(span, used)| {
        used[0] == changed[0]
            && !exempt.contains(span)
            && used.iter().zip(changed).all(|(a, b)| a == b)
    })?;
    Some(Conflict {
        changed: changed.join("."),
        used: used.join("."),
        span: *span,
    })
}

/// Returns the error for `conflict`, another use of what a call of the method `name` changes.
fn conflict_message(name: &str, conflict: &Conflict) -> String {
    format!(
        "'{name}' changes '{}' in this statement, so '{}' cannot also be used in it; assign the \
         result of '{name}' to a variable first",
        conflict.changed, conflict.used
    )
}

/// Returns the expression the place `expr` starts from, following its subscripts and fields.
///
/// For example, the root of `grid[i][j]` and of `ps[i].x` is `grid` or `ps`, and the root of
/// `f()[0]` is `f()`.
pub(crate) fn place_root(expr: &Expr) -> &Expr {
    match &expr.kind {
        ExprKind::Subscript { value, .. } | ExprKind::Attribute { value, .. } => place_root(value),
        _ => expr,
    }
}

/// One step from a variable toward the value a statement changes: an index into a list, or a
/// field of a struct.
#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Step<'a> {
    Index(&'a Expr),
    Field(&'a str),
}

/// Splits a value that a statement changes into the variable it starts from and the steps that
/// lead from that variable to it, outermost first, or returns `None` if it is not in a variable.
///
/// For example, `grid[i][j]` gives `grid` and the indexes `i` and `j`, `ps[i].x` gives `ps`, the
/// index `i`, and the field `x`, `xs` gives `xs` and no steps, and `f()[0]` gives `None`.
pub(crate) fn place(expr: &Expr) -> Option<(&str, Vec<Step<'_>>)> {
    match &expr.kind {
        ExprKind::Name { id, .. } => Some((id, vec![])),
        ExprKind::Subscript { value, slice, .. } => {
            let (root, mut steps) = place(value)?;
            steps.push(Step::Index(slice));
            Some((root, steps))
        }
        ExprKind::Attribute { value, attr, .. } => {
            let (root, mut steps) = place(value)?;
            steps.push(Step::Field(attr));
            Some((root, steps))
        }
        _ => None,
    }
}

/// Returns the span of each step in `place`, a variable or a value inside one, outermost first,
/// matching the steps [`place`] returns: a subscript's or a field's whole expression. A runtime
/// error for an index out of range points at its subscript.
///
/// For example, `grid[i][j]` gives the spans of `grid[i]` and `grid[i][j]`.
pub(crate) fn place_spans(place: &Expr) -> Vec<Span> {
    match &place.kind {
        ExprKind::Subscript { value, .. } | ExprKind::Attribute { value, .. } => {
            let mut spans = place_spans(value);
            spans.push(place.span);
            spans
        }
        _ => vec![],
    }
}

/// Returns the receiver of `expr` if it calls a method that changes its receiver: a builtin one,
/// or one of a struct's that changes `self`, as `methods` resolved it.
///
/// For example, for `grid[0].append(1)` this returns `grid[0]`, and for `xs.len()` it returns
/// `None`.
pub(crate) fn changed_receiver<'a>(expr: &'a Expr, methods: &Methods) -> Option<&'a Expr> {
    let ExprKind::Call { func, .. } = &expr.kind else {
        return None;
    };
    let ExprKind::Attribute { value, attr, .. } = &func.kind else {
        return None;
    };
    let changes = match methods.get(&expr.span) {
        Some(method) => method.receiver && method.changes,
        None => changes_receiver(attr),
    };
    changes.then_some(value.as_ref())
}

/// Collects the name of every variable that `body` changes in place, through `xs[i] = v`,
/// `p.x = v`, `xs.append(v)`, or a method that changes `self`, as `methods` resolved it, wherever
/// the call is in a statement (`print(zs.pop())`), without looking inside function definitions.
///
/// For example, for `grid[0][1] = 2` followed by `if x: ys.append(1)`, this collects `grid` and
/// `ys`.
pub(crate) fn collect_changed(body: &[Stmt], out: &mut HashSet<String>, methods: &Methods) {
    let add = |list: &Expr, out: &mut HashSet<String>| {
        if let Some((root, _)) = place(list) {
            out.insert(root.to_string());
        }
    };
    for stmt in body {
        match &stmt.kind {
            StmtKind::Assign { targets, .. } => {
                for target in targets {
                    match &target.kind {
                        ExprKind::Subscript { value, .. } => add(value, out),
                        ExprKind::Attribute { .. } => add(target, out),
                        _ => {}
                    }
                }
            }
            StmtKind::AugAssign { target, .. } => match &target.kind {
                ExprKind::Subscript { value, .. } => add(value, out),
                ExprKind::Attribute { .. } => add(target, out),
                _ => {}
            },
            _ => {}
        }
        // a call that changes its receiver may be anywhere in the statement's own expressions
        for expr in stmt.exprs() {
            expr.visit(&mut |expr| {
                if let Some(receiver) = changed_receiver(expr, methods) {
                    add(receiver, out);
                }
            });
        }
        match &stmt.kind {
            StmtKind::For { body, .. } | StmtKind::While { body, .. } => {
                collect_changed(body, out, methods)
            }
            StmtKind::If { body, orelse, .. } => {
                collect_changed(body, out, methods);
                collect_changed(orelse, out, methods);
            }
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    collect_changed(&case.body, out, methods);
                }
            }
            _ => {}
        }
    }
}

/// Returns where `body` first changes the parameter `name` if it never reads it anywhere else,
/// since the caller cannot see the change and nothing else uses it.
///
/// For example, the change in `fn push(xs: list[int], x: int): xs.append(x)` is lost, but a
/// later `ret xs` or `print(xs)` uses it.
fn lost_change(body: &[Stmt], name: &str, methods: &Methods) -> Option<Span> {
    let mut usage = ParameterUse {
        name,
        methods,
        changed: None,
        read: false,
    };
    usage.block(body);
    if usage.read { None } else { usage.changed }
}

/// How a function body uses one of its parameters, for [`lost_change`].
struct ParameterUse<'a> {
    name: &'a str,
    methods: &'a Methods,
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
                self.expr(value);
                for target in targets {
                    match &target.kind {
                        ExprKind::Name { .. } | ExprKind::Tuple { .. } => {}
                        ExprKind::Subscript { value, slice, .. } => {
                            self.change(value);
                            self.expr(slice);
                        }
                        ExprKind::Attribute { .. } => self.change(target),
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
                    ExprKind::Attribute { .. } => self.change(target),
                    _ => self.expr(target),
                }
                self.expr(value);
            }
            StmtKind::Expr { value } => self.expr(value),
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
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    /// Notes a change to `list`, which changes the parameter if `list` starts from it.
    fn change(&mut self, list: &Expr) {
        match place(list) {
            Some((root, steps)) if root == self.name => {
                self.changed.get_or_insert(place_root(list).span);
                for step in steps {
                    if let Step::Index(index) = step {
                        self.expr(index);
                    }
                }
            }
            _ => self.expr(list),
        }
    }

    /// Notes whether `expr` reads the parameter, and any change a call in it makes, as in
    /// `xs.append(x)` or `print(xs.pop())`.
    fn expr(&mut self, expr: &Expr) {
        if let (Some(receiver), ExprKind::Call { args, .. }) =
            (changed_receiver(expr, self.methods), &expr.kind)
        {
            self.change(receiver);
            args.iter().for_each(|arg| self.expr(arg));
            return;
        }
        match &expr.kind {
            ExprKind::Name { id, .. } => self.read |= id == self.name && !expr.is_unwritten(),
            ExprKind::BoolOp { values, .. } | ExprKind::JoinedStr { values } => {
                values.iter().for_each(|value| self.expr(value))
            }
            ExprKind::BinOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::UnaryOp { operand, .. } | ExprKind::Try { value: operand } => {
                self.expr(operand)
            }
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.expr(left);
                comparators.iter().for_each(|value| self.expr(value));
            }
            ExprKind::Call {
                func,
                args,
                keywords,
            } => {
                // a called name is a function, but a method's receiver is read
                if let ExprKind::Attribute { .. } = &func.kind {
                    self.expr(func);
                }
                args.iter().for_each(|arg| self.expr(arg));
                keywords.iter().for_each(|k| self.expr(&k.value));
            }
            ExprKind::Attribute { value, .. } => self.expr(value),
            ExprKind::Subscript { value, slice, .. } => {
                self.expr(value);
                self.expr(slice);
            }
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
                elts.iter().for_each(|elt| self.expr(elt))
            }
            ExprKind::Dict { keys, values } => {
                keys.iter().chain(values).for_each(|elt| self.expr(elt))
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
/// For example, `if x: ret 1` followed by `else: ret 2` always exits, and so does `while 1:` with no
/// `break`, but `if x: ret 1` alone does not.
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
