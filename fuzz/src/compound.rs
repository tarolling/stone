//! Compound values for [`generate`](crate::generate): structs and their `impl` methods, enums,
//! options, maps, and lists of them, plus `match` and `==` over them.
//!
//! Every compound value comes from [`Generator::value`], which follows a [`Ty`]. For example,
//! with `struct S0:` holding `x0: int` and `x1: option[str]`, `value(&Ty::Struct(0))` may write
//! `S0(x1=some("ab"), x0=g3 + 1)`.
//!
//! The rules that keep every program valid and every operation from failing:
//!
//! - A type only holds types declared before it, so no struct holds itself, except that an enum's
//!   variants after `V0` may hold the enum itself, so `V0` always ends a value.
//! - Every list has at least two elements and nothing shrinks one, so indexes from -2 to 1 are
//!   always in range, and a `pop` always follows an `append` to the same list.
//! - A map variable is never assigned again, only given entries, so the keys it is known to hold
//!   only grow, and only those are read with `m[k]` or changed with `m[k] += v`. An entry it
//!   pops is assigned again right after, and a map reached through a field or element is only
//!   given entries, since nothing tracks its keys.
//! - Bare `none` only appears where the type around it says what it holds, such as a field or an
//!   argument, so every new variable's type can be inferred.
//! - A method marked as changing `self` starts by changing it and is only called as a statement
//!   or the whole value of an assignment, and any other method sees `self` as read-only.
//! - A function that changes a parameter prints it before returning, so the change is never lost.
//! - Traits only appear in the fixed [`GENERIC_TYPES`]: `G` implements `K` and `ord`, so a `G`'s
//!   length is also read as `g.key()`, `g.twice()`, or `keyed(g)`, and two `G`s are ordered with
//!   `<`, picked from the path rather than drawn, so programs without them are unchanged. Results,
//!   options, and `?` appear there too, in `measured` and `looked_up`, read the same way, and so
//!   do function values: `plus_one` passed to `through` or held in an `F`, and `grown`, which owns
//!   its parameter, passed to `each_grown`.

use crate::{Generator, MAX_BLOCK_LEN, MAX_NESTING, Scope};
use arbitrary::{Result, Unstructured};

/// Most values a list element or map value may hold, well under the compiler's
/// `MAX_ELEMENT_VALUES`, and the most a struct field may hold, so constructions stay short.
const MAX_PART_VALUES: usize = 8;

/// Deepest a declared field type nests, as in `option[map[str, int]]`.
const MAX_TYPE_DEPTH: usize = 2;

/// Most steps from a variable to a value inside it, as in `v1.x0[-1].x2`.
const MAX_PATH_STEPS: usize = 3;

/// A type a generated value can have.
///
/// For example, `Ty::Map(Key::Str, Box::new(Ty::List(Box::new(Ty::Int))))` is written
/// `map[str, list[int]]`, and `Ty::Struct(2)` is `S2`, the third type declared.
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Ty {
    Int,
    Float,
    Bool,
    Str,
    List(Box<Ty>),
    Option(Box<Ty>),
    Map(Key, Box<Ty>),
    Struct(usize),
    Enum(usize),
    /// `G[T]`, the generic struct of [`GENERIC_TYPES`], holding a `T` and a list of them.
    G(Box<Ty>),
    /// `H[T]`, the generic enum of [`GENERIC_TYPES`]: `H.Nothing`, `H.Just(T)`, or
    /// `H.Both(T, T)`.
    H(Box<Ty>),
}

/// A generic struct and enum, with methods that read, change, and make a `G`, a trait `K` that `G`
/// implements, with a default method and a bounded function calling both, and an order on `G`s
/// by their lengths, which a program declares first when it uses them.
pub(crate) const GENERIC_TYPES: &str = "\
struct G[T]:
    v: T
    vs: list[T]

impl G:
    fn get(self) -> T:
        ret self.v

    fn add(self, x: T):
        self.vs.append(x)

    fn wrap(x: T) -> G[T]:
        ret G(v=x, vs=[x, x])

enum H[T]:
    Nothing
    Just(T)
    Both(T, T)

trait K:
    fn key(self) -> int
    fn twice(self) -> int:
        ret self.key() * 2

impl K for G:
    fn key(self) -> int:
        ret self.vs.len()

impl ord for G:
    fn cmp(self, other: Self) -> ordering:
        ret self.vs.len().cmp(other.vs.len())

fn keyed[T: K](x: T) -> int:
    ret x.twice() + x.key()

fn bounded[T](g: G[T], n: int) -> result[int, str]:
    if n >= g.vs.len():
        ret err(\"past \" + str(n))
    ret ok(n)

fn measured[T](g: G[T], n: int) -> result[int, str]:
    k = (\"n\" + str(n)).len() + bounded(g, n)?
    ret ok(k + g.key())

fn looked_up[T](g: G[T], n: int) -> option[int]:
    m = {0: g.key()}
    ret some(m.get(n)? + 1)

fn plus_one(x: int) -> int:
    ret x + 1

fn grown(xs: list[int]) -> list[int]:
    xs.append(xs.len())
    ret xs

struct F:
    f: fn(int) -> int

fn through(f: fn(int) -> int, x: int) -> int:
    ret f(x)

fn each_grown(f: fn(list[int]) -> list[int], xs: list[int]) -> list[int]:
    ret f(f(xs))
";

impl Ty {
    fn is_leaf(&self) -> bool {
        matches!(self, Ty::Int | Ty::Float | Ty::Bool | Ty::Str)
    }
}

/// The type of a map's keys.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Key {
    Int,
    Str,
    Bool,
}

impl Key {
    fn name(self) -> &'static str {
        match self {
            Key::Int => "int",
            Key::Str => "str",
            Key::Bool => "bool",
        }
    }

    /// Generates one of a few keys, so reads often find what was stored, such as `2` or `"b"`.
    fn literal(self, u: &mut Unstructured) -> Result<String> {
        Ok(match self {
            Key::Int => u.int_in_range(0..=3)?.to_string(),
            Key::Str => u.choose(&["\"a\"", "\"b\"", "\"c\""])?.to_string(),
            Key::Bool => u.choose(&["true", "false"])?.to_string(),
        })
    }
}

/// A declared type: `S<n>` with fields `x0`, `x1`, ... or `E<n>` with variants `V0`, `V1`, ...,
/// where `n` is its place among the declarations.
pub(crate) enum TypeDef {
    Struct {
        fields: Vec<Ty>,
        methods: Vec<Method>,
    },
    Enum {
        variants: Vec<Vec<Ty>>,
    },
}

/// A function of an `impl` block.
pub(crate) struct Method {
    name: String,
    params: Vec<Ty>,
    ret: Option<Ty>,
    /// Whether it takes `self`, or is an associated function such as `S0.make()`.
    has_self: bool,
    /// Whether it changes `self`, so a call is a statement or the whole value of an assignment.
    changes: bool,
}

/// What a generic function `u<n>[T](p: T, q: list[T])` returns, in terms of `T`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum GenericRet {
    Param,
    List,
    Option,
    Int,
    Nothing,
}

impl GenericRet {
    /// Returns the result type of a call that gives `T` the type `t`, or `None` without one.
    fn of(self, t: &Ty) -> Option<Ty> {
        match self {
            GenericRet::Param => Some(t.clone()),
            GenericRet::List => Some(Ty::List(Box::new(t.clone()))),
            GenericRet::Option => Some(Ty::Option(Box::new(t.clone()))),
            GenericRet::Int => Some(Ty::Int),
            GenericRet::Nothing => None,
        }
    }

    /// Returns the type `T` must be for a call to give `ty`, or `None` if any type does, or
    /// `Some(None)` if no call can.
    fn param_for(self, ty: &Ty) -> Option<Option<Ty>> {
        match (self, ty) {
            (GenericRet::Param, ty) => Some(Some(ty.clone())),
            (GenericRet::List, Ty::List(t)) | (GenericRet::Option, Ty::Option(t)) => {
                Some(Some((**t).clone()))
            }
            (GenericRet::Int, Ty::Int) => Some(None),
            _ => None,
        }
    }

    fn written(self) -> &'static str {
        match self {
            GenericRet::Param => " -> T",
            GenericRet::List => " -> list[T]",
            GenericRet::Option => " -> option[T]",
            GenericRet::Int => " -> int",
            GenericRet::Nothing => "",
        }
    }
}

/// A top-level function with typed parameters, named `t<n>`.
pub(crate) struct Signature {
    params: Vec<Ty>,
    ret: Option<Ty>,
}

/// A variable holding a compound value, or a bool.
#[derive(Clone)]
pub(crate) struct Var {
    pub(crate) name: String,
    pub(crate) ty: Ty,
    /// Whether statements may assign or change it, which `self` in a method that does not
    /// change it, loop variables, and a list being looped over may not.
    pub(crate) writable: bool,
    /// Keys a map variable is known to hold, written as literals such as `"a"`.
    pub(crate) keys: Vec<String>,
}

/// A value reachable from a variable through fields and list elements, such as `v1.x0[#]`, in
/// which each `#` becomes an index from -2 to 1 when it is used.
struct Path {
    text: String,
    ty: Ty,
    /// The variable it starts from, an index into `Scope::values`.
    root: usize,
    /// Whether a statement may assign or change it.
    place: bool,
}

/// A way to get a value of some type from what is already in scope.
enum Source {
    /// The path's value itself.
    Path(usize),
    /// `path.unwrap_or(default)` of an option path.
    UnwrapOr(usize),
    /// `path.is_some()` or `path.is_none()`.
    IsSome(usize),
    /// `path.get(key)` of a map path.
    Get(usize, Key),
    /// `path.len()` of a list or map path.
    Len(usize),
    /// `path.contains(key)` of a map path, or `key in path` or `not (key not in path)`.
    Contains(usize, Key),
    /// `m[key]` for a key that the map variable `m` is known to hold.
    Known(String, String),
    /// A method that does not change `self`, called on a struct path.
    Method(usize, usize, usize),
    /// An associated function of a struct, such as `S0.make()`.
    Associated(usize, usize),
    /// A typed function.
    Call(usize),
    /// A generic function, with the type its `T` must be, if a particular one.
    Generic(usize, Option<Ty>),
    /// `path.get()` of a `G` path.
    Value(usize),
    /// Two `G` paths of the same type compared by their order, as in `a < b`.
    Ordered(usize),
}

/// Returns the variants of `H[t]`, each as its pattern's head and the types of its values.
fn generic_variants(t: &Ty) -> Vec<(String, Vec<Ty>)> {
    vec![
        ("H.Nothing".to_string(), vec![]),
        ("H.Just".to_string(), vec![t.clone()]),
        ("H.Both".to_string(), vec![t.clone(), t.clone()]),
    ]
}

/// Replaces each `#` in a path with an index that is always in range.
///
/// For example, `fill(u, "v1[#].x0")` may return `v1[-2].x0`.
fn fill(u: &mut Unstructured, text: &str) -> Result<String> {
    let mut out = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            '#' => out.push_str(&u.int_in_range(-2..=1)?.to_string()),
            _ => out.push(c),
        }
    }
    Ok(out)
}

/// Shuffles `0..len`, so struct fields and match cases come in any order.
fn shuffled(u: &mut Unstructured, len: usize) -> Result<Vec<usize>> {
    let mut order: Vec<usize> = (0..len).collect();
    for k in (1..len).rev() {
        let j = u.int_in_range(0..=k)?;
        order.swap(k, j);
    }
    Ok(order)
}

impl Generator {
    fn fields(&self, index: usize) -> &[Ty] {
        match &self.types[index] {
            TypeDef::Struct { fields, .. } => fields,
            TypeDef::Enum { .. } => &[],
        }
    }

    fn methods(&self, index: usize) -> &[Method] {
        match &self.types[index] {
            TypeDef::Struct { methods, .. } => methods,
            TypeDef::Enum { .. } => &[],
        }
    }

    fn variants(&self, index: usize) -> &[Vec<Ty>] {
        match &self.types[index] {
            TypeDef::Enum { variants } => variants,
            TypeDef::Struct { .. } => &[],
        }
    }

    /// Writes a type as a signature does, such as `option[S0]` or `map[str, list[int]]`.
    fn type_name(&self, ty: &Ty) -> String {
        match ty {
            Ty::Int => "int".to_string(),
            Ty::Float => "float".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::Str => "str".to_string(),
            Ty::List(t) => format!("list[{}]", self.type_name(t)),
            Ty::Option(t) => format!("option[{}]", self.type_name(t)),
            Ty::Map(k, v) => format!("map[{}, {}]", k.name(), self.type_name(v)),
            Ty::Struct(i) => format!("S{i}"),
            Ty::Enum(i) => format!("E{i}"),
            Ty::G(t) => format!("G[{}]", self.type_name(t)),
            Ty::H(t) => format!("H[{}]", self.type_name(t)),
        }
    }

    /// Counts the values a type flattens to, as the compiler does: one, or one per leaf of a
    /// struct, since enums, options, lists, and maps are each one value.
    fn values_in(&self, ty: &Ty) -> usize {
        match ty {
            Ty::Struct(i) => self.fields(*i).iter().map(|t| self.values_in(t)).sum(),
            // its value, then its list
            Ty::G(t) => self.values_in(t) + 1,
            _ => 1,
        }
    }

    /// Returns whether `==` accepts the type: no list or map anywhere inside it.
    fn comparable(&self, ty: &Ty) -> bool {
        self.comparable_in(ty, &mut vec![])
    }

    fn comparable_in(&self, ty: &Ty, seen: &mut Vec<usize>) -> bool {
        match ty {
            Ty::List(_) | Ty::Map(..) | Ty::G(_) => false,
            Ty::Option(t) | Ty::H(t) => self.comparable_in(t, seen),
            Ty::Struct(i) | Ty::Enum(i) => {
                // an enum holding itself is comparable if the rest of it is, and one still being
                // declared only holds comparable types
                if seen.contains(i) || *i >= self.types.len() {
                    return true;
                }
                seen.push(*i);
                let parts: Vec<&Ty> = match &self.types[*i] {
                    TypeDef::Struct { fields, .. } => fields.iter().collect(),
                    TypeDef::Enum { variants } => variants.iter().flatten().collect(),
                };
                let all = parts.iter().all(|t| self.comparable_in(t, seen));
                seen.pop();
                all
            }
            _ => true,
        }
    }

    /// Emits one to four `struct` and `enum` declarations, such as `struct S0:` with
    /// `x0: option[int]`, then `enum E1:` with `V0` and `V1(S0, E1)`. About half hold no list or
    /// map, so `==` can compare them. About half the time [`GENERIC_TYPES`] come first, which
    /// the declarations may then hold.
    pub(crate) fn declarations(&mut self, u: &mut Unstructured) -> Result<()> {
        // decided here, so programs without declarations are generated as before
        if u.arbitrary()? {
            self.generic_types = true;
            self.source.push_str(GENERIC_TYPES);
        }
        for _ in 0..u.int_in_range(1..=4)? {
            let index = self.types.len();
            self.plain = u.arbitrary()?;
            if u.arbitrary()? {
                let mut fields = vec![];
                for _ in 0..u.int_in_range(1..=4)? {
                    fields.push(self.field_type(u, None, 0)?);
                }
                self.types.push(TypeDef::Struct {
                    fields,
                    methods: vec![],
                });
                self.line(0, &format!("struct S{index}:"));
                for (k, field) in self.fields(index).to_vec().iter().enumerate() {
                    let name = self.type_name(field);
                    self.line(1, &format!("x{k}: {name}"));
                }
            } else {
                let mut variants = vec![];
                for k in 0..u.int_in_range(1..=3)? {
                    // only later variants hold the enum itself, so `V0` always ends a value
                    let own = (k > 0).then_some(index);
                    let mut fields = vec![];
                    for _ in 0..u.int_in_range(0..=3)? {
                        fields.push(self.field_type(u, own, 0)?);
                    }
                    variants.push(fields);
                }
                self.types.push(TypeDef::Enum { variants });
                self.line(0, &format!("enum E{index}:"));
                for (k, fields) in self.variants(index).to_vec().iter().enumerate() {
                    let names: Vec<String> = fields.iter().map(|t| self.type_name(t)).collect();
                    match names.is_empty() {
                        true => self.line(1, &format!("V{k}")),
                        false => self.line(1, &format!("V{k}({})", names.join(", "))),
                    }
                }
            }
        }
        self.plain = false;
        Ok(())
    }

    /// Generates the type of a field or variant value, from the leaves, the types declared so
    /// far, and `own`, the enum being declared, if it may hold itself. While declaring a plain
    /// type, it gives only comparable types.
    fn field_type(&mut self, u: &mut Unstructured, own: Option<usize>, depth: usize) -> Result<Ty> {
        let nested = depth < MAX_TYPE_DEPTH;
        let lists = !self.plain;
        let last = if self.generic_types { 11 } else { 10 };
        let ty = match u.int_in_range(0..=last)? {
            1 => Ty::Float,
            2 => Ty::Str,
            3 => Ty::Bool,
            4 if lists => Ty::List(Box::new(Ty::Int)),
            5 | 6 if !self.types.is_empty() => self.declared_type(u)?,
            7 if nested => Ty::Option(Box::new(self.field_type(u, own, depth + 1)?)),
            8 if nested && lists => {
                let key = *u.choose(&[Key::Str, Key::Int, Key::Bool])?;
                Ty::Map(key, Box::new(self.part_type(u, own, depth + 1)?))
            }
            9 if nested && lists => Ty::List(Box::new(self.part_type(u, own, depth + 1)?)),
            11 if nested && self.generic_types => {
                let inner = Box::new(self.part_type(u, own, depth + 1)?);
                match u.arbitrary()? {
                    true if lists => Ty::G(inner),
                    _ => Ty::H(inner),
                }
            }
            10 if own.is_some() => {
                let own = Ty::Enum(own.unwrap());
                match u.int_in_range(0..=2)? {
                    1 => Ty::Option(Box::new(own)),
                    2 if lists => Ty::List(Box::new(own)),
                    _ => own,
                }
            }
            _ => Ty::Int,
        };
        // an earlier type may hold a list or map
        Ok(match self.plain && !self.comparable(&ty) {
            true => Ty::Int,
            false => ty,
        })
    }

    /// Generates the type of a list element or map value, which may hold few enough values.
    fn part_type(&mut self, u: &mut Unstructured, own: Option<usize>, depth: usize) -> Result<Ty> {
        let ty = self.field_type(u, own, depth)?;
        Ok(match self.values_in(&ty) <= MAX_PART_VALUES {
            true => ty,
            false => Ty::Str,
        })
    }

    /// Picks a declared struct or enum that holds few enough values to be part of another.
    fn declared_type(&mut self, u: &mut Unstructured) -> Result<Ty> {
        let small: Vec<usize> = (0..self.types.len())
            .filter(|&i| self.values_in(&Ty::Struct(i)) <= MAX_PART_VALUES)
            .collect();
        let Ok(&index) = u.choose(&small) else {
            return Ok(Ty::Int);
        };
        Ok(match self.types[index] {
            TypeDef::Struct { .. } => Ty::Struct(index),
            TypeDef::Enum { .. } => Ty::Enum(index),
        })
    }

    /// Generates the type of a new variable: always compound, such as `option[int]` or `S1`.
    fn any_type(&mut self, u: &mut Unstructured) -> Result<Ty> {
        let declared = !self.types.is_empty();
        // generic types widen the range, so programs without them choose as before
        let last = if self.generic_types { 11 } else { 7 };
        Ok(match u.int_in_range(0..=last)? {
            1..=3 if declared => self.declared_type(u)?,
            4 => {
                let key = *u.choose(&[Key::Str, Key::Int, Key::Bool])?;
                Ty::Map(key, Box::new(self.part_type(u, None, 1)?))
            }
            5 => Ty::List(Box::new(self.part_type(u, None, 1)?)),
            6 if declared => Ty::Option(Box::new(self.declared_type(u)?)),
            8..=11 if self.generic_types => {
                let inner = Box::new(self.part_type(u, None, 1)?);
                match u.arbitrary()? {
                    true => Ty::G(inner),
                    false => Ty::H(inner),
                }
            }
            7 => Ty::Option(Box::new(Ty::Str)),
            _ => Ty::Option(Box::new(Ty::Int)),
        })
    }

    /// Generates the type of a parameter or result, a leaf or a compound type.
    fn param_type(&mut self, u: &mut Unstructured) -> Result<Ty> {
        Ok(match u.int_in_range(0..=7)? {
            1 => Ty::Float,
            2 => Ty::Str,
            3 => Ty::Bool,
            4..=7 => self.any_type(u)?,
            _ => Ty::Int,
        })
    }

    /// Adds a variable to the scope, with ints, floats, and strings where the leaf generators
    /// look for them.
    fn bind(&self, scope: &mut Scope, name: &str, ty: Ty, writable: bool) {
        let name = name.to_string();
        match ty {
            Ty::Int if writable => scope.variables.push(name),
            Ty::Int => scope.counters.push(name),
            Ty::Float => scope.floats.push(name),
            Ty::Str => scope.strings.push(name),
            ty => scope.values.push(Var {
                name,
                ty,
                writable,
                keys: vec![],
            }),
        }
    }

    /// Emits an `impl` block for some of the declared structs.
    pub(crate) fn impls(&mut self, u: &mut Unstructured) -> Result<()> {
        for index in 0..self.types.len() {
            if matches!(self.types[index], TypeDef::Struct { .. }) && u.arbitrary()? {
                self.impl_block(u, index)?;
            }
        }
        Ok(())
    }

    /// Emits `impl S<n>:` with one to three functions: methods that read `self`, methods that
    /// change it, and an associated `make`.
    fn impl_block(&mut self, u: &mut Unstructured, index: usize) -> Result<()> {
        self.line(0, &format!("impl S{index}:"));
        for k in 0..u.int_in_range(1..=3)? {
            let has_make = self.methods(index).iter().any(|m| m.name == "make");
            let (name, has_self, changes) = match u.int_in_range(0..=2)? {
                2 if !has_make => ("make".to_string(), false, false),
                1 => (format!("m{k}"), true, true),
                _ => (format!("m{k}"), true, false),
            };
            let mut params = vec![];
            for _ in 0..u.int_in_range(0..=2)? {
                params.push(self.param_type(u)?);
            }
            let ret = match (has_self, changes) {
                (false, _) => Some(Ty::Struct(index)),
                (true, true) if u.arbitrary()? => None,
                _ => Some(self.param_type(u)?),
            };

            let names: Vec<String> = params.iter().map(|_| self.fresh("p")).collect();
            let mut header: Vec<String> = vec![];
            if has_self {
                header.push("self".to_string());
            }
            for (name, ty) in names.iter().zip(&params) {
                header.push(format!("{name}: {}", self.type_name(ty)));
            }
            let arrow = match &ret {
                Some(ty) => format!(" -> {}", self.type_name(ty)),
                None => String::new(),
            };
            self.line(1, &format!("fn {name}({}){arrow}:", header.join(", ")));

            let mut scope = Scope {
                in_function: true,
                returns: ret.clone(),
                ..Scope::default()
            };
            if has_self {
                self.bind(&mut scope, "self", Ty::Struct(index), changes);
            }
            for (name, ty) in names.iter().zip(&params) {
                self.bind(&mut scope, name, ty.clone(), true);
            }
            self.body(u, &mut scope, 2, &names, changes)?;

            // registered after the body, so a method never calls itself
            if let TypeDef::Struct { methods, .. } = &mut self.types[index] {
                methods.push(Method {
                    name,
                    params,
                    ret,
                    has_self,
                    changes,
                });
            }
        }
        Ok(())
    }

    /// Emits a top-level function `t<n>` with parameters and a result of any type, such as
    /// `fn t0(p3: S0, p4: map[str, int]) -> option[E1]:`.
    pub(crate) fn typed_function(&mut self, u: &mut Unstructured, index: usize) -> Result<()> {
        let mut params = vec![];
        for _ in 0..u.int_in_range(0..=4)? {
            params.push(self.param_type(u)?);
        }
        let ret = match u.arbitrary()? {
            true => Some(self.param_type(u)?),
            false => None,
        };
        let names: Vec<String> = params.iter().map(|_| self.fresh("p")).collect();
        let typed: Vec<String> = names
            .iter()
            .zip(&params)
            .map(|(name, ty)| format!("{name}: {}", self.type_name(ty)))
            .collect();
        let arrow = match &ret {
            Some(ty) => format!(" -> {}", self.type_name(ty)),
            None => String::new(),
        };
        self.line(0, &format!("fn t{index}({}){arrow}:", typed.join(", ")));

        let mut scope = Scope {
            in_function: true,
            returns: ret.clone(),
            ..Scope::default()
        };
        for (name, ty) in names.iter().zip(&params) {
            self.bind(&mut scope, name, ty.clone(), true);
        }
        self.body(u, &mut scope, 1, &names, false)?;
        // registered after the body, so a function never calls itself
        self.typed_functions.push(Signature { params, ret });
        Ok(())
    }

    /// Generates a call of the generic function `u<g>` with `t` for its `T`, such as
    /// `u0(some(2), [none, some(1)])`. Neither argument is pinned, since `T` is inferred.
    fn generic_call(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        g: usize,
        t: &Ty,
        nesting: usize,
    ) -> Result<String> {
        let p = self.value(u, scope, t, nesting, false)?;
        let q = self.value(u, scope, &Ty::List(Box::new(t.clone())), nesting, false)?;
        Ok(format!("u{g}({p}, {q})"))
    }

    /// Emits a generic function `u<n>[T](p: T, q: list[T])`, whose body only does what any `T`
    /// allows: it moves values of type `T` into lists, options, and tuples, reads `q`'s elements,
    /// length, and items, prints, and calls the generic functions before it. Lists only grow,
    /// and a caller's `q` has at least two elements, so every index is in range.
    pub(crate) fn generic_function(&mut self, u: &mut Unstructured, index: usize) -> Result<()> {
        let ret = *u.choose(&[
            GenericRet::Param,
            GenericRet::List,
            GenericRet::Option,
            GenericRet::Int,
            GenericRet::Nothing,
        ])?;
        self.line(
            0,
            &format!("fn u{index}[T](p: T, q: list[T]){}:", ret.written()),
        );
        // the names holding a T, and a list[T]
        let mut values = vec!["p".to_string()];
        let mut lists = vec!["q".to_string()];
        let mut emitted = 0;
        for _ in 0..u.int_in_range(0..=6)? {
            let value = u.choose(&values)?.clone();
            let list = u.choose(&lists)?.clone();
            let k = u.int_in_range(-2..=1)?;
            match u.int_in_range(0..=8)? {
                0 => {
                    let name = self.fresh("l");
                    self.line(1, &format!("{name} = [{value}, {list}[{k}]]"));
                    lists.push(name);
                }
                1 if lists.len() > 1 => {
                    // only a local list changes, so no change to a parameter is lost
                    let local = u.choose(&lists[1..])?.clone();
                    self.line(1, &format!("{local}.append({value})"));
                }
                2 => self.line(1, &format!("print({value}, {list})")),
                3 => {
                    let name = self.fresh("l");
                    self.line(1, &format!("{name} = {list}.len()"));
                    self.line(1, &format!("print({name})"));
                    emitted += 1;
                }
                4 => {
                    let name = self.fresh("l");
                    self.line(1, &format!("{name} = some({value})"));
                    self.line(1, &format!("print({name}.is_some(), {name})"));
                    emitted += 1;
                }
                5 => {
                    let (a, b) = (self.fresh("l"), self.fresh("l"));
                    self.line(1, &format!("{a}, {b} = ({value}, {k})"));
                    self.line(1, &format!("print({b})"));
                    emitted += 1;
                    values.push(a);
                }
                6 => {
                    let name = self.fresh("l");
                    self.line(1, &format!("for {name} in {list}:"));
                    self.line(2, &format!("print({name})"));
                }
                7 if index > 0 => {
                    let g = u.int_in_range(0..=index - 1)?;
                    let call = format!("u{g}({value}, {list})");
                    match self.generics[g] {
                        GenericRet::Nothing => self.line(1, &call),
                        other => {
                            let name = self.fresh("l");
                            self.line(1, &format!("{name} = {call}"));
                            match other {
                                GenericRet::Param => values.push(name),
                                GenericRet::List => lists.push(name),
                                _ => {
                                    self.line(1, &format!("print({name})"));
                                    emitted += 1;
                                }
                            }
                        }
                    }
                }
                _ => {
                    let name = self.fresh("l");
                    self.line(1, &format!("{name} = {list}[{k}]"));
                    values.push(name);
                }
            }
            emitted += 1;
        }
        let value = u.choose(&values)?.clone();
        match ret {
            GenericRet::Param => self.line(1, &format!("ret {value}")),
            GenericRet::List => {
                let list = u.choose(&lists)?.clone();
                self.line(1, &format!("ret {list}"));
            }
            GenericRet::Option if u.arbitrary()? => self.line(1, "ret none"),
            GenericRet::Option => self.line(1, &format!("ret some({value})")),
            GenericRet::Int => {
                let list = u.choose(&lists)?.clone();
                self.line(1, &format!("ret {list}.len()"));
            }
            GenericRet::Nothing if emitted == 0 => self.line(1, "ret"),
            GenericRet::Nothing => {}
        }
        // registered after the body, so a function never calls itself
        self.generics.push(ret);
        Ok(())
    }

    /// Emits the body of a method or typed function: a change to `self` first if `change_self`,
    /// then statements, a `print` of each parameter they changed, and a final `ret` if the
    /// function returns a value.
    fn body(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        params: &[String],
        change_self: bool,
    ) -> Result<()> {
        self.changed.clear();
        let mut emitted = 0;
        if change_self && self.place_change(u, scope, indent, Some("self"))? {
            emitted += 1;
        }
        for _ in 0..u.int_in_range(0..=MAX_BLOCK_LEN / 2)? {
            self.statement(u, scope, indent, 2)?;
            emitted += 1;
        }
        // reading a changed parameter keeps the checker from warning that the change is lost
        let changed: Vec<String> = params
            .iter()
            .filter(|p| self.changed.contains(p))
            .cloned()
            .collect();
        for param in changed {
            self.line(indent, &format!("print({param})"));
            emitted += 1;
        }
        match scope.returns.clone() {
            Some(ty) => {
                let value = self.value(u, scope, &ty, 0, true)?;
                self.line(indent, &format!("ret {value}"));
            }
            // a block needs a statement
            None if emitted == 0 => self.line(indent, "ret"),
            None => {}
        }
        Ok(())
    }

    /// Lists every path into the scope's compound variables.
    fn paths(&self, scope: &Scope) -> Vec<Path> {
        let mut out = vec![];
        for (root, var) in scope.values.iter().enumerate() {
            self.walk(&var.name, &var.ty, root, var.writable, 0, &mut out);
        }
        out
    }

    fn walk(
        &self,
        text: &str,
        ty: &Ty,
        root: usize,
        place: bool,
        depth: usize,
        out: &mut Vec<Path>,
    ) {
        out.push(Path {
            text: text.to_string(),
            ty: ty.clone(),
            root,
            place,
        });
        if depth == MAX_PATH_STEPS {
            return;
        }
        match ty {
            Ty::Struct(i) => {
                for (k, field) in self.fields(*i).iter().enumerate() {
                    self.walk(&format!("{text}.x{k}"), field, root, place, depth + 1, out);
                }
            }
            Ty::List(t) => self.walk(&format!("{text}[#]"), t, root, place, depth + 1, out),
            Ty::G(t) => {
                self.walk(&format!("{text}.v"), t, root, place, depth + 1, out);
                let list = Ty::List(t.clone());
                self.walk(&format!("{text}.vs"), &list, root, place, depth + 1, out);
            }
            _ => {}
        }
    }

    /// Lists the ways to get a value of type `ty` from the scope.
    fn sources(&self, scope: &Scope, paths: &[Path], ty: &Ty) -> Vec<Source> {
        let mut out = vec![];
        for (i, path) in paths.iter().enumerate() {
            if path.ty == *ty {
                out.push(Source::Path(i));
            }
            match &path.ty {
                Ty::Option(t) => {
                    if **t == *ty {
                        out.push(Source::UnwrapOr(i));
                    }
                    if *ty == Ty::Bool {
                        out.push(Source::IsSome(i));
                    }
                }
                Ty::Map(key, value) => {
                    if matches!(ty, Ty::Option(t) if t == value) {
                        out.push(Source::Get(i, *key));
                    }
                    match ty {
                        Ty::Int => out.push(Source::Len(i)),
                        Ty::Bool => out.push(Source::Contains(i, *key)),
                        _ => {}
                    }
                }
                Ty::List(_) if *ty == Ty::Int => out.push(Source::Len(i)),
                Ty::G(t) => {
                    if **t == *ty {
                        out.push(Source::Value(i));
                    }
                    if *ty == Ty::Bool {
                        out.push(Source::Ordered(i));
                    }
                }
                Ty::Struct(s) => {
                    for (m, method) in self.methods(*s).iter().enumerate() {
                        if method.has_self && !method.changes && method.ret.as_ref() == Some(ty) {
                            out.push(Source::Method(i, *s, m));
                        }
                    }
                }
                _ => {}
            }
        }
        for var in &scope.values {
            if matches!(&var.ty, Ty::Map(_, value) if **value == *ty) {
                for key in &var.keys {
                    out.push(Source::Known(var.name.clone(), key.clone()));
                }
            }
        }
        for (f, signature) in self.typed_functions.iter().enumerate() {
            if signature.ret.as_ref() == Some(ty) {
                out.push(Source::Call(f));
            }
        }
        for (g, ret) in self.generics.iter().enumerate() {
            match ret.param_for(ty) {
                Some(Some(t)) if self.values_in(&t) <= MAX_PART_VALUES => {
                    out.push(Source::Generic(g, Some(t)))
                }
                Some(None) => out.push(Source::Generic(g, None)),
                _ => {}
            }
        }
        if let Ty::Struct(s) = ty {
            for (m, method) in self.methods(*s).iter().enumerate() {
                if !method.has_self {
                    out.push(Source::Associated(*s, m));
                }
            }
        }
        out
    }

    /// Generates a value of type `ty` from what is already in scope, such as `v2.x1`,
    /// `v0.get("a")`, or `t1(g0)`, or returns `None` if nothing there gives one.
    pub(crate) fn derived(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        ty: &Ty,
        nesting: usize,
    ) -> Result<Option<String>> {
        let paths = self.paths(scope);
        let mut sources = self.sources(scope, &paths, ty);
        if sources.is_empty() {
            return Ok(None);
        }
        let source = sources.swap_remove(u.choose_index(sources.len())?);
        let inner = nesting + 1;
        let text = match source {
            Source::Path(i) => fill(u, &paths[i].text)?,
            Source::UnwrapOr(i) => {
                let path = fill(u, &paths[i].text)?;
                let default = self.value(u, scope, ty, inner, true)?;
                format!("{path}.unwrap_or({default})")
            }
            Source::IsSome(i) => {
                let method = *u.choose(&["is_some", "is_none"])?;
                format!("{}.{method}()", fill(u, &paths[i].text)?)
            }
            Source::Get(i, key) => format!("{}.get({})", fill(u, &paths[i].text)?, key.literal(u)?),
            Source::Value(i) => format!("{}.get()", fill(u, &paths[i].text)?),
            // `G`s are ordered by `cmp`, through the prelude's trait `ord`
            Source::Ordered(i) => {
                let path = fill(u, &paths[i].text)?;
                let op = ["<", "<=", ">", ">="][i % 4];
                format!("({path} {op} {path})")
            }
            // the length of a `G`'s list is its `key` through the trait `K`, which a call may
            // read instead, or a multiple of it, picked by the path rather than drawn, since
            // coverage elsewhere depends on how many draws a program makes
            Source::Len(i) if paths[i].text.ends_with(".vs") => {
                let path = fill(u, &paths[i].text)?;
                let g = path.strip_suffix(".vs").unwrap_or(&path);
                match i % 6 {
                    0 => format!("{path}.len()"),
                    1 => format!("{g}.key()"),
                    2 => format!("{g}.twice()"),
                    3 => format!("keyed({g})"),
                    // an err for an n past the list's two elements, and none for n = 1
                    4 => format!("measured({g}, {}).unwrap_or({g}.key())", i % 3),
                    _ => format!("looked_up({g}, {}).unwrap_or(0)", (i / 6) % 2),
                }
            }
            // a length may also go through function values, held by a parameter, a field, and a
            // list, once the program declares them, picked by the path as above
            Source::Len(i) if self.generic_types => {
                let path = fill(u, &paths[i].text)?;
                match (i + path.len()) % 6 {
                    0 => format!("through(plus_one, {path}.len())"),
                    1 => format!("F(f=plus_one).f({path}.len())"),
                    2 => format!("each_grown(grown, [{path}.len(), 1])[-1]"),
                    _ => format!("{path}.len()"),
                }
            }
            Source::Len(i) => format!("{}.len()", fill(u, &paths[i].text)?),
            Source::Contains(i, key) => {
                let (map, key) = (fill(u, &paths[i].text)?, key.literal(u)?);
                // the method, or either operator, which binds looser than whatever holds it
                match u.int_in_range(0..=2)? {
                    0 => format!("{map}.contains({key})"),
                    1 => format!("({key} in {map})"),
                    _ => format!("(not ({key} not in {map}))"),
                }
            }
            Source::Known(name, key) => format!("{name}[{key}]"),
            Source::Method(i, s, m) => {
                let path = fill(u, &paths[i].text)?;
                let (name, params) = {
                    let method = &self.methods(s)[m];
                    (method.name.clone(), method.params.clone())
                };
                let args = self.arguments(u, scope, &params, inner)?;
                format!("{path}.{name}({args})")
            }
            Source::Associated(s, m) => {
                let (name, params) = {
                    let method = &self.methods(s)[m];
                    (method.name.clone(), method.params.clone())
                };
                let args = self.arguments(u, scope, &params, inner)?;
                format!("S{s}.{name}({args})")
            }
            Source::Call(f) => {
                let params = self.typed_functions[f].params.clone();
                let args = self.arguments(u, scope, &params, inner)?;
                format!("t{f}({args})")
            }
            Source::Generic(g, t) => {
                let t = match t {
                    Some(t) => t,
                    None => self.part_type(u, None, 1)?,
                };
                self.generic_call(u, scope, g, &t, inner)?
            }
        };
        Ok(Some(text))
    }

    fn arguments(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        params: &[Ty],
        nesting: usize,
    ) -> Result<String> {
        let mut args = Vec::with_capacity(params.len());
        for ty in params {
            args.push(self.value(u, scope, ty, nesting, true)?);
        }
        Ok(args.join(", "))
    }

    /// Generates a value of type `ty`. `pinned` says whether the type around it fixes its type,
    /// as a field, an argument, or an assignment to an existing variable does, so bare `none` may
    /// appear.
    ///
    /// Past [`MAX_NESTING`] it only makes new values the simplest way: a struct's fields, an
    /// enum's `V0`, `some(..)`, two list elements, or one map entry.
    pub(crate) fn value(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        ty: &Ty,
        nesting: usize,
        pinned: bool,
    ) -> Result<String> {
        let deep = nesting >= MAX_NESTING;
        // leaves look in scope through their own generators
        if !deep
            && (!ty.is_leaf() || *ty == Ty::Bool)
            && u.int_in_range(0..=3)? == 3
            && let Some(text) = self.derived(u, scope, ty, nesting)?
        {
            return Ok(text);
        }
        match ty {
            Ty::Int => self.expression(u, scope, nesting),
            Ty::Float => self.float_expression(u, scope, nesting),
            Ty::Str => self.text(u, scope, nesting),
            Ty::Bool => {
                if deep || u.arbitrary()? {
                    return Ok(u.choose(&["true", "false"])?.to_string());
                }
                let a = self.sum(u, scope, nesting + 1)?;
                let op = *u.choose(&["==", "!=", "<", "<=", ">", ">="])?;
                let b = self.sum(u, scope, nesting + 1)?;
                Ok(format!("({a} {op} {b})"))
            }
            Ty::List(t) => {
                if **t == Ty::Int && !deep && !scope.lists.is_empty() && u.arbitrary()? {
                    return Ok(u.choose(&scope.lists)?.clone());
                }
                let count = if deep { 2 } else { u.int_in_range(2..=3)? };
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(self.value(u, scope, t, nesting + 1, pinned)?);
                }
                Ok(format!("[{}]", items.join(", ")))
            }
            Ty::Option(t) => match if deep { 0 } else { u.int_in_range(0..=4)? } {
                1 | 2 if pinned => Ok("none".to_string()),
                3 if **t == Ty::Int => {
                    let a = self.text(u, scope, nesting + 1)?;
                    let b = self.text(u, scope, nesting + 1)?;
                    Ok(format!("({a}).find({b})"))
                }
                // never set, unlike `STONE_LEAK_CHECK`, which only compiled programs get
                4 if **t == Ty::Str => {
                    self.uses_os = true;
                    Ok("os.env(\"STONE_FUZZ_UNSET\")".to_string())
                }
                _ => Ok(format!(
                    "some({})",
                    self.value(u, scope, t, nesting + 1, pinned)?
                )),
            },
            Ty::Map(key, value) => Ok(self.map_literal(u, scope, *key, value, nesting, pinned)?.0),
            Ty::Struct(i) => self.construct(u, scope, *i, nesting),
            Ty::Enum(i) => {
                let count = self.variants(*i).len();
                let k = if deep { 0 } else { u.choose_index(count)? };
                let fields = self.variants(*i)[k].clone();
                if fields.is_empty() {
                    return Ok(format!("E{i}.V{k}"));
                }
                let args = self.arguments(u, scope, &fields, nesting + 1)?;
                Ok(format!("E{i}.V{k}({args})"))
            }
            // the values of a generic type fix its type argument only as much as `pinned` says
            Ty::G(t) => match if deep { 0 } else { u.int_in_range(0..=2)? } {
                0 => {
                    let v = self.value(u, scope, t, nesting + 1, pinned)?;
                    Ok(format!("G.wrap({v})"))
                }
                _ => {
                    let v = self.value(u, scope, t, nesting + 1, pinned)?;
                    let list = Ty::List(t.clone());
                    let vs = self.value(u, scope, &list, nesting + 1, pinned)?;
                    Ok(match u.arbitrary()? {
                        true => format!("G(v={v}, vs={vs})"),
                        false => format!("G(vs={vs}, v={v})"),
                    })
                }
            },
            Ty::H(t) => match if deep { 0 } else { u.int_in_range(0..=3)? } {
                1 if pinned => Ok("H.Nothing".to_string()),
                2 => {
                    let a = self.value(u, scope, t, nesting + 1, pinned)?;
                    let b = self.value(u, scope, t, nesting + 1, pinned)?;
                    Ok(format!("H.Both({a}, {b})"))
                }
                _ => {
                    let a = self.value(u, scope, t, nesting + 1, pinned)?;
                    Ok(format!("H.Just({a})"))
                }
            },
        }
    }

    /// Generates `S<n>(...)` with every field named once, in any order.
    fn construct(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        index: usize,
        nesting: usize,
    ) -> Result<String> {
        let fields = self.fields(index).to_vec();
        let mut parts = Vec::with_capacity(fields.len());
        for k in shuffled(u, fields.len())? {
            let value = self.value(u, scope, &fields[k], nesting + 1, true)?;
            parts.push(format!("x{k}={value}"));
        }
        Ok(format!("S{index}({})", parts.join(", ")))
    }

    /// Generates a map literal of one to three entries and returns it with its keys, such as
    /// `{"a": 1, "b": 2}` and `["\"a\"", "\"b\""]`. It is never `{}`, whose type a new variable
    /// could not infer.
    fn map_literal(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        key: Key,
        value: &Ty,
        nesting: usize,
        pinned: bool,
    ) -> Result<(String, Vec<String>)> {
        let count = if nesting >= MAX_NESTING {
            1
        } else {
            u.int_in_range(1..=3)?
        };
        let mut entries = Vec::with_capacity(count);
        let mut keys: Vec<String> = vec![];
        for _ in 0..count {
            let k = key.literal(u)?;
            let v = self.value(u, scope, value, nesting + 1, pinned)?;
            entries.push(format!("{k}: {v}"));
            if !keys.contains(&k) {
                keys.push(k);
            }
        }
        Ok((format!("{{{}}}", entries.join(", ")), keys))
    }

    /// Notes that a statement changed the variable `name`, so a function prints a changed
    /// parameter before returning.
    fn note_change(&mut self, name: &str) {
        if !self.changed.iter().any(|n| n == name) {
            self.changed.push(name.to_string());
        }
    }

    /// Emits a statement that works with compound values: declaring one, changing one in place,
    /// working with a map, `match`, `==`, calling a method or typed function, looping over a
    /// list, popping, or printing.
    pub(crate) fn compound_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let nested = nesting < MAX_NESTING;
        let done = match u.int_in_range(0..=10)? {
            1 | 2 => self.place_change(u, scope, indent, None)?,
            3 => self.map_statement(u, scope, indent)?,
            4 if nested => {
                self.match_statement(u, scope, indent, nesting)?;
                true
            }
            5 => {
                let (a, b) = self.equality(u, scope, 0)?;
                let label = self.fresh("eq");
                self.line(
                    indent,
                    &format!("print(\"{label}\", {a} == {b}, {a} != {b})"),
                );
                true
            }
            6 | 7 => self.call_statement(u, scope, indent)?,
            8 if nested => {
                self.ensure_list(u, scope, indent, false)?;
                self.for_statement_over(u, scope, indent, nesting)?
            }
            9 => {
                self.ensure_list(u, scope, indent, true)?;
                self.pop_statement(u, scope, indent)?
            }
            10 => {
                let ty = self.any_type(u)?;
                let value = self.value(u, scope, &ty, 0, false)?;
                // as print writes it, as str makes it, or inside a string; spaces keep a map's
                // braces from doubling the string's
                let line = match u.int_in_range(0..=2)? {
                    0 => format!("print({value})"),
                    1 => format!("print(str({value}))"),
                    _ => format!("print(\"{} {{ {value} }} end\")", self.fresh("text")),
                };
                self.line(indent, &line);
                true
            }
            _ => false,
        };
        if !done {
            self.declare(u, scope, indent)?;
        }
        Ok(())
    }

    /// Declares a new list variable unless the scope already has a list path, a writable one if
    /// `place`, so a loop or `pop` has a list to work with.
    fn ensure_list(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        place: bool,
    ) -> Result<()> {
        let has_list = self
            .paths(scope)
            .iter()
            .any(|p| matches!(p.ty, Ty::List(_)) && (p.place || !place));
        if !has_list {
            let ty = Ty::List(Box::new(self.part_type(u, None, 1)?));
            self.declare_as(u, scope, indent, ty)?;
        }
        Ok(())
    }

    /// Emits `v<n> = value` for a new variable of a compound type.
    fn declare(&mut self, u: &mut Unstructured, scope: &mut Scope, indent: usize) -> Result<()> {
        let ty = self.any_type(u)?;
        self.declare_as(u, scope, indent, ty)
    }

    fn declare_as(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        ty: Ty,
    ) -> Result<()> {
        let (value, keys) = match &ty {
            Ty::Map(key, value) => self.map_literal(u, scope, *key, value, 0, false)?,
            _ => (self.value(u, scope, &ty, 0, false)?, vec![]),
        };
        let name = self.fresh("v");
        self.line(indent, &format!("{name} = {value}"));
        scope.values.push(Var {
            name,
            ty,
            writable: true,
            keys,
        });
        Ok(())
    }

    /// Emits a change to a writable path, such as `v1.x0 += 2`, `v2[-1].x1.append(3)`,
    /// `v3["a"] = 4`, or a call of a method that changes its receiver, limited to paths from the
    /// variable `root` if given. Returns whether there was a path to change.
    fn place_change(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        root: Option<&str>,
    ) -> Result<bool> {
        let paths: Vec<Path> = self
            .paths(scope)
            .into_iter()
            .filter(|p| p.place && root.is_none_or(|r| scope.values[p.root].name == r))
            .collect();
        if paths.is_empty() {
            return Ok(false);
        }
        // a `G` is one of several paths into its variable, so it is picked on its own too
        let generic: Vec<&Path> = paths.iter().filter(|p| matches!(p.ty, Ty::G(_))).collect();
        let path = match !generic.is_empty() && u.arbitrary()? {
            true => generic[u.choose_index(generic.len())?],
            false => &paths[u.choose_index(paths.len())?],
        };
        let target = fill(u, &path.text)?;
        let root_name = scope.values[path.root].name.clone();
        match &path.ty {
            Ty::Int => {
                let op = *u.choose(&["", "+", "-", "*"])?;
                let value = self.expression(u, scope, 0)?;
                self.line(indent, &format!("{target} {op}= {value}"));
            }
            Ty::Float => {
                let op = *u.choose(&["", "+", "-", "*", "/"])?;
                let value = self.float_expression(u, scope, 0)?;
                self.line(indent, &format!("{target} {op}= {value}"));
            }
            Ty::List(t) if u.arbitrary()? => {
                let value = self.value(u, scope, t, 0, true)?;
                self.line(indent, &format!("{target}.append({value})"));
            }
            // a map variable only gains entries, so the keys it holds are known
            Ty::Map(key, value) if target == root_name => {
                let (key, value) = (*key, (**value).clone());
                self.map_change(u, scope, indent, path.root, key, &value)?;
            }
            // a map inside a variable gains an entry, through its place
            Ty::Map(key, value) => {
                let key_text = key.literal(u)?;
                let value = self.value(u, scope, value, 0, true)?;
                self.line(indent, &format!("{target}[{key_text}] = {value}"));
            }
            Ty::G(t) => {
                let value = self.value(u, scope, t, 0, true)?;
                self.line(indent, &format!("{target}.add({value})"));
            }
            Ty::Struct(s) if self.methods(*s).iter().any(|m| m.changes) && u.arbitrary()? => {
                let changing: Vec<usize> = (0..self.methods(*s).len())
                    .filter(|&m| self.methods(*s)[m].changes)
                    .collect();
                let m = *u.choose(&changing)?;
                let (name, params) = {
                    let method = &self.methods(*s)[m];
                    (method.name.clone(), method.params.clone())
                };
                let args = self.arguments(u, scope, &params, 0)?;
                self.line(indent, &format!("{target}.{name}({args})"));
            }
            ty => {
                let ty = ty.clone();
                let value = self.value(u, scope, &ty, 0, true)?;
                self.line(indent, &format!("{target} = {value}"));
            }
        }
        self.note_change(&root_name);
        Ok(true)
    }

    /// Emits a change to the map variable `scope.values[root]`, whose keys are of type `key` and
    /// values of type `value`: a new or replaced entry, `m[k] op= v` for a key it holds, or a
    /// `pop` whose entry is assigned again right after, so the keys it holds only grow.
    fn map_change(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        root: usize,
        key: Key,
        value: &Ty,
    ) -> Result<()> {
        match u.int_in_range(0..=3)? {
            1 if self.map_augment(u, scope, indent, root, value)? => Ok(()),
            2 => self.map_pop(u, scope, indent, root, key, value),
            _ => {
                let key_text = key.literal(u)?;
                self.map_set(u, scope, indent, root, key_text, value)
            }
        }
    }

    /// Emits `m[key] = value` for the map variable `scope.values[root]` and notes that it holds
    /// `key`.
    fn map_set(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        root: usize,
        key_text: String,
        value: &Ty,
    ) -> Result<()> {
        let target = scope.values[root].name.clone();
        let value = self.value(u, scope, value, 0, true)?;
        self.line(indent, &format!("{target}[{key_text}] = {value}"));
        let keys = &mut scope.values[root].keys;
        if !keys.contains(&key_text) {
            keys.push(key_text);
        }
        Ok(())
    }

    /// Emits `m[k] op= v` for a key `k` that the map variable `scope.values[root]` holds, if
    /// its values are numbers and it holds any, and returns whether it did.
    fn map_augment(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        root: usize,
        value: &Ty,
    ) -> Result<bool> {
        let known = scope.values[root].keys.clone();
        if known.is_empty() {
            return Ok(false);
        }
        let target = scope.values[root].name.clone();
        let (op, amount) = match value {
            Ty::Int => (*u.choose(&["+", "-", "*"])?, self.expression(u, scope, 0)?),
            Ty::Float => (
                *u.choose(&["+", "-", "*", "/"])?,
                self.float_expression(u, scope, 0)?,
            ),
            _ => return Ok(false),
        };
        let key_text = u.choose(&known)?;
        self.line(indent, &format!("{target}[{key_text}] {op}= {amount}"));
        Ok(true)
    }

    /// Emits `p = m.pop(k)` for the map variable `scope.values[root]` and prints `p`, then
    /// assigns `m[k]` again, so the entry comes back at the end and the keys it holds only grow.
    fn map_pop(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
        root: usize,
        key: Key,
        value: &Ty,
    ) -> Result<()> {
        let target = scope.values[root].name.clone();
        let key_text = key.literal(u)?;
        let popped = self.fresh("p");
        self.line(indent, &format!("{popped} = {target}.pop({key_text})"));
        self.line(indent, &format!("print(\"{popped}\", {popped})"));
        self.map_set(u, scope, indent, root, key_text, value)
    }

    /// Emits `print` of a map's length, membership, lookups, and lists of its keys, values, and
    /// entries, such as `print(v1, v1.len(), v1.contains("a"), v1.get("a"), v1.keys(),
    /// v1.values(), v1.items(), v1["b"])`, then a loop over its keys that prints each entry.
    fn map_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
    ) -> Result<bool> {
        let paths: Vec<Path> = self
            .paths(scope)
            .into_iter()
            .filter(|p| matches!(p.ty, Ty::Map(..)))
            .collect();
        if paths.is_empty() {
            return Ok(false);
        }
        let path = &paths[u.choose_index(paths.len())?];
        let Ty::Map(key, _) = path.ty else {
            unreachable!()
        };
        let map = fill(u, &path.text)?;
        let (a, b) = (key.literal(u)?, key.literal(u)?);
        let mut parts = vec![
            map.clone(),
            format!("{map}.len()"),
            format!("{map}.contains({a})"),
            format!("{map}.get({b})"),
            format!("{map}.keys()"),
            format!("{map}.values()"),
            format!("{map}.items()"),
        ];
        let keys = &scope.values[path.root].keys;
        if map == scope.values[path.root].name && !keys.is_empty() {
            parts.push(format!("{map}[{}]", u.choose(keys)?));
        }
        self.line(indent, &format!("print({})", parts.join(", ")));
        // the loop's key is always one the map holds
        let key = self.fresh("k");
        self.line(indent, &format!("for {key} in {map}:"));
        self.line(
            indent + 1,
            &format!("print(\"{key}\", {key}, {map}[{key}])"),
        );
        // and a map variable that may change does
        let root = path.root;
        if map == scope.values[root].name && path.place {
            let Ty::Map(key, value) = path.ty.clone() else {
                unreachable!()
            };
            self.map_pop(u, scope, indent, root, key, &value)?;
            self.map_augment(u, scope, indent, root, &value)?;
            let name = scope.values[root].name.clone();
            self.note_change(&name);
        }
        Ok(true)
    }

    /// Generates a comparable type for `==`, such as `option[int]` or a struct with no list or
    /// map inside it.
    fn comparable_type(&mut self, u: &mut Unstructured) -> Result<Ty> {
        // only programs with generic types take a byte for them, so others choose as before
        if self.generic_types && u.int_in_range(0..=3)? == 0 {
            let inner = match u.arbitrary()? {
                true => Ty::Int,
                false => Ty::Option(Box::new(Ty::Str)),
            };
            return Ok(Ty::H(Box::new(inner)));
        }
        let mut choices = vec![Ty::Option(Box::new(Ty::Int)), Ty::Option(Box::new(Ty::Str))];
        for i in 0..self.types.len() {
            let ty = match self.types[i] {
                TypeDef::Struct { .. } => Ty::Struct(i),
                TypeDef::Enum { .. } => Ty::Enum(i),
            };
            if self.comparable(&ty) {
                choices.push(Ty::Option(Box::new(ty.clone())));
                choices.push(ty);
            }
        }
        // declared types first, as the fuzzer's bytes favor the end of the list
        choices.reverse();
        Ok(u.choose(&choices)?.clone())
    }

    /// Generates two values of one comparable compound type for `==`, the second often the
    /// same as the first, so both answers come up.
    pub(crate) fn equality(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        nesting: usize,
    ) -> Result<(String, String)> {
        let paths = self.paths(scope);
        let comparable: Vec<&Path> = paths
            .iter()
            .filter(|p| !p.ty.is_leaf() && self.comparable(&p.ty))
            .collect();
        let (a, ty) = if !comparable.is_empty() && u.arbitrary()? {
            let path = comparable[u.choose_index(comparable.len())?];
            (fill(u, &path.text)?, path.ty.clone())
        } else {
            let ty = self.comparable_type(u)?;
            (self.value(u, scope, &ty, nesting, false)?, ty)
        };
        let b = match u.int_in_range(0..=2)? {
            0 => a.clone(),
            _ => self.value(u, scope, &ty, nesting, true)?,
        };
        Ok((a, b))
    }

    /// Emits a `match` on an enum or option with a case for each variant in any order, some
    /// refined by a nested pattern first, and sometimes a final `case _:` or `case name:` for
    /// the variants left. No case is ever unreachable, and every value has one.
    fn match_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<()> {
        let paths = self.paths(scope);
        let subjects: Vec<&Path> = paths
            .iter()
            .filter(|p| matches!(p.ty, Ty::Enum(_) | Ty::Option(_) | Ty::H(_)))
            .collect();
        let (subject, ty) = if !subjects.is_empty() && u.arbitrary()? {
            let path = subjects[u.choose_index(subjects.len())?];
            (fill(u, &path.text)?, path.ty.clone())
        } else {
            let enums: Vec<usize> = (0..self.types.len())
                .filter(|&i| matches!(self.types[i], TypeDef::Enum { .. }))
                .collect();
            let ty = match u.int_in_range(0..=2)? {
                0 if !enums.is_empty() => Ty::Enum(*u.choose(&enums)?),
                1 if self.generic_types => Ty::H(Box::new(self.part_type(u, None, 1)?)),
                _ => Ty::Option(Box::new(self.part_type(u, None, 1)?)),
            };
            (self.value(u, scope, &ty, 0, false)?, ty)
        };

        // each variant's pattern head and the types of its values
        let variants: Vec<(String, Vec<Ty>)> = match &ty {
            Ty::Option(t) => vec![
                ("none".to_string(), vec![]),
                ("some".to_string(), vec![(**t).clone()]),
            ],
            Ty::Enum(i) => self
                .variants(*i)
                .iter()
                .enumerate()
                .map(|(k, fields)| (format!("E{i}.V{k}"), fields.clone()))
                .collect(),
            Ty::H(t) => generic_variants(t),
            _ => unreachable!(),
        };
        self.line(indent, &format!("match {subject}:"));
        let order = shuffled(u, variants.len())?;
        let covered = match variants.len() >= 2 && u.arbitrary()? {
            true => u.int_in_range(1..=variants.len() - 1)?,
            false => variants.len(),
        };
        for &k in &order[..covered] {
            let (head, fields) = &variants[k];
            // a case for part of a variant goes before the case for all of it
            if u.int_in_range(0..=2)? == 2
                && let Some(pattern) = self.refining(u, head, fields)?
            {
                self.case(u, scope, indent, nesting, &pattern, vec![])?;
            }
            let mut parts = vec![];
            let mut bindings = vec![];
            for field in fields {
                if u.arbitrary()? {
                    let name = self.fresh("b");
                    parts.push(name.clone());
                    bindings.push((name, field.clone()));
                } else {
                    parts.push("_".to_string());
                }
            }
            let pattern = match fields.is_empty() {
                true => head.clone(),
                false => format!("{head}({})", parts.join(", ")),
            };
            self.case(u, scope, indent, nesting, &pattern, bindings)?;
        }
        if covered < variants.len() {
            if u.arbitrary()? {
                let name = self.fresh("b");
                self.case(u, scope, indent, nesting, &name.clone(), vec![(name, ty)])?;
            } else {
                self.case(u, scope, indent, nesting, "_", vec![])?;
            }
        }
        Ok(())
    }

    /// Generates a pattern matching part of a variant through one value that is itself an
    /// option or an enum of two or more variants, such as `E1.V2(_, some(_))`, or `None` if the
    /// variant holds no such value.
    fn refining(
        &mut self,
        u: &mut Unstructured,
        head: &str,
        fields: &[Ty],
    ) -> Result<Option<String>> {
        let refinable: Vec<usize> = (0..fields.len())
            .filter(|&k| match &fields[k] {
                Ty::Option(_) | Ty::H(_) => true,
                Ty::Enum(j) => self.variants(*j).len() >= 2,
                _ => false,
            })
            .collect();
        if refinable.is_empty() {
            return Ok(None);
        }
        let k = *u.choose(&refinable)?;
        let nested = match &fields[k] {
            Ty::Option(_) => u.choose(&["none", "some(_)"])?.to_string(),
            Ty::H(_) => u
                .choose(&["H.Nothing", "H.Just(_)", "H.Both(_, _)"])?
                .to_string(),
            Ty::Enum(j) => {
                let w = u.choose_index(self.variants(*j).len())?;
                match self.variants(*j)[w].len() {
                    0 => format!("E{j}.V{w}"),
                    n => format!("E{j}.V{w}({})", vec!["_"; n].join(", ")),
                }
            }
            _ => unreachable!(),
        };
        let parts: Vec<String> = (0..fields.len())
            .map(|f| {
                if f == k {
                    nested.clone()
                } else {
                    "_".to_string()
                }
            })
            .collect();
        Ok(Some(format!("{head}({})", parts.join(", "))))
    }

    fn case(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
        pattern: &str,
        bindings: Vec<(String, Ty)>,
    ) -> Result<()> {
        self.line(indent + 1, &format!("case {pattern}:"));
        let mut inner = scope.clone();
        for (name, ty) in bindings {
            self.bind(&mut inner, &name, ty, true);
        }
        self.block(u, &inner, indent + 2, nesting + 1)
    }

    /// Emits a call as a statement or the whole value of an assignment: a method on a struct
    /// path, one that changes its receiver only on a writable path, or a typed function.
    fn call_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
    ) -> Result<bool> {
        let paths = self.paths(scope);
        // (path, struct, method) for methods, and (usize::MAX, 0, f) for typed functions
        let mut calls: Vec<(usize, usize, usize)> = vec![];
        for (i, path) in paths.iter().enumerate() {
            if let Ty::Struct(s) = path.ty {
                for (m, method) in self.methods(s).iter().enumerate() {
                    if method.has_self && (path.place || !method.changes) {
                        calls.push((i, s, m));
                    }
                }
            }
        }
        for f in 0..self.typed_functions.len() {
            calls.push((usize::MAX, 0, f));
        }
        for g in 0..self.generics.len() {
            calls.push((usize::MAX - 1, 0, g));
        }
        if calls.is_empty() {
            return Ok(false);
        }
        let (i, s, m) = calls[u.choose_index(calls.len())?];
        let (call, ret) = if i == usize::MAX - 1 {
            let t = self.part_type(u, None, 1)?;
            let call = self.generic_call(u, scope, m, &t, 0)?;
            (call, self.generics[m].of(&t))
        } else if i == usize::MAX {
            let (params, ret) = {
                let signature = &self.typed_functions[m];
                (signature.params.clone(), signature.ret.clone())
            };
            let args = self.arguments(u, scope, &params, 0)?;
            (format!("t{m}({args})"), ret)
        } else {
            let (name, params, ret, changes) = {
                let method = &self.methods(s)[m];
                let ret = method.ret.clone();
                (
                    method.name.clone(),
                    method.params.clone(),
                    ret,
                    method.changes,
                )
            };
            let target = fill(u, &paths[i].text)?;
            let args = self.arguments(u, scope, &params, 0)?;
            if changes {
                let root = scope.values[paths[i].root].name.clone();
                self.note_change(&root);
            }
            (format!("{target}.{name}({args})"), ret)
        };
        match ret {
            Some(ty) if u.arbitrary()? => {
                let name = self.fresh("v");
                self.line(indent, &format!("{name} = {call}"));
                self.bind(scope, &name, ty, true);
            }
            _ => self.line(indent, &call),
        }
        Ok(true)
    }

    /// Emits `for e in path:` over a list of compound values, whose variable cannot change while
    /// the loop runs.
    fn for_statement_over(
        &mut self,
        u: &mut Unstructured,
        scope: &Scope,
        indent: usize,
        nesting: usize,
    ) -> Result<bool> {
        let paths: Vec<Path> = self
            .paths(scope)
            .into_iter()
            .filter(|p| matches!(p.ty, Ty::List(_)))
            .collect();
        if paths.is_empty() {
            return Ok(false);
        }
        let path = &paths[u.choose_index(paths.len())?];
        let Ty::List(element) = &path.ty else {
            unreachable!()
        };
        let list = fill(u, &path.text)?;
        let variable = self.fresh("e");
        self.line(indent, &format!("for {variable} in {list}:"));
        let mut inner = scope.clone();
        inner.values[path.root].writable = false;
        self.bind(&mut inner, &variable, (**element).clone(), false);
        inner.in_loop = true;
        self.block(u, &inner, indent + 1, nesting + 1)?;
        Ok(true)
    }

    /// Emits an `append` to a writable list and then a `pop` from it, so it keeps at least two
    /// elements, such as `v1.x0.append(3)` then `v4 = v1.x0.pop()`. After a `pop` at an index
    /// from a list of ints, floats, or strings, it sorts the list and prints it.
    fn pop_statement(
        &mut self,
        u: &mut Unstructured,
        scope: &mut Scope,
        indent: usize,
    ) -> Result<bool> {
        let paths: Vec<Path> = self
            .paths(scope)
            .into_iter()
            .filter(|p| p.place && matches!(p.ty, Ty::List(_)))
            .collect();
        if paths.is_empty() {
            return Ok(false);
        }
        let path = &paths[u.choose_index(paths.len())?];
        let Ty::List(element) = &path.ty else {
            unreachable!()
        };
        let element = (**element).clone();
        let list = fill(u, &path.text)?;
        let value = self.value(u, scope, &element, 0, true)?;
        self.line(indent, &format!("{list}.append({value})"));
        let name = self.fresh("v");
        if u.arbitrary()? {
            self.line(indent, &format!("{name} = {list}.pop()"));
            self.bind(scope, &name, Ty::Option(Box::new(element)), true);
        } else {
            let index = u.int_in_range(-2..=1)?;
            self.line(indent, &format!("{name} = {list}.pop({index})"));
            // sorting here draws nothing more, so the programs it appears in stay as they were
            if matches!(element, Ty::Int | Ty::Float | Ty::Str) {
                self.line(indent, &format!("{list}.sort()"));
                self.line(indent, &format!("print({list})"));
            }
            self.bind(scope, &name, element, true);
        }
        let root = scope.values[path.root].name.clone();
        self.note_change(&root);
        Ok(true)
    }
}
