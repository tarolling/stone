//! Tree-walking interpreter that evaluates a stone AST directly.
//!
//! For example, running `x = 42` followed by `print(x)` prints `42`.

mod code;

use crate::ast::{Arg, BoolOp, CompOp, Constant, Expr, Mod, Operator, Stmt, StmtKind, UnaryOp};
use crate::checker::{
    Calls, Conversion, Converter, FunctionValue, Handle, Method, Step, Type, collect_assigned,
    place, prints_bare, struct_display,
};
use crate::diagnostic::Diagnostic;
use crate::last_use::{GlobalReads, global_reads, kept_at_exit, last_uses};
use crate::span::Span;
use crate::stdlib::random::Rng;
use crate::stdlib::{self, MAX_CALL_DEPTH, OUTPUT, fs, gui, math, net, os, time};
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::io::{BufRead, Write};
use std::rc::Rc;
use std::sync::Arc;

type EvalResult<T> = Result<T, Box<dyn std::error::Error>>;

/// Gives a runtime error the position of `span`, the expression that failed, unless it already
/// has one from an expression inside it or is an [`Exit`] or a [`Propagate`], so the innermost
/// expression wins.
///
/// For example, `division by zero` raised while evaluating `a / b` at 3:7 becomes a
/// [`Diagnostic`] at 3:7, which stays there as it passes through `print(a / b)`.
fn located(error: Box<dyn std::error::Error>, span: Span) -> Box<dyn std::error::Error> {
    if error.is::<Diagnostic>() || error.is::<Exit>() || error.is::<Propagate>() {
        return error;
    }
    Box::new(Diagnostic::error(span, error.to_string()))
}

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
    /// reads and what the function returns besides its value, `exit`.
    fn new(body: &[Stmt], reads: &GlobalReads, exit: &HashSet<String>) -> Self {
        let by_span = last_uses(body, reads, exit);
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

/// Where each local variable of a function lives in its [`Frame`]: the parameters first, in
/// order, then every other name the body assigns, in the order of their first assignments.
///
/// For example, `fn f(a: int) -> int:` with the body `b = a * 2` followed by `ret b` puts `a` in
/// slot 0 and `b` in slot 1.
#[derive(Debug, Default)]
struct Layout {
    names: Vec<String>,
    index: HashMap<String, usize>,
}

impl Layout {
    /// Lays out the locals of a function with the parameters `params` and the body `body`.
    fn new(params: &[Arg], body: &[Stmt]) -> Self {
        let mut layout = Layout::default();
        for param in params {
            layout.add(&param.arg);
        }
        let mut assigned = vec![];
        collect_assigned(body, &mut assigned);
        for (name, _) in &assigned {
            layout.add(name);
        }
        layout
    }

    fn add(&mut self, name: &str) {
        if !self.index.contains_key(name) {
            self.index.insert(name.to_string(), self.names.len());
            self.names.push(name.to_string());
        }
    }

    /// Returns the slot of the local `name`, or `None` if `name` is not one of the function's
    /// locals, so it names a global.
    fn slot(&self, name: &str) -> Option<usize> {
        self.index.get(name).copied()
    }
}

/// The local variables of a running call, one slot per name of its function's [`Layout`], empty
/// until the variable is assigned and again once it is dropped at its last use.
struct Frame {
    slots: Vec<Option<Value>>,
    layout: Rc<Layout>,
}

impl Frame {
    /// Takes the value out of the local `name`, leaving it empty.
    fn take(&mut self, name: &str) -> Option<Value> {
        self.slots.get_mut(self.layout.slot(name)?)?.take()
    }
}

/// A user-defined function.
///
/// For example, `fn add(a: int, b: int) -> int: ret a + b` takes 2 parameters, `a` and `b`, in
/// the first two slots of its [`Layout`].
struct Function {
    /// The function's linked name, such as `geo.area`.
    name: Rc<str>,
    params: usize,
    layout: Rc<Layout>,
    /// The body, resolved when the function was defined.
    body: Vec<code::Stmt>,
    /// How the method calls of the check that saw this body were resolved, and its function
    /// values and calls of them, since in an interactive session a later entry's check can
    /// place the same code at other lines.
    resolved: Rc<Calls>,
}

/// A function as a value: its linked name and printed type, and, for a lambda, the body it had
/// when the value was made, since in an interactive session a later entry's lambda can take the
/// same name, and the copies a closure holds, which its lambda takes before its arguments.
#[derive(Clone)]
pub struct FunctionRef {
    value: Arc<FunctionValue>,
    /// The function's id among the interpreter's functions.
    id: usize,
    body: Option<Rc<Function>>,
    /// The type arguments of a generic function, from where the value was made.
    types: Option<Types>,
    /// The copies of variables a closure holds, empty for any other function value.
    captures: Rc<[Value]>,
}

/// The type each type parameter of the running copy of a generic function stands for, by name,
/// which calls through bounds of generic traits need to pick their functions.
type Types = Rc<HashMap<String, Type>>;

impl std::fmt::Debug for FunctionRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
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
    /// Bytes, which may hold any byte and never change.
    Bytes(Rc<[u8]>),
    /// No value, what a function without `->` returns, which no variable can hold.
    Unit,
    List(Rc<Vec<Value>>),
    /// A tuple of two or more fields, which never changes.
    Tuple(Rc<[Value]>),
    /// A value of an enum, which never changes, such as `Shape.Circle(1.0)` or `some(3)`.
    Enum(Rc<EnumValue>),
    /// A value of a struct, such as `Point(x=1.0, y=2.0)`, whose fields a change copies first
    /// if anything else shares it, as a list's elements are.
    Struct(Rc<StructValue>),
    /// A map, such as `{"a": 1}`, whose entries a change copies first if anything else shares
    /// them, as a list's elements are.
    Map(Rc<MapValue>),
    /// A function named as a value, such as `double` in `xs.map(double)`, which prints as its
    /// type.
    Function(FunctionRef),
    /// A TCP connection, which copies share and which closes once nothing holds it.
    Socket(Rc<RefCell<net::Socket>>),
    /// A TCP port waiting for connections, which copies share and which closes once nothing
    /// holds it.
    Listener(Rc<RefCell<net::Listener>>),
}

/// A map key, which is an int, a string, or a bool, since those hash and compare exactly.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MapKey {
    Int(i64),
    Str(Rc<str>),
    Bool(bool),
}

impl MapKey {
    /// Returns the key a value is, or an error for a value the checker allows no map to hold.
    fn of(value: &Value) -> EvalResult<MapKey> {
        match value {
            Value::Int(i) => Ok(MapKey::Int(*i)),
            Value::Str(s) => Ok(MapKey::Str(s.clone())),
            Value::Bool(b) => Ok(MapKey::Bool(*b)),
            other => Err(format!("a map key cannot be {}", other.display(true)).into()),
        }
    }

    fn value(&self) -> Value {
        match self {
            MapKey::Int(i) => Value::Int(*i),
            MapKey::Str(s) => Value::Str(s.clone()),
            MapKey::Bool(b) => Value::Bool(*b),
        }
    }
}

/// The entries of a map in the order their keys were first added, with the position of each
/// key's entry.
///
/// For example, `{"b": 1, "a": 2}` keeps `"b"` first, and assigning `m["b"] = 3` keeps its place.
/// Removing an entry leaves a hole where it was, so that no later entry moves, and the entries
/// close up once holes are more than half of them, which keeps `pop` constant time on average.
#[derive(Debug, Clone, Default)]
pub struct MapValue {
    entries: Vec<Option<(MapKey, Value)>>,
    positions: HashMap<MapKey, usize>,
    holes: usize,
}

impl MapValue {
    /// Returns how many entries the map has.
    fn len(&self) -> usize {
        self.entries.len() - self.holes
    }

    /// Returns the entries in order.
    fn iter(&self) -> impl Iterator<Item = &(MapKey, Value)> {
        self.entries.iter().flatten()
    }

    /// Returns whether the map has an entry for `key`.
    fn contains(&self, key: &MapKey) -> bool {
        self.positions.contains_key(key)
    }

    /// Returns the value of `key`'s entry, or the error compiled code also gives if there is
    /// none, such as `key not found: 'apple'`.
    fn get(&self, key: &MapKey) -> EvalResult<&Value> {
        self.find(key)
            .ok_or_else(|| format!("key not found: {}", key.value().display(true)).into())
    }

    /// Returns the value of `key`'s entry, if there is one.
    fn find(&self, key: &MapKey) -> Option<&Value> {
        let &position = self.positions.get(key)?;
        self.entries[position].as_ref().map(|(_, value)| value)
    }

    /// Returns the value of `key`'s entry for changing, or the error [`MapValue::get`] gives.
    fn get_mut(&mut self, key: &MapKey) -> EvalResult<&mut Value> {
        let found = self
            .positions
            .get(key)
            .and_then(|&position| self.entries[position].as_mut());
        match found {
            Some((_, value)) => Ok(value),
            None => Err(format!("key not found: {}", key.value().display(true)).into()),
        }
    }

    /// Removes `key`'s entry and returns its value, if it has one, keeping the other entries in
    /// order.
    fn remove(&mut self, key: &MapKey) -> Option<Value> {
        let position = self.positions.remove(key)?;
        let (_, value) = self.entries[position].take()?;
        self.holes += 1;
        if 2 * self.holes > self.entries.len() {
            self.close_up();
        }
        Some(value)
    }

    /// Removes the holes, moving each entry down to its place among those left.
    fn close_up(&mut self) {
        self.entries.retain(Option::is_some);
        self.holes = 0;
        for (position, (key, _)) in self.entries.iter().flatten().enumerate() {
            if let Some(at) = self.positions.get_mut(key) {
                *at = position;
            }
        }
    }

    /// Sets `key`'s entry to `value`, adding it at the end if there is none.
    fn insert(&mut self, key: MapKey, value: Value) {
        match self.positions.get(&key) {
            Some(&position) => {
                if let Some(entry) = self.entries[position].as_mut() {
                    entry.1 = value;
                }
            }
            None => {
                self.positions.insert(key.clone(), self.entries.len());
                self.entries.push(Some((key, value)));
            }
        }
    }

    /// Takes every value out of the map, leaving it empty.
    fn drain_values(&mut self) -> impl Iterator<Item = Value> + '_ {
        self.positions.clear();
        self.holes = 0;
        self.entries.drain(..).flatten().map(|(_, value)| value)
    }
}

/// A struct's definition, as the interpreter needs it: how it prints and its fields' names, in
/// the order they are written.
#[derive(Debug)]
pub struct StructDef {
    /// The struct's name without its module, such as `Point`.
    pub name: Rc<str>,
    /// The struct's linked name, such as `geo.Point`, which calls through a bound look up.
    pub linked: Rc<str>,
    pub fields: Vec<String>,
    /// The ids of the functions `<linked>.display`, `<linked>.eq`, and `<linked>.cmp`, which an
    /// `impl` block may define, before or after the struct, so `print`, `==`, and `<` look for
    /// them by id when they run.
    display: usize,
    eq: usize,
    cmp: usize,
}

/// An enum's definition, as the interpreter needs it: its linked name, and how each variant
/// prints, in the order of their tags.
struct EnumInfo {
    name: String,
    variants: Vec<(String, Rc<str>)>,
}

/// Names given ids in the order they are first seen, so code resolved before a name is defined
/// can still refer to it, by an id whose entry a definition fills in later.
///
/// For example, `fn f() -> int: ret g()` gives `g` an id before `g` is defined.
#[derive(Default)]
struct Interner {
    ids: HashMap<String, usize>,
    names: Vec<String>,
}

impl Interner {
    /// Returns the id of `name`, giving it the next one if it has none yet.
    fn id(&mut self, name: &str) -> usize {
        if let Some(&id) = self.ids.get(name) {
            return id;
        }
        self.ids.insert(name.to_string(), self.names.len());
        self.names.push(name.to_string());
        self.names.len() - 1
    }

    fn get(&self, name: &str) -> Option<usize> {
        self.ids.get(name).copied()
    }
}

/// The values of a struct's fields, in the order its definition writes them.
#[derive(Debug, Clone)]
pub struct StructValue {
    pub def: Rc<StructDef>,
    pub fields: Vec<Value>,
}

impl StructValue {
    /// Returns the position of the field `name`.
    fn field(&self, name: &str) -> EvalResult<usize> {
        self.def
            .fields
            .iter()
            .position(|field| field == name)
            .ok_or_else(|| format!("'{}' has no field '{name}'", self.def.name).into())
    }
}

impl Drop for StructValue {
    /// Drops the values this one holds with a loop, as [`EnumValue`]'s drop does, since a struct
    /// can hold a chain of values through an option.
    fn drop(&mut self) {
        drop_values(std::mem::take(&mut self.fields));
    }
}

/// A variant of an enum and the values it holds, such as `Shape.Rect(2.0, 3.0)`.
#[derive(Debug)]
pub struct EnumValue {
    /// How the variant prints before its values, such as `Shape.Rect` or `some`.
    pub name: Rc<str>,
    /// The variant's position in its enum's definition, which tells variants apart.
    pub tag: usize,
    pub fields: Vec<Value>,
}

impl Drop for EnumValue {
    /// Drops the values this one holds with a loop rather than recursion, so dropping a long
    /// chain of values, such as a linked list of a million nodes, cannot overflow the stack.
    fn drop(&mut self) {
        drop_values(std::mem::take(&mut self.fields));
    }
}

/// Drops `pending` with a loop rather than recursion, emptying each enum value, struct value,
/// list, and tuple that nothing else holds into the worklist before it is dropped.
fn drop_values(mut pending: Vec<Value>) {
    {
        while let Some(value) = pending.pop() {
            match value {
                Value::Enum(mut inner) => {
                    if let Some(inner) = Rc::get_mut(&mut inner) {
                        pending.append(&mut inner.fields);
                    }
                }
                Value::Struct(mut inner) => {
                    if let Some(inner) = Rc::get_mut(&mut inner) {
                        pending.append(&mut inner.fields);
                    }
                }
                Value::List(mut items) => {
                    if let Some(items) = Rc::get_mut(&mut items) {
                        pending.append(items);
                    }
                }
                Value::Map(mut map) => {
                    if let Some(map) = Rc::get_mut(&mut map) {
                        pending.extend(map.drain_values());
                    }
                }
                Value::Tuple(mut fields) => {
                    if let Some(fields) = Rc::get_mut(&mut fields) {
                        pending
                            .extend(fields.iter_mut().map(|f| std::mem::replace(f, Value::Unit)));
                    }
                }
                _ => {}
            }
        }
    }
}

/// Makes the ordering `less`, `equal`, or `greater`, the prelude's variants in that order, for
/// `cmp`.
fn ordering(order: Ordering) -> Value {
    let (name, tag) = match order {
        Ordering::Less => ("less", 0),
        Ordering::Equal => ("equal", 1),
        Ordering::Greater => ("greater", 2),
    };
    Value::Enum(Rc::new(EnumValue {
        name: name.into(),
        tag,
        fields: vec![],
    }))
}

/// Reads an ordering a `cmp` returned.
fn ordering_of(value: &Value) -> EvalResult<Ordering> {
    match value {
        Value::Enum(order) => Ok(match order.tag {
            0 => Ordering::Less,
            1 => Ordering::Equal,
            _ => Ordering::Greater,
        }),
        other => Err(format!("expected an ordering, found {}", other.display(true)).into()),
    }
}

/// Makes the option `none`, the prelude's first variant, for builtins that may find nothing.
fn none() -> Value {
    Value::Enum(Rc::new(EnumValue {
        name: "none".into(),
        tag: 0,
        fields: vec![],
    }))
}

/// Makes the option `some(value)`, the prelude's second variant.
fn some(value: Value) -> Value {
    Value::Enum(Rc::new(EnumValue {
        name: "some".into(),
        tag: 1,
        fields: vec![value],
    }))
}

/// Makes the result `ok(value)` from `Ok(value)` or `err(message)` from `Err(message)`, as
/// `int(s)` and `float(s)` return.
fn result(outcome: Result<Value, String>) -> Value {
    let (name, tag, value) = match outcome {
        Ok(value) => ("ok", 0, value),
        Err(message) => ("err", 1, Value::Str(message.into())),
    };
    Value::Enum(Rc::new(EnumValue {
        name: name.into(),
        tag,
        fields: vec![value],
    }))
}

/// Returns whether `value` is a result, `ok` or `err`, rather than an option, which share the
/// methods `unwrap` and `unwrap_or`. Only the prelude's variants print as `ok` and `err`.
fn is_result(value: &EnumValue) -> bool {
    matches!(&*value.name, "ok" | "err")
}

/// Returns the ints of a list the checker knows holds only ints, such as an address's bytes.
fn ints(values: &[Value]) -> Vec<i64> {
    values
        .iter()
        .map(|value| match value {
            Value::Int(n) => *n,
            _ => 0,
        })
        .collect()
}

/// Makes a new list of strings, such as the pieces `split` returns.
fn str_list(items: Vec<&str>) -> Value {
    Value::list(items.into_iter().map(|s| Value::Str(s.into())).collect())
}

/// Returns the least of `values` for `math.min`, or the greatest for `math.max`, which must all be
/// ints or all floats, as the checker ensures.
///
/// For example, `min_or_max("math.max", &[Value::Int(1), Value::Int(3)])` returns `Value::Int(3)`.
fn min_or_max(name: &str, values: &[Value]) -> EvalResult<Value> {
    let max = name == "math.max";
    if let Some(Value::Float(_)) = values.first() {
        let floats: Vec<f64> = values
            .iter()
            .map(|value| match value {
                Value::Float(x) => Ok(*x),
                _ => Err(format!("{name}() takes ints or floats")),
            })
            .collect::<Result<_, _>>()?;
        let result = if max {
            math::max(&floats)
        } else {
            math::min(&floats)
        };
        return Ok(Value::Float(result?));
    }
    let ints: Vec<i64> = values
        .iter()
        .map(|value| match value {
            Value::Int(i) => Ok(*i),
            _ => Err(format!("{name}() takes ints or floats")),
        })
        .collect::<Result<_, _>>()?;
    let result = if max {
        math::max(&ints)
    } else {
        math::min(&ints)
    };
    Ok(Value::Int(result?))
}

impl Value {
    fn list(items: Vec<Value>) -> Value {
        Value::List(Rc::new(items))
    }

    /// Formats the value the way `print` shows it, with strings inside lists and tuples quoted.
    ///
    /// For example, `[1, 2]` prints as `[1, 2]`, `["a"]` as `['a']`, and `(1, "a")` as
    /// `(1, 'a')`.
    /// It never runs a struct's own `display`, so it serves error messages, which
    /// [`Interpreter::write_value`] does not.
    fn display(&self, nested: bool) -> String {
        let mut out = String::new();
        // an error message writes what it can of a value nested too deeply
        let _ = self.write(&mut out, nested, 0, &mut |_, _| Ok(false));
        out
    }

    /// Writes the value as [`Value::display`] does to `out`, inside `depth` enum and struct
    /// values, failing with [`stdlib::PRINT_DEPTH`] at one nested more than
    /// [`stdlib::MAX_PRINT_DEPTH`] deep. `custom` is offered each struct value first, and writes
    /// it itself when it returns true, as a struct's own `display` does.
    ///
    /// For example, `some(Shape.Circle(1.0))` writes `some(Shape.Circle(1.0))`, and a struct
    /// writes its name and each field's, as in `Point(x=1.0, y=2.0)`.
    fn write(
        &self,
        out: &mut String,
        nested: bool,
        depth: usize,
        custom: &mut dyn FnMut(&Value, &mut String) -> EvalResult<bool>,
    ) -> EvalResult<()> {
        match self {
            Value::Int(i) => out.push_str(&i.to_string()),
            Value::Float(x) => out.push_str(&stdlib::format_float(*x)),
            Value::Bool(b) => out.push_str(&b.to_string()),
            Value::Str(s) if nested => out.push_str(&stdlib::repr_in_list(s)),
            Value::Str(s) => out.push_str(s),
            Value::Bytes(b) => out.push_str(&stdlib::repr_bytes(b)),
            Value::Unit => {}
            Value::Function(function) => out.push_str(&function.value.text),
            // a handle prints as its type, as a function does
            Value::Socket(_) => out.push_str(Handle::Socket.name()),
            Value::Listener(_) => out.push_str(Handle::Listener.name()),
            Value::List(items) => {
                out.push('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    item.write(out, true, depth, custom)?;
                }
                out.push(']');
            }
            Value::Map(map) => {
                out.push('{');
                for (i, (key, value)) in map.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    key.value().write(out, true, depth, custom)?;
                    out.push_str(": ");
                    value.write(out, true, depth, custom)?;
                }
                out.push('}');
            }
            Value::Tuple(fields) => {
                out.push('(');
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    field.write(out, true, depth, custom)?;
                }
                out.push(')');
            }
            Value::Enum(value) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(stdlib::PRINT_DEPTH.into());
                }
                out.push_str(&value.name);
                if !value.fields.is_empty() {
                    out.push('(');
                    for (i, field) in value.fields.iter().enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        field.write(out, true, depth + 1, custom)?;
                    }
                    out.push(')');
                }
            }
            Value::Struct(_) if custom(self, out)? => {}
            Value::Struct(value) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(stdlib::PRINT_DEPTH.into());
                }
                out.push_str(&value.def.name);
                out.push('(');
                for (i, (name, field)) in value.def.fields.iter().zip(&value.fields).enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(name);
                    out.push('=');
                    field.write(out, true, depth + 1, custom)?;
                }
                out.push(')');
            }
        }
        Ok(())
    }

    /// Returns whether a variable holding this value is dropped at its last use: if it may hold
    /// a list, which a change can copy, a list, a tuple or struct with one among its fields at
    /// any depth, or an enum value; or a socket or listener, which closes once nothing holds it,
    /// so the other end sees it close where compiled code closes it.
    fn drops_early(&self) -> bool {
        match self {
            Value::List(_) | Value::Map(_) | Value::Enum(_) => true,
            Value::Socket(_) | Value::Listener(_) => true,
            Value::Tuple(fields) => fields.iter().any(Value::drops_early),
            Value::Struct(value) => value.fields.iter().any(Value::drops_early),
            _ => false,
        }
    }

    /// Returns whether this is `true`, the only value that passes a condition, since the checker
    /// allows only a `bool` there.
    fn is_true(&self) -> bool {
        matches!(self, Value::Bool(true))
    }

    /// Returns whether two values are equal, comparing tuples, structs, and enum values part by
    /// part, left to right, until two parts differ, and enum values by variant first. `depth` is
    /// how many enum values the two are inside, and comparing one more than
    /// [`stdlib::MAX_PRINT_DEPTH`] deep fails, as compiled code does.
    ///
    /// For example, `some((1, "a"))` equals `some((1, "a"))` but not `some((1, "b"))` or `none`.
    fn equals(&self, other: &Value, depth: usize) -> EvalResult<bool> {
        fn all_equal(a: &[Value], b: &[Value], depth: usize) -> EvalResult<bool> {
            for (a, b) in a.iter().zip(b) {
                if !a.equals(b, depth)? {
                    return Ok(false);
                }
            }
            Ok(a.len() == b.len())
        }
        Ok(match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Bytes(a), Value::Bytes(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => all_equal(a, b, depth)?,
            (Value::Struct(a), Value::Struct(b)) => all_equal(&a.fields, &b.fields, depth)?,
            (Value::Enum(a), Value::Enum(b)) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(stdlib::COMPARE_DEPTH.into());
                }
                a.tag == b.tag && all_equal(&a.fields, &b.fields, depth + 1)?
            }
            // the checker compares nothing else
            _ => false,
        })
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
            // byte order, which is also the order of the characters' code points
            (Value::Str(a), Value::Str(b)) => Ok(Some(a.as_bytes().cmp(b.as_bytes()))),
            (Value::Bytes(a), Value::Bytes(b)) => Ok(Some(a.cmp(b))),
            (a, b) => {
                Err(format!("cannot order {} and {}", a.display(true), b.display(true)).into())
            }
        }
    }

    fn as_struct(&self) -> EvalResult<&StructValue> {
        match self {
            Value::Struct(value) => Ok(value),
            other => Err(format!("expected a struct, found {}", other.display(true)).into()),
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
        match self {
            Value::List(items) => Rc::strong_count(items) > 1,
            Value::Map(map) => Rc::strong_count(map) > 1,
            _ => false,
        }
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
            Constant::Bytes(b) => Value::Bytes(b.as_slice().into()),
            Constant::Int(i) | Constant::I64(i) => Value::Int(*i),
            Constant::Float(x) | Constant::F64(x) => Value::Float(*x),
            // other literal kinds are not lexed yet
            _ => Value::Unit,
        }
    }
}

/// Converts `index` into a position in a list of length `len`, counting negative indexes from the
/// end like Python.
///
/// For example, index `-1` of a 3-element list is position 2, and index 3 is an error.
fn list_position(index: i64, len: usize) -> EvalResult<usize> {
    position(index, len, stdlib::INDEX_RANGE)
}

/// Turns `index` into a position in a sequence of `len` elements or bytes, as
/// [`list_position`] does, failing with the error `template` filled with the index and the length.
///
/// For example, `position(3, 3, STRING_INDEX_RANGE)` fails with
/// `string index out of range (index 3, length 3)`.
fn position(index: i64, len: usize, template: &str) -> EvalResult<usize> {
    let len = len as i64;
    let position = if index < 0 { index + len } else { index };
    if (0..len).contains(&position) {
        Ok(position as usize)
    } else {
        Err(stdlib::fill(template, &[index, len]).into())
    }
}

/// An arithmetic expression partly evaluated by [`Interpreter::arithmetic`]: a value, or an
/// operation that has a list among its operands and so works element by element.
enum Lazy {
    Value(Value),
    Binary(Operator, Box<Lazy>, Box<Lazy>),
    Unary(UnaryOp, Box<Lazy>),
}

impl Lazy {
    /// Returns the element at `index` of what this computes, where every list it holds has more
    /// than `index` elements.
    fn at(&self, index: usize) -> EvalResult<Value> {
        match self {
            Lazy::Value(Value::List(items)) => Ok(items[index].clone()),
            Lazy::Value(value) => Ok(value.clone()),
            Lazy::Binary(op, left, right) => binary(*op, left.at(index)?, right.at(index)?),
            Lazy::Unary(op, operand) => unary(*op, operand.at(index)?),
        }
    }

    /// Adds every list this holds to `out`, left to right.
    fn lists<'a>(&'a self, out: &mut Vec<&'a Rc<Vec<Value>>>) {
        match self {
            Lazy::Value(Value::List(items)) => out.push(items),
            Lazy::Value(_) => {}
            Lazy::Binary(_, left, right) => {
                left.lists(out);
                right.lists(out);
            }
            Lazy::Unary(_, operand) => operand.lists(out),
        }
    }
}

/// Computes `lazy`, which holds a list, element by element: once every list is known to have the
/// same length, the whole expression is evaluated for the first element, then the second, and so
/// on, so `a / b + c / d` reports the first element where either division fails.
///
/// For example, `[1, 2] * 2 + [10, 20]` gives `[12, 24]`.
fn elementwise(lazy: &Lazy) -> EvalResult<Value> {
    let mut lists = Vec::new();
    lazy.lists(&mut lists);
    let len = lists[0].len();
    if let Some(other) = lists.iter().find(|list| list.len() != len) {
        let lengths = [len as i64, other.len() as i64];
        return Err(stdlib::fill(stdlib::LENGTHS_DIFFER, &lengths).into());
    }
    let items = (0..len)
        .map(|index| lazy.at(index))
        .collect::<EvalResult<Vec<Value>>>()?;
    Ok(Value::list(items))
}

/// Applies the binary operator `op` to two numbers, or `+` to two strings.
/// Applies `op` to two evaluated operands, computing it at once when both are numbers or
/// strings and leaving it for [`elementwise`] when either holds a list.
///
/// For example, `combine(Add, 2, 3)` is the value 5, while `combine(Add, [1, 2], 3)` is a
/// `Lazy::Binary` that adds 3 to each element.
fn combine(op: Operator, left: Lazy, right: Lazy) -> EvalResult<Lazy> {
    match (left, right) {
        (Lazy::Value(lhs), Lazy::Value(rhs))
            if !matches!(lhs, Value::List(_)) && !matches!(rhs, Value::List(_)) =>
        {
            Ok(Lazy::Value(binary(op, lhs, rhs)?))
        }
        (left, right) => Ok(Lazy::Binary(op, Box::new(left), Box::new(right))),
    }
}

fn binary(op: Operator, lhs: Value, rhs: Value) -> EvalResult<Value> {
    match (lhs, rhs) {
        // the exponent is always an int, even for a float base
        (Value::Int(_), Value::Int(r)) if op == Operator::Power && r < 0 => {
            Err("negative exponent".into())
        }
        (Value::Int(l), Value::Int(r)) if op == Operator::Power => {
            Ok(Value::Int(stdlib::int_pow(l, r)))
        }
        (Value::Float(l), Value::Int(r)) if op == Operator::Power => {
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
        (Value::Bytes(l), Value::Bytes(r)) if matches!(op, Operator::Add) => {
            Ok(Value::Bytes([&l[..], &r[..]].concat().into()))
        }
        (Value::Str(l), Value::Str(r)) if matches!(op, Operator::Add) => {
            Ok(Value::Str(format!("{l}{r}").into()))
        }
        _ => Err("Type mismatch in binary operation".into()),
    }
}

/// Applies the unary operator `op` to a value.
fn unary(op: UnaryOp, value: Value) -> EvalResult<Value> {
    match op {
        UnaryOp::Not => Ok(Value::Bool(!value.is_true())),
        UnaryOp::UnaryAdd => Ok(value),
        UnaryOp::UnarySub => match value {
            Value::Float(x) => Ok(Value::Float(-x)),
            other => Ok(Value::Int(other.as_int()?.wrapping_neg())),
        },
    }
}

/// Calls the method `name`, which changes its receiver, on `map` with `args`.
///
/// For example, `pop` with the key `"a"` removes the entry for `"a"` and returns `some` of its
/// value, or `none` if the map has none.
fn change_map(map: &mut MapValue, name: &str, args: &[Value]) -> EvalResult<Value> {
    match (name, args) {
        ("pop", [key]) => Ok(map.remove(&MapKey::of(key)?).map_or_else(none, some)),
        _ => Err(format!("a map has no method '{name}' that changes it").into()),
    }
}

/// Calls the method `name`, which changes its receiver, on `items` with `args`.
///
/// For example, `append` with `[Value::Int(1)]` pushes 1 and returns `Value::Unit`, and `pop`
/// with no arguments removes the last element and returns `some` of it, or `none` if there is
/// none.
fn change_list(items: &mut Vec<Value>, name: &str, args: Vec<Value>) -> EvalResult<Value> {
    match (name, &args[..]) {
        ("append", [item]) => {
            items.push(item.clone());
            Ok(Value::Unit)
        }
        ("pop", []) => Ok(items.pop().map_or_else(none, some)),
        ("pop", [index]) => {
            if items.is_empty() {
                return Err(stdlib::POP_EMPTY.into());
            }
            let index = index.as_int()?;
            let len = items.len() as i64;
            let position = if index < 0 { index + len } else { index };
            if !(0..len).contains(&position) {
                return Err(stdlib::POP_RANGE.into());
            }
            Ok(items.remove(position as usize))
        }
        ("insert", [index, item]) => {
            let index = index.as_int()?;
            let len = items.len() as i64;
            let position = if index < 0 { index + len } else { index };
            if !(0..=len).contains(&position) {
                return Err(stdlib::fill(stdlib::INDEX_RANGE, &[index, len]).into());
            }
            items.insert(position as usize, item.clone());
            Ok(Value::Unit)
        }
        ("extend", [other]) => {
            items.extend(other.as_list()?.iter().cloned());
            Ok(Value::Unit)
        }
        ("sort", []) => {
            // the order is total on what a program can see, so any sort gives the same list
            items.sort_unstable_by(|a, b| match (a, b) {
                (Value::Int(a), Value::Int(b)) => a.cmp(b),
                (Value::Float(a), Value::Float(b)) => {
                    stdlib::float_key(*a).cmp(&stdlib::float_key(*b))
                }
                (Value::Str(a), Value::Str(b)) => a.as_bytes().cmp(b.as_bytes()),
                (Value::Bytes(a), Value::Bytes(b)) => a.cmp(b),
                _ => std::cmp::Ordering::Equal,
            });
            Ok(Value::Unit)
        }
        _ => Err(format!("{name}() is called on a list with the wrong arguments").into()),
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

/// A `?` found an `err` or a `none`, which the function it is in returns. It travels as an
/// error from the expression to the statement holding it, which returns it.
///
/// For example, in `n = int(s)?`, an `err("...")` stops evaluating the statement, which
/// returns that `err` from the function.
#[derive(Debug)]
struct Propagate(Value);

impl std::fmt::Display for Propagate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "'?' returned {}", self.0.display(true))
    }
}

impl std::error::Error for Propagate {}

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
    /// function uses several levels per call, so `fn f(n: int) -> int: ret f(n - 1)` uses about
    /// three.
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
    /// The global variables, by id, empty until assigned and again once dropped at their last
    /// use.
    globals: Vec<Option<Value>>,
    global_names: Interner,
    /// The local variables of each active call, the innermost call last.
    frames: Vec<Frame>,
    /// User-defined functions, by id, empty for a name code calls that is not defined yet.
    functions: Vec<Option<Rc<Function>>>,
    function_names: Interner,
    /// The enums the program defines, by id, and their ids by linked name.
    enums: Vec<EnumInfo>,
    enum_ids: HashMap<String, usize>,
    /// The structs the program defines, by id, and their ids by linked name.
    structs: Vec<Rc<StructDef>>,
    struct_ids: HashMap<String, usize>,
    /// For each function, the globals it may read, which must not move into a call to it.
    global_reads: GlobalReads,
    /// How each method call of the running body was resolved, and its function values and calls
    /// of them, by span.
    resolved: Rc<Calls>,
    /// The type arguments of the running copy of a generic function, empty outside one.
    types: Types,
    /// The type arguments of the next function [`Interpreter::invoke_body`] calls, if it is a
    /// copy of a generic function.
    next_types: Option<Types>,
    /// Whether globals can move at their last use, which is never in an interactive session,
    /// since a later entry may read them.
    moves_globals: bool,
    /// An empty set of type arguments, which every call outside a generic function runs with.
    no_types: Types,
    /// Where `print` writes, which is stdout unless a caller captures it.
    out: Box<dyn Write + 'out>,
    /// Where `eprint` writes, which is stderr unless a caller captures it.
    errors: Box<dyn Write + 'out>,
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
    /// The `random` module's generator, seeded from the system's entropy on first use unless
    /// `random.seed` seeds it first. It lasts the whole run, or the whole interactive session.
    rng: Option<Rng>,
}

/// Returns where `eprint` writes by default: stderr, or nowhere under `cargo fuzz`, whose inputs
/// would otherwise flood it.
fn default_errors() -> Box<dyn Write> {
    if cfg!(fuzzing) {
        Box::new(std::io::sink())
    } else {
        Box::new(std::io::stderr())
    }
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
            globals: vec![],
            global_names: Interner::default(),
            frames: vec![],
            functions: vec![],
            function_names: Interner::default(),
            enums: vec![],
            enum_ids: HashMap::new(),
            structs: vec![],
            struct_ids: HashMap::new(),
            global_reads: GlobalReads::default(),
            resolved: Rc::default(),
            types: Rc::default(),
            next_types: None,
            moves_globals: false,
            no_types: Rc::default(),
            out: Box::new(out),
            errors: default_errors(),
            input: Box::new(std::io::empty()),
            args: vec![],
            limits,
            fuel_used: 0,
            depth: 0,
            calls: 0,
            copies: 0,
            rng: None,
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

    /// Makes `eprint` write to `errors` instead of stderr.
    ///
    /// For example, `Interpreter::with_output(out, limits).with_errors(&mut buffer)` collects
    /// what `eprint` writes in a `Vec<u8>` named `buffer`.
    pub fn with_errors(mut self, errors: impl Write + 'out) -> Self {
        self.errors = Box::new(errors);
        self
    }

    /// Calls the functions of `impl` blocks and function values as `calls`, the checker's
    /// resolution of each, says, which a program with an `impl` block or a function value needs.
    pub fn with_calls(mut self, calls: Calls) -> Self {
        self.resolved = Rc::new(calls);
        self
    }

    /// Makes `args` return `args` instead of an empty list.
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.args = args;
        self
    }

    /// Reads one line for `input`, without its newline, or `None` at the end of the input. A
    /// last line without a newline is still a line.
    ///
    /// A read error counts as the end of the input, after any bytes read before it, as in
    /// compiled code, and bytes that are not UTF-8 become U+FFFD.
    fn read_line(&mut self) -> Option<String> {
        let mut line = Vec::new();
        // what was read before an error is still a line
        let _ = self.input.read_until(b'\n', &mut line);
        if line.is_empty() {
            return None;
        }
        if line.last() == Some(&b'\n') {
            line.pop();
        }
        Some(String::from_utf8_lossy(&line).into_owned())
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
    /// For example, the body of `if x: ret 1` yields `ControlFlow::Return(Int(1))` when `x` is
    /// true, which the enclosing function then returns.
    fn eval_block(&mut self, body: &[code::Stmt]) -> EvalResult<ControlFlow> {
        for stmt in body {
            let flow = self.eval_stmt(stmt)?;
            if !matches!(flow, ControlFlow::None) {
                return Ok(flow);
            }
        }
        Ok(ControlFlow::None)
    }

    /// Runs the definitions in `body`, a program's or an interactive entry's, enums and structs
    /// first, so the functions resolved after them know every type they name, then resolves the
    /// rest of `body`, whose statements use variables for the last time as `last_uses` says.
    fn define_all(&mut self, body: &[Stmt], last_uses: &LastUses) -> Vec<code::Stmt> {
        let is_type = |stmt: &Stmt| {
            matches!(
                stmt.kind,
                StmtKind::EnumDef { .. } | StmtKind::StructDef { .. }
            )
        };
        // functions, enums, and structs can be used before their definition
        for stmt in body.iter().filter(|stmt| is_type(stmt)) {
            self.define_stmt(stmt);
        }
        for stmt in body
            .iter()
            .filter(|stmt| is_definition(stmt) && !is_type(stmt))
        {
            self.define_stmt(stmt);
        }
        let calls = self.resolved.clone();
        code::Resolver::new(self, &calls, None, last_uses).body(body)
    }

    pub fn evaluate(&mut self, module: &Mod) -> Result<(), Box<dyn std::error::Error>> {
        match module {
            Mod::Module { body } => {
                let mut assigned = vec![];
                collect_assigned(body, &mut assigned);
                let globals = assigned.into_iter().map(|(name, _)| name).collect();
                self.global_reads = global_reads(body, &globals);
                self.moves_globals = true;
                let last_uses = LastUses::new(body, &self.global_reads, &HashSet::new());
                let program = self.define_all(body, &last_uses);
                for stmt in &program {
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
    pub fn run_entry(&mut self, body: &[Stmt], calls: Calls) -> EvalResult<()> {
        // a later entry may read any global, so none moves
        self.moves_globals = false;
        self.resolved = Rc::new(calls);
        let entry = self.define_all(body, &LastUses::default());
        for stmt in &entry {
            match &stmt.kind {
                code::StmtKind::Expr(value) => {
                    let value = self.eval_expr(value)?;
                    if !matches!(value, Value::Unit) {
                        let mut text = String::new();
                        self.write_value(&mut text, &value, true)?;
                        writeln!(self.out, "{text}")?;
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

    /// Executes one statement and reports whether it breaks, continues, or returns, then drops
    /// the variables it used for the last time.
    fn eval_stmt(&mut self, stmt: &code::Stmt) -> EvalResult<ControlFlow> {
        self.nested(|this| {
            let flow = match this.eval_stmt_unguarded(stmt) {
                Ok(flow) => flow,
                // a `?` in the statement returns what it found
                Err(error) => {
                    return match error.downcast::<Propagate>() {
                        Ok(propagate) => Ok(ControlFlow::Return(propagate.0)),
                        Err(error) => Err(error),
                    };
                }
            };
            // returning drops the whole frame anyway
            if !matches!(stmt.kind, code::StmtKind::Return(_)) {
                for &var in &stmt.last_uses {
                    this.drop_var(var, None);
                }
            }
            Ok(flow)
        })
    }

    /// Returns the id of the function `name`, giving it one if it has none yet.
    fn function_id(&mut self, name: &str) -> usize {
        let id = self.function_names.id(name);
        if self.functions.len() <= id {
            self.functions.resize(id + 1, None);
        }
        id
    }

    /// Returns the id of the global `name`, giving it one if it has none yet.
    fn global_id(&mut self, name: &str) -> usize {
        let id = self.global_names.id(name);
        if self.globals.len() <= id {
            self.globals.resize(id + 1, None);
        }
        id
    }

    /// Returns whether a function named `name` is defined.
    fn has_function(&self, name: &str) -> bool {
        self.function_names
            .get(name)
            .is_some_and(|id| self.functions[id].is_some())
    }

    /// Runs the definition `stmt`: of a function, the functions of an `impl` block or a trait,
    /// an enum, or a struct, replacing any earlier one of the same name.
    fn define_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name, args, body, ..
            } => self.define(name, &args.args, body),
            // a trait's default methods are functions like any others, named by the trait, and
            // its methods without a body are only the types'
            StmtKind::ImplDef { name, methods, .. } | StmtKind::TraitDef { name, methods, .. } => {
                for method in methods {
                    if let StmtKind::FunctionDef {
                        name: method,
                        args,
                        body,
                        ..
                    } = &method.kind
                        && !body.is_empty()
                    {
                        self.define(&format!("{name}.{method}"), &args.args, body);
                    }
                }
            }
            StmtKind::EnumDef { name, variants, .. } => {
                let short = name.rsplit('.').next().unwrap_or(name);
                let variants = variants
                    .iter()
                    .map(|v| {
                        let shown = if prints_bare(name) {
                            v.name.clone()
                        } else {
                            format!("{short}.{}", v.name)
                        };
                        (v.name.clone(), shown.into())
                    })
                    .collect();
                let info = EnumInfo {
                    name: name.clone(),
                    variants,
                };
                match self.enum_ids.get(name) {
                    Some(&id) => self.enums[id] = info,
                    None => {
                        self.enum_ids.insert(name.clone(), self.enums.len());
                        self.enums.push(info);
                    }
                }
            }
            StmtKind::StructDef { name, fields, .. } => {
                let def = Rc::new(StructDef {
                    name: struct_display(name).into(),
                    linked: name.as_str().into(),
                    fields: fields.iter().map(|f| f.name.clone()).collect(),
                    display: self.function_id(&format!("{name}.display")),
                    eq: self.function_id(&format!("{name}.eq")),
                    cmp: self.function_id(&format!("{name}.cmp")),
                });
                match self.struct_ids.get(name) {
                    Some(&id) => self.structs[id] = def,
                    None => {
                        self.struct_ids.insert(name.clone(), self.structs.len());
                        self.structs.push(def);
                    }
                }
            }
            _ => {}
        }
    }

    /// Defines the function `name` with the parameters `args` and the body `body`, resolving the
    /// body against the definitions so far and the check that saw it.
    fn define(&mut self, name: &str, args: &[Arg], body: &[Stmt]) {
        let layout = Layout::new(args, body);
        let last_uses = LastUses::new(body, &self.global_reads, &kept_at_exit(args));
        let resolved = self.resolved.clone();
        let body = code::Resolver::new(self, &resolved, Some(&layout), &last_uses).block(body);
        let id = self.function_id(name);
        self.functions[id] = Some(Rc::new(Function {
            name: name.into(),
            params: args.len(),
            layout: Rc::new(layout),
            body,
            resolved,
        }));
    }

    /// Returns the name of the variable `var`, for errors.
    fn var_name(&self, var: code::Var) -> &str {
        match var {
            code::Var::Local(slot) => self
                .frames
                .last()
                .and_then(|frame| frame.layout.names.get(slot))
                .map_or("?", String::as_str),
            code::Var::Global(id) => &self.global_names.names[id],
        }
    }

    /// Drops the variable `var` of the running body if it holds a list, the only value a
    /// change can copy, directly or in a tuple, or a handle (see [`Value::drops_early`]): a
    /// local of the running function, or a global at the top level, unless globals cannot move
    /// or the function `callee` may read it.
    fn drop_var(&mut self, var: code::Var, callee: Option<&str>) {
        match (var, self.frames.last_mut()) {
            (code::Var::Local(slot), Some(frame)) => {
                if let Some(value) = frame.slots.get_mut(slot)
                    && value.as_ref().is_some_and(Value::drops_early)
                {
                    *value = None;
                }
            }
            (code::Var::Global(id), None) if self.moves_globals => {
                let name = &self.global_names.names[id];
                if self.globals[id].as_ref().is_some_and(Value::drops_early)
                    && callee.is_none_or(|callee| {
                        self.global_reads
                            .get(callee)
                            .is_none_or(|reads| !reads.contains(name))
                    })
                {
                    self.globals[id] = None;
                }
            }
            _ => {}
        }
    }

    /// Takes the value of `field`, a field of a variable such as `b.items`, out of the variable
    /// if it may hold a list, leaving no value in its place, so that the call to `callee` it is
    /// passed to holds the only reference, as [`Interpreter::drop_var`] does for a whole
    /// variable.
    fn take_field(&mut self, field: &code::Place, callee: &str) {
        let global = self.frames.is_empty();
        if global
            && (!self.moves_globals
                || self
                    .global_reads
                    .get(callee)
                    .is_some_and(|reads| reads.contains(self.var_name(field.root))))
        {
            return;
        }
        let Some(mut value) = self.variable_mut(field.root) else {
            return;
        };
        for step in &field.steps {
            let (code::PlaceStep::Field(name), Value::Struct(inner)) = (step, value) else {
                return;
            };
            let inner = Rc::make_mut(inner);
            let Ok(position) = inner.field(name) else {
                return;
            };
            value = &mut inner.fields[position];
        }
        if value.drops_early() {
            *value = Value::Unit;
        }
    }

    /// Takes what `moves` names out of the caller for its call of `callee`, since nothing reads
    /// it after the call.
    fn apply_moves(&mut self, moves: &[code::Move], callee: &str) {
        for step in moves {
            match step {
                code::Move::Variable(var) => self.drop_var(*var, Some(callee)),
                code::Move::Field(place) => self.take_field(place, callee),
            }
        }
    }

    fn eval_stmt_unguarded(&mut self, stmt: &code::Stmt) -> EvalResult<ControlFlow> {
        use code::StmtKind as S;
        match &stmt.kind {
            S::Return(value) => {
                let value = match value {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Unit,
                };
                Ok(ControlFlow::Return(value))
            }
            S::Assign { targets, value } => {
                let rhs = self.eval_expr(value)?;
                for target in targets {
                    match target {
                        code::Target::Unpack(unpack) => self.assign_target(unpack, &rhs)?,
                        code::Target::Element { list, index, span } => {
                            let (root, indexes, spans) = self.eval_place(list)?;
                            let index = self.eval_expr(index)?;
                            self.store_at(root, &indexes, spans, index, *span, &rhs)?;
                        }
                        code::Target::Field(place) => {
                            let (root, steps, spans) = self.eval_place(place)?;
                            let field = self.place_value(root, &steps, spans)?;
                            *field = rhs.clone();
                        }
                        code::Target::Invalid => return Err("Invalid assignment target".into()),
                    }
                }
                Ok(ControlFlow::None)
            }
            S::AugAssign { target, op, value } => {
                match target {
                    code::AugTarget::Var { var, span } => {
                        let old = self.read_var(*var).map_err(|e| located(e, *span))?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        self.set_var(*var, new);
                    }
                    code::AugTarget::Element {
                        list,
                        index,
                        spans: target_spans,
                        span,
                    } => {
                        // the indexes once, then the old element, then the value
                        let (root, indexes, spans) = self.eval_place(list)?;
                        let index = self.eval_expr(index)?;
                        let mut steps = indexes.clone();
                        steps.push(PlaceStep::Index(index.clone()));
                        let old = self.read_place(root, &steps, target_spans)?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        // the value cannot change the list or map, which only this statement can
                        self.store_at(root, &indexes, spans, index, *span, &new)?;
                    }
                    code::AugTarget::Field(place) => {
                        // the indexes once, then the old field, then the value
                        let (root, steps, spans) = self.eval_place(place)?;
                        let old = self.read_place(root, &steps, spans)?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        *self.place_value(root, &steps, spans)? = new;
                    }
                    code::AugTarget::Invalid => return Err("Invalid assignment target".into()),
                }
                Ok(ControlFlow::None)
            }
            S::ForRange {
                target,
                bounds,
                body,
            } => {
                // the bounds are evaluated once, left to right
                let mut values = Vec::with_capacity(bounds.len());
                for bound in bounds {
                    values.push(self.eval_expr(bound)?.as_int()?);
                }
                let (start, end) = match values[..] {
                    [end] => (0, end),
                    [start, end] => (start, end),
                    _ => return Err("range() takes 1 or 2 arguments".into()),
                };
                for i in start..end {
                    self.assign_target(target, &Value::Int(i))?;
                    if let Some(flow) = self.run_iteration(body)? {
                        return Ok(flow);
                    }
                }
                Ok(ControlFlow::None)
            }
            S::For { target, iter, body } => {
                // a string gives each character, as a string of the bytes that make it up, and
                // bytes each byte's value
                let list = self.eval_expr(iter)?;
                if let Value::Bytes(bytes) = &list {
                    for &b in bytes.iter() {
                        self.assign_target(target, &Value::Int(b as i64))?;
                        if let Some(flow) = self.run_iteration(body)? {
                            return Ok(flow);
                        }
                    }
                    return Ok(ControlFlow::None);
                }
                if let Value::Str(text) = &list {
                    for ch in text.chars() {
                        self.assign_target(target, &Value::Str(ch.to_string().into()))?;
                        if let Some(flow) = self.run_iteration(body)? {
                            return Ok(flow);
                        }
                    }
                    return Ok(ControlFlow::None);
                }
                // a map gives its keys, as they were, so changing the map copies it first
                if let Value::Map(map) = &list {
                    let map = map.clone();
                    for (key, _) in map.iter() {
                        self.assign_target(target, &key.value())?;
                        if let Some(flow) = self.run_iteration(body)? {
                            return Ok(flow);
                        }
                    }
                    return Ok(ControlFlow::None);
                }
                // the loop holds the list as it was, so changing the variable copies it first
                let items = list.as_list()?.clone();
                for item in items.iter() {
                    self.assign_target(target, item)?;
                    if let Some(flow) = self.run_iteration(body)? {
                        return Ok(flow);
                    }
                }
                Ok(ControlFlow::None)
            }
            S::While { test, body } => {
                loop {
                    if !self.eval_expr(test)?.is_true() {
                        break;
                    }
                    if let Some(flow) = self.run_iteration(body)? {
                        return Ok(flow);
                    }
                }
                Ok(ControlFlow::None)
            }
            S::If { test, body, orelse } => {
                if self.eval_expr(test)?.is_true() {
                    self.eval_block(body)
                } else {
                    self.eval_block(orelse)
                }
            }
            S::Expr(value) => {
                self.eval_expr(value)?;
                Ok(ControlFlow::None)
            }
            S::Match { subject, cases } => {
                let value = self.eval_expr(subject)?;
                for (pattern, body) in cases {
                    let mut bindings = vec![];
                    if self.pattern_matches(pattern, &value, &mut bindings) {
                        for (var, value) in bindings {
                            self.set_var(var, value);
                        }
                        return self.eval_block(body);
                    }
                }
                Err("no case of the match fits the value".into())
            }
            S::Break => Ok(ControlFlow::Break),
            S::Continue => Ok(ControlFlow::Continue),
            S::Fail(message) => Err(message.clone().into()),
        }
    }

    /// Returns whether `pattern` matches `value`, pushing the value each of its names binds to
    /// `bindings`.
    ///
    /// For example, `some(x)` matches `some(3)` and binds `x` to 3, but does not match `none`.
    fn pattern_matches(
        &self,
        pattern: &code::Pattern,
        value: &Value,
        bindings: &mut Vec<(code::Var, Value)>,
    ) -> bool {
        match pattern {
            code::Pattern::Wildcard => true,
            code::Pattern::Bind(var) => {
                bindings.push((*var, value.clone()));
                true
            }
            code::Pattern::Variant { id, name, fields } => {
                let Value::Enum(found) = value else {
                    return false;
                };
                let variants = &self.enums[*id].variants;
                let Some(tag) = variants.iter().position(|(variant, _)| variant == name) else {
                    return false;
                };
                found.tag == tag
                    && found.fields.len() == fields.len()
                    && fields
                        .iter()
                        .zip(&found.fields)
                        .all(|(field, value)| self.pattern_matches(field, value, bindings))
            }
            code::Pattern::Never => false,
        }
    }

    /// Makes the variant `variant` of the enum with the id `id` holding `fields`.
    fn variant(&self, id: usize, variant: &str, fields: Vec<Value>) -> EvalResult<Value> {
        let info = &self.enums[id];
        let tag = info
            .variants
            .iter()
            .position(|(name, _)| name == variant)
            .ok_or_else(|| format!("'{}' has no variant '{variant}'", info.name))?;
        Ok(Value::Enum(Rc::new(EnumValue {
            name: info.variants[tag].1.clone(),
            tag,
            fields,
        })))
    }

    /// Runs one iteration of a loop body, spending fuel, and returns how the loop should end, or
    /// `None` to keep looping.
    ///
    /// For example, a `break` ends the loop normally with `ControlFlow::None`, while a `ret`
    /// passes its `ControlFlow::Return` on to the enclosing function.
    fn run_iteration(&mut self, body: &[code::Stmt]) -> EvalResult<Option<ControlFlow>> {
        self.burn_fuel()?;
        Ok(match self.eval_block(body)? {
            ControlFlow::Break => Some(ControlFlow::None),
            ControlFlow::Return(value) => Some(ControlFlow::Return(value)),
            ControlFlow::Continue | ControlFlow::None => None,
        })
    }

    /// Evaluates the arithmetic `expr`, computing each operation whose operands are both numbers
    /// as soon as they are known, left to right, and leaving those with a list among their
    /// operands for [`elementwise`].
    ///
    /// For example, `a * (2 + 3)` for a list `a` evaluates `a`, then `2 + 3`, and returns a
    /// multiplication of `a` by 5 for each element.
    fn arithmetic(&mut self, expr: &code::Expr) -> EvalResult<Lazy> {
        self.arithmetic_unlocated(expr)
            .map_err(|e| located(e, expr.span))
    }

    fn arithmetic_unlocated(&mut self, expr: &code::Expr) -> EvalResult<Lazy> {
        self.nested(|this| match &expr.kind {
            code::ExprKind::Binary(op, left, right) => {
                let left = this.arithmetic(left)?;
                let right = this.arithmetic(right)?;
                combine(*op, left, right)
            }
            code::ExprKind::Unary(op @ (UnaryOp::UnaryAdd | UnaryOp::UnarySub), operand) => {
                match this.arithmetic(operand)? {
                    Lazy::Value(value) if !matches!(value, Value::List(_)) => {
                        Ok(Lazy::Value(unary(*op, value)?))
                    }
                    operand => Ok(Lazy::Unary(*op, Box::new(operand))),
                }
            }
            _ => Ok(Lazy::Value(this.eval_expr(expr)?)),
        })
    }

    /// Computes `old op value` for an augmented assignment such as `x += value`, evaluating
    /// `value` after `old` was read, as `x = x + value` would. An error the operation itself
    /// raises points at the statement, `span`.
    fn augmented(
        &mut self,
        op: Operator,
        old: Value,
        value: &code::Expr,
        span: Span,
    ) -> EvalResult<Value> {
        let right = self.arithmetic(value)?;
        let result = match combine(op, Lazy::Value(old), right) {
            Ok(Lazy::Value(value)) => Ok(value),
            Ok(lazy) => elementwise(&lazy),
            Err(e) => Err(e),
        };
        result.map_err(|e| located(e, span))
    }

    /// Reads the value that `root` leads to through `steps`, as [`Interpreter::place_value`]
    /// walks it, failing at the same `spans`, but without changing anything.
    ///
    /// For example, `grid[1][0] += 3` reads `grid[1][0]` with the root `grid` and the indexes 1
    /// and 0.
    fn read_place(
        &self,
        root: code::Var,
        steps: &[PlaceStep],
        spans: &[Span],
    ) -> EvalResult<Value> {
        let mut current = self.read_var(root)?;
        for (step, &span) in steps.iter().zip(spans) {
            let next = match step {
                PlaceStep::Index(index) => match &current {
                    Value::Map(map) => map
                        .get(&MapKey::of(index)?)
                        .map_err(|e| located(e, span))?
                        .clone(),
                    _ => {
                        let items = current.as_list()?;
                        let position = list_position(index.as_int()?, items.len())
                            .map_err(|e| located(e, span))?;
                        items[position].clone()
                    }
                },
                PlaceStep::Field(name) => {
                    let value = current.as_struct()?;
                    value.fields[value.field(name)?].clone()
                }
            };
            current = next;
        }
        Ok(current)
    }

    /// Evaluates one expression to a value, so `1 + 2` evaluates to `Int(3)`, giving an error
    /// the position of the innermost expression that raised it.
    fn eval_expr(&mut self, expr: &code::Expr) -> EvalResult<Value> {
        self.nested(|this| this.eval_expr_unguarded(expr))
            .map_err(|e| located(e, expr.span))
    }

    /// Evaluates `exprs` left to right, as a call's arguments are.
    fn eval_exprs(&mut self, exprs: &[code::Expr]) -> EvalResult<Vec<Value>> {
        let mut values = Vec::with_capacity(exprs.len());
        for expr in exprs {
            values.push(self.eval_expr(expr)?);
        }
        Ok(values)
    }

    fn eval_expr_unguarded(&mut self, expr: &code::Expr) -> EvalResult<Value> {
        use code::ExprKind as E;
        match &expr.kind {
            E::Fail(message) => Err(message.clone().into()),
            E::Try(value) => {
                let value = self.eval_expr(value)?;
                let Value::Enum(variant) = &value else {
                    return Err("'?' needs a result or an option".into());
                };
                // `ok` and `some` hold the value, and `err` and `none` are returned
                match (&*variant.name, variant.fields.first()) {
                    ("ok" | "some", Some(inner)) => Ok(inner.clone()),
                    _ => Err(Box::new(Propagate(value))),
                }
            }
            // every value is evaluated before any is written, like a call's arguments
            E::JoinedStr(values) => {
                let values = self.eval_exprs(values)?;
                let mut text = String::new();
                for value in &values {
                    self.write_value(&mut text, value, false)?;
                }
                Ok(Value::Str(text.into()))
            }
            E::Dict(entries) => {
                let mut map = MapValue::default();
                for (key, value) in entries {
                    let key = MapKey::of(&self.eval_expr(key)?)?;
                    map.insert(key, self.eval_expr(value)?);
                }
                Ok(Value::Map(Rc::new(map)))
            }
            // short-circuiting
            E::BoolOp(op, values) => {
                let mut result = self.eval_expr(&values[0])?;
                for value in &values[1..] {
                    let done = match op {
                        BoolOp::And => !result.is_true(),
                        BoolOp::Or => result.is_true(),
                    };
                    if done {
                        return Ok(result);
                    }
                    result = self.eval_expr(value)?;
                }
                Ok(result)
            }
            E::Binary(..) | E::Unary(UnaryOp::UnaryAdd | UnaryOp::UnarySub, _) => {
                match self.arithmetic(expr)? {
                    Lazy::Value(value) => Ok(value),
                    lazy => elementwise(&lazy),
                }
            }
            E::Unary(op, operand) => {
                let val = self.eval_expr(operand)?;
                unary(*op, val)
            }
            E::Membership(op, needle, container) => {
                // the value looked for, then where
                let needle = self.eval_expr(needle)?;
                let container = self.eval_expr(container)?;
                let found = self.contains(&container, &needle)?;
                Ok(Value::Bool(found == (*op == CompOp::In)))
            }
            E::Compare(left, comparisons) => {
                let mut current = self.eval_expr(left)?;
                for (op, comparator) in comparisons {
                    let next = self.eval_expr(comparator)?;
                    let holds = match op {
                        CompOp::Equal => self.values_equal(&current, &next, 0)?,
                        CompOp::NotEqual => !self.values_equal(&current, &next, 0)?,
                        CompOp::LessThan => {
                            self.order(&current, &next)?.is_some_and(Ordering::is_lt)
                        }
                        CompOp::LessThanEqual => {
                            self.order(&current, &next)?.is_some_and(Ordering::is_le)
                        }
                        CompOp::GreaterThan => {
                            self.order(&current, &next)?.is_some_and(Ordering::is_gt)
                        }
                        CompOp::GreaterThanEqual => {
                            self.order(&current, &next)?.is_some_and(Ordering::is_ge)
                        }
                        CompOp::In | CompOp::NotIn => {
                            return Err("'in' cannot be chained with other comparisons".into());
                        }
                    };
                    if !holds {
                        return Ok(Value::Bool(false));
                    }
                    current = next;
                }
                Ok(Value::Bool(true))
            }
            E::Call(call) => self.eval_call(expr.span, call),
            E::Variant { id, name, args } => {
                let values = self.eval_exprs(args)?;
                self.variant(*id, name, values)
            }
            E::Field(value, attr) => {
                let value = self.eval_expr(value)?;
                let value = value.as_struct()?;
                Ok(value.fields[value.field(attr)?].clone())
            }
            E::Constant(value) => Ok(value.clone()),
            E::Index(value, index) => {
                let list = self.eval_expr(value)?;
                if let Value::Map(map) = &list {
                    let key = MapKey::of(&self.eval_expr(index)?)?;
                    return Ok(map.get(&key)?.clone());
                }
                let index = self.eval_expr(index)?.as_int()?;
                if let Value::Tuple(fields) = &list {
                    // the checker allows only a constant index in range
                    let position = list_position(index, fields.len())?;
                    return Ok(fields[position].clone());
                }
                if let Value::Bytes(bytes) = &list {
                    let position = position(index, bytes.len(), stdlib::BYTES_INDEX_RANGE)?;
                    return Ok(Value::Int(bytes[position] as i64));
                }
                if let Value::Str(text) = &list {
                    let position = position(index, text.len(), stdlib::STRING_INDEX_RANGE)?;
                    return Ok(Value::Str(stdlib::str_at(text, position)?.into()));
                }
                let items = list.as_list()?;
                let position = list_position(index, items.len())?;
                Ok(items[position].clone())
            }
            E::Slice {
                value,
                lower,
                upper,
                step,
            } => {
                let list = self.eval_expr(value)?;
                let mut bound = |bound: &Option<Box<code::Expr>>| -> EvalResult<Option<i64>> {
                    bound
                        .as_ref()
                        .map(|bound| self.eval_expr(bound)?.as_int())
                        .transpose()
                };
                let (lower, upper, step) = (bound(lower)?, bound(upper)?, bound(step)?);
                if let Value::Bytes(bytes) = &list {
                    let (start, step, count) =
                        stdlib::slice_indices(bytes.len(), lower, upper, step)?;
                    let sliced = stdlib::bytes_slice(bytes, start, step, count);
                    return Ok(Value::Bytes(sliced.into()));
                }
                if let Value::Str(text) = &list {
                    let (start, step, count) =
                        stdlib::slice_indices(text.len(), lower, upper, step)?;
                    let sliced = stdlib::str_slice(text, start, step, count)?;
                    return Ok(Value::Str(sliced.into()));
                }
                let items = list.as_list()?;
                let (start, step, count) = stdlib::slice_indices(items.len(), lower, upper, step)?;
                let picked = (0..count as i64)
                    .map(|k| items[(start + k * step) as usize].clone())
                    .collect();
                Ok(Value::list(picked))
            }
            E::Var(var) => self.read_var(*var),
            E::Function {
                value,
                id,
                lambda,
                types,
                captures,
            } => {
                let body = match lambda {
                    true => self.functions[*id].clone(),
                    false => None,
                };
                Ok(Value::Function(FunctionRef {
                    value: value.clone(),
                    id: *id,
                    body,
                    types: self.instance_types(types),
                    captures: self.eval_exprs(captures)?.into(),
                }))
            }
            E::List(elts) => Ok(Value::list(self.eval_exprs(elts)?)),
            E::Tuple(elts) => Ok(Value::Tuple(self.eval_exprs(elts)?.into())),
        }
    }

    /// Evaluates the call at `span`, as resolved into `call`.
    fn eval_call(&mut self, span: Span, call: &code::Call) -> EvalResult<Value> {
        match call {
            code::Call::Conversion {
                conversion,
                arg,
                types,
            } => self.eval_conversion(conversion, arg, types),
            code::Call::Value { func, args, moves } => self.call_value(func, args, moves),
            code::Call::Struct { id, name, fields } => {
                let def = self.structs[*id].clone();
                // the values in the order they are written, each into its field
                let mut values = vec![Value::Unit; def.fields.len()];
                for (field, value) in fields {
                    let value = self.eval_expr(value)?;
                    let position = def
                        .fields
                        .iter()
                        .position(|name| name == field)
                        .ok_or_else(|| format!("'{name}' has no field '{field}'"))?;
                    values[position] = value;
                }
                Ok(Value::Struct(Rc::new(StructValue {
                    def,
                    fields: values,
                })))
            }
            code::Call::Method {
                method,
                function,
                receiver,
                place,
                args,
                moves,
                types,
            } => self.call_method_of_struct(
                span,
                method,
                *function,
                receiver.as_ref(),
                place.as_ref(),
                args,
                moves,
                types,
            ),
            code::Call::Change { place, attr, args } => {
                // the receiver's indexes, then the arguments, then the change
                let (root, steps, spans) = self.eval_place(place)?;
                let values = self.eval_exprs(args)?;
                if let Value::Map(map) = self.place_value(root, &steps, spans)? {
                    let shared = Rc::strong_count(map) > 1;
                    let result = change_map(Rc::make_mut(map), attr, &values);
                    self.copies += shared as u64;
                    return result;
                }
                let items = self.place_list(root, &steps, spans)?;
                // sorted by the prelude's function, which gives the list back sorted
                if let Some(function) = stdlib::prelude_method(attr) {
                    let list = Value::List(Rc::new(items.clone()));
                    let mut args = vec![list];
                    args.extend(values);
                    let (sorted, _) = self.invoke(function, args, false)?;
                    let sorted = sorted.as_list()?.to_vec();
                    *self.place_list(root, &steps, spans)? = sorted;
                    return Ok(Value::Unit);
                }
                // values of other types sort by their `cmp`, in the prelude's sort
                if attr == "sort" && items.first().is_some_and(|v| matches!(v, Value::Struct(_))) {
                    let list = Value::List(Rc::new(items.clone()));
                    let (sorted, _) = self.invoke(stdlib::PRELUDE_SORT, vec![list], false)?;
                    let sorted = sorted.as_list()?.to_vec();
                    *self.place_list(root, &steps, spans)? = sorted;
                    return Ok(Value::Unit);
                }
                change_list(items, attr, values)
            }
            code::Call::BuiltinMethod {
                receiver,
                attr,
                args,
            } => {
                // the receiver is evaluated before the arguments
                let receiver = self.eval_expr(receiver)?;
                let values = self.eval_exprs(args)?;
                self.call_method(attr, receiver, values)
            }
            code::Call::Builtin { name, args } => {
                let values = self.eval_exprs(args)?;
                self.call(name, values)
            }
            code::Call::User {
                function,
                name,
                args,
                moves,
                types,
            } => {
                // arguments are evaluated left to right in the caller's frame
                let values = self.eval_exprs(args)?;
                self.apply_moves(moves, name);
                self.next_types = self.instance_types(types);
                let result = self.invoke_id(*function, values, false);
                self.next_types = None;
                Ok(result?.0)
            }
        }
    }

    /// Writes `value` to `out` as `print` does, with strings quoted if `nested`, and each struct
    /// whose type implements `display` itself written as its `display` method returns.
    ///
    /// For example, `[p]` writes `[(1, 2)]` when `p`'s `display` returns `"(1, 2)"`.
    fn write_value(&mut self, out: &mut String, value: &Value, nested: bool) -> EvalResult<()> {
        value.write(out, nested, 0, &mut |value, out| {
            let Value::Struct(s) = value else {
                return Ok(false);
            };
            let function = s.def.display;
            if self.functions[function].is_none() {
                return Ok(false);
            }
            if let (Value::Str(text), _) = self.invoke_id(function, vec![value.clone()], false)? {
                out.push_str(&text);
            }
            Ok(true)
        })
    }

    /// Calls the builtin method `name` on an already evaluated receiver and arguments.
    ///
    /// For example, `call_method("len", Value::Str("abc".into()), vec![])` returns `Value::Int(3)`.
    /// Returns whether two values are equal, as `==` compares them: part by part, except a
    /// struct that implements `eq`, whose `eq` says, and an enum value more than
    /// [`stdlib::MAX_PRINT_DEPTH`] deep, which fails as compiled code does.
    fn values_equal(&mut self, a: &Value, b: &Value, depth: usize) -> EvalResult<bool> {
        let all_equal = |this: &mut Self, a: &[Value], b: &[Value], depth: usize| {
            for (a, b) in a.iter().zip(b) {
                if !this.values_equal(a, b, depth)? {
                    return Ok(false);
                }
            }
            Ok::<bool, Box<dyn std::error::Error>>(a.len() == b.len())
        };
        match (a, b) {
            (Value::Tuple(x), Value::Tuple(y)) => all_equal(self, x, y, depth),
            (Value::Struct(x), Value::Struct(y)) => {
                let function = x.def.eq;
                if self.functions[function].is_some() {
                    let (result, _) =
                        self.invoke_id(function, vec![a.clone(), b.clone()], false)?;
                    return Ok(matches!(result, Value::Bool(true)));
                }
                all_equal(self, &x.fields, &y.fields, depth)
            }
            (Value::Enum(x), Value::Enum(y)) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(stdlib::COMPARE_DEPTH.into());
                }
                Ok(x.tag == y.tag && all_equal(self, &x.fields, &y.fields, depth + 1)?)
            }
            _ => a.equals(b, depth),
        }
    }

    /// Returns whether `container` holds `needle`, as `needle in container` and
    /// `container.contains(needle)` test: a map an entry for the key `needle`, a list an element
    /// that `needle == element`, looking from the first and stopping at a match, and a string
    /// the text `needle`.
    ///
    /// For example, `2 in [1, 2, 3]` compares 2 with 1 and then 2, and `"" in "abc"` is true.
    fn contains(&mut self, container: &Value, needle: &Value) -> EvalResult<bool> {
        match (container, needle) {
            (Value::Map(map), key) => Ok(map.contains(&MapKey::of(key)?)),
            (Value::Str(text), Value::Str(part)) => Ok(text.contains(&**part)),
            (Value::Bytes(bytes), Value::Bytes(part)) => {
                Ok(part.is_empty() || bytes.windows(part.len()).any(|w| w == &part[..]))
            }
            (Value::List(items), _) => {
                for item in items.iter() {
                    if self.values_equal(needle, item, 0)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            _ => Err("'in' needs a str, bytes, list, or map".into()),
        }
    }

    /// Orders two values as `<` does: numbers and strings by value, or `None` if either is nan,
    /// and a struct by its `cmp`.
    fn order(&mut self, a: &Value, b: &Value) -> EvalResult<Option<Ordering>> {
        let Value::Struct(x) = a else {
            return a.compare(b);
        };
        let (result, _) = self.invoke_id(x.def.cmp, vec![a.clone(), b.clone()], false)?;
        Ok(Some(ordering_of(&result)?))
    }

    fn call_method(&mut self, name: &str, receiver: Value, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &receiver, &args[..]) {
            ("eq", _, [other]) => Ok(Value::Bool(self.values_equal(&receiver, other, 0)?)),
            ("display", _, []) => {
                let mut text = String::new();
                self.write_value(&mut text, &receiver, false)?;
                Ok(Value::Str(text.into()))
            }
            (_, Value::Socket(socket), _) => {
                // a read may wait, so what was printed shows first
                self.out.flush()?;
                let mut socket = socket.borrow_mut();
                Ok(match (name, &args[..]) {
                    ("send", [Value::Str(text)]) => result(socket.send(text).map(Value::Int)),
                    ("read_line", []) => result(socket.read_line().map(|line| match line {
                        Some(line) => some(Value::Str(line.into())),
                        None => none(),
                    })),
                    ("read", []) => result(socket.read().map(|text| Value::Str(text.into()))),
                    ("read_all", []) => {
                        result(socket.read_all().map(|text| Value::Str(text.into())))
                    }
                    ("close", []) => {
                        socket.close();
                        Value::Unit
                    }
                    ("set_timeout", [Value::Float(seconds)]) => {
                        socket.set_timeout(*seconds);
                        Value::Unit
                    }
                    ("send_bytes", [Value::Bytes(data)]) => {
                        result(socket.send_bytes(data).map(Value::Int))
                    }
                    ("read_bytes", [Value::Int(max)]) => {
                        result(socket.read_bytes(*max).map(|b| Value::Bytes(b.into())))
                    }
                    ("read_exact", [Value::Int(n)]) => {
                        result(socket.read_exact(*n).map(|b| Value::Bytes(b.into())))
                    }
                    ("read_all_bytes", []) => {
                        result(socket.read_all_bytes().map(|b| Value::Bytes(b.into())))
                    }
                    _ => return Err(format!("there is no method '{name}' of a socket").into()),
                })
            }
            (_, Value::Listener(listener), _) => {
                let mut listener = listener.borrow_mut();
                Ok(match name {
                    "accept" => {
                        self.out.flush()?;
                        result(
                            listener
                                .accept()
                                .map(|socket| Value::Socket(Rc::new(RefCell::new(socket)))),
                        )
                    }
                    "port" => Value::Int(listener.port()),
                    "close" => {
                        listener.close();
                        Value::Unit
                    }
                    "set_timeout" => {
                        if let [Value::Float(seconds)] = &args[..] {
                            listener.set_timeout(*seconds);
                        }
                        Value::Unit
                    }
                    _ => return Err(format!("there is no method '{name}' of a listener").into()),
                })
            }
            ("cmp", Value::Int(a), [Value::Int(b)]) => Ok(ordering(a.cmp(b))),
            // the order `sort` uses, in which every float has its place
            ("cmp", Value::Float(a), [Value::Float(b)]) => {
                Ok(ordering(stdlib::float_key(*a).cmp(&stdlib::float_key(*b))))
            }
            ("cmp", Value::Str(a), [Value::Str(b)]) => Ok(ordering(a.as_bytes().cmp(b.as_bytes()))),
            ("cmp", _, [other]) => Ok(ordering(
                self.order(&receiver, other)?.unwrap_or(Ordering::Equal),
            )),
            // len counts bytes, like the compiled strlen
            ("len", Value::Str(s), []) => Ok(Value::Int(s.len() as i64)),
            ("len", Value::Bytes(b), []) => Ok(Value::Int(b.len() as i64)),
            ("bytes", Value::Str(s), []) => Ok(Value::Bytes(s.as_bytes().into())),
            ("text", Value::Bytes(b), []) => Ok(result(match fs::is_text(b) {
                true => Ok(Value::Str(String::from_utf8_lossy(b).into())),
                false => Err(fs::INVALID_TEXT.to_string()),
            })),
            ("len", Value::List(items), []) => Ok(Value::Int(items.len() as i64)),
            ("len", Value::Map(map), []) => Ok(Value::Int(map.len() as i64)),
            ("len", _, _) => {
                Err("len() is called on a str, list, or map, with no arguments".into())
            }
            ("contains", _, [needle]) => Ok(Value::Bool(self.contains(&receiver, needle)?)),
            ("contains", _, _) => Err("contains() is called with one value".into()),
            ("get", Value::Map(map), [key]) => Ok(match map.find(&MapKey::of(key)?) {
                Some(value) => some(value.clone()),
                None => none(),
            }),
            ("get", _, _) => Err("get() is called on a map, with one key".into()),
            ("keys", Value::Map(map), []) => {
                let keys = map.iter().map(|(key, _)| key.value()).collect();
                Ok(Value::List(Rc::new(keys)))
            }
            ("values", Value::Map(map), []) => {
                let values = map.iter().map(|(_, value)| value.clone()).collect();
                Ok(Value::List(Rc::new(values)))
            }
            ("items", Value::Map(map), []) => {
                let items = map
                    .iter()
                    .map(|(key, value)| Value::Tuple(Rc::from([key.value(), value.clone()])))
                    .collect();
                Ok(Value::List(Rc::new(items)))
            }
            ("strip", Value::Str(s), []) => Ok(Value::Str(stdlib::strip(s).into())),
            ("strip", _, _) => Err("strip() is called on a str, with no arguments".into()),
            ("split", Value::Str(s), []) => Ok(str_list(stdlib::split_whitespace(s))),
            ("split", Value::Str(s), [Value::Str(separator)]) => {
                Ok(str_list(stdlib::split(s, separator)?))
            }
            ("split", _, _) => Err("split() is called on a str, with at most one str".into()),
            ("join", Value::Str(separator), [Value::List(items)]) => {
                let pieces = items
                    .iter()
                    .map(|item| match item {
                        Value::Str(piece) => Ok(&**piece),
                        _ => Err("join() takes a list of strs"),
                    })
                    .collect::<Result<Vec<&str>, _>>()?;
                Ok(Value::Str(pieces.join(separator).into()))
            }
            ("join", _, _) => Err("join() is called on a str, with a list of strs".into()),
            // a byte index, like len
            ("find", Value::Str(s), [Value::Str(text)]) => Ok(s
                .find(&**text)
                .map_or_else(none, |at| some(Value::Int(at as i64)))),
            ("find", _, _) => Err("find() is called on a str, with a str".into()),
            ("rfind", Value::Str(s), [Value::Str(text)]) => Ok(s
                .rfind(&**text)
                .map_or_else(none, |at| some(Value::Int(at as i64)))),
            ("rfind", _, _) => Err("rfind() is called on a str, with a str".into()),
            // rust's matches, like python's count, finds an empty text at every character boundary
            ("count", Value::Str(s), [Value::Str(text)]) => {
                Ok(Value::Int(s.matches(&**text).count() as i64))
            }
            ("count", _, _) => Err("count() is called on a str, with a str".into()),
            ("replace", Value::Str(s), [Value::Str(old), Value::Str(new), rest @ ..]) => {
                let replaced = match rest {
                    [] | [Value::Int(..0)] => s.replace(&**old, new),
                    [Value::Int(count)] => s.replacen(&**old, new, *count as usize),
                    _ => return Err("replace() takes an int count".into()),
                };
                Ok(Value::Str(replaced.into()))
            }
            ("replace", _, _) => {
                Err("replace() is called on a str, with two strs and an optional int".into())
            }
            ("lower", Value::Str(s), []) => Ok(Value::Str(s.to_ascii_lowercase().into())),
            ("lower", _, _) => Err("lower() is called on a str, with no arguments".into()),
            ("upper", Value::Str(s), []) => Ok(Value::Str(s.to_ascii_uppercase().into())),
            ("upper", _, _) => Err("upper() is called on a str, with no arguments".into()),
            ("splitlines", Value::Str(s), []) => Ok(str_list(stdlib::splitlines(s))),
            ("splitlines", _, _) => {
                Err("splitlines() is called on a str, with no arguments".into())
            }
            ("startswith", Value::Str(s), [Value::Str(prefix)]) => {
                Ok(Value::Bool(s.starts_with(&**prefix)))
            }
            ("startswith", _, _) => Err("startswith() is called on a str, with a str".into()),
            ("endswith", Value::Str(s), [Value::Str(suffix)]) => {
                Ok(Value::Bool(s.ends_with(&**suffix)))
            }
            ("endswith", _, _) => Err("endswith() is called on a str, with a str".into()),
            ("is_ok", Value::Enum(result), []) => Ok(Value::Bool(result.tag == 0)),
            ("is_err", Value::Enum(result), []) => Ok(Value::Bool(result.tag == 1)),
            ("unwrap", Value::Enum(result), []) if is_result(result) => match result.tag {
                0 => Ok(result.fields[0].clone()),
                _ => Err(self.unwrap_failure(stdlib::UNWRAP_ERR, &result.fields[0])?),
            },
            ("unwrap_or", Value::Enum(result), [default]) if is_result(result) => {
                Ok(match result.tag {
                    0 => result.fields[0].clone(),
                    _ => default.clone(),
                })
            }
            ("unwrap_err", Value::Enum(result), []) => match result.tag {
                1 => Ok(result.fields[0].clone()),
                _ => Err(self.unwrap_failure(stdlib::UNWRAP_OK, &result.fields[0])?),
            },
            ("ok", Value::Enum(result), []) => Ok(match result.tag {
                0 => some(result.fields[0].clone()),
                _ => none(),
            }),
            ("err", Value::Enum(result), []) => Ok(match result.tag {
                1 => some(result.fields[0].clone()),
                _ => none(),
            }),
            ("is_some", Value::Enum(option), []) => Ok(Value::Bool(option.tag == 1)),
            ("is_none", Value::Enum(option), []) => Ok(Value::Bool(option.tag == 0)),
            ("unwrap", Value::Enum(option), []) => match option.fields.first() {
                Some(value) => Ok(value.clone()),
                None => Err(stdlib::UNWRAP_NONE.into()),
            },
            ("unwrap_or", Value::Enum(option), [default]) => {
                Ok(option.fields.first().unwrap_or(default).clone())
            }
            _ => Err(format!("there is no method '{name}'").into()),
        }
    }

    /// Returns the error a failed `unwrap` or `unwrap_err` stops the program with: `start`, then
    /// the value the result holds instead, as `print` writes it.
    ///
    /// For example, `unwrap` of `err("odd")` fails with `cannot unwrap err: odd`.
    fn unwrap_failure(
        &mut self,
        start: &str,
        value: &Value,
    ) -> EvalResult<Box<dyn std::error::Error>> {
        let mut message = format!("{start}: ");
        self.write_value(&mut message, value, false)?;
        Ok(message.into())
    }

    /// Returns the `random` module's generator, seeding it from the system's entropy if nothing
    /// has seeded it yet.
    fn rng(&mut self) -> &mut Rng {
        self.rng.get_or_insert_with(Rng::from_entropy)
    }

    /// Calls the function value `func` evaluates to with `args`. The value is evaluated first,
    /// then the arguments, which may move into the call as `moves` says, as they do into a call
    /// of a function by its name.
    fn call_value(
        &mut self,
        func: &code::Expr,
        args: &[code::Expr],
        moves: &[code::Move],
    ) -> EvalResult<Value> {
        let Value::Function(function) = self.eval_expr(func)? else {
            return Err("only a function can be called".into());
        };
        // a closure's lambda takes its copies first
        let mut values = function.captures.to_vec();
        values.extend(self.eval_exprs(args)?);
        self.apply_moves(moves, &function.value.function);
        self.next_types = function.types.clone();
        let result = match &function.body {
            Some(body) => self.invoke_body(body.clone(), values, false)?,
            None => self.invoke_id(function.id, values, false)?,
        };
        Ok(result.0)
    }

    /// Calls a function of an `impl` block as `method` resolved the call at `span`: an associated
    /// function with `args`, or a method with the value of `receiver` as `self` and then `args`.
    /// `function` is the id of `method.function`.
    ///
    /// A method that changes `self` changes the receiver at `place`: its indexes are evaluated,
    /// then the arguments, then the receiver moves into the call, unless the method may read the
    /// global it is in, and what the method leaves in `self` replaces it.
    #[allow(clippy::too_many_arguments)]
    fn call_method_of_struct(
        &mut self,
        span: Span,
        method: &Method,
        function: usize,
        receiver: Option<&code::Expr>,
        place: Option<&code::PlaceOr>,
        args: &[code::Expr],
        moves: &[code::Move],
        types: &Option<Rc<code::InstanceTypes>>,
    ) -> EvalResult<Value> {
        if method.implementations.is_some() {
            return self.call_through_bound(span, method, receiver, place, args, moves);
        }
        let name = method.function.as_str();
        if !method.receiver {
            let values = self.eval_exprs(args)?;
            self.apply_moves(moves, name);
            self.next_types = self.instance_types(types);
            return Ok(self.invoke_id(function, values, false)?.0);
        }
        if !method.changes {
            let Some(receiver) = receiver else {
                return Err("a method needs a receiver".into());
            };
            let mut values = Vec::with_capacity(args.len() + 1);
            values.push(self.eval_expr(receiver)?);
            for arg in args {
                values.push(self.eval_expr(arg)?);
            }
            self.apply_moves(moves, name);
            self.next_types = self.instance_types(types);
            let value = self.invoke_id(function, values, false)?.0;
            // the prelude's `clamp` gives first whether its bounds are in order
            if name == stdlib::PRELUDE_CLAMP
                && let Value::Tuple(values) = &value
                && let [Value::Bool(in_order), value] = &values[..]
            {
                return match in_order {
                    true => Ok(value.clone()),
                    false => Err(stdlib::CLAMP_RANGE.into()),
                };
            }
            return Ok(value);
        }
        let Some(place) = place else {
            return Err("a method that changes its receiver needs a place".into());
        };
        let (root, steps, spans) = self.eval_place(place)?;
        let mut values = Vec::with_capacity(args.len() + 1);
        values.push(Value::Unit);
        for arg in args {
            values.push(self.eval_expr(arg)?);
        }
        self.apply_moves(moves, name);
        // a global the method may read stays, and a failed call in a session leaves it as it was
        let keep = self.keeps_receiver(root, name);
        let place = self.place_value(root, &steps, spans)?;
        values[0] = match keep {
            true => place.clone(),
            false => std::mem::replace(place, Value::Unit),
        };
        self.next_types = self.instance_types(types);
        let (result, changed) = self.invoke_id(function, values, true)?;
        if let Some(changed) = changed {
            *self.place_value(root, &steps, spans)? = changed;
        }
        Ok(result)
    }

    /// Returns whether a method `callee` that changes the variable `root` must leave it as it
    /// is until the call returns: a global the method may read, or any global in a session,
    /// where a failed call leaves it as it was.
    fn keeps_receiver(&self, root: code::Var, callee: &str) -> bool {
        self.frames.is_empty()
            && (!self.moves_globals
                || self
                    .global_reads
                    .get(callee)
                    .is_some_and(|reads| reads.contains(self.var_name(root))))
    }

    /// Calls a method through a bound, as `x.area()` with `x` of type `T` in
    /// `fn f[T: Shape](x: T):`, as the method of the receiver's own type, which only the value
    /// tells.
    fn call_through_bound(
        &mut self,
        span: Span,
        method: &Method,
        receiver: Option<&code::Expr>,
        place: Option<&code::PlaceOr>,
        args: &[code::Expr],
        moves: &[code::Move],
    ) -> EvalResult<Value> {
        if !method.changes {
            let Some(receiver) = receiver else {
                return Err("a method needs a receiver".into());
            };
            let receiver = self.eval_expr(receiver)?;
            // a type that compares part by part, or a number or a string, has the builtin method
            if matches!(method.function.as_str(), "eq.eq" | "ord.cmp")
                && self.implementation(span, method, &receiver).is_err()
            {
                let values = self.eval_exprs(args)?;
                let name = method.function.rsplit('.').next().unwrap_or_default();
                return self.call_method(name, receiver, values);
            }
            let function = self.implementation(span, method, &receiver)?;
            let mut values = Vec::with_capacity(args.len() + 1);
            values.push(receiver);
            for arg in args {
                values.push(self.eval_expr(arg)?);
            }
            self.apply_moves(moves, &function);
            return Ok(self.invoke(&function, values, false)?.0);
        }
        let Some(place) = place else {
            return Err("a method that changes its receiver needs a place".into());
        };
        let (root, steps, spans) = self.eval_place(place)?;
        let mut values = Vec::with_capacity(args.len() + 1);
        values.push(Value::Unit);
        for arg in args {
            values.push(self.eval_expr(arg)?);
        }
        let value = self.place_value(root, &steps, spans)?.clone();
        let function = self.implementation(span, method, &value)?;
        self.apply_moves(moves, &function);
        let keep = self.keeps_receiver(root, &function);
        let place = self.place_value(root, &steps, spans)?;
        values[0] = match keep {
            true => place.clone(),
            false => std::mem::replace(place, Value::Unit),
        };
        let (result, changed) = self.invoke(&function, values, true)?;
        if let Some(changed) = changed {
            *self.place_value(root, &steps, spans)? = changed;
        }
        Ok(result)
    }

    /// Evaluates the conversion `Type(arg)`, with a builtin conversion or the function of an
    /// impl of `from` or `try_from`, called with `types`, which the running copy's types pick
    /// for a type parameter.
    fn eval_conversion(
        &mut self,
        conversion: &Conversion,
        arg: &code::Expr,
        types: &Option<Rc<code::InstanceTypes>>,
    ) -> EvalResult<Value> {
        let value = self.eval_expr(arg)?;
        let target = conversion.target.substitute(&self.types);
        let source = conversion.source.substitute(&self.types);
        match conversion.resolve(&target, &source) {
            Some(Converter::Builtin) => self.convert(&target, value),
            Some(Converter::Function(function, copy)) => {
                self.next_types = match conversion.function {
                    Some(_) => self.instance_types(types),
                    None => self
                        .resolved
                        .type_params
                        .get(&function)
                        .map(|params| Rc::new(params.iter().cloned().zip(copy).collect())),
                };
                Ok(self.invoke(&function, vec![value], false)?.0)
            }
            None => Err(format!("cannot convert {source} to {target}").into()),
        }
    }

    /// Converts `value` to `target` as a builtin conversion does: to the same value for its own
    /// type, written as `print` writes it for `str`, and to a result for a conversion that may
    /// fail, such as `int` of a `str`.
    fn convert(&mut self, target: &Type, value: Value) -> EvalResult<Value> {
        Ok(match (target, value) {
            (Type::Str, Value::Str(text)) => Value::Str(text),
            (Type::Str, value) => {
                let mut text = String::new();
                self.write_value(&mut text, &value, false)?;
                Value::Str(text.into())
            }
            (Type::Float, Value::Int(i)) => Value::Float(i as f64),
            // -2^63 fits in an int but 2^63 does not, and nan fails both checks
            (Type::Int, Value::Float(x)) => {
                result(match (i64::MIN as f64..-(i64::MIN as f64)).contains(&x) {
                    true => Ok(Value::Int(x as i64)),
                    false => Err(stdlib::math::FLOAT_TO_INT.to_string()),
                })
            }
            (Type::Int, Value::Str(s)) => result(stdlib::parse_int(&s).map(Value::Int)),
            (Type::Float, Value::Str(s)) => result(stdlib::parse_float(&s).map(Value::Float)),
            (Type::Bytes, Value::List(values)) => result(
                values
                    .iter()
                    .map(|value| match value {
                        Value::Int(n @ 0..=255) => Ok(*n as u8),
                        Value::Int(n) => Err(stdlib::fill(stdlib::BYTE_RANGE, &[*n])),
                        _ => Err("bytes() takes a list of ints".to_string()),
                    })
                    .collect::<Result<Vec<u8>, String>>()
                    .map(|bytes| Value::Bytes(bytes.into())),
            ),
            // every type converts from itself
            (_, value) => value,
        })
    }

    /// Returns the function the call through a bound at `span` calls for the receiver `value`:
    /// the method of its type, whose type arguments it leaves for the call in
    /// [`Interpreter::next_types`]. The type the running copy gives the bounded type parameter
    /// picks it, and the value's type if the copy does not know it.
    fn implementation(&mut self, span: Span, method: &Method, value: &Value) -> EvalResult<String> {
        if let Some(instance) = self.resolved.instances.get(&span) {
            let args: Vec<Type> = instance
                .args
                .iter()
                .map(|a| a.substitute(&self.types))
                .collect();
            if let [this, trait_args @ ..] = args.as_slice()
                && !matches!(this, Type::Param(_))
                && let Some((function, copy)) = method.resolve(this, trait_args)
            {
                self.next_types = self
                    .resolved
                    .type_params
                    .get(&function)
                    .map(|params| Rc::new(params.iter().cloned().zip(copy).collect()));
                return Ok(function);
            }
        }
        self.next_types = None;
        let key = match value {
            Value::Struct(value) => value.def.linked.to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Str(_) => "str".to_string(),
            _ => String::new(),
        };
        if let Some(found) = method
            .implementations
            .as_ref()
            .and_then(|implementations| implementations.iter().find(|i| i.ty == key))
        {
            return Ok(found.function.clone());
        }
        // a default method checked in an earlier REPL entry knows only the types that
        // implemented its trait then, but a type's methods have names of their own, so its
        // method is its own function if it has one, and the default otherwise
        let name = method.function.rsplit('.').next().unwrap_or_default();
        let own = format!("{key}.{name}");
        if self.has_function(&own) {
            return Ok(own);
        }
        match self.has_function(&method.function) {
            true => Ok(method.function.clone()),
            false => Err(format!("'{key}' does not implement '{}'", method.function).into()),
        }
    }

    /// Calls the builtin or user-defined function named `name` with already evaluated arguments.
    fn call(&mut self, name: &str, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &args[..]) {
            ("print" | "eprint", _) => {
                // the whole line, so a print that fails writes nothing
                let mut line = String::new();
                for (i, value) in args.iter().enumerate() {
                    if i > 0 {
                        line.push(' ');
                    }
                    self.write_value(&mut line, value, false)?;
                }
                if name == "print" {
                    writeln!(self.out, "{line}")?;
                } else {
                    // what was printed before shows first, as compiled code writes each line at once
                    self.out.flush()?;
                    writeln!(self.errors, "{line}")?;
                }
                return Ok(Value::Unit);
            }
            ("input", [] | [Value::Str(_)]) => {
                if let [prompt] = &args[..] {
                    write!(self.out, "{}", prompt.display(false))?;
                }
                // a prompt, or earlier output a pipe buffered, shows before waiting for input
                self.out.flush()?;
                let line = self
                    .read_line()
                    .ok_or_else(|| stdlib::END_OF_INPUT.to_string());
                return Ok(result(line.map(|line| Value::Str(line.into()))));
            }
            ("eof", []) => {
                self.out.flush()?;
                return Ok(Value::Bool(self.at_eof()));
            }
            ("args", []) => {
                let args = self.args.iter().map(|a| a.as_str()).collect();
                return Ok(str_list(args));
            }
            ("os.env", [Value::Str(name)]) => {
                return Ok(os::env(name).map_or_else(none, |value| some(Value::Str(value.into()))));
            }
            ("os.has_env", [Value::Str(name)]) => return Ok(Value::Bool(os::has_env(name))),
            ("os.platform", []) => return Ok(Value::Str(os::platform().into())),
            ("os.arch", []) => return Ok(Value::Str(os::arch().into())),
            ("os.hostname", []) => return Ok(Value::Str(os::hostname().into())),
            ("os.cpu_count", []) => return Ok(Value::Int(os::cpu_count())),
            ("os.pid", []) => return Ok(Value::Int(os::pid())),
            ("os.cwd", []) => return Ok(Value::Str(os::cwd()?.into())),
            ("fs.read", [Value::Str(path)]) => {
                return Ok(result(fs::read(path).map(|text| Value::Str(text.into()))));
            }
            ("fs.read_lines" | "fs.list_dir", [Value::Str(path)]) => {
                let names = match name {
                    "fs.read_lines" => fs::read_lines(path),
                    _ => fs::list_dir(path),
                };
                return Ok(result(names.map(|names| {
                    str_list(names.iter().map(String::as_str).collect())
                })));
            }
            ("fs.write" | "fs.append", [Value::Str(path), Value::Str(text)]) => {
                let written = match name {
                    "fs.write" => fs::write(path, text),
                    _ => fs::append(path, text),
                };
                return Ok(result(written.map(Value::Int)));
            }
            ("fs.read_bytes", [Value::Str(path)]) => {
                let read = fs::read_bytes(path).map(|data| Value::Bytes(data.into()));
                return Ok(result(read));
            }
            ("fs.write_bytes" | "fs.append_bytes", [Value::Str(path), Value::Bytes(data)]) => {
                let written = match name {
                    "fs.write_bytes" => fs::write_bytes(path, data),
                    _ => fs::append_bytes(path, data),
                };
                return Ok(result(written.map(Value::Int)));
            }
            ("fs.exists", [Value::Str(path)]) => return Ok(Value::Bool(fs::exists(path))),
            ("fs.remove" | "fs.make_dir", [Value::Str(path)]) => {
                let done = match name {
                    "fs.remove" => fs::remove(path),
                    _ => fs::make_dir(path),
                };
                return Ok(result(done.map(|path| Value::Str(path.into()))));
            }
            ("os.run", [Value::Str(cmd), Value::List(args)]) => {
                let args: Vec<String> = args
                    .iter()
                    .map(|arg| match arg {
                        Value::Str(arg) => arg.to_string(),
                        other => other.display(false),
                    })
                    .collect();
                let ran = os::run(cmd, &args).map(|(status, stdout, stderr)| {
                    let def = self.structs[self.struct_ids[OUTPUT]].clone();
                    let fields = vec![
                        Value::Int(status),
                        Value::Str(stdout.into()),
                        Value::Str(stderr.into()),
                    ];
                    Value::Struct(Rc::new(StructValue { def, fields }))
                });
                return Ok(result(ran));
            }
            ("os.exit", [Value::Int(code)]) => {
                self.out.flush()?;
                return Err(Box::new(Exit(*code as u8)));
            }
            ("math.abs", [Value::Int(x)]) => return Ok(Value::Int(math::abs_int(*x)?)),
            ("math.abs", [Value::Float(x)]) => return Ok(Value::Float(math::abs_float(*x))),
            ("math.min" | "math.max", [Value::List(items)]) => {
                return min_or_max(name, items);
            }
            ("math.min" | "math.max", [_, _, ..]) => return min_or_max(name, &args),
            ("math.sqrt", [Value::Int(x)]) => return Ok(Value::Float(math::sqrt(*x as f64)?)),
            ("math.sqrt", [Value::Float(x)]) => return Ok(Value::Float(math::sqrt(*x)?)),
            ("math.floor", [Value::Int(x)]) => return Ok(Value::Int(*x)),
            ("math.floor", [Value::Float(x)]) => return Ok(Value::Int(math::floor(*x)?)),
            ("random.seed", [Value::Int(seed)]) => {
                self.rng = Some(Rng::seeded(*seed));
                return Ok(Value::Unit);
            }
            ("random.random", []) => return Ok(Value::Float(self.rng().random())),
            ("random.randint", [Value::Int(low), Value::Int(high)]) => {
                return Ok(Value::Int(self.rng().randint(*low, *high)?));
            }
            ("random.choice", [Value::List(items)]) => {
                let index = self.rng().choice(items.len())?;
                return Ok(items[index].clone());
            }
            ("time.now", []) => return Ok(Value::Float(time::now())),
            ("time.clock", []) => return Ok(Value::Float(time::clock())),
            (
                "net_sys.connect",
                [
                    Value::List(address),
                    Value::Int(port),
                    Value::Str(name),
                    Value::Int(timeout),
                ],
            ) => {
                self.out.flush()?;
                let address = ints(address);
                let connected = net::connect(&address, *port, name, *timeout)
                    .map(|socket| Value::Socket(Rc::new(RefCell::new(socket))));
                return Ok(result(connected));
            }
            (
                "gui_sys.pack",
                [
                    Value::List(pixels),
                    Value::Int(start),
                    Value::Int(count),
                    Value::Int(format),
                ],
            ) => {
                let packed = gui::pack(&ints(pixels), *start, *count, *format);
                return Ok(Value::Bytes(packed.into()));
            }
            ("gui_sys.shm_create", [Value::Socket(socket), Value::Int(size)]) => {
                return Ok(result(
                    socket.borrow_mut().shm_create(*size).map(Value::Int),
                ));
            }
            (
                "gui_sys.shm_write",
                [
                    Value::Socket(socket),
                    Value::Int(offset),
                    Value::Bytes(data),
                ],
            ) => {
                let written = socket.borrow_mut().shm_write(*offset, data);
                return Ok(result(written.map(Value::Int)));
            }
            ("gui_sys.take_fd", [Value::Socket(socket), Value::Int(size)]) => {
                let taken = socket.borrow_mut().take_fd(*size);
                return Ok(result(taken.map(|data| Value::Bytes(data.into()))));
            }
            ("gui_sys.shm_send", [Value::Socket(socket), Value::Bytes(data)]) => {
                return Ok(result(socket.borrow_mut().shm_send(data).map(Value::Int)));
            }
            ("net_sys.connect_unix", [Value::Str(path), Value::Str(name)]) => {
                self.out.flush()?;
                let connected = net::connect_unix(path, name)
                    .map(|socket| Value::Socket(Rc::new(RefCell::new(socket))));
                return Ok(result(connected));
            }
            ("net_sys.ready", [Value::Socket(socket), Value::Int(timeout)]) => {
                self.out.flush()?;
                return Ok(Value::Bool(socket.borrow_mut().ready(*timeout)));
            }
            ("net_sys.listen", [Value::List(address), Value::Int(port), Value::Str(name)]) => {
                let address = ints(address);
                let listening = net::listen(&address, *port, name)
                    .map(|listener| Value::Listener(Rc::new(RefCell::new(listener))));
                return Ok(result(listening));
            }
            (
                "net_sys.exchange",
                [
                    Value::List(address),
                    Value::Int(port),
                    Value::Str(name),
                    Value::List(query),
                    Value::Int(timeout),
                ],
            ) => {
                self.out.flush()?;
                let reply = net::exchange(&ints(address), *port, name, &ints(query), *timeout)
                    .map(|bytes| Value::list(bytes.into_iter().map(Value::Int).collect()));
                return Ok(result(reply));
            }
            ("time.sleep", [Value::Int(seconds)]) => {
                self.out.flush()?;
                time::sleep(*seconds as f64)?;
                return Ok(Value::Unit);
            }
            ("time.sleep", [Value::Float(seconds)]) => {
                self.out.flush()?;
                time::sleep(*seconds)?;
                return Ok(Value::Unit);
            }
            _ => {}
        }

        Ok(self.invoke(name, args, false)?.0)
    }

    /// Calls the stone function `name` with `args`, returning its value and, if `keep_self`,
    /// what it leaves in `self`, which a method that changes `self` gives back to its caller.
    fn invoke(
        &mut self,
        name: &str,
        args: Vec<Value>,
        keep_self: bool,
    ) -> EvalResult<(Value, Option<Value>)> {
        match self.function_names.get(name) {
            Some(id) => self.invoke_id(id, args, keep_self),
            None => Err(format!("Function '{name}' not found").into()),
        }
    }

    /// Calls the stone function with the id `id`, as [`Interpreter::invoke`] does.
    fn invoke_id(
        &mut self,
        id: usize,
        args: Vec<Value>,
        keep_self: bool,
    ) -> EvalResult<(Value, Option<Value>)> {
        let Some(function) = self.functions[id].clone() else {
            let name = &self.function_names.names[id];
            return Err(format!("Function '{name}' not found").into());
        };
        self.invoke_body(function, args, keep_self)
    }

    /// Returns the type arguments of a call of a generic function, `instance`, with those of the
    /// running copy filled in, or `None` if it calls no generic function.
    fn instance_types(&self, instance: &Option<Rc<code::InstanceTypes>>) -> Option<Types> {
        let instance = instance.as_ref()?;
        Some(match &instance.constant {
            Some(types) => types.clone(),
            None => self.types_of(&instance.params, &instance.args),
        })
    }

    /// Returns the type each of `params` stands for, the type at its position in `args` with
    /// the running copy's type arguments filled in.
    fn types_of(&self, params: &[String], args: &[Type]) -> Types {
        Rc::new(
            params
                .iter()
                .cloned()
                .zip(args.iter().map(|a| a.substitute(&self.types)))
                .collect(),
        )
    }

    /// Calls `function` as [`Interpreter::invoke`] does.
    fn invoke_body(
        &mut self,
        function: Rc<Function>,
        args: Vec<Value>,
        keep_self: bool,
    ) -> EvalResult<(Value, Option<Value>)> {
        if function.params != args.len() {
            let (name, expected) = (&function.name, function.params);
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

        self.calls += 1;
        let resolved = std::mem::replace(&mut self.resolved, function.resolved.clone());
        let types = match self.next_types.take() {
            Some(types) => std::mem::replace(&mut self.types, types),
            None => std::mem::replace(&mut self.types, self.no_types.clone()),
        };
        // parameters fill the first slots, so they shadow globals of the same name
        let mut slots = Vec::with_capacity(function.layout.names.len());
        slots.extend(args.into_iter().map(Some));
        slots.resize(function.layout.names.len(), None);
        self.frames.push(Frame {
            slots,
            layout: function.layout.clone(),
        });
        let flow = self.eval_block(&function.body);
        let frame = self.frames.pop();
        let this = match keep_self {
            true => frame.and_then(|mut frame| frame.take("self")),
            false => None,
        };
        self.resolved = resolved;
        self.types = types;
        self.calls -= 1;

        let value = match flow? {
            ControlFlow::Return(value) => value,
            _ => Value::Unit,
        };
        Ok((value, this))
    }

    /// Assigns `value` to `target`, a variable, or names that take the fields of a tuple in
    /// order, at any depth.
    ///
    /// For example, `a, (b, c)` takes `(1, (2, 3))` as `a = 1`, `b = 2`, and `c = 3`.
    fn assign_target(&mut self, target: &code::Unpack, value: &Value) -> EvalResult<()> {
        match (target, value) {
            (code::Unpack::Var(var), _) => self.set_var(*var, value.clone()),
            (code::Unpack::Tuple(elts), Value::Tuple(fields)) if elts.len() == fields.len() => {
                for (elt, field) in elts.iter().zip(fields.iter()) {
                    self.assign_target(elt, field)?;
                }
            }
            (code::Unpack::Tuple(elts), other) => {
                return Err(format!(
                    "cannot unpack {} into {} names",
                    other.display(true),
                    elts.len()
                )
                .into());
            }
            (code::Unpack::Invalid, _) => return Err("Invalid assignment target".into()),
        }
        Ok(())
    }

    /// Assigns a variable: a local of the running function, or a global, like Python.
    ///
    /// For example, `x = 2` inside a function leaves a global `x` unchanged, since `x` is one of
    /// the function's locals.
    fn set_var(&mut self, var: code::Var, value: Value) {
        let slot = match var {
            code::Var::Local(slot) => self
                .frames
                .last_mut()
                .and_then(|frame| frame.slots.get_mut(slot)),
            code::Var::Global(id) => self.globals.get_mut(id),
        };
        if let Some(slot) = slot {
            *slot = Some(value);
        }
    }

    /// Reads a variable: a local of the running function, or a global.
    ///
    /// A function never sees its caller's locals, so scoping is lexical.
    fn read_var(&self, var: code::Var) -> EvalResult<Value> {
        let value = match var {
            code::Var::Local(slot) => self
                .frames
                .last()
                .and_then(|frame| frame.slots.get(slot))
                .and_then(Option::as_ref),
            code::Var::Global(id) => self.globals.get(id).and_then(Option::as_ref),
        };
        match value {
            Some(value) => Ok(value.clone()),
            None => {
                let name = self.var_name(var);
                Err(format!("'{name}' is used before it is assigned").into())
            }
        }
    }

    /// Returns the variable `var` of the running body for changing: a local of the running
    /// function, or a global at the top level.
    fn variable_mut(&mut self, var: code::Var) -> Option<&mut Value> {
        variable_in(&mut self.frames, &mut self.globals, var)
    }

    /// Evaluates the indexes of `place`, a variable or a value inside one that a statement is
    /// about to change, and returns the variable with the steps to the value, outermost first,
    /// and the span each step fails at.
    ///
    /// For example, `grid[i][j]` gives `grid` and the values of `i` and `j`, and `ps[i].x`
    /// gives `ps`, the value of `i`, and the field `x`. Nothing is checked against the values
    /// yet: [`Interpreter::place_value`] does that once the change is ready.
    fn eval_place<'p>(
        &mut self,
        place: &'p code::PlaceOr,
    ) -> EvalResult<(code::Var, Vec<PlaceStep<'p>>, &'p [Span])> {
        let place = place.as_ref().map_err(|message| message.clone())?;
        let mut steps = Vec::with_capacity(place.steps.len());
        for step in &place.steps {
            steps.push(match step {
                code::PlaceStep::Index(index) => PlaceStep::Index(self.eval_expr(index)?),
                code::PlaceStep::Field(name) => PlaceStep::Field(name),
            });
        }
        Ok((place.root, steps, &place.spans))
    }

    /// Returns the error for changing `root` before it is assigned.
    fn unassigned(&self, root: code::Var) -> Box<dyn std::error::Error> {
        let name = self.var_name(root);
        format!("'{name}' is used before it is assigned").into()
    }

    /// Walks from the variable `root` through `steps` to the value a statement changes, and
    /// returns it for changing.
    ///
    /// Every list and struct on the way that another value still shares is copied first, so the
    /// change shows through no other variable. The checker allows a function to change only its
    /// own variables, so `root` is in the function's frame, or in the globals at the top level.
    /// An index out of range fails at its subscript in `spans`, from [`place_spans`].
    fn place_value(
        &mut self,
        root: code::Var,
        steps: &[PlaceStep],
        spans: &[Span],
    ) -> EvalResult<&mut Value> {
        if self.variable_mut(root).is_none() {
            return Err(self.unassigned(root));
        }
        let value = variable_in(&mut self.frames, &mut self.globals, root)
            .ok_or("a variable went missing")?;
        walk_place(value, steps, spans, &mut self.copies)
    }

    /// Walks to the list a statement changes, as [`Interpreter::place_value`] does, and returns
    /// its elements for changing, copying them first if another value still shares them.
    fn place_list(
        &mut self,
        root: code::Var,
        steps: &[PlaceStep],
        spans: &[Span],
    ) -> EvalResult<&mut Vec<Value>> {
        if self.variable_mut(root).is_none() {
            return Err(self.unassigned(root));
        }
        let value = variable_in(&mut self.frames, &mut self.globals, root)
            .ok_or("a variable went missing")?;
        let value = walk_place(value, steps, spans, &mut self.copies)?;
        self.copies += value.is_shared() as u64;
        value.as_list_mut()
    }

    /// Stores `value` at `index` of the list or map that `root` leads to through `steps`: a list's
    /// element, failing at `span` if `index` is out of range, or a map's entry for the key
    /// `index`, which is added at the end if the map has none.
    fn store_at(
        &mut self,
        root: code::Var,
        steps: &[PlaceStep],
        spans: &[Span],
        index: Value,
        span: Span,
        value: &Value,
    ) -> EvalResult<()> {
        if let Value::Map(map) = self.place_value(root, steps, spans)? {
            let shared = Rc::strong_count(map) > 1;
            Rc::make_mut(map).insert(MapKey::of(&index)?, value.clone());
            self.copies += shared as u64;
            return Ok(());
        }
        let items = self.place_list(root, steps, spans)?;
        let position = list_position(index.as_int()?, items.len()).map_err(|e| located(e, span))?;
        items[position] = value.clone();
        Ok(())
    }
}

/// Returns the variable `var` of the running body for changing, as
/// [`Interpreter::variable_mut`] does, borrowing only the variables: a local of the running
/// function, or a global at the top level, since a function cannot change a global.
fn variable_in<'v>(
    frames: &'v mut [Frame],
    globals: &'v mut [Option<Value>],
    var: code::Var,
) -> Option<&'v mut Value> {
    match var {
        code::Var::Local(slot) => frames.last_mut()?.slots.get_mut(slot)?.as_mut(),
        code::Var::Global(id) if frames.is_empty() => globals.get_mut(id)?.as_mut(),
        code::Var::Global(_) => None,
    }
}

/// Walks from `value` through `steps` for [`Interpreter::place_value`], counting in `copies`
/// each shared list it copies on the way.
fn walk_place<'v>(
    mut value: &'v mut Value,
    steps: &[PlaceStep],
    spans: &[Span],
    copies: &mut u64,
) -> EvalResult<&'v mut Value> {
    for (step, &span) in steps.iter().zip(spans) {
        match step {
            PlaceStep::Index(index) => {
                *copies += value.is_shared() as u64;
                if let Value::Map(map) = value {
                    let key = MapKey::of(index)?;
                    value = Rc::make_mut(map)
                        .get_mut(&key)
                        .map_err(|e| located(e, span))?;
                    continue;
                }
                let items = value.as_list_mut()?;
                let position =
                    list_position(index.as_int()?, items.len()).map_err(|e| located(e, span))?;
                value = &mut items[position];
            }
            PlaceStep::Field(name) => {
                let Value::Struct(inner) = value else {
                    let found = value.display(true);
                    return Err(format!("expected a struct, found {found}").into());
                };
                let inner = Rc::make_mut(inner);
                let position = inner.field(name)?;
                value = &mut inner.fields[position];
            }
        }
    }
    Ok(value)
}

/// One step of an evaluated place, from [`Interpreter::eval_place`]: the index of a list's
/// element or the key of a map's entry, which the value it steps into tells apart, or the name of
/// a struct's field.
#[derive(Debug, Clone)]
enum PlaceStep<'a> {
    Index(Value),
    Field(&'a str),
}

/// Returns `expr` written out if it is a field of a variable reached through fields alone, such
/// as `b.items` or `line.start.x`, which a call may take as it takes a variable.
fn field_path(expr: &Expr) -> Option<String> {
    let (root, steps) = place(expr)?;
    if steps.is_empty() {
        return None;
    }
    let mut path = root.to_string();
    for step in steps {
        let Step::Field(name) = step else {
            return None;
        };
        path.push('.');
        path.push_str(name);
    }
    Some(path)
}

/// Returns whether `stmt` defines something code can use before the definition: a function, an
/// enum, a struct, or the functions of an `impl` block.
fn is_definition(stmt: &Stmt) -> bool {
    matches!(
        stmt.kind,
        StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::parse_program;

    #[test]
    fn removing_map_entries_keeps_the_rest_in_order() {
        let mut map = MapValue::default();
        let mut expected: Vec<i64> = Vec::new();
        for k in 0..100 {
            map.insert(MapKey::Int(k), Value::Int(k * 10));
            expected.push(k);
        }
        for round in 0..3 {
            for k in (round..100).step_by(3) {
                let removed = map.remove(&MapKey::Int(k));
                assert_eq!(removed.is_some(), expected.contains(&k), "{k}");
                expected.retain(|&e| e != k);
            }
            for k in 0..5 {
                let key = 1000 * (round + 1) + k;
                map.insert(MapKey::Int(key), Value::Int(key * 10));
                expected.push(key);
            }
            // setting an entry that is left keeps its place
            map.insert(MapKey::Int(expected[0]), Value::Int(expected[0] * 10));
            let keys: Vec<i64> = map
                .iter()
                .map(|(key, _)| match key {
                    MapKey::Int(k) => *k,
                    _ => unreachable!(),
                })
                .collect();
            assert_eq!(keys, expected);
            assert_eq!(map.len(), expected.len());
            for &k in &expected {
                assert!(matches!(map.get(&MapKey::Int(k)), Ok(Value::Int(v)) if *v == k * 10));
            }
            assert!(
                map.entries.len() <= 2 * map.len() + 1,
                "holes are closed up"
            );
        }
    }

    #[test]
    fn dropping_a_long_chain_of_enum_values_takes_no_stack() {
        // on a test thread's small stack, dropping each value inside the one holding it would
        // overflow long before a million
        let mut chain = none();
        for i in 0..1_000_000 {
            let link = Value::List(Rc::new(vec![Value::Int(i), chain]));
            chain = some(Value::Tuple(Rc::from([link, Value::Unit])));
        }
        drop(chain);
    }

    /// Runs `source` and returns how many times a change copied a list that something else
    /// still shared.
    fn copies(source: &str) -> u64 {
        run(source).1
    }

    /// Checks and runs `source`, returning what it prints and how many times a change copied a
    /// list that something else still shared.
    fn run(source: &str) -> (String, u64) {
        let module = parse_program(source).unwrap();
        // some programs here leave out parameter types, which only the checker requires
        let analysis = crate::checker::TypeChecker::new().analyze(&module);
        let mut out = Vec::new();
        let mut interpreter =
            Interpreter::with_output(&mut out, Limits::DEFAULT).with_calls(analysis.calls());
        interpreter.evaluate(&module).unwrap();
        let copies = interpreter.copies;
        drop(interpreter);
        (String::from_utf8(out).unwrap(), copies)
    }

    /// A stack whose methods change it in place.
    const STACK: &str = "struct Stack:\n    items: list[int]\nimpl Stack:\n    fn new() -> Stack:\n        \
                         ret Stack(items=[])\n    fn push(self, x: int):\n        self.items.append(x)\n    \
                         fn size(self) -> int:\n        ret self.items.len()\n    \
                         fn pop_top(self) -> int:\n        x = self.items.pop(-1)\n        ret x\n";

    #[test]
    fn methods_are_called_on_values_and_associated_functions_on_structs() {
        let source = format!(
            "{STACK}s = Stack.new()\ns.push(1)\ns.push(2)\nt = s.pop_top()\nprint(s.size(), t, s)\n"
        );
        assert_eq!(run(&source).0, "1 2 Stack(items=[1])\n");
        // a method named like a builtin one is the struct's
        let source = "struct Bag:\n    n: int\nimpl Bag:\n    fn append(self, x: int) -> int:\n        \
                      ret self.n + x\nb = Bag(n=1)\nb.append(5)\nprint(b.append(2), b)\n";
        assert_eq!(run(source).0, "3 Bag(n=1)\n");
    }

    #[test]
    fn a_method_changes_its_receiver_in_place_unless_another_value_shares_it() {
        let source = format!(
            "{STACK}s = Stack.new()\nfor i in range(100):\n    s.push(i)\nprint(s.size())\n"
        );
        assert_eq!(run(&source), ("100\n".to_string(), 0));
        let source = format!(
            "{STACK}ss = [Stack.new(), Stack.new()]\nss[1].push(1)\nss[1].push(2)\n\
             x = ss[1].pop_top()\nprint(ss, x)\n"
        );
        assert_eq!(
            run(&source),
            ("[Stack(items=[]), Stack(items=[1])] 2\n".to_string(), 0)
        );
        let source = format!("{STACK}s = Stack.new()\nt = s\ns.push(1)\nprint(s, t)\n");
        assert_eq!(
            run(&source),
            ("Stack(items=[1]) Stack(items=[])\n".to_string(), 1)
        );
        // a receiver that is also an argument is read before the change
        let source = "struct P:\n    xs: list[int]\nimpl P:\n    fn absorb(self, ys: list[int]):\n        \
                      for y in ys:\n            self.xs.append(y)\np = P(xs=[1, 2])\np.absorb(p.xs)\nprint(p)\n";
        assert_eq!(run(source).0, "P(xs=[1, 2, 1, 2])\n");
    }

    #[test]
    fn a_method_that_reads_the_global_it_changes_sees_it_unchanged() {
        let source = format!(
            "{STACK}impl Box:\n    fn grow(self):\n        self.s.push(g.s.size())\n\
             struct Box:\n    s: Stack\ng = Box(s=Stack.new())\ng.grow()\ng.grow()\nprint(g)\n"
        );
        assert_eq!(run(&source).0, "Box(s=Stack(items=[0, 1]))\n");
    }

    /// A function that appends to its parameter and returns it.
    const ADD: &str = "fn add(xs: list[int], x: int) -> list[int]:\n    xs.append(x)\n    ret xs\n";

    #[test]
    fn an_augmented_element_copies_only_a_shared_list() {
        assert_eq!(copies("xs = [1, 2]\nxs[0] += 1\nprint(xs)\n"), 0);
        assert_eq!(
            copies("grid = [[1], [2]]\ngrid[1][0] *= 3\nprint(grid)\n"),
            0
        );
        assert_eq!(copies("grid = [[1], [2]]\ngrid[1] += 3\nprint(grid)\n"), 0);
        assert_eq!(
            copies("xs = [1, 2]\nys = xs\nxs[0] += 1\nprint(xs, ys)\n"),
            1
        );
    }

    #[test]
    fn a_list_assigned_the_result_of_a_call_is_changed_in_place() {
        let source =
            format!("{ADD}xs = []\nfor i in range(100):\n    xs = add(xs, i)\nprint(xs)\n");
        assert_eq!(copies(&source), 0);
        let source = format!(
            "{ADD}fn fill(xs, n):\n    if n == 0:\n        ret xs\n    xs.append(n)\n    \
             ret fill(xs, n - 1)\nprint(fill([], 50))\n"
        );
        assert_eq!(copies(&source), 0);
        // a call among the arguments runs statements of its own first
        let source = format!(
            "{ADD}fn one():\n    n = 1\n    ret n\nxs = []\nfor i in range(10):\n    \
             xs = add(xs, one())\nprint(xs)\n"
        );
        assert_eq!(copies(&source), 0);
    }

    #[test]
    fn a_field_changes_in_place_unless_another_value_shares_it() {
        let bag = "struct Bag:\n    items: list[int]\n    n: int\n";
        let make = "b = Bag(items=[1], n=0)\n";
        assert_eq!(
            copies(&format!("{bag}{make}b.items.append(2)\nprint(b)\n")),
            0
        );
        assert_eq!(
            copies(&format!(
                "{bag}{make}bs = [b]\nbs[0].items[0] = 2\nprint(bs)\n"
            )),
            0
        );
        assert_eq!(
            copies(&format!(
                "{bag}{make}c = b\nc.items.append(2)\nc.n = 1\nprint(b, c)\n"
            )),
            1
        );
        // a field assigned the result of a call moves into it
        let source = format!(
            "{ADD}{bag}{make}for i in range(100):\n    b.items = add(b.items, i)\nprint(b.n)\n"
        );
        assert_eq!(copies(&source), 0);
        let source = format!(
            "{ADD}{bag}fn grow(b: Bag) -> Bag:\n    b.items = add(b.items, 1)\n    ret b\n\
             {make}for i in range(10):\n    b = grow(b)\nprint(b)\n"
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
        let source = "fn f(n: int) -> list[int]:\n    xs = [n]\n    ys = xs\n    ys.append(1)\n    ret ys\n\
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
        let source = "fn peek(ys: list[int]) -> list[int]:\n    ys.append(xs[0])\n    ret ys\nxs = [1]\nxs = peek(xs)\n\
                      print(xs)\n";
        assert_eq!(copies(source), 1);
    }

    /// Runs `source` and returns what it prints.
    fn output(source: &str) -> String {
        let module = parse_program(source).unwrap();
        let mut out = Vec::new();
        Interpreter::with_output(&mut out, Limits::DEFAULT)
            .evaluate(&module)
            .unwrap();
        String::from_utf8(out).unwrap()
    }

    #[test]
    fn a_function_builds_a_struct_defined_after_it() {
        let source = "fn make() -> P:\n    ret P(x=2)\nstruct P:\n    x: int\nprint(make())\n";
        assert_eq!(run(source).0, "P(x=2)\n");
    }

    #[test]
    fn a_local_shadows_a_global_and_a_function_reads_the_global_otherwise() {
        let source = "x = 1\nfn f() -> int:\n    x = 5\n    ret x\nfn g() -> int:\n    ret x\n\
                      print(f(), g(), x)\n";
        assert_eq!(run(source).0, "5 1 1\n");
    }

    #[test]
    fn locals_are_fresh_in_every_call() {
        let source = "fn f(n: int) -> int:\n    if n == 0:\n        ret 0\n    t = n\n    \
                      r = f(n - 1)\n    ret t + r\nprint(f(4))\n";
        assert_eq!(run(source).0, "10\n");
    }

    #[test]
    fn a_method_changes_a_receiver_in_a_list_element() {
        let source = format!(
            "{STACK}fn fill() -> list[Stack]:\n    ss = [Stack.new(), Stack.new()]\n    \
             ss[1].push(7)\n    ss[1].push(8)\n    ret ss\nss = fill()\n\
             print(ss[0].size(), ss[1].size(), ss[1].pop_top())\n"
        );
        assert_eq!(run(&source).0, "0 2 8\n");
    }

    #[test]
    fn tuples_print_like_python() {
        assert_eq!(
            output("t = (1, \"a\", (2.5, [\"x\"]), none, true)\nprint(t, t[1], t[-3][1])\n"),
            "(1, 'a', (2.5, ['x']), none, true) a ['x']\n"
        );
        assert_eq!(
            output("print([(\"a\", 1), (\"b\", 2)])\n"),
            "[('a', 1), ('b', 2)]\n"
        );
    }

    #[test]
    fn unpacking_assigns_after_the_whole_value() {
        assert_eq!(
            output("a = 1\nb = 2\na, b = (b, a)\nprint(a, b)\n"),
            "2 1\n"
        );
        assert_eq!(
            output("a, (b, c) = (1, (\"x\", [2]))\nprint(c, b, a)\n"),
            "[2] x 1\n"
        );
        assert_eq!(
            output("for k, (v, w) in [(1, (2, 3)), (4, (5, 6))]:\n    print(k + v + w)\n"),
            "6\n15\n"
        );
    }

    #[test]
    fn a_list_unpacked_from_a_tuple_at_its_last_use_is_not_copied() {
        assert_eq!(
            copies("t = ([1], 2)\nxs, n = t\nxs.append(n)\nprint(xs)\n"),
            0
        );
        assert_eq!(
            copies("t = ([1], 2)\nxs, n = t\nxs.append(n)\nprint(xs, t)\n"),
            1
        );
    }

    #[test]
    fn a_change_through_a_map_copies_only_a_shared_map() {
        let bag = "struct Bag:\n    m: map[str, list[int]]\nb = Bag(m={\"a\": [1]})\n";
        assert_eq!(copies(&format!("{bag}b.m[\"a\"].append(2)\nprint(b)\n")), 0);
        assert_eq!(copies(&format!("{bag}b.m[\"a\"][0] += 2\nprint(b)\n")), 0);
        // the map and its list are shared with `c`, so each is copied once
        assert_eq!(
            copies(&format!("{bag}c = b\nb.m[\"a\"].append(2)\nprint(b, c)\n")),
            2
        );
        assert_eq!(copies("m = {1: 1}\nx = m.pop(1)\nprint(m, x)\n"), 0);
        assert_eq!(
            copies("m = {1: 1}\nn = m\nx = m.pop(1)\nprint(m, n, x)\n"),
            1
        );
        assert_eq!(copies("m = {1: 1}\nn = m\nm[2] = 2\nprint(m, n)\n"), 1);
    }
}
