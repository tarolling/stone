//! Tree-walking interpreter that evaluates a stone AST directly.
//!
//! For example, running `x = 42` followed by `print(x)` prints `42`.

use crate::ast::{
    BoolOp, CompOp, Constant, Expr, ExprKind, Mod, Operator, Pattern, PatternKind, Stmt, StmtKind,
    UnaryOp, target_names,
};
use crate::checker::{Step, collect_assigned, place, place_spans, range_args, struct_display};
use crate::diagnostic::Diagnostic;
use crate::last_use::{GlobalReads, global_reads, last_uses};
use crate::span::Span;
use crate::stdlib::random::Rng;
use crate::stdlib::{self, MAX_CALL_DEPTH, math, os, time};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{BufRead, Write};
use std::rc::Rc;

type EvalResult<T> = Result<T, Box<dyn std::error::Error>>;

/// Gives a runtime error the position of `span`, the expression that failed, unless it already
/// has one from an expression inside it or is an [`Exit`], so the innermost expression wins.
///
/// For example, `division by zero` raised while evaluating `a / b` at 3:7 becomes a
/// [`Diagnostic`] at 3:7, which stays there as it passes through `print(a / b)`.
fn located(error: Box<dyn std::error::Error>, span: Span) -> Box<dyn std::error::Error> {
    if error.is::<Diagnostic>() || error.is::<Exit>() {
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
/// For example, `fn add(a: int, b: int) -> int; ret a + b` has the parameters `["a", "b"]`.
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
}

/// A struct's definition, as the interpreter needs it: how it prints and its fields' names, in
/// the order they are written.
#[derive(Debug)]
pub struct StructDef {
    /// The struct's name without its module, such as `Point`.
    pub name: Rc<str>,
    pub fields: Vec<String>,
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

/// Makes the option `none`.
fn none() -> Value {
    Value::Enum(Rc::new(EnumValue {
        name: "none".into(),
        tag: 0,
        fields: vec![],
    }))
}

/// Makes the option `some(value)`.
fn some(value: Value) -> Value {
    Value::Enum(Rc::new(EnumValue {
        name: "some".into(),
        tag: 1,
        fields: vec![value],
    }))
}

/// Printing stopped because an enum value is nested more than [`stdlib::MAX_PRINT_DEPTH`] deep.
struct TooDeep;

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
    fn display(&self, nested: bool) -> String {
        let mut out = String::new();
        // only `print` stops at a value nested too deeply, and writes what it has first
        let _ = self.write(&mut out, nested, 0);
        out
    }

    /// Writes the value as [`Value::display`] does to `out`, inside `depth` enum and struct
    /// values, stopping at one nested more than [`stdlib::MAX_PRINT_DEPTH`] deep.
    ///
    /// For example, `some(Shape.Circle(1.0))` writes `some(Shape.Circle(1.0))`, and a struct
    /// writes its name and each field's, as in `Point(x=1.0, y=2.0)`.
    fn write(&self, out: &mut String, nested: bool, depth: usize) -> Result<(), TooDeep> {
        match self {
            Value::Int(i) => out.push_str(&i.to_string()),
            Value::Float(x) => out.push_str(&stdlib::format_float(*x)),
            Value::Bool(b) => out.push_str(&b.to_string()),
            Value::Str(s) if nested => out.push_str(&stdlib::repr_in_list(s)),
            Value::Str(s) => out.push_str(s),
            Value::Unit => {}
            Value::List(items) => {
                out.push('[');
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    item.write(out, true, depth)?;
                }
                out.push(']');
            }
            Value::Tuple(fields) => {
                out.push('(');
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    field.write(out, true, depth)?;
                }
                out.push(')');
            }
            Value::Enum(value) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(TooDeep);
                }
                out.push_str(&value.name);
                if !value.fields.is_empty() {
                    out.push('(');
                    for (i, field) in value.fields.iter().enumerate() {
                        if i > 0 {
                            out.push_str(", ");
                        }
                        field.write(out, true, depth + 1)?;
                    }
                    out.push(')');
                }
            }
            Value::Struct(value) => {
                if depth >= stdlib::MAX_PRINT_DEPTH {
                    return Err(TooDeep);
                }
                out.push_str(&value.def.name);
                out.push('(');
                for (i, (name, field)) in value.def.fields.iter().zip(&value.fields).enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(name);
                    out.push('=');
                    field.write(out, true, depth + 1)?;
                }
                out.push(')');
            }
        }
        Ok(())
    }

    /// Returns whether this value may hold a list, which a change can copy: a list, a tuple or
    /// struct with one among its fields at any depth, or an enum value.
    fn holds_list(&self) -> bool {
        match self {
            Value::List(_) | Value::Enum(_) => true,
            Value::Tuple(fields) => fields.iter().any(Value::holds_list),
            Value::Struct(value) => value.fields.iter().any(Value::holds_list),
            _ => false,
        }
    }

    /// Returns whether this is `true`, the only value that passes a condition, since the checker
    /// allows only a `bool` there.
    fn is_true(&self) -> bool {
        matches!(self, Value::Bool(true))
    }

    /// Returns whether two values are equal, comparing lists by identity as compiled code does.
    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Unit, Value::Unit) => true,
            (Value::List(a), Value::List(b)) => Rc::ptr_eq(a, b),
            (Value::Tuple(a), Value::Tuple(b)) => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.equals(b))
            }
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
            // byte order, which is also the order of the characters' code points
            (Value::Str(a), Value::Str(b)) => Ok(Some(a.as_bytes().cmp(b.as_bytes()))),
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
            Constant::None => none(),
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

/// Calls the method `name`, which changes its receiver, on `items` with `args`.
///
/// For example, `append` with `[Value::Int(1)]` pushes 1 and returns `Value::Unit`, and `pop`
/// with no arguments removes and returns the last element.
fn change_list(items: &mut Vec<Value>, name: &str, args: Vec<Value>) -> EvalResult<Value> {
    match (name, &args[..]) {
        ("append", [item]) => {
            items.push(item.clone());
            Ok(Value::Unit)
        }
        ("pop", []) => items.pop().ok_or_else(|| stdlib::POP_EMPTY.into()),
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
    /// function uses several levels per call, so `fn f(n: int) -> int; ret f(n - 1)` uses about
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
    // global variables
    globals: HashMap<String, Value>,
    /// Stack of local scopes, with the innermost scope last.
    scopes: Vec<HashMap<String, Value>>,
    /// User-defined functions by name.
    functions: HashMap<String, Rc<Function>>,
    /// The enums the program defines, by linked name, each with how its variants print, in the
    /// order of their tags.
    enums: HashMap<String, Vec<(String, Rc<str>)>>,
    /// The structs the program defines, by linked name.
    structs: HashMap<String, Rc<StructDef>>,
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
    /// The `random` module's generator, seeded from the system's entropy on first use unless
    /// `random.seed` seeds it first. It lasts the whole run, or the whole interactive session.
    rng: Option<Rng>,
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
            enums: HashMap::new(),
            structs: HashMap::new(),
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
    /// true, which the enclosing function then returns.
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
                // functions, enums, and structs can be used before their definition
                for stmt in body {
                    if is_definition(stmt) {
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
        // functions, enums, and structs can be used before their definition
        for stmt in body {
            if is_definition(stmt) {
                self.eval_stmt(stmt)?;
            }
        }
        for stmt in body {
            match &stmt.kind {
                _ if is_definition(stmt) => {}
                StmtKind::Expr { value } => {
                    let value = self.eval_expr(value)?;
                    if !matches!(value, Value::Unit) {
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
    /// time, and `targets`, the names and fields it assigns, such as `b.items`.
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
            if let Some(path) = field_path(target) {
                names.push(path);
            }
            let mut assigned = Vec::new();
            target_names(target, &mut assigned);
            for name in assigned {
                if let ExprKind::Name { id, .. } = &name.kind {
                    names.push(id.clone());
                }
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
    /// change can copy, directly or in a tuple: a local of the running function, or a global at the top level, unless
    /// globals cannot move or the function `callee` may read it.
    fn drop_variable(&mut self, name: &str, callee: Option<&str>) {
        let holds_list =
            |scope: &HashMap<String, Value>| scope.get(name).is_some_and(Value::holds_list);
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

    /// Takes the value of `field`, a field of a variable such as `b.items`, out of the variable
    /// if it may hold a list, leaving no value in its place, so that the call to `callee` it is
    /// passed to holds the only reference, as [`Interpreter::drop_variable`] does for a whole
    /// variable.
    fn take_field(&mut self, field: &Expr, callee: &str) {
        let Some((root, steps)) = place(field) else {
            return;
        };
        let global = self.scopes.is_empty();
        if global
            && (!self.moves_globals
                || self
                    .global_reads
                    .get(callee)
                    .is_some_and(|reads| reads.contains(root)))
        {
            return;
        }
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        let Some(mut value) = scope.get_mut(root) else {
            return;
        };
        for step in steps {
            let (Step::Field(name), Value::Struct(inner)) = (step, value) else {
                return;
            };
            let inner = Rc::make_mut(inner);
            let Ok(position) = inner.field(name) else {
                return;
            };
            value = &mut inner.fields[position];
        }
        if value.holds_list() {
            *value = Value::Unit;
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
                    Value::Unit
                };
                Ok(ControlFlow::Return(val))
            }
            StmtKind::Assign { targets, value } => {
                // a target's indexes are read after the call
                if targets.iter().all(|target| {
                    matches!(target.kind, ExprKind::Name { .. } | ExprKind::Tuple { .. })
                        || field_path(target).is_some()
                }) {
                    self.note_top_call(stmt.span, value, targets);
                }
                let rhs = self.eval_expr(value)?;

                for target in targets {
                    match &target.kind {
                        ExprKind::Name { .. } | ExprKind::Tuple { .. } => {
                            self.assign_target(target, &rhs)?;
                        }
                        ExprKind::Subscript { value, slice, .. } => {
                            let (root, indexes) = self.eval_place(value)?;
                            let index = self.eval_expr(slice)?.as_int()?;
                            let items = self.place_list(root, &indexes, &place_spans(value))?;
                            let position = list_position(index, items.len())
                                .map_err(|e| located(e, target.span))?;
                            items[position] = rhs.clone();
                        }
                        ExprKind::Attribute { .. } => {
                            let (root, steps) = self.eval_place(target)?;
                            let field = self.place_value(root, &steps, &place_spans(target))?;
                            *field = rhs.clone();
                        }
                        _ => return Err("Invalid assignment target".into()),
                    }
                }
                Ok(ControlFlow::None)
            }
            StmtKind::AugAssign { target, op, value } => {
                match &target.kind {
                    ExprKind::Name { id, .. } => {
                        let old = self.eval_expr(target)?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        self.set_var(id, &new)?;
                    }
                    ExprKind::Subscript {
                        value: list, slice, ..
                    } => {
                        // the indexes once, then the old element, then the value
                        let (root, indexes) = self.eval_place(list)?;
                        let index = self.eval_expr(slice)?.as_int()?;
                        let spans = place_spans(list);
                        let mut steps = indexes.clone();
                        steps.push(PlaceStep::Index(index));
                        let old = self.read_place(root, &steps, &place_spans(target))?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        // the value cannot change the list, which only this statement can
                        let items = self.place_list(root, &indexes, &spans)?;
                        let position = list_position(index, items.len())
                            .map_err(|e| located(e, target.span))?;
                        items[position] = new;
                    }
                    ExprKind::Attribute { .. } => {
                        // the indexes once, then the old field, then the value
                        let (root, steps) = self.eval_place(target)?;
                        let spans = place_spans(target);
                        let old = self.read_place(root, &steps, &spans)?;
                        let new = self.augmented(*op, old, value, stmt.span)?;
                        *self.place_value(root, &steps, &spans)? = new;
                    }
                    _ => return Err("Invalid assignment target".into()),
                }
                Ok(ControlFlow::None)
            }
            StmtKind::For { target, iter, body } => {
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
                        self.assign_target(target, &Value::Int(i))?;
                        if let Some(flow) = self.run_iteration(body)? {
                            return Ok(flow);
                        }
                    }
                    return Ok(ControlFlow::None);
                }

                // a string gives each character, as a string of the bytes that make it up
                let list = self.eval_expr(iter)?;
                if let Value::Str(text) = &list {
                    for ch in text.chars() {
                        self.assign_target(target, &Value::Str(ch.to_string().into()))?;
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
            StmtKind::While { test, body } => {
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
            StmtKind::If { test, body, orelse } => {
                if self.eval_expr(test)?.is_true() {
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
            StmtKind::EnumDef { name, variants, .. } => {
                let short = name.rsplit('.').next().unwrap_or(name);
                let variants = variants
                    .iter()
                    .map(|v| (v.name.clone(), format!("{short}.{}", v.name).into()))
                    .collect();
                self.enums.insert(name.clone(), variants);
                Ok(ControlFlow::None)
            }
            StmtKind::StructDef { name, fields, .. } => {
                let def = StructDef {
                    name: struct_display(name).into(),
                    fields: fields.iter().map(|f| f.name.clone()).collect(),
                };
                self.structs.insert(name.clone(), Rc::new(def));
                Ok(ControlFlow::None)
            }
            StmtKind::Match { subject, cases } => {
                let value = self.eval_expr(subject)?;
                for case in cases {
                    let mut bindings = vec![];
                    if self.pattern_matches(&case.pattern, &value, &mut bindings) {
                        for (name, value) in bindings {
                            self.set_var(name, &value)?;
                        }
                        return self.eval_block(&case.body);
                    }
                }
                Err("no case of the match fits the value".into())
            }
            StmtKind::Break => Ok(ControlFlow::Break),
            StmtKind::Continue => Ok(ControlFlow::Continue),
            StmtKind::Use { .. } => Err("use is only allowed at the top level of a file".into()),
        }
    }

    /// Returns whether `pattern` matches `value`, pushing the value each of its names binds to
    /// `bindings`.
    ///
    /// For example, `some(x)` matches `some(3)` and binds `x` to 3, but does not match `none`.
    fn pattern_matches<'p>(
        &self,
        pattern: &'p Pattern,
        value: &Value,
        bindings: &mut Vec<(&'p str, Value)>,
    ) -> bool {
        match &pattern.kind {
            PatternKind::Wildcard => true,
            PatternKind::Name(name) => {
                bindings.push((name, value.clone()));
                true
            }
            PatternKind::Variant { path, fields } => {
                let Value::Enum(found) = value else {
                    return false;
                };
                let tag = match path.as_slice() {
                    [(name, _)] => usize::from(name == "some"),
                    [(enum_name, _), (variant, _)] => {
                        match self.enums.get(enum_name).and_then(|variants| {
                            variants.iter().position(|(name, _)| name == variant)
                        }) {
                            Some(tag) => tag,
                            None => return false,
                        }
                    }
                    _ => return false,
                };
                found.tag == tag
                    && found.fields.len() == fields.len()
                    && fields
                        .iter()
                        .zip(&found.fields)
                        .all(|(field, value)| self.pattern_matches(field, value, bindings))
            }
        }
    }

    /// Makes the variant `variant` of the enum `enum_name` holding `fields`.
    fn variant(&self, enum_name: &str, variant: &str, fields: Vec<Value>) -> EvalResult<Value> {
        let variants = self
            .enums
            .get(enum_name)
            .ok_or_else(|| format!("'{enum_name}' is not an enum"))?;
        let tag = variants
            .iter()
            .position(|(name, _)| name == variant)
            .ok_or_else(|| format!("'{enum_name}' has no variant '{variant}'"))?;
        Ok(Value::Enum(Rc::new(EnumValue {
            name: variants[tag].1.clone(),
            tag,
            fields,
        })))
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

    /// Evaluates the arithmetic `expr`, computing each operation whose operands are both numbers
    /// as soon as they are known, left to right, and leaving those with a list among their
    /// operands for [`elementwise`].
    ///
    /// For example, `a * (2 + 3)` for a list `a` evaluates `a`, then `2 + 3`, and returns a
    /// multiplication of `a` by 5 for each element.
    fn arithmetic(&mut self, expr: &Expr) -> EvalResult<Lazy> {
        self.arithmetic_unlocated(expr)
            .map_err(|e| located(e, expr.span))
    }

    fn arithmetic_unlocated(&mut self, expr: &Expr) -> EvalResult<Lazy> {
        self.nested(|this| match &expr.kind {
            ExprKind::BinOp { op, left, right } => {
                let left = this.arithmetic(left)?;
                let right = this.arithmetic(right)?;
                combine(*op, left, right)
            }
            ExprKind::UnaryOp {
                op: op @ (UnaryOp::UnaryAdd | UnaryOp::UnarySub),
                operand,
            } => match this.arithmetic(operand)? {
                Lazy::Value(value) if !matches!(value, Value::List(_)) => {
                    Ok(Lazy::Value(unary(*op, value)?))
                }
                operand => Ok(Lazy::Unary(*op, Box::new(operand))),
            },
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
        value: &Expr,
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
    /// For example, `grid[1][0] += 3` reads `grid[1][0]` with `root` `"grid"` and the indexes 1
    /// and 0.
    fn read_place(&self, root: &str, steps: &[PlaceStep], spans: &[Span]) -> EvalResult<Value> {
        let mut current = self.get_var(root)?;
        for (step, &span) in steps.iter().zip(spans) {
            let next = match step {
                PlaceStep::Index(i) => {
                    let items = current.as_list()?;
                    let position = list_position(*i, items.len()).map_err(|e| located(e, span))?;
                    items[position].clone()
                }
                PlaceStep::Field(name) => {
                    let value = current.as_struct()?;
                    value.fields[value.field(name)?].clone()
                }
            };
            current = next;
        }
        Ok(current)
    }

    /// Evaluates `expr`, giving an error raised by it, rather than by an expression inside it,
    /// the position of `expr`.
    fn eval_expr(&mut self, expr: &Expr) -> EvalResult<Value> {
        self.nested(|this| this.eval_expr_unguarded(expr))
            .map_err(|e| located(e, expr.span))
    }

    fn eval_expr_unguarded(&mut self, expr: &Expr) -> EvalResult<Value> {
        match &expr.kind {
            // short-circuiting
            ExprKind::BoolOp { op, values } => {
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
            ExprKind::BinOp { .. }
            | ExprKind::UnaryOp {
                op: UnaryOp::UnaryAdd | UnaryOp::UnarySub,
                ..
            } => match self.arithmetic(expr)? {
                Lazy::Value(value) => Ok(value),
                lazy => elementwise(&lazy),
            },
            ExprKind::UnaryOp { op, operand } => {
                let val = self.eval_expr(operand)?;
                unary(*op, val)
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
            ExprKind::Call {
                func,
                args,
                keywords,
            } => {
                if let ExprKind::Name { id, .. } = &func.kind
                    && let Some(def) = self.structs.get(id).cloned()
                {
                    // the values in the order they are written, each into its field
                    let mut fields = vec![Value::Unit; def.fields.len()];
                    for keyword in keywords {
                        let value = self.eval_expr(&keyword.value)?;
                        let position = def
                            .fields
                            .iter()
                            .position(|field| *field == keyword.arg)
                            .ok_or_else(|| format!("'{id}' has no field '{}'", keyword.arg))?;
                        fields[position] = value;
                    }
                    return Ok(Value::Struct(Rc::new(StructValue { def, fields })));
                }
                if let ExprKind::Attribute { value, attr, .. } = &func.kind
                    && let ExprKind::Name { id, .. } = &value.kind
                    && self.enums.contains_key(id)
                {
                    let mut values = Vec::with_capacity(args.len());
                    for arg in args {
                        values.push(self.eval_expr(arg)?);
                    }
                    return self.variant(id, attr, values);
                }
                if let ExprKind::Attribute { value, attr, .. } = &func.kind
                    && stdlib::changes_receiver(attr)
                {
                    // the receiver's indexes, then the arguments, then the change
                    let (root, steps) = self.eval_place(value)?;
                    let mut values = Vec::with_capacity(args.len());
                    for arg in args {
                        values.push(self.eval_expr(arg)?);
                    }
                    let items = self.place_list(root, &steps, &place_spans(value))?;
                    return change_list(items, attr, values);
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
                        if let Some(path) = field_path(arg)
                            && let Some((root, _)) = path.split_once('.')
                            && (names.contains(&path) || names.iter().any(|name| name == root))
                        {
                            self.take_field(arg, id);
                        }
                    }
                }
                self.call(id, values)
            }
            ExprKind::Attribute { value, attr, .. } => match &value.kind {
                ExprKind::Name { id, .. } if self.enums.contains_key(id) => {
                    self.variant(id, attr, vec![])
                }
                _ => {
                    let value = self.eval_expr(value)?;
                    let value = value.as_struct()?;
                    Ok(value.fields[value.field(attr)?].clone())
                }
            },
            ExprKind::Constant { value, kind: _ } => Ok(Value::from(&**value)),
            ExprKind::Subscript { value, slice, .. } => {
                let list = self.eval_expr(value)?;
                if let ExprKind::Slice { lower, upper, step } = &slice.kind {
                    let mut bound = |bound: &Option<Box<Expr>>| -> EvalResult<Option<i64>> {
                        bound
                            .as_ref()
                            .map(|bound| self.eval_expr(bound)?.as_int())
                            .transpose()
                    };
                    let (lower, upper, step) = (bound(lower)?, bound(upper)?, bound(step)?);
                    if let Value::Str(text) = &list {
                        let (start, step, count) =
                            stdlib::slice_indices(text.len(), lower, upper, step)?;
                        let sliced = stdlib::str_slice(text, start, step, count)?;
                        return Ok(Value::Str(sliced.into()));
                    }
                    let items = list.as_list()?;
                    let (start, step, count) =
                        stdlib::slice_indices(items.len(), lower, upper, step)?;
                    let picked = (0..count as i64)
                        .map(|k| items[(start + k * step) as usize].clone())
                        .collect();
                    return Ok(Value::list(picked));
                }
                let index = self.eval_expr(slice)?.as_int()?;
                if let Value::Tuple(fields) = &list {
                    // the checker allows only a constant index in range
                    let position = list_position(index, fields.len())?;
                    return Ok(fields[position].clone());
                }
                if let Value::Str(text) = &list {
                    let position = position(index, text.len(), stdlib::STRING_INDEX_RANGE)?;
                    return Ok(Value::Str(stdlib::str_at(text, position)?.into()));
                }
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
            ExprKind::Tuple { elts, ctx: _ } => {
                let mut fields = Vec::with_capacity(elts.len());
                for elt in elts {
                    fields.push(self.eval_expr(elt)?);
                }
                Ok(Value::Tuple(fields.into()))
            }
            ExprKind::Slice { .. } => Err("a slice can only appear in brackets".into()),
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
            ("find", Value::Str(s), [Value::Str(text)]) => {
                Ok(Value::Int(s.find(&**text).map_or(-1, |at| at as i64)))
            }
            ("find", _, _) => Err("find() is called on a str, with a str".into()),
            ("startswith", Value::Str(s), [Value::Str(prefix)]) => {
                Ok(Value::Bool(s.starts_with(&**prefix)))
            }
            ("startswith", _, _) => Err("startswith() is called on a str, with a str".into()),
            ("endswith", Value::Str(s), [Value::Str(suffix)]) => {
                Ok(Value::Bool(s.ends_with(&**suffix)))
            }
            ("endswith", _, _) => Err("endswith() is called on a str, with a str".into()),
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

    /// Returns the `random` module's generator, seeding it from the system's entropy if nothing
    /// has seeded it yet.
    fn rng(&mut self) -> &mut Rng {
        self.rng.get_or_insert_with(Rng::from_entropy)
    }

    /// Calls the builtin or user-defined function named `name` with already evaluated arguments.
    fn call(&mut self, name: &str, args: Vec<Value>) -> EvalResult<Value> {
        match (name, &args[..]) {
            ("print", _) => {
                let mut line = String::new();
                for (i, value) in args.iter().enumerate() {
                    if i > 0 {
                        line.push(' ');
                    }
                    if value.write(&mut line, false, 0).is_err() {
                        // what was written before the value nested too deeply stays written
                        write!(self.out, "{line}")?;
                        return Err(stdlib::PRINT_DEPTH.into());
                    }
                }
                writeln!(self.out, "{line}")?;
                return Ok(Value::Unit);
            }
            ("some", [value]) => return Ok(some(value.clone())),
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
            _ => Value::Unit,
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
    /// Assigns `value` to `target`, a name, or a tuple target whose names take the value's fields
    /// in order, at any depth.
    ///
    /// For example, `a, (b, c)` takes `(1, (2, 3))` as `a = 1`, `b = 2`, and `c = 3`.
    fn assign_target(&mut self, target: &Expr, value: &Value) -> EvalResult<()> {
        match (&target.kind, value) {
            (ExprKind::Name { id, .. }, _) => {
                self.set_var(id, value)?;
            }
            (ExprKind::Tuple { elts, .. }, Value::Tuple(fields)) if elts.len() == fields.len() => {
                for (elt, field) in elts.iter().zip(fields.iter()) {
                    self.assign_target(elt, field)?;
                }
            }
            (ExprKind::Tuple { elts, .. }, other) => {
                return Err(format!(
                    "cannot unpack {} into {} names",
                    other.display(true),
                    elts.len()
                )
                .into());
            }
            _ => return Err("Invalid assignment target".into()),
        }
        Ok(())
    }

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

    /// Evaluates the indexes of `place`, a variable or a value inside one that a statement is
    /// about to change, and returns the variable's name with the steps to the value, outermost
    /// first.
    ///
    /// For example, `grid[i][j]` gives `"grid"` and the values of `i` and `j`, and `ps[i].x`
    /// gives `"ps"`, the value of `i`, and the field `x`. Nothing is checked against the values
    /// yet: [`Interpreter::place_value`] does that once the change is ready.
    fn eval_place<'e>(
        &mut self,
        place_expr: &'e Expr,
    ) -> EvalResult<(&'e str, Vec<PlaceStep<'e>>)> {
        let Some((root, exprs)) = place(place_expr) else {
            return Err("only a value in a variable can be changed".into());
        };
        let mut steps = Vec::with_capacity(exprs.len());
        for step in exprs {
            steps.push(match step {
                Step::Index(index) => PlaceStep::Index(self.eval_expr(index)?.as_int()?),
                Step::Field(name) => PlaceStep::Field(name),
            });
        }
        Ok((root, steps))
    }

    /// Walks from the variable `root` through `steps` to the value a statement changes, and
    /// returns it for changing.
    ///
    /// Every list and struct on the way that another value still shares is copied first, so the
    /// change shows through no other variable. The checker allows a function to change only its
    /// own variables, so `root` is in the function's scope, or in the globals at the top level.
    /// An index out of range fails at its subscript in `spans`, from [`place_spans`].
    fn place_value(
        &mut self,
        root: &str,
        steps: &[PlaceStep],
        spans: &[Span],
    ) -> EvalResult<&mut Value> {
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        let value = scope
            .get_mut(root)
            .ok_or_else(|| format!("'{root}' is used before it is assigned"))?;
        walk_place(value, steps, spans, &mut self.copies)
    }

    /// Walks to the list a statement changes, as [`Interpreter::place_value`] does, and returns
    /// its elements for changing, copying them first if another value still shares them.
    fn place_list(
        &mut self,
        root: &str,
        steps: &[PlaceStep],
        spans: &[Span],
    ) -> EvalResult<&mut Vec<Value>> {
        let scope = self.scopes.last_mut().unwrap_or(&mut self.globals);
        let value = scope
            .get_mut(root)
            .ok_or_else(|| format!("'{root}' is used before it is assigned"))?;
        let value = walk_place(value, steps, spans, &mut self.copies)?;
        self.copies += value.is_shared() as u64;
        value.as_list_mut()
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
                let items = value.as_list_mut()?;
                let position = list_position(*index, items.len()).map_err(|e| located(e, span))?;
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
/// element, or the name of a struct's field.
#[derive(Debug, Clone, Copy)]
enum PlaceStep<'a> {
    Index(i64),
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
/// enum, or a struct.
fn is_definition(stmt: &Stmt) -> bool {
    matches!(
        stmt.kind,
        StmtKind::FunctionDef { .. } | StmtKind::EnumDef { .. } | StmtKind::StructDef { .. }
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::driver::parse;

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
        let module = parse(source).unwrap();
        let mut out = Vec::new();
        let mut interpreter = Interpreter::with_output(&mut out, Limits::DEFAULT);
        interpreter.evaluate(&module).unwrap();
        interpreter.copies
    }

    /// A function that appends to its parameter and returns it.
    const ADD: &str = "fn add(xs: list[int], x: int) -> list[int];\n    xs.append(x)\n    ret xs\n";

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
            format!("{ADD}xs = []\nfor i in range(100);\n    xs = add(xs, i)\nprint(xs)\n");
        assert_eq!(copies(&source), 0);
        let source = format!(
            "{ADD}fn fill(xs, n);\n    if n == 0;\n        ret xs\n    xs.append(n)\n    \
             ret fill(xs, n - 1)\nprint(fill([], 50))\n"
        );
        assert_eq!(copies(&source), 0);
        // a call among the arguments runs statements of its own first
        let source = format!(
            "{ADD}fn one();\n    n = 1\n    ret n\nxs = []\nfor i in range(10);\n    \
             xs = add(xs, one())\nprint(xs)\n"
        );
        assert_eq!(copies(&source), 0);
    }

    #[test]
    fn a_field_changes_in_place_unless_another_value_shares_it() {
        let bag = "struct Bag;\n    items: list[int]\n    n: int\n";
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
            "{ADD}{bag}{make}for i in range(100);\n    b.items = add(b.items, i)\nprint(b.n)\n"
        );
        assert_eq!(copies(&source), 0);
        let source = format!(
            "{ADD}{bag}fn grow(b: Bag) -> Bag;\n    b.items = add(b.items, 1)\n    ret b\n\
             {make}for i in range(10);\n    b = grow(b)\nprint(b)\n"
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
        let source = "fn f(n: int) -> list[int];\n    xs = [n]\n    ys = xs\n    ys.append(1)\n    ret ys\n\
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
        let source = "fn peek(ys: list[int]) -> list[int];\n    ys.append(xs[0])\n    ret ys\nxs = [1]\nxs = peek(xs)\n\
                      print(xs)\n";
        assert_eq!(copies(source), 1);
    }

    /// Runs `source` and returns what it prints.
    fn output(source: &str) -> String {
        let module = parse(source).unwrap();
        let mut out = Vec::new();
        Interpreter::with_output(&mut out, Limits::DEFAULT)
            .evaluate(&module)
            .unwrap();
        String::from_utf8(out).unwrap()
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
            output("for k, (v, w) in [(1, (2, 3)), (4, (5, 6))];\n    print(k + v + w)\n"),
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
}
