//! The values of constants, such as `const port = 53`, which are worked out when a program is
//! built rather than when it runs.
//!
//! A constant's value is made of literals, other constants, `math.pi`, `math.tau`, and `math.e`,
//! the operators, and tuples of them. [`evaluate`] computes it with the interpreter's own
//! operators, so a constant is exactly the value `stone run` would compute, and an operation
//! that would fail at runtime, such as a division by zero, fails the build instead.

use crate::ast::{BoolOp, CompOp, Expr, ExprKind};
use crate::interpreter::{Value, binary, unary};
use crate::span::Span;
use std::fmt;
use std::rc::Rc;

/// The value of a constant: an int, float, bool, str, bytes, or a tuple of them.
///
/// For example, `const limits = (3, 0.5)` is `Tuple([Int(3), Float(0.5)])`.
#[derive(Debug, Clone, PartialEq)]
pub enum ConstValue {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Bytes(Vec<u8>),
    Tuple(Vec<ConstValue>),
}

/// Why a constant has no value.
#[derive(Debug, Clone, PartialEq)]
pub enum Failure {
    /// The expression at the span cannot be known when the program is built, for the reason
    /// given, as `call a function` for `f(1)`.
    NotConstant(Span, String),
    /// Evaluating the expression at the span fails with the runtime error given, such as
    /// `division by zero`.
    Fails(Span, String),
    /// A constant it reads has no value, which was reported where that one is defined.
    Silent,
}

/// What a lookup answers for a name: the value of a constant (`Some(Ok(..))`), a constant with
/// no value (`Some(Err(()))`), or `None` for a name that is no constant.
///
/// For example, a lookup of `step` with `const step = 3` answers `Some(Ok(ConstValue::Int(3)))`.
type Lookup = Option<Result<ConstValue, ()>>;

/// Evaluates the value of a constant, `expr`, asking `lookup` for each name it reads.
///
/// For example, `2 * step + 1` with `step` a constant of 3 gives `Int(7)`, and `1 / 0` gives
/// `Failure::Fails` with `division by zero` at the division.
pub fn evaluate(
    expr: &Expr,
    lookup: &mut dyn FnMut(&str, Span) -> Lookup,
) -> Result<ConstValue, Failure> {
    let not = |what: &str| Err(Failure::NotConstant(expr.span, what.to_string()));
    match &expr.kind {
        ExprKind::Constant { value, .. } => Ok(ConstValue::from(Value::from(&**value))),
        ExprKind::Name { id, .. } => match lookup(id, expr.span) {
            Some(Ok(value)) => Ok(value),
            Some(Err(())) => Err(Failure::Silent),
            None => match crate::stdlib::module_constant(id) {
                Some(value) => Ok(ConstValue::Float(value)),
                None => not(&format!("read '{id}', which is not a constant")),
            },
        },
        ExprKind::UnaryOp { op, operand } => {
            let value = evaluate(operand, lookup)?;
            run(expr.span, unary(*op, Value::from(value)))
        }
        ExprKind::BinOp { op, left, right } => {
            let left = evaluate(left, lookup)?;
            let right = evaluate(right, lookup)?;
            run(
                expr.span,
                binary(*op, Value::from(left), Value::from(right)),
            )
        }
        ExprKind::BoolOp { op, values } => {
            // the operands are bools, and `and` stops at the first false one as at runtime
            let stop = matches!(op, BoolOp::Or);
            for value in values {
                if evaluate(value, lookup)? == ConstValue::Bool(stop) {
                    return Ok(ConstValue::Bool(stop));
                }
            }
            Ok(ConstValue::Bool(!stop))
        }
        ExprKind::Compare {
            left,
            ops,
            comparators,
        } => {
            let mut left = evaluate(left, lookup)?;
            for (op, right) in ops.iter().zip(comparators) {
                let span = right.span;
                let right = evaluate(right, lookup)?;
                if !compare(op, &left, &right).map_err(|what| Failure::NotConstant(span, what))? {
                    return Ok(ConstValue::Bool(false));
                }
                left = right;
            }
            Ok(ConstValue::Bool(true))
        }
        ExprKind::Tuple { elts, .. } => elts
            .iter()
            .map(|elt| evaluate(elt, lookup))
            .collect::<Result<_, _>>()
            .map(ConstValue::Tuple),
        ExprKind::Subscript { value, slice, .. } => {
            let tuple = evaluate(value, lookup)?;
            let ConstValue::Tuple(values) = tuple else {
                return not("index a value other than a tuple");
            };
            let ConstValue::Int(index) = evaluate(slice, lookup)? else {
                return not("index a tuple with anything but an int");
            };
            // the checker requires the index to be in range
            let at = if index < 0 {
                index + values.len() as i64
            } else {
                index
            };
            usize::try_from(at)
                .ok()
                .and_then(|at| values.get(at).cloned())
                .ok_or(Failure::Silent)
        }
        ExprKind::Call { .. } => not("call a function"),
        ExprKind::List { .. } => not("hold a list"),
        ExprKind::Dict { .. } => not("hold a map"),
        ExprKind::Set { .. } => not("hold a set"),
        ExprKind::JoinedStr { .. } => not("write values into a string"),
        ExprKind::Attribute { .. } => not("read a field"),
        ExprKind::Try { .. } => not("use '?'"),
        _ => not("hold this expression"),
    }
}

/// Gives the value an operator computed, or its runtime error at `span`.
fn run(
    span: Span,
    result: Result<Value, Box<dyn std::error::Error>>,
) -> Result<ConstValue, Failure> {
    result
        .map(ConstValue::from)
        .map_err(|error| Failure::Fails(span, error.to_string()))
}

/// Compares two values as the interpreter does, or says why the comparison is not constant.
fn compare(op: &CompOp, left: &ConstValue, right: &ConstValue) -> Result<bool, String> {
    let (left, right) = (Value::from(left.clone()), Value::from(right.clone()));
    let order = || {
        left.compare(&right)
            .map_err(|_| "order these values".to_string())
    };
    Ok(match op {
        CompOp::Equal => left.equals(&right, 0).map_err(|e| e.to_string())?,
        CompOp::NotEqual => !left.equals(&right, 0).map_err(|e| e.to_string())?,
        CompOp::LessThan => order()?.is_some_and(|o| o.is_lt()),
        CompOp::LessThanEqual => order()?.is_some_and(|o| o.is_le()),
        CompOp::GreaterThan => order()?.is_some_and(|o| o.is_gt()),
        CompOp::GreaterThanEqual => order()?.is_some_and(|o| o.is_ge()),
        CompOp::In | CompOp::NotIn => return Err("test membership".to_string()),
    })
}

impl From<Value> for ConstValue {
    fn from(value: Value) -> Self {
        match value {
            Value::Int(n) => ConstValue::Int(n),
            Value::Float(x) => ConstValue::Float(x),
            Value::Bool(b) => ConstValue::Bool(b),
            Value::Str(s) => ConstValue::Str(s.to_string()),
            Value::Bytes(b) => ConstValue::Bytes(b.to_vec()),
            Value::Tuple(values) => {
                ConstValue::Tuple(values.iter().cloned().map(ConstValue::from).collect())
            }
            // no operator on constants gives anything else
            other => ConstValue::Str(other.display(false)),
        }
    }
}

impl From<ConstValue> for Value {
    fn from(value: ConstValue) -> Self {
        match value {
            ConstValue::Int(n) => Value::Int(n),
            ConstValue::Float(x) => Value::Float(x),
            ConstValue::Bool(b) => Value::Bool(b),
            ConstValue::Str(s) => Value::Str(Rc::new(s)),
            ConstValue::Bytes(b) => Value::Bytes(b.into()),
            ConstValue::Tuple(values) => {
                Value::Tuple(values.into_iter().map(Value::from).collect())
            }
        }
    }
}

/// Writes the value as stone source writes it, for editors.
///
/// For example, `Str("a\n{")` is written `"a\n{{"`, and `Tuple([Int(1), Float(0.5)])` is
/// written `(1, 0.5)`.
impl fmt::Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ConstValue::Int(n) => write!(f, "{n}"),
            ConstValue::Float(x) => f.write_str(&crate::stdlib::format_float(*x)),
            ConstValue::Bool(b) => write!(f, "{b}"),
            ConstValue::Str(s) => {
                f.write_str("\"")?;
                for c in s.chars() {
                    match c {
                        '"' => f.write_str("\\\"")?,
                        '\\' => f.write_str("\\\\")?,
                        '\n' => f.write_str("\\n")?,
                        '\t' => f.write_str("\\t")?,
                        '\r' => f.write_str("\\r")?,
                        '{' => f.write_str("{{")?,
                        '}' => f.write_str("}}")?,
                        c if c.is_ascii_control() => write!(f, "\\x{:02x}", c as u32)?,
                        c => write!(f, "{c}")?,
                    }
                }
                f.write_str("\"")
            }
            ConstValue::Bytes(bytes) => {
                f.write_str("b\"")?;
                for &b in bytes {
                    match b {
                        b'"' => f.write_str("\\\"")?,
                        b'\\' => f.write_str("\\\\")?,
                        b' '..=b'~' => write!(f, "{}", b as char)?,
                        _ => write!(f, "\\x{b:02x}")?,
                    }
                }
                f.write_str("\"")
            }
            ConstValue::Tuple(values) => {
                f.write_str("(")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        f.write_str(", ")?;
                    }
                    write!(f, "{value}")?;
                }
                f.write_str(")")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Mod, StmtKind};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    /// Evaluates the value of `const c = <source>` with no other constants.
    fn value_of(source: &str) -> Result<ConstValue, Failure> {
        let tokens = Lexer::new(&format!("const c = {source}\n")).lex().unwrap();
        let Mod::Module { body } = Parser::new(&tokens).parse().unwrap();
        let StmtKind::ConstDef { value, .. } = &body[0].kind else {
            panic!("expected a constant");
        };
        evaluate(value, &mut |_, _| None)
    }

    #[test]
    fn constants_compute_as_the_interpreter_does() {
        assert_eq!(value_of("2 * 3 + 1"), Ok(ConstValue::Int(7)));
        assert_eq!(value_of("-7 / 2"), Ok(ConstValue::Int(-3)));
        assert_eq!(value_of("1 << 4 | 1"), Ok(ConstValue::Int(17)));
        assert_eq!(
            value_of("9223372036854775807 + 1"),
            Ok(ConstValue::Int(i64::MIN))
        );
        assert_eq!(value_of("1.0 / 4.0"), Ok(ConstValue::Float(0.25)));
        assert_eq!(
            value_of("\"ab\" + \"c\""),
            Ok(ConstValue::Str("abc".to_string()))
        );
        assert_eq!(
            value_of("b\"\\x00\" + b\"a\""),
            Ok(ConstValue::Bytes(vec![0, b'a']))
        );
        assert_eq!(value_of("1 < 2 < 2"), Ok(ConstValue::Bool(false)));
        assert_eq!(
            value_of("not (1 == 2) and true"),
            Ok(ConstValue::Bool(true))
        );
        assert_eq!(
            value_of("(1, (2.5, \"x\"))[1][0]"),
            Ok(ConstValue::Float(2.5))
        );
        assert_eq!(value_of("(1, 2)[-1]"), Ok(ConstValue::Int(2)));
    }

    #[test]
    fn a_constant_that_would_fail_at_runtime_fails() {
        let Err(Failure::Fails(span, message)) = value_of("1 + 10 / (2 - 2)") else {
            panic!("expected a failure");
        };
        assert_eq!(message, "division by zero");
        assert_eq!((span.start.col, span.end.col), (15, 27));
        assert!(matches!(value_of("1 << 64"), Err(Failure::Fails(_, _))));
        assert!(matches!(value_of("2 ** -1"), Err(Failure::Fails(_, _))));
    }

    #[test]
    fn only_literals_constants_and_operators_are_constant() {
        let reason = |source: &str| match value_of(source) {
            Err(Failure::NotConstant(_, reason)) => reason,
            other => panic!("expected no value for {source}, found {other:?}"),
        };
        assert_eq!(reason("f(1)"), "call a function");
        assert_eq!(reason("1 + x"), "read 'x', which is not a constant");
        assert_eq!(reason("[1]"), "hold a list");
        assert_eq!(reason("\"{1}\""), "write values into a string");
        assert_eq!(reason("1 in (1, 2)"), "test membership");
    }

    #[test]
    fn constants_are_written_as_source() {
        let text = |source: &str| value_of(source).unwrap().to_string();
        assert_eq!(text("\"a\\n{{\\\"\""), "\"a\\n{{\\\"\"");
        assert_eq!(text("b\"\\x00a\\\"\""), "b\"\\x00a\\\"\"");
        assert_eq!(text("(1, 0.5, true)"), "(1, 0.5, true)");
        assert_eq!(text("1.0 / 3.0"), "0.3333333333333333");
    }
}
