//! The builtin `math` module: absolute values, the least and greatest of several numbers, square
//! roots, and rounding down.
//!
//! A program imports it like any builtin module (see [`super::BuiltinModule`]), with `use math`
//! then `math.sqrt(2)`, or with `use math.sqrt` then `sqrt(2)`.
//!
//! The functions here are what the interpreter runs. Compiled code lowers each call to IR that
//! does the same operations in the same order, so both backends agree to the bit.

use super::BuiltinDoc;

/// The name a program imports the module by, as in `use math`.
pub const MODULE: &str = "math";

/// What the module is for, which editors show for `math`.
pub const MODULE_DOC: &str = "The builtin module of math on ints and floats.";

pub static DOCS: [BuiltinDoc; 5] = [
    BuiltinDoc {
        name: "math.abs",
        signature: "math.abs(x: int | float) -> int | float",
        description: "Returns `x` without its sign, as the same type, so `math.abs(-2.5)` is \
                      2.5 and `math.abs(-0.0)` is 0.0. It stops the program for the smallest \
                      int, whose absolute value does not fit in an int.",
    },
    BuiltinDoc {
        name: "math.min",
        signature: "math.min(values: int | float...) | math.min(values: list[int | float])",
        description: "Returns the least of two or more numbers of the same type, or of the \
                      numbers in a list, keeping the first of equal ones. It stops the program \
                      for an empty list.",
    },
    BuiltinDoc {
        name: "math.max",
        signature: "math.max(values: int | float...) | math.max(values: list[int | float])",
        description: "Returns the greatest of two or more numbers of the same type, or of the \
                      numbers in a list, keeping the first of equal ones. It stops the program \
                      for an empty list.",
    },
    BuiltinDoc {
        name: "math.sqrt",
        signature: "math.sqrt(x: int | float) -> float",
        description: "Returns the square root of `x`, correctly rounded, so `math.sqrt(2)` is \
                      1.4142135623730951. It stops the program if `x` is negative.",
    },
    BuiltinDoc {
        name: "math.floor",
        signature: "math.floor(x: int | float) -> int",
        description: "Returns the greatest int that is at most `x`, so `math.floor(-2.5)` is -3. \
                      It stops the program if `x` is nan or does not fit in an int.",
    },
];

/// The runtime error of `math.abs` of the smallest int.
pub const ABS_OVERFLOW: &str = "integer overflow in abs";

/// The runtime error of `math.sqrt` of a negative number, which is Python's message.
pub const DOMAIN_ERROR: &str = "math domain error";

/// The runtime error of `math.min` of an empty list.
pub const MIN_OF_EMPTY: &str = "min of an empty list";

/// The runtime error of `math.max` of an empty list.
pub const MAX_OF_EMPTY: &str = "max of an empty list";

/// The runtime error of converting a float that is nan or out of range to an int, shared with
/// `int`.
pub const FLOAT_TO_INT: &str = "cannot convert float to int (nan or out of range)";

/// Returns the absolute value of an int, or the error [`ABS_OVERFLOW`] for `i64::MIN`.
///
/// For example, `abs_int(-3)` returns `Ok(3)`.
pub fn abs_int(x: i64) -> Result<i64, String> {
    x.checked_abs().ok_or_else(|| ABS_OVERFLOW.to_string())
}

/// Returns the absolute value of a float as compiled code computes it: `0.0 - x` when `x <= 0.0`,
/// otherwise `x`.
///
/// For example, `abs_float(-2.5)` returns `2.5`, and `abs_float(-0.0)` returns `0.0`, since
/// `0.0 - -0.0` is `0.0`. A nan comes back unchanged, which prints the same as any other nan.
pub fn abs_float(x: f64) -> f64 {
    if x <= 0.0 { 0.0 - x } else { x }
}

/// Folds `values` from the left, keeping the first value and replacing it with a later one only
/// when `better(later, kept)` holds, as Python's `min` and `max` do.
///
/// For example, `fold(&[3, 1, 2], |a, b| a < b)` returns `Some(1)`, and an empty slice returns
/// `None`.
pub fn fold<T: Copy>(values: &[T], better: impl Fn(T, T) -> bool) -> Option<T> {
    let (&first, rest) = values.split_first()?;
    Some(rest.iter().fold(
        first,
        |kept, &value| if better(value, kept) { value } else { kept },
    ))
}

/// Returns the least of `values`, keeping the first of equal ones, or the error [`MIN_OF_EMPTY`].
///
/// For example, `min(&[0.0, -0.0])` returns `Ok(0.0)`, since `-0.0 < 0.0` is false.
pub fn min<T: Copy + PartialOrd>(values: &[T]) -> Result<T, String> {
    fold(values, |a, b| a < b).ok_or_else(|| MIN_OF_EMPTY.to_string())
}

/// Returns the greatest of `values`, keeping the first of equal ones, or the error
/// [`MAX_OF_EMPTY`].
///
/// For example, `max(&[f64::NAN, 1.0])` returns nan, since `1.0 > nan` is false, as in Python.
pub fn max<T: Copy + PartialOrd>(values: &[T]) -> Result<T, String> {
    fold(values, |a, b| a > b).ok_or_else(|| MAX_OF_EMPTY.to_string())
}

/// Returns the square root of `x`, or the error [`DOMAIN_ERROR`] if `x` is negative.
///
/// For example, `sqrt(4.0)` returns `Ok(2.0)`, `sqrt(-0.0)` returns `Ok(-0.0)`, and `sqrt` of nan
/// is nan.
pub fn sqrt(x: f64) -> Result<f64, String> {
    if x < 0.0 {
        return Err(DOMAIN_ERROR.to_string());
    }
    Ok(x.sqrt())
}

/// Returns the greatest int that is at most `x`, or the error [`FLOAT_TO_INT`] if `x` is nan or
/// out of range.
///
/// For example, `floor(-2.5)` returns `Ok(-3)` and `floor(2.5)` returns `Ok(2)`. Compiled code
/// truncates toward zero, converts back, and subtracts 1 if that went up, which is exact for
/// every float in range.
pub fn floor(x: f64) -> Result<i64, String> {
    // -2^63 fits in an int but 2^63 does not, and nan fails both checks
    if !(i64::MIN as f64..-(i64::MIN as f64)).contains(&x) {
        return Err(FLOAT_TO_INT.to_string());
    }
    let truncated = x as i64;
    Ok(if (truncated as f64) > x {
        truncated - 1
    } else {
        truncated
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn abs_of_the_smallest_int_overflows() {
        assert_eq!(abs_int(-3), Ok(3));
        assert_eq!(abs_int(0), Ok(0));
        assert_eq!(abs_int(i64::MAX), Ok(i64::MAX));
        assert_eq!(abs_int(i64::MIN + 1), Ok(i64::MAX));
        assert_eq!(abs_int(i64::MIN), Err(ABS_OVERFLOW.to_string()));
    }

    #[test]
    fn abs_of_a_float_clears_the_sign() {
        assert_eq!(abs_float(-2.5), 2.5);
        assert_eq!(abs_float(2.5), 2.5);
        assert!(abs_float(-0.0).is_sign_positive());
        assert!(abs_float(0.0).is_sign_positive());
        assert_eq!(abs_float(f64::NEG_INFINITY), f64::INFINITY);
        assert!(abs_float(f64::NAN).is_nan());
    }

    #[test]
    fn min_and_max_keep_the_first_of_equal_values() {
        assert_eq!(min(&[3, 1, 2]), Ok(1));
        assert_eq!(max(&[3, 1, 2]), Ok(3));
        assert_eq!(max(&[7]), Ok(7));
        assert!(min(&[0.0f64, -0.0]).unwrap().is_sign_positive());
        assert!(max(&[-0.0f64, 0.0]).unwrap().is_sign_negative());
        // a nan first stays, and a nan later never replaces anything
        assert!(max(&[f64::NAN, 1.0]).unwrap().is_nan());
        assert_eq!(max(&[1.0, f64::NAN]), Ok(1.0));
        assert_eq!(min::<i64>(&[]), Err(MIN_OF_EMPTY.to_string()));
        assert_eq!(max::<f64>(&[]), Err(MAX_OF_EMPTY.to_string()));
    }

    #[test]
    fn sqrt_of_a_negative_number_is_an_error() {
        assert_eq!(sqrt(4.0), Ok(2.0));
        assert_eq!(sqrt(2.0), Ok(std::f64::consts::SQRT_2));
        assert!(sqrt(-0.0).unwrap().is_sign_negative());
        assert!(sqrt(f64::NAN).unwrap().is_nan());
        assert_eq!(sqrt(f64::INFINITY), Ok(f64::INFINITY));
        assert_eq!(sqrt(-1.0), Err(DOMAIN_ERROR.to_string()));
        assert_eq!(sqrt(f64::NEG_INFINITY), Err(DOMAIN_ERROR.to_string()));
    }

    #[test]
    fn floor_rounds_toward_negative_infinity() {
        assert_eq!(floor(2.5), Ok(2));
        assert_eq!(floor(-2.5), Ok(-3));
        assert_eq!(floor(-2.0), Ok(-2));
        assert_eq!(floor(-0.0), Ok(0));
        assert_eq!(floor(-0.5), Ok(-1));
        assert_eq!(floor(4503599627370495.5), Ok(4503599627370495));
        assert_eq!(floor(-4503599627370495.5), Ok(-4503599627370496));
        assert_eq!(floor(-9223372036854775808.0), Ok(i64::MIN));
        for bad in [f64::NAN, f64::INFINITY, 9223372036854775808.0, -1e19] {
            assert_eq!(floor(bad), Err(FLOAT_TO_INT.to_string()), "{bad}");
        }
    }
}
