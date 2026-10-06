//! The builtin functions shared by the interpreter and the compiler, and the limits both enforce.

/// The most function calls that can be active at once, in both backends.
///
/// For example, a recursive function can call itself 999 times from a top-level call, but a
/// 1,001st nested call stops the program with an error instead of overflowing the stack.
pub const MAX_CALL_DEPTH: usize = 1_000;

/// Names of the builtin functions.
///
/// `range` is only valid as the iterable of a `for` loop, such as `for i in range(3);`.
pub static BUILTINS: [&str; 4] = ["print", "range", "int", "float"];

/// Names of the builtin methods, which are called on a value, such as `xs.len()`.
pub static METHODS: [&str; 2] = ["len", "append"];

/// Documentation for a builtin function or method, for editor tooling.
pub struct BuiltinDoc {
    pub name: &'static str,
    /// How the builtin is called, such as `int(value: int | float) -> int`.
    pub signature: &'static str,
    pub description: &'static str,
}

pub static BUILTIN_DOCS: [BuiltinDoc; 4] = [
    BuiltinDoc {
        name: "print",
        signature: "print(values...) -> none",
        description: "Prints the values separated by spaces, followed by a newline.",
    },
    BuiltinDoc {
        name: "range",
        signature: "range(end) | range(start, end)",
        description: "Counts from `start`, or 0, up to but not including `end`. \
                      It can only be the iterable of a `for` loop.",
    },
    BuiltinDoc {
        name: "int",
        signature: "int(value: int | float) -> int",
        description: "Converts a number to an int, dropping any fraction, so `int(-2.5)` is -2. \
                      It stops the program if `value` is nan or does not fit in an int.",
    },
    BuiltinDoc {
        name: "float",
        signature: "float(value: int | float) -> float",
        description: "Converts a number to a float, rounding to the nearest float if needed.",
    },
];

pub static METHOD_DOCS: [BuiltinDoc; 2] = [
    BuiltinDoc {
        name: "len",
        signature: "(str | list[T]).len() -> int",
        description: "Returns the number of bytes in a string or elements in a list.",
    },
    BuiltinDoc {
        name: "append",
        signature: "list[T].append(item: T) -> none",
        description: "Adds `item` to the end of the list.",
    },
];

/// Returns the documentation for the builtin function named `name`.
///
/// For example, `builtin_doc("int")` has the signature `int(value: int | float) -> int`.
pub fn builtin_doc(name: &str) -> Option<&'static BuiltinDoc> {
    BUILTIN_DOCS.iter().find(|doc| doc.name == name)
}

/// Returns the documentation for the builtin method named `name`.
///
/// For example, `method_doc("len")` has the signature `(str | list[T]).len() -> int`.
pub fn method_doc(name: &str) -> Option<&'static BuiltinDoc> {
    METHOD_DOCS.iter().find(|doc| doc.name == name)
}

/// Returns a stone file that documents every builtin, for editors to open when asked where a
/// builtin is defined.
///
/// Builtins are part of the interpreter and the compiler rather than written in stone, so the
/// file is only comments, which keeps it valid stone. Each builtin function, then each builtin
/// method, gets a line holding `// ` and its signature, followed by its description.
pub fn builtins_reference() -> String {
    let mut text = String::from(
        "// stone's builtin functions and methods\n\
         //\n\
         // These are part of the interpreter and the compiler rather than written in stone, so\n\
         // this file only documents them. stone-lsp generates it, and editing it changes\n\
         // nothing.\n",
    );
    for doc in &BUILTIN_DOCS {
        text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
    }
    text += "\n// Methods, called on a value as in `xs.len()`\n";
    for doc in &METHOD_DOCS {
        text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
    }
    text
}

/// Formats a float the way `print` shows it in both backends, which is how Python's `repr` does.
///
/// The digits are the fewest that read back as the same float. Exponents from -4 through 15 are
/// written out in full with at least one digit after the point, and others use `e` notation with a
/// signed exponent of at least two digits. For example, `format_float(1.0)` returns `"1.0"`,
/// `format_float(0.1 + 0.2)` returns `"0.30000000000000004"`, `format_float(1e16)` returns
/// `"1e+16"`, and `format_float(1e-5)` returns `"1e-05"`.
///
/// Compiled code gets the same digits from libc by asking `snprintf` for 1, then 2, up to 17
/// significant digits until `strtod` reads the text back as the same float.
pub fn format_float(value: f64) -> String {
    if value.is_nan() {
        return "nan".into();
    }
    let sign = if value.is_sign_negative() { "-" } else { "" };
    if value.is_infinite() {
        return format!("{sign}inf");
    }
    // `{:e}` finds how many digits round-trip, and formatting again with that precision breaks
    // ties to even the way libc does, so 1462468587316101.25 shows as ...101.2, not ...101.3
    let shortest = format!("{:e}", value.abs());
    let count = shortest.split_once('e').unwrap().0.replace('.', "").len();
    let scientific = format!("{:.*e}", count - 1, value.abs());
    let (mantissa, exponent) = scientific.split_once('e').unwrap();
    let digits = mantissa.replace('.', "");
    let exponent: i32 = exponent.parse().unwrap();
    if (-4..16).contains(&exponent) {
        let text = if exponent < 0 {
            format!("0.{}{digits}", "0".repeat((-exponent - 1) as usize))
        } else {
            let whole = exponent as usize + 1;
            if digits.len() <= whole {
                format!("{digits}{}.0", "0".repeat(whole - digits.len()))
            } else {
                format!("{}.{}", &digits[..whole], &digits[whole..])
            }
        };
        format!("{sign}{text}")
    } else {
        let exponent_sign = if exponent < 0 { '-' } else { '+' };
        format!("{sign}{mantissa}e{exponent_sign}{:02}", exponent.abs())
    }
}

/// Raises `base` to a non-negative `exp` by squaring and multiplying, wrapping on overflow like
/// `*` does.
///
/// For example, `int_pow(2, 10)` returns `1024` and `int_pow(2, 64)` wraps around to `0`. Unlike
/// `i64::wrapping_pow`, the exponent can be any `i64`, so `int_pow(1, i64::MAX)` is `1`. Compiled
/// code runs the same loop, and since wrapping multiplication is exact modulo 2^64, it gets the
/// same result.
pub fn int_pow(base: i64, exp: i64) -> i64 {
    let (mut result, mut base, mut exp) = (1i64, base, exp as u64);
    while exp != 0 {
        if exp & 1 == 1 {
            result = result.wrapping_mul(base);
        }
        base = base.wrapping_mul(base);
        exp >>= 1;
    }
    result
}

/// Raises `base` to an int `exp` by squaring and multiplying, then takes the reciprocal for a
/// negative `exp`.
///
/// For example, `float_pow(2.5, 3)` returns `15.625` and `float_pow(2.0, -2)` returns `0.25`.
/// Compiled code does the same float operations in the same order, so the two backends round
/// identically, though the result can differ from a correctly rounded power in the last bits.
/// The reciprocal is IEEE division, so `float_pow(0.0, -1)` is infinity rather than an error.
pub fn float_pow(base: f64, exp: i64) -> f64 {
    let (mut result, mut base, mut magnitude) = (1.0, base, exp.unsigned_abs());
    while magnitude != 0 {
        if magnitude & 1 == 1 {
            result *= base;
        }
        base *= base;
        magnitude >>= 1;
    }
    if exp < 0 { 1.0 / result } else { result }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_builtin_is_documented() {
        let documented: Vec<&str> = BUILTIN_DOCS.iter().map(|doc| doc.name).collect();
        assert_eq!(documented, BUILTINS);
        for name in BUILTINS {
            let doc = builtin_doc(name).unwrap();
            assert!(doc.signature.starts_with(&format!("{name}(")), "{name}");
        }
        assert!(builtin_doc("nope").is_none());
    }

    #[test]
    fn every_method_is_documented() {
        let documented: Vec<&str> = METHOD_DOCS.iter().map(|doc| doc.name).collect();
        assert_eq!(documented, METHODS);
        for name in METHODS {
            let doc = method_doc(name).unwrap();
            assert!(doc.signature.contains(&format!(".{name}(")), "{name}");
            assert!(!BUILTINS.contains(&name), "{name}");
        }
        assert!(method_doc("print").is_none());
    }

    #[test]
    fn int_powers_wrap_like_multiplication() {
        assert_eq!(int_pow(2, 10), 1024);
        assert_eq!(int_pow(0, 0), 1);
        assert_eq!(int_pow(-3, 3), -27);
        assert_eq!(int_pow(3, 50), 3i64.wrapping_pow(50));
        assert_eq!(int_pow(2, 64), 0);
        assert_eq!(int_pow(1, i64::MAX), 1);
        assert_eq!(int_pow(-1, i64::MAX), -1);
    }

    #[test]
    fn float_powers_multiply_in_a_fixed_order() {
        assert_eq!(float_pow(2.5, 3), 15.625);
        assert_eq!(float_pow(2.0, -2), 0.25);
        assert_eq!(float_pow(0.0, -1), f64::INFINITY);
        assert_eq!(float_pow(-0.0, -1), f64::NEG_INFINITY);
        assert_eq!(float_pow(1.0, i64::MIN), 1.0);
        assert_eq!(float_pow(f64::NAN, 0), 1.0);
        assert!(float_pow(f64::NAN, 1).is_nan());
        assert_eq!(float_pow(1.5, 4), 5.0625);
        assert_eq!(float_pow(10.0, 400), f64::INFINITY);
    }

    #[test]
    fn floats_format_like_python_repr() {
        let cases = [
            (1.0, "1.0"),
            (0.1, "0.1"),
            (-2.5, "-2.5"),
            (0.1 + 0.2, "0.30000000000000004"),
            (123456.789, "123456.789"),
            // halfway between ...101.2 and ...101.3, so it rounds to even
            (1462468587316101.0 + 0.25, "1462468587316101.2"),
            (100.0, "100.0"),
            (1e14, "100000000000000.0"),
            (1e15, "1000000000000000.0"),
            (1e16, "1e+16"),
            (1.5e16, "1.5e+16"),
            (1e300, "1e+300"),
            (0.0001, "0.0001"),
            (0.00012, "0.00012"),
            (1e-5, "1e-05"),
            (1.5e-7, "1.5e-07"),
            (5e-324, "5e-324"),
            (f64::MAX, "1.7976931348623157e+308"),
            (0.0, "0.0"),
            (-0.0, "-0.0"),
            (f64::INFINITY, "inf"),
            (f64::NEG_INFINITY, "-inf"),
            (f64::NAN, "nan"),
        ];
        for (value, expected) in cases {
            assert_eq!(format_float(value), expected, "{value:e}");
        }
    }

    #[test]
    fn the_reference_is_valid_stone() {
        assert_eq!(crate::driver::check(&builtins_reference()), []);
    }

    #[test]
    fn the_reference_has_a_line_for_each_builtin() {
        let reference = builtins_reference();
        for doc in BUILTIN_DOCS.iter().chain(&METHOD_DOCS) {
            let line = format!("// {}", doc.signature);
            assert_eq!(
                reference.lines().filter(|l| *l == line).count(),
                1,
                "{line}"
            );
        }
    }
}
