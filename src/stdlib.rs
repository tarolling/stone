//! The builtin functions shared by the interpreter and the compiler, and the limits both enforce.

pub mod math;
pub mod os;
pub mod random;
pub mod time;

/// A module that is part of stone rather than a file of the program, such as `os`.
///
/// A program imports one like a module file, with `use os` then `os.env("HOME")`, or with
/// `use os.env` then `env("HOME")`. Linking renames each call to the function's linked name,
/// such as `os.env`, which no stone name can collide with since it holds a dot, and the checker
/// and both backends treat that name as a builtin.
pub struct BuiltinModule {
    /// The name a program imports it by, as in `use os`.
    pub name: &'static str,
    /// What the module is for, which editors show for its name.
    pub doc: &'static str,
    /// Every function of the module, each named by its linked name, such as `os.env`.
    pub functions: &'static [BuiltinDoc],
}

/// Every builtin module.
pub static MODULES: [BuiltinModule; 4] = [
    BuiltinModule {
        name: os::MODULE,
        doc: os::MODULE_DOC,
        functions: &os::DOCS,
    },
    BuiltinModule {
        name: math::MODULE,
        doc: math::MODULE_DOC,
        functions: &math::DOCS,
    },
    BuiltinModule {
        name: random::MODULE,
        doc: random::MODULE_DOC,
        functions: &random::DOCS,
    },
    BuiltinModule {
        name: time::MODULE,
        doc: time::MODULE_DOC,
        functions: &time::DOCS,
    },
];

/// Returns the builtin module named `name`.
///
/// For example, `module("os")` is the `os` module, and `module("util")` is `None`.
pub fn module(name: &str) -> Option<&'static BuiltinModule> {
    MODULES.iter().find(|module| module.name == name)
}

/// Returns the documentation for the function of a builtin module with the linked name `name`.
///
/// For example, `module_function("os.env")` has the signature `os.env(name: str) -> str`, and
/// `module_function("env")` is `None`.
pub fn module_function(name: &str) -> Option<&'static BuiltinDoc> {
    let (prefix, _) = name.split_once('.')?;
    module(prefix)?
        .functions
        .iter()
        .find(|doc| doc.name == name)
}

/// The most function calls that can be active at once, in both backends.
///
/// For example, a recursive function can call itself 999 times from a top-level call, but a
/// 1,001st nested call stops the program with an error instead of overflowing the stack.
pub const MAX_CALL_DEPTH: usize = 1_000;

/// Names of the builtin functions.
///
/// `range` is only valid as the iterable of a `for` loop, such as `for i in range(3);`.
pub static BUILTINS: [&str; 8] = [
    "print", "range", "int", "float", "str", "input", "eof", "args",
];

/// Names of the builtin methods, which are called on a value, such as `xs.len()`.
pub static METHODS: [&str; 4] = ["len", "append", "strip", "split"];

/// Returns whether the builtin method `name` changes the value it is called on, as `append` does,
/// rather than only reading it.
///
/// Such a call changes the variable that holds the value, like an assignment, so it must be a
/// statement of its own whose receiver is a variable or an element of one, such as
/// `grid[0].append(1)`.
pub fn changes_receiver(name: &str) -> bool {
    name == "append"
}

/// Documentation for a builtin function or method, for editor tooling.
pub struct BuiltinDoc {
    pub name: &'static str,
    /// How the builtin is called, such as `int(value: int | float) -> int`.
    pub signature: &'static str,
    pub description: &'static str,
}

pub static BUILTIN_DOCS: [BuiltinDoc; 8] = [
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
        signature: "int(value: int | float | str) -> int",
        description: "Converts a number to an int, dropping any fraction, so `int(-2.5)` is -2, \
                      or reads a decimal int from a string, such as `int(\" -42\\n\")`. \
                      It stops the program if `value` is nan, does not fit in an int, \
                      or is a string that is not an int.",
    },
    BuiltinDoc {
        name: "float",
        signature: "float(value: int | float | str) -> float",
        description: "Converts a number to a float, rounding to the nearest float if needed, \
                      or reads a float from a string, such as `float(\"2.5e3\")` or \
                      `float(\"inf\")`. It stops the program if the string is not a float.",
    },
    BuiltinDoc {
        name: "str",
        signature: "str(value: int | float | bool | str) -> str",
        description: "Returns the text `print` would write for `value`, so `str(1.0)` is `\"1.0\"`.",
    },
    BuiltinDoc {
        name: "input",
        signature: "input() | input(prompt: str) -> str",
        description: "Writes `prompt`, if given, then reads a line from standard input and \
                      returns it without its newline. It returns `\"\"` at the end of the input, \
                      so check `eof()` to tell that apart from an empty line.",
    },
    BuiltinDoc {
        name: "eof",
        signature: "eof() -> bool",
        description: "Returns whether standard input has nothing left to read, waiting for \
                      input if none has arrived yet.",
    },
    BuiltinDoc {
        name: "args",
        signature: "args() -> list[str]",
        description: "Returns the arguments given after the program, so `stone run f.st a b` \
                      and a built `./f a b` both see `[\"a\", \"b\"]`.",
    },
];

pub static METHOD_DOCS: [BuiltinDoc; 4] = [
    BuiltinDoc {
        name: "len",
        signature: "(str | list[T]).len() -> int",
        description: "Returns the number of bytes in a string or elements in a list.",
    },
    BuiltinDoc {
        name: "append",
        signature: "list[T].append(item: T) -> none",
        description: "Adds `item` to the end of the list, changing only the variable it is \
                      called on, since lists are values. It is a statement of its own.",
    },
    BuiltinDoc {
        name: "strip",
        signature: "str.strip() -> str",
        description: "Returns the string without the spaces, tabs, and line breaks at either end.",
    },
    BuiltinDoc {
        name: "split",
        signature: "str.split() | str.split(separator: str) -> list[str]",
        description: "Splits the string at each `separator`, keeping empty pieces, so \
                      `\"a,,b\".split(\",\")` is `[\"a\", \"\", \"b\"]`. Without a separator it \
                      splits at runs of whitespace and drops empty pieces, so \
                      `\" a  b \".split()` is `[\"a\", \"b\"]`. An empty separator stops the program.",
    },
];

/// Returns whether `name` is a builtin function: one of [`BUILTINS`] or the linked name of a
/// function of a builtin module.
///
/// For example, `is_builtin("print")` and `is_builtin("os.env")` are true, but `is_builtin("env")`
/// is false.
pub fn is_builtin(name: &str) -> bool {
    BUILTINS.contains(&name) || module_function(name).is_some()
}

/// Returns the documentation for the builtin function named `name`, which may be a function of
/// a builtin module by its linked name.
///
/// For example, `builtin_doc("int")` has the signature `int(value: int | float) -> int`, and
/// `builtin_doc("os.pid")` has `os.pid() -> int`.
pub fn builtin_doc(name: &str) -> Option<&'static BuiltinDoc> {
    BUILTIN_DOCS
        .iter()
        .find(|doc| doc.name == name)
        .or_else(|| module_function(name))
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
/// method, then each function of each builtin module, gets a line holding `// ` and its
/// signature, followed by its description.
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
    for module in &MODULES {
        let name = module.name;
        text += &format!("\n// The {name} module, imported with `use {name}`\n");
        for doc in module.functions {
            text += &format!("\n// {}\n//     {}\n", doc.signature, doc.description);
        }
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
/// Compiled code follows the same steps with the bignums of its float runtime: it counts the
/// shortest digits the way Rust's `{:e}` does, then rounds the exact value to that many digits,
/// half to even.
pub fn format_float(value: f64) -> String {
    if value.is_nan() {
        return "nan".into();
    }
    let sign = if value.is_sign_negative() { "-" } else { "" };
    if value.is_infinite() {
        return format!("{sign}inf");
    }
    // `{:e}` finds how many digits round-trip, and formatting again with that precision breaks
    // ties to even, so 1462468587316101.25 shows as ...101.2, not ...101.3
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

/// The characters `strip`, `split()`, `int`, and `float` treat as whitespace, which are the ones
/// C's `isspace` accepts: space, tab, newline, vertical tab, form feed, and carriage return.
pub fn is_space(c: char) -> bool {
    matches!(c, ' ' | '\t' | '\n' | '\x0b' | '\x0c' | '\r')
}

/// Returns `text` without whitespace at either end.
///
/// For example, `strip(" a b\n")` returns `"a b"`.
pub fn strip(text: &str) -> &str {
    text.trim_matches(is_space)
}

/// Splits `text` at runs of whitespace, dropping empty pieces, like Python's `str.split()`.
///
/// For example, `split_whitespace(" a  b ")` returns `["a", "b"]`.
pub fn split_whitespace(text: &str) -> Vec<&str> {
    text.split(is_space)
        .filter(|piece| !piece.is_empty())
        .collect()
}

/// Splits `text` at each `separator`, keeping empty pieces, like Python's `str.split(sep)`.
///
/// For example, `split("a,,b", ",")` returns `["a", "", "b"]`, and an empty separator is the
/// error `empty separator`.
pub fn split<'a>(text: &'a str, separator: &str) -> Result<Vec<&'a str>, String> {
    if separator.is_empty() {
        return Err("empty separator".into());
    }
    Ok(text.split(separator).collect())
}

/// Reads a decimal int the way `int` does: whitespace at either end, an optional sign, then one or
/// more ASCII digits.
///
/// For example, `parse_int(" -42\n")` returns `-42`, `parse_int("1.5")` fails with
/// `invalid literal for int() with base 10: '1.5'`, and a number that does not fit in an int
/// fails with `int() argument out of range: '...'`. Compiled code reads the digits the same way,
/// checking each step for overflow.
pub fn parse_int(text: &str) -> Result<i64, String> {
    let trimmed = strip(text);
    let digits = trimmed.strip_prefix(['+', '-']).unwrap_or(trimmed);
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return Err(format!("invalid literal for int() with base 10: '{text}'"));
    }
    trimmed
        .parse()
        .map_err(|_| format!("int() argument out of range: '{text}'"))
}

/// Reads a float the way `float` does: whitespace at either end around a decimal number such as
/// `-1.5e3`, `.5`, or `3.`, or `inf`, `infinity`, or `nan` in any case, each with an optional sign.
///
/// For example, `parse_float(" 2.5\n")` returns `2.5`, `parse_float("1e999")` returns infinity,
/// and `parse_float("0x1p3")` fails with `could not convert string to float: '0x1p3'`. Compiled
/// code checks the same grammar, then rounds the number to the nearest float exactly, as Rust
/// does.
pub fn parse_float(text: &str) -> Result<f64, String> {
    let trimmed = strip(text);
    let unsigned = trimmed.strip_prefix(['+', '-']).unwrap_or(trimmed);
    let named = ["inf", "infinity", "nan"]
        .iter()
        .any(|name| unsigned.eq_ignore_ascii_case(name));
    if !named && !is_decimal(unsigned) {
        return Err(format!("could not convert string to float: '{text}'"));
    }
    Ok(trimmed.parse().expect("the grammar is a subset of Rust's"))
}

/// Returns whether `text` is digits with an optional point and exponent, such as `1.5e-3`, `.5`,
/// or `3.`, with at least one digit before the exponent.
fn is_decimal(text: &str) -> bool {
    let bytes = text.as_bytes();
    let digits = |i: &mut usize| {
        let start = *i;
        while *i < bytes.len() && bytes[*i].is_ascii_digit() {
            *i += 1;
        }
        *i - start
    };
    let mut i = 0;
    let mut count = digits(&mut i);
    if bytes.get(i) == Some(&b'.') {
        i += 1;
        count += digits(&mut i);
    }
    if count == 0 {
        return false;
    }
    if matches!(bytes.get(i), Some(b'e' | b'E')) {
        i += 1;
        if matches!(bytes.get(i), Some(b'+' | b'-')) {
            i += 1;
        }
        if digits(&mut i) == 0 {
            return false;
        }
    }
    i == bytes.len()
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
    fn every_module_function_is_documented_under_its_module() {
        for module in &MODULES {
            assert!(!module.functions.is_empty(), "{}", module.name);
            for doc in module.functions {
                let prefix = format!("{}.", module.name);
                assert!(doc.name.starts_with(&prefix), "{}", doc.name);
                assert!(doc.signature.starts_with(&format!("{}(", doc.name)));
                assert_eq!(module_function(doc.name).unwrap().name, doc.name);
                assert_eq!(builtin_doc(doc.name).unwrap().name, doc.name);
                assert!(is_builtin(doc.name), "{}", doc.name);
            }
        }
        assert!(module_function("os.nope").is_none());
        assert!(module_function("env").is_none());
        assert!(!is_builtin("env"));
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
    fn ints_parse_after_stripping_whitespace() {
        assert_eq!(parse_int("42"), Ok(42));
        assert_eq!(parse_int("  -7\n"), Ok(-7));
        assert_eq!(parse_int("+0012"), Ok(12));
        assert_eq!(parse_int("\t\x0b\x0c\r 5 "), Ok(5));
        assert_eq!(parse_int("-9223372036854775808"), Ok(i64::MIN));
        assert_eq!(parse_int("9223372036854775807"), Ok(i64::MAX));
        for bad in [
            "", " ", "+", "-", "1.5", "1e3", "0x10", "1_000", "--1", "+-1", "1 2", "\u{a0}1",
        ] {
            assert_eq!(
                parse_int(bad),
                Err(format!("invalid literal for int() with base 10: '{bad}'")),
                "{bad:?}"
            );
        }
        for big in [
            "9223372036854775808",
            "-9223372036854775809",
            "99999999999999999999999",
        ] {
            assert_eq!(
                parse_int(big),
                Err(format!("int() argument out of range: '{big}'")),
                "{big:?}"
            );
        }
    }

    #[test]
    fn floats_parse_a_decimal_grammar() {
        let cases = [
            ("1", 1.0),
            ("  2.5\n", 2.5),
            ("-.5", -0.5),
            ("+3.", 3.0),
            ("1e3", 1000.0),
            ("1E-2", 0.01),
            ("6.02e+23", 6.02e23),
            ("1e999", f64::INFINITY),
            ("1e-999", 0.0),
            ("inf", f64::INFINITY),
            ("-Infinity", f64::NEG_INFINITY),
            ("0.1", 0.1),
        ];
        for (text, expected) in cases {
            assert_eq!(parse_float(text), Ok(expected), "{text:?}");
        }
        assert!(parse_float(" NaN ").unwrap().is_nan());
        for bad in [
            "", ".", "e5", "1e", "1e+", "0x1p3", "in", "infinit", "nan(1)", "1.2.3", "1_0", "- 1",
        ] {
            assert_eq!(
                parse_float(bad),
                Err(format!("could not convert string to float: '{bad}'")),
                "{bad:?}"
            );
        }
    }

    #[test]
    fn strip_removes_ascii_whitespace() {
        assert_eq!(strip("  a b\t\n"), "a b");
        assert_eq!(strip("\u{a0}x\u{a0}"), "\u{a0}x\u{a0}");
        assert_eq!(strip(" \r\n"), "");
    }

    #[test]
    fn split_without_a_separator_drops_empty_pieces() {
        assert_eq!(split_whitespace("  a  b\tc\n"), ["a", "b", "c"]);
        assert!(split_whitespace("   ").is_empty());
    }

    #[test]
    fn split_with_a_separator_keeps_empty_pieces() {
        assert_eq!(split("a,,b,", ","), Ok(vec!["a", "", "b", ""]));
        assert_eq!(split("", ","), Ok(vec![""]));
        assert_eq!(split("a::b", "::"), Ok(vec!["a", "b"]));
        assert_eq!(split("aaa", "aa"), Ok(vec!["", "a"]));
        assert_eq!(split("a", ""), Err("empty separator".to_string()));
    }

    #[test]
    fn the_reference_is_valid_stone() {
        assert_eq!(crate::driver::check(&builtins_reference()), []);
    }

    #[test]
    fn the_reference_has_a_line_for_each_builtin() {
        let reference = builtins_reference();
        let functions = MODULES.iter().flat_map(|module| module.functions);
        for doc in BUILTIN_DOCS.iter().chain(&METHOD_DOCS).chain(functions) {
            let line = format!("// {}", doc.signature);
            assert_eq!(
                reference.lines().filter(|l| *l == line).count(),
                1,
                "{line}"
            );
        }
    }
}
