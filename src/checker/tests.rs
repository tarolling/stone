use super::*;
use crate::driver::parse;
use crate::span::{FileId, Pos, Span};

/// Parses and checks `source`, returning its analysis.
fn analyze_source(source: &str) -> Analysis {
    let module = parse(source).expect("test source should parse");
    TypeChecker::new().analyze(&module)
}

/// Returns each diagnostic of `source` as its message and start position.
///
/// For example, `errors("x = y\n")` returns `[("undefined name 'y'", 1, 5)]`.
fn errors(source: &str) -> Vec<(String, usize, usize)> {
    analyze_source(source)
        .diagnostics
        .into_iter()
        .map(|d| (d.message, d.span.start.line, d.span.start.col))
        .collect()
}

/// Asserts that `source` has exactly one diagnostic, with this message at this position.
fn assert_error(source: &str, message: &str, line: usize, col: usize) {
    assert_eq!(
        errors(source),
        [(message.to_string(), line, col)],
        "{source}"
    );
}

/// Returns the type of the symbol named `name`, which must be unique in `source`.
fn type_of_symbol(source: &str, name: &str) -> String {
    let analysis = analyze_source(source);
    assert_eq!(analysis.diagnostics, [], "{source}");
    let matching: Vec<&Symbol> = analysis.symbols.iter().filter(|s| s.name == name).collect();
    assert_eq!(matching.len(), 1, "expected one symbol named {name}");
    matching[0].ty.to_string()
}

#[test]
fn every_example_and_test_program_checks_cleanly() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    for dir in ["examples", "tests/programs"] {
        for entry in std::fs::read_dir(root.join(dir)).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|ext| ext == "stn") {
                // linked, so a program can use the builtin os module
                let source = std::fs::read_to_string(&path).unwrap();
                assert_eq!(linked_errors(&source), [], "{}", path.display());
            }
        }
    }
}

#[test]
fn literals_and_operators_have_types() {
    let source = "a = 1\nb = \"s\" + \"t\"\nc = a < 2\nd = none\ne = not c\nf = -a * 2\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_eq!(type_of_symbol(source, "c"), "bool");
    assert_eq!(type_of_symbol(source, "d"), "option[int]");
    assert_eq!(type_of_symbol(source, "e"), "bool");
    assert_eq!(type_of_symbol(source, "f"), "int");
}

#[test]
fn function_types_come_from_their_calls_and_returns() {
    let source = "fn add(a: int, b: int) -> int;\n    ret a + b\nx = add(1, 2)\n";
    assert_eq!(type_of_symbol(source, "add"), "fn(int, int) -> int");
    assert_eq!(type_of_symbol(source, "x"), "int");
}

#[test]
fn a_function_without_ret_returns_no_value() {
    let source = "fn greet(name: str);\n    print(\"hi\", name)\ngreet(\"bo\")\n";
    assert_eq!(type_of_symbol(source, "greet"), "fn(str)");
}

#[test]
fn recursive_functions_are_typed() {
    let source =
        "fn fact(n: int) -> int;\n    if n != 0;\n        ret n * fact(n - 1)\n    ret 1\n";
    assert_eq!(type_of_symbol(source, "fact"), "fn(int) -> int");
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let source = "print(twice(2))\nfn twice(n: int) -> int;\n    ret n * 2\n";
    assert_eq!(errors(source), []);
}

#[test]
fn unconstrained_types_default_to_int() {
    let source = "fn ignore(a: int) -> int;\n    ret 0\n";
    assert_eq!(type_of_symbol(source, "ignore"), "fn(int) -> int");
}

#[test]
fn parameters_and_returns_take_their_written_types() {
    let source = "fn scale(xs: list[float], k: float) -> list[float];\n    ret xs * k\n";
    assert_eq!(
        type_of_symbol(source, "scale"),
        "fn(list[float], float) -> list[float]"
    );
}

#[test]
fn a_function_without_a_return_type_returns_no_value() {
    let source = "fn greet(name: str);\n    print(\"hi\", name)\n";
    assert_eq!(type_of_symbol(source, "greet"), "fn(str)");
}

#[test]
fn a_call_that_returns_no_value_is_only_a_statement() {
    let message = "'greet' returns no value, so it can only be called as a statement of its own";
    let greet = "fn greet(name: str);\n    print(\"hi\", name)\n";
    assert_error(&format!("{greet}x = greet(\"bo\")\n"), message, 3, 5);
    assert_error(&format!("{greet}print(greet(\"bo\"))\n"), message, 3, 7);
    assert_error(&format!("{greet}xs = [greet(\"bo\")]\n"), message, 3, 7);
    assert_eq!(errors(&format!("{greet}greet(\"bo\")\n")), []);
    assert_error(
        "x = print(1)\n",
        "'print' returns no value, so it can only be called as a statement of its own",
        1,
        5,
    );
}

#[test]
fn ret_without_a_value_needs_a_function_without_a_return_type() {
    assert_error(
        "fn f() -> int;\n    ret\n",
        "'f' returns int, so 'ret' needs a value",
        2,
        5,
    );
}

#[test]
fn a_parameter_needs_a_type() {
    assert_error(
        "fn twice(n) -> int;\n    ret n * 2\n",
        "parameter 'n' needs a type, such as 'n: int'",
        1,
        10,
    );
}

#[test]
fn a_missing_type_is_suggested_from_the_calls() {
    assert_error(
        "fn greet(name);\n    print(name)\ngreet(\"bo\")\n",
        "parameter 'name' needs a type, such as 'name: str'",
        1,
        10,
    );
}

#[test]
fn returning_a_value_needs_a_return_type() {
    assert_error(
        "fn one();\n    ret 1\n",
        "'one' has no return type, so it cannot return int; add '-> int' after its parameters",
        2,
        9,
    );
}

#[test]
fn a_missing_return_type_is_suggested_once_it_is_known() {
    // `ys` is a list of unknown elements at the first `ret`, and only the append settles them
    let source =
        "fn f(x: float);\n    ys = []\n    if x > 0.0;\n        ret ys\n    ys.append(x)\n";
    let message = "'f' has no return type, so it cannot return list[float]; add '-> list[float]' \
                   after its parameters";
    assert_error(source, message, 4, 13);
}

#[test]
fn arguments_must_match_the_written_types() {
    assert_error(
        "fn half(x: float) -> float;\n    ret x / 2.0\nhalf(3)\n",
        "expected float, found int",
        3,
        6,
    );
}

#[test]
fn the_body_must_match_the_written_types() {
    assert_error(
        "fn f(s: str) -> int;\n    ret s\n",
        "'f' returns int, but this is str",
        2,
        9,
    );
    assert_error(
        "fn f() -> option[int];\n    ret 1\n",
        "'f' returns option[int], but this is int",
        2,
        9,
    );
}

#[test]
fn a_written_list_type_settles_arithmetic_before_any_call() {
    // without the types, `xs * 2` could be on numbers or on a list until a call settles it
    let analysis = analyze_source("fn f(xs: list[int]) -> list[int];\n    ret xs * 2\n");
    assert_eq!(analysis.diagnostics, []);
    let product = Span::new(Pos::new(2, 9), Pos::new(2, 15));
    assert_eq!(
        analysis.types.get(&product).map(Type::to_string).as_deref(),
        Some("list[int]")
    );
}

#[test]
fn expression_types_are_recorded_by_span() {
    let analysis = analyze_source("x = 1\ny = x < 2\n");
    let compare = Span::new(Pos::new(2, 5), Pos::new(2, 10));
    assert_eq!(analysis.types.get(&compare), Some(&Type::Bool));
}

#[test]
fn assigning_a_different_type_is_an_error() {
    assert_error(
        "x = 1\nx = \"a\"\n",
        "cannot assign str to 'x', which is int",
        2,
        5,
    );
}

#[test]
fn arithmetic_needs_numbers() {
    assert_error(
        "x = \"a\" - \"b\"\n",
        "'-' needs int or float operands, found str",
        1,
        5,
    );
    assert_error(
        "x = true * true\n",
        "'*' needs int or float operands, found bool",
        1,
        5,
    );
    assert_error(
        "x = -\"a\"\n",
        "'-' needs an int or float operand, found str",
        1,
        6,
    );
}

#[test]
fn arithmetic_on_lists_works_element_by_element() {
    let source = "a = [1, 2]\nb = a * 2 + a\nc = 2.5 * [1.0]\nd = -a\ne = a ** 2\nf = 2 ** a\n\
                  g = [1.5] ** [2]\nh = a % 3 - +a\n";
    for (name, ty) in [
        ("b", "list[int]"),
        ("c", "list[float]"),
        ("d", "list[int]"),
        ("e", "list[int]"),
        ("f", "list[int]"),
        ("g", "list[float]"),
        ("h", "list[int]"),
    ] {
        assert_eq!(type_of_symbol(source, name), ty, "{name}");
    }
}

#[test]
fn elementwise_operands_are_inferred_from_calls() {
    let source = "fn scale(xs: list[float], k: float) -> list[float];\n    ret xs * k\ny = scale([1.5], 2.0)\n";
    assert_eq!(
        type_of_symbol(source, "scale"),
        "fn(list[float], float) -> list[float]"
    );
    let source =
        "fn scale(k: int, xs: list[int]) -> list[int];\n    ret k * xs\ny = scale(2, [1])\n";
    assert_eq!(type_of_symbol(source, "y"), "list[int]");
    let source = "fn double(x: int) -> int;\n    ret x * 2\na = double(3)\n";
    assert_eq!(type_of_symbol(source, "double"), "fn(int) -> int");
    let source = "fn shift(xs: list[float]) -> list[float];\n    ret xs + 1.0\nb = shift([1.5])\n";
    assert_eq!(type_of_symbol(source, "b"), "list[float]");
    // a result used as a list makes its unknown operand a list
    let source =
        "fn twice(xs: list[int]) -> list[int];\n    ys = xs * 2\n    ys.append(1)\n    ret ys\n";
    assert_eq!(
        type_of_symbol(source, "twice"),
        "fn(list[int]) -> list[int]"
    );
    // with nothing else to go on, both operands have the same type, as for numbers
    let source = "fn add(a: int, b: int) -> int;\n    ret a + b\n";
    assert_eq!(type_of_symbol(source, "add"), "fn(int, int) -> int");
}

#[test]
fn a_wrong_argument_to_arithmetic_is_reported_where_it_is_passed() {
    let twice = "fn twice(n: int) -> int;\n    ret n * 2\n";
    assert_eq!(errors(&format!("{twice}a = twice(3)\n")), []);
    assert_error(
        &format!("{twice}x = twice(\"a\")\n"),
        "expected int, found str",
        3,
        11,
    );
    assert_error(
        &format!("{twice}x = twice([1.5])\n"),
        "expected int, found list[float]",
        3,
        11,
    );
}

/// An unknown operand with a list of unknown elements once recursed forever when forced
/// (fuzz/artifacts/codegen/crash-a68ccb90d3bb1a68416882987cd0fb824f07781e, `[]--d`).
#[test]
fn arithmetic_on_unknown_lists_and_operands_settles() {
    assert_error("[]--d\n", "undefined name 'd'", 1, 5);
    assert_eq!(
        type_of_symbol("fn f(a: int) -> list[int];\n    ret [] - a\n", "f"),
        "fn(int) -> list[int]"
    );
    assert_eq!(
        type_of_symbol("fn f(a: int) -> list[int];\n    ret -a * []\n", "f"),
        "fn(int) -> list[int]"
    );
    // an int times 2 is an int, so it cannot be used as a list of floats
    let source = "fn f(n: int) -> list[float];\n    ys = n * 2\n    ys.append(1.5)\n    ret ys\n";
    assert_eq!(
        errors(source),
        [
            ("expected list[float], found int".to_string(), 3, 5),
            ("'f' returns list[float], but this is int".to_string(), 4, 9)
        ]
    );
}

#[test]
fn elementwise_operands_must_agree() {
    assert_error(
        "x = [1] + [1.5]\n",
        "expected list[int], found list[float]",
        1,
        11,
    );
    assert_error("x = [1] * 2.5\n", "expected int, found float", 1, 11);
    assert_error("x = 2.5 * [1]\n", "expected float, found int", 1, 11);
    assert_error(
        "x = [\"a\"] + [\"b\"]\n",
        "'+' on lists needs int or float elements, found str",
        1,
        5,
    );
    assert_error(
        "x = [[1]] * [[2]]\n",
        "'*' on lists needs int or float elements, found list[int]",
        1,
        5,
    );
    assert_error(
        "x = -[true]\n",
        "'-' on lists needs int or float elements, found bool",
        1,
        6,
    );
    assert_error(
        "x = [1.5] ** 0.5\n",
        "'**' needs an int exponent, found float",
        1,
        14,
    );
    assert_error(
        "x = [1.5] ** [0.5]\n",
        "'**' needs an int exponent, found list[float]",
        1,
        14,
    );
    // lists still cannot be compared
    assert_error(
        "x = [1] < [2]\n",
        "'<' needs int, float, or str operands, found list[int]",
        1,
        5,
    );
}

#[test]
fn repeating_a_one_element_list_is_a_warning() {
    let source = "n = 3\nxs = [0] * n\nys = n * [0]\nzs = [1, 2] * n\nws = [0] * 2.5\n";
    let diagnostics = analyze_source(source).diagnostics;
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == crate::diagnostic::Severity::Warning)
        .map(|d| (d.message.as_str(), d.span.start.line))
        .collect();
    let message = "'*' multiplies each element of a list, so this is a list of one element, not \
                   copies of it as in Python; build a longer list with 'append' in a loop";
    assert_eq!(warnings, [(message, 2), (message, 3)]);
}

#[test]
fn arithmetic_needs_matching_operands() {
    assert_error("x = 1 - 2.5\n", "expected int, found float", 1, 9);
    assert_error("x = 2.5 / 2\n", "expected float, found int", 1, 11);
}

#[test]
fn adding_needs_matching_operands() {
    assert_error("x = \"a\" + 1\n", "expected str, found int", 1, 11);
    assert_error("x = 1 + 1.5\n", "expected int, found float", 1, 9);
}

#[test]
fn adding_needs_ints_floats_or_strs() {
    assert_error(
        "x = true + false\n",
        "'+' needs int, float, or str operands, found bool",
        1,
        5,
    );
}

#[test]
fn modulo_needs_matching_numbers() {
    assert_error(
        "x = \"a\" % \"b\"\n",
        "'%' needs int or float operands, found str",
        1,
        5,
    );
    assert_error("x = 1 % 1.5\n", "expected int, found float", 1, 9);
    assert_eq!(type_of_symbol("x = 7 % 2\n", "x"), "int");
    assert_eq!(type_of_symbol("x = 7.5 % 2.0\n", "x"), "float");
}

#[test]
fn power_takes_a_number_base_and_an_int_exponent() {
    assert_eq!(type_of_symbol("x = 2 ** 10\n", "x"), "int");
    assert_eq!(type_of_symbol("x = 2.5 ** 3\n", "x"), "float");
    assert_eq!(type_of_symbol("x = 2.5 ** -3\n", "x"), "float");
    assert_error(
        "x = 2.0 ** 0.5\n",
        "'**' needs an int exponent, found float",
        1,
        12,
    );
    assert_error(
        "x = 2 ** 0.5\n",
        "'**' needs an int exponent, found float",
        1,
        10,
    );
    assert_error(
        "x = \"a\" ** 2\n",
        "'**' needs an int or float base, found str",
        1,
        5,
    );
}

#[test]
fn a_power_exponent_is_inferred_as_int() {
    let source = "fn f(n: int) -> float;\n    ret 1.5 ** n\nx = f(2)\n";
    assert_eq!(type_of_symbol(source, "x"), "float");
    assert_error(
        "fn f(n: int) -> float;\n    ret 1.5 ** n\nx = f(2.0)\n",
        "expected int, found float",
        3,
        7,
    );
}

#[test]
fn float_arithmetic_gives_floats() {
    let source = "a = 1.5\nb = a + 2.0\nc = -a * b / 0.5 - 1e3\nd = a < b\ne = a == b\n";
    assert_eq!(type_of_symbol(source, "a"), "float");
    assert_eq!(type_of_symbol(source, "b"), "float");
    assert_eq!(type_of_symbol(source, "c"), "float");
    assert_eq!(type_of_symbol(source, "d"), "bool");
    assert_eq!(type_of_symbol(source, "e"), "bool");
}

#[test]
fn functions_take_their_operand_types_from_calls() {
    let source = "fn scale(a: float, b: float) -> float;\n    ret -a * b\nx = scale(1.5, 2.0)\n";
    assert_eq!(type_of_symbol(source, "scale"), "fn(float, float) -> float");
    let source = "fn less(a: float, b: float) -> bool;\n    ret a < b\nx = less(1.5, 2.0)\n";
    assert_eq!(type_of_symbol(source, "less"), "fn(float, float) -> bool");
}

#[test]
fn lists_can_hold_floats() {
    let source = "xs = []\nxs.append(0.5)\ny = xs[0] * 2.0\n";
    assert_eq!(type_of_symbol(source, "xs"), "list[float]");
    assert_eq!(type_of_symbol(source, "y"), "float");
}

#[test]
fn int_and_float_convert_numbers() {
    let source = "a = float(3)\nb = int(2.5)\nc = float(a)\nd = int(b)\n";
    assert_eq!(type_of_symbol(source, "a"), "float");
    assert_eq!(type_of_symbol(source, "b"), "int");
    assert_eq!(type_of_symbol(source, "c"), "float");
    assert_eq!(type_of_symbol(source, "d"), "int");
    assert_error(
        "x = int([3])\n",
        "'int' needs an int, float, or str, found list[int]",
        1,
        9,
    );
    assert_error(
        "x = float(1, 2)\n",
        "'float' takes 1 argument, but 2 were given",
        1,
        5,
    );
}

#[test]
fn floats_are_not_conditions() {
    assert_error(
        "if 1.5;\n    print(1)\n",
        "a condition must be bool, found float",
        1,
        4,
    );
}

#[test]
fn comparisons_need_matching_operands() {
    assert_error("x = 1 == \"a\"\n", "expected int, found str", 1, 10);
    assert_error("x = 1 < 2.5\n", "expected int, found float", 1, 9);
    assert_error(
        "x = true < false\n",
        "'<' needs int, float, or str operands, found bool",
        1,
        5,
    );
    assert_error("x = \"a\" >= 1\n", "expected str, found int", 1, 12);
}

#[test]
fn strings_are_ordered() {
    let source = "a = \"a\" < \"b\"\nb = \"b\" <= \"c\" > \"a\" >= \"\"\n";
    assert_eq!(type_of_symbol(source, "a"), "bool");
    assert_eq!(type_of_symbol(source, "b"), "bool");
    assert_eq!(errors("fn f(s: str, t: str) -> bool;\n    ret s < t\n"), []);
}

#[test]
fn strings_index_and_slice_to_strings() {
    let source = "s = \"hello\"\na = s[0]\nb = s[1:3]\nc = s[::-1][0]\n";
    assert_eq!(type_of_symbol(source, "a"), "str");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_eq!(type_of_symbol(source, "c"), "str");
    let source = "fn first(s: str) -> str;\n    ret s[0]\nwords = [\"ab\"]\nw = words[0][1:]\n";
    assert_eq!(type_of_symbol(source, "w"), "str");
    assert_error("x = \"ab\"[\"a\"]\n", "expected int, found str", 1, 10);
    assert_error("x = \"ab\"[1.5:]\n", "expected int, found float", 1, 10);
}

#[test]
fn strings_cannot_be_changed() {
    let message = "a string cannot be changed; build a new one, as in 's = s[:i] + c + s[i + 1:]'";
    assert_error("s = \"ab\"\ns[0] = \"c\"\n", message, 2, 1);
    assert_error("words = [\"ab\"]\nwords[0][0] = \"c\"\n", message, 2, 1);
    // an element of a list of strings is still replaced as a whole
    assert_eq!(errors("words = [\"ab\"]\nwords[0] = \"c\"\n"), []);
}

#[test]
fn for_loops_walk_the_characters_of_strings() {
    let source = "for c in \"abc\";\n    print(c)\nfn count(s: str) -> int;\n    n = 0\n    for ch in s;\n        n = n + ch.len()\n    ret n\n";
    assert_eq!(type_of_symbol(source, "c"), "str");
    assert_eq!(type_of_symbol(source, "ch"), "str");
    assert_error(
        "for x in 5;\n    print(x)\n",
        "expected list[unknown], found int",
        1,
        10,
    );
}

#[test]
fn join_find_and_startswith_take_strs() {
    let source =
        "s = \", \".join([\"a\", \"b\"])\nn = \"abc\".find(\"c\")\nb = \"abc\".startswith(\"a\")\n";
    assert_eq!(type_of_symbol(source, "s"), "str");
    assert_eq!(type_of_symbol(source, "n"), "int");
    assert_eq!(type_of_symbol(source, "b"), "bool");
    // the element type of an empty list comes from join
    assert_eq!(
        type_of_symbol("xs = []\ns = \"\".join(xs)\n", "xs"),
        "list[str]"
    );
    assert_error(
        "x = \",\".join([1])\n",
        "expected list[str], found list[int]",
        1,
        14,
    );
    assert_error(
        "x = [1].join([\"a\"])\n",
        "'join' needs a str, found list[int]",
        1,
        5,
    );
    assert_error("x = \"a\".find(1)\n", "expected str, found int", 1, 14);
    assert_error(
        "x = \"a\".startswith(1)\n",
        "expected str, found int",
        1,
        20,
    );
    assert_error(
        "x = \"a\".find()\n",
        "'find' takes 1 argument, but 0 were given",
        1,
        5,
    );
    assert_error(
        "x = \"a\".startswith(\"a\", \"b\")\n",
        "'startswith' takes 1 argument, but 2 were given",
        1,
        5,
    );
    assert_error("x = 1.find(\"a\")\n", "'find' needs a str, found int", 1, 5);
}

#[test]
fn endswith_takes_a_str() {
    assert_eq!(
        type_of_symbol("b = \"stone\".endswith(\"one\")\n", "b"),
        "bool"
    );
    assert_error("x = \"a\".endswith(1)\n", "expected str, found int", 1, 18);
    assert_error(
        "x = [1].endswith(\"a\")\n",
        "'endswith' needs a str, found list[int]",
        1,
        5,
    );
    assert_error(
        "x = \"a\".endswith()\n",
        "'endswith' takes 1 argument, but 0 were given",
        1,
        5,
    );
}

#[test]
fn conditions_must_be_bool() {
    assert_error(
        "if \"yes\";\n    print(1)\n",
        "a condition must be bool, found str",
        1,
        4,
    );
    assert_error(
        "x = not \"s\"\n",
        "a condition must be bool, found str",
        1,
        9,
    );
    assert_eq!(
        errors("if 1 or 0;\n    print(1)\n"),
        [
            ("a condition must be bool, found int".to_string(), 1, 4),
            ("a condition must be bool, found int".to_string(), 1, 9),
        ]
    );
    assert_error(
        "x = true or 0\n",
        "a condition must be bool, found int",
        1,
        13,
    );
    assert_error(
        "n = 3\nwhile n;\n    n -= 1\n",
        "a condition must be bool, found int",
        2,
        7,
    );
    assert_error("x = not 0\n", "a condition must be bool, found int", 1, 9);
}

#[test]
fn undefined_names_are_errors() {
    assert_error("x = y + 1\n", "undefined name 'y'", 1, 5);
    assert_error("nope(1)\n", "undefined function 'nope'", 1, 1);
}

#[test]
fn calls_need_the_right_number_of_arguments() {
    assert_error(
        "fn f(a: int, b: int) -> int;\n    ret a\nf(1)\n",
        "'f' takes 2 arguments, but 1 was given",
        3,
        1,
    );
    assert_error(
        "x = \"a\".len(\"b\")\n",
        "'len' takes 0 arguments, but 1 was given",
        1,
        5,
    );
}

#[test]
fn calls_must_agree_on_argument_types() {
    assert_error(
        "fn f(a: int) -> int;\n    ret a\nf(1)\nf(\"s\")\n",
        "expected int, found str",
        4,
        3,
    );
}

#[test]
fn returns_must_agree() {
    assert_error(
        "fn f(x: int) -> int;\n    if x > 0;\n        ret 1\n    ret \"a\"\n",
        "'f' returns int, but this is str",
        4,
        9,
    );
}

#[test]
fn a_value_returning_function_must_return_on_every_path() {
    assert_error(
        "fn f(x: int) -> int;\n    if x > 0;\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        4,
    );
}

#[test]
fn an_endless_loop_counts_as_returning() {
    let source = "fn f(k: int) -> int;\n    m = 1\n    while true;\n        if m * k == 12;\n            ret m\n        m = m + 1\nprint(f(4))\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_loop_that_can_break_does_not_count_as_returning() {
    assert_error(
        "fn f() -> int;\n    while true;\n        break\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        4,
    );
}

#[test]
fn break_and_cont_need_a_loop() {
    assert_error("break\n", "'break' outside a loop", 1, 1);
    assert_error("fn f();\n    cont\n", "'cont' outside a loop", 2, 5);
}

#[test]
fn functions_must_be_top_level() {
    assert_error(
        "fn outer() -> int;\n    fn inner();\n        ret 1\n    ret 2\n",
        "functions can only be defined at the top level",
        2,
        8,
    );
}

#[test]
fn functions_are_not_values() {
    assert_error(
        "fn f() -> int;\n    ret 1\nx = f\n",
        "'f' is a function, so it can only be called",
        3,
        5,
    );
    assert_error(
        "x = print\n",
        "'print' is a function, so it can only be called",
        1,
        5,
    );
}

#[test]
fn functions_and_builtins_cannot_be_assigned() {
    assert_error(
        "fn f() -> int;\n    ret 1\nf = 2\n",
        "cannot assign to function 'f'",
        3,
        1,
    );
    assert_error("print = 2\n", "cannot assign to builtin 'print'", 1, 1);
}

#[test]
fn only_functions_can_be_called() {
    assert_error("x = 1\nx(2)\n", "'x' is not a function", 2, 1);
}

#[test]
fn functions_cannot_be_redefined() {
    assert_error(
        "fn f() -> int;\n    ret 1\nfn f();\n    ret 2\n",
        "function 'f' is already defined",
        3,
        4,
    );
}

#[test]
fn parameters_must_be_unique() {
    assert_error(
        "fn f(a: int, a: int) -> int;\n    ret a\n",
        "duplicate parameter 'a'",
        1,
        14,
    );
}

#[test]
fn len_needs_a_str() {
    assert_error(
        "x = 5.len()\n",
        "'len' needs a str or list, found int",
        1,
        5,
    );
}

#[test]
fn locals_shadow_globals_and_keep_their_own_type() {
    let source = "x = 1\nfn f() -> str;\n    x = \"local\"\n    ret x\ny = f()\n";
    assert_eq!(errors(source), []);
    assert_eq!(type_of_symbol(source, "y"), "str");
}

#[test]
fn functions_read_globals() {
    let source = "fn f() -> int;\n    ret n + 1\nn = 3\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_local_assigned_anywhere_in_the_function_is_local_everywhere() {
    // like Python, `x` is local to `f` because `f` assigns it, so the global is never read, and
    // reading the local before assigning it is an error rather than a read of the global
    let source = "x = \"global\"\nfn f() -> int;\n    y = x\n    x = 1\n    ret y\nprint(f())\n";
    assert_error(source, "'x' might be used before it is assigned", 3, 9);
    let analysis = analyze_source(source);
    let y = analysis.symbols.iter().find(|s| s.name == "y").unwrap();
    assert_eq!(y.ty, Type::Int);
}

#[test]
fn every_use_of_a_symbol_is_a_reference() {
    let analysis = analyze_source("fn f(a: int) -> int;\n    b = a\n    ret a + b\nf(1)\n");
    let param = analysis.symbols.iter().position(|s| s.name == "a").unwrap();
    let uses: Vec<Pos> = analysis
        .references
        .iter()
        .filter(|r| r.symbol == param)
        .map(|r| r.span.start)
        .collect();
    assert_eq!(uses, [Pos::new(1, 6), Pos::new(2, 9), Pos::new(3, 9)]);
    assert_eq!(analysis.symbols[param].kind, SymbolKind::Parameter);
    assert_eq!(analysis.symbols[param].scope.as_deref(), Some("f"));
}

#[test]
fn diagnostics_are_sorted_by_position() {
    let positions: Vec<(usize, usize)> = errors("x = a\ny = b\nz = c\n")
        .into_iter()
        .map(|(_, line, col)| (line, col))
        .collect();
    assert_eq!(positions, [(1, 5), (2, 5), (3, 5)]);
}

#[test]
fn for_loops_over_ranges_give_ints() {
    let source = "for i in range(3);\n    x = i\nfor j in range(1, 4);\n    y = j\n";
    assert_eq!(type_of_symbol(source, "x"), "int");
    assert_eq!(type_of_symbol(source, "y"), "int");
}

#[test]
fn range_is_only_for_loops() {
    assert_error(
        "x = range(3)\n",
        "'range' can only be used as the iterable of a 'for' loop",
        1,
        5,
    );
}

#[test]
fn range_takes_one_or_two_ints() {
    assert_error(
        "for i in range();\n    print(i)\n",
        "'range' takes 1 or 2 arguments, but 0 were given",
        1,
        10,
    );
    assert_error(
        "for i in range(\"a\");\n    print(i)\n",
        "expected int, found str",
        1,
        16,
    );
}

#[test]
fn for_loop_variables_must_be_names() {
    let messages: Vec<String> = errors("a = 0\nfor a[0] in range(3);\n    print(1)\n")
        .into_iter()
        .map(|(message, _, _)| message)
        .collect();
    assert!(
        messages.contains(
            &"a 'for' loop's variable must be a name, or names to unpack each element into, as \
              in 'for a, b in pairs'"
                .to_string()
        ),
        "{messages:?}"
    );
}

#[test]
fn break_and_cont_work_in_for_loops() {
    let source =
        "for i in range(9);\n    if i == 2;\n        cont\n    if i == 5;\n        break\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_for_loop_does_not_count_as_returning() {
    assert_error(
        "fn f() -> int;\n    for i in range(3);\n        ret i\n",
        "'f' does not return a value on every path",
        1,
        4,
    );
}

#[test]
fn lists_have_element_types() {
    let source = "a = [1, 2]\nb = [[\"x\"], []]\nc = a[0]\nd = []\nd.append(true)\n";
    assert_eq!(type_of_symbol(source, "a"), "list[int]");
    assert_eq!(type_of_symbol(source, "b"), "list[list[str]]");
    assert_eq!(type_of_symbol(source, "c"), "int");
    assert_eq!(type_of_symbol(source, "d"), "list[bool]");
}

#[test]
fn an_empty_list_that_is_never_filled_holds_ints() {
    assert_eq!(type_of_symbol("a = []\n", "a"), "list[int]");
}

#[test]
fn list_elements_must_agree() {
    assert_error("a = [1, \"b\"]\n", "expected int, found str", 1, 9);
    assert_error(
        "a = [1]\na.append(\"b\")\n",
        "expected int, found str",
        2,
        10,
    );
    assert_error("a = [1]\na[0] = true\n", "expected int, found bool", 2, 8);
}

#[test]
fn indexes_must_be_ints_and_values_lists_or_strs() {
    assert_error("a = [1]\nx = a[\"0\"]\n", "expected int, found str", 2, 7);
    assert_error(
        "n = 5\nx = n[0]\n",
        "expected list[unknown], found int",
        2,
        5,
    );
}

#[test]
fn slices_are_lists_of_the_same_type() {
    let source = "a = [1.5, 2.5]\nb = a[1:]\nc = a[::-1][0]\ni = 1\nd = [[\"x\"]][i:i + 1:1]\n";
    assert_eq!(type_of_symbol(source, "b"), "list[float]");
    assert_eq!(type_of_symbol(source, "c"), "float");
    assert_eq!(type_of_symbol(source, "d"), "list[list[str]]");
}

#[test]
fn slice_bounds_must_be_ints() {
    assert_error("a = [1]\nb = a[0.5:]\n", "expected int, found float", 2, 7);
    assert_error("a = [1]\nb = a[:\"x\"]\n", "expected int, found str", 2, 8);
    assert_error("a = [1]\nb = a[::true]\n", "expected int, found bool", 2, 9);
    assert_error(
        "x = 1\nb = x[1:]\n",
        "expected list[unknown], found int",
        2,
        5,
    );
}

#[test]
fn slices_cannot_be_changed() {
    let message = "a slice is a new list, so changing it has no effect";
    assert_error("a = [1, 2]\na[1:] = [3]\n", message, 2, 1);
    assert_error("a = [1, 2]\na[1:][0] = 3\n", message, 2, 1);
    assert_error("a = [1, 2]\na[1:].append(3)\n", message, 2, 1);
    assert_error("a = [[1], [2]]\na[:1][0].append(3)\n", message, 2, 1);
}

#[test]
fn len_takes_lists() {
    assert_eq!(type_of_symbol("n = [1, 2].len()\n", "n"), "int");
}

#[test]
fn for_loops_iterate_over_lists() {
    let source = "for word in [\"a\", \"b\"];\n    w = word\n";
    assert_eq!(type_of_symbol(source, "w"), "str");
    assert_error(
        "for x in 5;\n    print(x)\n",
        "expected list[unknown], found int",
        1,
        10,
    );
}

#[test]
fn lists_cannot_be_compared() {
    assert_error(
        "x = [1] == [1]\n",
        "'==' and '!=' cannot compare list[int]",
        1,
        5,
    );
}

#[test]
fn append_needs_a_list() {
    assert_error(
        "x = 1\nx.append(2)\n",
        "expected list[int], found int",
        2,
        1,
    );
    assert_error(
        "a = []\na.append()\n",
        "'append' takes 1 argument, but 0 were given",
        2,
        1,
    );
}

const UNASSIGNED: &str = "'x' might be used before it is assigned";

#[test]
fn reading_before_assigning_is_an_error() {
    assert_error("print(x)\nx = 1\n", UNASSIGNED, 1, 7);
    assert_error("fn f();\n    print(x)\n    x = 1\n", UNASSIGNED, 2, 11);
}

#[test]
fn assignments_in_one_branch_are_not_enough() {
    assert_error("c = true\nif c;\n    x = 1\nprint(x)\n", UNASSIGNED, 4, 7);
    assert_error(
        "fn f(c: bool) -> int;\n    if c;\n        x = 1\n    ret x\n",
        UNASSIGNED,
        4,
        9,
    );
}

#[test]
fn assignments_in_every_branch_are_enough() {
    let source =
        "c = 1\nif c == 1;\n    x = 1\nelif c == 2;\n    x = 2\nelse;\n    x = 3\nprint(x)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn branches_that_leave_do_not_count() {
    let source =
        "fn f(c: bool) -> int;\n    if c;\n        x = 1\n    else;\n        ret 0\n    ret x\n";
    assert_eq!(errors(source), []);
    let source =
        "for i in range(3);\n    if i > 0;\n        x = i\n    else;\n        cont\n    print(x)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn loop_bodies_might_not_run() {
    assert_error(
        "c = 0\nwhile c < 1;\n    x = 1\n    c = c + 1\nprint(x)\n",
        UNASSIGNED,
        5,
        7,
    );
    assert_error(
        "for x in range(3);\n    y = x\nprint(x)\n",
        UNASSIGNED,
        3,
        7,
    );
}

#[test]
fn a_loop_body_cannot_read_what_it_assigns_later() {
    assert_error(
        "c = 0\nwhile c < 2;\n    print(x)\n    x = c\n    c = c + 1\n",
        UNASSIGNED,
        3,
        11,
    );
}

#[test]
fn functions_read_globals_whenever_they_are_called() {
    // whether `total` is assigned depends on when `f` runs, which the interpreter checks
    let source = "fn f() -> int;\n    ret total\ntotal = 1\nprint(f())\n";
    assert_eq!(errors(source), []);
}

#[test]
fn each_unassigned_variable_is_reported_once() {
    assert_error("print(x, x)\nx = 1\n", UNASSIGNED, 1, 7);
}

#[test]
fn references_are_found_by_position() {
    let analysis = analyze_source("fn add(a: int, b: int) -> int;\n    ret a + b\nx = add(1, 2)\n");
    // inside the `add` of the call
    let reference = analysis.reference_at(FileId(0), Pos::new(3, 6)).unwrap();
    let symbol = &analysis.symbols[reference.symbol];
    assert_eq!(
        (symbol.name.as_str(), symbol.kind),
        ("add", SymbolKind::Function)
    );
    // the end of a name still counts, since editors put the cursor after the last character
    assert_eq!(
        analysis
            .reference_at(FileId(0), Pos::new(3, 8))
            .map(|r| r.symbol),
        Some(reference.symbol)
    );
    assert_eq!(analysis.reference_at(FileId(0), Pos::new(3, 10)), None);

    let uses: Vec<Pos> = analysis
        .references_to(reference.symbol)
        .map(|r| r.span.start)
        .collect();
    assert_eq!(uses, [Pos::new(1, 4), Pos::new(3, 5)]);
}

#[test]
fn function_spans_cover_their_bodies() {
    let analysis = analyze_source("fn f(a: int) -> int;\n    b = a\n    ret b\nx = 1\n");
    assert_eq!(
        analysis.functions.get("f"),
        Some(&Span::new(Pos::new(1, 1), Pos::new(3, 10)))
    );
}

#[test]
fn visible_symbols_depend_on_position() {
    let source = "g = 1\nfn f(a: int) -> int;\n    b = a\n    ret b\nfn h() -> int;\n    ret 2\n";
    let analysis = analyze_source(source);
    let names_at = |pos: Pos| -> Vec<String> {
        let mut names: Vec<String> = analysis
            .visible_at(FileId(0), pos)
            .map(|s| s.name.clone())
            .collect();
        names.sort();
        names
    };
    assert_eq!(names_at(Pos::new(3, 5)), ["a", "b", "f", "g", "h"]);
    assert_eq!(names_at(Pos::new(6, 5)), ["f", "g", "h"]);
    assert_eq!(names_at(Pos::new(1, 1)), ["f", "g", "h"]);
}

#[test]
fn len_takes_strings() {
    assert_eq!(type_of_symbol("s = \"abc\"\nn = s.len()\n", "n"), "int");
}

#[test]
fn methods_chain_after_subscripts_and_calls() {
    let source =
        "fn f() -> list[str];\n    ret [\"a\"]\ngrid = [[1]]\ngrid[0].append(2)\nn = f().len()\n";
    assert_eq!(type_of_symbol(source, "n"), "int");
}

#[test]
fn append_infers_a_parameter_is_a_list() {
    let source = "fn push(xs: list[int], x: int) -> list[int];\n    xs.append(x + 1)\n    ret xs\npush([], 2)\n";
    assert_eq!(
        type_of_symbol(source, "push"),
        "fn(list[int], int) -> list[int]"
    );
}

#[test]
fn unknown_methods_are_errors() {
    assert_error("xs = []\nxs.push(1)\n", "there is no method 'push'", 2, 4);
}

#[test]
fn methods_must_be_called() {
    assert_error(
        "xs = []\nn = xs.len\n",
        "'len' is a method, so it can only be called",
        2,
        5,
    );
}

#[test]
fn len_and_append_are_no_longer_functions() {
    assert_error(
        "x = len(\"a\")\n",
        "'len' is a method, so call it as value.len()",
        1,
        5,
    );
    assert_error(
        "xs = []\nappend(xs, 1)\n",
        "'append' is a method, so call it as value.append(...)",
        2,
        1,
    );
}

#[test]
fn len_and_append_are_ordinary_names() {
    assert_eq!(type_of_symbol("len = 3\n", "len"), "int");
    assert_eq!(
        type_of_symbol("fn append(a: int) -> int;\n    ret a\n", "append"),
        "fn(int) -> int"
    );
}

#[test]
fn method_receivers_must_be_assigned_first() {
    assert_error(
        "xs.append(1)\nxs = []\n",
        "'xs' might be used before it is assigned",
        1,
        1,
    );
}

#[test]
fn pop_insert_and_extend_have_types() {
    let source = "xs = [1.5]\nx = xs.pop()\ny = xs.pop(0)\nxs.insert(0, 2.5)\nxs.extend([x, y])\n\
                  xs.pop()\n";
    assert_eq!(type_of_symbol(source, "x"), "float");
    assert_eq!(type_of_symbol(source, "y"), "float");
    assert_eq!(type_of_symbol(source, "xs"), "list[float]");
    // the list's type can come from what is put in it
    let source = "xs = []\nxs.insert(0, \"a\")\nys = []\nys.extend(xs)\nz = ys.pop()\n";
    assert_eq!(type_of_symbol(source, "z"), "str");
}

#[test]
fn pop_insert_and_extend_check_their_arguments() {
    assert_error(
        "xs = [1]\nxs.pop(1, 2)\n",
        "'pop' takes 0 or 1 arguments, but 2 were given",
        2,
        1,
    );
    assert_error("xs = [1]\nxs.pop(\"0\")\n", "expected int, found str", 2, 8);
    assert_error(
        "xs = [1]\nxs.insert(1)\n",
        "'insert' takes 2 arguments, but 1 was given",
        2,
        1,
    );
    assert_error(
        "xs = [1]\nxs.insert(0, \"a\")\n",
        "expected int, found str",
        2,
        14,
    );
    assert_error(
        "xs = [1]\nxs.insert(0.5, 1)\n",
        "expected int, found float",
        2,
        11,
    );
    assert_error(
        "xs = [1]\nxs.extend(2)\n",
        "expected list[int], found int",
        2,
        11,
    );
    assert_error(
        "xs = [1]\nxs.extend([\"a\"])\n",
        "expected list[int], found list[str]",
        2,
        11,
    );
    assert_error(
        "x = 1\nx.pop()\n",
        "expected list[unknown], found int",
        2,
        1,
    );
}

#[test]
fn pop_is_a_statement_or_an_assigned_value() {
    let message = "'pop' changes a list, so it can only be a statement of its own or the value of \
                   an assignment, as in 'x = xs.pop()'";
    assert_error("xs = [1]\nprint(xs.pop())\n", message, 2, 7);
    assert_error("xs = [1]\ny = xs.pop() + 1\n", message, 2, 5);
    assert_error(
        "fn f(xs: list[int]) -> int;\n    ret xs.pop()\nf([1])\n",
        message,
        2,
        9,
    );
    assert_eq!(
        errors("xs = [1, 2]\ny = xs.pop()\nxs.pop()\ngrid = [[1]]\nrow = grid.pop()\n"),
        []
    );
    let message =
        "'insert' changes a list and returns none, so it can only be a statement of its own";
    assert_error("xs = [1]\ny = xs.insert(0, 1)\n", message, 2, 5);
}

#[test]
fn popping_counts_as_a_change() {
    // a function cannot pop from a global, and popping a parameter it never reads again is lost
    assert_error(
        "xs = [1]\nfn f() -> int;\n    x = xs.pop()\n    ret x\nf()\n",
        "a function cannot change the global 'xs'; pass it as an argument and return the \
         changed value",
        3,
        9,
    );
    let source = "fn f(xs: list[int]) -> int;\n    x = xs.pop()\n    ret x\nf([1])\n";
    assert_eq!(
        errors(source),
        [(
            "changes to 'xs' are lost when 'f' returns, since parameters are copies; return it \
             and assign the result where 'f' is called"
                .into(),
            2,
            9
        )]
    );
    assert_eq!(
        errors("fn f(xs: list[int]) -> list[int];\n    x = xs.pop()\n    ret xs\nf([1])\n"),
        []
    );
}

#[test]
fn append_can_only_be_a_statement() {
    let message =
        "'append' changes a list and returns none, so it can only be a statement of its own";
    assert_error("xs = [1]\nys = xs.append(2)\n", message, 2, 6);
    // a borrowed `xs` would otherwise see the change partway through the call
    assert_error("xs = [none]\nprint(xs, xs.append(none))\n", message, 2, 11);
    assert_eq!(errors("xs = [1]\nxs.append(2)\n"), []);
}

#[test]
fn only_a_list_in_a_variable_can_be_changed() {
    let message = "this list is not stored in a variable, so changing it has no effect";
    assert_error(
        "fn f() -> list[int];\n    ret [1]\nf().append(2)\n",
        message,
        3,
        1,
    );
    assert_error(
        "fn f() -> list[list[int]];\n    ret [[1]]\nf()[0][0] = 2\n",
        message,
        3,
        1,
    );
    assert_error("[1].append(2)\n", message, 1, 1);
    assert_eq!(
        errors("grid = [[1]]\ngrid[0][0] = 2\ngrid[0].append(3)\n"),
        []
    );
}

#[test]
fn functions_cannot_change_globals() {
    let source = "names = []\nfn add(name: str);\n    names.append(name)\nadd(\"a\")\n";
    assert_error(
        source,
        "a function cannot change the global 'names'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    let source = "grid = [[0]]\nfn clear(i: int);\n    grid[i][0] = 0\nclear(0)\n";
    assert_error(
        source,
        "a function cannot change the global 'grid'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    // reading a global is fine, and so is changing a local that shadows one
    let source = "names = [1]\nfn f() -> list[int];\n    names = [2]\n    names.append(3)\n    ret names\n\
                  fn g() -> int;\n    ret names[0]\nprint(f(), g())\n";
    assert_eq!(errors(source), []);
}

#[test]
fn augmented_assignments_follow_their_operators() {
    let source = "a = 1\na += 2\nb = 1.5\nb **= 2\nc = \"x\"\nc += \"y\"\nd = [1, 2]\nd *= 3\n\
                  e = [1.0]\ne -= [2.0]\nf = [[1], [2]]\nf[0] += 1\nf[1][0] %= 2\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "float");
    assert_eq!(type_of_symbol(source, "c"), "str");
    assert_eq!(type_of_symbol(source, "d"), "list[int]");
    assert_eq!(type_of_symbol(source, "e"), "list[float]");
    assert_eq!(type_of_symbol(source, "f"), "list[list[int]]");
    // an empty list learns its element type from the operation
    assert_eq!(type_of_symbol("g = []\ng += 1.0\n", "g"), "list[float]");
}

#[test]
fn augmented_assignments_report_what_the_long_form_does() {
    assert_error("x = 1\nx += 1.0\n", "expected int, found float", 2, 6);
    assert_error(
        "s = \"a\"\ns -= \"b\"\n",
        "'-' needs int or float operands, found str",
        2,
        1,
    );
    assert_error(
        "x = 2\nx **= 0.5\n",
        "'**' needs an int exponent, found float",
        2,
        7,
    );
    assert_error(
        "xs = [1]\nxs[0] += \"a\"\n",
        "expected int, found str",
        2,
        10,
    );
    assert_error(
        "xs = [1]\nxs += [1.0]\n",
        "expected list[int], found list[float]",
        2,
        7,
    );
    assert_error("x += 1\n", "'x' might be used before it is assigned", 1, 1);
    // a function's augmented name is its own local, as in Python
    assert_error(
        "x = 1\nfn f();\n    x += 1\nf()\n",
        "'x' might be used before it is assigned",
        3,
        5,
    );
    assert_error(
        "fn g();\n    ret\ng += 1\n",
        "cannot assign to function 'g'",
        3,
        1,
    );
    assert_error("print += 1\n", "cannot assign to builtin 'print'", 1, 1);
    assert_error(
        "s = \"ab\"\ns[0] += \"c\"\n",
        "a string cannot be changed; build a new one, as in 's = s[:i] + c + s[i + 1:]'",
        2,
        1,
    );
    assert_error(
        "xs = [1, 2]\nxs[0:1] += 1\n",
        "a slice is a new list, so changing it has no effect",
        2,
        1,
    );
    assert_error(
        "xs = [1]\nfn f();\n    xs[0] += 1\nf()\n",
        "a function cannot change the global 'xs'; pass it as an argument and return the changed \
         value",
        3,
        5,
    );
}

#[test]
fn an_augmented_name_is_one_reference() {
    let analysis = analyze_source("total = 0\ntotal += 2\nprint(total)\n");
    assert_eq!(analysis.diagnostics, []);
    let spans: Vec<Pos> = analysis
        .references
        .iter()
        .map(|reference| reference.span.start)
        .collect();
    assert_eq!(spans, [Pos::new(1, 1), Pos::new(2, 1), Pos::new(3, 7)]);
}

#[test]
fn changing_a_parameter_by_augmented_assignment_can_be_lost() {
    let diagnostics =
        analyze_source("fn bump(xs: list[int]);\n    xs[0] += 1\nbump([1])\n").diagnostics;
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].severity,
        crate::diagnostic::Severity::Warning
    );
    assert_eq!(diagnostics[0].span.start, Pos::new(2, 5));
    // rebinding a parameter is no change to the caller's list
    assert_eq!(
        errors(
            "fn twice(xs: list[int]) -> list[int];\n    xs *= 2\n    ret xs\nprint(twice([1]))\n"
        ),
        []
    );
}

#[test]
fn changing_a_parameter_that_is_never_used_again_is_a_warning() {
    let source = "fn push(xs: list[int], x: int);\n    xs.append(x)\nys = [1]\npush(ys, 2)\n";
    let diagnostics = analyze_source(source).diagnostics;
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].severity,
        crate::diagnostic::Severity::Warning
    );
    assert_eq!(
        diagnostics[0].message,
        "changes to 'xs' are lost when 'push' returns, since parameters are copies; return it \
         and assign the result where 'push' is called"
    );
    assert_eq!(diagnostics[0].span.start, Pos::new(2, 5));
    let source = "fn clear(grid: list[list[int]], i: int);\n    if i > 0;\n        grid[i][0] = 0\nclear([[1]], 0)\n";
    assert_eq!(
        errors(source),
        [(
            "changes to 'grid' are lost when 'clear' returns, since parameters are copies; \
             return it and assign the result where 'clear' is called"
                .into(),
            3,
            9
        )]
    );
    // reading the parameter afterward, or returning it, uses the change
    for (returns, body) in [
        (" -> list[int]", "    xs.append(x)\n    ret xs\n"),
        ("", "    xs[0] = x\n    print(xs)\n"),
        (
            " -> int",
            "    for i in range(3);\n        xs.append(x)\n    ret xs.len()\n",
        ),
    ] {
        let source = format!("fn push(xs: list[int], x: int){returns};\n{body}push([1], 2)\n");
        assert_eq!(errors(&source), [], "{source}");
    }
}

#[test]
fn int_and_float_parse_strs() {
    let source = "a = int(\"3\")\nb = float(\"2.5\")\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "float");
    assert_error(
        "x = float(true)\n",
        "'float' needs an int, float, or str, found bool",
        1,
        11,
    );
}

#[test]
fn str_converts_scalars() {
    let source = "a = str(1)\nb = str(1.5)\nc = str(true)\nd = str(\"x\")\n";
    for name in ["a", "b", "c", "d"] {
        assert_eq!(type_of_symbol(source, name), "str");
    }
    assert_error(
        "x = str([1])\n",
        "'str' needs an int, float, bool, or str, found list[int]",
        1,
        9,
    );
    assert_error(
        "x = str()\n",
        "'str' takes 1 argument, but 0 were given",
        1,
        5,
    );
}

#[test]
fn input_reads_strs() {
    assert_eq!(type_of_symbol("a = input()\n", "a"), "str");
    assert_eq!(type_of_symbol("a = input(\"? \")\n", "a"), "str");
    assert_error("x = input(1)\n", "expected str, found int", 1, 11);
    assert_error(
        "x = input(\"a\", \"b\")\n",
        "'input' takes 0 or 1 arguments, but 2 were given",
        1,
        5,
    );
}

#[test]
fn eof_and_args_take_nothing() {
    assert_eq!(type_of_symbol("a = eof()\n", "a"), "bool");
    assert_eq!(type_of_symbol("a = args()\n", "a"), "list[str]");
    assert_error(
        "x = eof(1)\n",
        "'eof' takes 0 arguments, but 1 was given",
        1,
        5,
    );
    assert_error(
        "x = args(1)\n",
        "'args' takes 0 arguments, but 1 was given",
        1,
        5,
    );
}

#[test]
fn eof_is_a_condition() {
    assert_eq!(errors("while not eof();\n    print(input())\n"), []);
}

#[test]
fn strip_and_split_take_strs() {
    let source = "s = \" a b \"\na = s.strip()\nb = s.split()\nc = s.split(\",\")\n";
    assert_eq!(type_of_symbol(source, "a"), "str");
    assert_eq!(type_of_symbol(source, "b"), "list[str]");
    assert_eq!(type_of_symbol(source, "c"), "list[str]");
    assert_error(
        "x = [1].strip()\n",
        "'strip' needs a str, found list[int]",
        1,
        5,
    );
    assert_error(
        "x = \"a\".strip(1)\n",
        "'strip' takes 0 arguments, but 1 was given",
        1,
        5,
    );
    assert_error("x = \"a\".split(1)\n", "expected str, found int", 1, 15);
    assert_error("x = 5.split()\n", "'split' needs a str, found int", 1, 5);
    assert_error(
        "x = \"a\".split(\"b\", \"c\")\n",
        "'split' takes 0 or 1 arguments, but 2 were given",
        1,
        5,
    );
}

#[test]
fn new_builtins_cannot_be_assigned() {
    assert_error("args = 2\n", "cannot assign to builtin 'args'", 1, 1);
}

#[test]
fn use_inside_a_block_is_an_error() {
    assert_error(
        "fn f() -> int;\n    use util\n    ret 1\n",
        "use is only allowed at the top level of a file",
        2,
        5,
    );
}

/// Returns each diagnostic of `source`, linked first so that `use os` resolves, as its message
/// and start position.
fn linked_errors(source: &str) -> Vec<(String, usize, usize)> {
    crate::driver::analyze(source)
        .diagnostics
        .into_iter()
        .map(|d| (d.message, d.span.start.line, d.span.start.col))
        .collect()
}

/// Returns the type of the global `name` in `source`, linked first.
fn linked_type(source: &str, name: &str) -> String {
    let analysis = crate::driver::analyze(source);
    assert_eq!(analysis.diagnostics, [], "{source}");
    let symbol = analysis.symbols.iter().find(|s| s.name == name).unwrap();
    symbol.ty.to_string()
}

#[test]
fn os_functions_have_types() {
    let cases = [
        ("os.env(\"HOME\")", "str"),
        ("os.has_env(\"HOME\")", "bool"),
        ("os.platform()", "str"),
        ("os.arch()", "str"),
        ("os.hostname()", "str"),
        ("os.cpu_count()", "int"),
        ("os.pid()", "int"),
        ("os.cwd()", "str"),
    ];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use os\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
}

#[test]
fn os_functions_check_their_arguments() {
    assert_eq!(
        linked_errors("use os\nos.env()\n"),
        [(
            "'os.env' takes 1 argument, but 0 were given".to_string(),
            2,
            1
        )]
    );
    assert_eq!(
        linked_errors("use os\nos.has_env(1)\n"),
        [("expected str, found int".to_string(), 2, 12)]
    );
    assert_eq!(
        linked_errors("use os\nos.exit(\"no\")\n"),
        [("expected int, found str".to_string(), 2, 9)]
    );
    assert_eq!(
        linked_errors("use os\nos.pid(1)\n"),
        [(
            "'os.pid' takes 0 arguments, but 1 was given".to_string(),
            2,
            1
        )]
    );
}

#[test]
fn an_os_function_is_not_a_value() {
    assert_eq!(
        linked_errors("use os.pid\nx = pid\n"),
        [(
            "'os.pid' is a function, so it can only be called".to_string(),
            2,
            5
        )]
    );
}

#[test]
fn math_functions_have_types() {
    let cases = [
        ("math.abs(-1)", "int"),
        ("math.abs(-1.5)", "float"),
        ("math.min(1, 2)", "int"),
        ("math.max(1.5, 2.5, 0.5)", "float"),
        ("math.max([1, 2])", "int"),
        ("math.min([1.5])", "float"),
        ("math.sqrt(2)", "float"),
        ("math.sqrt(2.0)", "float"),
        ("math.floor(2.5)", "int"),
        ("math.floor(2)", "int"),
    ];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use math\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
}

#[test]
fn math_functions_infer_their_parameters() {
    let source =
        "use math\n\nfn f(a: int, b: int) -> int;\n    ret math.max(a, b) + 1\n\nx = f(1, 2)\n";
    assert_eq!(linked_type(source, "f"), "fn(int, int) -> int");
    let source =
        "use math\n\nfn g(xs: list[float]) -> float;\n    ret math.min(xs) * 2.0\n\nx = g([1.0])\n";
    assert_eq!(linked_type(source, "g"), "fn(list[float]) -> float");
}

#[test]
fn math_functions_check_their_arguments() {
    let cases = [
        (
            "math.abs(\"a\")",
            "'math.abs' needs an int or float, found str",
            10,
        ),
        (
            "math.sqrt(true)",
            "'math.sqrt' needs an int or float, found bool",
            11,
        ),
        (
            "math.floor([1])",
            "'math.floor' needs an int or float, found list[int]",
            12,
        ),
        ("math.max(1, 2.0)", "expected int, found float", 13),
        ("math.min(1.0, 2.0, 3)", "expected float, found int", 20),
        (
            "math.max(\"a\", \"b\")",
            "'math.max' needs ints or floats, found str",
            10,
        ),
        (
            "math.max(3)",
            "one argument must be a list of numbers, found int",
            10,
        ),
        (
            "math.min([\"a\"])",
            "the list must hold int or float, found list[str]",
            10,
        ),
        (
            "math.max()",
            "'math.max' takes 1 or more arguments, but 0 were given",
            1,
        ),
        (
            "math.abs(1, 2)",
            "'math.abs' takes 1 argument, but 2 were given",
            1,
        ),
    ];
    for (call, message, col) in cases {
        assert_eq!(
            linked_errors(&format!("use math\n{call}\n")),
            [(message.to_string(), 2, col)],
            "{call}"
        );
    }
}

#[test]
fn time_functions_have_types() {
    let cases = [("time.now()", "float"), ("time.clock()", "float")];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use time\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
    assert_eq!(
        linked_errors("use time\ntime.sleep(\"1\")\n"),
        [(
            "'time.sleep' needs an int or float, found str".to_string(),
            2,
            12
        )]
    );
    assert_eq!(
        linked_errors("use os\nos.time()\n"),
        [("module 'os' has no function 'time'".to_string(), 2, 4)]
    );
}

#[test]
fn random_functions_have_types() {
    let cases = [
        ("random.random()", "float"),
        ("random.randint(1, 6)", "int"),
        ("random.choice([\"a\"])", "str"),
        ("random.choice([[1.5]])", "list[float]"),
    ];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use random\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
    let source = "use random\n\nfn pick(xs: list[int]) -> int;\n    ret random.choice(xs) + 1\n\nx = pick([1])\n";
    assert_eq!(linked_type(source, "pick"), "fn(list[int]) -> int");
}

#[test]
fn random_functions_check_their_arguments() {
    let cases = [
        ("random.seed(1.5)", "expected int, found float", 13),
        ("random.randint(1, 2.0)", "expected int, found float", 19),
        (
            "random.choice(3)",
            "'random.choice' needs a list, found int",
            15,
        ),
        (
            "random.random(1)",
            "'random.random' takes 0 arguments, but 1 was given",
            1,
        ),
        (
            "random.randint(1)",
            "'random.randint' takes 2 arguments, but 1 was given",
            1,
        ),
    ];
    for (call, message, col) in cases {
        assert_eq!(
            linked_errors(&format!("use random\n{call}\n")),
            [(message.to_string(), 2, col)],
            "{call}"
        );
    }
}

#[test]
fn tuples_have_a_type_per_field() {
    let source = "t = (1, \"a\", (2.5, [true]))\n";
    assert_eq!(
        type_of_symbol(source, "t"),
        "tuple[int, str, tuple[float, list[bool]]]"
    );
}

#[test]
fn tuple_types_are_written_in_signatures() {
    let source =
        "fn divide(a: int, b: int) -> tuple[int, int];\n    ret (a / b, a % b)\nq = divide(7, 2)\n";
    assert_eq!(type_of_symbol(source, "q"), "tuple[int, int]");
    assert_error(
        "fn f() -> tuple[int, str];\n    ret (1, 2)\n",
        "'f' returns tuple[int, str], but this is tuple[int, int]",
        2,
        9,
    );
}

#[test]
fn unpacking_gives_each_name_its_field() {
    let source = "a, (b, c) = (1, (\"x\", [2.5]))\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_eq!(type_of_symbol(source, "c"), "list[float]");
    // a value not known yet becomes a tuple
    let source = "xs = []\nxs.append((1, \"a\"))\nn, s = xs.pop()\n";
    assert_eq!(type_of_symbol(source, "s"), "str");
    let source = "fn f(pairs: list[tuple[str, int]]);\n    for name, score in pairs;\n        print(name, score + 1)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn unpacking_needs_a_tuple_of_as_many_values() {
    assert_error(
        "a, b, c = (1, 2)\n",
        "cannot unpack tuple[int, int] into 3 names; it has 2 values",
        1,
        1,
    );
    assert_error(
        "a, b = 1\n",
        "cannot unpack int; only a tuple can be unpacked, as in 'a, b = (1, 2)'",
        1,
        1,
    );
    assert_error(
        "xs = [1]\nxs[0], b = (1, 2)\n",
        "only names can be unpacked into, as in 'a, b = t'; assign an element on a line of its own",
        2,
        1,
    );
    assert_error(
        "a, (b, a) = (1, (2, 3))\n",
        "'a' is unpacked into more than once",
        1,
        8,
    );
    assert_error(
        "for a, b in [1, 2];\n    print(a)\n",
        "cannot unpack int; only a tuple can be unpacked, as in 'a, b = (1, 2)'",
        1,
        5,
    );
}

#[test]
fn tuples_are_indexed_by_constants() {
    let source = "t = (1, \"a\", 2.5)\na = t[0]\nb = t[-1]\nc = t[1]\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "float");
    assert_eq!(type_of_symbol(source, "c"), "str");
    assert_error(
        "t = (1, 2)\ni = 0\nprint(t[i])\n",
        "a tuple index must be a number written in the code, such as 't[0]'; unpack the tuple \
         to name its values, as in 'a, b = t'",
        3,
        9,
    );
    assert_error(
        "t = (1, 2)\nprint(t[2])\n",
        "index 2 is out of range for tuple[int, int], which has 2 values",
        2,
        9,
    );
    assert_error(
        "t = (1, 2)\nprint(t[-3])\n",
        "index -3 is out of range for tuple[int, int], which has 2 values",
        2,
        9,
    );
    assert_error(
        "t = (1, 2)\nprint(t[1:])\n",
        "a tuple cannot be sliced; unpack it to name its values, as in 'a, b, c = t'",
        2,
        9,
    );
}

#[test]
fn tuples_cannot_change() {
    let message = "a tuple cannot be changed; build a new one, as in 't = (v, t[1])'";
    assert_error("t = (1, 2)\nt[0] = 3\n", message, 2, 1);
    assert_error("t = (1, 2)\nt[0] += 3\n", message, 2, 1);
    assert_error("t = ([1], 2)\nt[0].append(3)\n", message, 2, 1);
    assert_error("t = ([1], 2)\nt[0][0] = 3\n", message, 2, 1);
    assert_error("ps = [(1, 2)]\nps[0][1] = 3\n", message, 2, 1);
    // replacing a whole element of a list of tuples is fine
    assert_eq!(errors("ps = [(1, 2)]\nps[0] = (3, 4)\n"), []);
}

#[test]
fn tuples_are_not_numbers_strings_or_lists() {
    assert_error(
        "t = (1, 2)\nprint(t + 1)\n",
        "'+' needs int, float, or str operands, found tuple[int, int]",
        2,
        7,
    );
    assert_error(
        "t = (1, 2)\nprint(t == t)\n",
        "'==' and '!=' cannot compare tuple[int, int]",
        2,
        7,
    );
    assert_error(
        "t = (1, 2)\nprint(str(t))\n",
        "'str' needs an int, float, bool, or str, found tuple[int, int]",
        2,
        11,
    );
    assert_error(
        "t = (1, 2)\nfor x in t;\n    print(x)\n",
        "a tuple cannot be iterated; unpack it, or use a list",
        2,
        10,
    );
    assert_error(
        "t = (1, 2)\nif t;\n    print(1)\n",
        "a condition must be bool, found tuple[int, int]",
        2,
        4,
    );
}

#[test]
fn a_list_element_holds_at_most_32_values() {
    let fields = vec!["1"; 33].join(", ");
    let source = format!("xs = [({fields})]\n");
    let (message, line, col) = errors(&source).remove(0);
    assert_eq!(
        (
            message.starts_with("a list element can hold at most 32 values, but tuple[int"),
            line,
            col
        ),
        (true, 1, 6)
    );
    assert!(message.ends_with("holds 33"), "{message}");
    let fields = vec!["int"; 33].join(", ");
    let source = format!("fn f(xs: list[tuple[{fields}]]);\n    print(xs)\n");
    assert_eq!(errors(&source).len(), 1);
    // a tuple outside a list has no limit
    let fields = vec!["1"; 40].join(", ");
    assert_eq!(errors(&format!("t = ({fields})\nprint(t)\n")), []);
}

#[test]
fn a_function_indexes_a_tuple_global_assigned_after_it() {
    let source = "fn first() -> int;\n    ret pair[0]\nfn name() -> str;\n    ret pair[-1]\npair = (1, \"one\")\n";
    assert_eq!(errors(source), []);
    assert_error(
        "fn third() -> int;\n    ret pair[2]\npair = (1, 2)\n",
        "index 2 is out of range for tuple[int, int], which has 2 values",
        2,
        14,
    );
    assert_error(
        "fn first() -> str;\n    ret pair[0]\npair = (1, 2)\n",
        "expected int, found str",
        2,
        14,
    );
    // a list global still indexes as before
    assert_eq!(errors("fn first() -> int;\n    ret xs[0]\nxs = [1]\n"), []);
}

const SHAPE: &str = "enum Shape;\n    Circle(float)\n    Rect(float, float)\n    Empty\n";

#[test]
fn variants_make_values_of_their_enum() {
    let source = format!("{SHAPE}a = Shape.Circle(1.0)\nb = Shape.Empty\nc = [a, b]\n");
    assert_eq!(type_of_symbol(&source, "a"), "Shape");
    assert_eq!(type_of_symbol(&source, "b"), "Shape");
    assert_eq!(type_of_symbol(&source, "c"), "list[Shape]");
    assert_eq!(type_of_symbol(&source, "Rect"), "fn(float, float) -> Shape");
}

#[test]
fn enums_can_be_used_before_their_definition_and_hold_themselves() {
    let source = "fn leaf(n: int) -> Tree;\n    ret Tree.Leaf(n)\n\
                  t = Tree.Node(leaf(1), leaf(2))\n\
                  enum Tree;\n    Leaf(int)\n    Node(Tree, Tree)\n";
    assert_eq!(type_of_symbol(source, "t"), "Tree");
    assert_eq!(type_of_symbol(source, "leaf"), "fn(int) -> Tree");
}

#[test]
fn variants_check_their_values() {
    assert_error(
        &format!("{SHAPE}a = Shape.Circle(\"one\")\n"),
        "expected float, found str",
        5,
        18,
    );
    assert_error(
        &format!("{SHAPE}a = Shape.Rect(1.0)\n"),
        "'Shape.Rect' holds 2 values, but 1 was given",
        5,
        5,
    );
    assert_error(
        &format!("{SHAPE}a = Shape.Circle\n"),
        "'Shape.Circle' holds 1 value, so make one with a call, as in 'Shape.Circle(...)'",
        5,
        5,
    );
    assert_error(
        &format!("{SHAPE}a = Shape.Empty()\n"),
        "'Shape.Empty' holds no values, so it is written without parentheses, as 'Shape.Empty'",
        5,
        5,
    );
    assert_error(
        &format!("{SHAPE}a = Shape.Square(2.0)\n"),
        "'Shape' has no variant 'Square'",
        5,
        11,
    );
    assert_error(
        &format!("{SHAPE}a = Shape\n"),
        "'Shape' is an enum, so a value of it is one of its variants, as in 'Shape.Circle'",
        5,
        5,
    );
}

#[test]
fn enum_definitions_are_checked() {
    assert_error(
        "enum A;\n    X\nenum A;\n    Y\n",
        "enum 'A' is already defined",
        3,
        6,
    );
    assert_error(
        "enum A;\n    X\n    X(int)\n",
        "'A' already has a variant 'X'",
        3,
        5,
    );
    assert_error(
        "enum option;\n    X\n",
        "cannot redefine builtin 'option'",
        1,
        6,
    );
    assert_error(
        "enum list;\n    X\n",
        "cannot redefine builtin 'list'",
        1,
        6,
    );
    assert_error(
        "enum A;\n    X(Missing)\n",
        "there is no type 'Missing'",
        2,
        7,
    );
    assert_error(
        "if true;\n    enum A;\n        X\n",
        "enums can only be defined at the top level",
        2,
        10,
    );
    assert_error(
        "enum A;\n    X\nfn A();\n    ret\n",
        "'A' is already defined as an enum",
        3,
        4,
    );
    assert_error("enum A;\n    X\nA = 1\n", "cannot assign to enum 'A'", 3, 1);
    let wide = vec!["int"; 33].join(", ");
    assert_error(
        &format!("enum A;\n    X({wide})\n"),
        "a variant can hold at most 32 values, but 'A.X' holds 33",
        2,
        5,
    );
}

#[test]
fn written_types_name_enums_and_options() {
    let source =
        format!("{SHAPE}fn first(xs: list[Shape]) -> option[Shape];\n    ret some(xs[0])\n");
    assert_eq!(
        type_of_symbol(&source, "first"),
        "fn(list[Shape]) -> option[Shape]"
    );
    assert_error(
        "fn f(x: option) -> int;\n    ret 1\n",
        "'option' takes one type, as in 'option[int]'",
        1,
        9,
    );
    assert_error(
        &format!("{SHAPE}fn f(x: Shape[int]) -> int;\n    ret 1\n"),
        "'Shape' takes no types",
        5,
        9,
    );
}

#[test]
fn options_hold_a_value_or_none() {
    let source = "a = some(1)\nb = none\nb = some(\"x\")\nc = a.unwrap()\nd = b.unwrap_or(\"y\")\n\
                  e = a.is_some()\nf = a.is_none()\n";
    assert_eq!(type_of_symbol(source, "a"), "option[int]");
    assert_eq!(type_of_symbol(source, "b"), "option[str]");
    assert_eq!(type_of_symbol(source, "c"), "int");
    assert_eq!(type_of_symbol(source, "d"), "str");
    assert_eq!(type_of_symbol(source, "e"), "bool");
    assert_eq!(type_of_symbol(source, "f"), "bool");
    assert_error(
        "a = some(1)\na = some(2.5)\n",
        "cannot assign option[float] to 'a', which is option[int]",
        2,
        5,
    );
    assert_error(
        "x = [1].unwrap()\n",
        "'unwrap' needs an option, found list[int]",
        1,
        5,
    );
    assert_error(
        "x = some(1).unwrap_or(\"a\")\n",
        "expected int, found str",
        1,
        23,
    );
}

#[test]
fn enums_and_options_cannot_be_compared() {
    assert_error(
        "x = some(1)\nprint(x == none)\n",
        "'==' and '!=' cannot compare option[int]; tell its variants apart with 'match', or an \
         option's with 'is_some()' and 'is_none()'",
        2,
        7,
    );
    assert_error(
        "x = some(1)\nif x;\n    print(1)\n",
        "a condition must be bool, found option[int]",
        2,
        4,
    );
}

#[test]
fn a_match_binds_the_values_of_each_case() {
    let source = format!(
        "{SHAPE}fn area(s: Shape) -> float;\n    match s;\n        case Shape.Circle(r);\n            \
         ret 3.0 * r * r\n        case Shape.Rect(w, _);\n            ret w\n        case \
         Shape.Empty;\n            ret 0.0\n"
    );
    let analysis = analyze_source(&source);
    assert_eq!(analysis.diagnostics, []);
    let r = analysis.symbols.iter().find(|s| s.name == "r").unwrap();
    assert_eq!(r.ty, Type::Float);
    assert_eq!(r.kind, SymbolKind::Local);
}

#[test]
fn nested_patterns_match_options_of_enums() {
    let source = format!(
        "{SHAPE}x = some(Shape.Circle(1.0))\nmatch x;\n    case some(Shape.Circle(r));\n        \
         print(r)\n    case some(_);\n        print(0)\n    case none;\n        print(1)\n"
    );
    assert_eq!(errors(&source), []);
}

#[test]
fn a_match_must_cover_every_value() {
    assert_error(
        &format!("{SHAPE}s = Shape.Empty\nmatch s;\n    case Shape.Circle(_);\n        print(1)\n"),
        "'match' does not cover Shape.Rect(_, _); add 'case Shape.Rect(_, _);' or 'case _;'",
        6,
        7,
    );
    assert_error(
        "x = some(some(1))\nmatch x;\n    case some(some(n));\n        print(n)\n    case none;\n        \
         print(0)\n",
        "'match' does not cover some(none); add 'case some(none);' or 'case _;'",
        2,
        7,
    );
    assert_eq!(
        errors(&format!(
            "{SHAPE}s = Shape.Empty\nmatch s;\n    case Shape.Empty;\n        print(1)\n    case other;\n        print(2)\n"
        )),
        []
    );
}

#[test]
fn a_case_after_one_that_matches_everything_never_runs() {
    let source =
        "x = some(1)\nmatch x;\n    case _;\n        print(1)\n    case none;\n        print(0)\n";
    let analysis = analyze_source(source);
    assert_eq!(analysis.diagnostics.len(), 1);
    let warning = &analysis.diagnostics[0];
    assert_eq!(warning.severity, crate::diagnostic::Severity::Warning);
    assert_eq!(
        warning.message,
        "this case never runs, since the cases before it match every value it would"
    );
    assert_eq!(warning.span.start, Pos::new(5, 10));
}

#[test]
fn patterns_are_checked_against_the_value() {
    assert_error(
        &format!("{SHAPE}s = Shape.Empty\nmatch s;\n    case Empty;\n        print(1)\n"),
        "'Empty' here is a new variable that matches any value; to match the variant, write \
         'Shape.Empty'",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s;\n    case some(x);\n        print(x)\n    case _;\n        print(0)\n"
        ),
        "this pattern matches option[unknown], but the value is Shape",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s;\n    case Shape.Rect(w);\n        print(w)\n    case _;\n        print(0)\n"
        ),
        "'Shape.Rect' holds 2 values, but this pattern has 1",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s;\n    case Shape.Rect(w, w);\n        print(w)\n    case _;\n        print(0)\n"
        ),
        "'w' is bound more than once in this pattern",
        7,
        24,
    );
    assert_error(
        "match 3;\n    case n;\n        print(n)\n",
        "'match' needs an enum or option value, found int",
        1,
        7,
    );
    assert_error(
        "x = some(1)\nmatch x;\n    case other(y);\n        print(y)\n    case _;\n        print(0)\n",
        "'other' is not a variant; write a variant with its enum, as in 'Shape.Circle(r)', or 'some(x)'",
        3,
        10,
    );
}

#[test]
fn a_name_bound_in_every_case_is_assigned_after_the_match() {
    let both = "x = some(1)\nmatch x;\n    case some(n);\n        y = n\n    case none;\n        y = 0\nprint(y)\n";
    assert_eq!(errors(both), []);
    let one = "x = some(1)\nmatch x;\n    case some(n);\n        y = n\n    case none;\n        print(0)\nprint(y)\n";
    assert_error(one, "'y' might be used before it is assigned", 7, 7);
}

#[test]
fn a_match_whose_cases_all_return_returns() {
    let source = "fn f(x: option[int]) -> int;\n    match x;\n        case some(n);\n            ret n\n        case none;\n            ret 0\n";
    assert_eq!(errors(source), []);
}
