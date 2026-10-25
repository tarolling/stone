use super::*;
use crate::driver::parse_program;
use crate::span::{FileId, Pos, Span};

/// Parses and checks `source`, returning its analysis.
fn analyze_source(source: &str) -> Analysis {
    let module = parse_program(source).expect("test source should parse");
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
    // the prelude's own names are never a test's
    let matching: Vec<&Symbol> = analysis
        .symbols
        .iter()
        .filter(|s| s.name == name && s.span.file != FileId::PRELUDE)
        .collect();
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
    let source = "fn add(a: int, b: int) -> int:\n    ret a + b\nx = add(1, 2)\n";
    assert_eq!(type_of_symbol(source, "add"), "fn(int, int) -> int");
    assert_eq!(type_of_symbol(source, "x"), "int");
}

#[test]
fn a_function_without_ret_returns_no_value() {
    let source = "fn greet(name: str):\n    print(\"hi\", name)\ngreet(\"bo\")\n";
    assert_eq!(type_of_symbol(source, "greet"), "fn(str)");
}

#[test]
fn recursive_functions_are_typed() {
    let source =
        "fn fact(n: int) -> int:\n    if n != 0:\n        ret n * fact(n - 1)\n    ret 1\n";
    assert_eq!(type_of_symbol(source, "fact"), "fn(int) -> int");
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let source = "print(twice(2))\nfn twice(n: int) -> int:\n    ret n * 2\n";
    assert_eq!(errors(source), []);
}

#[test]
fn unconstrained_types_default_to_int() {
    let source = "fn ignore(a: int) -> int:\n    ret 0\n";
    assert_eq!(type_of_symbol(source, "ignore"), "fn(int) -> int");
}

#[test]
fn parameters_and_returns_take_their_written_types() {
    let source = "fn scale(xs: list[float], k: float) -> list[float]:\n    ret xs * k\n";
    assert_eq!(
        type_of_symbol(source, "scale"),
        "fn(list[float], float) -> list[float]"
    );
}

#[test]
fn a_function_without_a_return_type_returns_no_value() {
    let source = "fn greet(name: str):\n    print(\"hi\", name)\n";
    assert_eq!(type_of_symbol(source, "greet"), "fn(str)");
}

#[test]
fn a_call_that_returns_no_value_is_only_a_statement() {
    let message = "'greet' returns no value, so it can only be called as a statement of its own";
    let greet = "fn greet(name: str):\n    print(\"hi\", name)\n";
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
        "fn f() -> int:\n    ret\n",
        "'f' returns int, so 'ret' needs a value",
        2,
        5,
    );
}

#[test]
fn a_parameter_needs_a_type() {
    assert_error(
        "fn twice(n) -> int:\n    ret n * 2\n",
        "parameter 'n' needs a type, such as 'n: int'",
        1,
        10,
    );
}

#[test]
fn a_missing_type_is_suggested_from_the_calls() {
    assert_error(
        "fn greet(name):\n    print(name)\ngreet(\"bo\")\n",
        "parameter 'name' needs a type, such as 'name: str'",
        1,
        10,
    );
}

#[test]
fn returning_a_value_needs_a_return_type() {
    assert_error(
        "fn one():\n    ret 1\n",
        "'one' has no return type, so it cannot return int; add '-> int' after its parameters",
        2,
        9,
    );
}

#[test]
fn a_missing_return_type_is_suggested_once_it_is_known() {
    // `ys` is a list of unknown elements at the first `ret`, and only the append settles them
    let source =
        "fn f(x: float):\n    ys = []\n    if x > 0.0:\n        ret ys\n    ys.append(x)\n";
    let message = "'f' has no return type, so it cannot return list[float]; add '-> list[float]' \
                   after its parameters";
    assert_error(source, message, 4, 13);
}

#[test]
fn arguments_must_match_the_written_types() {
    assert_error(
        "fn half(x: float) -> float:\n    ret x / 2.0\nhalf(3)\n",
        "expected float, found int",
        3,
        6,
    );
}

#[test]
fn the_body_must_match_the_written_types() {
    assert_error(
        "fn f(s: str) -> int:\n    ret s\n",
        "'f' returns int, but this is str",
        2,
        9,
    );
    assert_error(
        "fn f() -> option[int]:\n    ret 1\n",
        "'f' returns option[int], but this is int",
        2,
        9,
    );
}

#[test]
fn a_written_list_type_settles_arithmetic_before_any_call() {
    // without the types, `xs * 2` could be on numbers or on a list until a call settles it
    let analysis = analyze_source("fn f(xs: list[int]) -> list[int]:\n    ret xs * 2\n");
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
    let source = "fn scale(xs: list[float], k: float) -> list[float]:\n    ret xs * k\ny = scale([1.5], 2.0)\n";
    assert_eq!(
        type_of_symbol(source, "scale"),
        "fn(list[float], float) -> list[float]"
    );
    let source =
        "fn scale(k: int, xs: list[int]) -> list[int]:\n    ret k * xs\ny = scale(2, [1])\n";
    assert_eq!(type_of_symbol(source, "y"), "list[int]");
    let source = "fn double(x: int) -> int:\n    ret x * 2\na = double(3)\n";
    assert_eq!(type_of_symbol(source, "double"), "fn(int) -> int");
    let source = "fn shift(xs: list[float]) -> list[float]:\n    ret xs + 1.0\nb = shift([1.5])\n";
    assert_eq!(type_of_symbol(source, "b"), "list[float]");
    // a result used as a list makes its unknown operand a list
    let source =
        "fn twice(xs: list[int]) -> list[int]:\n    ys = xs * 2\n    ys.append(1)\n    ret ys\n";
    assert_eq!(
        type_of_symbol(source, "twice"),
        "fn(list[int]) -> list[int]"
    );
    // with nothing else to go on, both operands have the same type, as for numbers
    let source = "fn add(a: int, b: int) -> int:\n    ret a + b\n";
    assert_eq!(type_of_symbol(source, "add"), "fn(int, int) -> int");
}

#[test]
fn a_wrong_argument_to_arithmetic_is_reported_where_it_is_passed() {
    let twice = "fn twice(n: int) -> int:\n    ret n * 2\n";
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
        type_of_symbol("fn f(a: int) -> list[int]:\n    ret [] - a\n", "f"),
        "fn(int) -> list[int]"
    );
    assert_eq!(
        type_of_symbol("fn f(a: int) -> list[int]:\n    ret -a * []\n", "f"),
        "fn(int) -> list[int]"
    );
    // an int times 2 is an int, so it cannot be used as a list of floats
    let source = "fn f(n: int) -> list[float]:\n    ys = n * 2\n    ys.append(1.5)\n    ret ys\n";
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
        "'<' needs int, float, str, bytes, or a type that implements 'ord', found list[int]",
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
        "'+' needs int, float, str, or bytes operands, found bool",
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
    let source = "fn f(n: int) -> float:\n    ret 1.5 ** n\nx = f(2)\n";
    assert_eq!(type_of_symbol(source, "x"), "float");
    assert_error(
        "fn f(n: int) -> float:\n    ret 1.5 ** n\nx = f(2.0)\n",
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
    let source = "fn scale(a: float, b: float) -> float:\n    ret -a * b\nx = scale(1.5, 2.0)\n";
    assert_eq!(type_of_symbol(source, "scale"), "fn(float, float) -> float");
    let source = "fn below(a: float, b: float) -> bool:\n    ret a < b\nx = below(1.5, 2.0)\n";
    assert_eq!(type_of_symbol(source, "below"), "fn(float, float) -> bool");
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
        "if 1.5:\n    print(1)\n",
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
        "'<' needs int, float, str, bytes, or a type that implements 'ord', found bool",
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
    assert_eq!(errors("fn f(s: str, t: str) -> bool:\n    ret s < t\n"), []);
}

#[test]
fn strings_index_and_slice_to_strings() {
    let source = "s = \"hello\"\na = s[0]\nb = s[1:3]\nc = s[::-1][0]\n";
    assert_eq!(type_of_symbol(source, "a"), "str");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_eq!(type_of_symbol(source, "c"), "str");
    let source = "fn first(s: str) -> str:\n    ret s[0]\nwords = [\"ab\"]\nw = words[0][1:]\n";
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
    let source = "for c in \"abc\":\n    print(c)\nfn count(s: str) -> int:\n    n = 0\n    for ch in s:\n        n = n + ch.len()\n    ret n\n";
    assert_eq!(type_of_symbol(source, "c"), "str");
    assert_eq!(type_of_symbol(source, "ch"), "str");
    assert_error(
        "for x in 5:\n    print(x)\n",
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
    assert_eq!(type_of_symbol(source, "n"), "option[int]");
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
fn replace_case_lines_rfind_and_count_take_strs() {
    let source = "a = \"ab\".replace(\"a\", \"b\")\nb = \"ab\".replace(\"a\", \"b\", 1)\n\
                  c = \"ab\".lower()\nd = \"ab\".upper()\ne = \"a\\nb\".splitlines()\n\
                  f = \"ab\".rfind(\"b\")\ng = \"ab\".count(\"b\")\n";
    for (name, ty) in [
        ("a", "str"),
        ("b", "str"),
        ("c", "str"),
        ("d", "str"),
        ("e", "list[str]"),
        ("f", "option[int]"),
        ("g", "int"),
    ] {
        assert_eq!(type_of_symbol(source, name), ty, "{name}");
    }
    assert_error(
        "x = 1.replace(\"a\", \"b\")\n",
        "'replace' needs a str, found int",
        1,
        5,
    );
    assert_error(
        "x = \"a\".replace(\"a\")\n",
        "'replace' takes 2 or 3 arguments, but 1 was given",
        1,
        5,
    );
    assert_error(
        "x = \"a\".replace(\"a\", \"b\", \"c\")\n",
        "expected int, found str",
        1,
        27,
    );
    assert_error(
        "x = \"a\".replace(1, \"b\")\n",
        "expected str, found int",
        1,
        17,
    );
    assert_error(
        "x = [1].lower()\n",
        "'lower' needs a str, found list[int]",
        1,
        5,
    );
    assert_error("x = 1.upper()\n", "'upper' needs a str, found int", 1, 5);
    assert_error(
        "x = \"a\".splitlines(\"\\n\")\n",
        "'splitlines' takes 0 arguments, but 1 was given",
        1,
        5,
    );
    assert_error("x = \"a\".rfind(1)\n", "expected str, found int", 1, 15);
    assert_error(
        "x = \"a\".count()\n",
        "'count' takes 1 argument, but 0 were given",
        1,
        5,
    );
    assert_error(
        "x = 1.count(\"a\")\n",
        "'count' needs a str, found int",
        1,
        5,
    );
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
        "if \"yes\":\n    print(1)\n",
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
        errors("if 1 or 0:\n    print(1)\n"),
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
        "n = 3\nwhile n:\n    n -= 1\n",
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
        "fn f(a: int, b: int) -> int:\n    ret a\nf(1)\n",
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
        "fn f(a: int) -> int:\n    ret a\nf(1)\nf(\"s\")\n",
        "expected int, found str",
        4,
        3,
    );
}

#[test]
fn returns_must_agree() {
    assert_error(
        "fn f(x: int) -> int:\n    if x > 0:\n        ret 1\n    ret \"a\"\n",
        "'f' returns int, but this is str",
        4,
        9,
    );
}

#[test]
fn a_value_returning_function_must_return_on_every_path() {
    assert_error(
        "fn f(x: int) -> int:\n    if x > 0:\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        4,
    );
}

#[test]
fn an_endless_loop_counts_as_returning() {
    let source = "fn f(k: int) -> int:\n    m = 1\n    while true:\n        if m * k == 12:\n            ret m\n        m = m + 1\nprint(f(4))\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_loop_that_can_break_does_not_count_as_returning() {
    assert_error(
        "fn f() -> int:\n    while true:\n        break\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        4,
    );
}

#[test]
fn break_and_cont_need_a_loop() {
    assert_error("break\n", "'break' outside a loop", 1, 1);
    assert_error("fn f():\n    cont\n", "'cont' outside a loop", 2, 5);
}

#[test]
fn functions_must_be_top_level() {
    assert_error(
        "fn outer() -> int:\n    fn inner():\n        ret 1\n    ret 2\n",
        "functions can only be defined at the top level",
        2,
        8,
    );
}

const DOUBLE: &str = "fn double(x: int) -> int:\n    ret x * 2\n";

#[test]
fn functions_are_values() {
    let source = format!(
        "{DOUBLE}fn first[T](xs: list[T]) -> T:\n    ret xs[0]\n\
         f = double\nfs = [double, f]\ng = first\ny = g([\"a\"])\nz = fs[1](f(2))\n"
    );
    assert_eq!(type_of_symbol(&source, "f"), "fn(int) -> int");
    assert_eq!(type_of_symbol(&source, "fs"), "list[fn(int) -> int]");
    assert_eq!(type_of_symbol(&source, "g"), "fn(list[str]) -> str");
    assert_eq!(type_of_symbol(&source, "z"), "int");
}

#[test]
fn function_types_are_written_in_signatures() {
    let source = format!(
        "{DOUBLE}fn apply(f: fn(int) -> int, x: int) -> int:\n    ret f(x)\n\
         fn run(f: fn()):\n    f()\n\
         fn twice(f: fn(int) -> int) -> fn(int) -> int:\n    ret f\n\
         y = apply(twice(double), 1)\n"
    );
    assert_eq!(type_of_symbol(&source, "y"), "int");
    assert_eq!(type_of_symbol(&source, "run"), "fn(fn())");
}

#[test]
fn a_function_value_must_have_the_right_type() {
    let source = format!(
        "{DOUBLE}fn apply(f: fn(int) -> bool, x: int) -> bool:\n    ret f(x)\nb = apply(double, 1)\n"
    );
    assert_error(
        &source,
        "expected fn(int) -> bool, found fn(int) -> int",
        5,
        11,
    );
}

#[test]
fn a_call_through_a_value_is_checked() {
    let f = format!("{DOUBLE}f = double\n");
    assert_error(
        &format!("{f}x = f(1, 2)\n"),
        "'f' takes 1 argument, but 2 were given",
        4,
        5,
    );
    assert_error(
        &format!("{f}x = f(\"a\")\n"),
        "expected int, found str",
        4,
        7,
    );
    assert_error(
        &format!("{f}x = [f][0](y=1)\n"),
        "only a struct is made with named values; pass this value without its name",
        4,
        12,
    );
}

#[test]
fn a_function_value_that_returns_nothing_is_only_a_statement() {
    assert_error(
        "fn hi(n: int):\n    print(n)\nf = hi\nf(1)\nx = f(2)\n",
        "'f' returns no value, so it can only be called as a statement of its own",
        5,
        5,
    );
}

#[test]
fn function_values_cannot_be_compared_or_keys() {
    let f = format!("{DOUBLE}f = double\n");
    assert_error(
        &format!("{f}b = f == double\n"),
        "'==' and '!=' cannot compare fn(int) -> int",
        4,
        5,
    );
    assert_error(
        &format!("{f}b = (1, f) == (1, f)\n"),
        "'==' and '!=' cannot compare tuple[int, fn(int) -> int], since it holds fn(int) -> int",
        4,
        5,
    );
    assert_error(
        &format!("{f}m = {{f: 1}}\n"),
        "a map key must be int, str, or bool, found fn(int) -> int",
        4,
        6,
    );
    assert_error(
        &format!("{f}x = f + 1\n"),
        "'+' needs int, float, str, or bytes operands, found fn(int) -> int",
        4,
        5,
    );
}

#[test]
fn a_field_holding_a_function_is_called_like_a_method() {
    let step = format!(
        "{DOUBLE}struct Step:\n    run: fn(int) -> int\n    n: int\n\
         s = Step(run=double, n=1)\n"
    );
    assert_eq!(type_of_symbol(&format!("{step}y = s.run(2)\n"), "y"), "int");
    assert_error(
        &format!("{step}y = s.run(\"a\")\n"),
        "expected int, found str",
        7,
        11,
    );
    assert_error(
        &format!("{step}y = s.n(1)\n"),
        "'Step' has no method 'n'; 'n' is a field of 'Step', so read it without parentheses",
        7,
        7,
    );
    // a global a function reads before its assignment is checked is settled at the end
    let late = format!(
        "{DOUBLE}struct Step:\n    run: fn(int) -> int\n\
         fn go() -> int:\n    ret s.run(3)\ns = Step(run=double)\nprint(go())\n"
    );
    assert_eq!(errors(&late), []);
}

#[test]
fn list_helpers_take_functions_of_the_elements() {
    let start = format!("{DOUBLE}fn pair(x: int) -> list[int]:\n    ret [x]\nxs = [1]\n");
    let source = format!("{start}ys = xs.map(double)\nb = xs.any(pair)\n");
    assert_eq!(
        linked_errors(&source),
        [(
            "expected fn(int) -> bool, found fn(int) -> list[int]".to_string(),
            7,
            12
        )]
    );
    let cases = [
        (
            "a = \"s\".map(double)\n",
            "'map' is a method of lists, but this is str",
            5,
        ),
        (
            "a = [\"s\"].map(double)\n",
            "expected fn(str) -> int, found fn(int) -> int",
            15,
        ),
        (
            "a = xs.sort_by_key(double)\n",
            "'sort_by_key' changes a list and returns none, so it can only be a statement of its \
             own",
            5,
        ),
        (
            "xs.sort_by_key(pair)\n",
            "'list[int]' does not implement 'ord', which 'sort_by_key' needs for 'K'",
            1,
        ),
        (
            "a = xs.map(double, 1)\n",
            "'map' takes 1 argument, but 2 were given",
            5,
        ),
    ];
    for (line, message, col) in cases {
        assert_eq!(
            linked_errors(&format!("{start}{line}")),
            [(message.to_string(), 6, col)],
            "{line}"
        );
    }
    assert_eq!(
        linked_type(&format!("{start}m = xs.max_by_key(double)\n"), "m"),
        "option[int]"
    );
}

#[test]
fn builtins_are_not_values_yet() {
    assert_error(
        "x = print\n",
        "'print' is a builtin, which cannot be a value yet; to pass it, define a function that \
         calls it",
        1,
        5,
    );
}

#[test]
fn a_function_value_in_generic_code_has_a_known_type() {
    let source = "fn id[T](x: T) -> T:\n    ret x\n\
                  fn wrap[T](x: T) -> T:\n    f = id\n    ret f(x)\n\
                  fn fine[T](x: T) -> int:\n    f = len_of\n    ret f(\"ab\")\n\
                  fn len_of(s: str) -> int:\n    ret s.len()\n\
                  y = wrap(1)\nz = fine(1)\n";
    assert_error(
        source,
        "'id' would be a value of type fn(T) -> T, which depends on the type parameter 'T'; a \
         function value cannot have such a type yet, so take it as a parameter instead",
        4,
        9,
    );
}

#[test]
fn functions_and_builtins_cannot_be_assigned() {
    assert_error(
        "fn f() -> int:\n    ret 1\nf = 2\n",
        "cannot assign to function 'f'",
        3,
        1,
    );
    assert_error("print = 2\n", "cannot assign to builtin 'print'", 1, 1);
}

#[test]
fn only_functions_can_be_called() {
    assert_error(
        "x = 1\nx(2)\n",
        "'x' is int, not a function, so it cannot be called",
        2,
        1,
    );
    assert_error(
        "x = [1]\nx[0](2)\n",
        "this is int, not a function, so it cannot be called",
        2,
        1,
    );
}

#[test]
fn functions_cannot_be_redefined() {
    assert_error(
        "fn f() -> int:\n    ret 1\nfn f():\n    ret 2\n",
        "function 'f' is already defined",
        3,
        4,
    );
}

#[test]
fn parameters_must_be_unique() {
    assert_error(
        "fn f(a: int, a: int) -> int:\n    ret a\n",
        "duplicate parameter 'a'",
        1,
        14,
    );
}

#[test]
fn len_needs_a_str() {
    assert_error(
        "x = 5.len()\n",
        "'len' needs a str, bytes, list, or map, found int",
        1,
        5,
    );
}

#[test]
fn locals_shadow_globals_and_keep_their_own_type() {
    let source = "x = 1\nfn f() -> str:\n    x = \"local\"\n    ret x\ny = f()\n";
    assert_eq!(errors(source), []);
    assert_eq!(type_of_symbol(source, "y"), "str");
}

#[test]
fn functions_read_globals() {
    let source = "fn f() -> int:\n    ret n + 1\nn = 3\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_local_assigned_anywhere_in_the_function_is_local_everywhere() {
    // like Python, `x` is local to `f` because `f` assigns it, so the global is never read, and
    // reading the local before assigning it is an error rather than a read of the global
    let source = "x = \"global\"\nfn f() -> int:\n    y = x\n    x = 1\n    ret y\nprint(f())\n";
    assert_error(source, "'x' might be used before it is assigned", 3, 9);
    let analysis = analyze_source(source);
    let y = analysis.symbols.iter().find(|s| s.name == "y").unwrap();
    assert_eq!(y.ty, Type::Int);
}

#[test]
fn every_use_of_a_symbol_is_a_reference() {
    let analysis = analyze_source("fn f(a: int) -> int:\n    b = a\n    ret a + b\nf(1)\n");
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
    let source = "for i in range(3):\n    x = i\nfor j in range(1, 4):\n    y = j\n";
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
        "for i in range():\n    print(i)\n",
        "'range' takes 1 or 2 arguments, but 0 were given",
        1,
        10,
    );
    assert_error(
        "for i in range(\"a\"):\n    print(i)\n",
        "expected int, found str",
        1,
        16,
    );
}

#[test]
fn for_loop_variables_must_be_names() {
    let messages: Vec<String> = errors("a = 0\nfor a[0] in range(3):\n    print(1)\n")
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
        "for i in range(9):\n    if i == 2:\n        cont\n    if i == 5:\n        break\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_for_loop_does_not_count_as_returning() {
    assert_error(
        "fn f() -> int:\n    for i in range(3):\n        ret i\n",
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
    let source = "for word in [\"a\", \"b\"]:\n    w = word\n";
    assert_eq!(type_of_symbol(source, "w"), "str");
    assert_error(
        "for x in 5:\n    print(x)\n",
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
    assert_error("fn f():\n    print(x)\n    x = 1\n", UNASSIGNED, 2, 11);
}

#[test]
fn assignments_in_one_branch_are_not_enough() {
    assert_error("c = true\nif c:\n    x = 1\nprint(x)\n", UNASSIGNED, 4, 7);
    assert_error(
        "fn f(c: bool) -> int:\n    if c:\n        x = 1\n    ret x\n",
        UNASSIGNED,
        4,
        9,
    );
}

#[test]
fn assignments_in_every_branch_are_enough() {
    let source =
        "c = 1\nif c == 1:\n    x = 1\nelif c == 2:\n    x = 2\nelse:\n    x = 3\nprint(x)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn branches_that_leave_do_not_count() {
    let source =
        "fn f(c: bool) -> int:\n    if c:\n        x = 1\n    else:\n        ret 0\n    ret x\n";
    assert_eq!(errors(source), []);
    let source =
        "for i in range(3):\n    if i > 0:\n        x = i\n    else:\n        cont\n    print(x)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn loop_bodies_might_not_run() {
    assert_error(
        "c = 0\nwhile c < 1:\n    x = 1\n    c = c + 1\nprint(x)\n",
        UNASSIGNED,
        5,
        7,
    );
    assert_error(
        "for x in range(3):\n    y = x\nprint(x)\n",
        UNASSIGNED,
        3,
        7,
    );
}

#[test]
fn a_loop_body_cannot_read_what_it_assigns_later() {
    assert_error(
        "c = 0\nwhile c < 2:\n    print(x)\n    x = c\n    c = c + 1\n",
        UNASSIGNED,
        3,
        11,
    );
}

#[test]
fn functions_read_globals_whenever_they_are_called() {
    // whether `total` is assigned depends on when `f` runs, which the interpreter checks
    let source = "fn f() -> int:\n    ret total\ntotal = 1\nprint(f())\n";
    assert_eq!(errors(source), []);
}

#[test]
fn each_unassigned_variable_is_reported_once() {
    assert_error("print(x, x)\nx = 1\n", UNASSIGNED, 1, 7);
}

#[test]
fn references_are_found_by_position() {
    let analysis = analyze_source("fn add(a: int, b: int) -> int:\n    ret a + b\nx = add(1, 2)\n");
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
    let analysis = analyze_source("fn f(a: int) -> int:\n    b = a\n    ret b\nx = 1\n");
    assert_eq!(
        analysis.functions.get("f"),
        Some(&Span::new(Pos::new(1, 1), Pos::new(3, 10)))
    );
}

#[test]
fn visible_symbols_depend_on_position() {
    let source = "g = 1\nfn f(a: int) -> int:\n    b = a\n    ret b\nfn h() -> int:\n    ret 2\n";
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
        "fn f() -> list[str]:\n    ret [\"a\"]\ngrid = [[1]]\ngrid[0].append(2)\nn = f().len()\n";
    assert_eq!(type_of_symbol(source, "n"), "int");
}

#[test]
fn append_infers_a_parameter_is_a_list() {
    let source = "fn push(xs: list[int], x: int) -> list[int]:\n    xs.append(x + 1)\n    ret xs\npush([], 2)\n";
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
        type_of_symbol("fn append(a: int) -> int:\n    ret a\n", "append"),
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
    // without an index, pop gives none for an empty list
    let source = "xs = [1.5]\nx = xs.pop()\ny = xs.pop(0)\nxs.insert(0, 2.5)\nxs.extend([y])\n\
                  xs.pop()\n";
    assert_eq!(type_of_symbol(source, "x"), "option[float]");
    assert_eq!(type_of_symbol(source, "y"), "float");
    assert_eq!(type_of_symbol(source, "xs"), "list[float]");
    // the list's type can come from what is put in it
    let source = "xs = []\nxs.insert(0, \"a\")\nys = []\nys.extend(xs)\nz = ys.pop()\n";
    assert_eq!(type_of_symbol(source, "z"), "option[str]");
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
fn sort_orders_lists_of_numbers_and_strings() {
    let source = "xs = [3, 1]\nxs.sort()\nys = []\nys.append(1.5)\nys.sort()\nzs = [\"b\"]\n\
                  zs.sort()\ngrid = [[2, 1]]\ngrid[0].sort()\n";
    assert_eq!(errors(source), []);
    assert_eq!(type_of_symbol(source, "ys"), "list[float]");
    let message = "'sort' needs a list of int, float, str, or a type that implements 'ord', found a list of {}";
    assert_error(
        "xs = [true]\nxs.sort()\n",
        &message.replace("{}", "bool"),
        2,
        1,
    );
    assert_error(
        "xs = [(1, 2)]\nxs.sort()\n",
        &message.replace("{}", "tuple[int, int]"),
        2,
        1,
    );
    assert_error(
        "fn f[T](xs: list[T]) -> list[T]:\n    xs.sort()\n    ret xs\n",
        &message.replace("{}", "T"),
        2,
        5,
    );
    assert_error(
        "xs = [1]\nxs.sort(1)\n",
        "'sort' takes 0 arguments, but 1 was given",
        2,
        1,
    );
    assert_error(
        "x = 1\nx.sort()\n",
        "expected list[unknown], found int",
        2,
        1,
    );
    let message =
        "'sort' changes a list and returns none, so it can only be a statement of its own";
    assert_error("xs = [1]\ny = xs.sort()\n", message, 2, 5);
    assert_error("xs = [1]\nprint(xs.sort())\n", message, 2, 7);
    // a function cannot sort a global
    assert_error(
        "xs = [1]\nfn f():\n    xs.sort()\nf()\n",
        "a function cannot change the global 'xs'; pass it as an argument and return the changed \
         value",
        3,
        5,
    );
}

#[test]
fn pop_may_be_inside_an_expression() {
    assert_eq!(
        errors(
            "xs = [1, 2, 3]\nprint(xs.pop().unwrap())\ny = xs.pop(0) + 1\n\
             a = xs.pop().unwrap_or(0) + xs.pop().unwrap_or(0)\nwhile xs.pop().is_some():\n    \
             print(1)\nxs.append(xs.pop().unwrap_or(4))\nmatch xs.pop():\n    case some(x):\n        \
             print(x)\n    case none:\n        print(0)\ngrid = [[1]]\n\
             for v in grid.pop().unwrap():\n    print(v)\n"
        ),
        []
    );
    assert_eq!(
        errors("fn f(xs: list[int]) -> list[int]:\n    print(xs.pop(-1))\n    ret xs\nf([1])\n"),
        []
    );
    let message =
        "'insert' changes a list and returns none, so it can only be a statement of its own";
    assert_error("xs = [1]\ny = xs.insert(0, 1)\n", message, 2, 5);
}

#[test]
fn pop_inside_an_expression_is_the_only_use_of_its_list_there() {
    let message = "'pop' changes 'xs' in this statement, so 'xs' cannot also be used in it; assign \
                   the result of 'pop' to a variable first";
    assert_error("xs = [1]\nprint(xs, xs.pop())\n", message, 2, 7);
    assert_error(
        "xs = [1]\ny = xs.pop().unwrap() + xs.len()\n",
        message,
        2,
        25,
    );
    assert_error("xs = [1, 2]\nxs[0] = xs.pop().unwrap()\n", message, 2, 1);
    assert_error("xs = [1, 2]\nxs[0] += xs.pop().unwrap()\n", message, 2, 1);
    // the whole value of an assignment is evaluated before its target, as before
    assert_eq!(errors("xs = [1, 2]\nxs[0] = xs.pop(0)\n"), []);
    let message = "'pop' changes 'grid' in this statement, so 'grid' cannot also be used in it; \
                   assign the result of 'pop' to a variable first";
    assert_error(
        "grid = [[1], [2]]\nprint(grid[0].pop(), grid[1])\n",
        message,
        2,
        22,
    );
}

#[test]
fn a_pop_inside_an_expression_is_a_change() {
    let mut changed = HashSet::new();
    let module =
        parse_program("print(xs.pop().unwrap())\nif ys[0].pop().is_some():\n    ret zs.pop(0)\n")
            .unwrap();
    let Mod::Module { body } = &module;
    collect_changed(body, &mut changed, &Methods::new());
    let mut changed: Vec<String> = changed.into_iter().collect();
    changed.sort();
    assert_eq!(changed, ["xs", "ys", "zs"]);
    // the caller cannot see the change, as with `x = xs.pop(0)`
    assert_eq!(
        errors("fn f(xs: list[int]) -> int:\n    ret xs.pop().unwrap()\nf([1])\n"),
        [(
            "changes to 'xs' are lost when 'f' returns, since parameters are copies; return it \
             and assign the result where 'f' is called"
                .into(),
            2,
            9
        )]
    );
}

#[test]
fn a_method_that_pops_inside_an_expression_changes_self() {
    let source = "struct Queue:\n    items: list[int]\n    offset: int\nimpl Queue:\n    \
                  fn take(self) -> int:\n        ret self.items.pop().unwrap_or(0) + self.offset\n\
                  q = Queue(items=[1], offset=2)\nn = q.take()\n";
    assert_eq!(
        method_calls(source),
        [("Queue.take".to_string(), true, true)]
    );
    assert_error(
        &source.replace("+ self.offset", "+ self.items.len()"),
        "'pop' changes 'self.items' in this statement, so 'self.items' cannot also be used in \
         it; assign the result of 'pop' to a variable first",
        6,
        45,
    );
    assert_error(
        &source.replace(
            "ret self.items.pop().unwrap_or(0) + self.offset",
            "print(self, self.items.pop())\n        ret 0",
        ),
        "'pop' changes 'self.items' in this statement, so 'self' cannot also be used in it; \
         assign the result of 'pop' to a variable first",
        6,
        15,
    );
}

#[test]
fn popping_counts_as_a_change() {
    // a function cannot pop from a global, and popping a parameter it never reads again is lost
    assert_error(
        "xs = [1]\nfn f() -> int:\n    x = xs.pop(0)\n    ret x\nf()\n",
        "a function cannot change the global 'xs'; pass it as an argument and return the \
         changed value",
        3,
        9,
    );
    let source = "fn f(xs: list[int]) -> int:\n    x = xs.pop(0)\n    ret x\nf([1])\n";
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
        errors("fn f(xs: list[int]) -> list[int]:\n    x = xs.pop()\n    ret xs\nf([1])\n"),
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
        "fn f() -> list[int]:\n    ret [1]\nf().append(2)\n",
        message,
        3,
        1,
    );
    assert_error(
        "fn f() -> list[list[int]]:\n    ret [[1]]\nf()[0][0] = 2\n",
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
    let source = "names = []\nfn add(name: str):\n    names.append(name)\nadd(\"a\")\n";
    assert_error(
        source,
        "a function cannot change the global 'names'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    let source = "grid = [[0]]\nfn clear(i: int):\n    grid[i][0] = 0\nclear(0)\n";
    assert_error(
        source,
        "a function cannot change the global 'grid'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    // reading a global is fine, and so is changing a local that shadows one
    let source = "names = [1]\nfn f() -> list[int]:\n    names = [2]\n    names.append(3)\n    ret names\n\
                  fn g() -> int:\n    ret names[0]\nprint(f(), g())\n";
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
        "x = 1\nfn f():\n    x += 1\nf()\n",
        "'x' might be used before it is assigned",
        3,
        5,
    );
    assert_error(
        "fn g():\n    ret\ng += 1\n",
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
        "xs = [1]\nfn f():\n    xs[0] += 1\nf()\n",
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
        .filter(|reference| reference.span.file == FileId(0))
        .map(|reference| reference.span.start)
        .collect();
    assert_eq!(spans, [Pos::new(1, 1), Pos::new(2, 1), Pos::new(3, 7)]);
}

#[test]
fn changing_a_parameter_by_augmented_assignment_can_be_lost() {
    let diagnostics =
        analyze_source("fn bump(xs: list[int]):\n    xs[0] += 1\nbump([1])\n").diagnostics;
    assert_eq!(diagnostics.len(), 1, "{diagnostics:?}");
    assert_eq!(
        diagnostics[0].severity,
        crate::diagnostic::Severity::Warning
    );
    assert_eq!(diagnostics[0].span.start, Pos::new(2, 5));
    // rebinding a parameter is no change to the caller's list
    assert_eq!(
        errors(
            "fn twice(xs: list[int]) -> list[int]:\n    xs *= 2\n    ret xs\nprint(twice([1]))\n"
        ),
        []
    );
}

#[test]
fn changing_a_parameter_that_is_never_used_again_is_a_warning() {
    let source = "fn push(xs: list[int], x: int):\n    xs.append(x)\nys = [1]\npush(ys, 2)\n";
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
    let source = "fn clear(grid: list[list[int]], i: int):\n    if i > 0:\n        grid[i][0] = 0\nclear([[1]], 0)\n";
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
            "    for i in range(3):\n        xs.append(x)\n    ret xs.len()\n",
        ),
    ] {
        let source = format!("fn push(xs: list[int], x: int){returns}:\n{body}push([1], 2)\n");
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
fn str_converts_every_value() {
    let source = "struct P:\n    n: int\nenum E:\n    A(int)\n    B\n\
                  a = str(1)\nb = str(1.5)\nc = str(true)\nd = str(\"x\")\ne = str([1])\n\
                  f = str((1, \"a\"))\ng = str({\"k\": [2]})\nh = str(P(n=1))\ni = str(E.A(1))\n\
                  j = str(some(E.B))\nk = P(n=1).display()\nl = [1].display()\n";
    for name in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"] {
        assert_eq!(type_of_symbol(source, name), "str");
    }
    let generic = "fn show[T](v: T) -> str:\n    ret str(v) + v.display()\nshown = show([1])\n";
    assert_eq!(type_of_symbol(generic, "shown"), "str");
    assert_error(
        "fn f():\n    ret\nx = str(f())\n",
        "'f' returns no value, so it can only be called as a statement of its own",
        3,
        9,
    );
    assert_error(
        "x = 1.display(2)\n",
        "'display' takes 0 arguments, but 1 was given",
        1,
        5,
    );
    assert_error(
        "x = str()\n",
        "'str' takes 1 argument, but 0 were given",
        1,
        5,
    );
}

#[test]
fn a_string_with_values_is_a_str() {
    let source = "n = 1\nxs = [n]\ns = \"{n} {xs} {some(xs)} {(n, xs)}\"\n";
    assert_eq!(type_of_symbol(source, "s"), "str");
    assert_error(
        "n = 1\ns = \"{n + 1.5}\"\n",
        "expected int, found float",
        2,
        11,
    );
    assert_error(
        "fn f():\n    ret\ns = \"a {f()} b\"\n",
        "'f' returns no value, so it can only be called as a statement of its own",
        3,
        9,
    );
    assert_error("s = \"{y}\"\n", "undefined name 'y'", 1, 7);
    // a value is read like any other
    assert_error(
        "if true:\n    print(\"{n}\")\nn = 1\n",
        "'n' might be used before it is assigned",
        2,
        13,
    );
}

#[test]
fn input_reads_strs() {
    assert_eq!(type_of_symbol("a = input()\n", "a"), "result[str, str]");
    assert_eq!(
        type_of_symbol("a = input(\"? \")\n", "a"),
        "result[str, str]"
    );
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
    assert_eq!(errors("while not eof():\n    print(input())\n"), []);
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
        "fn f() -> int:\n    use util\n    ret 1\n",
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
        ("os.env(\"HOME\")", "option[str]"),
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
fn fs_functions_have_types() {
    let cases = [
        ("fs.read(\"a\")", "result[str, str]"),
        ("fs.read_lines(\"a\")", "result[list[str], str]"),
        ("fs.write(\"a\", \"b\")", "result[int, str]"),
        ("fs.append(\"a\", \"b\")", "result[int, str]"),
        ("fs.list_dir(\"a\")", "result[list[str], str]"),
        ("fs.exists(\"a\")", "bool"),
        ("fs.remove(\"a\")", "result[str, str]"),
        ("fs.make_dir(\"a\")", "result[str, str]"),
    ];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use fs\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
}

#[test]
fn fs_functions_check_their_arguments() {
    assert_eq!(
        linked_errors("use fs\nfs.write(\"a\")\n"),
        [(
            "'fs.write' takes 2 arguments, but 1 was given".to_string(),
            2,
            1
        )]
    );
    assert_eq!(
        linked_errors("use fs\nfs.read(1)\n"),
        [("expected str, found int".to_string(), 2, 9)]
    );
    assert_eq!(
        linked_errors("use fs\nfs.append(\"a\", 1)\n"),
        [("expected str, found int".to_string(), 2, 16)]
    );
}

#[test]
fn fs_errors_pass_on_with_try() {
    let source = "use fs\nfn size(path: str) -> result[int, str]:\n    \
                  ret ok(fs.read(path)?.len())\nprint(size(\"a\"))\n";
    assert_eq!(linked_errors(source), []);
    let wrong = "use fs\nfn size(path: str) -> result[int, int]:\n    \
                 ret ok(fs.read(path)?.len())\nprint(size(\"a\"))\n";
    assert_eq!(linked_errors(wrong).len(), 1);
}

#[test]
fn os_run_has_a_type_and_checks_its_arguments() {
    assert_eq!(
        linked_type("use os\nx = os.run(\"ls\", [\"-l\"])\n", "x"),
        "result[output, str]"
    );
    assert_eq!(
        linked_type("use os\nx = os.run(\"ls\", []).unwrap().stdout\n", "x"),
        "str"
    );
    assert_eq!(
        linked_errors("use os\nos.run(\"ls\")\n"),
        [(
            "'os.run' takes 2 arguments, but 1 was given".to_string(),
            2,
            1
        )]
    );
    assert_eq!(
        linked_errors("use os\nos.run(1, [])\n"),
        [("expected str, found int".to_string(), 2, 8)]
    );
    assert_eq!(
        linked_errors("use os\nos.run(\"ls\", [1])\n"),
        [("expected list[str], found list[int]".to_string(), 2, 14)]
    );
}

#[test]
fn a_value_may_be_named_like_the_prelude_struct() {
    let source = "use os\noutput = os.run(\"ls\", []).unwrap()\nprint(output.stdout.len())\n\
                  fn run(cmd: str) -> result[output, str]:\n    output = os.run(cmd, [])?\n    \
                  ret ok(output)\nfn status(output: output) -> int:\n    ret output.status\n\
                  print(status(run(\"ls\").unwrap()))\n";
    assert_eq!(linked_errors(source), []);
    assert_eq!(linked_type("output = 1\no = output + 1\n", "o"), "int");
    // without a variable of that name, the name makes one
    assert_eq!(
        linked_type("o = output(status=0, stdout=\"\", stderr=\"\")\n", "o"),
        "output"
    );
    // a function cannot take the name, since a call of it would make the struct
    assert_eq!(
        linked_errors("fn output() -> int:\n    ret 1\n"),
        [("'output' is already defined as a struct".to_string(), 1, 4)]
    );
}

#[test]
fn an_os_function_is_not_a_value() {
    assert_eq!(
        linked_errors("use os.pid\nx = pid\n"),
        [(
            "'os.pid' is a builtin, which cannot be a value yet; to pass it, define a function \
             that calls it"
                .to_string(),
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
        "use math\n\nfn f(a: int, b: int) -> int:\n    ret math.max(a, b) + 1\n\nx = f(1, 2)\n";
    assert_eq!(linked_type(source, "f"), "fn(int, int) -> int");
    let source =
        "use math\n\nfn g(xs: list[float]) -> float:\n    ret math.min(xs) * 2.0\n\nx = g([1.0])\n";
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
    let source = "use random\n\nfn pick(xs: list[int]) -> int:\n    ret random.choice(xs) + 1\n\nx = pick([1])\n";
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
        "fn divide(a: int, b: int) -> tuple[int, int]:\n    ret (a / b, a % b)\nq = divide(7, 2)\n";
    assert_eq!(type_of_symbol(source, "q"), "tuple[int, int]");
    assert_error(
        "fn f() -> tuple[int, str]:\n    ret (1, 2)\n",
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
    let source = "xs = []\nxs.append((1, \"a\"))\nn, s = xs.pop(-1)\n";
    assert_eq!(type_of_symbol(source, "s"), "str");
    let source = "fn f(pairs: list[tuple[str, int]]):\n    for name, score in pairs:\n        print(name, score + 1)\n";
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
        "for a, b in [1, 2]:\n    print(a)\n",
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
        "'+' needs int, float, str, or bytes operands, found tuple[int, int]",
        2,
        7,
    );
    // `str` writes a tuple as `print` does
    assert_eq!(errors("t = (1, 2)\nprint(str(t))\n"), []);
    assert_error(
        "t = (1, 2)\nfor x in t:\n    print(x)\n",
        "a tuple cannot be iterated; unpack it, or use a list",
        2,
        10,
    );
    assert_error(
        "t = (1, 2)\nif t:\n    print(1)\n",
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
    let source = format!("fn f(xs: list[tuple[{fields}]]):\n    print(xs)\n");
    assert_eq!(errors(&source).len(), 1);
    // a tuple outside a list has no limit
    let fields = vec!["1"; 40].join(", ");
    assert_eq!(errors(&format!("t = ({fields})\nprint(t)\n")), []);
}

#[test]
fn a_function_indexes_a_tuple_global_assigned_after_it() {
    let source = "fn first() -> int:\n    ret pair[0]\nfn name() -> str:\n    ret pair[-1]\npair = (1, \"one\")\n";
    assert_eq!(errors(source), []);
    assert_error(
        "fn third() -> int:\n    ret pair[2]\npair = (1, 2)\n",
        "index 2 is out of range for tuple[int, int], which has 2 values",
        2,
        14,
    );
    assert_error(
        "fn first() -> str:\n    ret pair[0]\npair = (1, 2)\n",
        "expected int, found str",
        2,
        14,
    );
    // a list global still indexes as before
    assert_eq!(errors("fn first() -> int:\n    ret xs[0]\nxs = [1]\n"), []);
}

const SHAPE: &str = "enum Shape:\n    Circle(float)\n    Rect(float, float)\n    Empty\n";

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
    let source = "fn leaf(n: int) -> Tree:\n    ret Tree.Leaf(n)\n\
                  t = Tree.Node(leaf(1), leaf(2))\n\
                  enum Tree:\n    Leaf(int)\n    Node(Tree, Tree)\n";
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
        "enum A:\n    X\nenum A:\n    Y\n",
        "enum 'A' is already defined",
        3,
        6,
    );
    assert_error(
        "enum A:\n    X\n    X(int)\n",
        "'A' already has a variant 'X'",
        3,
        5,
    );
    assert_error(
        "enum option:\n    X\n",
        "cannot redefine builtin 'option'",
        1,
        6,
    );
    assert_error(
        "enum list:\n    X\n",
        "cannot redefine builtin 'list'",
        1,
        6,
    );
    assert_error(
        "enum A:\n    X(Missing)\n",
        "there is no type 'Missing'",
        2,
        7,
    );
    assert_error(
        "if true:\n    enum A:\n        X\n",
        "enums can only be defined at the top level",
        2,
        10,
    );
    assert_error(
        "enum A:\n    X\nfn A():\n    ret\n",
        "'A' is already defined as an enum",
        3,
        4,
    );
    assert_error("enum A:\n    X\nA = 1\n", "cannot assign to enum 'A'", 3, 1);
    let wide = vec!["int"; 33].join(", ");
    assert_error(
        &format!("enum A:\n    X({wide})\n"),
        "a variant can hold at most 32 values, but 'A.X' holds 33",
        2,
        5,
    );
}

#[test]
fn written_types_name_enums_and_options() {
    let source =
        format!("{SHAPE}fn first(xs: list[Shape]) -> option[Shape]:\n    ret some(xs[0])\n");
    assert_eq!(
        type_of_symbol(&source, "first"),
        "fn(list[Shape]) -> option[Shape]"
    );
    assert_error(
        "fn f(x: option) -> int:\n    ret 1\n",
        "'option' takes one type, as in 'option[int]'",
        1,
        9,
    );
    assert_error(
        &format!("{SHAPE}fn f(x: Shape[int]) -> int:\n    ret 1\n"),
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
        "'unwrap' needs an option or a result, found list[int]",
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
fn enums_and_options_are_not_conditions() {
    assert_error(
        "x = some(1)\nif x:\n    print(1)\n",
        "a condition must be bool, found option[int]",
        2,
        4,
    );
}

#[test]
fn a_match_binds_the_values_of_each_case() {
    let source = format!(
        "{SHAPE}fn area(s: Shape) -> float:\n    match s:\n        case Shape.Circle(r):\n            \
         ret 3.0 * r * r\n        case Shape.Rect(w, _):\n            ret w\n        case \
         Shape.Empty:\n            ret 0.0\n"
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
        "{SHAPE}x = some(Shape.Circle(1.0))\nmatch x:\n    case some(Shape.Circle(r)):\n        \
         print(r)\n    case some(_):\n        print(0)\n    case none:\n        print(1)\n"
    );
    assert_eq!(errors(&source), []);
}

#[test]
fn a_match_must_cover_every_value() {
    assert_error(
        &format!("{SHAPE}s = Shape.Empty\nmatch s:\n    case Shape.Circle(_):\n        print(1)\n"),
        "'match' does not cover Shape.Rect(_, _); add 'case Shape.Rect(_, _):' or 'case _:'",
        6,
        7,
    );
    assert_error(
        "x = some(some(1))\nmatch x:\n    case some(some(n)):\n        print(n)\n    case none:\n        \
         print(0)\n",
        "'match' does not cover some(none); add 'case some(none):' or 'case _:'",
        2,
        7,
    );
    assert_eq!(
        errors(&format!(
            "{SHAPE}s = Shape.Empty\nmatch s:\n    case Shape.Empty:\n        print(1)\n    case other:\n        print(2)\n"
        )),
        []
    );
}

#[test]
fn a_case_after_one_that_matches_everything_never_runs() {
    let source =
        "x = some(1)\nmatch x:\n    case _:\n        print(1)\n    case none:\n        print(0)\n";
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
        &format!("{SHAPE}s = Shape.Empty\nmatch s:\n    case Empty:\n        print(1)\n"),
        "'Empty' here is a new variable that matches any value; to match the variant, write \
         'Shape.Empty'",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s:\n    case some(x):\n        print(x)\n    case _:\n        print(0)\n"
        ),
        "this pattern matches option[unknown], but the value is Shape",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s:\n    case Shape.Rect(w):\n        print(w)\n    case _:\n        print(0)\n"
        ),
        "'Shape.Rect' holds 2 values, but this pattern has 1",
        7,
        10,
    );
    assert_error(
        &format!(
            "{SHAPE}s = Shape.Empty\nmatch s:\n    case Shape.Rect(w, w):\n        print(w)\n    case _:\n        print(0)\n"
        ),
        "'w' is bound more than once in this pattern",
        7,
        24,
    );
    assert_error(
        "match 3:\n    case n:\n        print(n)\n",
        "'match' needs an enum or option value, found int",
        1,
        7,
    );
    assert_error(
        "x = some(1)\nmatch x:\n    case other(y):\n        print(y)\n    case _:\n        print(0)\n",
        "'other' is not a variant; write a variant with its enum, as in 'Shape.Circle(r)', or 'some(x)'",
        3,
        10,
    );
}

#[test]
fn a_name_bound_in_every_case_is_assigned_after_the_match() {
    let both = "x = some(1)\nmatch x:\n    case some(n):\n        y = n\n    case none:\n        y = 0\nprint(y)\n";
    assert_eq!(errors(both), []);
    let one = "x = some(1)\nmatch x:\n    case some(n):\n        y = n\n    case none:\n        print(0)\nprint(y)\n";
    assert_error(one, "'y' might be used before it is assigned", 7, 7);
}

#[test]
fn a_match_whose_cases_all_return_returns() {
    let source = "fn f(x: option[int]) -> int:\n    match x:\n        case some(n):\n            ret n\n        case none:\n            ret 0\n";
    assert_eq!(errors(source), []);
}

const POINT: &str = "struct Point:\n    x: float\n    y: float\n";

#[test]
fn structs_are_made_from_named_values_in_any_order() {
    let source = format!("{POINT}p = Point(y=2.0, x=1.0)\na = p.x\n");
    assert_eq!(type_of_symbol(&source, "p"), "Point");
    assert_eq!(type_of_symbol(&source, "a"), "float");
    let source = format!("{POINT}fn f(p: Point) -> list[Point]:\n    ret [p]\n");
    assert_eq!(type_of_symbol(&source, "f"), "fn(Point) -> list[Point]");
}

#[test]
fn struct_values_are_checked() {
    assert_error(
        &format!("{POINT}p = Point(1.0, 2.0)\n"),
        "a struct's values are named, as in 'Point(x=..., y=...)'",
        4,
        5,
    );
    assert_error(
        &format!("{POINT}p = Point(x=1.0, y=2.0, z=3.0)\n"),
        "'Point' has no field 'z'",
        4,
        25,
    );
    assert_error(
        &format!("{POINT}p = Point(x=1.0, x=2.0, y=0.0)\n"),
        "field 'x' is given more than once",
        4,
        18,
    );
    assert_error(
        &format!("{POINT}p = Point(x=1.0)\n"),
        "missing field 'y' for 'Point'",
        4,
        5,
    );
    assert_error(
        "struct T:\n    a: int\n    b: int\n    c: int\nt = T(b=1)\n",
        "missing fields 'a' and 'c' for 'T'",
        5,
        5,
    );
    assert_error(
        &format!("{POINT}p = Point(x=1, y=2.0)\n"),
        "expected float, found int",
        4,
        13,
    );
    assert_error(
        "fn f(a: int) -> int:\n    ret a\nprint(f(a=1))\n",
        "only a struct is made with named values; pass this value without its name",
        3,
        9,
    );
    assert_error(
        "xs = [1]\nxs.append(x=1)\n",
        "only a struct is made with named values; pass this value without its name",
        2,
        11,
    );
    assert_error(
        &format!("{POINT}q = Point\n"),
        "'Point' is a struct, so a value of it is made with a call, as in 'Point(x=..., y=...)'",
        4,
        5,
    );
}

#[test]
fn struct_definitions_are_checked() {
    assert_error(
        "struct A:\n    x: int\nstruct A:\n    y: int\n",
        "struct 'A' is already defined",
        3,
        8,
    );
    assert_error(
        "struct A:\n    x: int\n    x: str\n",
        "'A' already has a field 'x'",
        3,
        5,
    );
    assert_error(
        "struct list:\n    x: int\n",
        "cannot redefine builtin 'list'",
        1,
        8,
    );
    assert_error(
        "struct A:\n    x: Missing\n",
        "there is no type 'Missing'",
        2,
        8,
    );
    assert_error(
        "if true:\n    struct A:\n        x: int\n",
        "structs can only be defined at the top level",
        2,
        12,
    );
    assert_error(
        "struct A:\n    x: int\nfn A():\n    ret\n",
        "'A' is already defined as a struct",
        3,
        4,
    );
    assert_error(
        "enum A:\n    X\nstruct A:\n    x: int\n",
        "'A' is already defined as an enum",
        3,
        8,
    );
    assert_error(
        "struct A:\n    x: int\nA = 1\n",
        "cannot assign to struct 'A'",
        3,
        1,
    );
}

#[test]
fn a_struct_holds_itself_only_through_a_list_or_an_enum() {
    assert_error(
        "struct Node:\n    value: int\n    next: Node\n",
        "'Node' holds itself through 'next', so a value of it would never end; hold it in a \
         list or an option, as in 'next: option[Node]'",
        3,
        5,
    );
    // through a tuple and another struct, reported once
    assert_error(
        "struct A:\n    b: tuple[int, B]\nstruct B:\n    a: A\n",
        "'A' holds itself through 'b', so a value of it would never end; hold it in a list or \
         an option, as in 'b: option[A]'",
        2,
        5,
    );
    assert_eq!(
        errors(
            "struct Node:\n    kids: list[Node]\n    next: option[Node]\n\
             n = Node(kids=[], next=none)\nm = Node(kids=[n], next=some(n))\nprint(m)\n"
        ),
        []
    );
}

#[test]
fn fields_are_read_and_changed() {
    let p = format!("{POINT}p = Point(x=1.0, y=2.0)\n");
    assert_eq!(
        errors(&format!(
            "{p}p.x = 3.0\np.y += p.x\nps = [p]\nps[0].x = 2.0\nps[0].y *= 2.0\nprint(p, ps)\n"
        )),
        []
    );
    assert_error(&format!("{p}p.x = 3\n"), "expected float, found int", 5, 7);
    assert_error(
        &format!("{p}print(p.z)\n"),
        "'Point' has no field 'z'",
        5,
        9,
    );
    assert_error("n = 1\nprint(n.x)\n", "int has no field 'x'", 2, 9);
    assert_error(
        &format!("{p}print(p.x())\n"),
        "'Point' has no method 'x'; 'x' is a field of 'Point', so read it without parentheses",
        5,
        9,
    );
    assert_error(&format!("{p}p.x += 1\n"), "expected float, found int", 5, 8);
}

#[test]
fn a_change_reaches_a_field_of_a_variable() {
    let bag = "struct Bag:\n    items: list[int]\n    name: str\n";
    assert_eq!(
        errors(&format!(
            "{bag}b = Bag(items=[], name=\"b\")\nb.items.append(1)\nb.items[0] = 2\n\
             bs = [b]\nbs[0].items.append(3)\nbs[0].name = \"c\"\nprint(bs)\n"
        )),
        []
    );
    assert_error(
        &format!("{POINT}fn f() -> Point:\n    ret Point(x=1.0, y=2.0)\nf().x = 1.0\n"),
        "this value is not stored in a variable, so changing it has no effect",
        6,
        1,
    );
    assert_error(
        &format!("{POINT}p = Point(x=1.0, y=2.0)\nfn f():\n    p.x = 1.0\n"),
        "a function cannot change the global 'p'; pass it as an argument and return the changed \
         value",
        6,
        5,
    );
    assert_error(
        &format!("{POINT}t = (Point(x=1.0, y=2.0), 1)\nt[0].x = 2.0\n"),
        "a tuple cannot be changed; build a new one, as in 't = (v, t[1])'",
        5,
        1,
    );
    assert_error(
        &format!("{bag}b = Bag(items=[], name=\"b\")\nb.items[:1].append(1)\n"),
        "a slice is a new list, so changing it has no effect",
        5,
        1,
    );
}

#[test]
fn a_change_to_a_fields_parameter_is_lost_unless_it_is_read() {
    let source = format!("{POINT}fn f(p: Point):\n    p.x = 1.0\n");
    let analysis = analyze_source(&source);
    let warnings: Vec<(String, usize, usize)> = analysis
        .diagnostics
        .into_iter()
        .map(|d| (d.message, d.span.start.line, d.span.start.col))
        .collect();
    assert_eq!(
        warnings,
        [(
            "changes to 'p' are lost when 'f' returns, since parameters are copies; return it and \
             assign the result where 'f' is called"
                .to_string(),
            5,
            5
        )]
    );
    assert_eq!(
        errors(&format!(
            "{POINT}fn f(p: Point) -> Point:\n    p.x = 1.0\n    ret p\n"
        )),
        []
    );
    let mut changed = HashSet::new();
    let module = parse_program("p.x = 1.0\nq.items.append(1)\nr[0].y += 1\n").unwrap();
    let Mod::Module { body } = &module;
    collect_changed(body, &mut changed, &Methods::new());
    let mut changed: Vec<String> = changed.into_iter().collect();
    changed.sort();
    assert_eq!(changed, ["p", "q", "r"]);
}

#[test]
fn structs_are_not_numbers_or_strings() {
    let p = format!("{POINT}p = Point(x=1.0, y=2.0)\n");
    assert_error(
        &format!("{p}print(p + 1)\n"),
        "'+' needs int, float, str, or bytes operands, found Point",
        5,
        7,
    );
    assert_eq!(errors(&format!("{p}print(str(p))\n")), []);
    assert_error(
        &format!("{p}if p:\n    print(1)\n"),
        "a condition must be bool, found Point",
        5,
        4,
    );
}

#[test]
fn a_list_element_of_a_struct_holds_at_most_32_values() {
    let wide = vec!["int"; 33].join(", ");
    assert_error(
        &format!("struct W:\n    t: tuple[{wide}]\nfn f(ws: list[W]):\n    print(ws)\n"),
        "a list element can hold at most 32 values, but W holds 33",
        3,
        10,
    );
}

#[test]
fn a_function_reads_a_field_of_a_global_assigned_after_it() {
    let source = format!("{POINT}fn f() -> float:\n    ret p.x\np = Point(x=1.0, y=2.0)\n");
    assert_eq!(errors(&source), []);
    assert_error(
        &format!("{POINT}fn f() -> str:\n    ret p.x\np = Point(x=1.0, y=2.0)\n"),
        "expected float, found str",
        5,
        11,
    );
    assert_error(
        &format!("{POINT}fn f() -> float:\n    ret p.x\np = 1\n"),
        "int has no field 'x'",
        5,
        11,
    );
}

#[test]
fn map_literals_have_types() {
    assert_eq!(type_of_symbol("m = {\"a\": 1}\n", "m"), "map[str, int]");
    assert_eq!(
        type_of_symbol("m = {}\nm[1] = [true]\n", "m"),
        "map[int, list[bool]]"
    );
    assert_eq!(type_of_symbol("m = {}\n", "m"), "map[int, int]");
    assert_eq!(
        type_of_symbol("m = {1: {\"a\": 1.5}}\nx = m[1][\"a\"]\n", "x"),
        "float"
    );
}

#[test]
fn map_entries_have_one_type() {
    assert_error("m = {1: 2, 3: \"a\"}\n", "expected int, found str", 1, 15);
    assert_error("m = {1: 2, \"a\": 3}\n", "expected int, found str", 1, 12);
}

#[test]
fn map_keys_are_int_str_or_bool() {
    assert_error(
        "m = {1.5: 2}\n",
        "a map key must be int, str, or bool, found float",
        1,
        6,
    );
    assert_error(
        "fn f(m: map[list[int], int]): ret\n",
        "a map key must be int, str, or bool, found list[int]",
        1,
        13,
    );
}

#[test]
fn maps_are_indexed_by_their_key_type() {
    assert_error(
        "m = {\"a\": 1}\nx = m[0]\n",
        "expected str, found int",
        2,
        7,
    );
    assert_error(
        "m = {\"a\": 1}\nm[\"b\"] = \"c\"\n",
        "expected int, found str",
        2,
        10,
    );
    assert_error("m = {1: 1}\nx = m[0:1]\n", "a map cannot be sliced", 2, 7);
}

#[test]
fn map_entries_change_through_any_place() {
    let sources = [
        "m = {1: 1}\nm[1] += 1\n".to_string(),
        "m = {1: [1]}\nm[1].append(2)\nm[1][0] = 3\nm[1] *= 2\n".to_string(),
        "m = {\"a\": {\"b\": 1}}\nm[\"a\"][\"c\"] = 2\nm[\"a\"][\"b\"] -= 1\n".to_string(),
        format!("{POINT}m = {{1: Point(x=1.0, y=2.0)}}\nm[1].x = 3.0\nm[1].y += 1.0\n"),
        "struct Bag:\n    m: map[str, int]\nb = Bag(m={})\nb.m[\"a\"] = 1\nb.m[\"a\"] += 1\n\
         x = b.m.pop(\"a\")\nbs = [b]\nbs[0].m[\"b\"] = 2\nbs[0].m[\"b\"] += 2\n"
            .to_string(),
        "fn f(m: map[str, list[int]]) -> map[str, list[int]]:\n    m[\"a\"].append(1)\n    ret m\n\
         print(f({\"a\": []}))\n"
            .to_string(),
    ];
    for source in &sources {
        assert_eq!(errors(source), [], "{source}");
    }
    assert_error(
        "m = {1: (1, 2)}\nm[1][0] = 3\n",
        "a tuple cannot be changed; build a new one, as in 't = (v, t[1])'",
        2,
        1,
    );
    assert_error(
        "m = {1: 1}\nm[1] += 1.5\n",
        "expected int, found float",
        2,
        9,
    );
}

#[test]
fn a_method_changing_a_map_field_changes_self() {
    let source = concat!(
        "struct Counter:\n",
        "    counts: map[str, int]\n",
        "impl Counter:\n",
        "    fn add(self, k: str):\n",
        "        if self.counts.contains(k):\n",
        "            self.counts[k] += 1\n",
        "        else:\n",
        "            self.counts[k] = 1\n",
        "    fn drop(self, k: str):\n",
        "        x = self.counts.pop(k)\n",
        "c = Counter(counts={})\n",
        "c.add(\"a\")\n",
        "c.drop(\"a\")\n",
        "print(c)\n",
    );
    assert_eq!(errors(source), [], "{source}");
}

#[test]
fn maps_pop_by_key_into_an_option() {
    assert_eq!(
        type_of_symbol("m = {\"a\": (1, 2.5)}\nv = m.pop(\"a\")\n", "v"),
        "option[tuple[int, float]]"
    );
    assert!(errors("m = {1: 1}\nm.pop(1)\n").is_empty());
    assert_error(
        "m = {1: 1}\nv = m.pop(\"a\")\n",
        "expected int, found str",
        2,
        11,
    );
    assert!(errors("m = {1: 1}\nprint(m.pop(1).unwrap_or(0))\n").is_empty());
    assert_error(
        "m = {1: 1}\nprint(m.pop(1), m)\n",
        "'pop' changes 'm' in this statement, so 'm' cannot also be used in it; assign the \
         result of 'pop' to a variable first",
        2,
        17,
    );
    assert_error(
        "g = {1: \"a\"}\nfn f():\n    v = g.pop(1)\n    print(v)\n",
        "a function cannot change the global 'g'; pass it as an argument and return the \
         changed value",
        3,
        9,
    );
}

#[test]
fn maps_have_keys_values_and_items() {
    let source = "m = {\"a\": (1, 2.5)}\nks = m.keys()\nvs = m.values()\nits = m.items()\n";
    assert_eq!(type_of_symbol(source, "ks"), "list[str]");
    assert_eq!(type_of_symbol(source, "vs"), "list[tuple[int, float]]");
    assert_eq!(
        type_of_symbol(source, "its"),
        "list[tuple[str, tuple[int, float]]]"
    );
    assert_error(
        "xs = [1]\nks = xs.keys()\n",
        "expected map[unknown, unknown], found list[int]",
        2,
        6,
    );
    assert_error(
        "m = {1: 1}\nks = m.keys(1)\n",
        "'keys' takes 0 arguments, but 1 was given",
        2,
        6,
    );
}

#[test]
fn maps_iterate_their_keys() {
    assert_eq!(
        type_of_symbol("for k in {\"a\": 1}:\n    print(k)\n", "k"),
        "str"
    );
    assert_error(
        "for k, v in {1: 1}:\n    print(k)\n",
        "a map gives its keys; iterate 'm.items()' for its keys and values",
        1,
        5,
    );
}

#[test]
fn maps_have_len_contains_and_get() {
    assert_eq!(type_of_symbol("m = {1: 1}\nn = m.len()\n", "n"), "int");
    assert_eq!(
        type_of_symbol("m = {1: 1}\nb = m.contains(2)\n", "b"),
        "bool"
    );
    assert_error(
        "m = {1: 1}\nb = m.contains(\"a\")\n",
        "expected int, found str",
        2,
        16,
    );
    assert_eq!(
        type_of_symbol("m = {\"a\": (1, [2.5])}\nv = m.get(\"b\")\n", "v"),
        "option[tuple[int, list[float]]]"
    );
    // the map's type can come from get
    assert_eq!(
        type_of_symbol("m = {}\nv = m.get(true)\nm[false] = \"x\"\n", "m"),
        "map[bool, str]"
    );
    assert_error(
        "m = {1: 1}\nv = m.get(\"a\")\n",
        "expected int, found str",
        2,
        11,
    );
    assert_error(
        "m = {1: 1}\nv = m.get()\n",
        "'get' takes 1 argument, but 0 were given",
        2,
        5,
    );
    assert_error(
        "xs = [1]\nv = xs.get(0)\n",
        "expected map[unknown, unknown], found list[int]",
        2,
        5,
    );
}

#[test]
fn tuples_structs_options_and_enums_compare_by_value() {
    let sources = [
        "t = (1, (\"a\", 2.0))\nprint(t == t, t != (1, (\"b\", 2.0)))\n".to_string(),
        format!("{POINT}p = Point(x=1.0, y=2.0)\nprint(p == p, p != Point(x=0.0, y=2.0))\n"),
        "x = some(1)\nprint(x == none, x != some(2), none == none)\n".to_string(),
        "enum List:\n    Nil\n    Cons(int, List)\nl = List.Cons(1, List.Nil)\n\
         print(l == List.Nil, l == l == l)\n"
            .to_string(),
        "struct Named:\n    name: str\n    shape: option[tuple[int, float]]\n\
         n = Named(name=\"a\", shape=none)\nif n != n:\n    print(n)\n"
            .to_string(),
    ];
    for source in &sources {
        assert_eq!(errors(source), [], "{source}");
    }
}

#[test]
fn compared_values_must_have_one_type() {
    assert_error(
        "print((1, 2) == (1, \"a\"))\n",
        "expected tuple[int, int], found tuple[int, str]",
        1,
        17,
    );
    assert_error(
        "print(some(1) == some(\"a\"))\n",
        "expected option[int], found option[str]",
        1,
        18,
    );
}

#[test]
fn values_holding_lists_or_maps_cannot_be_compared() {
    assert_error(
        "struct Bag:\n    items: list[int]\nb = Bag(items=[])\nprint(b == b)\n",
        "'==' and '!=' cannot compare Bag, since field 'Bag.items' holds list[int]",
        4,
        7,
    );
    assert_error(
        "struct Bag:\n    items: list[int]\nstruct Box:\n    bag: tuple[int, Bag]\n\
         b = Box(bag=(1, Bag(items=[])))\nprint(b != b)\n",
        "'==' and '!=' cannot compare Box, since field 'Bag.items' holds list[int]",
        6,
        7,
    );
    assert_error(
        "t = (1, [2])\nprint(t == t)\n",
        "'==' and '!=' cannot compare tuple[int, list[int]], since it holds list[int]",
        2,
        7,
    );
    assert_error(
        "x = some({1: 2})\nprint(x == none)\n",
        "'==' and '!=' cannot compare option[map[int, int]], since it holds map[int, int]",
        2,
        7,
    );
    assert_error(
        "enum Tree:\n    Leaf\n    Node(int, list[Tree])\nt = Tree.Leaf\nprint(t == t)\n",
        "'==' and '!=' cannot compare Tree, since it holds list[Tree]",
        5,
        7,
    );
}

#[test]
fn maps_cannot_be_compared() {
    assert_error(
        "x = {1: 1} == {1: 1}\n",
        "'==' and '!=' cannot compare map[int, int]",
        1,
        5,
    );
}

#[test]
fn a_function_changing_a_map_owns_it() {
    assert!(
        errors(
            "fn f(m: map[str, int]) -> map[str, int]:\n    m[\"a\"] = 1\n    ret m\nprint(f({}))\n"
        )
        .is_empty()
    );
}

#[test]
fn a_function_can_index_a_global_map_assigned_after_it() {
    assert!(
        errors("fn f(k: str) -> int:\n    ret table[k]\ntable = {\"a\": 1}\nprint(f(\"a\"))\n")
            .is_empty()
    );
    assert_error(
        "fn f(k: str) -> int:\n    ret table[k]\ntable = [1]\nprint(f(\"a\"))\n",
        "expected int, found str",
        2,
        15,
    );
}

const STACK: &str = "struct Stack:\n    items: list[int]\nimpl Stack:\n    fn new() -> Stack:\n        \
                     ret Stack(items=[])\n    fn size(self) -> int:\n        ret self.items.len()\n    \
                     fn push(self, x: int):\n        self.items.append(x)\n    \
                     fn pop_top(self) -> int:\n        x = self.items.pop(-1)\n        ret x\n";

/// Returns each method call of `source` as the function it calls, whether it has a receiver,
/// and whether it changes it, sorted.
fn method_calls(source: &str) -> Vec<(String, bool, bool)> {
    let analysis = analyze_source(source);
    assert_eq!(analysis.diagnostics, [], "{source}");
    let mut calls: Vec<(String, bool, bool)> = analysis
        .methods
        .values()
        .map(|m| (m.function.clone(), m.receiver, m.changes))
        .collect();
    calls.sort();
    calls.dedup();
    calls
}

#[test]
fn methods_and_associated_functions_are_typed() {
    let source = format!("{STACK}s = Stack.new()\ns.push(1)\nn = s.size()\nt = s.pop_top()\n");
    assert_eq!(type_of_symbol(&source, "s"), "Stack");
    assert_eq!(type_of_symbol(&source, "n"), "int");
    assert_eq!(type_of_symbol(&source, "t"), "int");
    assert_eq!(type_of_symbol(&source, "size"), "fn(Stack) -> int");
    assert_eq!(type_of_symbol(&source, "new"), "fn() -> Stack");
    assert_eq!(
        method_calls(&source),
        [
            ("Stack.new".to_string(), false, false),
            ("Stack.pop_top".to_string(), true, true),
            ("Stack.push".to_string(), true, true),
            ("Stack.size".to_string(), true, false),
        ]
    );
    let analysis = analyze_source(&source);
    let method = analysis.symbols.iter().find(|s| s.name == "push").unwrap();
    assert_eq!(method.kind, SymbolKind::Method);
    assert_eq!(method.scope.as_deref(), Some("Stack"));
    let this = analysis
        .symbols
        .iter()
        .find(|s| s.name == "self" && s.scope.as_deref() == Some("Stack.push"))
        .unwrap();
    assert_eq!(this.ty.to_string(), "Stack");
}

#[test]
fn a_method_changes_self_through_its_fields_and_other_methods() {
    let source = format!(
        "{STACK}struct Pair:\n    a: Stack\n    rest: list[Stack]\nimpl Pair:\n    \
         fn both(self):\n        self.a.push(1)\n        self.rest[0].push(2)\n    \
         fn twice(self):\n        self.both()\n        self.both()\n    \
         fn total(self) -> int:\n        ret self.a.size() + self.rest[0].size()\n    \
         fn reset(self):\n        self = Pair(a=Stack.new(), rest=[])\n\
         p = Pair(a=Stack.new(), rest=[])\np.twice()\np.reset()\nprint(p.total())\n"
    );
    let calls = method_calls(&source);
    for (function, changes) in [
        ("Pair.both", true),
        ("Pair.twice", true),
        ("Pair.total", false),
        ("Pair.reset", true),
        ("Stack.size", false),
    ] {
        assert!(
            calls.iter().any(|(f, _, c)| f == function && *c == changes),
            "{function} {calls:?}"
        );
    }
}

#[test]
fn a_struct_method_is_not_the_builtin_of_the_same_name() {
    let source = "struct Bag:\n    n: int\nimpl Bag:\n    fn append(self, x: int) -> int:\n        \
                  ret self.n + x\n    fn len(self) -> int:\n        ret self.n\n\
                  b = Bag(n=1)\nb.append(2)\nprint(b.append(3), b.len())\n";
    assert_eq!(
        method_calls(source),
        [
            ("Bag.append".to_string(), true, false),
            ("Bag.len".to_string(), true, false),
        ]
    );
}

#[test]
fn a_method_that_changes_its_receiver_is_placed_like_pop() {
    let s = format!("{STACK}s = Stack.new()\n");
    assert_error(
        &format!("{s}print(s.push(1))\n"),
        "'push' changes its receiver and returns no value, so it can only be a statement of its \
         own",
        14,
        7,
    );
    assert!(
        errors(&format!(
            "{s}print(s.pop_top())\nif s.pop_top() > 0:\n    print(1)\n"
        ))
        .is_empty()
    );
    assert_error(
        &format!("{s}print(s.pop_top() + s.size())\n"),
        "'pop_top' changes 's' in this statement, so 's' cannot also be used in it; assign the \
         result of 'pop_top' to a variable first",
        14,
        21,
    );
    assert_error(
        &format!("{s}Stack.new().push(1)\n"),
        "this value is not stored in a variable, so changing it has no effect",
        14,
        1,
    );
    assert_error(
        &format!("{s}fn f():\n    s.push(1)\n"),
        "a function cannot change the global 's'; pass it as an argument and return the changed \
         value",
        15,
        5,
    );
    assert_eq!(
        errors(&format!(
            "{s}ss = [s]\nss[0].push(1)\nx = ss[0].pop_top()\n"
        )),
        []
    );
}

#[test]
fn method_calls_are_checked() {
    let s = format!("{STACK}s = Stack.new()\n");
    assert_error(
        &format!("{s}s.nope()\n"),
        "'Stack' has no method 'nope'",
        14,
        3,
    );
    assert_error(
        &format!("{s}s.items()\n"),
        "'Stack' has no method 'items'; 'items' is a field of 'Stack', so read it without \
         parentheses",
        14,
        3,
    );
    assert_error(
        &format!("{s}s.new()\n"),
        "'new' has no 'self', so call it on the struct, as in 'Stack.new()'",
        14,
        3,
    );
    assert_error(
        &format!("{s}print(Stack.size(s))\n"),
        "'size' is a method, so call it on a value, as in 'value.size()'",
        14,
        13,
    );
    assert_error(
        &format!("{s}Stack.nope()\n"),
        "'Stack' has no function 'nope'",
        14,
        7,
    );
    assert_error(
        &format!("{s}s.push()\n"),
        "'push' takes 1 argument, but 0 were given",
        14,
        1,
    );
    assert_error(
        &format!("{s}s.push(\"a\")\n"),
        "expected int, found str",
        14,
        8,
    );
    assert_error(
        &format!("{s}n = s.size\n"),
        "'size' is a method, so it can only be called",
        14,
        7,
    );
    assert_eq!(
        errors(&format!(
            "{STACK}fn f() -> int:\n    ret g.size()\ng = Stack.new()\nprint(f())\n"
        )),
        []
    );
}

#[test]
fn impl_blocks_are_checked() {
    let s = "struct S:\n    n: int\n";
    assert_error(
        &format!(
            "{s}impl S:\n    fn f(self):\n        ret\nimpl S:\n    fn f(self):\n        ret\n"
        ),
        "'S' already has a method 'f'",
        7,
        8,
    );
    assert_error(
        &format!("{s}impl S:\n    fn n(self) -> int:\n        ret 1\n"),
        "'n' is a field of 'S', so a method cannot have its name",
        4,
        8,
    );
    assert_error(
        &format!("{s}impl S:\n    fn f(self: S):\n        ret\n"),
        "'self' needs no type, since it is the 'S' the method is called on",
        4,
        10,
    );
    assert_error(
        &format!("{s}impl S:\n    fn f(x: int, self):\n        ret\n"),
        "'self' can only be the first parameter of a method",
        4,
        18,
    );
    assert_error(
        "fn f(self):\n    ret\n",
        "'self' can only be the first parameter of a method",
        1,
        6,
    );
    assert_error(
        &format!("{s}fn f():\n    impl S:\n        fn g(self):\n            ret\n"),
        "impl blocks can only be at the top level",
        4,
        10,
    );
}

#[test]
fn a_change_through_a_method_is_lost_on_a_parameter_but_not_on_self() {
    let source = format!("{STACK}fn f(s: Stack):\n    s.push(1)\n");
    let analysis = analyze_source(&source);
    let warnings: Vec<(String, usize, usize)> = analysis
        .diagnostics
        .into_iter()
        .map(|d| (d.message, d.span.start.line, d.span.start.col))
        .collect();
    assert_eq!(
        warnings,
        [(
            "changes to 's' are lost when 'f' returns, since parameters are copies; return it and \
             assign the result where 'f' is called"
                .to_string(),
            14,
            5
        )]
    );

    let source = format!("{STACK}s = Stack.new()\ns.push(1)\nn = s.size()\n");
    let analysis = analyze_source(&source);
    let module = parse_program(&source).unwrap();
    let Mod::Module { body } = &module;
    let mut changed = HashSet::new();
    collect_changed(&body[3..], &mut changed, &analysis.methods);
    assert_eq!(changed, HashSet::from(["s".to_string()]));
}

/// A generic function every test below may call.
const FIRST: &str = "fn first[T](xs: list[T]) -> T:\n    ret xs[0]\n";

#[test]
fn a_generic_function_is_called_at_any_type() {
    let source = format!("{FIRST}a = first([1])\nb = first([\"s\"])\nc = first([[1.5]])\n");
    assert_eq!(type_of_symbol(&source, "a"), "int");
    assert_eq!(type_of_symbol(&source, "b"), "str");
    assert_eq!(type_of_symbol(&source, "c"), "list[float]");
    assert_eq!(type_of_symbol(&source, "first"), "fn(list[T]) -> T");
    assert_eq!(type_of_symbol(&source, "xs"), "list[T]");
}

#[test]
fn a_call_records_its_type_arguments() {
    let source = format!("{FIRST}a = first([1])\nb = first([(1, \"s\")])\n");
    let analysis = analyze_source(&source);
    assert_eq!(analysis.diagnostics, []);
    let mut instances: Vec<(usize, String, Vec<String>)> = analysis
        .instances
        .iter()
        .map(|(span, instance)| {
            let args = instance.args.iter().map(Type::to_string).collect();
            (span.start.line, instance.function.clone(), args)
        })
        .collect();
    instances.sort();
    assert_eq!(
        instances,
        [
            (3, "first".to_string(), vec!["int".to_string()]),
            (4, "first".to_string(), vec!["tuple[int, str]".to_string()]),
        ]
    );
}

#[test]
fn arguments_agree_on_a_type_parameter() {
    assert_error(
        "fn pair[T](a: T, b: T) -> list[T]:\n    ret [a, b]\nxs = pair(1, \"s\")\n",
        "expected int, found str",
        3,
        14,
    );
    // a type parameter only in the result is filled in by its use
    let source = "fn empty[T]() -> list[T]:\n    ret []\nxs = empty()\nxs.append(\"a\")\n";
    assert_eq!(type_of_symbol(source, "xs"), "list[str]");
}

#[test]
fn a_type_parameter_is_any_type_inside_its_function() {
    let message = |op: &str| format!("'{op}' needs an int or float operand, found T");
    assert_error("fn f[T](x: T) -> T:\n    ret -x\n", &message("-"), 2, 10);
    assert_error(
        "fn f[T](x: T, y: T) -> T:\n    ret x + y\n",
        "'+' needs int, float, str, or bytes operands, found T",
        2,
        9,
    );
    assert_error(
        "fn f[T](x: T, y: T) -> bool:\n    ret x < y\n",
        "'<' needs int, float, str, bytes, or a type that implements 'ord', found T",
        2,
        9,
    );
    assert_error(
        "fn f[T](x: T, y: T) -> bool:\n    ret x == y\n",
        "'==' and '!=' cannot compare T, since 'T' may be any type; to compare its values, bound \
         it by 'eq', as in '[T: eq]'",
        2,
        9,
    );
    // printed, so written as text too
    assert_eq!(errors("fn f[T](x: T) -> str:\n    ret str(x)\n"), []);
    assert_error(
        "fn f[T](x: T):\n    if x:\n        ret\n",
        "a condition must be bool, found T",
        2,
        8,
    );
    assert_error(
        "fn f[T](x: T) -> int:\n    ret x.len()\n",
        "values of type 'T' have no method 'len', since 'T' may be any type",
        2,
        11,
    );
    assert_error(
        "fn f[T](x: T) -> int:\n    ret x[0]\n",
        "expected list[unknown], found T",
        2,
        9,
    );
    assert_error(
        "fn f[T](x: T):\n    for y in x:\n        print(y)\n",
        "expected list[unknown], found T",
        2,
        14,
    );
    assert_error(
        "fn f[T](x: T) -> int:\n    ret x\n",
        "'f' returns int, but this is T",
        2,
        9,
    );
}

#[test]
fn a_type_parameter_moves_through_values_that_hold_it() {
    let source = "fn wrap[T](x: T) -> option[T]:\n    xs = [x]\n    xs.append(x)\n    \
                  t = (x, xs.len())\n    a, n = t\n    print(a, n)\n    last = xs.pop()\n    ret last\n\
                  y = wrap(\"s\").unwrap()\n";
    assert_eq!(type_of_symbol(source, "y"), "str");
}

#[test]
fn two_type_parameters_differ() {
    assert_error(
        "fn f[A, B](a: A, b: B) -> A:\n    ret b\n",
        "'f' returns A, but this is B",
        2,
        9,
    );
    // so do the type parameters of different functions
    assert_error(
        "fn f[T](a: T) -> T:\n    ret a\nfn g[T](a: T, b: list[T]) -> T:\n    ret f(b)\n",
        "'g' returns T, but this is list[T]",
        4,
        9,
    );
}

#[test]
fn a_map_key_cannot_be_a_type_parameter() {
    assert_error(
        "fn f[K](m: map[K, int]):\n    print(m)\n",
        "a map key must be int, str, or bool, found K",
        1,
        16,
    );
}

#[test]
fn type_parameters_are_declared_once_and_never_shadow_types() {
    assert_error(
        "struct Point:\n    x: int\nfn f[Point](p: Point):\n    print(p)\n",
        "'Point' is already a type",
        3,
        6,
    );
    assert_error(
        "fn f[int](p: int):\n    print(p)\n",
        "'int' is already a type",
        1,
        6,
    );
    assert_error(
        "fn f[T, T](p: T):\n    print(p)\n",
        "'f' already has a type parameter 'T'",
        1,
        9,
    );
    assert_error(
        "fn f[T](p: T[int]):\n    print(p)\n",
        "'T' takes no types",
        1,
        12,
    );
    // a type parameter belongs to its own function only
    assert_error(
        "fn f[T](p: T):\n    print(p)\nfn g(p: T):\n    print(p)\n",
        "there is no type 'T'",
        3,
        9,
    );
}

#[test]
fn a_method_has_no_type_parameters_for_now() {
    assert_error(
        "struct P:\n    x: int\nimpl P:\n    fn f[T](self, t: T):\n        print(t)\n",
        "a method cannot have type parameters for now",
        4,
        10,
    );
}

#[test]
fn a_generic_function_cannot_call_itself_with_ever_larger_types() {
    assert_error(
        "fn f[T](x: T, n: int):\n    if n > 0:\n        f([x], n - 1)\nf(1, 3)\n",
        "this call gives 'f' list[T] for 'T' and leads back to itself, so 'f' would need a copy \
         for ever larger types",
        3,
        9,
    );
    // through another function too
    assert_error(
        "fn f[T](x: T):\n    g((x, 1))\nfn g[U](y: U):\n    f(y)\n",
        "this call gives 'g' tuple[T, int] for 'U' and leads back to itself, so 'g' would need a \
         copy for ever larger types",
        2,
        5,
    );
    // the same types, or smaller ones, are fine
    assert_eq!(
        errors(
            "fn f[T](x: T, n: int):\n    if n > 0:\n        f(x, n - 1)\n\
             fn g[T](xs: list[T]) -> T:\n    ret h(xs[0])\nfn h[T](x: T) -> T:\n    ret x\n\
             f(g([1]), 2)\n"
        ),
        []
    );
}

#[test]
fn a_type_parameter_counts_its_values_once_filled_in() {
    let wide = format!("tuple[{}]", ["int"; 20].join(", "));
    let source = format!(
        "fn pairs[T](x: T) -> int:\n    xs = [(x, x)]\n    ret xs.len()\n\
         fn outer[T](x: T) -> int:\n    ret pairs(x)\n\
         fn make(t: {wide}) -> int:\n    ret outer(t)\nprint(outer(1))\n"
    );
    assert_eq!(
        errors(&source),
        [(
            format!(
                "this call makes 'pairs' store a list element of tuple[{wide}, {wide}], which \
                 holds 40 values, but at most 32 fit"
            ),
            7,
            9
        )]
    );
}

#[test]
fn a_global_cannot_take_a_type_parameter() {
    assert_error(
        "fn f[T]() -> T:\n    ret g[0]\ng = []\n",
        "'g' holds a value of type T, the type parameter of 'f', which means nothing outside 'f'",
        3,
        1,
    );
}

/// A generic struct, enum, and methods every test below may use.
const TYPES: &str = "struct Pair[A, B]:\n    first: A\n    second: B\nimpl Pair:\n    \
                     fn swap(self) -> Pair[B, A]:\n        ret Pair(first=self.second, \
                     second=self.first)\n    fn make(a: A, b: B) -> Pair[A, B]:\n        \
                     ret Pair(first=a, second=b)\nenum Tree[T]:\n    Leaf\n    \
                     Node(Tree[T], T, Tree[T])\n";

#[test]
fn generic_types_take_their_arguments_from_their_values() {
    let source = format!(
        "{TYPES}p = Pair(first=1, second=\"s\")\nq = p.swap()\nr = Pair.make([1.5], none)\n\
         x = q.first\nt = Tree.Node(Tree.Leaf, \"a\", Tree.Leaf)\nmatch t:\n    \
         case Tree.Node(_, v, _):\n        print(v)\n    case Tree.Leaf:\n        print(0)\n\
         r.second = some(2)\n"
    );
    assert_eq!(type_of_symbol(&source, "p"), "Pair[int, str]");
    assert_eq!(type_of_symbol(&source, "q"), "Pair[str, int]");
    assert_eq!(
        type_of_symbol(&source, "r"),
        "Pair[list[float], option[int]]"
    );
    assert_eq!(type_of_symbol(&source, "x"), "str");
    assert_eq!(type_of_symbol(&source, "t"), "Tree[str]");
    assert_eq!(type_of_symbol(&source, "v"), "str");
    assert_eq!(type_of_symbol(&source, "first"), "A");
    assert_eq!(
        type_of_symbol(&source, "swap"),
        "fn(Pair[A, B]) -> Pair[B, A]"
    );
}

#[test]
fn a_generic_type_is_written_with_its_arguments() {
    assert_error(
        &format!("{TYPES}fn f(t: Tree) -> int:\n    ret 0\n"),
        "'Tree' takes one type, as in 'Tree[int]'",
        12,
        9,
    );
    assert_error(
        &format!("{TYPES}fn f(p: Pair[int]) -> int:\n    ret 0\n"),
        "'Pair' takes 2 types, as in 'Pair[int, int]'",
        12,
        9,
    );
    assert_error(
        &format!("{TYPES}fn f(p: Pair[int, str]) -> str:\n    ret p.first\n"),
        "'f' returns str, but this is int",
        13,
        9,
    );
}

#[test]
fn values_of_a_generic_type_agree_on_its_arguments() {
    assert_error(
        "struct Same[T]:\n    a: T\n    b: T\nx = Same(a=1, b=\"s\")\n",
        "expected int, found str",
        4,
        17,
    );
    assert_error(
        &format!("{TYPES}t = Tree.Node(Tree.Leaf, 1, Tree.Node(Tree.Leaf, \"s\", Tree.Leaf))\n"),
        "expected Tree[int], found Tree[str]",
        12,
        29,
    );
}

#[test]
fn a_generic_types_parameters_are_opaque_in_its_methods() {
    assert_error(
        "struct Box[T]:\n    x: T\nimpl Box:\n    fn bump(self) -> T:\n        ret self.x + 1\n",
        "'+' needs int, float, str, or bytes operands, found T",
        5,
        13,
    );
    assert_error(
        "struct Box[T]:\n    x: T\nimpl Box:\n    fn same(self, other: Box[T]) -> bool:\n        \
         ret self == other\n",
        "'==' and '!=' cannot compare Box[T], since field 'Box.x' holds T",
        5,
        13,
    );
}

#[test]
fn equality_looks_through_type_arguments() {
    assert_eq!(
        errors(&format!(
            "{TYPES}print(Pair(first=1, second=\"a\") == Pair(first=2, second=\"b\"))\n"
        )),
        []
    );
    assert_error(
        &format!("{TYPES}print(Pair(first=[1], second=2) == Pair(first=[1], second=2))\n"),
        "'==' and '!=' cannot compare Pair[list[int], int], since field 'Pair.first' holds \
         list[int]",
        12,
        7,
    );
}

#[test]
fn a_generic_type_cannot_hold_itself_with_larger_types() {
    assert_error(
        "enum Bad[T]:\n    Stop\n    More(Bad[list[T]])\n",
        "this gives 'Bad' a type that nests 'T' for 'T', so its values could hold ever larger \
         types",
        3,
        5,
    );
    // nor hold itself through a type argument without a list or an option in between
    assert_error(
        "struct Wrap[T]:\n    x: T\nstruct S:\n    w: Wrap[S]\n",
        "'S' holds itself through 'w', so a value of it would never end; hold it in a list or \
         an option, as in 'w: option[S]'",
        4,
        5,
    );
    // the same types, or a list of itself, are fine
    assert_eq!(
        errors(
            "enum List[T]:\n    Empty\n    Cons(T, List[T])\nstruct Node[T]:\n    kids: list[Node[T]]\n"
        ),
        []
    );
}

#[test]
fn a_generic_enum_counts_its_values_once_filled_in() {
    let wide = format!("tuple[{}]", ["int"; 20].join(", "));
    let source = format!(
        "enum Box[T]:\n    Full(T, T)\nfn make(t: {wide}) -> int:\n    b = Box.Full(t, t)\n    ret 0\n"
    );
    assert_eq!(
        errors(&source),
        [(
            format!(
                "this makes a value of Box[{wide}] variant 'Full', which holds 40 values, but at \
                 most 32 fit"
            ),
            4,
            9
        )]
    );
}

#[test]
fn a_types_parameters_cannot_reuse_type_names() {
    assert_error(
        "struct Point:\n    x: int\nstruct Box[Point]:\n    x: Point\n",
        "'Point' is already a type",
        3,
        12,
    );
    assert_error(
        "enum E[T, T]:\n    X(T)\n",
        "'E' already has a type parameter 'T'",
        1,
        11,
    );
    // a struct's type parameters belong to it and its methods only
    assert_error(
        "struct Box[T]:\n    x: T\nfn f(x: T):\n    print(x)\n",
        "there is no type 'T'",
        3,
        9,
    );
}

#[test]
fn an_option_holds_at_most_32_values() {
    let values: Vec<String> = (0..34).map(|i| i.to_string()).collect();
    let source = format!("t = ({})\no = some(t)\n", values.join(", "));
    let ints = vec!["int"; 34].join(", ");
    assert_error(
        &source,
        &format!(
            "this makes a value of option[tuple[{ints}]] variant 'some', which holds 34 values, \
             but at most 32 fit"
        ),
        2,
        5,
    );
}

#[test]
fn the_preludes_names_are_reserved() {
    assert_error(
        "fn some() -> int:\n    ret 1\n",
        "cannot redefine builtin 'some'",
        1,
        4,
    );
    assert_error("some = 1\n", "cannot assign to builtin 'some'", 1, 1);
    assert_error(
        "enum option:\n    X\n",
        "cannot redefine builtin 'option'",
        1,
        6,
    );
    assert_error(
        "struct some:\n    x: int\n",
        "cannot redefine builtin 'some'",
        1,
        8,
    );
    // the prelude's type parameter is its own, so a program may name a type `T`
    assert_eq!(
        errors("struct T:\n    a: int\nx = some(T(a=1))\nprint(x)\n"),
        []
    );
}

#[test]
fn some_and_none_are_checked_like_any_variant() {
    assert_error(
        "x = some()\n",
        "'some' holds 1 value, but 0 were given",
        1,
        5,
    );
    assert_error(
        "x = some\n",
        "'some' holds 1 value, so make one with a call, as in 'some(...)'",
        1,
        5,
    );
    assert_error(
        "x = none(1)\n",
        "'none' holds no values, so it is written without parentheses, as 'none'",
        1,
        5,
    );
    assert_eq!(type_of_symbol("x = some(1.5)\n", "x"), "option[float]");
}

#[test]
fn some_refers_to_the_preludes_variant() {
    let analysis = analyze_source("x = some(1)\n");
    let reference = analysis
        .reference_at(FileId(0), Pos::new(1, 5))
        .expect("a reference at 'some'");
    let symbol = &analysis.symbols[reference.symbol];
    assert_eq!(symbol.name, "some");
    assert_eq!(symbol.kind, SymbolKind::Variant);
    assert_eq!(symbol.span.file, FileId::PRELUDE);
    // the enum's name was never written, so nothing refers to it there
    let option = analysis
        .symbols
        .iter()
        .position(|s| s.name == "option" && s.kind == SymbolKind::Enum)
        .unwrap();
    assert!(
        analysis
            .references_to(option)
            .all(|r| r.span.file == FileId::PRELUDE)
    );
}

#[test]
fn a_written_option_type_refers_to_the_preludes_enum() {
    let analysis = analyze_source("fn f(o: option[int]) -> int:\n    ret o.unwrap_or(0)\n");
    assert_eq!(analysis.diagnostics, []);
    let reference = analysis
        .reference_at(FileId(0), Pos::new(1, 9))
        .expect("a reference at 'option'");
    let symbol = &analysis.symbols[reference.symbol];
    assert_eq!(
        (symbol.name.as_str(), symbol.kind),
        ("option", SymbolKind::Enum)
    );
}

const SHAPES: &str = "trait Shape:\n    fn area(self) -> float\n    fn twice(self) -> float:\n        \
ret self.area() * 2.0\nstruct Circle:\n    r: float\nstruct Square:\n    s: float\n\
impl Shape for Circle:\n    fn area(self) -> float:\n        ret 3.0 * self.r * self.r\n\
impl Shape for Square:\n    fn area(self) -> float:\n        ret self.s * self.s\n    \
fn twice(self) -> float:\n        ret 4.0\n\
fn total[T: Shape](xs: list[T]) -> float:\n    t = 0.0\n    for x in xs:\n        t += x.area()\n    \
ret t\n";

/// Returns every call through a bound in `source`, which must check cleanly, as the trait's
/// method and the function each implementing type calls.
fn trait_calls(source: &str) -> Vec<(String, Vec<(String, String)>)> {
    let analysis = analyze_source(source);
    assert_eq!(analysis.diagnostics, [], "{source}");
    let mut calls: Vec<(String, Vec<(String, String)>)> = analysis
        .methods
        .values()
        .filter_map(|m| {
            let mut implementations: Vec<(String, String)> =
                m.implementations.clone()?.into_iter().collect();
            implementations.sort();
            Some((m.function.clone(), implementations))
        })
        .collect();
    calls.sort();
    calls.dedup();
    calls
}

#[test]
fn trait_methods_are_called_on_types_that_implement_them() {
    let source = format!(
        "{SHAPES}c = Circle(r=1.0)\na = c.area()\nb = c.twice()\nd = Square(s=2.0).twice()\n\
         all = total([c])\n"
    );
    for name in ["a", "b", "d", "all"] {
        assert_eq!(type_of_symbol(&source, name), "float");
    }
    assert_eq!(
        method_calls(&source),
        [
            ("Circle.area".to_string(), true, false),
            ("Shape.area".to_string(), true, false),
            ("Shape.twice".to_string(), true, false),
            ("Square.twice".to_string(), true, false),
        ]
    );
    let both = vec![
        ("Circle".to_string(), "Circle.area".to_string()),
        ("Square".to_string(), "Square.area".to_string()),
    ];
    assert_eq!(trait_calls(&source), [("Shape.area".to_string(), both)]);
}

#[test]
fn a_type_parameter_has_the_methods_of_its_bounds() {
    assert_error(
        "fn f[T](x: T) -> int:\n    ret x.size()\n",
        "values of type 'T' have no method 'size', since 'T' may be any type",
        2,
        11,
    );
    assert_error(
        &format!("{SHAPES}fn f[T](x: T) -> float:\n    ret x.area()\n"),
        "values of type 'T' have no method 'area', since 'T' may be any type that implements \
         a trait with it, as in '[T: Shape]'",
        23,
        11,
    );
    assert_error(
        &format!(
            "{SHAPES}trait Round:\n    fn area(self) -> float\n\
             fn f[T: Shape + Round](x: T) -> float:\n    ret x.area()\n"
        ),
        "'area' is a method of both 'Shape' and 'Round', which bound 'T', so this call could \
         mean either",
        25,
        11,
    );
    assert_error(
        &format!("{SHAPES}fn f[T: Shape](x: T) -> float:\n    ret x.area(1)\n"),
        "'area' takes 0 arguments, but 1 was given",
        23,
        9,
    );
}

#[test]
fn a_call_of_a_bounded_function_needs_types_that_implement_the_bounds() {
    assert_error(
        &format!("{SHAPES}struct P:\n    x: int\nt = total([P(x=1)])\n"),
        "'P' does not implement 'Shape', which 'total' needs for 'T'",
        24,
        5,
    );
    assert_error(
        &format!("{SHAPES}fn g[T](xs: list[T]) -> float:\n    ret total(xs)\n"),
        "'T' does not implement 'Shape', which 'total' needs for 'T'",
        23,
        9,
    );
    assert_eq!(
        errors(&format!(
            "{SHAPES}fn g[U: Shape](xs: list[U]) -> float:\n    ret total(xs)\n"
        )),
        []
    );
}

#[test]
fn an_impl_block_of_a_trait_defines_its_methods() {
    let base = "trait Shape:\n    fn area(self) -> float\n    fn name(self) -> str\n\
                struct Sq:\n    s: float\n";
    assert_error(
        &format!("{base}impl Shape for Sq:\n    fn area(self) -> float:\n        ret 1.0\n"),
        "'Sq' does not implement 'name' of 'Shape'",
        6,
        16,
    );
    assert_eq!(
        errors(&format!(
            "{base}impl Shape for Sq:\n    fn nothing(self):\n        ret\n"
        )),
        [
            (
                "'Sq' does not implement 'area' and 'name' of 'Shape'".to_string(),
                6,
                16
            ),
            (
                "'nothing' is not a method of 'Shape', so it goes in an 'impl Sq:' block"
                    .to_string(),
                7,
                8
            ),
        ]
    );
    let full = "    fn area(self) -> float:\n        ret 1.0\n    fn name(self) -> str:\n        \
                ret \"s\"\n";
    assert_error(
        &format!("{base}impl Shape for Sq:\n{full}    fn side(self) -> float:\n        ret 1.0\n"),
        "'side' is not a method of 'Shape', so it goes in an 'impl Sq:' block",
        11,
        8,
    );
    assert_error(
        &format!(
            "{base}impl Shape for Sq:\n    fn area(self, k: int) -> float:\n        ret 1.0\n    \
             fn name(self) -> str:\n        ret \"s\"\n"
        ),
        "'area' must take and return what 'Shape' says, as in 'fn area(self) -> float'",
        7,
        8,
    );
    assert_error(
        &format!("{base}impl Shape for Sq:\n{full}impl Shape for Sq:\n{full}"),
        "'Sq' already implements 'Shape'",
        11,
        16,
    );
    assert_error(
        &format!(
            "{base}impl Shape for Sq:\n{full}impl Sq:\n    fn area(self) -> float:\n        ret 2.0\n"
        ),
        "'Sq' already has a method 'area'",
        12,
        8,
    );
    assert_error(
        "struct Sq:\n    s: float\nimpl Shape for Sq:\n    fn area(self) -> float:\n        \
         ret 1.0\n",
        "there is no trait 'Shape'",
        3,
        6,
    );
    assert_error(
        "struct Sq:\n    s: float\nimpl Sq for Sq:\n    fn area(self) -> float:\n        \
         ret 1.0\n",
        "'Sq' is a struct, not a trait",
        3,
        6,
    );
}

#[test]
fn traits_are_named_only_as_bounds() {
    assert_error(
        &format!("{SHAPES}fn f(x: Shape) -> float:\n    ret 1.0\n"),
        "'Shape' is a trait, not a type; to take any type that implements it, write a type \
         parameter, as in 'fn f[T: Shape](x: T)'",
        22,
        9,
    );
    assert_error(
        &format!("{SHAPES}fn f[T: Circle](x: T):\n    ret\n"),
        "'Circle' is a struct, not a trait",
        22,
        9,
    );
    assert_error(
        "fn f[T: Shape](x: T):\n    ret\n",
        "there is no trait 'Shape'",
        1,
        9,
    );
    assert_error(
        &format!("{SHAPES}struct Box[T: Shape]:\n    x: T\n"),
        "only a function's type parameters can have bounds for now",
        22,
        15,
    );
}

#[test]
fn self_names_the_type_of_a_trait_or_impl_block() {
    let source = "trait Ordered:\n    fn before(self, other: Self) -> bool\n\
                  struct V:\n    n: int\nimpl Ordered for V:\n    fn before(self, other: Self) -> \
                  bool:\n        ret self.n < other.n\nimpl V:\n    fn zero() -> Self:\n        \
                  ret V(n=0)\nb = V(n=1).before(V.zero())\n";
    assert_eq!(type_of_symbol(source, "b"), "bool");
    assert_error(
        "fn f(x: Self):\n    ret\n",
        "'Self' is only a type in a trait or an impl block",
        1,
        9,
    );
    assert_error(
        "struct Self:\n    x: int\n",
        "cannot redefine builtin 'Self'",
        1,
        8,
    );
}

#[test]
fn a_default_method_sees_only_the_traits_methods() {
    assert_error(
        "trait N:\n    fn n(self) -> int\n    fn m(self) -> int:\n        ret self.x\n",
        "Self has no field 'x'",
        4,
        18,
    );
    assert_error(
        "trait N:\n    fn n() -> int\n",
        "a trait's method takes 'self' first, as in 'fn n(self)', since it is called on a value",
        2,
        8,
    );
}

const TICKER: &str = "trait Ticker:\n    fn next(self) -> Self\n    fn advance(self):\n        \
self = self.next()\nstruct C:\n    n: int\nimpl Ticker for C:\n    fn next(self) -> Self:\n        \
ret C(n=self.n + 1)\n";

#[test]
fn a_trait_decides_which_methods_change_self() {
    let source = format!("{TICKER}c = C(n=0)\nc.advance()\nd = c.next()\n");
    assert_eq!(
        method_calls(&source),
        [
            ("C.next".to_string(), true, false),
            ("Ticker.advance".to_string(), true, true),
            ("Ticker.next".to_string(), true, false),
        ]
    );
    assert_error(
        &format!("{TICKER}c = C(n=0)\nx = c.advance()\n"),
        "'advance' changes its receiver and returns no value, so it can only be a statement of \
         its own",
        11,
        5,
    );
    // an override changes `self` as the trait's method does
    let overriding = format!(
        "{TICKER}    fn advance(self):\n        self.n += 1\nfn f[T: Ticker](t: T) -> T:\n    \
         t.advance()\n    ret t\nc = f(C(n=0))\nc.advance()\n"
    );
    assert!(method_calls(&overriding).contains(&("C.advance".to_string(), true, true)));
    assert_error(
        &format!(
            "{TICKER}trait Bump:\n    fn bump(self)\nimpl Bump for C:\n    fn bump(self):\n        \
             self.n += 1\n"
        ),
        "'bump' cannot change 'self', since 'bump' of 'Bump' does not",
        13,
        8,
    );
}

const BAG_EQ: &str = "struct Bag:\n    items: list[int]\nimpl eq for Bag:\n    \
fn eq(self, other: Self) -> bool:\n        ret self.items.len() == other.items.len()\n";

#[test]
fn a_struct_that_implements_eq_compares_however_it_says() {
    let source = format!(
        "{BAG_EQ}a = Bag(items=[1])\nb = a == Bag(items=[2])\nc = (a, 1) != (a, 2)\n\
         d = some(a) == none\ne = a.eq(a)\n"
    );
    for name in ["b", "c", "d", "e"] {
        assert_eq!(type_of_symbol(&source, name), "bool");
    }
    assert_eq!(method_calls(&source), [("Bag.eq".to_string(), true, false)]);
    // without the impl block, the list keeps it from comparing
    assert_error(
        "struct Bag:\n    items: list[int]\nb = Bag(items=[]) == Bag(items=[])\n",
        "'==' and '!=' cannot compare Bag, since field 'Bag.items' holds list[int]",
        3,
        5,
    );
    // a struct without its own `eq` has the one that compares part by part
    assert_eq!(
        type_of_symbol("struct P:\n    x: int\nb = P(x=1).eq(P(x=2))\n", "b"),
        "bool"
    );
    assert_error(
        "b = [1].eq([2])\n",
        "'==' and '!=' cannot compare list[int]",
        1,
        5,
    );
}

#[test]
fn bounds_by_eq_and_ord_allow_comparisons() {
    assert_eq!(
        errors(
            "fn same[T: eq](a: T, b: T) -> bool:\n    ret a == b and a.eq(b)\n\
             fn least[T: ord](xs: list[T]) -> T:\n    xs.sort()\n    if xs[0] < xs[1]:\n        \
             ret xs[0]\n    ret xs[1]\nx = same((1, \"a\"), (1, \"b\"))\ny = least([2.5, 1.5])\n"
        ),
        []
    );
    assert_error(
        "fn same[T: ord](a: T, b: T) -> bool:\n    ret a == b\n",
        "'==' and '!=' cannot compare T, since 'T' may be any type; to compare its values, bound \
         it by 'eq', as in '[T: eq]'",
        2,
        9,
    );
    assert_error(
        "fn same[T: eq](a: T, b: T) -> bool:\n    ret a == b\nx = same([1], [2])\n",
        "'list[int]' does not implement 'eq', which 'same' needs for 'T'",
        3,
        5,
    );
}

const VERSION: &str = "struct Version:\n    major: int\n    minor: int\nimpl ord for Version:\n    \
fn cmp(self, other: Self) -> ordering:\n        if self.major != other.major:\n            \
ret self.major.cmp(other.major)\n        ret self.minor.cmp(other.minor)\n";

#[test]
fn a_struct_that_implements_ord_is_ordered() {
    let source = format!(
        "{VERSION}a = Version(major=1, minor=2)\nb = a < Version(major=1, minor=3)\n\
         vs = [a]\nvs.sort()\no = a.cmp(a)\nn = 1.5.cmp(2.5)\n"
    );
    assert_eq!(type_of_symbol(&source, "b"), "bool");
    assert_eq!(type_of_symbol(&source, "o"), "ordering");
    assert_eq!(type_of_symbol(&source, "n"), "ordering");
    assert_error(
        "struct P:\n    x: int\nb = P(x=1) < P(x=2)\n",
        "'<' needs int, float, str, bytes, or a type that implements 'ord', found P",
        3,
        5,
    );
    assert_error(
        "b = true.cmp(false)\n",
        "'cmp' needs int, float, str, bytes, or a type that implements 'ord', found bool",
        1,
        5,
    );
}

#[test]
fn a_struct_may_implement_display() {
    let source = "struct P:\n    x: int\nimpl display for P:\n    fn display(self) -> str:\n        \
                  ret \"P\" + str(self.x)\na = P(x=1).display()\nb = str([P(x=1)])\n";
    assert_eq!(type_of_symbol(source, "a"), "str");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_error(
        "struct P:\n    x: int\nimpl display for P:\n    fn display(self) -> int:\n        ret 1\n",
        "'display' must take and return what 'display' says, as in 'fn display(self) -> str'",
        4,
        8,
    );
}

#[test]
fn eq_and_cmp_are_only_the_traits_methods() {
    assert_error(
        "struct P:\n    x: int\nimpl P:\n    fn eq(self, other: P) -> bool:\n        ret true\n",
        "'eq' is the method of the trait 'eq', so it goes in an 'impl eq for P:' block",
        4,
        8,
    );
    assert_error(
        "impl ord for int:\n    fn cmp(self, other: int) -> ordering:\n        ret less\n",
        "int already implements 'ord'",
        1,
        14,
    );
    assert_error(
        &format!("{VERSION}impl Version:\n    fn cmp(self) -> int:\n        ret 1\n"),
        "'cmp' is the method of the trait 'ord', so it goes in an 'impl ord for Version:' block",
        10,
        8,
    );
    assert_error(
        "struct P:\n    x: int\nimpl P:\n    fn display(self) -> str:\n        ret \"p\"\n",
        "'display' is the method of the trait 'display', so it goes in an 'impl display for P:' \
         block",
        4,
        8,
    );
    assert_error(
        "impl display for int:\n    fn display(self) -> str:\n        ret \"i\"\n",
        "int already implements 'display'",
        1,
        18,
    );
    assert_error(
        "fn less() -> int:\n    ret 1\n",
        "cannot redefine builtin 'less'",
        1,
        4,
    );
}

#[test]
fn result_is_the_preludes_enum() {
    assert_eq!(
        type_of_symbol("fn f() -> result[int, str]:\n    ret ok(1)\nx = f()\n", "x"),
        "result[int, str]"
    );
    assert_error(
        "fn f() -> result[int, str]:\n    ret 1\n",
        "'f' returns result[int, str], but this is int",
        2,
        9,
    );
    assert_error(
        "enum result:\n    X\n",
        "cannot redefine builtin 'result'",
        1,
        6,
    );
    assert_error("ok = 1\n", "cannot assign to builtin 'ok'", 1, 1);
    assert_error(
        "fn err() -> int:\n    ret 1\n",
        "cannot redefine builtin 'err'",
        1,
        4,
    );
    assert_error(
        "fn f(r: result[int, str]) -> int:\n    match r:\n        case ok(v):\n            ret v\n",
        "'match' does not cover err(_); add 'case err(_):' or 'case _:'",
        2,
        11,
    );
}

#[test]
fn result_methods() {
    let source = "fn f() -> result[int, str]:\n    ret err(\"bad\")\nr = f()\n";
    for (expr, ty) in [
        ("r.is_ok()", "bool"),
        ("r.is_err()", "bool"),
        ("r.unwrap()", "int"),
        ("r.unwrap_or(2)", "int"),
        ("r.unwrap_err()", "str"),
        ("r.ok()", "option[int]"),
        ("r.err()", "option[str]"),
        ("\"1\".parse_int()", "result[int, str]"),
        ("\"1.5\".parse_float()", "result[float, str]"),
    ] {
        assert_eq!(
            type_of_symbol(&format!("{source}x = {expr}\n"), "x"),
            ty,
            "{expr}"
        );
    }
    assert_error(
        &format!("{source}x = r.unwrap_or(\"no\")\n"),
        "expected int, found str",
        4,
        17,
    );
    assert_error(
        "x = some(1).unwrap_err()\n",
        "'unwrap_err' needs a result, found option[int]",
        1,
        5,
    );
    assert_error(
        "x = 1.unwrap()\n",
        "'unwrap' needs an option or a result, found int",
        1,
        5,
    );
    assert_error(
        "x = 1.parse_int()\n",
        "'parse_int' needs a str, found int",
        1,
        5,
    );
}

#[test]
fn values_may_take_the_names_of_the_preludes_enums() {
    assert_eq!(errors("result = 0\nprint(result + 1)\n"), []);
    assert_eq!(
        errors(
            "fn f(n: int) -> result[int, str]:\n    result = ok(n)\n    if n < 0:\n        \
             result = err(\"negative\")\n    ret result\nprint(f(1))\n"
        ),
        []
    );
    assert_eq!(
        errors("fn f(option: int) -> option[int]:\n    ret some(option)\nprint(f(1))\n"),
        []
    );
    assert_eq!(
        errors("fn ordering() -> ordering:\n    ret less\nprint(ordering())\n"),
        []
    );
    assert_eq!(
        errors("result = [ok(1)]\nresult.append(err(\"x\"))\nprint(result.len())\n"),
        []
    );
    assert_error(
        "x = result\n",
        "'result' is an enum, so a value of it is one of its variants, as in 'result.ok'",
        1,
        5,
    );
}

#[test]
fn a_question_mark_unwraps_a_result_or_returns_its_error() {
    let parse =
        "fn twice(s: str) -> result[int, str]:\n    n = s.parse_int()?\n    ret ok(n * 2)\n";
    assert_eq!(errors(&format!("{parse}print(twice(\"2\"))\n")), []);
    assert_eq!(type_of_symbol(parse, "n"), "int");
    // options work the same way, in a function that returns an option
    let option = "fn at(s: str) -> option[int]:\n    n = s.find(\"a\")? + 1\n    ret some(n)\n";
    assert_eq!(errors(&format!("{option}print(at(\"ba\"))\n")), []);
    // generic functions too
    assert_eq!(
        errors(
            "fn both[T, E](a: result[T, E], b: result[T, E]) -> result[tuple[T, T], E]:\n    \
             ret ok((a?, b?))\nprint(both(ok(1), err(\"x\")))\n"
        ),
        []
    );
    // and methods
    assert_eq!(
        errors(
            "struct Acc:\n    total: int\nimpl Acc:\n    fn add(self, s: str) -> result[int, str]:\n        \
             self.total += s.parse_int()?\n        ret ok(self.total)\n\
             a = Acc(total=0)\nprint(a.add(\"3\"))\n"
        ),
        []
    );
}

#[test]
fn a_question_mark_needs_a_function_that_returns_a_result_or_an_option() {
    assert_error(
        "x = \"1\".parse_int()?\n",
        "'?' can only return from a function that returns a result or an option",
        1,
        5,
    );
    assert_error(
        "fn f(s: str) -> int:\n    ret s.parse_int()?\n",
        "'?' returns from 'f', which returns int, not a result or an option",
        2,
        9,
    );
    assert_error(
        "fn f(s: str):\n    print(s.parse_int()?)\n",
        "'?' returns from 'f', which returns no value, not a result or an option",
        2,
        11,
    );
}

#[test]
fn a_question_mark_checks_what_it_unwraps() {
    assert_error(
        "fn f(n: int) -> result[int, str]:\n    ret ok(n?)\n",
        "'?' needs a result, found int",
        2,
        12,
    );
    assert_error(
        "fn f(r: result[int, int]) -> result[int, str]:\n    ret ok(r?)\n",
        "'f' returns result[int, str], so '?' needs an error of type str, found int",
        2,
        12,
    );
    assert_error(
        "fn f(o: option[int]) -> result[int, str]:\n    ret ok(o?)\n",
        "'?' on an option needs 'f' to return an option, but it returns result[int, str]",
        2,
        12,
    );
    assert_error(
        "fn f(r: result[int, str]) -> option[int]:\n    ret some(r?)\n",
        "'?' on a result needs 'f' to return a result, but it returns option[int]; add '.ok()' \
         before '?' to drop the error",
        2,
        14,
    );
    assert_error(
        "fn f(n: int) -> option[int]:\n    ret some(n?)\n",
        "'?' needs an option, found int",
        2,
        14,
    );
}

#[test]
fn in_tests_a_map_for_a_key_a_list_for_an_element_and_a_string_for_text() {
    assert_eq!(type_of_symbol("m = {1: \"a\"}\nb = 2 in m\n", "b"), "bool");
    assert_eq!(
        type_of_symbol("xs = [1.5]\nb = 2.5 not in xs\n", "b"),
        "bool"
    );
    assert_eq!(type_of_symbol("b = \"el\" in \"hello\"\n", "b"), "bool");
    // the container's type can come from what it is asked for
    assert_eq!(
        type_of_symbol("xs = []\nb = \"a\" in xs\n", "xs"),
        "list[str]"
    );
    assert_eq!(
        type_of_symbol("m = {}\nb = true in m\nm[false] = 1\n", "m"),
        "map[bool, int]"
    );
    assert!(errors("ps = [(1, \"a\")]\nif (1, \"a\") in ps:\n    print(1)\n").is_empty());
}

#[test]
fn contains_tests_lists_and_strings_too() {
    assert_eq!(type_of_symbol("b = [1, 2].contains(2)\n", "b"), "bool");
    assert_eq!(
        type_of_symbol("b = \"abc\".contains(\"bc\")\n", "b"),
        "bool"
    );
    assert_error(
        "b = [1, 2].contains(\"a\")\n",
        "expected int, found str",
        1,
        21,
    );
    assert_error(
        "b = \"abc\".contains(1)\n",
        "expected str, found int",
        1,
        20,
    );
    assert_error(
        "b = [1].contains()\n",
        "'contains' takes 1 argument, but 0 were given",
        1,
        5,
    );
}

#[test]
fn in_needs_a_value_of_the_containers_kind() {
    assert_error("b = 1 in \"abc\"\n", "expected str, found int", 1, 5);
    assert_error("b = \"a\" in [1, 2]\n", "expected int, found str", 1, 5);
    assert_error("b = \"a\" in {1: 2}\n", "expected int, found str", 1, 5);
    assert_error(
        "b = 1 in 5\n",
        "'in' needs a str, bytes, list, or map, found int",
        1,
        10,
    );
    assert_error(
        "struct P:\n    x: int\nb = 1 in P(x=1)\n",
        "'in' needs a str, bytes, list, or map, found P",
        3,
        10,
    );
}

#[test]
fn in_compares_elements_as_equality_does() {
    assert_error(
        "b = [1] in [[1]]\n",
        "'in' and 'not in' cannot compare list[int]",
        1,
        5,
    );
    assert_error(
        "b = [[1]].contains([1])\n",
        "'contains' cannot compare list[int]",
        1,
        5,
    );
    assert_error(
        "fn has[T](xs: list[T], x: T) -> bool:\n    ret x in xs\n",
        "'in' and 'not in' cannot compare T, since 'T' may be any type; to compare its values, \
         bound it by 'eq', as in '[T: eq]'",
        2,
        9,
    );
    assert!(errors("fn has[T: eq](xs: list[T], x: T) -> bool:\n    ret x in xs\n").is_empty());
    // a type parameter is no container
    assert_error(
        "fn has[T](c: T) -> bool:\n    ret 1 in c\n",
        "'in' needs a str, bytes, list, or map, found T",
        2,
        14,
    );
}

#[test]
fn in_never_chains_with_other_comparisons() {
    assert_error(
        "xs = [1]\nb = 1 in xs == true\n",
        "'in' cannot be chained with other comparisons; add parentheses, as in '(x in xs) == b'",
        2,
        5,
    );
    assert_error(
        "xs = [1]\nb = 0 < 1 not in xs\n",
        "'not in' cannot be chained with other comparisons; add parentheses, as in \
         '(x not in xs) == b'",
        2,
        5,
    );
    assert!(errors("xs = [true]\nb = (1 in [1]) in xs\n").is_empty());
}

#[test]
fn a_function_can_test_a_global_assigned_after_it() {
    for global in ["{\"a\": 1}", "[\"a\"]", "\"abc\""] {
        let source = format!(
            "fn f(k: str) -> bool:\n    ret k in table\ntable = {global}\nprint(f(\"a\"))\n"
        );
        assert!(errors(&source).is_empty(), "{source}");
    }
    assert_error(
        "fn f(k: str) -> bool:\n    ret k in table\ntable = [1]\nprint(f(\"a\"))\n",
        "expected int, found str",
        2,
        9,
    );
    assert_error(
        "fn f(k: str) -> bool:\n    ret k in table\ntable = 3\nprint(f(\"a\"))\n",
        "'in' needs a str, bytes, list, or map, found int",
        2,
        14,
    );
}

#[test]
fn sockets_and_listeners_have_types() {
    let cases = [
        ("net.connect(\"127.0.0.1\", 80)", "result[socket, str]"),
        ("net.listen(\"127.0.0.1\", 0)", "result[listener, str]"),
        (
            "net.listen(\"\", 0).unwrap().accept()",
            "result[socket, str]",
        ),
        ("net.listen(\"\", 0).unwrap().port()", "int"),
        (
            "net.connect(\"\", 1).unwrap().send(\"hi\")",
            "result[int, str]",
        ),
        (
            "net.connect(\"\", 1).unwrap().read_line()",
            "result[option[str], str]",
        ),
        ("net.connect(\"\", 1).unwrap().read()", "result[str, str]"),
        (
            "net.connect(\"\", 1).unwrap().read_all()",
            "result[str, str]",
        ),
    ];
    for (call, ty) in cases {
        assert_eq!(
            linked_type(&format!("use net\nx = {call}\n"), "x"),
            ty,
            "{call}"
        );
    }
}

#[test]
fn signatures_name_sockets_and_listeners() {
    let source = "use net\nfn greet(c: socket) -> result[int, str]:\n    ret c.send(\"hi\")\n\
                  fn serve(l: listener) -> result[socket, str]:\n    ret l.accept()\n\
                  fn stop(c: socket, l: listener):\n    c.close()\n    l.close()\n";
    assert_eq!(linked_errors(source), []);
    assert_eq!(
        linked_errors("fn f(c: socket[int]):\n    c.close()\n"),
        [("'socket' takes no types".to_string(), 1, 9)]
    );
    assert_eq!(
        linked_errors("struct socket:\n    x: int\n"),
        [("cannot redefine builtin 'socket'".to_string(), 1, 8)]
    );
    // a variable may still take the name
    assert_eq!(
        linked_type("use net\nsocket = net.connect(\"\", 1)\n", "socket"),
        "result[socket, str]"
    );
}

#[test]
fn handle_methods_check_their_receivers_and_arguments() {
    assert_eq!(
        linked_errors("use net\nl = net.listen(\"\", 0).unwrap()\nx = l.send(\"a\")\n"),
        [("expected socket, found listener".to_string(), 3, 5)]
    );
    assert_eq!(
        linked_errors("use net\nc = net.connect(\"\", 1).unwrap()\nx = c.send(1)\n"),
        [("expected str, found int".to_string(), 3, 12)]
    );
    assert_eq!(
        linked_errors("x = 1\nx.close()\n"),
        [(
            "'close' needs a socket or listener, found int".to_string(),
            2,
            1
        )]
    );
}

#[test]
fn handles_are_never_compared_or_map_keys() {
    let source = "use net\nc = net.connect(\"\", 1).unwrap()\nd = c\nprint(c == d)\n";
    assert_eq!(
        linked_errors(source),
        [("'==' and '!=' cannot compare socket".to_string(), 4, 7)]
    );
    let source = "use net\nc = net.connect(\"\", 1).unwrap()\nm = {c: 1}\n";
    assert_eq!(
        linked_errors(source),
        [(
            "a map key must be int, str, or bool, found socket".to_string(),
            3,
            6
        )]
    );
}

#[test]
fn bytes_have_types() {
    let cases = [
        ("b\"ab\"", "bytes"),
        ("b\"ab\"[0]", "int"),
        ("b\"ab\"[1:]", "bytes"),
        ("b\"a\" + b\"b\"", "bytes"),
        ("b\"a\" < b\"b\"", "bool"),
        ("b\"a\" == b\"b\"", "bool"),
        ("b\"a\" in b\"ab\"", "bool"),
        ("b\"ab\".len()", "int"),
        ("b\"ab\".contains(b\"a\")", "bool"),
        ("b\"ab\".text()", "result[str, str]"),
        ("\"ab\".bytes()", "bytes"),
        ("bytes([1, 2])", "bytes"),
        ("b\"ab\".cmp(b\"a\")", "ordering"),
        ("[b\"b\", b\"a\"]", "list[bytes]"),
    ];
    for (expr, ty) in cases {
        assert_eq!(type_of_symbol(&format!("x = {expr}\n"), "x"), ty, "{expr}");
    }
    let source = "fn first(data: bytes) -> int:\n    for b in data:\n        ret b\n    ret -1\n\
                  xs = [b\"b\", b\"a\"]\nxs.sort()\nprint(first(xs[0]))\n";
    assert_eq!(errors(source), []);
}

#[test]
fn bytes_are_not_strings_and_never_change() {
    assert_error("x = b\"a\" + \"b\"\n", "expected bytes, found str", 1, 12);
    assert_error(
        "x = b\"a\"\nx[0] = 1\n",
        "bytes cannot be changed; build new bytes instead",
        2,
        1,
    );
    assert_error(
        "m = {b\"a\": 1}\n",
        "a map key must be int, str, or bool, found bytes",
        1,
        6,
    );
    assert_error(
        "x = bytes([\"a\"])\n",
        "expected list[int], found list[str]",
        1,
        11,
    );
    assert_error(
        "x = b\"a\".strip()\n",
        "'strip' needs a str, found bytes",
        1,
        5,
    );
    assert_error("x = 1 in b\"a\"\n", "expected bytes, found int", 1, 5);
}

#[test]
fn byte_reads_and_timeouts_have_types() {
    let cases = [
        ("c.send_bytes(b\"x\")", "result[int, str]"),
        ("c.read_bytes(4)", "result[bytes, str]"),
        ("c.read_exact(4)", "result[bytes, str]"),
        ("c.read_all_bytes()", "result[bytes, str]"),
        ("net.connect_timeout(\"\", 1, 0.5)", "result[socket, str]"),
        ("fs.read_bytes(\"a\")", "result[bytes, str]"),
        ("fs.write_bytes(\"a\", b\"\")", "result[int, str]"),
        ("fs.append_bytes(\"a\", b\"\")", "result[int, str]"),
    ];
    for (call, ty) in cases {
        let source = format!(
            "use net\nuse fs\nc = net.connect(\"\", 1).unwrap()\nc.set_timeout(1.0)\nx = {call}\n"
        );
        assert_eq!(linked_type(&source, "x"), ty, "{call}");
    }
    assert_eq!(
        linked_errors("use net\nc = net.connect(\"\", 1).unwrap()\nc.set_timeout(1)\n"),
        [("expected float, found int".to_string(), 3, 15)]
    );
    assert_eq!(
        linked_errors("use fs\nx = fs.write_bytes(\"a\", \"b\")\n"),
        [("expected bytes, found str".to_string(), 2, 25)]
    );
}
