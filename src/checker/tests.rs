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
            if path.extension().is_some_and(|ext| ext == "st") {
                // linked, so a program can use the builtin os module
                let source = std::fs::read_to_string(&path).unwrap();
                assert_eq!(linked_errors(&source), [], "{}", path.display());
            }
        }
    }
}

#[test]
fn literals_and_operators_have_types() {
    let source = "a = 1\nb = \"s\" + \"t\"\nc = a < 2\nd = none\ne = not a\nf = -a * 2\n";
    assert_eq!(type_of_symbol(source, "a"), "int");
    assert_eq!(type_of_symbol(source, "b"), "str");
    assert_eq!(type_of_symbol(source, "c"), "bool");
    assert_eq!(type_of_symbol(source, "d"), "none");
    assert_eq!(type_of_symbol(source, "e"), "bool");
    assert_eq!(type_of_symbol(source, "f"), "int");
}

#[test]
fn function_types_come_from_their_calls_and_returns() {
    let source = "def add(a, b);\n    ret a + b\nx = add(1, 2)\n";
    assert_eq!(type_of_symbol(source, "add"), "def(int, int) -> int");
    assert_eq!(type_of_symbol(source, "x"), "int");
}

#[test]
fn a_function_without_ret_returns_none() {
    let source = "def greet(name);\n    print(\"hi\", name)\ngreet(\"bo\")\n";
    assert_eq!(type_of_symbol(source, "greet"), "def(str) -> none");
}

#[test]
fn recursive_functions_are_typed() {
    let source = "def fact(n);\n    if n;\n        ret n * fact(n - 1)\n    ret 1\n";
    assert_eq!(type_of_symbol(source, "fact"), "def(int) -> int");
}

#[test]
fn functions_can_be_called_before_they_are_defined() {
    let source = "print(twice(2))\ndef twice(n);\n    ret n * 2\n";
    assert_eq!(errors(source), []);
}

#[test]
fn unconstrained_types_default_to_int() {
    let source = "def ignore(a);\n    ret 0\n";
    assert_eq!(type_of_symbol(source, "ignore"), "def(int) -> int");
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
        "x = [1] * [2]\n",
        "'*' needs int or float operands, found list[int]",
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
    let source = "def f(n);\n    ret 1.5 ** n\nx = f(2)\n";
    assert_eq!(type_of_symbol(source, "x"), "float");
    assert_error(
        "def f(n);\n    ret 1.5 ** n\nx = f(2.0)\n",
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
    let source = "def scale(a, b);\n    ret -a * b\nx = scale(1.5, 2.0)\n";
    assert_eq!(
        type_of_symbol(source, "scale"),
        "def(float, float) -> float"
    );
    let source = "def less(a, b);\n    ret a < b\nx = less(1.5, 2.0)\n";
    assert_eq!(type_of_symbol(source, "less"), "def(float, float) -> bool");
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
        "a condition must be int or bool, found float",
        1,
        4,
    );
}

#[test]
fn comparisons_need_matching_operands() {
    assert_error("x = 1 == \"a\"\n", "expected int, found str", 1, 10);
    assert_error("x = 1 < 2.5\n", "expected int, found float", 1, 9);
    assert_error(
        "x = \"a\" < \"b\"\n",
        "'<' needs int or float operands, found str",
        1,
        5,
    );
}

#[test]
fn conditions_must_be_int_or_bool() {
    assert_error(
        "if \"yes\";\n    print(1)\n",
        "a condition must be int or bool, found str",
        1,
        4,
    );
    assert_error(
        "x = not \"s\"\n",
        "a condition must be int or bool, found str",
        1,
        9,
    );
}

#[test]
fn undefined_names_are_errors() {
    assert_error("x = y + 1\n", "undefined name 'y'", 1, 5);
    assert_error("nope(1)\n", "undefined function 'nope'", 1, 1);
}

#[test]
fn calls_need_the_right_number_of_arguments() {
    assert_error(
        "def f(a, b);\n    ret a\nf(1)\n",
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
        "def f(a);\n    ret a\nf(1)\nf(\"s\")\n",
        "expected int, found str",
        4,
        3,
    );
}

#[test]
fn returns_must_agree() {
    assert_error(
        "def f(x);\n    if x;\n        ret 1\n    ret \"a\"\n",
        "'f' returns int, but this is str",
        4,
        9,
    );
}

#[test]
fn a_value_returning_function_must_return_on_every_path() {
    assert_error(
        "def f(x);\n    if x;\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        5,
    );
}

#[test]
fn an_endless_loop_counts_as_returning() {
    let source = "def f(k);\n    m = 1\n    while 1;\n        if m * k == 12;\n            ret m\n        m = m + 1\nprint(f(4))\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_loop_that_can_break_does_not_count_as_returning() {
    assert_error(
        "def f();\n    while true;\n        break\n        ret 1\n",
        "'f' does not return a value on every path",
        1,
        5,
    );
}

#[test]
fn break_and_cont_need_a_loop() {
    assert_error("break\n", "'break' outside a loop", 1, 1);
    assert_error("def f();\n    cont\n", "'cont' outside a loop", 2, 5);
}

#[test]
fn functions_must_be_top_level() {
    assert_error(
        "def outer();\n    def inner();\n        ret 1\n    ret 2\n",
        "functions can only be defined at the top level",
        2,
        9,
    );
}

#[test]
fn functions_are_not_values() {
    assert_error(
        "def f();\n    ret 1\nx = f\n",
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
        "def f();\n    ret 1\nf = 2\n",
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
        "def f();\n    ret 1\ndef f();\n    ret 2\n",
        "function 'f' is already defined",
        3,
        5,
    );
}

#[test]
fn parameters_must_be_unique() {
    assert_error(
        "def f(a, a);\n    ret a\n",
        "duplicate parameter 'a'",
        1,
        10,
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
    let source = "x = 1\ndef f();\n    x = \"local\"\n    ret x\ny = f()\n";
    assert_eq!(errors(source), []);
    assert_eq!(type_of_symbol(source, "y"), "str");
}

#[test]
fn functions_read_globals() {
    let source = "def f();\n    ret n + 1\nn = 3\n";
    assert_eq!(errors(source), []);
}

#[test]
fn a_local_assigned_anywhere_in_the_function_is_local_everywhere() {
    // like Python, `x` is local to `f` because `f` assigns it, so the global is never read, and
    // reading the local before assigning it is an error rather than a read of the global
    let source = "x = \"global\"\ndef f();\n    y = x\n    x = 1\n    ret y\nprint(f())\n";
    assert_error(source, "'x' might be used before it is assigned", 3, 9);
    let analysis = analyze_source(source);
    let y = analysis.symbols.iter().find(|s| s.name == "y").unwrap();
    assert_eq!(y.ty, Type::Int);
}

#[test]
fn every_use_of_a_symbol_is_a_reference() {
    let analysis = analyze_source("def f(a);\n    b = a\n    ret a + b\nf(1)\n");
    let param = analysis.symbols.iter().position(|s| s.name == "a").unwrap();
    let uses: Vec<Pos> = analysis
        .references
        .iter()
        .filter(|r| r.symbol == param)
        .map(|r| r.span.start)
        .collect();
    assert_eq!(uses, [Pos::new(1, 7), Pos::new(2, 9), Pos::new(3, 9)]);
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
        messages.contains(&"a 'for' loop's variable must be a name".to_string()),
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
        "def f();\n    for i in range(3);\n        ret i\n",
        "'f' does not return a value on every path",
        1,
        5,
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
fn indexes_must_be_ints_and_values_lists() {
    assert_error("a = [1]\nx = a[\"0\"]\n", "expected int, found str", 2, 7);
    assert_error(
        "s = \"abc\"\nx = s[0]\n",
        "expected list[unknown], found str",
        2,
        5,
    );
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
    assert_error("def f();\n    print(x)\n    x = 1\n", UNASSIGNED, 2, 11);
}

#[test]
fn assignments_in_one_branch_are_not_enough() {
    assert_error("c = 1\nif c;\n    x = 1\nprint(x)\n", UNASSIGNED, 4, 7);
    assert_error(
        "def f(c);\n    if c;\n        x = 1\n    ret x\n",
        UNASSIGNED,
        4,
        9,
    );
}

#[test]
fn assignments_in_every_branch_are_enough() {
    let source = "c = 1\nif c;\n    x = 1\nelif c + 1;\n    x = 2\nelse;\n    x = 3\nprint(x)\n";
    assert_eq!(errors(source), []);
}

#[test]
fn branches_that_leave_do_not_count() {
    let source = "def f(c);\n    if c;\n        x = 1\n    else;\n        ret 0\n    ret x\n";
    assert_eq!(errors(source), []);
    let source =
        "for i in range(3);\n    if i;\n        x = i\n    else;\n        cont\n    print(x)\n";
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
    let source = "def f();\n    ret total\ntotal = 1\nprint(f())\n";
    assert_eq!(errors(source), []);
}

#[test]
fn each_unassigned_variable_is_reported_once() {
    assert_error("print(x, x)\nx = 1\n", UNASSIGNED, 1, 7);
}

#[test]
fn references_are_found_by_position() {
    let analysis = analyze_source("def add(a, b);\n    ret a + b\nx = add(1, 2)\n");
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
    assert_eq!(uses, [Pos::new(1, 5), Pos::new(3, 5)]);
}

#[test]
fn function_spans_cover_their_bodies() {
    let analysis = analyze_source("def f(a);\n    b = a\n    ret b\nx = 1\n");
    assert_eq!(
        analysis.functions.get("f"),
        Some(&Span::new(Pos::new(1, 1), Pos::new(3, 10)))
    );
}

#[test]
fn visible_symbols_depend_on_position() {
    let source = "g = 1\ndef f(a);\n    b = a\n    ret b\ndef h();\n    ret 2\n";
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
    let source = "def f();\n    ret [\"a\"]\ngrid = [[1]]\ngrid[0].append(2)\nn = f().len()\n";
    assert_eq!(type_of_symbol(source, "n"), "int");
}

#[test]
fn append_infers_a_parameter_is_a_list() {
    let source = "def push(xs, x);\n    xs.append(x + 1)\n    ret xs\npush([], 2)\n";
    assert_eq!(
        type_of_symbol(source, "push"),
        "def(list[int], int) -> list[int]"
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
        type_of_symbol("def append(a);\n    ret a\n", "append"),
        "def(int) -> int"
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
    assert_error("def f();\n    ret [1]\nf().append(2)\n", message, 3, 1);
    assert_error("def f();\n    ret [[1]]\nf()[0][0] = 2\n", message, 3, 1);
    assert_error("[1].append(2)\n", message, 1, 1);
    assert_eq!(
        errors("grid = [[1]]\ngrid[0][0] = 2\ngrid[0].append(3)\n"),
        []
    );
}

#[test]
fn functions_cannot_change_globals() {
    let source = "names = []\ndef add(name);\n    names.append(name)\nadd(\"a\")\n";
    assert_error(
        source,
        "a function cannot change the global 'names'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    let source = "grid = [[0]]\ndef clear(i);\n    grid[i][0] = 0\nclear(0)\n";
    assert_error(
        source,
        "a function cannot change the global 'grid'; pass it as an argument and return the \
         changed value",
        3,
        5,
    );
    // reading a global is fine, and so is changing a local that shadows one
    let source = "names = [1]\ndef f();\n    names = [2]\n    names.append(3)\n    ret names\n\
                  def g();\n    ret names[0]\nprint(f(), g())\n";
    assert_eq!(errors(source), []);
}

#[test]
fn changing_a_parameter_that_is_never_used_again_is_a_warning() {
    let source = "def push(xs, x);\n    xs.append(x)\nys = [1]\npush(ys, 2)\n";
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
    let source = "def clear(grid, i);\n    if i > 0;\n        grid[i][0] = 0\nclear([[1]], 0)\n";
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
    for body in [
        "    xs.append(x)\n    ret xs\n",
        "    xs[0] = x\n    print(xs)\n",
        "    for i in range(3);\n        xs.append(x)\n    ret xs.len()\n",
    ] {
        let source = format!("def push(xs, x);\n{body}push([1], 2)\n");
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
        "def f();\n    use util\n    ret 1\n",
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
        ("os.exit(0)", "none"),
        ("os.time()", "float"),
        ("os.clock()", "float"),
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
