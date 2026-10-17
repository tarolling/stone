use super::*;
use crate::checker::TypeChecker;
use crate::driver::parse_program;
use crate::span::Pos;

/// Parses, checks, and lowers `source`, returning the IR dump of every function and `main`.
fn dump(source: &str) -> String {
    let program = lower_source(source);
    program
        .functions
        .iter()
        .map(ToString::to_string)
        .collect::<String>()
}

fn lower_source(source: &str) -> Program {
    let module = parse_program(source).unwrap();
    let analysis = TypeChecker::new().analyze(&module);
    assert!(
        analysis.diagnostics.is_empty(),
        "{:?}",
        analysis.diagnostics
    );
    lower(&module, &analysis).unwrap()
}

#[test]
fn assigning_an_expression_to_a_local_writes_it_directly() {
    assert_eq!(
        dump("fn f(x: int) -> int:\n    x = x + 1\n    ret x\n"),
        "fn f(v0):\nb0:\n  v0 = add v0, 1\n  ret v0\nfn main():\nb0:\n  ret 0\n"
    );
}

#[test]
fn an_augmented_local_writes_it_directly() {
    assert_eq!(
        dump("fn f(x: int) -> int:\n    x += 1\n    ret x\n"),
        dump("fn f(x: int) -> int:\n    x = x + 1\n    ret x\n")
    );
}

#[test]
fn an_augmented_element_reads_it_then_changes_it() {
    let source =
        "fn f(xs: list[int], i: int) -> list[int]:\n    xs[i] *= 3\n    ret xs\nprint(f([1], 0))\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = list_load v0, v1\n  v3 = mul v2, 3\n  v0 = list_unique v0\n  \
         list_store v0, v1, v3\n  ret v0\n"
    );
}

#[test]
fn an_augmented_element_evaluates_its_index_once() {
    let source = "fn pick() -> int:\n    ret 0\nfn f(xs: list[int]) -> list[int]:\n    xs[pick()] += 1\n    ret xs\nprint(f([1]))\n";
    let ir = dump(source);
    let f = ir
        .split("fn f(")
        .nth(1)
        .unwrap()
        .split("fn main")
        .next()
        .unwrap();
    assert_eq!(f.matches("pick").count(), 1, "{f}");
}

#[test]
fn an_augmented_list_is_changed_in_place() {
    let ir = first_function(
        "fn f(a: list[int], b: list[int]) -> list[int]:\n    a += b\n    ret a\nf([1], [2])\n",
    );
    assert!(ir.contains("call stone.list_reuse"), "{ir}");
    assert!(!ir.contains("stone.list_new"), "{ir}");
}

#[test]
fn an_augmented_row_is_replaced_by_a_new_one() {
    let ir = first_function(
        "fn f(rows: list[list[int]]) -> list[list[int]]:\n    rows[0] += 1\n    ret rows\nf([[1]])\n",
    );
    // one loop over the row, stored over the old row, which is released
    assert_eq!(ir.matches("list_set").count(), 1, "{ir}");
    assert!(ir.contains("call stone.list_new"), "{ir}");
    assert!(ir.contains("= list_store v0, 0, v"), "{ir}");
    assert!(ir.contains("release_list"), "{ir}");
}

#[test]
fn a_while_test_becomes_a_compare_and_branch() {
    let source =
        "fn f(n: int) -> int:\n    i = 0\n    while i < n:\n        i = i + 1\n    ret i\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0):\n\
         b0:\n  v1 = copy 0\n  jmp b1\n\
         b1:\n  br_lt v1, v0 -> b2, b3\n\
         b2:\n  v1 = add v1, 1\n  jmp b1\n\
         b3:\n  ret v1\n"
    );
    let program = lower_source(source);
    let depths: Vec<u32> = program.functions[0]
        .blocks
        .iter()
        .map(|b| b.loop_depth)
        .collect();
    assert_eq!(depths, [0, 1, 1, 0]);
}

#[test]
fn chained_compares_and_boolean_operators_in_conditions_branch_directly() {
    let source = "fn f(a: int, b: int, c: int) -> int:\n    if a < b < c and not a == c:\n        ret 1\n    ret 0\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1, v2):\n\
         b0:\n  br_lt v0, v1 -> b1, b4\n\
         b1:\n  br_lt v1, v2 -> b2, b4\n\
         b2:\n  br_eq v0, v2 -> b4, b3\n\
         b3:\n  ret 1\n\
         b4:\n  ret 0\n"
    );
}

#[test]
fn a_range_loop_counts_with_hidden_vregs_and_copies_a_variable_bound() {
    let source = "fn f(n: int) -> int:\n    t = 0\n    for i in range(1, n):\n        t = t + i\n    ret t\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0):\n\
         b0:\n  v1 = copy 0\n  v3 = copy 1\n  v4 = copy v0\n  jmp b1\n\
         b1:\n  br_lt v3, v4 -> b2, b3\n\
         b2:\n  v2 = copy v3\n  v3 = add v3, 1\n  v1 = add v1, v2\n  jmp b1\n\
         b3:\n  ret v1\n"
    );
}

#[test]
fn a_constant_range_bound_stays_an_immediate() {
    let dump = dump("fn f():\n    for i in range(10):\n        print(i)\n");
    assert!(dump.contains("br_lt v1, 10 -> "), "{dump}");
}

#[test]
fn top_level_list_loops_hold_their_list_and_main_releases_globals() {
    let source = "xs = [1, 2]\nfor x in xs:\n    if x > 1:\n        break\n    print(x)\nret 5\n";
    assert_eq!(
        dump(source),
        "fn main():\n\
         b0:\n  v0 = call stone.list_new(2, 0)\n  list_init v0[0], 1\n  list_init v0[1], 2\n  \
         v1 = load_global xs\n  store_global xs, v0\n  release_list v1\n  \
         v3 = load_global xs\n  retain v3\n  v2 = copy 0\n  jmp b1\n\
         b1:\n  v4 = len v3\n  br_lt v2, v4 -> b2, b5\n\
         b2:\n  v5 = list_get v3, v2\n  store_global x, v5\n  v2 = add v2, 1\n  \
         v6 = load_global x\n  br_gt v6, 1 -> b3, b4\n\
         b3:\n  jmp b5\n\
         b4:\n  v7 = load_global x\n  call print[int](v7)\n  call stone.print_char(10)\n  jmp b1\n\
         b5:\n  release_list v3\n  v8 = load_global xs\n  release_list v8\n  ret 0\n"
    );
}

#[test]
fn and_as_a_value_short_circuits_through_a_result_vreg() {
    assert_eq!(
        dump("fn f(a: bool, b: bool) -> bool:\n    ret a and b\nprint(f(true, false))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = copy v0\n  br v2 -> b1, b2\n\
         b1:\n  v2 = copy v1\n  jmp b2\n\
         b2:\n  ret v2\n"
    );
}

#[test]
fn a_list_literal_is_built_before_its_variable_lets_go_of_the_old_list() {
    assert_eq!(
        dump("fn f(x: list[int]) -> list[int]:\n    x = [x[0]]\n    ret x\nprint(f([1]))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0):\n\
         b0:\n  v1 = list_load v0, 0\n  v2 = call stone.list_new(1, 0)\n  \
         list_init v2[0], v1\n  release_list v0\n  v0 = copy v2\n  ret v0\n"
    );
}

#[test]
fn assigning_a_string_releases_the_old_value_after_computing_the_new_one() {
    assert_eq!(
        dump("fn f(s: str) -> str:\n    s = s + \"x\"\n    ret s\nprint(f(\"a\"))\n"),
        "fn f(v0):\n\
         b0:\n  v1 = str \"x\"\n  v2 = call stone.str_concat(v0, v1)\n  \
         release_str v0\n  v0 = copy v2\n  ret v0\n\
         fn main():\n\
         b0:\n  v0 = str \"a\"\n  retain v0\n  v1 = call f(v0)\n  call print[str](v1)\n  \
         call stone.print_char(10)\n  release_str v1\n  ret 0\n"
    );
}

#[test]
fn parameters_are_borrowed_unless_changed_and_temporaries_are_released_once_used() {
    let source = "fn f(xs: list[str], i: int):\n    xs[i] = xs[0]\n    g(xs[1] + \"!\")\n\
                  fn g(s: str) -> str:\n    ret s\nf([\"a\"], 0)\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = list_load v0, 0\n  retain v2\n  v0 = list_unique v0\n  \
         v3 = list_store v0, v1, v2\n  release_str v3\n  v4 = list_load v0, 1\n  retain v4\n  \
         v5 = str \"!\"\n  v6 = call stone.str_concat(v4, v5)\n  release_str v4\n  \
         v7 = call g(v6)\n  release_str v6\n  release_str v7\n  release_list v0\n  ret 0\n\
         fn g(v0):\n\
         b0:\n  retain v0\n  ret v0\n"
    );
}

#[test]
fn a_chain_of_string_compares_releases_its_operand_when_it_stops_early() {
    assert_eq!(
        dump("fn f(a: str, b: str, c: str) -> bool:\n    ret a == b + \"\" == c\nprint(f(\"a\", \"b\", \"c\"))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0, v1, v2):\n\
         b0:\n  v4 = str \"\"\n  v5 = call stone.str_concat(v1, v4)\n  \
         v3 = call stone.str_eq(v0, v5)\n  br v3 -> b2, b1\n\
         b1:\n  release_str v5\n  jmp b3\n\
         b2:\n  v3 = call stone.str_eq(v5, v2)\n  release_str v5\n  jmp b3\n\
         b3:\n  ret v3\n"
    );
}

#[test]
fn returning_from_a_list_loop_releases_the_list_and_gives_up_the_local() {
    let source = "fn f(xs: list[str]) -> str:\n    for x in xs:\n        if x == \"a\":\n            ret x\n    \
                  ret \"\"\nprint(f([\"a\"]))\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0):\n\
         b0:\n  v1 = copy 0\n  v3 = copy v0\n  retain v3\n  v2 = copy 0\n  jmp b1\n\
         b1:\n  v4 = len v3\n  br_lt v2, v4 -> b2, b5\n\
         b2:\n  v5 = list_get v3, v2\n  retain v5\n  release_str v1\n  v1 = copy v5\n  \
         v2 = add v2, 1\n  v7 = str \"a\"\n  v6 = call stone.str_eq(v1, v7)\n  br v6 -> b3, b4\n\
         b3:\n  release_list v3\n  ret v1\n\
         b4:\n  jmp b1\n\
         b5:\n  release_list v3\n  v8 = str \"\"\n  retain v8\n  release_str v1\n  ret v8\n"
    );
}

#[test]
fn functions_read_globals_with_a_check_and_main_without() {
    let dump = dump("g = 1\nfn f() -> int:\n    ret g\nprint(g + f())\n");
    assert!(dump.contains("v0 = load_global_checked g"), "{dump}");
    assert!(dump.contains("= load_global g"), "{dump}");
}

#[test]
fn negative_literals_fold_to_immediates() {
    let ints = dump("fn f(x: int) -> int:\n    ret x / -4\nprint(f(8))\n");
    assert!(ints.contains("div v0, -4"), "{ints}");
    let floats = dump("fn f(x: float) -> float:\n    ret x * -1.5\nprint(f(2.0))\n");
    let bits = (-1.5f64).to_bits() as i64;
    assert!(floats.contains(&format!("fmul v0, {bits}")), "{floats}");
}

#[test]
fn methods_lower_to_list_and_string_operations() {
    let source = "fn f(xs: list[int], s: str) -> int:\n    xs.append(s.len())\n    ret xs.len()\nf([], \"a\")\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = call stone.str_len(v1)\n  v0 = list_unique v0\n  \
         call stone.list_append(v0, v2)\n  v3 = len v0\n  release_list v0\n  ret v3\n"
    );
}

#[test]
fn a_changed_parameter_is_owned_and_made_unique_before_each_change() {
    let source = "fn f(xs: list[int], i: int) -> list[int]:\n    xs[i] = 0\n    xs.append(i)\n    ret xs\nprint(f([1], 0))\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v0 = list_unique v0\n  list_store v0, v1, 0\n  \
         v0 = list_unique v0\n  call stone.list_append(v0, v1)\n  ret v0\n"
    );
}

#[test]
fn a_nested_change_makes_every_list_on_the_way_unique() {
    let dump = dump("grid = [[1]]\ngrid[0][0] = 2\n");
    let change = "v3 = load_global grid\n  v4 = list_unique v3\n  store_global grid, v4\n  \
                  v5 = list_load v4, 0\n  v5 = list_unique v5\n  list_store v4, 0, v5\n  \
                  list_store v5, 0, 2\n";
    assert!(dump.contains(change), "{dump}");
}

/// A function that appends to its parameter and returns it, for the tests of moves below.
const ADD: &str = "fn add(xs: list[int], x: int) -> list[int]:\n    xs.append(x)\n    ret xs\n";

#[test]
fn the_caller_gives_an_owned_parameter_its_reference() {
    // a borrowed variable is retained for the call, and a temporary is given up
    let source = format!(
        "{ADD}fn f(ys: list[int]) -> list[list[int]]:\n    ret [add(ys, 1), add([2], 3)]\nprint(f([0]))\n"
    );
    let dump = dump(&source);
    assert!(
        dump.contains(
            "retain v0\n  v1 = call add(v0, 1)\n  v2 = call stone.list_new(1, 0)\n  \
             list_init v2[0], 2\n  v3 = call add(v2, 3)\n  v4 = call stone.list_new(2, 2)\n"
        ),
        "{dump}"
    );
}

#[test]
fn a_local_assigned_the_result_of_a_call_is_moved_into_it() {
    let source = format!(
        "{ADD}fn f(xs: list[int]) -> list[int]:\n    xs = add(xs, 1)\n    ret xs\nprint(f([0]))\n"
    );
    let dump = dump(&source);
    assert!(
        dump.contains("fn f(v0):\nb0:\n  v1 = call add(v0, 1)\n  v0 = copy v1\n  ret v0\n"),
        "{dump}"
    );
}

#[test]
fn only_one_of_two_copies_of_an_argument_moves() {
    let source = "fn two(xs: list[int], ys: list[int]) -> list[int]:\n    ys.append(2)\n    xs.append(ys.len())\n    ret xs\n\
                  fn f(xs: list[int]) -> list[int]:\n    xs = two(xs, xs)\n    ret xs\nprint(f([0]))\n";
    let dump = dump(source);
    assert!(
        dump.contains("fn f(v0):\nb0:\n  retain v0\n  v1 = call two(v0, v0)\n  v0 = copy v1\n"),
        "{dump}"
    );
}

#[test]
fn a_global_moves_only_into_a_function_that_never_reads_it() {
    let moved = dump(&format!("{ADD}xs = [0]\nxs = add(xs, 1)\n"));
    assert!(
        moved.contains("v2 = load_global xs\n  v3 = call add(v2, 1)\n  store_global xs, v3\n"),
        "{moved}"
    );
    // `peek` reads xs, so the call needs a copy
    let source = "fn peek(ys: list[int]) -> list[int]:\n    ys.append(xs[0])\n    ret ys\n\
                  fn add(ys: list[int]) -> list[int]:\n    ys.append(1)\n    ret peek(ys)\nxs = [0]\nxs = add(xs)\n";
    let kept = dump(source);
    assert!(
        kept.contains("v2 = load_global xs\n  retain v2\n  v3 = call add(v2)\n"),
        "{kept}"
    );
}

#[test]
fn a_variable_moves_into_a_call_at_its_last_use_and_is_left_null() {
    let source = format!(
        "{ADD}fn f(n: int) -> list[int]:\n    xs = [n]\n    ys = add(xs, 1)\n    ret ys\nprint(f(0))\n"
    );
    let dump = dump(&source);
    assert!(
        dump.contains("v4 = call add(v1, 1)\n  release_list v2\n  v2 = copy v4\n  v1 = copy 0\n"),
        "{dump}"
    );
    // a later read keeps the variable's own copy
    let source = format!(
        "{ADD}fn f(n: int) -> list[int]:\n    xs = [n]\n    ys = add(xs, 1)\n    print(xs)\n    ret ys\n\
         print(f(0))\n"
    );
    let dump = super::tests::dump(&source);
    assert!(
        dump.contains("retain v1\n  v4 = call add(v1, 1)\n"),
        "{dump}"
    );
}

#[test]
fn a_variable_moves_into_another_variable_or_a_list_at_its_last_use() {
    let dump =
        dump("fn f(n: int) -> list[int]:\n    xs = [n]\n    ys = xs\n    ret ys\nprint(f(0))\n");
    assert!(
        dump.contains(
            "release_list v2\n  v2 = copy v1\n  v1 = copy 0\n  release_list v1\n  ret v2\n"
        ),
        "{dump}"
    );
    let source = "fn f(n: int) -> list[list[int]]:\n    rows = []\n    row = [n]\n    rows.append(row)\n    \
                  ret rows\nprint(f(0))\n";
    let dump = super::tests::dump(source);
    assert!(
        dump.contains("v1 = list_unique v1\n  call stone.list_append(v1, v2)\n  v2 = copy 0\n"),
        "{dump}"
    );
}

#[test]
fn a_global_moves_at_its_last_use_in_main() {
    let dump = dump(&format!("{ADD}xs = [1]\nys = add(xs, 2)\nprint(ys)\n"));
    assert!(
        dump.contains("v2 = load_global xs\n  v3 = call add(v2, 2)\n"),
        "{dump}"
    );
    assert!(dump.contains("store_global xs, 0\n"), "{dump}");
}

#[test]
fn returning_a_call_moves_the_locals_it_is_given() {
    let source = "fn fill(xs: list[int], n: int) -> list[int]:\n    if n == 0:\n        ret xs\n    xs.append(n)\n    \
                  ret fill(xs, n - 1)\nprint(fill([], 3))\n";
    let dump = dump(source);
    assert!(
        dump.contains("v2 = sub v1, 1\n  v3 = call fill(v0, v2)\n  ret v3\n"),
        "{dump}"
    );
}

#[test]
fn modulo_and_power_lower_to_arithmetic() {
    assert_eq!(
        dump("fn f(x: int, n: int) -> int:\n    ret x % n ** 2\nprint(f(7, 2))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0, v1):\nb0:\n  v2 = pow v1, 2\n  v3 = rem v0, v2\n  ret v3\n"
    );
    // a float power's exponent stays an int, not float bits
    let floats = dump(
        "fn f(x: float, n: int) -> float:\n    ret x % 1.5 ** n + x ** -2\nprint(f(2.0, 3))\n",
    );
    let bits = 1.5f64.to_bits() as i64;
    assert!(floats.contains(&format!("fpow {bits}, v1")), "{floats}");
    assert!(floats.contains("frem v0, "), "{floats}");
    assert!(floats.contains("fpow v0, -2"), "{floats}");
}

#[test]
fn io_and_string_builtins_call_the_runtime() {
    let source = "fn f(s: str, n: int, x: float, b: bool) -> list[str]:\n    \
                  ret [input(), input(s), str(n), str(x), str(b), str(s), s.strip()]\n\
                  fn g(s: str) -> int:\n    \
                  ret int(s) + int(float(s)) + args().len() + s.split().len() + s.split(s).len()\n\
                  f(\"a\", 1, 1.5, true)\ng(\"1\")\nprint(eof())\n";
    let text = dump(source);
    for call in [
        "call stone.input(0)",
        "call stone.input(v0)",
        "call stone.str_int(v1)",
        "call stone.str_float(v2)",
        "call stone.str_bool(v3)",
        "call stone.str_strip(v0)",
        "call stone.parse_int(v0)",
        "call stone.parse_float(v0)",
        "call stone.args()",
        "call stone.str_split_ws(v0)",
        "call stone.str_split(v0, v0)",
        "call stone.eof()",
    ] {
        assert!(text.contains(call), "{call} in\n{text}");
    }
}

#[test]
fn string_indexes_check_the_length_then_call_the_runtime() {
    let source = "fn f(s: str, i: int) -> str:\n    ret s[i]\nprint(f(\"a\", 0))\n";
    let text = dump(source);
    let f = text.split("fn main").next().unwrap();
    for line in [
        "v2 = call stone.str_len(v0)",
        "br_lt v3, 0",
        "v3 = add v3, v2",
        // the error reports the index as written and the length
        "fail \"string index out of range (index {}, length {})\", v1, v2",
        "call stone.str_at(v0, v3)",
    ] {
        assert!(f.contains(line), "{line} in\n{f}");
    }
}

#[test]
fn string_slices_and_methods_call_the_runtime() {
    let source = "fn f(s: str, xs: list[str]) -> list[str]:\n    \
                  ret [s[1:], s[::-1], s.join(xs), str(s.find(s).unwrap_or(-1)), str(s.startswith(s))]\n\
                  print(f(\"a\", []))\n";
    let text = dump(source);
    for call in [
        "call stone.str_slice_of(v0, 1, 0, 0, 6)",
        "call stone.str_slice_of(v0, 0, 0, -1, 3)",
        "call stone.str_join(v0, v1)",
        "call stone.str_position(v0, v0)",
        "call stone.str_starts(v0, v0)",
    ] {
        assert!(text.contains(call), "{call} in\n{text}");
    }
    assert!(!text.contains("list_slice"), "{text}");
}

#[test]
fn a_for_loop_over_a_string_steps_by_each_character() {
    let source = "fn f(s: str) -> int:\n    n = 0\n    for c in s:\n        n = n + 1\n    ret n\n\
                  print(f(\"a\"))\n";
    let text = dump(source);
    let f = text.split("fn main").next().unwrap();
    // the length once, then a character and its length each time around
    assert_eq!(f.matches("call stone.str_len").count(), 2, "{f}");
    for line in ["call stone.str_char(", "release_str"] {
        assert!(f.contains(line), "{line} in\n{f}");
    }
}

#[test]
fn endswith_calls_the_runtime() {
    let text = dump("fn f(s: str) -> bool:\n    ret s.endswith(s)\nprint(f(\"a\"))\n");
    assert!(text.contains("call stone.str_ends(v0, v0)"), "{text}");
}

#[test]
fn string_order_compares_the_runtime_result_with_zero() {
    let source = "fn f(a: str, b: str) -> bool:\n    ret a <= b\n\
                  fn g(a: str, b: str) -> int:\n    if a > b:\n        ret 1\n    ret 0\n\
                  print(f(\"a\", \"b\"), g(\"a\", \"b\"))\n";
    let text = dump(source);
    assert!(
        text.contains("v3 = call stone.str_cmp(v0, v1)\n  v2 = le v3, 0"),
        "{text}"
    );
    assert!(
        text.contains("v3 = call stone.str_cmp(v0, v1)\n  v2 = gt v3, 0"),
        "{text}"
    );
}

#[test]
fn a_row_read_for_indexing_is_borrowed_unless_a_call_could_replace_it() {
    let source = "fn f(a: list[list[int]], i: int, k: int) -> int:\n    ret a[i][k] + a[i][g(k)]\nfn g(k: int) -> int:\n    ret k\n\
                  print(f([[1]], 0, 0))\n";
    assert_eq!(
        dump(source).split("fn g").next().unwrap(),
        "fn f(v0, v1, v2):\n\
         b0:\n  v3 = list_load v0, v1\n  v4 = list_load v3, v2\n  \
         v5 = list_load v0, v1\n  retain v5\n  v6 = call g(v2)\n  v7 = list_load v5, v6\n  \
         release_list v5\n  v8 = add v4, v7\n  ret v8\n"
    );
}

/// Links, checks, and lowers `source` like [`dump`], so that it can `use os`.
fn dump_linked(source: &str) -> String {
    let (_, module) = crate::driver::load(
        std::path::Path::new("main.stn"),
        source,
        &crate::project::MapSources::default(),
    );
    let module = module.unwrap().module;
    let analysis = TypeChecker::new().analyze(&module);
    let program = lower(&module, &analysis).unwrap();
    program.functions.iter().map(ToString::to_string).collect()
}

#[test]
fn os_functions_call_the_runtime() {
    let source = "use os\nx = os.env(\"A\").unwrap_or(\"\")\n\
                  print(os.has_env(x), os.platform(), os.arch(), os.hostname(), os.cpu_count())\n\
                  print(os.pid(), os.cwd())\nos.exit(1)\n";
    let text = dump_linked(source);
    for call in [
        "call stone.os_env(",
        "call stone.os_has_env(",
        "call stone.os_platform()",
        "call stone.os_arch()",
        "call stone.os_hostname()",
        "call stone.os_cpu_count()",
        "call stone.os_pid()",
        "call stone.os_cwd()",
        "call stone.os_exit(1)",
    ] {
        assert!(text.contains(call), "{call} in\n{text}");
    }
}

#[test]
fn os_strings_are_owned_by_whoever_uses_them() {
    // a printed result is a temporary, released after the print
    let text = dump_linked("use os\nprint(os.cwd())\n");
    assert!(
        text.contains("v0 = call stone.os_cwd()") && text.contains("release_str v0"),
        "{text}"
    );
}

/// Returns the IR of the first function of `source`, linked first so it can use `math`.
fn first_function(source: &str) -> String {
    dump_linked(source)
        .split("fn main")
        .next()
        .unwrap()
        .to_string()
}

#[test]
fn math_abs_branches_instead_of_calling() {
    let ints = "use math\nfn f(x: int) -> int:\n    ret math.abs(x)\nprint(f(1))\n";
    assert_eq!(
        first_function(ints),
        "fn f(v0):\n\
         b0:\n  br_eq v0, -9223372036854775808 -> b1, b2\n\
         b1:\n  fail \"integer overflow in abs\"\n\
         b2:\n  br_lt v0, 0 -> b3, b4\n\
         b3:\n  v1 = neg v0\n  jmp b5\n\
         b4:\n  v1 = copy v0\n  jmp b5\n\
         b5:\n  ret v1\n"
    );
    let floats = "use math\nfn f(x: float) -> float:\n    ret math.abs(x)\nprint(f(1.5))\n";
    // 0.0 - x rather than a sign flip, so -0.0 becomes 0.0
    assert_eq!(
        first_function(floats),
        "fn f(v0):\n\
         b0:\n  fbr_le v0, 0 -> b1, b2\n\
         b1:\n  v1 = fsub 0, v0\n  jmp b3\n\
         b2:\n  v1 = copy v0\n  jmp b3\n\
         b3:\n  ret v1\n"
    );
}

#[test]
fn math_min_and_max_fold_from_the_left() {
    let several = "use math\nfn f(a: int, b: int, c: int) -> int:\n    ret math.max(a, b, c)\nprint(f(1, 2, 3))\n";
    assert_eq!(
        first_function(several),
        "fn f(v0, v1, v2):\n\
         b0:\n  v3 = copy v0\n  br_gt v1, v3 -> b1, b2\n\
         b1:\n  v3 = copy v1\n  jmp b2\n\
         b2:\n  br_gt v2, v3 -> b3, b4\n\
         b3:\n  v3 = copy v2\n  jmp b4\n\
         b4:\n  ret v3\n"
    );
    let list = "use math\nfn f(xs: list[float]) -> float:\n    ret math.min(xs)\nprint(f([1.5]))\n";
    assert_eq!(
        first_function(list),
        "fn f(v0):\n\
         b0:\n  v2 = len v0\n  br_eq v2, 0 -> b1, b2\n\
         b1:\n  fail \"min of an empty list\"\n\
         b2:\n  v1 = list_get v0, 0\n  v3 = copy 1\n  jmp b3\n\
         b3:\n  br_lt v3, v2 -> b4, b7\n\
         b4:\n  v4 = list_get v0, v3\n  fbr_lt v4, v1 -> b5, b6\n\
         b5:\n  v1 = copy v4\n  jmp b6\n\
         b6:\n  v3 = add v3, 1\n  jmp b3\n\
         b7:\n  ret v1\n"
    );
}

#[test]
fn math_sqrt_and_floor_check_their_argument_first() {
    let sqrt = "use math\nfn f(x: int) -> float:\n    ret math.sqrt(x)\nprint(f(2))\n";
    assert_eq!(
        first_function(sqrt),
        "fn f(v0):\n\
         b0:\n  v1 = int_to_float v0\n  fbr_lt v1, 0 -> b1, b2\n\
         b1:\n  fail \"math domain error\"\n\
         b2:\n  v2 = fsqrt v1\n  ret v2\n"
    );
    let floor = "use math\nfn f(x: float) -> int:\n    ret math.floor(x)\nprint(f(2.5))\n";
    assert_eq!(
        first_function(floor),
        "fn f(v0):\n\
         b0:\n  v1 = float_to_int v0\n  v2 = int_to_float v1\n  fbr_lt v0, v2 -> b1, b2\n\
         b1:\n  v1 = sub v1, 1\n  jmp b2\n\
         b2:\n  ret v1\n"
    );
}

#[test]
fn time_functions_call_the_runtime() {
    let text = dump_linked("use time\nprint(time.now(), time.clock())\ntime.sleep(0.5)\n");
    for call in [
        "call stone.time_now()",
        "call stone.time_clock()",
        "call stone.time_sleep(4602678819172646912)",
    ] {
        assert!(text.contains(call), "{call} in\n{text}");
    }
    // an int length becomes a float first
    let text = dump_linked("use time\ntime.sleep(2)\n");
    assert!(
        text.contains("v0 = int_to_float 2\n  call stone.time_sleep(v0)"),
        "{text}"
    );
}

#[test]
fn randint_checks_its_range_then_draws_below_the_span() {
    assert_eq!(
        first_function(
            "use random\nfn f(a: int, b: int) -> int:\n    ret random.randint(a, b)\nprint(f(1, 6))\n"
        ),
        "fn f(v0, v1):\n\
         b0:\n  br_gt v0, v1 -> b1, b2\n\
         b1:\n  fail \"empty range for randint\"\n\
         b2:\n  v2 = sub v1, v0\n  v2 = add v2, 1\n  v3 = call stone.random_below(v2)\n  \
         v4 = add v0, v3\n  ret v4\n"
    );
}

#[test]
fn choice_retains_a_counted_element() {
    let text = first_function(
        "use random\nfn f(xs: list[str]) -> str:\n    ret random.choice(xs)\nprint(f([\"a\"]))\n",
    );
    assert_eq!(
        text,
        "fn f(v0):\n\
         b0:\n  v1 = len v0\n  br_eq v1, 0 -> b1, b2\n\
         b1:\n  fail \"cannot choose from an empty list\"\n\
         b2:\n  v2 = call stone.random_below(v1)\n  v3 = list_get v0, v2\n  retain v3\n  \
         ret v3\n"
    );
}

#[test]
fn elementwise_arithmetic_is_one_loop_with_no_temporary_lists() {
    let ir = first_function(
        "fn f(a: list[int], b: list[int]) -> list[int]:\n    ret a * 2 + b * b\nf([1], [2])\n",
    );
    // the lengths are checked once, then each element is read, computed, and stored
    assert_eq!(ir.matches("= len ").count(), 2, "{ir}");
    // `b` is read once for both of its uses
    assert_eq!(ir.matches("list_get").count(), 2, "{ir}");
    assert_eq!(ir.matches("list_set").count(), 1, "{ir}");
    assert_eq!(ir.matches("call stone.list_new").count(), 1, "{ir}");
    assert_eq!(ir.matches("call ").count(), 1, "{ir}");
    assert!(
        ir.contains("fail \"lists have different lengths ({} and {})\", v2, v3"),
        "{ir}"
    );
    assert!(!ir.contains("list_load"), "{ir}");
}

#[test]
fn elementwise_float_arithmetic_uses_float_instructions() {
    let ir = first_function(
        "fn f(a: list[float], k: float) -> list[float]:\n    ret -(a * k) + 1.5\nf([1.0], 2.0)\n",
    );
    assert!(ir.contains("fmul"), "{ir}");
    assert!(ir.contains("fneg"), "{ir}");
    assert!(ir.contains("fadd"), "{ir}");
}

#[test]
fn elementwise_arithmetic_reuses_a_list_at_its_last_use() {
    // `a` is assigned, so its old list can hold the result
    let ir = first_function(
        "fn f(a: list[int], b: list[int]) -> list[int]:\n    a = a * 2 + b\n    ret a\nf([1], [2])\n",
    );
    assert!(ir.contains("call stone.list_reuse"), "{ir}");
    assert!(!ir.contains("stone.list_new"), "{ir}");
    // `b` is not read afterward, so it can too, though `a`, read afterward, cannot
    let ir = first_function(
        "fn f(x: list[int]) -> list[list[int]]:\n    a = x * 1\n    b = a * 2\n    c = b + a\n    ret [a, c]\nf([1])\n",
    );
    assert_eq!(ir.matches("call stone.list_reuse").count(), 1, "{ir}");
    assert_eq!(ir.matches("call stone.list_new(v").count(), 2, "{ir}");
    // and a borrowed parameter belongs to the caller
    let ir = first_function("fn f(a: list[int]) -> list[int]:\n    c = a + 1\n    ret c\nf([1])\n");
    assert!(!ir.contains("stone.list_reuse"), "{ir}");
    assert!(ir.contains("call stone.list_new"), "{ir}");
}

#[test]
fn elementwise_arithmetic_packs_pairs_when_it_can() {
    let ir = first_function(
        "fn f(a: list[float], b: list[float]) -> list[float]:\n    ret -(a * 2.5) / b\nf([1.0], [2.0])\n",
    );
    assert!(
        ir.contains("= packed v") && ir.contains("fdiv(fneg(fmul([v"),
        "{ir}"
    );
    assert_eq!(ir.matches("= list_data ").count(), 3, "{ir}");
    // the loop over single elements starts where the packed one stopped
    let done = ir
        .split(" = packed")
        .next()
        .unwrap()
        .rsplit('\n')
        .next()
        .unwrap();
    assert!(ir.contains(&format!("= copy {}", done.trim())), "{ir}");
    // ints add and subtract in pairs
    let ir = first_function("fn f(a: list[int]) -> list[int]:\n    ret a - 1 + a\nf([1])\n");
    assert!(ir.contains("add(sub([v"), "{ir}");
    // but nothing else on ints packs, so the single loop does all of it
    let ir = first_function("fn f(a: list[int]) -> list[int]:\n    ret a * 3\nf([1])\n");
    assert!(!ir.contains("packed"), "{ir}");
    let ir = first_function("fn f(a: list[float]) -> list[float]:\n    ret a ** 2\nf([1.5])\n");
    assert!(!ir.contains("packed"), "{ir}");
}

/// Lowers `source` and returns the span of each instruction of its first function that matches
/// `pick`.
fn spans_of(source: &str, pick: impl Fn(&Inst) -> bool) -> Vec<Span> {
    let program = lower_source(source);
    program.functions[0]
        .blocks
        .iter()
        .flat_map(|block| block.insts.iter().zip(&block.spans))
        .filter(|(inst, _)| pick(inst))
        .map(|(_, span)| *span)
        .collect()
}

#[test]
fn each_instruction_records_the_innermost_expression_it_came_from() {
    let source = "fn f(a: int, b: int) -> int:\n    ret 1 + a / b\n";
    let division = spans_of(source, |inst| {
        matches!(inst, Inst::Binary { op: BinOp::Div, .. })
    });
    assert_eq!(division, [Span::new(Pos::new(2, 13), Pos::new(2, 18))]);
    let addition = spans_of(source, |inst| {
        matches!(inst, Inst::Binary { op: BinOp::Add, .. })
    });
    assert_eq!(addition, [Span::new(Pos::new(2, 9), Pos::new(2, 18))]);
}

#[test]
fn a_change_records_the_subscript_whose_index_it_checks() {
    let source =
        "fn f(grid: list[list[int]]) -> list[list[int]]:\n    grid[1][2] = 3\n    ret grid\n";
    // the row comes from grid[1], and the store goes to grid[1][2]
    let load = spans_of(source, |inst| matches!(inst, Inst::ListLoad { .. }));
    assert_eq!(load, [Span::new(Pos::new(2, 5), Pos::new(2, 12))]);
    let store = spans_of(source, |inst| {
        matches!(
            inst,
            Inst::ListStore {
                value: Operand::Imm(3),
                ..
            }
        )
    });
    assert_eq!(store, [Span::new(Pos::new(2, 5), Pos::new(2, 15))]);
}

#[test]
fn a_tuple_is_returned_and_received_as_its_values() {
    let source = "fn swap(a: int, b: int) -> tuple[int, int]:\n    a, b = (b, a)\n    ret (a, b)\n\
                  fn f() -> int:\n    x, y = swap(1, 2)\n    ret x - y\nprint(f())\n";
    let ir = dump(source);
    // the swap copies both values before assigning either
    assert_eq!(
        ir.split("fn main").next().unwrap(),
        "fn swap(v0, v1):\nb0:\n  v2 = copy v1\n  v3 = copy v0\n  v0 = copy v2\n  v1 = copy v3\n  \
         ret v0, v1\n\
         fn f():\nb0:\n  v2, v3 = call swap(1, 2)\n  v0 = copy v2\n  v1 = copy v3\n  \
         v4 = sub v0, v1\n  ret v4\n"
    );
}

#[test]
fn a_tuple_parameter_is_passed_as_its_values() {
    let source = "fn first(t: tuple[int, tuple[str, float]]) -> str:\n    ret t[1][0]\n\
                  print(first((1, (\"a\", 2.5))))\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn first(v0, v1, v2):\nb0:\n  retain v1\n  ret v1\n"
    );
}

#[test]
fn a_local_returned_twice_gives_one_reference_and_retains_the_other() {
    let source = "fn twice(s: str) -> tuple[str, str]:\n    t = s + \"!\"\n    ret (t, t)\n\
                  print(twice(\"a\"))\n";
    let ir = dump(source);
    let twice = ir.split("fn main").next().unwrap();
    assert_eq!(twice.matches("retain").count(), 1, "{twice}");
    assert!(twice.ends_with("  retain v1\n  ret v1, v1\n"), "{twice}");
}

#[test]
fn a_field_of_a_list_element_reads_only_its_slot() {
    let source = "fn first(pairs: list[tuple[str, int]], i: int) -> int:\n    ret pairs[i][1]\n\
                  print(first([(\"a\", 1)], 0))\n";
    let ir = dump(source);
    let first = ir.split("fn main").next().unwrap();
    // the element's slots start at twice its position, and only the int's is read
    assert_eq!(first.matches("list_get").count(), 1, "{first}");
    assert!(first.contains("mul v"), "{first}");
    assert!(!first.contains("retain"), "{first}");
}

#[test]
fn a_list_of_tuples_records_its_width_and_kinds() {
    let ir = dump("xs = [(1, \"a\", [2])]\nprint(xs)\n");
    // three slots of one element: an int (0), a string (1 << 2), and a list (2 << 4)
    assert!(ir.contains("call stone.list_new_wide(3, 36, 3)"), "{ir}");
}

#[test]
fn a_variant_with_values_is_a_new_value_and_one_without_is_static() {
    let source = "enum Shape:\n    Circle(float)\n    Named(str, int)\n    Empty\n\
                  fn f(s: str) -> Shape:\n    ret Shape.Named(s, 2)\n\
                  fn g() -> Shape:\n    ret Shape.Empty\n";
    let ir = dump(source);
    let f = ir.split("fn g").next().unwrap();
    // the string is counted, as the kinds 0b00_01 say, so the value keeps its own reference
    assert_eq!(
        f,
        "fn f(v0):\nb0:\n  retain v0\n  v1 = call stone.enum_new(1, 1, 2)\n  \
         field_init v1[0], v0\n  field_init v1[1], 2\n  ret v1\n"
    );
    let g = ir
        .split("fn g")
        .nth(1)
        .unwrap()
        .split("fn main")
        .next()
        .unwrap();
    assert_eq!(g, "():\nb0:\n  v0 = variant 2\n  retain v0\n  ret v0\n");
}

#[test]
fn a_match_checks_tags_then_binds_retained_values() {
    let source = "fn f(x: option[str]) -> str:\n    match x:\n        case some(s):\n            \
                  ret s\n        case none:\n            ret \"\"\n";
    let ir = dump(source);
    let f = ir.split("fn main").next().unwrap();
    // the subject is read through a copy, since a case may assign the variable it came from
    assert!(
        f.contains("v2 = copy v0\n  v3 = enum_tag v2\n  br_eq v3, 1 -> b1, b2"),
        "{f}"
    );
    assert!(
        f.contains("b1:\n  v4 = field_get v2, 0\n  retain v4\n  release_str v1\n  v1 = copy v4"),
        "{f}"
    );
}

#[test]
fn pop_without_an_index_is_some_of_the_last_element_or_none() {
    let ir = first_function(
        "fn f(xs: list[str]) -> option[str]:\n    o = xs.pop()\n    ret o\nprint(f([]))\n",
    );
    // the list's reference to the element moves into the option, and the static none is
    // retained, so the option is owned either way
    assert_eq!(
        ir,
        "fn f(v0):\nb0:\n  v1 = copy 0\n  v0 = list_unique v0\n  v2 = len v0\n  \
         br_ne v2, 0 -> b1, b2\nb1:\n  v4 = sub v2, 1\n  v5 = call stone.list_pop(v0, v4)\n  \
         v3 = call stone.enum_new(1, 1, 1)\n  field_init v3[0], v5\n  jmp b3\nb2:\n  \
         v3 = variant 0\n  retain v3\n  jmp b3\nb3:\n  release_enum v1\n  v1 = copy v3\n  \
         release_list v0\n  ret v1\n"
    );
}

#[test]
fn find_get_and_os_env_are_some_of_a_value_or_none() {
    let ir = first_function(
        "fn f(s: str, m: map[int, str]) -> tuple[option[int], option[str]]:\n    \
         ret (s.find(\"a\"), m.get(1))\nprint(f(\"a\", {}))\n",
    );
    assert_eq!(
        ir,
        "fn f(v0, v1):\nb0:\n  v2 = str \"a\"\n  v3 = call stone.str_position(v0, v2)\n  \
         br_ge v3, 0 -> b1, b2\nb1:\n  v4 = call stone.enum_new(1, 0, 1)\n  \
         field_init v4[0], v3\n  jmp b3\nb2:\n  v4 = variant 0\n  retain v4\n  jmp b3\nb3:\n  \
         v5 = call stone.map_find(v1, 1, 0)\n  br_ge v5, 0 -> b4, b5\nb4:\n  v7 = add v5, 1\n  \
         v8 = list_get v1, v7\n  retain v8\n  v6 = call stone.enum_new(1, 1, 1)\n  \
         field_init v6[0], v8\n  jmp b6\nb5:\n  v6 = variant 0\n  retain v6\n  jmp b6\nb6:\n  \
         ret v4, v6\n"
    );
    let ir = dump_linked("use os\nprint(os.env(\"A\"))\n");
    assert!(
        ir.contains(
            "call stone.os_env(v0)\n  br_ne v1, 0 -> b1, b2\nb1:\n  \
                     v2 = call stone.enum_new(1, 1, 1)\n  field_init v2[0], v1"
        ),
        "{ir}"
    );
}

#[test]
fn unwrap_of_none_stops_the_program() {
    let ir =
        first_function("fn f(x: option[int]) -> int:\n    ret x.unwrap()\nprint(f(some(1)))\n");
    assert!(ir.contains("fail \"cannot unwrap none\""), "{ir}");
}

#[test]
fn printing_an_enum_calls_a_function_made_for_its_type() {
    let ir = dump("print(some([1]), [none, some([2])])\n");
    assert!(
        ir.contains("call stone.print_enum.option.list_int(v"),
        "{ir}"
    );
    assert!(
        ir.contains("fn stone.print_enum.option.list_int(v0, v1):"),
        "{ir}"
    );
    assert!(
        ir.contains("fail \"value is nested too deeply to print (more than 10000 levels)\""),
        "{ir}"
    );
}

const POINT: &str = "struct Point:\n    x: int\n    y: int\n";

#[test]
fn a_struct_field_is_a_variable_of_its_own() {
    assert_eq!(
        dump(&format!(
            "{POINT}fn f(p: Point) -> Point:\n    p.y = 7\n    p.x += p.y\n    ret p\n"
        ))
        .split("fn main")
        .next()
        .unwrap(),
        "fn f(v0, v1):\nb0:\n  v1 = copy 7\n  v2 = add v0, v1\n  v0 = copy v2\n  ret v0, v1\n"
    );
}

#[test]
fn a_field_of_a_list_element_is_one_slot_of_it() {
    assert_eq!(
        dump(&format!(
            "{POINT}fn f(ps: list[Point], i: int) -> list[Point]:\n    ps[i].y = 7\n    ret ps\n"
        ))
        .split("fn main")
        .next()
        .unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v0 = list_unique v0\n  v2 = len v0\n  v3 = copy v1\n  br_lt v3, 0 -> b1, b2\n\
         b1:\n  v4 = div v2, 2\n  v3 = add v3, v4\n  br_lt v3, 0 -> b4, b2\n\
         b2:\n  br_lt v3, v2 -> b3, b4\n\
         b3:\n  v5 = mul v3, 2\n  br_lt v5, v2 -> b5, b4\n\
         b4:\n  v6 = div v2, 2\n  fail \"list index out of range (index {}, length {})\", v1, v6\n\
         b5:\n  v7 = add v5, 1\n  list_set v0, v7, 7\n  ret v0\n"
    );
}

#[test]
fn a_field_assigned_the_result_of_a_call_moves_into_it() {
    let bag = "struct Bag:\n    items: list[int]\n    n: int\n";
    let add = "fn add(xs: list[int], x: int) -> list[int]:\n    xs.append(x)\n    ret xs\n";
    let source =
        format!("{bag}{add}fn f(b: Bag) -> Bag:\n    b.items = add(b.items, 1)\n    ret b\n");
    let dumped = dump(&source);
    let f = dumped
        .split("fn f")
        .nth(1)
        .unwrap()
        .split("fn main")
        .next()
        .unwrap();
    assert_eq!(
        f,
        "(v0, v1):\nb0:\n  v2 = call add(v0, 1)\n  v0 = copy v2\n  ret v0, v1\n"
    );
}

#[test]
fn a_method_that_changes_self_returns_it_for_its_caller_to_store() {
    let source = "struct Bag:\n    items: list[int]\n    n: int\nimpl Bag:\n    \
                  fn size(self) -> int:\n        ret self.items.len()\n    \
                  fn add(self, x: int):\n        self.items.append(x)\n    \
                  fn items_of(self) -> list[int]:\n        self.n += 1\n        ret self.items\n\
                  fn f(b: Bag) -> int:\n    b.add(1)\n    ret b.size()\n";
    let dumped = dump(source);
    let function = |name: &str| {
        dumped
            .split(&format!("fn {name}("))
            .nth(1)
            .unwrap()
            .split("\nfn ")
            .next()
            .unwrap()
            .to_string()
    };
    // a method that does not change `self` borrows it
    assert_eq!(
        function("Bag.size"),
        "v0, v1):\nb0:\n  v2 = len v0\n  ret v2"
    );
    assert_eq!(
        function("Bag.add"),
        "v0, v1, v2):\nb0:\n  v0 = list_unique v0\n  call stone.list_append(v0, v2)\n  \
         ret 0, v0, v1"
    );
    // returning a value of `self` retains it, since `self` gives its own reference back
    assert_eq!(
        function("Bag.items_of"),
        "v0, v1):\nb0:\n  v2 = add v1, 1\n  v1 = copy v2\n  retain v0\n  ret v0, v0, v1"
    );
    // the receiver moves into the call and its new values replace it, unreleased
    assert_eq!(
        function("f"),
        "v0, v1):\nb0:\n  v2, v3, v4 = call Bag.add(v0, v1, 1)\n  v0 = copy v3\n  \
         v1 = copy v4\n  v5 = call Bag.size(v0, v1)\n  release_list v0\n  ret v5"
    );
}

#[test]
fn comparing_structs_in_a_condition_branches_on_each_value() {
    let ir = dump(&format!(
        "{POINT}fn f(a: Point, b: Point) -> int:\n    if a == b:\n        ret 1\n    ret 0\n"
    ));
    let f = ir.split("fn main").next().unwrap();
    assert!(f.contains("br_eq v0, v2 -> b"), "{f}");
    assert!(f.contains("br_eq v1, v3 -> b"), "{f}");
    assert!(!f.contains("call"), "{f}");
}

#[test]
fn comparing_tuples_as_a_value_writes_one_or_zero() {
    let ir = dump(
        "fn f(a: tuple[str, float], b: tuple[str, float]) -> bool:\n    ret a != b\n\
         print(f((\"a\", 1.0), (\"a\", 1.0)))\n",
    );
    let f = ir.split("fn main").next().unwrap();
    assert!(f.contains("call stone.str_eq(v0, v2)"), "{f}");
    assert!(f.contains("fbr_eq v1, v3 -> b"), "{f}");
}

#[test]
fn comparing_enums_calls_a_function_made_for_their_type() {
    let ir = dump(
        "fn f(a: option[int], b: option[int]) -> bool:\n    ret a == b\nprint(f(none, none))\n",
    );
    assert!(
        ir.contains("call stone.eq_enum.option.int(v0, v1, 0)"),
        "{ir}"
    );
    let helper = ir.split("fn stone.eq_enum.option.int(v0, v1, v2):").nth(1);
    let helper = helper.unwrap_or_else(|| panic!("{ir}"));
    for line in [
        "fail \"value is nested too deeply to compare (more than 10000 levels)\"",
        "= enum_tag v0",
        "= enum_tag v1",
        "= field_get v0, 0",
        "= field_get v1, 0",
        "ret 1",
        "ret 0",
    ] {
        assert!(helper.contains(line), "{line} in {helper}");
    }
}

/// Returns whether every division in `function` either stops the program, computing the length
/// a failure message prints, or counts a negative index from the end, so an index that is in
/// range never divides.
fn divides_only_off_the_in_range_path(function: &Function) -> bool {
    function.blocks.iter().all(|block| {
        block.insts.iter().all(|inst| {
            let Inst::Binary {
                op: BinOp::Div,
                dst,
                ..
            } = inst
            else {
                return true;
            };
            let adjusts = block.insts.iter().any(|other| {
                matches!(other, Inst::Binary { op: BinOp::Add, rhs, .. } if *rhs == Operand::Reg(*dst))
            });
            adjusts || matches!(block.term, Terminator::Fail(..))
        })
    })
}

#[test]
fn an_index_into_a_list_of_wide_elements_divides_only_when_negative_or_failing() {
    let read = "fn f(ps: list[tuple[int, int, int]], i: int) -> int:\n    ret ps[i][1]\n";
    let change = "struct P:\n    x: float\n    y: float\n    z: float\n\n\
                  fn g(ps: list[P], i: int) -> list[P]:\n    ps[i].y += 1.0\n    ret ps\n";
    for source in [read, change] {
        let program = lower_source(source);
        let function = &program.functions[0];
        assert!(divides_only_off_the_in_range_path(function), "{function}");
    }
}

#[test]
fn a_generic_function_is_lowered_once_for_each_set_of_types() {
    let source = "fn id[T](x: T) -> T:\n    ret x\nfn pair[A, B](a: A, b: B) -> tuple[A, B]:\n    \
                  ret (id(a), b)\nprint(id(1), id(\"s\"), id(2))\nprint(pair(\"s\", (1, 2.5)))\n";
    let program = lower_source(source);
    let mut names: Vec<String> = program
        .functions
        .iter()
        .filter_map(|f| f.name.clone())
        .collect();
    names.sort();
    // never lowered as itself, only as the copies the program calls
    assert_eq!(
        names,
        ["id..int", "id..str", "pair..str..tuple2_int_float",]
    );
    let ir = dump(source);
    // a string is counted, so the copy for it takes and gives references, unlike the int copy
    assert!(ir.contains("fn id..int(v0):\nb0:\n  ret v0\n"), "{ir}");
    assert!(ir.contains("call id..str("), "{ir}");
    // a tuple is passed as its values
    assert!(
        ir.contains("fn pair..str..tuple2_int_float(v0, v1, v2):"),
        "{ir}"
    );
}

#[test]
fn a_copy_of_a_generic_function_moves_its_owned_list() {
    let main = |source: &str| {
        let ir = dump(source);
        let main = ir.split("fn main").nth(1).unwrap();
        main.split("fn push").next().unwrap().to_string()
    };
    let generic = main(
        "fn push[T](xs: list[T], x: T) -> list[T]:\n    xs.append(x)\n    ret xs\n\
         ys = [\"a\"]\nys = push(ys, \"b\")\nprint(ys)\n",
    );
    let plain = main(
        "fn push(xs: list[str], x: str) -> list[str]:\n    xs.append(x)\n    ret xs\n\
         ys = [\"a\"]\nys = push(ys, \"b\")\nprint(ys)\n",
    );
    // ys moves into the copy as it would into a function written for strings
    assert_eq!(generic.replace("push..str", "push"), plain);
}

#[test]
fn a_generic_structs_methods_are_lowered_for_each_set_of_types() {
    let source = "struct Box[T]:\n    x: T\nimpl Box:\n    fn get(self) -> T:\n        ret self.x\n    \
                  fn put(self, x: T):\n        self.x = x\nb = Box(x=1)\nb.put(2)\nc = Box(x=(\"s\", 1.5))\n\
                  print(b.get(), c.get())\n";
    let program = lower_source(source);
    let mut names: Vec<String> = program
        .functions
        .iter()
        .filter_map(|f| f.name.clone())
        .filter(|name| !name.starts_with("stone."))
        .collect();
    names.sort();
    assert_eq!(
        names,
        ["Box.get..int", "Box.get..tuple2_str_float", "Box.put..int"]
    );
    let ir = dump(source);
    // a struct of a tuple is passed as its values
    assert!(ir.contains("fn Box.get..tuple2_str_float(v0, v1):"), "{ir}");
}
