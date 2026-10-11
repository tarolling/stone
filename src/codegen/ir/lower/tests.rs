use super::*;
use crate::checker::TypeChecker;
use crate::driver::parse;

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
    let module = parse(source).unwrap();
    let analysis = TypeChecker::new().analyze(&module);
    assert!(
        analysis.diagnostics.is_empty(),
        "{:?}",
        analysis.diagnostics
    );
    lower(&module, &analysis.types, &analysis.symbols).unwrap()
}

#[test]
fn assigning_an_expression_to_a_local_writes_it_directly() {
    assert_eq!(
        dump("def f(x);\n    x = x + 1\n    ret x\n"),
        "fn f(v0):\nb0:\n  v0 = add v0, 1\n  ret v0\nfn main():\nb0:\n  ret 0\n"
    );
}

#[test]
fn a_while_test_becomes_a_compare_and_branch() {
    let source = "def f(n);\n    i = 0\n    while i < n;\n        i = i + 1\n    ret i\n";
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
    let source = "def f(a, b, c);\n    if a < b < c and not a == c;\n        ret 1\n    ret 0\n";
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
    let source = "def f(n);\n    t = 0\n    for i in range(1, n);\n        t = t + i\n    ret t\n";
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
    let dump = dump("def f();\n    for i in range(10);\n        print(i)\n");
    assert!(dump.contains("br_lt v1, 10 -> "), "{dump}");
}

#[test]
fn top_level_list_loops_hold_their_list_and_main_releases_globals() {
    let source = "xs = [1, 2]\nfor x in xs;\n    if x > 1;\n        break\n    print(x)\nret 5\n";
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
        dump("def f(a, b);\n    ret a and b\nprint(f(1, 2))\n")
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
        dump("def f(x);\n    x = [x[0]]\n    ret x\nprint(f([1]))\n")
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
        dump("def f(s);\n    s = s + \"x\"\n    ret s\nprint(f(\"a\"))\n"),
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
    let source = "def f(xs, i);\n    xs[i] = xs[0]\n    g(xs[1] + \"!\")\n\
                  def g(s);\n    ret s\nf([\"a\"], 0)\n";
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
        dump("def f(a, b, c);\n    ret a == b + \"\" == c\nprint(f(\"a\", \"b\", \"c\"))\n")
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
    let source = "def f(xs);\n    for x in xs;\n        if x == \"a\";\n            ret x\n    \
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
    let dump = dump("g = 1\ndef f();\n    ret g\nprint(g + f())\n");
    assert!(dump.contains("v0 = load_global_checked g"), "{dump}");
    assert!(dump.contains("= load_global g"), "{dump}");
}

#[test]
fn negative_literals_fold_to_immediates() {
    let ints = dump("def f(x);\n    ret x / -4\nprint(f(8))\n");
    assert!(ints.contains("div v0, -4"), "{ints}");
    let floats = dump("def f(x);\n    ret x * -1.5\nprint(f(2.0))\n");
    let bits = (-1.5f64).to_bits() as i64;
    assert!(floats.contains(&format!("fmul v0, {bits}")), "{floats}");
}

#[test]
fn methods_lower_to_list_and_string_operations() {
    let source = "def f(xs, s);\n    xs.append(s.len())\n    ret xs.len()\nf([], \"a\")\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = call stone.str_len(v1)\n  v0 = list_unique v0\n  \
         call stone.list_append(v0, v2)\n  v3 = len v0\n  release_list v0\n  ret v3\n"
    );
}

#[test]
fn a_changed_parameter_is_owned_and_made_unique_before_each_change() {
    let source = "def f(xs, i);\n    xs[i] = 0\n    xs.append(i)\n    ret xs\nprint(f([1], 0))\n";
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
const ADD: &str = "def add(xs, x);\n    xs.append(x)\n    ret xs\n";

#[test]
fn the_caller_gives_an_owned_parameter_its_reference() {
    // a borrowed variable is retained for the call, and a temporary is given up
    let source = format!("{ADD}def f(ys);\n    ret [add(ys, 1), add([2], 3)]\nprint(f([0]))\n");
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
    let source = format!("{ADD}def f(xs);\n    xs = add(xs, 1)\n    ret xs\nprint(f([0]))\n");
    let dump = dump(&source);
    assert!(
        dump.contains("fn f(v0):\nb0:\n  v1 = call add(v0, 1)\n  v0 = copy v1\n  ret v0\n"),
        "{dump}"
    );
}

#[test]
fn only_one_of_two_copies_of_an_argument_moves() {
    let source = "def two(xs, ys);\n    ys.append(2)\n    xs.append(ys.len())\n    ret xs\n\
                  def f(xs);\n    xs = two(xs, xs)\n    ret xs\nprint(f([0]))\n";
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
    let source = "def peek(ys);\n    ys.append(xs[0])\n    ret ys\n\
                  def add(ys);\n    ys.append(1)\n    ret peek(ys)\nxs = [0]\nxs = add(xs)\n";
    let kept = dump(source);
    assert!(
        kept.contains("v2 = load_global xs\n  retain v2\n  v3 = call add(v2)\n"),
        "{kept}"
    );
}

#[test]
fn a_variable_moves_into_a_call_at_its_last_use_and_is_left_null() {
    let source =
        format!("{ADD}def f(n);\n    xs = [n]\n    ys = add(xs, 1)\n    ret ys\nprint(f(0))\n");
    let dump = dump(&source);
    assert!(
        dump.contains("v4 = call add(v1, 1)\n  release_list v2\n  v2 = copy v4\n  v1 = copy 0\n"),
        "{dump}"
    );
    // a later read keeps the variable's own copy
    let source = format!(
        "{ADD}def f(n);\n    xs = [n]\n    ys = add(xs, 1)\n    print(xs)\n    ret ys\n\
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
    let dump = dump("def f(n);\n    xs = [n]\n    ys = xs\n    ret ys\nprint(f(0))\n");
    assert!(
        dump.contains(
            "release_list v2\n  v2 = copy v1\n  v1 = copy 0\n  release_list v1\n  ret v2\n"
        ),
        "{dump}"
    );
    let source = "def f(n);\n    rows = []\n    row = [n]\n    rows.append(row)\n    \
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
    let source = "def fill(xs, n);\n    if n == 0;\n        ret xs\n    xs.append(n)\n    \
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
        dump("def f(x, n);\n    ret x % n ** 2\nprint(f(7, 2))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0, v1):\nb0:\n  v2 = pow v1, 2\n  v3 = rem v0, v2\n  ret v3\n"
    );
    // a float power's exponent stays an int, not float bits
    let floats = dump("def f(x, n);\n    ret x % 1.5 ** n + x ** -2\nprint(f(2.0, 3))\n");
    let bits = 1.5f64.to_bits() as i64;
    assert!(floats.contains(&format!("fpow {bits}, v1")), "{floats}");
    assert!(floats.contains("frem v0, "), "{floats}");
    assert!(floats.contains("fpow v0, -2"), "{floats}");
}

#[test]
fn io_and_string_builtins_call_the_runtime() {
    let source = "def f(s, n, x, b);\n    \
                  ret [input(), input(s), str(n), str(x), str(b), str(s), s.strip()]\n\
                  def g(s);\n    \
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
fn a_row_read_for_indexing_is_borrowed_unless_a_call_could_replace_it() {
    let source = "def f(a, i, k);\n    ret a[i][k] + a[i][g(k)]\ndef g(k);\n    ret k\n\
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
        std::path::Path::new("main.st"),
        source,
        &crate::project::MapSources::default(),
    );
    let (module, _) = module.unwrap();
    let analysis = TypeChecker::new().analyze(&module);
    let program = lower(&module, &analysis.types, &analysis.symbols).unwrap();
    program.functions.iter().map(ToString::to_string).collect()
}

#[test]
fn os_functions_call_the_runtime() {
    let source = "use os\nx = os.env(\"A\")\n\
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
    let ints = "use math\ndef f(x);\n    ret math.abs(x)\nprint(f(1))\n";
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
    let floats = "use math\ndef f(x);\n    ret math.abs(x)\nprint(f(1.5))\n";
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
    let several = "use math\ndef f(a, b, c);\n    ret math.max(a, b, c)\nprint(f(1, 2, 3))\n";
    assert_eq!(
        first_function(several),
        "fn f(v0, v1, v2):\n\
         b0:\n  v3 = copy v0\n  br_gt v1, v3 -> b1, b2\n\
         b1:\n  v3 = copy v1\n  jmp b2\n\
         b2:\n  br_gt v2, v3 -> b3, b4\n\
         b3:\n  v3 = copy v2\n  jmp b4\n\
         b4:\n  ret v3\n"
    );
    let list = "use math\ndef f(xs);\n    ret math.min(xs)\nprint(f([1.5]))\n";
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
    let sqrt = "use math\ndef f(x);\n    ret math.sqrt(x)\nprint(f(2))\n";
    assert_eq!(
        first_function(sqrt),
        "fn f(v0):\n\
         b0:\n  v1 = int_to_float v0\n  fbr_lt v1, 0 -> b1, b2\n\
         b1:\n  fail \"math domain error\"\n\
         b2:\n  v2 = fsqrt v1\n  ret v2\n"
    );
    let floor = "use math\ndef f(x);\n    ret math.floor(x)\nprint(f(2.5))\n";
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
        first_function("use random\ndef f(a, b);\n    ret random.randint(a, b)\nprint(f(1, 6))\n"),
        "fn f(v0, v1):\n\
         b0:\n  br_gt v0, v1 -> b1, b2\n\
         b1:\n  fail \"empty range for randint\"\n\
         b2:\n  v2 = sub v1, v0\n  v2 = add v2, 1\n  v3 = call stone.random_below(v2)\n  \
         v4 = add v0, v3\n  ret v4\n"
    );
}

#[test]
fn choice_retains_a_counted_element() {
    let text =
        first_function("use random\ndef f(xs);\n    ret random.choice(xs)\nprint(f([\"a\"]))\n");
    assert_eq!(
        text,
        "fn f(v0):\n\
         b0:\n  v1 = len v0\n  br_eq v1, 0 -> b1, b2\n\
         b1:\n  fail \"cannot choose from an empty list\"\n\
         b2:\n  v2 = call stone.random_below(v1)\n  v3 = list_get v0, v2\n  retain v3\n  \
         ret v3\n"
    );
}
