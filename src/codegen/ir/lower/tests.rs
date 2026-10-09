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
         b0:\n  retain v0\n  v1 = list_load v0, 0\n  v2 = call stone.list_new(1, 0)\n  \
         list_init v2[0], v1\n  release_list v0\n  v0 = copy v2\n  ret v0\n"
    );
}

#[test]
fn assigning_a_string_releases_the_old_value_after_computing_the_new_one() {
    assert_eq!(
        dump("def f(s);\n    s = s + \"x\"\n    ret s\nprint(f(\"a\"))\n"),
        "fn f(v0):\n\
         b0:\n  retain v0\n  v1 = str \"x\"\n  v2 = call stone.str_concat(v0, v1)\n  \
         release_str v0\n  v0 = copy v2\n  ret v0\n\
         fn main():\n\
         b0:\n  v0 = str \"a\"\n  v1 = call f(v0)\n  call print[str](v1)\n  \
         call stone.print_char(10)\n  release_str v1\n  ret 0\n"
    );
}

#[test]
fn parameters_are_borrowed_and_temporaries_are_released_once_used() {
    let source = "def f(xs, i);\n    xs[i] = xs[0]\n    g(xs[1] + \"!\")\n\
                  def g(s);\n    ret s\nf([\"a\"], 0)\n";
    assert_eq!(
        dump(source).split("fn main").next().unwrap(),
        "fn f(v0, v1):\n\
         b0:\n  v2 = list_load v0, 0\n  retain v2\n  v3 = list_store v0, v1, v2\n  \
         release_str v3\n  v4 = list_load v0, 1\n  retain v4\n  v5 = str \"!\"\n  \
         v6 = call stone.str_concat(v4, v5)\n  release_str v4\n  v7 = call g(v6)\n  \
         release_str v6\n  release_str v7\n  ret 0\n\
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
         b0:\n  v2 = call stone.str_len(v1)\n  call stone.list_append(v0, v2)\n  \
         v3 = len v0\n  ret v3\n"
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
    let module = module.unwrap();
    let analysis = TypeChecker::new().analyze(&module);
    let program = lower(&module, &analysis.types, &analysis.symbols).unwrap();
    program.functions.iter().map(ToString::to_string).collect()
}

#[test]
fn os_functions_call_the_runtime() {
    let source = "use os\nx = os.env(\"A\")\n\
                  print(os.has_env(x), os.platform(), os.arch(), os.hostname(), os.cpu_count())\n\
                  print(os.pid(), os.cwd(), os.time(), os.clock())\nos.exit(1)\n";
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
        "call stone.os_time()",
        "call stone.os_clock()",
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
