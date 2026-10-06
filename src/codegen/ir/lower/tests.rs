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
    lower(&module, &analysis.types).unwrap()
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
fn top_level_list_loops_use_globals_and_break_jumps_to_the_exit() {
    let source = "xs = [1, 2]\nfor x in xs;\n    if x > 1;\n        break\n    print(x)\nret 5\n";
    assert_eq!(
        dump(source),
        "fn main():\n\
         b0:\n  v0 = call stone.list_new(2)\n  list_init v0[0], 1\n  list_init v0[1], 2\n  \
         store_global xs, v0\n  v2 = load_global xs\n  v1 = copy 0\n  jmp b1\n\
         b1:\n  v3 = len v2\n  br_lt v1, v3 -> b2, b5\n\
         b2:\n  v4 = list_get v2, v1\n  store_global x, v4\n  v1 = add v1, 1\n  \
         v5 = load_global x\n  br_gt v5, 1 -> b3, b4\n\
         b3:\n  jmp b5\n\
         b4:\n  v6 = load_global x\n  call print[int](v6)\n  call stone.print_char(10)\n  jmp b1\n\
         b5:\n  ret 0\n"
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
fn a_list_literal_evaluates_its_elements_before_writing_its_target() {
    assert_eq!(
        dump("def f(x);\n    x = [x[0]]\n    ret x\nprint(f([1]))\n")
            .split("fn main")
            .next()
            .unwrap(),
        "fn f(v0):\n\
         b0:\n  v1 = list_load v0, 0\n  v0 = call stone.list_new(1)\n  list_init v0[0], v1\n  \
         ret v0\n"
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
