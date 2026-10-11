//! The x86-64 backend, which compiles a stone AST to GNU assembler source in Intel syntax.
//!
//! The AST is first lowered to [`ir`](crate::codegen::ir), then each function's vregs are
//! assigned registers by linear scan and emitted (see `x64/emit.rs`). For example, `x = 42` in a
//! function becomes `mov` of 42 into whichever register holds `x`.

pub mod builtins;
mod emit;
pub mod floats;

use crate::ast::Mod;
use crate::codegen::context::{Context, global_label, immortal_string};
use crate::codegen::x64::builtins::print;
use crate::codegen::{AssemblyGenerator, Target};
use std::path::Path;

/// Registers that carry arguments under the System V ABI, in order.
///
/// Calls between stone functions pass the first six arguments in these registers, and push any
/// past the sixth on the stack, first argument deepest. So with `n` arguments, argument `i >= 6`
/// is at `[rbp + 16 + 8 * (n - 1 - i)]` in the callee.
const ARG_REGS: [&str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

/// Code generator for x86-64 that emits GNU assembler source in Intel syntax.
///
/// For example, `a + b` with `a` and `b` in registers becomes a `mov` and an `add`.
#[derive(Default)]
pub struct X64Generator {
    output: String,
    /// What the backends share about the program being compiled.
    ctx: Context,
}

impl AssemblyGenerator for X64Generator {
    fn compile(&mut self, module: &Mod, output: &Path) -> std::io::Result<()> {
        let text = self.assemble(module).map_err(std::io::Error::other)?;
        crate::codegen::link(&text, output, self.target())
    }

    fn assemble(&mut self, module: &Mod) -> Result<String, String> {
        self.ctx.check(module)?;
        // first pass lowers to IR, and the second allocates registers and emits
        self.scan(module)?;
        self.generate(module)?;
        Ok(self.output.clone())
    }

    fn scan(&mut self, module: &Mod) -> Result<(), String> {
        self.ctx.lower(module)
    }

    fn generate(&mut self, module: &Mod) -> Result<(), String> {
        self.emit("\t.intel_syntax noprefix");
        self.emit("\t.text");

        // only emit the print routines if the program prints
        if self.ctx.needs_print(module) {
            self.emit("\t# Standard Library Functions");
            print(self);
        }

        // stone functions first, then main, which the lowering puts last
        for function in std::mem::take(&mut self.ctx.program.functions) {
            self.emit_function(&function)?;
        }

        builtins::start_runtime(self);
        self.emit_io_runtime();
        self.emit_failures();
        self.emit_list_runtime()?;
        let formats_floats = self.ctx.prints_floats || self.ctx.uses(&["stone.str_float"]);
        let parses_floats = self.ctx.uses(&["stone.parse_float"]);
        if formats_floats {
            floats::float_runtime(self);
        }
        if parses_floats {
            floats::decimal_runtime(self);
        }
        if formats_floats || parses_floats {
            floats::bignum_runtime(self);
        }
        if self.ctx.uses(&["stone.str_float"]) {
            builtins::str_float_runtime(self);
        }
        if self.ctx.needs_strings() {
            builtins::string_runtime(self);
        }
        if self.ctx.counts_references {
            builtins::memory_runtime(self);
        }
        if self.ctx.needs_env() {
            builtins::env_runtime(self);
        }

        self.emit_string_literals();
        self.emit_globals();

        // suppress the linker's executable stack warning
        self.emit("\t.section\t.note.GNU-stack,\"\",@progbits");

        Ok(())
    }

    fn emit(&mut self, code: &str) {
        self.output.push_str(code);
        self.output.push('\n');
    }

    fn target(&self) -> Target {
        Target::X64_LINUX
    }
}

impl X64Generator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Emits the code behind every [`Context::fail_label`], plus `stone.fail`, which writes
    /// `error: `, the string in `rdi`, and a newline to stderr, then exits with status 1.
    fn emit_failures(&mut self) {
        let Some(failures) = self.ctx.failures() else {
            return;
        };
        for (label, message) in failures {
            let text = self.ctx.intern_string(&message);
            self.emit(&format!("{label}:"));
            self.emit(&format!("\tlea\trdi, [rip + {text}]"));
            self.emit("\tjmp\tstone.fail");
        }
        let prefix = self.ctx.intern_string("error: ");
        let newline = self.ctx.intern_string("\n");
        self.emit("stone.fail:");
        self.emit("\tmov\tr12, rdi");
        for text in [prefix, "r12".to_string(), newline] {
            if text == "r12" {
                self.emit("\tmov\trsi, r12");
            } else {
                self.emit(&format!("\tlea\trsi, [rip + {text}]"));
            }
            // strlen, then write to stderr
            self.emit("\txor\trdx, rdx");
            let length = self.ctx.new_label("fail_length");
            let write = self.ctx.new_label("fail_write");
            self.emit(&format!("{length}:"));
            self.emit("\tcmp\tbyte ptr [rsi + rdx], 0");
            self.emit(&format!("\tje\t{write}"));
            self.emit("\tinc\trdx");
            self.emit(&format!("\tjmp\t{length}"));
            self.emit(&format!("{write}:"));
            self.emit("\tmov\trax, 1"); // sys_write
            self.emit("\tmov\trdi, 2"); // stderr
            self.emit("\tsyscall");
        }
        self.emit("\tmov\trax, 231"); // sys_exit_group
        self.emit("\tmov\trdi, 1");
        self.emit("\tsyscall");
    }

    /// Emits the routines behind input, `args`, the `os` module, parsing, `str`, and the string
    /// methods that the program calls, before the failures, since some of them fail through `stone.fail`.
    fn emit_io_runtime(&mut self) {
        let input: Vec<&str> = ["stone.input", "stone.eof"]
            .into_iter()
            .filter(|label| self.ctx.uses(&[label]))
            .collect();
        if !input.is_empty() {
            builtins::io_runtime(self, &input);
        }
        if self.ctx.uses(&["stone.args"]) {
            builtins::args_runtime(self);
        }
        let os = self.ctx.module_routines("stone.os_");
        if !os.is_empty() {
            let cwd_failure = self
                .ctx
                .failure_of("stone.os_cwd", crate::stdlib::os::CWD_FAILURE);
            builtins::os_runtime(self, &os, &cwd_failure);
        }
        let random: Vec<&str> = [
            "stone.random_seed",
            "stone.random_float",
            "stone.random_below",
        ]
        .into_iter()
        .filter(|label| self.ctx.uses(&[label]))
        .collect();
        if !random.is_empty() {
            builtins::random_runtime(self, &random);
        }
        let time = self.ctx.module_routines("stone.time_");
        if !time.is_empty() {
            let negative = self
                .ctx
                .failure_of("stone.time_sleep", crate::stdlib::time::SLEEP_NEGATIVE);
            let too_large = self
                .ctx
                .failure_of("stone.time_sleep", crate::stdlib::time::SLEEP_TOO_LARGE);
            builtins::time_runtime(self, &time, (&negative, &too_large));
        }
        let parsing: Vec<&str> = ["stone.parse_int", "stone.parse_float"]
            .into_iter()
            .filter(|label| self.ctx.uses(&[label]))
            .collect();
        if !parsing.is_empty() {
            builtins::parse_runtime(self, &parsing);
            self.ctx.needs_fail = true;
        }
        if self.ctx.uses(&["stone.str_int", "stone.str_bool"]) {
            builtins::conversion_runtime(self);
        }
        let methods: Vec<&str> = ["stone.str_strip", "stone.str_split_ws", "stone.str_split"]
            .into_iter()
            .filter(|label| self.ctx.uses(&[label]))
            .collect();
        if !methods.is_empty() {
            let empty_separator = if self.ctx.uses(&["stone.str_split"]) {
                self.ctx.fail_label("empty separator")
            } else {
                String::new()
            };
            builtins::string_methods(self, &methods, &empty_separator);
        }
    }

    /// Emits the list runtime and every list printer `print` asked for, if the program uses lists.
    fn emit_list_runtime(&mut self) -> Result<(), String> {
        if !self.ctx.needs_lists() {
            return Ok(());
        }
        builtins::list_runtime(self);
        for (label, element) in self.ctx.list_printers()? {
            builtins::print_list(self, &label, &element);
        }
        Ok(())
    }

    /// Emits every interned string literal as an immortal string (see [`immortal_string`]). They
    /// go in `.data` rather than `.rodata`, since retaining and releasing a string writes its
    /// count.
    fn emit_string_literals(&mut self) {
        let literals = self.ctx.string_literals();
        if literals.is_empty() {
            return;
        }

        self.emit("");
        self.emit("\t.data");
        for (label, escaped) in literals {
            immortal_string(self, &label, &escaped);
        }
        self.emit("");
    }

    /// Emits the `.bss` data: the count of active calls, the count of live objects if the program
    /// has strings or lists, and a zeroed 8-byte slot for each global variable, plus a flag that
    /// is set once the global is assigned.
    ///
    /// For example, `total = 1` at the top level produces `g.total` and `g.total.set`.
    fn emit_globals(&mut self) {
        self.emit("\t.bss");
        self.emit("\t.p2align\t3");
        self.emit("stone.call_depth:");
        self.emit("\t.zero\t8");
        if self.ctx.counts_references {
            self.emit("stone.live:");
            self.emit("\t.zero\t8");
        }
        if self.ctx.uses(&["stone.args"]) {
            // main saves its argc and argv here for args()
            self.emit("stone.argc:");
            self.emit("\t.zero\t8");
            self.emit("stone.argv:");
            self.emit("\t.zero\t8");
        }
        if self.ctx.needs_env() {
            // and its environment here for stone.getenv
            self.emit("stone.envp:");
            self.emit("\t.zero\t8");
        }
        for name in self.ctx.program.globals.clone() {
            let label = global_label(&name);
            self.emit(&format!("{label}:"));
            self.emit("\t.zero\t8");
            self.emit(&format!("{label}.set:"));
            self.emit("\t.zero\t8");
        }
        self.emit("\t.text");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::context::function_label;
    use crate::driver::parse;

    /// Parses and assembles `source` without invoking gcc.
    fn assemble(source: &str) -> Result<String, String> {
        let module = parse(source).map_err(|e| e.to_string())?;
        X64Generator::new().assemble(&module)
    }

    #[test]
    fn assemble_returns_assembly_text() {
        let assembly = assemble("print(1)\n").unwrap();
        assert!(assembly.contains("main:"));
        assert!(assembly.contains("\tcall\tstone.print_int"));
    }

    #[test]
    fn invalid_programs_are_rejected_before_code_generation() {
        assert_eq!(
            assemble("x = 1 + \"a\"\n"),
            Err("1:9: error: expected int, found str".to_string())
        );
    }

    #[test]
    fn more_than_six_parameters_assemble() {
        assert!(
            assemble("def f(a, b, c, d, e, g, h);\n    ret h\nprint(f(1, 2, 3, 4, 5, 6, 7))\n")
                .is_ok()
        );
    }

    #[test]
    fn print_takes_any_number_of_arguments() {
        assert!(assemble("print(1, 2, 3, 4, 5, 6, 7)\n").is_ok());
    }

    #[test]
    fn division_by_a_power_of_two_shifts_instead_of_dividing() {
        let assembly = assemble("x = 7\nprint(x / 8, x / -2)\n").unwrap();
        assert!(!assembly.contains("idiv"), "{assembly}");
        assert!(assembly.contains("\tsar\trax, 3"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
    }

    #[test]
    fn division_by_another_nonzero_constant_skips_the_checks() {
        let assembly = assemble("x = 7\nprint(x / 10, x / -3)\n").unwrap();
        assert!(assembly.contains("\tidiv\trcx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        assert!(!assembly.contains("integer overflow"), "{assembly}");
    }

    #[test]
    fn modulo_by_a_safe_constant_skips_the_checks() {
        let assembly = assemble("x = 7\nprint(x % 10, x % -8)\n").unwrap();
        assert!(assembly.contains("\tidiv\trcx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        let checked = assemble("x = 7\ny = 3\nprint(x % y)\n").unwrap();
        assert!(checked.contains("division by zero"), "{checked}");
        assert!(!checked.contains("integer overflow"), "{checked}");
    }

    #[test]
    fn powers_and_float_remainders_do_not_call_libc() {
        let assembly =
            assemble("x = 2.5\nn = 3\nprint(x ** n, x ** -2, x % 0.75, n ** n, n ** 2)\n").unwrap();
        assert!(!assembly.contains("\tcall\tpow"), "{assembly}");
        assert!(!assembly.contains("\tcall\tfmod"), "{assembly}");
        assert!(assembly.contains("\tfprem"), "{assembly}");
        // only the exponent that is not a literal is checked
        let main = assembly.split("\nmain:").nth(1).unwrap();
        let main = main.split("\nstone.").next().unwrap();
        assert_eq!(main.matches("\tjs\t").count(), 1, "{main}");
    }

    #[test]
    fn list_indexing_checks_bounds_inline() {
        let assembly = assemble("xs = [1, 2]\nxs[0] = xs[-1]\nprint(xs[1])\n").unwrap();
        assert!(!assembly.contains("stone.list_slot"), "{assembly}");
        assert!(assembly.contains("list index out of range"), "{assembly}");
    }

    #[test]
    fn float_arithmetic_uses_sse() {
        let assembly = assemble("a = 1.5\nb = -a * 2.0 + a / 0.5\nprint(a < b)\n").unwrap();
        for instruction in [
            "\tmulsd\t",
            "\taddsd\t",
            "\tdivsd\t",
            "\tucomisd\t",
            "\tbtc\t",
        ] {
            assert!(assembly.contains(instruction), "{instruction}");
        }
        // 1.5 is loaded by its bits
        assert!(assembly.contains("\tmov\trax, 4609434218613702656"));
        // even a literal float divisor is checked, unlike an int one
        assert!(assembly.contains("division by zero"));
    }

    #[test]
    fn float_printing_is_only_emitted_when_needed() {
        let assembly = assemble("print([1.5])\n").unwrap();
        assert!(assembly.contains("stone.print_float:"));
        assert!(assembly.contains("\tcall\tstone.format_float"));
        assert!(assembly.contains("stone.big_mul_add:"));
        assert!(!assembly.contains("stone.decimal_to_float:"));
        let assembly = assemble("x = 1.5\nprint(1)\n").unwrap();
        assert!(!assembly.contains("stone.print_float:"));
        assert!(!assembly.contains("stone.big_mul_add:"));
        let assembly = assemble("x = float(\"1.5\")\n").unwrap();
        assert!(assembly.contains("stone.decimal_to_float:"));
        assert!(!assembly.contains("stone.format_float:"));
        assert!(!assembly.contains("stone.parse_int:"));
    }

    #[test]
    fn converting_a_float_to_an_int_is_checked() {
        let assembly = assemble("print(int(2.5), float(2))\n").unwrap();
        assert!(assembly.contains("\tcvttsd2si\trax, xmm0"));
        assert!(assembly.contains("\tcvtsi2sd\txmm0, rax"));
        assert!(assembly.contains("cannot convert float to int"));
    }

    /// Returns the assembly of stone function `name`, from its label to the next function's.
    fn function_text(assembly: &str, name: &str) -> String {
        let start = assembly
            .find(&format!("{}:\n", function_label(name)))
            .unwrap();
        let rest = &assembly[start..];
        let end = ["\nfn.", "\n\t.globl\tmain"]
            .iter()
            .filter_map(|next| rest.find(next))
            .min()
            .unwrap_or(rest.len());
        rest[..end].to_string()
    }

    const LCG: &str = "def lcg(rounds);\n    x = 1\n    total = 0\n    i = 0\n    \
                       while i < rounds;\n        x = x * 1103515245 + 12345\n        \
                       x = x - (x / 2147483648) * 2147483648\n        \
                       total = total + x / 65536\n        i = i + 1\n    ret total\n\
                       print(lcg(20))\n";

    const FIB: &str = "def fib(n);\n    if n < 2;\n        ret n\n    \
                       ret fib(n - 1) + fib(n - 2)\nprint(fib(10))\n";

    #[test]
    fn a_call_free_loop_keeps_its_variables_in_registers() {
        let lcg = function_text(&assemble(LCG).unwrap(), "lcg");
        assert!(!lcg.contains("[rbp -"), "{lcg}");
        assert!(!lcg.contains("\tpush\trax"), "{lcg}");
        assert!(!lcg.contains("idiv"), "{lcg}");
    }

    #[test]
    fn releasing_is_not_a_call_so_a_loop_keeps_caller_saved_registers() {
        let source = "def count(xs);\n    n = 0\n    for x in xs;\n        n = n + 1\n    ret n\n\
                      print(count([\"a\", \"b\"]))\n";
        let count = function_text(&assemble(source).unwrap(), "count");
        // each element is retained for x, and x's old value released, freeing it if it was last
        assert!(count.contains("\tinc\tQWORD PTR ["), "{count}");
        assert!(count.contains("\tdec\tQWORD PTR ["), "{count}");
        assert!(count.contains("\tcall\tstone.free_str"), "{count}");
        assert!(count.contains("\tcall\tstone.free_list"), "{count}");
        // so nothing needs a callee-saved register or a spill slot
        for reg in ["rbx", "r12", "r13", "r14", "r15"] {
            assert!(!count.contains(&format!("\tpush\t{reg}")), "{count}");
        }
        assert!(!count.contains("[rbp -"), "{count}");
    }

    #[test]
    fn string_literals_are_writable_and_never_freed() {
        let text = assemble("print(\"hi\" + \"!\")\n").unwrap();
        let data = text.split("\t.data").nth(1).expect("a data section");
        assert!(
            data.contains(&format!("\t.quad\t{}\n", builtins::IMMORTAL)),
            "{data}"
        );
        assert!(data.contains("\t.string \"hi\""), "{data}");
        assert!(text.contains("\tcall\tstone.leak_check"), "{text}");
    }

    #[test]
    fn programs_without_strings_or_lists_have_no_memory_runtime() {
        let text = assemble("print(1 + 2)\n").unwrap();
        assert!(!text.contains("stone.alloc"), "{text}");
        assert!(!text.contains("stone.leak_check"), "{text}");
    }

    #[test]
    fn functions_save_exactly_the_callee_saved_registers_they_use() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        let mut saved = 0;
        for reg in ["rbx", "r12", "r13", "r14", "r15"] {
            let used = fib.contains(&format!(", {reg}")) || fib.contains(&format!("\t{reg},"));
            let pushed = fib.contains(&format!("\tpush\t{reg}\n"));
            let popped = fib.contains(&format!("\tpop\t{reg}\n"));
            assert_eq!(used, pushed, "{reg} in\n{fib}");
            assert_eq!(used, popped, "{reg} in\n{fib}");
            saved += used as usize;
        }
        // n and the first call's result both live across a call
        assert!(saved >= 2, "{fib}");
    }

    #[test]
    fn calls_pass_arguments_in_registers() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        assert!(!fib.contains("[rsp"), "{fib}");
    }

    #[test]
    fn arguments_are_computed_in_their_argument_registers() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        assert!(fib.contains("\tsub\trdi, 1\n\tcall\tfn.fib"), "{fib}");
        assert!(fib.contains("\tsub\trdi, 2\n\tcall\tfn.fib"), "{fib}");
    }

    #[test]
    fn no_value_is_stored_and_immediately_reloaded() {
        let assembly = assemble(FIB).unwrap() + &assemble(LCG).unwrap();
        let lines: Vec<&str> = assembly.lines().collect();
        for pair in lines.windows(2) {
            if let (Some(store), Some(load)) = (
                pair[0].strip_prefix("\tmov\t"),
                pair[1].strip_prefix("\tmov\t"),
            ) && let (Some((a, b)), Some((c, d))) =
                (store.split_once(", "), load.split_once(", "))
            {
                assert!(!(a == d && b == c), "{}\n{}", pair[0], pair[1]);
            }
        }
    }

    #[test]
    fn commutative_operations_write_their_right_operand_in_place() {
        // the product's destination is the register of its right operand
        let source = "def f(a, b);\n    t = 0\n    for k in range(a);\n        \
                      t = t + a * (b + k)\n    ret t\nprint(f(3, 4))\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(!f.contains("\timul\trax"), "{f}");
    }

    #[test]
    fn loop_tests_compare_and_jump_without_a_bool() {
        let source = "def f(n);\n    i = 0\n    while i < n;\n        i = i + 1\n    ret i\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(f.contains("\tcmp\t"), "{f}");
        assert!(f.contains("\tjge\t") || f.contains("\tjl\t"), "{f}");
        assert!(!f.contains("\tset"), "{f}");
    }

    #[test]
    fn division_by_a_variable_zero_or_minus_one_keeps_the_checks() {
        for divisor in ["y", "0", "-1"] {
            let source = format!("x = 7\ny = 2\nprint(x / {divisor})\n");
            let assembly = assemble(&source).unwrap();
            assert!(assembly.contains("division by zero"), "{divisor}");
            assert!(
                assembly.contains("integer overflow in division"),
                "{divisor}"
            );
        }
    }

    /// Links and assembles `source` without invoking gcc, so that it can `use os`.
    fn assemble_linked(source: &str) -> String {
        let (_, module) = crate::driver::load(
            Path::new("main.st"),
            source,
            &crate::project::MapSources::default(),
        );
        X64Generator::new().assemble(&module.unwrap().0).unwrap()
    }

    #[test]
    fn os_routines_are_only_emitted_when_used() {
        let plain = assemble_linked("use os\nprint(1)\n");
        assert!(!plain.contains("stone.os_"), "{plain}");

        let pid = assemble_linked("use os\nprint(os.pid())\n");
        assert!(pid.contains("stone.os_pid:"), "{pid}");
        assert!(!pid.contains("stone.os_cwd:"), "{pid}");
        assert!(!pid.contains("stone.fail:"), "{pid}");

        let env = assemble_linked("use os\nprint(os.env(\"A\"), os.has_env(\"B\"))\n");
        for label in ["stone.os_env:", "stone.os_has_env:", "stone.os_getenv:"] {
            assert!(env.contains(label), "{label} in {env}");
        }

        let cwd = assemble_linked("use os\nprint(os.cwd())\n");
        assert!(cwd.contains("stone.os_cwd:"), "{cwd}");
        assert!(
            cwd.contains("could not read the current directory"),
            "{cwd}"
        );

        let arch = assemble_linked("use os\nprint(os.arch(), os.platform())\n");
        assert!(arch.contains("\t.string \"x86_64\""), "{arch}");
        assert!(arch.contains("\t.string \"linux\""), "{arch}");

        let rest = "use os\nprint(os.hostname(), os.cpu_count())\nos.exit(0)\n";
        let rest = assemble_linked(rest);
        for label in [
            "stone.os_hostname:",
            "stone.os_cpu_count:",
            "stone.os_exit:",
        ] {
            assert!(rest.contains(label), "{label} in {rest}");
        }
    }

    #[test]
    fn random_routines_are_only_emitted_when_used() {
        let plain = assemble_linked("use random\nprint(1)\n");
        assert!(!plain.contains("stone.random_"), "{plain}");

        let seed = assemble_linked("use random\nrandom.seed(1)\n");
        for label in [
            "stone.random_state:",
            "stone.random_seed:",
            "stone.random_next:",
        ] {
            assert!(seed.contains(label), "{label} in {seed}");
        }
        for label in ["stone.random_float:", "stone.random_below:"] {
            assert!(!seed.contains(label), "{label} in {seed}");
        }

        let draws = assemble_linked(
            "use random\nprint(random.random(), random.randint(1, 2), random.choice([3]))\n",
        );
        for label in ["stone.random_float:", "stone.random_below:"] {
            assert!(draws.contains(label), "{label} in {draws}");
        }
        for message in [
            crate::stdlib::random::EMPTY_RANGE,
            crate::stdlib::random::EMPTY_CHOICE,
        ] {
            assert!(draws.contains(message), "{message} in {draws}");
        }
    }

    #[test]
    fn time_routines_are_only_emitted_when_used() {
        let now = assemble_linked("use time\nprint(time.now())\n");
        assert!(now.contains("stone.time_now:"), "{now}");
        assert!(!now.contains("stone.time_clock:"), "{now}");
        assert!(!now.contains("stone.fail:"), "{now}");

        let sleep = assemble_linked("use time\ntime.sleep(1)\nprint(time.clock())\n");
        for label in ["stone.time_sleep:", "stone.time_clock:"] {
            assert!(sleep.contains(label), "{label} in {sleep}");
        }
        assert!(!sleep.contains("stone.time_now:"), "{sleep}");
        for message in [
            crate::stdlib::time::SLEEP_NEGATIVE,
            crate::stdlib::time::SLEEP_TOO_LARGE,
        ] {
            assert!(sleep.contains(message), "{message} in {sleep}");
        }
    }

    #[test]
    fn io_and_string_routines_are_only_emitted_when_used() {
        let plain = assemble("print(1)\n").unwrap();
        for label in [
            "stone.input:",
            "stone.eof:",
            "stone.args:",
            "stone.argc",
            "stone.parse_int:",
            "stone.str_int:",
            "stone.str_split:",
            "stone.format_float:",
        ] {
            assert!(!plain.contains(label), "{label} in {plain}");
        }

        let reading = assemble("while not eof();\n    x = input(\"> \")\n").unwrap();
        assert!(reading.contains("stone.input:"), "{reading}");
        assert!(reading.contains("stone.print_str:"), "{reading}");
        assert!(!reading.contains("stone.args:"), "{reading}");
        let peeking = assemble("print(eof())\n").unwrap();
        assert!(peeking.contains("stone.eof:"), "{peeking}");
        assert!(!peeking.contains("stone.input:"), "{peeking}");

        let arguments = assemble("x = args()\n").unwrap();
        assert!(arguments.contains("stone.args:"), "{arguments}");
        assert!(arguments.contains("[rip + stone.argc], edi"), "{arguments}");

        let parsing = assemble("x = int(\"1\")\n").unwrap();
        assert!(parsing.contains("stone.parse_int:"), "{parsing}");
        assert!(parsing.contains("stone.fail:"), "{parsing}");

        let converting = assemble("x = str(1.5)\n").unwrap();
        assert!(converting.contains("stone.str_float:"), "{converting}");
        assert!(converting.contains("stone.format_float:"), "{converting}");

        let splitting = assemble("x = \"a b\".split(\" \")\n").unwrap();
        assert!(splitting.contains("stone.str_split:"), "{splitting}");
        assert!(splitting.contains("empty separator"), "{splitting}");
    }
}
