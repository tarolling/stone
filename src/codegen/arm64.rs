//! The arm64 backend, which compiles a stone AST to GNU assembler source for AArch64 Linux.
//!
//! It shares everything up to register allocation with the x86-64 backend: the AST is lowered to
//! [`ir`](crate::codegen::ir), each function's vregs are assigned registers by linear scan, and
//! `arm64/emit.rs` selects instructions. For example, `x = 42` in a function becomes
//! `mov x19, #42` if `x` was given `x19`. The runtime in `arm64/builtins.rs` mirrors
//! `x64/builtins.rs` routine for routine, under the same labels.
//!
//! Generated code follows AAPCS64 between stone functions and the runtime, except that the inline
//! release of a string or list calls a free routine that preserves every allocatable register
//! (see [`builtins::memory_runtime`]), so that it does not count as a call. The stack pointer
//! stays 16-byte aligned at every instruction, as AArch64 requires, so frames are saved in pairs.

pub mod builtins;
mod emit;
pub mod floats;

use crate::ast::Mod;
use crate::codegen::context::{Context, global_label, immortal_string};
use crate::codegen::{Architecture, AssemblyGenerator};
use std::path::Path;

/// Registers that carry arguments under AAPCS64, in order.
///
/// Calls between stone functions pass the first eight arguments in these registers, and store any
/// past the eighth at the bottom of the caller's frame, first argument lowest. So argument
/// `i >= 8` is at `[x29, #16 + 8 * (i - 8)]` in the callee.
const ARG_REGS: [&str; 8] = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];

/// Code generator for arm64 that emits GNU assembler source for AArch64 Linux.
///
/// For example, `a + b` with `a` and `b` in registers becomes one `add`.
#[derive(Default)]
pub struct Arm64Generator {
    output: String,
    /// What the backends share about the program being compiled.
    ctx: Context,
    /// Whether some float `%` calls `stone.fmod`, emitted after the code that uses it.
    uses_fmod: bool,
}

impl AssemblyGenerator for Arm64Generator {
    fn compile(&mut self, module: &Mod, output: &Path) -> std::io::Result<()> {
        let text = self.assemble(module).map_err(std::io::Error::other)?;
        crate::codegen::link(&text, output, self.architecture())
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
        self.emit("\t.text");

        // only emit the print routines if the program prints
        if self.ctx.needs_print(module) {
            self.emit("\t// Standard Library Functions");
            builtins::print(self);
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
        if self.uses_fmod {
            floats::fmod_runtime(self);
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

    fn architecture(&self) -> Architecture {
        Architecture::Arm64
    }
}

impl Arm64Generator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Emits the code behind every [`Context::fail_label`], plus `stone.fail`, which writes
    /// `error: `, the string in `x0`, and a newline to stderr, then exits with status 1.
    fn emit_failures(&mut self) {
        let Some(failures) = self.ctx.failures() else {
            return;
        };
        for (label, message) in failures {
            let text = self.ctx.intern_string(&message);
            self.emit(&format!("{label}:"));
            address(self, "x0", &text);
            self.emit("\tb\tstone.fail");
        }
        let prefix = self.ctx.intern_string("error: ");
        let newline = self.ctx.intern_string("\n");
        self.emit("stone.fail:");
        self.emit("\tmov\tx19, x0");
        for text in [Some(prefix), None, Some(newline)] {
            match text {
                Some(text) => address(self, "x1", &text),
                None => self.emit("\tmov\tx1, x19"),
            }
            // strlen, then write to stderr
            self.emit("\tmov\tx2, #0");
            let length = self.ctx.new_label("fail_length");
            let write = self.ctx.new_label("fail_write");
            self.emit(&format!("{length}:"));
            self.emit("\tldrb\tw9, [x1, x2]");
            self.emit(&format!("\tcbz\tw9, {write}"));
            self.emit("\tadd\tx2, x2, #1");
            self.emit(&format!("\tb\t{length}"));
            self.emit(&format!("{write}:"));
            self.emit("\tmov\tx0, #2"); // stderr
            self.emit("\tmov\tx8, #64"); // write
            self.emit("\tsvc\t#0");
        }
        self.emit("\tmov\tx0, #1");
        self.emit("\tmov\tx8, #94"); // exit_group
        self.emit("\tsvc\t#0");
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
        let os = self.ctx.os_routines();
        if !os.is_empty() {
            let cwd_failure = self.ctx.os_cwd_failure();
            builtins::os_runtime(self, &os, &cwd_failure);
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

    /// Emits every interned string literal as an immortal string (see [`immortal_string`]), in
    /// `.data`, since retaining and releasing a string writes its count.
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

/// Emits code that puts the address of `symbol` in `reg`, which works wherever the executable is
/// loaded.
///
/// For example, `address(gen, "x0", ".Lstone_true")` emits `adrp x0, .Lstone_true` and
/// `add x0, x0, :lo12:.Lstone_true`.
fn address(r#gen: &mut dyn AssemblyGenerator, reg: &str, symbol: &str) {
    r#gen.emit(&format!("\tadrp\t{reg}, {symbol}"));
    r#gen.emit(&format!("\tadd\t{reg}, {reg}, :lo12:{symbol}"));
}

/// Emits the start of a frame: it saves the frame pointer and link register, points `x29` at
/// them, and saves `saved` below them in pairs, so the stack stays 16-byte aligned. An odd
/// register out gets 16 bytes of its own.
///
/// For example, `push_frame(gen, &["x19", "x20", "x21"])` emits three stores: the frame record,
/// `x19` and `x20` together, then `x21`.
fn push_frame(r#gen: &mut dyn AssemblyGenerator, saved: &[&str]) {
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tmov\tx29, sp");
    for pair in saved.chunks(2) {
        match pair {
            [a, b] => r#gen.emit(&format!("\tstp\t{a}, {b}, [sp, #-16]!")),
            _ => r#gen.emit(&format!("\tstr\t{}, [sp, #-16]!", pair[0])),
        }
    }
}

/// Emits the end of a frame that [`push_frame`] started with the same `saved`, restoring every
/// register it saved, then returns.
fn pop_frame(r#gen: &mut dyn AssemblyGenerator, saved: &[&str]) {
    let pairs = saved.len().div_ceil(2);
    if pairs == 0 {
        r#gen.emit("\tmov\tsp, x29");
    } else {
        r#gen.emit(&format!("\tsub\tsp, x29, #{}", 16 * pairs));
    }
    for pair in saved.chunks(2).rev() {
        match pair {
            [a, b] => r#gen.emit(&format!("\tldp\t{a}, {b}, [sp], #16")),
            _ => r#gen.emit(&format!("\tldr\t{}, [sp], #16", pair[0])),
        }
    }
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::context::function_label;
    use crate::driver::parse;

    /// Parses and assembles `source` without invoking gcc.
    fn assemble(source: &str) -> Result<String, String> {
        let module = parse(source).map_err(|e| e.to_string())?;
        Arm64Generator::new().assemble(&module)
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
    fn assemble_returns_assembly_text() {
        let assembly = assemble("print(1)\n").unwrap();
        assert!(assembly.contains("main:"), "{assembly}");
        assert!(assembly.contains("\tbl\tstone.print_int"), "{assembly}");
        assert!(!assembly.contains("intel_syntax"), "{assembly}");
    }

    #[test]
    fn invalid_programs_are_rejected_before_code_generation() {
        assert_eq!(
            assemble("x = 1 + \"a\"\n"),
            Err("1:9: error: expected int, found str".to_string())
        );
    }

    #[test]
    fn arguments_past_the_eighth_go_on_the_stack() {
        let source = "def f(a, b, c, d, e, g, h, i, j, k);\n    ret k - j\n\
                      print(f(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))\n";
        let assembly = assemble(source).unwrap();
        let f = function_text(&assembly, "f");
        assert!(f.contains("[x29, #16]"), "{f}");
        assert!(f.contains("[x29, #24]"), "{f}");
        assert!(assembly.contains("[sp, #8]"), "{assembly}");
    }

    #[test]
    fn division_by_a_power_of_two_shifts_instead_of_dividing() {
        let assembly = assemble("x = 7\nprint(x / 8, x / -2)\n").unwrap();
        assert!(!assembly.contains("sdiv"), "{assembly}");
        assert!(assembly.contains(", #3\n"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
    }

    #[test]
    fn division_by_a_variable_keeps_both_checks() {
        for divisor in ["y", "0", "-1"] {
            let source = format!("x = 7\ny = 2\nprint(x / {divisor}, x % {divisor})\n");
            let assembly = assemble(&source).unwrap();
            assert!(assembly.contains("\tsdiv\t"), "{divisor}");
            assert!(assembly.contains("division by zero"), "{divisor}");
            assert!(
                assembly.contains("integer overflow in division"),
                "{divisor}"
            );
        }
        let constant = assemble("x = 7\nprint(x / 10, x % 10)\n").unwrap();
        assert!(constant.contains("\tmsub\t"), "{constant}");
        assert!(!constant.contains("division by zero"), "{constant}");
    }

    #[test]
    fn float_comparisons_use_conditions_that_are_false_for_nan() {
        let assembly = assemble("a = 1.5\nb = -a * 2.0 + a / 0.5\nprint(a < b, a <= b)\n").unwrap();
        for instruction in [
            "\tfmul\t", "\tfadd\t", "\tfdiv\t", "\tfcmp\t", "mi\n", "ls\n",
        ] {
            assert!(assembly.contains(instruction), "{instruction}");
        }
        // even a literal float divisor is checked, unlike an int one
        assert!(assembly.contains("division by zero"));
    }

    #[test]
    fn float_remainders_call_the_register_preserving_fmod() {
        let assembly = assemble("x = 2.5\nprint(x % 0.75, x ** 3)\n").unwrap();
        assert!(assembly.contains("\tbl\tstone.fmod"), "{assembly}");
        assert!(assembly.contains("stone.fmod:"), "{assembly}");
        assert!(!assembly.contains("\tbl\tpow"), "{assembly}");
    }

    #[test]
    fn converting_a_float_to_an_int_is_checked() {
        let assembly = assemble("print(int(2.5), float(2))\n").unwrap();
        assert!(assembly.contains("\tfcvtzs\t"), "{assembly}");
        assert!(assembly.contains("\tscvtf\t"), "{assembly}");
        assert!(assembly.contains("cannot convert float to int"));
    }

    #[test]
    fn list_indexing_checks_bounds_inline() {
        let assembly = assemble("xs = [1, 2]\nxs[0] = xs[-1]\nprint(xs[1])\n").unwrap();
        assert!(assembly.contains("list index out of range"), "{assembly}");
        assert!(assembly.contains("lsl #3]"), "{assembly}");
    }

    #[test]
    fn a_call_free_loop_keeps_its_variables_in_registers() {
        let lcg = function_text(&assemble(LCG).unwrap(), "lcg");
        let spills = lcg
            .lines()
            .filter(|line| line.starts_with("\tldr\t") || line.starts_with("\tstr\t"))
            .filter(|line| line.contains("[sp, #"));
        assert_eq!(spills.count(), 0, "{lcg}");
    }

    #[test]
    fn arguments_are_computed_in_their_argument_registers() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        assert!(fib.contains(", #1\n\tbl\tfn.fib"), "{fib}");
        assert!(fib.contains(", #2\n\tbl\tfn.fib"), "{fib}");
        assert!(fib.contains("\tsub\tx0, "), "{fib}");
    }

    #[test]
    fn functions_save_exactly_the_callee_saved_registers_they_use() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        let mut saved = 0;
        for n in 19..=28 {
            let reg = format!("x{n}");
            let used = fib.contains(&format!(", {reg}")) || fib.contains(&format!("\t{reg},"));
            let pushed = fib.contains(&format!("\tstp\t{reg}, "))
                || fib.contains(&format!(", {reg}, [sp, #-16]!"))
                || fib.contains(&format!("\tstr\t{reg}, [sp, #-16]!"));
            assert_eq!(used, pushed, "{reg} in\n{fib}");
            saved += used as usize;
        }
        // n and the first call's result both live across a call
        assert!(saved >= 2, "{fib}");
    }

    #[test]
    fn the_stack_pointer_only_moves_by_multiples_of_sixteen() {
        let source = "def f(a, b, c, d, e, g, h, i, j);\n    s = \"x\" + str(j)\n    \
                      print(s, a + b + c + d + e + g + h + i)\n    ret 0\n\
                      f(1, 2, 3, 4, 5, 6, 7, 8, 9)\nprint(input(\"?\"), args(), 1.5 % 1.0)\n";
        let assembly = assemble(source).unwrap();
        for line in assembly.lines() {
            let moves_sp = line.starts_with("\tsub\tsp, sp, #")
                || line.starts_with("\tadd\tsp, sp, #")
                || line.contains("[sp, #-")
                || line.contains("[sp], #");
            if !moves_sp {
                continue;
            }
            let amount: i64 = line
                .rsplit('#')
                .next()
                .and_then(|n| n.trim_end_matches('!').trim_end_matches(']').parse().ok())
                .unwrap_or_else(|| panic!("no amount in {line}"));
            assert_eq!(amount % 16, 0, "{line}");
        }
    }

    /// Links and assembles `source` without invoking gcc, so that it can `use os`.
    fn assemble_linked(source: &str) -> String {
        let (_, module) = crate::driver::load(
            Path::new("main.st"),
            source,
            &crate::project::MapSources::default(),
        );
        Arm64Generator::new().assemble(&module.unwrap().0).unwrap()
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
        assert!(arch.contains("\t.string \"aarch64\""), "{arch}");
        assert!(arch.contains("\t.string \"linux\""), "{arch}");

        let rest =
            "use os\nprint(os.hostname(), os.cpu_count(), os.time(), os.clock())\nos.exit(0)\n";
        let rest = assemble_linked(rest);
        for label in [
            "stone.os_hostname:",
            "stone.os_cpu_count:",
            "stone.os_time:",
            "stone.os_clock:",
            "stone.os_exit:",
        ] {
            assert!(rest.contains(label), "{label} in {rest}");
        }
    }

    #[test]
    fn io_and_string_routines_are_only_emitted_when_used() {
        let plain = assemble("print(1)\n").unwrap();
        for label in [
            "stone.input:",
            "stone.args:",
            "stone.parse_int:",
            "stone.format_float:",
            "stone.alloc:",
            "stone.fmod:",
        ] {
            assert!(!plain.contains(label), "{label} in {plain}");
        }
        let reading = assemble("while not eof();\n    x = input(\"> \")\n").unwrap();
        assert!(reading.contains("stone.input:"), "{reading}");
        assert!(reading.contains("stone.stdin_fill:"), "{reading}");
        let peeking = assemble("print(eof())\n").unwrap();
        assert!(peeking.contains("stone.eof:"), "{peeking}");
        assert!(!peeking.contains("stone.input:"), "{peeking}");
        let arguments = assemble("x = args()\n").unwrap();
        assert!(arguments.contains("stone.args:"), "{arguments}");
        let splitting = assemble("x = \"a b\".split(\" \")\n").unwrap();
        assert!(splitting.contains("empty separator"), "{splitting}");
    }
}
