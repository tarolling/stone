//! The x86-64 backend, which compiles a stone AST to GNU assembler source in Intel syntax.
//!
//! The AST is first lowered to [`ir`](crate::codegen::ir), then each function's vregs are
//! assigned registers by linear scan and emitted (see `x64/emit.rs`). For example, `x = 42` in a
//! function becomes `mov` of 42 into whichever register holds `x`.

pub mod builtins;
mod emit;
pub mod floats;
pub mod fs;
pub mod net;
pub mod process;
pub mod sort;

use crate::ast::Mod;
use crate::codegen::context::{
    Context, LIST_ROUTINES, MAP_ROUTINES, NET_ROUTINES, STRING_METHOD_ROUTINES, escape,
    global_label, immortal_string, variant_statics,
};
use crate::codegen::x64::builtins::print;
use crate::codegen::{AssemblyGenerator, Target};
use crate::project::SourceMap;
use crate::span::Span;
use crate::stdlib::CUT_CHARACTER;
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
    /// The span of the instruction being emitted, which its runtime errors point at.
    span: Span,
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
        let prints = self.ctx.needs_print(module);
        if prints {
            self.emit("\t# Standard Library Functions");
            let strings = self.ctx.needs_strings();
            let eprints = self.ctx.uses(&["stone.text_eprint"]);
            print(self, strings, eprints);
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
        let parses_floats = self
            .ctx
            .uses(&["stone.parse_float", "stone.parse_float_result"]);
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
            let enums = self.ctx.needs_enums();
            builtins::memory_runtime(self, enums);
            if enums {
                let handles = self.ctx.needs_handles();
                builtins::enum_runtime(self, handles);
            }
        } else if prints {
            // the text print builds
            builtins::allocator(self);
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

    fn set_sources(&mut self, sources: SourceMap) {
        self.ctx.set_sources(sources);
    }
}

impl X64Generator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Emits the code behind every [`Context::fail_label`] and [`Context::fail_at`], plus
    /// `stone.fail`, which writes the location of the site in `stone.site` if there is one,
    /// `error: `, the string in `rdi`, a newline, and the site's excerpt to stderr, then exits
    /// with status 1. A failure with a site records it in `stone.site` first, and so does a call
    /// to one of the
    /// [`FALLIBLE_ROUTINES`](crate::codegen::context::FALLIBLE_ROUTINES), whose own failures have none.
    ///
    /// A failure whose message has two holes jumps to `stone.fail_detail` instead, or one hole
    /// `stone.fail_detail1`, with `rdi`
    /// pointing at the pieces of text around them, one after another, and the ints for the
    /// holes in [`FAIL_VALUES`](emit::FAIL_VALUES), so `(index {}, length {})` prints as
    /// `(index 5, length 2)` without allocating.
    fn emit_failures(&mut self) {
        let Some(failures) = self.ctx.failures() else {
            return;
        };
        let mut pieces = Vec::new();
        for failure in failures {
            self.emit(&format!("{}:", failure.label));
            for line in &failure.setup {
                self.emit(line);
            }
            if let Some(site) = &failure.site {
                self.record_site(site);
            }
            match failure.pieces() {
                Some(texts) => {
                    let label = self.ctx.new_label("fail_pieces");
                    self.emit(&format!("\tlea\trdi, [rip + {label}]"));
                    match texts.len() {
                        2 => self.emit("\tjmp\tstone.fail_detail1"),
                        _ => self.emit("\tjmp\tstone.fail_detail"),
                    }
                    let texts: Vec<String> = texts.into_iter().map(escape).collect();
                    pieces.push((label, texts));
                }
                None => {
                    let text = self.ctx.intern_string(&failure.message);
                    self.emit(&format!("\tlea\trdi, [rip + {text}]"));
                    self.emit("\tjmp\tstone.fail");
                }
            }
        }
        let prefix = self.ctx.intern_string("error: ");
        let newline = self.ctx.intern_string("\n");
        let no_location = self.ctx.new_label("fail_message");
        let common = self.ctx.new_label("fail_common");
        let [first, second] = emit::FAIL_VALUES;
        // rbx says whether rdi starts the pieces of a detailed message, whose values wait in r14
        // and r15 and which has rbp holes, and r12 and r13 hold the message and the site
        self.emit("stone.fail_detail1:");
        self.emit("\tmov\trbp, 1");
        let detailed = self.ctx.new_label("fail_detailed");
        self.emit(&format!("\tjmp\t{detailed}"));
        self.emit("stone.fail_detail:");
        self.emit("\tmov\trbp, 2");
        self.emit(&format!("{detailed}:"));
        self.emit(&format!("\tmov\tr14, {first}"));
        self.emit(&format!("\tmov\tr15, {second}"));
        self.emit("\tmov\trbx, 1");
        // room to write an int's digits in
        self.emit("\tsub\trsp, 32");
        self.emit(&format!("\tjmp\t{common}"));
        self.emit("stone.fail:");
        self.emit("\txor\trbx, rbx");
        self.emit(&format!("{common}:"));
        self.emit("\tmov\tr12, rdi");
        self.emit("\tmov\tr13, QWORD PTR [rip + stone.site]");
        self.emit("\ttest\tr13, r13");
        self.emit(&format!("\tje\t{no_location}"));
        self.emit("\tmov\trsi, r13");
        self.write_stderr();
        // the excerpt follows the location's terminating zero
        self.emit("\tmov\tr13, rsi");
        self.emit("\tadd\tr13, rdx");
        self.emit("\tinc\tr13");
        self.emit(&format!("{no_location}:"));
        self.emit(&format!("\tlea\trsi, [rip + {prefix}]"));
        self.write_stderr();
        self.emit("\tmov\trsi, r12");
        self.write_stderr();
        let plain = self.ctx.new_label("fail_plain");
        self.emit("\ttest\trbx, rbx");
        self.emit(&format!("\tje\t{plain}"));
        for value in ["r14", "r15"] {
            if value == "r15" {
                self.emit("\tcmp\trbp, 1");
                self.emit(&format!("\tje\t{plain}"));
            }
            // the next piece follows the last one's terminating zero
            self.emit("\tmov\trbx, rsi");
            self.emit("\tadd\trbx, rdx");
            self.emit("\tinc\trbx");
            self.emit(&format!("\tmov\trax, {value}"));
            self.write_int_stderr();
            self.emit("\tmov\trsi, rbx");
            self.write_stderr();
        }
        self.emit(&format!("{plain}:"));
        self.emit(&format!("\tlea\trsi, [rip + {newline}]"));
        self.write_stderr();
        let done = self.ctx.new_label("fail_done");
        self.emit("\ttest\tr13, r13");
        self.emit(&format!("\tje\t{done}"));
        self.emit("\tmov\trsi, r13");
        self.write_stderr();
        self.emit(&format!("{done}:"));
        self.emit("\tmov\trax, 231"); // sys_exit_group
        self.emit("\tmov\trdi, 1");
        self.emit("\tsyscall");

        if !pieces.is_empty() {
            self.emit("\t.section\t.rodata");
            for (label, texts) in pieces {
                self.emit(&format!("{label}:"));
                for text in texts {
                    self.emit(&format!("\t.string \"{text}\""));
                }
            }
            self.emit("\t.text");
        }
        let sites = self.ctx.sites();
        if !sites.is_empty() {
            self.emit("\t.section\t.rodata");
            for site in sites {
                self.emit(&format!("{}:", site.label));
                self.emit(&format!("\t.string \"{}\"", site.location));
                self.emit(&format!("\t.string \"{}\"", site.excerpt));
            }
            self.emit("\t.text");
        }
    }

    /// Writes the zero-terminated string at `rsi` to stderr, leaving its length in `rdx` and
    /// `rsi` unchanged.
    fn write_stderr(&mut self) {
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

    /// Writes the int in `rax` to stderr in decimal, as `print` writes it, into the 32 bytes at
    /// `rsp`. Clobbers `rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`, and `r11`.
    ///
    /// For example, `-12` writes `-12`.
    fn write_int_stderr(&mut self) {
        let digits = self.ctx.new_label("fail_digits");
        let digit = self.ctx.new_label("fail_digit");
        let write = self.ctx.new_label("fail_int_write");
        // digits are written backwards from the terminating zero
        self.emit("\tmov\trsi, rsp");
        self.emit("\tadd\trsi, 31");
        self.emit("\tmov\tbyte ptr [rsi], 0");
        self.emit("\txor\tr8, r8"); // negative flag
        self.emit("\ttest\trax, rax");
        self.emit(&format!("\tjns\t{digits}"));
        // negating the minimum leaves it unchanged, which unsigned division still reads correctly
        self.emit("\tneg\trax");
        self.emit("\tmov\tr8, 1");
        self.emit(&format!("{digits}:"));
        self.emit("\tmov\trcx, 10");
        self.emit(&format!("{digit}:"));
        self.emit("\txor\trdx, rdx");
        self.emit("\tdiv\trcx");
        self.emit("\tadd\tdl, 48"); // '0'
        self.emit("\tdec\trsi");
        self.emit("\tmov\tbyte ptr [rsi], dl");
        self.emit("\ttest\trax, rax");
        self.emit(&format!("\tjnz\t{digit}"));
        self.emit("\ttest\tr8, r8");
        self.emit(&format!("\tjz\t{write}"));
        self.emit("\tdec\trsi");
        self.emit("\tmov\tbyte ptr [rsi], 45"); // '-'
        self.emit(&format!("{write}:"));
        self.write_stderr();
    }

    /// Records the site record `site` in `stone.site`, for `stone.fail` to point at, using only
    /// `rax`.
    fn record_site(&mut self, site: &str) {
        self.emit(&format!("\tlea\trax, [rip + {site}]"));
        self.emit("\tmov\tQWORD PTR [rip + stone.site], rax");
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
        // os.run shares the fs routines' pieces, so it comes with them
        let (run, os): (Vec<&str>, Vec<&str>) = self
            .ctx
            .module_routines("stone.os_")
            .into_iter()
            .partition(|label| *label == "stone.os_run");
        if !os.is_empty() {
            let cwd_failure = self
                .ctx
                .failure_of("stone.os_cwd", crate::stdlib::os::CWD_FAILURE);
            builtins::os_runtime(self, &os, &cwd_failure);
        }
        let mut fs = self.ctx.module_routines("stone.fs_");
        fs.extend(run);
        fs.extend(
            NET_ROUTINES
                .into_iter()
                .filter(|label| self.ctx.uses(&[label])),
        );
        if !fs.is_empty() {
            fs::fs_runtime(self, &fs);
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
        let parsing: Vec<&str> = [
            "stone.parse_int",
            "stone.parse_float",
            "stone.parse_int_result",
            "stone.parse_float_result",
            "stone.map_missing_str",
        ]
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
        let methods: Vec<&str> = STRING_METHOD_ROUTINES
            .iter()
            .copied()
            .filter(|label| self.ctx.uses(&[label]))
            .collect();
        if !methods.is_empty() {
            let empty_separator = self.ctx.failure_of("stone.str_split", "empty separator");
            let cut_character = match self.ctx.uses(&["stone.str_at", "stone.str_slice_of"]) {
                true => self.ctx.fail_label(CUT_CHARACTER),
                false => String::new(),
            };
            builtins::string_methods(self, &methods, &empty_separator, &cut_character);
        }
    }

    /// Emits the list runtime and every list printer `print` asked for, if the program uses lists.
    fn emit_list_runtime(&mut self) -> Result<(), String> {
        if !self.ctx.needs_lists() {
            return Ok(());
        }
        let maps_shrink = self.ctx.uses(&["stone.map_take"]);
        builtins::list_runtime(self, maps_shrink);
        let methods: Vec<&str> = LIST_ROUTINES
            .iter()
            .copied()
            .filter(|label| self.ctx.uses(&[label]))
            .collect();
        builtins::list_methods(self, &methods);
        if self.ctx.uses(MAP_ROUTINES) {
            let strings = self.ctx.needs_strings();
            builtins::map_runtime(self, strings);
        }
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
        let statics = std::mem::take(&mut self.ctx.variant_statics);
        if literals.is_empty() && statics.is_empty() {
            return;
        }

        self.emit("");
        self.emit("\t.data");
        for (label, text) in literals {
            immortal_string(self, &label, &text);
        }
        variant_statics(self, &statics);
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
        if self.ctx.failures().is_some() {
            // the site a runtime error points at, or 0
            self.emit("stone.site:");
            self.emit("\t.zero\t8");
        }
        if self.ctx.counts_references {
            self.emit("stone.live:");
            self.emit("\t.zero\t8");
            // the lists waiting to be freed, linked through their capacities, and whether
            // `stone.free_list` is freeing them
            self.emit("stone.lists_pending:");
            self.emit("\t.zero\t8");
            self.emit("stone.freeing_lists:");
            self.emit("\t.zero\t8");
        }
        if self.ctx.counts_references && self.ctx.needs_enums() {
            // the enum values waiting to be freed, linked through their tags, and whether
            // `stone.free_enum` is freeing them
            self.emit("stone.free_pending:");
            self.emit("\t.zero\t8");
            self.emit("stone.freeing:");
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
    use crate::driver::parse_program;
    use crate::stdlib::MAX_CALL_DEPTH;

    /// Parses and assembles `source` without invoking gcc.
    fn assemble(source: &str) -> Result<String, String> {
        let module = parse_program(source).map_err(|e| e.to_string())?;
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
            assemble("fn f(a: int, b: int, c: int, d: int, e: int, g: int, h: int) -> int:\n    ret h\nprint(f(1, 2, 3, 4, 5, 6, 7))\n")
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
        // a magic multiplier instead of idiv
        assert!(!assembly.contains("idiv"), "{assembly}");
        assert!(assembly.contains("\timul\trdx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        assert!(!assembly.contains("integer overflow"), "{assembly}");
    }

    #[test]
    fn modulo_by_a_power_of_two_shifts_instead_of_dividing() {
        let assembly = assemble("x = 7\nprint(x % 8, x % -2, x % 1)\n").unwrap();
        assert!(!assembly.contains("idiv"), "{assembly}");
        assert!(assembly.contains("\tshl\trcx, 3"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
    }

    #[test]
    fn modulo_by_a_safe_constant_skips_the_checks() {
        let assembly = assemble("x = 7\nprint(x % 10, x % -3)\n").unwrap();
        assert!(!assembly.contains("idiv"), "{assembly}");
        assert!(assembly.contains("\timul\trdx"), "{assembly}");
        assert!(!assembly.contains("division by zero"), "{assembly}");
        let checked = assemble("x = 7\ny = int(\"3\")\nprint(x % y)\n").unwrap();
        assert!(checked.contains("division by zero"), "{checked}");
        assert!(!checked.contains("integer overflow"), "{checked}");
    }

    #[test]
    fn powers_and_float_remainders_do_not_call_libc() {
        let assembly =
            assemble("x = 2.5\nn = int(\"3\")\nprint(x ** n, x ** -2, x % 0.75, n ** n, n ** 2)\n")
                .unwrap();
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
            // a float in a vector register is negated by flipping its sign bit there
            "\txorpd\t",
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

    const LCG: &str = "fn lcg(rounds: int) -> int:\n    x = 1\n    total = 0\n    i = 0\n    \
                       while i < rounds:\n        x = x * 1103515245 + 12345\n        \
                       x = x - (x / 2147483648) * 2147483648\n        \
                       total = total + x / 65536\n        i = i + 1\n    ret total\n\
                       print(lcg(20))\n";

    const FIB: &str = "fn fib(n: int) -> int:\n    if n < 2:\n        ret n\n    \
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
        let source = "fn count(xs: list[str]) -> int:\n    n = 0\n    for x in xs:\n        n = n + 1\n    ret n\n\
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
    fn strings_carry_their_length_before_their_count() {
        let text = assemble("print(\"h\\x01!\" + \"caf\u{e9}\", \"\".len())\n").unwrap();
        // lengths count bytes, so the two bytes of \u{e9} count twice
        for (literal, length) in [("h\\001!", 3), ("caf\u{e9}", 5), ("", 0)] {
            let header = format!("\t.quad\t{length}\n\t.quad\t{}\n", builtins::IMMORTAL);
            let string = format!(":\n\t.string \"{literal}\"\n");
            // the header is right before the label, which is right before the text
            let before = &text[..text.find(&string).expect(literal)];
            let label = before.rfind('\n').unwrap() + 1;
            assert!(before[..label].ends_with(&header), "{literal} in\n{text}");
        }
        // so measuring a string reads its header rather than scanning it
        let len = text.split("stone.str_len:\n").nth(1).unwrap();
        assert!(
            len.starts_with("\tmov\trax, QWORD PTR [rdi - 16]\n\tret\n"),
            "{len}"
        );
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
        // each call checks the depth just before it, which touches no register
        let fib: String = fib
            .lines()
            .filter(|line| !line.contains("stone.call_depth") && !line.starts_with("\tjge\t.Lfail"))
            .map(|line| format!("{line}\n"))
            .collect();
        assert!(fib.contains("\tsub\trdi, 1\n\tcall\tfn.fib"), "{fib}");
        assert!(fib.contains("\tsub\trdi, 2\n\tcall\tfn.fib"), "{fib}");
    }

    #[test]
    fn runtime_errors_point_at_their_site_only_with_sources() {
        let source = "a = 1\nb = 0\nprint(a / b, a % b)\nprint(\"\\t\" + str(a / b))\n";
        let (sources, loaded) = crate::driver::load(
            Path::new("main.stn"),
            source,
            &crate::project::MapSources::default(),
        );
        let module = loaded.unwrap().module;
        let mut generator = X64Generator::new();
        generator.set_sources(sources);
        let assembly = generator.assemble(&module).unwrap();
        let site = "\t.string \"main.stn:3:7: \"\n\t.string \"  |\\n3 | print(a / b, a % b)\\n  |       ^^^^^\\n\"";
        assert!(assembly.contains(site), "{assembly}");
        // the tab in the excerpt is escaped like any string
        assert!(
            assembly.contains("4 | print(\\\"\\\\t\\\" + str(a / b))"),
            "{assembly}"
        );
        // one stub per message and site, each recording its site: a division can divide by
        // zero or overflow, and a remainder only divide by zero
        assert_eq!(assembly.matches("main.stn:3:7: ").count(), 1, "{assembly}");
        assert_eq!(assembly.matches("main.stn:3:14: ").count(), 1, "{assembly}");
        assert_eq!(
            assembly.matches("], rax\n\tlea\trdi").count(),
            5,
            "{assembly}"
        );

        let without = assemble(source).unwrap();
        assert!(!without.contains(".Lsite"), "{without}");
    }

    #[test]
    fn each_call_checks_the_depth_so_the_error_points_at_it() {
        let fib = function_text(&assemble(FIB).unwrap(), "fib");
        let check = format!("\tcmp\tQWORD PTR [rip + stone.call_depth], {MAX_CALL_DEPTH}\n\tjge\t");
        assert_eq!(fib.matches(&check).count(), 2, "{fib}");
        assert_eq!(fib.matches("\tcall\tfn.fib").count(), 2, "{fib}");
        // the prologue only counts the call
        assert!(!fib.contains("\tjg\t"), "{fib}");
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
        let source = "fn f(a: int, b: int) -> int:\n    t = 0\n    for k in range(a):\n        \
                      t = t + a * (b + k)\n    ret t\nprint(f(3, 4))\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(!f.contains("\timul\trax"), "{f}");
    }

    #[test]
    fn loop_tests_compare_and_jump_without_a_bool() {
        let source =
            "fn f(n: int) -> int:\n    i = 0\n    while i < n:\n        i = i + 1\n    ret i\n";
        let f = function_text(&assemble(source).unwrap(), "f");
        assert!(f.contains("\tcmp\t"), "{f}");
        assert!(f.contains("\tjge\t") || f.contains("\tjl\t"), "{f}");
        assert!(!f.contains("\tset"), "{f}");
    }

    #[test]
    fn division_by_a_variable_zero_or_minus_one_keeps_the_checks() {
        for divisor in ["y", "0", "-1"] {
            let source = format!("x = 7\ny = int(\"2\")\nprint(x / {divisor})\n");
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
            Path::new("main.stn"),
            source,
            &crate::project::MapSources::default(),
        );
        X64Generator::new()
            .assemble(&module.unwrap().module)
            .unwrap()
    }

    #[test]
    fn fs_routines_are_only_emitted_when_used() {
        let plain = assemble_linked("use fs\nprint(1)\n");
        assert!(!plain.contains("stone.fs_"), "{plain}");

        // exists cannot fail, so it needs none of the error tables
        let exists = assemble_linked("use fs\nprint(fs.exists(\"a\"))\n");
        assert!(exists.contains("stone.fs_exists:"), "{exists}");
        assert!(!exists.contains(".Lfs_err:"), "{exists}");
        assert!(!exists.contains("stone.fs_read:"), "{exists}");

        let read = assemble_linked("use fs\nprint(fs.read(\"a\"))\n");
        for label in [
            "stone.fs_read:",
            ".Lfs_err:",
            ".Lfs_utf8:",
            "stone.str_int:",
        ] {
            assert!(read.contains(label), "{label} in {read}");
        }
        assert!(!read.contains("stone.fs_list_dir:"), "{read}");

        let list = assemble_linked("use fs\nprint(fs.list_dir(\".\"))\n");
        for label in ["stone.fs_list_dir:", "stone.list_sort_str:"] {
            assert!(list.contains(label), "{label} in {list}");
        }
    }

    #[test]
    fn os_run_is_only_emitted_when_used_and_shares_the_fs_pieces() {
        let plain = assemble_linked("use os\nprint(os.pid())\n");
        assert!(!plain.contains("stone.os_run:"), "{plain}");
        assert!(!plain.contains(".Lfs_err:"), "{plain}");

        let run = assemble_linked("use os\nprint(os.run(\"ls\", []))\n");
        for label in [
            "stone.os_run:",
            ".Lfs_err:",
            ".Lfs_utf8:",
            ".Lfs_check_path:",
            "stone.getenv:",
            "stone.str_int:",
        ] {
            assert!(run.contains(label), "{label} in {run}");
        }
        assert!(!run.contains("stone.fs_read:"), "{run}");
        // with fs.read too, the shared pieces are emitted once
        let both = assemble_linked("use os\nuse fs\nprint(os.run(\"ls\", []), fs.read(\"a\"))\n");
        assert_eq!(both.matches(".Lfs_utf8:").count(), 1, "{both}");
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

        let reading = assemble("while not eof():\n    x = input(\"> \")\n").unwrap();
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
