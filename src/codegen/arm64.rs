//! The arm64 backend, which compiles a stone AST to GNU assembler source for AArch64 Linux or
//! macOS.
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
pub mod fs;
pub mod sort;

use crate::ast::Mod;
use crate::codegen::context::{
    Context, LIST_ROUTINES, MAP_ROUTINES, STRING_METHOD_ROUTINES, escape, global_label,
    immortal_string, variant_statics,
};
use crate::codegen::{AssemblyGenerator, Os, Target};
use crate::project::SourceMap;
use crate::span::Span;
use crate::stdlib::CUT_CHARACTER;
use builtins::{Sys, system_call};
use std::path::Path;

/// Registers that carry arguments under AAPCS64, in order.
///
/// Calls between stone functions pass the first eight arguments in these registers, and store any
/// past the eighth at the bottom of the caller's frame, first argument lowest. So argument
/// `i >= 8` is at `[x29, #16 + 8 * (i - 8)]` in the callee.
const ARG_REGS: [&str; 8] = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"];

/// Code generator for arm64 that emits GNU assembler source for AArch64 Linux or macOS. Only the
/// runtime's calls into the system differ between the two (see [`builtins::system_call`]).
///
/// For example, `a + b` with `a` and `b` in registers becomes one `add`.
#[derive(Default)]
pub struct Arm64Generator {
    output: String,
    /// What the backends share about the program being compiled.
    ctx: Context,
    /// Whether some float `%` calls `stone.fmod`, emitted after the code that uses it.
    uses_fmod: bool,
    /// The system the program runs on, which decides how the runtime calls it.
    os: Os,
    /// The span of the instruction being emitted, which its runtime errors point at.
    span: Span,
}

impl AssemblyGenerator for Arm64Generator {
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
        self.emit("\t.text");

        // only emit the print routines if the program prints
        let prints = self.ctx.needs_print(module);
        if prints {
            self.emit("\t// Standard Library Functions");
            let strings = self.ctx.needs_strings();
            builtins::print(self, strings);
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
        if self.uses_fmod {
            floats::fmod_runtime(self);
        }
        if self.ctx.needs_strings() {
            builtins::string_runtime(self);
        }
        if self.ctx.counts_references {
            let enums = self.ctx.needs_enums();
            builtins::memory_runtime(self, enums);
            if enums {
                builtins::enum_runtime(self);
            }
        } else if prints {
            // the text print builds
            builtins::allocator(self);
        }
        if self.ctx.needs_env() {
            builtins::env_runtime(self);
        }
        if self.os == Os::MacOs {
            self.emit("\t.text");
            let text = self.output.clone();
            builtins::system_shims(self, &text);
        }

        self.emit_string_literals();
        self.emit_globals();

        if self.os == Os::Linux {
            // suppress the linker's executable stack warning
            self.emit("\t.section\t.note.GNU-stack,\"\",@progbits");
        }

        Ok(())
    }

    fn emit(&mut self, code: &str) {
        self.output.push_str(code);
        self.output.push('\n');
    }

    fn target(&self) -> Target {
        let arch = crate::codegen::Architecture::Arm64;
        Target {
            arch,
            os: self.os,
            level: arch.baseline(),
        }
    }

    fn set_sources(&mut self, sources: SourceMap) {
        self.ctx.set_sources(sources);
    }
}

impl Arm64Generator {
    /// Returns a generator for AArch64 Linux.
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns a generator for AArch64 running `os`.
    pub fn for_os(os: Os) -> Self {
        Self {
            os,
            ..Self::default()
        }
    }

    /// Emits the code behind every [`Context::fail_label`] and [`Context::fail_at`], plus
    /// `stone.fail`, which writes the location of the site in `stone.site` if there is one,
    /// `error: `, the string in `x0`, a newline, and the site's excerpt to stderr, then exits
    /// with status 1. A failure with a site records it in `stone.site` first, and so does a call
    /// to one of the
    /// [`FALLIBLE_ROUTINES`](crate::codegen::context::FALLIBLE_ROUTINES), whose own failures have none.
    ///
    /// A failure whose message has holes jumps to `stone.fail_detail` instead, with `x0`
    /// pointing at the three pieces of text around them, one after another, and the ints for the
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
                    address(self, "x0", &label);
                    match texts.len() {
                        2 => self.emit("\tb\tstone.fail_detail1"),
                        _ => self.emit("\tb\tstone.fail_detail"),
                    }
                    let texts: Vec<String> = texts.into_iter().map(escape).collect();
                    pieces.push((label, texts));
                }
                None => {
                    let text = self.ctx.intern_string(&failure.message);
                    address(self, "x0", &text);
                    self.emit("\tb\tstone.fail");
                }
            }
        }
        let prefix = self.ctx.intern_string("error: ");
        let newline = self.ctx.intern_string("\n");
        let no_location = self.ctx.new_label("fail_message");
        let common = self.ctx.new_label("fail_common");
        let [first, second] = emit::FAIL_VALUES;
        // x21 is 0, or the number of holes of a detailed message whose pieces x0 starts and
        // whose values wait in x22 and x23, and x19 and x20 hold the message and the site
        self.emit("stone.fail_detail1:");
        self.emit("\tmov\tx21, #1");
        let detailed = self.ctx.new_label("fail_detailed");
        self.emit(&format!("\tb\t{detailed}"));
        self.emit("stone.fail_detail:");
        self.emit("\tmov\tx21, #2");
        self.emit(&format!("{detailed}:"));
        self.emit(&format!("\tmov\tx22, {first}"));
        self.emit(&format!("\tmov\tx23, {second}"));
        // room to write an int's digits in
        self.emit("\tsub\tsp, sp, #32");
        self.emit(&format!("\tb\t{common}"));
        self.emit("stone.fail:");
        self.emit("\tmov\tx21, #0");
        self.emit(&format!("{common}:"));
        self.emit("\tmov\tx19, x0");
        self.emit("\tadrp\tx16, stone.site");
        self.emit("\tldr\tx20, [x16, :lo12:stone.site]");
        self.emit(&format!("\tcbz\tx20, {no_location}"));
        self.emit("\tmov\tx1, x20");
        self.write_stderr();
        // the excerpt follows the location's terminating zero
        self.emit("\tadd\tx20, x1, x2");
        self.emit("\tadd\tx20, x20, #1");
        self.emit(&format!("{no_location}:"));
        address(self, "x1", &prefix);
        self.write_stderr();
        self.emit("\tmov\tx1, x19");
        self.write_stderr();
        let plain = self.ctx.new_label("fail_plain");
        self.emit(&format!("\tcbz\tx21, {plain}"));
        for value in ["x22", "x23"] {
            if value == "x23" {
                self.emit("\tcmp\tx21, #1");
                self.emit(&format!("\tb.eq\t{plain}"));
            }
            // the next piece follows the last one's terminating zero
            self.emit("\tadd\tx24, x1, x2");
            self.emit("\tadd\tx24, x24, #1");
            self.emit(&format!("\tmov\tx9, {value}"));
            self.write_int_stderr();
            self.emit("\tmov\tx1, x24");
            self.write_stderr();
        }
        self.emit(&format!("{plain}:"));
        address(self, "x1", &newline);
        self.write_stderr();
        let done = self.ctx.new_label("fail_done");
        self.emit(&format!("\tcbz\tx20, {done}"));
        self.emit("\tmov\tx1, x20");
        self.write_stderr();
        self.emit(&format!("{done}:"));
        self.emit("\tmov\tx0, #1");
        system_call(self, Sys::Exit);

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

    /// Writes the zero-terminated string at `x1` to stderr, leaving its length in `x2` and `x1`
    /// unchanged.
    fn write_stderr(&mut self) {
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
        system_call(self, Sys::Write);
    }

    /// Writes the int in `x9` to stderr in decimal, as `print` writes it, into the 32 bytes at
    /// `sp`. Clobbers `x0`-`x2` and `x9`-`x13`.
    ///
    /// For example, `-12` writes `-12`.
    fn write_int_stderr(&mut self) {
        let digits = self.ctx.new_label("fail_digits");
        let digit = self.ctx.new_label("fail_digit");
        let write = self.ctx.new_label("fail_int_write");
        // digits are written backwards from the end of the room
        self.emit("\tadd\tx1, sp, #32");
        self.emit("\tmov\tx10, #0"); // negative flag
        self.emit("\tcmp\tx9, #0");
        self.emit(&format!("\tb.ge\t{digits}"));
        // negating the minimum leaves it unchanged, which unsigned division still reads correctly
        self.emit("\tneg\tx9, x9");
        self.emit("\tmov\tx10, #1");
        self.emit(&format!("{digits}:"));
        self.emit("\tmov\tx11, #10");
        self.emit(&format!("{digit}:"));
        self.emit("\tudiv\tx12, x9, x11");
        self.emit("\tmsub\tx13, x12, x11, x9");
        self.emit("\tadd\tx13, x13, #48"); // '0'
        self.emit("\tstrb\tw13, [x1, #-1]!");
        self.emit("\tmov\tx9, x12");
        self.emit(&format!("\tcbnz\tx9, {digit}"));
        self.emit(&format!("\tcbz\tx10, {write}"));
        self.emit("\tmov\tx13, #45"); // '-'
        self.emit("\tstrb\tw13, [x1, #-1]!");
        self.emit(&format!("{write}:"));
        self.emit("\tadd\tx2, sp, #32");
        self.emit("\tsub\tx2, x2, x1");
        self.emit("\tmov\tx0, #2"); // stderr
        system_call(self, Sys::Write);
    }

    /// Records the site record `site` in `stone.site`, for `stone.fail` to point at, using only
    /// `x16` and `x17`.
    fn record_site(&mut self, site: &str) {
        address(self, "x16", site);
        self.emit("\tadrp\tx17, stone.site");
        self.emit("\tstr\tx16, [x17, :lo12:stone.site]");
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
        let fs = self.ctx.module_routines("stone.fs_");
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
        builtins::list_runtime(self);
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

    /// Emits every interned string literal as an immortal string (see [`immortal_string`]), in
    /// `.data`, since retaining and releasing a string writes its count.
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
    use crate::driver::parse_program;

    /// Parses and assembles `source` without invoking gcc.
    fn assemble(source: &str) -> Result<String, String> {
        let module = parse_program(source).map_err(|e| e.to_string())?;
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

    const LCG: &str = "fn lcg(rounds: int) -> int:\n    x = 1\n    total = 0\n    i = 0\n    \
                       while i < rounds:\n        x = x * 1103515245 + 12345\n        \
                       x = x - (x / 2147483648) * 2147483648\n        \
                       total = total + x / 65536\n        i = i + 1\n    ret total\n\
                       print(lcg(20))\n";

    const FIB: &str = "fn fib(n: int) -> int:\n    if n < 2:\n        ret n\n    \
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
        let source = "fn f(a: int, b: int, c: int, d: int, e: int, g: int, h: int, i: int, j: int, k: int) -> int:\n    ret k - j\n\
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
    fn modulo_by_a_power_of_two_shifts_instead_of_dividing() {
        let assembly = assemble("x = 7\nprint(x % 8, x % -2, x % 1)\n").unwrap();
        assert!(!assembly.contains("sdiv"), "{assembly}");
        assert!(assembly.contains("\tlsl\tx9, x9, #3"), "{assembly}");
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
        // each call checks the depth just before it, which only uses x16 and x17
        let fib: String = fib
            .lines()
            .filter(|line| {
                !line.contains("stone.call_depth")
                    && !line.starts_with("\tb.ge\t.Lfail")
                    && !line.starts_with("\tcmp\tx17, ")
            })
            .map(|line| format!("{line}\n"))
            .collect();
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
        let source = "fn f(a: int, b: int, c: int, d: int, e: int, g: int, h: int, i: int, j: int) -> int:\n    s = \"x\" + str(j)\n    \
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
            Path::new("main.stn"),
            source,
            &crate::project::MapSources::default(),
        );
        Arm64Generator::new()
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
            "stone.args:",
            "stone.parse_int:",
            "stone.format_float:",
            "stone.alloc:",
            "stone.fmod:",
        ] {
            assert!(!plain.contains(label), "{label} in {plain}");
        }
        let reading = assemble("while not eof():\n    x = input(\"> \")\n").unwrap();
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
