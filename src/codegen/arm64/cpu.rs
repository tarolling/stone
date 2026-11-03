//! The check at the start of a Linux program built for Armv9 or above, such as
//! `aarch64v9-linux`, that the processor it runs on has SVE and SVE2, which the program's
//! arithmetic on lists uses.
//!
//! Without it, a program that uses SVE on a processor without it would die of an illegal
//! instruction wherever it first used one. With it, the program stops before it runs anything,
//! with an error naming what it needs. macOS programs use no SVE, so they check nothing.

use crate::codegen::arm64::builtins::{Sys, system_call};
use crate::codegen::context::escape;
use crate::codegen::{AssemblyGenerator, Level, Os};

/// The auxiliary vector entry with the processor's first word of feature bits.
const AT_HWCAP: u32 = 16;
/// The auxiliary vector entry with the second word.
const AT_HWCAP2: u32 = 26;
/// The bit of `AT_HWCAP` that says the processor has SVE.
const HWCAP_SVE: u32 = 22;
/// The bit of `AT_HWCAP2` that says it has SVE2.
const HWCAP2_SVE2: u32 = 1;

/// Returns the line a program built for `level` writes to stderr, before exiting with status 1,
/// on a processor without SVE2.
///
/// For example, `level_missing(Level { major: 9, minor: 0 })` is
/// `"error: this program needs an aarch64 v9 processor with SVE2\n"`.
pub fn level_missing(level: Level) -> String {
    format!("error: this program needs an aarch64 {level} processor with SVE2\n")
}

/// Emits the check, which Linux's `_start` runs before `main`, that the processor has SVE and
/// SVE2, or nothing below `v9` or on macOS. It finds the end of the environment, whose pointer is
/// in `x2`, then reads the feature words the kernel left in the auxiliary vector after it. A
/// processor that lacks either writes [`level_missing`] to stderr and exits with status 1. It
/// changes `x9` to `x13` and keeps `x0` to `x2`.
pub fn level_check(r#gen: &mut dyn AssemblyGenerator) {
    let target = r#gen.target();
    if target.os != Os::Linux || target.level.major < 9 {
        return;
    }
    let missing = ".Llevel_missing";
    let checked = ".Llevel_checked";
    // the auxiliary vector starts after the environment's null pointer
    r#gen.emit("\tmov\tx9, x2");
    r#gen.emit(".Llevel_environment:");
    r#gen.emit("\tldr\tx10, [x9]");
    r#gen.emit("\tadd\tx9, x9, #8");
    r#gen.emit("\tcbnz\tx10, .Llevel_environment");
    // pairs of a type and a value, up to a type of 0
    r#gen.emit("\tmov\tx11, #0");
    r#gen.emit("\tmov\tx12, #0");
    r#gen.emit(".Llevel_entry:");
    r#gen.emit("\tldr\tx10, [x9]");
    r#gen.emit("\tcbz\tx10, .Llevel_test");
    r#gen.emit("\tldr\tx13, [x9, #8]");
    r#gen.emit("\tadd\tx9, x9, #16");
    r#gen.emit(&format!("\tcmp\tx10, #{AT_HWCAP}"));
    r#gen.emit("\tcsel\tx11, x13, x11, eq");
    r#gen.emit(&format!("\tcmp\tx10, #{AT_HWCAP2}"));
    r#gen.emit("\tcsel\tx12, x13, x12, eq");
    r#gen.emit("\tb\t.Llevel_entry");
    r#gen.emit(".Llevel_test:");
    r#gen.emit(&format!("\ttbz\tx11, #{HWCAP_SVE}, {missing}"));
    r#gen.emit(&format!("\ttbz\tx12, #{HWCAP2_SVE2}, {missing}"));
    r#gen.emit(&format!("\tb\t{checked}"));
    let message = level_missing(target.level);
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Llevel_message:");
    r#gen.emit(&format!("\t.string \"{}\"", escape(&message)));
    r#gen.emit("\t.text");
    r#gen.emit(&format!("{missing}:"));
    r#gen.emit("\tmov\tx0, #2");
    r#gen.emit("\tadrp\tx1, .Llevel_message");
    r#gen.emit("\tadd\tx1, x1, :lo12:.Llevel_message");
    r#gen.emit(&format!("\tmov\tx2, #{}", message.len()));
    system_call(r#gen, Sys::Write);
    r#gen.emit("\tmov\tx0, #1");
    system_call(r#gen, Sys::Exit);
    r#gen.emit(&format!("{checked}:"));
}
