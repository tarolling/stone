//! The check at the start of a program built for an x86-64 level above the baseline, such as
//! `x86_64v3-linux`, that the processor it runs on has every feature the level assumes.
//!
//! Without it, a program that uses AVX2 on a processor without it would die of an illegal
//! instruction wherever it first used one. With it, the program stops before it runs anything,
//! with an error naming the level it needs.

use crate::codegen::context::escape;
use crate::codegen::x64::system::{Sys, system_call};
use crate::codegen::{AssemblyGenerator, Level};

/// Returns the line a program built for `level` writes to stderr, before exiting with status 1,
/// on a processor that lacks a feature of the level.
///
/// For example, `level_missing(Level { major: 3, minor: 0 })` is
/// `"error: this program needs an x86-64 v3 processor\n"`.
pub fn level_missing(level: Level) -> String {
    format!("error: this program needs an x86-64 {level} processor\n")
}

/// The features one CPUID leaf reports, as a mask of bits of one of the registers it returns.
struct Features {
    leaf: u32,
    register: &'static str,
    mask: u32,
}

/// Returns the mask with each of `bits` set, such as `0b101` for `[0, 2]`.
fn mask(bits: &[u32]) -> u32 {
    bits.iter().fold(0, |mask, bit| mask | 1 << bit)
}

/// Returns the features each x86-64 level adds to the one before it, as the x86-64 psABI lists
/// them, and the bits of `XCR0` the system must set for the level's registers: SSE and AVX
/// state for `v3`, plus the AVX-512 state for `v4`.
///
/// For example, `v2` adds SSE3, SSSE3, SSE4.1, SSE4.2, POPCNT, CMPXCHG16B, and LAHF in 64-bit
/// mode, and needs nothing of `XCR0`.
fn features(major: u8) -> (Vec<Features>, u32) {
    let leaf1 = |bits: &[u32]| Features {
        leaf: 1,
        register: "ecx",
        mask: mask(bits),
    };
    let leaf7 = |bits: &[u32]| Features {
        leaf: 7,
        register: "ebx",
        mask: mask(bits),
    };
    let extended = |bits: &[u32]| Features {
        leaf: 0x8000_0001,
        register: "ecx",
        mask: mask(bits),
    };
    match major {
        // SSE3 0, SSSE3 9, CMPXCHG16B 13, SSE4.1 19, SSE4.2 20, POPCNT 23; LAHF 0
        2 => (vec![leaf1(&[0, 9, 13, 19, 20, 23]), extended(&[0])], 0),
        // FMA 12, MOVBE 22, OSXSAVE 27, AVX 28, F16C 29; BMI1 3, AVX2 5, BMI2 8; LZCNT 5
        3 => (
            vec![
                leaf1(&[12, 22, 27, 28, 29]),
                leaf7(&[3, 5, 8]),
                extended(&[5]),
            ],
            0b110,
        ),
        // AVX512F 16, AVX512DQ 17, AVX512CD 28, AVX512BW 30, AVX512VL 31
        4 => (vec![leaf7(&[16, 17, 28, 30, 31])], 0b1110_0110),
        _ => (Vec::new(), 0),
    }
}

/// Emits the check, which `_start` runs first, that the processor has every feature of the
/// target's level, or nothing for the baseline. It reads CPUID's leaves 1, 7, and `0x80000001`,
/// after checking the processor has them, and for `v3` and above `XCR0` through `xgetbv`, which
/// the OSXSAVE bit of leaf 1 says may run. A processor that lacks any of them writes
/// [`level_missing`] to stderr and exits with status 1. It changes `rax`, `rbx`, `rcx`, and
/// `rdx`.
pub fn level_check(r#gen: &mut dyn AssemblyGenerator) {
    let level = r#gen.target().level;
    if level.major < 2 {
        return;
    }
    // every level's features, one test per leaf
    let mut checks: Vec<Features> = Vec::new();
    let mut state = 0;
    for major in 2..=level.major {
        let (features, xcr0) = features(major);
        for added in features {
            match checks.iter_mut().find(|known| known.leaf == added.leaf) {
                Some(known) => known.mask |= added.mask,
                None => checks.push(added),
            }
        }
        state |= xcr0;
    }
    let missing = ".Llevel_missing";
    let checked = ".Llevel_checked";
    let highest = |r#gen: &mut dyn AssemblyGenerator, leaf: u32, needed: u32| {
        r#gen.emit(&format!("\tmov\teax, {leaf:#x}"));
        r#gen.emit("\tcpuid");
        r#gen.emit(&format!("\tcmp\teax, {needed:#x}"));
        r#gen.emit(&format!("\tjb\t{missing}"));
    };
    // the leaves past the highest one a processor has report another leaf's bits
    let basic = checks
        .iter()
        .filter(|c| c.leaf < 0x8000_0000)
        .map(|c| c.leaf);
    highest(r#gen, 0, basic.max().unwrap_or(1));
    highest(r#gen, 0x8000_0000, 0x8000_0001);
    // leaf 1 comes first, since its OSXSAVE bit says xgetbv may run
    for check in &checks {
        r#gen.emit(&format!("\tmov\teax, {:#x}", check.leaf));
        r#gen.emit("\txor\tecx, ecx");
        r#gen.emit("\tcpuid");
        r#gen.emit(&format!("\tnot\t{}", check.register));
        r#gen.emit(&format!("\ttest\t{}, {:#x}", check.register, check.mask));
        r#gen.emit(&format!("\tjnz\t{missing}"));
    }
    if state != 0 {
        r#gen.emit("\txor\tecx, ecx");
        r#gen.emit("\txgetbv");
        r#gen.emit("\tnot\teax");
        r#gen.emit(&format!("\ttest\teax, {state:#x}"));
        r#gen.emit(&format!("\tjnz\t{missing}"));
    }
    r#gen.emit(&format!("\tjmp\t{checked}"));
    let message = level_missing(level);
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Llevel_message:");
    r#gen.emit(&format!("\t.string \"{}\"", escape(&message)));
    r#gen.emit("\t.text");
    r#gen.emit(&format!("{missing}:"));
    r#gen.emit("\tmov\tedi, 2");
    r#gen.emit("\tlea\trsi, [rip + .Llevel_message]");
    r#gen.emit(&format!("\tmov\tedx, {}", message.len()));
    system_call(r#gen, Sys::Write);
    r#gen.emit("\tmov\tedi, 1");
    system_call(r#gen, Sys::Exit);
    r#gen.emit(&format!("{checked}:"));
}
