//! The x86-64 Windows routines behind the internal `gpu_sys` module's reach into NVIDIA's CUDA
//! driver interface, which the display driver installs as `nvcuda.dll`.
//!
//! The driver itself is written in stone, in `gpu_cuda.stn`, as Linux's is in `gpu_nv.stn`; it
//! needs only three routines from here:
//!
//! - `stone.gpu_nvcuda` (`gpu_sys.nvcuda()`) loads `nvcuda.dll` from the system directory alone
//!   (`LoadLibraryExW` with `LOAD_LIBRARY_SEARCH_SYSTEM32`, so no copy beside the program or in
//!   the working directory is ever loaded), giving its handle or 0. Nothing imports the DLL, so a
//!   program runs on a machine without it.
//! - `stone.gpu_symbol` (`gpu_sys.symbol(library, name)`) finds a function with
//!   `GetProcAddress`, giving its address or 0.
//! - `stone.gpu_call` (`gpu_sys.call(function, args)`) calls a function with Microsoft's
//!   convention: the first four of `args` in `rcx`, `rdx`, `r8`, and `r9`, the rest on the stack
//!   above 32 bytes of shadow space, at most [`MAX_ARGS`] in all, from a 16-byte aligned stack.
//!   It gives `rax`, all 64 bits, so a caller of a function that returns a 32-bit value masks it.
//!
//! Each follows the System V convention, as the rest of the runtime does. What a Microsoft call
//! changes (`rax`, `rcx`, `rdx`, `r8`-`r11`, `xmm0`-`xmm5`) is a part of what a System V call may
//! change, so each is an ordinary call for the code that calls it.

use super::gui_windows::{enter, leave};
use crate::codegen::AssemblyGenerator;

/// The most values `gpu_sys.call` passes; a call with more gives -1 without calling.
pub const MAX_ARGS: usize = 16;

/// `LOAD_LIBRARY_SEARCH_SYSTEM32`, which loads a DLL from the system directory alone.
const SEARCH_SYSTEM32: i64 = 0x800;

/// The labels of the routines this module emits.
pub const ROUTINES: [&str; 3] = ["stone.gpu_nvcuda", "stone.gpu_symbol", "stone.gpu_call"];

/// Emits each routine of [`ROUTINES`] among `labels`.
pub fn gpu_windows_runtime(r#gen: &mut dyn AssemblyGenerator, labels: &[&str]) {
    let uses = |label: &str| labels.contains(&label);
    if uses("stone.gpu_nvcuda") {
        r#gen.emit("\t.section\t.rodata");
        r#gen.emit("\t.p2align\t3");
        r#gen.emit(".Lgpu_library:");
        for quad in wide_quads("nvcuda.dll") {
            r#gen.emit(&format!("\t.quad\t{quad:#018x}"));
        }
        r#gen.emit("\t.text");
        r#gen.emit("stone.gpu_nvcuda:");
        enter(r#gen, &[]);
        r#gen.emit("\tsub\trsp, 32");
        r#gen.emit("\tlea\trcx, [rip + .Lgpu_library]");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(&format!("\tmov\tr8d, {SEARCH_SYSTEM32}"));
        r#gen.emit("\tcall\tLoadLibraryExW");
        leave(r#gen, &[]);
    }
    if uses("stone.gpu_symbol") {
        r#gen.emit("stone.gpu_symbol:");
        enter(r#gen, &[]);
        r#gen.emit("\tsub\trsp, 32");
        r#gen.emit("\tmov\trcx, rdi");
        r#gen.emit("\tmov\trdx, rsi");
        r#gen.emit("\tcall\tGetProcAddress");
        leave(r#gen, &[]);
    }
    if uses("stone.gpu_call") {
        call(r#gen);
    }
}

/// Emits `stone.gpu_call`, which takes the function in `rdi` and a list of ints in `rsi`.
fn call(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12"];
    // 32 bytes of shadow space, then a slot for each value past the fourth
    let frame = 32 + 8 * (MAX_ARGS - 4);
    r#gen.emit("stone.gpu_call:");
    enter(r#gen, &saved);
    r#gen.emit(&format!("\tsub\trsp, {frame}"));
    r#gen.emit("\tmov\tr11, rdi");
    r#gen.emit("\tmov\trbx, QWORD PTR [rsi + 16]");
    r#gen.emit("\tmov\tr12, QWORD PTR [rsi]");
    r#gen.emit("\tmov\trax, -1");
    r#gen.emit(&format!("\tcmp\tr12, {MAX_ARGS}"));
    r#gen.emit("\tja\t.Lgpu_call_done");
    // value i past the fourth goes at [rsp + 32 + 8 * (i - 4)], which is [rsp + 8 * i]
    r#gen.emit("\tmov\tecx, 4");
    r#gen.emit(".Lgpu_call_stack:");
    r#gen.emit("\tcmp\trcx, r12");
    r#gen.emit("\tjae\t.Lgpu_call_registers");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rcx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lgpu_call_stack");
    // a register with no value holds 0
    r#gen.emit(".Lgpu_call_registers:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    for (index, register) in ["rcx", "rdx", "r8", "r9"].into_iter().enumerate() {
        r#gen.emit(&format!("\tcmp\tr12, {index}"));
        r#gen.emit("\tjbe\t.Lgpu_call_go");
        r#gen.emit(&format!(
            "\tmov\t{register}, QWORD PTR [rbx + {}]",
            8 * index
        ));
    }
    r#gen.emit(".Lgpu_call_go:");
    r#gen.emit("\tcall\tr11");
    r#gen.emit(".Lgpu_call_done:");
    leave(r#gen, &saved);
}

/// Returns `text` in UTF-16 with a 0 after it, as little-endian quads, padded with zeros.
///
/// For example, `wide_quads("ab")` is `[0x0000_0000_0062_0061]`.
fn wide_quads(text: &str) -> Vec<u64> {
    let units: Vec<u16> = text.encode_utf16().chain([0]).collect();
    units
        .chunks(4)
        .map(|chunk| {
            chunk.iter().enumerate().fold(0, |quad, (index, &unit)| {
                quad | u64::from(unit) << (16 * index)
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::Target;
    use crate::codegen::x64::X64Generator;

    #[test]
    fn a_name_is_written_in_utf16_with_a_0_after_it() {
        assert_eq!(wide_quads("ab"), [0x0000_0000_0062_0061]);
        assert_eq!(
            wide_quads("nvcuda.dll"),
            [
                0x0075_0063_0076_006e,
                0x0064_002e_0061_0064,
                0x0000_0000_006c_006c
            ]
        );
    }

    #[test]
    fn a_call_passes_four_values_in_registers_and_the_rest_above_the_shadow_space() {
        let mut generator = X64Generator::for_target(Target::X64_WINDOWS);
        gpu_windows_runtime(&mut generator, &ROUTINES);
        let text = &generator.output;
        for piece in [
            "stone.gpu_call:\n",
            // the function is called through r11, after the values are in place
            "\tmov\tr11, rdi\n",
            "\tmov\tQWORD PTR [rsp + rcx * 8], rax\n",
            "\tmov\trcx, QWORD PTR [rbx + 0]\n",
            "\tmov\trdx, QWORD PTR [rbx + 8]\n",
            "\tmov\tr8, QWORD PTR [rbx + 16]\n",
            "\tmov\tr9, QWORD PTR [rbx + 24]\n",
            ".Lgpu_call_go:\n\tcall\tr11\n",
            // the library comes from the system directory alone
            "\tmov\tr8d, 2048\n\tcall\tLoadLibraryExW\n",
            "\tcall\tGetProcAddress\n",
        ] {
            assert!(text.contains(piece), "{piece:?} in {text}");
        }
        // the stack keeps 32 bytes of shadow space plus a slot per value past the fourth
        assert!(
            text.contains(&format!("\tsub\trsp, {}\n", 32 + 8 * 12)),
            "{text}"
        );
        // the built-in assembler encodes every line
        crate::codegen::asm::assemble(text, crate::codegen::Architecture::X64).unwrap();
    }

    #[test]
    fn only_the_routines_called_are_emitted() {
        let mut generator = X64Generator::for_target(Target::X64_WINDOWS);
        gpu_windows_runtime(&mut generator, &["stone.gpu_symbol"]);
        let text = &generator.output;
        assert!(text.contains("stone.gpu_symbol:"), "{text}");
        assert!(!text.contains("stone.gpu_call:"), "{text}");
        assert!(!text.contains("nvcuda"), "{text}");
    }
}
