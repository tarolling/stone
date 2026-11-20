//! The routines behind the internal `gpu_sys` module, the thin native help that the GPU drivers
//! written in stone drive an NVIDIA GPU through: `gpu_nv.stn` on Linux, with raw system calls,
//! and `gpu_cuda.stn` on Windows, through `nvcuda.dll` (whose routines are in
//! [`gpu_windows`](super::gpu_windows)); plus reads and writes of raw memory, mappings, and slots
//! in `.bss` for their state and the planner's.

use super::system::{Sys, system_call};
use crate::codegen::{AssemblyGenerator, Os};

/// Emits the `gpu_sys` routines among `labels`, each taking its arguments as a System V call
/// does and giving its result in `rax`. The system calls give a negative error number on failure,
/// as Linux returns it; `mmap` and `munmap` go through [`system_call`], so they work on Windows
/// too. The routines that reach `nvcuda.dll` exist only on Windows, and give 0 elsewhere, where
/// nothing calls them.
///
/// The table of each of the program's `records` shortest lists worth a launch, holding
/// `min_lengths` when the build measured the GPU
/// ([`min_length_table`](crate::codegen::gpu::min_length_table)), comes with
/// `stone.gpu_min_lengths_address`, and the planner's block with `stone.gpu_plan_address`.
pub fn gpu_sys_runtime(
    r#gen: &mut dyn AssemblyGenerator,
    labels: &[&str],
    records: usize,
    min_lengths: Option<&[i64]>,
) {
    let uses = |label: &str| labels.contains(&label);
    if uses("stone.gpu_get_slot") || uses("stone.gpu_set_slot") {
        r#gen.emit("\t.bss");
        r#gen.emit("\t.p2align\t3");
        r#gen.emit("stone.gpu_slot:");
        r#gen.emit("\t.zero\t8");
        r#gen.emit("\t.text");
    }
    if uses("stone.gpu_plan_address") {
        r#gen.emit("\t.bss");
        r#gen.emit("\t.p2align\t3");
        r#gen.emit("stone.gpu_plan:");
        r#gen.emit("\t.zero\t4096");
        r#gen.emit("\t.text");
    }
    if uses("stone.gpu_min_lengths_address") {
        for line in crate::codegen::gpu::min_length_table(records, min_lengths).lines() {
            r#gen.emit(line);
        }
    }
    let windows = r#gen.target().os == Os::Windows;
    if windows {
        super::gpu_windows::gpu_windows_runtime(r#gen, labels);
    }
    for &label in labels {
        if label == "stone.gpu_mmap" || label == "stone.gpu_munmap" {
            r#gen.emit(&format!("{label}:"));
            if label == "stone.gpu_mmap" {
                // the fourth argument of a system call goes in r10
                r#gen.emit("\tmov\tr10, rcx");
                system_call(r#gen, Sys::Mmap);
            } else {
                system_call(r#gen, Sys::Munmap);
            }
            r#gen.emit("\tret");
            continue;
        }
        if super::gpu_windows::ROUTINES.contains(&label) {
            if !windows {
                r#gen.emit(&format!("{label}:"));
                r#gen.emit("\txor\teax, eax");
                r#gen.emit("\tret");
            }
            continue;
        }
        let body: &[&str] = match label {
            "stone.gpu_open" => &[
                "\tmov\trsi, rdi",
                "\tmov\trdi, -100",   // AT_FDCWD
                "\tmov\tedx, 524290", // O_RDWR | O_CLOEXEC
                "\txor\tr10d, r10d",
                "\tmov\teax, 257", // sys_openat
                "\tsyscall",
            ],
            "stone.gpu_close" => &["\tmov\teax, 3", "\tsyscall"],
            "stone.gpu_ioctl" => &["\tmov\teax, 16", "\tsyscall"],

            "stone.gpu_peek32" => &["\tmov\teax, DWORD PTR [rdi]"],
            // a float travels as its bits, in the same registers as an int
            "stone.gpu_peek64" | "stone.gpu_peek_float" => &["\tmov\trax, QWORD PTR [rdi]"],
            "stone.gpu_poke32" => &["\tmov\tDWORD PTR [rdi], esi"],
            "stone.gpu_poke64" | "stone.gpu_poke_float" => &["\tmov\tQWORD PTR [rdi], rsi"],
            "stone.gpu_copy" => &["\tmov\trcx, rdx", "\trep\tmovsb"],
            "stone.gpu_fence" => &["\tmfence"],
            "stone.gpu_get_slot" => &["\tmov\trax, QWORD PTR [rip + stone.gpu_slot]"],
            "stone.gpu_set_slot" => &["\tmov\tQWORD PTR [rip + stone.gpu_slot], rdi"],
            "stone.gpu_image_address" => &["\tlea\trax, [rip + stone.gpu_image]"],
            "stone.gpu_plan_address" => &["\tlea\trax, [rip + stone.gpu_plan]"],
            "stone.gpu_min_lengths_address" => &["\tlea\trax, [rip + stone.gpu_min_len]"],
            _ => continue,
        };
        r#gen.emit(&format!("{label}:"));
        for line in body {
            r#gen.emit(line);
        }
        r#gen.emit("\tret");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::x64::X64Generator;

    #[test]
    fn the_gpu_sys_routines_make_raw_system_calls() {
        let mut generator = X64Generator::default();
        let labels = crate::codegen::context::MODULE_ROUTINES
            .iter()
            .map(|(_, label)| *label)
            .filter(|label| label.starts_with("stone.gpu_"))
            .collect::<Vec<_>>();
        assert_eq!(labels.len(), 21);
        gpu_sys_runtime(&mut generator, &labels, 2, None);
        let text = &generator.output;
        for piece in [
            "stone.gpu_open:\n\tmov\trsi, rdi\n\tmov\trdi, -100\n\tmov\tedx, 524290\n",
            "\tmov\teax, 257\n\tsyscall\n",
            "stone.gpu_ioctl:\n\tmov\teax, 16\n\tsyscall\n\tret\n",
            "stone.gpu_mmap:\n\tmov\tr10, rcx\n\tmov\teax, 9\n\tsyscall\n\tret\n",
            "stone.gpu_peek32:\n\tmov\teax, DWORD PTR [rdi]\n\tret\n",
            "stone.gpu_poke64:\n\tmov\tQWORD PTR [rdi], rsi\n\tret\n",
            "stone.gpu_copy:\n\tmov\trcx, rdx\n\trep\tmovsb\n\tret\n",
            "stone.gpu_fence:\n\tmfence\n\tret\n",
            "stone.gpu_get_slot:\n\tmov\trax, QWORD PTR [rip + stone.gpu_slot]\n",
            "stone.gpu_image_address:\n\tlea\trax, [rip + stone.gpu_image]\n",
            "stone.gpu_slot:\n\t.zero\t8\n",
        ] {
            assert!(text.contains(piece), "{piece} in {text}");
        }
        // the built-in assembler encodes every line
        crate::codegen::asm::assemble(text, crate::codegen::Architecture::X64).unwrap();
    }

    #[test]
    fn windows_maps_memory_through_its_shim_and_reaches_nvcuda() {
        let mut generator = X64Generator::for_target(crate::codegen::Target::X64_WINDOWS);
        let labels = ["stone.gpu_mmap", "stone.gpu_munmap", "stone.gpu_call"];
        gpu_sys_runtime(&mut generator, &labels, 0, None);
        let text = &generator.output;
        assert!(
            text.contains("stone.gpu_mmap:\n\tmov\tr10, rcx\n\tcall\tstone.sys_mmap\n\tret\n"),
            "{text}"
        );
        assert!(text.contains("\tcall\tstone.sys_munmap\n"), "{text}");
        assert!(text.contains(".Lgpu_call_go:\n\tcall\tr11\n"), "{text}");
        // Linux never calls nvcuda.dll, so its routines there give 0
        let mut generator = X64Generator::default();
        gpu_sys_runtime(&mut generator, &labels, 0, None);
        let text = &generator.output;
        assert!(
            text.contains("stone.gpu_call:\n\txor\teax, eax\n\tret\n"),
            "{text}"
        );
        assert!(text.contains("\tmov\teax, 9\n\tsyscall\n"), "{text}");
    }

    #[test]
    fn only_the_routines_called_are_emitted() {
        let mut generator = X64Generator::default();
        gpu_sys_runtime(&mut generator, &["stone.gpu_fence"], 0, None);
        let text = &generator.output;
        assert!(text.contains("stone.gpu_fence:"), "{text}");
        assert!(!text.contains("stone.gpu_open:"), "{text}");
        assert!(!text.contains("stone.gpu_slot:"), "{text}");
    }

    #[test]
    fn a_build_that_measured_the_gpu_bakes_in_the_shortest_lists() {
        let labels = ["stone.gpu_min_lengths_address"];
        let mut generator = X64Generator::default();
        gpu_sys_runtime(&mut generator, &labels, 2, Some(&[5000, i64::MAX]));
        let text = &generator.output;
        let table = "\t.data\n\t.p2align\t3\nstone.gpu_min_len:\nstone.gpu_min_len.0:\n\t.quad\t5000\n\
                     stone.gpu_min_len.1:\n\t.quad\t9223372036854775807\n";
        assert!(text.contains(table), "{text}");
        crate::codegen::asm::assemble(text, crate::codegen::Architecture::X64).unwrap();
        // without measurements, the program fills the table itself
        let mut generator = X64Generator::default();
        gpu_sys_runtime(&mut generator, &labels, 2, None);
        let text = &generator.output;
        assert!(
            text.contains(
                "\t.bss\n\t.p2align\t3\nstone.gpu_min_len:\nstone.gpu_min_len.0:\n\t.zero\t8\n"
            ),
            "{text}"
        );
    }
}
