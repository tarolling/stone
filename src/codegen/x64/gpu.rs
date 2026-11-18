//! The runtime that runs a kernel on a GPU, on every system but Windows, which has its own in
//! [`gpu_windows`](super::gpu_windows).
//!
//! `stone.gpu_launch` takes the number of a piece of arithmetic in `rdi`, as
//! `Program::device_kernels` numbers them (the image at `stone.gpu_image` maps it to a kernel, see
//! [`image`](crate::codegen::gpu::image)), and in `rsi` a list of ints holding what the kernel
//! takes: the address of the result's elements, their count, then the address of each list's
//! elements and the bits of each number the kernel's arithmetic names, left to right. It gives 1
//! in `rax` if the GPU computed the result, and 0 if the processor must, which every caller then
//! does.
//!
//! Linux programs reach no GPU yet, so it always gives 0.
//!
//! [`gpu_sys_runtime`] emits the routines behind the internal `gpu_sys` module, the thin native
//! help that `gpu_nv.stn` drives an NVIDIA GPU through on Linux: raw system calls, reads and
//! writes of memory the driver maps, and a slot in `.bss` for its state.

use crate::codegen::AssemblyGenerator;

/// Emits the `gpu_sys` routines among `labels`, each taking its arguments as a System V call
/// does and giving its result in `rax`. The system calls give a negative error number on failure,
/// as Linux returns it.
///
/// The table of each of the program's `records` shortest lists worth a launch comes with
/// `stone.gpu_min_lengths_address`, and the planner's block with `stone.gpu_plan_address`.
pub fn gpu_sys_runtime(r#gen: &mut dyn AssemblyGenerator, labels: &[&str], records: usize) {
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
        for line in crate::codegen::gpu::min_length_table(records).lines() {
            r#gen.emit(line);
        }
    }
    for &label in labels {
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
            // the fourth argument of a system call goes in r10
            "stone.gpu_mmap" => &["\tmov\tr10, rcx", "\tmov\teax, 9", "\tsyscall"],
            "stone.gpu_munmap" => &["\tmov\teax, 11", "\tsyscall"],
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

/// Emits `stone.gpu_launch`.
pub fn gpu_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.gpu_launch:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::x64::X64Generator;

    #[test]
    fn a_launch_that_reaches_no_gpu_gives_zero() {
        let mut generator = X64Generator::default();
        gpu_runtime(&mut generator);
        let text = &generator.output;
        assert!(
            text.contains("stone.gpu_launch:\n\txor\teax, eax\n\tret\n"),
            "{text}"
        );
    }

    #[test]
    fn the_gpu_sys_routines_make_raw_system_calls() {
        let mut generator = X64Generator::default();
        let labels = crate::codegen::context::MODULE_ROUTINES
            .iter()
            .map(|(_, label)| *label)
            .filter(|label| label.starts_with("stone.gpu_"))
            .collect::<Vec<_>>();
        assert_eq!(labels.len(), 18);
        gpu_sys_runtime(&mut generator, &labels, 2);
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
    fn only_the_routines_called_are_emitted() {
        let mut generator = X64Generator::default();
        gpu_sys_runtime(&mut generator, &["stone.gpu_fence"], 0);
        let text = &generator.output;
        assert!(text.contains("stone.gpu_fence:"), "{text}");
        assert!(!text.contains("stone.gpu_open:"), "{text}");
        assert!(!text.contains("stone.gpu_slot:"), "{text}");
    }
}
