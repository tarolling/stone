//! [`gpu_sys_runtime`] emits the routines behind the internal `gpu_sys` module on Linux, as
//! [`x64::gpu::gpu_sys_runtime`](crate::codegen::x64::gpu::gpu_sys_runtime) does.

use crate::codegen::AssemblyGenerator;

/// Emits the `gpu_sys` routines among `labels`, each taking its arguments in `x0`-`x5` and giving
/// its result in `x0`. The system calls give a negative error number on failure, as Linux returns
/// it, so these routines are for Linux only.
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
                "\tmov\tx1, x0",
                "\tmov\tx0, #-100", // AT_FDCWD
                "\tmov\tx2, #2",    // O_RDWR | O_CLOEXEC
                "\tmovk\tx2, #8, lsl #16",
                "\tmov\tx3, #0",
                "\tmov\tx8, #56", // openat
                "\tsvc\t#0",
            ],
            "stone.gpu_close" => &["\tmov\tx8, #57", "\tsvc\t#0"],
            "stone.gpu_ioctl" => &["\tmov\tx8, #29", "\tsvc\t#0"],
            "stone.gpu_mmap" => &["\tmov\tx8, #222", "\tsvc\t#0"],
            "stone.gpu_munmap" => &["\tmov\tx8, #215", "\tsvc\t#0"],
            "stone.gpu_peek32" => &["\tldr\tw0, [x0]"],
            // a float travels as its bits, in the same registers as an int
            "stone.gpu_peek64" | "stone.gpu_peek_float" => &["\tldr\tx0, [x0]"],
            "stone.gpu_poke32" => &["\tstr\tw1, [x0]"],
            "stone.gpu_poke64" | "stone.gpu_poke_float" => &["\tstr\tx1, [x0]"],
            "stone.gpu_copy" => {
                r#gen.emit(&format!("{label}:"));
                super::builtins::copy_bytes(r#gen, "x0", "x1", "x2", ".Lgpu_copy");
                r#gen.emit("\tret");
                continue;
            }
            "stone.gpu_fence" => &["\tdsb\tsy"],
            "stone.gpu_get_slot" => &[
                "\tadrp\tx16, stone.gpu_slot",
                "\tldr\tx0, [x16, :lo12:stone.gpu_slot]",
            ],
            "stone.gpu_set_slot" => &[
                "\tadrp\tx16, stone.gpu_slot",
                "\tstr\tx0, [x16, :lo12:stone.gpu_slot]",
            ],
            "stone.gpu_image_address" => &[
                "\tadrp\tx0, stone.gpu_image",
                "\tadd\tx0, x0, :lo12:stone.gpu_image",
            ],
            "stone.gpu_plan_address" => &[
                "\tadrp\tx0, stone.gpu_plan",
                "\tadd\tx0, x0, :lo12:stone.gpu_plan",
            ],
            "stone.gpu_min_lengths_address" => &[
                "\tadrp\tx0, stone.gpu_min_len",
                "\tadd\tx0, x0, :lo12:stone.gpu_min_len",
            ],
            // only an x86-64 Windows program reaches nvcuda.dll
            "stone.gpu_nvcuda" | "stone.gpu_symbol" | "stone.gpu_call" => &["\tmov\tx0, #0"],
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
    use crate::codegen::arm64::Arm64Generator;

    #[test]
    fn the_gpu_sys_routines_make_raw_system_calls() {
        let mut generator = Arm64Generator::default();
        let labels = crate::codegen::context::MODULE_ROUTINES
            .iter()
            .map(|(_, label)| *label)
            .filter(|label| label.starts_with("stone.gpu_"))
            .collect::<Vec<_>>();
        assert_eq!(labels.len(), 21);
        gpu_sys_runtime(&mut generator, &labels, 2);
        let text = &generator.output;
        for piece in [
            "stone.gpu_open:\n\tmov\tx1, x0\n\tmov\tx0, #-100\n",
            "\tmov\tx8, #56\n\tsvc\t#0\n\tret\n",
            "stone.gpu_ioctl:\n\tmov\tx8, #29\n\tsvc\t#0\n\tret\n",
            "stone.gpu_mmap:\n\tmov\tx8, #222\n\tsvc\t#0\n\tret\n",
            "stone.gpu_munmap:\n\tmov\tx8, #215\n\tsvc\t#0\n\tret\n",
            "stone.gpu_close:\n\tmov\tx8, #57\n\tsvc\t#0\n\tret\n",
            "stone.gpu_peek32:\n\tldr\tw0, [x0]\n\tret\n",
            "stone.gpu_poke64:\n\tstr\tx1, [x0]\n\tret\n",
            "stone.gpu_fence:\n\tdsb\tsy\n\tret\n",
            "stone.gpu_get_slot:\n\tadrp\tx16, stone.gpu_slot\n",
            "stone.gpu_image_address:\n\tadrp\tx0, stone.gpu_image\n",
            "stone.gpu_slot:\n\t.zero\t8\n",
        ] {
            assert!(text.contains(piece), "{piece} in {text}");
        }
        // the built-in assembler encodes every line
        crate::codegen::asm::assemble(text, crate::codegen::Architecture::Arm64).unwrap();
    }

    #[test]
    fn only_the_routines_called_are_emitted() {
        let mut generator = Arm64Generator::default();
        gpu_sys_runtime(&mut generator, &["stone.gpu_fence"], 0);
        let text = &generator.output;
        assert!(text.contains("stone.gpu_fence:"), "{text}");
        assert!(!text.contains("stone.gpu_open:"), "{text}");
        assert!(!text.contains("stone.gpu_slot:"), "{text}");
    }
}
