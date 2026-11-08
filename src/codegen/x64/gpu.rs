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

use crate::codegen::AssemblyGenerator;

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
}
