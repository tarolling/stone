//! The runtime that runs a kernel on a GPU, as [`x64::gpu`](crate::codegen::x64::gpu) does.
//!
//! `stone.gpu_launch` takes the number of a piece of arithmetic in `x0`, which the image at
//! `stone.gpu_image` maps to a kernel, and the list of what it takes in `x1`,
//! and gives 1 in `x0` if the GPU computed the result and 0 if the processor must. No GPU is
//! reachable yet, so it always gives 0.

use crate::codegen::AssemblyGenerator;

/// Emits `stone.gpu_launch`.
pub fn gpu_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.gpu_launch:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::arm64::Arm64Generator;

    #[test]
    fn a_launch_that_reaches_no_gpu_gives_zero() {
        let mut generator = Arm64Generator::default();
        gpu_runtime(&mut generator);
        let text = &generator.output;
        assert!(
            text.contains("stone.gpu_launch:\n\tmov\tx0, #0\n\tret\n"),
            "{text}"
        );
    }
}
