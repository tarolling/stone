//! Code for GPUs: kernels that compute a program's arithmetic on lists, one element per thread.
//!
//! Lowering records each piece of arithmetic on lists that a GPU could compute
//! ([`DeviceKernel`](crate::codegen::ir::DeviceKernel)), [`kernels`] turns them into the device
//! [`ir`], and [`ptx`] writes that as PTX for an NVIDIA GPU. [`compile`] turns the PTX into
//! machine code with `ptxas`, and [`cubin`] reads the kernels out of it. [`image`] lays the machine
//! code and what a launch needs to know about each kernel out as one table that a Linux program
//! carries. A Windows program runs the kernels through the GPU's driver (see
//! `x64::gpu_windows`), giving up for the processor whenever it cannot; elsewhere nothing runs
//! them yet, and the program computes everything on the processor.

pub mod compile;
pub mod cubin;
#[cfg(test)]
mod eval;
pub mod image;
pub mod ir;
pub mod kernel;
pub mod ptx;

pub use kernel::{kernels, numbered};

/// The environment variable that says whether a program runs its kernels on a GPU: `off` for
/// never, `require` for always, failing with one of the errors below when it cannot, and else
/// whenever it can.
pub const VARIABLE: &str = "STONE_GPU";

/// The error of a program run with `STONE_GPU=require` on a machine without NVIDIA's driver.
pub const NO_DRIVER: &str = "no GPU ran this program's kernels: nvcuda.dll is missing or too old";

/// The error of a program run with `STONE_GPU=require` on a machine with NVIDIA's driver and no
/// GPU it can use.
pub const NO_DEVICE: &str = "no GPU ran this program's kernels: there is no CUDA device";

/// The error of a program run with `STONE_GPU=require` on a GPU it carries no code for, with its
/// compute capability's major and minor versions in the holes.
pub const NO_CODE: &str =
    "no GPU ran this program's kernels: it has no code for compute capability {}.{}";

/// The error of a program run with `STONE_GPU=require` whose GPU's driver failed, with CUDA's
/// error number in the hole.
pub const CUDA_ERROR: &str = "no GPU ran this program's kernels: CUDA error {}";

/// The error of a program run with `STONE_GPU=require` with a kernel that takes more values
/// than a launch passes.
pub const TOO_MANY_VALUES: &str =
    "no GPU ran this program's kernels: a kernel takes more than 64 values";
