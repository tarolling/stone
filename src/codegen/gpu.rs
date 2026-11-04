//! Code for GPUs: kernels that compute a program's arithmetic on lists, one element per thread.
//!
//! Lowering records each piece of arithmetic on lists that a GPU could compute
//! ([`DeviceKernel`](crate::codegen::ir::DeviceKernel)), [`kernels`] turns them into the device
//! [`ir`], and [`ptx`] writes that as PTX for an NVIDIA GPU. [`compile`] turns the PTX into
//! machine code with `ptxas`, and [`cubin`] reads the kernels out of it. Nothing runs the kernels
//! yet: `stone build --gpu sm_75` writes them beside the program, which still computes everything
//! on the processor.

pub mod compile;
pub mod cubin;
#[cfg(test)]
mod eval;
pub mod ir;
pub mod kernel;
pub mod ptx;

pub use kernel::kernels;
