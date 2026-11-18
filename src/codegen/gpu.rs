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
pub mod cost;
pub mod cubin;
#[cfg(test)]
mod eval;
pub mod image;
pub mod ir;
pub mod kernel;
pub mod ptx;

pub use kernel::{kernels, numbered};

/// Whether and how a build runs its arithmetic on lists on a GPU.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Launch {
    /// Every piece of arithmetic is computed on the processor, with no call to try a GPU.
    #[default]
    Never,
    /// Each piece of arithmetic a GPU could compute first tries one.
    Device,
    /// As `Device`, but the runtime computes each kernel on the processor from its bytecode, as
    /// if a GPU had, so tests reach every path a GPU takes on a machine without one. A Linux
    /// build made with [`SIMULATE`] set asks for it, and `gpu_sys.simulated()` is a constant
    /// that tells the planner.
    Simulated,
}

impl Launch {
    /// Returns whether arithmetic first tries a GPU, real or simulated.
    pub fn launches(self) -> bool {
        self != Launch::Never
    }
}

/// The environment variable that makes a Linux `stone build --gpu` simulate its GPU, for tests.
pub const SIMULATE: &str = "STONE_GPU_SIMULATE";

/// The label of the table of each record's shortest list worth a launch, 8 bytes per record,
/// which `gpu_sys.min_lengths()` gives the address of.
pub const MIN_LENGTHS: &str = "stone.gpu_min_len";

/// Returns the label of record `record`'s entry of the [`MIN_LENGTHS`] table.
///
/// For example, `min_length_label(2)` is `stone.gpu_min_len.2`.
pub fn min_length_label(record: u32) -> String {
    format!("{MIN_LENGTHS}.{record}")
}

/// Returns the `.bss` text of the [`MIN_LENGTHS`] table of `records` records, each entry
/// labeled by [`min_length_label`] and zero until the runtime fills it.
///
/// For example, two records give the table's label, then `stone.gpu_min_len.0:` and
/// `stone.gpu_min_len.1:`, each before 8 zero bytes.
pub fn min_length_table(records: usize) -> String {
    let mut text = format!("\t.bss\n\t.p2align\t3\n{MIN_LENGTHS}:\n");
    for record in 0..records {
        text.push_str(&format!(
            "{}:\n\t.zero\t8\n",
            min_length_label(record as u32)
        ));
    }
    if records == 0 {
        text.push_str("\t.zero\t8\n");
    }
    text.push_str("\t.text\n");
    text
}

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

/// The error of a Linux program run with `STONE_GPU=require` on a machine without NVIDIA's
/// kernel driver, or with a version of it stone does not speak.
pub const NO_LINUX_DRIVER: &str =
    "no GPU ran this program's kernels: NVIDIA's driver is missing or unsupported";

/// The error of a Linux program run with `STONE_GPU=require` whose GPU's driver refused a
/// request, with the driver's status or the system's error number in the hole.
pub const DRIVER_ERROR: &str = "no GPU ran this program's kernels: the driver failed (status {})";

/// The error of a Linux program run with `STONE_GPU=require` whose GPU did not finish a kernel
/// in time.
pub const TIMED_OUT: &str = "no GPU ran this program's kernels: the GPU did not finish in time";

/// The error of a Linux program run with `STONE_GPU=require` whose GPU failed the test the
/// planner runs when it starts the GPU: a submission that did nothing took too long, or moving
/// a list to it was too slow.
pub const PROBE_FAILED: &str =
    "no GPU ran this program's kernels: the GPU failed its test at startup";
