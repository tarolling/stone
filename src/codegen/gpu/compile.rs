//! Compiles PTX to a cubin with NVIDIA's `ptxas`.
//!
//! A program that talks to the GPU's kernel driver itself has no driver compiler to hand it PTX,
//! so `stone build --gpu` compiles each module to machine code first and embeds the result.
//! `ptxas` comes with the CUDA toolkit, or from PyPI as the `nvidia-cuda-nvcc-cu12` package (in
//! `nvidia/cuda_nvcc/bin`), which needs no GPU. `STONE_PTXAS` names another path for it.

use crate::codegen::device::Device;
use std::fmt;
use std::path::PathBuf;
use std::process::Command;

/// Why PTX did not become a cubin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CompileError {
    /// There is no `ptxas` to run, so the program cannot carry machine code for the GPU, but
    /// nothing is wrong with the kernels.
    Missing(String),
    /// `ptxas` ran and refused the kernels, or the files it works with could not be used.
    Failed(String),
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (CompileError::Missing(message) | CompileError::Failed(message)) = self;
        f.write_str(message)
    }
}

impl std::error::Error for CompileError {}

/// The environment variable that names the `ptxas` to run instead of the one on `PATH`.
pub const PTXAS_VARIABLE: &str = "STONE_PTXAS";

/// Returns the cubin `ptxas` compiles `ptx` into for `device`.
///
/// For example, the PTX of a module for `sm_75` gives the bytes of an ELF file whose kernels
/// [`cubin::parse`](super::cubin::parse) reads. A `ptxas` that is missing is
/// [`CompileError::Missing`] (`cannot run 'ptxas' ...`), and one that rejects the PTX is
/// [`CompileError::Failed`] with its own message.
pub fn compile_cubin(ptx: &str, device: Device) -> Result<Vec<u8>, CompileError> {
    let program = std::env::var_os(PTXAS_VARIABLE).unwrap_or_else(|| "ptxas".into());
    let dir = scratch_dir().map_err(CompileError::Failed)?;
    let result = run(&program, &dir, ptx, device);
    let _ = std::fs::remove_dir_all(&dir);
    result
}

fn run(
    program: &std::ffi::OsStr,
    dir: &std::path::Path,
    ptx: &str,
    device: Device,
) -> Result<Vec<u8>, CompileError> {
    let (input, output) = (dir.join("kernels.ptx"), dir.join("kernels.cubin"));
    std::fs::write(&input, ptx)
        .map_err(|error| CompileError::Failed(format!("cannot write the PTX: {error}")))?;
    let ran = Command::new(program)
        .arg(format!("--gpu-name={device}"))
        .arg("-O3")
        .arg("-o")
        .arg(&output)
        .arg(&input)
        .output()
        .map_err(|error| {
            CompileError::Missing(format!(
                "cannot run '{}' to compile the kernels for {device}: {error} (install the CUDA \
                 toolkit, or `pip install nvidia-cuda-nvcc-cu12` and put its \
                 nvidia/cuda_nvcc/bin on PATH, or set {PTXAS_VARIABLE})",
                program.to_string_lossy()
            ))
        })?;
    if !ran.status.success() {
        return Err(CompileError::Failed(format!(
            "ptxas rejected the kernels for {device}: {}",
            String::from_utf8_lossy(&ran.stderr).trim()
        )));
    }
    std::fs::read(&output)
        .map_err(|error| CompileError::Failed(format!("ptxas wrote no cubin: {error}")))
}

/// Makes an empty directory of its own for one compilation, so two builds never share files.
fn scratch_dir() -> Result<PathBuf, String> {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNT: AtomicUsize = AtomicUsize::new(0);
    let dir = std::env::temp_dir().join(format!(
        "stone-ptxas-{}-{}",
        std::process::id(),
        COUNT.fetch_add(1, Ordering::Relaxed)
    ));
    std::fs::create_dir_all(&dir)
        .map_err(|error| format!("cannot make '{}': {error}", dir.display()))?;
    Ok(dir)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::gpu::cubin;

    fn has_ptxas() -> bool {
        Command::new("ptxas")
            .arg("--version")
            .output()
            .is_ok_and(|output| output.status.success())
    }

    #[test]
    fn the_cubin_of_a_module_has_its_kernels() {
        if !has_ptxas() {
            eprintln!("skipped compiling PTX with ptxas: it was not found");
            return;
        }
        let ptx = include_str!("testdata/two_kernels.sm_75.ptx");
        let device: Device = "sm_75".parse().unwrap();
        let kernels = cubin::parse(&compile_cubin(ptx, device).unwrap()).unwrap();
        let names: Vec<&str> = kernels.iter().map(|kernel| kernel.name.as_str()).collect();
        assert_eq!(names.len(), 2);
        assert!(names.contains(&"stone_k0") && names.contains(&"stone_k1"));
    }

    #[test]
    fn ptx_that_ptxas_rejects_is_an_error_with_its_message() {
        if !has_ptxas() {
            eprintln!("skipped compiling PTX with ptxas: it was not found");
            return;
        }
        let device: Device = "sm_75".parse().unwrap();
        let error = compile_cubin(".version 6.3\n.target sm_75\nnonsense\n", device).unwrap_err();
        assert!(matches!(error, CompileError::Failed(_)), "{error}");
        assert!(
            error
                .to_string()
                .starts_with("ptxas rejected the kernels for sm_75: ")
        );
    }

    #[test]
    fn a_missing_ptxas_says_how_to_get_one() {
        let device: Device = "sm_75".parse().unwrap();
        let dir = scratch_dir().unwrap();
        let error = run(std::ffi::OsStr::new("/nonexistent/ptxas"), &dir, "", device).unwrap_err();
        let _ = std::fs::remove_dir_all(&dir);
        assert!(matches!(error, CompileError::Missing(_)), "{error}");
        let text = error.to_string();
        assert!(
            text.starts_with("cannot run '/nonexistent/ptxas'"),
            "{text}"
        );
        assert!(text.contains("nvidia-cuda-nvcc-cu12"), "{text}");
    }
}
