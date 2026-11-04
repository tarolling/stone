//! Checks the PTX that `stone build --gpu` writes for every golden program.
//!
//! Every program's arithmetic on lists must become kernels that the PTX backend writes for
//! every NVIDIA GPU it supports. When NVIDIA's `ptxas` is on `PATH`, each module must also
//! compile with it for its GPU, the check the driver makes when it loads the module; without
//! it, that part is skipped with a note. `ptxas` comes with the CUDA toolkit, or from PyPI as
//! the `nvidia-cuda-nvcc-cu12` package (in `nvidia/cuda_nvcc/bin`), which needs no GPU.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use stone::codegen::Target;
use stone::codegen::device::Device;
use stone::codegen::gpu::{self, ptx};
use stone::project::FsSources;

const PROGRAM_DIRS: [&str; 4] = [
    "examples",
    "tests/programs",
    "docs/examples",
    "bench/programs",
];

/// The oldest, a middle, and the newest GPUs the backend writes PTX for.
const DEVICES: [&str; 3] = ["sm_50", "sm_75", "sm_120"];

/// Returns every `.stn` file directly in [`PROGRAM_DIRS`] and the `main.stn` of each of their
/// subdirectories, except a program's `.files` directory of fixtures.
fn programs() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut programs = Vec::new();
    for dir in PROGRAM_DIRS {
        for entry in std::fs::read_dir(root.join(dir)).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|ext| ext == "files")
                || path.join(stone::driver::MARKER).is_file()
            {
                continue;
            }
            if path.is_dir() {
                programs.push(path.join("main.stn"));
            } else if path.extension().is_some_and(|ext| ext == "stn") {
                programs.push(path);
            }
        }
    }
    programs.sort();
    programs
}

/// Returns the PTX module of `program` for each of [`DEVICES`], or nothing if it has no
/// arithmetic on lists.
fn modules(program: &Path) -> Result<Vec<(Device, String)>, String> {
    let source = std::fs::read_to_string(program).unwrap();
    let (_, loaded) = stone::driver::load(program, &source, &FsSources);
    let module = loaded.map_err(|e| format!("does not check: {e:?}"))?.module;
    let mut generator = Target::X64_LINUX.generator();
    generator.assemble(&module)?;
    let kernels = gpu::kernels(generator.device_kernels())?;
    if kernels.is_empty() {
        return Ok(Vec::new());
    }
    DEVICES
        .iter()
        .map(|name| {
            let device: Device = name.parse()?;
            Ok((device, ptx::module(&kernels, device)?))
        })
        .collect()
}

/// Returns whether `ptxas` runs.
fn has_ptxas() -> bool {
    Command::new("ptxas")
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}

#[test]
fn every_programs_kernels_are_ptx_that_ptxas_compiles() {
    let ptxas = has_ptxas();
    if !ptxas {
        eprintln!("skipped compiling PTX with ptxas: it was not found");
    }
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("gpu");
    std::fs::create_dir_all(&dir).unwrap();
    let (mut failures, mut compiled) = (Vec::new(), 0);
    for program in programs() {
        let modules = match modules(&program) {
            Ok(modules) => modules,
            Err(error) => {
                failures.push(format!("{}: {error}", program.display()));
                continue;
            }
        };
        for (device, text) in modules {
            if !ptxas {
                continue;
            }
            let stem = program.with_extension("");
            let name = stem.file_name().unwrap().to_string_lossy();
            let file = dir.join(format!("{name}.{device}.ptx"));
            std::fs::write(&file, text).unwrap();
            let output = Command::new("ptxas")
                .arg(format!("-arch={device}"))
                .arg(&file)
                .arg("-o")
                .arg(file.with_extension("cubin"))
                .output()
                .unwrap();
            match output.status.success() {
                true => compiled += 1,
                false => failures.push(format!(
                    "{} for {device}: {}",
                    program.display(),
                    String::from_utf8_lossy(&output.stderr)
                )),
            }
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
    if ptxas {
        assert!(compiled > 0, "no program has arithmetic on lists");
    }
}
