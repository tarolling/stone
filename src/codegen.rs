//! Backend-independent pieces of code generation, plus the per-architecture backends.

pub mod arm64;
pub mod context;
pub mod ir;
pub mod regalloc;
pub mod x64;

use std::fmt;
use std::path::Path;
use std::process::Command;
use std::str::FromStr;

use crate::ast::Mod;

/// A processor that `stone build` can compile for, always running Linux.
///
/// For example, `"aarch64".parse::<Architecture>()` is `Ok(Architecture::Arm64)`, and it
/// displays as `aarch64` again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Architecture {
    X64,
    Arm64,
}

impl Architecture {
    /// Every architecture, in the order `--target` lists them.
    pub const ALL: [Architecture; 2] = [Architecture::X64, Architecture::Arm64];

    /// Returns the architecture stone itself was built for, which `stone build` targets unless
    /// told otherwise.
    pub fn host() -> Architecture {
        if cfg!(target_arch = "aarch64") {
            Architecture::Arm64
        } else {
            Architecture::X64
        }
    }

    /// Returns the compiler driver that assembles and links for this architecture: `gcc` for
    /// the host, and the Debian name of a cross compiler otherwise.
    ///
    /// For example, on an x86-64 machine `Architecture::Arm64.linker()` is
    /// `aarch64-linux-gnu-gcc`.
    pub fn linker(self) -> &'static str {
        if self == Architecture::host() {
            return "gcc";
        }
        match self {
            Architecture::X64 => "x86_64-linux-gnu-gcc",
            Architecture::Arm64 => "aarch64-linux-gnu-gcc",
        }
    }

    /// Returns a new code generator for this architecture.
    pub fn generator(self) -> Box<dyn AssemblyGenerator> {
        match self {
            Architecture::X64 => Box::new(x64::X64Generator::new()),
            Architecture::Arm64 => Box::new(arm64::Arm64Generator::new()),
        }
    }
}

impl fmt::Display for Architecture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Architecture::X64 => "x86_64",
            Architecture::Arm64 => "aarch64",
        })
    }
}

impl FromStr for Architecture {
    type Err = String;

    /// Parses a target name: `x86_64` or `x64`, and `aarch64` or `arm64`.
    fn from_str(name: &str) -> Result<Self, Self::Err> {
        match name {
            "x86_64" | "x64" => Ok(Architecture::X64),
            "aarch64" | "arm64" => Ok(Architecture::Arm64),
            _ => Err(format!(
                "unknown target '{name}' (expected {})",
                Architecture::ALL.map(|arch| arch.to_string()).join(" or ")
            )),
        }
    }
}

/// Writes `assembly` to `output` with a `.s` extension, then assembles and links it into the
/// executable `output` with the architecture's [`Architecture::linker`], against libc and libm
/// (the arm64 runtime's float `%` calls `fmod`).
///
/// For example, linking to `build/out` writes `build/out.s` and runs
/// `gcc -g -no-pie -o build/out build/out.s -lm`.
pub fn link(assembly: &str, output: &Path, arch: Architecture) -> std::io::Result<()> {
    let source = output.with_extension("s");
    if let Some(dir) = output.parent() {
        std::fs::create_dir_all(dir)?;
    }
    std::fs::write(&source, assembly)?;

    let linker = arch.linker();
    let status = Command::new(linker)
        .arg("-g")
        .arg("-no-pie")
        .arg("-o")
        .arg(output)
        .arg(&source)
        .arg("-lm")
        .status()
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                std::io::Error::other(format!(
                    "{linker} was not found; install it to build for {arch}"
                ))
            } else {
                std::io::Error::other(format!("could not run {linker}: {e}"))
            }
        })?;
    if !status.success() {
        return Err(std::io::Error::other(format!(
            "{linker} failed with {status}"
        )));
    }
    Ok(())
}

/// Returns the first assembler found on the system, checking `as`, `nasm`, and `yasm` in order.
pub fn find_assembler() -> Option<String> {
    for tool in &["as", "nasm", "yasm"] {
        if std::process::Command::new(tool)
            .arg("--version")
            .output()
            .is_ok()
        {
            return Some(tool.to_string());
        }
    }
    None
}

pub fn find_linker() -> Option<String> {
    for tool in &["gcc", "clang", "ld", "ld.lld"] {
        if Command::new(tool).arg("--version").output().is_ok() {
            return Some(tool.to_string());
        }
    }
    None
}

pub trait AssemblyGenerator {
    /// Compiles the module into an executable by scanning, generating, assembling, and linking it.
    ///
    /// For example, compiling to `build/out` writes the assembly to `build/out.s` and links `build/out`.
    fn compile(&mut self, module: &Mod, output: &Path) -> std::io::Result<()>;
    /// Checks the module, then runs both compilation passes and returns the assembly text,
    /// without assembling or linking.
    ///
    /// For example, assembling the module for `print(1)` returns text containing `main:` and a
    /// call to `stone.print_int`.
    fn assemble(&mut self, module: &Mod) -> Result<String, String>;
    /// Runs the first compilation pass, which lowers the module to [`ir`] functions.
    ///
    /// For example, scanning `def f(a); b = a + 1` lowers `f` to `v1 = add v0, 1`.
    fn scan(&mut self, module: &Mod) -> Result<(), String>;
    /// Runs the second compilation pass, which allocates registers for the functions lowered by
    /// [`Self::scan`] and emits their assembly.
    fn generate(&mut self, module: &Mod) -> Result<(), String>;
    fn emit(&mut self, code: &str);
    fn architecture(&self) -> Architecture;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn targets_parse_by_either_name_and_display_their_first() {
        for (name, arch) in [
            ("x86_64", Architecture::X64),
            ("x64", Architecture::X64),
            ("aarch64", Architecture::Arm64),
            ("arm64", Architecture::Arm64),
        ] {
            assert_eq!(name.parse(), Ok(arch));
        }
        assert_eq!(Architecture::Arm64.to_string(), "aarch64");
        assert_eq!(
            "sparc".parse::<Architecture>(),
            Err("unknown target 'sparc' (expected x86_64 or aarch64)".to_string())
        );
    }

    #[test]
    fn only_the_host_links_with_plain_gcc() {
        for arch in Architecture::ALL {
            let linker = arch.linker();
            assert_eq!(linker == "gcc", arch == Architecture::host(), "{arch}");
        }
    }
}
