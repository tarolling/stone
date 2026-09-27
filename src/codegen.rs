//! Backend-independent pieces of code generation, plus the per-architecture backends.

pub mod x64;

use std::path::Path;
use std::process::Command;

use crate::ast::Mod;

pub enum Architecture {
    X64,
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
    /// Runs the first compilation pass, which gathers stack offsets, scopes, functions, and string literals.
    ///
    /// For example, scanning `def f(a); b = a` records that `f` needs two stack slots.
    fn scan(&mut self, module: &Mod) -> Result<(), String>;
    /// Runs the second compilation pass, which emits assembly using the layout found by [`Self::scan`].
    fn generate(&mut self, module: &Mod) -> Result<(), String>;
    fn emit(&mut self, code: &str);
    fn architecture(&self) -> Architecture;
}
