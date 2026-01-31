use std::process::Command;

pub enum Architecture {
    X64,
}

/// finds a system
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
    fn emit(&mut self, code: &str);
    fn architecture(&self) -> Architecture;
}
