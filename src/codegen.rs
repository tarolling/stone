//! Backend-independent pieces of code generation, plus the per-architecture backends.

pub mod arm64;
pub mod asm;
pub mod context;
pub mod elf;
pub mod ir;
pub mod macho;
pub mod regalloc;
pub mod sha256;
pub mod x64;

use std::fmt;
use std::io::Write;
use std::path::Path;
use std::str::FromStr;

use crate::ast::Mod;

/// A processor that `stone build` can compile for.
///
/// For example, [`Architecture::Arm64`] displays as `aarch64`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Architecture {
    X64,
    Arm64,
}

impl Architecture {
    /// Every architecture, in the order `--target` lists them.
    pub const ALL: [Architecture; 2] = [Architecture::X64, Architecture::Arm64];

    /// Returns the architecture stone itself was built for.
    pub fn host() -> Architecture {
        if cfg!(target_arch = "aarch64") {
            Architecture::Arm64
        } else {
            Architecture::X64
        }
    }

    /// Returns a new code generator for this architecture running Linux.
    pub fn generator(self) -> Box<dyn AssemblyGenerator> {
        Target {
            arch: self,
            os: Os::Linux,
        }
        .generator()
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

/// An operating system that `stone build` can write executables for.
///
/// Linux programs make system calls themselves and are static ELF files. macOS has no stable
/// system call interface, so macOS programs are Mach-O files that call the system through
/// `libSystem`, the only interface Apple supports.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Os {
    #[default]
    Linux,
    MacOs,
}

/// The error for a target stone cannot build for: macOS on an Intel processor.
pub const INTEL_MACOS: &str =
    "stone build writes macOS programs only for arm64; pass --target aarch64-macos or x86_64";

/// What `stone build` compiles for: a processor and the operating system it runs.
///
/// For example, `"aarch64-macos".parse::<Target>()` is `Ok(Target::ARM64_MACOS)`, and a bare
/// processor name such as `aarch64` means Linux.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Target {
    pub arch: Architecture,
    pub os: Os,
}

impl Target {
    pub const X64_LINUX: Target = Target {
        arch: Architecture::X64,
        os: Os::Linux,
    };
    pub const ARM64_LINUX: Target = Target {
        arch: Architecture::Arm64,
        os: Os::Linux,
    };
    pub const ARM64_MACOS: Target = Target {
        arch: Architecture::Arm64,
        os: Os::MacOs,
    };

    /// Every target, in the order `--target` lists them.
    pub const ALL: [Target; 3] = [Target::X64_LINUX, Target::ARM64_LINUX, Target::ARM64_MACOS];

    /// Returns the machine stone itself runs on, which `stone build` targets unless told
    /// otherwise, or [`INTEL_MACOS`] on an Intel Mac.
    pub fn host() -> Result<Target, String> {
        let os = if cfg!(target_os = "macos") {
            Os::MacOs
        } else {
            Os::Linux
        };
        let target = Target {
            arch: Architecture::host(),
            os,
        };
        if Target::ALL.contains(&target) {
            Ok(target)
        } else {
            Err(INTEL_MACOS.to_string())
        }
    }

    /// Returns a new code generator for this target.
    pub fn generator(self) -> Box<dyn AssemblyGenerator> {
        match self.arch {
            Architecture::X64 => Box::new(x64::X64Generator::new()),
            Architecture::Arm64 => Box::new(arm64::Arm64Generator::for_os(self.os)),
        }
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.os {
            Os::Linux => write!(f, "{}", self.arch),
            Os::MacOs => write!(f, "{}-macos", self.arch),
        }
    }
}

impl FromStr for Target {
    type Err = String;

    /// Parses a target name: a processor (`x86_64` or `x64`, `aarch64` or `arm64`), which means
    /// Linux, optionally followed by `-linux` or `-macos`. `aarch64-apple-darwin`, the name Rust
    /// uses, means arm64 macOS too.
    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let (arch, os) = match name.split_once('-') {
            Some((arch, "linux")) => (arch, Os::Linux),
            Some((arch, "macos" | "apple-darwin")) => (arch, Os::MacOs),
            Some(_) => ("", Os::Linux),
            None => (name, Os::Linux),
        };
        let arch = match arch {
            "x86_64" | "x64" => Architecture::X64,
            "aarch64" | "arm64" => Architecture::Arm64,
            _ => {
                let names = Target::ALL.map(|target| target.to_string());
                let (last, rest) = names.split_last().unwrap();
                return Err(format!(
                    "unknown target '{name}' (expected {}, or {last})",
                    rest.join(", ")
                ));
            }
        };
        let target = Target { arch, os };
        if Target::ALL.contains(&target) {
            Ok(target)
        } else {
            Err(INTEL_MACOS.to_string())
        }
    }
}

/// Assembles `assembly` and links it into the bytes of an executable for `target`, named `name`
/// in a macOS program's signature.
///
/// For example, a program built for [`Target::ARM64_MACOS`] starts with the Mach-O magic
/// `cf fa ed fe`, and one built for Linux with `7f 45 4c 46` (`\x7fELF`).
pub fn executable(assembly: &str, target: Target, name: &str) -> Result<Vec<u8>, String> {
    let object = asm::assemble(assembly, target.arch)?;
    match target.os {
        Os::Linux => elf::link(&object, target.arch),
        Os::MacOs => macho::link(&object, name),
    }
}

/// Writes `assembly` to `output` with a `.s` extension, then assembles it with the built-in
/// assembler ([`asm`]) and links it with the built-in linker for `target`'s system ([`elf`] or
/// [`macho`]) into the executable `output`. A Linux program needs no C library, since its
/// runtime makes system calls itself and starts at its own `_start`, and a macOS program needs
/// only `libSystem`. Building either needs no assembler, linker, or C compiler.
///
/// For example, linking to `build/out` for Linux writes `build/out.s` and the static executable
/// `build/out`. The executable is written beside `output` first and then renamed over it, so a
/// copy of the old program that is still running keeps working.
pub fn link(assembly: &str, output: &Path, target: Target) -> std::io::Result<()> {
    let source = output.with_extension("s");
    if let Some(dir) = output.parent() {
        std::fs::create_dir_all(dir)?;
    }
    std::fs::write(&source, assembly)?;

    let name = output.file_name().ok_or_else(|| {
        std::io::Error::other(format!("'{}' is not a file name", output.display()))
    })?;
    let executable = executable(assembly, target, &name.to_string_lossy()).map_err(|e| {
        std::io::Error::other(format!(
            "internal error: cannot assemble {}: {e}",
            source.display()
        ))
    })?;
    let mut temporary = std::ffi::OsString::from(".");
    temporary.push(name);
    temporary.push(".tmp");
    let temporary = output.with_file_name(temporary);
    let _ = std::fs::remove_file(&temporary);
    let mut options = std::fs::OpenOptions::new();
    options.write(true).create_new(true);
    // executable by everyone the umask allows, as a linker's output is
    #[cfg(unix)]
    std::os::unix::fs::OpenOptionsExt::mode(&mut options, 0o777);
    let mut file = options.open(&temporary)?;
    file.write_all(&executable)?;
    drop(file);
    std::fs::rename(&temporary, output)
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
    /// Returns the processor and operating system this generator compiles for.
    fn target(&self) -> Target;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    #[test]
    fn targets_parse_by_any_name_and_display_their_first() {
        for (name, target) in [
            ("x86_64", Target::X64_LINUX),
            ("x64", Target::X64_LINUX),
            ("x86_64-linux", Target::X64_LINUX),
            ("aarch64", Target::ARM64_LINUX),
            ("arm64", Target::ARM64_LINUX),
            ("aarch64-linux", Target::ARM64_LINUX),
            ("aarch64-macos", Target::ARM64_MACOS),
            ("arm64-macos", Target::ARM64_MACOS),
            ("aarch64-apple-darwin", Target::ARM64_MACOS),
        ] {
            assert_eq!(name.parse(), Ok(target), "{name}");
        }
        let names = Target::ALL.map(|target| target.to_string());
        assert_eq!(names, ["x86_64", "aarch64", "aarch64-macos"]);
        assert_eq!(
            "sparc".parse::<Target>(),
            Err("unknown target 'sparc' (expected x86_64, aarch64, or aarch64-macos)".to_string())
        );
        assert_eq!(
            "x86_64-macos".parse::<Target>(),
            Err(INTEL_MACOS.to_string())
        );
    }

    #[test]
    fn the_host_target_is_this_machine() {
        let host = Target::host();
        if cfg!(all(target_os = "macos", target_arch = "x86_64")) {
            assert_eq!(host, Err(INTEL_MACOS.to_string()));
        } else {
            let host = host.unwrap();
            assert_eq!(host.arch, Architecture::host());
            assert_eq!(host.os == Os::MacOs, cfg!(target_os = "macos"));
        }
    }

    /// A program that calls every runtime routine: every builtin, method, and `os` function,
    /// plus float `%`, so its assembly holds the whole runtime.
    const EVERY_ROUTINE: &str = "\
use os
xs = [1.5, float(\" 2.5 \")]
xs.append(xs[0] % 0.5)
words = input(\"> \").strip().split()
words.append(str(xs[0]) + str(int(\"-7\")) + str(true))
parts = \"a,b\".split(\",\")
print(xs, words, parts, eof(), args(), words.len())
print(os.env(\"HOME\"), os.has_env(\"HOME\"), os.platform(), os.arch(), os.hostname())
print(os.cpu_count(), os.pid(), os.cwd(), os.time(), os.clock())
os.exit(0)
";

    /// Returns every symbol `assembly` jumps to, calls, or takes the address of, such as
    /// `stone.alloc` for `\tcall\tstone.alloc` or `stdin` for `[rip + stdin]`.
    fn referenced_symbols(assembly: &str) -> Vec<String> {
        let branches = [
            "call", "jmp", "bl", "b", "cbz", "cbnz", "tbz", "tbnz", "adrp", "adr",
        ];
        let mut symbols = Vec::new();
        for line in assembly.lines() {
            let mut fields = line.trim().splitn(2, '\t');
            let (Some(op), Some(operands)) = (fields.next(), fields.next()) else {
                continue;
            };
            let operands = operands.split(" //").next().unwrap();
            let jumps = op.starts_with('j') || op.starts_with("b.");
            if branches.contains(&op) || jumps {
                let target = operands.rsplit(", ").next().unwrap().trim();
                symbols.push(target.trim_start_matches(":got:").to_string());
            }
            for prefix in ["rip + ", ":got:", ":got_lo12:", ":lo12:"] {
                for piece in operands.split(prefix).skip(1) {
                    let end = piece.find([']', ',', ' ']).unwrap_or(piece.len());
                    symbols.push(piece[..end].to_string());
                }
            }
        }
        symbols
    }

    #[test]
    fn the_leak_check_reports_how_many_objects_were_never_freed() {
        let module = crate::driver::parse("xs = [\"a\"]\nprint(xs)\n").unwrap();
        let target = Target::host().unwrap();
        let assembly = target.generator().assemble(&module).unwrap();
        // pretends 12 objects were never freed
        let (call, leak) = match target.arch {
            Architecture::X64 => (
                "\tcall\tstone.leak_check",
                "\tadd\tQWORD PTR [rip + stone.live], 12\n",
            ),
            Architecture::Arm64 => (
                "\tbl\tstone.leak_check",
                "\tadrp\tx9, stone.live\n\tldr\tx10, [x9, :lo12:stone.live]\n\
                 \tadd\tx10, x10, #12\n\tstr\tx10, [x9, :lo12:stone.live]\n",
            ),
        };
        assert!(assembly.contains(call), "{assembly}");
        let leaky = assembly.replace(call, &format!("{leak}{call}"));
        let output = std::env::temp_dir().join(format!("stone-leak-{}", std::process::id()));
        link(&leaky, &output, target).unwrap();

        let checked = Command::new(&output)
            .env("STONE_LEAK_CHECK", "1")
            .output()
            .unwrap();
        assert_eq!(String::from_utf8_lossy(&checked.stdout), "['a']\n");
        assert_eq!(
            String::from_utf8_lossy(&checked.stderr),
            "error: 12 objects were never freed\n"
        );
        assert_eq!(checked.status.code(), Some(1));

        let unchecked = Command::new(&output)
            .env_remove("STONE_LEAK_CHECK")
            .output()
            .unwrap();
        assert_eq!(unchecked.stderr, b"");
        assert_eq!(unchecked.status.code(), Some(0));
        let _ = std::fs::remove_file(&output);
        let _ = std::fs::remove_file(output.with_extension("s"));
    }

    #[test]
    fn compiled_programs_need_nothing_but_their_own_routines_and_the_system() {
        let (_, module) = crate::driver::load(
            Path::new("main.st"),
            EVERY_ROUTINE,
            &crate::project::MapSources::default(),
        );
        let module = module.unwrap().0;
        let system: Vec<&str> = arm64::builtins::Sys::ALL
            .iter()
            .filter_map(|call| call.macos())
            .collect();
        let mut needs = Vec::new();
        for target in Target::ALL {
            let assembly = target.generator().assemble(&module).unwrap();
            let defined: std::collections::HashSet<&str> = assembly
                .lines()
                .filter_map(|line| line.strip_suffix(':'))
                .collect();
            let mut missing: Vec<String> = referenced_symbols(&assembly)
                .into_iter()
                .filter(|symbol| !defined.contains(symbol.as_str()))
                .filter(|symbol| target.os == Os::Linux || !system.contains(&symbol.as_str()))
                .collect();
            missing.sort();
            missing.dedup();
            if !missing.is_empty() {
                needs.push(format!("{target} needs {missing:?}"));
            }
            if let Err(e) = asm::assemble(&assembly, target.arch) {
                needs.push(format!("{target} does not assemble: {e}"));
            }
            if target.os == Os::MacOs && assembly.contains("\tsvc\t") {
                needs.push(format!("{target} makes a system call directly"));
            }
        }
        assert!(needs.is_empty(), "{}", needs.join("\n"));
    }
}
