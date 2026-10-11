//! Backend-independent pieces of code generation, plus the per-architecture backends.

pub mod arm64;
pub mod asm;
pub mod context;
pub mod device;
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

    /// Returns the level every processor of this architecture reaches, which a target name
    /// without a level means.
    ///
    /// For example, `Architecture::X64.baseline()` is x86-64 `v1` and
    /// `Architecture::Arm64.baseline()` is Armv8.0 (`v8`).
    pub const fn baseline(self) -> Level {
        match self {
            Architecture::X64 => Level { major: 1, minor: 0 },
            Architecture::Arm64 => Level { major: 8, minor: 0 },
        }
    }

    /// Returns whether `level` names a level of this architecture: x86-64 `v1` through `v4`
    /// (the psABI microarchitecture levels), or Arm `v8` through `v8.9` and `v9` through `v9.5`.
    ///
    /// For example, x86-64 has `v3` but no `v3.1`, and Arm has `v8.2` but no `v7`.
    fn has_level(self, level: Level) -> bool {
        match self {
            Architecture::X64 => (1..=4).contains(&level.major) && level.minor == 0,
            Architecture::Arm64 => match level.major {
                8 => level.minor <= 9,
                9 => level.minor <= 5,
                _ => false,
            },
        }
    }

    /// Returns a new code generator for this architecture running Linux.
    pub fn generator(self) -> Box<dyn AssemblyGenerator> {
        Target {
            arch: self,
            os: Os::Linux,
            level: self.baseline(),
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

/// A level of an architecture: the processor features a program may assume, beyond those of
/// every processor of the architecture (its [baseline](Architecture::baseline)).
///
/// It is written after the architecture in a target name, such as `x86_64v3-linux` (x86-64 with
/// AVX2, `major: 3, minor: 0`) or `aarch64v8.2-macos` (Armv8.2, `major: 8, minor: 2`). A level
/// lets the compiler use those features, but never requires it: code generation emits baseline
/// code for now, which runs on every processor of the architecture.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Level {
    pub major: u8,
    pub minor: u8,
}

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.minor {
            0 => write!(f, "v{}", self.major),
            minor => write!(f, "v{}.{minor}", self.major),
        }
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
    "stone build writes macOS programs only for arm64; pass --target aarch64-macos or x86_64-linux";

/// What `stone build` compiles for: a processor, its [`Level`], and the operating system it
/// runs, named `<arch>[<level>]-<os>`.
///
/// Compiled programs use no C library, so a target names no vendor or C environment as LLVM's
/// triples do, though Rust's names for the same machines parse too. A GPU is never part of a
/// target: a program that runs code on GPUs will be built for one target plus a device target per
/// GPU, since one program may carry code for several.
///
/// For example, `"aarch64-macos".parse::<Target>()` is `Ok(Target::ARM64_MACOS)`, which displays
/// as `aarch64-macos`; `x86_64v3-linux` is x86-64 Linux at level `v3`; and a bare processor name
/// such as `aarch64` means Linux at the baseline level.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Target {
    pub arch: Architecture,
    pub os: Os,
    pub level: Level,
}

impl Target {
    pub const X64_LINUX: Target = Target {
        arch: Architecture::X64,
        os: Os::Linux,
        level: Architecture::X64.baseline(),
    };
    pub const ARM64_LINUX: Target = Target {
        arch: Architecture::Arm64,
        os: Os::Linux,
        level: Architecture::Arm64.baseline(),
    };
    pub const ARM64_MACOS: Target = Target {
        arch: Architecture::Arm64,
        os: Os::MacOs,
        level: Architecture::Arm64.baseline(),
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
        let arch = Architecture::host();
        Target {
            arch,
            os,
            level: arch.baseline(),
        }
        .supported()
    }

    /// Returns this target if `stone build` can write programs for its processor and system,
    /// at any level, or [`INTEL_MACOS`] otherwise.
    fn supported(self) -> Result<Target, String> {
        if Target::ALL
            .iter()
            .any(|target| (target.arch, target.os) == (self.arch, self.os))
        {
            Ok(self)
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
        write!(f, "{}", self.arch)?;
        if self.level != self.arch.baseline() {
            write!(f, "{}", self.level)?;
        }
        match self.os {
            Os::Linux => f.write_str("-linux"),
            Os::MacOs => f.write_str("-macos"),
        }
    }
}

impl FromStr for Target {
    type Err = String;

    /// Parses a target name: a processor (`x86_64` or `x64`, `aarch64` or `arm64`), optionally
    /// followed by a [`Level`] such as `v3` or `v8.2`, then optionally by `-linux` or `-macos`
    /// (without a system, the target is Linux). Rust's names for the same machines, such as
    /// `x86_64-unknown-linux-gnu` and `aarch64-apple-darwin`, parse too.
    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let unknown = || {
            let names = Target::ALL.map(|target| target.to_string());
            let (last, rest) = names.split_last().unwrap();
            format!(
                "unknown target '{name}' (expected {}, or {last}, with an optional level such \
                 as x86_64v3-linux)",
                rest.join(", ")
            )
        };
        let (processor, os) = match name.split_once('-') {
            None => (name, Os::Linux),
            Some((processor, system)) => match system {
                "linux" | "linux-gnu" | "linux-musl" | "unknown-linux-gnu"
                | "unknown-linux-musl" => (processor, Os::Linux),
                "macos" | "apple-darwin" => (processor, Os::MacOs),
                _ => return Err(unknown()),
            },
        };
        let (arch, level) = ["x86_64", "x64", "aarch64", "arm64"]
            .into_iter()
            .find_map(|prefix| {
                let level = processor.strip_prefix(prefix)?;
                (level.is_empty() || level.starts_with('v')).then_some((prefix, level))
            })
            .ok_or_else(unknown)?;
        let arch = match arch {
            "x86_64" | "x64" => Architecture::X64,
            _ => Architecture::Arm64,
        };
        let level = match level {
            "" => arch.baseline(),
            level => parse_level(level)
                .filter(|&parsed| arch.has_level(parsed))
                .ok_or_else(|| {
                    let expected = match arch {
                        Architecture::X64 => "v1, v2, v3, or v4",
                        Architecture::Arm64 => "v8, v8.1 to v8.9, v9, or v9.1 to v9.5",
                    };
                    format!("unknown level '{level}' for {arch} (expected {expected})")
                })?,
        };
        Target { arch, os, level }.supported()
    }
}

/// Parses a level such as `v3` or `v8.2`, whose numbers have no leading zeros, or returns
/// `None`.
///
/// For example, `parse_level("v8.2")` is `Some(Level { major: 8, minor: 2 })`, and `"v08"` and
/// `"v8."` are `None`.
fn parse_level(text: &str) -> Option<Level> {
    let number = |digits: &str| {
        let canonical = !digits.is_empty()
            && digits.bytes().all(|b| b.is_ascii_digit())
            && (digits == "0" || !digits.starts_with('0'));
        canonical.then(|| digits.parse::<u8>().ok()).flatten()
    };
    let text = text.strip_prefix('v')?;
    let (major, minor) = match text.split_once('.') {
        Some((major, minor)) => (number(major)?, number(minor)?),
        None => (number(text)?, 0),
    };
    Some(Level { major, minor })
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
    fn targets_parse_by_any_name_and_display_their_canonical_one() {
        for (name, target) in [
            ("x86_64", Target::X64_LINUX),
            ("x64", Target::X64_LINUX),
            ("x86_64-linux", Target::X64_LINUX),
            ("x86_64-unknown-linux-gnu", Target::X64_LINUX),
            ("x86_64-unknown-linux-musl", Target::X64_LINUX),
            ("aarch64", Target::ARM64_LINUX),
            ("arm64", Target::ARM64_LINUX),
            ("aarch64-linux", Target::ARM64_LINUX),
            ("aarch64-linux-gnu", Target::ARM64_LINUX),
            ("aarch64-macos", Target::ARM64_MACOS),
            ("arm64-macos", Target::ARM64_MACOS),
            ("aarch64-apple-darwin", Target::ARM64_MACOS),
        ] {
            assert_eq!(name.parse(), Ok(target), "{name}");
        }
        let names = Target::ALL.map(|target| target.to_string());
        assert_eq!(names, ["x86_64-linux", "aarch64-linux", "aarch64-macos"]);
        assert_eq!(
            "sparc".parse::<Target>(),
            Err(
                "unknown target 'sparc' (expected x86_64-linux, aarch64-linux, or \
                 aarch64-macos, with an optional level such as x86_64v3-linux)"
                    .to_string()
            )
        );
        assert_eq!(
            "x86_64-windows".parse::<Target>(),
            "sparc"
                .parse::<Target>()
                .map_err(|e| e.replace("sparc", "x86_64-windows"))
        );
        assert_eq!(
            "arm64x".parse::<Target>(),
            "sparc"
                .parse::<Target>()
                .map_err(|e| e.replace("sparc", "arm64x"))
        );
        assert_eq!(
            "x86_64-macos".parse::<Target>(),
            Err(INTEL_MACOS.to_string())
        );
    }

    #[test]
    fn targets_carry_a_level_that_displays_unless_it_is_the_baseline() {
        for (name, arch, os, level, canonical) in [
            (
                "x86_64v3-linux",
                Architecture::X64,
                Os::Linux,
                (3, 0),
                "x86_64v3-linux",
            ),
            (
                "x64v4",
                Architecture::X64,
                Os::Linux,
                (4, 0),
                "x86_64v4-linux",
            ),
            (
                "x86_64v1",
                Architecture::X64,
                Os::Linux,
                (1, 0),
                "x86_64-linux",
            ),
            (
                "aarch64v8.2-macos",
                Architecture::Arm64,
                Os::MacOs,
                (8, 2),
                "aarch64v8.2-macos",
            ),
            (
                "arm64v9-linux",
                Architecture::Arm64,
                Os::Linux,
                (9, 0),
                "aarch64v9-linux",
            ),
            (
                "aarch64v9.5",
                Architecture::Arm64,
                Os::Linux,
                (9, 5),
                "aarch64v9.5-linux",
            ),
            (
                "aarch64v8.0",
                Architecture::Arm64,
                Os::Linux,
                (8, 0),
                "aarch64-linux",
            ),
        ] {
            let target: Target = name.parse().unwrap();
            let (major, minor) = level;
            assert_eq!(
                target,
                Target {
                    arch,
                    os,
                    level: Level { major, minor }
                },
                "{name}"
            );
            assert_eq!(target.to_string(), canonical, "{name}");
        }
        assert_eq!(Target::X64_LINUX.level, Architecture::X64.baseline());
        assert_eq!(Target::ARM64_MACOS.level, Architecture::Arm64.baseline());
        let x64 = "unknown level 'v5' for x86_64 (expected v1, v2, v3, or v4)";
        let arm64 =
            "unknown level 'v7' for aarch64 (expected v8, v8.1 to v8.9, v9, or v9.1 to v9.5)";
        for (name, error) in [
            ("x86_64v5-linux", x64.to_string()),
            ("x86_64v0", x64.replace("v5", "v0")),
            ("x86_64v3.1", x64.replace("v5", "v3.1")),
            ("x86_64v", x64.replace("v5", "v")),
            ("aarch64v7-macos", arm64.to_string()),
            ("aarch64v8.10", arm64.replace("v7", "v8.10")),
            ("aarch64v9.6", arm64.replace("v7", "v9.6")),
            ("aarch64v8.", arm64.replace("v7", "v8.")),
            ("aarch64v08", arm64.replace("v7", "v08")),
        ] {
            assert_eq!(name.parse::<Target>(), Err(error), "{name}");
        }
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

    /// A program that calls every runtime routine: every builtin, method, and function of a
    /// builtin module, plus float `%`, so its assembly holds the whole runtime.
    const EVERY_ROUTINE: &str = "\
use os
use math
use time
xs = [1.5, float(\" 2.5 \")]
xs.append(xs[0] % 0.5)
words = input(\"> \").strip().split()
words.append(str(xs[0]) + str(int(\"-7\")) + str(true))
parts = \"a,b\".split(\",\")
print(xs, words, parts, eof(), args(), words.len())
print(os.env(\"HOME\"), os.has_env(\"HOME\"), os.platform(), os.arch(), os.hostname())
print(os.cpu_count(), os.pid(), os.cwd(), time.now(), time.clock())
print(math.abs(-1), math.abs(-1.5), math.min(1, 2), math.max([1.5]), math.sqrt(2), math.floor(1.5))
time.sleep(0)
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
