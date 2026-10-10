//! Checks the built-in assembler and linker against GNU as and ld, byte for byte.
//!
//! For every golden program (the `.st` files `tests/programs.rs` runs) and both architectures,
//! this assembles the backend's text with [`asm::assemble`] and links it with [`elf::link`],
//! then assembles the same text with GNU as and links it with GNU ld at the same section
//! addresses, and requires `.text`, `.rodata`, and `.data` to hold the same bytes and `.bss`
//! to have the same size. Nothing runs, so no emulator is needed.
//!
//! The host's `as` and `ld` serve its own architecture, and `aarch64-linux-gnu-as` and
//! `aarch64-linux-gnu-ld` (or the `x86_64-linux-gnu-` pair) the other one. An architecture
//! whose tools are not on `PATH` is skipped with a note.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use stone::codegen::asm::{self, Section};
use stone::codegen::{Architecture, elf};
use stone::project::FsSources;

const PROGRAM_DIRS: [&str; 4] = [
    "examples",
    "tests/programs",
    "docs/examples",
    "bench/programs",
];

/// Returns every `.st` file directly in [`PROGRAM_DIRS`] and the `main.st` of each of their
/// subdirectories.
fn programs() -> Vec<PathBuf> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut programs = Vec::new();
    for dir in PROGRAM_DIRS {
        for entry in std::fs::read_dir(root.join(dir)).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                programs.push(path.join("main.st"));
            } else if path.extension().is_some_and(|ext| ext == "st") {
                programs.push(path);
            }
        }
    }
    programs.sort();
    programs
}

/// Returns GNU as and ld for `arch`, if both are on `PATH`. Only Linux's own `as` and `ld` are
/// GNU's, so elsewhere both need the cross tools' prefix.
fn gnu_tools(arch: Architecture) -> Option<(String, String)> {
    let prefix = if arch == Architecture::host() && cfg!(target_os = "linux") {
        String::new()
    } else {
        format!("{arch}-linux-gnu-")
    };
    let tools = (format!("{prefix}as"), format!("{prefix}ld"));
    let runs = |tool: &str| {
        Command::new(tool)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .is_ok_and(|status| status.success())
    };
    (runs(&tools.0) && runs(&tools.1)).then_some(tools)
}

fn read<const N: usize>(file: &[u8], at: usize) -> u64 {
    let mut bytes = [0; 8];
    bytes[..N].copy_from_slice(&file[at..at + N]);
    u64::from_le_bytes(bytes)
}

/// Returns each section of an ELF file by name, as its address, size, and contents (empty for
/// `.bss`).
fn sections(file: &[u8]) -> HashMap<String, (u64, u64, Vec<u8>)> {
    let headers = read::<8>(file, 40) as usize;
    let count = read::<2>(file, 60) as usize;
    let names_index = read::<2>(file, 62) as usize;
    let header = |i: usize| headers + 64 * i;
    let names = read::<8>(file, header(names_index) + 24) as usize;
    let mut sections = HashMap::new();
    for i in 1..count {
        let at = header(i);
        let name = names + read::<4>(file, at) as usize;
        let end = file[name..].iter().position(|&b| b == 0).unwrap();
        let name = String::from_utf8_lossy(&file[name..name + end]).into_owned();
        let kind = read::<4>(file, at + 4);
        let address = read::<8>(file, at + 16);
        let offset = read::<8>(file, at + 24) as usize;
        let size = read::<8>(file, at + 32);
        let bytes = if kind == 8 {
            Vec::new()
        } else {
            file[offset..offset + size as usize].to_vec()
        };
        sections.insert(name, (address, size, bytes));
    }
    sections
}

/// Describes where two sections first differ, naming the label before that offset.
fn difference(object: &asm::Object, section: Section, ours: &[u8], theirs: &[u8]) -> String {
    let at = ours
        .iter()
        .zip(theirs)
        .position(|(a, b)| a != b)
        .unwrap_or(ours.len().min(theirs.len()));
    let label = object
        .symbols
        .iter()
        .filter(|s| s.section == section && s.offset as usize <= at)
        .max_by_key(|s| s.offset)
        .map(|s| format!("{} + {}", s.name, at as u64 - s.offset))
        .unwrap_or_default();
    let window = |bytes: &[u8]| {
        bytes[at.min(bytes.len())..(at + 12).min(bytes.len())]
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<Vec<_>>()
            .join(" ")
    };
    format!(
        "{} differs at offset {at} ({label}): ours {}, GNU's {} (sizes {} and {})",
        section.name(),
        window(ours),
        window(theirs),
        ours.len(),
        theirs.len()
    )
}

/// Assembles one program both ways for `arch` and returns how they differ, if at all.
fn compare(
    program: &Path,
    arch: Architecture,
    (gnu_as, gnu_ld): &(String, String),
    dir: &Path,
) -> Result<(), String> {
    let source = std::fs::read_to_string(program).unwrap();
    let (_, loaded) = stone::driver::load(program, &source, &FsSources);
    let module = loaded.map_err(|e| format!("does not check: {e:?}"))?.0;
    let text = arch.generator().assemble(&module)?;
    let object = asm::assemble(&text, arch)?;
    let layout = elf::layout(&object, arch);
    let ours = sections(&elf::link(&object, arch)?);

    let stem = format!("{}-{arch}", program.display().to_string().replace('/', "_"));
    let source = dir.join(format!("{stem}.s"));
    let objfile = dir.join(format!("{stem}.o"));
    let exe = dir.join(&stem);
    std::fs::write(&source, &text).unwrap();
    let run = |command: &mut Command| -> Result<(), String> {
        let output = command.output().map_err(|e| e.to_string())?;
        if output.status.success() {
            Ok(())
        } else {
            Err(format!(
                "{command:?} failed: {}",
                String::from_utf8_lossy(&output.stderr)
            ))
        }
    };
    run(Command::new(gnu_as).arg("-o").arg(&objfile).arg(&source))?;
    let address = |section| format!("{:#x}", layout.address(section));
    run(Command::new(gnu_ld)
        .args(["-static", "-nostdlib", "-e", "_start"])
        .arg(format!("-Ttext={}", address(Section::Text)))
        .arg(format!(
            "--section-start=.rodata={}",
            address(Section::Rodata)
        ))
        .arg(format!("-Tdata={}", address(Section::Data)))
        .arg(format!("-Tbss={}", address(Section::Bss)))
        .arg("-o")
        .arg(&exe)
        .arg(&objfile))?;
    let theirs = sections(&std::fs::read(&exe).unwrap());
    for path in [&source, &objfile, &exe] {
        let _ = std::fs::remove_file(path);
    }

    let empty = (0, 0, Vec::new());
    for section in [Section::Text, Section::Rodata, Section::Data] {
        let ours = &ours.get(section.name()).unwrap_or(&empty).2;
        let theirs = &theirs.get(section.name()).unwrap_or(&empty).2;
        if ours != theirs {
            return Err(difference(&object, section, ours, theirs));
        }
    }
    let bss = |sections: &HashMap<String, (u64, u64, Vec<u8>)>| {
        sections.get(".bss").map_or(0, |(_, size, _)| *size)
    };
    if bss(&ours) != bss(&theirs) {
        return Err(format!(
            ".bss has {} bytes, but GNU's has {}",
            bss(&ours),
            bss(&theirs)
        ));
    }
    Ok(())
}

fn matches_gnu(arch: Architecture) {
    let Some(tools) = gnu_tools(arch) else {
        eprintln!("skipped comparing with GNU as for {arch}: its as and ld were not found");
        return;
    };
    let dir = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("assembler-{arch}"));
    std::fs::create_dir_all(&dir).unwrap();
    let failures: Vec<String> = programs()
        .iter()
        .filter_map(|program| {
            compare(program, arch, &tools, &dir)
                .err()
                .map(|error| format!("{}: {error}", program.display()))
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn x86_64_matches_gnu_as() {
    matches_gnu(Architecture::X64);
}

#[test]
fn aarch64_matches_gnu_as() {
    matches_gnu(Architecture::Arm64);
}
