//! The built-in linker, which places an assembled [`Object`] at its addresses, resolves every
//! [`Fixup`](crate::codegen::asm::Fixup), and writes a static ELF executable that needs no C library or dynamic linker.
//!
//! The file starts with the ELF header and program headers, which the first segment maps along
//! with `.text`, starting on the next cache line. `.rodata`, then `.data` and `.bss`, get segments of their own, each starting
//! on a fresh page so its permissions can differ. A `PT_GNU_STACK` header marks the stack as
//! not executable. Section headers and a symbol table follow the loaded bytes, so tools such as
//! `objdump`, `gdb`, and `perf` show `main` and `fn.area` by name.

use std::collections::HashMap;

use crate::codegen::Architecture;
use crate::codegen::asm::{Object, Section, Symbol, apply_fixup};

/// Where the first segment, which holds the headers, is mapped, as GNU ld places a static
/// executable.
pub const BASE: u64 = 0x400000;

const HEADER_SIZE: u64 = 64;
const PROGRAM_HEADER_SIZE: u64 = 56;
const SECTION_HEADER_SIZE: u64 = 64;
const SYMBOL_SIZE: u64 = 24;

/// The alignment of each section's start, at least what any of the backends ask for.
const SECTION_ALIGN: u64 = 16;

/// The alignment of `.text`'s start: a cache line, so every loop sits in the same place within
/// its cache lines as when GNU ld starts `.text` on a page.
const TEXT_ALIGN: u64 = 64;

const PT_LOAD: u32 = 1;
const PT_GNU_STACK: u32 = 0x6474_e551;
const PF_X: u32 = 1;
const PF_W: u32 = 2;
const PF_R: u32 = 4;

/// Where each section of an [`Object`] lands, in the file and in memory.
///
/// For example, a program whose headers take 232 bytes on x86-64 has its `.text` at the next
/// cache line, file offset 256 and address `0x400100`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Layout {
    /// The address of each section, indexed like [`Section::ALL`].
    pub addresses: [u64; 4],
    /// The file offset of each section, indexed like [`Section::ALL`]. `.bss` has none in the
    /// file, so its offset is where `.data` ends.
    pub offsets: [u64; 4],
    /// Where the loaded bytes end in the file.
    pub end: u64,
    /// How many program headers there are.
    pub program_headers: u64,
}

impl Layout {
    /// Returns the address of `section`.
    pub fn address(&self, section: Section) -> u64 {
        self.addresses[section as usize]
    }
}

/// Returns the page size segments are aligned to: 4 KiB on x86-64, and 64 KiB on AArch64,
/// whose kernels may use pages that large, as GNU ld assumes.
pub fn page_size(arch: Architecture) -> u64 {
    match arch {
        Architecture::X64 => 0x1000,
        Architecture::Arm64 => 0x10000,
    }
}

/// Returns the ELF `e_machine` of `arch`: 62 for x86-64 and 183 for AArch64.
pub fn machine(arch: Architecture) -> u16 {
    match arch {
        Architecture::X64 => 62,
        Architecture::Arm64 => 183,
    }
}

fn has_rodata(object: &Object) -> bool {
    !object.rodata.is_empty()
}

fn has_data(object: &Object) -> bool {
    !object.data.is_empty() || object.bss > 0
}

/// Decides where every section of `object` goes.
pub fn layout(object: &Object, arch: Architecture) -> Layout {
    let page = page_size(arch);
    let program_headers = 2 + has_rodata(object) as u64 + has_data(object) as u64;
    let headers = HEADER_SIZE + PROGRAM_HEADER_SIZE * program_headers;
    let mut addresses = [0; 4];
    let mut offsets = [0; 4];

    let text = headers.next_multiple_of(TEXT_ALIGN);
    offsets[0] = text;
    addresses[0] = BASE + text;
    let mut offset = text + object.text.len() as u64;
    let mut address = BASE + offset;

    // a section in a new segment starts on the next page, at the same offset within the page
    // as in the file, as mmap requires
    let next_segment = |offset: &mut u64, address: u64| {
        *offset = offset.next_multiple_of(SECTION_ALIGN);
        address.next_multiple_of(page) + *offset % page
    };
    address = next_segment(&mut offset, address);
    offsets[1] = offset;
    addresses[1] = address;
    offset += object.rodata.len() as u64;
    address += object.rodata.len() as u64;

    address = next_segment(&mut offset, address);
    offsets[2] = offset;
    addresses[2] = address;
    offset += object.data.len() as u64;
    address += object.data.len() as u64;

    offsets[3] = offset;
    addresses[3] = address.next_multiple_of(SECTION_ALIGN);
    Layout {
        addresses,
        offsets,
        end: offset,
        program_headers,
    }
}

/// Links `object` into the bytes of an executable for `arch`, starting at its `_start`.
///
/// Errors name a missing symbol or a fixup whose target is out of reach, such as
/// `'_start' is never defined`.
pub fn link(object: &Object, arch: Architecture) -> Result<Vec<u8>, String> {
    let layout = layout(object, arch);
    let mut symbols = HashMap::new();
    for symbol in &object.symbols {
        symbols.insert(
            symbol.name.as_str(),
            layout.address(symbol.section) + symbol.offset,
        );
    }
    let entry = *symbols
        .get("_start")
        .ok_or_else(|| "'_start' is never defined".to_string())?;

    // section headers follow the loaded bytes, 8-byte aligned
    let mut file = vec![0u8; layout.end.next_multiple_of(8) as usize];
    for section in [Section::Text, Section::Rodata, Section::Data] {
        let start = layout.offsets[section as usize] as usize;
        let bytes = object.bytes(section);
        file[start..start + bytes.len()].copy_from_slice(bytes);
    }
    for fixup in &object.fixups {
        let target = *symbols
            .get(fixup.symbol.as_str())
            .ok_or_else(|| format!("undefined label '{}'", fixup.symbol))?;
        let place = layout.address(fixup.section) + fixup.offset;
        let at = (layout.offsets[fixup.section as usize] + fixup.offset) as usize;
        apply_fixup(&mut file[at..], fixup, place, target)?;
    }

    write_headers(&mut file, object, arch, &layout, entry);
    write_sections(&mut file, object, &layout);
    Ok(file)
}

fn put16(file: &mut Vec<u8>, value: u16) {
    file.extend_from_slice(&value.to_le_bytes());
}

fn put32(file: &mut Vec<u8>, value: u32) {
    file.extend_from_slice(&value.to_le_bytes());
}

fn put64(file: &mut Vec<u8>, value: u64) {
    file.extend_from_slice(&value.to_le_bytes());
}

/// Fills in the ELF header and program headers at the start of `file`.
fn write_headers(
    file: &mut [u8],
    object: &Object,
    arch: Architecture,
    layout: &Layout,
    entry: u64,
) {
    let page = page_size(arch);
    let mut out = Vec::new();
    out.extend_from_slice(b"\x7fELF");
    // 64-bit, little-endian, version 1, System V ABI
    out.extend_from_slice(&[2, 1, 1, 0]);
    out.extend_from_slice(&[0; 8]);
    put16(&mut out, 2); // ET_EXEC
    put16(&mut out, machine(arch));
    put32(&mut out, 1);
    put64(&mut out, entry);
    put64(&mut out, HEADER_SIZE);
    put64(&mut out, file.len() as u64); // section headers follow the loaded bytes
    put32(&mut out, 0);
    put16(&mut out, HEADER_SIZE as u16);
    put16(&mut out, PROGRAM_HEADER_SIZE as u16);
    put16(&mut out, layout.program_headers as u16);
    put16(&mut out, SECTION_HEADER_SIZE as u16);
    put16(&mut out, SECTIONS.len() as u16);
    put16(&mut out, SECTIONS.len() as u16 - 1); // .shstrtab is last

    let mut segment = |flags: u32, offset: u64, address: u64, size: u64, memory: u64, align| {
        put32(&mut out, PT_LOAD);
        put32(&mut out, flags);
        put64(&mut out, offset);
        put64(&mut out, address);
        put64(&mut out, address);
        put64(&mut out, size);
        put64(&mut out, memory);
        put64(&mut out, align);
    };
    let text_end = layout.offsets[0] + object.text.len() as u64;
    segment(PF_R | PF_X, 0, BASE, text_end, text_end, page);
    if has_rodata(object) {
        let size = object.rodata.len() as u64;
        segment(
            PF_R,
            layout.offsets[1],
            layout.addresses[1],
            size,
            size,
            page,
        );
    }
    if has_data(object) {
        let size = object.data.len() as u64;
        let memory = layout.addresses[3] + object.bss - layout.addresses[2];
        segment(
            PF_R | PF_W,
            layout.offsets[2],
            layout.addresses[2],
            size,
            memory,
            page,
        );
    }
    put32(&mut out, PT_GNU_STACK);
    put32(&mut out, PF_R | PF_W);
    out.extend_from_slice(&[0; 40]);
    put64(&mut out, 16);
    file[..out.len()].copy_from_slice(&out);
}

/// The sections listed in the section header table, after the null section, by name.
const SECTIONS: [&str; 8] = [
    "",
    ".text",
    ".rodata",
    ".data",
    ".bss",
    ".symtab",
    ".strtab",
    ".shstrtab",
];

/// Appends the symbol table, its strings, the section names, and the section headers.
fn write_sections(file: &mut Vec<u8>, object: &Object, layout: &Layout) {
    // locals come before globals, as ELF requires, and `.L` labels are left out like GNU as
    let named: Vec<_> = object
        .symbols
        .iter()
        .filter(|symbol| !symbol.name.starts_with(".L"))
        .collect();
    let (mut ordered, globals): (Vec<&Symbol>, Vec<&Symbol>) =
        named.iter().partition(|symbol| !symbol.global);
    let first_global = ordered.len() as u32 + 1;
    ordered.extend(globals);

    let mut strings = vec![0u8];
    let mut symtab = vec![0u8; SYMBOL_SIZE as usize];
    for symbol in &ordered {
        let name = strings.len() as u32;
        strings.extend_from_slice(symbol.name.as_bytes());
        strings.push(0);
        // a symbol's size runs to the next symbol in its section, or the section's end
        let end = named
            .iter()
            .filter(|other| other.section == symbol.section && other.offset > symbol.offset)
            .map(|other| other.offset)
            .min()
            .unwrap_or(object.size(symbol.section));
        let kind = if symbol.section == Section::Text {
            2 // STT_FUNC
        } else {
            1 // STT_OBJECT
        };
        let binding = if symbol.global { 1 } else { 0 };
        put32(&mut symtab, name);
        symtab.push(binding << 4 | kind);
        symtab.push(0);
        put16(&mut symtab, symbol.section as u16 + 1);
        put64(&mut symtab, layout.address(symbol.section) + symbol.offset);
        put64(&mut symtab, end - symbol.offset);
    }

    let mut names = vec![0u8];
    let mut name_offsets = Vec::new();
    for name in SECTIONS {
        if name.is_empty() {
            name_offsets.push(0);
            continue;
        }
        name_offsets.push(names.len() as u32);
        names.extend_from_slice(name.as_bytes());
        names.push(0);
    }

    // section headers go at the end of the loaded bytes, which the ELF header already says
    let headers_at = file.len() as u64;
    let symtab_at = headers_at + SECTION_HEADER_SIZE * SECTIONS.len() as u64;
    let strings_at = symtab_at + symtab.len() as u64;
    let names_at = strings_at + strings.len() as u64;

    let header = |file: &mut Vec<u8>, index: usize, fields: [u64; 9]| {
        put32(file, name_offsets[index]);
        put32(file, fields[0] as u32);
        for field in &fields[1..5] {
            put64(file, *field);
        }
        put32(file, fields[5] as u32);
        put32(file, fields[6] as u32);
        put64(file, fields[7]);
        put64(file, fields[8]);
    };
    // fields: type, flags, address, offset, size, link, info, alignment, entry size
    header(file, 0, [0; 9]);
    const ALLOC: u64 = 2;
    for (index, (section, flags)) in [
        (Section::Text, ALLOC | 4),
        (Section::Rodata, ALLOC),
        (Section::Data, ALLOC | 1),
        (Section::Bss, ALLOC | 1),
    ]
    .into_iter()
    .enumerate()
    {
        let kind = if section == Section::Bss { 8 } else { 1 };
        header(
            file,
            index + 1,
            [
                kind,
                flags,
                layout.address(section),
                layout.offsets[section as usize],
                object.size(section),
                0,
                0,
                SECTION_ALIGN,
                0,
            ],
        );
    }
    header(
        file,
        5,
        [
            2,
            0,
            0,
            symtab_at,
            symtab.len() as u64,
            6,
            first_global as u64,
            8,
            SYMBOL_SIZE,
        ],
    );
    header(
        file,
        6,
        [3, 0, 0, strings_at, strings.len() as u64, 0, 0, 1, 0],
    );
    header(file, 7, [3, 0, 0, names_at, names.len() as u64, 0, 0, 1, 0]);
    file.extend_from_slice(&symtab);
    file.extend_from_slice(&strings);
    file.extend_from_slice(&names);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::asm::{Fixup, FixupKind};

    /// Reads a little-endian integer of `N` bytes at `at`.
    fn read<const N: usize>(file: &[u8], at: usize) -> u64 {
        let mut bytes = [0; 8];
        bytes[..N].copy_from_slice(&file[at..at + N]);
        u64::from_le_bytes(bytes)
    }

    /// Returns each program header's type, flags, offset, address, file size, and memory size.
    fn program_headers(file: &[u8]) -> Vec<[u64; 6]> {
        let count = read::<2>(file, 56) as usize;
        (0..count)
            .map(|i| {
                let at = 64 + 56 * i;
                [
                    read::<4>(file, at),
                    read::<4>(file, at + 4),
                    read::<8>(file, at + 8),
                    read::<8>(file, at + 16),
                    read::<8>(file, at + 32),
                    read::<8>(file, at + 40),
                ]
            })
            .collect()
    }

    /// Returns every symbol's name and value from the `.symtab` of `file`.
    fn symbols(file: &[u8]) -> Vec<(String, u64)> {
        let headers = read::<8>(file, 40) as usize;
        let section = |i: usize| headers + 64 * i;
        let symtab = section(5);
        let strtab = read::<8>(file, section(6) + 24) as usize;
        let start = read::<8>(file, symtab + 24) as usize;
        let size = read::<8>(file, symtab + 32) as usize;
        (start + 24..start + size)
            .step_by(24)
            .map(|at| {
                let name = strtab + read::<4>(file, at) as usize;
                let end = file[name..].iter().position(|&b| b == 0).unwrap();
                (
                    String::from_utf8(file[name..name + end].to_vec()).unwrap(),
                    read::<8>(file, at + 8),
                )
            })
            .collect()
    }

    fn symbol(name: &str, section: Section, offset: u64, global: bool) -> Symbol {
        Symbol {
            name: name.to_string(),
            section,
            offset,
            global,
        }
    }

    /// An object whose `_start` exits with status 7, for the host.
    fn exit_seven() -> Object {
        let text = match Architecture::host() {
            // mov edi, 7; mov eax, 231; syscall
            Architecture::X64 => vec![0xbf, 7, 0, 0, 0, 0xb8, 231, 0, 0, 0, 0x0f, 0x05],
            // mov x0, #7; mov x8, #94; svc #0
            Architecture::Arm64 => [0xd28000e0u32, 0xd2800bc8, 0xd4000001]
                .iter()
                .flat_map(|word| word.to_le_bytes())
                .collect(),
        };
        Object {
            text,
            data: vec![1, 2, 3],
            bss: 8,
            symbols: vec![
                symbol("_start", Section::Text, 0, true),
                symbol(".Lhidden", Section::Text, 4, false),
                symbol("stone.live", Section::Bss, 0, false),
            ],
            ..Object::default()
        }
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn a_linked_object_runs_on_its_own() {
        let file = link(&exit_seven(), Architecture::host()).unwrap();
        let dir = std::env::temp_dir().join(format!("stone-elf-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("seven");
        std::fs::write(&path, &file).unwrap();
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
        let status = std::process::Command::new(&path).status().unwrap();
        std::fs::remove_dir_all(&dir).unwrap();
        assert_eq!(status.code(), Some(7));
    }

    #[test]
    fn segments_map_each_section_with_its_permissions() {
        let object = exit_seven();
        let arch = Architecture::X64;
        let file = link(&object, arch).unwrap();
        let layout = layout(&object, arch);
        assert_eq!(read::<2>(&file, 18), 62);
        assert_eq!(read::<8>(&file, 24), layout.address(Section::Text));
        let text = layout.offsets[0];
        let data = layout.offsets[2];
        assert_eq!(
            program_headers(&file),
            [
                [1, 5, 0, BASE, text + 12, text + 12],
                [1, 6, data, layout.address(Section::Data), 3, 16 + 8],
                [PT_GNU_STACK as u64, 6, 0, 0, 0, 0],
            ]
        );
        assert_eq!(&file[data as usize..data as usize + 3], [1, 2, 3]);
        // data starts on a page of its own, at the same offset within the page as in the file
        assert_eq!(layout.address(Section::Data) % 0x1000, data % 0x1000);
        assert!(layout.address(Section::Data) >= BASE + 0x1000);
        assert_eq!(
            layout.address(Section::Bss),
            layout.address(Section::Data) + 16
        );
    }

    #[test]
    fn the_symbol_table_names_every_label_but_local_ones() {
        let object = exit_seven();
        let file = link(&object, Architecture::X64).unwrap();
        let layout = layout(&object, Architecture::X64);
        assert_eq!(
            symbols(&file),
            [
                ("stone.live".to_string(), layout.address(Section::Bss)),
                ("_start".to_string(), layout.address(Section::Text)),
            ]
        );
    }

    #[test]
    fn rodata_gets_a_read_only_segment() {
        let object = Object {
            rodata: vec![9; 5],
            ..exit_seven()
        };
        let file = link(&object, Architecture::Arm64).unwrap();
        let layout = layout(&object, Architecture::Arm64);
        let headers = program_headers(&file);
        assert_eq!(headers.len(), 4);
        assert_eq!(headers[1][..2], [1, 4]);
        assert_eq!(layout.address(Section::Rodata) % 0x10000, layout.offsets[1]);
        assert_eq!(read::<2>(&file, 18), 183);
    }

    /// Links `word` at the start of `.text` with a fixup of `kind` to a label `distance` bytes
    /// later, returning the patched word.
    fn patch(word: u32, kind: FixupKind, distance: u64) -> Result<u32, String> {
        let mut object = exit_seven();
        object.text = word.to_le_bytes().to_vec();
        object.text.resize(distance as usize + 4, 0);
        object
            .symbols
            .push(symbol("there", Section::Text, distance, false));
        object.fixups.push(Fixup {
            section: Section::Text,
            offset: 0,
            kind,
            symbol: "there".to_string(),
            addend: 0,
        });
        let file = link(&object, Architecture::Arm64)?;
        let at = layout(&object, Architecture::Arm64).offsets[0] as usize;
        Ok(read::<4>(&file, at) as u32)
    }

    #[test]
    fn arm64_fixups_fill_their_fields() {
        // b, bl, b.ne, tbz, each 8 bytes ahead: 2 words
        assert_eq!(
            patch(0x14000000, FixupKind::Arm64Branch26, 8),
            Ok(0x14000002)
        );
        assert_eq!(
            patch(0x54000001, FixupKind::Arm64Branch19, 8),
            Ok(0x54000041)
        );
        assert_eq!(
            patch(0x36000000, FixupKind::Arm64Branch14, 8),
            Ok(0x36000040)
        );
        assert_eq!(
            patch(0x36000000, FixupKind::Arm64Branch14, 1 << 15),
            Err("'there' is too far away to reach".to_string())
        );
        // a label 0x2000 bytes on is one or two pages on, depending on where .text starts
        let layout = layout(&exit_seven(), Architecture::Arm64);
        let text = layout.address(Section::Text);
        let pages = ((text + 0x2000) >> 12) - (text >> 12);
        assert_eq!(
            patch(0x90000000, FixupKind::Arm64AdrpPage21, 0x2000),
            Ok(0x90000000 | ((pages as u32) & 3) << 29 | ((pages as u32) >> 2) << 5)
        );
        let low = ((text + 0x2000) & 0xfff) as u32;
        assert_eq!(
            patch(0x91000000, FixupKind::Arm64AddLo12, 0x2000),
            Ok(0x91000000 | low << 10)
        );
        assert_eq!(
            patch(0xf9400000, FixupKind::Arm64LdStLo12(3), 0x2000),
            Ok(0xf9400000 | (low >> 3) << 10)
        );
    }

    #[test]
    fn missing_symbols_are_errors() {
        let mut object = exit_seven();
        object.symbols.remove(0);
        assert_eq!(
            link(&object, Architecture::X64),
            Err("'_start' is never defined".to_string())
        );

        let object = crate::codegen::asm::assemble(
            "\t.text\n\t.globl\t_start\n_start:\n\tcall\tnowhere\n",
            Architecture::X64,
        )
        .unwrap();
        assert_eq!(
            link(&object, Architecture::X64),
            Err("undefined label 'nowhere'".to_string())
        );
    }
}
