//! The built-in PE linker, which turns an assembled x86-64 [`Object`] into a Windows executable.
//!
//! Windows has no stable system call interface, so a Windows program calls the system through
//! `kernel32.dll`, and `bcrypt.dll` for random numbers, the DLLs that every Windows has. The
//! executable is a PE32+ console program that imports the functions in [`IMPORTS`]. Each call
//! to one, `call WriteFile`, goes to a thunk after the code that jumps through the function's
//! slot in the import address table, which the loader fills in. Every other address is relative
//! to the instruction that uses it, so the image needs no base relocations and loads anywhere.
//!
//! The image starts at [`BASE`], and its sections follow the headers, each on a fresh 4 KiB
//! page in memory and a 512-byte boundary in the file:
//!
//! - `.text` (read and execute): the code, then the thunks
//! - `.rdata` (read): the object's `.rodata`, then the import directory, the lookup and
//!   address tables, and the names of the DLLs and functions
//! - `.data` (read and write): the object's `.data`, then its `.bss`, which takes no room in the
//!   file
//!
//! The stack reserves and commits [`STACK_SIZE`] up front, so a frame larger than a page never
//! needs to touch the pages above it first, as code that Windows grows the stack for must.
//!
//! See Microsoft's "PE Format" specification for the structures.

#[cfg(test)]
mod tests;

use std::collections::HashMap;

use crate::codegen::asm::{FixupKind, Object, Section, apply_fixup};
use crate::codegen::sha256::sha256;

/// Where the image asks to be loaded, as Microsoft's linker places a 64-bit program.
pub const BASE: u64 = 0x1_4000_0000;

/// How sections are aligned in memory: one page.
pub const SECTION_ALIGN: u64 = 0x1000;

/// How sections are aligned in the file.
pub const FILE_ALIGN: u64 = 0x200;

/// How much stack the program's thread has, all of it committed when it starts. Linux gives a
/// program 8 MiB unless told otherwise.
pub const STACK_SIZE: u64 = 8 << 20;

/// The functions a program may call, each through `call` with its name, such as
/// `call WriteFile`, by the DLL that has them. Any other undefined symbol is an error.
pub const IMPORTS: [(&str, &[&str]); 2] = [
    (
        "KERNEL32.dll",
        &[
            "CloseHandle",
            "CreateDirectoryW",
            "CreateFileW",
            "DeleteFileW",
            "ExitProcess",
            "FindClose",
            "FindFirstFileW",
            "FindNextFileW",
            "GetActiveProcessorCount",
            "GetCommandLineW",
            "GetComputerNameExW",
            "GetConsoleMode",
            "GetCurrentDirectoryW",
            "GetCurrentProcessId",
            "GetEnvironmentStringsW",
            "GetFileAttributesW",
            "GetLastError",
            "GetStdHandle",
            "GetSystemTimePreciseAsFileTime",
            "MultiByteToWideChar",
            "QueryPerformanceCounter",
            "QueryPerformanceFrequency",
            "ReadFile",
            "Sleep",
            "VirtualAlloc",
            "VirtualFree",
            "WideCharToMultiByte",
            "WriteConsoleW",
            "WriteFile",
        ],
    ),
    ("bcrypt.dll", &["BCryptGenRandom"]),
];

/// The bytes each thunk takes: `jmp QWORD PTR [rip + slot]`, padded to 8.
const THUNK_SIZE: u64 = 8;

/// The alignment of `.text`'s start, a cache line, as in the ELF linker.
const TEXT_ALIGN: u64 = 64;

/// The alignment of `.bss` after `.data`, and of the imports after `.rodata`.
const ALIGN: u64 = 16;

const DOS_HEADER_SIZE: u64 = 64;

/// The program DOS runs in place of a Windows program, as Microsoft's linker writes it: it prints
/// `This program cannot be run in DOS mode.` and exits.
const DOS_STUB: [u8; 64] = *b"\x0e\x1f\xba\x0e\x00\xb4\x09\xcd\x21\xb8\x01\x4c\xcd\x21This program cannot be run in DOS mode.\r\r\n$\0\0\0\0\0\0\0";
const COFF_HEADER_SIZE: u64 = 20;
const OPTIONAL_HEADER_SIZE: u64 = 240;
const SECTION_HEADER_SIZE: u64 = 40;
const IMPORT_DESCRIPTOR_SIZE: u64 = 20;

const IMAGE_FILE_MACHINE_AMD64: u16 = 0x8664;
const IMAGE_FILE_EXECUTABLE_IMAGE: u16 = 0x2;
const IMAGE_FILE_LARGE_ADDRESS_AWARE: u16 = 0x20;
const IMAGE_SUBSYSTEM_WINDOWS_CUI: u16 = 3;
const IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA: u16 = 0x20;
const IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE: u16 = 0x40;
const IMAGE_DLLCHARACTERISTICS_NX_COMPAT: u16 = 0x100;
const IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE: u16 = 0x8000;
const IMAGE_SCN_CNT_CODE: u32 = 0x20;
const IMAGE_SCN_CNT_INITIALIZED_DATA: u32 = 0x40;
const IMAGE_SCN_MEM_DISCARDABLE: u32 = 0x0200_0000;
const IMAGE_SCN_MEM_EXECUTE: u32 = 0x2000_0000;
const IMAGE_SCN_MEM_READ: u32 = 0x4000_0000;
const IMAGE_SCN_MEM_WRITE: u32 = 0x8000_0000;

/// The data directories this linker fills, by their index.
const IMPORT_DIRECTORY: usize = 1;
const RELOCATION_DIRECTORY: usize = 5;
const IAT_DIRECTORY: usize = 12;

/// One section of the image: its name, flags, place in memory (relative to [`BASE`]), size in
/// memory, and bytes.
struct ImageSection {
    name: &'static [u8; 8],
    flags: u32,
    rva: u64,
    virtual_size: u64,
    bytes: Vec<u8>,
}

/// Links `object` into the bytes of a Windows executable, starting at its `_start`.
///
/// Errors name a missing symbol, or one that is neither defined nor imported, such as
/// `undefined label 'printf'`.
pub fn link(object: &Object) -> Result<Vec<u8>, String> {
    let mut defined = HashMap::new();
    for symbol in &object.symbols {
        defined.insert(symbol.name.as_str(), (symbol.section, symbol.offset));
    }
    // the functions called, by DLL in the order IMPORTS lists them
    let mut called = Vec::new();
    for fixup in &object.fixups {
        if defined.contains_key(fixup.symbol.as_str()) {
            continue;
        }
        let imported = IMPORTS
            .iter()
            .any(|(_, functions)| functions.contains(&fixup.symbol.as_str()));
        if !imported || fixup.kind != FixupKind::X64Rel32 {
            return Err(format!("undefined label '{}'", fixup.symbol));
        }
        if !called.contains(&fixup.symbol.as_str()) {
            called.push(fixup.symbol.as_str());
        }
    }
    let dlls: Vec<(&str, Vec<&str>)> = IMPORTS
        .iter()
        .map(|(dll, functions)| {
            let used = functions.iter().copied().filter(|f| called.contains(f));
            (*dll, used.collect::<Vec<_>>())
        })
        .filter(|(_, functions)| !functions.is_empty())
        .collect();
    let functions: Vec<&str> = dlls.iter().flat_map(|(_, f)| f.iter().copied()).collect();

    // .text, .rdata, .data, and .reloc
    let sections = 4;
    let headers = (DOS_HEADER_SIZE
        + DOS_STUB.len() as u64
        + 4
        + COFF_HEADER_SIZE
        + OPTIONAL_HEADER_SIZE
        + SECTION_HEADER_SIZE * sections)
        .next_multiple_of(FILE_ALIGN);

    // .text: the code, then a thunk per function
    let text_rva = headers.next_multiple_of(SECTION_ALIGN).max(SECTION_ALIGN);
    debug_assert_eq!(text_rva % TEXT_ALIGN, 0);
    let thunks = (object.text.len() as u64).next_multiple_of(THUNK_SIZE);
    let text_size = thunks + THUNK_SIZE * functions.len() as u64;

    // .rdata: .rodata, then the imports
    let rdata_rva = (text_rva + text_size).next_multiple_of(SECTION_ALIGN);
    let imports = (object.rodata.len() as u64).next_multiple_of(ALIGN);
    let directory = imports;
    let lookup = directory + IMPORT_DESCRIPTOR_SIZE * (dlls.len() as u64 + 1);
    // each DLL's lookup table, and then its address table, ends with a null entry
    let table_size = 8 * (functions.len() + dlls.len()) as u64;
    let address = lookup + table_size;
    let mut names = address + table_size;
    let mut name_of = HashMap::new();
    for function in &functions {
        name_of.insert(*function, names);
        // a 2-byte hint, the name, and its terminator, padded to an even length
        names += (2 + function.len() as u64 + 1).next_multiple_of(2);
    }
    let mut dll_name_of = HashMap::new();
    for (dll, _) in &dlls {
        dll_name_of.insert(*dll, names);
        names += dll.len() as u64 + 1;
    }
    let rdata_size = names;

    // .data, then .bss
    let data_rva = (rdata_rva + rdata_size).next_multiple_of(SECTION_ALIGN);
    let bss = (object.data.len() as u64).next_multiple_of(ALIGN);
    let data_size = bss + object.bss;

    let rva_of = |section: Section| match section {
        Section::Text => text_rva,
        Section::Rodata => rdata_rva,
        Section::Data => data_rva,
        Section::Bss => data_rva + bss,
    };
    let mut symbols: HashMap<&str, u64> = defined
        .iter()
        .map(|(&name, &(section, offset))| (name, rva_of(section) + offset))
        .collect();
    for (i, function) in functions.iter().enumerate() {
        symbols.insert(function, text_rva + thunks + THUNK_SIZE * i as u64);
    }
    let entry = *symbols
        .get("_start")
        .ok_or_else(|| "'_start' is never defined".to_string())?;

    let mut text = object.text.clone();
    text.resize(text_size as usize, 0xcc);
    let mut rdata = object.rodata.clone();
    rdata.resize(rdata_size as usize, 0);
    let mut data = object.data.clone();
    data.resize(bss as usize, 0);

    // the thunks jump through the address table's slots
    let mut slot = address;
    let mut thunk = thunks;
    for (_, used) in &dlls {
        for _ in used {
            let at = thunk as usize;
            text[at..at + 2].copy_from_slice(&[0xff, 0x25]);
            let next = text_rva + thunk + 6;
            let displacement = (rdata_rva + slot) as i64 - next as i64;
            let displacement = i32::try_from(displacement).map_err(|_| "imports too far")?;
            text[at + 2..at + 6].copy_from_slice(&displacement.to_le_bytes());
            thunk += THUNK_SIZE;
            slot += 8;
        }
        slot += 8;
    }

    // the import directory, then the lookup and address tables, then the names
    let mut entry_at = lookup;
    for (i, (dll, used)) in dlls.iter().enumerate() {
        let descriptor = (directory + IMPORT_DESCRIPTOR_SIZE * i as u64) as usize;
        let tables = entry_at - lookup;
        let fields = [
            (rdata_rva + lookup + tables) as u32,
            0,
            0,
            (rdata_rva + dll_name_of[dll]) as u32,
            (rdata_rva + address + tables) as u32,
        ];
        for (k, field) in fields.iter().enumerate() {
            let at = descriptor + 4 * k;
            rdata[at..at + 4].copy_from_slice(&field.to_le_bytes());
        }
        for function in used {
            let hint_name = rdata_rva + name_of[function];
            for table in [lookup, address] {
                let at = (table + (entry_at - lookup)) as usize;
                rdata[at..at + 8].copy_from_slice(&hint_name.to_le_bytes());
            }
            entry_at += 8;
        }
        entry_at += 8;
    }
    for function in &functions {
        let at = name_of[function] as usize + 2;
        rdata[at..at + function.len()].copy_from_slice(function.as_bytes());
    }
    for (dll, _) in &dlls {
        let at = dll_name_of[dll] as usize;
        rdata[at..at + dll.len()].copy_from_slice(dll.as_bytes());
    }

    // every fixup is relative, so it holds wherever the image loads
    for fixup in &object.fixups {
        let target = symbols[fixup.symbol.as_str()];
        let place = rva_of(fixup.section) + fixup.offset;
        let bytes = match fixup.section {
            Section::Text => &mut text,
            Section::Rodata => &mut rdata,
            Section::Data => &mut data,
            Section::Bss => return Err(format!("a fixup in .bss for '{}'", fixup.symbol)),
        };
        apply_fixup(&mut bytes[fixup.offset as usize..], fixup, place, target)?;
    }

    let mut image = vec![
        ImageSection {
            name: b".text\0\0\0",
            flags: IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ,
            rva: text_rva,
            virtual_size: text_size,
            bytes: text,
        },
        ImageSection {
            name: b".rdata\0\0",
            flags: IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ,
            rva: rdata_rva,
            virtual_size: rdata_size,
            bytes: rdata,
        },
    ];
    if data_size > 0 {
        image.push(ImageSection {
            name: b".data\0\0\0",
            flags: IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE,
            rva: data_rva,
            virtual_size: data_size,
            bytes: data,
        });
    }
    // no address needs fixing where the image loads, which one empty block of base
    // relocations says, as Windows expects of an image that may load anywhere
    let last = image.last().unwrap();
    let reloc_rva = (last.rva + last.virtual_size).next_multiple_of(SECTION_ALIGN);
    let mut relocations = Vec::new();
    relocations.extend_from_slice(&(text_rva as u32).to_le_bytes());
    relocations.extend_from_slice(&(RELOCATION_BLOCK as u32).to_le_bytes());
    image.push(ImageSection {
        name: b".reloc\0\0",
        flags: IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE,
        rva: reloc_rva,
        virtual_size: RELOCATION_BLOCK,
        bytes: relocations,
    });
    let directories = [
        (IMPORT_DIRECTORY, rdata_rva + directory, lookup - directory),
        (RELOCATION_DIRECTORY, reloc_rva, RELOCATION_BLOCK),
        (IAT_DIRECTORY, rdata_rva + address, table_size),
    ];
    let mut file = write(&image, headers, entry, &directories);
    stamp(&mut file);
    Ok(file)
}

/// The size of the one block of base relocations: its page and its size, with no entries.
const RELOCATION_BLOCK: u64 = 8;

/// Where the PE header starts, after the DOS header and stub.
const PE_HEADER: usize = DOS_HEADER_SIZE as usize + DOS_STUB.len();

/// Fills in the image's time stamp and checksum. The time stamp is the first four bytes of the
/// SHA-256 of the image, as Microsoft's linker writes for a reproducible build, so the same
/// program always links to the same bytes. The checksum is the one `CheckSumMappedFile`
/// computes: the 16-bit words of the file summed with their carries folded back in, plus the
/// file's length.
fn stamp(file: &mut [u8]) {
    let time = PE_HEADER + 8;
    let checksum = PE_HEADER + 24 + 64;
    let hash = sha256(file);
    file[time..time + 4].copy_from_slice(&hash[..4]);
    let mut sum: u64 = 0;
    for (i, word) in file.chunks(2).enumerate() {
        if (checksum / 2..checksum / 2 + 2).contains(&i) {
            continue;
        }
        let word = match word {
            [low, high] => u16::from_le_bytes([*low, *high]),
            [low] => *low as u16,
            _ => unreachable!(),
        };
        sum += word as u64;
        sum = (sum & 0xffff) + (sum >> 16);
    }
    let sum = (sum & 0xffff) + (sum >> 16);
    let sum = sum as u32 + file.len() as u32;
    file[checksum..checksum + 4].copy_from_slice(&sum.to_le_bytes());
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

/// Writes the headers, which take `headers` bytes, then each section's bytes, for a program
/// that starts at the address `entry` (relative to [`BASE`]) and has the data directories
/// `directories`, each an index, an address, and a size.
fn write(
    image: &[ImageSection],
    headers: u64,
    entry: u64,
    directories: &[(usize, u64, u64)],
) -> Vec<u8> {
    let raw_size =
        |section: &ImageSection| (section.bytes.len() as u64).next_multiple_of(FILE_ALIGN);
    let last = image.last().unwrap();
    let image_size = (last.rva + last.virtual_size).next_multiple_of(SECTION_ALIGN);
    let code_size: u64 = image
        .iter()
        .filter(|s| s.flags & IMAGE_SCN_CNT_CODE != 0)
        .map(raw_size)
        .sum();
    let data_size: u64 = image
        .iter()
        .filter(|s| s.flags & IMAGE_SCN_CNT_INITIALIZED_DATA != 0)
        .map(raw_size)
        .sum();

    let mut file = Vec::new();
    // the DOS header, of which Windows reads only the magic and where the PE header is, then
    // the DOS program, as Microsoft's linker writes them
    file.extend_from_slice(b"MZ");
    for word in [0x90, 3, 0, 4, 0, 0xffff, 0, 0xb8, 0, 0, 0, 0x40] {
        put16(&mut file, word);
    }
    file.resize(0x3c, 0);
    put32(&mut file, PE_HEADER as u32);
    file.extend_from_slice(&DOS_STUB);
    file.extend_from_slice(b"PE\0\0");

    put16(&mut file, IMAGE_FILE_MACHINE_AMD64);
    put16(&mut file, image.len() as u16);
    put32(&mut file, 0); // the time stamp, which `stamp` fills in
    put32(&mut file, 0); // no symbol table
    put32(&mut file, 0);
    put16(&mut file, OPTIONAL_HEADER_SIZE as u16);
    put16(
        &mut file,
        IMAGE_FILE_EXECUTABLE_IMAGE | IMAGE_FILE_LARGE_ADDRESS_AWARE,
    );

    put16(&mut file, 0x20b); // PE32+
    file.extend_from_slice(&[14, 0]); // the linker's version, as Microsoft's
    put32(&mut file, code_size as u32);
    put32(&mut file, data_size as u32);
    put32(&mut file, 0); // uninitialized data, which .data's tail holds
    put32(&mut file, entry as u32);
    put32(&mut file, image[0].rva as u32);
    put64(&mut file, BASE);
    put32(&mut file, SECTION_ALIGN as u32);
    put32(&mut file, FILE_ALIGN as u32);
    // the system and subsystem versions Microsoft's linker writes, though the runtime needs
    // Windows 8 for GetSystemTimePreciseAsFileTime
    for version in [6, 0, 0, 0, 6, 0] {
        put16(&mut file, version);
    }
    put32(&mut file, 0);
    put32(&mut file, image_size as u32);
    put32(&mut file, headers as u32);
    put32(&mut file, 0); // the checksum, which `stamp` fills in
    put16(&mut file, IMAGE_SUBSYSTEM_WINDOWS_CUI);
    put16(
        &mut file,
        IMAGE_DLLCHARACTERISTICS_HIGH_ENTROPY_VA
            | IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
            | IMAGE_DLLCHARACTERISTICS_NX_COMPAT
            | IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE,
    );
    put64(&mut file, STACK_SIZE);
    put64(&mut file, STACK_SIZE);
    put64(&mut file, 1 << 20); // the heap, which only the system's own code uses
    put64(&mut file, 0x1000);
    put32(&mut file, 0);
    put32(&mut file, 16);
    for index in 0..16 {
        let (rva, size) = directories
            .iter()
            .find(|(i, ..)| *i == index)
            .map_or((0, 0), |&(_, rva, size)| (rva, size));
        put32(&mut file, rva as u32);
        put32(&mut file, size as u32);
    }

    let mut offset = headers;
    for section in image {
        file.extend_from_slice(section.name);
        put32(&mut file, section.virtual_size as u32);
        put32(&mut file, section.rva as u32);
        put32(&mut file, raw_size(section) as u32);
        put32(&mut file, offset as u32);
        put32(&mut file, 0); // no relocations or line numbers
        put32(&mut file, 0);
        put16(&mut file, 0);
        put16(&mut file, 0);
        put32(&mut file, section.flags);
        offset += raw_size(section);
    }
    file.resize(headers as usize, 0);
    for section in image {
        file.extend_from_slice(&section.bytes);
        file.resize(file.len().next_multiple_of(FILE_ALIGN as usize), 0);
    }
    file
}
