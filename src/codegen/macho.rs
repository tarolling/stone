//! The built-in Mach-O linker, which turns an assembled arm64 [`Object`] into a macOS executable.
//!
//! macOS has no stable system call interface, so unlike a Linux program, a macOS program calls
//! the system through `libSystem`, which dyld loads before the program starts. The executable
//! is a position-independent `MH_EXECUTE` file that names `/usr/lib/dyld` and
//! `/usr/lib/libSystem.B.dylib`, and imports the functions in [`SYSTEM_FUNCTIONS`]. Each call to
//! one goes through a stub that jumps through a GOT slot, and dyld fills those slots from
//! chained fixups. Nothing else holds an absolute address, so nothing needs rebasing. macOS on
//! arm64 runs only signed code, so the file ends with an ad hoc signature: a SHA-256 hash of
//! every 4 KiB page, which is what `ld` writes when it signs a program it links.
//!
//! The image starts at [`BASE`], and every segment but `__LINKEDIT` sits at the same offset in
//! the file as in memory:
//!
//! - `__PAGEZERO`: the first 4 GiB, unmapped, so null pointers fault
//! - `__TEXT` (read and execute): the headers, then `__text` on a 64-byte boundary, `__stubs`,
//!   and `__const` (the object's `.rodata`)
//! - `__DATA` (read and write): `__got`, `__data`, and `__bss`
//! - `__LINKEDIT`: the chained fixups, the export trie, the symbol table, and the signature,
//!   which follow `__DATA`'s bytes in the file but its `__bss` in memory
//!
//! See Apple's `mach-o/loader.h`, `mach-o/fixup-chains.h`, and `kern/cs_blobs.h` for the
//! structures.

use std::collections::HashMap;

use crate::codegen::asm::{Fixup, FixupKind, Object, Section, apply_fixup};
use crate::codegen::sha256::sha256;

/// Where the image starts in memory, past `__PAGEZERO`, as `ld` places it.
pub const BASE: u64 = 0x1_0000_0000;

/// The page size of macOS on arm64, which every segment is aligned to.
pub const PAGE: u64 = 0x4000;

/// The `libSystem` functions a program may call, each through `bl` with its C name, such as
/// `bl write`. Any other undefined symbol is an error.
pub const SYSTEM_FUNCTIONS: [&str; 9] = [
    "read",
    "write",
    "mmap",
    "munmap",
    "_exit",
    "getpid",
    "getcwd",
    "clock_gettime",
    "sysctl",
];

/// The bytes of each stub: `adrp`, `ldr`, and `br`.
const STUB_SIZE: u64 = 12;
/// The size of the Mach-O header, then of all the load commands [`link`] writes.
const HEADER_SIZE: u64 = 32;
const COMMANDS_SIZE: u64 = 1080;
/// `.text` starts on a cache line, as it does in an ELF file (see `elf::layout`).
const TEXT_ALIGN: u64 = 64;
const SECTION_ALIGN: u64 = 16;
/// The size of the pages the signature hashes one by one, which is not the memory page size.
const CODE_PAGE: usize = 4096;

const LC_SEGMENT_64: u32 = 0x19;
const LC_SYMTAB: u32 = 0x2;
const LC_DYSYMTAB: u32 = 0xb;
const LC_LOAD_DYLIB: u32 = 0xc;
const LC_LOAD_DYLINKER: u32 = 0xe;
const LC_UUID: u32 = 0x1b;
const LC_CODE_SIGNATURE: u32 = 0x1d;
const LC_BUILD_VERSION: u32 = 0x32;
const LC_MAIN: u32 = 0x8000_0028;
const LC_DYLD_EXPORTS_TRIE: u32 = 0x8000_0033;
const LC_DYLD_CHAINED_FIXUPS: u32 = 0x8000_0034;

/// The oldest macOS the executable runs on, 12.0, the first to read chained fixups in programs
/// that are not arm64e, encoded as `xxxx.yy.zz` in 16, 8, and 8 bits.
const MIN_MACOS: u32 = 12 << 16;

/// Where each piece of the executable goes. Addresses are in memory, and every one but
/// `__bss`'s is also at `address - BASE` in the file.
#[derive(Debug)]
pub struct Layout {
    pub text: u64,
    pub stubs: u64,
    pub constants: u64,
    /// The end of `__TEXT`, a whole number of pages from [`BASE`].
    pub text_end: u64,
    pub got: u64,
    pub data: u64,
    pub bss: u64,
    /// The end of `__DATA` in the file, which `__LINKEDIT` starts at, and in memory.
    pub data_file_end: u64,
    pub data_end: u64,
}

impl Layout {
    /// Places the sections of `object`, plus a stub and a GOT slot for each of `imports`.
    pub fn new(object: &Object, imports: usize) -> Layout {
        let imports = imports as u64;
        let text = BASE + (HEADER_SIZE + COMMANDS_SIZE).next_multiple_of(TEXT_ALIGN);
        let stubs = (text + object.text.len() as u64).next_multiple_of(4);
        let constants = (stubs + STUB_SIZE * imports).next_multiple_of(SECTION_ALIGN);
        let text_end = (constants + object.rodata.len() as u64).next_multiple_of(PAGE);
        let got = text_end;
        let data = (got + 8 * imports).next_multiple_of(SECTION_ALIGN);
        let bss = (data + object.data.len() as u64).next_multiple_of(SECTION_ALIGN);
        Layout {
            text,
            stubs,
            constants,
            text_end,
            got,
            data,
            bss,
            data_file_end: (data + object.data.len() as u64).next_multiple_of(PAGE),
            data_end: (bss + object.bss).next_multiple_of(PAGE),
        }
    }

    /// Returns the address of the start of `section`.
    pub fn address(&self, section: Section) -> u64 {
        match section {
            Section::Text => self.text,
            Section::Rodata => self.constants,
            Section::Data => self.data,
            Section::Bss => self.bss,
        }
    }
}

/// Links `object`, assembled for arm64, into the bytes of a signed macOS executable that starts
/// at its `_start`. `identifier` names the program in its signature, as the output file's name.
///
/// Errors name a missing symbol or a fixup whose target is out of reach, such as
/// `'_start' is never defined` or `undefined label 'nowhere'`.
pub fn link(object: &Object, identifier: &str) -> Result<Vec<u8>, String> {
    let mut symbols: HashMap<&str, (Section, u64)> = HashMap::new();
    for symbol in &object.symbols {
        symbols.insert(symbol.name.as_str(), (symbol.section, symbol.offset));
    }
    // the system functions the program calls, in the order of their first call
    let mut imports: Vec<&str> = Vec::new();
    for fixup in &object.fixups {
        let name = fixup.symbol.as_str();
        if symbols.contains_key(name) || imports.contains(&name) {
            continue;
        }
        if !SYSTEM_FUNCTIONS.contains(&name) {
            return Err(format!("undefined label '{name}'"));
        }
        if fixup.kind != FixupKind::Arm64Branch26 {
            return Err(format!(
                "'{name}' is a system function, so it can only be called"
            ));
        }
        imports.push(name);
    }

    let layout = Layout::new(object, imports.len());
    let mut addresses: HashMap<&str, u64> = symbols
        .iter()
        .map(|(name, (section, offset))| (*name, layout.address(*section) + offset))
        .collect();
    for (i, name) in imports.iter().enumerate() {
        addresses.insert(name, layout.stubs + STUB_SIZE * i as u64);
    }
    let entry = *addresses
        .get("_start")
        .ok_or_else(|| "'_start' is never defined".to_string())?;

    let linkedit = Linkedit::new(object, &imports, &layout, identifier);
    let mut file = vec![0u8; linkedit.end as usize];
    for (section, bytes) in [
        (Section::Text, &object.text),
        (Section::Rodata, &object.rodata),
        (Section::Data, &object.data),
    ] {
        let at = (layout.address(section) - BASE) as usize;
        file[at..at + bytes.len()].copy_from_slice(bytes);
    }
    for fixup in &object.fixups {
        let place = layout.address(fixup.section) + fixup.offset;
        let at = (place - BASE) as usize;
        apply_fixup(
            &mut file[at..],
            fixup,
            place,
            addresses[fixup.symbol.as_str()],
        )?;
    }
    write_stubs(&mut file, &imports, &layout)?;

    let uuid = write_commands(&mut file, object, &imports, &layout, &linkedit, entry);
    let start = linkedit.start as usize;
    let end = linkedit.signature as usize;
    file[start..end].copy_from_slice(&linkedit.bytes);
    // the UUID comes from the contents, so building the same program twice gives the same file
    let digest = sha256(&file[..end]);
    file[uuid..uuid + 16].copy_from_slice(&digest[..16]);
    let signature = signature(&file[..end], identifier, layout.text_end - BASE);
    file[end..].copy_from_slice(&signature);
    Ok(file)
}

/// Writes each import's stub, which loads the function's address from its GOT slot and jumps
/// there, and the slot, which holds a chained bind of the import until dyld replaces it with
/// the address.
fn write_stubs(file: &mut [u8], imports: &[&str], layout: &Layout) -> Result<(), String> {
    for (i, name) in imports.iter().enumerate() {
        let i = i as u64;
        let stub = layout.stubs + STUB_SIZE * i;
        let slot = layout.got + 8 * i;
        let at = (stub - BASE) as usize;
        let words = [0x9000_0010u32, 0xf940_0210, 0xd61f_0200]; // adrp x16; ldr x16, [x16]; br x16
        for (k, word) in words.iter().enumerate() {
            file[at + 4 * k..at + 4 * k + 4].copy_from_slice(&word.to_le_bytes());
        }
        for (k, kind) in [FixupKind::Arm64AdrpPage21, FixupKind::Arm64LdStLo12(3)]
            .into_iter()
            .enumerate()
        {
            let fixup = Fixup {
                section: Section::Text,
                offset: 0,
                kind,
                symbol: name.to_string(),
                addend: 0,
            };
            let place = stub + 4 * k as u64;
            apply_fixup(&mut file[at + 4 * k..], &fixup, place, slot)?;
        }
        // DYLD_CHAINED_PTR_64_BIND: the import's index, the bind bit, and the next slot's
        // distance in 4-byte strides, or 0 at the end of the chain
        let next = if i + 1 < imports.len() as u64 { 2 } else { 0 };
        let bind = i | next << 51 | 1 << 63;
        let at = (slot - BASE) as usize;
        file[at..at + 8].copy_from_slice(&bind.to_le_bytes());
    }
    Ok(())
}

/// The contents of `__LINKEDIT`, which starts at `start` in the file: the chained fixups, the
/// export trie, the symbol table, the indirect symbol table, the string table, then the
/// signature at `signature`, which runs to `end`.
struct Linkedit {
    start: u64,
    bytes: Vec<u8>,
    fixups: (u64, u64),
    exports: (u64, u64),
    symbols: u64,
    locals: u32,
    indirect: u64,
    strings: (u64, u64),
    signature: u64,
    end: u64,
}

impl Linkedit {
    fn new(object: &Object, imports: &[&str], layout: &Layout, identifier: &str) -> Linkedit {
        let start = layout.data_file_end - BASE;
        let mut bytes = Vec::new();
        let piece = |bytes: &mut Vec<u8>, data: &[u8]| {
            let offset = start + bytes.len() as u64;
            bytes.extend_from_slice(data);
            bytes.resize(bytes.len().next_multiple_of(8), 0);
            (offset, data.len().next_multiple_of(8) as u64)
        };
        let fixups = piece(&mut bytes, &chained_fixups(imports, layout));
        let exports = piece(&mut bytes, &export_trie());

        // local labels first, then __mh_execute_header, then the imports, as LC_DYSYMTAB
        // groups them, with every name in the string table
        let mut strings = vec![b' ', 0];
        let mut symtab = Vec::new();
        let mut entry =
            |symtab: &mut Vec<u8>, name: &str, kind: u8, section: u8, desc: u16, value| {
                put32(symtab, strings.len() as u32);
                strings.extend_from_slice(name.as_bytes());
                strings.push(0);
                symtab.push(kind);
                symtab.push(section);
                put16(symtab, desc);
                put64(symtab, value);
            };
        let mut locals = 0;
        for symbol in &object.symbols {
            if symbol.name.starts_with(".L") {
                continue;
            }
            let section = match symbol.section {
                Section::Text => 1,
                Section::Rodata => 3,
                Section::Data => 5,
                Section::Bss => 6,
            };
            let value = layout.address(symbol.section) + symbol.offset;
            entry(&mut symtab, &symbol.name, N_SECT, section, 0, value);
            locals += 1;
        }
        entry(
            &mut symtab,
            "__mh_execute_header",
            N_SECT | N_EXT,
            1,
            REFERENCED_DYNAMICALLY,
            BASE,
        );
        for name in imports {
            // the library ordinal, 1 for libSystem, goes in the high byte of n_desc
            entry(&mut symtab, &format!("_{name}"), N_EXT, 0, 1 << 8, 0);
        }
        let symbols = piece(&mut bytes, &symtab).0;
        // the stubs, then the GOT slots, each naming its import's symbol
        let mut indirect = Vec::new();
        for _ in 0..2 {
            for i in 0..imports.len() as u32 {
                put32(&mut indirect, locals + 1 + i);
            }
        }
        let indirect = piece(&mut bytes, &indirect).0;
        let strings_at = start + bytes.len() as u64;
        bytes.extend_from_slice(&strings);
        // the signature starts 16-byte aligned, as ld places it
        let signature = (start + bytes.len() as u64).next_multiple_of(16);
        bytes.resize((signature - start) as usize, 0);
        let strings = (strings_at, signature - strings_at);
        let end = signature + signature_size(signature, identifier);
        Linkedit {
            start,
            bytes,
            fixups,
            exports,
            symbols,
            locals,
            indirect,
            strings,
            signature,
            end,
        }
    }
}

const N_EXT: u8 = 0x1;
const N_SECT: u8 = 0xe;
const REFERENCED_DYNAMICALLY: u16 = 0x10;

/// `DYLD_CHAINED_PTR_64_OFFSET`, the pointer format of the GOT's chain.
const CHAINED_PTR_64_OFFSET: u16 = 6;
/// The page start of a page that no chain starts in.
const CHAIN_START_NONE: u16 = 0xffff;

/// Returns the `LC_DYLD_CHAINED_FIXUPS` data that binds each GOT slot to its import from
/// libSystem: a `dyld_chained_fixups_header`, the chain starts of every segment (only
/// `__DATA` has any), the `DYLD_CHAINED_IMPORT` table, and the imports' names.
fn chained_fixups(imports: &[&str], layout: &Layout) -> Vec<u8> {
    const STARTS: u32 = 32;
    // __PAGEZERO, __TEXT, __DATA, __LINKEDIT, with only __DATA's starts 24 bytes in
    const SEGMENT_STARTS: u32 = 24;
    let pages = (layout.data_end - layout.text_end) / PAGE;
    let segment_size = 22 + 2 * pages as u32;
    let imports_at = (STARTS + SEGMENT_STARTS + segment_size).next_multiple_of(4);
    let names_at = imports_at + 4 * imports.len() as u32;

    let mut out = Vec::new();
    put32(&mut out, 0); // version
    put32(&mut out, STARTS);
    put32(&mut out, imports_at);
    put32(&mut out, names_at);
    put32(&mut out, imports.len() as u32);
    put32(&mut out, 1); // DYLD_CHAINED_IMPORT
    put32(&mut out, 0); // uncompressed names
    out.resize(STARTS as usize, 0);

    put32(&mut out, 4);
    for offset in [0, 0, SEGMENT_STARTS, 0] {
        put32(&mut out, offset);
    }
    out.resize((STARTS + SEGMENT_STARTS) as usize, 0);
    put32(&mut out, segment_size);
    put16(&mut out, PAGE as u16);
    put16(&mut out, CHAINED_PTR_64_OFFSET);
    put64(&mut out, layout.text_end - BASE);
    put32(&mut out, 0); // max_valid_pointer, for 32-bit programs only
    put16(&mut out, pages as u16);
    // the GOT is at the start of __DATA's first page
    for page in 0..pages {
        let first = if page == 0 && !imports.is_empty() {
            0
        } else {
            CHAIN_START_NONE
        };
        put16(&mut out, first);
    }
    out.resize(imports_at as usize, 0);

    let mut names = Vec::new();
    for name in imports {
        // library ordinal 1, not weak, and the name's offset
        put32(&mut out, 1 | (names.len() as u32) << 9);
        names.push(b'_');
        names.extend_from_slice(name.as_bytes());
        names.push(0);
    }
    out.extend_from_slice(&names);
    out
}

/// Returns the export trie, which exports only `__mh_execute_header`, at offset 0 of the image,
/// as every executable `ld` links does.
fn export_trie() -> Vec<u8> {
    let name = b"__mh_execute_header\0";
    // the root has no export of its own and one edge, to the node that follows it
    let mut trie = vec![0, 1];
    trie.extend_from_slice(name);
    let child = trie.len() as u8 + 1;
    trie.push(child);
    // that node exports a symbol with flags 0 at offset 0, and has no edges
    trie.extend_from_slice(&[2, 0, 0, 0]);
    trie
}

/// Writes the Mach-O header and the load commands at the start of `file`, and returns the
/// offset of the UUID, which [`link`] fills in last.
fn write_commands(
    file: &mut [u8],
    object: &Object,
    imports: &[&str],
    layout: &Layout,
    linkedit: &Linkedit,
    entry: u64,
) -> usize {
    let imports = imports.len() as u64;
    let mut out = Vec::new();
    put32(&mut out, 0xfeed_facf); // MH_MAGIC_64
    put32(&mut out, 0x0100_000c); // CPU_TYPE_ARM64
    put32(&mut out, 0); // CPU_SUBTYPE_ARM64_ALL
    put32(&mut out, 2); // MH_EXECUTE
    put32(&mut out, 14);
    put32(&mut out, COMMANDS_SIZE as u32);
    put32(&mut out, 0x0020_0085); // MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE
    put32(&mut out, 0);

    segment(&mut out, "__PAGEZERO", (0, BASE), (0, 0), 0, &[]);
    let text_size = layout.text_end - BASE;
    segment(
        &mut out,
        "__TEXT",
        (BASE, text_size),
        (0, text_size),
        5, // read and execute
        &[
            SectionHeader {
                name: "__text",
                address: layout.text,
                size: object.text.len() as u64,
                align: TEXT_ALIGN,
                // S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
                flags: 0x8000_0400,
                reserved: (0, 0),
            },
            SectionHeader {
                name: "__stubs",
                address: layout.stubs,
                size: STUB_SIZE * imports,
                align: 4,
                // S_SYMBOL_STUBS, with its stubs first in the indirect symbol table
                flags: 0x8000_0408,
                reserved: (0, STUB_SIZE as u32),
            },
            SectionHeader {
                name: "__const",
                address: layout.constants,
                size: object.rodata.len() as u64,
                align: SECTION_ALIGN,
                flags: 0,
                reserved: (0, 0),
            },
        ],
    );
    let data_start = layout.text_end - BASE;
    segment(
        &mut out,
        "__DATA",
        (layout.text_end, layout.data_end - layout.text_end),
        (data_start, layout.data_file_end - layout.text_end),
        3, // read and write
        &[
            SectionHeader {
                name: "__got",
                address: layout.got,
                size: 8 * imports,
                align: 8,
                flags: 0x6, // S_NON_LAZY_SYMBOL_POINTERS
                reserved: (imports as u32, 0),
            },
            SectionHeader {
                name: "__data",
                address: layout.data,
                size: object.data.len() as u64,
                align: SECTION_ALIGN,
                flags: 0,
                reserved: (0, 0),
            },
            SectionHeader {
                name: "__bss",
                address: layout.bss,
                size: object.bss,
                align: SECTION_ALIGN,
                flags: 0x1, // S_ZEROFILL
                reserved: (0, 0),
            },
        ],
    );
    let linkedit_size = linkedit.end - linkedit.start;
    segment(
        &mut out,
        "__LINKEDIT",
        (layout.data_end, linkedit_size.next_multiple_of(PAGE)),
        (linkedit.start, linkedit_size),
        1, // read
        &[],
    );
    linkedit_data(&mut out, LC_DYLD_CHAINED_FIXUPS, linkedit.fixups);
    linkedit_data(&mut out, LC_DYLD_EXPORTS_TRIE, linkedit.exports);

    let symbols = linkedit.locals + 1 + imports as u32;
    put32(&mut out, LC_SYMTAB);
    put32(&mut out, 24);
    put32(&mut out, linkedit.symbols as u32);
    put32(&mut out, symbols);
    put32(&mut out, linkedit.strings.0 as u32);
    put32(&mut out, linkedit.strings.1 as u32);

    put32(&mut out, LC_DYSYMTAB);
    put32(&mut out, 80);
    for value in [
        0,                   // ilocalsym
        linkedit.locals,     // nlocalsym
        linkedit.locals,     // iextdefsym
        1,                   // nextdefsym
        linkedit.locals + 1, // iundefsym
        imports as u32,      // nundefsym
        0,                   // tocoff
        0,                   // ntoc
        0,                   // modtaboff
        0,                   // nmodtab
        0,                   // extrefsymoff
        0,                   // nextrefsyms
        linkedit.indirect as u32,
        2 * imports as u32, // nindirectsyms
        0,                  // extreloff
        0,                  // nextrel
        0,                  // locreloff
        0,                  // nlocrel
    ] {
        put32(&mut out, value);
    }

    put32(&mut out, LC_LOAD_DYLINKER);
    put32(&mut out, 32);
    put32(&mut out, 12);
    put_padded(&mut out, "/usr/lib/dyld", 20);

    put32(&mut out, LC_UUID);
    put32(&mut out, 24);
    let uuid = out.len();
    out.extend_from_slice(&[0; 16]);

    put32(&mut out, LC_BUILD_VERSION);
    put32(&mut out, 24);
    put32(&mut out, 1); // PLATFORM_MACOS
    put32(&mut out, MIN_MACOS);
    put32(&mut out, MIN_MACOS); // the SDK
    put32(&mut out, 0); // no tools listed

    put32(&mut out, LC_MAIN);
    put32(&mut out, 24);
    put64(&mut out, entry - BASE);
    put64(&mut out, 0); // the default stack size

    put32(&mut out, LC_LOAD_DYLIB);
    put32(&mut out, 56);
    put32(&mut out, 24);
    put32(&mut out, 2); // timestamp
    put32(&mut out, 1351 << 16); // current version, 1351.0.0
    put32(&mut out, 1 << 16); // compatibility version, 1.0.0
    put_padded(&mut out, "/usr/lib/libSystem.B.dylib", 32);

    linkedit_data(
        &mut out,
        LC_CODE_SIGNATURE,
        (linkedit.signature, linkedit.end - linkedit.signature),
    );

    debug_assert_eq!(out.len() as u64, HEADER_SIZE + COMMANDS_SIZE);
    file[..out.len()].copy_from_slice(&out);
    uuid
}

/// A section header inside a segment command.
struct SectionHeader {
    name: &'static str,
    address: u64,
    size: u64,
    align: u64,
    flags: u32,
    /// `reserved1` and `reserved2`: for stubs, the first indirect symbol and the stub size, and
    /// for the GOT, the first indirect symbol.
    reserved: (u32, u32),
}

/// Appends an `LC_SEGMENT_64` command for `name`, spanning `memory` (address and size) and
/// `file` (offset and size), with the protection `prot` and the given sections.
fn segment(
    out: &mut Vec<u8>,
    name: &str,
    memory: (u64, u64),
    file: (u64, u64),
    prot: u32,
    sections: &[SectionHeader],
) {
    put32(out, LC_SEGMENT_64);
    put32(out, 72 + 80 * sections.len() as u32);
    put_padded(out, name, 16);
    put64(out, memory.0);
    put64(out, memory.1);
    put64(out, file.0);
    put64(out, file.1);
    put32(out, prot); // maxprot
    put32(out, prot); // initprot
    put32(out, sections.len() as u32);
    put32(out, 0);
    for section in sections {
        put_padded(out, section.name, 16);
        put_padded(out, name, 16);
        put64(out, section.address);
        put64(out, section.size);
        // a zero-fill section has no bytes in the file
        let offset = if section.flags == 0x1 {
            0
        } else {
            section.address - BASE
        };
        put32(out, offset as u32);
        put32(out, section.align.trailing_zeros());
        put32(out, 0); // no relocations
        put32(out, 0);
        put32(out, section.flags);
        put32(out, section.reserved.0);
        put32(out, section.reserved.1);
        put32(out, 0);
    }
}

/// Appends a `linkedit_data_command` for data at `(offset, size)` in the file.
fn linkedit_data(out: &mut Vec<u8>, command: u32, (offset, size): (u64, u64)) {
    put32(out, command);
    put32(out, 16);
    put32(out, offset as u32);
    put32(out, size as u32);
}

const CSMAGIC_EMBEDDED_SIGNATURE: u32 = 0xfade_0cc0;
const CSMAGIC_CODEDIRECTORY: u32 = 0xfade_0c02;
/// The size of a `CS_SuperBlob` with one index entry, and of a version `0x20400`
/// `CS_CodeDirectory`.
const SUPER_BLOB_SIZE: u64 = 20;
const CODE_DIRECTORY_SIZE: u64 = 88;

/// Returns the size of the signature of the `code_limit` bytes before it.
fn signature_size(code_limit: u64, identifier: &str) -> u64 {
    let pages = code_limit.div_ceil(CODE_PAGE as u64);
    SUPER_BLOB_SIZE + CODE_DIRECTORY_SIZE + identifier.len() as u64 + 1 + 32 * pages
}

/// Returns an ad hoc signature of `code`, the file up to the signature: a `CS_SuperBlob` that
/// holds one `CS_CodeDirectory` with the SHA-256 hash of each 4 KiB page, marked linker-signed
/// as `ld`'s are. `text_size` is the size of `__TEXT`, the segment the kernel lets execute.
/// Unlike the rest of the file, signatures are big-endian.
fn signature(code: &[u8], identifier: &str, text_size: u64) -> Vec<u8> {
    let size = signature_size(code.len() as u64, identifier) as u32;
    let directory = size - SUPER_BLOB_SIZE as u32;
    let identifier_at = CODE_DIRECTORY_SIZE as u32;
    let hashes_at = identifier_at + identifier.len() as u32 + 1;
    let pages = code.len().div_ceil(CODE_PAGE) as u32;

    let mut out = Vec::new();
    let be32 = |out: &mut Vec<u8>, value: u32| out.extend_from_slice(&value.to_be_bytes());
    be32(&mut out, CSMAGIC_EMBEDDED_SIGNATURE);
    be32(&mut out, size);
    be32(&mut out, 1); // one blob
    be32(&mut out, 0); // CSSLOT_CODEDIRECTORY
    be32(&mut out, SUPER_BLOB_SIZE as u32);

    be32(&mut out, CSMAGIC_CODEDIRECTORY);
    be32(&mut out, directory);
    be32(&mut out, 0x20400); // the version, which has the executable segment fields
    be32(&mut out, 0x2_0002); // CS_ADHOC | CS_LINKER_SIGNED
    be32(&mut out, hashes_at);
    be32(&mut out, identifier_at);
    be32(&mut out, 0); // no special slots
    be32(&mut out, pages);
    be32(&mut out, code.len() as u32); // codeLimit
    out.push(32); // hash size
    out.push(2); // CS_HASHTYPE_SHA256
    out.push(0); // not a platform binary
    out.push(CODE_PAGE.trailing_zeros() as u8);
    be32(&mut out, 0); // spare2
    be32(&mut out, 0); // scatterOffset
    be32(&mut out, 0); // teamOffset
    be32(&mut out, 0); // spare3
    out.extend_from_slice(&0u64.to_be_bytes()); // codeLimit64, unused below 4 GiB
    out.extend_from_slice(&0u64.to_be_bytes()); // execSegBase
    out.extend_from_slice(&text_size.to_be_bytes()); // execSegLimit
    out.extend_from_slice(&1u64.to_be_bytes()); // CS_EXECSEG_MAIN_BINARY
    out.extend_from_slice(identifier.as_bytes());
    out.push(0);
    for page in code.chunks(CODE_PAGE) {
        out.extend_from_slice(&sha256(page));
    }
    debug_assert_eq!(out.len() as u32, size);
    out
}

fn put16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn put64(out: &mut Vec<u8>, value: u64) {
    out.extend_from_slice(&value.to_le_bytes());
}

/// Appends `text` padded with zeros to `size` bytes.
fn put_padded(out: &mut Vec<u8>, text: &str, size: usize) {
    out.extend_from_slice(text.as_bytes());
    out.resize(out.len() + size - text.len(), 0);
}

#[cfg(test)]
mod tests;
