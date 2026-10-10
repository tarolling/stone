use super::*;
use crate::codegen::Architecture;
use crate::codegen::arm64::builtins::Sys;
use crate::codegen::asm::assemble;

/// A program that calls two system functions, with something in every section.
const PROGRAM: &str = "\
\t.text
\t.globl\t_start
_start:
\tadrp\tx1, message
\tadd\tx1, x1, :lo12:message
\tbl\twrite
\tmov\tx0, #7
\tbl\t_exit
helper:
\tret
\t.section\t.rodata
table:
\t.quad\t1
\t.data
message:
\t.string \"hi\"
\t.bss
counter:
\t.zero\t8
";

fn linked(text: &str) -> Result<Vec<u8>, String> {
    link(&assemble(text, Architecture::Arm64).unwrap(), "out")
}

fn read32(file: &[u8], at: usize) -> u32 {
    u32::from_le_bytes(file[at..at + 4].try_into().unwrap())
}

fn read64(file: &[u8], at: usize) -> u64 {
    u64::from_le_bytes(file[at..at + 8].try_into().unwrap())
}

fn read_be32(file: &[u8], at: usize) -> u32 {
    u32::from_be_bytes(file[at..at + 4].try_into().unwrap())
}

fn name(bytes: &[u8]) -> String {
    let end = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
    String::from_utf8_lossy(&bytes[..end]).into_owned()
}

/// Returns each load command as its number and its offset in `file`.
fn commands(file: &[u8]) -> Vec<(u32, usize)> {
    let mut at = HEADER_SIZE as usize;
    let mut commands = Vec::new();
    for _ in 0..read32(file, 16) {
        commands.push((read32(file, at), at));
        at += read32(file, at + 4) as usize;
    }
    commands
}

/// Returns the offset of the only load command numbered `command`.
fn command(file: &[u8], command: u32) -> usize {
    let found: Vec<usize> = commands(file)
        .into_iter()
        .filter(|(c, _)| *c == command)
        .map(|(_, at)| at)
        .collect();
    assert_eq!(found.len(), 1, "command {command:#x}");
    found[0]
}

#[derive(Debug)]
struct Segment {
    name: String,
    address: u64,
    memory_size: u64,
    offset: u64,
    file_size: u64,
    sections: Vec<SectionInfo>,
}

#[derive(Debug)]
struct SectionInfo {
    name: String,
    address: u64,
    size: u64,
    offset: u32,
    flags: u32,
    reserved1: u32,
    reserved2: u32,
}

fn segments(file: &[u8]) -> Vec<Segment> {
    commands(file)
        .into_iter()
        .filter(|(c, _)| *c == LC_SEGMENT_64)
        .map(|(_, at)| Segment {
            name: name(&file[at + 8..at + 24]),
            address: read64(file, at + 24),
            memory_size: read64(file, at + 32),
            offset: read64(file, at + 40),
            file_size: read64(file, at + 48),
            sections: (0..read32(file, at + 64) as usize)
                .map(|i| {
                    let s = at + 72 + 80 * i;
                    SectionInfo {
                        name: name(&file[s..s + 16]),
                        address: read64(file, s + 32),
                        size: read64(file, s + 40),
                        offset: read32(file, s + 48),
                        flags: read32(file, s + 64),
                        reserved1: read32(file, s + 68),
                        reserved2: read32(file, s + 72),
                    }
                })
                .collect(),
        })
        .collect()
}

fn section(file: &[u8], wanted: &str) -> SectionInfo {
    segments(file)
        .into_iter()
        .flat_map(|segment| segment.sections)
        .find(|section| section.name == wanted)
        .unwrap()
}

/// Returns where the `adrp` at `place` and the `add` or `ldr` after it point.
fn adrp_target(file: &[u8], place: u64, scale: u64) -> u64 {
    let adrp = read32(file, (place - BASE) as usize);
    let low = read32(file, (place + 4 - BASE) as usize);
    let pages = ((adrp >> 29 & 3) | (adrp >> 5 & 0x7ffff) << 2) as i64;
    let pages = pages << 43 >> 43; // sign-extends the 21 bits
    let page = ((place >> 12) as i64 + pages) as u64;
    (page << 12) + (low >> 10 & 0xfff) as u64 * scale
}

/// Returns where the `bl` at `place` goes.
fn branch_target(file: &[u8], place: u64) -> u64 {
    let word = read32(file, (place - BASE) as usize);
    let words = ((word & 0x3ff_ffff) as i64) << 38 >> 38;
    (place as i64 + 4 * words) as u64
}

#[test]
fn the_header_lists_every_command_dyld_needs() {
    let file = linked(PROGRAM).unwrap();
    assert_eq!(read32(&file, 0), 0xfeed_facf);
    assert_eq!(read32(&file, 4), 0x0100_000c);
    assert_eq!(read32(&file, 12), 2);
    assert_eq!(read32(&file, 24), 0x0020_0085);
    let order: Vec<u32> = commands(&file).iter().map(|(c, _)| *c).collect();
    assert_eq!(
        order,
        [
            LC_SEGMENT_64,
            LC_SEGMENT_64,
            LC_SEGMENT_64,
            LC_SEGMENT_64,
            LC_DYLD_CHAINED_FIXUPS,
            LC_DYLD_EXPORTS_TRIE,
            LC_SYMTAB,
            LC_DYSYMTAB,
            LC_LOAD_DYLINKER,
            LC_UUID,
            LC_BUILD_VERSION,
            LC_MAIN,
            LC_LOAD_DYLIB,
            LC_CODE_SIGNATURE,
        ]
    );
    let (_, last) = *commands(&file).last().unwrap();
    assert_eq!(
        last + read32(&file, last + 4) as usize,
        (HEADER_SIZE + COMMANDS_SIZE) as usize
    );
    assert_eq!(read32(&file, 20) as u64, COMMANDS_SIZE);

    let dylinker = command(&file, LC_LOAD_DYLINKER);
    assert_eq!(name(&file[dylinker + 12..dylinker + 32]), "/usr/lib/dyld");
    let dylib = command(&file, LC_LOAD_DYLIB);
    assert_eq!(
        name(&file[dylib + 24..dylib + 56]),
        "/usr/lib/libSystem.B.dylib"
    );
    let build = command(&file, LC_BUILD_VERSION);
    assert_eq!(read32(&file, build + 8), 1);
    assert_eq!(read32(&file, build + 12), 12 << 16);
}

#[test]
fn segments_sit_on_pages_at_the_same_offset_in_the_file_as_in_memory() {
    let file = linked(PROGRAM).unwrap();
    let segments = segments(&file);
    let names: Vec<&str> = segments.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(names, ["__PAGEZERO", "__TEXT", "__DATA", "__LINKEDIT"]);
    assert_eq!((segments[0].address, segments[0].memory_size), (0, BASE));
    assert_eq!(segments[0].file_size, 0);
    assert_eq!((segments[1].address, segments[1].offset), (BASE, 0));
    for segment in &segments[1..] {
        assert_eq!(segment.address % PAGE, 0, "{}", segment.name);
        assert_eq!(segment.offset % PAGE, 0, "{}", segment.name);
        assert_eq!(segment.memory_size % PAGE, 0, "{}", segment.name);
        assert!(segment.file_size <= segment.memory_size, "{}", segment.name);
    }
    for segment in &segments[1..3] {
        assert_eq!(segment.address - BASE, segment.offset, "{}", segment.name);
        for section in &segment.sections {
            assert!(section.address >= segment.address, "{}", section.name);
            let end = section.address + section.size;
            assert!(
                end <= segment.address + segment.memory_size,
                "{}",
                section.name
            );
            if section.flags != 0x1 {
                assert_eq!(
                    section.offset as u64,
                    section.address - BASE,
                    "{}",
                    section.name
                );
            }
        }
    }
    // __LINKEDIT follows __bss in memory, and its data ends the file
    let data = &segments[2];
    assert!(segments[3].address >= data.address + data.memory_size);
    assert_eq!(
        segments[3].offset + segments[3].file_size,
        file.len() as u64
    );
    let text = section(&file, "__text");
    assert_eq!(text.offset as u64 % TEXT_ALIGN, 0);
    let bss = section(&file, "__bss");
    assert_eq!((bss.size, bss.offset), (8, 0));
}

#[test]
fn the_program_starts_at_start_and_reaches_its_data() {
    let file = linked(PROGRAM).unwrap();
    let text = section(&file, "__text");
    let main = command(&file, LC_MAIN);
    assert_eq!(read64(&file, main + 8), text.offset as u64);
    assert_eq!(
        adrp_target(&file, text.address, 1),
        section(&file, "__data").address
    );
    let data = section(&file, "__data");
    assert_eq!(
        &file[data.offset as usize..data.offset as usize + 3],
        b"hi\0"
    );
    let constants = section(&file, "__const");
    assert_eq!(read64(&file, constants.offset as usize), 1);
}

#[test]
fn system_calls_go_through_stubs_that_dyld_binds() {
    let file = linked(PROGRAM).unwrap();
    let text = section(&file, "__text");
    let stubs = section(&file, "__stubs");
    let got = section(&file, "__got");
    assert_eq!((stubs.size, stubs.reserved1, stubs.reserved2), (24, 0, 12));
    assert_eq!((got.size, got.reserved1), (16, 2));
    // `bl write` and `bl _exit` are the third and fifth instructions
    for (i, place) in [text.address + 8, text.address + 16]
        .into_iter()
        .enumerate()
    {
        let stub = stubs.address + 12 * i as u64;
        assert_eq!(branch_target(&file, place), stub);
        assert_eq!(adrp_target(&file, stub, 8), got.address + 8 * i as u64);
        assert_eq!(read32(&file, (stub + 8 - BASE) as usize), 0xd61f_0200);
        // each slot binds import i, and the first chains to the next slot 8 bytes on
        let bind = read64(&file, (got.address + 8 * i as u64 - BASE) as usize);
        let next = if i == 0 { 2 } else { 0 };
        assert_eq!(bind, i as u64 | next << 51 | 1 << 63);
    }

    let fixups = command(&file, LC_DYLD_CHAINED_FIXUPS);
    let blob = read32(&file, fixups + 8) as usize;
    let header = |field: usize| read32(&file, blob + 4 * field) as usize;
    assert_eq!((header(0), header(4), header(5), header(6)), (0, 2, 1, 0));
    let starts = blob + header(1);
    assert_eq!(read32(&file, starts), 4);
    let data_starts = read32(&file, starts + 12) as usize;
    let mut offsets = [0; 4];
    for (i, offset) in offsets.iter_mut().enumerate() {
        *offset = read32(&file, starts + 4 + 4 * i);
    }
    assert_eq!(offsets, [0, 0, data_starts as u32, 0]);
    let segment = starts + data_starts;
    assert_eq!(
        u16::from_le_bytes([file[segment + 4], file[segment + 5]]),
        0x4000
    );
    assert_eq!(
        u16::from_le_bytes([file[segment + 6], file[segment + 7]]),
        6
    );
    assert_eq!(read64(&file, segment + 8), got.address - BASE);
    // one page, whose chain starts at its first byte
    assert_eq!(
        u16::from_le_bytes([file[segment + 20], file[segment + 21]]),
        1
    );
    assert_eq!(
        u16::from_le_bytes([file[segment + 22], file[segment + 23]]),
        0
    );
    assert_eq!(read32(&file, segment) as usize, 24);
    let mut imports = Vec::new();
    for i in 0..header(4) {
        let import = read32(&file, blob + header(2) + 4 * i);
        assert_eq!(import & 0x1ff, 1); // libSystem, not weak
        let at = blob + header(3) + (import >> 9) as usize;
        imports.push(name(&file[at..]));
    }
    assert_eq!(imports, ["_write", "__exit"]);
}

#[test]
fn the_symbol_table_names_labels_and_imports() {
    let file = linked(PROGRAM).unwrap();
    let symtab = command(&file, LC_SYMTAB);
    let (symbols, count) = (read32(&file, symtab + 8), read32(&file, symtab + 12));
    let strings = read32(&file, symtab + 16) as usize;
    let mut found = Vec::new();
    for i in 0..count as usize {
        let at = symbols as usize + 16 * i;
        let symbol = name(&file[strings + read32(&file, at) as usize..]);
        found.push((symbol, file[at + 4], file[at + 5]));
    }
    let expected = [
        ("_start", N_SECT, 1),
        ("helper", N_SECT, 1),
        ("table", N_SECT, 3),
        ("message", N_SECT, 5),
        ("counter", N_SECT, 6),
        ("__mh_execute_header", N_SECT | N_EXT, 1),
        ("_write", N_EXT, 0),
        ("__exit", N_EXT, 0),
    ];
    let expected: Vec<(String, u8, u8)> = expected
        .iter()
        .map(|(name, kind, section)| (name.to_string(), *kind, *section))
        .collect();
    assert_eq!(found, expected);
    let dysymtab = command(&file, LC_DYSYMTAB);
    let field = |i: usize| read32(&file, dysymtab + 8 + 4 * i);
    assert_eq!(
        [field(0), field(1), field(2), field(3), field(4), field(5)],
        [0, 5, 5, 1, 6, 2]
    );
    let indirect = field(12) as usize;
    let entries: Vec<u32> = (0..field(13) as usize)
        .map(|i| read32(&file, indirect + 4 * i))
        .collect();
    assert_eq!(entries, [6, 7, 6, 7]);
}

#[test]
fn the_signature_hashes_every_page_before_it() {
    let file = linked(PROGRAM).unwrap();
    let signature = command(&file, LC_CODE_SIGNATURE);
    let (offset, size) = (
        read32(&file, signature + 8) as usize,
        read32(&file, signature + 12) as usize,
    );
    assert_eq!(offset % 16, 0);
    assert_eq!(offset + size, file.len());
    let blob = &file[offset..];
    assert_eq!(read_be32(blob, 0), CSMAGIC_EMBEDDED_SIGNATURE);
    assert_eq!(read_be32(blob, 4) as usize, size);
    assert_eq!((read_be32(blob, 8), read_be32(blob, 12)), (1, 0));
    let directory = &blob[read_be32(blob, 16) as usize..];
    assert_eq!(read_be32(directory, 0), CSMAGIC_CODEDIRECTORY);
    assert_eq!(read_be32(directory, 8), 0x20400);
    assert_eq!(read_be32(directory, 12), 0x2_0002);
    let identifier = read_be32(directory, 20) as usize;
    assert_eq!(name(&directory[identifier..]), "out");
    let pages = read_be32(directory, 28) as usize;
    assert_eq!(read_be32(directory, 32) as usize, offset);
    assert_eq!(pages, offset.div_ceil(4096));
    assert_eq!(directory[36..40], [32, 2, 0, 12]);
    let text = &segments(&file)[1];
    let exec_limit = u64::from_be_bytes(directory[72..80].try_into().unwrap());
    assert_eq!(exec_limit, text.file_size);
    let hashes = read_be32(directory, 16) as usize;
    for (i, page) in file[..offset].chunks(4096).enumerate() {
        let hash = &directory[hashes + 32 * i..hashes + 32 * i + 32];
        assert_eq!(hash, sha256(page), "page {i}");
    }
    assert_eq!(hashes + 32 * pages, read_be32(directory, 4) as usize);
}

#[test]
fn linking_is_reproducible() {
    let first = linked(PROGRAM).unwrap();
    assert_eq!(first, linked(PROGRAM).unwrap());
    let uuid = command(&first, LC_UUID) + 8;
    assert_ne!(first[uuid..uuid + 16], [0; 16]);
    let other = linked(&PROGRAM.replace("#7", "#8")).unwrap();
    assert_ne!(first[uuid..uuid + 16], other[uuid..uuid + 16]);
}

#[test]
fn unknown_and_misused_symbols_are_errors() {
    for (text, error) in [
        (
            "\t.text\n\t.globl\t_start\n_start:\n\tbl\tnowhere\n",
            "undefined label 'nowhere'",
        ),
        (
            "\t.text\n\t.globl\t_start\n_start:\n\tadrp\tx0, write\n",
            "'write' is a system function, so it can only be called",
        ),
        ("\t.text\nmain:\n\tbl\t_exit\n", "'_start' is never defined"),
    ] {
        assert_eq!(linked(text), Err(error.to_string()), "{text}");
    }
}

#[test]
fn every_call_the_runtime_makes_is_a_system_function() {
    for call in Sys::ALL {
        if let Some(function) = call.macos() {
            assert!(SYSTEM_FUNCTIONS.contains(&function), "{function}");
        }
    }
}
