use super::*;
use crate::codegen::Architecture;
use crate::codegen::asm::assemble;

fn u16_at(file: &[u8], at: usize) -> u16 {
    u16::from_le_bytes(file[at..at + 2].try_into().unwrap())
}

fn u32_at(file: &[u8], at: usize) -> u32 {
    u32::from_le_bytes(file[at..at + 4].try_into().unwrap())
}

fn u64_at(file: &[u8], at: usize) -> u64 {
    u64::from_le_bytes(file[at..at + 8].try_into().unwrap())
}

/// A section header read back: its name, address, size in memory, file offset, size in the
/// file, and flags.
#[derive(Debug)]
struct Header {
    name: String,
    rva: u32,
    virtual_size: u32,
    offset: u32,
    raw_size: u32,
    flags: u32,
}

/// Reads back the headers of an image: the optional header's start and the sections.
fn headers(file: &[u8]) -> (usize, Vec<Header>) {
    assert_eq!(&file[..2], b"MZ");
    let pe = u32_at(file, 0x3c) as usize;
    assert_eq!(&file[pe..pe + 4], b"PE\0\0");
    let coff = pe + 4;
    assert_eq!(u16_at(file, coff), IMAGE_FILE_MACHINE_AMD64);
    let count = u16_at(file, coff + 2) as usize;
    let optional = coff + 20;
    let mut sections = Vec::new();
    for i in 0..count {
        let at = optional + OPTIONAL_HEADER_SIZE as usize + 40 * i;
        let name = String::from_utf8_lossy(&file[at..at + 8])
            .trim_end_matches('\0')
            .to_string();
        sections.push(Header {
            name,
            virtual_size: u32_at(file, at + 8),
            rva: u32_at(file, at + 12),
            raw_size: u32_at(file, at + 16),
            offset: u32_at(file, at + 20),
            flags: u32_at(file, at + 36),
        });
    }
    (optional, sections)
}

/// Returns the file offset of the address `rva`.
fn offset_of(sections: &[Header], rva: u32) -> usize {
    let section = sections
        .iter()
        .find(|s| s.rva <= rva && rva < s.rva + s.virtual_size.max(s.raw_size))
        .unwrap_or_else(|| panic!("no section holds {rva:#x}"));
    (section.offset + rva - section.rva) as usize
}

/// Returns the null-terminated text at the address `rva`.
fn text_at(file: &[u8], sections: &[Header], rva: u32) -> String {
    let start = offset_of(sections, rva);
    let end = start + file[start..].iter().position(|&b| b == 0).unwrap();
    String::from_utf8_lossy(&file[start..end]).into_owned()
}

/// Returns each DLL the image imports with the functions it imports from it, read from the
/// import directory.
fn imports(file: &[u8]) -> Vec<(String, Vec<String>)> {
    let (optional, sections) = headers(file);
    let directory = u32_at(file, optional + 112 + 8 * IMPORT_DIRECTORY);
    let mut dlls = Vec::new();
    let mut at = offset_of(&sections, directory);
    loop {
        let lookup = u32_at(file, at);
        if lookup == 0 {
            break;
        }
        let name = text_at(file, &sections, u32_at(file, at + 12));
        let address = u32_at(file, at + 16);
        let mut functions = Vec::new();
        for (k, table) in [lookup, address].into_iter().enumerate() {
            let mut entry = offset_of(&sections, table);
            let mut names = Vec::new();
            loop {
                let hint_name = u64_at(file, entry);
                if hint_name == 0 {
                    break;
                }
                names.push(text_at(file, &sections, hint_name as u32 + 2));
                entry += 8;
            }
            if k == 0 {
                functions = names;
            } else {
                assert_eq!(
                    names, functions,
                    "the address table matches the lookup table"
                );
            }
        }
        dlls.push((name, functions));
        at += IMPORT_DESCRIPTOR_SIZE as usize;
    }
    dlls
}

const PROGRAM: &str = "\
\t.intel_syntax noprefix
\t.text
\t.globl\t_start
_start:
\tlea\trcx, [rip + message]
\tcall\tWriteFile
\tcall\tBCryptGenRandom
\tmov\trax, QWORD PTR [rip + counter]
\tcall\tExitProcess
\t.section\t.rodata
message:
\t.string \"hi\"
\t.data
value:
\t.quad\t7
\t.bss
counter:
\t.zero\t8
";

#[test]
fn programs_import_what_they_call_by_dll() {
    let object = assemble(PROGRAM, Architecture::X64).unwrap();
    let file = link(&object).unwrap();
    assert_eq!(
        imports(&file),
        [
            (
                "KERNEL32.dll".to_string(),
                vec!["ExitProcess".to_string(), "WriteFile".to_string()]
            ),
            (
                "bcrypt.dll".to_string(),
                vec!["BCryptGenRandom".to_string()]
            ),
        ]
    );
}

#[test]
fn sections_are_laid_out_on_their_alignments() {
    let object = assemble(PROGRAM, Architecture::X64).unwrap();
    let file = link(&object).unwrap();
    let (optional, sections) = headers(&file);
    assert_eq!(u16_at(&file, optional), 0x20b);
    assert_eq!(u64_at(&file, optional + 24), BASE);
    assert_eq!(u16_at(&file, optional + 68), IMAGE_SUBSYSTEM_WINDOWS_CUI);
    assert_eq!(u64_at(&file, optional + 72), STACK_SIZE);
    assert_eq!(u64_at(&file, optional + 80), STACK_SIZE);
    let names: Vec<&str> = sections.iter().map(|s| s.name.as_str()).collect();
    assert_eq!(names, [".text", ".rdata", ".data", ".reloc"]);
    for section in &sections {
        assert_eq!(section.rva as u64 % SECTION_ALIGN, 0, "{section:?}");
        assert_eq!(section.offset as u64 % FILE_ALIGN, 0, "{section:?}");
        assert_eq!(section.raw_size as u64 % FILE_ALIGN, 0, "{section:?}");
    }
    assert_eq!(
        sections[0].flags & IMAGE_SCN_MEM_EXECUTE,
        IMAGE_SCN_MEM_EXECUTE
    );
    assert_eq!(sections[1].flags & IMAGE_SCN_MEM_WRITE, 0);
    // .bss follows .data in memory but not in the file
    assert_eq!(sections[2].virtual_size, 16 + 8);
    assert_eq!(sections[2].raw_size as u64, FILE_ALIGN);
    let last = sections.last().unwrap();
    let image_size = u32_at(&file, optional + 56) as u64;
    assert_eq!(
        image_size,
        ((last.rva + last.virtual_size) as u64).next_multiple_of(SECTION_ALIGN)
    );
    assert_eq!(file.len() as u32, last.offset + last.raw_size);
    // one empty block of base relocations
    let relocations = u32_at(&file, optional + 112 + 8 * RELOCATION_DIRECTORY);
    assert_eq!(relocations, last.rva);
    assert_eq!(u32_at(&file, offset_of(&sections, relocations) + 4), 8);
}

#[test]
fn calls_reach_thunks_that_jump_through_the_address_table() {
    let object = assemble(PROGRAM, Architecture::X64).unwrap();
    let file = link(&object).unwrap();
    let (optional, sections) = headers(&file);
    let entry = u32_at(&file, optional + 16);
    let text = &sections[0];
    assert_eq!(entry, text.rva);
    let iat = u32_at(&file, optional + 112 + 8 * IAT_DIRECTORY);
    let names = imports(&file);
    let order: Vec<&str> = names
        .iter()
        .flat_map(|(_, f)| f.iter().map(String::as_str))
        .collect();
    // the call of WriteFile is the second instruction, after a 7-byte lea
    let call = offset_of(&sections, entry + 7);
    assert_eq!(file[call], 0xe8);
    let thunk = (entry + 12) as i64
        + i32::from_le_bytes(file[call + 1..call + 5].try_into().unwrap()) as i64;
    let at = offset_of(&sections, thunk as u32);
    assert_eq!(&file[at..at + 2], &[0xff, 0x25]);
    let slot = thunk + 6 + i32::from_le_bytes(file[at + 2..at + 6].try_into().unwrap()) as i64;
    // the slots of KERNEL32.dll come first, in the order the names are, then its null
    let index = order.iter().position(|f| *f == "WriteFile").unwrap();
    assert_eq!(slot, iat as i64 + 8 * index as i64);
    // nothing reaches the system directly
    assert!(!file.windows(2).any(|pair| pair == [0x0f, 0x05]));
}

#[test]
fn images_are_stamped_and_checksummed_reproducibly() {
    let object = assemble(PROGRAM, Architecture::X64).unwrap();
    let file = link(&object).unwrap();
    assert_eq!(link(&object).unwrap(), file);
    assert_eq!(&file[64..78], &DOS_STUB[..14]);
    let (optional, _) = headers(&file);
    let time = u32_at(&file, optional - 20 + 4);
    assert_ne!(time, 0);
    // the checksum is the folded sum of every other 16-bit word, plus the length
    let at = optional + 64;
    let mut sum: u32 = 0;
    for i in (0..file.len()).step_by(2) {
        if i == at || i == at + 2 {
            continue;
        }
        sum += u16_at(&file, i) as u32;
        sum = (sum & 0xffff) + (sum >> 16);
    }
    assert_eq!(u32_at(&file, at), sum + file.len() as u32);
}

#[test]
fn unknown_functions_are_errors() {
    let object = assemble(
        "\t.intel_syntax noprefix\n\t.text\n_start:\n\tcall\tprintf\n",
        Architecture::X64,
    )
    .unwrap();
    assert_eq!(link(&object), Err("undefined label 'printf'".to_string()));
    let object = assemble(
        "\t.intel_syntax noprefix\n\t.text\nmain:\n\tret\n",
        Architecture::X64,
    )
    .unwrap();
    assert_eq!(link(&object), Err("'_start' is never defined".to_string()));
}

#[test]
fn every_function_the_runtime_calls_is_imported() {
    use crate::codegen::x64::system::Sys;
    // a program whose runtime makes every call, so the Windows shims call every function
    let source = "\
use os
use time
use fs
use random
print(args(), os.env(\"A\"), os.hostname(), os.cpu_count(), os.pid(), os.cwd())
print(time.now(), time.clock(), random.randint(1, 6), input(\"\"))
print(fs.read(\"a\"), fs.write(\"a\", \"b\"), fs.append(\"a\", \"b\"), fs.list_dir(\".\"))
print(fs.exists(\"a\"), fs.remove(\"a\"), fs.make_dir(\"d\"))
time.sleep(0)
xs = [0]
for i in range(100000):
    xs.append(i)
";
    let (_, module) = crate::driver::load(
        std::path::Path::new("main.stn"),
        source,
        &crate::project::MapSources::default(),
    );
    let module = module.unwrap().module;
    let assembly = crate::codegen::Target::X64_WINDOWS
        .generator()
        .assemble(&module)
        .unwrap();
    for call in Sys::ALL {
        assert!(
            assembly.contains(&format!("{}:", call.shim())),
            "no shim for {call:?}"
        );
    }
    let object = assemble(&assembly, Architecture::X64).unwrap();
    let file = link(&object).unwrap();
    let imported: Vec<String> = imports(&file).into_iter().flat_map(|(_, f)| f).collect();
    for (_, functions) in IMPORTS {
        for function in functions {
            assert!(
                imported.iter().any(|f| f == function),
                "{function} is never called"
            );
        }
    }
}
