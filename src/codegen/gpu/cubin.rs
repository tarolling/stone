//! Reads the kernels out of a cubin, the ELF file NVIDIA's `ptxas` compiles PTX into.
//!
//! A cubin holds, for each kernel, its machine code (SASS) in `.text.<name>` and the facts a
//! launch needs in `.nv.info.<name>` and `.nv.constant0.<name>`: how many registers a thread
//! uses, and where in constant bank 0 the kernel's parameters start and how large they are.

/// One kernel of a cubin, with what a launch needs to know about it.
///
/// For example, the kernel `stone_k0` of `a * 2.0 + b` over lists of floats takes four 8-byte
/// parameters, so `param_size` is 32.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CubinKernel {
    pub name: String,
    /// The kernel's machine code, including any routine it calls, such as float division's.
    pub sass: Vec<u8>,
    /// The registers each thread uses.
    pub registers: u32,
    /// Where the parameters start in constant bank 0, which the driver fills in first.
    pub param_offset: u32,
    /// The bytes of parameters the launch writes at `param_offset`.
    pub param_size: u32,
    /// The bytes of static shared memory a block needs.
    pub shared: u32,
    /// The bytes of stack each thread needs in local memory, 0 unless the kernel spills
    /// registers or indexes an array at run time.
    pub stack: u32,
    /// The kernel's literal pool, constant bank 2, which `ptxas` fills with constants too wide
    /// for an instruction, such as a 64-bit 1; empty when the kernel needs none.
    pub literals: Vec<u8>,
}

/// Returns the kernels of `cubin`, in the order of its symbol table.
///
/// For example, a cubin of two entry points gives two kernels, and bytes that are not a 64-bit
/// little-endian ELF file are the error `not a cubin: ...`.
pub fn parse(cubin: &[u8]) -> Result<Vec<CubinKernel>, String> {
    let file = Elf::new(cubin)?;
    let symbols = file
        .section_named(".symtab")
        .ok_or_else(|| not_a_cubin("no symbol table"))?;
    let stacks = file.stacks()?;
    let mut kernels = Vec::new();
    for (index, entry) in symbols.data.as_chunks::<SYMBOL_SIZE>().0.iter().enumerate() {
        let name_offset = u32_at(entry, 0).ok_or_else(|| not_a_cubin("short symbol"))?;
        let is_function = entry[4] & 0xf == STT_FUNC;
        let is_global = entry[4] >> 4 == STB_GLOBAL;
        if !is_function || !is_global {
            continue;
        }
        let name = file.symbol_name(symbols.link, name_offset)?;
        let text = file
            .section_named(&format!(".text.{name}"))
            .ok_or_else(|| not_a_cubin(&format!("no code for kernel '{name}'")))?;
        let (param_offset, param_size) = file.parameters(&name)?;
        let shared = file
            .section_named(&format!(".nv.shared.{name}"))
            .map_or(0, |section| section.size);
        let literals = file
            .section_named(&format!(".nv.constant2.{name}"))
            .map_or_else(Vec::new, |section| section.data.to_vec());
        kernels.push(CubinKernel {
            name,
            sass: text.data.to_vec(),
            // `ptxas` keeps the register count in the high byte of the section's info field
            registers: text.info >> 24,
            param_offset,
            param_size,
            shared: shared as u32,
            stack: stacks
                .iter()
                .filter(|&&(symbol, _)| symbol as usize == index)
                .map(|&(_, size)| size)
                .max()
                .unwrap_or(0),
            literals,
        });
    }
    Ok(kernels)
}

/// Returns the names of the sections of `cubin` that a program loading its kernels itself would
/// have to place in memory or patch, which stone cannot do: constant banks other than bank 0 and
/// a kernel's own bank 2, global and local memory, and relocations of code.
///
/// For example, a cubin of kernels that use only registers, their parameters, literals, and
/// global memory through pointers gives no names, and one with a shared `.nv.constant3` gives
/// that name.
pub fn unplaceable_sections(cubin: &[u8]) -> Result<Vec<String>, String> {
    let file = Elf::new(cubin)?;
    Ok(file
        .sections
        .iter()
        .filter(|section| is_unplaceable(&section.name))
        .map(|section| section.name.clone())
        .collect())
}

/// Returns whether a section called `name` holds data or relocations that a loader must place.
/// A kernel's own literal pool, `.nv.constant2.<kernel>`, is placed with its kernel.
fn is_unplaceable(name: &str) -> bool {
    let constant_bank = name
        .strip_prefix(".nv.constant")
        .is_some_and(|rest| !rest.starts_with('0') && !rest.starts_with("2."));
    constant_bank
        || name.starts_with(".nv.global")
        || name.starts_with(".nv.local")
        || name.starts_with(".rel.text")
        || name.starts_with(".rela.text")
}

const SYMBOL_SIZE: usize = 24;
const SECTION_HEADER_SIZE: usize = 64;
const STT_FUNC: u8 = 2;
const STB_GLOBAL: u8 = 1;

/// `.nv.info` records with a value of 4 bytes or more have this format byte.
const FORMAT_BLOCK: u8 = 4;
/// The record that says where a kernel's parameters sit in constant bank 0: the kernel's symbol
/// index, then the offset and the size.
const ATTR_PARAM_CBANK: u8 = 0x0a;
/// The record that gives the bytes of a kernel's parameters.
const ATTR_PARAM_SIZE: u8 = 0x19;
/// The records of the global `.nv.info` that give a kernel's stack frame and the least stack it
/// needs, each the kernel's symbol index and then the bytes.
const ATTR_FRAME_SIZE: u8 = 0x11;
const ATTR_MIN_STACK_SIZE: u8 = 0x12;

fn not_a_cubin(reason: &str) -> String {
    format!("not a cubin: {reason}")
}

fn u16_at(bytes: &[u8], at: usize) -> Option<u16> {
    Some(u16::from_le_bytes(bytes.get(at..at + 2)?.try_into().ok()?))
}

fn u32_at(bytes: &[u8], at: usize) -> Option<u32> {
    Some(u32::from_le_bytes(bytes.get(at..at + 4)?.try_into().ok()?))
}

fn u64_at(bytes: &[u8], at: usize) -> Option<u64> {
    Some(u64::from_le_bytes(bytes.get(at..at + 8)?.try_into().ok()?))
}

/// A section of a cubin: its name, header fields, and bytes.
struct Section<'a> {
    name: String,
    size: u64,
    link: u32,
    info: u32,
    data: &'a [u8],
}

/// The section headers of a 64-bit little-endian ELF file, with every offset checked.
struct Elf<'a> {
    sections: Vec<Section<'a>>,
}

impl<'a> Elf<'a> {
    fn new(bytes: &'a [u8]) -> Result<Elf<'a>, String> {
        if bytes.get(..4) != Some(b"\x7fELF")
            || bytes.get(4) != Some(&2)
            || bytes.get(5) != Some(&1)
        {
            return Err(not_a_cubin("not a 64-bit little-endian ELF file"));
        }
        let short = || not_a_cubin("truncated headers");
        let table = u64_at(bytes, 0x28).ok_or_else(short)? as usize;
        let count = u16_at(bytes, 0x3c).ok_or_else(short)? as usize;
        let names = u16_at(bytes, 0x3e).ok_or_else(short)? as usize;
        let mut headers = Vec::new();
        for index in 0..count {
            let at = table
                .checked_add(index * SECTION_HEADER_SIZE)
                .ok_or_else(short)?;
            let header = bytes.get(at..at + SECTION_HEADER_SIZE).ok_or_else(short)?;
            let offset = u64_at(header, 0x18).ok_or_else(short)? as usize;
            let size = u64_at(header, 0x20).ok_or_else(short)?;
            let kind = u32_at(header, 4).ok_or_else(short)?;
            // a section with no bytes in the file (NOBITS) has no data to read
            let data = if kind == 8 {
                &[][..]
            } else {
                let end = offset.checked_add(size as usize).ok_or_else(short)?;
                bytes.get(offset..end).ok_or_else(short)?
            };
            headers.push((
                u32_at(header, 0).ok_or_else(short)?,
                size,
                u32_at(header, 0x28).ok_or_else(short)?,
                u32_at(header, 0x2c).ok_or_else(short)?,
                data,
            ));
        }
        let strings = headers.get(names).ok_or_else(short)?.4;
        let sections = headers
            .iter()
            .map(|&(name, size, link, info, data)| {
                Ok(Section {
                    name: c_string(strings, name as usize).ok_or_else(short)?,
                    size,
                    link,
                    info,
                    data,
                })
            })
            .collect::<Result<_, String>>()?;
        Ok(Elf { sections })
    }

    fn section_named(&self, name: &str) -> Option<&Section<'a>> {
        self.sections.iter().find(|section| section.name == name)
    }

    /// Returns the name at `offset` of the string table section `table`.
    fn symbol_name(&self, table: u32, offset: u32) -> Result<String, String> {
        let strings = self
            .sections
            .get(table as usize)
            .ok_or_else(|| not_a_cubin("symbols name a missing string table"))?;
        c_string(strings.data, offset as usize).ok_or_else(|| not_a_cubin("bad symbol name"))
    }

    /// Returns where kernel `name`'s parameters start in constant bank 0 and how many bytes
    /// they take, from the records of its `.nv.info.<name>` section.
    fn parameters(&self, name: &str) -> Result<(u32, u32), String> {
        let info = self
            .section_named(&format!(".nv.info.{name}"))
            .ok_or_else(|| not_a_cubin(&format!("no launch information for '{name}'")))?;
        let (mut offset, mut size) = (None, None);
        for record in records(info.data)? {
            match record {
                Record::Block(ATTR_PARAM_CBANK, block) => {
                    offset = u16_at(block, 4).map(u32::from);
                }
                Record::Value(ATTR_PARAM_SIZE, value) => size = Some(u32::from(value)),
                _ => {}
            }
        }
        match (offset, size) {
            (Some(offset), Some(size)) => Ok((offset, size)),
            _ => Err(not_a_cubin(&format!("no parameters recorded for '{name}'"))),
        }
    }

    /// Returns the stack sizes the global `.nv.info` section records, as pairs of a kernel's
    /// symbol index and bytes; a cubin without the section records none.
    fn stacks(&self) -> Result<Vec<(u32, u32)>, String> {
        let Some(info) = self.section_named(".nv.info") else {
            return Ok(Vec::new());
        };
        let mut stacks = Vec::new();
        for record in records(info.data)? {
            if let Record::Block(ATTR_FRAME_SIZE | ATTR_MIN_STACK_SIZE, block) = record {
                let short = || not_a_cubin("truncated stack size");
                stacks.push((
                    u32_at(block, 0).ok_or_else(short)?,
                    u32_at(block, 4).ok_or_else(short)?,
                ));
            }
        }
        Ok(stacks)
    }
}

/// A record of an `.nv.info` section: an attribute with a block of bytes, or with a 16-bit value.
enum Record<'a> {
    Block(u8, &'a [u8]),
    Value(u8, u16),
}

/// Returns the records of an `.nv.info` section, each a format byte, an attribute byte, and a
/// 16-bit value that, for a block, is the length of the bytes that follow.
///
/// For example, `04 0a 08 00` then 8 bytes is a block of attribute `0x0a`, and `03 19 20 00` is
/// attribute `0x19` with the value 32.
fn records(data: &[u8]) -> Result<Vec<Record<'_>>, String> {
    let mut records = Vec::new();
    let mut at = 0;
    while at + 4 <= data.len() {
        let (format, attribute) = (data[at], data[at + 1]);
        let value = u16_at(data, at + 2).unwrap_or(0);
        at += 4;
        if format == FORMAT_BLOCK {
            let end = at + value as usize;
            let block = data
                .get(at..end)
                .ok_or_else(|| not_a_cubin("truncated launch information"))?;
            records.push(Record::Block(attribute, block));
            at = end;
        } else {
            records.push(Record::Value(attribute, value));
        }
    }
    Ok(records)
}

/// Returns the null-terminated text at `offset` of `strings`.
fn c_string(strings: &[u8], offset: usize) -> Option<String> {
    let rest = strings.get(offset..)?;
    let end = rest.iter().position(|&byte| byte == 0)?;
    String::from_utf8(rest[..end].to_vec()).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    const TWO_KERNELS: &[u8] = include_bytes!("testdata/two_kernels.sm_75.cubin");

    #[test]
    fn a_cubin_gives_each_kernel_its_code_registers_and_parameters() {
        let kernels = parse(TWO_KERNELS).unwrap();
        let names: Vec<&str> = kernels.iter().map(|k| k.name.as_str()).collect();
        assert_eq!(names, ["stone_k1", "stone_k0"]);
        let k0 = &kernels[1];
        // `ptxas` reports 18 registers and four 8-byte parameters after the driver's 0x160 bytes
        assert_eq!(k0.sass.len(), 0x200);
        assert_eq!(k0.registers, 18);
        assert_eq!((k0.param_offset, k0.param_size), (0x160, 32));
        assert_eq!(k0.shared, 0);
        let k1 = &kernels[0];
        // the division routine is part of the kernel's code
        assert_eq!(k1.sass.len(), 0x980);
        assert_eq!(k1.registers, 26);
        assert_eq!((k1.param_offset, k1.param_size), (0x160, 40));
        assert_eq!((k0.stack, k1.stack), (0, 0));
    }

    #[test]
    fn a_kernel_with_a_local_array_needs_a_stack() {
        // `ptxas -v` reports "64 bytes cumulative stack size" for its `.local` array
        let kernels = parse(include_bytes!("testdata/stack.sm_75.cubin")).unwrap();
        assert_eq!(kernels[0].name, "stone_k0");
        assert_eq!(kernels[0].stack, 64);
    }

    #[test]
    fn a_kernel_with_a_literal_pool_carries_its_bytes() {
        // `xs * 3 + 1` over ints: ptxas keeps the 64-bit 1 in `.nv.constant2.stone_k0`
        let cubin = include_bytes!("testdata/literals.sm_75.cubin");
        let kernels = parse(cubin).unwrap();
        assert_eq!(kernels[0].literals, [1, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(kernels[1..].len(), 0);
        assert_eq!(unplaceable_sections(cubin).unwrap(), Vec::<String>::new());
        let two = parse(TWO_KERNELS).unwrap();
        assert!(two.iter().all(|kernel| kernel.literals.is_empty()));
    }

    #[test]
    fn a_cubin_of_plain_kernels_needs_nothing_placed_besides_their_code() {
        assert_eq!(
            unplaceable_sections(TWO_KERNELS).unwrap(),
            Vec::<String>::new()
        );
    }

    #[test]
    fn sections_that_hold_data_or_relocations_are_unplaceable() {
        for name in [
            ".nv.constant3",
            ".nv.global",
            ".nv.global.init",
            ".nv.local.stone_k0",
            ".rel.text.stone_k0",
            ".rela.text.stone_k0",
        ] {
            assert!(is_unplaceable(name), "{name}");
        }
        for name in [
            ".text.stone_k0",
            ".nv.constant0.stone_k0",
            ".nv.constant2.stone_k0",
            ".nv.info.stone_k0",
            ".nv.shared.stone_k0",
            ".nv.callgraph",
            ".debug_frame",
            ".rel.debug_frame",
            ".nv.rel.action",
        ] {
            assert!(!is_unplaceable(name), "{name}");
        }
    }

    #[test]
    fn bytes_that_are_not_a_cubin_are_an_error() {
        assert!(parse(b"").unwrap_err().starts_with("not a cubin"));
        assert!(parse(&[0; 64]).unwrap_err().starts_with("not a cubin"));
        let mut cut = TWO_KERNELS.to_vec();
        cut.truncate(0x1500);
        assert!(parse(&cut).unwrap_err().starts_with("not a cubin"));
    }
}
