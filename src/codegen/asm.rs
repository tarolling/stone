//! The built-in assembler, which turns the GNU assembler text both backends emit into machine
//! code, so `stone build` needs no assembler or linker installed.
//!
//! It accepts exactly the subset of GNU as syntax the backends write: labels, the directives
//! `.text`, `.data`, `.bss`, `.section`, `.globl`, `.p2align`, `.balign`, `.zero`, `.quad`, and
//! `.string`, and the instructions [`x64`] and [`arm64`] encode. [`assemble`] returns an
//! [`Object`], whose sections still refer to labels through [`Fixup`]s, and
//! [`elf::link`](crate::codegen::elf::link) lays it out and resolves them. For example,
//! assembling `main:\n\tret` for x86-64 gives an `.text` of `[0xc3]` and the symbol `main` at 0.

pub mod arm64;
pub mod x64;

use std::collections::{HashMap, HashSet};

use crate::codegen::Architecture;

/// A section of the executable. Every byte the assembler writes goes in one of these.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Section {
    Text,
    Rodata,
    Data,
    Bss,
}

impl Section {
    /// Every section, in the order the executable lays them out.
    pub const ALL: [Section; 4] = [Section::Text, Section::Rodata, Section::Data, Section::Bss];

    /// Returns the section's ELF name, such as `.text`.
    pub fn name(self) -> &'static str {
        match self {
            Section::Text => ".text",
            Section::Rodata => ".rodata",
            Section::Data => ".data",
            Section::Bss => ".bss",
        }
    }

    fn index(self) -> usize {
        self as usize
    }
}

/// How a [`Fixup`] patches the bytes at its offset once the address of its symbol is known.
///
/// For example, `X64Rel32` writes `S + A - P` as a little-endian `i32`, where `S` is the symbol's
/// address, `A` the addend, and `P` the address of the fixup itself.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FixupKind {
    /// A 32-bit displacement relative to the fixup's own address (`call`, `jmp`, `[rip + x]`).
    X64Rel32,
    /// The 26-bit word offset of `b` and `bl`.
    Arm64Branch26,
    /// The 19-bit word offset of `b.cond`, `cbz`, and `cbnz`.
    Arm64Branch19,
    /// The 14-bit word offset of `tbz` and `tbnz`.
    Arm64Branch14,
    /// The 21-bit page offset of `adrp`.
    Arm64AdrpPage21,
    /// The low 12 bits of the address, in an `add` immediate.
    Arm64AddLo12,
    /// The low 12 bits of the address, shifted right by the access size's log2 (`0` for a byte,
    /// `3` for a doubleword), in a load or store's unsigned offset.
    Arm64LdStLo12(u8),
}

/// A place in a section that holds a symbol's address, filled in when the program is linked.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fixup {
    pub section: Section,
    pub offset: u64,
    pub kind: FixupKind,
    pub symbol: String,
    pub addend: i64,
}

/// A label, with the section and offset it marks.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Symbol {
    pub name: String,
    pub section: Section,
    pub offset: u64,
    /// Whether a `.globl` named it.
    pub global: bool,
}

/// Assembled code and data, before it is placed at an address.
///
/// For example, assembling `\t.bss\nx:\n\t.zero\t8` gives a `bss` of 8 and the symbol `x` at
/// offset 0 of [`Section::Bss`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Object {
    pub text: Vec<u8>,
    pub rodata: Vec<u8>,
    pub data: Vec<u8>,
    /// The size of `.bss`, which takes no room in the file.
    pub bss: u64,
    /// Every label, in the order the text defines them.
    pub symbols: Vec<Symbol>,
    pub fixups: Vec<Fixup>,
}

impl Object {
    /// Returns the bytes of `section`, which are empty for `.bss`.
    pub fn bytes(&self, section: Section) -> &[u8] {
        match section {
            Section::Text => &self.text,
            Section::Rodata => &self.rodata,
            Section::Data => &self.data,
            Section::Bss => &[],
        }
    }

    /// Returns the size of `section` once loaded.
    pub fn size(&self, section: Section) -> u64 {
        match section {
            Section::Bss => self.bss,
            _ => self.bytes(section).len() as u64,
        }
    }
}

/// A fixup inside one instruction's bytes, at `offset` from the instruction's start.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LocalFixup {
    pub offset: usize,
    pub kind: FixupKind,
    pub symbol: String,
    pub addend: i64,
}

/// What one instruction encodes to.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Encoded {
    /// Bytes whose length never changes, with any fixups inside them.
    Fixed(Vec<u8>, Vec<LocalFixup>),
    /// An x86-64 jump that takes the 2-byte form `short` followed by an 8-bit displacement when
    /// its target is near, and otherwise the bytes `long` followed by a 32-bit displacement.
    ///
    /// For example, `jmp .L1` is `Branch { short: 0xeb, long: vec![0xe9], .. }`.
    Branch {
        short: u8,
        long: Vec<u8>,
        target: String,
    },
}

/// One architecture's instruction encoder.
pub trait Encoder {
    /// Returns what starts a comment, such as `#` on x86-64.
    fn comment(&self) -> &'static str;
    /// Returns the bytes that fill a gap in `.text` before an aligned label.
    fn nop(&self) -> &'static [u8];
    /// Encodes one instruction, given its mnemonic and operands as written.
    ///
    /// For example, `instruction("ret", &[])` is `Encoded::Fixed(vec![0xc3], vec![])` on x86-64.
    fn instruction(&self, mnemonic: &str, operands: &[&str]) -> Result<Encoded, String>;
}

/// Returns the encoder for `arch`.
pub fn encoder(arch: Architecture) -> Box<dyn Encoder> {
    match arch {
        Architecture::X64 => Box::new(x64::X64Encoder),
        Architecture::Arm64 => Box::new(arm64::Arm64Encoder),
    }
}

/// Assembles `text`, the assembly a backend emitted for `arch`, into an [`Object`].
///
/// Errors name the line, such as
/// `line 3 (\`frob\trax\`): unknown instruction 'frob'`.
pub fn assemble(text: &str, arch: Architecture) -> Result<Object, String> {
    let encoder = encoder(arch);
    let mut assembler = Assembler::default();
    for (index, raw) in text.lines().enumerate() {
        assembler
            .line(raw, encoder.as_ref())
            .map_err(|message| format!("line {} (`{}`): {message}", index + 1, raw.trim()))?;
    }
    assembler.finish(encoder.as_ref())
}

/// Something placed in a section, in order.
#[derive(Debug)]
enum Item {
    Bytes(Vec<u8>, Vec<LocalFixup>),
    Branch {
        short: u8,
        long: Vec<u8>,
        target: String,
        /// Whether relaxation has given up on the short form.
        grown: bool,
    },
    Align(u64),
    Zero(u64),
    Label(String),
}

impl Item {
    /// Returns how many bytes the item takes when it starts at `offset`.
    fn size(&self, offset: u64) -> u64 {
        match self {
            Item::Bytes(bytes, _) => bytes.len() as u64,
            Item::Branch { long, grown, .. } => {
                if *grown {
                    long.len() as u64 + 4
                } else {
                    2
                }
            }
            Item::Align(align) => offset.next_multiple_of(*align) - offset,
            Item::Zero(size) => *size,
            Item::Label(_) => 0,
        }
    }
}

#[derive(Default)]
struct Assembler {
    /// The section lines go to, or `None` after a section the program cannot hold, such as
    /// `.note.GNU-stack`.
    section: Option<Section>,
    items: [Vec<Item>; 4],
    globals: HashSet<String>,
    labels: HashSet<String>,
}

impl Assembler {
    fn line(&mut self, raw: &str, encoder: &dyn Encoder) -> Result<(), String> {
        let line = strip_comment(raw, encoder.comment()).trim();
        if line.is_empty() {
            return Ok(());
        }
        if let Some(name) = line.strip_suffix(':')
            && is_symbol(name)
        {
            if !self.labels.insert(name.to_string()) {
                return Err(format!("label '{name}' is defined twice"));
            }
            return self.push(Item::Label(name.to_string()));
        }
        let (head, rest) = match line.find(char::is_whitespace) {
            Some(end) => (&line[..end], line[end..].trim()),
            None => (line, ""),
        };
        if head.starts_with('.') {
            return self.directive(head, rest);
        }
        let operands = split_operands(rest);
        let item = match encoder.instruction(head, &operands)? {
            Encoded::Fixed(bytes, fixups) => Item::Bytes(bytes, fixups),
            Encoded::Branch {
                short,
                long,
                target,
            } => Item::Branch {
                short,
                long,
                target,
                grown: false,
            },
        };
        if self.section != Some(Section::Text) {
            return Err("instructions must be in .text".to_string());
        }
        self.push(item)
    }

    fn directive(&mut self, name: &str, rest: &str) -> Result<(), String> {
        match name {
            ".intel_syntax" if rest == "noprefix" => {}
            ".text" => self.section = Some(Section::Text),
            ".data" => self.section = Some(Section::Data),
            ".bss" => self.section = Some(Section::Bss),
            ".section" => {
                let section = rest.split(',').next().unwrap_or("").trim();
                self.section = match section {
                    ".text" => Some(Section::Text),
                    ".rodata" => Some(Section::Rodata),
                    ".data" => Some(Section::Data),
                    ".bss" => Some(Section::Bss),
                    // only says the stack is not executable, which the executable always says
                    ".note.GNU-stack" => None,
                    _ => return Err(format!("unknown section '{section}'")),
                };
            }
            ".globl" => {
                if !is_symbol(rest) {
                    return Err(format!("'{rest}' is not a symbol"));
                }
                self.globals.insert(rest.to_string());
            }
            ".p2align" => {
                let power = parse_int(rest)
                    .filter(|power| (0..16).contains(power))
                    .ok_or_else(|| format!("bad alignment '{rest}'"))?;
                self.push(Item::Align(1 << power))?;
            }
            ".balign" => {
                let align = parse_int(rest)
                    .filter(|align| (1..=1 << 15).contains(align) && align & (align - 1) == 0)
                    .ok_or_else(|| format!("bad alignment '{rest}'"))?;
                self.push(Item::Align(align as u64))?;
            }
            ".zero" => {
                let size = parse_int(rest)
                    .filter(|size| *size >= 0)
                    .ok_or_else(|| format!("bad size '{rest}'"))?;
                self.push(Item::Zero(size as u64))?;
            }
            ".quad" => {
                let value = parse_int(rest).ok_or_else(|| format!("bad number '{rest}'"))?;
                self.push(Item::Bytes(value.to_le_bytes().to_vec(), vec![]))?;
            }
            ".string" => {
                let mut bytes = parse_string(rest)?;
                bytes.push(0);
                self.push(Item::Bytes(bytes, vec![]))?;
            }
            _ => return Err(format!("unknown directive '{name}'")),
        }
        Ok(())
    }

    fn push(&mut self, item: Item) -> Result<(), String> {
        let Some(section) = self.section else {
            return Err("this section cannot hold anything".to_string());
        };
        if section == Section::Bss && matches!(item, Item::Bytes(..) | Item::Branch { .. }) {
            return Err(".bss can only hold zeros".to_string());
        }
        self.items[section.index()].push(item);
        Ok(())
    }

    /// Picks the size of every jump, then writes out each section.
    fn finish(mut self, encoder: &dyn Encoder) -> Result<Object, String> {
        let text = Section::Text.index();
        // like GNU as, every jump starts short and only grows, so this reaches a fixed point
        loop {
            let offsets = label_offsets(&self.items[text]);
            let mut offset = 0;
            let mut grow = Vec::new();
            for (i, item) in self.items[text].iter().enumerate() {
                if let Item::Branch {
                    target,
                    grown: false,
                    ..
                } = item
                {
                    let fits = offsets
                        .get(target.as_str())
                        .is_some_and(|&to| i8::try_from(to as i64 - (offset + 2) as i64).is_ok());
                    if !fits {
                        grow.push(i);
                    }
                }
                offset += item.size(offset);
            }
            if grow.is_empty() {
                break;
            }
            for i in grow {
                if let Item::Branch { grown, .. } = &mut self.items[text][i] {
                    *grown = true;
                }
            }
        }

        let mut object = Object::default();
        for section in Section::ALL {
            let items = &self.items[section.index()];
            let offsets = label_offsets(items);
            let mut bytes = Vec::new();
            let mut offset = 0u64;
            for item in items {
                let size = item.size(offset);
                match item {
                    Item::Bytes(code, fixups) => {
                        bytes.extend_from_slice(code);
                        for fixup in fixups {
                            object.fixups.push(Fixup {
                                section,
                                offset: offset + fixup.offset as u64,
                                kind: fixup.kind,
                                symbol: fixup.symbol.clone(),
                                addend: fixup.addend,
                            });
                        }
                    }
                    Item::Branch {
                        short,
                        long,
                        target,
                        grown,
                    } => {
                        if *grown {
                            bytes.extend_from_slice(long);
                            bytes.extend_from_slice(&[0; 4]);
                            object.fixups.push(Fixup {
                                section,
                                offset: offset + long.len() as u64,
                                kind: FixupKind::X64Rel32,
                                symbol: target.clone(),
                                addend: -4,
                            });
                        } else {
                            let to = offsets[target.as_str()];
                            bytes.push(*short);
                            bytes.push((to as i64 - (offset + 2) as i64) as u8);
                        }
                    }
                    Item::Align(_) if section == Section::Text => {
                        let nop = encoder.nop();
                        if size % nop.len() as u64 != 0 {
                            return Err(format!("{} is misaligned", section.name()));
                        }
                        for _ in 0..size / nop.len() as u64 {
                            bytes.extend_from_slice(nop);
                        }
                    }
                    Item::Align(_) | Item::Zero(_) => {
                        if section != Section::Bss {
                            bytes.resize(bytes.len() + size as usize, 0);
                        }
                    }
                    Item::Label(name) => object.symbols.push(Symbol {
                        name: name.clone(),
                        section,
                        offset,
                        global: self.globals.contains(name),
                    }),
                }
                offset += size;
            }
            match section {
                Section::Text => object.text = bytes,
                Section::Rodata => object.rodata = bytes,
                Section::Data => object.data = bytes,
                Section::Bss => object.bss = offset,
            }
        }

        let defined: HashSet<&str> = object.symbols.iter().map(|s| s.name.as_str()).collect();
        for name in &self.globals {
            if !defined.contains(name.as_str()) {
                return Err(format!(".globl names '{name}', which is never defined"));
            }
        }
        for fixup in &object.fixups {
            if !defined.contains(fixup.symbol.as_str()) {
                return Err(format!("undefined label '{}'", fixup.symbol));
            }
        }
        Ok(object)
    }
}

/// Returns the offset of every label among `items`, with every jump at its current size.
fn label_offsets(items: &[Item]) -> HashMap<&str, u64> {
    let mut offsets = HashMap::new();
    let mut offset = 0;
    for item in items {
        if let Item::Label(name) = item {
            offsets.insert(name.as_str(), offset);
        }
        offset += item.size(offset);
    }
    offsets
}

/// Returns `line` up to any comment that starts with `comment` outside a string.
///
/// For example, `strip_comment("\tmov\trax, 1 # one", "#")` is `"\tmov\trax, 1 "`.
fn strip_comment<'a>(line: &'a str, comment: &str) -> &'a str {
    let mut quoted = false;
    let mut escaped = false;
    for (i, c) in line.char_indices() {
        if quoted {
            match c {
                _ if escaped => escaped = false,
                '\\' => escaped = true,
                '"' => quoted = false,
                _ => {}
            }
        } else if c == '"' {
            quoted = true;
        } else if line[i..].starts_with(comment) {
            return &line[..i];
        }
    }
    line
}

/// Splits operands at the commas that are not inside brackets or parentheses.
///
/// For example, `"x0, [sp, #16]!"` splits into `["x0", "[sp, #16]!"]`.
pub fn split_operands(text: &str) -> Vec<&str> {
    let mut operands = Vec::new();
    let mut depth = 0i32;
    let mut start = 0;
    for (i, c) in text.char_indices() {
        match c {
            '[' | '(' => depth += 1,
            ']' | ')' => depth -= 1,
            ',' if depth == 0 => {
                operands.push(text[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }
    let last = text[start..].trim();
    if !last.is_empty() || !operands.is_empty() {
        operands.push(last);
    }
    operands
}

/// Returns whether `name` can be a label, such as `fn.area`, `.Lstone_true`, or `_start`.
pub fn is_symbol(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_' || c == '.')
        && chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
}

/// Parses an integer written in decimal or hex, with an optional `-`, the way GNU as reads
/// `.quad` and immediates. Values up to `u64::MAX` wrap to their two's complement.
///
/// For example, `parse_int("0x10")` is `Some(16)`, `parse_int("-8")` is `Some(-8)`, and
/// `parse_int("0xffffffffffffffff")` is `Some(-1)`.
pub fn parse_int(text: &str) -> Option<i64> {
    let (negative, digits) = match text.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, text),
    };
    let magnitude = match digits
        .strip_prefix("0x")
        .or_else(|| digits.strip_prefix("0X"))
    {
        Some(hex) => u64::from_str_radix(hex, 16).ok()?,
        None if digits.bytes().all(|b| b.is_ascii_digit()) => digits.parse::<u64>().ok()?,
        None => return None,
    };
    if negative {
        if magnitude > 1 << 63 {
            return None;
        }
        Some((magnitude as i64).wrapping_neg())
    } else {
        Some(magnitude as i64)
    }
}

/// Parses a sum of integers such as `8 + 4095`, which the backends write where a constant is
/// clearer as two parts, or a single integer as [`parse_int`] does.
///
/// For example, `parse_sum("8 + 4095")` is `Some(4103)` and `parse_sum("16 - 1")` is `Some(15)`.
pub fn parse_sum(text: &str) -> Option<i64> {
    let mut total = 0i64;
    let mut negative = false;
    let mut start = 0;
    for (i, c) in text.char_indices() {
        if (c == '+' || c == '-') && !text[..i].trim().is_empty() {
            let term = parse_int(text[start..i].trim())?;
            total = if negative {
                total.checked_sub(term)?
            } else {
                total.checked_add(term)?
            };
            negative = c == '-';
            start = i + 1;
        }
    }
    let term = parse_int(text[start..].trim())?;
    if negative {
        total.checked_sub(term)
    } else {
        total.checked_add(term)
    }
}

/// Parses a quoted string with the escapes `Context::string_literals` writes, returning its
/// bytes.
///
/// For example, `parse_string("\"a\\n\"")` is `Ok(b"a\n".to_vec())`.
fn parse_string(text: &str) -> Result<Vec<u8>, String> {
    let inner = text
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .ok_or_else(|| format!("bad string {text}"))?;
    let mut bytes = Vec::with_capacity(inner.len());
    let mut chars = inner.bytes();
    while let Some(b) = chars.next() {
        if b != b'\\' {
            bytes.push(b);
            continue;
        }
        bytes.push(match chars.next() {
            Some(b'n') => b'\n',
            Some(b't') => b'\t',
            Some(b'r') => b'\r',
            Some(b'\\') => b'\\',
            Some(b'"') => b'"',
            other => {
                return Err(format!(
                    "unknown escape '\\{}'",
                    other.map(|b| b as char).unwrap_or(' ')
                ));
            }
        });
    }
    Ok(bytes)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn x64(text: &str) -> Result<Object, String> {
        assemble(text, Architecture::X64)
    }

    #[test]
    fn labels_mark_offsets_in_their_sections() {
        let object = x64("\t.text\nmain:\n\tret\nnext:\n\t.bss\nx:\n\t.zero\t8\ny:\n").unwrap();
        assert_eq!(object.text, [0xc3]);
        assert_eq!(object.bss, 8);
        let symbols: Vec<(&str, Section, u64)> = object
            .symbols
            .iter()
            .map(|s| (s.name.as_str(), s.section, s.offset))
            .collect();
        assert_eq!(
            symbols,
            [
                ("main", Section::Text, 0),
                ("next", Section::Text, 1),
                ("x", Section::Bss, 0),
                ("y", Section::Bss, 8),
            ]
        );
    }

    #[test]
    fn data_directives_write_bytes() {
        let object = x64(
            "\t.section\t.rodata\n\t.quad\t0x102\n\t.quad\t-1\n\t.data\n\t.string \"a\\\"\\n\"\n\
             \t.balign\t8\n\t.quad\t4611686018427387904\n",
        )
        .unwrap();
        assert_eq!(
            object.rodata,
            [
                2, 1, 0, 0, 0, 0, 0, 0, 255, 255, 255, 255, 255, 255, 255, 255
            ]
        );
        assert_eq!(
            object.data,
            [b'a', b'"', b'\n', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0x40]
        );
    }

    #[test]
    fn alignment_pads_with_zeros_in_data_and_nops_in_text() {
        let object =
            x64("\t.data\n\t.string \"\"\n\t.p2align\t2\nx:\n\t.text\n\tret\n\t.p2align\t2\n")
                .unwrap();
        assert_eq!(object.data, [0, 0, 0, 0]);
        assert_eq!(object.symbols[0].offset, 4);
        assert_eq!(object.text, [0xc3, 0x90, 0x90, 0x90]);
    }

    #[test]
    fn comments_and_globals_are_understood() {
        let object = x64(
            "\t.intel_syntax noprefix\n\t# a comment\n\t.text\n\t.globl\tmain\nmain:\n\tret # done\n\
             \t.data\n\t.string \"#not a comment\"\n\t.section\t.note.GNU-stack,\"\",@progbits\n",
        )
        .unwrap();
        assert!(object.symbols[0].global);
        assert_eq!(object.data, b"#not a comment\0");
    }

    #[test]
    fn mistakes_are_errors_naming_the_line() {
        for (text, error) in [
            (
                "\t.text\n\tfrob\n",
                "line 2 (`frob`): unknown instruction 'frob'",
            ),
            (
                "\t.text\nx:\nx:\n",
                "line 3 (`x:`): label 'x' is defined twice",
            ),
            (
                "\t.weird\n",
                "line 1 (`.weird`): unknown directive '.weird'",
            ),
            (
                "\t.bss\n\t.quad\t1\n",
                "line 2 (`.quad\t1`): .bss can only hold zeros",
            ),
            (
                "\t.data\n\tret\n",
                "line 2 (`ret`): instructions must be in .text",
            ),
            ("\t.text\n\tcall\tnowhere\n", "undefined label 'nowhere'"),
            (
                "\t.globl\tmain\n",
                ".globl names 'main', which is never defined",
            ),
            (
                "\t.data\n\t.string \"\\q\"\n",
                "line 2 (`.string \"\\q\"`): unknown escape '\\q'",
            ),
        ] {
            assert_eq!(x64(text), Err(error.to_string()), "{text}");
        }
    }

    #[test]
    fn jumps_stay_short_until_their_target_is_too_far() {
        let near = format!("\t.text\n\tjmp\t.L1\n{}.L1:\n", "\tnop\n".repeat(127));
        let object = x64(&near).unwrap();
        assert_eq!(&object.text[..2], [0xeb, 127]);
        assert!(object.fixups.is_empty());

        let far = format!("\t.text\n\tjmp\t.L1\n{}.L1:\n", "\tnop\n".repeat(128));
        let object = x64(&far).unwrap();
        assert_eq!(&object.text[..5], [0xe9, 0, 0, 0, 0]);
        assert_eq!(
            object.fixups,
            [Fixup {
                section: Section::Text,
                offset: 1,
                kind: FixupKind::X64Rel32,
                symbol: ".L1".to_string(),
                addend: -4,
            }]
        );

        let backward = format!("\t.text\n.L1:\n{}\tjne\t.L1\n", "\tnop\n".repeat(126));
        let object = x64(&backward).unwrap();
        assert_eq!(&object.text[126..], [0x75, (-128i8) as u8]);
    }

    #[test]
    fn growing_one_jump_can_grow_another() {
        // the first jump fits only while the second stays short, which it cannot
        let text = format!(
            "\t.text\n\tjmp\t.L1\n{}\tjmp\t.L2\n.L1:\n{}.L2:\n",
            "\tnop\n".repeat(124),
            "\tnop\n".repeat(128)
        );
        let object = x64(&text).unwrap();
        assert_eq!(object.text[0], 0xe9);
        assert_eq!(object.text[5 + 124], 0xe9);
    }

    #[test]
    fn integers_parse_like_gnu_as() {
        assert_eq!(parse_int("42"), Some(42));
        assert_eq!(parse_int("-0x10"), Some(-16));
        assert_eq!(parse_int("0x8000000000000000"), Some(i64::MIN));
        assert_eq!(parse_int("18446744073709551615"), Some(-1));
        assert_eq!(parse_int("-9223372036854775808"), Some(i64::MIN));
        assert_eq!(parse_int("-9223372036854775809"), None);
        assert_eq!(parse_int("1.5"), None);
        assert_eq!(parse_int(""), None);
    }

    #[test]
    fn operands_split_outside_brackets() {
        assert_eq!(split_operands("x0, [sp, #16]!"), ["x0", "[sp, #16]!"]);
        assert_eq!(split_operands("st(0)"), ["st(0)"]);
        assert_eq!(split_operands(""), Vec::<&str>::new());
    }
}
