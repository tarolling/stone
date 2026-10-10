//! Encodes the x86-64 instructions the backend emits, in GNU as's Intel syntax.
//!
//! Where an instruction has several encodings, this picks the one GNU as picks without `-O`,
//! so the two produce the same bytes: `mov rax, rbx` is `48 89 d8` (the store form), an
//! immediate that fits in a signed byte uses the `83` form, and so on.

use super::{Encoded, Encoder, FixupKind, LocalFixup, is_symbol, parse_int, parse_sum};

/// The x86-64 [`Encoder`].
pub struct X64Encoder;

impl Encoder for X64Encoder {
    fn comment(&self) -> &'static str {
        "#"
    }

    fn nop(&self) -> &'static [u8] {
        &[0x90]
    }

    fn instruction(&self, mnemonic: &str, operands: &[&str]) -> Result<Encoded, String> {
        let operands = operands
            .iter()
            .map(|text| parse_operand(text))
            .collect::<Result<Vec<_>, _>>()?;
        encode(mnemonic, &operands)
    }
}

/// Which register file a register belongs to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    General,
    /// `ah`, `ch`, `dh`, or `bh`, which no instruction with a REX prefix can name.
    HighByte,
    Xmm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Reg {
    num: u8,
    /// The size in bytes, 16 for `xmm` registers.
    size: u8,
    kind: Kind,
}

impl Reg {
    /// Whether naming the register needs a REX prefix even when no REX bit is set, as `sil`
    /// does.
    fn needs_rex(self) -> bool {
        self.kind == Kind::General && self.size == 1 && (4..8).contains(&self.num)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Mem {
    /// The size from `QWORD PTR` and the like, if written.
    size: Option<u8>,
    base: Option<u8>,
    /// The index register and its scale.
    index: Option<(u8, u8)>,
    disp: i64,
    /// The label of a `[rip + label]` operand.
    rip: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Operand {
    Reg(Reg),
    Imm(i64),
    Mem(Mem),
    Label(String),
    /// An x87 stack register, `st(i)`.
    St(u8),
}

/// Returns the register `name` names, such as a 64-bit general register 0 for `rax`.
fn register(name: &str) -> Option<Reg> {
    const R64: [&str; 8] = ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"];
    const R32: [&str; 8] = ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"];
    const R16: [&str; 8] = ["ax", "cx", "dx", "bx", "sp", "bp", "si", "di"];
    const R8: [&str; 8] = ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil"];
    const HIGH: [&str; 4] = ["ah", "ch", "dh", "bh"];
    let general = |num: usize, size| {
        Some(Reg {
            num: num as u8,
            size,
            kind: Kind::General,
        })
    };
    for (names, size) in [(R64, 8), (R32, 4), (R16, 2), (R8, 1)] {
        if let Some(num) = names.iter().position(|n| *n == name) {
            return general(num, size);
        }
    }
    if let Some(num) = HIGH.iter().position(|n| *n == name) {
        return Some(Reg {
            num: num as u8 + 4,
            size: 1,
            kind: Kind::HighByte,
        });
    }
    if let Some(num) = name.strip_prefix("xmm") {
        let num: u8 = num.parse().ok().filter(|n| *n < 16)?;
        return Some(Reg {
            num,
            size: 16,
            kind: Kind::Xmm,
        });
    }
    let rest = name.strip_prefix('r')?;
    let digits = rest.trim_end_matches(['d', 'w', 'b']);
    let num: u8 = digits.parse().ok().filter(|n| (8..16).contains(n))?;
    let size = match &rest[digits.len()..] {
        "" => 8,
        "d" => 4,
        "w" => 2,
        "b" => 1,
        _ => return None,
    };
    general(num as usize, size)
}

fn parse_operand(text: &str) -> Result<Operand, String> {
    if let Some(reg) = register(text) {
        return Ok(Operand::Reg(reg));
    }
    if text == "st" {
        return Ok(Operand::St(0));
    }
    if let Some(inner) = text
        .strip_prefix("st(")
        .and_then(|rest| rest.strip_suffix(')'))
    {
        let i: u8 = inner
            .parse()
            .ok()
            .filter(|i| *i < 8)
            .ok_or_else(|| format!("bad register '{text}'"))?;
        return Ok(Operand::St(i));
    }
    if let Some(value) = parse_sum(text) {
        return Ok(Operand::Imm(value));
    }
    if text.contains('[') {
        return parse_memory(text).map(Operand::Mem);
    }
    if is_symbol(text) {
        return Ok(Operand::Label(text.to_string()));
    }
    Err(format!("bad operand '{text}'"))
}

/// Parses an operand such as `QWORD PTR [rbp - 8 + rcx * 8]` or `[rip + stone.live]`.
fn parse_memory(text: &str) -> Result<Mem, String> {
    let bad = || format!("bad memory operand '{text}'");
    let open = text.find('[').ok_or_else(bad)?;
    let inner = text[open + 1..].strip_suffix(']').ok_or_else(bad)?;
    let size = match text[..open].trim().to_ascii_lowercase().as_str() {
        "" => None,
        "byte ptr" => Some(1),
        "word ptr" => Some(2),
        "dword ptr" => Some(4),
        "qword ptr" => Some(8),
        _ => return Err(bad()),
    };
    let mut mem = Mem {
        size,
        base: None,
        index: None,
        disp: 0,
        rip: None,
    };
    let mut rip = false;
    // split into signed terms, such as `rbp`, `-8`, and `rcx * 8`
    let mut terms = Vec::new();
    let mut negative = false;
    let mut start = 0;
    for (i, c) in inner.char_indices() {
        if c == '+' || c == '-' {
            terms.push((negative, inner[start..i].trim()));
            negative = c == '-';
            start = i + 1;
        }
    }
    terms.push((negative, inner[start..].trim()));
    for (i, (negative, term)) in terms.into_iter().enumerate() {
        if term.is_empty() {
            if i == 0 && !negative {
                continue;
            }
            return Err(bad());
        }
        if let Some((reg, scale)) = term.split_once('*') {
            let reg = register(reg.trim()).filter(|r| r.size == 8 && r.kind == Kind::General);
            let scale = parse_int(scale.trim()).filter(|s| [1, 2, 4, 8].contains(s));
            match (reg, scale, negative, mem.index) {
                (Some(reg), Some(scale), false, None) => {
                    mem.index = Some((reg.num, scale as u8));
                }
                _ => return Err(bad()),
            }
        } else if term == "rip" && !negative {
            rip = true;
        } else if let Some(reg) = register(term) {
            if reg.size != 8 || reg.kind != Kind::General || negative {
                return Err(bad());
            }
            if mem.base.is_none() {
                mem.base = Some(reg.num);
            } else if mem.index.is_none() {
                mem.index = Some((reg.num, 1));
            } else {
                return Err(bad());
            }
        } else if let Some(value) = parse_int(term) {
            mem.disp = if negative {
                mem.disp.wrapping_sub(value)
            } else {
                mem.disp.wrapping_add(value)
            };
        } else if is_symbol(term) && !negative && mem.rip.is_none() {
            mem.rip = Some(term.to_string());
        } else {
            return Err(bad());
        }
    }
    if mem.index.is_some_and(|(index, _)| index == 4) {
        return Err(format!("rsp cannot be an index in '{text}'"));
    }
    match (rip, &mem.rip) {
        (true, Some(_)) if mem.base.is_none() && mem.index.is_none() => {}
        (false, None) => {}
        _ => return Err(bad()),
    }
    if i32::try_from(mem.disp).is_err() {
        return Err(bad());
    }
    Ok(mem)
}

/// The register or memory operand of a ModRM byte.
enum Rm<'a> {
    Reg(Reg),
    Mem(&'a Mem),
}

/// An instruction under construction: prefixes, REX bits, opcode, ModRM operands, and an
/// immediate, assembled in that order by [`Inst::finish`].
#[derive(Default)]
struct Inst<'a> {
    /// Mandatory and operand-size prefixes, which go before any REX prefix.
    prefixes: Vec<u8>,
    w: bool,
    /// The REX.B bit for an opcode that holds its register in its low three bits.
    opcode_b: bool,
    /// Whether a byte register such as `sil` needs a REX prefix.
    force_rex: bool,
    /// Whether a register such as `ah` rules out a REX prefix.
    forbid_rex: bool,
    opcode: Vec<u8>,
    modrm: Option<(u8, Rm<'a>)>,
    imm: Vec<u8>,
}

impl<'a> Inst<'a> {
    fn new(opcode: &[u8]) -> Self {
        Inst {
            opcode: opcode.to_vec(),
            ..Default::default()
        }
    }

    /// Sets the operand size: `0x66` for 2 bytes, REX.W for 8.
    fn size(mut self, size: u8) -> Self {
        match size {
            2 => self.prefixes.push(0x66),
            8 => self.w = true,
            _ => {}
        }
        self
    }

    fn prefix(mut self, prefix: u8) -> Self {
        self.prefixes.push(prefix);
        self
    }

    fn w(mut self, w: bool) -> Self {
        self.w = w;
        self
    }

    fn note(&mut self, reg: Reg) {
        self.force_rex |= reg.needs_rex();
        self.forbid_rex |= reg.kind == Kind::HighByte;
    }

    /// Sets the ModRM byte's `reg` field to a register.
    fn reg(mut self, reg: Reg, rm: Rm<'a>) -> Self {
        self.note(reg);
        if let Rm::Reg(r) = rm {
            self.note(r);
        }
        self.modrm = Some((reg.num, rm));
        self
    }

    /// Sets the ModRM byte's `reg` field to an opcode extension, as in `83 /0`.
    fn ext(mut self, ext: u8, rm: Rm<'a>) -> Self {
        if let Rm::Reg(r) = rm {
            self.note(r);
        }
        self.modrm = Some((ext, rm));
        self
    }

    /// Adds a register to the opcode's low three bits, as in `50+r`.
    fn plus(mut self, reg: Reg) -> Self {
        self.note(reg);
        *self.opcode.last_mut().unwrap() += reg.num & 7;
        self.opcode_b = reg.num >= 8;
        self
    }

    fn imm(mut self, value: i64, size: usize) -> Self {
        self.imm = value.to_le_bytes()[..size].to_vec();
        self
    }

    fn finish(self) -> Result<Encoded, String> {
        let mut bytes = self.prefixes.clone();
        let mut rex = 0x40 | if self.w { 8 } else { 0 };
        if self.opcode_b {
            rex |= 1;
        }
        let mut fixups = Vec::new();
        let mut modrm = Vec::new();
        if let Some((reg, rm)) = &self.modrm {
            rex |= (reg >> 3 & 1) << 2;
            match rm {
                Rm::Reg(r) => {
                    rex |= r.num >> 3 & 1;
                    modrm.push(0xc0 | (reg & 7) << 3 | (r.num & 7));
                }
                Rm::Mem(mem) => {
                    if let Some(base) = mem.base {
                        rex |= base >> 3 & 1;
                    }
                    if let Some((index, _)) = mem.index {
                        rex |= (index >> 3 & 1) << 1;
                    }
                    if let Some(label) = &mem.rip {
                        modrm.push((reg & 7) << 3 | 0b101);
                        fixups.push((modrm.len(), label.clone(), mem.disp));
                        modrm.extend_from_slice(&[0; 4]);
                    } else {
                        encode_memory(*reg, mem, &mut modrm);
                    }
                }
            }
        }
        if rex != 0x40 || self.force_rex {
            if self.forbid_rex {
                return Err("ah, bh, ch, and dh cannot be used here".to_string());
            }
            bytes.push(rex);
        }
        bytes.extend_from_slice(&self.opcode);
        let start = bytes.len();
        bytes.extend_from_slice(&modrm);
        let fixups = fixups
            .into_iter()
            .map(|(offset, symbol, disp)| LocalFixup {
                offset: start + offset,
                kind: FixupKind::X64Rel32,
                symbol,
                // the displacement counts from the end of the instruction
                addend: disp - 4 - self.imm.len() as i64,
            })
            .collect();
        bytes.extend_from_slice(&self.imm);
        Ok(Encoded::Fixed(bytes, fixups))
    }
}

/// Writes the ModRM byte, any SIB byte, and any displacement for a memory operand that is not
/// `rip`-relative.
fn encode_memory(reg: u8, mem: &Mem, out: &mut Vec<u8>) {
    let reg = (reg & 7) << 3;
    let scale = |s: u8| match s {
        1 => 0,
        2 => 1,
        4 => 2,
        _ => 3,
    };
    let disp = mem.disp;
    let Some(base) = mem.base else {
        // no base: a 32-bit displacement stands in for it
        let sib = match mem.index {
            Some((index, s)) => scale(s) << 6 | (index & 7) << 3 | 0b101,
            None => 0x25,
        };
        out.extend_from_slice(&[reg | 0b100, sib]);
        out.extend_from_slice(&(disp as i32).to_le_bytes());
        return;
    };
    // rbp and r13 have no form without a displacement
    let mode = if disp == 0 && base & 7 != 5 {
        0b00
    } else if i8::try_from(disp).is_ok() {
        0b01
    } else {
        0b10
    };
    match mem.index {
        Some((index, s)) => {
            out.push(mode << 6 | reg | 0b100);
            out.push(scale(s) << 6 | (index & 7) << 3 | (base & 7));
        }
        None if base & 7 == 4 => {
            out.push(mode << 6 | reg | 0b100);
            out.push(0x24);
        }
        None => out.push(mode << 6 | reg | (base & 7)),
    }
    match mode {
        0b01 => out.push(disp as u8),
        0b10 => out.extend_from_slice(&(disp as i32).to_le_bytes()),
        _ => {}
    }
}

/// Returns the condition code of a `jcc`, `setcc`, or `cmovcc` suffix, such as 4 for `e`.
fn condition(suffix: &str) -> Option<u8> {
    Some(match suffix {
        "o" => 0,
        "no" => 1,
        "b" | "c" | "nae" => 2,
        "ae" | "nb" | "nc" => 3,
        "e" | "z" => 4,
        "ne" | "nz" => 5,
        "be" | "na" => 6,
        "a" | "nbe" => 7,
        "s" => 8,
        "ns" => 9,
        "p" | "pe" => 10,
        "np" | "po" => 11,
        "l" | "nge" => 12,
        "ge" | "nl" => 13,
        "le" | "ng" => 14,
        "g" | "nle" => 15,
        _ => return None,
    })
}

/// Returns the size of an operand the instruction applies to, from a register or a memory
/// operand's `PTR`.
fn size_of(operand: &Operand) -> Option<u8> {
    match operand {
        Operand::Reg(reg) => Some(reg.size),
        Operand::Mem(mem) => mem.size,
        _ => None,
    }
}

/// Returns an operand as the ModRM `rm` field, if it can be one.
fn rm(operand: &Operand) -> Option<Rm<'_>> {
    match operand {
        Operand::Reg(reg) if reg.kind != Kind::Xmm => Some(Rm::Reg(*reg)),
        Operand::Mem(mem) => Some(Rm::Mem(mem)),
        _ => None,
    }
}

/// Like [`rm`], but also accepts an `xmm` register.
fn xmm_rm(operand: &Operand) -> Option<Rm<'_>> {
    match operand {
        Operand::Reg(reg) => Some(Rm::Reg(*reg)),
        Operand::Mem(mem) => Some(Rm::Mem(mem)),
        _ => None,
    }
}

fn gpr(operand: &Operand) -> Option<Reg> {
    match operand {
        Operand::Reg(reg) if reg.kind != Kind::Xmm => Some(*reg),
        _ => None,
    }
}

fn xmm(operand: &Operand) -> Option<Reg> {
    match operand {
        Operand::Reg(reg) if reg.kind == Kind::Xmm => Some(*reg),
        _ => None,
    }
}

/// Truncates an immediate to the operand size the way GNU as reads it, so `0xffffffff` in a
/// 32-bit instruction is -1 and fits in a signed byte. Returns `None` if it does not fit.
fn immediate(value: i64, size: u8) -> Option<i64> {
    match size {
        1 => (-128..=255).contains(&value).then_some(value as i8 as i64),
        2 => (-32768..=65535)
            .contains(&value)
            .then_some(value as i16 as i64),
        4 => (i32::MIN as i64..=u32::MAX as i64)
            .contains(&value)
            .then_some(value as i32 as i64),
        _ => i32::try_from(value).is_ok().then_some(value),
    }
}

/// Returns how many bytes an immediate takes in an instruction of `size` that has no 64-bit
/// immediate form.
fn imm_len(size: u8) -> usize {
    match size {
        1 => 1,
        2 => 2,
        _ => 4,
    }
}

fn fits_i8(value: i64) -> bool {
    i8::try_from(value).is_ok()
}

/// The arithmetic instructions that share one encoding pattern, with the opcode extension
/// each uses in the `83 /n` form.
const ARITHMETIC: [(&str, u8); 8] = [
    ("add", 0),
    ("or", 1),
    ("adc", 2),
    ("sbb", 3),
    ("and", 4),
    ("sub", 5),
    ("xor", 6),
    ("cmp", 7),
];

/// Instructions on one register or memory operand, with their opcode for bytes, their opcode
/// otherwise, and their extension.
const UNARY: [(&str, u8, u8, u8); 8] = [
    ("inc", 0xfe, 0xff, 0),
    ("dec", 0xfe, 0xff, 1),
    ("not", 0xf6, 0xf7, 2),
    ("neg", 0xf6, 0xf7, 3),
    ("mul", 0xf6, 0xf7, 4),
    ("imul", 0xf6, 0xf7, 5),
    ("div", 0xf6, 0xf7, 6),
    ("idiv", 0xf6, 0xf7, 7),
];

/// Scalar double instructions written `op xmm, xmm/m64`, with their prefix and opcode.
const SSE: [(&str, u8, u8); 7] = [
    ("addsd", 0xf2, 0x58),
    ("mulsd", 0xf2, 0x59),
    ("subsd", 0xf2, 0x5c),
    ("divsd", 0xf2, 0x5e),
    ("ucomisd", 0x66, 0x2e),
    ("xorpd", 0x66, 0x57),
    ("movapd", 0x66, 0x28),
];

fn encode(mnemonic: &str, operands: &[Operand]) -> Result<Encoded, String> {
    use Operand::*;
    let bad = || {
        Err(format!(
            "'{mnemonic}' cannot take these operands ({} given)",
            operands.len()
        ))
    };
    let mismatch = || Err(format!("operand sizes of '{mnemonic}' do not match"));
    let unknown = || Err(format!("unknown instruction '{mnemonic}'"));

    if let Some(&(_, ext)) = ARITHMETIC.iter().find(|(name, _)| *name == mnemonic) {
        let [dst, src] = operands else { return bad() };
        let base = ext * 8;
        return match (dst, src) {
            (Reg(_) | Mem(_), Reg(src)) if src.kind != Kind::Xmm => {
                let Some(dst_rm) = rm(dst) else { return bad() };
                if size_of(dst).is_some_and(|size| size != src.size) {
                    return mismatch();
                }
                let opcode = if src.size == 1 { base } else { base + 1 };
                Inst::new(&[opcode])
                    .size(src.size)
                    .reg(*src, dst_rm)
                    .finish()
            }
            (Reg(dst), Mem(mem)) if dst.kind != Kind::Xmm => {
                if mem.size.is_some_and(|size| size != dst.size) {
                    return mismatch();
                }
                let opcode = if dst.size == 1 { base + 2 } else { base + 3 };
                Inst::new(&[opcode])
                    .size(dst.size)
                    .reg(*dst, Rm::Mem(mem))
                    .finish()
            }
            (Reg(_) | Mem(_), Imm(value)) => {
                let (Some(size), Some(dst_rm)) = (size_of(dst), rm(dst)) else {
                    return bad();
                };
                let Some(value) = immediate(*value, size) else {
                    return Err(format!("immediate {value} is out of range"));
                };
                let accumulator = matches!(dst, Reg(r) if r.num == 0 && r.kind == Kind::General);
                if size == 1 {
                    if accumulator {
                        Inst::new(&[base + 4]).imm(value, 1).finish()
                    } else {
                        Inst::new(&[0x80]).ext(ext, dst_rm).imm(value, 1).finish()
                    }
                } else if fits_i8(value) {
                    Inst::new(&[0x83])
                        .size(size)
                        .ext(ext, dst_rm)
                        .imm(value, 1)
                        .finish()
                } else if accumulator {
                    Inst::new(&[base + 5])
                        .size(size)
                        .imm(value, imm_len(size))
                        .finish()
                } else {
                    Inst::new(&[0x81])
                        .size(size)
                        .ext(ext, dst_rm)
                        .imm(value, imm_len(size))
                        .finish()
                }
            }
            _ => bad(),
        };
    }

    if let Some(&(_, byte_op, op, ext)) = UNARY.iter().find(|(name, ..)| *name == mnemonic)
        && operands.len() == 1
    {
        let (Some(size), Some(target)) = (size_of(&operands[0]), rm(&operands[0])) else {
            return bad();
        };
        let opcode = if size == 1 { byte_op } else { op };
        return Inst::new(&[opcode]).size(size).ext(ext, target).finish();
    }

    if let Some(&(_, prefix, opcode)) = SSE.iter().find(|(name, ..)| *name == mnemonic) {
        let [Reg(dst), src] = operands else {
            return bad();
        };
        let Some(src) = xmm_rm(src).filter(|_| dst.kind == Kind::Xmm) else {
            return bad();
        };
        return Inst::new(&[0x0f, opcode])
            .prefix(prefix)
            .reg(*dst, src)
            .finish();
    }

    if let Some(cc) = mnemonic.strip_prefix('j').and_then(condition) {
        let [Label(target)] = operands else {
            return bad();
        };
        return Ok(Encoded::Branch {
            short: 0x70 + cc,
            long: vec![0x0f, 0x80 + cc],
            target: target.clone(),
        });
    }
    if let Some(cc) = mnemonic.strip_prefix("set").and_then(condition) {
        let [target] = operands else { return bad() };
        let (Some(1), Some(target)) = (size_of(target), rm(target)) else {
            return bad();
        };
        return Inst::new(&[0x0f, 0x90 + cc]).ext(0, target).finish();
    }
    if let Some(cc) = mnemonic.strip_prefix("cmov").and_then(condition) {
        let [Reg(dst), src] = operands else {
            return bad();
        };
        let Some(src) = rm(src) else { return bad() };
        return Inst::new(&[0x0f, 0x40 + cc])
            .size(dst.size)
            .reg(*dst, src)
            .finish();
    }

    match (mnemonic, operands) {
        ("mov", [dst, src]) => mov(dst, src),
        ("movabs", [Reg(dst), Imm(value)]) if dst.size == 8 => Inst::new(&[0xb8])
            .w(true)
            .plus(*dst)
            .imm(*value, 8)
            .finish(),
        ("lea", [Reg(dst), Mem(mem)]) if dst.size >= 4 => Inst::new(&[0x8d])
            .size(dst.size)
            .reg(*dst, Rm::Mem(mem))
            .finish(),
        ("movzx", [Reg(dst), src]) => {
            let (Some(from), Some(src)) = (size_of(src), rm(src)) else {
                return bad();
            };
            let opcode = match from {
                1 => 0xb6,
                2 => 0xb7,
                _ => return bad(),
            };
            Inst::new(&[0x0f, opcode])
                .size(dst.size)
                .reg(*dst, src)
                .finish()
        }
        ("movsxd", [Reg(dst), src]) if dst.size == 8 && size_of(src) == Some(4) => {
            let Some(src) = rm(src) else { return bad() };
            Inst::new(&[0x63]).w(true).reg(*dst, src).finish()
        }
        ("push", [Reg(reg)]) if reg.size == 8 => Inst::new(&[0x50]).plus(*reg).finish(),
        ("push", [Mem(mem)]) if mem.size == Some(8) => {
            Inst::new(&[0xff]).ext(6, Rm::Mem(mem)).finish()
        }
        ("push", [Imm(value)]) if fits_i8(*value) => Inst::new(&[0x6a]).imm(*value, 1).finish(),
        ("push", [Imm(value)]) if i32::try_from(*value).is_ok() => {
            Inst::new(&[0x68]).imm(*value, 4).finish()
        }
        ("pop", [Reg(reg)]) if reg.size == 8 => Inst::new(&[0x58]).plus(*reg).finish(),
        ("call", [Label(target)]) => Ok(Encoded::Fixed(
            vec![0xe8, 0, 0, 0, 0],
            vec![LocalFixup {
                offset: 1,
                kind: FixupKind::X64Rel32,
                symbol: target.clone(),
                addend: -4,
            }],
        )),
        ("jmp", [Label(target)]) => Ok(Encoded::Branch {
            short: 0xeb,
            long: vec![0xe9],
            target: target.clone(),
        }),
        ("test", [dst, Reg(src)]) if src.kind != Kind::Xmm => {
            let Some(dst_rm) = rm(dst) else { return bad() };
            if size_of(dst).is_some_and(|size| size != src.size) {
                return mismatch();
            }
            let opcode = if src.size == 1 { 0x84 } else { 0x85 };
            Inst::new(&[opcode])
                .size(src.size)
                .reg(*src, dst_rm)
                .finish()
        }
        ("test", [dst, Imm(value)]) => {
            let (Some(size), Some(dst_rm)) = (size_of(dst), rm(dst)) else {
                return bad();
            };
            let Some(value) = immediate(*value, size) else {
                return Err(format!("immediate {value} is out of range"));
            };
            let accumulator = matches!(dst, Reg(r) if r.num == 0 && r.kind == Kind::General);
            match (size, accumulator) {
                (1, true) => Inst::new(&[0xa8]).imm(value, 1).finish(),
                (1, false) => Inst::new(&[0xf6]).ext(0, dst_rm).imm(value, 1).finish(),
                (_, true) => Inst::new(&[0xa9])
                    .size(size)
                    .imm(value, imm_len(size))
                    .finish(),
                (_, false) => Inst::new(&[0xf7])
                    .size(size)
                    .ext(0, dst_rm)
                    .imm(value, imm_len(size))
                    .finish(),
            }
        }
        ("imul", [Reg(dst), src]) => {
            let Some(src) = rm(src) else { return bad() };
            Inst::new(&[0x0f, 0xaf])
                .size(dst.size)
                .reg(*dst, src)
                .finish()
        }
        ("imul", [Reg(dst), src, Imm(value)]) => {
            let Some(src) = rm(src) else { return bad() };
            let Some(value) = immediate(*value, dst.size) else {
                return Err(format!("immediate {value} is out of range"));
            };
            if fits_i8(value) {
                Inst::new(&[0x6b])
                    .size(dst.size)
                    .reg(*dst, src)
                    .imm(value, 1)
                    .finish()
            } else {
                Inst::new(&[0x69])
                    .size(dst.size)
                    .reg(*dst, src)
                    .imm(value, imm_len(dst.size))
                    .finish()
            }
        }
        ("shl" | "sal" | "shr" | "sar", [dst, count]) => {
            let ext = match mnemonic {
                "shr" => 5,
                "sar" => 7,
                _ => 4,
            };
            let (Some(size), Some(dst)) = (size_of(dst), rm(dst)) else {
                return bad();
            };
            let byte = size == 1;
            match count {
                Imm(1) => Inst::new(&[if byte { 0xd0 } else { 0xd1 }])
                    .size(size)
                    .ext(ext, dst)
                    .finish(),
                Imm(n) if (0..256).contains(n) => Inst::new(&[if byte { 0xc0 } else { 0xc1 }])
                    .size(size)
                    .ext(ext, dst)
                    .imm(*n, 1)
                    .finish(),
                Reg(count) if is_cl(*count) => Inst::new(&[if byte { 0xd2 } else { 0xd3 }])
                    .size(size)
                    .ext(ext, dst)
                    .finish(),
                _ => bad(),
            }
        }
        ("shld", [dst, Reg(src), count]) => {
            let Some(dst) = rm(dst) else { return bad() };
            match count {
                Reg(count) if is_cl(*count) => Inst::new(&[0x0f, 0xa5])
                    .size(src.size)
                    .reg(*src, dst)
                    .finish(),
                Imm(n) if (0..256).contains(n) => Inst::new(&[0x0f, 0xa4])
                    .size(src.size)
                    .reg(*src, dst)
                    .imm(*n, 1)
                    .finish(),
                _ => bad(),
            }
        }
        ("bsr" | "bsf", [Reg(dst), src]) => {
            let Some(src) = rm(src) else { return bad() };
            let opcode = if mnemonic == "bsr" { 0xbd } else { 0xbc };
            Inst::new(&[0x0f, opcode])
                .size(dst.size)
                .reg(*dst, src)
                .finish()
        }
        ("bt" | "bts" | "btr" | "btc", [dst, Imm(bit)]) if (0..256).contains(bit) => {
            let ext = match mnemonic {
                "bt" => 4,
                "bts" => 5,
                "btr" => 6,
                _ => 7,
            };
            let (Some(size), Some(dst)) = (size_of(dst), rm(dst)) else {
                return bad();
            };
            Inst::new(&[0x0f, 0xba])
                .size(size)
                .ext(ext, dst)
                .imm(*bit, 1)
                .finish()
        }
        ("movq", [Reg(dst), Reg(src)]) if dst.kind == Kind::Xmm && src.size == 8 => {
            Inst::new(&[0x0f, 0x6e])
                .prefix(0x66)
                .w(true)
                .reg(*dst, Rm::Reg(*src))
                .finish()
        }
        ("movq", [Reg(dst), Reg(src)]) if src.kind == Kind::Xmm && dst.size == 8 => {
            Inst::new(&[0x0f, 0x7e])
                .prefix(0x66)
                .w(true)
                .reg(*src, Rm::Reg(*dst))
                .finish()
        }
        ("movq", [Reg(dst), Mem(mem)]) if dst.kind == Kind::Xmm => Inst::new(&[0x0f, 0x7e])
            .prefix(0xf3)
            .reg(*dst, Rm::Mem(mem))
            .finish(),
        ("movq", [Mem(mem), Reg(src)]) if src.kind == Kind::Xmm => Inst::new(&[0x0f, 0xd6])
            .prefix(0x66)
            .reg(*src, Rm::Mem(mem))
            .finish(),
        ("cvtsi2sd", [dst, src]) => {
            let (Some(dst), Some(size), Some(src)) = (xmm(dst), size_of(src), rm(src)) else {
                return bad();
            };
            Inst::new(&[0x0f, 0x2a])
                .prefix(0xf2)
                .w(size == 8)
                .reg(dst, src)
                .finish()
        }
        ("cvttsd2si", [dst, src]) => {
            let (Some(dst), Some(src)) = (gpr(dst), xmm_rm(src)) else {
                return bad();
            };
            Inst::new(&[0x0f, 0x2c])
                .prefix(0xf2)
                .w(dst.size == 8)
                .reg(dst, src)
                .finish()
        }
        ("fld", [Mem(mem)]) => match mem.size {
            Some(8) => Inst::new(&[0xdd]).ext(0, Rm::Mem(mem)).finish(),
            Some(4) => Inst::new(&[0xd9]).ext(0, Rm::Mem(mem)).finish(),
            _ => bad(),
        },
        ("fstp", [Mem(mem)]) => match mem.size {
            Some(8) => Inst::new(&[0xdd]).ext(3, Rm::Mem(mem)).finish(),
            Some(4) => Inst::new(&[0xd9]).ext(3, Rm::Mem(mem)).finish(),
            _ => bad(),
        },
        ("fstp", [St(i)]) => fixed(&[0xdd, 0xd8 + i]),
        ("fprem", []) => fixed(&[0xd9, 0xf8]),
        ("fnstsw", [Reg(ax)]) if ax.num == 0 && ax.size == 2 => fixed(&[0xdf, 0xe0]),
        ("cqo", []) => fixed(&[0x48, 0x99]),
        ("ret", []) => fixed(&[0xc3]),
        ("leave", []) => fixed(&[0xc9]),
        ("nop", []) => fixed(&[0x90]),
        ("syscall", []) => fixed(&[0x0f, 0x05]),
        ("movsb", []) => fixed(&[0xa4]),
        ("movsq", []) => fixed(&[0x48, 0xa5]),
        ("stosb", []) => fixed(&[0xaa]),
        ("rep", [Label(string)]) => match string.as_str() {
            "movsb" => fixed(&[0xf3, 0xa4]),
            "movsq" => fixed(&[0xf3, 0x48, 0xa5]),
            "stosb" => fixed(&[0xf3, 0xaa]),
            _ => bad(),
        },
        _ if is_known(mnemonic) => bad(),
        _ => unknown(),
    }
}

/// Returns whether `reg` is `cl`, the only register a shift count can be in.
fn is_cl(reg: Reg) -> bool {
    reg.num == 1 && reg.size == 1 && reg.kind == Kind::General
}

fn fixed(bytes: &[u8]) -> Result<Encoded, String> {
    Ok(Encoded::Fixed(bytes.to_vec(), vec![]))
}

/// Returns whether `mnemonic` is one [`encode`] knows, to tell a misused instruction from an
/// unknown one.
fn is_known(mnemonic: &str) -> bool {
    [
        "mov",
        "movabs",
        "lea",
        "movzx",
        "movsxd",
        "push",
        "pop",
        "call",
        "jmp",
        "test",
        "imul",
        "shl",
        "sal",
        "shr",
        "sar",
        "shld",
        "bsr",
        "bsf",
        "bt",
        "bts",
        "btr",
        "btc",
        "movq",
        "cvtsi2sd",
        "cvttsd2si",
        "fld",
        "fstp",
        "fprem",
        "fnstsw",
        "cqo",
        "ret",
        "leave",
        "nop",
        "syscall",
        "movsb",
        "movsq",
        "stosb",
        "rep",
    ]
    .contains(&mnemonic)
        || UNARY.iter().any(|(name, ..)| *name == mnemonic)
}

fn mov(dst: &Operand, src: &Operand) -> Result<Encoded, String> {
    use Operand::*;
    let bad = || Err("'mov' cannot take these operands".to_string());
    match (dst, src) {
        (Reg(_) | Mem(_), Reg(src)) if src.kind != Kind::Xmm => {
            let Some(dst_rm) = rm(dst) else { return bad() };
            if size_of(dst).is_some_and(|size| size != src.size) {
                return Err("operand sizes of 'mov' do not match".to_string());
            }
            let opcode = if src.size == 1 { 0x88 } else { 0x89 };
            Inst::new(&[opcode])
                .size(src.size)
                .reg(*src, dst_rm)
                .finish()
        }
        (Reg(dst), Mem(mem)) if dst.kind != Kind::Xmm => {
            if mem.size.is_some_and(|size| size != dst.size) {
                return Err("operand sizes of 'mov' do not match".to_string());
            }
            let opcode = if dst.size == 1 { 0x8a } else { 0x8b };
            Inst::new(&[opcode])
                .size(dst.size)
                .reg(*dst, Rm::Mem(mem))
                .finish()
        }
        (Reg(dst), Imm(value)) if dst.kind != Kind::Xmm => match dst.size {
            8 if i32::try_from(*value).is_ok() => Inst::new(&[0xc7])
                .w(true)
                .ext(0, Rm::Reg(*dst))
                .imm(*value, 4)
                .finish(),
            8 => Inst::new(&[0xb8])
                .w(true)
                .plus(*dst)
                .imm(*value, 8)
                .finish(),
            size => {
                let Some(value) = immediate(*value, size) else {
                    return Err(format!("immediate {value} is out of range"));
                };
                let opcode = if size == 1 { 0xb0 } else { 0xb8 };
                Inst::new(&[opcode])
                    .size(size)
                    .plus(*dst)
                    .imm(value, imm_len(size))
                    .finish()
            }
        },
        (Mem(mem), Imm(value)) => {
            let Some(size) = mem.size else { return bad() };
            let Some(value) = immediate(*value, size) else {
                return Err(format!("immediate {value} is out of range"));
            };
            let opcode = if size == 1 { 0xc6 } else { 0xc7 };
            Inst::new(&[opcode])
                .size(size)
                .ext(0, Rm::Mem(mem))
                .imm(value, imm_len(size))
                .finish()
        }
        _ => bad(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Encodes one line, returning its bytes, which must need no fixup.
    fn bytes(line: &str) -> Vec<u8> {
        let line = line.trim();
        let (mnemonic, rest) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        let operands = crate::codegen::asm::split_operands(rest.trim());
        match X64Encoder.instruction(mnemonic, &operands) {
            Ok(Encoded::Fixed(bytes, fixups)) if fixups.is_empty() => bytes,
            other => panic!("{line}: {other:?}"),
        }
    }

    fn hex(bytes: &[u8]) -> String {
        bytes
            .iter()
            .map(|b| format!("{b:02x}"))
            .collect::<Vec<_>>()
            .join(" ")
    }

    /// Checks lines against the bytes GNU as 2.42 writes for them.
    fn check(cases: &[(&str, &str)]) {
        let mut wrong = Vec::new();
        for (line, expected) in cases {
            let got = hex(&bytes(line));
            if got != *expected {
                wrong.push(format!("{line}: got {got}, expected {expected}"));
            }
        }
        assert!(wrong.is_empty(), "{}", wrong.join("\n"));
    }

    #[test]
    fn moves_match_gnu_as() {
        check(&[
            ("mov rax, rbx", "48 89 d8"),
            ("mov r12, rdi", "49 89 fc"),
            ("mov eax, ecx", "89 c8"),
            ("mov rax, 1", "48 c7 c0 01 00 00 00"),
            ("mov r8, -1", "49 c7 c0 ff ff ff ff"),
            ("mov eax, 60", "b8 3c 00 00 00"),
            ("mov r10d, 0x302e", "41 ba 2e 30 00 00"),
            ("mov dl, 45", "b2 2d"),
            ("mov rax, 0x100000000", "48 b8 00 00 00 00 01 00 00 00"),
            (
                "movabs rax, 0x7ff0000000000000",
                "48 b8 00 00 00 00 00 00 f0 7f",
            ),
            ("mov rax, QWORD PTR [rbp - 8]", "48 8b 45 f8"),
            ("mov QWORD PTR [rsp], rax", "48 89 04 24"),
            ("mov QWORD PTR [r12 + 16], 0", "49 c7 44 24 10 00 00 00 00"),
            ("mov rcx, QWORD PTR [rdi + 32 + r8 * 8]", "4a 8b 4c c7 20"),
            ("mov rax, QWORD PTR [rbp - 24 + rcx]", "48 8b 44 0d e8"),
            ("mov rax, QWORD PTR [r13]", "49 8b 45 00"),
            ("mov rax, QWORD PTR [rbx + 4096]", "48 8b 83 00 10 00 00"),
            ("mov byte ptr [rsi], dl", "88 16"),
            ("mov byte ptr [rdi], 48", "c6 07 30"),
            ("mov BYTE PTR [rdi + rcx], sil", "40 88 34 0f"),
            ("mov al, BYTE PTR [rdi]", "8a 07"),
            ("mov DWORD PTR [r12], 0x666e69", "41 c7 04 24 69 6e 66 00"),
            ("mov WORD PTR [rax], 10", "66 c7 00 0a 00"),
            ("lea rsi, [rbp - 32]", "48 8d 75 e0"),
            ("lea rdi, [rax * 8]", "48 8d 3c c5 00 00 00 00"),
            ("lea ebx, [rbx + rax * 2]", "8d 1c 43"),
            ("movzx eax, byte ptr [rsi]", "0f b6 06"),
            ("movzx ecx, al", "0f b6 c8"),
            ("movzx rax, dil", "48 0f b6 c7"),
            ("push rbp", "55"),
            ("push r12", "41 54"),
            ("push 1", "6a 01"),
            ("push QWORD PTR [rbp - 120]", "ff 75 88"),
            ("push QWORD PTR [r12]", "41 ff 34 24"),
            ("push 1000", "68 e8 03 00 00"),
            ("pop r15", "41 5f"),
        ]);
    }

    #[test]
    fn arithmetic_matches_gnu_as() {
        check(&[
            ("add rax, rbx", "48 01 d8"),
            ("add rax, 1", "48 83 c0 01"),
            ("add rax, 1000", "48 05 e8 03 00 00"),
            ("add rcx, 1000", "48 81 c1 e8 03 00 00"),
            ("add rdi, 8 + 4095", "48 81 c7 07 10 00 00"),
            ("add dl, 48", "80 c2 30"),
            ("add al, 48", "04 30"),
            ("add QWORD PTR [rax - 8], 1", "48 83 40 f8 01"),
            ("sub rsp, 32", "48 83 ec 20"),
            ("sub r8, QWORD PTR [rdi]", "4c 2b 07"),
            ("and ecx, 255", "81 e1 ff 00 00 00"),
            ("and eax, 0xffffffff", "83 e0 ff"),
            ("xor eax, eax", "31 c0"),
            ("cmp rax, rcx", "48 39 c8"),
            ("cmp byte ptr [rsi + rdx], 0", "80 3c 16 00"),
            ("cmp cl, BYTE PTR [rdi]", "3a 0f"),
            ("cmp rax, 128", "48 3d 80 00 00 00"),
            ("adc rdx, 0", "48 83 d2 00"),
            ("sbb QWORD PTR [rdi], rax", "48 19 07"),
            ("test rax, rax", "48 85 c0"),
            ("test al, 1", "a8 01"),
            ("test cl, 1", "f6 c1 01"),
            ("test rcx, 1", "48 f7 c1 01 00 00 00"),
            ("test byte ptr [rdi], 1", "f6 07 01"),
            ("inc rax", "48 ff c0"),
            ("dec QWORD PTR [rdi - 8]", "48 ff 4f f8"),
            ("neg rax", "48 f7 d8"),
            ("not ecx", "f7 d1"),
            ("div rcx", "48 f7 f1"),
            ("idiv r8", "49 f7 f8"),
            ("mul rdx", "48 f7 e2"),
            ("imul rax, rcx", "48 0f af c1"),
            ("imul rax, rax, 10", "48 6b c0 0a"),
            ("imul rcx, rdx, 1000", "48 69 ca e8 03 00 00"),
            ("shl rax, 1", "48 d1 e0"),
            ("shl rax, 3", "48 c1 e0 03"),
            ("sar rdx, 63", "48 c1 fa 3f"),
            ("shr rax, cl", "48 d3 e8"),
            ("shl QWORD PTR [rdi], cl", "48 d3 27"),
            ("shld rdx, rax, cl", "48 0f a5 c2"),
            ("bsr rax, rcx", "48 0f bd c1"),
            ("bts rax, 63", "48 0f ba e8 3f"),
            ("btc rax, 63", "48 0f ba f8 3f"),
            ("cmovl rax, rcx", "48 0f 4c c1"),
            ("sete al", "0f 94 c0"),
            ("setnp dl", "0f 9b c2"),
            ("cqo", "48 99"),
        ]);
    }

    #[test]
    fn floats_and_strings_match_gnu_as() {
        check(&[
            ("movq xmm0, rax", "66 48 0f 6e c0"),
            ("movq rax, xmm1", "66 48 0f 7e c8"),
            ("movq xmm0, QWORD PTR [rsp]", "f3 0f 7e 04 24"),
            ("movq QWORD PTR [rsp], xmm0", "66 0f d6 04 24"),
            ("addsd xmm0, xmm1", "f2 0f 58 c1"),
            ("divsd xmm0, QWORD PTR [rsp + 8]", "f2 0f 5e 44 24 08"),
            ("ucomisd xmm0, xmm1", "66 0f 2e c1"),
            ("xorpd xmm2, xmm2", "66 0f 57 d2"),
            ("movapd xmm1, xmm0", "66 0f 28 c8"),
            ("cvtsi2sd xmm0, rax", "f2 48 0f 2a c0"),
            ("cvtsi2sd xmm1, QWORD PTR [rsp]", "f2 48 0f 2a 0c 24"),
            ("cvttsd2si rax, xmm0", "f2 48 0f 2c c0"),
            ("fld QWORD PTR [rsp + 8]", "dd 44 24 08"),
            ("fstp QWORD PTR [rsp]", "dd 1c 24"),
            ("fstp st(0)", "dd d8"),
            ("fprem", "d9 f8"),
            ("fnstsw ax", "df e0"),
            ("rep movsb", "f3 a4"),
            ("rep movsq", "f3 48 a5"),
            ("rep stosb", "f3 aa"),
            ("movsb", "a4"),
            ("syscall", "0f 05"),
            ("leave", "c9"),
        ]);
    }

    #[test]
    fn labels_become_fixups_after_the_displacement() {
        let operands = ["QWORD PTR [rip + stone.live]", "12"];
        let Ok(Encoded::Fixed(bytes, fixups)) = X64Encoder.instruction("add", &operands) else {
            panic!()
        };
        assert_eq!(hex(&bytes), "48 83 05 00 00 00 00 0c");
        assert_eq!(
            fixups,
            [LocalFixup {
                offset: 3,
                kind: FixupKind::X64Rel32,
                symbol: "stone.live".to_string(),
                addend: -5,
            }]
        );
        let Ok(Encoded::Fixed(_, fixups)) =
            X64Encoder.instruction("lea", &["rax", "[rip + x + 8]"])
        else {
            panic!()
        };
        assert_eq!(fixups[0].addend, 4);
    }

    #[test]
    fn bad_operands_are_errors() {
        for (mnemonic, operands, error) in [
            ("frob", &["rax"][..], "unknown instruction 'frob'"),
            (
                "mov",
                &["rax", "ecx"],
                "operand sizes of 'mov' do not match",
            ),
            ("mov", &["[rax]", "1"], "'mov' cannot take these operands"),
            (
                "add",
                &["eax", "0x100000000"],
                "immediate 4294967296 is out of range",
            ),
            (
                "mov",
                &["ah", "sil"],
                "ah, bh, ch, and dh cannot be used here",
            ),
            (
                "lea",
                &["rax", "[rsp * 2]"],
                "rsp cannot be an index in '[rsp * 2]'",
            ),
            (
                "mov",
                &["rax", "[rax + rbx + rcx]"],
                "bad memory operand '[rax + rbx + rcx]'",
            ),
            (
                "push",
                &["eax"],
                "'push' cannot take these operands (1 given)",
            ),
        ] {
            assert_eq!(
                X64Encoder.instruction(mnemonic, operands),
                Err(error.to_string()),
                "{mnemonic} {operands:?}"
            );
        }
    }
}
