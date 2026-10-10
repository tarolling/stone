//! Encodes the AArch64 instructions the backend emits.
//!
//! Every instruction is one little-endian 32-bit word. Where GNU as accepts an alias or picks
//! among encodings, this picks what it picks: `mov x0, #-1` is `movn`, `ldr x0, [x1, #-8]` is
//! `ldur`, and `add x0, x0, #-8` is `sub`.

use super::{Encoded, Encoder, FixupKind, LocalFixup, is_symbol, parse_int};

/// The AArch64 [`Encoder`].
pub struct Arm64Encoder;

impl Encoder for Arm64Encoder {
    fn comment(&self) -> &'static str {
        "//"
    }

    fn nop(&self) -> &'static [u8] {
        &[0x1f, 0x20, 0x03, 0xd5]
    }

    fn instruction(
        &self,
        mnemonic: &str,
        operands: &[&str],
    ) -> std::result::Result<Encoded, String> {
        encode(mnemonic, operands).map(|(word, fixup)| {
            let fixups = fixup
                .map(|(kind, symbol)| LocalFixup {
                    offset: 0,
                    kind,
                    symbol,
                    addend: 0,
                })
                .into_iter()
                .collect();
            Encoded::Fixed(word.to_le_bytes().to_vec(), fixups)
        })
    }
}

/// Which register file, and which width, a register belongs to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    X,
    W,
    D,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Reg {
    num: u32,
    kind: Kind,
    /// Whether register 31 was written `sp` rather than `xzr`.
    sp: bool,
}

impl Reg {
    /// The `sf` bit: 1 for a 64-bit register.
    fn sf(self) -> u32 {
        (self.kind == Kind::X) as u32
    }
}

type Result<T> = std::result::Result<T, String>;

/// An encoded word and the fixup it needs, if any.
type Word = (u32, Option<(FixupKind, String)>);

fn reg(text: &str) -> Result<Reg> {
    let bad = || Err(format!("bad register '{text}'"));
    let (num, kind, sp) = match text {
        "sp" => (31, Kind::X, true),
        "wsp" => (31, Kind::W, true),
        "xzr" => (31, Kind::X, false),
        "wzr" => (31, Kind::W, false),
        _ => {
            let kind = match text.chars().next() {
                Some('x') => Kind::X,
                Some('w') => Kind::W,
                Some('d') => Kind::D,
                _ => return bad(),
            };
            let Ok(num) = text[1..].parse::<u32>() else {
                return bad();
            };
            if num > 30 + (kind == Kind::D) as u32 || text[1..].starts_with('+') {
                return bad();
            }
            (num, kind, false)
        }
    };
    Ok(Reg { num, kind, sp })
}

/// Parses a general register of either width that is not `sp`.
fn gpr(text: &str) -> Result<Reg> {
    let reg = reg(text)?;
    if reg.kind == Kind::D || reg.sp {
        return Err(format!("'{text}' cannot be used here"));
    }
    Ok(reg)
}

/// Parses a general register of either width, allowing `sp`.
fn gpr_or_sp(text: &str) -> Result<Reg> {
    let reg = reg(text)?;
    if reg.kind == Kind::D || (reg.num == 31 && !reg.sp) {
        return Err(format!("'{text}' cannot be used here"));
    }
    Ok(reg)
}

fn xreg(text: &str) -> Result<Reg> {
    gpr(text).and_then(|reg| match reg.kind {
        Kind::X => Ok(reg),
        _ => Err(format!("'{text}' must be a 64-bit register")),
    })
}

fn dreg(text: &str) -> Result<Reg> {
    reg(text).and_then(|reg| match reg.kind {
        Kind::D => Ok(reg),
        _ => Err(format!("'{text}' must be a d register")),
    })
}

/// Parses an immediate such as `#-8` or `#0x2b65`.
fn imm(text: &str) -> Result<i64> {
    text.strip_prefix('#')
        .and_then(parse_int)
        .ok_or_else(|| format!("bad immediate '{text}'"))
}

fn label(text: &str) -> Result<String> {
    if is_symbol(text) {
        Ok(text.to_string())
    } else {
        Err(format!("bad label '{text}'"))
    }
}

/// Returns the condition code `name` stands for, such as 1 for `ne`.
fn condition(name: &str) -> Result<u32> {
    const NAMES: [&[&str]; 15] = [
        &["eq"],
        &["ne"],
        &["cs", "hs"],
        &["cc", "lo"],
        &["mi"],
        &["pl"],
        &["vs"],
        &["vc"],
        &["hi"],
        &["ls"],
        &["ge"],
        &["lt"],
        &["gt"],
        &["le"],
        &["al"],
    ];
    NAMES
        .iter()
        .position(|names| names.contains(&name))
        .map(|code| code as u32)
        .ok_or_else(|| format!("bad condition '{name}'"))
}

/// Parses a shift such as `lsl #3`, returning its type (0 for `lsl`, 1 for `lsr`, 2 for `asr`)
/// and amount.
fn shift(text: &str) -> Result<(u32, u32)> {
    let bad = || format!("bad shift '{text}'");
    let (kind, amount) = text.split_once(' ').ok_or_else(bad)?;
    let kind = match kind {
        "lsl" => 0,
        "lsr" => 1,
        "asr" => 2,
        _ => return Err(bad()),
    };
    let amount = imm(amount.trim()).map_err(|_| bad())?;
    let amount = u32::try_from(amount)
        .ok()
        .filter(|n| *n < 64)
        .ok_or_else(bad)?;
    Ok((kind, amount))
}

/// Checks that registers that must share a width do.
fn same_width(regs: &[Reg]) -> Result<()> {
    if regs.windows(2).any(|pair| pair[0].kind != pair[1].kind) {
        return Err("operands must all be 64-bit or all 32-bit".to_string());
    }
    Ok(())
}

/// Returns the `N:immr:imms` fields that encode `value` as a logical immediate of `width`
/// bits, if it is one: a repeating element of a rotated run of ones.
///
/// For example, `0xff` in 64 bits is `N=1, immr=0, imms=7`.
fn bitmask(value: u64, width: u32) -> Option<u32> {
    let value = if width == 32 {
        if value >> 32 != 0 {
            return None;
        }
        value | value << 32
    } else {
        value
    };
    if value == 0 || value == u64::MAX {
        return None;
    }
    // the smallest element size the value repeats with
    let mut size = 64;
    while size > 2 {
        let half = size / 2;
        let mask = (1u64 << half) - 1;
        if value & mask != (value >> half) & mask {
            break;
        }
        size = half;
    }
    let mask = if size == 64 {
        u64::MAX
    } else {
        (1u64 << size) - 1
    };
    let element = value & mask;
    let ones = element.count_ones();
    let run = (1u64 << ones) - 1;
    let rotate = |x: u64, r: u32| {
        if r == 0 {
            x
        } else {
            ((x >> r) | (x << (size - r))) & mask
        }
    };
    let r = (0..size).find(|&r| rotate(element, r) == run)?;
    let immr = (size - r) % size;
    let imms = ((!(size - 1) << 1) | (ones - 1)) & 0x3f;
    let n = (size == 64) as u32;
    Some(n << 22 | immr << 16 | imms << 10)
}

/// Encodes an `fmov` immediate, which holds a sign, a 3-bit exponent, and a 4-bit fraction.
///
/// For example, `1.0` is `0x70` and `-2.0` is `0x80`.
fn float_imm(value: f64) -> Option<u32> {
    let bits = value.to_bits();
    let sign = (bits >> 63) as u32;
    let exponent = (bits >> 52 & 0x7ff) as u32;
    let fraction = bits & ((1 << 52) - 1);
    if fraction & ((1 << 48) - 1) != 0 {
        return None;
    }
    let b = (exponent >> 10 == 0) as u32;
    // the exponent is NOT(b), then b eight times, then two free bits
    let middle = exponent >> 2 & 0xff;
    if (b == 1 && middle != 0xff) || (b == 0 && middle != 0) {
        return None;
    }
    Some(sign << 7 | b << 6 | (exponent & 3) << 4 | (fraction >> 48) as u32)
}

/// Data-processing instructions on two registers and an immediate or shifted register.
enum AddSub {
    Add,
    Sub,
}

/// Encodes `add`, `adds`, `sub`, or `subs` of a register and `operand`, which is an immediate,
/// `:lo12:label`, or a register with an optional shift.
fn add_sub(op: AddSub, flags: bool, rd: Reg, rn: Reg, rest: &[&str]) -> Result<Word> {
    let s = flags as u32;
    let Some(&operand) = rest.first() else {
        return Err("missing operand".to_string());
    };
    if let Some(symbol) = operand.strip_prefix(":lo12:") {
        if !matches!(op, AddSub::Add) || flags || rest.len() > 1 {
            return Err("only add takes :lo12:".to_string());
        }
        let word = rd.sf() << 31 | 0x11000000 | rn.num << 5 | rd.num;
        return Ok((word, Some((FixupKind::Arm64AddLo12, label(symbol)?))));
    }
    if operand.starts_with('#') {
        let mut value = imm(operand)?;
        let mut op = op;
        if let Some(extra) = rest.get(1) {
            match shift(extra)? {
                (0, 12) => value <<= 12,
                (0, 0) => {}
                _ => return Err(format!("bad shift '{extra}'")),
            }
        }
        // like GNU as, a negative immediate flips add and sub
        if value < 0 {
            value = -value;
            op = match op {
                AddSub::Add => AddSub::Sub,
                AddSub::Sub => AddSub::Add,
            };
        }
        let (sh, imm12) = if value < 4096 {
            (0, value)
        } else if value & 0xfff == 0 && value >> 12 < 4096 {
            (1, value >> 12)
        } else {
            return Err(format!("immediate {value} is out of range"));
        };
        let op = matches!(op, AddSub::Sub) as u32;
        let word = rd.sf() << 31
            | op << 30
            | s << 29
            | 0x11000000
            | sh << 22
            | (imm12 as u32) << 10
            | rn.num << 5
            | rd.num;
        return Ok((word, None));
    }
    let rm = gpr(operand)?;
    same_width(&[rd, rn, rm])?;
    let (kind, amount) = match rest.get(1) {
        Some(text) => shift(text)?,
        None => (0, 0),
    };
    if rd.sp || rn.sp {
        // with sp, only the extended register form applies: `uxtx`, which `lsl` stands for
        if kind != 0 || amount > 4 || rd.kind != Kind::X {
            return Err("bad shift".to_string());
        }
        let op = matches!(op, AddSub::Sub) as u32;
        let word = 1 << 31
            | op << 30
            | s << 29
            | 0x0b206000
            | rm.num << 16
            | amount << 10
            | rn.num << 5
            | rd.num;
        return Ok((word, None));
    }
    if kind == 3 || amount >= 32 << rd.sf() {
        return Err("bad shift".to_string());
    }
    let op = matches!(op, AddSub::Sub) as u32;
    let word = rd.sf() << 31
        | op << 30
        | s << 29
        | 0x0b000000
        | kind << 22
        | rm.num << 16
        | amount << 10
        | rn.num << 5
        | rd.num;
    Ok((word, None))
}

/// Encodes a logical instruction (`and`, `orr`, `eor`, `ands`, given as `opc` 0 to 3) of a
/// register and an immediate or shifted register. `invert` gives `orn` and the like.
fn logical(opc: u32, invert: bool, rd: Reg, rn: Reg, rest: &[&str]) -> Result<Word> {
    let Some(&operand) = rest.first() else {
        return Err("missing operand".to_string());
    };
    if operand.starts_with('#') {
        let value = imm(operand)? as u64;
        let width = 32 << rd.sf();
        let value = if width == 32 && value >> 32 == 0xffffffff {
            value & 0xffffffff
        } else {
            value
        };
        let fields =
            bitmask(value, width).ok_or_else(|| format!("{operand} is not a logical immediate"))?;
        let word = rd.sf() << 31 | opc << 29 | 0x12000000 | fields | rn.num << 5 | rd.num;
        return Ok((word, None));
    }
    let rm = gpr(operand)?;
    same_width(&[rd, rn, rm])?;
    let (kind, amount) = match rest.get(1) {
        Some(text) => shift(text)?,
        None => (0, 0),
    };
    if amount >= 32 << rd.sf() {
        return Err("bad shift".to_string());
    }
    let word = rd.sf() << 31
        | opc << 29
        | 0x0a000000
        | kind << 22
        | (invert as u32) << 21
        | rm.num << 16
        | amount << 10
        | rn.num << 5
        | rd.num;
    Ok((word, None))
}

/// Encodes `ubfm` (`signed` false) or `sbfm` with the given fields.
fn bitfield(signed: bool, rd: Reg, rn: Reg, immr: u32, imms: u32) -> Word {
    let sf = rd.sf();
    let opc = if signed { 0 } else { 2 };
    let word = sf << 31
        | opc << 29
        | 0x13000000
        | sf << 22
        | immr << 16
        | imms << 10
        | rn.num << 5
        | rd.num;
    (word, None)
}

/// Encodes a two-source data-processing instruction such as `sdiv` (`opcode` 3) or `lslv`
/// (`opcode` 8).
fn two_source(opcode: u32, rd: Reg, rn: Reg, rm: Reg) -> Result<Word> {
    same_width(&[rd, rn, rm])?;
    let word = rd.sf() << 31 | 0x1ac00000 | rm.num << 16 | opcode << 10 | rn.num << 5 | rd.num;
    Ok((word, None))
}

/// The size and kind of a load or store: the access's log2 size, whether it is to a `d`
/// register, and its `opc` field.
struct Access {
    size: u32,
    vector: bool,
    opc: u32,
}

/// Encodes a load or store of `rt` at the memory operand `mem`, followed by the post-index
/// immediate `post` if there is one. `unscaled` forces the `ldur`/`stur` form.
fn load_store(
    access: Access,
    rt: Reg,
    mem: &str,
    post: Option<&str>,
    unscaled: bool,
) -> Result<Word> {
    let bad = || format!("bad memory operand '{mem}'");
    let base = access.size << 30 | 0x38000000 | (access.vector as u32) << 26 | access.opc << 22;
    let (inner, writeback) = match mem.strip_suffix('!') {
        Some(inner) => (inner, true),
        None => (mem, false),
    };
    let inner = inner
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
        .ok_or_else(bad)?;
    let parts: Vec<&str> = inner.split(',').map(str::trim).collect();
    let rn = gpr_or_sp(parts[0]).or_else(|_| xreg(parts[0]))?;
    if rn.kind != Kind::X {
        return Err(bad());
    }
    let imm9 = |value: i64| -> Result<u32> {
        if (-256..256).contains(&value) {
            Ok((value as u32 & 0x1ff) << 12)
        } else {
            Err(format!("offset {value} is out of range"))
        }
    };
    if let Some(post) = post {
        if writeback || parts.len() != 1 || unscaled {
            return Err(bad());
        }
        let word = base | imm9(imm(post)?)? | 0b01 << 10 | rn.num << 5 | rt.num;
        return Ok((word, None));
    }
    if writeback {
        let [_, offset] = parts[..] else {
            return Err(bad());
        };
        if unscaled {
            return Err(bad());
        }
        let word = base | imm9(imm(offset)?)? | 0b11 << 10 | rn.num << 5 | rt.num;
        return Ok((word, None));
    }
    match parts[1..] {
        [] => Ok((base | 1 << 24 | rn.num << 5 | rt.num, None)),
        [offset] if offset.starts_with(":lo12:") => {
            if unscaled {
                return Err(bad());
            }
            let symbol = label(&offset[":lo12:".len()..])?;
            let word = base | 1 << 24 | rn.num << 5 | rt.num;
            Ok((
                word,
                Some((FixupKind::Arm64LdStLo12(access.size as u8), symbol)),
            ))
        }
        [offset] if offset.starts_with('#') => {
            let value = imm(offset)?;
            let scale = 1i64 << access.size;
            if !unscaled && value >= 0 && value % scale == 0 && value / scale < 4096 {
                let word = base | 1 << 24 | ((value / scale) as u32) << 10 | rn.num << 5 | rt.num;
                return Ok((word, None));
            }
            Ok((base | imm9(value)? | rn.num << 5 | rt.num, None))
        }
        [index] | [index, _] if !unscaled => {
            let rm = xreg(index)?;
            let s = match parts.get(2) {
                Some(text) => {
                    let (kind, amount) = shift(text)?;
                    if kind != 0 || (amount != 0 && amount != access.size) {
                        return Err(bad());
                    }
                    1
                }
                None => 0,
            };
            let word = base | 1 << 21 | rm.num << 16 | 0b011 << 13 | s << 12 | 0b10 << 10;
            Ok((word | rn.num << 5 | rt.num, None))
        }
        _ => Err(bad()),
    }
}

/// Encodes `ldp` or `stp` of two 64-bit registers.
fn pair(load: bool, operands: &[&str]) -> Result<Word> {
    let (rt, rt2, mem, post) = match operands {
        [rt, rt2, mem] => (rt, rt2, *mem, None),
        [rt, rt2, mem, post] => (rt, rt2, *mem, Some(*post)),
        _ => return Err("bad operands".to_string()),
    };
    let rt = reg(rt)?;
    let rt2 = reg(rt2)?;
    if rt.kind != rt2.kind || rt.kind == Kind::W || rt.sp || rt2.sp {
        return Err("ldp and stp take two x or two d registers".to_string());
    }
    let (opc, vector) = match rt.kind {
        Kind::D => (1, 1),
        _ => (2, 0),
    };
    let bad = || format!("bad memory operand '{mem}'");
    let (inner, writeback) = match mem.strip_suffix('!') {
        Some(inner) => (inner, true),
        None => (mem, false),
    };
    let inner = inner
        .strip_prefix('[')
        .and_then(|rest| rest.strip_suffix(']'))
        .ok_or_else(bad)?;
    let parts: Vec<&str> = inner.split(',').map(str::trim).collect();
    let rn = gpr_or_sp(parts[0]).or_else(|_| xreg(parts[0]))?;
    let (mode, offset) = match (post, writeback, &parts[1..]) {
        (Some(post), false, []) => (0b01, imm(post)?),
        (None, true, [offset]) => (0b11, imm(offset)?),
        (None, false, [offset]) => (0b10, imm(offset)?),
        (None, false, []) => (0b10, 0),
        _ => return Err(bad()),
    };
    if offset % 8 != 0 || !(-512..512).contains(&offset) {
        return Err(format!("offset {offset} is out of range"));
    }
    let imm7 = (offset / 8) as u32 & 0x7f;
    let word = opc << 30
        | 0x28000000
        | vector << 26
        | mode << 23
        | (load as u32) << 22
        | imm7 << 15
        | rt2.num << 10
        | rn.num << 5
        | rt.num;
    Ok((word, None))
}

/// Encodes `mov` of an immediate the way GNU as does: `movz` if it is one 16-bit piece,
/// `movn` if its complement is, and otherwise `orr` of a logical immediate.
fn mov_imm(rd: Reg, value: i64) -> Result<Word> {
    let sf = rd.sf();
    let width = 32 << sf;
    let value = if width == 32 {
        if !(i32::MIN as i64..=u32::MAX as i64).contains(&value) {
            return Err(format!("immediate {value} is out of range"));
        }
        value as u64 & 0xffffffff
    } else {
        value as u64
    };
    let mask = if width == 32 { 0xffffffff } else { u64::MAX };
    let piece = |value: u64| (0..width / 16).find(|hw| value & !(0xffff << (16 * hw)) == 0);
    if let Some(hw) = piece(value) {
        let imm16 = (value >> (16 * hw)) as u32 & 0xffff;
        return Ok((sf << 31 | 0x52800000 | hw << 21 | imm16 << 5 | rd.num, None));
    }
    let inverted = !value & mask;
    if let Some(hw) = piece(inverted) {
        let imm16 = (inverted >> (16 * hw)) as u32 & 0xffff;
        return Ok((sf << 31 | 0x12800000 | hw << 21 | imm16 << 5 | rd.num, None));
    }
    if let Some(fields) = bitmask(value, width) {
        return Ok((sf << 31 | 0x32000000 | fields | 31 << 5 | rd.num, None));
    }
    Err(format!("{value:#x} cannot be moved in one instruction"))
}

/// Encodes `movz`, `movn`, or `movk` (`opc` 0, 2, or 3) with an optional `lsl`.
fn move_wide(opc: u32, operands: &[&str]) -> Result<Word> {
    let (rd, value, shift_text) = match operands {
        [rd, value] => (gpr(rd)?, imm(value)?, None),
        [rd, value, shift_text] => (gpr(rd)?, imm(value)?, Some(*shift_text)),
        _ => return Err("bad operands".to_string()),
    };
    let hw = match shift_text {
        Some(text) => match shift(text)? {
            (0, amount) if amount % 16 == 0 && amount < 32 << rd.sf() => amount / 16,
            _ => return Err(format!("bad shift '{text}'")),
        },
        None => 0,
    };
    if !(0..=0xffff).contains(&value) {
        return Err(format!("immediate {value} is out of range"));
    }
    let word = rd.sf() << 31 | opc << 29 | 0x12800000 | hw << 21 | (value as u32) << 5 | rd.num;
    Ok((word, None))
}

fn branch(word: u32, kind: FixupKind, target: &str) -> Result<Word> {
    Ok((word, Some((kind, label(target)?))))
}

fn encode(mnemonic: &str, operands: &[&str]) -> Result<Word> {
    let count = |n: usize| -> Result<()> {
        if operands.len() == n {
            Ok(())
        } else {
            Err(format!(
                "'{mnemonic}' takes {n} operands, but {} were given",
                operands.len()
            ))
        }
    };
    if let Some(cond) = mnemonic.strip_prefix("b.") {
        count(1)?;
        return branch(
            0x54000000 | condition(cond)?,
            FixupKind::Arm64Branch19,
            operands[0],
        );
    }
    match mnemonic {
        "mov" => {
            count(2)?;
            if operands[1].starts_with('#') {
                return mov_imm(gpr(operands[0])?, imm(operands[1])?);
            }
            let rd = reg(operands[0])?;
            let rm = reg(operands[1])?;
            same_width(&[rd, rm])?;
            if rd.kind == Kind::D {
                return Ok((0x1e604000 | rm.num << 5 | rd.num, None));
            }
            if rd.sp || rm.sp {
                // moves to and from sp are `add rd, rn, #0`
                return Ok((rd.sf() << 31 | 0x11000000 | rm.num << 5 | rd.num, None));
            }
            Ok((rd.sf() << 31 | 0x2a0003e0 | rm.num << 16 | rd.num, None))
        }
        "movz" => move_wide(2, operands),
        "movn" => move_wide(0, operands),
        "movk" => move_wide(3, operands),
        "add" | "adds" | "sub" | "subs" => {
            if operands.len() < 3 {
                return count(3).map(|_| (0, None));
            }
            let flags = mnemonic.ends_with('s');
            let op = if mnemonic.starts_with("add") {
                AddSub::Add
            } else {
                AddSub::Sub
            };
            let rd = if flags {
                gpr(operands[0])?
            } else {
                reg(operands[0])?
            };
            let rn = reg(operands[1])?;
            if rd.kind == Kind::D || rn.kind == Kind::D {
                return Err("add and sub take general registers".to_string());
            }
            add_sub(op, flags, rd, rn, &operands[2..])
        }
        "cmp" | "cmn" => {
            if operands.len() < 2 {
                return count(2).map(|_| (0, None));
            }
            let rn = reg(operands[0])?;
            let zr = Reg {
                num: 31,
                kind: rn.kind,
                sp: false,
            };
            let op = if mnemonic == "cmp" {
                AddSub::Sub
            } else {
                AddSub::Add
            };
            add_sub(op, true, zr, rn, &operands[1..])
        }
        "neg" | "negs" => {
            if operands.len() < 2 {
                return count(2).map(|_| (0, None));
            }
            let rd = gpr(operands[0])?;
            let zr = Reg {
                num: 31,
                kind: rd.kind,
                sp: false,
            };
            add_sub(AddSub::Sub, mnemonic == "negs", rd, zr, &operands[1..])
        }
        "adc" | "adcs" | "sbc" | "sbcs" => {
            count(3)?;
            let (rd, rn, rm) = (gpr(operands[0])?, gpr(operands[1])?, gpr(operands[2])?);
            same_width(&[rd, rn, rm])?;
            let op = mnemonic.starts_with("sbc") as u32;
            let s = mnemonic.ends_with('s') as u32;
            let word = rd.sf() << 31 | op << 30 | s << 29 | 0x1a000000 | rm.num << 16;
            Ok((word | rn.num << 5 | rd.num, None))
        }
        "and" | "orr" | "eor" | "ands" | "orn" | "bic" => {
            if operands.len() < 3 {
                return count(3).map(|_| (0, None));
            }
            let (opc, invert) = match mnemonic {
                "and" => (0, false),
                "orr" => (1, false),
                "eor" => (2, false),
                "ands" => (3, false),
                "orn" => (1, true),
                _ => (0, true),
            };
            let rd = gpr(operands[0])?;
            let rn = gpr(operands[1])?;
            if invert && operands[2].starts_with('#') {
                return Err(format!("'{mnemonic}' takes no immediate"));
            }
            logical(opc, invert, rd, rn, &operands[2..])
        }
        "tst" => {
            if operands.len() < 2 {
                return count(2).map(|_| (0, None));
            }
            let rn = gpr(operands[0])?;
            let zr = Reg {
                num: 31,
                kind: rn.kind,
                sp: false,
            };
            logical(3, false, zr, rn, &operands[1..])
        }
        "mvn" => {
            if operands.len() < 2 {
                return count(2).map(|_| (0, None));
            }
            let rd = gpr(operands[0])?;
            let zr = Reg {
                num: 31,
                kind: rd.kind,
                sp: false,
            };
            logical(1, true, rd, zr, &operands[1..])
        }
        "lsl" | "lsr" | "asr" => {
            count(3)?;
            let (rd, rn) = (gpr(operands[0])?, gpr(operands[1])?);
            same_width(&[rd, rn])?;
            if !operands[2].starts_with('#') {
                let opcode = match mnemonic {
                    "lsl" => 8,
                    "lsr" => 9,
                    _ => 10,
                };
                return two_source(opcode, rd, rn, gpr(operands[2])?);
            }
            let width = 32 << rd.sf();
            let amount = u32::try_from(imm(operands[2])?)
                .ok()
                .filter(|n| *n < width)
                .ok_or_else(|| format!("bad shift amount '{}'", operands[2]))?;
            Ok(match mnemonic {
                "lsl" => bitfield(false, rd, rn, (width - amount) % width, width - 1 - amount),
                "lsr" => bitfield(false, rd, rn, amount, width - 1),
                _ => bitfield(true, rd, rn, amount, width - 1),
            })
        }
        "ubfx" | "sbfx" => {
            count(4)?;
            let (rd, rn) = (gpr(operands[0])?, gpr(operands[1])?);
            same_width(&[rd, rn])?;
            let width = 32 << rd.sf();
            let lsb = imm(operands[2])?;
            let bits = imm(operands[3])?;
            if lsb < 0 || bits < 1 || lsb + bits > width as i64 {
                return Err("bad bitfield".to_string());
            }
            Ok(bitfield(
                mnemonic == "sbfx",
                rd,
                rn,
                lsb as u32,
                (lsb + bits - 1) as u32,
            ))
        }
        "mul" | "madd" | "msub" => {
            let (rd, rn, rm, ra) = match operands {
                [rd, rn, rm] if mnemonic == "mul" => (gpr(rd)?, gpr(rn)?, gpr(rm)?, 31),
                [rd, rn, rm, ra] if mnemonic != "mul" => {
                    (gpr(rd)?, gpr(rn)?, gpr(rm)?, gpr(ra)?.num)
                }
                _ => return Err(format!("bad operands for '{mnemonic}'")),
            };
            same_width(&[rd, rn, rm])?;
            let o0 = (mnemonic == "msub") as u32;
            let word = rd.sf() << 31 | 0x1b000000 | rm.num << 16 | o0 << 15 | ra << 10;
            Ok((word | rn.num << 5 | rd.num, None))
        }
        "smulh" | "umulh" => {
            count(3)?;
            let (rd, rn, rm) = (xreg(operands[0])?, xreg(operands[1])?, xreg(operands[2])?);
            let u = (mnemonic == "umulh") as u32;
            let word = 0x9b407c00 | u << 23 | rm.num << 16 | rn.num << 5 | rd.num;
            Ok((word, None))
        }
        "sdiv" | "udiv" => {
            count(3)?;
            let opcode = if mnemonic == "sdiv" { 3 } else { 2 };
            two_source(
                opcode,
                gpr(operands[0])?,
                gpr(operands[1])?,
                gpr(operands[2])?,
            )
        }
        "csel" | "csinc" | "csneg" => {
            count(4)?;
            let (rd, rn, rm) = (gpr(operands[0])?, gpr(operands[1])?, gpr(operands[2])?);
            same_width(&[rd, rn, rm])?;
            let cond = condition(operands[3])?;
            let (op, o2) = match mnemonic {
                "csel" => (0, 0),
                "csinc" => (0, 1),
                _ => (1, 1),
            };
            let word = rd.sf() << 31 | op << 30 | 0x1a800000 | rm.num << 16 | cond << 12;
            Ok((word | o2 << 10 | rn.num << 5 | rd.num, None))
        }
        "cset" => {
            count(2)?;
            let rd = gpr(operands[0])?;
            let cond = condition(operands[1])? ^ 1;
            Ok((rd.sf() << 31 | 0x1a9f07e0 | cond << 12 | rd.num, None))
        }
        "cneg" => {
            count(3)?;
            let (rd, rn) = (gpr(operands[0])?, gpr(operands[1])?);
            same_width(&[rd, rn])?;
            let cond = condition(operands[2])? ^ 1;
            let word = rd.sf() << 31 | 0x5a800400 | rn.num << 16 | cond << 12;
            Ok((word | rn.num << 5 | rd.num, None))
        }
        "clz" => {
            count(2)?;
            let (rd, rn) = (gpr(operands[0])?, gpr(operands[1])?);
            same_width(&[rd, rn])?;
            Ok((rd.sf() << 31 | 0x5ac01000 | rn.num << 5 | rd.num, None))
        }
        "ldr" | "str" | "ldur" | "stur" | "ldrb" | "strb" | "ldrh" | "strh" | "ldrsw" => {
            let (rt, mem, post) = match operands {
                [rt, mem] => (*rt, *mem, None),
                [rt, mem, post] => (*rt, *mem, Some(*post)),
                _ => return count(2).map(|_| (0, None)),
            };
            let rt = reg(rt)?;
            if rt.sp {
                return Err("sp cannot be loaded or stored".to_string());
            }
            let load = mnemonic.starts_with("ld");
            let access = match (mnemonic, rt.kind) {
                ("ldrb" | "strb", Kind::W) => Access {
                    size: 0,
                    vector: false,
                    opc: load as u32,
                },
                ("ldrh" | "strh", Kind::W) => Access {
                    size: 1,
                    vector: false,
                    opc: load as u32,
                },
                ("ldrsw", Kind::X) => Access {
                    size: 2,
                    vector: false,
                    opc: 2,
                },
                ("ldr" | "str" | "ldur" | "stur", kind) => Access {
                    size: if kind == Kind::W { 2 } else { 3 },
                    vector: kind == Kind::D,
                    opc: load as u32,
                },
                _ => return Err(format!("'{mnemonic}' cannot take '{}'", operands[0])),
            };
            load_store(access, rt, mem, post, mnemonic.ends_with("ur"))
        }
        "ldp" | "stp" => pair(mnemonic == "ldp", operands),
        "adrp" => {
            count(2)?;
            let rd = xreg(operands[0])?;
            branch(0x90000000 | rd.num, FixupKind::Arm64AdrpPage21, operands[1])
        }
        "b" | "bl" => {
            count(1)?;
            let word = if mnemonic == "b" {
                0x14000000
            } else {
                0x94000000
            };
            branch(word, FixupKind::Arm64Branch26, operands[0])
        }
        "cbz" | "cbnz" => {
            count(2)?;
            let rt = gpr(operands[0])?;
            let op = (mnemonic == "cbnz") as u32;
            branch(
                rt.sf() << 31 | 0x34000000 | op << 24 | rt.num,
                FixupKind::Arm64Branch19,
                operands[1],
            )
        }
        "tbz" | "tbnz" => {
            count(3)?;
            let rt = gpr(operands[0])?;
            let bit = imm(operands[1])?;
            if !(0..32 << rt.sf()).contains(&bit) {
                return Err(format!("bad bit '{}'", operands[1]));
            }
            let bit = bit as u32;
            let op = (mnemonic == "tbnz") as u32;
            let word = (bit >> 5) << 31 | 0x36000000 | op << 24 | (bit & 31) << 19 | rt.num;
            branch(word, FixupKind::Arm64Branch14, operands[2])
        }
        "ret" => {
            count(0)?;
            Ok((0xd65f03c0, None))
        }
        "nop" => {
            count(0)?;
            Ok((0xd503201f, None))
        }
        "svc" => {
            count(1)?;
            let value = imm(operands[0])?;
            if !(0..=0xffff).contains(&value) {
                return Err(format!("immediate {value} is out of range"));
            }
            Ok((0xd4000001 | (value as u32) << 5, None))
        }
        "fmov" => {
            count(2)?;
            let rd = reg(operands[0])?;
            if let Some(text) = operands[1].strip_prefix('#') {
                let value: f64 = text
                    .parse()
                    .map_err(|_| format!("bad immediate '{}'", operands[1]))?;
                let imm8 = float_imm(value)
                    .ok_or_else(|| format!("{value} cannot be an fmov immediate"))?;
                if rd.kind != Kind::D {
                    return Err("fmov of an immediate needs a d register".to_string());
                }
                return Ok((0x1e601000 | imm8 << 13 | rd.num, None));
            }
            let rn = reg(operands[1])?;
            match (rd.kind, rn.kind) {
                (Kind::D, Kind::X) if !rn.sp => Ok((0x9e670000 | rn.num << 5 | rd.num, None)),
                (Kind::X, Kind::D) if !rd.sp => Ok((0x9e660000 | rn.num << 5 | rd.num, None)),
                (Kind::D, Kind::D) => Ok((0x1e604000 | rn.num << 5 | rd.num, None)),
                _ => Err("bad operands for 'fmov'".to_string()),
            }
        }
        "fadd" | "fsub" | "fmul" | "fdiv" => {
            count(3)?;
            let (rd, rn, rm) = (dreg(operands[0])?, dreg(operands[1])?, dreg(operands[2])?);
            let opcode = match mnemonic {
                "fmul" => 0,
                "fdiv" => 1,
                "fadd" => 2,
                _ => 3,
            };
            let word = 0x1e600800 | rm.num << 16 | opcode << 12 | rn.num << 5 | rd.num;
            Ok((word, None))
        }
        "fcmp" => {
            count(2)?;
            let rn = dreg(operands[0])?;
            if let Some(text) = operands[1].strip_prefix('#') {
                if text.parse::<f64>() != Ok(0.0) {
                    return Err("fcmp only compares with #0.0".to_string());
                }
                return Ok((0x1e602008 | rn.num << 5, None));
            }
            let rm = dreg(operands[1])?;
            Ok((0x1e602000 | rm.num << 16 | rn.num << 5, None))
        }
        "fcvtzs" => {
            count(2)?;
            let (rd, rn) = (xreg(operands[0])?, dreg(operands[1])?);
            Ok((0x9e780000 | rn.num << 5 | rd.num, None))
        }
        "scvtf" => {
            count(2)?;
            let (rd, rn) = (dreg(operands[0])?, xreg(operands[1])?);
            Ok((0x9e620000 | rn.num << 5 | rd.num, None))
        }
        _ => Err(format!("unknown instruction '{mnemonic}'")),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::asm::split_operands;

    /// Encodes one line, returning its word as hex and any fixup.
    fn word(line: &str) -> Result<(String, Option<(FixupKind, String)>)> {
        let line = line.trim();
        let (mnemonic, rest) = line.split_once(char::is_whitespace).unwrap_or((line, ""));
        encode(mnemonic, &split_operands(rest.trim()))
            .map(|(word, fixup)| (format!("{word:08x}"), fixup))
    }

    /// Checks lines against the words GNU as 2.42 writes for them.
    fn check(cases: &[(&str, &str)]) {
        let mut wrong = Vec::new();
        for (line, expected) in cases {
            match word(line) {
                Ok((got, None)) if got == *expected => {}
                other => wrong.push(format!("{line}: got {other:?}, expected {expected}")),
            }
        }
        assert!(wrong.is_empty(), "{}", wrong.join("\n"));
    }

    #[test]
    fn moves_and_arithmetic_match_gnu_as() {
        check(&[
            ("mov x0, x1", "aa0103e0"),
            ("mov w10, w11", "2a0b03ea"),
            ("mov x29, sp", "910003fd"),
            ("mov sp, x29", "910003bf"),
            ("mov x0, #2", "d2800040"),
            ("mov x8, #94", "d2800bc8"),
            ("mov w10, #0x2b65", "52856caa"),
            ("mov x0, #-4096", "9281ffe0"),
            ("mov x0, #-1", "92800000"),
            ("mov w0, #-1", "12800000"),
            ("mov x9, #0x10000000000000", "d2e00209"),
            ("mov x9, #0x8000000000000000", "d2f00009"),
            ("mov x9, #0x7fffffffffffffff", "92f00009"),
            ("mov x9, #0xfffffffffffff", "92fffe09"),
            ("mov x9, #0x10000", "d2a00029"),
            ("movz x9, #0x7ff0, lsl #48", "d2effe09"),
            ("movk x9, #0x1234, lsl #16", "f2a24689"),
            ("movz x1, #5", "d28000a1"),
            ("add x0, x1, #16", "91004020"),
            ("add x0, sp, #16", "910043e0"),
            ("add sp, sp, #4096", "914007ff"),
            ("add x0, x0, #-8", "d1002000"),
            ("sub sp, sp, #32", "d10083ff"),
            ("sub sp, x29, #48", "d100c3bf"),
            ("add w10, w10, #48", "1100c14a"),
            ("add x0, x1, x2", "8b020020"),
            ("add x8, sp, x8", "8b2863e8"),
            ("sub x8, sp, x8", "cb2863e8"),
            ("add sp, sp, x9", "8b2963ff"),
            ("add x0, sp, x1, lsl #3", "8b216fe0"),
            ("adds x0, sp, x1", "ab2163e0"),
            ("cmp sp, x1", "eb2163ff"),
            ("add x0, x1, x2, lsl #3", "8b020c20"),
            ("add x0, x1, x2, lsr #63", "8b42fc20"),
            ("sub x0, x1, x2", "cb020020"),
            ("adds x0, x1, #1", "b1000420"),
            ("adds x0, x1, x2", "ab020020"),
            ("subs x0, x1, #1", "f1000420"),
            ("subs x0, x1, x2", "eb020020"),
            ("cmp x0, #0", "f100001f"),
            ("cmp x0, #-1", "b100041f"),
            ("cmp x0, x1", "eb01001f"),
            ("cmp w10, #48", "7100c15f"),
            ("cmp w10, w11", "6b0b015f"),
            ("cmp x0, x1, asr #63", "eb81fc1f"),
            ("cmp xzr, xzr", "eb1f03ff"),
            ("cmn x0, #1", "b100041f"),
            ("cmn xzr, xzr", "ab1f03ff"),
            ("neg x0, x1", "cb0103e0"),
            ("negs x0, x1", "eb0103e0"),
            ("adc x0, x1, xzr", "9a1f0020"),
            ("adcs x0, x1, x2", "ba020020"),
            ("sbcs x0, x1, x2", "fa020020"),
            ("mul x0, x1, x2", "9b027c20"),
            ("madd x0, x1, x2, x3", "9b020c20"),
            ("msub x0, x1, x2, x3", "9b028c20"),
            ("msub x0, x1, x2, xzr", "9b02fc20"),
            ("smulh x0, x1, x2", "9b427c20"),
            ("umulh x0, x1, x2", "9bc27c20"),
            ("sdiv x0, x1, x2", "9ac20c20"),
            ("sdiv x0, xzr, x2", "9ac20fe0"),
            ("udiv x0, x1, x2", "9ac20820"),
        ]);
    }

    #[test]
    fn logic_and_shifts_match_gnu_as() {
        check(&[
            ("and x0, x1, #0xff", "92401c20"),
            ("and x0, x1, #-16", "927cec20"),
            ("and x0, x1, #1", "92400020"),
            ("orr x0, x1, #0x8000000000000000", "b2410020"),
            ("orr w10, w10, #32", "321b014a"),
            ("eor x0, x1, #1", "d2400020"),
            ("tst x0, #1", "f240001f"),
            ("orr x0, x1, x2", "aa020020"),
            ("orr x0, x1, x2, lsl #52", "aa02d020"),
            ("mvn x0, x1", "aa2103e0"),
            ("lsl x0, x1, #3", "d37df020"),
            ("lsl x0, x1, #0", "d340fc20"),
            ("lsr x0, x1, #3", "d343fc20"),
            ("asr x0, x1, #63", "937ffc20"),
            ("lsl x0, x1, x2", "9ac22020"),
            ("lsr x0, x1, x2", "9ac22420"),
            ("ubfx x0, x1, #52, #11", "d374f820"),
            ("clz x0, x1", "dac01020"),
            ("csel x0, x1, x2, ne", "9a821020"),
            ("csel x0, x1, xzr, gt", "9a9fc020"),
            ("cset x0, eq", "9a9f17e0"),
            ("cset x0, lt", "9a9fa7e0"),
            ("cneg x0, x1, lt", "da81a420"),
        ]);
    }

    #[test]
    fn loads_and_stores_match_gnu_as() {
        check(&[
            ("ldr x0, [x1]", "f9400020"),
            ("ldr x0, [x1, #8]", "f9400420"),
            ("ldr x0, [x1, #-8]", "f85f8020"),
            ("ldr x0, [x1, #4]", "f8404020"),
            ("ldr x0, [sp, #16]", "f9400be0"),
            ("ldr x0, [x1, #8]!", "f8408c20"),
            ("ldr x0, [x1], #8", "f8408420"),
            ("ldr x0, [x1, x2, lsl #3]", "f8627820"),
            ("ldr x0, [x1, x2]", "f8626820"),
            ("ldr x0, [sp, x2]", "f8626be0"),
            ("ldr w0, [x1]", "b9400020"),
            ("ldr d0, [x1, #8]", "fd400420"),
            ("ldrb w0, [x1]", "39400020"),
            ("ldrb w0, [x1, #3]", "39400c20"),
            ("ldrb w0, [x1, x2]", "38626820"),
            ("ldrb w0, [x1], #1", "38401420"),
            ("ldrsw x0, [x1]", "b9800020"),
            ("str x0, [sp, #-16]!", "f81f0fe0"),
            ("str x0, [x1, #8]", "f9000420"),
            ("str xzr, [x1]", "f900003f"),
            ("str w0, [x1]", "b9000020"),
            ("str d0, [sp]", "fd0003e0"),
            ("strb w0, [x1], #1", "38001420"),
            ("strb wzr, [x1]", "3900003f"),
            ("strb w0, [x1, #-1]!", "381ffc20"),
            ("strh w0, [x1]", "79000020"),
            ("strh w0, [x1, #2]", "79000420"),
            ("ldur x0, [x1, #-8]", "f85f8020"),
            ("ldur x0, [x1, #8]", "f8408020"),
            ("stur x0, [x29, #-8]", "f81f83a0"),
            ("ldp x29, x30, [sp], #16", "a8c17bfd"),
            ("ldp x19, x20, [sp, #16]", "a94153f3"),
            ("stp x29, x30, [sp, #-16]!", "a9bf7bfd"),
            ("stp x19, x20, [sp, #16]", "a90153f3"),
            ("stp x0, x1, [x2]", "a9000440"),
        ]);
    }

    #[test]
    fn floats_and_control_match_gnu_as() {
        check(&[
            ("fmov d0, x1", "9e670020"),
            ("fmov x0, d1", "9e660020"),
            ("fmov d0, xzr", "9e6703e0"),
            ("fmov d0, #1.0", "1e6e1000"),
            ("fadd d0, d1, d2", "1e622820"),
            ("fsub d0, d1, d2", "1e623820"),
            ("fmul d0, d1, d2", "1e620820"),
            ("fdiv d0, d1, d2", "1e621820"),
            ("fcmp d0, d1", "1e612000"),
            ("fcmp d1, #0.0", "1e602028"),
            ("fcvtzs x0, d1", "9e780020"),
            ("scvtf d0, x1", "9e620020"),
            ("scvtf d0, xzr", "9e6203e0"),
            ("ret", "d65f03c0"),
            ("svc #0", "d4000001"),
        ]);
    }

    #[test]
    fn labels_become_fixups() {
        for (line, word_hex, kind) in [
            ("b .L1", "14000000", FixupKind::Arm64Branch26),
            ("bl .L1", "94000000", FixupKind::Arm64Branch26),
            ("b.ne .L1", "54000001", FixupKind::Arm64Branch19),
            ("cbz x0, .L1", "b4000000", FixupKind::Arm64Branch19),
            ("cbnz w1, .L1", "35000001", FixupKind::Arm64Branch19),
            ("tbz x0, #63, .L1", "b6f80000", FixupKind::Arm64Branch14),
            ("tbnz w2, #3, .L1", "37180002", FixupKind::Arm64Branch14),
            ("adrp x16, .L1", "90000010", FixupKind::Arm64AdrpPage21),
            (
                "add x0, x16, :lo12:.L1",
                "91000200",
                FixupKind::Arm64AddLo12,
            ),
            (
                "ldr x0, [x16, :lo12:.L1]",
                "f9400200",
                FixupKind::Arm64LdStLo12(3),
            ),
            (
                "strb w0, [x16, :lo12:.L1]",
                "39000200",
                FixupKind::Arm64LdStLo12(0),
            ),
        ] {
            assert_eq!(
                word(line),
                Ok((word_hex.to_string(), Some((kind, ".L1".to_string())))),
                "{line}"
            );
        }
    }

    #[test]
    fn logical_immediates_round_trip() {
        // decodes `N:immr:imms` back into the value it repeats
        fn decode(fields: u32, width: u32) -> u64 {
            let n = fields >> 22 & 1;
            let immr = fields >> 16 & 0x3f;
            let imms = fields >> 10 & 0x3f;
            let len = 31 - ((n << 6) | (!imms & 0x3f)).leading_zeros();
            let size = 1u32 << len;
            let ones = (imms & (size - 1)) + 1;
            let element = if ones == 64 {
                u64::MAX
            } else {
                (1u64 << ones) - 1
            };
            let mask = if size == 64 {
                u64::MAX
            } else {
                (1u64 << size) - 1
            };
            let rotated = if immr == 0 {
                element
            } else {
                ((element >> immr) | (element << (size - immr))) & mask
            };
            let mut value = 0;
            for i in (0..width).step_by(size as usize) {
                value |= rotated << i;
            }
            value
        }
        for value in [
            1u64,
            0xff,
            0x8000000000000000,
            0x5555555555555555,
            0xfff0,
            !0xf,
        ] {
            let fields = bitmask(value, 64).unwrap();
            assert_eq!(decode(fields, 64), value, "{value:#x}");
        }
        assert_eq!(bitmask(0, 64), None);
        assert_eq!(bitmask(u64::MAX, 64), None);
        assert_eq!(bitmask(0x1234, 64), None);
        assert_eq!(decode(bitmask(0xff00ff00, 32).unwrap(), 32), 0xff00ff00);
    }

    #[test]
    fn bad_operands_are_errors() {
        for (line, error) in [
            ("frob x0", "unknown instruction 'frob'"),
            (
                "mov x0, #0x1234567",
                "0x1234567 cannot be moved in one instruction",
            ),
            (
                "add x0, w1, w2",
                "operands must all be 64-bit or all 32-bit",
            ),
            ("and x0, x1, #3", ""),
            ("and x0, x1, #5", "#5 is not a logical immediate"),
            ("ldr x0, [x1, #32768]", "offset 32768 is out of range"),
            ("ret x0", "'ret' takes 0 operands, but 1 were given"),
            ("b.xx .L1", "bad condition 'xx'"),
            ("mov x31, x0", "bad register 'x31'"),
        ] {
            match word(line) {
                Ok(_) if error.is_empty() => {}
                result => assert_eq!(result, Err(error.to_string()), "{line}"),
            }
        }
    }
}
