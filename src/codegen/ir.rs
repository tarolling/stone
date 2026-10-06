//! A small, backend-independent intermediate representation that sits between the AST and
//! assembly.
//!
//! A [`Function`] is a list of [`Block`]s of [`Inst`]s over virtual registers ([`VReg`]), each
//! ending in a [`Terminator`]. It is not SSA: every stone local is one vreg for the whole
//! function, and temporaries get fresh vregs. Blocks are stored in layout order, which is also the
//! order liveness numbers instructions in and the order a backend emits them in.
//!
//! For example, `def inc(x); ret x + 1` lowers to:
//!
//! ```text
//! fn inc(v0):
//! b0:
//!   v1 = add v0, 1
//!   ret v1
//! ```

pub mod liveness;
pub mod lower;

use crate::ast::CompOp;
use crate::checker::Type;
use std::fmt;

/// A virtual register, numbered from 0 within its function.
///
/// For example, `VReg(3)` prints as `v3`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VReg(pub u32);

/// A block's index in [`Function::blocks`], which is also its position in layout order.
///
/// For example, `BlockId(2)` prints as `b2`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub usize);

/// An instruction input: a vreg, or an immediate 64-bit value.
///
/// Floats are immediates of their bits, bools are 0 or 1, and `none` is 0, so for example the
/// literal `1.5` is `Imm(4609434218613702656)`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Operand {
    Reg(VReg),
    Imm(i64),
}

/// An arithmetic operator, for ints in [`Inst::Binary`] and floats in [`Inst::FloatBinary`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    /// The remainder of `Div`, which takes the dividend's sign.
    Rem,
    /// `lhs` raised to the int `rhs`, by squaring and multiplying.
    Pow,
}

/// A comparison, as in [`Inst::Compare`] and [`Terminator::CmpBranch`].
///
/// For example, `Cond::from(&CompOp::LessThan)` is `Cond::Lt`, and its negation is `Cond::Ge`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Cond {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

impl Cond {
    /// Returns the condition that holds exactly when `self` does not, for ints.
    ///
    /// For example, `Cond::Lt.negate()` is `Cond::Ge`. Floats need care, since every ordered
    /// comparison with nan is false, so a backend must not negate float conditions this way.
    pub fn negate(self) -> Cond {
        match self {
            Cond::Eq => Cond::Ne,
            Cond::Ne => Cond::Eq,
            Cond::Lt => Cond::Ge,
            Cond::Le => Cond::Gt,
            Cond::Gt => Cond::Le,
            Cond::Ge => Cond::Lt,
        }
    }

    /// Returns the condition that holds for `b ? a` exactly when `self` holds for `a ? b`.
    ///
    /// For example, `a < b` is `b > a`, so `Cond::Lt.swap()` is `Cond::Gt`.
    pub fn swap(self) -> Cond {
        match self {
            Cond::Eq => Cond::Eq,
            Cond::Ne => Cond::Ne,
            Cond::Lt => Cond::Gt,
            Cond::Le => Cond::Ge,
            Cond::Gt => Cond::Lt,
            Cond::Ge => Cond::Le,
        }
    }

    fn name(self) -> &'static str {
        match self {
            Cond::Eq => "eq",
            Cond::Ne => "ne",
            Cond::Lt => "lt",
            Cond::Le => "le",
            Cond::Gt => "gt",
            Cond::Ge => "ge",
        }
    }
}

impl From<&CompOp> for Cond {
    fn from(op: &CompOp) -> Cond {
        match op {
            CompOp::Equal => Cond::Eq,
            CompOp::NotEqual => Cond::Ne,
            CompOp::LessThan => Cond::Lt,
            CompOp::LessThanEqual => Cond::Le,
            CompOp::GreaterThan => Cond::Gt,
            CompOp::GreaterThanEqual => Cond::Ge,
        }
    }
}

/// What a [`Inst::Call`] calls.
#[derive(Clone, Debug, PartialEq)]
pub enum Callee {
    /// A stone function, by its stone name.
    User(String),
    /// A runtime routine by its assembly label, such as `stone.list_append`.
    Runtime(&'static str),
    /// The routine that prints one value of the type, which the backend picks and emits.
    Print(Type),
}

/// One instruction. Every instruction reads all of its inputs before writing `dst`, so `dst` may
/// be the same vreg as an input.
#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    /// `dst = src`.
    Copy { dst: VReg, src: Operand },
    /// Wrapping int arithmetic. `Div` rounds toward zero and fails on a zero divisor and on
    /// `MIN / -1`, unless `rhs` is an immediate other than 0 and -1, which needs no checks. `Rem`
    /// fails on a zero divisor too, but `x % -1` is 0. `Pow` fails on a negative exponent and
    /// wraps like `stdlib::int_pow`.
    Binary {
        op: BinOp,
        dst: VReg,
        lhs: Operand,
        rhs: Operand,
    },
    /// Float arithmetic on bits. `Div` and `Rem` fail on a zero divisor. For `Pow`, `rhs` is an
    /// int exponent rather than float bits, and the result matches `stdlib::float_pow`.
    FloatBinary {
        op: BinOp,
        dst: VReg,
        lhs: Operand,
        rhs: Operand,
    },
    /// Wrapping int negation.
    Neg { dst: VReg, src: Operand },
    /// Float negation, which flips the sign bit.
    FloatNeg { dst: VReg, src: Operand },
    /// 1 if `src` is zero, else 0.
    Not { dst: VReg, src: Operand },
    /// 1 if the comparison holds, else 0. Ints compare signed, and floats compare with every
    /// ordered comparison involving nan false.
    Compare {
        cond: Cond,
        float: bool,
        dst: VReg,
        lhs: Operand,
        rhs: Operand,
    },
    /// Reads a global. A `checked` read fails if the global was never assigned.
    LoadGlobal {
        dst: VReg,
        name: String,
        checked: bool,
    },
    /// Writes a global, also marking it assigned when `mark_set` is true.
    StoreGlobal {
        name: String,
        src: Operand,
        mark_set: bool,
    },
    /// The address of a string literal.
    StrAddr { dst: VReg, text: String },
    /// The length of a list.
    ListLen { dst: VReg, list: Operand },
    /// `dst = list[index]`, counting a negative index from the end and failing when out of range.
    ListLoad {
        dst: VReg,
        list: Operand,
        index: Operand,
    },
    /// `list[index] = value`, with the same index rules as [`Inst::ListLoad`].
    ListStore {
        list: Operand,
        index: Operand,
        value: Operand,
    },
    /// `dst = list[index]` for an index already known to be in range.
    ListGet {
        dst: VReg,
        list: Operand,
        index: Operand,
    },
    /// Fills slot `index` of a list that was just created with room for it.
    ListInit {
        list: Operand,
        index: usize,
        value: Operand,
    },
    /// Converts an int to the bits of the nearest float.
    IntToFloat { dst: VReg, src: Operand },
    /// Converts a float's bits to an int, dropping the fraction and failing on nan or overflow.
    FloatToInt { dst: VReg, src: Operand },
    /// Calls a function with `args` and stores its result in `dst`, if any. A call may change
    /// globals and lists, but never a caller's vregs.
    Call {
        dst: Option<VReg>,
        callee: Callee,
        args: Vec<Operand>,
    },
}

impl Inst {
    /// Returns the vreg this instruction writes, if any.
    ///
    /// For example, `v2 = add v0, v1` defines `v2`.
    pub fn def(&self) -> Option<VReg> {
        match self {
            Inst::Copy { dst, .. }
            | Inst::Binary { dst, .. }
            | Inst::FloatBinary { dst, .. }
            | Inst::Neg { dst, .. }
            | Inst::FloatNeg { dst, .. }
            | Inst::Not { dst, .. }
            | Inst::Compare { dst, .. }
            | Inst::LoadGlobal { dst, .. }
            | Inst::StrAddr { dst, .. }
            | Inst::ListLen { dst, .. }
            | Inst::ListLoad { dst, .. }
            | Inst::ListGet { dst, .. }
            | Inst::IntToFloat { dst, .. }
            | Inst::FloatToInt { dst, .. } => Some(*dst),
            Inst::Call { dst, .. } => *dst,
            Inst::StoreGlobal { .. } | Inst::ListStore { .. } | Inst::ListInit { .. } => None,
        }
    }

    /// Returns the operands this instruction reads, in order.
    ///
    /// For example, `list_store v0, 1, v2` reads `v0`, `1`, and `v2`.
    pub fn operands(&self) -> Vec<Operand> {
        match self {
            Inst::Copy { src, .. }
            | Inst::Neg { src, .. }
            | Inst::FloatNeg { src, .. }
            | Inst::Not { src, .. }
            | Inst::IntToFloat { src, .. }
            | Inst::FloatToInt { src, .. }
            | Inst::StoreGlobal { src, .. } => vec![*src],
            Inst::Binary { lhs, rhs, .. }
            | Inst::FloatBinary { lhs, rhs, .. }
            | Inst::Compare { lhs, rhs, .. } => vec![*lhs, *rhs],
            Inst::LoadGlobal { .. } | Inst::StrAddr { .. } => vec![],
            Inst::ListLen { list, .. } => vec![*list],
            Inst::ListLoad { list, index, .. } | Inst::ListGet { list, index, .. } => {
                vec![*list, *index]
            }
            Inst::ListStore { list, index, value } => vec![*list, *index, *value],
            Inst::ListInit { list, value, .. } => vec![*list, *value],
            Inst::Call { args, .. } => args.clone(),
        }
    }

    /// Returns the vregs this instruction reads.
    pub fn uses(&self) -> Vec<VReg> {
        regs(&self.operands())
    }

    /// Returns whether this instruction calls out, which clobbers every caller-saved register.
    pub fn is_call(&self) -> bool {
        matches!(self, Inst::Call { .. })
    }
}

/// How a block ends.
#[derive(Clone, Debug, PartialEq)]
pub enum Terminator {
    Jump(BlockId),
    /// Goes to `then` if `cond` is nonzero, else to `otherwise`.
    Branch {
        cond: Operand,
        then: BlockId,
        otherwise: BlockId,
    },
    /// Goes to `then` if the comparison holds, else to `otherwise`, comparing the way
    /// [`Inst::Compare`] does.
    CmpBranch {
        cond: Cond,
        float: bool,
        lhs: Operand,
        rhs: Operand,
        then: BlockId,
        otherwise: BlockId,
    },
    /// Returns from the function, with `none` when there is no value.
    Return(Option<Operand>),
}

impl Terminator {
    /// Returns the operands the terminator reads.
    pub fn operands(&self) -> Vec<Operand> {
        match self {
            Terminator::Jump(_) | Terminator::Return(None) => vec![],
            Terminator::Branch { cond, .. } => vec![*cond],
            Terminator::CmpBranch { lhs, rhs, .. } => vec![*lhs, *rhs],
            Terminator::Return(Some(value)) => vec![*value],
        }
    }

    /// Returns the vregs the terminator reads.
    pub fn uses(&self) -> Vec<VReg> {
        regs(&self.operands())
    }

    /// Returns the blocks control can go to next.
    ///
    /// For example, `br v0 -> b1, b2` has successors `b1` and `b2`, and `ret` has none.
    pub fn successors(&self) -> Vec<BlockId> {
        match self {
            Terminator::Jump(target) => vec![*target],
            Terminator::Branch {
                then, otherwise, ..
            }
            | Terminator::CmpBranch {
                then, otherwise, ..
            } => vec![*then, *otherwise],
            Terminator::Return(_) => vec![],
        }
    }
}

/// A straight-line run of instructions ending in a terminator.
#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub insts: Vec<Inst>,
    pub term: Terminator,
    /// How many loops the block is nested in, which weights how costly spilling is.
    pub loop_depth: u32,
}

/// A stone function, or the synthesized `main` for top-level code when `name` is `None`.
#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub name: Option<String>,
    /// The vregs that hold the parameters on entry, in order.
    pub params: Vec<VReg>,
    /// The blocks in layout order, starting with the entry block.
    pub blocks: Vec<Block>,
    /// How many vregs the function uses, numbered `v0` up to this.
    pub vreg_count: u32,
}

/// The lowered module: every function, then `main`, plus the names of every global.
#[derive(Clone, Debug, PartialEq, Default)]
pub struct Program {
    pub functions: Vec<Function>,
    pub globals: Vec<String>,
}

fn regs(operands: &[Operand]) -> Vec<VReg> {
    operands
        .iter()
        .filter_map(|op| match op {
            Operand::Reg(reg) => Some(*reg),
            Operand::Imm(_) => None,
        })
        .collect()
}

impl fmt::Display for VReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

impl fmt::Display for Operand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Operand::Reg(reg) => write!(f, "{reg}"),
            Operand::Imm(n) => write!(f, "{n}"),
        }
    }
}

impl fmt::Display for Callee {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Callee::User(name) => write!(f, "{name}"),
            Callee::Runtime(label) => write!(f, "{label}"),
            Callee::Print(ty) => write!(f, "print[{ty}]"),
        }
    }
}

fn bin_name(op: BinOp) -> &'static str {
    match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "div",
        BinOp::Rem => "rem",
        BinOp::Pow => "pow",
    }
}

fn list(operands: &[Operand]) -> String {
    operands
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join(", ")
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Copy { dst, src } => write!(f, "{dst} = copy {src}"),
            Inst::Binary { op, dst, lhs, rhs } => {
                write!(f, "{dst} = {} {lhs}, {rhs}", bin_name(*op))
            }
            Inst::FloatBinary { op, dst, lhs, rhs } => {
                write!(f, "{dst} = f{} {lhs}, {rhs}", bin_name(*op))
            }
            Inst::Neg { dst, src } => write!(f, "{dst} = neg {src}"),
            Inst::FloatNeg { dst, src } => write!(f, "{dst} = fneg {src}"),
            Inst::Not { dst, src } => write!(f, "{dst} = not {src}"),
            Inst::Compare {
                cond,
                float,
                dst,
                lhs,
                rhs,
            } => {
                let prefix = if *float { "f" } else { "" };
                write!(f, "{dst} = {prefix}{} {lhs}, {rhs}", cond.name())
            }
            Inst::LoadGlobal { dst, name, checked } => {
                let suffix = if *checked { "_checked" } else { "" };
                write!(f, "{dst} = load_global{suffix} {name}")
            }
            Inst::StoreGlobal {
                name,
                src,
                mark_set,
            } => {
                let suffix = if *mark_set { "" } else { "_unmarked" };
                write!(f, "store_global{suffix} {name}, {src}")
            }
            Inst::StrAddr { dst, text } => write!(f, "{dst} = str {text:?}"),
            Inst::ListLen { dst, list } => write!(f, "{dst} = len {list}"),
            Inst::ListLoad { dst, list, index } => {
                write!(f, "{dst} = list_load {list}, {index}")
            }
            Inst::ListStore { list, index, value } => {
                write!(f, "list_store {list}, {index}, {value}")
            }
            Inst::ListGet { dst, list, index } => write!(f, "{dst} = list_get {list}, {index}"),
            Inst::ListInit { list, index, value } => {
                write!(f, "list_init {list}[{index}], {value}")
            }
            Inst::IntToFloat { dst, src } => write!(f, "{dst} = int_to_float {src}"),
            Inst::FloatToInt { dst, src } => write!(f, "{dst} = float_to_int {src}"),
            Inst::Call { dst, callee, args } => {
                if let Some(dst) = dst {
                    write!(f, "{dst} = ")?;
                }
                write!(f, "call {callee}({})", list(args))
            }
        }
    }
}

impl fmt::Display for Terminator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Terminator::Jump(target) => write!(f, "jmp {target}"),
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => write!(f, "br {cond} -> {then}, {otherwise}"),
            Terminator::CmpBranch {
                cond,
                float,
                lhs,
                rhs,
                then,
                otherwise,
            } => {
                let prefix = if *float { "f" } else { "" };
                write!(
                    f,
                    "{prefix}br_{} {lhs}, {rhs} -> {then}, {otherwise}",
                    cond.name()
                )
            }
            Terminator::Return(None) => write!(f, "ret"),
            Terminator::Return(Some(value)) => write!(f, "ret {value}"),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .params
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(
            f,
            "fn {}({params}):",
            self.name.as_deref().unwrap_or("main")
        )?;
        for (i, block) in self.blocks.iter().enumerate() {
            writeln!(f, "b{i}:")?;
            for inst in &block.insts {
                writeln!(f, "  {inst}")?;
            }
            writeln!(f, "  {}", block.term)?;
        }
        Ok(())
    }
}
