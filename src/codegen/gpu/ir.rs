//! The device IR: a kernel that each thread of a GPU runs, over typed values.
//!
//! A [`Kernel`] is a list of [`Block`]s of [`Inst`]s, each ending in a [`Terminator`], like a
//! function of the host [`ir`](crate::codegen::ir). Every value has one [`Ty`] for the whole
//! kernel, which PTX's typed register declarations need, but a value may be written more than
//! once, as a loop's index is. Blocks are stored in layout order.
//!
//! For example, the kernel that computes `a * 2.0` for every element prints as:
//!
//! ```text
//! kernel stone_k0(ptr, i64, ptr):
//! b0:
//!   v0 = param 0
//!   v1 = param 1
//!   v2 = param 2
//!   v3 = const 2.0
//!   v4 = global_index
//!   v5 = grid_size
//!   jmp b1
//! b1:
//!   v6 = cmp_lt v4, v1
//!   br v6 -> b3, b2
//! b2:
//!   ret
//! b3:
//!   v7 = load v2[v4]
//!   v8 = fmul v7, v3
//!   store v0[v4], v8
//!   v4 = add v4, v5
//!   jmp b1
//! ```

use crate::codegen::ir::Cond;
use crate::span::Span;
use std::fmt;

/// A value, numbered from 0 within its kernel.
///
/// For example, `Value(3)` prints as `v3`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Value(pub u32);

/// A block's index in [`Kernel::blocks`], which is also its position in layout order.
///
/// For example, `BlockId(2)` prints as `b2`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BlockId(pub usize);

/// The type of a value.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Ty {
    /// The result of a comparison, which only a branch reads.
    Pred,
    I64,
    F64,
    /// The address of an element in the GPU's global memory.
    Ptr,
}

/// An arithmetic operation, on two `I64`s or two `F64`s. `I64`s wrap, as on the CPU, and `Div`
/// applies only to `F64`s.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
}

/// A number the GPU gives each thread, as an `I64`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Special {
    /// The thread's position among all threads of the launch, from 0.
    GlobalIndex,
    /// How many threads the launch has.
    GridSize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    /// The kernel's parameter at `index`, as [`Kernel::params`] types it.
    Param {
        dst: Value,
        index: usize,
    },
    Special {
        dst: Value,
        which: Special,
    },
    /// A number, given by its bits, so a float is the integer of its IEEE 754 bits.
    Const {
        dst: Value,
        bits: i64,
    },
    /// Reads the 8-byte element at `index` of the elements starting at `ptr`.
    Load {
        dst: Value,
        ptr: Value,
        index: Value,
    },
    /// Writes `value` over the 8-byte element at `index` of the elements starting at `ptr`.
    Store {
        ptr: Value,
        index: Value,
        value: Value,
    },
    Binary {
        op: Op,
        dst: Value,
        lhs: Value,
        rhs: Value,
    },
    Neg {
        dst: Value,
        operand: Value,
    },
    /// Compares two `I64`s or two `F64`s into a `Pred`. Every comparison of floats but `Ne` is
    /// false when either is nan, as on the CPU.
    Cmp {
        cond: Cond,
        dst: Value,
        lhs: Value,
        rhs: Value,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Terminator {
    /// Ends the thread.
    Ret,
    Jump(BlockId),
    /// Goes to `then` if the `Pred` `cond` holds, else to `otherwise`.
    Branch {
        cond: Value,
        then: BlockId,
        otherwise: BlockId,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub insts: Vec<Inst>,
    pub term: Terminator,
}

/// A kernel, which every thread of a launch runs with the same parameters.
#[derive(Clone, Debug, PartialEq)]
pub struct Kernel {
    /// Its name, which PTX identifiers limit to letters, digits, `_`, and `$`.
    pub name: String,
    /// The type of each parameter, in order.
    pub params: Vec<Ty>,
    /// The type of each value, indexed by its number.
    pub types: Vec<Ty>,
    pub blocks: Vec<Block>,
    /// Each place in the program whose arithmetic the kernel computes.
    pub sites: Vec<Span>,
}

impl Kernel {
    /// Returns the type of `value`.
    pub fn ty(&self, value: Value) -> Ty {
        self.types[value.0 as usize]
    }

    /// Returns whether two kernels compute the same thing, whatever their names and sites.
    pub fn same_code(&self, other: &Kernel) -> bool {
        self.params == other.params && self.types == other.types && self.blocks == other.blocks
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "v{}", self.0)
    }
}

impl fmt::Display for BlockId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{}", self.0)
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Ty::Pred => "pred",
            Ty::I64 => "i64",
            Ty::F64 => "f64",
            Ty::Ptr => "ptr",
        })
    }
}

impl fmt::Display for Kernel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params: Vec<String> = self.params.iter().map(Ty::to_string).collect();
        writeln!(f, "kernel {}({}):", self.name, params.join(", "))?;
        for (id, block) in self.blocks.iter().enumerate() {
            writeln!(f, "{}:", BlockId(id))?;
            for inst in &block.insts {
                write!(f, "  ")?;
                self.show(inst, f)?;
                writeln!(f)?;
            }
            match &block.term {
                Terminator::Ret => writeln!(f, "  ret")?,
                Terminator::Jump(target) => writeln!(f, "  jmp {target}")?,
                Terminator::Branch {
                    cond,
                    then,
                    otherwise,
                } => writeln!(f, "  br {cond} -> {then}, {otherwise}")?,
            }
        }
        Ok(())
    }
}

impl Kernel {
    /// Writes one instruction, with an `f` before an operation on floats as the host IR writes
    /// it (`v3 = fadd v1, v2`).
    fn show(&self, inst: &Inst, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = |value: Value| match self.ty(value) {
            Ty::F64 => "f",
            _ => "",
        };
        match inst {
            Inst::Param { dst, index } => write!(f, "{dst} = param {index}"),
            Inst::Special { dst, which } => {
                let name = match which {
                    Special::GlobalIndex => "global_index",
                    Special::GridSize => "grid_size",
                };
                write!(f, "{dst} = {name}")
            }
            Inst::Const { dst, bits } => match self.ty(*dst) {
                Ty::F64 => write!(f, "{dst} = const {:?}", f64::from_bits(*bits as u64)),
                _ => write!(f, "{dst} = const {bits}"),
            },
            Inst::Load { dst, ptr, index } => write!(f, "{dst} = load {ptr}[{index}]"),
            Inst::Store { ptr, index, value } => write!(f, "store {ptr}[{index}], {value}"),
            Inst::Binary { op, dst, lhs, rhs } => {
                let name = match op {
                    Op::Add => "add",
                    Op::Sub => "sub",
                    Op::Mul => "mul",
                    Op::Div => "div",
                };
                write!(f, "{dst} = {}{name} {lhs}, {rhs}", prefix(*dst))
            }
            Inst::Neg { dst, operand } => write!(f, "{dst} = {}neg {operand}", prefix(*dst)),
            Inst::Cmp {
                cond,
                dst,
                lhs,
                rhs,
            } => write!(
                f,
                "{dst} = {}cmp_{} {lhs}, {rhs}",
                prefix(*lhs),
                cond.name()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_kernel_prints_its_blocks_with_float_operations_marked() {
        let (v, b) = (Value, BlockId);
        let kernel = Kernel {
            name: "stone_k0".to_string(),
            params: vec![Ty::Ptr, Ty::I64, Ty::Ptr],
            types: vec![
                Ty::Ptr,
                Ty::I64,
                Ty::Ptr,
                Ty::F64,
                Ty::I64,
                Ty::I64,
                Ty::Pred,
                Ty::F64,
                Ty::F64,
            ],
            blocks: vec![
                Block {
                    insts: vec![
                        Inst::Param {
                            dst: v(0),
                            index: 0,
                        },
                        Inst::Param {
                            dst: v(1),
                            index: 1,
                        },
                        Inst::Param {
                            dst: v(2),
                            index: 2,
                        },
                        Inst::Const {
                            dst: v(3),
                            bits: 2.0f64.to_bits() as i64,
                        },
                        Inst::Special {
                            dst: v(4),
                            which: Special::GlobalIndex,
                        },
                        Inst::Special {
                            dst: v(5),
                            which: Special::GridSize,
                        },
                    ],
                    term: Terminator::Jump(b(1)),
                },
                Block {
                    insts: vec![Inst::Cmp {
                        cond: Cond::Lt,
                        dst: v(6),
                        lhs: v(4),
                        rhs: v(1),
                    }],
                    term: Terminator::Branch {
                        cond: v(6),
                        then: b(3),
                        otherwise: b(2),
                    },
                },
                Block {
                    insts: vec![],
                    term: Terminator::Ret,
                },
                Block {
                    insts: vec![
                        Inst::Load {
                            dst: v(7),
                            ptr: v(2),
                            index: v(4),
                        },
                        Inst::Binary {
                            op: Op::Mul,
                            dst: v(8),
                            lhs: v(7),
                            rhs: v(3),
                        },
                        Inst::Store {
                            ptr: v(0),
                            index: v(4),
                            value: v(8),
                        },
                        Inst::Binary {
                            op: Op::Add,
                            dst: v(4),
                            lhs: v(4),
                            rhs: v(5),
                        },
                    ],
                    term: Terminator::Jump(b(1)),
                },
            ],
            sites: vec![],
        };
        assert_eq!(
            kernel.to_string(),
            "\
kernel stone_k0(ptr, i64, ptr):
b0:
  v0 = param 0
  v1 = param 1
  v2 = param 2
  v3 = const 2.0
  v4 = global_index
  v5 = grid_size
  jmp b1
b1:
  v6 = cmp_lt v4, v1
  br v6 -> b3, b2
b2:
  ret
b3:
  v7 = load v2[v4]
  v8 = fmul v7, v3
  store v0[v4], v8
  v4 = add v4, v5
  jmp b1
"
        );
    }
}
