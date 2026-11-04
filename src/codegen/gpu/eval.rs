//! Runs a device kernel on the processor, one thread after another, so tests can check what a
//! kernel computes without a GPU.

use super::ir::{Inst, Kernel, Op, Special, Terminator, Ty};
use crate::codegen::ir::Cond;

/// What a launch passes for one parameter.
#[derive(Clone, Copy, Debug)]
pub enum Arg {
    /// The start of a buffer, by its index in the buffers [`run`] takes.
    Buffer(usize),
    /// A number, by its bits.
    Number(i64),
}

/// Runs `kernel` with `args` over `buffers` of 8-byte elements, as a launch of `blocks` blocks of
/// `threads` threads each would.
///
/// For example, running the kernel of `a * 2.0` with `[Buffer(0), Number(2), Buffer(1)]` on one
/// thread doubles the two elements of buffer 1 into buffer 0.
pub fn run(kernel: &Kernel, args: &[Arg], buffers: &mut [Vec<u64>], blocks: u64, threads: u64) {
    assert_eq!(
        args.len(),
        kernel.params.len(),
        "arguments of {}",
        kernel.name
    );
    for global in 0..blocks * threads {
        thread(kernel, args, buffers, global, blocks * threads);
    }
}

/// Runs one thread, the `global`th of `grid`.
fn thread(kernel: &Kernel, args: &[Arg], buffers: &mut [Vec<u64>], global: u64, grid: u64) {
    let mut values = vec![0u64; kernel.types.len()];
    let float = |bits: u64| f64::from_bits(bits);
    let mut block = 0;
    for _ in 0..1_000_000 {
        for inst in &kernel.blocks[block].insts {
            match *inst {
                Inst::Param { dst, index } => {
                    values[dst.0 as usize] = match args[index] {
                        Arg::Buffer(buffer) => buffer as u64,
                        Arg::Number(bits) => bits as u64,
                    }
                }
                Inst::Special { dst, which } => {
                    values[dst.0 as usize] = match which {
                        Special::GlobalIndex => global,
                        Special::GridSize => grid,
                    }
                }
                Inst::Const { dst, bits } => values[dst.0 as usize] = bits as u64,
                Inst::Load { dst, ptr, index } => {
                    let buffer = &buffers[values[ptr.0 as usize] as usize];
                    values[dst.0 as usize] = buffer[values[index.0 as usize] as usize];
                }
                Inst::Store { ptr, index, value } => {
                    let buffer = &mut buffers[values[ptr.0 as usize] as usize];
                    buffer[values[index.0 as usize] as usize] = values[value.0 as usize];
                }
                Inst::Binary { op, dst, lhs, rhs } => {
                    let (a, b) = (values[lhs.0 as usize], values[rhs.0 as usize]);
                    values[dst.0 as usize] = match kernel.ty(dst) {
                        Ty::F64 => match op {
                            Op::Add => float(a) + float(b),
                            Op::Sub => float(a) - float(b),
                            Op::Mul => float(a) * float(b),
                            Op::Div => float(a) / float(b),
                        }
                        .to_bits(),
                        _ => match op {
                            Op::Add => a.wrapping_add(b),
                            Op::Sub => a.wrapping_sub(b),
                            Op::Mul => a.wrapping_mul(b),
                            Op::Div => panic!("int division in {}", kernel.name),
                        },
                    }
                }
                Inst::Neg { dst, operand } => {
                    let a = values[operand.0 as usize];
                    values[dst.0 as usize] = match kernel.ty(dst) {
                        Ty::F64 => (-float(a)).to_bits(),
                        _ => a.wrapping_neg(),
                    }
                }
                Inst::Cmp {
                    cond,
                    dst,
                    lhs,
                    rhs,
                } => {
                    let (a, b) = (values[lhs.0 as usize], values[rhs.0 as usize]);
                    let holds = match kernel.ty(lhs) {
                        Ty::F64 => compare(cond, float(a), float(b)),
                        _ => compare(cond, a as i64, b as i64),
                    };
                    values[dst.0 as usize] = holds as u64;
                }
            }
        }
        block = match kernel.blocks[block].term {
            Terminator::Ret => return,
            Terminator::Jump(target) => target.0,
            Terminator::Branch {
                cond,
                then,
                otherwise,
            } => match values[cond.0 as usize] {
                0 => otherwise.0,
                _ => then.0,
            },
        };
    }
    panic!("{} ran too long", kernel.name);
}

/// Returns whether `cond` holds for `a` and `b`, which for floats is false with a nan unless
/// `cond` is `Ne`.
fn compare<T: PartialOrd>(cond: Cond, a: T, b: T) -> bool {
    match cond {
        Cond::Eq => a == b,
        Cond::Ne => a != b,
        Cond::Lt => a < b,
        Cond::Le => a <= b,
        Cond::Gt => a > b,
        Cond::Ge => a >= b,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::gpu::kernel::{Argument, arguments, translate};
    use crate::codegen::ir::{self, BinOp, Operand, VReg};
    use crate::stdlib::random::Rng;
    use std::collections::HashMap;

    /// The lists random kernels read, as `v1`, `v2`, and `v3`, and the number in a register they
    /// may name, as `v9`.
    const LISTS: u32 = 3;
    const NUMBER: VReg = VReg(9);

    fn below(rng: &mut Rng, n: u64) -> u64 {
        rng.next_u64() % n
    }

    /// Returns random bits for an element, often a value with a rule of its own (zeros, nan,
    /// infinities, the ints at the ends of the range).
    fn bits(rng: &mut Rng, float: bool) -> u64 {
        let specials: &[u64] = match float {
            true => &[
                0.0f64.to_bits(),
                (-0.0f64).to_bits(),
                1.5f64.to_bits(),
                (-2.25f64).to_bits(),
                f64::INFINITY.to_bits(),
                f64::NAN.to_bits(),
                f64::MAX.to_bits(),
            ],
            false => &[0, 1, u64::MAX, i64::MIN as u64, i64::MAX as u64, 3],
        };
        match below(rng, 3) {
            0 => specials[below(rng, specials.len() as u64) as usize],
            1 if float => (below(rng, 2000) as f64 / 8.0 - 100.0).to_bits(),
            _ => rng.next_u64(),
        }
    }

    fn tree(rng: &mut Rng, float: bool, depth: u32) -> ir::Kernel {
        let leaf = depth == 0 || below(rng, 3) == 0;
        if leaf {
            return match below(rng, 4) {
                0 => ir::Kernel::Number(Operand::Imm(bits(rng, float) as i64)),
                1 => ir::Kernel::Number(Operand::Reg(NUMBER)),
                _ => ir::Kernel::Element(Operand::Reg(VReg(1 + below(rng, LISTS as u64) as u32))),
            };
        }
        let ops: &[BinOp] = match float {
            true => &[BinOp::Add, BinOp::Sub, BinOp::Mul, BinOp::Div],
            false => &[BinOp::Add, BinOp::Sub, BinOp::Mul],
        };
        match below(rng, 5) {
            0 => ir::Kernel::Neg(Box::new(tree(rng, float, depth - 1))),
            _ => ir::Kernel::Binary(
                ops[below(rng, ops.len() as u64) as usize],
                Box::new(tree(rng, float, depth - 1)),
                Box::new(tree(rng, float, depth - 1)),
            ),
        }
    }

    /// Computes one element of `kernel` as the processor's loop does, noting a zero divisor in
    /// `failed`.
    fn scalar(
        kernel: &ir::Kernel,
        float: bool,
        operands: &HashMap<Operand, u64>,
        lists: &HashMap<Operand, Vec<u64>>,
        i: usize,
        failed: &mut bool,
    ) -> u64 {
        match kernel {
            ir::Kernel::Element(list) => lists[list][i],
            ir::Kernel::Number(Operand::Imm(bits)) => *bits as u64,
            ir::Kernel::Number(number) => operands[number],
            ir::Kernel::Neg(operand) => {
                let a = scalar(operand, float, operands, lists, i, failed);
                match float {
                    true => (-f64::from_bits(a)).to_bits(),
                    false => a.wrapping_neg(),
                }
            }
            ir::Kernel::Binary(op, lhs, rhs) => {
                let a = scalar(lhs, float, operands, lists, i, failed);
                let b = scalar(rhs, float, operands, lists, i, failed);
                if !float {
                    return match op {
                        BinOp::Add => a.wrapping_add(b),
                        BinOp::Sub => a.wrapping_sub(b),
                        _ => a.wrapping_mul(b),
                    };
                }
                let (a, b) = (f64::from_bits(a), f64::from_bits(b));
                match op {
                    BinOp::Add => a + b,
                    BinOp::Sub => a - b,
                    BinOp::Mul => a * b,
                    _ => {
                        *failed |= b == 0.0;
                        a / b
                    }
                }
                .to_bits()
            }
        }
    }

    #[test]
    fn random_kernels_compute_what_the_processor_loop_does_at_every_launch_shape() {
        let mut rng = Rng::seeded(7);
        for case in 0..400 {
            let float = case % 2 == 0;
            let source = tree(&mut rng, float, 4);
            let kernel = translate(&source, float).unwrap();
            let len = below(&mut rng, 20) as usize;
            let mut lists = HashMap::new();
            for n in 1..=LISTS {
                let elements = (0..len).map(|_| bits(&mut rng, float)).collect();
                lists.insert(Operand::Reg(VReg(n)), elements);
            }
            let number = bits(&mut rng, float);
            let operands = HashMap::from([(Operand::Reg(NUMBER), number)]);

            let mut failed = false;
            let expected: Vec<u64> = (0..len)
                .map(|i| scalar(&source, float, &operands, &lists, i, &mut failed))
                .collect();

            for (blocks, threads) in [(1, 1), (1, 3), (2, 4), (8, 32)] {
                // the result, then the lists, then the flag
                let mut buffers = vec![vec![0; len]];
                let mut args = vec![Arg::Buffer(0), Arg::Number(len as i64)];
                for argument in arguments(&source) {
                    args.push(match argument {
                        Argument::List(list) => {
                            buffers.push(lists[&list].clone());
                            Arg::Buffer(buffers.len() - 1)
                        }
                        Argument::Number(_) => Arg::Number(number as i64),
                    });
                }
                if kernel.params.len() > args.len() {
                    buffers.push(vec![0]);
                    args.push(Arg::Buffer(buffers.len() - 1));
                }
                run(&kernel, &args, &mut buffers, blocks, threads);
                let shape = format!("{source:?} on {blocks}x{threads}, length {len}");
                assert_eq!(buffers[0], expected, "{shape}");
                if kernel.params.len() > 2 + arguments(&source).len() {
                    assert_eq!(buffers.last().unwrap()[0], failed as u64, "{shape}");
                } else {
                    assert!(!failed, "{shape}");
                }
            }
        }
    }
}
