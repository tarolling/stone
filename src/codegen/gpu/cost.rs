//! What computing a kernel costs per element, on the processor and on a GPU, for the runtime to
//! decide where to compute it.
//!
//! The compiler knows each piece of arithmetic's operations and the lists it reads and writes,
//! and the target's level says how many elements the processor's packed loop computes at a time.
//! It records, per kernel, the bytes a launch moves per element each way and an estimate of the
//! time an element takes on either side, in femtoseconds (a millionth of a nanosecond), so that
//! fractions of a picosecond survive in integers. What a GPU and its bus cost to reach (the
//! latency of a launch, the bandwidth of a transfer) depends on the machine the program runs
//! on, so the runtime measures those when it starts the GPU and combines them with these.
//!
//! Every estimate is the larger of the time the operations take and the time their memory
//! traffic takes, since arithmetic on lists is limited by one or the other.

use crate::codegen::ir::{BinOp, Kernel};
use crate::codegen::{Architecture, Os, Target};

/// The most nodes a kernel may have for the processor to compute it in packed instructions,
/// as lowering's `MAX_PACKED_SIZE` says.
pub const MAX_PACKED_SIZE: usize = 10;

/// A processor's time for one scalar operation of weight 1, in femtoseconds: a cycle at 4 GHz.
pub const CPU_OP_FS: u64 = 250_000;
/// A processor's time to move a byte between memory and its registers in a loop that streams
/// lists, in femtoseconds: about 14 GB/s.
pub const CPU_BYTE_FS: u64 = 70_000;
/// A GPU's time per element for an operation of weight 1 on floats, in femtoseconds: the
/// double precision rate of a consumer GPU, which is a small fraction of its single precision.
pub const GPU_FLOAT_OP_FS: u64 = 2_000;
/// A GPU's time per element for an operation of weight 1 on ints, in femtoseconds.
pub const GPU_INT_OP_FS: u64 = 500;
/// A GPU's time to move a byte between its own memory and its threads, in femtoseconds: about
/// 400 GB/s.
pub const GPU_BYTE_FS: u64 = 2_500;

/// What a kernel costs per element.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Cost {
    /// The bytes a launch copies to the GPU per element: 8 per list the kernel reads.
    pub in_bytes: u32,
    /// The bytes a launch copies back per element: 8, the result's.
    pub out_bytes: u32,
    /// The processor's time per element, in femtoseconds.
    pub cpu_fs: u32,
    /// The GPU's time per element once the lists are in its memory, in femtoseconds.
    pub gpu_fs: u32,
}

/// Returns what `kernel` costs per element on `target`'s processor and on a GPU, for a kernel of
/// floats if `float` and of ints otherwise that reads `lists` distinct lists.
///
/// For example, `a * 2.0 + b` on `x86_64-linux` reads 16 bytes and writes 8 per element, and
/// takes the processor 24 bytes times [`CPU_BYTE_FS`], which outweighs its two operations
/// computed two at a time.
pub fn estimate(kernel: &Kernel, float: bool, lists: u32, target: Target) -> Cost {
    let in_bytes = 8 * lists;
    let out_bytes = 8;
    let bytes = u64::from(in_bytes + out_bytes);
    let lanes = lanes(kernel, float, target);
    let cpu = (weight(kernel, float, Side::Cpu) * CPU_OP_FS / lanes).max(bytes * CPU_BYTE_FS);
    let op = if float {
        GPU_FLOAT_OP_FS
    } else {
        GPU_INT_OP_FS
    };
    let gpu = (weight(kernel, float, Side::Gpu) * op).max(bytes * GPU_BYTE_FS);
    Cost {
        in_bytes,
        out_bytes,
        cpu_fs: saturate(cpu),
        gpu_fs: saturate(gpu),
    }
}

/// Returns how many elements `target`'s processor computes at a time in the packed loop of
/// `kernel`: the elements of 8 bytes its widest vector registers hold, or 1 when the kernel is
/// too large to pack or multiplies ints on a level with no vector multiply of 64-bit ints.
///
/// For example, a float kernel packs 2 at a time on `x86_64-linux`, 4 on `x86_64v3-linux`, and
/// 8 on `x86_64v4-linux`, and an int multiply only on `x86_64v4-linux` and `aarch64v9-linux`.
pub fn lanes(kernel: &Kernel, float: bool, target: Target) -> u64 {
    if size(kernel) > MAX_PACKED_SIZE {
        return 1;
    }
    let (lanes, int_multiply) = match target.arch {
        Architecture::X64 => match target.level.major {
            0..=2 => (2, false),
            3 => (4, false),
            _ => (8, true),
        },
        // SVE's vectors may be wider, but 128 bits is the width every processor has
        Architecture::Arm64 => (2, target.os == Os::Linux && target.level.major >= 9),
    };
    if !float && kernel.multiplies() && !int_multiply {
        1
    } else {
        lanes
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Side {
    Cpu,
    Gpu,
}

/// Returns the sum of the weights of `kernel`'s operations on one side: 1 for adding,
/// subtracting, negating, and the bitwise operations, 1 for multiplying (4 for ints on a GPU,
/// which multiplies 64-bit ints in several steps), and for dividing floats 8 on a processor and
/// 40 on a GPU, whose double precision divider is slow.
fn weight(kernel: &Kernel, float: bool, side: Side) -> u64 {
    match kernel {
        Kernel::Element(_) | Kernel::Number(_) => 0,
        Kernel::Neg(operand) => 1 + weight(operand, float, side),
        Kernel::Binary(op, lhs, rhs) => {
            let own = match (op, side) {
                (BinOp::Div, Side::Cpu) => 8,
                (BinOp::Div, Side::Gpu) => 40,
                (BinOp::Mul, Side::Gpu) if !float => 4,
                _ => 1,
            };
            own + weight(lhs, float, side) + weight(rhs, float, side)
        }
    }
}

/// Returns how many nodes `kernel` has, as lowering counts them for its packed loop.
fn size(kernel: &Kernel) -> usize {
    match kernel {
        Kernel::Element(_) | Kernel::Number(_) => 1,
        Kernel::Neg(operand) => 1 + size(operand),
        Kernel::Binary(_, lhs, rhs) => 1 + size(lhs) + size(rhs),
    }
}

fn saturate(value: u64) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::Level;
    use crate::codegen::ir::{Operand, VReg};

    fn list(reg: u32) -> Box<Kernel> {
        Box::new(Kernel::Element(Operand::Reg(VReg(reg))))
    }

    fn number() -> Box<Kernel> {
        Box::new(Kernel::Number(Operand::Imm(2)))
    }

    fn binary(op: BinOp, lhs: Box<Kernel>, rhs: Box<Kernel>) -> Box<Kernel> {
        Box::new(Kernel::Binary(op, lhs, rhs))
    }

    fn at(arch: Architecture, os: Os, major: u8) -> Target {
        Target {
            arch,
            os,
            level: Level { major, minor: 0 },
        }
    }

    #[test]
    fn the_processor_packs_as_many_elements_as_its_vectors_hold() {
        let saxpy = binary(BinOp::Add, binary(BinOp::Mul, list(1), number()), list(2));
        let x64 = |major| at(Architecture::X64, Os::Linux, major);
        assert_eq!(lanes(&saxpy, true, x64(1)), 2);
        assert_eq!(lanes(&saxpy, true, x64(2)), 2);
        assert_eq!(lanes(&saxpy, true, x64(3)), 4);
        assert_eq!(lanes(&saxpy, true, x64(4)), 8);
        assert_eq!(lanes(&saxpy, true, Target::ARM64_LINUX), 2);
        assert_eq!(
            lanes(&saxpy, true, at(Architecture::Arm64, Os::Linux, 9)),
            2
        );
    }

    #[test]
    fn an_int_multiply_packs_only_where_vectors_multiply_ints() {
        let product = binary(BinOp::Mul, list(1), list(2));
        assert_eq!(lanes(&product, false, Target::X64_LINUX), 1);
        assert_eq!(
            lanes(&product, false, at(Architecture::X64, Os::Linux, 3)),
            1
        );
        assert_eq!(
            lanes(&product, false, at(Architecture::X64, Os::Linux, 4)),
            8
        );
        assert_eq!(lanes(&product, false, Target::ARM64_LINUX), 1);
        assert_eq!(
            lanes(&product, false, at(Architecture::Arm64, Os::Linux, 9)),
            2
        );
        // macOS has no SVE at any level
        assert_eq!(
            lanes(&product, false, at(Architecture::Arm64, Os::MacOs, 9)),
            1
        );
        // a sum of ints packs everywhere
        let sum = binary(BinOp::Add, list(1), list(2));
        assert_eq!(lanes(&sum, false, Target::X64_LINUX), 2);
    }

    #[test]
    fn a_kernel_too_large_to_pack_is_computed_an_element_at_a_time() {
        // eleven nodes: five additions of six lists
        let mut tree = list(0);
        for reg in 1..6 {
            tree = binary(BinOp::Add, tree, list(reg));
        }
        assert_eq!(size(&tree), 11);
        assert_eq!(lanes(&tree, true, at(Architecture::X64, Os::Linux, 4)), 1);
    }

    #[test]
    fn arithmetic_on_lists_is_limited_by_its_memory_traffic() {
        let saxpy = binary(BinOp::Add, binary(BinOp::Mul, list(1), number()), list(2));
        let cost = estimate(&saxpy, true, 2, Target::X64_LINUX);
        assert_eq!((cost.in_bytes, cost.out_bytes), (16, 8));
        // 24 bytes outweigh two operations two at a time
        assert_eq!(u64::from(cost.cpu_fs), 24 * CPU_BYTE_FS);
        assert_eq!(u64::from(cost.gpu_fs), 24 * GPU_BYTE_FS);
    }

    #[test]
    fn division_weighs_more_on_either_side() {
        // a / b / b / b / b: four divisions of two lists
        let mut tree = list(1);
        for _ in 0..4 {
            tree = binary(BinOp::Div, tree, list(2));
        }
        let cost = estimate(&tree, true, 2, Target::X64_LINUX);
        assert_eq!(u64::from(cost.cpu_fs), 4 * 8 * CPU_OP_FS / 2);
        assert_eq!(u64::from(cost.gpu_fs), 4 * 40 * GPU_FLOAT_OP_FS);
        // ints multiply in several steps on a GPU
        let product = binary(BinOp::Mul, list(1), list(2));
        let cost = estimate(&product, false, 2, Target::X64_LINUX);
        assert_eq!(u64::from(cost.gpu_fs), 24 * GPU_BYTE_FS);
        assert_eq!(weight(&product, false, Side::Gpu), 4);
        assert_eq!(weight(&product, true, Side::Gpu), 1);
    }

    #[test]
    fn a_wider_vector_makes_the_processor_cheaper_until_memory_limits_it() {
        // four divisions of one list by a number
        let mut tree = list(1);
        for _ in 0..4 {
            tree = binary(BinOp::Div, tree, number());
        }
        let v1 = estimate(&tree, true, 1, Target::X64_LINUX).cpu_fs;
        let v3 = estimate(&tree, true, 1, at(Architecture::X64, Os::Linux, 3)).cpu_fs;
        let v4 = estimate(&tree, true, 1, at(Architecture::X64, Os::Linux, 4)).cpu_fs;
        assert_eq!(u64::from(v1), 4 * 8 * CPU_OP_FS / 2);
        assert_eq!(u64::from(v3), 4 * 8 * CPU_OP_FS / 4);
        assert_eq!(u64::from(v4), 16 * CPU_BYTE_FS);
    }
}
