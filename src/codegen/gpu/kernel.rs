//! Turns the arithmetic on lists that lowering recorded into device [`Kernel`]s.
//!
//! Each kernel takes, in order, the address of the result's elements, the length, then for each
//! list and each number that is not a constant, left to right as the arithmetic names them, its
//! elements' address or its value, and last, if it divides, the address of a flag it sets to 1
//! when a divisor is zero. Its threads walk the elements in a grid-stride loop, so it computes
//! every element whatever the launch's shape: a thread starts at its global index and steps by
//! the number of threads.

use super::cost::{self, Cost};
use super::ir::{Block, BlockId, Inst, Kernel, Op, Special, Terminator, Ty, Value};
use crate::codegen::Target;
use crate::codegen::ir::{self, BinOp, Cond, DeviceKernel, Operand};
use std::collections::HashMap;

/// The kernels of a program and which of them computes each recorded piece of arithmetic.
#[derive(Debug)]
pub struct Numbered {
    /// The distinct kernels, named `stone_k0`, `stone_k1`, ... in the order first met.
    pub kernels: Vec<Kernel>,
    /// For each recorded piece of arithmetic, the index in `kernels` of the kernel that computes
    /// it, which is the number `stone.gpu_launch` is called with.
    pub record_kernel: Vec<usize>,
    /// What a launch needs to know about each kernel besides its code, indexed like `kernels`.
    pub shapes: Vec<Shape>,
    /// Each kernel's arithmetic as [`program`] writes it, for a simulated GPU, indexed like
    /// `kernels`.
    pub programs: Vec<Vec<u8>>,
}

/// How a kernel is launched, apart from its machine code.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Shape {
    /// Whether the kernel computes floats, rather than ints.
    pub float: bool,
    /// Whether the kernel takes the address of a flag it sets when a divisor is zero, last.
    pub fail_flag: bool,
    /// The number of 8-byte parameters, the flag included.
    pub params: u32,
    /// The parameters that are the address of a list's elements, the result's included, as bit
    /// `i` for parameter `i`; a kernel of more than 64 parameters is never launched.
    pub lists: u64,
    /// What computing an element costs on the processor and on a GPU.
    pub cost: Cost,
}

impl Shape {
    /// The most parameters a kernel that is launched may have, one per bit of
    /// [`lists`](Shape::lists).
    pub const MAX_PARAMS: u32 = 64;

    /// Returns whether a launch can pass the kernel what it takes.
    pub fn launchable(&self) -> bool {
        self.params <= Self::MAX_PARAMS
    }
}

/// Returns a kernel for each distinct piece of arithmetic in `recorded`, named `stone_k0`,
/// `stone_k1`, ... in the order first met, each with the sites that compute it.
///
/// For example, `c = a * 2.0 + b` in two copies of a generic function records the same
/// arithmetic twice, which gives one kernel with two sites.
pub fn kernels(recorded: &[DeviceKernel]) -> Result<Vec<Kernel>, String> {
    // the code is the same whatever the processor, which only changes the costs
    numbered(recorded, Target::X64_LINUX).map(|numbered| numbered.kernels)
}

/// Returns the kernels of `recorded` as [`kernels`] does, and for each record the kernel that
/// computes it, with what each kernel costs per element on `target`'s processor.
///
/// For example, records of `a * 2.0`, `b * 2.0` (the same code), and `a * 4.0` give two kernels
/// and the map `[0, 0, 1]`.
pub fn numbered(recorded: &[DeviceKernel], target: Target) -> Result<Numbered, String> {
    let mut kernels: Vec<Kernel> = Vec::new();
    let mut record_kernel = Vec::with_capacity(recorded.len());
    let mut shapes = Vec::new();
    let mut programs = Vec::new();
    for record in recorded {
        let mut kernel = translate(&record.kernel, record.float)?;
        match kernels.iter().position(|known| known.same_code(&kernel)) {
            Some(index) => {
                let known = &mut kernels[index];
                if !known.sites.contains(&record.span) {
                    known.sites.push(record.span);
                }
                record_kernel.push(index);
            }
            None => {
                kernel.name = format!("stone_k{}", kernels.len());
                kernel.sites.push(record.span);
                record_kernel.push(kernels.len());
                let fail_flag = record.float && divides(&record.kernel);
                // every address but the flag's is a list's
                let addresses = kernel.params.len() - usize::from(fail_flag);
                let lists = kernel.params[..addresses]
                    .iter()
                    .enumerate()
                    .filter(|&(index, &ty)| ty == Ty::Ptr && index < Shape::MAX_PARAMS as usize)
                    .fold(0u64, |lists, (index, _)| lists | 1 << index);
                // the result is the first list, and the others are what the kernel reads
                let read = lists.count_ones().saturating_sub(1);
                shapes.push(Shape {
                    float: record.float,
                    fail_flag,
                    params: kernel.params.len() as u32,
                    lists,
                    cost: cost::estimate(&record.kernel, record.float, read, target),
                });
                programs.push(program(&record.kernel));
                kernels.push(kernel);
            }
        }
    }
    Ok(Numbered {
        kernels,
        record_kernel,
        shapes,
        programs,
    })
}

/// The operations of a [`program`], one byte each. A read names the parameter it reads in the
/// byte after it, and a constant's 8 bytes follow it, little-endian.
pub mod op {
    /// Pushes the element of the list whose elements' address is the parameter.
    pub const ELEMENT: u8 = 1;
    /// Pushes the parameter, a number the same for every element.
    pub const NUMBER: u8 = 2;
    /// Pushes the constant in the next 8 bytes.
    pub const CONSTANT: u8 = 3;
    pub const ADD: u8 = 16;
    pub const SUB: u8 = 17;
    pub const MUL: u8 = 18;
    pub const DIV: u8 = 19;
    pub const NEG: u8 = 20;
    pub const AND: u8 = 21;
    pub const OR: u8 = 22;
    pub const XOR: u8 = 23;
}

/// Returns `kernel` as a program for a stack machine that computes one element: its values and
/// operations in postfix order, each read naming the kernel's parameter it reads (2 for the
/// first after the result's address and the length). Whether it computes floats or ints is the
/// kernel's shape's.
///
/// For example, `fadd(fmul([v1], 2.0), [v2])` is `ELEMENT 2, CONSTANT <2.0's bits>, MUL,
/// ELEMENT 3, ADD`.
pub fn program(kernel: &ir::Kernel) -> Vec<u8> {
    fn write(kernel: &ir::Kernel, params: &[Operand], out: &mut Vec<u8>) {
        let param = |operand: &Operand| 2 + params.iter().position(|p| p == operand).unwrap() as u8;
        match kernel {
            ir::Kernel::Element(list) => out.extend([op::ELEMENT, param(list)]),
            ir::Kernel::Number(Operand::Imm(value)) => {
                out.push(op::CONSTANT);
                out.extend(value.to_le_bytes());
            }
            ir::Kernel::Number(number) => out.extend([op::NUMBER, param(number)]),
            ir::Kernel::Binary(binary, lhs, rhs) => {
                write(lhs, params, out);
                write(rhs, params, out);
                out.push(match binary {
                    BinOp::Add => op::ADD,
                    BinOp::Sub => op::SUB,
                    BinOp::Mul => op::MUL,
                    BinOp::Div => op::DIV,
                    BinOp::And => op::AND,
                    BinOp::Or => op::OR,
                    // only what `translate` accepts is recorded
                    _ => op::XOR,
                });
            }
            ir::Kernel::Neg(operand) => {
                write(operand, params, out);
                out.push(op::NEG);
            }
        }
    }
    let params: Vec<Operand> = arguments(kernel)
        .into_iter()
        .map(|argument| match argument {
            Argument::List(operand) | Argument::Number(operand) => operand,
        })
        .collect();
    let mut out = Vec::new();
    write(kernel, &params, &mut out);
    out
}

/// Returns the kernel that computes `kernel` for every element, of floats if `float` and of ints
/// otherwise, with no name or sites yet.
///
/// For example, `fmul([v1], 4611686018427387904)` (`a * 2.0`) becomes a kernel of parameters
/// `(ptr, i64, ptr)` whose loop stores `load v2[v4] * 2.0` at `v0[v4]`.
pub fn translate(kernel: &ir::Kernel, float: bool) -> Result<Kernel, String> {
    let element = if float { Ty::F64 } else { Ty::I64 };
    let mut builder = Builder::default();
    let out = builder.param(Ty::Ptr);
    let len = builder.param(Ty::I64);
    let mut operands = HashMap::new();
    for argument in arguments(kernel) {
        let (operand, ty) = match argument {
            Argument::List(list) => (list, Ty::Ptr),
            Argument::Number(number) => (number, element),
        };
        operands.insert(operand, builder.param(ty));
    }
    let divides = float && divides(kernel);
    let fail = divides.then(|| builder.param(Ty::Ptr));
    let mut constants = HashMap::new();
    builder.constants_of(kernel, element, &mut constants);
    let checks = fail.map(|fail| Checks {
        fail,
        zero: builder.constant(Ty::F64, 0),
        first: builder.constant(Ty::I64, 0),
        one: builder.constant(Ty::I64, 1),
    });
    let index = builder.special(Special::GlobalIndex);
    let stride = builder.special(Special::GridSize);

    let (header, exit, body) = (builder.block(), builder.block(), builder.block());
    builder.end(Terminator::Jump(header));
    builder.current = header;
    let more = builder.value(Ty::Pred);
    builder.push(Inst::Cmp {
        cond: Cond::Lt,
        dst: more,
        lhs: index,
        rhs: len,
    });
    builder.end(Terminator::Branch {
        cond: more,
        then: body,
        otherwise: exit,
    });
    builder.current = exit;
    builder.end(Terminator::Ret);
    builder.current = body;
    let mut element = Element {
        element,
        index,
        operands: &operands,
        constants: &constants,
        checks,
        loads: HashMap::new(),
    };
    let value = element.compute(&mut builder, kernel)?;
    builder.push(Inst::Store {
        ptr: out,
        index,
        value,
    });
    builder.push(Inst::Binary {
        op: Op::Add,
        dst: index,
        lhs: index,
        rhs: stride,
    });
    builder.end(Terminator::Jump(header));
    Ok(Kernel {
        name: String::new(),
        params: builder.params,
        types: builder.types,
        blocks: builder.blocks,
        sites: Vec::new(),
    })
}

/// A value a kernel takes between the length and the flag.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Argument {
    /// A list, whose elements' address the kernel takes.
    List(Operand),
    /// A number the same for every element that is not a constant.
    Number(Operand),
}

/// Returns what the kernel computing `kernel` takes after the result's address and the length:
/// each list and each number in a register, left to right, the first time `kernel` names it.
///
/// For example, `fadd(fmul([v1], v4), [v1])` takes `[List(v1), Number(v4)]`.
pub fn arguments(kernel: &ir::Kernel) -> Vec<Argument> {
    fn walk(kernel: &ir::Kernel, out: &mut Vec<Argument>) {
        let argument = match kernel {
            ir::Kernel::Element(list) => Argument::List(*list),
            ir::Kernel::Number(number @ Operand::Reg(_)) => Argument::Number(*number),
            ir::Kernel::Number(Operand::Imm(_)) => return,
            ir::Kernel::Binary(_, lhs, rhs) => {
                walk(lhs, out);
                walk(rhs, out);
                return;
            }
            ir::Kernel::Neg(operand) => return walk(operand, out),
        };
        if !out.contains(&argument) {
            out.push(argument);
        }
    }
    let mut out = Vec::new();
    walk(kernel, &mut out);
    out
}

/// Returns whether `kernel` divides anywhere.
fn divides(kernel: &ir::Kernel) -> bool {
    match kernel {
        ir::Kernel::Element(_) | ir::Kernel::Number(_) => false,
        ir::Kernel::Binary(op, lhs, rhs) => *op == BinOp::Div || divides(lhs) || divides(rhs),
        ir::Kernel::Neg(operand) => divides(operand),
    }
}

/// What a division needs to check its divisor: the flag's address, the float 0.0, and the ints 0
/// and 1 that set the flag.
#[derive(Clone, Copy)]
struct Checks {
    fail: Value,
    zero: Value,
    first: Value,
    one: Value,
}

/// Builds a kernel's blocks, starting in the entry block.
struct Builder {
    params: Vec<Ty>,
    types: Vec<Ty>,
    blocks: Vec<Block>,
    current: BlockId,
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            params: Vec::new(),
            types: Vec::new(),
            blocks: vec![Block {
                insts: Vec::new(),
                term: Terminator::Ret,
            }],
            current: BlockId(0),
        }
    }
}

impl Builder {
    fn value(&mut self, ty: Ty) -> Value {
        self.types.push(ty);
        Value(self.types.len() as u32 - 1)
    }

    fn push(&mut self, inst: Inst) {
        self.blocks[self.current.0].insts.push(inst);
    }

    fn end(&mut self, term: Terminator) {
        self.blocks[self.current.0].term = term;
    }

    fn block(&mut self) -> BlockId {
        self.blocks.push(Block {
            insts: Vec::new(),
            term: Terminator::Ret,
        });
        BlockId(self.blocks.len() - 1)
    }

    fn param(&mut self, ty: Ty) -> Value {
        let dst = self.value(ty);
        self.push(Inst::Param {
            dst,
            index: self.params.len(),
        });
        self.params.push(ty);
        dst
    }

    fn constant(&mut self, ty: Ty, bits: i64) -> Value {
        let dst = self.value(ty);
        self.push(Inst::Const { dst, bits });
        dst
    }

    fn special(&mut self, which: Special) -> Value {
        let dst = self.value(Ty::I64);
        self.push(Inst::Special { dst, which });
        dst
    }

    /// Adds a constant for each distinct immediate number `kernel` names, left to right.
    fn constants_of(
        &mut self,
        kernel: &ir::Kernel,
        element: Ty,
        constants: &mut HashMap<i64, Value>,
    ) {
        match kernel {
            ir::Kernel::Number(Operand::Imm(bits)) => {
                if !constants.contains_key(bits) {
                    let value = self.constant(element, *bits);
                    constants.insert(*bits, value);
                }
            }
            ir::Kernel::Element(_) | ir::Kernel::Number(Operand::Reg(_)) => {}
            ir::Kernel::Binary(_, lhs, rhs) => {
                self.constants_of(lhs, element, constants);
                self.constants_of(rhs, element, constants);
            }
            ir::Kernel::Neg(operand) => self.constants_of(operand, element, constants),
        }
    }
}

/// Computes one element in the loop's body, reading each list once.
struct Element<'a> {
    /// The type of every list's elements.
    element: Ty,
    index: Value,
    operands: &'a HashMap<Operand, Value>,
    constants: &'a HashMap<i64, Value>,
    checks: Option<Checks>,
    loads: HashMap<Value, Value>,
}

impl Element<'_> {
    fn compute(&mut self, builder: &mut Builder, kernel: &ir::Kernel) -> Result<Value, String> {
        Ok(match kernel {
            ir::Kernel::Element(list) => {
                let ptr = self.operands[list];
                if let Some(&loaded) = self.loads.get(&ptr) {
                    return Ok(loaded);
                }
                let dst = builder.value(self.element);
                builder.push(Inst::Load {
                    dst,
                    ptr,
                    index: self.index,
                });
                self.loads.insert(ptr, dst);
                dst
            }
            ir::Kernel::Number(Operand::Imm(bits)) => self.constants[bits],
            ir::Kernel::Number(number) => self.operands[number],
            ir::Kernel::Binary(op, lhs, rhs) => {
                let lhs = self.compute(builder, lhs)?;
                let rhs = self.compute(builder, rhs)?;
                let ty = builder.types[lhs.0 as usize];
                let op = match (op, ty) {
                    (BinOp::Add, _) => Op::Add,
                    (BinOp::Sub, _) => Op::Sub,
                    (BinOp::Mul, _) => Op::Mul,
                    (BinOp::Div, Ty::F64) => Op::Div,
                    (BinOp::And, Ty::I64) => Op::And,
                    (BinOp::Or, Ty::I64) => Op::Or,
                    (BinOp::Xor, Ty::I64) => Op::Xor,
                    _ => return Err(format!("a GPU kernel cannot compute {op:?} on {ty} yet")),
                };
                if op == Op::Div
                    && let Some(checks) = self.checks
                {
                    let zero = builder.value(Ty::Pred);
                    builder.push(Inst::Cmp {
                        cond: Cond::Eq,
                        dst: zero,
                        lhs: rhs,
                        rhs: checks.zero,
                    });
                    let (fail, after) = (builder.block(), builder.block());
                    builder.end(Terminator::Branch {
                        cond: zero,
                        then: fail,
                        otherwise: after,
                    });
                    builder.current = fail;
                    builder.push(Inst::Store {
                        ptr: checks.fail,
                        index: checks.first,
                        value: checks.one,
                    });
                    builder.end(Terminator::Jump(after));
                    builder.current = after;
                }
                let dst = builder.value(ty);
                builder.push(Inst::Binary { op, dst, lhs, rhs });
                dst
            }
            ir::Kernel::Neg(operand) => {
                let operand = self.compute(builder, operand)?;
                let dst = builder.value(builder.types[operand.0 as usize]);
                builder.push(Inst::Neg { dst, operand });
                dst
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::ir::VReg;
    use crate::span::{Pos, Span};

    fn list(n: u32) -> Box<ir::Kernel> {
        Box::new(ir::Kernel::Element(Operand::Reg(VReg(n))))
    }

    fn number(bits: i64) -> Box<ir::Kernel> {
        Box::new(ir::Kernel::Number(Operand::Imm(bits)))
    }

    fn binary(op: BinOp, lhs: Box<ir::Kernel>, rhs: Box<ir::Kernel>) -> Box<ir::Kernel> {
        Box::new(ir::Kernel::Binary(op, lhs, rhs))
    }

    fn dump(kernel: Box<ir::Kernel>, float: bool) -> String {
        let mut kernel = translate(&kernel, float).unwrap();
        kernel.name = "stone_k0".to_string();
        kernel.to_string()
    }

    #[test]
    fn a_float_kernel_loads_each_list_and_keeps_constants_out_of_the_loop() {
        // a * 2.0 + b
        let kernel = binary(
            BinOp::Add,
            binary(BinOp::Mul, list(1), number(2.0f64.to_bits() as i64)),
            list(2),
        );
        assert_eq!(
            dump(kernel, true),
            "\
kernel stone_k0(ptr, i64, ptr, ptr):
b0:
  v0 = param 0
  v1 = param 1
  v2 = param 2
  v3 = param 3
  v4 = const 2.0
  v5 = global_index
  v6 = grid_size
  jmp b1
b1:
  v7 = cmp_lt v5, v1
  br v7 -> b3, b2
b2:
  ret
b3:
  v8 = load v2[v5]
  v9 = fmul v8, v4
  v10 = load v3[v5]
  v11 = fadd v9, v10
  store v0[v5], v11
  v5 = add v5, v6
  jmp b1
"
        );
    }

    #[test]
    fn an_int_kernel_takes_a_number_in_a_register_as_a_parameter() {
        // a - b * n, then -a
        let kernel = binary(
            BinOp::Sub,
            list(1),
            binary(
                BinOp::Mul,
                list(2),
                Box::new(ir::Kernel::Number(Operand::Reg(VReg(7)))),
            ),
        );
        assert_eq!(
            dump(kernel, false),
            "\
kernel stone_k0(ptr, i64, ptr, ptr, i64):
b0:
  v0 = param 0
  v1 = param 1
  v2 = param 2
  v3 = param 3
  v4 = param 4
  v5 = global_index
  v6 = grid_size
  jmp b1
b1:
  v7 = cmp_lt v5, v1
  br v7 -> b3, b2
b2:
  ret
b3:
  v8 = load v2[v5]
  v9 = load v3[v5]
  v10 = mul v9, v4
  v11 = sub v8, v10
  store v0[v5], v11
  v5 = add v5, v6
  jmp b1
"
        );
        let negated = dump(Box::new(ir::Kernel::Neg(list(1))), false);
        assert!(
            negated.contains("  v6 = load v2[v3]\n  v7 = neg v6\n"),
            "{negated}"
        );
    }

    #[test]
    fn a_list_used_twice_is_one_parameter_loaded_once() {
        let kernel = dump(binary(BinOp::Mul, list(1), list(1)), true);
        assert!(
            kernel.starts_with("kernel stone_k0(ptr, i64, ptr):"),
            "{kernel}"
        );
        assert!(
            kernel.contains("  v6 = load v2[v3]\n  v7 = fmul v6, v6\n"),
            "{kernel}"
        );
    }

    #[test]
    fn a_float_division_sets_the_flag_for_a_zero_divisor() {
        assert_eq!(
            dump(binary(BinOp::Div, list(1), list(2)), true),
            "\
kernel stone_k0(ptr, i64, ptr, ptr, ptr):
b0:
  v0 = param 0
  v1 = param 1
  v2 = param 2
  v3 = param 3
  v4 = param 4
  v5 = const 0.0
  v6 = const 0
  v7 = const 1
  v8 = global_index
  v9 = grid_size
  jmp b1
b1:
  v10 = cmp_lt v8, v1
  br v10 -> b3, b2
b2:
  ret
b3:
  v11 = load v2[v8]
  v12 = load v3[v8]
  v13 = fcmp_eq v12, v5
  br v13 -> b4, b5
b4:
  store v4[v6], v7
  jmp b5
b5:
  v14 = fdiv v11, v12
  store v0[v8], v14
  v8 = add v8, v9
  jmp b1
"
        );
    }

    #[test]
    fn operations_a_gpu_cannot_compute_yet_are_errors() {
        for (op, float) in [(BinOp::Div, false), (BinOp::Rem, false), (BinOp::Pow, true)] {
            let kernel = binary(op, list(1), list(2));
            assert!(translate(&kernel, float).is_err(), "{op:?}");
        }
    }

    #[test]
    fn the_same_arithmetic_at_two_sites_is_one_kernel() {
        let site = |line| Span::new(Pos::new(line, 5), Pos::new(line, 16));
        let record = |line, first, constant: f64| DeviceKernel {
            span: site(line),
            float: true,
            kernel: *binary(BinOp::Mul, list(first), number(constant.to_bits() as i64)),
        };
        let kernels = kernels(&[
            record(1, 3, 2.0),
            record(2, 9, 2.0),
            record(3, 3, 4.0),
            record(1, 3, 2.0),
        ])
        .unwrap();
        let names: Vec<(&str, &[Span])> = kernels
            .iter()
            .map(|kernel| (kernel.name.as_str(), kernel.sites.as_slice()))
            .collect();
        assert_eq!(
            names,
            [
                ("stone_k0", &[site(1), site(2)][..]),
                ("stone_k1", &[site(3)][..])
            ]
        );
    }

    #[test]
    fn each_record_maps_to_the_kernel_that_computes_it() {
        let site = |line| Span::new(Pos::new(line, 5), Pos::new(line, 16));
        let record = |line, first, constant: f64| DeviceKernel {
            span: site(line),
            float: true,
            kernel: *binary(BinOp::Mul, list(first), number(constant.to_bits() as i64)),
        };
        let numbered = numbered(
            &[
                record(1, 3, 2.0),
                record(2, 9, 2.0),
                record(3, 3, 4.0),
                record(4, 3, 2.0),
            ],
            Target::X64_LINUX,
        )
        .unwrap();
        assert_eq!(numbered.kernels.len(), 2);
        assert_eq!(numbered.record_kernel, [0, 0, 1, 0]);
        // out, length, and one list each; a multiplication has no flag
        let shape = Shape {
            float: true,
            fail_flag: false,
            params: 3,
            lists: 0b101,
            cost: numbered.shapes[0].cost,
        };
        assert_eq!(numbered.shapes, [shape, shape]);
        // one list in and the result out
        assert_eq!((shape.cost.in_bytes, shape.cost.out_bytes), (8, 8));
    }

    #[test]
    fn a_shape_marks_the_parameters_that_are_lists() {
        let site = Span::new(Pos::new(1, 5), Pos::new(1, 16));
        // a * x + b, then a / x, which takes the flag last
        let x = Box::new(ir::Kernel::Number(Operand::Reg(VReg(7))));
        let record = |kernel: Box<ir::Kernel>| DeviceKernel {
            span: site,
            float: true,
            kernel: *kernel,
        };
        let numbered = numbered(
            &[
                record(binary(
                    BinOp::Add,
                    binary(BinOp::Mul, list(1), x.clone()),
                    list(2),
                )),
                record(binary(BinOp::Div, list(1), x)),
            ],
            Target::X64_LINUX,
        )
        .unwrap();
        // out, length, a, x, b
        assert_eq!(numbered.shapes[0].params, 5);
        assert_eq!(numbered.shapes[0].lists, 0b10101);
        // a and b in, the result out
        assert_eq!(numbered.shapes[0].cost.in_bytes, 16);
        assert_eq!(numbered.shapes[1].cost.in_bytes, 8);
        // out, length, a, x, flag
        assert_eq!(numbered.shapes[1].params, 5);
        assert!(numbered.shapes[1].fail_flag);
        assert_eq!(numbered.shapes[1].lists, 0b101);
    }

    #[test]
    fn a_kernel_is_a_postfix_program_for_a_simulated_gpu() {
        // a * 2.0 + b * x, with x a number in a register, then -a
        let two = 2.0f64.to_bits() as i64;
        let x = Box::new(ir::Kernel::Number(Operand::Reg(VReg(7))));
        let kernel = binary(
            BinOp::Add,
            binary(BinOp::Mul, list(1), number(two)),
            binary(BinOp::Mul, list(2), x),
        );
        let mut expected = vec![op::ELEMENT, 2, op::CONSTANT];
        expected.extend(two.to_le_bytes());
        expected.extend([op::MUL, op::ELEMENT, 3, op::NUMBER, 4, op::MUL, op::ADD]);
        assert_eq!(program(&kernel), expected);
        let negated = Box::new(ir::Kernel::Neg(list(1)));
        assert_eq!(program(&negated), [op::ELEMENT, 2, op::NEG]);
        // a list read twice is one parameter
        let square = binary(BinOp::Mul, list(1), list(1));
        assert_eq!(program(&square), [op::ELEMENT, 2, op::ELEMENT, 2, op::MUL]);
    }
}
