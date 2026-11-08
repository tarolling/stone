//! The table of machine code a GPU build embeds in the executable.
//!
//! A program built with `--gpu` carries, in `.rodata` under the label `stone.gpu_image`, one blob
//! that holds the code of every kernel for every GPU it was built for, and what a launch needs to
//! know about each. The blob refers to itself by offsets from its start, so it holds no address
//! and is the same on every processor and system. Everything is little-endian, and every part
//! starts on a multiple of 8 bytes, except machine code and cubins, which start on a multiple of
//! 128.
//!
//! ```text
//! header   (32 bytes)  "STONEGPU", version u32, devices u32, records u32, kernels u32, size u64
//! records  (4 each)    the kernel that computes each record, padded to 8 bytes
//! shapes   (16 each)   per kernel: params u32, flags u32, lists u64
//! devices  (48 each)   sm u32, specific u32, offset of the device's kernels u64 (0 for none),
//!                      cubin offset u64, cubin size u64, PTX offset u64, PTX size u64
//! kernels  (32 each)   per device, in kernel order: code offset u64, code size u64, registers,
//!                      param_offset, param_size, shared (all u32)
//! code                 the machine code of each kernel, each cubin, and each PTX text
//! ```
//!
//! A record is a piece of arithmetic on lists, as `Program::device_kernels` numbers them, which
//! is the number `stone.gpu_launch` is called with. A shape's flags bit 0 says the kernel takes
//! the address of a flag it sets when a divisor is zero as its last parameter, and bit 1 that it
//! computes floats; bit `i` of its lists says parameter `i` is the address of a list's elements.
//!
//! What a device entry carries depends on the system ([`Carry`]): a Linux program carries each
//! kernel's machine code (SASS) and the facts a launch needs to place it, and a Windows program,
//! whose GPU driver loads modules itself, carries the whole cubin (when `ptxas` made one) and the
//! PTX, ended by a 0 byte, which the driver compiles when no cubin suits the GPU. A size of 0
//! means the entry has no such part.

use super::cubin::CubinKernel;
use super::kernel::Shape;
use crate::codegen::context::escape_bytes;
use crate::codegen::device::Device;

/// The first bytes of an image.
pub const MAGIC: &[u8; 8] = b"STONEGPU";
/// The version of the layout above.
pub const VERSION: u32 = 2;
/// The label of the image in the executable.
pub const LABEL: &str = "stone.gpu_image";

/// The size of the header.
pub const HEADER_SIZE: usize = 32;
/// The size of a shape.
pub const SHAPE_SIZE: usize = 16;
/// The size of a device entry.
pub const DEVICE_SIZE: usize = 48;
/// The size of a kernel entry.
pub const KERNEL_SIZE: usize = 32;
const CODE_ALIGNMENT: usize = 128;

/// The flag of a shape whose kernel takes a fail flag last.
pub const FLAG_FAIL: u32 = 1;
/// The flag of a shape whose kernel computes floats.
pub const FLAG_FLOAT: u32 = 2;

/// What an image carries for each device.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Carry {
    /// Each kernel's machine code and where its parameters go, for a program that places the
    /// code itself (Linux). Every device needs a cubin.
    Sass,
    /// The whole cubin, if any, and the PTX, for a program whose driver loads them (Windows).
    Modules,
}

/// A cubin as `ptxas` wrote it, with its kernels read out.
pub struct Cubin {
    pub bytes: Vec<u8>,
    pub kernels: Vec<CubinKernel>,
}

/// The code a GPU build embeds for one device: its PTX and the cubin `ptxas` made of it, if it
/// ran.
pub struct DeviceCode {
    pub device: Device,
    pub cubin: Option<Cubin>,
    pub ptx: String,
}

/// A kernel's machine code as an image holds it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KernelImage {
    pub sass: Vec<u8>,
    pub registers: u32,
    pub param_offset: u32,
    pub param_size: u32,
    pub shared: u32,
}

/// The code of an image that is built for one GPU.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DeviceImage {
    /// The compute capability times 10, such as 75.
    pub sm: u32,
    /// Whether the code may use features that later GPUs lack (`sm_90a`).
    pub specific: bool,
    /// The kernels' machine code, indexed like the records' kernel numbers, or none.
    pub kernels: Vec<KernelImage>,
    /// The whole cubin, or nothing.
    pub cubin: Vec<u8>,
    /// The PTX text without its 0 byte, or nothing.
    pub ptx: Vec<u8>,
}

/// An image read back.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Image {
    pub record_kernel: Vec<u32>,
    pub shapes: Vec<Shape>,
    pub devices: Vec<DeviceImage>,
}

/// What a GPU loads of a device entry: its cubin or its PTX.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Module {
    Cubin,
    Ptx,
}

/// Returns the image of `devices`' code for a program whose records are computed by the
/// kernels `record_kernel` names, the kernels having `shapes`, carrying what `carry` says.
/// Devices are sorted by compute capability, and each cubin must have every kernel, named
/// `stone_k0`, `stone_k1`, ... as [`kernels`](super::kernels) names them, and with as many
/// parameters as the shape says.
///
/// For example, a program with two kernels built for `sm_75` gives an image of one device whose
/// two kernels hold the cubin's code under [`Carry::Sass`], or that holds the cubin and the PTX
/// under [`Carry::Modules`].
pub fn build(
    record_kernel: &[usize],
    shapes: &[Shape],
    devices: &[DeviceCode],
    carry: Carry,
) -> Result<Vec<u8>, String> {
    if record_kernel.iter().any(|&kernel| kernel >= shapes.len()) {
        return Err("a record names a kernel that does not exist".into());
    }
    struct Ordered<'a> {
        sm: u32,
        specific: bool,
        kernels: Vec<&'a CubinKernel>,
        cubin: &'a [u8],
        ptx: &'a str,
    }
    let mut ordered: Vec<Ordered> = Vec::new();
    for code in devices {
        let (sm, specific) = match code.device {
            Device::Nvptx { sm, specific } => (u32::from(sm), specific),
            device @ Device::Amdgcn { .. } => {
                return Err(format!("{device} has no image format yet"));
            }
        };
        let mut kernels = Vec::new();
        if let Some(cubin) = &code.cubin {
            for (index, shape) in shapes.iter().enumerate() {
                let name = format!("stone_k{index}");
                let kernel = cubin
                    .kernels
                    .iter()
                    .find(|kernel| kernel.name == name)
                    .ok_or_else(|| {
                        format!("the cubin for {} has no kernel '{name}'", code.device)
                    })?;
                if kernel.param_size != shape.params * 8 {
                    return Err(format!(
                        "kernel '{name}' for {} takes {} bytes of parameters, not {}",
                        code.device,
                        kernel.param_size,
                        shape.params * 8
                    ));
                }
                kernels.push(kernel);
            }
        } else if carry == Carry::Sass {
            return Err(format!("there is no cubin for {}", code.device));
        }
        let (cubin, ptx) = match (carry, &code.cubin) {
            (Carry::Sass, _) => (&[][..], ""),
            (Carry::Modules, cubin) => (
                cubin.as_ref().map_or(&[][..], |cubin| &cubin.bytes[..]),
                &code.ptx[..],
            ),
        };
        if carry == Carry::Modules {
            kernels.clear();
        }
        ordered.push(Ordered {
            sm,
            specific,
            kernels,
            cubin,
            ptx,
        });
    }
    ordered.sort_by_key(|device| (device.sm, device.specific));

    let shapes_at = align(HEADER_SIZE + 4 * record_kernel.len(), 8);
    let devices_at = shapes_at + SHAPE_SIZE * shapes.len();
    let tables = devices_at + DEVICE_SIZE * ordered.len();
    let table_size = KERNEL_SIZE * shapes.len();
    let carried = ordered.iter().filter(|device| !device.kernels.is_empty());
    let code_start = align(tables + table_size * carried.count(), CODE_ALIGNMENT);

    let mut bytes = vec![0u8; code_start];
    for (index, shape) in shapes.iter().enumerate() {
        let at = shapes_at + SHAPE_SIZE * index;
        put_u32(&mut bytes, at, shape.params);
        let flags =
            if shape.fail_flag { FLAG_FAIL } else { 0 } | if shape.float { FLAG_FLOAT } else { 0 };
        put_u32(&mut bytes, at + 4, flags);
        put_u64(&mut bytes, at + 8, shape.lists);
    }
    let mut code = Vec::new();
    let mut table_at = tables;
    for (index, device) in ordered.iter().enumerate() {
        let device_at = devices_at + DEVICE_SIZE * index;
        put_u32(&mut bytes, device_at, device.sm);
        put_u32(&mut bytes, device_at + 4, u32::from(device.specific));
        if !device.kernels.is_empty() {
            put_u64(&mut bytes, device_at + 8, table_at as u64);
            for (kernel_index, kernel) in device.kernels.iter().enumerate() {
                let at = table_at + KERNEL_SIZE * kernel_index;
                put_u64(&mut bytes, at, (code_start + code.len()) as u64);
                put_u64(&mut bytes, at + 8, kernel.sass.len() as u64);
                put_u32(&mut bytes, at + 16, kernel.registers);
                put_u32(&mut bytes, at + 20, kernel.param_offset);
                put_u32(&mut bytes, at + 24, kernel.param_size);
                put_u32(&mut bytes, at + 28, kernel.shared);
                code.extend_from_slice(&kernel.sass);
                code.resize(align(code.len(), CODE_ALIGNMENT), 0);
            }
            table_at += table_size;
        }
        if !device.cubin.is_empty() {
            put_u64(&mut bytes, device_at + 16, (code_start + code.len()) as u64);
            put_u64(&mut bytes, device_at + 24, device.cubin.len() as u64);
            code.extend_from_slice(device.cubin);
            code.resize(align(code.len(), CODE_ALIGNMENT), 0);
        }
        if !device.ptx.is_empty() {
            if device.ptx.as_bytes().contains(&0) {
                return Err(format!("the PTX for sm_{} holds a 0 byte", device.sm));
            }
            put_u64(&mut bytes, device_at + 32, (code_start + code.len()) as u64);
            put_u64(&mut bytes, device_at + 40, device.ptx.len() as u64);
            code.extend_from_slice(device.ptx.as_bytes());
            code.push(0);
            code.resize(align(code.len(), 8), 0);
        }
    }
    bytes[..8].copy_from_slice(MAGIC);
    put_u32(&mut bytes, 8, VERSION);
    put_u32(&mut bytes, 12, ordered.len() as u32);
    put_u32(&mut bytes, 16, record_kernel.len() as u32);
    put_u32(&mut bytes, 20, shapes.len() as u32);
    for (index, &kernel) in record_kernel.iter().enumerate() {
        put_u32(&mut bytes, HEADER_SIZE + 4 * index, kernel as u32);
    }
    bytes.extend_from_slice(&code);
    let size = align(bytes.len(), 8);
    bytes.resize(size, 0);
    put_u64(&mut bytes, 24, size as u64);
    Ok(bytes)
}

/// Reads an image back, checking every offset.
///
/// For example, `parse(&build(..)?)` gives the records' kernels, the kernels' shapes, and, for
/// each device, the code it carries. Bytes that do not start with [`MAGIC`] are the error
/// `not a GPU image: ...`.
pub fn parse(bytes: &[u8]) -> Result<Image, String> {
    let bad = |reason: &str| format!("not a GPU image: {reason}");
    if bytes.get(..8) != Some(MAGIC) {
        return Err(bad("wrong magic"));
    }
    // the image may be followed by bytes of its section, such as the terminator of a string
    let size = bytes
        .get(24..32)
        .map(|slice| u64::from_le_bytes(slice.try_into().unwrap()))
        .ok_or_else(|| bad("truncated"))?;
    let bytes = usize::try_from(size)
        .ok()
        .and_then(|size| bytes.get(..size))
        .ok_or_else(|| bad("the size does not match"))?;
    let word = |at: usize| -> Result<u32, String> {
        let slice = bytes.get(at..at + 4).ok_or_else(|| bad("truncated"))?;
        Ok(u32::from_le_bytes(slice.try_into().unwrap()))
    };
    let long = |at: usize| -> Result<u64, String> {
        let slice = bytes.get(at..at + 8).ok_or_else(|| bad("truncated"))?;
        Ok(u64::from_le_bytes(slice.try_into().unwrap()))
    };
    let offset = |at: usize| -> Result<usize, String> {
        usize::try_from(long(at)?).map_err(|_| bad("an offset is too large"))
    };
    let range = |at: usize| -> Result<&[u8], String> {
        let (start, size) = (offset(at)?, offset(at + 8)?);
        let end = start
            .checked_add(size)
            .ok_or_else(|| bad("bad code range"))?;
        bytes.get(start..end).ok_or_else(|| bad("code is cut off"))
    };
    if word(8)? != VERSION {
        return Err(bad("unknown version"));
    }
    let (devices, records, kernels) = (word(12)? as usize, word(16)? as usize, word(20)? as usize);
    let mut record_kernel = Vec::new();
    for index in 0..records {
        let kernel = word(HEADER_SIZE + 4 * index)?;
        if kernel as usize >= kernels {
            return Err(bad("a record names a missing kernel"));
        }
        record_kernel.push(kernel);
    }
    let shapes_at = align(HEADER_SIZE + 4 * records, 8);
    let mut shapes = Vec::new();
    for index in 0..kernels {
        let at = shapes_at + SHAPE_SIZE * index;
        let flags = word(at + 4)?;
        shapes.push(Shape {
            params: word(at)?,
            fail_flag: flags & FLAG_FAIL != 0,
            float: flags & FLAG_FLOAT != 0,
            lists: long(at + 8)?,
        });
    }
    let devices_at = shapes_at + SHAPE_SIZE * kernels;
    let mut images = Vec::new();
    for index in 0..devices {
        let at = devices_at + DEVICE_SIZE * index;
        let table = offset(at + 8)?;
        let mut device_kernels = Vec::new();
        if table != 0 {
            for kernel in 0..kernels {
                let at = table + KERNEL_SIZE * kernel;
                device_kernels.push(KernelImage {
                    sass: range(at)?.to_vec(),
                    registers: word(at + 16)?,
                    param_offset: word(at + 20)?,
                    param_size: word(at + 24)?,
                    shared: word(at + 28)?,
                });
            }
        }
        let ptx = range(at + 32)?;
        if !ptx.is_empty() && bytes.get(offset(at + 32)? + ptx.len()) != Some(&0) {
            return Err(bad("the PTX has no 0 byte after it"));
        }
        images.push(DeviceImage {
            sm: word(at)?,
            specific: word(at + 4)? != 0,
            kernels: device_kernels,
            cubin: range(at + 16)?.to_vec(),
            ptx: ptx.to_vec(),
        });
    }
    Ok(Image {
        record_kernel,
        shapes,
        devices: images,
    })
}

/// Returns the index in `devices` of the machine code for a GPU of compute capability
/// `major`.`minor`: the newest code of the same major version that is not newer than the GPU.
/// Code for a specific device (`sm_90a`) runs only on a GPU of exactly that capability.
///
/// For example, with code for 7.0, 7.5, and 8.0, a 7.2 GPU gets the 7.0 code, a 7.5 GPU the 7.5
/// code, and a 6.1 or 9.0 GPU none.
pub fn select(devices: &[DeviceImage], major: u32, minor: u32) -> Option<usize> {
    newest(devices, major * 10 + minor, |device| {
        device.sm / 10 == major
    })
}

/// Returns which device entry, and which part of it, a GPU driver should load for a GPU of
/// compute capability `major`.`minor`: a cubin by the rule of [`select`], else the PTX of the
/// newest device that is not newer than the GPU, which the driver compiles for it. PTX for a
/// specific device (`sm_90a`) compiles only for exactly that capability.
///
/// For example, with a cubin and PTX for 7.5, a 7.5 GPU gets the cubin and an 8.9 GPU the PTX,
/// and a 6.1 GPU nothing.
pub fn select_module(devices: &[DeviceImage], major: u32, minor: u32) -> Option<(usize, Module)> {
    let gpu = major * 10 + minor;
    newest(devices, gpu, |device| {
        device.sm / 10 == major && !device.cubin.is_empty()
    })
    .map(|index| (index, Module::Cubin))
    .or_else(|| {
        newest(devices, gpu, |device| !device.ptx.is_empty()).map(|index| (index, Module::Ptx))
    })
}

/// Returns the index of the newest of `devices` that `fits` and that runs on a GPU of
/// capability `gpu` (times 10): not newer than it, and exactly it if specific.
fn newest(devices: &[DeviceImage], gpu: u32, fits: impl Fn(&DeviceImage) -> bool) -> Option<usize> {
    devices
        .iter()
        .enumerate()
        .filter(|(_, device)| {
            fits(device) && device.sm <= gpu && (!device.specific || device.sm == gpu)
        })
        .max_by_key(|(_, device)| (device.sm, device.specific))
        .map(|(index, _)| index)
}

/// Returns assembly text that places `image` in `.rodata` under [`LABEL`].
///
/// For example, an image of 64 bytes becomes a `.section .rodata` directive, an alignment of 128,
/// the label, and one `.string` of escaped bytes.
pub fn assembly(image: &[u8]) -> String {
    format!(
        "\t.section .rodata\n\t.balign\t{CODE_ALIGNMENT}\n{LABEL}:\n\t.string \"{}\"\n",
        escape_bytes(image)
    )
}

fn align(value: usize, to: usize) -> usize {
    value.div_ceil(to) * to
}

fn put_u32(bytes: &mut [u8], at: usize, value: u32) {
    bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
}

fn put_u64(bytes: &mut [u8], at: usize, value: u64) {
    bytes[at..at + 8].copy_from_slice(&value.to_le_bytes());
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::Architecture;
    use crate::codegen::asm;
    use crate::codegen::gpu::cubin;

    const TWO_KERNELS: &[u8] = include_bytes!("testdata/two_kernels.sm_75.cubin");

    fn device(name: &str) -> Device {
        name.parse().unwrap()
    }

    /// `stone_k0` is `a * 2.0 + b` and `stone_k1` divides, as the fixture's PTX has them.
    fn shapes() -> Vec<Shape> {
        vec![
            Shape {
                float: true,
                fail_flag: false,
                params: 4,
                lists: 0b1101,
            },
            Shape {
                float: true,
                fail_flag: true,
                params: 5,
                lists: 0b1101,
            },
        ]
    }

    fn code(name: &str) -> DeviceCode {
        DeviceCode {
            device: device(name),
            cubin: Some(Cubin {
                bytes: TWO_KERNELS.to_vec(),
                kernels: cubin::parse(TWO_KERNELS).unwrap(),
            }),
            ptx: format!(".version 6.3\n.target {name}\n"),
        }
    }

    fn ptx_only(name: &str) -> DeviceCode {
        DeviceCode {
            cubin: None,
            ..code(name)
        }
    }

    /// Returns where the first device's table of kernels starts in an image of `records`
    /// records and two kernels.
    fn first_table(bytes: &[u8], records: usize) -> usize {
        let devices_at = align(HEADER_SIZE + 4 * records, 8) + SHAPE_SIZE * 2;
        u64::from_le_bytes(bytes[devices_at + 8..devices_at + 16].try_into().unwrap()) as usize
    }

    #[test]
    fn an_image_gives_back_each_kernels_code_and_launch_details() {
        let bytes = build(&[1, 0, 0], &shapes(), &[code("sm_75")], Carry::Sass).unwrap();
        assert_eq!(bytes.len() % 8, 0);
        let image = parse(&bytes).unwrap();
        assert_eq!(image.record_kernel, [1, 0, 0]);
        assert_eq!(image.shapes, shapes());
        assert_eq!(image.devices.len(), 1);
        let device = &image.devices[0];
        assert_eq!((device.sm, device.specific), (75, false));
        let kernels = cubin::parse(TWO_KERNELS).unwrap();
        let by_name = |name: &str| kernels.iter().find(|kernel| kernel.name == name).unwrap();
        let (k0, k1) = (by_name("stone_k0"), by_name("stone_k1"));
        assert_eq!(device.kernels[0].sass, k0.sass);
        assert_eq!(device.kernels[1].sass, k1.sass);
        assert_eq!(device.kernels[0].registers, 18);
        assert_eq!(device.kernels[1].registers, 26);
        assert_eq!(device.kernels[0].param_offset, 0x160);
        assert_eq!(device.kernels[0].param_size, 32);
        assert_eq!(device.kernels[1].param_size, 40);
        // a program that places machine code itself carries no module
        assert!(device.cubin.is_empty() && device.ptx.is_empty());
    }

    #[test]
    fn a_program_whose_driver_loads_modules_carries_the_cubin_and_the_ptx() {
        let bytes = build(&[1, 0], &shapes(), &[code("sm_75")], Carry::Modules).unwrap();
        let image = parse(&bytes).unwrap();
        assert_eq!(image.shapes, shapes());
        let device = &image.devices[0];
        assert!(device.kernels.is_empty());
        assert_eq!(device.cubin, TWO_KERNELS);
        assert_eq!(device.ptx, b".version 6.3\n.target sm_75\n");
        // the cubin starts on a 128-byte boundary and the PTX ends with a 0 byte
        let start = bytes
            .windows(TWO_KERNELS.len())
            .position(|window| window == TWO_KERNELS)
            .unwrap();
        assert_eq!(start % 128, 0);
        let ptx = bytes
            .windows(7)
            .position(|window| window == b".target")
            .unwrap();
        let end = ptx + bytes[ptx..].iter().position(|&byte| byte == 0).unwrap();
        assert_eq!(&bytes[end - 6..end], b"sm_75\n");
    }

    #[test]
    fn a_device_without_a_cubin_carries_its_ptx_alone() {
        let bytes = build(
            &[0],
            &shapes(),
            &[ptx_only("sm_86"), code("sm_75")],
            Carry::Modules,
        )
        .unwrap();
        let image = parse(&bytes).unwrap();
        assert_eq!(image.devices.len(), 2);
        assert_eq!(image.devices[1].sm, 86);
        assert!(image.devices[1].cubin.is_empty());
        assert_eq!(image.devices[1].ptx, b".version 6.3\n.target sm_86\n");
        // a program that places machine code itself needs every cubin
        let error = build(&[0], &shapes(), &[ptx_only("sm_86")], Carry::Sass).unwrap_err();
        assert_eq!(error, "there is no cubin for sm_86");
    }

    #[test]
    fn machine_code_starts_on_a_128_byte_boundary() {
        let bytes = build(
            &[0],
            &shapes(),
            &[code("sm_75"), code("sm_90")],
            Carry::Sass,
        )
        .unwrap();
        // every code offset in every kernel entry is a multiple of 128
        let image = parse(&bytes).unwrap();
        assert_eq!(image.devices.len(), 2);
        let table = first_table(&bytes, 1);
        for kernel in 0..2 {
            let at = table + KERNEL_SIZE * kernel;
            let offset = u64::from_le_bytes(bytes[at..at + 8].try_into().unwrap());
            assert_eq!(offset % 128, 0);
        }
    }

    #[test]
    fn devices_are_sorted_by_capability() {
        let bytes = build(
            &[],
            &shapes(),
            &[code("sm_90"), code("sm_75"), code("sm_90a")],
            Carry::Sass,
        )
        .unwrap();
        let image = parse(&bytes).unwrap();
        let order: Vec<(u32, bool)> = image.devices.iter().map(|d| (d.sm, d.specific)).collect();
        assert_eq!(order, [(75, false), (90, false), (90, true)]);
    }

    #[test]
    fn a_kernel_the_cubin_lacks_or_with_other_parameters_is_an_error() {
        let mut shapes = shapes();
        shapes.push(shapes[0]);
        let error = build(&[0], &shapes, &[code("sm_75")], Carry::Sass).unwrap_err();
        assert_eq!(error, "the cubin for sm_75 has no kernel 'stone_k2'");
        let error = build(&[0], &shapes, &[code("sm_75")], Carry::Modules).unwrap_err();
        assert_eq!(error, "the cubin for sm_75 has no kernel 'stone_k2'");
        let mut shapes = self::shapes();
        shapes[0].params = 3;
        let error = build(&[0], &shapes, &[code("sm_75")], Carry::Sass).unwrap_err();
        assert!(
            error.starts_with("kernel 'stone_k0' for sm_75 takes 32 bytes"),
            "{error}"
        );
        let error = build(&[2], &self::shapes(), &[code("sm_75")], Carry::Sass).unwrap_err();
        assert!(error.contains("a kernel that does not exist"), "{error}");
        let error = build(&[0], &self::shapes(), &[code("gfx1100")], Carry::Sass).unwrap_err();
        assert!(error.contains("no image format"), "{error}");
    }

    #[test]
    fn a_damaged_image_is_an_error() {
        let bytes = build(&[0, 1], &shapes(), &[code("sm_75")], Carry::Sass).unwrap();
        assert!(parse(b"").unwrap_err().starts_with("not a GPU image"));
        assert!(parse(&[0; 64]).unwrap_err().starts_with("not a GPU image"));
        let mut cut = bytes.clone();
        cut.truncate(bytes.len() - 8);
        assert!(parse(&cut).unwrap_err().starts_with("not a GPU image"));
        // a record that names a kernel past the last
        let mut wrong = bytes.clone();
        wrong[HEADER_SIZE] = 9;
        assert!(parse(&wrong).unwrap_err().contains("missing kernel"));
        // a code range outside the image
        let mut far = bytes.clone();
        let at = first_table(&bytes, 2) + 8;
        far[at..at + 8].copy_from_slice(&u64::MAX.to_le_bytes());
        assert!(parse(&far).unwrap_err().starts_with("not a GPU image"));
        // an older layout
        let mut old = bytes.clone();
        old[8] = 1;
        assert!(parse(&old).unwrap_err().contains("unknown version"));
        // trailing bytes after the image are fine
        let mut longer = bytes.clone();
        longer.push(0);
        assert!(parse(&longer).is_ok());
    }

    fn device_image(sm: u32, specific: bool) -> DeviceImage {
        DeviceImage {
            sm,
            specific,
            kernels: Vec::new(),
            cubin: b"cubin".to_vec(),
            ptx: b"ptx".to_vec(),
        }
    }

    fn ptx_image(sm: u32, specific: bool) -> DeviceImage {
        DeviceImage {
            cubin: Vec::new(),
            ..device_image(sm, specific)
        }
    }

    #[test]
    fn code_runs_on_the_newest_gpu_of_its_major_version_that_is_not_older() {
        let devices = [
            device_image(70, false),
            device_image(75, false),
            device_image(80, false),
        ];
        assert_eq!(select(&devices, 7, 2), Some(0));
        assert_eq!(select(&devices, 7, 5), Some(1));
        assert_eq!(select(&devices, 7, 9), Some(1));
        assert_eq!(select(&devices, 8, 6), Some(2));
        assert_eq!(select(&devices, 6, 1), None);
        assert_eq!(select(&devices, 9, 0), None);
        assert_eq!(select(&[], 7, 5), None);
    }

    #[test]
    fn code_for_a_specific_device_runs_only_on_that_capability() {
        let devices = [device_image(90, false), device_image(90, true)];
        assert_eq!(select(&devices, 9, 0), Some(1));
        let only = [device_image(90, true)];
        assert_eq!(select(&only, 9, 0), Some(0));
        assert_eq!(select(&only, 9, 2), None);
        let mixed = [device_image(90, true), device_image(90, false)];
        assert_eq!(select(&mixed, 9, 2), Some(1));
    }

    #[test]
    fn a_driver_loads_a_fitting_cubin_and_else_the_newest_ptx_not_newer_than_the_gpu() {
        let devices = [device_image(70, false), device_image(75, false)];
        assert_eq!(select_module(&devices, 7, 5), Some((1, Module::Cubin)));
        assert_eq!(select_module(&devices, 7, 2), Some((0, Module::Cubin)));
        // a newer major version compiles the newest PTX
        assert_eq!(select_module(&devices, 8, 9), Some((1, Module::Ptx)));
        assert_eq!(select_module(&devices, 12, 0), Some((1, Module::Ptx)));
        // nothing runs on an older GPU
        assert_eq!(select_module(&devices, 6, 1), None);
        assert_eq!(select_module(&[], 7, 5), None);
    }

    #[test]
    fn a_device_without_a_cubin_gives_its_ptx() {
        let devices = [ptx_image(75, false)];
        assert_eq!(select_module(&devices, 7, 5), Some((0, Module::Ptx)));
        // a cubin of the GPU's major version wins over newer PTX
        let devices = [device_image(80, false), ptx_image(86, false)];
        assert_eq!(select_module(&devices, 8, 6), Some((0, Module::Cubin)));
        assert_eq!(select_module(&devices, 8, 9), Some((0, Module::Cubin)));
        assert_eq!(select_module(&devices, 9, 0), Some((1, Module::Ptx)));
        // specific PTX compiles only for its own capability
        let devices = [ptx_image(75, false), ptx_image(90, true)];
        assert_eq!(select_module(&devices, 9, 0), Some((1, Module::Ptx)));
        assert_eq!(select_module(&devices, 10, 0), Some((0, Module::Ptx)));
    }

    #[test]
    fn the_assembly_places_the_image_in_rodata_unchanged() {
        let bytes = build(&[0, 1], &shapes(), &[code("sm_75")], Carry::Modules).unwrap();
        let text = assembly(&bytes);
        assert!(text.starts_with("\t.section .rodata\n\t.balign\t128\nstone.gpu_image:\n"));
        let object = asm::assemble(
            &format!("\t.text\n_start:\n\tret\n{text}"),
            Architecture::X64,
        )
        .unwrap();
        let label = object
            .symbols
            .iter()
            .find(|symbol| symbol.name == LABEL)
            .expect("the label is defined");
        let at = label.offset as usize;
        // the string adds a terminator after the image
        assert_eq!(&object.rodata[at..at + bytes.len()], &bytes[..]);
        assert_eq!(object.rodata[at + bytes.len()], 0);
        assert_eq!(at % 128, 0);
        assert_eq!(parse(&object.rodata[at..]).unwrap(), parse(&bytes).unwrap());
    }
}
