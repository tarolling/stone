//! The x86-64 Windows runtime that runs kernels on an NVIDIA GPU, through the CUDA driver
//! interface that NVIDIA's display driver installs as `nvcuda.dll`.
//!
//! Nothing imports `nvcuda.dll`, so a program runs on a machine without it: the first launch
//! loads it from the system directory alone (`LoadLibraryExW` with
//! `LOAD_LIBRARY_SEARCH_SYSTEM32`, so no copy beside the program or in the working directory is
//! ever loaded) and finds its functions with `GetProcAddress`. It then makes the first GPU's
//! primary context current, picks the code the image at `stone.gpu_image` holds for the GPU's
//! compute capability by the rule of [`select_module`](crate::codegen::gpu::image::select_module)
//! (a cubin of its major version, else the newest PTX not newer than it, which the driver
//! compiles), loads it, and finds every kernel. If anything fails, or `STONE_GPU` is `off`, it
//! gives up for good and every launch gives 0, so the processor computes everything.
//!
//! A launch copies each list a kernel reads to the GPU, runs the kernel over the whole length
//! with blocks of [`BLOCK`] threads, and waits for it. A kernel that divides takes a flag the GPU
//! sets on a zero divisor: then nothing is copied back and the launch gives 0, so the processor
//! computes the arithmetic again and fails at the interpreter's element. Otherwise the result is
//! copied into the result's elements, which may be an operand's, only now that every element is
//! known, and the launch gives 1.
//!
//! With `STONE_GPU` set to `require`, a launch that would give 0 for any reason but a zero
//! divisor fails instead, saying why (see [`Refusals`]), at the arithmetic, since
//! `stone.gpu_launch` is one of the
//! [`FALLIBLE_ROUTINES`](crate::codegen::context::FALLIBLE_ROUTINES).
//!
//! `stone.gpu_launch` follows the System V convention, as the rest of the runtime does, and
//! calls Windows and CUDA with Microsoft's, from a 16-byte aligned stack with its 32 bytes of
//! shadow space; CUDA's functions are called through `r11`.

use super::gui_windows::{enter, leave};
use crate::codegen::AssemblyGenerator;
use crate::codegen::gpu::image::{DEVICE_SIZE, FLAG_FAIL, HEADER_SIZE, LABEL, SHAPE_SIZE};
use crate::codegen::gpu::kernel::Shape;

/// The functions of `nvcuda.dll` the runtime calls, in the order of their slots at
/// `stone.gpu_api`.
pub const CUDA_FUNCTIONS: [&str; 14] = [
    "cuInit",
    "cuDeviceGet",
    "cuDeviceGetAttribute",
    "cuDevicePrimaryCtxRetain",
    "cuCtxSetCurrent",
    "cuModuleLoadData",
    "cuModuleGetFunction",
    "cuMemAlloc_v2",
    "cuMemFree_v2",
    "cuMemcpyHtoD_v2",
    "cuMemcpyDtoH_v2",
    "cuMemsetD8_v2",
    "cuLaunchKernel",
    "cuCtxSynchronize",
];

/// The threads of each block a kernel is launched with.
pub const BLOCK: i64 = 256;

/// The most blocks a kernel is launched with; its threads loop over the elements beyond.
pub const MAX_BLOCKS: i64 = 65535;

/// `LOAD_LIBRARY_SEARCH_SYSTEM32`, which loads a DLL from the system directory alone.
const SEARCH_SYSTEM32: i64 = 0x800;

/// `CU_DEVICE_ATTRIBUTE_COMPUTE_CAPABILITY_MAJOR`, and the minor version's after it.
const CAPABILITY_MAJOR: i64 = 75;

/// `CUDA_ERROR_NO_DEVICE` and `CUDA_ERROR_INVALID_DEVICE`, which mean there is no GPU to use.
const NO_DEVICE_ERRORS: [i64; 2] = [100, 101];

/// Why the runtime gave up, in `stone.gpu_reason`.
const REASON_NONE: i64 = 0;
const REASON_DRIVER: i64 = 1;
const REASON_DEVICE: i64 = 2;
const REASON_CODE: i64 = 3;
const REASON_CUDA: i64 = 4;

/// The labels of the runtime errors `STONE_GPU=require` gives, from
/// [`Context::fail_label`](crate::codegen::context::Context::fail_label) of the messages in
/// [`gpu`](crate::codegen::gpu): no `nvcuda.dll` (or one without a function the runtime calls),
/// no GPU, no code for the GPU (the major and minor versions in `rcx` and `rdx`), a CUDA error
/// (its number in `rcx`), and a kernel that takes more values than a launch passes.
pub struct Refusals {
    pub driver: String,
    pub device: String,
    pub code: String,
    pub cuda: String,
    pub too_many: String,
}

/// Emits `stone.gpu_launch` and what it keeps: its state in `.bss`, and the names it looks up.
pub fn gpu_runtime(r#gen: &mut dyn AssemblyGenerator, refusals: &Refusals) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    // 0 until the first launch, then 1 if the GPU is ready and 2 if the runtime gave up
    for (label, size) in [
        ("stone.gpu_state", 8),
        ("stone.gpu_require", 8),
        ("stone.gpu_reason", 8),
        ("stone.gpu_detail", 16),
        ("stone.gpu_context", 8),
        ("stone.gpu_module", 8),
        ("stone.gpu_shapes", 8),
        ("stone.gpu_functions", 8),
        ("stone.gpu_api", 8 * CUDA_FUNCTIONS.len()),
    ] {
        r#gen.emit(&format!("{label}:"));
        r#gen.emit(&format!("\t.zero\t{size}"));
    }
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lgpu_names:");
    for name in CUDA_FUNCTIONS {
        r#gen.emit(&format!("\t.string\t\"{name}\""));
    }
    r#gen.emit(".Lgpu_variable:");
    r#gen.emit(&format!("\t.string\t\"{}\"", crate::codegen::gpu::VARIABLE));
    r#gen.emit(".Lgpu_off:");
    r#gen.emit("\t.string\t\"off\"");
    r#gen.emit(".Lgpu_require:");
    r#gen.emit("\t.string\t\"require\"");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit(".Lgpu_library:");
    for quad in wide_quads("nvcuda.dll") {
        r#gen.emit(&format!("\t.quad\t{quad:#018x}"));
    }
    r#gen.emit("\t.text");
    same(r#gen);
    init(r#gen);
    launch(r#gen, refusals);
}

/// Returns `text` in UTF-16 with a 0 after it, as little-endian quads, padded with zeros.
///
/// For example, `wide_quads("ab")` is `[0x0000_0000_0062_0061]`.
fn wide_quads(text: &str) -> Vec<u64> {
    let units: Vec<u16> = text.encode_utf16().chain([0]).collect();
    units
        .chunks(4)
        .map(|chunk| {
            chunk.iter().enumerate().fold(0, |quad, (index, &unit)| {
                quad | u64::from(unit) << (16 * index)
            })
        })
        .collect()
}

/// Emits code that calls the CUDA function `name` with the Microsoft convention, its arguments
/// in place.
fn cuda(r#gen: &mut dyn AssemblyGenerator, name: &str) {
    let slot = CUDA_FUNCTIONS
        .iter()
        .position(|&function| function == name)
        .expect("a CUDA function the runtime looks up");
    r#gen.emit("\tlea\tr11, [rip + stone.gpu_api]");
    r#gen.emit(&format!("\tmov\tr11, QWORD PTR [r11 + {}]", 8 * slot));
    r#gen.emit("\tcall\tr11");
}

/// Emits `.Lgpu_same`, which sets the zero flag if the C strings at `rdi` and `rsi` are equal,
/// changing `rax`, `rcx`, `rdi`, and `rsi`.
fn same(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lgpu_same:");
    r#gen.emit("\tmovzx\teax, BYTE PTR [rdi]");
    r#gen.emit("\tmovzx\tecx, BYTE PTR [rsi]");
    r#gen.emit("\tcmp\teax, ecx");
    r#gen.emit("\tjne\t.Lgpu_same_done");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_same");
    r#gen.emit(".Lgpu_same_done:");
    r#gen.emit("\tret");
}

/// Emits `.Lgpu_init`, which the first launch calls to make the GPU ready, setting
/// `stone.gpu_state` to 1, or to 2 with why in `stone.gpu_reason` and `stone.gpu_detail`. It
/// keeps the registers a Windows call keeps.
fn init(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    // the device goes at [rsp + 32], its capability at [rsp + 40] and [rsp + 44], and a
    // kernel's name at [rsp + 48]
    r#gen.emit(".Lgpu_init:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 96");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_state], 2");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_NONE}"
    ));
    r#gen.emit("\tlea\trdi, [rip + .Lgpu_variable]");
    r#gen.emit("\tcall\tstone.getenv");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lgpu_init_load");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tlea\trsi, [rip + .Lgpu_off]");
    r#gen.emit("\tcall\t.Lgpu_same");
    r#gen.emit("\tje\t.Lgpu_init_done");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tlea\trsi, [rip + .Lgpu_require]");
    r#gen.emit("\tcall\t.Lgpu_same");
    r#gen.emit("\tjne\t.Lgpu_init_load");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_require], 1");

    // the driver and its functions
    r#gen.emit(".Lgpu_init_load:");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_DRIVER}"
    ));
    r#gen.emit("\tlea\trcx, [rip + .Lgpu_library]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(&format!("\tmov\tr8d, {SEARCH_SYSTEM32}"));
    r#gen.emit("\tcall\tLoadLibraryExW");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lgpu_init_done");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tlea\tr12, [rip + .Lgpu_names]");
    r#gen.emit("\tlea\tr13, [rip + stone.gpu_api]");
    r#gen.emit(&format!("\tmov\tr14d, {}", CUDA_FUNCTIONS.len()));
    r#gen.emit(".Lgpu_init_resolve:");
    r#gen.emit("\tmov\trcx, rbx");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\tcall\tGetProcAddress");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lgpu_init_done");
    r#gen.emit("\tmov\tQWORD PTR [r13], rax");
    r#gen.emit("\tadd\tr13, 8");
    // past the name and its 0
    r#gen.emit(".Lgpu_init_skip:");
    r#gen.emit("\tmovzx\teax, BYTE PTR [r12]");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_skip");
    r#gen.emit("\tdec\tr14");
    r#gen.emit("\tjnz\t.Lgpu_init_resolve");

    // the first GPU and its primary context
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_CUDA}"
    ));
    r#gen.emit("\txor\tecx, ecx");
    cuda(r#gen, "cuInit");
    no_device(r#gen);
    r#gen.emit("\tlea\trcx, [rsp + 32]");
    r#gen.emit("\txor\tedx, edx");
    cuda(r#gen, "cuDeviceGet");
    no_device(r#gen);
    for (offset, attribute) in [(40, CAPABILITY_MAJOR), (44, CAPABILITY_MAJOR + 1)] {
        r#gen.emit(&format!("\tlea\trcx, [rsp + {offset}]"));
        r#gen.emit(&format!("\tmov\tedx, {attribute}"));
        r#gen.emit("\tmov\tr8d, DWORD PTR [rsp + 32]");
        cuda(r#gen, "cuDeviceGetAttribute");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit("\tjnz\t.Lgpu_init_cuda");
    }
    r#gen.emit("\tlea\trcx, [rip + stone.gpu_context]");
    r#gen.emit("\tmov\tedx, DWORD PTR [rsp + 32]");
    cuda(r#gen, "cuDevicePrimaryCtxRetain");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_cuda");
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.gpu_context]");
    cuda(r#gen, "cuCtxSetCurrent");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_cuda");

    // the image's shapes and devices: rbx holds the image, r12 the first device, r13 the
    // number of devices, r14 the capability times 10, and r15 the number of kernels
    r#gen.emit(&format!("\tlea\trbx, [rip + {LABEL}]"));
    r#gen.emit("\tmov\teax, DWORD PTR [rbx + 16]");
    r#gen.emit("\tshl\trax, 2");
    r#gen.emit(&format!("\tadd\trax, {}", HEADER_SIZE + 7));
    r#gen.emit("\tand\trax, -8");
    r#gen.emit("\tlea\trcx, [rbx + rax]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_shapes], rcx");
    r#gen.emit("\tmov\tr15d, DWORD PTR [rbx + 20]");
    r#gen.emit("\tmov\trdx, r15");
    r#gen.emit(&format!("\timul\trdx, rdx, {SHAPE_SIZE}"));
    r#gen.emit("\tadd\trax, rdx");
    r#gen.emit("\tlea\tr12, [rbx + rax]");
    r#gen.emit("\tmov\tr13d, DWORD PTR [rbx + 12]");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 40]");
    r#gen.emit("\timul\teax, eax, 10");
    r#gen.emit("\tmov\tedx, eax");
    r#gen.emit("\tadd\teax, DWORD PTR [rsp + 44]");
    r#gen.emit("\tmov\tr14d, eax");
    // devices are sorted by capability, so the last that fits is the newest: first a cubin of
    // the GPU's major version (edx holds it times 10), then any PTX
    pick(r#gen, "cubin", true, 24);
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lgpu_init_ptx");
    r#gen.emit("\tmov\trdx, QWORD PTR [r8 + 16]");
    r#gen.emit("\tjmp\t.Lgpu_init_module");
    r#gen.emit(".Lgpu_init_ptx:");
    pick(r#gen, "ptx", false, 40);
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lgpu_init_no_code");
    r#gen.emit("\tmov\trdx, QWORD PTR [r8 + 32]");
    r#gen.emit(".Lgpu_init_module:");
    r#gen.emit("\tadd\trdx, rbx");
    r#gen.emit("\tlea\trcx, [rip + stone.gpu_module]");
    cuda(r#gen, "cuModuleLoadData");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_cuda");

    // every kernel, by its name, which is written at [rsp + 48]: `stone_k` then its number
    r#gen.emit("\tcall\tGetProcessHeap");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tlea\tr8, [r15 + 1]");
    r#gen.emit("\tshl\tr8, 3");
    r#gen.emit("\tcall\tHeapAlloc");
    r#gen.emit("\ttest\trax, rax");
    // CUDA_ERROR_OUT_OF_MEMORY
    r#gen.emit("\tmov\tecx, 2");
    r#gen.emit("\tcmovz\teax, ecx");
    r#gen.emit("\tjz\t.Lgpu_init_cuda");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_functions], rax");
    r#gen.emit("\tmov\trax, 0x006b5f656e6f7473");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], rax");
    r#gen.emit("\txor\tr12d, r12d");
    r#gen.emit(".Lgpu_init_kernel:");
    r#gen.emit("\tcmp\tr12, r15");
    r#gen.emit("\tjae\t.Lgpu_init_ready");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tmov\tr8d, 10");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lgpu_init_count:");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\tr8");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lgpu_init_count");
    r#gen.emit("\tlea\tr9, [rsp + rcx + 55]");
    r#gen.emit("\tmov\tBYTE PTR [r9], 0");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit(".Lgpu_init_digit:");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\tr8");
    r#gen.emit("\tadd\tedx, 48");
    r#gen.emit("\tdec\tr9");
    r#gen.emit("\tmov\tBYTE PTR [r9], dl");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lgpu_init_digit");
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.gpu_functions]");
    r#gen.emit("\tlea\trcx, [rcx + r12 * 8]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stone.gpu_module]");
    r#gen.emit("\tlea\tr8, [rsp + 48]");
    cuda(r#gen, "cuModuleGetFunction");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_cuda");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\tjmp\t.Lgpu_init_kernel");
    r#gen.emit(".Lgpu_init_ready:");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_state], 1");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_NONE}"
    ));
    r#gen.emit("\tjmp\t.Lgpu_init_done");

    r#gen.emit(".Lgpu_init_no_code:");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_CODE}"
    ));
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 40]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_detail], rax");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 44]");
    r#gen.emit("\tlea\trcx, [rip + stone.gpu_detail]");
    r#gen.emit("\tmov\tQWORD PTR [rcx + 8], rax");
    r#gen.emit("\tjmp\t.Lgpu_init_done");
    r#gen.emit(".Lgpu_init_no_device:");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_DEVICE}"
    ));
    r#gen.emit("\tjmp\t.Lgpu_init_done");
    // a CUDA error, whose number is in eax
    r#gen.emit(".Lgpu_init_cuda:");
    r#gen.emit("\tmov\teax, eax");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_detail], rax");
    r#gen.emit(".Lgpu_init_done:");
    leave(r#gen, &saved);
}

/// Emits code that goes to `.Lgpu_init_no_device` if the CUDA call just made found no GPU, and
/// to `.Lgpu_init_cuda` if it failed otherwise.
fn no_device(r#gen: &mut dyn AssemblyGenerator) {
    for error in NO_DEVICE_ERRORS {
        r#gen.emit(&format!("\tcmp\teax, {error}"));
        r#gen.emit("\tje\t.Lgpu_init_no_device");
    }
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_init_cuda");
}

/// Emits a loop over the image's devices (from `r12`, `r13` of them) that leaves in `r8` the
/// last one whose part with its size at `size` (from the entry's start) is not empty and that
/// runs on the GPU of capability `r14` (times 10): not newer, and exactly it if specific, and if
/// `same_major`, of the major version `edx` holds times 10. It leaves 0 if none does.
fn pick(r#gen: &mut dyn AssemblyGenerator, name: &str, same_major: bool, size: usize) {
    let label = format!(".Lgpu_pick_{name}");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tr9, r12");
    r#gen.emit("\tmov\tr10, r13");
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\ttest\tr10, r10");
    r#gen.emit(&format!("\tjz\t{label}_done"));
    r#gen.emit("\tmov\teax, DWORD PTR [r9]");
    r#gen.emit("\tcmp\teax, r14d");
    r#gen.emit(&format!("\tja\t{label}_next"));
    if same_major {
        r#gen.emit("\tcmp\teax, edx");
        r#gen.emit(&format!("\tjb\t{label}_next"));
        r#gen.emit("\tlea\tecx, [rdx + 10]");
        r#gen.emit("\tcmp\teax, ecx");
        r#gen.emit(&format!("\tjae\t{label}_next"));
    }
    r#gen.emit(&format!("\tcmp\tQWORD PTR [r9 + {size}], 0"));
    r#gen.emit(&format!("\tje\t{label}_next"));
    r#gen.emit("\tcmp\tDWORD PTR [r9 + 4], 0");
    r#gen.emit(&format!("\tje\t{label}_fits"));
    r#gen.emit("\tcmp\teax, r14d");
    r#gen.emit(&format!("\tjne\t{label}_next"));
    r#gen.emit(&format!("{label}_fits:"));
    r#gen.emit("\tmov\tr8, r9");
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit(&format!("\tadd\tr9, {DEVICE_SIZE}"));
    r#gen.emit("\tdec\tr10");
    r#gen.emit(&format!("\tjmp\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits code that sets the carry flag if bit `bit` (a register) of `r13`, the parameters that
/// are the GPU's memory, is set, changing `rax` and `rcx`.
fn is_memory(r#gen: &mut dyn AssemblyGenerator, bit: &str) {
    r#gen.emit(&format!("\tmov\trcx, {bit}"));
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tshr\trax, cl");
    r#gen.emit("\tshr\teax, 1");
}

/// Emits code that frees the GPU's memory the launch's frame says is allocated, keeping `rsi`.
fn free_memory(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tcmp\trsi, r12");
    r#gen.emit(&format!("\tjae\t{label}_done"));
    is_memory(r#gen, "rsi");
    r#gen.emit(&format!("\tjnc\t{label}_next"));
    r#gen.emit("\tmov\trcx, QWORD PTR [rsp + rsi * 8 + 128]");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjz\t{label}_next"));
    cuda(r#gen, "cuMemFree_v2");
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit("\tinc\trsi");
    r#gen.emit(&format!("\tjmp\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits `stone.gpu_launch`.
fn launch(r#gen: &mut dyn AssemblyGenerator, refusals: &Refusals) {
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    // rbx holds the values lowering passes, r12 the number of parameters, r13 those that are
    // the GPU's memory, r14 the bytes of a list, and r15 the kernel's shape; the frame holds a
    // CUDA error at [rsp + 88], the flag read back at [rsp + 96], the flag's parameter (or -1)
    // at [rsp + 104], the kernel at [rsp + 112], each parameter's value from [rsp + 128], and
    // the address of each value, which the launch takes, from [rsp + 640]
    let values = 128;
    let pointers = values + 8 * Shape::MAX_PARAMS as usize;
    let frame = pointers + 8 * Shape::MAX_PARAMS as usize;
    r#gen.emit("stone.gpu_launch:");
    enter(r#gen, &saved);
    r#gen.emit(&format!("\tsub\trsp, {frame}"));
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\trbx, QWORD PTR [rsi + 16]");
    r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.gpu_state]");
    r#gen.emit("\tcmp\trax, 1");
    r#gen.emit("\tje\t.Lgpu_launch_ready");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lgpu_launch_refused");
    r#gen.emit("\tcall\t.Lgpu_init");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.gpu_state], 1");
    r#gen.emit("\tjne\t.Lgpu_launch_refused");
    r#gen.emit(".Lgpu_launch_ready:");
    r#gen.emit(&format!("\tlea\trax, [rip + {LABEL}]"));
    r#gen.emit(&format!(
        "\tmov\teax, DWORD PTR [rax + r12 * 4 + {HEADER_SIZE}]"
    ));
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.gpu_functions]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rcx + rax * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 112], rcx");
    r#gen.emit(&format!("\timul\trax, rax, {SHAPE_SIZE}"));
    r#gen.emit("\tadd\trax, QWORD PTR [rip + stone.gpu_shapes]");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\tr12d, DWORD PTR [r15]");
    r#gen.emit(&format!("\tcmp\tr12, {}", Shape::MAX_PARAMS));
    r#gen.emit("\tja\t.Lgpu_launch_too_many");
    r#gen.emit("\tmov\tr13, QWORD PTR [r15 + 8]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 104], -1");
    r#gen.emit("\tmov\teax, DWORD PTR [r15 + 4]");
    r#gen.emit(&format!("\ttest\teax, {FLAG_FAIL}"));
    r#gen.emit("\tjz\t.Lgpu_launch_unflagged");
    // the flag is the last parameter, and the GPU's memory
    r#gen.emit("\tlea\trcx, [r12 - 1]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 104], rcx");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tshl\trax, cl");
    r#gen.emit("\tor\tr13, rax");
    r#gen.emit(".Lgpu_launch_unflagged:");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 8]");
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit("\tjz\t.Lgpu_launch_processor");
    r#gen.emit("\tshl\tr14, 3");

    // each value, 0 for the GPU's memory until it is allocated, and its address
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(".Lgpu_launch_value:");
    r#gen.emit("\tcmp\trsi, r12");
    r#gen.emit("\tjae\t.Lgpu_launch_valued");
    r#gen.emit(&format!("\tlea\trdx, [rsp + rsi * 8 + {values}]"));
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rsp + rsi * 8 + {pointers}], rdx"
    ));
    r#gen.emit("\tmov\tQWORD PTR [rdx], 0");
    is_memory(r#gen, "rsi");
    r#gen.emit("\tjc\t.Lgpu_launch_value_next");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + rsi * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdx], rax");
    r#gen.emit(".Lgpu_launch_value_next:");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tjmp\t.Lgpu_launch_value");
    r#gen.emit(".Lgpu_launch_valued:");

    // the GPU's memory: the result, each list, copied over, and the flag, cleared
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(".Lgpu_launch_alloc:");
    r#gen.emit("\tcmp\trsi, r12");
    r#gen.emit("\tjae\t.Lgpu_launch_allocated");
    is_memory(r#gen, "rsi");
    r#gen.emit("\tjnc\t.Lgpu_launch_alloc_next");
    r#gen.emit(&format!("\tlea\trcx, [rsp + rsi * 8 + {values}]"));
    r#gen.emit("\tmov\trdx, r14");
    r#gen.emit("\tmov\teax, 8");
    r#gen.emit("\tcmp\trsi, QWORD PTR [rsp + 104]");
    r#gen.emit("\tcmove\trdx, rax");
    cuda(r#gen, "cuMemAlloc_v2");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    r#gen.emit("\tcmp\trsi, QWORD PTR [rsp + 104]");
    r#gen.emit("\tje\t.Lgpu_launch_clear");
    // the result's elements are only written
    r#gen.emit("\ttest\trsi, rsi");
    r#gen.emit("\tjz\t.Lgpu_launch_alloc_next");
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [rsp + rsi * 8 + {values}]"));
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + rsi * 8]");
    r#gen.emit("\tmov\tr8, r14");
    cuda(r#gen, "cuMemcpyHtoD_v2");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    r#gen.emit("\tjmp\t.Lgpu_launch_alloc_next");
    r#gen.emit(".Lgpu_launch_clear:");
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [rsp + rsi * 8 + {values}]"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8d, 8");
    cuda(r#gen, "cuMemsetD8_v2");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    r#gen.emit(".Lgpu_launch_alloc_next:");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tjmp\t.Lgpu_launch_alloc");
    r#gen.emit(".Lgpu_launch_allocated:");

    // as many blocks as the elements need, up to the most, then wait for them
    r#gen.emit("\tmov\trdx, r14");
    r#gen.emit("\tshr\trdx, 3");
    r#gen.emit(&format!("\tadd\trdx, {}", BLOCK - 1));
    r#gen.emit(&format!("\tshr\trdx, {}", BLOCK.trailing_zeros()));
    r#gen.emit(&format!("\tmov\teax, {MAX_BLOCKS}"));
    r#gen.emit("\tcmp\trdx, rax");
    r#gen.emit("\tcmova\trdx, rax");
    r#gen.emit("\tmov\trcx, QWORD PTR [rsp + 112]");
    r#gen.emit("\tmov\tr8d, 1");
    r#gen.emit("\tmov\tr9d, 1");
    for (offset, value) in [(32, BLOCK), (40, 1), (48, 1), (56, 0), (64, 0)] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], {value}"));
    }
    r#gen.emit(&format!("\tlea\trax, [rsp + {pointers}]"));
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 80], 0");
    cuda(r#gen, "cuLaunchKernel");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    cuda(r#gen, "cuCtxSynchronize");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");

    // a zero divisor leaves the result to the processor, which fails where it is
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 104]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjs\t.Lgpu_launch_copy");
    r#gen.emit("\tlea\trcx, [rsp + 96]");
    r#gen.emit(&format!("\tmov\trdx, QWORD PTR [rsp + rax * 8 + {values}]"));
    r#gen.emit("\tmov\tr8d, 8");
    cuda(r#gen, "cuMemcpyDtoH_v2");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    r#gen.emit("\tcmp\tQWORD PTR [rsp + 96], 0");
    r#gen.emit("\tjne\t.Lgpu_launch_divisor");
    r#gen.emit(".Lgpu_launch_copy:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx]");
    r#gen.emit(&format!("\tmov\trdx, QWORD PTR [rsp + {values}]"));
    r#gen.emit("\tmov\tr8, r14");
    cuda(r#gen, "cuMemcpyDtoH_v2");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjnz\t.Lgpu_launch_cuda");
    free_memory(r#gen, ".Lgpu_launch_free");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tjmp\t.Lgpu_launch_done");
    r#gen.emit(".Lgpu_launch_divisor:");
    free_memory(r#gen, ".Lgpu_launch_free_divisor");
    r#gen.emit("\tjmp\t.Lgpu_launch_processor");

    // a CUDA error gives up for good, since the context may be lost
    r#gen.emit(".Lgpu_launch_cuda:");
    r#gen.emit("\tmov\teax, eax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 88], rax");
    free_memory(r#gen, ".Lgpu_launch_free_failed");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_state], 2");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rip + stone.gpu_reason], {REASON_CUDA}"
    ));
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 88]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.gpu_detail], rax");

    // with STONE_GPU=require, a launch the GPU does not make is an error
    r#gen.emit(".Lgpu_launch_refused:");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.gpu_require], 0");
    r#gen.emit("\tje\t.Lgpu_launch_processor");
    r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.gpu_reason]");
    r#gen.emit("\tlea\trdx, [rip + stone.gpu_detail]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdx]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rdx + 8]");
    for (reason, label) in [
        (REASON_DRIVER, &refusals.driver),
        (REASON_DEVICE, &refusals.device),
        (REASON_CODE, &refusals.code),
        (REASON_CUDA, &refusals.cuda),
    ] {
        r#gen.emit(&format!("\tcmp\trax, {reason}"));
        r#gen.emit(&format!("\tje\t{label}"));
    }
    r#gen.emit("\tjmp\t.Lgpu_launch_processor");
    r#gen.emit(".Lgpu_launch_too_many:");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.gpu_require], 0");
    r#gen.emit(&format!("\tjne\t{}", refusals.too_many));
    r#gen.emit(".Lgpu_launch_processor:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit(".Lgpu_launch_done:");
    leave(r#gen, &saved);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_name_is_written_in_utf16_with_a_0_after_it() {
        assert_eq!(wide_quads("ab"), [0x0000_0000_0062_0061]);
        assert_eq!(
            wide_quads("nvcuda.dll"),
            [
                0x0075_0063_0076_006e,
                0x0064_002e_0061_0064,
                0x0000_0000_006c_006c
            ]
        );
    }
}
