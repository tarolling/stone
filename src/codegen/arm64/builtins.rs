//! The hand-written arm64 runtime, which mirrors `x64/builtins.rs` routine for routine, under the
//! same labels and with the same contracts.
//!
//! Routines take their arguments in `x0` and up and return in `x0`, as AAPCS64 does, keep what
//! must survive a call in `x19` and up, and save those with [`push_frame`]. Floats travel as
//! their bits in general registers, as in generated code, and only move to `d0` for arithmetic.
//! The float text routines and `stone.fmod` are in `floats.rs`.

use super::emit::CALLER_SAVED;
use super::{address, pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{CPU_ONLINE, PATH_BUFFER, UTSNAME_FIELD, UTSNAME_SIZE};
use crate::codegen::context::{
    HEAP_REGION, LARGEST_CLASS, LEAK_PREFIX, LEAK_SUFFIX, OUT_OF_MEMORY, OUT_OF_MEMORY_LENGTH,
    SMALLEST_CLASS, STDIN_BUFFER, immortal_string,
};

/// Emits a loop that copies `count` bytes from `src` to `dst`, advancing both and counting
/// `count` down to 0. It clobbers `w16`, and uses `label` and `label_done` as its labels.
///
/// For example, `copy_bytes(gen, "x9", "x19", "x21", ".Lstr_concat_left")` copies `x21` bytes
/// from `x19` to `x9`.
pub(super) fn copy_bytes(
    r#gen: &mut dyn AssemblyGenerator,
    dst: &str,
    src: &str,
    count: &str,
    label: &str,
) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\tcbz\t{count}, {label}_done"));
    r#gen.emit(&format!("\tldrb\tw16, [{src}], #1"));
    r#gen.emit(&format!("\tstrb\tw16, [{dst}], #1"));
    r#gen.emit(&format!("\tsub\t{count}, {count}, #1"));
    r#gen.emit(&format!("\tb\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits code that adds `delta` to `stone.live`, the count of objects allocated and not yet
/// freed. It clobbers `x16` and `x17`.
fn count_live(r#gen: &mut dyn AssemblyGenerator, delta: i64) {
    r#gen.emit("\tadrp\tx16, stone.live");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.live]");
    if delta < 0 {
        r#gen.emit(&format!("\tsub\tx17, x17, #{}", -delta));
    } else {
        r#gen.emit(&format!("\tadd\tx17, x17, #{delta}"));
    }
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.live]");
}

/// Emits `_start`, where the kernel starts the program, with the argument count at `[sp]`, the
/// arguments after it, then a null, then the environment. It calls `main` with the count in `x0`,
/// the arguments in `x1`, and the environment in `x2`, as a C program's `main` gets them, then
/// ends the program with the status `main` returns.
pub fn start_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.globl\t_start");
    r#gen.emit("_start:");
    r#gen.emit("\tmov\tx29, #0"); // the outermost frame
    r#gen.emit("\tmov\tx30, #0");
    r#gen.emit("\tldr\tx0, [sp]");
    r#gen.emit("\tadd\tx1, sp, #8");
    r#gen.emit("\tadd\tx2, x1, x0, lsl #3");
    r#gen.emit("\tadd\tx2, x2, #8");
    r#gen.emit("\tbl\tmain");
    r#gen.emit("\tmov\tx8, #94"); // sys_exit_group
    r#gen.emit("\tsvc\t#0");
}

/// Emits the allocator, which takes memory from the system with the `mmap` syscall instead of
/// libc's `malloc`, with the same blocks, classes, and regions as x64's `allocator`.
///
/// - `stone.mem_alloc` returns at least `x0` bytes, aligned to 8
/// - `stone.mem_free` frees the bytes at `x9`
/// - `stone.mem_realloc` returns at least `x1` bytes holding what the `x0` bytes held, moving
///   them only if their block is too small
///
/// All three clobber only `x8` to `x11`, `x16`, `x17`, and `x30`, so the free routines and
/// `stone.list_copy` can call them. If the system has no memory to give, the program stops with
/// [`OUT_OF_MEMORY`].
pub fn allocator(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.heap_next:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.heap_end:");
    r#gen.emit("\t.zero\t8");
    // the first free block of each class, linked through the 8 bytes after each header
    r#gen.emit("stone.free_blocks:");
    r#gen.emit(&format!("\t.zero\t{}", 8 * (LARGEST_CLASS + 1)));
    r#gen.emit("\t.text");

    r#gen.emit("stone.mem_alloc:");
    // the class is the smallest power of two that holds the header too
    r#gen.emit("\tadd\tx9, x0, #7");
    r#gen.emit("\tclz\tx9, x9");
    r#gen.emit("\tmov\tx10, #64");
    r#gen.emit("\tsub\tx9, x10, x9");
    r#gen.emit(&format!("\tmov\tx10, #{SMALLEST_CLASS}"));
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tcsel\tx9, x9, x10, hs");
    r#gen.emit(&format!("\tcmp\tx9, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_alloc_large");
    address(r#gen, "x10", "stone.free_blocks");
    r#gen.emit("\tadd\tx10, x10, x9, lsl #3");
    r#gen.emit("\tldr\tx11, [x10]");
    r#gen.emit("\tcbz\tx11, .Lmem_alloc_cut");
    // a freed block keeps its class in its header
    r#gen.emit("\tldr\tx16, [x11, #8]");
    r#gen.emit("\tstr\tx16, [x10]");
    r#gen.emit("\tadd\tx0, x11, #8");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_alloc_cut:");
    r#gen.emit("\tadrp\tx10, stone.heap_next");
    r#gen.emit("\tldr\tx11, [x10, :lo12:stone.heap_next]");
    r#gen.emit("\tmov\tx16, #1");
    r#gen.emit("\tlsl\tx16, x16, x9");
    r#gen.emit("\tadd\tx16, x16, x11");
    r#gen.emit("\tadrp\tx17, stone.heap_end");
    r#gen.emit("\tldr\tx17, [x17, :lo12:stone.heap_end]");
    r#gen.emit("\tcmp\tx16, x17");
    r#gen.emit("\tb.hi\t.Lmem_alloc_region");
    r#gen.emit("\tstr\tx16, [x10, :lo12:stone.heap_next]");
    r#gen.emit("\tstr\tx9, [x11]");
    r#gen.emit("\tadd\tx0, x11, #8");
    r#gen.emit("\tret");
    // what is left of the old region is too small, and stays unused
    r#gen.emit(".Lmem_alloc_region:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tstr\tx0, [sp, #-16]!");
    r#gen.emit(&format!("\tmov\tx0, #{HEAP_REGION}"));
    r#gen.emit("\tbl\tstone.mem_map");
    r#gen.emit("\tadrp\tx10, stone.heap_next");
    r#gen.emit("\tstr\tx0, [x10, :lo12:stone.heap_next]");
    r#gen.emit(&format!("\tadd\tx0, x0, #{HEAP_REGION}"));
    r#gen.emit("\tadrp\tx10, stone.heap_end");
    r#gen.emit("\tstr\tx0, [x10, :lo12:stone.heap_end]");
    r#gen.emit("\tldr\tx0, [sp], #16");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tb\tstone.mem_alloc");
    r#gen.emit(".Lmem_alloc_large:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tadd\tx0, x0, #4095"); // whole pages
    r#gen.emit("\tand\tx0, x0, #-4096");
    r#gen.emit("\tstr\tx0, [sp, #-16]!");
    r#gen.emit("\tbl\tstone.mem_map");
    r#gen.emit("\tldr\tx9, [sp], #16");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_free:");
    r#gen.emit("\tsub\tx9, x9, #8");
    r#gen.emit("\tldr\tx10, [x9]");
    r#gen.emit(&format!("\tcmp\tx10, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_free_large");
    address(r#gen, "x11", "stone.free_blocks");
    r#gen.emit("\tadd\tx11, x11, x10, lsl #3");
    r#gen.emit("\tldr\tx16, [x11]");
    r#gen.emit("\tstr\tx16, [x9, #8]");
    r#gen.emit("\tstr\tx9, [x11]");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_free_large:");
    r#gen.emit("\tstp\tx0, x1, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x9");
    r#gen.emit("\tmov\tx1, x10");
    r#gen.emit("\tmov\tx8, #215"); // sys_munmap
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tldp\tx0, x1, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_realloc:");
    r#gen.emit("\tldur\tx9, [x0, #-8]");
    r#gen.emit("\tadd\tx10, x1, #8"); // the size the block needs
    r#gen.emit(&format!("\tcmp\tx9, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_realloc_large");
    r#gen.emit("\tmov\tx11, #1");
    r#gen.emit("\tlsl\tx11, x11, x9");
    r#gen.emit("\tcmp\tx10, x11");
    r#gen.emit("\tb.hi\t.Lmem_realloc_move");
    r#gen.emit("\tret");
    // the old block's x11 bytes, less its header, are copied to a new one 8 at a time
    r#gen.emit(".Lmem_realloc_move:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tstp\tx0, x11, [sp, #-16]!");
    r#gen.emit("\tstr\tx1, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x1");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tldr\tx1, [sp], #16");
    r#gen.emit("\tldp\tx9, x11, [sp], #16");
    r#gen.emit("\tsub\tx11, x11, #8");
    r#gen.emit("\tmov\tx10, x0");
    r#gen.emit("\tmov\tx17, x9");
    r#gen.emit(".Lmem_realloc_copy:");
    r#gen.emit("\tldr\tx16, [x17], #8");
    r#gen.emit("\tstr\tx16, [x10], #8");
    r#gen.emit("\tsubs\tx11, x11, #8");
    r#gen.emit("\tb.ne\t.Lmem_realloc_copy");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_realloc_large:");
    r#gen.emit("\tcmp\tx10, x9");
    r#gen.emit("\tb.hi\t.Lmem_realloc_remap");
    r#gen.emit("\tret");
    // the kernel moves the pages if it cannot grow the mapping where it is
    r#gen.emit(".Lmem_realloc_remap:");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tstp\tx3, x4, [sp, #-16]!");
    r#gen.emit("\tadd\tx2, x10, #4095");
    r#gen.emit("\tand\tx2, x2, #-4096");
    r#gen.emit("\tmov\tx1, x9");
    r#gen.emit("\tsub\tx0, x0, #8");
    r#gen.emit("\tmov\tx3, #1"); // MREMAP_MAYMOVE
    r#gen.emit("\tmov\tx8, #216"); // sys_mremap
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tcmn\tx0, #4095");
    r#gen.emit("\tb.hs\tstone.out_of_memory");
    r#gen.emit("\tstr\tx2, [x0]");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tldp\tx3, x4, [sp], #16");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tret");

    // returns x0 new bytes of zeros, a whole number of pages, clobbering only x8
    r#gen.emit("stone.mem_map:");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tstp\tx3, x4, [sp, #-16]!");
    r#gen.emit("\tstr\tx5, [sp, #-16]!");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, #0"); // anywhere
    r#gen.emit("\tmov\tx2, #3"); // PROT_READ | PROT_WRITE
    r#gen.emit("\tmov\tx3, #0x22"); // MAP_PRIVATE | MAP_ANONYMOUS
    r#gen.emit("\tmov\tx4, #-1"); // no file
    r#gen.emit("\tmov\tx5, #0");
    r#gen.emit("\tmov\tx8, #222"); // sys_mmap
    r#gen.emit("\tsvc\t#0");
    // the kernel returns an error as -4095 through -1
    r#gen.emit("\tcmn\tx0, #4095");
    r#gen.emit("\tb.hs\tstone.out_of_memory");
    r#gen.emit("\tldr\tx5, [sp], #16");
    r#gen.emit("\tldp\tx3, x4, [sp], #16");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.out_of_memory:");
    r#gen.emit("\tmov\tx0, #2"); // stderr
    address(r#gen, "x1", ".Lstone_out_of_memory");
    r#gen.emit(&format!("\tmov\tx2, #{OUT_OF_MEMORY_LENGTH}"));
    r#gen.emit("\tmov\tx8, #64"); // sys_write
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tmov\tx8, #94"); // sys_exit_group
    r#gen.emit("\tsvc\t#0");
}

/// Emits the memory runtime. Every string and list `p` is counted: `[p - 8]` holds how many
/// variables, list slots, and temporaries refer to it, and it is freed when that reaches 0.
/// `stone.live` counts the objects allocated and not yet freed.
///
/// - `stone.alloc` returns a new object with room for `x0` bytes and a count of 1
/// - `stone.free_str` frees the string in `x9`, whose count just reached 0
/// - `stone.free_list` frees the list in `x9`, whose count just reached 0, along with its
///   elements' storage, first releasing each element if its `elem` field says they are counted
/// - `stone.leak_check` exits with an error if the `STONE_LEAK_CHECK` environment variable is set
///   and any object is still allocated, which tests use to find leaks
///
/// The free routines are reached from inline releases that do not count as calls, so they
/// preserve every register a vreg can be in, clobbering only `x8` to `x11`, `x16`, `x17`, `x30`,
/// and the float registers. `stone.leak_check` also preserves `x0`, which holds `main`'s exit
/// status. Memory comes from [`allocator`].
pub fn memory_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_leak_env:");
    r#gen.emit("\t.string \"STONE_LEAK_CHECK\"");
    r#gen.emit(".Lstone_leak_prefix:");
    r#gen.emit(&format!("\t.string \"{LEAK_PREFIX}\""));
    r#gen.emit(".Lstone_leak_suffix:");
    r#gen.emit(&format!("\t.string \"{LEAK_SUFFIX}\\n\""));
    r#gen.emit(".Lstone_out_of_memory:");
    r#gen.emit(&format!("\t.string \"{OUT_OF_MEMORY}\""));
    r#gen.emit("\t.text");

    allocator(r#gen);

    r#gen.emit("stone.alloc:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tadd\tx0, x0, #8"); // room for the count
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    count_live(r#gen, 1);
    r#gen.emit("\tadd\tx0, x0, #8");
    pop_frame(r#gen, &[]);

    // the count is where the allocation starts
    r#gen.emit("stone.free_str:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tsub\tx9, x9, #8");
    r#gen.emit("\tbl\tstone.mem_free");
    count_live(r#gen, -1);
    pop_frame(r#gen, &[]);

    // x19 holds the list and x20 the index of the element being released
    let saved = ["x19", "x20"];
    r#gen.emit("stone.free_list:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x9");
    r#gen.emit("\tldr\tx10, [x19, #24]");
    r#gen.emit("\tcbz\tx10, .Lfree_list_storage"); // the elements are not counted
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit(".Lfree_list_loop:");
    r#gen.emit("\tldr\tx10, [x19]");
    r#gen.emit("\tcmp\tx20, x10");
    r#gen.emit("\tb.ge\t.Lfree_list_storage");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tldr\tx9, [x10, x20, lsl #3]");
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit("\tldur\tx10, [x9, #-8]");
    r#gen.emit("\tsubs\tx10, x10, #1");
    r#gen.emit("\tstur\tx10, [x9, #-8]");
    r#gen.emit("\tb.ne\t.Lfree_list_loop");
    r#gen.emit("\tldr\tx10, [x19, #24]");
    r#gen.emit("\tcmp\tx10, #1");
    r#gen.emit("\tb.eq\t.Lfree_list_str");
    // nesting is bounded by the element type, so this recursion is shallow
    r#gen.emit("\tbl\tstone.free_list");
    r#gen.emit("\tb\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_str:");
    r#gen.emit("\tbl\tstone.free_str");
    r#gen.emit("\tb\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_storage:");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tsub\tx9, x19, #8");
    r#gen.emit("\tbl\tstone.mem_free");
    count_live(r#gen, -1);
    pop_frame(r#gen, &saved);

    // the count's digits are written backwards into 32 bytes of stack
    r#gen.emit("stone.leak_check:");
    push_frame(r#gen, &["x0"]);
    r#gen.emit("\tadrp\tx9, stone.live");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.live]");
    r#gen.emit("\tcbz\tx9, .Lleak_check_done");
    address(r#gen, "x0", ".Lstone_leak_env");
    r#gen.emit("\tbl\tstone.getenv");
    r#gen.emit("\tcbz\tx0, .Lleak_check_done");
    write_stderr(r#gen, ".Lstone_leak_prefix", LEAK_PREFIX.len());
    r#gen.emit("\tsub\tsp, sp, #32");
    r#gen.emit("\tadd\tx1, sp, #32");
    r#gen.emit("\tadrp\tx9, stone.live");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.live]");
    r#gen.emit("\tmov\tx10, #10");
    r#gen.emit(".Lleak_check_digit:");
    r#gen.emit("\tudiv\tx11, x9, x10");
    r#gen.emit("\tmsub\tx16, x11, x10, x9");
    r#gen.emit("\tadd\tw16, w16, #48"); // '0'
    r#gen.emit("\tstrb\tw16, [x1, #-1]!");
    r#gen.emit("\tmov\tx9, x11");
    r#gen.emit("\tcbnz\tx9, .Lleak_check_digit");
    r#gen.emit("\tadd\tx2, sp, #32");
    r#gen.emit("\tsub\tx2, x2, x1");
    r#gen.emit("\tmov\tx0, #2"); // stderr
    r#gen.emit("\tmov\tx8, #64"); // sys_write
    r#gen.emit("\tsvc\t#0");
    write_stderr(r#gen, ".Lstone_leak_suffix", LEAK_SUFFIX.len() + 1);
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tmov\tx8, #94"); // sys_exit_group
    r#gen.emit("\tsvc\t#0");
    r#gen.emit(".Lleak_check_done:");
    pop_frame(r#gen, &["x0"]);
}

/// Emits code that writes the `length` bytes at `label` to stderr, clobbering `x0` to `x2` and
/// `x8`.
fn write_stderr(r#gen: &mut dyn AssemblyGenerator, label: &str, length: usize) {
    r#gen.emit("\tmov\tx0, #2"); // stderr
    address(r#gen, "x1", label);
    r#gen.emit(&format!("\tmov\tx2, #{length}"));
    r#gen.emit("\tmov\tx8, #64"); // sys_write
    r#gen.emit("\tsvc\t#0");
}

/// Emits `stone.getenv`, which returns the value of the environment variable named by the
/// string in `x0`, or 0 if it is not set, like libc's `getenv`. It reads the environment `main`
/// saved in `stone.envp`, and clobbers `x9` to `x11`, `x16`, and `x17`.
pub fn env_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.getenv:");
    r#gen.emit("\tadrp\tx9, stone.envp");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.envp]");
    r#gen.emit(".Lgetenv_entry:");
    r#gen.emit("\tldr\tx10, [x9], #8");
    r#gen.emit("\tcbz\tx10, .Lgetenv_none"); // the list ends with null
    r#gen.emit("\tmov\tx11, #0");
    // the entry must start with the name, then =
    r#gen.emit(".Lgetenv_compare:");
    r#gen.emit("\tldrb\tw16, [x0, x11]");
    r#gen.emit("\tcbz\tw16, .Lgetenv_name_end");
    r#gen.emit("\tldrb\tw17, [x10, x11]");
    r#gen.emit("\tcmp\tw16, w17");
    r#gen.emit("\tb.ne\t.Lgetenv_entry");
    r#gen.emit("\tadd\tx11, x11, #1");
    r#gen.emit("\tb\t.Lgetenv_compare");
    r#gen.emit(".Lgetenv_name_end:");
    r#gen.emit("\tldrb\tw17, [x10, x11]");
    r#gen.emit("\tcmp\tw17, #61"); // '='
    r#gen.emit("\tb.ne\t.Lgetenv_entry");
    r#gen.emit("\tadd\tx0, x10, x11");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tret");
    r#gen.emit(".Lgetenv_none:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");
}

/// Emits the routines behind `print`, each writing one value to stdout with no newline, so a call
/// like `print("n", 1)` writes `n`, a space, `1`, and a newline with four calls.
///
/// - `stone.print_int` writes the signed integer in `x0`
/// - `stone.print_str` writes the null-terminated string `x0` points to
/// - `stone.print_bool` writes `true` if `x0` is nonzero and `false` otherwise
/// - `stone.print_none` writes `none`
/// - `stone.print_char` writes the byte in `w0`
/// - `stone.print_str_quoted` writes the string `x0` points to in single quotes, as a
///   printed list shows its strings
///
/// They write with the `write` system call and clobber `x0` to `x2`, `x8` to `x13`, and `x16`.
pub fn print(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_true:");
    r#gen.emit("\t.string \"true\"");
    r#gen.emit(".Lstone_false:");
    r#gen.emit("\t.string \"false\"");
    r#gen.emit(".Lstone_none:");
    r#gen.emit("\t.string \"none\"");
    r#gen.emit("\t.text");

    // digits are written backwards from the end of 32 bytes, which holds all 20 and a sign
    r#gen.emit("stone.print_int:");
    r#gen.emit("\tsub\tsp, sp, #32");
    r#gen.emit("\tadd\tx1, sp, #32");
    r#gen.emit("\tmov\tx9, x0");
    r#gen.emit("\tmov\tx10, #0"); // negative flag
    r#gen.emit("\tcmp\tx9, #0");
    r#gen.emit("\tb.ge\t.Lprint_int_digits");
    // negating the minimum leaves it unchanged, which unsigned division still reads correctly
    r#gen.emit("\tneg\tx9, x9");
    r#gen.emit("\tmov\tx10, #1");
    r#gen.emit(".Lprint_int_digits:");
    r#gen.emit("\tmov\tx11, #10");
    r#gen.emit(".Lprint_int_loop:");
    // at least one digit, so zero prints as 0
    r#gen.emit("\tudiv\tx12, x9, x11");
    r#gen.emit("\tmsub\tx13, x12, x11, x9");
    r#gen.emit("\tadd\tx13, x13, #48"); // '0'
    r#gen.emit("\tstrb\tw13, [x1, #-1]!");
    r#gen.emit("\tmov\tx9, x12");
    r#gen.emit("\tcbnz\tx9, .Lprint_int_loop");
    r#gen.emit("\tcbz\tx10, .Lprint_int_write");
    r#gen.emit("\tmov\tx13, #45"); // '-'
    r#gen.emit("\tstrb\tw13, [x1, #-1]!");
    r#gen.emit(".Lprint_int_write:");
    r#gen.emit("\tadd\tx2, sp, #32");
    r#gen.emit("\tsub\tx2, x2, x1");
    r#gen.emit("\tmov\tx0, #1"); // stdout
    r#gen.emit("\tmov\tx8, #64"); // write
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tadd\tsp, sp, #32");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_str:");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit(".Lprint_str_len:");
    r#gen.emit("\tldrb\tw9, [x1, x2]");
    r#gen.emit("\tcbz\tw9, .Lprint_str_write");
    r#gen.emit("\tadd\tx2, x2, #1");
    r#gen.emit("\tb\t.Lprint_str_len");
    r#gen.emit(".Lprint_str_write:");
    r#gen.emit("\tcbz\tx2, .Lprint_str_done"); // empty string
    r#gen.emit("\tmov\tx0, #1"); // stdout
    r#gen.emit("\tmov\tx8, #64"); // write
    r#gen.emit("\tsvc\t#0");
    r#gen.emit(".Lprint_str_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_bool:");
    r#gen.emit("\tcmp\tx0, #0");
    address(r#gen, "x0", ".Lstone_true");
    address(r#gen, "x9", ".Lstone_false");
    r#gen.emit("\tcsel\tx0, x0, x9, ne");
    r#gen.emit("\tb\tstone.print_str");

    r#gen.emit("stone.print_none:");
    address(r#gen, "x0", ".Lstone_none");
    r#gen.emit("\tb\tstone.print_str");

    r#gen.emit("stone.print_char:");
    r#gen.emit("\tsub\tsp, sp, #16");
    r#gen.emit("\tstrb\tw0, [sp]");
    r#gen.emit("\tmov\tx1, sp");
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tmov\tx0, #1"); // stdout
    r#gen.emit("\tmov\tx8, #64"); // write
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tadd\tsp, sp, #16");
    r#gen.emit("\tret");

    // strings inside a printed list are quoted, like ['a']
    r#gen.emit("stone.print_str_quoted:");
    push_frame(r#gen, &["x19"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx0, #39"); // '\''
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\tstone.print_str");
    r#gen.emit("\tmov\tx0, #39");
    r#gen.emit("\tbl\tstone.print_char");
    pop_frame(r#gen, &["x19"]);
}

/// Emits `stone.str_float`, which returns the float whose bits are in `x0` as a new string, the
/// way `stone.print_float` prints it. It needs `floats::float_runtime` and the memory runtime.
pub fn str_float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the bits, and x20 the new string
    let saved = ["x19", "x20"];
    r#gen.emit("stone.str_float:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx0, #64");
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\tstone.format_float");
    r#gen.emit("\tmov\tx0, x20");
    pop_frame(r#gen, &saved);
}

/// Emits the string runtime. Strings are null-terminated and counted (see [`memory_runtime`]), and
/// `+` makes a new one with `stone.alloc`.
///
/// - `stone.str_len` returns the length in bytes of the string in `x0`
/// - `stone.str_eq` returns 1 if the strings in `x0` and `x1` hold the same bytes, else 0
/// - `stone.str_concat` returns a new string holding `x0` followed by `x1`
/// - `stone.str_slice` returns a new string holding the `x1` bytes at `x0`
pub fn string_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.str_len:");
    r#gen.emit("\tmov\tx9, x0");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit(".Lstr_len_loop:");
    r#gen.emit("\tldrb\tw10, [x9, x0]");
    r#gen.emit("\tcbz\tw10, .Lstr_len_done");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tb\t.Lstr_len_loop");
    r#gen.emit(".Lstr_len_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.str_eq:");
    r#gen.emit(".Lstr_eq_loop:");
    r#gen.emit("\tldrb\tw9, [x0], #1");
    r#gen.emit("\tldrb\tw10, [x1], #1");
    r#gen.emit("\tcmp\tw9, w10");
    r#gen.emit("\tb.ne\t.Lstr_eq_differ");
    r#gen.emit("\tcbnz\tw9, .Lstr_eq_loop"); // both ended together otherwise
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tret");
    r#gen.emit(".Lstr_eq_differ:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");

    // x19 and x20 hold the two strings and x21 and x22 their lengths across the calls
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.str_concat:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx22, x0");
    r#gen.emit("\tadd\tx0, x21, x22");
    r#gen.emit("\tadd\tx0, x0, #1"); // room for the terminator
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx9, x0");
    copy_bytes(r#gen, "x9", "x19", "x21", ".Lstr_concat_left");
    r#gen.emit("\tadd\tx22, x22, #1"); // copy the terminator too
    copy_bytes(r#gen, "x9", "x20", "x22", ".Lstr_concat_right");
    pop_frame(r#gen, &saved);

    // x19 and x20 hold the bytes and their count across stone.alloc
    let saved = ["x19", "x20"];
    r#gen.emit("stone.str_slice:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tadd\tx0, x1, #1"); // room for the terminator
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx9, x0");
    copy_bytes(r#gen, "x9", "x19", "x20", ".Lstr_slice_copy");
    r#gen.emit("\tstrb\twzr, [x9]");
    pop_frame(r#gen, &saved);
}

/// Emits the list runtime. A list is a pointer to a counted 32-byte header `{len, cap, data,
/// elem}` (see [`memory_runtime`]), where `data` points to `cap` 8-byte elements and `elem` says
/// what they are: 0 for values that are not counted, 1 for strings, and 2 for lists.
///
/// - `stone.list_new` returns a list of length `x0` whose elements the caller fills in, with
///   `elem` set to `x1`
/// - `stone.list_append` appends `x1` to list `x0`, doubling its capacity when full
/// - `stone.list_copy` returns in `x9` a copy of the list in `x9`, retaining each element if
///   they are counted, and drops one reference to the original, which something else still
///   holds. Like the free routines, it is reached from an inline `list_unique` that does not
///   count as a call, so it clobbers only `x8` to `x11`, `x16`, `x17`, `x30`, and the float
///   registers.
///
/// Indexing is generated inline by `Arm64Generator::list_slot` (in `emit.rs`) rather than called
/// here.
pub fn list_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the list, x20 its length, and x21 what its elements are
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.list_new:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tmov\tx0, #32");
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tstr\tx20, [x19]");
    r#gen.emit("\tstr\tx21, [x19, #24]");
    // room for at least 4 elements, so the first appends do not reallocate
    r#gen.emit("\tmov\tx9, #4");
    r#gen.emit("\tcmp\tx20, x9");
    r#gen.emit("\tcsel\tx9, x20, x9, ge");
    r#gen.emit("\tstr\tx9, [x19, #8]");
    r#gen.emit("\tlsl\tx0, x9, #3");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit("\tmov\tx0, x19");
    pop_frame(r#gen, &saved);

    let saved = ["x19", "x20"];
    r#gen.emit("stone.list_append:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #8]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.lt\t.Llist_append_store");
    r#gen.emit("\tlsl\tx10, x10, #1");
    r#gen.emit("\tstr\tx10, [x19, #8]");
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\tlsl\tx1, x10, #3");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit(".Llist_append_store:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tstr\tx20, [x10, x9, lsl #3]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstr\tx9, [x19]");
    r#gen.emit("\tmov\tx0, #0"); // append returns none
    pop_frame(r#gen, &saved);

    // x19 holds the original, x20 the copy, x10 and x11 their elements, and x17 what they are
    let mut saved = CALLER_SAVED.to_vec();
    saved.extend(["x19", "x20"]);
    r#gen.emit("stone.list_copy:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x9");
    r#gen.emit("\tldr\tx0, [x19]");
    r#gen.emit("\tldr\tx1, [x19, #24]");
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tldr\tx10, [x20, #16]");
    r#gen.emit("\tldr\tx11, [x19, #16]");
    r#gen.emit("\tldr\tx17, [x19, #24]");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Llist_copy_loop:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tcmp\tx8, x9");
    r#gen.emit("\tb.ge\t.Llist_copy_done");
    r#gen.emit("\tldr\tx9, [x11, x8, lsl #3]");
    r#gen.emit("\tstr\tx9, [x10, x8, lsl #3]");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tcbz\tx17, .Llist_copy_loop"); // the elements are not counted
    r#gen.emit("\tldur\tx16, [x9, #-8]");
    r#gen.emit("\tadd\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x9, #-8]");
    r#gen.emit("\tb\t.Llist_copy_loop");
    r#gen.emit(".Llist_copy_done:");
    // the variable or slot that held the original now holds the copy instead
    r#gen.emit("\tldur\tx16, [x19, #-8]");
    r#gen.emit("\tsub\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x19, #-8]");
    r#gen.emit("\tmov\tx9, x20");
    pop_frame(r#gen, &saved);
}

/// Emits `label`, a routine that prints the list in `x0` as `[a, b]`, calling `element` for each
/// element.
///
/// For example, `print_list(gen, "stone.print_list_int", "stone.print_int")` emits the printer for
/// `list[int]`.
pub fn print_list(r#gen: &mut dyn AssemblyGenerator, label: &str, element: &str) {
    let local = label.replace('.', "_");
    // x19 holds the list and x20 the index
    let saved = ["x19", "x20"];
    r#gen.emit(&format!("{label}:"));
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit("\tmov\tx0, #91"); // '['
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit(&format!(".L{local}_loop:"));
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tcmp\tx20, x9");
    r#gen.emit(&format!("\tb.ge\t.L{local}_done"));
    r#gen.emit(&format!("\tcbz\tx20, .L{local}_element"));
    r#gen.emit("\tmov\tx0, #44"); // ','
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tmov\tx0, #32"); // ' '
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit(&format!(".L{local}_element:"));
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tldr\tx0, [x9, x20, lsl #3]");
    r#gen.emit(&format!("\tbl\t{element}"));
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit(&format!("\tb\t.L{local}_loop"));
    r#gen.emit(&format!(".L{local}_done:"));
    r#gen.emit("\tmov\tx0, #93"); // ']'
    r#gen.emit("\tbl\tstone.print_char");
    pop_frame(r#gen, &saved);
}

/// Emits a jump to `label` if the byte zero-extended in `reg`, a 32-bit register other than
/// `w16`, is whitespace as `stdlib::is_space` defines it: a space, or a tab through a carriage
/// return. It clobbers `w16`.
///
/// For example, `jump_if_space(gen, "w10", ".Lskip")` jumps for `\t` but not for `a` or 0.
pub(super) fn jump_if_space(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tcmp\t{reg}, #32"));
    r#gen.emit(&format!("\tb.eq\t{label}"));
    // tab, newline, vertical tab, form feed, and carriage return are 9 through 13
    r#gen.emit(&format!("\tsub\tw16, {reg}, #9"));
    r#gen.emit("\tcmp\tw16, #4");
    r#gen.emit(&format!("\tb.ls\t{label}"));
}

/// Emits the input routines named in `used`, which read stdin with the `read` syscall into a
/// buffer, the same way as x64's `io_runtime`.
///
/// - `stone.input` prints the string in `x0` unless it is null, then returns the next line of
///   stdin as a new string without its newline, or an empty string at the end of the input
/// - `stone.eof` returns 1 if stdin has nothing left and 0 otherwise
/// - `stone.stdin_fill` reads into the buffer from its start and returns how many bytes it
///   read, or 0 at the end of the input or on an error
///
/// `stone.input` needs `print`'s routines, the string runtime, and the allocator.
pub fn io_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.stdin_pos:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.stdin_len:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.stdin_buffer:");
    r#gen.emit(&format!("\t.zero\t{STDIN_BUFFER}"));
    r#gen.emit("\t.text");

    r#gen.emit("stone.stdin_fill:");
    r#gen.emit("\tmov\tx0, #0"); // stdin
    address(r#gen, "x1", "stone.stdin_buffer");
    r#gen.emit(&format!("\tmov\tx2, #{STDIN_BUFFER}"));
    r#gen.emit("\tmov\tx8, #63"); // sys_read
    r#gen.emit("\tsvc\t#0");
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tcsel\tx0, x0, xzr, gt");
    r#gen.emit("\tadrp\tx9, stone.stdin_len");
    r#gen.emit("\tstr\tx0, [x9, :lo12:stone.stdin_len]");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tstr\txzr, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tret");

    if used.contains(&"stone.eof") {
        r#gen.emit("stone.eof:");
        r#gen.emit("\tadrp\tx9, stone.stdin_pos");
        r#gen.emit("\tldr\tx9, [x9, :lo12:stone.stdin_pos]");
        r#gen.emit("\tadrp\tx10, stone.stdin_len");
        r#gen.emit("\tldr\tx10, [x10, :lo12:stone.stdin_len]");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tb.lo\t.Leof_no");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.stdin_fill");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit("\tcset\tx0, eq");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Leof_no:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }

    if !used.contains(&"stone.input") {
        return;
    }
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_empty", "");
    r#gen.emit("\t.text");

    // x19 holds the block a long line is gathered in, or 0 before there is one, x20 how many
    // bytes it holds, and x21 how many it has room for
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.input:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tcbz\tx0, .Linput_read");
    r#gen.emit("\tbl\tstone.print_str");
    r#gen.emit(".Linput_read:");
    r#gen.emit("\tmov\tx19, #0");
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit("\tmov\tx21, #0");
    r#gen.emit(".Linput_scan:");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tldr\tx1, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadrp\tx9, stone.stdin_len");
    r#gen.emit("\tldr\tx2, [x9, :lo12:stone.stdin_len]");
    r#gen.emit("\tcmp\tx1, x2");
    r#gen.emit("\tb.lo\t.Linput_search");
    r#gen.emit("\tbl\tstone.stdin_fill");
    r#gen.emit("\tcbnz\tx0, .Linput_scan");
    // the input ended, so what was gathered is the last line, unless nothing was
    r#gen.emit("\tcbnz\tx19, .Linput_gathered");
    address(r#gen, "x0", ".Lstone_empty");
    r#gen.emit("\tb\t.Linput_done");
    // x3 looks for a newline from x1 up to x2 in the buffer at x0
    r#gen.emit(".Linput_search:");
    address(r#gen, "x0", "stone.stdin_buffer");
    r#gen.emit("\tmov\tx3, x1");
    r#gen.emit(".Linput_find:");
    r#gen.emit("\tcmp\tx3, x2");
    r#gen.emit("\tb.eq\t.Linput_partial");
    r#gen.emit("\tldrb\tw9, [x0, x3]");
    r#gen.emit("\tcmp\tw9, #10"); // newline
    r#gen.emit("\tb.eq\t.Linput_newline");
    r#gen.emit("\tadd\tx3, x3, #1");
    r#gen.emit("\tb\t.Linput_find");
    // the rest of the buffer is part of a longer line
    r#gen.emit(".Linput_partial:");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tstr\tx2, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadd\tx0, x0, x1");
    r#gen.emit("\tsub\tx1, x2, x1");
    r#gen.emit("\tbl\t.Linput_append");
    r#gen.emit("\tb\t.Linput_scan");
    // the line ends at x3, and the newline is read but left out
    r#gen.emit(".Linput_newline:");
    r#gen.emit("\tadd\tx9, x3, #1");
    r#gen.emit("\tadrp\tx10, stone.stdin_pos");
    r#gen.emit("\tstr\tx9, [x10, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadd\tx0, x0, x1");
    r#gen.emit("\tsub\tx1, x3, x1");
    r#gen.emit("\tcbnz\tx19, .Linput_last_piece");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tb\t.Linput_done");
    r#gen.emit(".Linput_last_piece:");
    r#gen.emit("\tbl\t.Linput_append");
    // the gathered line becomes a string, and its block is freed
    r#gen.emit(".Linput_gathered:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx9, x19");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit(".Linput_done:");
    pop_frame(r#gen, &saved);

    // adds the x1 bytes at x0 to the block in x19, growing it to twice what it must hold
    r#gen.emit(".Linput_append:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tadd\tx3, x20, x1");
    r#gen.emit("\tcmp\tx3, x21");
    r#gen.emit("\tb.ls\t.Linput_append_copy");
    r#gen.emit("\tstp\tx0, x1, [sp, #-16]!");
    r#gen.emit("\tstr\tx3, [sp, #-16]!");
    r#gen.emit("\tlsl\tx21, x3, #1");
    r#gen.emit("\tcbnz\tx19, .Linput_append_grow");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tb\t.Linput_append_grown");
    r#gen.emit(".Linput_append_grow:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit(".Linput_append_grown:");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx3, [sp], #16");
    r#gen.emit("\tldp\tx0, x1, [sp], #16");
    r#gen.emit(".Linput_append_copy:");
    r#gen.emit("\tadd\tx9, x19, x20");
    copy_bytes(r#gen, "x9", "x0", "x1", ".Linput_append_bytes");
    r#gen.emit("\tmov\tx20, x3");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
}

/// Emits `stone.args`, which returns a new list of copies of the program's arguments without its
/// name, from `stone.argc` and `stone.argv`, which `main` saves on entry. It needs the list and
/// string runtimes.
pub fn args_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the list, x20 its length, and x21 the argument being copied
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.args:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tadrp\tx9, stone.argc");
    r#gen.emit("\tldrsw\tx0, [x9, :lo12:stone.argc]");
    // a program started with no name at all still has no arguments
    r#gen.emit("\tsub\tx0, x0, #1");
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tcsel\tx0, x0, xzr, ge");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx1, #1"); // a list of strings
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx19, x0");
    // each argument is copied into a counted string, filling the list from the end
    r#gen.emit(".Largs_loop:");
    r#gen.emit("\tcbz\tx20, .Largs_done");
    r#gen.emit("\tadrp\tx9, stone.argv");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.argv]");
    r#gen.emit("\tldr\tx21, [x9, x20, lsl #3]");
    r#gen.emit("\tsub\tx20, x20, #1");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tstr\tx0, [x9, x20, lsl #3]");
    r#gen.emit("\tb\t.Largs_loop");
    r#gen.emit(".Largs_done:");
    r#gen.emit("\tmov\tx0, x19");
    pop_frame(r#gen, &saved);
}

/// Emits the routines behind `int(str)` and `float(str)`, which follow `stdlib::parse_int` and
/// `stdlib::parse_float`, and stop the program through `stone.fail` with the same messages, each
/// emitted only if named in `used`. They need the string runtime.
///
/// - `stone.parse_int` returns the int in the string in `x0`
/// - `stone.parse_float` returns the bits of the float in the string in `x0`, checking the
///   grammar before `stone.decimal_to_float` reads it
/// - `stone.fail_quoted` stops the program with the message `x0` followed by the string `x1`
///   and a closing quote
pub fn parse_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_int_invalid:");
    r#gen.emit("\t.string \"invalid literal for int() with base 10: '\"");
    r#gen.emit(".Lstone_int_range:");
    r#gen.emit("\t.string \"int() argument out of range: '\"");
    r#gen.emit(".Lstone_float_invalid:");
    r#gen.emit("\t.string \"could not convert string to float: '\"");
    r#gen.emit(".Lstone_quote:");
    r#gen.emit("\t.string \"'\"");
    r#gen.emit(".Lstone_infinity:");
    r#gen.emit("\t.string \"infinity\"");
    r#gen.emit(".Lstone_parse_inf:");
    r#gen.emit("\t.string \"inf\"");
    r#gen.emit(".Lstone_parse_nan:");
    r#gen.emit("\t.string \"nan\"");
    r#gen.emit("\t.text");

    r#gen.emit("stone.fail_quoted:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tbl\tstone.str_concat");
    address(r#gen, "x1", ".Lstone_quote");
    r#gen.emit("\tbl\tstone.str_concat");
    r#gen.emit("\tb\tstone.fail");

    if used.contains(&"stone.parse_int") {
        // x0 keeps the text, x9 is the cursor, x12 where the digits start, x13 the value so far,
        // negated so that the most negative int fits, x11 whether a minus sign came first, and x14
        // whether the value overflowed
        r#gen.emit("stone.parse_int:");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit(".Lparse_int_lead:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        jump_if_space(r#gen, "w10", ".Lparse_int_lead");
        r#gen.emit("\tsub\tx9, x9, #1");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit("\tcmp\tw10, #43"); // '+'
        r#gen.emit("\tb.eq\t.Lparse_int_sign");
        r#gen.emit("\tcmp\tw10, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lparse_int_digits");
        r#gen.emit("\tmov\tx11, #1");
        r#gen.emit(".Lparse_int_sign:");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit(".Lparse_int_digits:");
        r#gen.emit("\tmov\tx12, x9");
        r#gen.emit("\tmov\tx13, #0");
        r#gen.emit("\tmov\tx14, #0");
        r#gen.emit("\tmov\tx15, #10");
        r#gen.emit(".Lparse_int_digit:");
        r#gen.emit("\tldrb\tw10, [x9]");
        r#gen.emit("\tsub\tw10, w10, #48"); // '0'
        r#gen.emit("\tcmp\tw10, #9");
        r#gen.emit("\tb.hi\t.Lparse_int_digits_done");
        r#gen.emit("\tadd\tx9, x9, #1");
        // the product overflowed if its high half is not just the sign of its low half
        r#gen.emit("\tsmulh\tx17, x13, x15");
        r#gen.emit("\tmul\tx13, x13, x15");
        r#gen.emit("\tcmp\tx17, x13, asr #63");
        r#gen.emit("\tb.ne\t.Lparse_int_overflow");
        r#gen.emit("\tsubs\tx13, x13, x10");
        r#gen.emit("\tb.vc\t.Lparse_int_digit");
        // the digits are still read, since text after them makes the error a different one
        r#gen.emit(".Lparse_int_overflow:");
        r#gen.emit("\tmov\tx14, #1");
        r#gen.emit("\tb\t.Lparse_int_digit");
        r#gen.emit(".Lparse_int_digits_done:");
        r#gen.emit("\tcmp\tx9, x12"); // no digits
        r#gen.emit("\tb.eq\t.Lparse_int_invalid");
        // only whitespace may follow, which is checked before overflow, as in stdlib::parse_int
        r#gen.emit(".Lparse_int_trailing:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        r#gen.emit("\tcbz\tw10, .Lparse_int_end");
        jump_if_space(r#gen, "w10", ".Lparse_int_trailing");
        r#gen.emit(".Lparse_int_invalid:");
        r#gen.emit("\tmov\tx1, x0");
        address(r#gen, "x0", ".Lstone_int_invalid");
        r#gen.emit("\tb\tstone.fail_quoted");
        r#gen.emit(".Lparse_int_end:");
        r#gen.emit("\tcbnz\tx14, .Lparse_int_range");
        r#gen.emit("\tcbnz\tx11, .Lparse_int_done");
        r#gen.emit("\tnegs\tx13, x13");
        r#gen.emit("\tb.vs\t.Lparse_int_range"); // 9223372036854775808 has no positive int
        r#gen.emit(".Lparse_int_done:");
        r#gen.emit("\tmov\tx0, x13");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_range:");
        r#gen.emit("\tmov\tx1, x0");
        address(r#gen, "x0", ".Lstone_int_range");
        r#gen.emit("\tb\tstone.fail_quoted");
    }

    if used.contains(&"stone.parse_float") {
        // x19 holds the text, x20 the cursor, and x21 a count of digits
        let saved = ["x19", "x20", "x21"];
        r#gen.emit("stone.parse_float:");
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tmov\tx20, x0");
        r#gen.emit(".Lparse_float_lead:");
        r#gen.emit("\tldrb\tw10, [x20], #1");
        jump_if_space(r#gen, "w10", ".Lparse_float_lead");
        r#gen.emit("\tsub\tx20, x20, #1");
        r#gen.emit("\tcmp\tw10, #43"); // '+'
        r#gen.emit("\tb.eq\t.Lparse_float_sign");
        r#gen.emit("\tcmp\tw10, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lparse_float_named");
        r#gen.emit(".Lparse_float_sign:");
        r#gen.emit("\tadd\tx20, x20, #1");
        // infinity before inf, so the longer name is taken whole
        r#gen.emit(".Lparse_float_named:");
        for (name, length) in [
            (".Lstone_infinity", 8),
            (".Lstone_parse_inf", 3),
            (".Lstone_parse_nan", 3),
        ] {
            // setting bit 5 lowercases a letter, and turns nothing else into one
            let next = format!("{name}_next");
            address(r#gen, "x9", name);
            r#gen.emit("\tmov\tx10, #0");
            r#gen.emit(&format!("{name}_loop:"));
            r#gen.emit("\tldrb\tw11, [x20, x10]");
            r#gen.emit("\torr\tw11, w11, #32");
            r#gen.emit("\tldrb\tw17, [x9, x10]");
            r#gen.emit("\tcmp\tw11, w17");
            r#gen.emit(&format!("\tb.ne\t{next}"));
            r#gen.emit("\tadd\tx10, x10, #1");
            r#gen.emit(&format!("\tcmp\tx10, #{length}"));
            r#gen.emit(&format!("\tb.ne\t{name}_loop"));
            r#gen.emit(&format!("\tadd\tx20, x20, #{length}"));
            r#gen.emit("\tb\t.Lparse_float_trailing");
            r#gen.emit(&format!("{next}:"));
        }
        // digits, then an optional point and more digits, with at least one digit in all
        r#gen.emit("\tmov\tx21, #0");
        let digits = |r#gen: &mut dyn AssemblyGenerator, name: &str| {
            r#gen.emit(&format!(".Lparse_float_{name}:"));
            r#gen.emit("\tldrb\tw10, [x20]");
            r#gen.emit("\tsub\tw10, w10, #48"); // '0'
            r#gen.emit("\tcmp\tw10, #9");
            r#gen.emit(&format!("\tb.hi\t.Lparse_float_{name}_done"));
            r#gen.emit("\tadd\tx20, x20, #1");
            r#gen.emit("\tadd\tx21, x21, #1");
            r#gen.emit(&format!("\tb\t.Lparse_float_{name}"));
            r#gen.emit(&format!(".Lparse_float_{name}_done:"));
        };
        digits(r#gen, "whole");
        r#gen.emit("\tldrb\tw10, [x20]");
        r#gen.emit("\tcmp\tw10, #46"); // '.'
        r#gen.emit("\tb.ne\t.Lparse_float_mantissa");
        r#gen.emit("\tadd\tx20, x20, #1");
        digits(r#gen, "fraction");
        r#gen.emit(".Lparse_float_mantissa:");
        r#gen.emit("\tcbz\tx21, .Lparse_float_invalid");
        r#gen.emit("\tldrb\tw10, [x20]");
        r#gen.emit("\torr\tw10, w10, #32"); // 'E' becomes 'e'
        r#gen.emit("\tcmp\tw10, #101"); // 'e'
        r#gen.emit("\tb.ne\t.Lparse_float_trailing");
        r#gen.emit("\tadd\tx20, x20, #1");
        r#gen.emit("\tldrb\tw10, [x20]");
        r#gen.emit("\tcmp\tw10, #43"); // '+'
        r#gen.emit("\tb.eq\t.Lparse_float_exponent_sign");
        r#gen.emit("\tcmp\tw10, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lparse_float_exponent");
        r#gen.emit(".Lparse_float_exponent_sign:");
        r#gen.emit("\tadd\tx20, x20, #1");
        r#gen.emit(".Lparse_float_exponent:");
        r#gen.emit("\tmov\tx21, #0");
        digits(r#gen, "exponent_digits");
        r#gen.emit("\tcbz\tx21, .Lparse_float_invalid");
        r#gen.emit(".Lparse_float_trailing:");
        r#gen.emit("\tldrb\tw10, [x20], #1");
        r#gen.emit("\tcbz\tw10, .Lparse_float_valid");
        jump_if_space(r#gen, "w10", ".Lparse_float_trailing");
        r#gen.emit(".Lparse_float_invalid:");
        address(r#gen, "x0", ".Lstone_float_invalid");
        r#gen.emit("\tmov\tx1, x19");
        r#gen.emit("\tb\tstone.fail_quoted");
        r#gen.emit(".Lparse_float_valid:");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.decimal_to_float");
        pop_frame(r#gen, &saved);
    }
}

/// Emits `str` for ints and bools. Floats have `stone.str_float` in [`str_float_runtime`].
///
/// - `stone.str_int` returns the int in `x0` as a new string, written like `stone.print_int`
/// - `stone.str_bool` returns a constant `true` if `x0` is nonzero and `false` otherwise
pub fn conversion_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_str_true", "true");
    immortal_string(r#gen, ".Lstone_str_false", "false");
    r#gen.emit("\t.text");

    // digits are written backwards from the terminator at [sp, #31], and x19 and x20 hold the
    // text and its size across stone.alloc
    let saved = ["x19", "x20"];
    r#gen.emit("stone.str_int:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tsub\tsp, sp, #32");
    r#gen.emit("\tadd\tx1, sp, #31");
    r#gen.emit("\tstrb\twzr, [x1]");
    r#gen.emit("\tmov\tx9, x0");
    r#gen.emit("\tmov\tx10, #0"); // negative flag
    r#gen.emit("\tcmp\tx9, #0");
    r#gen.emit("\tb.ge\t.Lstr_int_digits");
    // negating the minimum leaves it unchanged, which unsigned division still reads correctly
    r#gen.emit("\tneg\tx9, x9");
    r#gen.emit("\tmov\tx10, #1");
    r#gen.emit(".Lstr_int_digits:");
    r#gen.emit("\tmov\tx11, #10");
    r#gen.emit(".Lstr_int_loop:");
    r#gen.emit("\tudiv\tx12, x9, x11");
    r#gen.emit("\tmsub\tx13, x12, x11, x9");
    r#gen.emit("\tadd\tx13, x13, #48"); // '0'
    r#gen.emit("\tstrb\tw13, [x1, #-1]!");
    r#gen.emit("\tmov\tx9, x12");
    r#gen.emit("\tcbnz\tx9, .Lstr_int_loop");
    r#gen.emit("\tcbz\tx10, .Lstr_int_copy");
    r#gen.emit("\tmov\tx13, #45"); // '-'
    r#gen.emit("\tstrb\tw13, [x1, #-1]!");
    r#gen.emit(".Lstr_int_copy:");
    r#gen.emit("\tmov\tx19, x1");
    r#gen.emit("\tadd\tx20, sp, #32");
    r#gen.emit("\tsub\tx20, x20, x19"); // the size, counting the terminator
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx9, x0");
    copy_bytes(r#gen, "x9", "x19", "x20", ".Lstr_int_copy_bytes");
    pop_frame(r#gen, &saved);

    r#gen.emit("stone.str_bool:");
    r#gen.emit("\tcmp\tx0, #0");
    address(r#gen, "x0", ".Lstone_str_true");
    address(r#gen, "x9", ".Lstone_str_false");
    r#gen.emit("\tcsel\tx0, x0, x9, ne");
    r#gen.emit("\tret");
}

/// Emits the `strip` and `split` methods, which follow `stdlib::strip`, `stdlib::split_whitespace`,
/// and `stdlib::split`, each emitted only if named in `used`. They need the string runtime, the
/// `split` routines need the list runtime too, and `stone.str_split` jumps to `empty_separator`, a
/// failure label, when given an empty separator.
///
/// - `stone.str_strip` returns the string in `x0` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `x0` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `x0` between each `x1`
pub fn string_methods(r#gen: &mut dyn AssemblyGenerator, used: &[&str], empty_separator: &str) {
    let uses = |label: &str| used.contains(&label);
    if uses("stone.str_strip") {
        // x19 holds the start and x20 the end
        let saved = ["x19", "x20"];
        r#gen.emit("stone.str_strip:");
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit(".Lstr_strip_lead:");
        r#gen.emit("\tldrb\tw10, [x19], #1");
        jump_if_space(r#gen, "w10", ".Lstr_strip_lead");
        r#gen.emit("\tsub\tx19, x19, #1");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_len");
        r#gen.emit("\tadd\tx20, x19, x0");
        r#gen.emit(".Lstr_strip_trail:");
        r#gen.emit("\tcmp\tx20, x19");
        r#gen.emit("\tb.eq\t.Lstr_strip_copy");
        r#gen.emit("\tldrb\tw10, [x20, #-1]!");
        jump_if_space(r#gen, "w10", ".Lstr_strip_trail");
        r#gen.emit("\tadd\tx20, x20, #1");
        r#gen.emit(".Lstr_strip_copy:");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tsub\tx1, x20, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        pop_frame(r#gen, &saved);
    }
    if uses("stone.str_split_ws") {
        // x19 holds the cursor, x20 the list, and x21 where the piece started
        let saved = ["x19", "x20", "x21"];
        r#gen.emit("stone.str_split_ws:");
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tmov\tx1, #1"); // a list of strings
        r#gen.emit("\tbl\tstone.list_new");
        r#gen.emit("\tmov\tx20, x0");
        r#gen.emit(".Lstr_split_ws_skip:");
        r#gen.emit("\tldrb\tw10, [x19]");
        r#gen.emit("\tcbz\tw10, .Lstr_split_ws_done");
        r#gen.emit("\tadd\tx19, x19, #1");
        jump_if_space(r#gen, "w10", ".Lstr_split_ws_skip");
        r#gen.emit("\tsub\tx19, x19, #1");
        r#gen.emit("\tmov\tx21, x19");
        r#gen.emit(".Lstr_split_ws_word:");
        r#gen.emit("\tldrb\tw10, [x19]");
        r#gen.emit("\tcbz\tw10, .Lstr_split_ws_piece");
        jump_if_space(r#gen, "w10", ".Lstr_split_ws_piece");
        r#gen.emit("\tadd\tx19, x19, #1");
        r#gen.emit("\tb\t.Lstr_split_ws_word");
        r#gen.emit(".Lstr_split_ws_piece:");
        r#gen.emit("\tmov\tx0, x21");
        r#gen.emit("\tsub\tx1, x19, x21");
        r#gen.emit("\tbl\tstone.str_slice");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tbl\tstone.list_append");
        r#gen.emit("\tb\t.Lstr_split_ws_skip");
        r#gen.emit(".Lstr_split_ws_done:");
        r#gen.emit("\tmov\tx0, x20");
        pop_frame(r#gen, &saved);
    }
    if uses("stone.str_split") {
        // x19 holds where the piece starts, x20 the list, x21 the separator, x22 its length, and x23
        // where it was found
        let saved = ["x19", "x20", "x21", "x22", "x23"];
        // returns where the string in x1 first appears in the one in x0, or 0, like strstr
        r#gen.emit("stone.str_find:");
        r#gen.emit("\tmov\tx9, #0");
        r#gen.emit(".Lstr_find_compare:");
        r#gen.emit("\tldrb\tw10, [x1, x9]");
        r#gen.emit("\tcbz\tw10, .Lstr_find_found");
        r#gen.emit("\tldrb\tw11, [x0, x9]");
        r#gen.emit("\tcmp\tw10, w11");
        r#gen.emit("\tb.ne\t.Lstr_find_next");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit("\tb\t.Lstr_find_compare");
        r#gen.emit(".Lstr_find_next:");
        r#gen.emit("\tldrb\tw11, [x0]");
        r#gen.emit("\tcbz\tw11, .Lstr_find_none");
        r#gen.emit("\tadd\tx0, x0, #1");
        r#gen.emit("\tb\tstone.str_find");
        r#gen.emit(".Lstr_find_found:");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_find_none:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");

        r#gen.emit("stone.str_split:");
        r#gen.emit("\tldrb\tw9, [x1]");
        r#gen.emit(&format!("\tcbz\tw9, {empty_separator}"));
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tmov\tx21, x1");
        r#gen.emit("\tmov\tx0, x1");
        r#gen.emit("\tbl\tstone.str_len");
        r#gen.emit("\tmov\tx22, x0");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tmov\tx1, #1"); // a list of strings
        r#gen.emit("\tbl\tstone.list_new");
        r#gen.emit("\tmov\tx20, x0");
        r#gen.emit(".Lstr_split_find:");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tmov\tx1, x21");
        r#gen.emit("\tbl\tstone.str_find");
        r#gen.emit("\tcbz\tx0, .Lstr_split_last");
        r#gen.emit("\tmov\tx23, x0");
        r#gen.emit("\tsub\tx1, x0, x19");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tbl\tstone.list_append");
        r#gen.emit("\tadd\tx19, x23, x22");
        r#gen.emit("\tb\t.Lstr_split_find");
        // whatever follows the last separator is a piece too, even if empty
        r#gen.emit(".Lstr_split_last:");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_len");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tbl\tstone.list_append");
        r#gen.emit("\tmov\tx0, x20");
        pop_frame(r#gen, &saved);
    }
}

/// Emits the routines behind the `os` module that the program calls, each named in `used`, under
/// the same labels and contracts as `x64::builtins::os_runtime`, with arguments in `x0` and
/// results in `x0`. `cwd_failure` is the failure label for `stone.os_cwd`, needed only if it is
/// used. The routines that return strings need the string runtime.
pub fn os_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str], cwd_failure: &str) {
    let uses = |label: &str| used.contains(&label);
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_os_empty", "");
    immortal_string(r#gen, ".Lstone_os_platform", "linux");
    immortal_string(r#gen, ".Lstone_os_arch", "aarch64");
    r#gen.emit("\t.text");

    if uses("stone.os_env") || uses("stone.os_has_env") {
        // x19 holds the name across strchr
        // a name that is empty or holds = is never set, though it could match an entry
        r#gen.emit("stone.os_getenv:");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit("\tldrb\tw10, [x9]");
        r#gen.emit("\tcbz\tw10, .Los_getenv_unset");
        r#gen.emit(".Los_getenv_scan:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        r#gen.emit("\tcbz\tw10, stone.getenv");
        r#gen.emit("\tcmp\tw10, #61"); // '='
        r#gen.emit("\tb.ne\t.Los_getenv_scan");
        r#gen.emit(".Los_getenv_unset:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }
    if uses("stone.os_env") {
        r#gen.emit("stone.os_env:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.os_getenv");
        r#gen.emit("\tcbz\tx0, .Los_env_unset");
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Los_env_unset:");
        address(r#gen, "x0", ".Lstone_os_empty");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_has_env") {
        r#gen.emit("stone.os_has_env:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.os_getenv");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit("\tcset\tx0, ne");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_env") || uses("stone.os_hostname") || uses("stone.os_cwd") {
        // returns a counted copy of the C string in x0, which x19 holds across str_len
        r#gen.emit("stone.os_copy:");
        push_frame(r#gen, &["x19"]);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tbl\tstone.str_len");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        pop_frame(r#gen, &["x19"]);
    }
    if uses("stone.os_platform") {
        r#gen.emit("stone.os_platform:");
        address(r#gen, "x0", ".Lstone_os_platform");
        r#gen.emit("\tret");
    }
    if uses("stone.os_arch") {
        r#gen.emit("stone.os_arch:");
        address(r#gen, "x0", ".Lstone_os_arch");
        r#gen.emit("\tret");
    }
    if uses("stone.os_hostname") {
        // uname fills six fields of 65 bytes, and the host name is the second
        r#gen.emit("stone.os_hostname:");
        push_frame(r#gen, &[]);
        r#gen.emit(&format!("\tsub\tsp, sp, #{UTSNAME_SIZE}"));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit("\tmov\tx8, #160"); // sys_uname
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\tcbnz\tx0, .Los_hostname_empty");
        r#gen.emit(&format!("\tadd\tx0, sp, #{UTSNAME_FIELD}"));
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Los_hostname_empty:");
        address(r#gen, "x0", ".Lstone_os_empty");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_cpu_count") {
        r#gen.emit("\t.section\t.rodata");
        r#gen.emit(".Lstone_cpu_online:");
        r#gen.emit(&format!("\t.string \"{CPU_ONLINE}\""));
        r#gen.emit("\t.text");
        // the file lists ranges like 0-3,6, read into 256 bytes of stack, which x9 walks
        let digits = |r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str| {
            r#gen.emit(&format!("\tmov\t{reg}, #0"));
            r#gen.emit("\tmov\tx17, #10");
            r#gen.emit(&format!("{label}:"));
            r#gen.emit("\tldrb\tw16, [x9]");
            r#gen.emit("\tsub\tw16, w16, #48"); // '0'
            r#gen.emit("\tcmp\tw16, #9");
            r#gen.emit(&format!("\tb.hi\t{label}_done"));
            r#gen.emit("\tadd\tx9, x9, #1");
            r#gen.emit(&format!("\tmadd\t{reg}, {reg}, x17, x16"));
            r#gen.emit(&format!("\tb\t{label}"));
            r#gen.emit(&format!("{label}_done:"));
        };
        r#gen.emit("stone.os_cpu_count:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tsub\tsp, sp, #256");
        r#gen.emit("\tmov\tx0, #-100"); // AT_FDCWD
        address(r#gen, "x1", ".Lstone_cpu_online");
        r#gen.emit("\tmov\tx2, #0"); // O_RDONLY
        r#gen.emit("\tmov\tx3, #0");
        r#gen.emit("\tmov\tx8, #56"); // sys_openat
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\ttbnz\tx0, #63, .Lcpu_count_one");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit("\tmov\tx1, sp");
        r#gen.emit("\tmov\tx2, #255");
        r#gen.emit("\tmov\tx8, #63"); // sys_read
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\tmov\tx10, x0");
        r#gen.emit("\tmov\tx0, x9");
        r#gen.emit("\tmov\tx8, #57"); // sys_close
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\tcmp\tx10, #1");
        r#gen.emit("\tb.lt\t.Lcpu_count_one");
        r#gen.emit("\tstrb\twzr, [sp, x10]");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tmov\tx9, sp");
        r#gen.emit(".Lcpu_count_range:");
        digits(r#gen, "x10", ".Lcpu_count_first");
        r#gen.emit("\tmov\tx11, x10");
        r#gen.emit("\tldrb\tw16, [x9]");
        r#gen.emit("\tcmp\tw16, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lcpu_count_add");
        r#gen.emit("\tadd\tx9, x9, #1");
        digits(r#gen, "x11", ".Lcpu_count_last");
        r#gen.emit(".Lcpu_count_add:");
        r#gen.emit("\tsub\tx11, x11, x10");
        r#gen.emit("\tadd\tx0, x0, x11");
        r#gen.emit("\tadd\tx0, x0, #1");
        r#gen.emit("\tldrb\tw16, [x9]");
        r#gen.emit("\tcmp\tw16, #44"); // ','
        r#gen.emit("\tb.ne\t.Lcpu_count_done");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit("\tb\t.Lcpu_count_range");
        r#gen.emit(".Lcpu_count_done:");
        r#gen.emit("\tcmp\tx0, #1");
        r#gen.emit("\tb.ge\t.Lcpu_count_return");
        r#gen.emit(".Lcpu_count_one:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit(".Lcpu_count_return:");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_pid") {
        r#gen.emit("stone.os_pid:");
        r#gen.emit("\tmov\tx8, #172"); // sys_getpid
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\tret");
    }
    if uses("stone.os_cwd") {
        // the kernel writes the path into a buffer on the stack, and marks a directory that is
        // no longer reachable from the root by not starting it with /, which glibc treats as
        // an error too
        r#gen.emit("stone.os_cwd:");
        push_frame(r#gen, &[]);
        r#gen.emit(&format!("\tsub\tsp, sp, #{PATH_BUFFER}"));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit(&format!("\tmov\tx1, #{PATH_BUFFER}"));
        r#gen.emit("\tmov\tx8, #17"); // sys_getcwd
        r#gen.emit("\tsvc\t#0");
        r#gen.emit(&format!("\ttbnz\tx0, #63, {cwd_failure}"));
        r#gen.emit("\tldrb\tw9, [sp]");
        r#gen.emit("\tcmp\tw9, #47"); // '/'
        r#gen.emit(&format!("\tb.ne\t{cwd_failure}"));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_exit") {
        r#gen.emit("stone.os_exit:");
        r#gen.emit("\tmov\tx8, #94"); // sys_exit_group
        r#gen.emit("\tsvc\t#0");
    }
    for (label, clock) in [("stone.os_time", 0), ("stone.os_clock", 1)] {
        if !uses(label) {
            continue;
        }
        // clock_gettime fills the seconds at [sp] and the nanoseconds at [sp, #8]
        r#gen.emit(&format!("{label}:"));
        push_frame(r#gen, &[]);
        r#gen.emit("\tsub\tsp, sp, #16");
        r#gen.emit(&format!("\tmov\tx0, #{clock}"));
        r#gen.emit("\tmov\tx1, sp");
        r#gen.emit("\tmov\tx8, #113"); // sys_clock_gettime
        r#gen.emit("\tsvc\t#0");
        r#gen.emit("\tldp\tx9, x10, [sp]");
        r#gen.emit("\tscvtf\td0, x9");
        r#gen.emit("\tscvtf\td1, x10");
        // 1e9
        r#gen.emit("\tmovz\tx11, #0x41cd, lsl #48");
        r#gen.emit("\tmovk\tx11, #0xcd65, lsl #32");
        r#gen.emit("\tfmov\td2, x11");
        r#gen.emit("\tfdiv\td1, d1, d2");
        r#gen.emit("\tfadd\td0, d0, d1");
        r#gen.emit("\tfmov\tx0, d0");
        pop_frame(r#gen, &[]);
    }
}
