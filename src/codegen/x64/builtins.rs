use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{CPU_ONLINE, PATH_BUFFER, UTSNAME_FIELD, UTSNAME_SIZE};
use crate::codegen::context::{
    HEAP_REGION, LARGEST_CLASS, LEAK_PREFIX, LEAK_SUFFIX, OUT_OF_MEMORY, OUT_OF_MEMORY_LENGTH,
    SMALLEST_CLASS, STDIN_BUFFER,
};
pub use crate::codegen::context::{IMMORTAL, immortal_string};

/// Emits `_start`, where the kernel starts the program, with the argument count at `[rsp]`, the
/// arguments after it, then a null, then the environment. It calls `main` with the count in
/// `rdi`, the arguments in `rsi`, and the environment in `rdx`, as a C program's `main` gets them,
/// then ends the program with the status `main` returns.
pub fn start_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.globl\t_start");
    r#gen.emit("_start:");
    r#gen.emit("\txor\tebp, ebp"); // the outermost frame
    r#gen.emit("\tmov\trdi, QWORD PTR [rsp]");
    r#gen.emit("\tlea\trsi, [rsp + 8]");
    r#gen.emit("\tlea\trdx, [rsi + rdi * 8 + 8]");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tcall\tmain");
    r#gen.emit("\tmov\tedi, eax");
    r#gen.emit("\tmov\teax, 231"); // sys_exit_group
    r#gen.emit("\tsyscall");
}

/// Emits the allocator, which takes memory from the system with the `mmap` syscall instead of
/// libc's `malloc`. Every block starts with an 8-byte header and is a power of two of
/// [`SMALLEST_CLASS`] through [`LARGEST_CLASS`] bytes, whose exponent (the block's class) is in
/// the header. Freed blocks go on a list per class for the next allocation of that class, and
/// new ones are cut from regions of [`HEAP_REGION`] bytes, which are never unmapped. A block
/// too large for any class is a mapping of its own, whose header holds its length instead,
/// which is always more than any class.
///
/// - `stone.mem_alloc` returns at least `rdi` bytes, aligned to 8
/// - `stone.mem_free` frees the bytes at `rax`
/// - `stone.mem_realloc` returns at least `rsi` bytes holding what the `rdi` bytes held, moving
///   them only if their block is too small
///
/// All three clobber only `rax`, `rcx`, and `rdx`, so the free routines and `stone.list_copy`
/// can call them. If the system has no memory to give, the program stops with
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
    r#gen.emit("\tlea\trcx, [rdi + 7]");
    r#gen.emit("\tbsr\trcx, rcx");
    r#gen.emit("\tinc\trcx");
    r#gen.emit(&format!("\tcmp\trcx, {SMALLEST_CLASS}"));
    r#gen.emit("\tjae\t.Lmem_alloc_class");
    r#gen.emit(&format!("\tmov\tecx, {SMALLEST_CLASS}"));
    r#gen.emit(".Lmem_alloc_class:");
    r#gen.emit(&format!("\tcmp\trcx, {LARGEST_CLASS}"));
    r#gen.emit("\tja\t.Lmem_alloc_large");
    r#gen.emit("\tlea\trdx, [rip + stone.free_blocks]");
    r#gen.emit("\tlea\trdx, [rdx + rcx * 8]");
    r#gen.emit("\tmov\trax, QWORD PTR [rdx]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lmem_alloc_cut");
    // a freed block keeps its class in its header
    r#gen.emit("\tmov\trcx, QWORD PTR [rax + 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdx], rcx");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_alloc_cut:");
    r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.heap_next]");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tshl\trdx, cl");
    r#gen.emit("\tadd\trdx, rax");
    r#gen.emit("\tcmp\trdx, QWORD PTR [rip + stone.heap_end]");
    r#gen.emit("\tja\t.Lmem_alloc_region");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.heap_next], rdx");
    r#gen.emit("\tmov\tQWORD PTR [rax], rcx");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tret");
    // what is left of the old region is too small, and stays unused
    r#gen.emit(".Lmem_alloc_region:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit(&format!("\tmov\tedi, {HEAP_REGION}"));
    r#gen.emit("\tcall\tstone.mem_map");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.heap_next], rax");
    r#gen.emit(&format!("\tadd\trax, {HEAP_REGION}"));
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.heap_end], rax");
    r#gen.emit("\tjmp\tstone.mem_alloc");
    r#gen.emit(".Lmem_alloc_large:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tadd\trdi, 8 + 4095"); // whole pages
    r#gen.emit("\tand\trdi, -4096");
    r#gen.emit("\tcall\tstone.mem_map");
    r#gen.emit("\tmov\tQWORD PTR [rax], rdi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_free:");
    r#gen.emit("\tsub\trax, 8");
    r#gen.emit("\tmov\trcx, QWORD PTR [rax]");
    r#gen.emit(&format!("\tcmp\trcx, {LARGEST_CLASS}"));
    r#gen.emit("\tja\t.Lmem_free_large");
    r#gen.emit("\tlea\trdx, [rip + stone.free_blocks]");
    r#gen.emit("\tlea\trdx, [rdx + rcx * 8]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdx]");
    r#gen.emit("\tmov\tQWORD PTR [rax + 8], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rdx], rax");
    r#gen.emit("\tret");
    // syscalls clobber rcx and r11
    r#gen.emit(".Lmem_free_large:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tpush\tr11");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\tmov\teax, 11"); // sys_munmap
    r#gen.emit("\tsyscall");
    r#gen.emit("\tpop\tr11");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_realloc:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi - 8]");
    r#gen.emit("\tlea\trax, [rsi + 8]"); // the size the block needs
    r#gen.emit(&format!("\tcmp\trcx, {LARGEST_CLASS}"));
    r#gen.emit("\tja\t.Lmem_realloc_large");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tshl\trdx, cl");
    r#gen.emit("\tcmp\trax, rdx");
    r#gen.emit("\tja\t.Lmem_realloc_move");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tret");
    // the old block's rdx bytes, less its header, are copied to a new one
    r#gen.emit(".Lmem_realloc_move:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tmov\trdi, rsi");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tsub\trcx, 8");
    r#gen.emit("\tmov\trsi, QWORD PTR [rsp + 8]");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 16]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tpop\trax");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_realloc_large:");
    r#gen.emit("\tcmp\trax, rcx");
    r#gen.emit("\tja\t.Lmem_realloc_remap");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tret");
    // the kernel moves the pages if it cannot grow the mapping where it is
    r#gen.emit(".Lmem_realloc_remap:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tpush\tr10");
    r#gen.emit("\tpush\tr11");
    r#gen.emit("\tlea\trdx, [rax + 4095]");
    r#gen.emit("\tand\trdx, -4096");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\tsub\trdi, 8");
    r#gen.emit("\tmov\tr10d, 1"); // MREMAP_MAYMOVE
    r#gen.emit("\tmov\teax, 25"); // sys_mremap
    r#gen.emit("\tsyscall");
    r#gen.emit("\tcmp\trax, -4096");
    r#gen.emit("\tja\tstone.out_of_memory");
    r#gen.emit("\tmov\tQWORD PTR [rax], rdx");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tpop\tr11");
    r#gen.emit("\tpop\tr10");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tret");

    // returns rdi new bytes of zeros, a whole number of pages, clobbering only rax, rcx, and rdx
    let saved = ["rdi", "rsi", "r8", "r9", "r10", "r11"];
    r#gen.emit("stone.mem_map:");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit("\txor\tedi, edi"); // anywhere
    r#gen.emit("\tmov\tedx, 3"); // PROT_READ | PROT_WRITE
    r#gen.emit("\tmov\tr10d, 0x22"); // MAP_PRIVATE | MAP_ANONYMOUS
    r#gen.emit("\tmov\tr8, -1"); // no file
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tmov\teax, 9"); // sys_mmap
    r#gen.emit("\tsyscall");
    // the kernel returns an error as -4095 through -1
    r#gen.emit("\tcmp\trax, -4096");
    r#gen.emit("\tja\tstone.out_of_memory");
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tret");

    r#gen.emit("stone.out_of_memory:");
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tmov\tedi, 2"); // stderr
    r#gen.emit("\tlea\trsi, [rip + .Lstone_out_of_memory]");
    r#gen.emit(&format!("\tmov\tedx, {OUT_OF_MEMORY_LENGTH}"));
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\teax, 231"); // sys_exit_group
    r#gen.emit("\tmov\tedi, 1");
    r#gen.emit("\tsyscall");
}

/// Emits the memory runtime. Every string and list `p` is counted: `[p - 8]` holds how many
/// variables, list slots, and temporaries refer to it, and it is freed when that reaches 0.
/// `stone.live` counts the objects allocated and not yet freed.
///
/// - `stone.alloc` returns a new object with room for `rdi` bytes and a count of 1
/// - `stone.free_str` frees the string in `rax`, whose count just reached 0
/// - `stone.free_list` frees the list in `rax`, whose count just reached 0, along with its
///   elements' storage, first releasing each element if its `elem` field says they are counted
/// - `stone.leak_check` exits with an error if the `STONE_LEAK_CHECK` environment variable is set
///   and any object is still allocated, which tests use to find leaks
///
/// The free routines are reached from inline releases that do not count as calls, so they
/// preserve every register except `rax`, `rcx`, `rdx`, and the `xmm` registers. `stone.leak_check`
/// also preserves `rax`, which holds `main`'s exit status. Memory comes from [`allocator`].
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
    r#gen.emit("\tadd\trdi, 8"); // room for the count
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rax], 1");
    r#gen.emit("\tinc\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tret");

    // the count is where the allocation starts
    r#gen.emit("stone.free_str:");
    r#gen.emit("\tsub\trax, 8");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tret");

    // rbx holds the list and r12 the index of the element being released
    r#gen.emit("stone.free_list:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 24], 0");
    r#gen.emit("\tje\t.Lfree_list_storage"); // the elements are not counted
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit(".Lfree_list_loop:");
    r#gen.emit("\tcmp\tr12, QWORD PTR [rbx]");
    r#gen.emit("\tjge\t.Lfree_list_storage");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trax, QWORD PTR [rax + r12 * 8]");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\tdec\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjnz\t.Lfree_list_loop");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 24], 1");
    r#gen.emit("\tje\t.Lfree_list_str");
    // nesting is bounded by the element type, so this recursion is shallow
    r#gen.emit("\tcall\tstone.free_list");
    r#gen.emit("\tjmp\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_str:");
    r#gen.emit("\tcall\tstone.free_str");
    r#gen.emit("\tjmp\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_storage:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tlea\trax, [rbx - 8]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");

    // the count's digits are written backwards into 24 bytes below the saved status
    r#gen.emit("stone.leak_check:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tsub\trsp, 24");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.live], 0");
    r#gen.emit("\tje\t.Lleak_check_done");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_leak_env]");
    r#gen.emit("\tcall\tstone.getenv");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lleak_check_done");
    write_stderr(r#gen, ".Lstone_leak_prefix", LEAK_PREFIX.len());
    r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.live]");
    r#gen.emit("\tlea\trsi, [rbp - 8]");
    r#gen.emit("\tmov\tecx, 10");
    r#gen.emit(".Lleak_check_digit:");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tdl, 48"); // '0'
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], dl");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lleak_check_digit");
    r#gen.emit("\tlea\trdx, [rbp - 8]");
    r#gen.emit("\tsub\trdx, rsi");
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tmov\tedi, 2"); // stderr
    r#gen.emit("\tsyscall");
    write_stderr(r#gen, ".Lstone_leak_suffix", LEAK_SUFFIX.len() + 1);
    r#gen.emit("\tmov\teax, 231"); // sys_exit_group
    r#gen.emit("\tmov\tedi, 1");
    r#gen.emit("\tsyscall");
    r#gen.emit(".Lleak_check_done:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 8]");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
}

/// Emits code that writes the `length` bytes at `label` to stderr, clobbering `rax`, `rcx`,
/// `rdx`, `rsi`, `rdi`, and `r11`.
fn write_stderr(r#gen: &mut dyn AssemblyGenerator, label: &str, length: usize) {
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tmov\tedi, 2"); // stderr
    r#gen.emit(&format!("\tlea\trsi, [rip + {label}]"));
    r#gen.emit(&format!("\tmov\tedx, {length}"));
    r#gen.emit("\tsyscall");
}

/// Emits `stone.getenv`, which returns the value of the environment variable named by the
/// string in `rdi`, or 0 if it is not set, like libc's `getenv`. It reads the environment
/// `main` saved in `stone.envp`, and clobbers `rax`, `rcx`, `rdx`, and `rsi`.
///
/// For example, with `HOME=/root` in the environment, `stone.getenv` of `HOME` returns a
/// pointer to `/root`, and of `HOM` returns 0.
pub fn env_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.getenv:");
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stone.envp]");
    r#gen.emit(".Lgetenv_entry:");
    r#gen.emit("\tmov\trax, QWORD PTR [rdx]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lgetenv_done"); // the list ends with null
    r#gen.emit("\tadd\trdx, 8");
    r#gen.emit("\txor\tecx, ecx");
    // the entry must start with the name, then =
    r#gen.emit(".Lgetenv_compare:");
    r#gen.emit("\tmovzx\tesi, byte ptr [rdi + rcx]");
    r#gen.emit("\ttest\tesi, esi");
    r#gen.emit("\tjz\t.Lgetenv_name_end");
    r#gen.emit("\tcmp\tsil, byte ptr [rax + rcx]");
    r#gen.emit("\tjne\t.Lgetenv_entry");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lgetenv_compare");
    r#gen.emit(".Lgetenv_name_end:");
    r#gen.emit("\tcmp\tbyte ptr [rax + rcx], 61"); // '='
    r#gen.emit("\tjne\t.Lgetenv_entry");
    r#gen.emit("\tlea\trax, [rax + rcx + 1]");
    r#gen.emit(".Lgetenv_done:");
    r#gen.emit("\tret");
}

/// Emits the routines behind `print`, each writing one value to stdout with no newline, so a call
/// like `print("n", 1)` writes `n`, a space, `1`, and a newline with four calls.
///
/// - `stone.print_int` writes the signed integer in `rdi`
/// - `stone.print_str` writes the null-terminated string `rdi` points to
/// - `stone.print_bool` writes `true` if `rdi` is nonzero and `false` otherwise
/// - `stone.print_none` writes `none`
/// - `stone.print_char` writes the byte in `dil`
/// - `stone.print_str_quoted` writes the string `rdi` points to in single quotes, as a
///   printed list shows its strings
///
/// They write with the `write` syscall and clobber `rax`, `rcx`, `rdx`, `rsi`, `rdi`, `r8`, and
/// `r11`.
pub fn print(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_true:");
    r#gen.emit("\t.string \"true\"");
    r#gen.emit(".Lstone_false:");
    r#gen.emit("\t.string \"false\"");
    r#gen.emit(".Lstone_none:");
    r#gen.emit("\t.string \"none\"");
    r#gen.emit("\t.text");

    // digits are written backwards from rbp, so 32 bytes holds all 20 digits and a sign
    r#gen.emit("stone.print_int:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tmov\trsi, rbp");
    r#gen.emit("\txor\tr8, r8"); // negative flag
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjns\t.Lprint_int_digits");
    // negating the minimum leaves it unchanged, which unsigned division still reads correctly
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr8, 1");
    r#gen.emit(".Lprint_int_digits:");
    r#gen.emit("\tmov\trcx, 10");
    r#gen.emit(".Lprint_int_loop:");
    // at least one digit, so zero prints as 0
    r#gen.emit("\txor\trdx, rdx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tdl, 48"); // '0'
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], dl");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lprint_int_loop");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lprint_int_write");
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], 45"); // '-'
    r#gen.emit(".Lprint_int_write:");
    r#gen.emit("\tmov\trdx, rbp");
    r#gen.emit("\tsub\trdx, rsi");
    r#gen.emit("\tmov\trax, 1"); // sys_write
    r#gen.emit("\tmov\trdi, 1"); // stdout
    r#gen.emit("\tsyscall");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_str:");
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit("\txor\trdx, rdx");
    r#gen.emit(".Lprint_str_len:");
    r#gen.emit("\tcmp\tbyte ptr [rsi + rdx], 0");
    r#gen.emit("\tje\t.Lprint_str_write");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tjmp\t.Lprint_str_len");
    r#gen.emit(".Lprint_str_write:");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjz\t.Lprint_str_done"); // empty string
    r#gen.emit("\tmov\trax, 1"); // sys_write
    r#gen.emit("\tmov\trdi, 1"); // stdout
    r#gen.emit("\tsyscall");
    r#gen.emit(".Lprint_str_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_bool:");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_true]");
    r#gen.emit("\tjnz\tstone.print_str");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_false]");
    r#gen.emit("\tjmp\tstone.print_str");

    r#gen.emit("stone.print_none:");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_none]");
    r#gen.emit("\tjmp\tstone.print_str");

    r#gen.emit("stone.print_char:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 16");
    r#gen.emit("\tmov\tbyte ptr [rbp - 1], dil");
    r#gen.emit("\tlea\trsi, [rbp - 1]");
    r#gen.emit("\tmov\trdx, 1");
    r#gen.emit("\tmov\trax, 1"); // sys_write
    r#gen.emit("\tmov\trdi, 1"); // stdout
    r#gen.emit("\tsyscall");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");

    // strings inside a printed list are quoted, like ['a']
    r#gen.emit("stone.print_str_quoted:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tmov\trdi, 39"); // '\''
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit("\tmov\trdi, 39");
    r#gen.emit("\tjmp\tstone.print_char");
}

/// Emits `stone.str_float`, which returns the float whose bits are in `rdi` as a new string, the
/// way `stone.print_float` prints it. It needs `floats::float_runtime` and the memory runtime.
pub fn str_float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the bits, then the new string
    r#gen.emit("stone.str_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tedi, 64");
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tcall\tstone.format_float");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tmov\trbx, QWORD PTR [rbp - 8]");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
}

/// Emits the string runtime. Strings are null-terminated and counted (see [`memory_runtime`]), and
/// `+` makes a new one with `stone.alloc`.
///
/// - `stone.str_len` returns the length in bytes of the string in `rdi`
/// - `stone.str_eq` returns 1 if the strings in `rdi` and `rsi` hold the same bytes, else 0
/// - `stone.str_concat` returns a new string holding `rdi` followed by `rsi`
/// - `stone.str_slice` returns a new string holding the `rsi` bytes at `rdi`
pub fn string_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.str_len:");
    r#gen.emit("\txor\trax, rax");
    r#gen.emit(".Lstr_len_loop:");
    r#gen.emit("\tcmp\tbyte ptr [rdi + rax], 0");
    r#gen.emit("\tje\t.Lstr_len_done");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Lstr_len_loop");
    r#gen.emit(".Lstr_len_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.str_eq:");
    r#gen.emit(".Lstr_eq_loop:");
    r#gen.emit("\tmov\tal, byte ptr [rdi]");
    r#gen.emit("\tcmp\tal, byte ptr [rsi]");
    r#gen.emit("\tjne\t.Lstr_eq_differ");
    r#gen.emit("\ttest\tal, al");
    r#gen.emit("\tjz\t.Lstr_eq_same"); // both ended together
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tjmp\t.Lstr_eq_loop");
    r#gen.emit(".Lstr_eq_same:");
    r#gen.emit("\tmov\trax, 1");
    r#gen.emit("\tret");
    r#gen.emit(".Lstr_eq_differ:");
    r#gen.emit("\txor\trax, rax");
    r#gen.emit("\tret");

    // r12 and r13 hold the two strings and r14 and r15 their lengths across the calls
    r#gen.emit("stone.str_concat:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tlea\trdi, [r14 + r15 + 1]"); // room for the terminator
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tlea\trcx, [r15 + 1]"); // copy the terminator too
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tlea\trsp, [rbp - 32]");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
    // rbx and r12 hold the bytes and their count across stone.alloc
    r#gen.emit("stone.str_slice:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tlea\trdi, [rsi + 1]"); // room for the terminator
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tbyte ptr [rdi], 0");
    r#gen.emit("\tlea\trsp, [rbp - 16]");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits the list runtime. A list is a pointer to a counted 32-byte header `{len, cap, data,
/// elem}` (see [`memory_runtime`]), where `data` points to `cap` 8-byte elements and `elem` says
/// what they are: 0 for values that are not counted, 1 for strings, and 2 for lists. The routines
/// follow the System V calling convention, and take their memory from [`allocator`].
///
/// - `stone.list_new` returns a list of length `rdi` whose elements the caller fills in, with
///   `elem` set to `rsi`
/// - `stone.list_append` appends `rsi` to list `rdi`, doubling its capacity when full
/// - `stone.list_copy` returns in `rax` a copy of the list in `rax`, retaining each element if
///   they are counted, and drops one reference to the original, which something else still
///   holds. Like the free routines, it is reached from an inline `list_unique` that does not
///   count as a call, so it preserves every register except `rax`, `rcx`, `rdx`, and the `xmm`
///   registers.
///
/// Indexing is generated inline by `X64Generator::list_slot` (in `emit.rs`) rather than called here.
pub fn list_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 its length, and r13 what its elements are
    r#gen.emit("stone.list_new:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tmov\trdi, 32");
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx], r12");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], r13");
    // room for at least 4 elements, so the first appends do not reallocate
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tmov\trcx, 4");
    r#gen.emit("\tcmp\trax, rcx");
    r#gen.emit("\tcmovl\trax, rcx");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 8], rax");
    r#gen.emit("\tlea\trdi, [rax * 8]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], rax");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tlea\trsp, [rbp - 24]");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    r#gen.emit("stone.list_append:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tcmp\trax, QWORD PTR [rbx + 8]");
    r#gen.emit("\tjl\t.Llist_append_store");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 8]");
    r#gen.emit("\tshl\trsi, 1");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 8], rsi");
    r#gen.emit("\tshl\trsi, 3");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], rax");
    r#gen.emit(".Llist_append_store:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tQWORD PTR [rdx + rax * 8], r12");
    r#gen.emit("\tinc\tQWORD PTR [rbx]");
    r#gen.emit("\txor\trax, rax"); // append returns none
    r#gen.emit("\tlea\trsp, [rbp - 16]");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // rbx holds the original, r12 the copy, rdi and rsi their elements, and r8 what they are
    let saved = ["rdi", "rsi", "r8", "r9", "r10", "r11", "rbx", "r12"];
    r#gen.emit("stone.list_copy:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 24]");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Llist_copy_loop:");
    r#gen.emit("\tcmp\trcx, QWORD PTR [rbx]");
    r#gen.emit("\tjge\t.Llist_copy_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + rcx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdi + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Llist_copy_loop"); // the elements are not counted
    r#gen.emit("\tinc\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjmp\t.Llist_copy_loop");
    r#gen.emit(".Llist_copy_done:");
    // the variable or slot that held the original now holds the copy instead
    r#gen.emit("\tdec\tQWORD PTR [rbx - 8]");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `label`, a routine that prints the list in `rdi` as `[a, b]`, calling `element` for each
/// element.
///
/// For example, `print_list(gen, "stone.print_list_int", "stone.print_int")` emits the printer for
/// `list[int]`.
pub fn print_list(r#gen: &mut dyn AssemblyGenerator, label: &str, element: &str) {
    let local = label.replace('.', "_");
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\txor\tr12, r12"); // index
    r#gen.emit("\tmov\trdi, 91"); // '['
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit(&format!(".L{local}_loop:"));
    r#gen.emit("\tcmp\tr12, QWORD PTR [rbx]");
    r#gen.emit(&format!("\tjge\t.L{local}_done"));
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjz\t.L{local}_element"));
    r#gen.emit("\tmov\trdi, 44"); // ','
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tmov\trdi, 32"); // ' '
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit(&format!(".L{local}_element:"));
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rax + r12 * 8]");
    r#gen.emit(&format!("\tcall\t{element}"));
    r#gen.emit("\tinc\tr12");
    r#gen.emit(&format!("\tjmp\t.L{local}_loop"));
    r#gen.emit(&format!(".L{local}_done:"));
    r#gen.emit("\tmov\trdi, 93"); // ']'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits a jump to `label` if the byte zero-extended in `reg`, a 32-bit register other than `eax`,
/// is whitespace as `stdlib::is_space` defines it: a space, or a tab through a carriage return.
/// It clobbers `eax`.
///
/// For example, `jump_if_space(gen, "ecx", ".Lskip")` jumps for `\t` but not for `a` or 0.
pub(super) fn jump_if_space(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tcmp\t{reg}, 32"));
    r#gen.emit(&format!("\tje\t{label}"));
    // tab, newline, vertical tab, form feed, and carriage return are 9 through 13
    r#gen.emit(&format!("\tmov\teax, {reg}"));
    r#gen.emit("\tsub\teax, 9");
    r#gen.emit("\tcmp\teax, 4");
    r#gen.emit(&format!("\tjbe\t{label}"));
}

/// Emits the input routines named in `used`, which read stdin with the `read` syscall into a
/// buffer of [`STDIN_BUFFER`] bytes, `stone.stdin_buffer`, of which the bytes from
/// `stone.stdin_pos` up to `stone.stdin_len` are still to be read. Printing writes with
/// syscalls too, so a prompt shows before the program waits.
///
/// - `stone.input` prints the string in `rdi` unless it is null, then returns the next line of
///   stdin as a new string without its newline, or an empty string at the end of the input. A
///   line that the buffer holds whole is copied straight out of it, and a longer one is first
///   gathered in a block from `stone.mem_alloc`
/// - `stone.eof` returns 1 if stdin has nothing left and 0 otherwise, reading more into the
///   buffer if it is empty
/// - `stone.stdin_fill` reads into the buffer from its start and returns how many bytes it
///   read, or 0 at the end of the input or on an error, as `getline` does
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
    r#gen.emit("\txor\teax, eax"); // sys_read
    r#gen.emit("\txor\tedi, edi"); // stdin
    r#gen.emit("\tlea\trsi, [rip + stone.stdin_buffer]");
    r#gen.emit(&format!("\tmov\tedx, {STDIN_BUFFER}"));
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjg\t.Lstdin_fill_done");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit(".Lstdin_fill_done:");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.stdin_len], rax");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.stdin_pos], 0");
    r#gen.emit("\tret");

    if used.contains(&"stone.eof") {
        r#gen.emit("stone.eof:");
        r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.stdin_pos]");
        r#gen.emit("\tcmp\trax, QWORD PTR [rip + stone.stdin_len]");
        r#gen.emit("\tjb\t.Leof_no");
        r#gen.emit("\tcall\tstone.stdin_fill");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjz\t.Leof_yes");
        r#gen.emit(".Leof_no:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tret");
        r#gen.emit(".Leof_yes:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tret");
    }

    if !used.contains(&"stone.input") {
        return;
    }
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_empty", "");
    r#gen.emit("\t.text");

    // rbx holds the block a long line is gathered in, or 0 before there is one, r12 how many
    // bytes it holds, and r13 how many it has room for
    let saved = ["rbx", "r12", "r13"];
    r#gen.emit("stone.input:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjz\t.Linput_read");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit(".Linput_read:");
    r#gen.emit("\txor\tebx, ebx");
    r#gen.emit("\txor\tr12d, r12d");
    r#gen.emit("\txor\tr13d, r13d");
    r#gen.emit(".Linput_scan:");
    r#gen.emit("\tmov\trsi, QWORD PTR [rip + stone.stdin_pos]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stone.stdin_len]");
    r#gen.emit("\tcmp\trsi, rdx");
    r#gen.emit("\tjb\t.Linput_search");
    r#gen.emit("\tcall\tstone.stdin_fill");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Linput_scan");
    // the input ended, so what was gathered is the last line, unless nothing was
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjnz\t.Linput_gathered");
    r#gen.emit("\tlea\trax, [rip + .Lstone_empty]");
    r#gen.emit("\tjmp\t.Linput_done");
    // rcx looks for a newline from rsi up to rdx in the buffer at rdi
    r#gen.emit(".Linput_search:");
    r#gen.emit("\tlea\trdi, [rip + stone.stdin_buffer]");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit(".Linput_find:");
    r#gen.emit("\tcmp\trcx, rdx");
    r#gen.emit("\tje\t.Linput_partial");
    r#gen.emit("\tcmp\tbyte ptr [rdi + rcx], 10"); // newline
    r#gen.emit("\tje\t.Linput_newline");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Linput_find");
    // the rest of the buffer is part of a longer line
    r#gen.emit(".Linput_partial:");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.stdin_pos], rdx");
    r#gen.emit("\tadd\trdi, rsi");
    r#gen.emit("\tsub\trdx, rsi");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tcall\t.Linput_append");
    r#gen.emit("\tjmp\t.Linput_scan");
    // the line ends at rcx, and the newline is read but left out
    r#gen.emit(".Linput_newline:");
    r#gen.emit("\tlea\trax, [rcx + 1]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.stdin_pos], rax");
    r#gen.emit("\tadd\trdi, rsi");
    r#gen.emit("\tsub\trcx, rsi");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjnz\t.Linput_last_piece");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tjmp\t.Linput_done");
    r#gen.emit(".Linput_last_piece:");
    r#gen.emit("\tcall\t.Linput_append");
    // the gathered line becomes a string, and its block is freed
    r#gen.emit(".Linput_gathered:");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit(".Linput_done:");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // adds the rsi bytes at rdi to the block in rbx, growing it to twice what it must hold
    r#gen.emit(".Linput_append:");
    r#gen.emit("\tlea\tr8, [r12 + rsi]");
    r#gen.emit("\tcmp\tr8, r13");
    r#gen.emit("\tjbe\t.Linput_append_copy");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tlea\tr13, [r8 + r8]");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjnz\t.Linput_append_grow");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tjmp\t.Linput_append_grown");
    r#gen.emit(".Linput_append_grow:");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit(".Linput_append_grown:");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit(".Linput_append_copy:");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit("\tlea\trdi, [rbx + r12]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tr12, r8");
    r#gen.emit("\tret");
}

/// Emits `stone.args`, which returns a new list of copies of the program's arguments without its
/// name, from `stone.argc` and `stone.argv`, which `main` saves on entry. It needs the list and
/// string runtimes.
pub fn args_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list and r12 its length
    r#gen.emit("stone.args:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmovsxd\trdi, DWORD PTR [rip + stone.argc]");
    // a program started with no name at all still has no arguments
    r#gen.emit("\tdec\trdi");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tcmovs\trdi, rax");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tesi, 1"); // a list of strings
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\trbx, rax");
    // each argument is copied into a counted string, filling the list from the end
    r#gen.emit(".Largs_loop:");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Largs_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.argv]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rax + r12 * 8]");
    r#gen.emit("\tdec\tr12");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tsub\trsp, 8");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tadd\trsp, 8");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tQWORD PTR [rcx + r12 * 8], rax");
    r#gen.emit("\tjmp\t.Largs_loop");
    r#gen.emit(".Largs_done:");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tlea\trsp, [rbp - 16]");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits the routines behind `int(str)` and `float(str)`, which follow `stdlib::parse_int` and
/// `stdlib::parse_float`, and stop the program through `stone.fail` with the same messages, each
/// emitted only if named in `used`. They need the string runtime.
///
/// - `stone.parse_int` returns the int in the string in `rdi`
/// - `stone.parse_float` returns the bits of the float in the string in `rdi`, checking the
///   grammar before `stone.decimal_to_float` reads it
/// - `stone.fail_quoted` stops the program with the message `rdi` followed by the string `rsi`
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
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tcall\tstone.str_concat");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tlea\trsi, [rip + .Lstone_quote]");
    r#gen.emit("\tcall\tstone.str_concat");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tjmp\tstone.fail");

    if used.contains(&"stone.parse_int") {
        // rdi keeps the text, rsi is the cursor, rdx where the digits start, r10 the value so far,
        // negated so that the most negative int fits, r8 whether a minus sign came first, and r9
        // whether the value overflowed
        r#gen.emit("stone.parse_int:");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit(".Lparse_int_lead:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rsi]");
        r#gen.emit("\tinc\trsi");
        jump_if_space(r#gen, "ecx", ".Lparse_int_lead");
        r#gen.emit("\tdec\trsi");
        r#gen.emit("\txor\tr8d, r8d");
        r#gen.emit("\tcmp\tecx, 43"); // '+'
        r#gen.emit("\tje\t.Lparse_int_sign");
        r#gen.emit("\tcmp\tecx, 45"); // '-'
        r#gen.emit("\tjne\t.Lparse_int_digits");
        r#gen.emit("\tmov\tr8d, 1");
        r#gen.emit(".Lparse_int_sign:");
        r#gen.emit("\tinc\trsi");
        r#gen.emit(".Lparse_int_digits:");
        r#gen.emit("\tmov\trdx, rsi");
        r#gen.emit("\txor\tr10d, r10d");
        r#gen.emit("\txor\tr9d, r9d");
        r#gen.emit(".Lparse_int_digit:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rsi]");
        r#gen.emit("\tsub\tecx, 48"); // '0'
        r#gen.emit("\tcmp\tecx, 9");
        r#gen.emit("\tja\t.Lparse_int_digits_done");
        r#gen.emit("\tinc\trsi");
        r#gen.emit("\timul\tr10, r10, 10");
        r#gen.emit("\tjo\t.Lparse_int_overflow");
        r#gen.emit("\tsub\tr10, rcx");
        r#gen.emit("\tjno\t.Lparse_int_digit");
        // the digits are still read, since text after them makes the error a different one
        r#gen.emit(".Lparse_int_overflow:");
        r#gen.emit("\tmov\tr9d, 1");
        r#gen.emit("\tjmp\t.Lparse_int_digit");
        r#gen.emit(".Lparse_int_digits_done:");
        r#gen.emit("\tcmp\trsi, rdx"); // no digits
        r#gen.emit("\tje\t.Lparse_int_invalid");
        // only whitespace may follow, which is checked before overflow, as in stdlib::parse_int
        r#gen.emit(".Lparse_int_trailing:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rsi]");
        r#gen.emit("\ttest\tecx, ecx");
        r#gen.emit("\tjz\t.Lparse_int_end");
        r#gen.emit("\tinc\trsi");
        jump_if_space(r#gen, "ecx", ".Lparse_int_trailing");
        r#gen.emit(".Lparse_int_invalid:");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit("\tlea\trdi, [rip + .Lstone_int_invalid]");
        r#gen.emit("\tjmp\tstone.fail_quoted");
        r#gen.emit(".Lparse_int_end:");
        r#gen.emit("\ttest\tr9d, r9d");
        r#gen.emit("\tjnz\t.Lparse_int_range");
        r#gen.emit("\tmov\trax, r10");
        r#gen.emit("\ttest\tr8d, r8d");
        r#gen.emit("\tjnz\t.Lparse_int_done");
        r#gen.emit("\tneg\trax");
        r#gen.emit("\tjo\t.Lparse_int_range"); // 9223372036854775808 has no positive int
        r#gen.emit(".Lparse_int_done:");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_range:");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit("\tlea\trdi, [rip + .Lstone_int_range]");
        r#gen.emit("\tjmp\tstone.fail_quoted");
    }

    if used.contains(&"stone.parse_float") {
        // rbx holds the text, r12 the cursor, and r13 a count of digits
        r#gen.emit("stone.parse_float:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tpush\tr13");
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\tmov\tr12, rdi");
        r#gen.emit(".Lparse_float_lead:");
        r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
        r#gen.emit("\tinc\tr12");
        jump_if_space(r#gen, "ecx", ".Lparse_float_lead");
        r#gen.emit("\tdec\tr12");
        r#gen.emit("\tcmp\tecx, 43"); // '+'
        r#gen.emit("\tje\t.Lparse_float_sign");
        r#gen.emit("\tcmp\tecx, 45"); // '-'
        r#gen.emit("\tjne\t.Lparse_float_named");
        r#gen.emit(".Lparse_float_sign:");
        r#gen.emit("\tinc\tr12");
        // infinity before inf, so the longer name is taken whole
        r#gen.emit(".Lparse_float_named:");
        for (name, length) in [
            (".Lstone_infinity", 8),
            (".Lstone_parse_inf", 3),
            (".Lstone_parse_nan", 3),
        ] {
            // setting bit 5 lowercases a letter, and turns nothing else into one
            let next = format!("{name}_next");
            r#gen.emit(&format!("\tlea\trdx, [rip + {name}]"));
            r#gen.emit("\txor\tecx, ecx");
            r#gen.emit(&format!("{name}_loop:"));
            r#gen.emit("\tmovzx\teax, byte ptr [r12 + rcx]");
            r#gen.emit("\tor\teax, 32");
            r#gen.emit("\tcmp\tal, byte ptr [rdx + rcx]");
            r#gen.emit(&format!("\tjne\t{next}"));
            r#gen.emit("\tinc\tecx");
            r#gen.emit(&format!("\tcmp\tecx, {length}"));
            r#gen.emit(&format!("\tjne\t{name}_loop"));
            r#gen.emit(&format!("\tadd\tr12, {length}"));
            r#gen.emit("\tjmp\t.Lparse_float_trailing");
            r#gen.emit(&format!("{next}:"));
        }
        // digits, then an optional point and more digits, with at least one digit in all
        r#gen.emit("\txor\tr13, r13");
        let digits = |r#gen: &mut dyn AssemblyGenerator, name: &str| {
            r#gen.emit(&format!(".Lparse_float_{name}:"));
            r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
            r#gen.emit("\tsub\tecx, 48"); // '0'
            r#gen.emit("\tcmp\tecx, 9");
            r#gen.emit(&format!("\tja\t.Lparse_float_{name}_done"));
            r#gen.emit("\tinc\tr12");
            r#gen.emit("\tinc\tr13");
            r#gen.emit(&format!("\tjmp\t.Lparse_float_{name}"));
            r#gen.emit(&format!(".Lparse_float_{name}_done:"));
        };
        digits(r#gen, "whole");
        r#gen.emit("\tcmp\tbyte ptr [r12], 46"); // '.'
        r#gen.emit("\tjne\t.Lparse_float_mantissa");
        r#gen.emit("\tinc\tr12");
        digits(r#gen, "fraction");
        r#gen.emit(".Lparse_float_mantissa:");
        r#gen.emit("\ttest\tr13, r13");
        r#gen.emit("\tjz\t.Lparse_float_invalid");
        r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
        r#gen.emit("\tor\tecx, 32"); // 'E' becomes 'e'
        r#gen.emit("\tcmp\tecx, 101"); // 'e'
        r#gen.emit("\tjne\t.Lparse_float_trailing");
        r#gen.emit("\tinc\tr12");
        r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
        r#gen.emit("\tcmp\tecx, 43"); // '+'
        r#gen.emit("\tje\t.Lparse_float_exponent_sign");
        r#gen.emit("\tcmp\tecx, 45"); // '-'
        r#gen.emit("\tjne\t.Lparse_float_exponent");
        r#gen.emit(".Lparse_float_exponent_sign:");
        r#gen.emit("\tinc\tr12");
        r#gen.emit(".Lparse_float_exponent:");
        r#gen.emit("\txor\tr13, r13");
        digits(r#gen, "exponent_digits");
        r#gen.emit("\ttest\tr13, r13");
        r#gen.emit("\tjz\t.Lparse_float_invalid");
        r#gen.emit(".Lparse_float_trailing:");
        r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
        r#gen.emit("\ttest\tecx, ecx");
        r#gen.emit("\tjz\t.Lparse_float_valid");
        r#gen.emit("\tinc\tr12");
        jump_if_space(r#gen, "ecx", ".Lparse_float_trailing");
        r#gen.emit(".Lparse_float_invalid:");
        r#gen.emit("\tlea\trdi, [rip + .Lstone_float_invalid]");
        r#gen.emit("\tmov\trsi, rbx");
        r#gen.emit("\tjmp\tstone.fail_quoted");
        r#gen.emit(".Lparse_float_valid:");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tcall\tstone.decimal_to_float");
        r#gen.emit("\tlea\trsp, [rbp - 24]");
        r#gen.emit("\tpop\tr13");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }
}

/// Emits `str` for ints and bools. Floats have `stone.str_float` in [`str_float_runtime`].
///
/// - `stone.str_int` returns the int in `rdi` as a new string, written like `stone.print_int`
/// - `stone.str_bool` returns a constant `true` if `rdi` is nonzero and `false` otherwise
pub fn conversion_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_str_true", "true");
    immortal_string(r#gen, ".Lstone_str_false", "false");
    r#gen.emit("\t.text");

    // digits are written backwards from the terminator at [rbp - 17], and rbx and r12 hold the
    // text and its size across stone.alloc
    r#gen.emit("stone.str_int:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tlea\trsi, [rbp - 17]");
    r#gen.emit("\tmov\tbyte ptr [rsi], 0");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\txor\tr8, r8"); // negative flag
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjns\t.Lstr_int_digits");
    // negating the minimum leaves it unchanged, which unsigned division still reads correctly
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr8, 1");
    r#gen.emit(".Lstr_int_digits:");
    r#gen.emit("\tmov\trcx, 10");
    r#gen.emit(".Lstr_int_loop:");
    r#gen.emit("\txor\trdx, rdx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tadd\tdl, 48"); // '0'
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], dl");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lstr_int_loop");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lstr_int_copy");
    r#gen.emit("\tdec\trsi");
    r#gen.emit("\tmov\tbyte ptr [rsi], 45"); // '-'
    r#gen.emit(".Lstr_int_copy:");
    r#gen.emit("\tmov\trbx, rsi");
    r#gen.emit("\tlea\tr12, [rbp - 16]");
    r#gen.emit("\tsub\tr12, rbx"); // the size, counting the terminator
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tlea\trsp, [rbp - 16]");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    r#gen.emit("stone.str_bool:");
    r#gen.emit("\tlea\trax, [rip + .Lstone_str_true]");
    r#gen.emit("\tlea\trcx, [rip + .Lstone_str_false]");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tcmovz\trax, rcx");
    r#gen.emit("\tret");
}

/// Emits the `strip` and `split` methods, which follow `stdlib::strip`, `stdlib::split_whitespace`,
/// and `stdlib::split`, each emitted only if named in `used`. They need the string runtime, the
/// `split` routines need the list runtime too, and `stone.str_split` jumps to `empty_separator`, a
/// failure label, when given an empty separator.
///
/// - `stone.str_strip` returns the string in `rdi` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `rdi` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `rdi` between each `rsi`
pub fn string_methods(r#gen: &mut dyn AssemblyGenerator, used: &[&str], empty_separator: &str) {
    let uses = |label: &str| used.contains(&label);
    if uses("stone.str_strip") {
        // rbx holds the start and r12 the end
        r#gen.emit("stone.str_strip:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit(".Lstr_strip_lead:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
        r#gen.emit("\tinc\trbx");
        jump_if_space(r#gen, "ecx", ".Lstr_strip_lead");
        r#gen.emit("\tdec\trbx");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tcall\tstone.str_len");
        r#gen.emit("\tlea\tr12, [rbx + rax]");
        r#gen.emit(".Lstr_strip_trail:");
        r#gen.emit("\tcmp\tr12, rbx");
        r#gen.emit("\tje\t.Lstr_strip_copy");
        r#gen.emit("\tdec\tr12");
        r#gen.emit("\tmovzx\tecx, byte ptr [r12]");
        jump_if_space(r#gen, "ecx", ".Lstr_strip_trail");
        r#gen.emit("\tinc\tr12");
        r#gen.emit(".Lstr_strip_copy:");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tmov\trsi, r12");
        r#gen.emit("\tsub\trsi, rbx");
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit("\tlea\trsp, [rbp - 16]");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }
    if uses("stone.str_split_ws") {
        // rbx holds the cursor, r12 the list, and r13 where the piece started
        r#gen.emit("stone.str_split_ws:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tpush\tr13");
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\txor\tedi, edi");
        r#gen.emit("\tmov\tesi, 1"); // a list of strings
        r#gen.emit("\tcall\tstone.list_new");
        r#gen.emit("\tmov\tr12, rax");
        r#gen.emit(".Lstr_split_ws_skip:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
        r#gen.emit("\ttest\tecx, ecx");
        r#gen.emit("\tjz\t.Lstr_split_ws_done");
        r#gen.emit("\tinc\trbx");
        jump_if_space(r#gen, "ecx", ".Lstr_split_ws_skip");
        r#gen.emit("\tdec\trbx");
        r#gen.emit("\tmov\tr13, rbx");
        r#gen.emit(".Lstr_split_ws_word:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
        r#gen.emit("\ttest\tecx, ecx");
        r#gen.emit("\tjz\t.Lstr_split_ws_piece");
        jump_if_space(r#gen, "ecx", ".Lstr_split_ws_piece");
        r#gen.emit("\tinc\trbx");
        r#gen.emit("\tjmp\t.Lstr_split_ws_word");
        r#gen.emit(".Lstr_split_ws_piece:");
        r#gen.emit("\tmov\trdi, r13");
        r#gen.emit("\tmov\trsi, rbx");
        r#gen.emit("\tsub\trsi, r13");
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit("\tmov\trdi, r12");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tcall\tstone.list_append");
        r#gen.emit("\tjmp\t.Lstr_split_ws_skip");
        r#gen.emit(".Lstr_split_ws_done:");
        r#gen.emit("\tmov\trax, r12");
        r#gen.emit("\tlea\trsp, [rbp - 24]");
        r#gen.emit("\tpop\tr13");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }
    if uses("stone.str_split") {
        // rbx holds where the piece starts, r12 the list, r13 the separator, r14 its length, and r15
        // where it was found
        // returns where the string in rsi first appears in the one in rdi, or 0, like strstr
        r#gen.emit("stone.str_find:");
        r#gen.emit("\txor\tecx, ecx");
        r#gen.emit(".Lstr_find_compare:");
        r#gen.emit("\tmovzx\tedx, byte ptr [rsi + rcx]");
        r#gen.emit("\ttest\tedx, edx");
        r#gen.emit("\tjz\t.Lstr_find_found");
        r#gen.emit("\tcmp\tdl, byte ptr [rdi + rcx]");
        r#gen.emit("\tjne\t.Lstr_find_next");
        r#gen.emit("\tinc\trcx");
        r#gen.emit("\tjmp\t.Lstr_find_compare");
        r#gen.emit(".Lstr_find_next:");
        r#gen.emit("\tcmp\tbyte ptr [rdi], 0");
        r#gen.emit("\tje\t.Lstr_find_none");
        r#gen.emit("\tinc\trdi");
        r#gen.emit("\tjmp\tstone.str_find");
        r#gen.emit(".Lstr_find_found:");
        r#gen.emit("\tmov\trax, rdi");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_find_none:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tret");

        r#gen.emit("stone.str_split:");
        r#gen.emit("\tcmp\tbyte ptr [rsi], 0");
        r#gen.emit(&format!("\tje\t{empty_separator}"));
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tpush\tr13");
        r#gen.emit("\tpush\tr14");
        r#gen.emit("\tpush\tr15");
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\tmov\tr13, rsi");
        r#gen.emit("\tmov\trdi, rsi");
        r#gen.emit("\tcall\tstone.str_len");
        r#gen.emit("\tmov\tr14, rax");
        r#gen.emit("\txor\tedi, edi");
        r#gen.emit("\tmov\tesi, 1"); // a list of strings
        r#gen.emit("\tcall\tstone.list_new");
        r#gen.emit("\tmov\tr12, rax");
        r#gen.emit(".Lstr_split_find:");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tmov\trsi, r13");
        r#gen.emit("\tcall\tstone.str_find");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjz\t.Lstr_split_last");
        r#gen.emit("\tmov\tr15, rax");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tsub\trsi, rbx");
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit("\tmov\trdi, r12");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tcall\tstone.list_append");
        r#gen.emit("\tlea\trbx, [r15 + r14]");
        r#gen.emit("\tjmp\t.Lstr_split_find");
        // whatever follows the last separator is a piece too, even if empty
        r#gen.emit(".Lstr_split_last:");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tcall\tstone.str_len");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit("\tmov\trdi, r12");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tcall\tstone.list_append");
        r#gen.emit("\tmov\trax, r12");
        r#gen.emit("\tlea\trsp, [rbp - 40]");
        r#gen.emit("\tpop\tr15");
        r#gen.emit("\tpop\tr14");
        r#gen.emit("\tpop\tr13");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }
}

/// Emits the routines behind the `os` module that the program calls, each named in `used`, such
/// as `stone.os_pid`. Each follows a function of `stdlib::os`, making the syscalls behind the libc
/// function the interpreter calls.
/// `cwd_failure` is the failure label for `stone.os_cwd`, needed only if it is used.
///
/// - `stone.os_getenv` returns `stone.getenv` of the string in `rdi`, or null if it is empty or
///   holds `=`, which `stone.getenv` could otherwise match. `stone.os_env` and `stone.os_has_env` use it
/// - `stone.os_env` returns a copy of the variable named by `rdi`, or an empty string
/// - `stone.os_has_env` returns 1 if the variable named by `rdi` is set, else 0
/// - `stone.os_platform` and `stone.os_arch` return immortal strings
/// - `stone.os_hostname` returns a copy of the host name in `uname`'s answer, as `gethostname`
///   does, or an empty string
/// - `stone.os_cpu_count` returns how many processors [`CPU_ONLINE`] lists, as
///   `sysconf(_SC_NPROCESSORS_ONLN)` does, at least 1
/// - `stone.os_pid` returns the process id
/// - `stone.os_cwd` returns a copy of the current directory, failing if there is none
/// - `stone.os_exit` ends the program with the status in `rdi`, so it never returns
/// - `stone.os_time` and `stone.os_clock` return the bits of `clock_gettime`'s seconds plus its
///   nanoseconds over 1e9, for `CLOCK_REALTIME` and `CLOCK_MONOTONIC`
///
/// The routines that return strings need the string runtime.
pub fn os_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str], cwd_failure: &str) {
    let uses = |label: &str| used.contains(&label);
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_os_empty", "");
    immortal_string(r#gen, ".Lstone_os_platform", "linux");
    immortal_string(r#gen, ".Lstone_os_arch", "x86_64");
    r#gen.emit("\t.text");

    if uses("stone.os_env") || uses("stone.os_has_env") {
        // a name that is empty or holds = is never set, though it could match an entry
        r#gen.emit("stone.os_getenv:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tcmp\tbyte ptr [rdi], 0");
        r#gen.emit("\tje\t.Los_getenv_done");
        r#gen.emit("\tmov\trcx, rdi");
        r#gen.emit(".Los_getenv_scan:");
        r#gen.emit("\tmovzx\tedx, byte ptr [rcx]");
        r#gen.emit("\ttest\tedx, edx");
        r#gen.emit("\tjz\tstone.getenv");
        r#gen.emit("\tinc\trcx");
        r#gen.emit("\tcmp\tedx, 61"); // '='
        r#gen.emit("\tjne\t.Los_getenv_scan");
        r#gen.emit(".Los_getenv_done:");
        r#gen.emit("\tret");
    }
    if uses("stone.os_env") {
        r#gen.emit("stone.os_env:");
        r#gen.emit("\tcall\tstone.os_getenv");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjnz\tstone.os_copy");
        r#gen.emit("\tlea\trax, [rip + .Lstone_os_empty]");
        r#gen.emit("\tret");
    }
    if uses("stone.os_has_env") {
        r#gen.emit("stone.os_has_env:");
        r#gen.emit("\tcall\tstone.os_getenv");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tsetne\tal");
        r#gen.emit("\tmovzx\teax, al");
        r#gen.emit("\tret");
    }
    if uses("stone.os_env") || uses("stone.os_hostname") || uses("stone.os_cwd") {
        // returns a counted copy of the C string in rax, which rbx holds across str_len
        r#gen.emit("stone.os_copy:");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tmov\trbx, rax");
        r#gen.emit("\tmov\trdi, rax");
        r#gen.emit("\tcall\tstone.str_len");
        r#gen.emit("\tmov\trsi, rax");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tret");
    }
    if uses("stone.os_platform") {
        r#gen.emit("stone.os_platform:");
        r#gen.emit("\tlea\trax, [rip + .Lstone_os_platform]");
        r#gen.emit("\tret");
    }
    if uses("stone.os_arch") {
        r#gen.emit("stone.os_arch:");
        r#gen.emit("\tlea\trax, [rip + .Lstone_os_arch]");
        r#gen.emit("\tret");
    }
    if uses("stone.os_hostname") {
        // uname fills six fields of 65 bytes, and the host name is the second
        r#gen.emit("stone.os_hostname:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit(&format!("\tsub\trsp, {UTSNAME_SIZE}"));
        r#gen.emit("\tmov\trdi, rsp");
        r#gen.emit("\tmov\teax, 63"); // sys_uname
        r#gen.emit("\tsyscall");
        r#gen.emit("\tlea\trcx, [rip + .Lstone_os_empty]");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjnz\t.Los_hostname_done");
        r#gen.emit(&format!("\tlea\trax, [rsp + {UTSNAME_FIELD}]"));
        r#gen.emit("\tcall\tstone.os_copy");
        r#gen.emit("\tmov\trcx, rax");
        r#gen.emit(".Los_hostname_done:");
        r#gen.emit("\tmov\trax, rcx");
        r#gen.emit("\tleave");
        r#gen.emit("\tret");
    }
    if uses("stone.os_cpu_count") {
        r#gen.emit("\t.section\t.rodata");
        r#gen.emit(".Lstone_cpu_online:");
        r#gen.emit(&format!("\t.string \"{CPU_ONLINE}\""));
        r#gen.emit("\t.text");
        // the file lists ranges like 0-3,6, read into 256 bytes of stack, and r8 holds the file
        // and then each range's end
        let digits = |r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str| {
            r#gen.emit(&format!("\txor\t{reg}, {reg}"));
            r#gen.emit(&format!("{label}:"));
            r#gen.emit("\tmovzx\tedx, byte ptr [rsi]");
            r#gen.emit("\tsub\tedx, 48"); // '0'
            r#gen.emit("\tcmp\tedx, 9");
            r#gen.emit(&format!("\tja\t{label}_done"));
            r#gen.emit("\tinc\trsi");
            r#gen.emit(&format!("\timul\t{reg}, {reg}, 10"));
            r#gen.emit(&format!("\tadd\t{reg}, rdx"));
            r#gen.emit(&format!("\tjmp\t{label}"));
            r#gen.emit(&format!("{label}_done:"));
        };
        r#gen.emit("stone.os_cpu_count:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tsub\trsp, 256");
        r#gen.emit("\tmov\teax, 257"); // sys_openat
        r#gen.emit("\tmov\tedi, -100"); // AT_FDCWD
        r#gen.emit("\tlea\trsi, [rip + .Lstone_cpu_online]");
        r#gen.emit("\txor\tedx, edx"); // O_RDONLY
        r#gen.emit("\txor\tr10d, r10d");
        r#gen.emit("\tsyscall");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjs\t.Lcpu_count_one");
        r#gen.emit("\tmov\tr8, rax");
        r#gen.emit("\tmov\trdi, rax");
        r#gen.emit("\txor\teax, eax"); // sys_read
        r#gen.emit("\tmov\trsi, rsp");
        r#gen.emit("\tmov\tedx, 255");
        r#gen.emit("\tsyscall");
        r#gen.emit("\tmov\tr9, rax");
        r#gen.emit("\tmov\trdi, r8");
        r#gen.emit("\tmov\teax, 3"); // sys_close
        r#gen.emit("\tsyscall");
        r#gen.emit("\ttest\tr9, r9");
        r#gen.emit("\tjle\t.Lcpu_count_one");
        r#gen.emit("\tmov\tbyte ptr [rsp + r9], 0");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tmov\trsi, rsp");
        r#gen.emit(".Lcpu_count_range:");
        digits(r#gen, "rcx", ".Lcpu_count_first");
        r#gen.emit("\tmov\tr8, rcx");
        r#gen.emit("\tcmp\tbyte ptr [rsi], 45"); // '-'
        r#gen.emit("\tjne\t.Lcpu_count_add");
        r#gen.emit("\tinc\trsi");
        digits(r#gen, "r8", ".Lcpu_count_last");
        r#gen.emit(".Lcpu_count_add:");
        r#gen.emit("\tsub\tr8, rcx");
        r#gen.emit("\tlea\trax, [rax + r8 + 1]");
        r#gen.emit("\tcmp\tbyte ptr [rsi], 44"); // ','
        r#gen.emit("\tjne\t.Lcpu_count_done");
        r#gen.emit("\tinc\trsi");
        r#gen.emit("\tjmp\t.Lcpu_count_range");
        r#gen.emit(".Lcpu_count_done:");
        r#gen.emit("\tcmp\trax, 1");
        r#gen.emit("\tjge\t.Lcpu_count_return");
        r#gen.emit(".Lcpu_count_one:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit(".Lcpu_count_return:");
        r#gen.emit("\tleave");
        r#gen.emit("\tret");
    }
    if uses("stone.os_pid") {
        r#gen.emit("stone.os_pid:");
        r#gen.emit("\tmov\teax, 39"); // sys_getpid
        r#gen.emit("\tsyscall");
        r#gen.emit("\tret");
    }
    if uses("stone.os_cwd") {
        // the kernel writes the path into a buffer on the stack, and marks a directory that is
        // no longer reachable from the root by not starting it with /, which glibc treats as
        // an error too
        r#gen.emit("stone.os_cwd:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit(&format!("\tsub\trsp, {PATH_BUFFER}"));
        r#gen.emit("\tmov\trdi, rsp");
        r#gen.emit(&format!("\tmov\tesi, {PATH_BUFFER}"));
        r#gen.emit("\tmov\teax, 79"); // sys_getcwd
        r#gen.emit("\tsyscall");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjs\t{cwd_failure}"));
        r#gen.emit("\tcmp\tbyte ptr [rsp], 47"); // '/'
        r#gen.emit(&format!("\tjne\t{cwd_failure}"));
        r#gen.emit("\tmov\trax, rsp");
        r#gen.emit("\tcall\tstone.os_copy");
        r#gen.emit("\tleave");
        r#gen.emit("\tret");
    }
    if uses("stone.os_exit") {
        r#gen.emit("stone.os_exit:");
        r#gen.emit("\tmov\teax, 231"); // sys_exit_group
        r#gen.emit("\tsyscall");
    }
    for (label, clock) in [("stone.os_time", 0), ("stone.os_clock", 1)] {
        if !uses(label) {
            continue;
        }
        // clock_gettime fills the seconds at [rsp] and the nanoseconds at [rsp + 8]
        r#gen.emit(&format!("{label}:"));
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tsub\trsp, 16");
        r#gen.emit(&format!("\tmov\tedi, {clock}"));
        r#gen.emit("\tmov\trsi, rsp");
        r#gen.emit("\tmov\teax, 228"); // sys_clock_gettime
        r#gen.emit("\tsyscall");
        r#gen.emit("\tcvtsi2sd\txmm0, QWORD PTR [rsp]");
        r#gen.emit("\tcvtsi2sd\txmm1, QWORD PTR [rsp + 8]");
        r#gen.emit("\tmovabs\trax, 0x41cdcd6500000000"); // 1e9
        r#gen.emit("\tmovq\txmm2, rax");
        r#gen.emit("\tdivsd\txmm1, xmm2");
        r#gen.emit("\taddsd\txmm0, xmm1");
        r#gen.emit("\tmovq\trax, xmm0");
        r#gen.emit("\tleave");
        r#gen.emit("\tret");
    }
}
