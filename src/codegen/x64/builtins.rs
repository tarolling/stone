use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{CPU_ONLINE, PATH_BUFFER, UTSNAME_FIELD, UTSNAME_SIZE};
use crate::codegen::context::{
    HEAP_REGION, LARGEST_CLASS, LEAK_PREFIX, LEAK_SUFFIX, OUT_OF_MEMORY, OUT_OF_MEMORY_LENGTH,
    SMALLEST_CLASS, STDIN_BUFFER, TEXT_BLOCK,
};
pub use crate::codegen::context::{IMMORTAL, immortal_string};
use crate::codegen::x64::sort::sort_runtime;

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
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_out_of_memory:");
    r#gen.emit(&format!("\t.string \"{OUT_OF_MEMORY}\""));
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
/// variables, list slots, and temporaries refer to it, and it is freed when that reaches 0. A
/// string also holds its length in bytes at `[p - 16]`, so measuring it takes no scan, and its
/// bytes end with a terminator. `stone.live` counts the objects allocated and not yet freed.
///
/// - `stone.alloc` returns a new object with room for `rdi` bytes and a count of 1
/// - `stone.str_new` returns a new string of length `rdi` with a count of 1 and its terminator
///   written, whose bytes the caller fills in
/// - `stone.free_str` frees the string in `rax`, whose count just reached 0
/// - `stone.free_list` frees the list in `rax`, whose count just reached 0, along with its
///   elements' storage, first releasing each value its `kinds` field says is counted, through
///   `stone.free_enum` for enum values if `enums`; a list it releases that dies too waits on
///   `stone.lists_pending` for the same loop, so freeing never recurses through lists
/// - `stone.leak_check` exits with an error if the `STONE_LEAK_CHECK` environment variable is set
///   and any object is still allocated, which tests use to find leaks
///
/// The free routines are reached from inline releases that do not count as calls, so they
/// preserve every register except `rax`, `rcx`, `rdx`, and the `xmm` registers. `stone.leak_check`
/// also preserves `rax`, which holds `main`'s exit status. Memory comes from [`allocator`].
pub fn memory_runtime(r#gen: &mut dyn AssemblyGenerator, enums: bool) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_leak_env:");
    r#gen.emit("\t.string \"STONE_LEAK_CHECK\"");
    r#gen.emit(".Lstone_leak_prefix:");
    r#gen.emit(&format!("\t.string \"{LEAK_PREFIX}\""));
    r#gen.emit(".Lstone_leak_suffix:");
    r#gen.emit(&format!("\t.string \"{LEAK_SUFFIX}\\n\""));
    r#gen.emit("\t.text");

    allocator(r#gen);

    r#gen.emit("stone.alloc:");
    r#gen.emit("\tadd\trdi, 8"); // room for the count
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rax], 1");
    r#gen.emit("\tinc\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tret");

    // the length, then the count, then the bytes and a terminator
    r#gen.emit("stone.str_new:");
    r#gen.emit("\tadd\trdi, 17");
    r#gen.emit("\tcall\tstone.mem_alloc"); // which leaves rdi alone
    r#gen.emit("\tsub\trdi, 17");
    r#gen.emit("\tmov\tQWORD PTR [rax], rdi");
    r#gen.emit("\tmov\tQWORD PTR [rax + 8], 1");
    r#gen.emit("\tinc\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tadd\trax, 16");
    r#gen.emit("\tmov\tbyte ptr [rax + rdi], 0");
    r#gen.emit("\tret");

    // the length is where the allocation starts
    r#gen.emit("stone.free_str:");
    r#gen.emit("\tsub\trax, 16");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tret");

    // a list whose count reached 0 joins the lists waiting on stone.lists_pending, linked through
    // their capacity, and the first call frees them in a loop, so freeing lists nested any number
    // of levels deep, as a struct holding a list of itself makes them, never recurses; rbx holds
    // the list being freed and r12 the index of the element being released
    r#gen.emit("stone.free_list:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.lists_pending]");
    r#gen.emit("\tmov\tQWORD PTR [rax + 8], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.lists_pending], rax");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.freeing_lists], 0");
    r#gen.emit("\tjne\t.Lfree_list_queued");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.freeing_lists], 1");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit(".Lfree_list_next:");
    r#gen.emit("\tmov\trbx, QWORD PTR [rip + stone.lists_pending]");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjz\t.Lfree_list_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 8]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.lists_pending], rax");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 24], 0");
    r#gen.emit("\tje\t.Lfree_list_storage"); // the elements are not counted
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tjne\t.Lfree_list_wide");
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
    if enums {
        r#gen.emit("\tcmp\tQWORD PTR [rbx + 24], 3");
        r#gen.emit("\tje\t.Lfree_list_enum");
    }
    // only queues the list, since this call is freeing lists already
    r#gen.emit("\tcall\tstone.free_list");
    r#gen.emit("\tjmp\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_str:");
    r#gen.emit("\tcall\tstone.free_str");
    r#gen.emit("\tjmp\t.Lfree_list_loop");
    if enums {
        // freeing an enum value only queues it while another is being freed, so this
        // recursion stays shallow too
        r#gen.emit(".Lfree_list_enum:");
        r#gen.emit("\tcall\tstone.free_enum");
        r#gen.emit("\tjmp\t.Lfree_list_loop");
    }
    // each element of a list of tuples is several slots, each counted as its field's kind says:
    // r12 is the slot, r13 how many fields of the element are left, and r14 the kinds of the
    // rest, two bits each
    r#gen.emit(".Lfree_list_wide:");
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit("\txor\tr13, r13");
    r#gen.emit(".Lfree_list_wide_loop:");
    r#gen.emit("\tcmp\tr12, QWORD PTR [rbx]");
    r#gen.emit("\tjge\t.Lfree_list_wide_done");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjnz\t.Lfree_list_wide_field");
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 24]");
    r#gen.emit(".Lfree_list_wide_field:");
    r#gen.emit("\tmov\trcx, r14");
    r#gen.emit("\tand\trcx, 3");
    r#gen.emit("\tshr\tr14, 2");
    r#gen.emit("\tdec\tr13");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trax, QWORD PTR [rax + r12 * 8]");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tjz\t.Lfree_list_wide_loop"); // this field is not counted
    r#gen.emit("\tdec\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjnz\t.Lfree_list_wide_loop");
    r#gen.emit("\tcmp\trcx, 1");
    r#gen.emit("\tje\t.Lfree_list_wide_str");
    if enums {
        r#gen.emit("\tcmp\trcx, 3");
        r#gen.emit("\tje\t.Lfree_list_wide_enum");
    }
    r#gen.emit("\tcall\tstone.free_list");
    r#gen.emit("\tjmp\t.Lfree_list_wide_loop");
    r#gen.emit(".Lfree_list_wide_str:");
    r#gen.emit("\tcall\tstone.free_str");
    r#gen.emit("\tjmp\t.Lfree_list_wide_loop");
    if enums {
        r#gen.emit(".Lfree_list_wide_enum:");
        r#gen.emit("\tcall\tstone.free_enum");
        r#gen.emit("\tjmp\t.Lfree_list_wide_loop");
    }
    r#gen.emit(".Lfree_list_wide_done:");
    r#gen.emit(".Lfree_list_storage:");
    // a map's hash index
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 40]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lfree_list_unindexed");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lfree_list_unindexed:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tlea\trax, [rbx - 8]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tjmp\t.Lfree_list_next");
    r#gen.emit(".Lfree_list_done:");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.freeing_lists], 0");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit(".Lfree_list_queued:");
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

/// Emits the routines for enum values, which are objects of [`memory_runtime`] laid out as their
/// tag at `[p]`, how each of their slots is counted at `[p + 8]`, two bits per slot as a list's
/// `kinds` (see [`list_runtime`]), and the slots from `[p + 16]`:
///
/// - `stone.enum_new` returns a new enum value with tag `rdi`, kinds `rsi`, and `rdx` slots, for
///   the caller to fill in
/// - `stone.free_enum` frees the enum value in `rax`, whose count just reached 0, first releasing
///   each value its kinds say is counted
///
/// A value's slots can hold more enum values, as in a linked list, so `stone.free_enum` never
/// recurses into them: it queues each one whose count reaches 0 on `stone.free_pending`, linked
/// through its tag, and frees the queue in a loop, which a call made while it runs, through
/// `stone.free_list`, only adds to. Freeing a list of a million nodes takes no more stack than
/// freeing one. It preserves every register except `rax`, `rcx`, `rdx`, and the `xmm` registers,
/// like the other free routines.
pub fn enum_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.enum_new:");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tmov\trdi, rdx");
    r#gen.emit("\tshl\trdi, 3");
    r#gen.emit("\tadd\trdi, 16");
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tmov\tQWORD PTR [rax], rdi");
    r#gen.emit("\tmov\tQWORD PTR [rax + 8], rsi");
    r#gen.emit("\tret");

    // rbx holds the value being freed, r12 the kinds of its slots left, and r13 the next slot
    r#gen.emit("stone.free_enum:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.free_pending]");
    r#gen.emit("\tmov\tQWORD PTR [rax], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.free_pending], rax");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.freeing], 0");
    r#gen.emit("\tjne\t.Lfree_enum_queued");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.freeing], 1");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit(".Lfree_enum_next:");
    r#gen.emit("\tmov\trbx, QWORD PTR [rip + stone.free_pending]");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjz\t.Lfree_enum_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.free_pending], rax");
    r#gen.emit("\tmov\tr12, QWORD PTR [rbx + 8]");
    r#gen.emit("\tlea\tr13, [rbx + 16]");
    r#gen.emit(".Lfree_enum_slot:");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Lfree_enum_storage"); // no counted slots are left
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tand\trcx, 3");
    r#gen.emit("\tshr\tr12, 2");
    r#gen.emit("\tmov\trax, QWORD PTR [r13]");
    r#gen.emit("\tadd\tr13, 8");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tjz\t.Lfree_enum_slot"); // this slot is not counted
    r#gen.emit("\tdec\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjnz\t.Lfree_enum_slot");
    r#gen.emit("\tcmp\trcx, 1");
    r#gen.emit("\tje\t.Lfree_enum_str");
    r#gen.emit("\tcmp\trcx, 2");
    r#gen.emit("\tje\t.Lfree_enum_list");
    // an enum value joins the queue
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.free_pending]");
    r#gen.emit("\tmov\tQWORD PTR [rax], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.free_pending], rax");
    r#gen.emit("\tjmp\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_str:");
    r#gen.emit("\tcall\tstone.free_str");
    r#gen.emit("\tjmp\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_list:");
    r#gen.emit("\tcall\tstone.free_list");
    r#gen.emit("\tjmp\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_storage:");
    r#gen.emit("\tlea\trax, [rbx - 8]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tjmp\t.Lfree_enum_next");
    r#gen.emit(".Lfree_enum_done:");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.freeing], 0");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit(".Lfree_enum_queued:");
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
/// - `stone.print_char` writes the byte in `dil`
/// - `stone.print_str_quoted` writes the string `rdi` points to as a printed list shows its
///   strings, quoted and escaped like Python's `repr`
///
/// They write into the text being built (see [`text_runtime`]), which `print` then writes to
/// stdout in one go and `str` makes a string of, and clobber `rax`, `rcx`, `rdx`, `rsi`, `rdi`,
/// `r8`, and `r11`.
pub fn print(r#gen: &mut dyn AssemblyGenerator, strings: bool) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_true:");
    r#gen.emit("\t.string \"true\"");
    r#gen.emit(".Lstone_false:");
    r#gen.emit("\t.string \"false\"");
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
    r#gen.emit("\tcall\tstone.text_append");
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
    r#gen.emit("\tjmp\tstone.text_append");
    r#gen.emit(".Lprint_str_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_bool:");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_true]");
    r#gen.emit("\tjnz\tstone.print_str");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_false]");
    r#gen.emit("\tjmp\tstone.print_str");

    r#gen.emit("stone.print_char:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 16");
    r#gen.emit("\tmov\tbyte ptr [rbp - 1], dil");
    r#gen.emit("\tlea\trsi, [rbp - 1]");
    r#gen.emit("\tmov\trdx, 1");
    r#gen.emit("\tcall\tstone.text_append");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");

    print_str_quoted(r#gen);
    text_runtime(r#gen, strings);
}

/// Emits the text builder that every `stone.print_*` routine writes into. The text being built
/// is a block from `stone.mem_alloc` laid out as a string will be, its length at `[p]` and its
/// room for bytes at `[p + 8]`, where the string's count goes, then the bytes, so finishing it
/// as a string copies nothing. `stone.text` points to the block of the text being built.
///
/// - `stone.text_begin` starts a new text and returns the one being built before, to give back
///   when the new one is done, so building nests, as when a struct's `display` runs inside a
///   `print`
/// - `stone.text_append` adds the `rdx` bytes at `rsi` to the text, growing its block to twice
///   what it must hold
/// - `stone.text_end` finishes the text as a new string with a count of 1, and goes back to
///   building the text in `rdi`
/// - `stone.text_print` writes the text and a newline to stdout, frees it, and goes back to
///   building the text in `rdi`
/// - `stone.write_out` writes the `rdx` bytes at `rsi` to stdout, however many `write` syscalls
///   that takes
///
/// `stone.text_print` preserves the callee-saved registers, and the others clobber `rax`, `rcx`,
/// `rdx`, `rsi`, `rdi`, and `r11`, as a `write` syscall does. `stone.text_end` is emitted only
/// if the program has `strings`, since it counts the string it makes in `stone.live`.
fn text_runtime(r#gen: &mut dyn AssemblyGenerator, strings: bool) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.text:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("\t.text");

    // a block of 128 bytes, of which the header, the length, the room, and the terminator take
    // 8, 8, 8, and 1
    r#gen.emit("stone.text_begin:");
    r#gen.emit(&format!("\tmov\tedi, {}", TEXT_BLOCK));
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rax], 0");
    r#gen.emit(&format!("\tmov\tQWORD PTR [rax + 8], {}", TEXT_BLOCK - 17));
    r#gen.emit("\tmov\trcx, QWORD PTR [rip + stone.text]");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.text], rax");
    r#gen.emit("\tmov\trax, rcx");
    r#gen.emit("\tret");

    r#gen.emit("stone.text_append:");
    r#gen.emit("\tmov\trdi, QWORD PTR [rip + stone.text]");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi]");
    r#gen.emit("\tadd\trax, rdx");
    r#gen.emit("\tcmp\trax, QWORD PTR [rdi + 8]");
    r#gen.emit("\tjbe\t.Ltext_append_copy");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tpush\trdx");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tlea\trsi, [rax + rax + 17]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tpop\trcx");
    r#gen.emit("\tadd\trcx, rcx");
    r#gen.emit("\tmov\tQWORD PTR [rax + 8], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.text], rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tpop\trdx");
    r#gen.emit("\tpop\trsi");
    r#gen.emit(".Ltext_append_copy:");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi]");
    r#gen.emit("\tlea\trcx, [rax + rdx]");
    r#gen.emit("\tmov\tQWORD PTR [rdi], rcx");
    r#gen.emit("\tlea\trdi, [rdi + rax + 16]");
    r#gen.emit("\tmov\trcx, rdx");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tret");

    if strings {
        r#gen.emit("stone.text_end:");
        r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.text]");
        r#gen.emit("\tmov\tQWORD PTR [rip + stone.text], rdi");
        r#gen.emit("\tmov\trcx, QWORD PTR [rax]");
        r#gen.emit("\tmov\tbyte ptr [rax + rcx + 16], 0");
        r#gen.emit("\tmov\tQWORD PTR [rax + 8], 1");
        r#gen.emit("\tinc\tQWORD PTR [rip + stone.live]");
        r#gen.emit("\tadd\trax, 16");
        r#gen.emit("\tret");
    }

    // rbx holds the text to go back to and r12 the one written
    r#gen.emit("stone.text_print:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tedi, 10"); // '\n'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tmov\tr12, QWORD PTR [rip + stone.text]");
    r#gen.emit("\tlea\trsi, [r12 + 16]");
    r#gen.emit("\tmov\trdx, QWORD PTR [r12]");
    r#gen.emit("\tcall\tstone.write_out");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.text], rbx");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");

    // a write may take fewer bytes than asked, or be interrupted before it takes any
    r#gen.emit("stone.write_out:");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjz\t.Lwrite_out_done");
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tmov\tedi, 1"); // stdout
    r#gen.emit("\tsyscall");
    r#gen.emit("\tcmp\trax, -4"); // EINTR
    r#gen.emit("\tje\tstone.write_out");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjle\t.Lwrite_out_done");
    r#gen.emit("\tadd\trsi, rax");
    r#gen.emit("\tsub\trdx, rax");
    r#gen.emit("\tjmp\tstone.write_out");
    r#gen.emit(".Lwrite_out_done:");
    r#gen.emit("\tret");
}

/// Emits `stone.print_str_quoted`, which prints a string inside a list the way
/// `stdlib::repr_in_list` writes it: in single quotes, or double quotes if it holds a `'` but no
/// `"`, with backslashes, the quote, and control characters escaped. Each run of bytes that needs
/// no escape is appended at once.
fn print_str_quoted(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds where the run starts, r12 the quote, and r13 where the run ends, then the byte
    // that ended it
    r#gen.emit("stone.print_str_quoted:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tmov\trbx, rdi");
    // ecx is set by a ' and edx by a "
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(".Lprint_quoted_scan:");
    r#gen.emit("\tmovzx\teax, byte ptr [rdi]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjz\t.Lprint_quoted_choose");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tcmp\teax, 39"); // '\''
    r#gen.emit("\tjne\t.Lprint_quoted_double");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit("\tjmp\t.Lprint_quoted_scan");
    r#gen.emit(".Lprint_quoted_double:");
    r#gen.emit("\tcmp\teax, 34"); // '"'
    r#gen.emit("\tjne\t.Lprint_quoted_scan");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tjmp\t.Lprint_quoted_scan");
    r#gen.emit(".Lprint_quoted_choose:");
    r#gen.emit("\tmov\tr12d, 39");
    r#gen.emit("\ttest\tecx, ecx");
    r#gen.emit("\tjz\t.Lprint_quoted_open");
    r#gen.emit("\ttest\tedx, edx");
    r#gen.emit("\tjnz\t.Lprint_quoted_open");
    r#gen.emit("\tmov\tr12d, 34");
    r#gen.emit(".Lprint_quoted_open:");
    r#gen.emit("\tmov\tedi, r12d");
    r#gen.emit("\tcall\tstone.print_char");
    // a run ends at the terminator or a byte that needs an escape
    r#gen.emit(".Lprint_quoted_run:");
    r#gen.emit("\tmov\tr13, rbx");
    r#gen.emit(".Lprint_quoted_plain:");
    r#gen.emit("\tmovzx\teax, byte ptr [r13]");
    r#gen.emit("\tcmp\teax, 92"); // '\\'
    r#gen.emit("\tje\t.Lprint_quoted_flush");
    r#gen.emit("\tcmp\teax, r12d");
    r#gen.emit("\tje\t.Lprint_quoted_flush");
    r#gen.emit("\tcmp\teax, 32");
    r#gen.emit("\tjb\t.Lprint_quoted_flush"); // a control character or the terminator
    r#gen.emit("\tcmp\teax, 127");
    r#gen.emit("\tje\t.Lprint_quoted_flush");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tjmp\t.Lprint_quoted_plain");
    r#gen.emit(".Lprint_quoted_flush:");
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit("\tsub\trdx, rbx");
    r#gen.emit("\tjz\t.Lprint_quoted_special");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tcall\tstone.text_append");
    r#gen.emit(".Lprint_quoted_special:");
    r#gen.emit("\tmovzx\teax, byte ptr [r13]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjz\t.Lprint_quoted_close");
    r#gen.emit("\tlea\trbx, [r13 + 1]");
    r#gen.emit("\tmov\tr13d, eax");
    r#gen.emit("\tmov\tedi, 92");
    r#gen.emit("\tcall\tstone.print_char");
    // what follows the backslash: n, t, r, the byte itself, or x and two hex digits
    r#gen.emit("\tmov\tedi, 110"); // 'n'
    r#gen.emit("\tcmp\tr13d, 10");
    r#gen.emit("\tje\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tedi, 116"); // 't'
    r#gen.emit("\tcmp\tr13d, 9");
    r#gen.emit("\tje\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tedi, 114"); // 'r'
    r#gen.emit("\tcmp\tr13d, 13");
    r#gen.emit("\tje\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tcmp\tr13d, 92");
    r#gen.emit("\tje\t.Lprint_quoted_code");
    r#gen.emit("\tcmp\tr13d, r12d");
    r#gen.emit("\tje\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tedi, 120"); // 'x'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tshr\tedi, 4");
    r#gen.emit("\tcall\t.Lprint_quoted_hex");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tand\tedi, 15");
    r#gen.emit("\tcall\t.Lprint_quoted_hex");
    r#gen.emit("\tjmp\t.Lprint_quoted_run");
    r#gen.emit(".Lprint_quoted_code:");
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tjmp\t.Lprint_quoted_run");
    r#gen.emit(".Lprint_quoted_close:");
    r#gen.emit("\tmov\tedi, r12d");
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
    // prints the hex digit in edi, from 0 to 15, in lowercase
    r#gen.emit(".Lprint_quoted_hex:");
    r#gen.emit("\tlea\teax, [rdi + 48]"); // '0'
    r#gen.emit("\tlea\tecx, [rdi + 87]"); // 'a' - 10
    r#gen.emit("\tcmp\tedi, 10");
    r#gen.emit("\tcmovae\teax, ecx");
    r#gen.emit("\tmov\tedi, eax");
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
    r#gen.emit("\tmov\tedi, 63"); // more than any float's text
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tcall\tstone.format_float");
    // the text is shorter than the room it was given
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\tstone.cstr_len");
    r#gen.emit("\tmov\tQWORD PTR [rbx - 16], rax");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tmov\trbx, QWORD PTR [rbp - 8]");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
}

/// Emits the string runtime. Strings are null-terminated, counted, and carry their length (see
/// [`memory_runtime`]), and `+` makes a new one with `stone.str_new`.
///
/// - `stone.str_len` returns the length in bytes of the string in `rdi`, from its header, and
///   changes only `rax`
/// - `stone.cstr_len` returns the length of the null-terminated bytes at `rdi`, such as a
///   program argument or a pointer into a string, by scanning them, and changes only `rax`
/// - `stone.str_eq` returns 1 if the strings in `rdi` and `rsi` hold the same bytes, else 0
/// - `stone.str_concat` returns a new string holding `rdi` followed by `rsi`
/// - `stone.str_slice` returns a new string holding the `rsi` bytes at `rdi`
pub fn string_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.str_len:");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi - 16]");
    r#gen.emit("\tret");

    r#gen.emit("stone.cstr_len:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit(".Lcstr_len_loop:");
    r#gen.emit("\tcmp\tbyte ptr [rdi + rax], 0");
    r#gen.emit("\tje\t.Lcstr_len_done");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Lcstr_len_loop");
    r#gen.emit(".Lcstr_len_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.str_eq:");
    // strings of different lengths differ without a look at their bytes
    r#gen.emit("\tmov\trax, QWORD PTR [rdi - 16]");
    r#gen.emit("\tcmp\trax, QWORD PTR [rsi - 16]");
    r#gen.emit("\tjne\t.Lstr_eq_differ");
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
    r#gen.emit("\tlea\trdi, [r14 + r15]");
    r#gen.emit("\tcall\tstone.str_new");
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
    // rbx and r12 hold the bytes and their count across stone.str_new
    r#gen.emit("stone.str_slice:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\trdi, rsi");
    r#gen.emit("\tcall\tstone.str_new");
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

/// Emits the list runtime. A list is a pointer to a counted 48-byte header `{len, cap, data,
/// kinds, width, index}` (see [`memory_runtime`]), where `data` points to `cap` 8-byte slots, `len` of
/// them in use. Each element is `width` slots: one, or one for each value of a tuple, stored side
/// by side, so a `list[tuple[int, str]]` of 3 elements has a `len` of 6. `kinds` says what each
/// value of an element is, two bits for each: 0 for values that are not counted, 1 for strings,
/// and 2 for lists, so it is 0, 1, or 2 for a list of one value each. `index` is 0 except in a
/// map, which is a list of entries that may point to its hash index (see [`map_runtime`]). The
/// routines follow the System V calling convention, and take their memory from [`allocator`].
///
/// - `stone.list_new_wide` returns a list of `rdi` slots whose values the caller fills in, with
///   `kinds` set to `rsi` and `width` to `rdx`
/// - `stone.list_new` does the same for a list of one value each, whose `width` is 1
/// - `stone.retain_slots` adds a reference to each counted value of the `rsi` slots from `rdi`,
///   which start at an element and whose `kinds` and `width` are `rdx` and `rcx`, preserving
///   every register except `rax`, `rcx`, and `rdx`
/// - `stone.list_append` appends `rsi` to list `rdi`, doubling its capacity when full
/// - `stone.list_copy` returns in `rax` a copy of the list in `rax`, retaining each element if
///   they are counted, and drops one reference to the original, which something else still
///   holds. Like the free routines, it is reached from an inline `list_unique` that does not
///   count as a call, so it preserves every register except `rax`, `rcx`, `rdx`, and the `xmm`
///   registers.
///
/// Indexing is generated inline by `X64Generator::list_slot` (in `emit.rs`) rather than called here.
pub fn list_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 its length, r13 what its elements are, and r14 their width
    r#gen.emit("stone.list_new:");
    r#gen.emit("\tmov\trdx, 1");
    r#gen.emit("stone.list_new_wide:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tmov\tr14, rdx");
    r#gen.emit("\tmov\trdi, 48");
    r#gen.emit("\tcall\tstone.alloc");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx], r12");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 40], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], r13");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 32], r14");
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
    r#gen.emit("\tlea\trsp, [rbp - 32]");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // r8 counts the slots, r9 how many fields of the element are left, and r10 the kinds of the
    // rest, two bits each
    r#gen.emit("stone.retain_slots:");
    for reg in ["r8", "r9", "r10", "r11"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\txor\tr8, r8");
    r#gen.emit("\txor\tr9, r9");
    r#gen.emit(".Lretain_slots_loop:");
    r#gen.emit("\tcmp\tr8, rsi");
    r#gen.emit("\tjge\t.Lretain_slots_done");
    r#gen.emit("\ttest\tr9, r9");
    r#gen.emit("\tjnz\t.Lretain_slots_field");
    r#gen.emit("\tmov\tr9, rcx");
    r#gen.emit("\tmov\tr10, rdx");
    r#gen.emit(".Lretain_slots_field:");
    r#gen.emit("\tmov\tr11, r10");
    r#gen.emit("\tshr\tr10, 2");
    r#gen.emit("\tdec\tr9");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + r8 * 8]");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\ttest\tr11, 3");
    r#gen.emit("\tjz\t.Lretain_slots_loop"); // this field is not counted
    r#gen.emit("\tinc\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjmp\t.Lretain_slots_loop");
    r#gen.emit(".Lretain_slots_done:");
    for reg in ["r11", "r10", "r9", "r8"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
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
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcall\tstone.list_new_wide");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 24]");
    // the values of wider elements are retained after copying
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tje\t.Llist_copy_start");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Llist_copy_start:");
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
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tje\t.Llist_copy_retained");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcall\tstone.retain_slots");
    r#gen.emit(".Llist_copy_retained:");
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

/// Emits the map runtime. A map is a list (see [`list_runtime`]) whose elements are its entries in
/// the order their keys were first added, each a key followed by the values of its value, so a
/// `map[str, tuple[int, int]]` has a `width` of 3. Its `index` is 0 until the first lookup, and
/// then points to a hash table: its capacity, a power of two from 8, followed by that many slots,
/// each 0 if empty or else 1 more than the first slot of an entry. The table stays at most half
/// full. A copy of a map (`stone.list_copy`) has no index, and builds one when it is first used.
/// Keys are ints, bools, or strings, which `rdx` says by being 1. Strings are equal if their
/// bytes are, which needs the string runtime only if `strings`.
///
/// - `stone.map_find` returns the first slot of the entry of key `rsi` in map `rdi`, or -1 if
///   there is none
/// - `stone.map_add` adds an entry for key `rsi`, which the map does not have, at the end of map
///   `rdi`, retaining a string key, and returns its first slot. The caller fills in its values.
/// - `stone.map_unindex` frees the index of map `rdi`, whose entries moved, as when one is
///   removed, so the next lookup builds a new one
pub fn map_runtime(r#gen: &mut dyn AssemblyGenerator, strings: bool) {
    // rbx holds the map, r12 the key, and r13 whether it is a string
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    let enter = |r#gen: &mut dyn AssemblyGenerator| {
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        for reg in saved {
            r#gen.emit(&format!("\tpush\t{reg}"));
        }
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\tmov\tr12, rsi");
        r#gen.emit("\tmov\tr13, rdx");
    };
    let leave = |r#gen: &mut dyn AssemblyGenerator| {
        r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
        for reg in saved.iter().rev() {
            r#gen.emit(&format!("\tpop\t{reg}"));
        }
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    };

    r#gen.emit("stone.map_find:");
    enter(r#gen);
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 40], 0");
    r#gen.emit("\tjne\t.Lmap_find_indexed");
    r#gen.emit("\tcall\t.Lmap_rebuild");
    r#gen.emit(".Lmap_find_indexed:");
    r#gen.emit("\tcall\t.Lmap_probe");
    // an empty slot holds 0, which becomes -1
    r#gen.emit("\tmov\trax, QWORD PTR [rax]");
    r#gen.emit("\tdec\trax");
    leave(r#gen);

    // r14 is the first slot of the new entry, and r15 counts the slots appended
    r#gen.emit("stone.map_add:");
    enter(r#gen);
    // a bigger index when one more entry would fill more than half of it: 2 * (len + width)
    // slots must be at most capacity * width
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 40]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lmap_add_rebuild");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx]");
    r#gen.emit("\tadd\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\tadd\trcx, rcx");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 32]");
    r#gen.emit("\timul\trdx, QWORD PTR [rax]");
    r#gen.emit("\tcmp\trcx, rdx");
    r#gen.emit("\tjle\t.Lmap_add_room");
    r#gen.emit(".Lmap_add_rebuild:");
    r#gen.emit("\tcall\t.Lmap_rebuild");
    r#gen.emit(".Lmap_add_room:");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tr15, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit(".Lmap_add_slot:");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\tstone.list_append");
    r#gen.emit("\txor\tesi, esi"); // the values are filled in by the caller
    r#gen.emit("\tdec\tr15");
    r#gen.emit("\tjnz\t.Lmap_add_slot");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjz\t.Lmap_add_key");
    r#gen.emit("\tinc\tQWORD PTR [r12 - 8]");
    r#gen.emit(".Lmap_add_key:");
    r#gen.emit("\tcall\t.Lmap_probe");
    r#gen.emit("\tlea\trcx, [r14 + 1]");
    r#gen.emit("\tmov\tQWORD PTR [rax], rcx");
    r#gen.emit("\tmov\trax, r14");
    leave(r#gen);

    r#gen.emit("stone.map_unindex:");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 40]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lmap_unindex_done");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 40], 0");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lmap_unindex_done:");
    r#gen.emit("\tret");

    // replaces map rbx's index with one for its entries and one more, keeping r12 and r13 for its
    // caller: r14 is the first slot of each entry in turn, and r15 the capacity
    r#gen.emit(".Lmap_rebuild:");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tpush\tr15");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 40]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lmap_rebuild_size");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lmap_rebuild_size:");
    r#gen.emit("\tmov\tr15, 8");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx]");
    r#gen.emit("\tadd\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\tadd\trcx, rcx");
    r#gen.emit(".Lmap_rebuild_grow:");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 32]");
    r#gen.emit("\timul\trdx, r15");
    r#gen.emit("\tcmp\trcx, rdx");
    r#gen.emit("\tjle\t.Lmap_rebuild_alloc");
    r#gen.emit("\tadd\tr15, r15");
    r#gen.emit("\tjmp\t.Lmap_rebuild_grow");
    r#gen.emit(".Lmap_rebuild_alloc:");
    r#gen.emit("\tlea\trdi, [r15 * 8 + 8]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 40], rax");
    r#gen.emit("\tmov\tQWORD PTR [rax], r15");
    r#gen.emit("\tmov\trcx, r15");
    r#gen.emit(".Lmap_rebuild_zero:");
    r#gen.emit("\tmov\tQWORD PTR [rax + rcx * 8], 0");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tjnz\t.Lmap_rebuild_zero");
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit(".Lmap_rebuild_next:");
    r#gen.emit("\tcmp\tr14, QWORD PTR [rbx]");
    r#gen.emit("\tjge\t.Lmap_rebuild_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr12, QWORD PTR [rax + r14 * 8]");
    // the keys differ, so this finds an empty slot
    r#gen.emit("\tcall\t.Lmap_probe");
    r#gen.emit("\tlea\trcx, [r14 + 1]");
    r#gen.emit("\tmov\tQWORD PTR [rax], rcx");
    r#gen.emit("\tadd\tr14, QWORD PTR [rbx + 32]");
    r#gen.emit("\tjmp\t.Lmap_rebuild_next");
    r#gen.emit(".Lmap_rebuild_done:");
    r#gen.emit("\tpop\tr15");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tret");

    // returns in rax the address of the slot of map rbx's index that holds key r12's entry, or
    // of the empty slot where it would go, probing one slot after another: r8 is the index, r9
    // the mask of a slot number, r10 the slot's address, and r11 what it holds
    r#gen.emit(".Lmap_probe:");
    // the hash, FNV-1a over a string's bytes, then mixed by Fibonacci hashing
    r#gen.emit("\tmov\trax, r12");
    if strings {
        r#gen.emit("\ttest\tr13, r13");
        r#gen.emit("\tjz\t.Lmap_probe_mix");
        r#gen.emit("\tmovabs\trax, 0xcbf29ce484222325");
        r#gen.emit("\tmovabs\trcx, 0x100000001b3");
        r#gen.emit("\tmov\trdx, r12");
        r#gen.emit(".Lmap_probe_byte:");
        r#gen.emit("\tmovzx\tr8d, byte ptr [rdx]");
        r#gen.emit("\ttest\tr8d, r8d");
        r#gen.emit("\tjz\t.Lmap_probe_mix");
        r#gen.emit("\txor\trax, r8");
        r#gen.emit("\timul\trax, rcx");
        r#gen.emit("\tinc\trdx");
        r#gen.emit("\tjmp\t.Lmap_probe_byte");
    }
    r#gen.emit(".Lmap_probe_mix:");
    r#gen.emit("\tmovabs\trcx, 0x9e3779b97f4a7c15");
    r#gen.emit("\timul\trax, rcx");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\tshr\trcx, 32");
    r#gen.emit("\txor\trax, rcx");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 40]");
    r#gen.emit("\tmov\tr9, QWORD PTR [r8]");
    r#gen.emit("\tdec\tr9");
    r#gen.emit(".Lmap_probe_slot:");
    r#gen.emit("\tand\trax, r9");
    r#gen.emit("\tlea\tr10, [r8 + rax * 8 + 8]");
    r#gen.emit("\tmov\tr11, QWORD PTR [r10]");
    r#gen.emit("\ttest\tr11, r11");
    r#gen.emit("\tjz\t.Lmap_probe_done");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rdx + r11 * 8 - 8]");
    r#gen.emit("\tcmp\trsi, r12");
    r#gen.emit("\tje\t.Lmap_probe_done"); // the same int, or the same string
    if strings {
        r#gen.emit("\ttest\tr13, r13");
        r#gen.emit("\tjz\t.Lmap_probe_next");
        // stone.str_eq changes only rax, rdi, and rsi
        r#gen.emit("\tmov\trcx, rax");
        r#gen.emit("\tmov\trdi, r12");
        r#gen.emit("\tcall\tstone.str_eq");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tmov\trax, rcx");
        r#gen.emit("\tjnz\t.Lmap_probe_done");
        r#gen.emit(".Lmap_probe_next:");
    }
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Lmap_probe_slot");
    r#gen.emit(".Lmap_probe_done:");
    r#gen.emit("\tmov\trax, r10");
    r#gen.emit("\tret");
}

/// Emits each list routine in `labels`, which are the [`LIST_ROUTINES`] a program calls. They
/// follow the System V calling convention, like [`list_runtime`]'s.
///
/// - `stone.list_slice` returns a new list of the elements of list `rdi` that the slice from `rsi`
///   to `rdx` by `rcx` picks, as `stdlib::slice_indices` picks them, retaining them if they are
///   counted. Bit 0 of `r8` says `rsi` is missing, bit 1 `rdx`, and bit 2 `rcx`, and the step is
///   never 0, which the caller checks.
/// - `stone.list_pop` removes the element at position `rsi` of list `rdi`, moving the ones after it
///   down, and returns it in `rax` with the list's reference to it. The caller checks that `rsi`
///   is in range.
/// - `stone.list_insert` puts `rdx` at position `rsi` of list `rdi`, moving the elements from
///   there on up, and the list takes the caller's reference to it. The caller checks that `rsi`
///   is at most the length.
/// - `stone.list_extend` appends every element of list `rsi` to list `rdi`, retaining them if
///   they are counted. The two may be the same list.
/// - `stone.list_reuse` takes a reference to list `rdi`, whose elements are not counted, and
///   returns it in `rax` if nothing else refers to it, for arithmetic to write its result over.
///   Otherwise it drops the reference and returns a new list of length `rsi` from
///   `stone.list_new`.
/// - `stone.list_sort_int`, `stone.list_sort_float`, and `stone.list_sort_str` sort list `rdi`
///   (see [`sort_runtime`]).
///
/// [`LIST_ROUTINES`]: crate::codegen::context::LIST_ROUTINES
pub fn list_methods(r#gen: &mut dyn AssemblyGenerator, labels: &[&str]) {
    sort_runtime(r#gen, labels);
    if labels.contains(&"stone.list_slice") {
        list_slice(r#gen);
    }
    if labels.contains(&"stone.list_pop") {
        list_pop(r#gen);
    }
    if labels.contains(&"stone.list_insert") {
        list_insert(r#gen);
    }
    if labels.contains(&"stone.list_extend") {
        list_extend(r#gen);
    }
    if labels.contains(&"stone.list_reuse") {
        r#gen.emit("stone.list_reuse:");
        r#gen.emit("\tcmp\tQWORD PTR [rdi - 8], 1");
        r#gen.emit("\tjne\t.Llist_reuse_new");
        r#gen.emit("\tmov\trax, rdi");
        r#gen.emit("\tret");
        r#gen.emit(".Llist_reuse_new:");
        // something else still holds it, so this only drops a reference
        r#gen.emit("\tdec\tQWORD PTR [rdi - 8]");
        r#gen.emit("\tmov\trdi, rsi");
        r#gen.emit("\txor\tesi, esi");
        r#gen.emit("\tjmp\tstone.list_new");
    }
}

/// Emits `stone.list_pop` (see [`list_methods`]).
fn list_pop(r#gen: &mut dyn AssemblyGenerator) {
    // rax holds the elements, rcx the new length, and r8 the element popped
    r#gen.emit("stone.list_pop:");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 16]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rax + rsi * 8]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tmov\tQWORD PTR [rdi], rcx");
    r#gen.emit(".Llist_pop_loop:");
    r#gen.emit("\tcmp\trsi, rcx");
    r#gen.emit("\tjge\t.Llist_pop_done");
    r#gen.emit("\tmov\tr9, QWORD PTR [rax + rsi * 8 + 8]");
    r#gen.emit("\tmov\tQWORD PTR [rax + rsi * 8], r9");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tjmp\t.Llist_pop_loop");
    r#gen.emit(".Llist_pop_done:");
    r#gen.emit("\tmov\trax, r8");
    r#gen.emit("\tret");
}

/// Emits `stone.list_insert` (see [`list_methods`]).
fn list_insert(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 the position, and r13 the element
    r#gen.emit("stone.list_insert:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tcmp\trax, QWORD PTR [rbx + 8]");
    r#gen.emit("\tjl\t.Llist_insert_shift");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 8]");
    r#gen.emit("\tshl\trsi, 1");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 8], rsi");
    r#gen.emit("\tshl\trsi, 3");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], rax");
    r#gen.emit(".Llist_insert_shift:");
    // move each element from the end down to the position up by one
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 16]");
    r#gen.emit(".Llist_insert_loop:");
    r#gen.emit("\tcmp\trcx, r12");
    r#gen.emit("\tjle\t.Llist_insert_place");
    r#gen.emit("\tmov\trax, QWORD PTR [rdx + rcx * 8 - 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdx + rcx * 8], rax");
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tjmp\t.Llist_insert_loop");
    r#gen.emit(".Llist_insert_place:");
    r#gen.emit("\tmov\tQWORD PTR [rdx + r12 * 8], r13");
    r#gen.emit("\tinc\tQWORD PTR [rbx]");
    r#gen.emit("\txor\teax, eax"); // insert returns none
    r#gen.emit("\tlea\trsp, [rbp - 24]");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `stone.list_extend` (see [`list_methods`]).
fn list_extend(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 the other list, and r13 how many elements it adds, read before
    // growing, since the other list may be this one
    r#gen.emit("stone.list_extend:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, QWORD PTR [r12]");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tadd\trax, r13");
    r#gen.emit("\tcmp\trax, QWORD PTR [rbx + 8]");
    r#gen.emit("\tjle\t.Llist_extend_copy");
    // double the capacity, or more if that is not enough
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 8]");
    r#gen.emit("\tshl\trsi, 1");
    r#gen.emit("\tcmp\trsi, rax");
    r#gen.emit("\tcmovl\trsi, rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 8], rsi");
    r#gen.emit("\tshl\trsi, 3");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], rax");
    r#gen.emit(".Llist_extend_copy:");
    // the other list's elements are read after growing, which may have moved them
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tlea\trdi, [rdi + rax * 8]");
    r#gen.emit("\tmov\trsi, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 24]");
    // the values of wider elements are retained after copying
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tje\t.Llist_extend_start");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Llist_extend_start:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Llist_extend_loop:");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjge\t.Llist_extend_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + rcx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdi + rcx * 8], rax");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Llist_extend_loop"); // the elements are not counted
    r#gen.emit("\tinc\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjmp\t.Llist_extend_loop");
    r#gen.emit(".Llist_extend_done:");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tje\t.Llist_extend_retained");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcall\tstone.retain_slots");
    r#gen.emit(".Llist_extend_retained:");
    r#gen.emit("\tadd\tQWORD PTR [rbx], r13");
    r#gen.emit("\txor\teax, eax"); // extend returns none
    r#gen.emit("\tlea\trsp, [rbp - 24]");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `stone.list_slice` (see [`list_methods`]).
fn list_slice(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 the first index, r13 the step, r14 the count, and r15 the new list;
    // r9 holds the length while the bounds are clamped
    r#gen.emit("stone.list_slice:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr9, QWORD PTR [rbx]");
    // the bounds count elements, each `width` slots
    r#gen.emit("\tmov\tr10, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcmp\tr10, 1");
    r#gen.emit("\tje\t.Llist_slice_counted_elements");
    r#gen.emit("\tmov\tr11, rdx");
    r#gen.emit("\tmov\trax, r9");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\tr10");
    r#gen.emit("\tmov\tr9, rax");
    r#gen.emit("\tmov\trdx, r11");
    r#gen.emit(".Llist_slice_counted_elements:");
    slice_bounds(r#gen, "list_slice");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 1");
    r#gen.emit("\tjne\t.Llist_slice_wide");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [r15 + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Llist_slice_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Llist_slice_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + rdx * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdi + rcx * 8], rax");
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Llist_slice_loop"); // the elements are not counted
    r#gen.emit("\tinc\tQWORD PTR [rax - 8]");
    r#gen.emit("\tjmp\t.Llist_slice_loop");
    r#gen.emit(".Llist_slice_done:");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
    // each element picked is `width` slots, in r10, copied from rsi to rdi, then retained:
    // rdx is the element picked, rcx how many are copied, and r8 the slot within one
    r#gen.emit(".Llist_slice_wide:");
    r#gen.emit("\tmov\tr10, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\timul\trdi, r10");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trdx, r10");
    r#gen.emit("\tcall\tstone.list_new_wide");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\tr10, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\trdi, QWORD PTR [r15 + 16]");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Llist_slice_wide_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Llist_slice_wide_copied");
    r#gen.emit("\tmov\trax, rdx");
    r#gen.emit("\timul\trax, r10");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tlea\trsi, [rsi + rax * 8]");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Llist_slice_wide_slot:");
    r#gen.emit("\tmov\tr9, QWORD PTR [rsi + r8 * 8]");
    r#gen.emit("\tmov\tQWORD PTR [rdi], r9");
    r#gen.emit("\tadd\trdi, 8");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tcmp\tr8, r10");
    r#gen.emit("\tjl\t.Llist_slice_wide_slot");
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Llist_slice_wide_loop");
    r#gen.emit(".Llist_slice_wide_copied:");
    r#gen.emit("\tmov\trdi, QWORD PTR [r15 + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [r15]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trcx, r10");
    r#gen.emit("\tcall\tstone.retain_slots");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits the steps of a slice routine that pick what the slice from `rsi` to `rdx` by `rcx`
/// takes from a list or string of length `r9`, as `stdlib::slice_indices` does, leaving the first
/// index in `r12`, the step in `r13`, and the count in `r14`. Bit 0 of `r8` says `rsi` is
/// missing, bit 1 `rdx`, and bit 2 `rcx`, and the step is never 0. It clobbers `rax`, `rcx`,
/// `rdx`, and `r10`, and `routine` keeps the labels of each routine apart.
fn slice_bounds(r#gen: &mut dyn AssemblyGenerator, routine: &str) {
    let label = |name: &str| format!(".L{routine}_{name}");
    r#gen.emit("\ttest\tr8, 4");
    r#gen.emit(&format!("\tjz\t{}", label("step")));
    r#gen.emit("\tmov\trcx, 1");
    r#gen.emit(&format!("{}:", label("step")));
    // a step of MIN, the only one that doubles to 0, acts like -MAX
    r#gen.emit("\tlea\trax, [rcx + rcx]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjnz\t{}", label("bounds")));
    r#gen.emit("\tinc\trcx");
    r#gen.emit(&format!("{}:", label("bounds")));
    r#gen.emit("\tmov\tr13, rcx");
    // r10 is -1 for a negative step and 0 otherwise
    r#gen.emit("\tmov\tr10, rcx");
    r#gen.emit("\tsar\tr10, 63");
    // a missing start is len - 1 going backward and 0 going forward
    r#gen.emit("\tmov\trax, rsi");
    r#gen.emit("\ttest\tr8, 1");
    r#gen.emit(&format!("\tjz\t{}", label("start")));
    r#gen.emit("\tlea\trax, [r9 - 1]");
    r#gen.emit("\tand\trax, r10");
    r#gen.emit(&format!("\tjmp\t{}", label("start_done")));
    r#gen.emit(&format!("{}:", label("start")));
    clamp_slice_bound(r#gen, &label("start"));
    r#gen.emit("\tmov\tr12, rax");
    // a missing stop is -1 going backward and len going forward
    r#gen.emit("\tmov\trax, rdx");
    r#gen.emit("\ttest\tr8, 2");
    r#gen.emit(&format!("\tjz\t{}", label("stop")));
    r#gen.emit("\tmov\trax, r10");
    r#gen.emit("\tnot\trax");
    r#gen.emit("\tand\trax, r9");
    r#gen.emit("\tadd\trax, r10");
    r#gen.emit(&format!("\tjmp\t{}", label("stop_done")));
    r#gen.emit(&format!("{}:", label("stop")));
    clamp_slice_bound(r#gen, &label("stop"));
    // the distance to cover, start - stop going backward and stop - start going forward
    r#gen.emit("\tsub\trax, r12");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\ttest\tr10, r10");
    r#gen.emit(&format!("\tjz\t{}", label("count")));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tneg\trcx");
    r#gen.emit(&format!("{}:", label("count")));
    // count = (distance - 1) / |step| + 1 for a positive distance, else 0
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjle\t{}", label("counted")));
    r#gen.emit("\tdec\trax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tlea\tr14, [rax + 1]");
    r#gen.emit(&format!("{}:", label("counted")));
}

/// Emits the steps of [`slice_bounds`] that clamp the bound in `rax` to a list or string of
/// length `r9`, given, in `r10`, -1 for a negative step or 0 otherwise: a negative bound counts
/// from the end, and a bound still before the start becomes -1 going backward or 0 going
/// forward, while one at or past the end becomes len - 1 going backward or len going forward.
/// The labels start with `prefix`, and the steps end at `<prefix>_done`.
fn clamp_slice_bound(r#gen: &mut dyn AssemblyGenerator, prefix: &str) {
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{prefix}_high"));
    // len is at most 2^60, so adding it cannot overflow
    r#gen.emit("\tadd\trax, r9");
    r#gen.emit(&format!("\tjns\t{prefix}_done"));
    r#gen.emit("\tmov\trax, r10");
    r#gen.emit(&format!("\tjmp\t{prefix}_done"));
    r#gen.emit(&format!("{prefix}_high:"));
    r#gen.emit("\tcmp\trax, r9");
    r#gen.emit(&format!("\tjl\t{prefix}_done"));
    r#gen.emit("\tlea\trax, [r9 + r10]");
    r#gen.emit(&format!("{prefix}_done:"));
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
/// `stone.stdin_pos` up to `stone.stdin_len` are still to be read. `print` writes each line with
/// syscalls as it finishes it, and a prompt is written at once, so it shows before the program
/// waits.
///
/// - `stone.input` writes the string in `rdi` to stdout unless it is null, then returns the next line of
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
    // straight to stdout, since a prompt is never part of a text being built
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit("\tmov\trdx, QWORD PTR [rdi - 16]");
    r#gen.emit("\tcall\tstone.write_out");
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
    r#gen.emit("\tcall\tstone.cstr_len");
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
/// - `stone.parse_int_result` and `stone.parse_float_result` read the same way, but return a new
///   result instead of failing: `ok` of the number (tag 0), or `err` (tag 1) of the message the
///   others fail with
/// - `stone.fail_quoted` stops the program with the message `rdi` followed by the string `rsi`
///   and a closing quote
/// - `stone.map_missing_str` stops the program because a map has no entry for the string key
///   `rdi`, as in `key not found: 'pear'`
pub fn parse_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    // the pieces of a message are joined with stone.str_concat, so they are strings
    r#gen.emit("\t.data");
    for (label, text) in [
        (
            ".Lstone_int_invalid",
            "invalid literal for int() with base 10: '",
        ),
        (".Lstone_int_range", "int() argument out of range: '"),
        (
            ".Lstone_float_invalid",
            "could not convert string to float: '",
        ),
        (".Lstone_quote", "'"),
        (".Lstone_key_missing", "key not found: '"),
    ] {
        immortal_string(r#gen, label, text);
    }
    r#gen.emit("\t.section\t.rodata");
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

    if used.contains(&"stone.map_missing_str") {
        r#gen.emit("stone.map_missing_str:");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit("\tlea\trdi, [rip + .Lstone_key_missing]");
        r#gen.emit("\tjmp\tstone.fail_quoted");
    }

    // each core returns the number in rax and 0 in rdx, or the start of the message in rdx and
    // the text in rsi, for the entry points to fail with or to make an err of
    let ints = used.contains(&"stone.parse_int") || used.contains(&"stone.parse_int_result");
    let floats = used.contains(&"stone.parse_float") || used.contains(&"stone.parse_float_result");
    for (label, core) in [
        ("stone.parse_int", ".Lparse_int_core"),
        ("stone.parse_float", ".Lparse_float_core"),
    ] {
        if used.contains(&label) {
            r#gen.emit(&format!("{label}:"));
            r#gen.emit(&format!("\tcall\t{core}"));
            r#gen.emit("\ttest\trdx, rdx");
            r#gen.emit(&format!("\tjnz\t{core}_fail"));
            r#gen.emit("\tret");
            r#gen.emit(&format!("{core}_fail:"));
            r#gen.emit("\tmov\trdi, rdx");
            r#gen.emit("\tjmp\tstone.fail_quoted");
        }
        let result = format!("{label}_result");
        if used.contains(&result.as_str()) {
            r#gen.emit(&format!("{result}:"));
            r#gen.emit(&format!("\tcall\t{core}"));
            r#gen.emit("\tjmp\t.Lparse_result");
        }
    }
    if used.contains(&"stone.parse_int_result") || used.contains(&"stone.parse_float_result") {
        // rbx holds the number, then the first piece of the message, and r12 the message
        r#gen.emit(".Lparse_result:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\ttest\trdx, rdx");
        r#gen.emit("\tjnz\t.Lparse_result_err");
        r#gen.emit("\tmov\trbx, rax");
        r#gen.emit("\txor\tedi, edi"); // ok
        r#gen.emit("\txor\tesi, esi"); // an uncounted slot
        r#gen.emit("\tmov\tedx, 1");
        r#gen.emit("\tcall\tstone.enum_new");
        r#gen.emit("\tmov\tQWORD PTR [rax + 16], rbx");
        r#gen.emit("\tjmp\t.Lparse_result_done");
        r#gen.emit(".Lparse_result_err:");
        r#gen.emit("\tmov\trdi, rdx");
        r#gen.emit("\tcall\tstone.str_concat");
        r#gen.emit("\tmov\trbx, rax");
        r#gen.emit("\tmov\trdi, rax");
        r#gen.emit("\tlea\trsi, [rip + .Lstone_quote]");
        r#gen.emit("\tcall\tstone.str_concat");
        r#gen.emit("\tmov\tr12, rax");
        // the first piece was only for joining
        r#gen.emit("\tdec\tQWORD PTR [rbx - 8]");
        r#gen.emit("\tjnz\t.Lparse_result_joined");
        r#gen.emit("\tmov\trax, rbx");
        r#gen.emit("\tcall\tstone.free_str");
        r#gen.emit(".Lparse_result_joined:");
        r#gen.emit("\tmov\tedi, 1"); // err
        r#gen.emit("\tmov\tesi, 1"); // a string slot
        r#gen.emit("\tmov\tedx, 1");
        r#gen.emit("\tcall\tstone.enum_new");
        r#gen.emit("\tmov\tQWORD PTR [rax + 16], r12");
        r#gen.emit(".Lparse_result_done:");
        r#gen.emit("\tlea\trsp, [rbp - 16]");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }

    if ints {
        // rdi keeps the text, rsi is the cursor, rdx where the digits start, r10 the value so far,
        // negated so that the most negative int fits, r8 whether a minus sign came first, and r9
        // whether the value overflowed
        r#gen.emit(".Lparse_int_core:");
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
        r#gen.emit("\tlea\trdx, [rip + .Lstone_int_invalid]");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_end:");
        r#gen.emit("\ttest\tr9d, r9d");
        r#gen.emit("\tjnz\t.Lparse_int_range");
        r#gen.emit("\tmov\trax, r10");
        r#gen.emit("\ttest\tr8d, r8d");
        r#gen.emit("\tjnz\t.Lparse_int_done");
        r#gen.emit("\tneg\trax");
        r#gen.emit("\tjo\t.Lparse_int_range"); // 9223372036854775808 has no positive int
        r#gen.emit(".Lparse_int_done:");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_range:");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit("\tlea\trdx, [rip + .Lstone_int_range]");
        r#gen.emit("\tret");
    }

    if floats {
        // rbx holds the text, r12 the cursor, and r13 a count of digits
        r#gen.emit(".Lparse_float_core:");
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
        r#gen.emit("\tlea\trdx, [rip + .Lstone_float_invalid]");
        r#gen.emit("\tmov\trsi, rbx");
        r#gen.emit("\tjmp\t.Lparse_float_return");
        r#gen.emit(".Lparse_float_valid:");
        r#gen.emit("\tmov\trdi, rbx");
        r#gen.emit("\tcall\tstone.decimal_to_float");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(".Lparse_float_return:");
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
    // text and its size across stone.str_new
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
    r#gen.emit("\tlea\trdi, [r12 - 1]");
    r#gen.emit("\tcall\tstone.str_new");
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

/// Emits the string methods and the routines behind indexing, slicing, and ordering strings,
/// each emitted only if named in `used`. `strip` and `split` follow `stdlib::strip`,
/// `stdlib::split_whitespace`, and `stdlib::split`, and indexes and slices follow
/// `stdlib::str_at` and `stdlib::str_slice`. They need the string runtime, and the `split`
/// routines need the list runtime too. `stone.str_split` jumps to `empty_separator`, a failure
/// label, when given an empty separator, and `stone.str_at` and `stone.str_slice_of` jump to
/// `cut_character` when the result would hold part of a character.
///
/// - `stone.str_strip` returns the string in `rdi` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `rdi` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `rdi` between each `rsi`
/// - `stone.str_at` returns the byte at position `rsi` of the string in `rdi` as a string of one
///   byte, from a table of immortal ones, so it allocates nothing. The caller checks that `rsi`
///   is in range.
/// - `stone.str_slice_of` returns a new string of the bytes of `rdi` that the slice from `rsi` to
///   `rdx` by `rcx` picks, with the missing bounds in `r8` as `stone.list_slice` takes them
/// - `stone.str_cmp` returns -1, 0, or 1 as the string in `rdi` orders before, the same as, or
///   after the one in `rsi`, comparing bytes as unsigned numbers
/// - `stone.str_join` returns a new string of the strings in list `rsi` with `rdi` between each
///   pair
/// - `stone.str_position` returns the byte index where `rsi` first appears in `rdi`, or -1
/// - `stone.str_starts` returns 1 if the string in `rdi` begins with the one in `rsi`, else 0
/// - `stone.str_ends` returns 1 if the string in `rdi` ends with the one in `rsi`, else 0
/// - `stone.str_char` returns the character that starts at byte `rsi` of the string in `rdi`, a
///   string of its 1 to 4 bytes, for a `for` loop over a string
pub fn string_methods(
    r#gen: &mut dyn AssemblyGenerator,
    used: &[&str],
    empty_separator: &str,
    cut_character: &str,
) {
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
        r#gen.emit("\tcall\tstone.str_len");
        r#gen.emit("\tlea\tr12, [rbx + rax]");
        r#gen.emit(".Lstr_strip_lead:");
        r#gen.emit("\tmovzx\tecx, byte ptr [rbx]");
        r#gen.emit("\tinc\trbx");
        jump_if_space(r#gen, "ecx", ".Lstr_strip_lead");
        r#gen.emit("\tdec\trbx");
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
    if uses("stone.str_split") || uses("stone.str_position") {
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
    }
    if uses("stone.str_split") {
        // rbx holds where the piece starts, r12 the list, r13 the separator, r14 its length, and r15
        // where it was found
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
        r#gen.emit("\tcall\tstone.cstr_len"); // rbx points inside the string
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
    if uses("stone.str_at") || uses("stone.str_char") {
        byte_strings(r#gen);
    }
    if uses("stone.str_at") {
        r#gen.emit("stone.str_at:");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + rsi]");
        r#gen.emit("\ttest\tal, al");
        r#gen.emit(&format!("\tjs\t{cut_character}")); // past ascii, so part of a character
        byte_string(r#gen);
    }
    if uses("stone.str_char") {
        r#gen.emit("stone.str_char:");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + rsi]");
        r#gen.emit("\ttest\tal, al");
        r#gen.emit("\tjs\t.Lstr_char_long");
        byte_string(r#gen);
        // a lead byte 110xxxxx starts 2 bytes, 1110xxxx 3, and 11110xxx 4, and a byte of any other
        // form is taken alone; rdx is how many bytes are left, which bounds the step
        r#gen.emit(".Lstr_char_long:");
        r#gen.emit("\tmov\trdx, QWORD PTR [rdi - 16]");
        r#gen.emit("\tsub\trdx, rsi");
        r#gen.emit("\tadd\trdi, rsi");
        r#gen.emit("\tmov\tesi, 1");
        for (below, width) in [(192, 2), (224, 3), (240, 4)] {
            r#gen.emit(&format!("\tcmp\teax, {below}"));
            r#gen.emit("\tjb\t.Lstr_char_copy");
            r#gen.emit(&format!("\tmov\tesi, {width}"));
        }
        r#gen.emit(".Lstr_char_copy:");
        r#gen.emit("\tcmp\trsi, rdx");
        r#gen.emit("\tcmova\trsi, rdx");
        r#gen.emit("\tjmp\tstone.str_slice");
    }
    if uses("stone.str_slice_of") {
        str_slice_of(r#gen, cut_character);
    }
    if uses("stone.str_cmp") {
        r#gen.emit("stone.str_cmp:");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi]");
        r#gen.emit("\tmovzx\tecx, byte ptr [rsi]");
        r#gen.emit("\tcmp\teax, ecx");
        r#gen.emit("\tjne\t.Lstr_cmp_differ");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit("\tjz\t.Lstr_cmp_done"); // both ended together, so rax is 0
        r#gen.emit("\tinc\trdi");
        r#gen.emit("\tinc\trsi");
        r#gen.emit("\tjmp\tstone.str_cmp");
        // a string that ends first has the smaller byte, its terminator
        r#gen.emit(".Lstr_cmp_differ:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tmov\trcx, -1");
        r#gen.emit("\tcmovb\trax, rcx");
        r#gen.emit(".Lstr_cmp_done:");
        r#gen.emit("\tret");
    }
    if uses("stone.str_join") {
        str_join(r#gen);
    }
    if uses("stone.str_position") {
        // stone.str_find leaves r8 alone, so it keeps the start
        r#gen.emit("stone.str_position:");
        r#gen.emit("\tmov\tr8, rdi");
        r#gen.emit("\tcall\tstone.str_find");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjz\t.Lstr_position_none");
        r#gen.emit("\tsub\trax, r8");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_position_none:");
        r#gen.emit("\tmov\trax, -1");
        r#gen.emit("\tret");
    }
    if uses("stone.str_ends") {
        // the suffix must start where the string has exactly its length left
        r#gen.emit("stone.str_ends:");
        r#gen.emit("\tmov\trax, QWORD PTR [rdi - 16]");
        r#gen.emit("\tsub\trax, QWORD PTR [rsi - 16]");
        r#gen.emit("\tjb\t.Lstr_starts_no"); // the suffix is longer
        r#gen.emit("\tadd\trdi, rax");
        r#gen.emit("\tjmp\tstone.str_starts");
    }
    if uses("stone.str_starts") || uses("stone.str_ends") {
        r#gen.emit("stone.str_starts:");
        r#gen.emit("\tmovzx\teax, byte ptr [rsi]");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit("\tjz\t.Lstr_starts_yes");
        r#gen.emit("\tcmp\tal, byte ptr [rdi]");
        r#gen.emit("\tjne\t.Lstr_starts_no");
        r#gen.emit("\tinc\trdi");
        r#gen.emit("\tinc\trsi");
        r#gen.emit("\tjmp\tstone.str_starts");
        r#gen.emit(".Lstr_starts_yes:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_starts_no:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tret");
    }
}

/// Emits the table of the 128 one-byte strings that `stone.str_at` and `stone.str_char` (see
/// [`string_methods`]) return for an ASCII byte, so neither allocates for one. Each entry is 32
/// bytes: a length of 1, an immortal reference count, then the byte and its terminator as one
/// little-endian quad, and a quad of padding, so entry `b` starts at `32 * b` and its string 16
/// bytes later. Entry 0 would be a null byte, which neither returns, since no string holds one.
fn byte_strings(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    r#gen.emit("\t.balign\t8");
    r#gen.emit(".Lstone_bytes:");
    for byte in 0..128 {
        r#gen.emit("\t.quad\t1");
        r#gen.emit(&format!("\t.quad\t{IMMORTAL}"));
        r#gen.emit(&format!("\t.quad\t{byte}"));
        r#gen.emit("\t.quad\t0");
    }
    r#gen.emit("\t.text");
}

/// Emits the end of a routine that returns the string of the ASCII byte in `eax` from the table
/// [`byte_strings`] emits.
fn byte_string(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tshl\teax, 5");
    r#gen.emit("\tlea\trcx, [rip + .Lstone_bytes]");
    r#gen.emit("\tlea\trax, [rcx + rax + 16]");
    r#gen.emit("\tret");
}

/// Emits `stone.str_slice_of` (see [`string_methods`]), which follows `stdlib::str_slice`: a
/// slice by 1 copies the bytes between its ends once both are between characters, and any other
/// slice copies the bytes it picks once each is known to be ascii.
fn str_slice_of(r#gen: &mut dyn AssemblyGenerator, cut_character: &str) {
    // rbx holds the string, r12 the first index, r13 the step, r14 the count, and r15 the new
    // string; r9 holds the length while the bounds are clamped
    r#gen.emit("stone.str_slice_of:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    // stone.str_len only changes rax
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\tr9, rax");
    slice_bounds(r#gen, "str_slice_of");
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit("\tjz\t.Lstr_slice_of_copy"); // an empty slice cuts nothing
    r#gen.emit("\tcmp\tr13, 1");
    r#gen.emit("\tjne\t.Lstr_slice_of_check");
    // a byte of the form 10xxxxxx continues a character, so neither end may be one
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + r12]");
    r#gen.emit("\tand\teax, 192");
    r#gen.emit("\tcmp\teax, 128");
    r#gen.emit(&format!("\tje\t{cut_character}"));
    r#gen.emit("\tlea\trcx, [r12 + r14]");
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rcx]");
    r#gen.emit("\tand\teax, 192");
    r#gen.emit("\tcmp\teax, 128");
    r#gen.emit(&format!("\tje\t{cut_character}"));
    r#gen.emit("\tlea\trdi, [rbx + r12]");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tjmp\t.Lstr_slice_of_done");
    // any other step needs every byte it picks to be ascii
    r#gen.emit(".Lstr_slice_of_check:");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lstr_slice_of_check_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Lstr_slice_of_copy");
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rdx]");
    r#gen.emit("\ttest\tal, al");
    r#gen.emit(&format!("\tjs\t{cut_character}"));
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lstr_slice_of_check_loop");
    r#gen.emit(".Lstr_slice_of_copy:");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lstr_slice_of_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Lstr_slice_of_end");
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rdx]");
    r#gen.emit("\tmov\tbyte ptr [r15 + rcx], al");
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lstr_slice_of_loop");
    r#gen.emit(".Lstr_slice_of_end:");
    r#gen.emit("\tmov\tbyte ptr [r15 + rcx], 0");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit(".Lstr_slice_of_done:");
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `stone.str_join` (see [`string_methods`]), which measures every piece, allocates the
/// result once, and then copies the pieces and separators into it.
fn str_join(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the separator, r12 the list, r13 the separator's length, r14 the total length and
    // then the new string, and r15 the index
    r#gen.emit("stone.str_join:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit("\txor\tr15d, r15d");
    // each piece and a separator after it
    r#gen.emit(".Lstr_join_measure:");
    r#gen.emit("\tcmp\tr15, QWORD PTR [r12]");
    r#gen.emit("\tjge\t.Lstr_join_measured");
    r#gen.emit("\tmov\trax, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rax + r15 * 8]");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tlea\tr14, [r14 + r13]");
    r#gen.emit("\tadd\tr14, rax");
    r#gen.emit("\tinc\tr15");
    r#gen.emit("\tjmp\t.Lstr_join_measure");
    // less the separator after the last piece, if there is one
    r#gen.emit(".Lstr_join_measured:");
    r#gen.emit("\ttest\tr15, r15");
    r#gen.emit("\tjz\t.Lstr_join_alloc");
    r#gen.emit("\tsub\tr14, r13");
    r#gen.emit(".Lstr_join_alloc:");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\trdi, rax"); // where the next byte goes
    r#gen.emit("\txor\tr15d, r15d");
    r#gen.emit(".Lstr_join_piece:");
    r#gen.emit("\tcmp\tr15, QWORD PTR [r12]");
    r#gen.emit("\tjge\t.Lstr_join_done");
    r#gen.emit("\ttest\tr15, r15");
    r#gen.emit("\tjz\t.Lstr_join_item");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit(".Lstr_join_item:");
    r#gen.emit("\tmov\trax, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rax + r15 * 8]");
    r#gen.emit(".Lstr_join_byte:");
    r#gen.emit("\tmovzx\teax, byte ptr [rsi]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjz\t.Lstr_join_next");
    r#gen.emit("\tmov\tbyte ptr [rdi], al");
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tjmp\t.Lstr_join_byte");
    r#gen.emit(".Lstr_join_next:");
    r#gen.emit("\tinc\tr15");
    r#gen.emit("\tjmp\t.Lstr_join_piece");
    r#gen.emit(".Lstr_join_done:");
    r#gen.emit("\tmov\tbyte ptr [rdi], 0");
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits the routines behind the `os` module that the program calls, each named in `used`, such
/// as `stone.os_pid`. Each follows a function of `stdlib::os`, making the syscalls behind the libc
/// function the interpreter calls.
/// `cwd_failure` is the failure label for `stone.os_cwd`, needed only if it is used.
///
/// - `stone.os_getenv` returns `stone.getenv` of the string in `rdi`, or null if it is empty or
///   holds `=`, which `stone.getenv` could otherwise match. `stone.os_env` and `stone.os_has_env` use it
/// - `stone.os_env` returns a copy of the variable named by `rdi`, or 0 if it is not set
/// - `stone.os_has_env` returns 1 if the variable named by `rdi` is set, else 0
/// - `stone.os_platform` and `stone.os_arch` return immortal strings
/// - `stone.os_hostname` returns a copy of the host name in `uname`'s answer, as `gethostname`
///   does, or an empty string
/// - `stone.os_cpu_count` returns how many processors [`CPU_ONLINE`] lists, as
///   `sysconf(_SC_NPROCESSORS_ONLN)` does, at least 1
/// - `stone.os_pid` returns the process id
/// - `stone.os_cwd` returns a copy of the current directory, failing if there is none
/// - `stone.os_exit` ends the program with the status in `rdi`, so it never returns
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
        // rax is 0
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
        // returns a counted copy of the C string in rax, which rbx holds across cstr_len
        r#gen.emit("stone.os_copy:");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tmov\trbx, rax");
        r#gen.emit("\tmov\trdi, rax");
        r#gen.emit("\tcall\tstone.cstr_len");
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
}

/// Emits the runtime routines of the `time` module that `used` lists:
///
/// - `stone.time_now` and `stone.time_clock` return the bits of `clock_gettime`'s seconds plus
///   its nanoseconds over 1e9, for `CLOCK_REALTIME` and `CLOCK_MONOTONIC`
/// - `stone.time_sleep` waits for the float whose bits are in `rdi`, splitting it into seconds
///   and nanoseconds as `stdlib::time::split_seconds` does, and calling `nanosleep` again with
///   what is left when a signal interrupts it. It jumps to `negative` for a negative length or
///   nan, and to `too_large` for 2^63 seconds or more
pub fn time_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str], failures: (&str, &str)) {
    let uses = |label: &str| used.contains(&label);
    for (label, clock) in [("stone.time_now", 0), ("stone.time_clock", 1)] {
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
    if uses("stone.time_sleep") {
        let (negative, too_large) = failures;
        // the request is at [rsp] and what is left after a signal at [rsp + 16]
        r#gen.emit("stone.time_sleep:");
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        r#gen.emit("\tsub\trsp, 32");
        r#gen.emit("\tmovq\txmm0, rdi");
        r#gen.emit("\txorpd\txmm1, xmm1");
        // CF is set below 0, and PF and CF for nan
        r#gen.emit("\tucomisd\txmm0, xmm1");
        r#gen.emit(&format!("\tjb\t{negative}"));
        r#gen.emit("\tmovabs\trax, 0x43e0000000000000"); // 2^63
        r#gen.emit("\tmovq\txmm1, rax");
        r#gen.emit("\tucomisd\txmm0, xmm1");
        r#gen.emit(&format!("\tjae\t{too_large}"));
        r#gen.emit("\tcvttsd2si\trax, xmm0");
        r#gen.emit("\tmov\tQWORD PTR [rsp], rax");
        r#gen.emit("\tcvtsi2sd\txmm1, rax");
        r#gen.emit("\tsubsd\txmm0, xmm1");
        r#gen.emit("\tmovabs\trax, 0x41cdcd6500000000"); // 1e9
        r#gen.emit("\tmovq\txmm1, rax");
        r#gen.emit("\tmulsd\txmm0, xmm1");
        r#gen.emit("\tcvttsd2si\trax, xmm0");
        r#gen.emit("\tmov\tQWORD PTR [rsp + 8], rax");
        r#gen.emit(".Ltime_sleep_again:");
        r#gen.emit("\tmov\trdi, rsp");
        r#gen.emit("\tlea\trsi, [rsp + 16]");
        r#gen.emit("\tmov\teax, 35"); // sys_nanosleep
        r#gen.emit("\tsyscall");
        // a signal interrupted it, so sleep for what is left
        r#gen.emit("\tcmp\trax, -4"); // -EINTR
        r#gen.emit("\tjne\t.Ltime_sleep_done");
        r#gen.emit("\tmov\trax, QWORD PTR [rsp + 16]");
        r#gen.emit("\tmov\tQWORD PTR [rsp], rax");
        r#gen.emit("\tmov\trax, QWORD PTR [rsp + 24]");
        r#gen.emit("\tmov\tQWORD PTR [rsp + 8], rax");
        r#gen.emit("\tjmp\t.Ltime_sleep_again");
        r#gen.emit(".Ltime_sleep_done:");
        r#gen.emit("\tleave");
        r#gen.emit("\tret");
    }
}

/// Emits the runtime of the `random` module, for a program that calls any of `used`. It keeps
/// the xoshiro256** state in `.bss` at `stone.random_state`, four words followed by a word that
/// is 1 once something seeded it, and draws exactly what `stdlib::random::Rng` draws:
///
/// - `stone.random_seed` seeds the state from the int in `rdi` through splitmix64, returning 0
/// - `stone.random_next` returns the next 64 bits in `rax`, first seeding the state from
///   `getrandom` if nothing has
/// - `stone.random_float` returns the bits of a float from 0.0 up to but not including 1.0
/// - `stone.random_below` returns a number below the one in `rdi`, throwing away draws that
///   would bias it, or any 64 bits for 0, which stands for 2^64
pub fn random_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    use crate::stdlib::random::{SPLITMIX_GAMMA, SPLITMIX_MUL1, SPLITMIX_MUL2};
    let uses = |label: &str| used.contains(&label);
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.random_state:");
    r#gen.emit("\t.zero\t40");
    r#gen.emit("\t.text");

    // splitmix64, unrolled for the four words of the state
    r#gen.emit("stone.random_seed:");
    r#gen.emit("\tlea\trcx, [rip + stone.random_state]");
    r#gen.emit(&format!("\tmovabs\trsi, {}", SPLITMIX_GAMMA as i64));
    for word in 0..4 {
        r#gen.emit("\tadd\trdi, rsi");
        r#gen.emit("\tmov\trax, rdi");
        for (shift, multiplier) in [
            (30, Some(SPLITMIX_MUL1)),
            (27, Some(SPLITMIX_MUL2)),
            (31, None),
        ] {
            r#gen.emit("\tmov\trdx, rax");
            r#gen.emit(&format!("\tshr\trdx, {shift}"));
            r#gen.emit("\txor\trax, rdx");
            if let Some(multiplier) = multiplier {
                r#gen.emit(&format!("\tmovabs\trdx, {}", multiplier as i64));
                r#gen.emit("\timul\trax, rdx");
            }
        }
        r#gen.emit(&format!("\tmov\tQWORD PTR [rcx + {}], rax", 8 * word));
    }
    r#gen.emit("\tmov\tQWORD PTR [rcx + 32], 1");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");

    // xoshiro256**, with s0 through s3 in r8, rsi, r9, and r10
    r#gen.emit("stone.random_next:");
    r#gen.emit("\tlea\trcx, [rip + stone.random_state]");
    r#gen.emit("\tcmp\tQWORD PTR [rcx + 32], 0");
    r#gen.emit("\tjne\t.Lrandom_next_seeded");
    r#gen.emit("\tsub\trsp, 16");
    r#gen.emit("\tmov\tQWORD PTR [rsp], 0");
    r#gen.emit("\tmov\trdi, rsp");
    r#gen.emit("\tmov\tesi, 8");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\teax, 318"); // sys_getrandom
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\trdi, QWORD PTR [rsp]");
    r#gen.emit("\tadd\trsp, 16");
    r#gen.emit("\tcall\tstone.random_seed");
    r#gen.emit(".Lrandom_next_seeded:");
    r#gen.emit("\tmov\tr8, QWORD PTR [rcx]");
    r#gen.emit("\tmov\trsi, QWORD PTR [rcx + 8]");
    r#gen.emit("\tmov\tr9, QWORD PTR [rcx + 16]");
    r#gen.emit("\tmov\tr10, QWORD PTR [rcx + 24]");
    // the result is rotl(s1 * 5, 7) * 9
    r#gen.emit("\tlea\trax, [rsi + rsi * 4]");
    r#gen.emit("\trol\trax, 7");
    r#gen.emit("\tlea\trax, [rax + rax * 8]");
    r#gen.emit("\tmov\trdx, rsi");
    r#gen.emit("\tshl\trdx, 17");
    r#gen.emit("\txor\tr9, r8");
    r#gen.emit("\txor\tr10, rsi");
    r#gen.emit("\txor\trsi, r9");
    r#gen.emit("\txor\tr8, r10");
    r#gen.emit("\txor\tr9, rdx");
    r#gen.emit("\trol\tr10, 45");
    r#gen.emit("\tmov\tQWORD PTR [rcx], r8");
    r#gen.emit("\tmov\tQWORD PTR [rcx + 8], rsi");
    r#gen.emit("\tmov\tQWORD PTR [rcx + 16], r9");
    r#gen.emit("\tmov\tQWORD PTR [rcx + 24], r10");
    r#gen.emit("\tret");

    if uses("stone.random_float") {
        // the top 53 bits over 2^53, which is exact
        r#gen.emit("stone.random_float:");
        r#gen.emit("\tcall\tstone.random_next");
        r#gen.emit("\tshr\trax, 11");
        r#gen.emit("\tcvtsi2sd\txmm0, rax");
        r#gen.emit("\tmovabs\trax, 0x3ca0000000000000"); // 2^-53
        r#gen.emit("\tmovq\txmm1, rax");
        r#gen.emit("\tmulsd\txmm0, xmm1");
        r#gen.emit("\tmovq\trax, xmm0");
        r#gen.emit("\tret");
    }
    if uses("stone.random_below") {
        // the span stays in rbx and the smallest draw to keep, 2^64 % span, in r12
        r#gen.emit("stone.random_below:");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tpush\tr12");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\ttest\trbx, rbx");
        r#gen.emit("\tjz\t.Lrandom_below_any");
        r#gen.emit("\tmov\trax, rbx");
        r#gen.emit("\tneg\trax");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tdiv\trbx");
        r#gen.emit("\tmov\tr12, rdx");
        r#gen.emit(".Lrandom_below_draw:");
        r#gen.emit("\tcall\tstone.random_next");
        r#gen.emit("\tcmp\trax, r12");
        r#gen.emit("\tjb\t.Lrandom_below_draw");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tdiv\trbx");
        r#gen.emit("\tmov\trax, rdx");
        r#gen.emit("\tjmp\t.Lrandom_below_done");
        r#gen.emit(".Lrandom_below_any:");
        r#gen.emit("\tcall\tstone.random_next");
        r#gen.emit(".Lrandom_below_done:");
        r#gen.emit("\tpop\tr12");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tret");
    }
}
