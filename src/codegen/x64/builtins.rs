use crate::codegen::AssemblyGenerator;
pub use crate::codegen::context::{IMMORTAL, immortal_string};

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
/// also preserves `rax`, which holds `main`'s exit status.
pub fn memory_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_leak_env:");
    r#gen.emit("\t.string \"STONE_LEAK_CHECK\"");
    r#gen.emit(".Lstone_leak_message:");
    r#gen.emit("\t.string \"error: %ld objects were never freed\\n\"");
    r#gen.emit("\t.text");

    r#gen.emit("stone.alloc:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tadd\trdi, 8"); // room for the count
    r#gen.emit("\tcall\tmalloc");
    r#gen.emit("\tmov\tQWORD PTR [rax], 1");
    r#gen.emit("\tinc\tQWORD PTR [rip + stone.live]");
    r#gen.emit("\tadd\trax, 8");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");

    let saved = ["rdi", "rsi", "r8", "r9", "r10", "r11"];
    r#gen.emit("stone.free_str:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tlea\trdi, [rax - 8]");
    r#gen.emit("\tcall\tfree");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // rbx holds the list and r12 the index of the element being released
    let saved = ["rdi", "rsi", "r8", "r9", "r10", "r11", "rbx", "r12"];
    r#gen.emit("stone.free_list:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
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
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tfree");
    r#gen.emit("\tlea\trdi, [rbx - 8]");
    r#gen.emit("\tcall\tfree");
    r#gen.emit("\tdec\tQWORD PTR [rip + stone.live]");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    r#gen.emit("stone.leak_check:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.live], 0");
    r#gen.emit("\tje\t.Lleak_check_done");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_leak_env]");
    r#gen.emit("\tcall\tgetenv");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lleak_check_done");
    r#gen.emit("\tmov\tedi, 2"); // stderr
    r#gen.emit("\tlea\trsi, [rip + .Lstone_leak_message]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stone.live]");
    r#gen.emit("\txor\teax, eax"); // no vector register arguments
    r#gen.emit("\tcall\tdprintf");
    r#gen.emit("\tmov\tedi, 1");
    r#gen.emit("\tcall\texit");
    r#gen.emit(".Lleak_check_done:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 8]");
    r#gen.emit("\tleave");
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
}

/// Emits the float routines: `stone.format_float`, which writes the float whose bits are in `rdi`
/// into the 64-byte buffer at `rsi` the way `stdlib::format_float` formats it, such as `1.0`,
/// `0.1`, `1e+16`, or `nan`, plus `stone.print_float`, which prints it.
///
/// It asks libc's `snprintf` for `%.*e` with 1, then 2, up to 17 significant digits, stopping at
/// the first text `strtod` reads back as the same float, which is the shortest that round-trips.
/// Exponents from -4 through 15 are then rewritten with `%.*f` to keep those digits, adding `.0`
/// when nothing follows the point. `stone.print_float` needs `print`'s routines. Both call libc,
/// so they clobber every caller-saved register, and realign the stack first, since generated code
/// does not keep it aligned.
pub fn float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_float_e:");
    r#gen.emit("\t.string \"%.*e\"");
    r#gen.emit(".Lstone_float_f:");
    r#gen.emit("\t.string \"%.*f\"");
    r#gen.emit(".Lstone_inf:");
    r#gen.emit("\t.string \"inf\"");
    r#gen.emit(".Lstone_negative_inf:");
    r#gen.emit("\t.string \"-inf\"");
    r#gen.emit(".Lstone_nan:");
    r#gen.emit("\t.string \"nan\"");
    r#gen.emit(".Lstone_point_zero:");
    r#gen.emit("\t.string \".0\"");
    r#gen.emit("\t.text");

    // rbx holds the bits, r12 the digits after the first, r13 the text, and r14 the exponent
    r#gen.emit("stone.format_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tmov\trbx, rdi");
    // an exponent of all ones means inf or nan
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tshl\trax, 1"); // drop the sign
    r#gen.emit("\tshr\trax, 53");
    r#gen.emit("\tcmp\trax, 2047");
    r#gen.emit("\tjne\t.Lformat_float_finite");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tshl\trax, 12"); // nan has fraction bits, inf does not
    r#gen.emit("\tlea\trsi, [rip + .Lstone_nan]");
    r#gen.emit("\tjnz\t.Lformat_float_named");
    r#gen.emit("\tlea\trsi, [rip + .Lstone_inf]");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjns\t.Lformat_float_named");
    r#gen.emit("\tlea\trsi, [rip + .Lstone_negative_inf]");
    r#gen.emit(".Lformat_float_named:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcall\tstrcpy");
    r#gen.emit("\tjmp\t.Lformat_float_done");

    r#gen.emit(".Lformat_float_finite:");
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit(".Lformat_float_digits:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, 64");
    r#gen.emit("\tlea\trdx, [rip + .Lstone_float_e]");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tmovq\txmm0, rbx");
    r#gen.emit("\tmov\teax, 1"); // one vector register argument
    r#gen.emit("\tcall\tsnprintf");
    // 17 significant digits always round-trip
    r#gen.emit("\tcmp\tr12, 16");
    r#gen.emit("\tje\t.Lformat_float_layout");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\tstrtod");
    r#gen.emit("\tmovq\trax, xmm0");
    r#gen.emit("\tcmp\trax, rbx"); // the same bits, which tells -0.0 from 0.0
    r#gen.emit("\tje\t.Lformat_float_layout");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\tjmp\t.Lformat_float_digits");

    r#gen.emit(".Lformat_float_layout:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\tesi, 101"); // 'e'
    r#gen.emit("\tcall\tstrchr");
    r#gen.emit("\tlea\trdi, [rax + 1]");
    r#gen.emit("\tcall\tatoi");
    r#gen.emit("\tmovsxd\tr14, eax");
    // outside -4 through 15, the e notation is already right, like 1e+16 or 1.5e-07
    r#gen.emit("\tcmp\tr14, -4");
    r#gen.emit("\tjl\t.Lformat_float_done");
    r#gen.emit("\tcmp\tr14, 16");
    r#gen.emit("\tjge\t.Lformat_float_done");
    // the same digits written out need max(digits after the first - exponent, 0) decimals
    r#gen.emit("\tsub\tr12, r14");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tcmovs\tr12, rax");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, 64");
    r#gen.emit("\tlea\trdx, [rip + .Lstone_float_f]");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tmovq\txmm0, rbx");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tcall\tsnprintf");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjnz\t.Lformat_float_done");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tlea\trsi, [rip + .Lstone_point_zero]"); // 100 prints as 100.0
    r#gen.emit("\tcall\tstrcat");
    r#gen.emit(".Lformat_float_done:");
    r#gen.emit("\tlea\trsp, [rbp - 32]");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

    // the text goes in 64 bytes of stack
    r#gen.emit("stone.print_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 64");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trsi, rsp");
    r#gen.emit("\tcall\tstone.format_float");
    r#gen.emit("\tmov\trdi, rsp");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
}

/// Emits `stone.str_float`, which returns the float whose bits are in `rdi` as a new string, the
/// way `stone.print_float` prints it. It needs [`float_runtime`] and the memory runtime.
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
    // rbx and r12 hold the bytes and their count across malloc
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
/// follow the System V calling convention, and realign the stack before calling `malloc` or
/// `realloc`, since generated code does not keep it aligned.
///
/// - `stone.list_new` returns a list of length `rdi` whose elements the caller fills in, with
///   `elem` set to `rsi`
/// - `stone.list_append` appends `rsi` to list `rdi`, doubling its capacity when full
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
    r#gen.emit("\tcall\tmalloc");
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
    r#gen.emit("\tcall\trealloc");
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
fn jump_if_space(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tcmp\t{reg}, 32"));
    r#gen.emit(&format!("\tje\t{label}"));
    // tab, newline, vertical tab, form feed, and carriage return are 9 through 13
    r#gen.emit(&format!("\tmov\teax, {reg}"));
    r#gen.emit("\tsub\teax, 9");
    r#gen.emit("\tcmp\teax, 4");
    r#gen.emit(&format!("\tjbe\t{label}"));
}

/// Emits the input routines, which read stdin through libc's buffered `stdin`. Printing writes
/// with syscalls, so a prompt shows before the program waits.
///
/// - `stone.input` prints the string in `rdi` unless it is null, then returns the next line of
///   stdin as a new string without its newline, or an empty string at the end of the input. The
///   line is copied out of `getline`'s buffer, which is then freed
/// - `stone.eof` returns 1 if stdin has nothing left and 0 otherwise, peeking one byte with
///   `getc` and putting it back with `ungetc`
///
/// `stone.input` needs `print`'s routines and the string runtime.
pub fn io_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_empty", "");
    r#gen.emit("\t.text");

    // getline fills the line pointer at [rbp - 16] and its capacity at [rbp - 24]
    r#gen.emit("stone.input:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjz\t.Linput_read");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit(".Linput_read:");
    // a null line asks getline for a new buffer, so each line is a string of its own
    r#gen.emit("\tmov\tQWORD PTR [rbp - 16], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 24], 0");
    r#gen.emit("\tlea\trdi, [rbp - 16]");
    r#gen.emit("\tlea\trsi, [rbp - 24]");
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stdin]");
    r#gen.emit("\tcall\tgetline");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjle\t.Linput_end");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 16]");
    r#gen.emit("\tcmp\tbyte ptr [rdi + rax - 1], 10"); // newline
    r#gen.emit("\tjne\t.Linput_copy");
    r#gen.emit("\tdec\trax");
    // the line becomes a counted string, and getline's buffer is freed
    r#gen.emit(".Linput_copy:");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 8], rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 16]");
    r#gen.emit("\tcall\tfree");
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 8]");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
    // getline can allocate even when nothing is left to read
    r#gen.emit(".Linput_end:");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 16]");
    r#gen.emit("\tcall\tfree");
    r#gen.emit("\tlea\trax, [rip + .Lstone_empty]");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");

    r#gen.emit("stone.eof:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trdi, QWORD PTR [rip + stdin]");
    r#gen.emit("\tcall\tgetc");
    r#gen.emit("\tcmp\teax, -1"); // EOF
    r#gen.emit("\tje\t.Leof_yes");
    r#gen.emit("\tmov\tedi, eax");
    r#gen.emit("\tmov\trsi, QWORD PTR [rip + stdin]");
    r#gen.emit("\tcall\tungetc");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tleave");
    r#gen.emit("\tret");
    r#gen.emit(".Leof_yes:");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tleave");
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
/// `stdlib::parse_float`, and stop the program through `stone.fail` with the same messages. They
/// need the string runtime.
///
/// - `stone.parse_int` returns the int in the string in `rdi`, using libc's `strtoll`, which skips
///   the same whitespace and reports overflow through `errno`
/// - `stone.parse_float` returns the bits of the float in the string in `rdi`, checking the
///   grammar itself, since `strtod` also takes hex floats and `nan(...)`
/// - `stone.fail_quoted` stops the program with the message `rdi` followed by the string `rsi`
///   and a closing quote
pub fn parse_runtime(r#gen: &mut dyn AssemblyGenerator) {
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

    // rbx holds the text, r12 errno's address, r13 the result, and [rbp - 32] where it ended
    r#gen.emit("stone.parse_int:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tsub\trsp, 8");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tcall\t__errno_location");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\tDWORD PTR [r12], 0");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tlea\trsi, [rbp - 32]");
    r#gen.emit("\tmov\tedx, 10");
    r#gen.emit("\tcall\tstrtoll");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbp - 32]");
    r#gen.emit("\tcmp\trsi, rbx"); // no digits
    r#gen.emit("\tje\t.Lparse_int_invalid");
    // only whitespace may follow, which is checked before overflow, as in stdlib::parse_int
    r#gen.emit(".Lparse_int_trailing:");
    r#gen.emit("\tmovzx\tecx, byte ptr [rsi]");
    r#gen.emit("\ttest\tecx, ecx");
    r#gen.emit("\tjz\t.Lparse_int_end");
    r#gen.emit("\tinc\trsi");
    jump_if_space(r#gen, "ecx", ".Lparse_int_trailing");
    r#gen.emit(".Lparse_int_invalid:");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_int_invalid]");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tjmp\tstone.fail_quoted");
    r#gen.emit(".Lparse_int_end:");
    r#gen.emit("\tcmp\tDWORD PTR [r12], 34"); // ERANGE
    r#gen.emit("\tjne\t.Lparse_int_done");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_int_range]");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tjmp\tstone.fail_quoted");
    r#gen.emit(".Lparse_int_done:");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tlea\trsp, [rbp - 24]");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");

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
        let next = format!("{name}_next");
        r#gen.emit("\tmov\trdi, r12");
        r#gen.emit(&format!("\tlea\trsi, [rip + {name}]"));
        r#gen.emit(&format!("\tmov\tedx, {length}"));
        r#gen.emit("\tcall\tstrncasecmp");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit(&format!("\tjnz\t{next}"));
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
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\tstrtod");
    r#gen.emit("\tmovq\trax, xmm0");
    r#gen.emit("\tlea\trsp, [rbp - 24]");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `str` for ints and bools. Floats have `stone.str_float` in [`float_runtime`].
///
/// - `stone.str_int` returns the int in `rdi` as a new string, written like `stone.print_int`
/// - `stone.str_bool` returns a constant `true` if `rdi` is nonzero and `false` otherwise
pub fn conversion_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_str_true", "true");
    immortal_string(r#gen, ".Lstone_str_false", "false");
    r#gen.emit("\t.text");

    // digits are written backwards from the terminator at [rbp - 17], and rbx and r12 hold the
    // text and its size across malloc
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
/// and `stdlib::split`. They need the string and list runtimes, and jump to `empty_separator`, a
/// failure label, when `split` is given an empty separator.
///
/// - `stone.str_strip` returns the string in `rdi` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `rdi` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `rdi` between each `rsi`
pub fn string_methods(r#gen: &mut dyn AssemblyGenerator, empty_separator: &str) {
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
    r#gen.emit("\tcall\tstrstr");
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
