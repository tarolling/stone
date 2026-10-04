use crate::codegen::AssemblyGenerator;

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

/// Emits `stone.print_float`, which writes the float whose bits are in `rdi` the way
/// `stdlib::format_float` formats it, such as `1.0`, `0.1`, `1e+16`, or `nan`.
///
/// It asks libc's `snprintf` for `%.*e` with 1, then 2, up to 17 significant digits, stopping at
/// the first text `strtod` reads back as the same float, which is the shortest that round-trips.
/// Exponents from -4 through 15 are then rewritten with `%.*f` to keep those digits, adding `.0`
/// when nothing follows the point. It needs `print`'s routines, calls libc, so it clobbers every
/// caller-saved register, and realigns the stack first, since generated code does not keep it
/// aligned.
pub fn print_float(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_float_e:");
    r#gen.emit("\t.string \"%.*e\"");
    r#gen.emit(".Lstone_float_f:");
    r#gen.emit("\t.string \"%.*f\"");
    r#gen.emit(".Lstone_inf:");
    r#gen.emit("\t.string \"inf\"");
    r#gen.emit(".Lstone_nan:");
    r#gen.emit("\t.string \"nan\"");
    r#gen.emit(".Lstone_point_zero:");
    r#gen.emit("\t.string \".0\"");
    r#gen.emit("\t.text");

    // rbx holds the bits, r12 the digits after the first, r13 the text, and r14 the exponent
    r#gen.emit("stone.print_float:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    // 64 bytes below the saved registers hold the text, which is at most about 40 bytes
    r#gen.emit("\tsub\trsp, 64");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tlea\tr13, [rbp - 96]");
    r#gen.emit("\tmov\trbx, rdi");
    // an exponent of all ones means inf or nan
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tshl\trax, 1"); // drop the sign
    r#gen.emit("\tshr\trax, 53");
    r#gen.emit("\tcmp\trax, 2047");
    r#gen.emit("\tjne\t.Lprint_float_finite");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tshl\trax, 12"); // nan has fraction bits, inf does not
    r#gen.emit("\tlea\trdi, [rip + .Lstone_nan]");
    r#gen.emit("\tjnz\t.Lprint_float_text");
    r#gen.emit("\ttest\trbx, rbx");
    r#gen.emit("\tjns\t.Lprint_float_inf");
    r#gen.emit("\tmov\trdi, 45"); // '-'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit(".Lprint_float_inf:");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_inf]");
    r#gen.emit("\tjmp\t.Lprint_float_text");

    r#gen.emit(".Lprint_float_finite:");
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit(".Lprint_float_digits:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, 64");
    r#gen.emit("\tlea\trdx, [rip + .Lstone_float_e]");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tmovq\txmm0, rbx");
    r#gen.emit("\tmov\teax, 1"); // one vector register argument
    r#gen.emit("\tcall\tsnprintf");
    // 17 significant digits always round-trip
    r#gen.emit("\tcmp\tr12, 16");
    r#gen.emit("\tje\t.Lprint_float_layout");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\tstrtod");
    r#gen.emit("\tmovq\trax, xmm0");
    r#gen.emit("\tcmp\trax, rbx"); // the same bits, which tells -0.0 from 0.0
    r#gen.emit("\tje\t.Lprint_float_layout");
    r#gen.emit("\tinc\tr12");
    r#gen.emit("\tjmp\t.Lprint_float_digits");

    r#gen.emit(".Lprint_float_layout:");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\tesi, 101"); // 'e'
    r#gen.emit("\tcall\tstrchr");
    r#gen.emit("\tlea\trdi, [rax + 1]");
    r#gen.emit("\tcall\tatoi");
    r#gen.emit("\tmovsxd\tr14, eax");
    // outside -4 through 15, the e notation is already right, like 1e+16 or 1.5e-07
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcmp\tr14, -4");
    r#gen.emit("\tjl\t.Lprint_float_text");
    r#gen.emit("\tcmp\tr14, 16");
    r#gen.emit("\tjge\t.Lprint_float_text");
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
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjnz\t.Lprint_float_done");
    r#gen.emit("\tlea\trdi, [rip + .Lstone_point_zero]"); // 100 prints as 100.0

    r#gen.emit(".Lprint_float_text:");
    r#gen.emit("\tcall\tstone.print_str");
    r#gen.emit(".Lprint_float_done:");
    r#gen.emit("\tlea\trsp, [rbp - 32]");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits the string runtime. Strings are null-terminated, and `+` makes a new one with `malloc`.
///
/// - `stone.str_len` returns the length in bytes of the string in `rdi`
/// - `stone.str_eq` returns 1 if the strings in `rdi` and `rsi` hold the same bytes, else 0
/// - `stone.str_concat` returns a new string holding `rdi` followed by `rsi`
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
    r#gen.emit("\tcall\tmalloc");
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
}

/// Emits the list runtime. A list is a pointer to a 24-byte header `{len, cap, data}`, where
/// `data` points to `cap` 8-byte elements. The routines follow the System V calling convention,
/// and realign the stack before calling `malloc` or `realloc`, since generated code does not keep
/// it aligned.
///
/// - `stone.list_new` returns a list of length `rdi` whose elements the caller fills in
/// - `stone.list_append` appends `rsi` to list `rdi`, doubling its capacity when full
///
/// Indexing is generated inline by `X64Generator::gen_list_slot` rather than called here.
pub fn list_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.list_new:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\tr12, rdi"); // length
    r#gen.emit("\tmov\trdi, 24");
    r#gen.emit("\tcall\tmalloc");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx], r12");
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
    r#gen.emit("\tlea\trsp, [rbp - 16]");
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
