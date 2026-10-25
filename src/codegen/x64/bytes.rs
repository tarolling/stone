//! The x86-64 runtime of `bytes`, which are laid out as strings are (see
//! `builtins::string_runtime`): their length at `[p - 16]`, their count at `[p - 8]`, their bytes,
//! and a 0 after them. The string routines that go by the length, such as `stone.str_len`,
//! `stone.str_concat`, and `stone.str_slice`, work on bytes too; these are the ones that would
//! stop at a 0 byte or need UTF-8 text, as the interpreter's `Value::Bytes` arms do.

use crate::codegen::AssemblyGenerator;

/// Emits the routines `used` lists, each following the System V calling convention:
///
/// - `stone.bytes_eq` returns 1 if the bytes in `rdi` and `rsi` are the same, else 0
/// - `stone.bytes_cmp` returns -1, 0, or 1 as the bytes in `rdi` order before, the same as, or
///   after those in `rsi`, byte by byte as unsigned numbers, the shorter first if one starts the
///   other
/// - `stone.bytes_find` returns where the bytes in `rsi` first appear in `rdi`, or -1
/// - `stone.bytes_at` returns the byte at position `rsi` of `rdi` as an int, which the caller
///   checked is in range
/// - `stone.bytes_slice_of` returns new bytes of what the slice from `rsi` to `rdx` by `rcx`
///   picks from `rdi`, with the missing bounds in `r8`, as `stone.str_slice_of` takes them
/// - `stone.bytes_check` returns where the first value of the list of ints `rdi` that is not a
///   byte is, or -1
/// - `stone.bytes_from_list` returns new bytes of the values of the list `rdi`, which
///   `stone.bytes_check` found are all bytes
pub fn bytes_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    if uses("stone.bytes_eq") {
        r#gen.emit("stone.bytes_eq:");
        r#gen.emit("\tmov\trcx, QWORD PTR [rdi - 16]");
        r#gen.emit("\tcmp\trcx, QWORD PTR [rsi - 16]");
        r#gen.emit("\tjne\t.Lbytes_eq_no");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(".Lbytes_eq_next:");
        r#gen.emit("\tcmp\trdx, rcx");
        r#gen.emit("\tjae\t.Lbytes_eq_yes");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + rdx]");
        r#gen.emit("\tmovzx\tr8d, byte ptr [rsi + rdx]");
        r#gen.emit("\tcmp\teax, r8d");
        r#gen.emit("\tjne\t.Lbytes_eq_no");
        r#gen.emit("\tinc\trdx");
        r#gen.emit("\tjmp\t.Lbytes_eq_next");
        r#gen.emit(".Lbytes_eq_yes:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tret");
        r#gen.emit(".Lbytes_eq_no:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_cmp") {
        // r8 and r9 hold the lengths, rcx the shorter, and rdx the position
        r#gen.emit("stone.bytes_cmp:");
        r#gen.emit("\tmov\tr8, QWORD PTR [rdi - 16]");
        r#gen.emit("\tmov\tr9, QWORD PTR [rsi - 16]");
        r#gen.emit("\tmov\trcx, r8");
        r#gen.emit("\tcmp\trcx, r9");
        r#gen.emit("\tcmova\trcx, r9");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(".Lbytes_cmp_next:");
        r#gen.emit("\tcmp\trdx, rcx");
        r#gen.emit("\tjae\t.Lbytes_cmp_lengths");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + rdx]");
        r#gen.emit("\tmovzx\tr10d, byte ptr [rsi + rdx]");
        r#gen.emit("\tcmp\teax, r10d");
        r#gen.emit("\tjne\t.Lbytes_cmp_differ");
        r#gen.emit("\tinc\trdx");
        r#gen.emit("\tjmp\t.Lbytes_cmp_next");
        // one starts the other, so the shorter comes first
        r#gen.emit(".Lbytes_cmp_lengths:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tcmp\tr8, r9");
        r#gen.emit("\tje\t.Lbytes_cmp_done");
        r#gen.emit(".Lbytes_cmp_differ:");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tmov\trcx, -1");
        r#gen.emit("\tcmovb\trax, rcx");
        r#gen.emit(".Lbytes_cmp_done:");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_find") {
        // r8 holds the length searched, r9 the length sought, rdx where a match may start, and
        // rcx how far it matched
        r#gen.emit("stone.bytes_find:");
        r#gen.emit("\tmov\tr8, QWORD PTR [rdi - 16]");
        r#gen.emit("\tmov\tr9, QWORD PTR [rsi - 16]");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(".Lbytes_find_start:");
        r#gen.emit("\tlea\trax, [rdx + r9]");
        r#gen.emit("\tcmp\trax, r8");
        r#gen.emit("\tja\t.Lbytes_find_none");
        r#gen.emit("\txor\tecx, ecx");
        r#gen.emit(".Lbytes_find_next:");
        r#gen.emit("\tcmp\trcx, r9");
        r#gen.emit("\tjae\t.Lbytes_find_found");
        r#gen.emit("\tlea\tr10, [rdx + rcx]");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + r10]");
        r#gen.emit("\tmovzx\tr11d, byte ptr [rsi + rcx]");
        r#gen.emit("\tcmp\teax, r11d");
        r#gen.emit("\tjne\t.Lbytes_find_on");
        r#gen.emit("\tinc\trcx");
        r#gen.emit("\tjmp\t.Lbytes_find_next");
        r#gen.emit(".Lbytes_find_on:");
        r#gen.emit("\tinc\trdx");
        r#gen.emit("\tjmp\t.Lbytes_find_start");
        r#gen.emit(".Lbytes_find_found:");
        r#gen.emit("\tmov\trax, rdx");
        r#gen.emit("\tret");
        r#gen.emit(".Lbytes_find_none:");
        r#gen.emit("\tmov\trax, -1");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_at") {
        r#gen.emit("stone.bytes_at:");
        r#gen.emit("\tmovzx\teax, byte ptr [rdi + rsi]");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_slice_of") {
        slice_of(r#gen);
    }
    if uses("stone.bytes_check") {
        // rcx holds the count, rsi the values, and rax the position
        r#gen.emit("stone.bytes_check:");
        r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
        r#gen.emit("\tmov\trsi, QWORD PTR [rdi + 16]");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit(".Lbytes_check_next:");
        r#gen.emit("\tcmp\trax, rcx");
        r#gen.emit("\tjae\t.Lbytes_check_none");
        // unsigned, so a negative value is out of range too
        r#gen.emit("\tcmp\tQWORD PTR [rsi + rax * 8], 255");
        r#gen.emit("\tja\t.Lbytes_check_done");
        r#gen.emit("\tinc\trax");
        r#gen.emit("\tjmp\t.Lbytes_check_next");
        r#gen.emit(".Lbytes_check_none:");
        r#gen.emit("\tmov\trax, -1");
        r#gen.emit(".Lbytes_check_done:");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_from_list") {
        // rbx holds the list; rsi the values, rcx the count, and rdx the position
        r#gen.emit("stone.bytes_from_list:");
        r#gen.emit("\tpush\trbx");
        r#gen.emit("\tmov\trbx, rdi");
        r#gen.emit("\tmov\trdi, QWORD PTR [rbx]");
        r#gen.emit("\tcall\tstone.str_new");
        r#gen.emit("\tmov\tr8, rax");
        r#gen.emit("\tmov\trcx, QWORD PTR [rbx]");
        r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(".Lbytes_from_list_next:");
        r#gen.emit("\tcmp\trdx, rcx");
        r#gen.emit("\tjae\t.Lbytes_from_list_done");
        r#gen.emit("\tmov\trax, QWORD PTR [rsi + rdx * 8]");
        r#gen.emit("\tmov\tbyte ptr [r8 + rdx], al");
        r#gen.emit("\tinc\trdx");
        r#gen.emit("\tjmp\t.Lbytes_from_list_next");
        r#gen.emit(".Lbytes_from_list_done:");
        r#gen.emit("\tmov\trax, r8");
        r#gen.emit("\tpop\trbx");
        r#gen.emit("\tret");
    }
}

/// Emits `stone.bytes_slice_of`, which copies the bytes a slice picks, as `stone.str_slice_of`
/// does without its checks for cut characters.
fn slice_of(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the bytes, r12 the first index, r13 the step, r14 the count, and r15 the new
    // bytes; r9 holds the length while the bounds are clamped
    r#gen.emit("stone.bytes_slice_of:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tcall\tstone.str_len");
    r#gen.emit("\tmov\tr9, rax");
    super::builtins::slice_bounds(r#gen, "bytes_slice_of");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lbytes_slice_of_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Lbytes_slice_of_end");
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rdx]");
    r#gen.emit("\tmov\tbyte ptr [r15 + rcx], al");
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lbytes_slice_of_loop");
    r#gen.emit(".Lbytes_slice_of_end:");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits `stone.print_bytes`, which prints the bytes in `rdi` the way `stdlib::repr_bytes`
/// writes them: `b` and the bytes in single quotes, or double quotes if they hold a `'` but no
/// `"`, with backslashes, the quote, and every byte outside printable ASCII escaped. Each run of
/// bytes that needs no escape is appended at once, as in `stone.print_str_quoted`.
pub fn print_bytes(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds where the run starts, r12 the quote, r13 where the run ends, then the byte that
    // ended it, and r14 where the bytes end
    r#gen.emit("stone.print_bytes:");
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr14, QWORD PTR [rdi - 16]");
    r#gen.emit("\tadd\tr14, rdi");
    // ecx is set by a ' and edx by a "
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(".Lprint_bytes_scan:");
    r#gen.emit("\tcmp\trdi, r14");
    r#gen.emit("\tjae\t.Lprint_bytes_choose");
    r#gen.emit("\tmovzx\teax, byte ptr [rdi]");
    r#gen.emit("\tinc\trdi");
    r#gen.emit("\tcmp\teax, 39"); // '\''
    r#gen.emit("\tjne\t.Lprint_bytes_double");
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit("\tjmp\t.Lprint_bytes_scan");
    r#gen.emit(".Lprint_bytes_double:");
    r#gen.emit("\tcmp\teax, 34"); // '"'
    r#gen.emit("\tjne\t.Lprint_bytes_scan");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tjmp\t.Lprint_bytes_scan");
    r#gen.emit(".Lprint_bytes_choose:");
    r#gen.emit("\tmov\tr12d, 39");
    r#gen.emit("\ttest\tecx, ecx");
    r#gen.emit("\tjz\t.Lprint_bytes_open");
    r#gen.emit("\ttest\tedx, edx");
    r#gen.emit("\tjnz\t.Lprint_bytes_open");
    r#gen.emit("\tmov\tr12d, 34");
    r#gen.emit(".Lprint_bytes_open:");
    r#gen.emit("\tmov\tedi, 98"); // 'b'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tmov\tedi, r12d");
    r#gen.emit("\tcall\tstone.print_char");
    // a run ends at the end or a byte that needs an escape
    r#gen.emit(".Lprint_bytes_run:");
    r#gen.emit("\tmov\tr13, rbx");
    r#gen.emit(".Lprint_bytes_plain:");
    r#gen.emit("\tcmp\tr13, r14");
    r#gen.emit("\tjae\t.Lprint_bytes_flush");
    r#gen.emit("\tmovzx\teax, byte ptr [r13]");
    r#gen.emit("\tcmp\teax, 92"); // '\\'
    r#gen.emit("\tje\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\teax, r12d");
    r#gen.emit("\tje\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\teax, 32");
    r#gen.emit("\tjb\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\teax, 127");
    r#gen.emit("\tjae\t.Lprint_bytes_flush");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tjmp\t.Lprint_bytes_plain");
    r#gen.emit(".Lprint_bytes_flush:");
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit("\tsub\trdx, rbx");
    r#gen.emit("\tjz\t.Lprint_bytes_special");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tcall\tstone.text_append");
    r#gen.emit(".Lprint_bytes_special:");
    r#gen.emit("\tcmp\tr13, r14");
    r#gen.emit("\tjae\t.Lprint_bytes_close");
    r#gen.emit("\tmovzx\teax, byte ptr [r13]");
    r#gen.emit("\tlea\trbx, [r13 + 1]");
    r#gen.emit("\tmov\tr13d, eax");
    r#gen.emit("\tmov\tedi, 92");
    r#gen.emit("\tcall\tstone.print_char");
    // what follows the backslash: n, t, r, the byte itself, or x and two hex digits
    r#gen.emit("\tmov\tedi, 110"); // 'n'
    r#gen.emit("\tcmp\tr13d, 10");
    r#gen.emit("\tje\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tedi, 116"); // 't'
    r#gen.emit("\tcmp\tr13d, 9");
    r#gen.emit("\tje\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tedi, 114"); // 'r'
    r#gen.emit("\tcmp\tr13d, 13");
    r#gen.emit("\tje\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tcmp\tr13d, 92");
    r#gen.emit("\tje\t.Lprint_bytes_code");
    r#gen.emit("\tcmp\tr13d, r12d");
    r#gen.emit("\tje\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tedi, 120"); // 'x'
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tshr\tedi, 4");
    r#gen.emit("\tcall\t.Lprint_bytes_hex");
    r#gen.emit("\tmov\tedi, r13d");
    r#gen.emit("\tand\tedi, 15");
    r#gen.emit("\tcall\t.Lprint_bytes_hex");
    r#gen.emit("\tjmp\t.Lprint_bytes_run");
    r#gen.emit(".Lprint_bytes_code:");
    r#gen.emit("\tcall\tstone.print_char");
    r#gen.emit("\tjmp\t.Lprint_bytes_run");
    r#gen.emit(".Lprint_bytes_close:");
    r#gen.emit("\tmov\tedi, r12d");
    r#gen.emit("\tcall\tstone.print_char");
    for reg in ["r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
    // prints the hex digit in edi, from 0 to 15, in lowercase
    r#gen.emit(".Lprint_bytes_hex:");
    r#gen.emit("\tlea\teax, [rdi + 48]"); // '0'
    r#gen.emit("\tlea\tecx, [rdi + 87]"); // 'a' - 10
    r#gen.emit("\tcmp\tedi, 10");
    r#gen.emit("\tcmovae\teax, ecx");
    r#gen.emit("\tmov\tedi, eax");
    r#gen.emit("\tjmp\tstone.print_char");
}
