//! The x86-64 runtime of `bytes`, which are laid out as strings are (see
//! `builtins::string_runtime`): their length at `[p - 16]`, their count at `[p - 8]`, their bytes,
//! and a 0 after them. The string routines that go by the length, such as `stone.str_len`,
//! `stone.str_concat`, and `stone.str_slice`, work on bytes too; these are the ones that would
//! stop at a 0 byte or need UTF-8 text, as the interpreter's `Value::Bytes` arms do.

use crate::codegen::AssemblyGenerator;
use crate::stdlib::gui::MAX_SCALED;

/// Emits the routines `used` lists, each following the System V calling convention:
///
/// - `stone.bytes_eq` returns 1 if the bytes in `rdi` and `rsi` are the same, else 0
/// - `stone.bytes_cmp` returns -1, 0, or 1 as the bytes in `rdi` order before, the same as, or
///   after those in `rsi`, byte by byte as unsigned numbers, the shorter first if one starts the
///   other
/// - `stone.bytes_find` returns where the bytes in `rsi` first appear in `rdi`, or -1
/// - `stone.bytes_at` returns the byte at position `rsi` of `rdi` as an int, which the caller
///   checked is in range
/// - `stone.bytes_slice_of` returns the bytes the slice from `rsi` to `rdx` by `rcx` picks from
///   `rdi`, with the missing bounds in `r8`, as `stone.str_slice_of` takes them: `rdi` itself,
///   retained, if that is all of them, and otherwise new bytes
/// - `stone.bytes_check` returns where the first value of the list of ints `rdi` that is not a
///   byte is, or -1
/// - `stone.bytes_from_list` returns new bytes of the values of the list `rdi`, which
///   `stone.bytes_check` found are all bytes
/// - `stone.gui_pack` returns new bytes of the `rdx` colors of the list `rdi` from `rsi`, laid
///   out as `rcx` says, as `stdlib::gui::pack` does
/// - `stone.gui_pack_scaled` returns new bytes of the image `rsi` by `rdx` whose colors are the
///   list `rdi`, scaled to `rcx` by `r8`, laid out as `r9` says, as `stdlib::gui::pack_scaled`
///   does
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
    if uses("stone.gui_pack") {
        pack(r#gen);
    }
    if uses("stone.gui_pack_scaled") {
        pack_scaled(r#gen);
    }
}

/// Emits `stone.gui_pack`, which writes each color's bytes one at a time: blue, green, red, and
/// 0 for format 0, the same backward for format 1, blue, green, red, and bits 24 to 31 for
/// format 3, and red, green, and blue for any other.
fn pack(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the list, r12 the first position, r13 the count, r14 the format, and r9 the new
    // bytes; rsi points at the colors, rdi where the next color's bytes go, and rcx counts
    r#gen.emit("stone.gui_pack:");
    for reg in ["rbx", "r12", "r13", "r14"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr14, rcx");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx]");
    // the range is cut to the list
    r#gen.emit("\ttest\trsi, rsi");
    r#gen.emit("\tjns\t.Lgui_pack_start");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(".Lgui_pack_start:");
    r#gen.emit("\tcmp\trsi, r8");
    r#gen.emit("\tjle\t.Lgui_pack_started");
    r#gen.emit("\tmov\trsi, r8");
    r#gen.emit(".Lgui_pack_started:");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tsub\tr8, rsi");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjns\t.Lgui_pack_count");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(".Lgui_pack_count:");
    r#gen.emit("\tcmp\trdx, r8");
    r#gen.emit("\tjle\t.Lgui_pack_counted");
    r#gen.emit("\tmov\trdx, r8");
    r#gen.emit(".Lgui_pack_counted:");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tshl\trdi, 2");
    r#gen.emit("\tcmp\tr14, 1");
    r#gen.emit("\tjbe\t.Lgui_pack_sized");
    r#gen.emit("\tcmp\tr14, 3");
    r#gen.emit("\tje\t.Lgui_pack_sized");
    r#gen.emit("\tsub\trdi, r13");
    r#gen.emit(".Lgui_pack_sized:");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr9, rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tlea\trsi, [rsi + r12 * 8]");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lgui_pack_next:");
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit("\tjae\t.Lgui_pack_done");
    r#gen.emit("\tmov\trdx, QWORD PTR [rsi + rcx * 8]");
    r#gen.emit("\tinc\trcx");
    store_color(r#gen, "r14", ".Lgui_pack", ".Lgui_pack_next");
    r#gen.emit(".Lgui_pack_done:");
    r#gen.emit("\tmov\trax, r9");
    for reg in ["r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tret");
}

/// Emits code that stores the color in `rdx` at `rdi` as the format in `format` (a register)
/// lays it out, moves `rdi` past it, and jumps to `next`, changing `rax` and `rdx`. Its labels
/// start with `prefix`.
fn store_color(r#gen: &mut dyn AssemblyGenerator, format: &str, prefix: &str, next: &str) {
    r#gen.emit(&format!("\tcmp\t{format}, 1"));
    r#gen.emit(&format!("\tje\t{prefix}_xrgb"));
    r#gen.emit(&format!("\tjb\t{prefix}_bgrx"));
    r#gen.emit(&format!("\tcmp\t{format}, 3"));
    r#gen.emit(&format!("\tje\t{prefix}_bgra"));
    for (offset, last) in [(2, false), (1, false), (0, true)] {
        pack_byte(r#gen, offset, last);
    }
    r#gen.emit("\tadd\trdi, 3");
    r#gen.emit(&format!("\tjmp\t{next}"));
    r#gen.emit(&format!("{prefix}_bgrx:"));
    for (offset, last) in [(0, false), (1, false), (2, true)] {
        pack_byte(r#gen, offset, last);
    }
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tmov\tbyte ptr [rdi + 3], al");
    r#gen.emit("\tadd\trdi, 4");
    r#gen.emit(&format!("\tjmp\t{next}"));
    r#gen.emit(&format!("{prefix}_bgra:"));
    for (offset, last) in [(0, false), (1, false), (2, false), (3, true)] {
        pack_byte(r#gen, offset, last);
    }
    r#gen.emit("\tadd\trdi, 4");
    r#gen.emit(&format!("\tjmp\t{next}"));
    r#gen.emit(&format!("{prefix}_xrgb:"));
    for (offset, last) in [(3, false), (2, false), (1, true)] {
        pack_byte(r#gen, offset, last);
    }
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tmov\tbyte ptr [rdi], al");
    r#gen.emit("\tadd\trdi, 4");
    r#gen.emit(&format!("\tjmp\t{next}"));
}

/// Emits `stone.gui_pack_scaled`, which writes each pixel of the scaled image in turn, as
/// `stdlib::gui::pack_scaled` does: the row it takes is a division per row, and the pixel of
/// that row a running sum per pixel.
fn pack_scaled(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the colors, r12 the width, r13 the height, r14 the new width, r15 the new
    // height, rbp the format, [rsp] the colors' count, and [rsp + 8] the new bytes; rdi points
    // where the next pixel's bytes go, r8 counts rows, r9 is where the row taken starts, r10
    // counts pixels, r11 is the pixel of the row taken, and rsi the running sum
    r#gen.emit("stone.gui_pack_scaled:");
    for reg in ["rbp", "rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tsub\trsp, 24");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\tr14, rcx");
    r#gen.emit("\tmov\tr15, r8");
    r#gen.emit("\tmov\trbp, r9");
    // no bytes unless every size is from 1 to MAX_SCALED, which as unsigned is one test each
    for reg in ["r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tlea\trax, [{reg} - 1]"));
        r#gen.emit(&format!("\tcmp\trax, {}", MAX_SCALED - 1));
        r#gen.emit("\tja\t.Lgui_pack_scaled_empty");
    }
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\timul\trdi, r15");
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tshl\trdi, 2");
    r#gen.emit("\tcmp\trbp, 1");
    r#gen.emit("\tjbe\t.Lgui_pack_scaled_sized");
    r#gen.emit("\tcmp\trbp, 3");
    r#gen.emit("\tje\t.Lgui_pack_scaled_sized");
    r#gen.emit("\tsub\trdi, rax");
    r#gen.emit(".Lgui_pack_scaled_sized:");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 8], rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx]");
    r#gen.emit("\tmov\tQWORD PTR [rsp], rax");
    r#gen.emit("\tmov\trbx, QWORD PTR [rbx + 16]");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit(".Lgui_pack_scaled_row:");
    r#gen.emit("\tcmp\tr8, r15");
    r#gen.emit("\tjae\t.Lgui_pack_scaled_done");
    r#gen.emit("\tmov\trax, r8");
    r#gen.emit("\timul\trax, r13");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tdiv\tr15");
    r#gen.emit("\timul\trax, r12");
    r#gen.emit("\tmov\tr9, rax");
    r#gen.emit("\txor\tr10d, r10d");
    r#gen.emit("\txor\tr11d, r11d");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(".Lgui_pack_scaled_next:");
    r#gen.emit("\tcmp\tr10, r14");
    r#gen.emit("\tjae\t.Lgui_pack_scaled_rowed");
    r#gen.emit("\tinc\tr10");
    // a pixel past the colors is black
    r#gen.emit("\tlea\trax, [r9 + r11]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcmp\trax, QWORD PTR [rsp]");
    r#gen.emit("\tjae\t.Lgui_pack_scaled_color");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + rax * 8]");
    r#gen.emit(".Lgui_pack_scaled_color:");
    store_color(r#gen, "rbp", ".Lgui_pack_scaled", ".Lgui_pack_scaled_step");
    // the pixel taken moves on by one each time the sum of widths passes the new width
    r#gen.emit(".Lgui_pack_scaled_step:");
    r#gen.emit("\tadd\trsi, r12");
    r#gen.emit(".Lgui_pack_scaled_carry:");
    r#gen.emit("\tcmp\trsi, r14");
    r#gen.emit("\tjb\t.Lgui_pack_scaled_next");
    r#gen.emit("\tsub\trsi, r14");
    r#gen.emit("\tinc\tr11");
    r#gen.emit("\tjmp\t.Lgui_pack_scaled_carry");
    r#gen.emit(".Lgui_pack_scaled_rowed:");
    r#gen.emit("\tinc\tr8");
    r#gen.emit("\tjmp\t.Lgui_pack_scaled_row");
    r#gen.emit(".Lgui_pack_scaled_empty:");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 8], rax");
    r#gen.emit(".Lgui_pack_scaled_done:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 8]");
    r#gen.emit("\tadd\trsp, 24");
    for reg in ["r15", "r14", "r13", "r12", "rbx", "rbp"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tret");
}

/// Emits code that stores the lowest byte of `rdx` at `[rdi + offset]` and, unless it is the
/// `last`, shifts the next byte down into its place.
fn pack_byte(r#gen: &mut dyn AssemblyGenerator, offset: i64, last: bool) {
    r#gen.emit("\tmov\trax, rdx");
    match offset {
        0 => r#gen.emit("\tmov\tbyte ptr [rdi], al"),
        _ => r#gen.emit(&format!("\tmov\tbyte ptr [rdi + {offset}], al")),
    }
    if !last {
        r#gen.emit("\tshr\trdx, 8");
    }
}

/// Emits `stone.bytes_slice_of`, which gives the bytes a slice picks, as `stone.str_slice_of`
/// does without its checks for cut characters: the bytes themselves, retained, for a slice of all
/// of them (`data[:]`), one block copy for any other step of 1 (`data[2:5]`), and otherwise a
/// copy of each byte picked in turn (`data[::2]`).
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
    r#gen.emit("\tcmp\tr13, 1");
    r#gen.emit("\tjne\t.Lbytes_slice_of_each");
    // bytes never change, so a slice of all of them is them, shared
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjnz\t.Lbytes_slice_of_run");
    r#gen.emit("\tcmp\tr14, r9");
    r#gen.emit("\tjne\t.Lbytes_slice_of_run");
    r#gen.emit("\tinc\tQWORD PTR [rbx - 8]");
    r#gen.emit("\tmov\trax, rbx");
    r#gen.emit("\tjmp\t.Lbytes_slice_of_end");
    // a run of them is one block copy
    r#gen.emit(".Lbytes_slice_of_run:");
    r#gen.emit("\tlea\trdi, [rbx + r12]");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tjmp\t.Lbytes_slice_of_end");
    // any other step picks them one at a time
    r#gen.emit(".Lbytes_slice_of_each:");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lbytes_slice_of_loop:");
    r#gen.emit("\tcmp\trcx, r14");
    r#gen.emit("\tjge\t.Lbytes_slice_of_copied");
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rdx]");
    r#gen.emit("\tmov\tbyte ptr [r15 + rcx], al");
    r#gen.emit("\tadd\trdx, r13");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tjmp\t.Lbytes_slice_of_loop");
    r#gen.emit(".Lbytes_slice_of_copied:");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit(".Lbytes_slice_of_end:");
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
