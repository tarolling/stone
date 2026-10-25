//! The arm64 runtime of `bytes`, which mirrors `x64/bytes.rs` routine for routine, under the same
//! labels and contracts, with arguments in `x0` and up and results in `x0`.

use super::{pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;

/// Emits the routines `used` lists, as `x64::bytes::bytes_runtime` describes them.
pub fn bytes_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    if uses("stone.bytes_eq") {
        // x9 holds the length and x11 the position
        r#gen.emit("stone.bytes_eq:");
        r#gen.emit("\tldur\tx9, [x0, #-16]");
        r#gen.emit("\tldur\tx10, [x1, #-16]");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tb.ne\t.Lbytes_eq_no");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit(".Lbytes_eq_next:");
        r#gen.emit("\tcmp\tx11, x9");
        r#gen.emit("\tb.hs\t.Lbytes_eq_yes");
        r#gen.emit("\tldrb\tw12, [x0, x11]");
        r#gen.emit("\tldrb\tw13, [x1, x11]");
        r#gen.emit("\tcmp\tw12, w13");
        r#gen.emit("\tb.ne\t.Lbytes_eq_no");
        r#gen.emit("\tadd\tx11, x11, #1");
        r#gen.emit("\tb\t.Lbytes_eq_next");
        r#gen.emit(".Lbytes_eq_yes:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit("\tret");
        r#gen.emit(".Lbytes_eq_no:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_cmp") {
        // x9 and x10 hold the lengths, x11 the shorter, and x12 the position
        r#gen.emit("stone.bytes_cmp:");
        r#gen.emit("\tldur\tx9, [x0, #-16]");
        r#gen.emit("\tldur\tx10, [x1, #-16]");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tcsel\tx11, x9, x10, lo");
        r#gen.emit("\tmov\tx12, #0");
        r#gen.emit(".Lbytes_cmp_next:");
        r#gen.emit("\tcmp\tx12, x11");
        r#gen.emit("\tb.hs\t.Lbytes_cmp_lengths");
        r#gen.emit("\tldrb\tw13, [x0, x12]");
        r#gen.emit("\tldrb\tw14, [x1, x12]");
        r#gen.emit("\tcmp\tw13, w14");
        r#gen.emit("\tb.ne\t.Lbytes_cmp_differ");
        r#gen.emit("\tadd\tx12, x12, #1");
        r#gen.emit("\tb\t.Lbytes_cmp_next");
        // one starts the other, so the shorter comes first
        r#gen.emit(".Lbytes_cmp_lengths:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tb.eq\t.Lbytes_cmp_done");
        r#gen.emit(".Lbytes_cmp_differ:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit("\tmov\tx15, #-1");
        r#gen.emit("\tcsel\tx0, x15, x0, lo");
        r#gen.emit(".Lbytes_cmp_done:");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_find") {
        // x9 holds the length searched, x10 the length sought, x11 where a match may start, and
        // x13 how far it matched
        r#gen.emit("stone.bytes_find:");
        r#gen.emit("\tldur\tx9, [x0, #-16]");
        r#gen.emit("\tldur\tx10, [x1, #-16]");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit(".Lbytes_find_start:");
        r#gen.emit("\tadd\tx12, x11, x10");
        r#gen.emit("\tcmp\tx12, x9");
        r#gen.emit("\tb.hi\t.Lbytes_find_none");
        r#gen.emit("\tmov\tx13, #0");
        r#gen.emit(".Lbytes_find_next:");
        r#gen.emit("\tcmp\tx13, x10");
        r#gen.emit("\tb.hs\t.Lbytes_find_found");
        r#gen.emit("\tadd\tx14, x11, x13");
        r#gen.emit("\tldrb\tw15, [x0, x14]");
        r#gen.emit("\tldrb\tw16, [x1, x13]");
        r#gen.emit("\tcmp\tw15, w16");
        r#gen.emit("\tb.ne\t.Lbytes_find_on");
        r#gen.emit("\tadd\tx13, x13, #1");
        r#gen.emit("\tb\t.Lbytes_find_next");
        r#gen.emit(".Lbytes_find_on:");
        r#gen.emit("\tadd\tx11, x11, #1");
        r#gen.emit("\tb\t.Lbytes_find_start");
        r#gen.emit(".Lbytes_find_found:");
        r#gen.emit("\tmov\tx0, x11");
        r#gen.emit("\tret");
        r#gen.emit(".Lbytes_find_none:");
        r#gen.emit("\tmov\tx0, #-1");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_at") {
        r#gen.emit("stone.bytes_at:");
        r#gen.emit("\tldrb\tw0, [x0, x1]");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_slice_of") {
        slice_of(r#gen);
    }
    if uses("stone.bytes_check") {
        // x9 holds the count, x10 the values, and x11 the position
        r#gen.emit("stone.bytes_check:");
        r#gen.emit("\tldr\tx9, [x0]");
        r#gen.emit("\tldr\tx10, [x0, #16]");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit(".Lbytes_check_next:");
        r#gen.emit("\tcmp\tx11, x9");
        r#gen.emit("\tb.hs\t.Lbytes_check_none");
        // unsigned, so a negative value is out of range too
        r#gen.emit("\tldr\tx12, [x10, x11, lsl #3]");
        r#gen.emit("\tcmp\tx12, #255");
        r#gen.emit("\tb.hi\t.Lbytes_check_found");
        r#gen.emit("\tadd\tx11, x11, #1");
        r#gen.emit("\tb\t.Lbytes_check_next");
        r#gen.emit(".Lbytes_check_none:");
        r#gen.emit("\tmov\tx0, #-1");
        r#gen.emit("\tret");
        r#gen.emit(".Lbytes_check_found:");
        r#gen.emit("\tmov\tx0, x11");
        r#gen.emit("\tret");
    }
    if uses("stone.bytes_from_list") {
        // x19 holds the list; x9 the count, x10 the values, and x11 the position
        r#gen.emit("stone.bytes_from_list:");
        push_frame(r#gen, &["x19", "x20"]);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tldr\tx0, [x19]");
        r#gen.emit("\tbl\tstone.str_new");
        r#gen.emit("\tldr\tx9, [x19]");
        r#gen.emit("\tldr\tx10, [x19, #16]");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit(".Lbytes_from_list_next:");
        r#gen.emit("\tcmp\tx11, x9");
        r#gen.emit("\tb.hs\t.Lbytes_from_list_done");
        r#gen.emit("\tldr\tx12, [x10, x11, lsl #3]");
        r#gen.emit("\tstrb\tw12, [x0, x11]");
        r#gen.emit("\tadd\tx11, x11, #1");
        r#gen.emit("\tb\t.Lbytes_from_list_next");
        r#gen.emit(".Lbytes_from_list_done:");
        pop_frame(r#gen, &["x19", "x20"]);
    }
}

/// Emits `stone.bytes_slice_of`, as `x64::bytes` does.
fn slice_of(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the bytes, x20 the first index, x21 the step, x22 the count, and x23 the new
    // bytes; x9 holds the length while the bounds are clamped
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.bytes_slice_of:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx9, x0");
    super::builtins::slice_bounds(r#gen, "bytes_slice_of");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx23, x0");
    r#gen.emit("\tmov\tx12, x20");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Lbytes_slice_of_loop:");
    r#gen.emit("\tcmp\tx8, x22");
    r#gen.emit("\tb.ge\t.Lbytes_slice_of_end");
    r#gen.emit("\tldrb\tw9, [x19, x12]");
    r#gen.emit("\tstrb\tw9, [x23, x8]");
    r#gen.emit("\tadd\tx12, x12, x21");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tb\t.Lbytes_slice_of_loop");
    r#gen.emit(".Lbytes_slice_of_end:");
    r#gen.emit("\tmov\tx0, x23");
    pop_frame(r#gen, &saved);
}

/// Emits `stone.print_bytes`, as `x64::bytes` does.
pub fn print_bytes(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds where the run starts, x20 the quote, x21 where the run ends, then the byte that
    // ended it, and x22 where the bytes end
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.print_bytes:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldur\tx22, [x0, #-16]");
    r#gen.emit("\tadd\tx22, x22, x0");
    // x9 is set by a ' and x10 by a "
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit("\tmov\tx10, #0");
    r#gen.emit("\tmov\tx11, x0");
    r#gen.emit(".Lprint_bytes_scan:");
    r#gen.emit("\tcmp\tx11, x22");
    r#gen.emit("\tb.hs\t.Lprint_bytes_choose");
    r#gen.emit("\tldrb\tw12, [x11], #1");
    r#gen.emit("\tcmp\tw12, #39"); // '\''
    r#gen.emit("\tb.ne\t.Lprint_bytes_double");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tb\t.Lprint_bytes_scan");
    r#gen.emit(".Lprint_bytes_double:");
    r#gen.emit("\tcmp\tw12, #34"); // '"'
    r#gen.emit("\tb.ne\t.Lprint_bytes_scan");
    r#gen.emit("\tmov\tx10, #1");
    r#gen.emit("\tb\t.Lprint_bytes_scan");
    r#gen.emit(".Lprint_bytes_choose:");
    r#gen.emit("\tmov\tx20, #39");
    r#gen.emit("\tcbz\tx9, .Lprint_bytes_open");
    r#gen.emit("\tcbnz\tx10, .Lprint_bytes_open");
    r#gen.emit("\tmov\tx20, #34");
    r#gen.emit(".Lprint_bytes_open:");
    r#gen.emit("\tmov\tx0, #98"); // 'b'
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.print_char");
    // a run ends at the end or a byte that needs an escape
    r#gen.emit(".Lprint_bytes_run:");
    r#gen.emit("\tmov\tx21, x19");
    r#gen.emit(".Lprint_bytes_plain:");
    r#gen.emit("\tcmp\tx21, x22");
    r#gen.emit("\tb.hs\t.Lprint_bytes_flush");
    r#gen.emit("\tldrb\tw9, [x21]");
    r#gen.emit("\tcmp\tw9, #92"); // '\\'
    r#gen.emit("\tb.eq\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\tw9, w20");
    r#gen.emit("\tb.eq\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\tw9, #32");
    r#gen.emit("\tb.lo\t.Lprint_bytes_flush");
    r#gen.emit("\tcmp\tw9, #127");
    r#gen.emit("\tb.hs\t.Lprint_bytes_flush");
    r#gen.emit("\tadd\tx21, x21, #1");
    r#gen.emit("\tb\t.Lprint_bytes_plain");
    r#gen.emit(".Lprint_bytes_flush:");
    r#gen.emit("\tsub\tx2, x21, x19");
    r#gen.emit("\tcbz\tx2, .Lprint_bytes_special");
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit("\tbl\tstone.text_append");
    r#gen.emit(".Lprint_bytes_special:");
    r#gen.emit("\tcmp\tx21, x22");
    r#gen.emit("\tb.hs\t.Lprint_bytes_close");
    r#gen.emit("\tldrb\tw9, [x21]");
    r#gen.emit("\tadd\tx19, x21, #1");
    r#gen.emit("\tmov\tx21, x9");
    r#gen.emit("\tmov\tx0, #92");
    r#gen.emit("\tbl\tstone.print_char");
    // what follows the backslash: n, t, r, the byte itself, or x and two hex digits
    r#gen.emit("\tmov\tx0, #110"); // 'n'
    r#gen.emit("\tcmp\tx21, #10");
    r#gen.emit("\tb.eq\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tx0, #116"); // 't'
    r#gen.emit("\tcmp\tx21, #9");
    r#gen.emit("\tb.eq\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tx0, #114"); // 'r'
    r#gen.emit("\tcmp\tx21, #13");
    r#gen.emit("\tb.eq\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tcmp\tx21, #92");
    r#gen.emit("\tb.eq\t.Lprint_bytes_code");
    r#gen.emit("\tcmp\tx21, x20");
    r#gen.emit("\tb.eq\t.Lprint_bytes_code");
    r#gen.emit("\tmov\tx0, #120"); // 'x'
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tlsr\tx0, x21, #4");
    r#gen.emit("\tbl\t.Lprint_bytes_hex");
    r#gen.emit("\tubfx\tx0, x21, #0, #4");
    r#gen.emit("\tbl\t.Lprint_bytes_hex");
    r#gen.emit("\tb\t.Lprint_bytes_run");
    r#gen.emit(".Lprint_bytes_code:");
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tb\t.Lprint_bytes_run");
    r#gen.emit(".Lprint_bytes_close:");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.print_char");
    pop_frame(r#gen, &saved);
    // prints the hex digit in x0, from 0 to 15, in lowercase, returning to the caller's caller
    r#gen.emit(".Lprint_bytes_hex:");
    r#gen.emit("\tadd\tx9, x0, #48"); // '0'
    r#gen.emit("\tadd\tx10, x0, #87"); // 'a' - 10
    r#gen.emit("\tcmp\tx0, #10");
    r#gen.emit("\tcsel\tx0, x10, x9, hs");
    r#gen.emit("\tb\tstone.print_char");
}
