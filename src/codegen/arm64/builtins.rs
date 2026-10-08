//! The hand-written arm64 runtime, which mirrors `x64/builtins.rs` routine for routine, under the
//! same labels and with the same contracts.
//!
//! Routines take their arguments in `x0` and up and return in `x0`, as AAPCS64 does, keep what
//! must survive a call in `x19` and up, and save those with [`push_frame`]. Floats travel as
//! their bits in general registers, as in generated code, and only move to `d0` for libc.

use super::emit::CALLER_SAVED;
use super::{address, pop_frame, push_frame};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::immortal_string;

/// Emits a loop that copies `count` bytes from `src` to `dst`, advancing both and counting
/// `count` down to 0. It clobbers `w16`, and uses `label` and `label_done` as its labels.
///
/// For example, `copy_bytes(gen, "x9", "x19", "x21", ".Lstr_concat_left")` copies `x21` bytes
/// from `x19` to `x9`.
fn copy_bytes(r#gen: &mut dyn AssemblyGenerator, dst: &str, src: &str, count: &str, label: &str) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\tcbz\t{count}, {label}_done"));
    r#gen.emit(&format!("\tldrb\tw16, [{src}], #1"));
    r#gen.emit(&format!("\tstrb\tw16, [{dst}], #1"));
    r#gen.emit(&format!("\tsub\t{count}, {count}, #1"));
    r#gen.emit(&format!("\tb\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits code that loads libc's `stdin` stream into `reg`, through the global offset table.
fn load_stdin(r#gen: &mut dyn AssemblyGenerator, reg: &str) {
    r#gen.emit(&format!("\tadrp\t{reg}, :got:stdin"));
    r#gen.emit(&format!("\tldr\t{reg}, [{reg}, :got_lo12:stdin]"));
    r#gen.emit(&format!("\tldr\t{reg}, [{reg}]"));
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
/// status.
pub fn memory_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_leak_env:");
    r#gen.emit("\t.string \"STONE_LEAK_CHECK\"");
    r#gen.emit(".Lstone_leak_message:");
    r#gen.emit("\t.string \"error: %ld objects were never freed\\n\"");
    r#gen.emit("\t.text");

    r#gen.emit("stone.alloc:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tadd\tx0, x0, #8"); // room for the count
    r#gen.emit("\tbl\tmalloc");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    count_live(r#gen, 1);
    r#gen.emit("\tadd\tx0, x0, #8");
    pop_frame(r#gen, &[]);

    r#gen.emit("stone.free_str:");
    push_frame(r#gen, &CALLER_SAVED);
    r#gen.emit("\tsub\tx0, x9, #8");
    r#gen.emit("\tbl\tfree");
    count_live(r#gen, -1);
    pop_frame(r#gen, &CALLER_SAVED);

    // x19 holds the list and x20 the index of the element being released
    let mut saved = CALLER_SAVED.to_vec();
    saved.extend(["x19", "x20"]);
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
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\tbl\tfree");
    r#gen.emit("\tsub\tx0, x19, #8");
    r#gen.emit("\tbl\tfree");
    count_live(r#gen, -1);
    pop_frame(r#gen, &saved);

    r#gen.emit("stone.leak_check:");
    push_frame(r#gen, &["x0"]);
    r#gen.emit("\tadrp\tx9, stone.live");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.live]");
    r#gen.emit("\tcbz\tx9, .Lleak_check_done");
    address(r#gen, "x0", ".Lstone_leak_env");
    r#gen.emit("\tbl\tgetenv");
    r#gen.emit("\tcbz\tx0, .Lleak_check_done");
    r#gen.emit("\tmov\tx0, #2"); // stderr
    address(r#gen, "x1", ".Lstone_leak_message");
    r#gen.emit("\tadrp\tx2, stone.live");
    r#gen.emit("\tldr\tx2, [x2, :lo12:stone.live]");
    r#gen.emit("\tbl\tdprintf");
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tbl\texit");
    r#gen.emit(".Lleak_check_done:");
    pop_frame(r#gen, &["x0"]);
}

/// Emits `stone.fmod`, which sets `d0` to the remainder of `d0` divided by `d1` with libc's
/// `fmod`, which is exact and takes the dividend's sign, as Rust's `%` does.
///
/// Float `%` is emitted inline rather than as a call, so like the free routines this preserves
/// every register a vreg can be in.
pub fn fmod_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.fmod:");
    push_frame(r#gen, &CALLER_SAVED);
    r#gen.emit("\tbl\tfmod");
    pop_frame(r#gen, &CALLER_SAVED);
}

/// Emits the routines behind `print`, each writing one value to stdout with no newline, so a call
/// like `print("n", 1)` writes `n`, a space, `1`, and a newline with four calls.
///
/// - `stone.print_int` writes the signed integer in `x0`
/// - `stone.print_str` writes the null-terminated string `x0` points to
/// - `stone.print_bool` writes `true` if `x0` is nonzero and `false` otherwise
/// - `stone.print_none` writes `none`
/// - `stone.print_char` writes the byte in `w0`
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
}

/// Emits the float routines: `stone.format_float`, which writes the float whose bits are in `x0`
/// into the 64-byte buffer at `x1` the way `stdlib::format_float` formats it, such as `1.0`,
/// `0.1`, `1e+16`, or `nan`, plus `stone.print_float`, which prints it.
///
/// It asks libc's `snprintf` for `%.*e` with 1, then 2, up to 17 significant digits, stopping at
/// the first text `strtod` reads back as the same float, which is the shortest that round-trips.
/// Exponents from -4 through 15 are then rewritten with `%.*f` to keep those digits, adding `.0`
/// when nothing follows the point. `snprintf` is variadic, and AAPCS64 passes the precision in
/// `w3` and the float in `d0`, as for any other function. `stone.print_float` needs `print`'s
/// routines. Both call libc, so they clobber every caller-saved register.
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

    // x19 holds the bits, x20 the digits after the first, x21 the text, and x22 the exponent
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.format_float:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tmov\tx19, x0");
    // an exponent of all ones means inf or nan
    r#gen.emit("\tubfx\tx9, x19, #52, #11");
    r#gen.emit("\tcmp\tx9, #2047");
    r#gen.emit("\tb.ne\t.Lformat_float_finite");
    r#gen.emit("\tlsl\tx9, x19, #12"); // nan has fraction bits, inf does not
    address(r#gen, "x1", ".Lstone_nan");
    r#gen.emit("\tcbnz\tx9, .Lformat_float_named");
    address(r#gen, "x1", ".Lstone_inf");
    r#gen.emit("\ttbz\tx19, #63, .Lformat_float_named");
    address(r#gen, "x1", ".Lstone_negative_inf");
    r#gen.emit(".Lformat_float_named:");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\tstrcpy");
    r#gen.emit("\tb\t.Lformat_float_done");

    r#gen.emit(".Lformat_float_finite:");
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit(".Lformat_float_digits:");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, #64");
    address(r#gen, "x2", ".Lstone_float_e");
    r#gen.emit("\tmov\tw3, w20");
    r#gen.emit("\tfmov\td0, x19");
    r#gen.emit("\tbl\tsnprintf");
    // 17 significant digits always round-trip
    r#gen.emit("\tcmp\tx20, #16");
    r#gen.emit("\tb.eq\t.Lformat_float_layout");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tbl\tstrtod");
    r#gen.emit("\tfmov\tx9, d0");
    r#gen.emit("\tcmp\tx9, x19"); // the same bits, which tells -0.0 from 0.0
    r#gen.emit("\tb.eq\t.Lformat_float_layout");
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit("\tb\t.Lformat_float_digits");

    r#gen.emit(".Lformat_float_layout:");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tw1, #101"); // 'e'
    r#gen.emit("\tbl\tstrchr");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tbl\tatoi");
    r#gen.emit("\tsxtw\tx22, w0");
    // outside -4 through 15, the e notation is already right, like 1e+16 or 1.5e-07
    r#gen.emit("\tcmn\tx22, #4");
    r#gen.emit("\tb.lt\t.Lformat_float_done");
    r#gen.emit("\tcmp\tx22, #16");
    r#gen.emit("\tb.ge\t.Lformat_float_done");
    // the same digits written out need max(digits after the first - exponent, 0) decimals
    r#gen.emit("\tsub\tx20, x20, x22");
    r#gen.emit("\tcmp\tx20, #0");
    r#gen.emit("\tcsel\tx20, x20, xzr, ge");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, #64");
    address(r#gen, "x2", ".Lstone_float_f");
    r#gen.emit("\tmov\tw3, w20");
    r#gen.emit("\tfmov\td0, x19");
    r#gen.emit("\tbl\tsnprintf");
    r#gen.emit("\tcbnz\tx20, .Lformat_float_done");
    r#gen.emit("\tmov\tx0, x21");
    address(r#gen, "x1", ".Lstone_point_zero"); // 100 prints as 100.0
    r#gen.emit("\tbl\tstrcat");
    r#gen.emit(".Lformat_float_done:");
    pop_frame(r#gen, &saved);

    // the text goes in 64 bytes of stack above the frame record
    r#gen.emit("stone.print_float:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-80]!");
    r#gen.emit("\tmov\tx29, sp");
    r#gen.emit("\tadd\tx1, sp, #16");
    r#gen.emit("\tbl\tstone.format_float");
    r#gen.emit("\tadd\tx0, sp, #16");
    r#gen.emit("\tbl\tstone.print_str");
    r#gen.emit("\tldp\tx29, x30, [sp], #80");
    r#gen.emit("\tret");
}

/// Emits `stone.str_float`, which returns the float whose bits are in `x0` as a new string, the
/// way `stone.print_float` prints it. It needs [`float_runtime`] and the memory runtime.
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

    // x19 and x20 hold the bytes and their count across malloc
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
    r#gen.emit("\tbl\tmalloc");
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
    r#gen.emit("\tbl\trealloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit(".Llist_append_store:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tstr\tx20, [x10, x9, lsl #3]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstr\tx9, [x19]");
    r#gen.emit("\tmov\tx0, #0"); // append returns none
    pop_frame(r#gen, &saved);

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
fn jump_if_space(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tcmp\t{reg}, #32"));
    r#gen.emit(&format!("\tb.eq\t{label}"));
    // tab, newline, vertical tab, form feed, and carriage return are 9 through 13
    r#gen.emit(&format!("\tsub\tw16, {reg}, #9"));
    r#gen.emit("\tcmp\tw16, #4");
    r#gen.emit(&format!("\tb.ls\t{label}"));
}

/// Emits the input routines, which read stdin through libc's buffered `stdin`. Printing writes
/// with system calls, so a prompt shows before the program waits.
///
/// - `stone.input` prints the string in `x0` unless it is null, then returns the next line of
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

    // getline fills the line pointer at [sp, #16] and its capacity at [sp, #24], and the new
    // string waits at [sp, #32] while the line is freed
    r#gen.emit("stone.input:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-48]!");
    r#gen.emit("\tmov\tx29, sp");
    r#gen.emit("\tcbz\tx0, .Linput_read");
    r#gen.emit("\tbl\tstone.print_str");
    r#gen.emit(".Linput_read:");
    // a null line asks getline for a new buffer, so each line is a string of its own
    r#gen.emit("\tstp\txzr, xzr, [sp, #16]");
    r#gen.emit("\tadd\tx0, sp, #16");
    r#gen.emit("\tadd\tx1, sp, #24");
    load_stdin(r#gen, "x2");
    r#gen.emit("\tbl\tgetline");
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tb.le\t.Linput_end");
    r#gen.emit("\tldr\tx9, [sp, #16]");
    r#gen.emit("\tadd\tx10, x9, x0");
    r#gen.emit("\tldurb\tw10, [x10, #-1]");
    r#gen.emit("\tcmp\tw10, #10"); // newline
    r#gen.emit("\tb.ne\t.Linput_copy");
    r#gen.emit("\tsub\tx0, x0, #1");
    // the line becomes a counted string, and getline's buffer is freed
    r#gen.emit(".Linput_copy:");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x9");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tstr\tx0, [sp, #32]");
    r#gen.emit("\tldr\tx0, [sp, #16]");
    r#gen.emit("\tbl\tfree");
    r#gen.emit("\tldr\tx0, [sp, #32]");
    r#gen.emit("\tldp\tx29, x30, [sp], #48");
    r#gen.emit("\tret");
    // getline can allocate even when nothing is left to read
    r#gen.emit(".Linput_end:");
    r#gen.emit("\tldr\tx0, [sp, #16]");
    r#gen.emit("\tbl\tfree");
    address(r#gen, "x0", ".Lstone_empty");
    r#gen.emit("\tldp\tx29, x30, [sp], #48");
    r#gen.emit("\tret");

    r#gen.emit("stone.eof:");
    push_frame(r#gen, &[]);
    load_stdin(r#gen, "x0");
    r#gen.emit("\tbl\tgetc");
    r#gen.emit("\tcmn\tw0, #1"); // EOF
    r#gen.emit("\tb.eq\t.Leof_yes");
    load_stdin(r#gen, "x1");
    r#gen.emit("\tbl\tungetc");
    r#gen.emit("\tmov\tx0, #0");
    pop_frame(r#gen, &[]);
    r#gen.emit(".Leof_yes:");
    r#gen.emit("\tmov\tx0, #1");
    pop_frame(r#gen, &[]);
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
/// `stdlib::parse_float`, and stop the program through `stone.fail` with the same messages. They
/// need the string runtime.
///
/// - `stone.parse_int` returns the int in the string in `x0`, using libc's `strtoll`, which skips
///   the same whitespace and reports overflow through `errno`
/// - `stone.parse_float` returns the bits of the float in the string in `x0`, checking the
///   grammar itself, since `strtod` also takes hex floats and `nan(...)`
/// - `stone.fail_quoted` stops the program with the message `x0` followed by the string `x1`
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
    push_frame(r#gen, &[]);
    r#gen.emit("\tbl\tstone.str_concat");
    address(r#gen, "x1", ".Lstone_quote");
    r#gen.emit("\tbl\tstone.str_concat");
    r#gen.emit("\tb\tstone.fail");

    // x19 holds the text, x20 errno's address, x21 the result, and [sp] where it ended
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.parse_int:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tsub\tsp, sp, #16");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tbl\t__errno_location");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tstr\twzr, [x20]");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, sp");
    r#gen.emit("\tmov\tx2, #10");
    r#gen.emit("\tbl\tstrtoll");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tldr\tx9, [sp]");
    r#gen.emit("\tcmp\tx9, x19"); // no digits
    r#gen.emit("\tb.eq\t.Lparse_int_invalid");
    // only whitespace may follow, which is checked before overflow, as in stdlib::parse_int
    r#gen.emit(".Lparse_int_trailing:");
    r#gen.emit("\tldrb\tw10, [x9], #1");
    r#gen.emit("\tcbz\tw10, .Lparse_int_end");
    jump_if_space(r#gen, "w10", ".Lparse_int_trailing");
    r#gen.emit(".Lparse_int_invalid:");
    address(r#gen, "x0", ".Lstone_int_invalid");
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit("\tb\tstone.fail_quoted");
    r#gen.emit(".Lparse_int_end:");
    r#gen.emit("\tldr\tw9, [x20]");
    r#gen.emit("\tcmp\tw9, #34"); // ERANGE
    r#gen.emit("\tb.ne\t.Lparse_int_done");
    address(r#gen, "x0", ".Lstone_int_range");
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit("\tb\tstone.fail_quoted");
    r#gen.emit(".Lparse_int_done:");
    r#gen.emit("\tmov\tx0, x21");
    pop_frame(r#gen, &saved);

    // x19 holds the text, x20 the cursor, and x21 a count of digits
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
        let next = format!("{name}_next");
        r#gen.emit("\tmov\tx0, x20");
        address(r#gen, "x1", name);
        r#gen.emit(&format!("\tmov\tx2, #{length}"));
        r#gen.emit("\tbl\tstrncasecmp");
        r#gen.emit(&format!("\tcbnz\tw0, {next}"));
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
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tbl\tstrtod");
    r#gen.emit("\tfmov\tx0, d0");
    pop_frame(r#gen, &saved);
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
    // text and its size across malloc
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
/// and `stdlib::split`. They need the string and list runtimes, and jump to `empty_separator`, a
/// failure label, when `split` is given an empty separator.
///
/// - `stone.str_strip` returns the string in `x0` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `x0` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `x0` between each `x1`
pub fn string_methods(r#gen: &mut dyn AssemblyGenerator, empty_separator: &str) {
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

    // x19 holds where the piece starts, x20 the list, x21 the separator, x22 its length, and x23
    // where it was found
    let saved = ["x19", "x20", "x21", "x22", "x23"];
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
    r#gen.emit("\tbl\tstrstr");
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
