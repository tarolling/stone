//! The arm64 runtime of the `fs` module, which mirrors `x64/fs.rs` routine for routine, under the
//! same labels and contracts, with arguments in `x0` and up and results in `x0`.
//!
//! Every call into the system goes through [`system_call`], so on macOS each is a `libSystem`
//! function with Linux's contract: only `x0` changes, and an error comes back as a negative error
//! number. Linux reads a directory with `getdents64`, and macOS with `opendir` and `readdir`.

use super::builtins::{Sys, system_call};
use super::{address, pop_frame, push_frame};
use crate::codegen::context::{
    DIR_BUFFER, FILE_BUFFER, INVALID_TEXT_CODE, NULL_IN_PATH_CODE, OpenFlags, immortal_string,
};
use crate::codegen::{AssemblyGenerator, Os};
use crate::stdlib::fs::{INVALID_TEXT, NULL_IN_PATH, errors};

/// `openat`'s directory for a path relative to the working directory, which macOS's shims drop.
const AT_FDCWD: i64 = -100;

/// The error number of a system call a signal interrupted, which is made again. It is 4 on both
/// systems.
const EINTR: i64 = 4;

/// The registers every routine with a path keeps its values in: `x19` holds the path.
const SAVED: [&str; 8] = ["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26"];

/// Emits the routines of the `fs` module that `used` lists, as `x64::fs::fs_runtime` describes
/// them.
pub fn fs_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    let flags = OpenFlags::of(r#gen.target());
    let fails = used.iter().any(|label| *label != "stone.fs_exists");

    if fails {
        errors_and_results(r#gen);
    }
    path_check(r#gen);
    if used.iter().any(|label| {
        matches!(
            *label,
            "stone.fs_read" | "stone.fs_read_lines" | "stone.fs_write" | "stone.fs_append"
        )
    }) || (uses("stone.fs_list_dir") && r#gen.target().os == Os::Linux)
    {
        open(r#gen);
    }
    if uses("stone.fs_read") || uses("stone.fs_read_lines") || uses("stone.fs_list_dir") {
        utf8_check(r#gen);
    }
    if uses("stone.fs_read") || uses("stone.fs_read_lines") {
        read_all(r#gen, flags.read);
    }
    if uses("stone.fs_read") {
        read(r#gen);
    }
    if uses("stone.fs_read_lines") {
        read_lines(r#gen);
    }
    if uses("stone.fs_write") || uses("stone.fs_append") {
        write(
            r#gen,
            flags,
            uses("stone.fs_write"),
            uses("stone.fs_append"),
        );
    }
    if uses("stone.fs_list_dir") {
        list_dir(r#gen, flags.directory);
    }
    if uses("stone.fs_exists") {
        // faccessat(AT_FDCWD, path, F_OK), and a path with a null character names nothing
        r#gen.emit("stone.fs_exists:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\t.Lfs_check_path");
        r#gen.emit("\tcbnz\tx9, .Lfs_exists_no");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit(&format!("\tmov\tx0, #{AT_FDCWD}"));
        r#gen.emit("\tmov\tx2, #0");
        system_call(r#gen, Sys::Faccessat);
        r#gen.emit("\tcbnz\tx0, .Lfs_exists_no");
        r#gen.emit("\tmov\tx0, #1");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Lfs_exists_no:");
        r#gen.emit("\tmov\tx0, #0");
        pop_frame(r#gen, &[]);
    }
    for (label, call, mode) in [
        ("stone.fs_remove", Sys::Unlinkat, 0),    // no flags
        ("stone.fs_make_dir", Sys::Mkdirat, 511), // mode 0o777
    ] {
        if uses(label) {
            path_call(r#gen, label, call, mode);
        }
    }
}

/// Emits code that puts `value`, which must be below 2^32, in `reg`, in two halves if it needs
/// them, since few such values fit one `mov`.
///
/// For example, `load(gen, "x1", 0x80241)` emits `mov x1, #577` and `movk x1, #8, lsl #16`.
fn load(r#gen: &mut dyn AssemblyGenerator, reg: &str, value: i64) {
    r#gen.emit(&format!("\tmov\t{reg}, #{}", value & 0xffff));
    if value >> 16 != 0 {
        r#gen.emit(&format!("\tmovk\t{reg}, #{}, lsl #16", value >> 16));
    }
}

/// Emits the pieces every failing routine shares, as `x64::fs` describes them, with the error in
/// `x0` and the path in `x1` for `.Lfs_err`, the value and its kind in `x0` and `x1` for
/// `.Lfs_ok`, and the two strings in `x0` and `x1` for `.Lfs_join`.
fn errors_and_results(r#gen: &mut dyn AssemblyGenerator) {
    let os = r#gen.target().os;
    let reasons: Vec<(i64, &str)> = errors(os)
        .chain([
            (INVALID_TEXT_CODE, INVALID_TEXT),
            (NULL_IN_PATH_CODE, NULL_IN_PATH),
        ])
        .collect();
    r#gen.emit("\t.data");
    for (number, reason) in &reasons {
        immortal_string(r#gen, &reason_label(*number), reason);
    }
    immortal_string(r#gen, ".Lfs_os_error", "os error ");
    immortal_string(r#gen, ".Lfs_open_quote", ": '");
    immortal_string(r#gen, ".Lfs_close_quote", "'");
    r#gen.emit("\t.text");

    // x19 holds a string to release, x20 the path, and x21 the message so far
    let saved = ["x19", "x20", "x21"];
    r#gen.emit(".Lfs_err:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx20, x1");
    for (number, _) in &reasons {
        let next = format!(".Lfs_err_not{}", number.to_string().replace('-', "_"));
        if *number < 0 {
            r#gen.emit(&format!("\tcmn\tx0, #{}", -number));
        } else {
            r#gen.emit(&format!("\tcmp\tx0, #{number}"));
        }
        r#gen.emit(&format!("\tb.ne\t{next}"));
        address(r#gen, "x0", &reason_label(*number));
        r#gen.emit("\tb\t.Lfs_err_reason");
        r#gen.emit(&format!("{next}:"));
    }
    // an error number with no reason is written as one
    r#gen.emit("\tbl\tstone.str_int");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx1, x0");
    address(r#gen, "x0", ".Lfs_os_error");
    r#gen.emit("\tbl\tstone.str_concat");
    r#gen.emit("\tmov\tx21, x0");
    release(r#gen, "x19", ".Lfs_err_number");
    r#gen.emit("\tmov\tx0, x21");
    // the reason is owned, or immortal, which releasing never frees
    r#gen.emit(".Lfs_err_reason:");
    address(r#gen, "x1", ".Lfs_open_quote");
    r#gen.emit("\tbl\t.Lfs_join");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit("\tbl\t.Lfs_join");
    address(r#gen, "x1", ".Lfs_close_quote");
    r#gen.emit("\tbl\t.Lfs_join");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tmov\tx0, #1"); // err
    r#gen.emit("\tmov\tx1, #1"); // a string slot
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit("\tstr\tx21, [x0, #16]");
    pop_frame(r#gen, &saved);

    r#gen.emit(".Lfs_ok:");
    push_frame(r#gen, &["x19"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx0, #0"); // ok
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit("\tstr\tx19, [x0, #16]");
    pop_frame(r#gen, &["x19"]);

    // x19 holds the first string and x20 the joined one
    r#gen.emit(".Lfs_join:");
    push_frame(r#gen, &["x19", "x20"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tbl\tstone.str_concat");
    r#gen.emit("\tmov\tx20, x0");
    release(r#gen, "x19", ".Lfs_join_kept");
    r#gen.emit("\tmov\tx0, x20");
    pop_frame(r#gen, &["x19", "x20"]);
}

/// Returns the label of the immortal string holding the reason for the error `number`, as
/// `x64::fs` names it.
fn reason_label(number: i64) -> String {
    format!(".Lfs_reason_{}", number.to_string().replace('-', "_"))
}

/// Emits a release of the counted object in `reg`, freeing it with `free` (`stone.free_str` or
/// `stone.free_list`) if that was its last reference, then `done`, a label after the release.
fn release_with(r#gen: &mut dyn AssemblyGenerator, reg: &str, free: &str, done: &str) {
    r#gen.emit(&format!("\tldur\tx16, [{reg}, #-8]"));
    r#gen.emit("\tsubs\tx16, x16, #1");
    r#gen.emit(&format!("\tstur\tx16, [{reg}, #-8]"));
    r#gen.emit(&format!("\tb.ne\t{done}"));
    r#gen.emit(&format!("\tmov\tx9, {reg}"));
    r#gen.emit(&format!("\tbl\t{free}"));
    r#gen.emit(&format!("{done}:"));
}

/// Emits a release of the string in `reg`, as [`release_with`] does.
fn release(r#gen: &mut dyn AssemblyGenerator, reg: &str, done: &str) {
    release_with(r#gen, reg, "stone.free_str", done);
}

/// Emits `.Lfs_check_path`, which leaves 0 in `x9` if the string `x0` holds no null character,
/// so it can name a file, else nonzero, changing only `x9` through `x11`.
fn path_check(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfs_check_path:");
    r#gen.emit("\tmov\tx11, x0");
    r#gen.emit("\tstr\tx30, [sp, #-16]!");
    r#gen.emit("\tbl\tstone.cstr_len");
    r#gen.emit("\tldur\tx9, [x11, #-16]");
    r#gen.emit("\tsub\tx9, x0, x9");
    r#gen.emit("\tmov\tx0, x11");
    r#gen.emit("\tldr\tx30, [sp], #16");
    r#gen.emit("\tret");
}

/// Emits `.Lfs_utf8`, which leaves 1 in `x9` if the `x1` bytes at `x0` are UTF-8 text with no
/// null byte, else 0, as `x64::fs`'s checks, changing only `x9` through `x14`.
fn utf8_check(r#gen: &mut dyn AssemblyGenerator) {
    // x10 is the position, x11 the byte, x12 the continuation bytes left, and x13 and x14 the
    // bounds of the next
    r#gen.emit(".Lfs_utf8:");
    r#gen.emit("\tmov\tx10, #0");
    r#gen.emit(".Lfs_utf8_next:");
    r#gen.emit("\tcmp\tx10, x1");
    r#gen.emit("\tb.hs\t.Lfs_utf8_valid");
    r#gen.emit("\tldrb\tw11, [x0, x10]");
    r#gen.emit("\tadd\tx10, x10, #1");
    r#gen.emit("\tcbz\tx11, .Lfs_utf8_invalid");
    r#gen.emit("\tcmp\tx11, #128");
    r#gen.emit("\tb.lo\t.Lfs_utf8_next");
    r#gen.emit("\tmov\tx13, #128");
    r#gen.emit("\tmov\tx14, #191");
    r#gen.emit("\tcmp\tx11, #194");
    r#gen.emit("\tb.lo\t.Lfs_utf8_invalid");
    r#gen.emit("\tmov\tx12, #1");
    r#gen.emit("\tcmp\tx11, #224");
    r#gen.emit("\tb.lo\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tx12, #2");
    r#gen.emit("\tcmp\tx11, #240");
    r#gen.emit("\tb.lo\t.Lfs_utf8_three");
    r#gen.emit("\tmov\tx12, #3");
    r#gen.emit("\tcmp\tx11, #245");
    r#gen.emit("\tb.hs\t.Lfs_utf8_invalid");
    r#gen.emit("\tcmp\tx11, #240");
    r#gen.emit("\tb.ne\t.Lfs_utf8_not_f0");
    r#gen.emit("\tmov\tx13, #144");
    r#gen.emit(".Lfs_utf8_not_f0:");
    r#gen.emit("\tcmp\tx11, #244");
    r#gen.emit("\tb.ne\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tx14, #143");
    r#gen.emit("\tb\t.Lfs_utf8_rest");
    r#gen.emit(".Lfs_utf8_three:");
    r#gen.emit("\tcmp\tx11, #224");
    r#gen.emit("\tb.ne\t.Lfs_utf8_not_e0");
    r#gen.emit("\tmov\tx13, #160");
    r#gen.emit(".Lfs_utf8_not_e0:");
    r#gen.emit("\tcmp\tx11, #237");
    r#gen.emit("\tb.ne\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tx14, #159");
    r#gen.emit(".Lfs_utf8_rest:");
    r#gen.emit("\tcmp\tx10, x1");
    r#gen.emit("\tb.hs\t.Lfs_utf8_invalid");
    r#gen.emit("\tldrb\tw11, [x0, x10]");
    r#gen.emit("\tadd\tx10, x10, #1");
    r#gen.emit("\tcmp\tx11, x13");
    r#gen.emit("\tb.lo\t.Lfs_utf8_invalid");
    r#gen.emit("\tcmp\tx11, x14");
    r#gen.emit("\tb.hi\t.Lfs_utf8_invalid");
    r#gen.emit("\tmov\tx13, #128");
    r#gen.emit("\tmov\tx14, #191");
    r#gen.emit("\tsubs\tx12, x12, #1");
    r#gen.emit("\tb.ne\t.Lfs_utf8_rest");
    r#gen.emit("\tb\t.Lfs_utf8_next");
    r#gen.emit(".Lfs_utf8_valid:");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tret");
    r#gen.emit(".Lfs_utf8_invalid:");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit("\tret");
}

/// Emits `.Lfs_open`, which opens the path string `x0` with the flags `x1` and the mode `x2`,
/// made again if a signal interrupts it, returning the descriptor or a negative error number.
fn open(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfs_open:");
    r#gen.emit("\tmov\tx3, x2");
    r#gen.emit("\tmov\tx2, x1");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit(".Lfs_open_again:");
    r#gen.emit(&format!("\tmov\tx0, #{AT_FDCWD}"));
    system_call(r#gen, Sys::Openat);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit("\tb.eq\t.Lfs_open_again");
    r#gen.emit("\tret");
}

/// Emits a `close` of the descriptor in `reg`, whose result is ignored.
fn close(r#gen: &mut dyn AssemblyGenerator, reg: &str) {
    r#gen.emit(&format!("\tmov\tx0, {reg}"));
    system_call(r#gen, Sys::Close);
}

/// Emits `.Lfs_read_all`, which reads the whole file named by the string `x0`, returning a buffer
/// from `stone.mem_alloc` in `x0` and the bytes it holds in `x1`, or a negative error number in
/// `x0`, having freed the buffer.
fn read_all(r#gen: &mut dyn AssemblyGenerator, flags: i64) {
    // x19 holds the descriptor, x20 the buffer, x21 the bytes read, and x22 the buffer's size
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit(".Lfs_read_all:");
    push_frame(r#gen, &saved);
    load(r#gen, "x1", flags);
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit("\tbl\t.Lfs_open");
    r#gen.emit("\ttbnz\tx0, #63, .Lfs_read_all_done");
    r#gen.emit("\tmov\tx19, x0");
    load(r#gen, "x22", FILE_BUFFER);
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, #0");
    r#gen.emit(".Lfs_read_all_more:");
    r#gen.emit("\tcmp\tx21, x22");
    r#gen.emit("\tb.ne\t.Lfs_read_all_room");
    // the buffer is full, so it doubles
    r#gen.emit("\tadd\tx22, x22, x22");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, x22");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit(".Lfs_read_all_room:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tadd\tx1, x20, x21");
    r#gen.emit("\tsub\tx2, x22, x21");
    system_call(r#gen, Sys::Read);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit("\tb.eq\t.Lfs_read_all_room");
    r#gen.emit("\ttbnz\tx0, #63, .Lfs_read_all_failed");
    r#gen.emit("\tcbz\tx0, .Lfs_read_all_end");
    r#gen.emit("\tadd\tx21, x21, x0");
    r#gen.emit("\tb\t.Lfs_read_all_more");
    r#gen.emit(".Lfs_read_all_failed:");
    // the error survives the close in x21, and the buffer is freed
    r#gen.emit("\tmov\tx21, x0");
    close(r#gen, "x19");
    r#gen.emit("\tmov\tx9, x20");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tb\t.Lfs_read_all_done");
    r#gen.emit(".Lfs_read_all_end:");
    close(r#gen, "x19");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit(".Lfs_read_all_done:");
    pop_frame(r#gen, &saved);
}

/// Emits the start of a routine that keeps the path string `x0` in `x19` and saves `x20` through
/// `x26` for its own use, then fails with [`NULL_IN_PATH_CODE`] if the path holds a null
/// character. [`routine_end`] emits `<label>_err`, which returns the error for the code in `x0`,
/// and `<label>_done`, which returns `x0`.
fn routine_start(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}:"));
    push_frame(r#gen, &SAVED);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tbl\t.Lfs_check_path");
    r#gen.emit(&format!("\tmov\tx0, #{NULL_IN_PATH_CODE}"));
    r#gen.emit(&format!("\tcbnz\tx9, {label}_err"));
}

/// Emits the end of a routine [`routine_start`] began.
fn routine_end(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_err:"));
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit("\tbl\t.Lfs_err");
    r#gen.emit(&format!("{label}_done:"));
    pop_frame(r#gen, &SAVED);
}

/// Emits a jump to `<label>_err` for the negative error number in `x0`.
fn fail_with_errno(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tneg\tx0, x0");
    r#gen.emit(&format!("\tb\t{label}_err"));
}

/// Emits `stone.fs_read`, as `x64::fs` does.
fn read(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.fs_read";
    // x20 holds the buffer and x21 its length, then x22 the string
    routine_start(r#gen, label);
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\t.Lfs_read_all");
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_read"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_read:"));
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tbl\t.Lfs_utf8");
    r#gen.emit("\tmov\tx22, x9");
    r#gen.emit(&format!("\tcbz\tx9, {label}_free"));
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx22, x0");
    r#gen.emit(&format!("{label}_free:"));
    r#gen.emit("\tmov\tx9, x20");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit(&format!("\tmov\tx0, #{INVALID_TEXT_CODE}"));
    r#gen.emit(&format!("\tcbz\tx22, {label}_err"));
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, #1"); // a string slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits `stone.fs_read_lines`, as `x64::fs` does.
fn read_lines(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.fs_read_lines";
    // x20 holds the buffer, x21 its end, x22 the list, x23 where the line starts, and x24 where
    // the next one does
    routine_start(r#gen, label);
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\t.Lfs_read_all");
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_read"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_read:"));
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tadd\tx21, x0, x1");
    r#gen.emit("\tbl\t.Lfs_utf8");
    r#gen.emit(&format!("\tcbnz\tx9, {label}_text"));
    r#gen.emit("\tmov\tx9, x20");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit(&format!("\tmov\tx0, #{INVALID_TEXT_CODE}"));
    r#gen.emit(&format!("\tb\t{label}_err"));
    r#gen.emit(&format!("{label}_text:"));
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx1, #1"); // strings
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx22, x0");
    r#gen.emit("\tmov\tx23, x20");
    r#gen.emit(&format!("{label}_line:"));
    r#gen.emit("\tcmp\tx23, x21");
    r#gen.emit(&format!("\tb.eq\t{label}_end"));
    // x1 finds the line's end, then x24 is past its \n, or the end of the text
    r#gen.emit("\tmov\tx1, x23");
    r#gen.emit(&format!("{label}_scan:"));
    r#gen.emit("\tcmp\tx1, x21");
    r#gen.emit(&format!("\tb.eq\t{label}_last"));
    r#gen.emit("\tldrb\tw9, [x1]");
    r#gen.emit("\tcmp\tw9, #10");
    r#gen.emit(&format!("\tb.eq\t{label}_newline"));
    r#gen.emit("\tadd\tx1, x1, #1");
    r#gen.emit(&format!("\tb\t{label}_scan"));
    r#gen.emit(&format!("{label}_newline:"));
    r#gen.emit("\tadd\tx24, x1, #1");
    r#gen.emit("\tcmp\tx1, x23");
    r#gen.emit(&format!("\tb.eq\t{label}_copy"));
    r#gen.emit("\tsub\tx10, x1, #1");
    r#gen.emit("\tldrb\tw9, [x10]");
    r#gen.emit("\tcmp\tw9, #13");
    r#gen.emit(&format!("\tb.ne\t{label}_copy"));
    r#gen.emit("\tmov\tx1, x10");
    r#gen.emit(&format!("\tb\t{label}_copy"));
    r#gen.emit(&format!("{label}_last:"));
    r#gen.emit("\tmov\tx24, x1");
    r#gen.emit(&format!("{label}_copy:"));
    r#gen.emit("\tsub\tx1, x1, x23");
    r#gen.emit("\tmov\tx0, x23");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.list_append");
    r#gen.emit("\tmov\tx23, x24");
    r#gen.emit(&format!("\tb\t{label}_line"));
    r#gen.emit(&format!("{label}_end:"));
    r#gen.emit("\tmov\tx9, x20");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, #2"); // a list slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits `stone.fs_write` and `stone.fs_append`, as `x64::fs` does.
fn write(r#gen: &mut dyn AssemblyGenerator, flags: OpenFlags, write: bool, append: bool) {
    let label = ".Lfs_write";
    if write {
        r#gen.emit("stone.fs_write:");
        load(r#gen, "x12", flags.write);
        r#gen.emit(&format!("\tb\t{label}"));
    }
    if append {
        r#gen.emit("stone.fs_append:");
        load(r#gen, "x12", flags.append);
    }
    // x12 holds the flags and x13 the text until the frame is made, then x20 the text, x21 the
    // descriptor, and x22 the bytes written
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tmov\tx13, x1");
    let path = format!("{label}_path");
    routine_start(r#gen, &path);
    r#gen.emit("\tmov\tx20, x13");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x12");
    r#gen.emit("\tmov\tx2, #438"); // mode 0o666, less the umask
    r#gen.emit("\tbl\t.Lfs_open");
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_opened"));
    fail_with_errno(r#gen, &path);
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tldur\tx2, [x20, #-16]");
    r#gen.emit("\tsub\tx2, x2, x22");
    r#gen.emit(&format!("\tcbz\tx2, {label}_written"));
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tadd\tx1, x20, x22");
    system_call(r#gen, Sys::Write);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_more"));
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit("\tadd\tx22, x22, x0");
    r#gen.emit(&format!("\tb\t{label}_more"));
    r#gen.emit(&format!("{label}_failed:"));
    r#gen.emit("\tmov\tx22, x0");
    close(r#gen, "x21");
    r#gen.emit("\tmov\tx0, x22");
    fail_with_errno(r#gen, &path);
    r#gen.emit(&format!("{label}_written:"));
    close(r#gen, "x21");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, #0"); // an int slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{path}_done"));
    routine_end(r#gen, &path);
}

/// Emits `stone.fs_list_dir`: on Linux the directory's entries from `getdents64` calls, as
/// `x64::fs` reads them, and on macOS from `readdir` calls on what `opendir` returns.
fn list_dir(r#gen: &mut dyn AssemblyGenerator, flags: i64) {
    let label = "stone.fs_list_dir";
    let linux = r#gen.target().os == Os::Linux;
    // x20 holds the descriptor (or the macOS DIR), x21 the buffer, x22 the list, x23 the bytes of
    // entries read, x24 where the next entry starts, and x25 the name
    routine_start(r#gen, label);
    r#gen.emit("\tmov\tx0, x19");
    if linux {
        load(r#gen, "x1", flags);
        r#gen.emit("\tmov\tx2, #0");
        r#gen.emit("\tbl\t.Lfs_open");
    } else {
        system_call(r#gen, Sys::Opendir);
    }
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_opened"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tx20, x0");
    if linux {
        load(r#gen, "x0", DIR_BUFFER);
        r#gen.emit("\tbl\tstone.mem_alloc");
        r#gen.emit("\tmov\tx21, x0");
    }
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx1, #1"); // strings
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx22, x0");
    if linux {
        r#gen.emit(&format!("{label}_fill:"));
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tmov\tx1, x21");
        load(r#gen, "x2", DIR_BUFFER);
        system_call(r#gen, Sys::Getdents64);
        r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
        r#gen.emit(&format!("\tb.eq\t{label}_fill"));
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
        r#gen.emit(&format!("\tcbz\tx0, {label}_end"));
        r#gen.emit("\tmov\tx23, x0");
        r#gen.emit("\tmov\tx24, #0");
        r#gen.emit(&format!("{label}_entry:"));
        r#gen.emit("\tcmp\tx24, x23");
        r#gen.emit(&format!("\tb.hs\t{label}_fill"));
        // a linux_dirent64 holds its length at 16 and its name from 19
        r#gen.emit("\tadd\tx9, x21, x24");
        r#gen.emit("\tldrh\tw10, [x9, #16]");
        r#gen.emit("\tadd\tx24, x24, x10");
        r#gen.emit("\tadd\tx0, x9, #19");
    } else {
        // readdir gives the next entry, null at the end, or a negative error number, and macOS's
        // struct dirent holds its name from 21
        r#gen.emit(&format!("{label}_entry:"));
        r#gen.emit("\tmov\tx0, x20");
        system_call(r#gen, Sys::Readdir);
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
        r#gen.emit(&format!("\tcbz\tx0, {label}_end"));
        r#gen.emit("\tadd\tx0, x0, #21");
    }
    dot_check(r#gen, &format!("{label}_entry"));
    r#gen.emit("\tmov\tx25, x0");
    r#gen.emit("\tbl\tstone.cstr_len");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x25");
    r#gen.emit("\tbl\t.Lfs_utf8");
    r#gen.emit(&format!("\tcbz\tx9, {label}_invalid"));
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.list_append");
    r#gen.emit(&format!("\tb\t{label}_entry"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\tx0, #{}", -INVALID_TEXT_CODE));
    r#gen.emit(&format!("{label}_failed:"));
    // the error survives the cleanup in x23
    r#gen.emit("\tmov\tx23, x0");
    close_dir(r#gen, linux);
    release_with(
        r#gen,
        "x22",
        "stone.free_list",
        &format!("{label}_released"),
    );
    r#gen.emit("\tmov\tx0, x23");
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_end:"));
    close_dir(r#gen, linux);
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.list_sort_str");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, #2"); // a list slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits the end of reading the directory `x20`: on Linux a `close` of its descriptor and a free
/// of the entries' buffer in `x21`, and on macOS a `closedir`.
fn close_dir(r#gen: &mut dyn AssemblyGenerator, linux: bool) {
    if linux {
        close(r#gen, "x20");
        r#gen.emit("\tmov\tx9, x21");
        r#gen.emit("\tbl\tstone.mem_free");
    } else {
        r#gen.emit("\tmov\tx0, x20");
        system_call(r#gen, Sys::Closedir);
    }
}

/// Emits a jump to `skip` if the null-terminated name at `x0` is `.` or `..`, which no listing
/// includes, using only `x9`.
fn dot_check(r#gen: &mut dyn AssemblyGenerator, skip: &str) {
    let named = format!("{skip}_named");
    r#gen.emit("\tldrb\tw9, [x0]");
    r#gen.emit("\tcmp\tw9, #46"); // '.'
    r#gen.emit(&format!("\tb.ne\t{named}"));
    r#gen.emit("\tldrb\tw9, [x0, #1]");
    r#gen.emit(&format!("\tcbz\tx9, {skip}"));
    r#gen.emit("\tcmp\tw9, #46");
    r#gen.emit(&format!("\tb.ne\t{named}"));
    r#gen.emit("\tldrb\tw9, [x0, #2]");
    r#gen.emit(&format!("\tcbz\tx9, {skip}"));
    r#gen.emit(&format!("{named}:"));
}

/// Emits `label`, a routine that makes `call` on the path string `x0` relative to the working
/// directory, with `third` as its third argument, and returns `ok` of the path or `err` of the
/// call's error.
fn path_call(r#gen: &mut dyn AssemblyGenerator, label: &str, call: Sys, third: i64) {
    routine_start(r#gen, label);
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit(&format!("\tmov\tx0, #{AT_FDCWD}"));
    r#gen.emit(&format!("\tmov\tx2, #{third}"));
    system_call(r#gen, call);
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_made"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_made:"));
    // the ok keeps a reference to the path of its own
    r#gen.emit("\tldur\tx16, [x19, #-8]");
    r#gen.emit("\tadd\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x19, #-8]");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, #1"); // a string slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    routine_end(r#gen, label);
}
