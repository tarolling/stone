//! The x86-64 runtime of the `fs` module, which reads and writes files and directories with
//! Linux system calls, as `stdlib::fs` does through Rust's `std::fs` for the interpreter.
//!
//! Each routine takes counted strings, which it borrows, and returns a new result, as
//! `stone.parse_int_result` does: `ok` (tag 0) of its value, or `err` (tag 1) of a message worded
//! from the same tables as the interpreter's (`stdlib::fs::errors`), such as
//! `no such file or directory: 'notes.txt'`.

use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{
    DIR_BUFFER, FILE_BUFFER, INVALID_ADDRESS_CODE, INVALID_TEXT_CODE, LISTENER_CLOSED_CODE,
    NULL_IN_COMMAND_CODE, NULL_IN_PATH_CODE, OpenFlags, SOCKET_CLOSED_CODE, TIMED_OUT_CODE,
    immortal_string,
};
use crate::stdlib::fs::{INVALID_TEXT, NULL_IN_PATH, errors};
use crate::stdlib::net::{INVALID_ADDRESS, LISTENER_CLOSED, SOCKET_CLOSED, TIMED_OUT};
use crate::stdlib::os::NULL_IN_COMMAND;

/// `openat`'s directory for a path relative to the working directory.
const AT_FDCWD: i64 = -100;

/// The error number of a system call a signal interrupted, which is made again.
pub(super) const EINTR: i64 = 4;

/// Emits the routines of the `fs` module that `used` lists, each following the function of
/// `stdlib::fs` with the same name:
///
/// - `stone.fs_read` returns a result of the text of the file named by `rdi`
/// - `stone.fs_read_lines` returns a result of a list of its lines, split as `stdlib::fs::lines`
///   splits them
/// - `stone.fs_write` and `stone.fs_append` write the string `rsi` to the file named by `rdi`,
///   over what it held or at its end, and return a result of the bytes written
/// - `stone.fs_list_dir` returns a result of the sorted names in the directory named by `rdi`
/// - `stone.fs_exists` returns 1 if anything is at the path `rdi`, else 0
/// - `stone.fs_remove` and `stone.fs_make_dir` delete the file or create the directory named by
///   `rdi` and return a result of that path
///
/// They follow the System V calling convention and need the string, list, and enum runtimes,
/// `stone.str_int` for an error number with no reason in the tables, and `stone.list_sort_str`
/// for `stone.fs_list_dir`. `used` may also hold `stone.os_run` and the `stone.net_` routines,
/// which share their pieces, so they are emitted once (see `process::run_runtime` and
/// `net::net_runtime`).
pub fn fs_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    let flags = OpenFlags::of(r#gen.target());
    let fails = used.iter().any(|label| *label != "stone.fs_exists");
    let net: Vec<&str> = used
        .iter()
        .copied()
        .filter(|label| label.starts_with("stone.net_"))
        .collect();

    if fails {
        errors_and_results(r#gen, !net.is_empty());
    }
    path_check(r#gen);
    // os.run and the net routines check text as fs.read does
    let checks_text = uses("stone.os_run") || !net.is_empty();
    if checks_text {
        utf8_check(r#gen);
    }
    if uses("stone.os_run") {
        super::process::run_runtime(r#gen);
    }
    if !net.is_empty() {
        super::net::net_runtime(r#gen, &net);
    }
    if used.iter().any(|label| {
        matches!(
            *label,
            "stone.fs_read"
                | "stone.fs_read_lines"
                | "stone.fs_write"
                | "stone.fs_append"
                | "stone.fs_list_dir"
        )
    }) {
        open(r#gen);
    }
    if uses("stone.fs_read") || uses("stone.fs_read_lines") {
        read_all(r#gen, flags.read);
        if !checks_text {
            utf8_check(r#gen);
        }
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
        if !uses("stone.fs_read") && !uses("stone.fs_read_lines") && !checks_text {
            utf8_check(r#gen);
        }
        list_dir(r#gen, flags.directory);
    }
    if uses("stone.fs_exists") {
        // faccessat(AT_FDCWD, path, F_OK), and a path with a null character names nothing
        r#gen.emit("stone.fs_exists:");
        r#gen.emit("\tcall\t.Lfs_check_path");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjnz\t.Lfs_exists_no");
        r#gen.emit("\tmov\trsi, rdi");
        r#gen.emit(&format!("\tmov\trdi, {AT_FDCWD}"));
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tmov\teax, 269"); // sys_faccessat
        r#gen.emit("\tsyscall");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit("\tjnz\t.Lfs_exists_no");
        r#gen.emit("\tmov\teax, 1");
        r#gen.emit("\tret");
        r#gen.emit(".Lfs_exists_no:");
        r#gen.emit("\txor\teax, eax");
        r#gen.emit("\tret");
    }
    for (label, number, extra) in [
        ("stone.fs_remove", 263, "\txor\tedx, edx"), // sys_unlinkat, no flags
        ("stone.fs_make_dir", 258, "\tmov\tedx, 511"), // sys_mkdirat, mode 0o777
    ] {
        if uses(label) {
            path_call(r#gen, label, number, extra);
        }
    }
}

/// Emits the pieces every failing routine shares, with the reasons of the `net` routines' own
/// errors if `net`:
///
/// - `.Lfs_err` returns a new `err` of the message for the error `rdi` about the path `rsi`, or
///   of the reason alone if `rsi` is 0: a positive error number, [`INVALID_TEXT_CODE`], or
///   another of the codes in `context`, such as [`NULL_IN_PATH_CODE`]
/// - `.Lfs_ok` returns a new `ok` of `rdi`, a slot counted as the kind `rsi` says
/// - `.Lfs_join` returns a new string holding `rdi` followed by `rsi`, releasing `rdi`
fn errors_and_results(r#gen: &mut dyn AssemblyGenerator, net: bool) {
    let os = r#gen.target().os;
    let mut reasons: Vec<(i64, &str)> = errors(os)
        .chain([
            (INVALID_TEXT_CODE, INVALID_TEXT),
            (NULL_IN_PATH_CODE, NULL_IN_PATH),
            (NULL_IN_COMMAND_CODE, NULL_IN_COMMAND),
        ])
        .collect();
    if net {
        reasons.extend([
            (INVALID_ADDRESS_CODE, INVALID_ADDRESS),
            (TIMED_OUT_CODE, TIMED_OUT),
            (SOCKET_CLOSED_CODE, SOCKET_CLOSED),
            (LISTENER_CLOSED_CODE, LISTENER_CLOSED),
        ]);
    }
    r#gen.emit("\t.data");
    for (number, reason) in &reasons {
        immortal_string(r#gen, &reason_label(*number), reason);
    }
    immortal_string(r#gen, ".Lfs_os_error", "os error ");
    immortal_string(r#gen, ".Lfs_open_quote", ": '");
    immortal_string(r#gen, ".Lfs_close_quote", "'");
    r#gen.emit("\t.text");

    // rbx holds a string to release and r12 the path
    r#gen.emit(".Lfs_err:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\tr12, rsi");
    for (number, _) in &reasons {
        let next = format!(".Lfs_err_not{}", number.to_string().replace('-', "_"));
        r#gen.emit(&format!("\tcmp\trdi, {number}"));
        r#gen.emit(&format!("\tjne\t{next}"));
        r#gen.emit(&format!("\tlea\trax, [rip + {}]", reason_label(*number)));
        r#gen.emit("\tjmp\t.Lfs_err_reason");
        r#gen.emit(&format!("{next}:"));
    }
    // an error number with no reason is written as one
    r#gen.emit("\tcall\tstone.str_int");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tlea\trdi, [rip + .Lfs_os_error]");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tcall\tstone.str_concat");
    release(r#gen, "rbx", ".Lfs_err_number");
    // the reason is owned, or immortal, which releasing never frees, and stands alone without a
    // path
    r#gen.emit(".Lfs_err_reason:");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Lfs_err_alone");
    for piece in [
        "lea\trsi, [rip + .Lfs_open_quote]",
        "mov\trsi, r12",
        "lea\trsi, [rip + .Lfs_close_quote]",
    ] {
        r#gen.emit("\tmov\trdi, rax");
        r#gen.emit(&format!("\t{piece}"));
        r#gen.emit("\tcall\t.Lfs_join");
    }
    r#gen.emit(".Lfs_err_alone:");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit("\tmov\tedi, 1"); // err
    r#gen.emit("\tmov\tesi, 1"); // a string slot
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tQWORD PTR [rax + 16], rbx");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");

    r#gen.emit(".Lfs_ok:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\txor\tedi, edi"); // ok
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tQWORD PTR [rax + 16], rbx");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");

    // rbx holds the first string and r12 the joined one
    r#gen.emit(".Lfs_join:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tcall\tstone.str_concat");
    r#gen.emit("\tmov\tr12, rax");
    release(r#gen, "rbx", ".Lfs_join_kept");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Returns the label of the immortal string holding the reason for the error `number`.
///
/// For example, error 2 is `.Lfs_reason_2` and [`INVALID_TEXT_CODE`] is `.Lfs_reason__1`.
fn reason_label(number: i64) -> String {
    format!(".Lfs_reason_{}", number.to_string().replace('-', "_"))
}

/// Emits a release of the string in `reg`, freeing it if that was its last reference, then
/// `done`, a label after the release.
fn release(r#gen: &mut dyn AssemblyGenerator, reg: &str, done: &str) {
    r#gen.emit(&format!("\tdec\tQWORD PTR [{reg} - 8]"));
    r#gen.emit(&format!("\tjnz\t{done}"));
    r#gen.emit(&format!("\tmov\trax, {reg}"));
    r#gen.emit("\tcall\tstone.free_str");
    r#gen.emit(&format!("{done}:"));
}

/// Emits `.Lfs_check_path`, which returns 0 in `rax` if the string `rdi` holds no null
/// character, so it can name a file, else nonzero, changing nothing else.
fn path_check(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfs_check_path:");
    r#gen.emit("\tcall\tstone.cstr_len");
    r#gen.emit("\tsub\trax, QWORD PTR [rdi - 16]");
    r#gen.emit("\tret");
}

/// Emits `.Lfs_utf8`, which returns 1 in `rax` if the `rsi` bytes at `rdi` are UTF-8 text with no
/// null byte, as `stdlib::fs::is_text` checks, else 0, changing only `rax`, `rcx`, `rdx`, `r8`,
/// and `r9`.
///
/// Each character's first byte says how many continuation bytes follow, which must each be
/// `0x80` through `0xbf`, except that the first is narrower after `e0` (no overlong forms), `ed`
/// (no surrogates), `f0` (no overlong forms), and `f4` (nothing past U+10FFFF), as the Unicode
/// Standard's table 3-7 lists.
fn utf8_check(r#gen: &mut dyn AssemblyGenerator) {
    // rcx is the position, rdx the continuation bytes left, and r8 and r9 the bounds of the next
    r#gen.emit(".Lfs_utf8:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(".Lfs_utf8_next:");
    r#gen.emit("\tcmp\trcx, rsi");
    r#gen.emit("\tjae\t.Lfs_utf8_valid");
    r#gen.emit("\tmovzx\teax, byte ptr [rdi + rcx]");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lfs_utf8_invalid");
    r#gen.emit("\tcmp\trax, 128");
    r#gen.emit("\tjb\t.Lfs_utf8_next");
    r#gen.emit("\tmov\tr8, 128");
    r#gen.emit("\tmov\tr9, 191");
    r#gen.emit("\tcmp\trax, 194");
    r#gen.emit("\tjb\t.Lfs_utf8_invalid");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tcmp\trax, 224");
    r#gen.emit("\tjb\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tedx, 2");
    r#gen.emit("\tcmp\trax, 240");
    r#gen.emit("\tjb\t.Lfs_utf8_three");
    r#gen.emit("\tmov\tedx, 3");
    r#gen.emit("\tcmp\trax, 245");
    r#gen.emit("\tjae\t.Lfs_utf8_invalid");
    r#gen.emit("\tcmp\trax, 240");
    r#gen.emit("\tjne\t.Lfs_utf8_not_f0");
    r#gen.emit("\tmov\tr8, 144");
    r#gen.emit(".Lfs_utf8_not_f0:");
    r#gen.emit("\tcmp\trax, 244");
    r#gen.emit("\tjne\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tr9, 143");
    r#gen.emit("\tjmp\t.Lfs_utf8_rest");
    r#gen.emit(".Lfs_utf8_three:");
    r#gen.emit("\tcmp\trax, 224");
    r#gen.emit("\tjne\t.Lfs_utf8_not_e0");
    r#gen.emit("\tmov\tr8, 160");
    r#gen.emit(".Lfs_utf8_not_e0:");
    r#gen.emit("\tcmp\trax, 237");
    r#gen.emit("\tjne\t.Lfs_utf8_rest");
    r#gen.emit("\tmov\tr9, 159");
    r#gen.emit(".Lfs_utf8_rest:");
    r#gen.emit("\tcmp\trcx, rsi");
    r#gen.emit("\tjae\t.Lfs_utf8_invalid");
    r#gen.emit("\tmovzx\teax, byte ptr [rdi + rcx]");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\tcmp\trax, r8");
    r#gen.emit("\tjb\t.Lfs_utf8_invalid");
    r#gen.emit("\tcmp\trax, r9");
    r#gen.emit("\tja\t.Lfs_utf8_invalid");
    r#gen.emit("\tmov\tr8, 128");
    r#gen.emit("\tmov\tr9, 191");
    r#gen.emit("\tdec\trdx");
    r#gen.emit("\tjnz\t.Lfs_utf8_rest");
    r#gen.emit("\tjmp\t.Lfs_utf8_next");
    r#gen.emit(".Lfs_utf8_valid:");
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tret");
    r#gen.emit(".Lfs_utf8_invalid:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");
}

/// Emits `.Lfs_open`, which opens the path string `rdi` with the flags `rsi` and the mode `rdx`
/// through `openat`, made again if a signal interrupts it, returning the descriptor or a
/// negative error number in `rax`.
fn open(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfs_open:");
    r#gen.emit("\tmov\tr10, rdx");
    r#gen.emit("\tmov\trdx, rsi");
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit(".Lfs_open_again:");
    r#gen.emit(&format!("\tmov\trdi, {AT_FDCWD}"));
    r#gen.emit("\tmov\teax, 257"); // sys_openat
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit("\tje\t.Lfs_open_again");
    r#gen.emit("\tret");
}

/// Emits `.Lfs_read_all`, which reads the whole file named by the string `rdi`, returning a
/// buffer from `stone.mem_alloc` in `rax` and the bytes it holds in `rdx`, or a negative error
/// number in `rax`, having freed the buffer.
fn read_all(r#gen: &mut dyn AssemblyGenerator, flags: i64) {
    // rbx holds the descriptor, r12 the buffer, r13 the bytes read, and r14 the buffer's size
    r#gen.emit(".Lfs_read_all:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tpush\tr14");
    r#gen.emit(&format!("\tmov\trsi, {flags}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcall\t.Lfs_open");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjs\t.Lfs_read_all_done");
    r#gen.emit("\tmov\trbx, rax");
    r#gen.emit(&format!("\tmov\tr14, {FILE_BUFFER}"));
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\txor\tr13, r13");
    r#gen.emit(".Lfs_read_all_more:");
    r#gen.emit("\tcmp\tr13, r14");
    r#gen.emit("\tjne\t.Lfs_read_all_room");
    // the buffer is full, so it doubles
    r#gen.emit("\tadd\tr14, r14");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit(".Lfs_read_all_room:");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tlea\trsi, [r12 + r13]");
    r#gen.emit("\tmov\trdx, r14");
    r#gen.emit("\tsub\trdx, r13");
    r#gen.emit("\txor\teax, eax"); // sys_read
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit("\tje\t.Lfs_read_all_room");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjs\t.Lfs_read_all_failed");
    r#gen.emit("\tjz\t.Lfs_read_all_end");
    r#gen.emit("\tadd\tr13, rax");
    r#gen.emit("\tjmp\t.Lfs_read_all_more");
    r#gen.emit(".Lfs_read_all_failed:");
    // the error survives the close in r13, and the buffer is freed
    r#gen.emit("\tmov\tr13, rax");
    close(r#gen, "rbx");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tjmp\t.Lfs_read_all_done");
    r#gen.emit(".Lfs_read_all_end:");
    close(r#gen, "rbx");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit(".Lfs_read_all_done:");
    r#gen.emit("\tpop\tr14");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits a `close` of the descriptor in `reg`, whose result is ignored, as Rust ignores it when
/// a file is dropped.
pub(super) fn close(r#gen: &mut dyn AssemblyGenerator, reg: &str) {
    r#gen.emit(&format!("\tmov\trdi, {reg}"));
    r#gen.emit("\tmov\teax, 3"); // sys_close
    r#gen.emit("\tsyscall");
}

/// Emits the start of a routine that keeps the path string `rdi` in `rbx` and saves `r12` through
/// `r15` for its own use, then fails with `null_code`, such as [`NULL_IN_PATH_CODE`], if the
/// path holds a null character. [`routine_end`] emits the matching end, `<label>_done`, and
/// `<label>_err`, which returns the error for the code in `rdi`.
pub(super) fn routine_start(r#gen: &mut dyn AssemblyGenerator, label: &str, null_code: i64) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tsub\trsp, 8");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tcall\t.Lfs_check_path");
    r#gen.emit(&format!("\tmov\trdi, {null_code}"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjnz\t{label}_err"));
}

/// Emits the end of a routine [`routine_start`] began: `<label>_err`, which makes an `err` for the
/// error code in `rdi` about the path, and `<label>_done`, which returns `rax`.
pub(super) fn routine_end(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_err:"));
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tcall\t.Lfs_err");
    r#gen.emit(&format!("{label}_done:"));
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits a jump to `<label>_err` for the negative error number in `rax`.
fn fail_with_errno(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
}

/// Emits `stone.fs_read`: the file's bytes from `.Lfs_read_all`, checked to be text, copied into
/// a new string.
fn read(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.fs_read";
    // r12 holds the buffer and r13 its length, then r14 the string
    routine_start(r#gen, label, NULL_IN_PATH_CODE);
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\t.Lfs_read_all");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_read"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_read:"));
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tcall\t.Lfs_utf8");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_free"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit(&format!("{label}_free:"));
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(&format!("\tmov\trdi, {INVALID_TEXT_CODE}"));
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit(&format!("\tjz\t{label}_err"));
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\tesi, 1"); // a string slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits `stone.fs_read_lines`: the file's bytes from `.Lfs_read_all`, checked to be text, split
/// at each `\n` into a new list of strings, dropping a `\r` before it and making no empty line
/// after the last one.
fn read_lines(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.fs_read_lines";
    // r12 holds the buffer, r13 its end, r14 the list, r15 where the line starts, and [rbp - 48]
    // the next line's start, found before the line is copied
    routine_start(r#gen, label, NULL_IN_PATH_CODE);
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\t.Lfs_read_all");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_read"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_read:"));
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tlea\tr13, [rax + rdx]");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, rdx");
    r#gen.emit("\tcall\t.Lfs_utf8");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjnz\t{label}_text"));
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(&format!("\tmov\trdi, {INVALID_TEXT_CODE}"));
    r#gen.emit(&format!("\tjmp\t{label}_err"));
    r#gen.emit(&format!("{label}_text:"));
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tmov\tesi, 1"); // strings
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\tr15, r12");
    r#gen.emit(&format!("{label}_line:"));
    r#gen.emit("\tcmp\tr15, r13");
    r#gen.emit(&format!("\tje\t{label}_end"));
    // rsi finds the line's end, then rdx is past its \n, or the end of the text
    r#gen.emit("\tmov\trsi, r15");
    r#gen.emit(&format!("{label}_scan:"));
    r#gen.emit("\tcmp\trsi, r13");
    r#gen.emit(&format!("\tje\t{label}_last"));
    r#gen.emit("\tcmp\tbyte ptr [rsi], 10");
    r#gen.emit(&format!("\tje\t{label}_newline"));
    r#gen.emit("\tinc\trsi");
    r#gen.emit(&format!("\tjmp\t{label}_scan"));
    r#gen.emit(&format!("{label}_newline:"));
    r#gen.emit("\tlea\trdx, [rsi + 1]");
    r#gen.emit("\tcmp\trsi, r15");
    r#gen.emit(&format!("\tje\t{label}_copy"));
    r#gen.emit("\tcmp\tbyte ptr [rsi - 1], 13");
    r#gen.emit(&format!("\tjne\t{label}_copy"));
    r#gen.emit("\tdec\trsi");
    r#gen.emit(&format!("\tjmp\t{label}_copy"));
    r#gen.emit(&format!("{label}_last:"));
    r#gen.emit("\tmov\trdx, rsi");
    r#gen.emit(&format!("{label}_copy:"));
    r#gen.emit("\tmov\tQWORD PTR [rbp - 48], rdx");
    r#gen.emit("\tsub\trsi, r15");
    r#gen.emit("\tmov\trdi, r15");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tcall\tstone.list_append");
    r#gen.emit("\tmov\tr15, QWORD PTR [rbp - 48]");
    r#gen.emit(&format!("\tjmp\t{label}_line"));
    r#gen.emit(&format!("{label}_end:"));
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\tesi, 2"); // a list slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits `stone.fs_write` and `stone.fs_append`, as `write` and `append` say, which open the file
/// with their flags and write the whole string, again after a partial write or a signal.
fn write(r#gen: &mut dyn AssemblyGenerator, flags: OpenFlags, write: bool, append: bool) {
    let label = ".Lfs_write";
    if write {
        r#gen.emit("stone.fs_write:");
        r#gen.emit(&format!("\tmov\trdx, {}", flags.write));
        r#gen.emit(&format!("\tjmp\t{label}"));
    }
    if append {
        r#gen.emit("stone.fs_append:");
        r#gen.emit(&format!("\tmov\trdx, {}", flags.append));
    }
    // r12 holds the text, r13 the descriptor, r14 the bytes written, and r15 the flags
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tmov\tr8, rdx");
    r#gen.emit("\tmov\tr9, rsi");
    routine_start(r#gen, &format!("{label}_path"), NULL_IN_PATH_CODE);
    r#gen.emit("\tmov\tr12, r9");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, r8");
    r#gen.emit("\tmov\tedx, 438"); // mode 0o666, less the umask
    r#gen.emit("\tcall\t.Lfs_open");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_opened"));
    fail_with_errno(r#gen, &format!("{label}_path"));
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\txor\tr14, r14");
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tmov\trdx, QWORD PTR [r12 - 16]");
    r#gen.emit("\tsub\trdx, r14");
    r#gen.emit(&format!("\tjz\t{label}_written"));
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tlea\trsi, [r12 + r14]");
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_more"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit("\tadd\tr14, rax");
    r#gen.emit(&format!("\tjmp\t{label}_more"));
    r#gen.emit(&format!("{label}_failed:"));
    r#gen.emit("\tmov\tr14, rax");
    close(r#gen, "r13");
    r#gen.emit("\tmov\trax, r14");
    fail_with_errno(r#gen, &format!("{label}_path"));
    r#gen.emit(&format!("{label}_written:"));
    close(r#gen, "r13");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\txor\tesi, esi"); // an int slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_path_done"));
    routine_end(r#gen, &format!("{label}_path"));
}

/// Emits `stone.fs_list_dir`: the directory's entries from `getdents64` calls, each name but `.`
/// and `..` checked to be text and copied into a new list, which is then sorted.
fn list_dir(r#gen: &mut dyn AssemblyGenerator, flags: i64) {
    let label = "stone.fs_list_dir";
    // r12 holds the descriptor, r13 the buffer, r14 the list, r15 the bytes of entries read, and
    // [rbp - 48] where the next entry starts
    routine_start(r#gen, label, NULL_IN_PATH_CODE);
    r#gen.emit(&format!("\tmov\trsi, {flags}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tcall\t.Lfs_open");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_opened"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit(&format!("\tmov\trdi, {DIR_BUFFER}"));
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tmov\tesi, 1"); // strings
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit(&format!("{label}_fill:"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit(&format!("\tmov\tedx, {DIR_BUFFER}"));
    r#gen.emit("\tmov\teax, 217"); // sys_getdents64
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_fill"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjz\t{label}_end"));
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 48], 0");
    r#gen.emit(&format!("{label}_entry:"));
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 48]");
    r#gen.emit("\tcmp\trax, r15");
    r#gen.emit(&format!("\tjae\t{label}_fill"));
    // a linux_dirent64 holds its length at 16 and its name from 19
    r#gen.emit("\tlea\trdi, [r13 + rax + 19]");
    r#gen.emit("\tmovzx\tecx, word ptr [r13 + rax + 16]");
    r#gen.emit("\tadd\trax, rcx");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 48], rax");
    dot_check(r#gen, &format!("{label}_entry"));
    r#gen.emit("\tcall\tstone.cstr_len");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tcall\t.Lfs_utf8");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_invalid"));
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\tcall\tstone.list_append");
    r#gen.emit(&format!("\tjmp\t{label}_entry"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\trax, {}", -INVALID_TEXT_CODE));
    r#gen.emit(&format!("{label}_failed:"));
    // the error survives the cleanup in r15
    r#gen.emit("\tmov\tr15, rax");
    close(r#gen, "r12");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tdec\tQWORD PTR [r14 - 8]");
    r#gen.emit(&format!("\tjnz\t{label}_released"));
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit("\tcall\tstone.free_list");
    r#gen.emit(&format!("{label}_released:"));
    r#gen.emit("\tmov\trax, r15");
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_end:"));
    close(r#gen, "r12");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.list_sort_str");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\tesi, 2"); // a list slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    routine_end(r#gen, label);
}

/// Emits a jump to `skip` if the null-terminated name at `rdi` is `.` or `..`, which no listing
/// includes, using only `rax`.
fn dot_check(r#gen: &mut dyn AssemblyGenerator, skip: &str) {
    let named = format!("{skip}_named");
    r#gen.emit("\tcmp\tbyte ptr [rdi], 46"); // '.'
    r#gen.emit(&format!("\tjne\t{named}"));
    r#gen.emit("\tmovzx\teax, byte ptr [rdi + 1]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{skip}"));
    r#gen.emit("\tcmp\trax, 46");
    r#gen.emit(&format!("\tjne\t{named}"));
    r#gen.emit("\tcmp\tbyte ptr [rdi + 2], 0");
    r#gen.emit(&format!("\tje\t{skip}"));
    r#gen.emit(&format!("{named}:"));
}

/// Emits `label`, a routine that makes the system call `number` on the path string `rdi` relative
/// to the working directory, with `extra` setting its third argument, and returns `ok` of the
/// path or `err` of the call's error.
fn path_call(r#gen: &mut dyn AssemblyGenerator, label: &str, number: i64, extra: &str) {
    routine_start(r#gen, label, NULL_IN_PATH_CODE);
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit(&format!("\tmov\trdi, {AT_FDCWD}"));
    r#gen.emit(extra);
    r#gen.emit(&format!("\tmov\teax, {number}"));
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_made"));
    fail_with_errno(r#gen, label);
    r#gen.emit(&format!("{label}_made:"));
    // the ok keeps a reference to the path of its own
    r#gen.emit("\tinc\tQWORD PTR [rbx - 8]");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\tesi, 1"); // a string slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    routine_end(r#gen, label);
}
