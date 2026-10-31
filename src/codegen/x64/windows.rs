//! The Windows half of the x86-64 runtime: `_start`, which builds the arguments and environment a
//! Linux program gets from the kernel, and the shims behind [`system_call`] on Windows, which
//! keep a Linux system call's contract over `kernel32.dll` (see [`super::system`]).
//!
//! Windows calls follow the Microsoft x64 convention: the arguments in `rcx`, `rdx`, `r8`, and
//! `r9`, then on the stack above 32 bytes the callee may use, the stack 16-byte aligned at the
//! call, and `rax`, `rcx`, `rdx`, `r8` to `r11`, and `xmm0` to `xmm5` changed. Generated code does
//! not keep the stack aligned, so every shim aligns it itself, and saves every register the call
//! may change that a system call would not.
//!
//! The runtime names files by descriptors, which on Windows are handles: 0, 1, and 2 stand for
//! the standard handles, and a directory being listed is a `FindFirstFileW` handle whose first
//! entry waits in `stone.find_data`. Paths become UTF-16 in `stone.wide_path`, and an error code
//! becomes Linux's number through `stdlib::fs::windows_errno`'s rules, so `.Lfs_err` words it as
//! the interpreter does.

use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{INVALID_TEXT_CODE, UTSNAME_FIELD};
use crate::codegen::x64::system::{Sys, system_call};
use crate::stdlib::fs::{WINDOWS_ERRNO, WINDOWS_ERROR_BASE};

/// How many UTF-16 code units `stone.wide_path` holds, a path's longest length plus its
/// terminator, as Windows' long paths allow.
pub const WIDE_PATH_CHARS: i64 = 32768;

/// How many bytes of UTF-8 a write to a console converts at a time, into `stone.wide_text`.
pub const CONSOLE_CHUNK: i64 = 8192;

/// UTF-8's code page, which `MultiByteToWideChar` and `WideCharToMultiByte` take.
const CP_UTF8: i64 = 65001;

/// `openat`'s flag for a directory, on x86-64 Linux, which the `openat` shim reads.
const O_DIRECTORY: i64 = 0o200000;

/// `openat`'s flag for writing at a file's end.
const O_APPEND: i64 = 0o2000;

/// Where `cFileName` starts in a `WIN32_FIND_DATAW`, after its attributes, three times, size,
/// and two reserved words.
const FIND_NAME: i64 = 44;

/// The size of a `WIN32_FIND_DATAW`: 44 bytes, 260 UTF-16 code units of name, then 14 of short
/// name, rounded up.
const FIND_DATA_SIZE: i64 = 600;

/// Emits `_start`, where Windows starts the program. It builds what Linux's kernel hands a
/// program: if `args`, the arguments from `GetCommandLineW`, split as Rust's standard library
/// splits them on Windows, and if `env`, the environment from `GetEnvironmentStringsW`, each a
/// null-terminated UTF-8 string in memory of its own from `VirtualAlloc`, listed in an array that
/// ends with null. Then it calls `main` with the count in `rdi`, the arguments in `rsi`, and the
/// environment in `rdx`, as on Linux, and ends the program with the status `main` returns.
///
/// The command line splits as `std::env::args` splits it: the program's name runs to the first
/// space or tab outside quotes, with quotes dropped; then arguments are separated by spaces and
/// tabs outside quotes, a quote starts or ends a quoted part, two quotes inside one are a quote,
/// and backslashes are literal unless they come before a quote, when each pair is one backslash
/// and an odd one left over makes the quote literal. For example, `prog a "b c" d\"e` gives
/// `a`, `b c`, and `d"e`. Its text is UTF-8 before it is split, since every byte that matters is
/// ASCII, which UTF-8 never uses inside a character.
pub fn start_runtime(r#gen: &mut dyn AssemblyGenerator, args: bool, env: bool) {
    let emit = |r#gen: &mut dyn AssemblyGenerator, line: &str| r#gen.emit(line);
    emit(r#gen, "\t.globl\t_start");
    emit(r#gen, "_start:");
    emit(r#gen, "\txor\tebp, ebp"); // the outermost frame
    emit(r#gen, "\tand\trsp, -16");
    // room for the arguments of the calls, then two words of its own
    emit(r#gen, "\tsub\trsp, 80");
    // rbx holds the count of arguments, r12 the arguments, and r13 the environment
    emit(r#gen, "\txor\tebx, ebx");
    emit(r#gen, "\txor\tr12d, r12d");
    emit(r#gen, "\txor\tr13d, r13d");
    if args {
        command_line(r#gen);
    }
    if env {
        environment(r#gen);
    }
    emit(r#gen, "\tmov\tedi, ebx");
    emit(r#gen, "\tmov\trsi, r12");
    emit(r#gen, "\tmov\trdx, r13");
    emit(r#gen, "\tcall\tmain");
    emit(r#gen, "\tmov\tedi, eax");
    system_call(r#gen, Sys::Exit);
}

/// Emits a call of `WideCharToMultiByte` to UTF-8 from the UTF-16 at `source`, `units` code units
/// of it (-1 for up to and including its terminator), into `length` bytes at `target` (0 and 0 to
/// measure), for a stack 16-byte aligned with room for the arguments at `[rsp + 32]`.
fn to_utf8(
    r#gen: &mut dyn AssemblyGenerator,
    source: &str,
    units: &str,
    target: &str,
    length: &str,
) {
    r#gen.emit(&format!("\tmov\tr8, {source}"));
    r#gen.emit(&format!("\tmov\tr9, {units}"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + 32], {target}"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + 40], {length}"));
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 56], 0");
    r#gen.emit(&format!("\tmov\tecx, {CP_UTF8}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcall\tWideCharToMultiByte");
}

/// Emits a call of `VirtualAlloc` for `rdx` bytes of new zeros, readable and writable, whose
/// address is in `rax`, or 0 if there is no memory.
fn allocate(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\txor\tecx, ecx"); // anywhere
    r#gen.emit("\tmov\tr8d, 0x3000"); // MEM_COMMIT | MEM_RESERVE
    r#gen.emit("\tmov\tr9d, 4"); // PAGE_READWRITE
    r#gen.emit("\tcall\tVirtualAlloc");
}

/// Emits the part of `_start` that splits the command line into `rbx` arguments listed at `r12`
/// (see [`start_runtime`]).
///
/// The memory holds the line's `n` bytes of UTF-8 (`r15` holds `n`, its terminator included),
/// then the arguments, each followed by its terminator, in at most `2n + 2` bytes, then the
/// array of at most `n + 2` pointers.
fn command_line(r#gen: &mut dyn AssemblyGenerator) {
    let emit = |r#gen: &mut dyn AssemblyGenerator, line: &str| r#gen.emit(line);
    emit(r#gen, "\tcall\tGetCommandLineW");
    emit(r#gen, "\tmov\tr14, rax");
    to_utf8(r#gen, "r14", "-1", "0", "0");
    emit(r#gen, "\tmov\tr15, rax");
    emit(r#gen, "\ttest\trax, rax");
    emit(r#gen, "\tjz\t.Lstart_args_done");
    emit(r#gen, "\timul\trdx, rax, 11");
    emit(r#gen, "\tadd\trdx, 32");
    allocate(r#gen);
    emit(r#gen, "\ttest\trax, rax");
    emit(r#gen, "\tjz\t.Lstart_args_done");
    emit(r#gen, "\tmov\tr13, rax");
    to_utf8(r#gen, "r14", "-1", "r13", "r15");
    // r14 reads the line, rdi writes the arguments, r8 is where the one being written starts,
    // and ecx says whether a quoted part is open
    emit(r#gen, "\tmov\tr14, r13");
    emit(r#gen, "\tlea\trdi, [r13 + r15]");
    emit(r#gen, "\tlea\tr12, [r15 + r15 * 2]");
    emit(r#gen, "\tlea\tr12, [r13 + r12 + 9]");
    emit(r#gen, "\tand\tr12, -8");
    // the program's name, in which a quote only opens or closes a quoted part
    emit(r#gen, "\tmov\tQWORD PTR [r12], rdi");
    emit(r#gen, "\tmov\tebx, 1");
    emit(r#gen, "\txor\tecx, ecx");
    emit(r#gen, ".Lstart_name:");
    emit(r#gen, "\tmovzx\teax, byte ptr [r14]");
    emit(r#gen, "\ttest\teax, eax");
    emit(r#gen, "\tjz\t.Lstart_name_end");
    emit(r#gen, "\tinc\tr14");
    emit(r#gen, "\tcmp\teax, 34"); // '"'
    emit(r#gen, "\tjne\t.Lstart_name_other");
    emit(r#gen, "\txor\tecx, 1");
    emit(r#gen, "\tjmp\t.Lstart_name");
    emit(r#gen, ".Lstart_name_other:");
    emit(r#gen, "\ttest\tecx, ecx");
    emit(r#gen, "\tjnz\t.Lstart_name_take");
    emit(r#gen, "\tcmp\teax, 32"); // ' '
    emit(r#gen, "\tje\t.Lstart_name_end");
    emit(r#gen, "\tcmp\teax, 9"); // '\t'
    emit(r#gen, "\tje\t.Lstart_name_end");
    emit(r#gen, ".Lstart_name_take:");
    emit(r#gen, "\tmov\tbyte ptr [rdi], al");
    emit(r#gen, "\tinc\trdi");
    emit(r#gen, "\tjmp\t.Lstart_name");
    emit(r#gen, ".Lstart_name_end:");
    emit(r#gen, "\tmov\tbyte ptr [rdi], 0");
    emit(r#gen, "\tinc\trdi");
    blanks(r#gen, ".Lstart_blank");
    emit(r#gen, "\tmov\tr8, rdi");
    emit(r#gen, "\txor\tecx, ecx");
    emit(r#gen, ".Lstart_arg:");
    emit(r#gen, "\tmovzx\teax, byte ptr [r14]");
    emit(r#gen, "\ttest\teax, eax");
    emit(r#gen, "\tjz\t.Lstart_end");
    emit(r#gen, "\tinc\tr14");
    emit(r#gen, "\tcmp\teax, 92"); // '\\'
    emit(r#gen, "\tje\t.Lstart_backslash");
    emit(r#gen, "\tcmp\teax, 34"); // '"'
    emit(r#gen, "\tje\t.Lstart_quote");
    emit(r#gen, "\ttest\tecx, ecx");
    emit(r#gen, "\tjnz\t.Lstart_take");
    emit(r#gen, "\tcmp\teax, 32");
    emit(r#gen, "\tje\t.Lstart_separate");
    emit(r#gen, "\tcmp\teax, 9");
    emit(r#gen, "\tje\t.Lstart_separate");
    emit(r#gen, ".Lstart_take:");
    emit(r#gen, "\tmov\tbyte ptr [rdi], al");
    emit(r#gen, "\tinc\trdi");
    emit(r#gen, "\tjmp\t.Lstart_arg");
    // a blank outside quotes ends the argument, and the blanks after it
    emit(r#gen, ".Lstart_separate:");
    emit(r#gen, "\tmov\tbyte ptr [rdi], 0");
    emit(r#gen, "\tinc\trdi");
    emit(r#gen, "\tmov\tQWORD PTR [r12 + rbx * 8], r8");
    emit(r#gen, "\tinc\trbx");
    blanks(r#gen, ".Lstart_separator");
    emit(r#gen, "\tmov\tr8, rdi");
    emit(r#gen, "\tjmp\t.Lstart_arg");
    // edx counts the backslashes, and r9 how many to write
    emit(r#gen, ".Lstart_backslash:");
    emit(r#gen, "\tmov\tedx, 1");
    emit(r#gen, ".Lstart_backslashes:");
    emit(r#gen, "\tcmp\tbyte ptr [r14], 92");
    emit(r#gen, "\tjne\t.Lstart_backslashes_end");
    emit(r#gen, "\tinc\tr14");
    emit(r#gen, "\tinc\tedx");
    emit(r#gen, "\tjmp\t.Lstart_backslashes");
    emit(r#gen, ".Lstart_backslashes_end:");
    emit(r#gen, "\tmov\tr9d, edx");
    emit(r#gen, "\tcmp\tbyte ptr [r14], 34");
    emit(r#gen, "\tjne\t.Lstart_backslash_write");
    emit(r#gen, "\tshr\tr9d, 1");
    emit(r#gen, ".Lstart_backslash_write:");
    emit(r#gen, "\ttest\tr9d, r9d");
    emit(r#gen, "\tjz\t.Lstart_backslash_written");
    emit(r#gen, "\tmov\tbyte ptr [rdi], 92");
    emit(r#gen, "\tinc\trdi");
    emit(r#gen, "\tdec\tr9d");
    emit(r#gen, "\tjmp\t.Lstart_backslash_write");
    emit(r#gen, ".Lstart_backslash_written:");
    // an odd backslash before a quote makes it literal
    emit(r#gen, "\tcmp\tbyte ptr [r14], 34");
    emit(r#gen, "\tjne\t.Lstart_arg");
    emit(r#gen, "\ttest\tedx, 1");
    emit(r#gen, "\tjz\t.Lstart_arg");
    emit(r#gen, "\tinc\tr14");
    emit(r#gen, "\tmov\tbyte ptr [rdi], 34");
    emit(r#gen, "\tinc\trdi");
    emit(r#gen, "\tjmp\t.Lstart_arg");
    // a quote opens a quoted part, and in one is a quote if another follows, or else closes it
    emit(r#gen, ".Lstart_quote:");
    emit(r#gen, "\ttest\tecx, ecx");
    emit(r#gen, "\tjnz\t.Lstart_quoted");
    emit(r#gen, "\tmov\tecx, 1");
    emit(r#gen, "\tjmp\t.Lstart_arg");
    emit(r#gen, ".Lstart_quoted:");
    emit(r#gen, "\tmovzx\teax, byte ptr [r14]");
    emit(r#gen, "\tcmp\teax, 34");
    emit(r#gen, "\tjne\t.Lstart_unquote");
    emit(r#gen, "\tinc\tr14");
    emit(r#gen, "\tjmp\t.Lstart_take");
    emit(r#gen, ".Lstart_unquote:");
    emit(r#gen, "\ttest\teax, eax");
    emit(r#gen, "\tjz\t.Lstart_last");
    emit(r#gen, "\txor\tecx, ecx");
    emit(r#gen, "\tjmp\t.Lstart_arg");
    // the last argument counts if it has anything or a quoted part is open
    emit(r#gen, ".Lstart_end:");
    emit(r#gen, "\tcmp\trdi, r8");
    emit(r#gen, "\tjne\t.Lstart_last");
    emit(r#gen, "\ttest\tecx, ecx");
    emit(r#gen, "\tjz\t.Lstart_listed");
    emit(r#gen, ".Lstart_last:");
    emit(r#gen, "\tmov\tbyte ptr [rdi], 0");
    emit(r#gen, "\tmov\tQWORD PTR [r12 + rbx * 8], r8");
    emit(r#gen, "\tinc\trbx");
    emit(r#gen, ".Lstart_listed:");
    emit(r#gen, "\tmov\tQWORD PTR [r12 + rbx * 8], 0");
    emit(r#gen, ".Lstart_args_done:");
}

/// Emits a loop under `label` that moves `r14` past spaces and tabs.
fn blanks(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tmovzx\teax, byte ptr [r14]");
    r#gen.emit("\tcmp\teax, 32");
    r#gen.emit(&format!("\tje\t{label}_skip"));
    r#gen.emit("\tcmp\teax, 9");
    r#gen.emit(&format!("\tjne\t{label}_done"));
    r#gen.emit(&format!("{label}_skip:"));
    r#gen.emit("\tinc\tr14");
    r#gen.emit(&format!("\tjmp\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
}

/// Emits the part of `_start` that lists the environment at `r13` (see [`start_runtime`]).
///
/// `GetEnvironmentStringsW` gives the variables one after another, each ending with a
/// terminator, then one more terminator. The memory holds their `n` bytes of UTF-8, then the
/// array of at most `n + 2` pointers to them.
fn environment(r#gen: &mut dyn AssemblyGenerator) {
    let emit = |r#gen: &mut dyn AssemblyGenerator, line: &str| r#gen.emit(line);
    emit(r#gen, "\tcall\tGetEnvironmentStringsW");
    emit(r#gen, "\ttest\trax, rax");
    emit(r#gen, "\tjz\t.Lstart_env_done");
    emit(r#gen, "\tmov\tr14, rax");
    // r15 counts the code units through the last terminator
    emit(r#gen, "\tmov\trcx, rax");
    emit(r#gen, ".Lstart_env_count:");
    emit(r#gen, "\tcmp\tword ptr [rcx], 0");
    emit(r#gen, "\tje\t.Lstart_env_counted");
    emit(r#gen, ".Lstart_env_skip:");
    emit(r#gen, "\tadd\trcx, 2");
    emit(r#gen, "\tcmp\tword ptr [rcx - 2], 0");
    emit(r#gen, "\tjne\t.Lstart_env_skip");
    emit(r#gen, "\tjmp\t.Lstart_env_count");
    emit(r#gen, ".Lstart_env_counted:");
    emit(r#gen, "\tsub\trcx, r14");
    emit(r#gen, "\tshr\trcx, 1");
    emit(r#gen, "\tlea\tr15, [rcx + 1]");
    to_utf8(r#gen, "r14", "r15", "0", "0");
    emit(r#gen, "\ttest\trax, rax");
    emit(r#gen, "\tjz\t.Lstart_env_done");
    emit(r#gen, "\tmov\tQWORD PTR [rsp + 64], rax");
    emit(r#gen, "\tlea\trdx, [rax * 8 + 24]");
    emit(r#gen, "\tadd\trdx, rax");
    allocate(r#gen);
    emit(r#gen, "\ttest\trax, rax");
    emit(r#gen, "\tjz\t.Lstart_env_done");
    emit(r#gen, "\tmov\tQWORD PTR [rsp + 72], rax");
    emit(r#gen, "\tmov\trdx, QWORD PTR [rsp + 64]");
    to_utf8(r#gen, "r14", "r15", "rax", "rdx");
    // rcx reads the variables and rdx writes the array, after them on an 8-byte boundary
    emit(r#gen, "\tmov\trcx, QWORD PTR [rsp + 72]");
    emit(r#gen, "\tmov\trdx, QWORD PTR [rsp + 64]");
    emit(r#gen, "\tlea\tr13, [rcx + rdx + 7]");
    emit(r#gen, "\tand\tr13, -8");
    emit(r#gen, "\tmov\trdx, r13");
    emit(r#gen, ".Lstart_env_entry:");
    emit(r#gen, "\tcmp\tbyte ptr [rcx], 0");
    emit(r#gen, "\tje\t.Lstart_env_listed");
    emit(r#gen, "\tmov\tQWORD PTR [rdx], rcx");
    emit(r#gen, "\tadd\trdx, 8");
    emit(r#gen, ".Lstart_env_past:");
    emit(r#gen, "\tinc\trcx");
    emit(r#gen, "\tcmp\tbyte ptr [rcx - 1], 0");
    emit(r#gen, "\tjne\t.Lstart_env_past");
    emit(r#gen, "\tjmp\t.Lstart_env_entry");
    emit(r#gen, ".Lstart_env_listed:");
    emit(r#gen, "\tmov\tQWORD PTR [rdx], 0");
    emit(r#gen, ".Lstart_env_done:");
}

/// Where a shim saves each register it keeps: the six general registers a system call reads
/// its arguments from, below `rbp`, then the low halves of `xmm0` to `xmm5`, which floats live
/// in and a Windows call may change.
const SAVED: [&str; 6] = ["rdx", "rsi", "rdi", "r8", "r9", "r10"];

/// Returns where a shim saved `reg`, one of [`SAVED`], such as `QWORD PTR [rbp - 8]` for `rdx`.
fn saved(reg: &str) -> String {
    let index = SAVED.iter().position(|saved| *saved == reg).unwrap() as i64;
    format!("QWORD PTR [rbp - {}]", 8 * (index + 1))
}

/// Returns a shim's local slot `n`, above the room its calls take for their arguments.
fn local(n: i64) -> String {
    format!("QWORD PTR [rsp + {}]", 64 + 8 * n)
}

/// Emits the Windows shim of every call that `assembly`, the program's text so far, makes through
/// [`system_call`], and the memory and helpers they share. Each shim saves what a Windows call
/// may change, aligns the stack with 64 bytes for the calls' arguments and 64 of its own below
/// them, makes its calls, and returns its result in `rax`, a negative Linux error number for a
/// failure.
pub fn windows_shims(r#gen: &mut dyn AssemblyGenerator, assembly: &str) {
    let calls: Vec<Sys> = Sys::ALL
        .into_iter()
        .filter(|call| assembly.contains(&format!("\tcall\t{}\n", call.shim())))
        .collect();
    if calls.is_empty() {
        return;
    }
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t4");
    r#gen.emit("stone.wide_path:");
    r#gen.emit(&format!("\t.zero\t{}", 2 * WIDE_PATH_CHARS));
    r#gen.emit("stone.wide_text:");
    r#gen.emit(&format!("\t.zero\t{}", 2 * CONSOLE_CHUNK));
    r#gen.emit("stone.find_data:");
    r#gen.emit(&format!("\t.zero\t{FIND_DATA_SIZE}"));
    // the directory being listed, whether its next entry waits in stone.find_data, and the
    // performance counter's frequency once it is known
    r#gen.emit("stone.find_handle:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.find_pending:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.counter_frequency:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("\t.text");
    helpers(r#gen);

    for call in calls {
        let name = call.name();
        let done = format!(".Lsys_{name}_done");
        r#gen.emit(&format!("{}:", call.shim()));
        r#gen.emit("\tpush\trbp");
        r#gen.emit("\tmov\trbp, rsp");
        for reg in SAVED {
            r#gen.emit(&format!("\tpush\t{reg}"));
        }
        r#gen.emit("\tsub\trsp, 48");
        for xmm in 0..6 {
            r#gen.emit(&format!(
                "\tmovq\tQWORD PTR [rbp - {}], xmm{xmm}",
                56 + 8 * xmm
            ));
        }
        r#gen.emit("\tand\trsp, -16");
        r#gen.emit("\tsub\trsp, 128");
        shim_body(r#gen, call, &done);
        r#gen.emit(&format!("{done}:"));
        for xmm in 0..6 {
            r#gen.emit(&format!(
                "\tmovq\txmm{xmm}, QWORD PTR [rbp - {}]",
                56 + 8 * xmm
            ));
        }
        r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * SAVED.len()));
        for reg in SAVED.iter().rev() {
            r#gen.emit(&format!("\tpop\t{reg}"));
        }
        r#gen.emit("\tpop\trbp");
        r#gen.emit("\tret");
    }
}

/// Emits the helpers the shims call, each from a 16-byte aligned stack:
///
/// - `.Lwin_map` returns in `rax` the negative of Linux's number for the Windows error code in
///   `ecx`, as `stdlib::fs::windows_errno` gives it without a path, using only `rax`
/// - `.Lwin_errno` does the same for `GetLastError`'s code
/// - `.Lwin_path_errno` does the same for a call about the path in `stone.wide_path`, which
///   decides `ENOTDIR` and `EISDIR` as `windows_errno` does
/// - `.Lwin_wide` writes the UTF-8 path string `rax` into `stone.wide_path` as UTF-16 and
///   returns the code units written, its terminator included, or 0 if it does not fit
fn helpers(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lwin_map:");
    for (code, errno) in WINDOWS_ERRNO {
        r#gen.emit(&format!("\tmov\trax, {}", -errno));
        r#gen.emit(&format!("\tcmp\tecx, {code}"));
        r#gen.emit("\tje\t.Lwin_map_done");
    }
    r#gen.emit("\tmov\teax, ecx");
    r#gen.emit(&format!("\tadd\trax, {WINDOWS_ERROR_BASE}"));
    r#gen.emit("\tneg\trax");
    r#gen.emit(".Lwin_map_done:");
    r#gen.emit("\tret");

    r#gen.emit(".Lwin_errno:");
    r#gen.emit("\tsub\trsp, 40");
    r#gen.emit("\tcall\tGetLastError");
    r#gen.emit("\tmov\tecx, eax");
    r#gen.emit("\tadd\trsp, 40");
    r#gen.emit("\tjmp\t.Lwin_map");

    // ebx holds the code, r12 walks the path, r13 is its start, and [rsp + 32] the unit that a
    // terminator stands in for while a leading part of the path is asked about
    r#gen.emit(".Lwin_path_errno:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tsub\trsp, 48");
    r#gen.emit("\tcall\tGetLastError");
    r#gen.emit("\tmov\tebx, eax");
    r#gen.emit("\tlea\tr13, [rip + stone.wide_path]");
    r#gen.emit("\tcmp\tebx, 5"); // ERROR_ACCESS_DENIED
    r#gen.emit("\tje\t.Lwin_path_denied");
    r#gen.emit("\tcmp\tebx, 2"); // ERROR_FILE_NOT_FOUND
    r#gen.emit("\tje\t.Lwin_path_missing");
    r#gen.emit("\tcmp\tebx, 3"); // ERROR_PATH_NOT_FOUND
    r#gen.emit("\tjne\t.Lwin_path_mapped");
    r#gen.emit(".Lwin_path_missing:");
    r#gen.emit("\tmov\tr12, r13");
    r#gen.emit(".Lwin_path_walk:");
    r#gen.emit("\tmovzx\teax, word ptr [r12]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjz\t.Lwin_path_whole");
    r#gen.emit("\tcmp\teax, 47"); // '/'
    r#gen.emit("\tje\t.Lwin_path_part");
    r#gen.emit("\tcmp\teax, 92"); // '\\'
    r#gen.emit("\tjne\t.Lwin_path_next");
    r#gen.emit(".Lwin_path_part:");
    r#gen.emit("\tcmp\tr12, r13");
    r#gen.emit("\tje\t.Lwin_path_next");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\tword ptr [r12], 0");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tcall\tGetFileAttributesW");
    r#gen.emit("\tmov\trcx, QWORD PTR [rsp + 32]");
    r#gen.emit("\tmov\tword ptr [r12], cx");
    is_file(r#gen, ".Lwin_path_next");
    r#gen.emit("\tjmp\t.Lwin_path_not_dir");
    r#gen.emit(".Lwin_path_next:");
    r#gen.emit("\tadd\tr12, 2");
    r#gen.emit("\tjmp\t.Lwin_path_walk");
    r#gen.emit(".Lwin_path_whole:");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tcall\tGetFileAttributesW");
    is_file(r#gen, ".Lwin_path_mapped");
    r#gen.emit(".Lwin_path_not_dir:");
    r#gen.emit("\tmov\trax, -20"); // ENOTDIR
    r#gen.emit("\tjmp\t.Lwin_path_done");
    r#gen.emit(".Lwin_path_denied:");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tcall\tGetFileAttributesW");
    r#gen.emit("\tcmp\teax, -1");
    r#gen.emit("\tje\t.Lwin_path_mapped");
    r#gen.emit("\ttest\teax, 16"); // FILE_ATTRIBUTE_DIRECTORY
    r#gen.emit("\tjz\t.Lwin_path_mapped");
    r#gen.emit("\tmov\trax, -21"); // EISDIR
    r#gen.emit("\tjmp\t.Lwin_path_done");
    r#gen.emit(".Lwin_path_mapped:");
    r#gen.emit("\tmov\tecx, ebx");
    r#gen.emit("\tcall\t.Lwin_map");
    r#gen.emit(".Lwin_path_done:");
    r#gen.emit("\tadd\trsp, 48");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");

    r#gen.emit(".Lwin_wide:");
    r#gen.emit("\tsub\trsp, 56");
    r#gen.emit(&format!("\tmov\tecx, {CP_UTF8}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8, rax");
    r#gen.emit("\tmov\tr9, -1"); // through the terminator
    r#gen.emit("\tlea\trax, [rip + stone.wide_path]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + 40], {WIDE_PATH_CHARS}"));
    r#gen.emit("\tcall\tMultiByteToWideChar");
    r#gen.emit("\tadd\trsp, 56");
    r#gen.emit("\tret");
}

/// Emits a jump to `otherwise` unless the attributes `GetFileAttributesW` returned in `eax` are
/// of something that is there and is not a directory.
fn is_file(r#gen: &mut dyn AssemblyGenerator, otherwise: &str) {
    r#gen.emit("\tcmp\teax, -1"); // INVALID_FILE_ATTRIBUTES
    r#gen.emit(&format!("\tje\t{otherwise}"));
    r#gen.emit("\ttest\teax, 16"); // FILE_ATTRIBUTE_DIRECTORY
    r#gen.emit(&format!("\tjnz\t{otherwise}"));
}

/// Emits code that puts the handle for the descriptor `rdi` in `rax`: the standard handle for 0,
/// 1, or 2, else the descriptor itself, which is a handle.
fn handle(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tmov\trax, rdi");
    r#gen.emit("\tcmp\trdi, 2");
    r#gen.emit(&format!("\tja\t{label}"));
    r#gen.emit("\tmov\tecx, -10"); // STD_INPUT_HANDLE, then -11 and -12
    r#gen.emit("\tsub\tecx, edi");
    r#gen.emit("\tcall\tGetStdHandle");
    r#gen.emit(&format!("{label}:"));
}

/// Emits the body of `call`'s shim, which ends at `done` with the result in `rax`. The shim's
/// arguments are still in their registers at the start, and saved where [`saved`] says.
fn shim_body(r#gen: &mut dyn AssemblyGenerator, call: Sys, done: &str) {
    let emit = |r#gen: &mut dyn AssemblyGenerator, line: &str| r#gen.emit(line);
    let label = |part: &str| format!(".Lsys_{}_{part}", call.name());
    let fail = |r#gen: &mut dyn AssemblyGenerator| {
        r#gen.emit("\tcall\t.Lwin_errno");
        r#gen.emit(&format!("\tjmp\t{done}"));
    };
    match call {
        Sys::Exit => {
            emit(r#gen, "\tmov\tecx, edi");
            emit(r#gen, "\tcall\tExitProcess");
        }
        Sys::Write => {
            // a console takes UTF-16, so the bytes are converted a chunk at a time, ending on
            // a character's first byte
            emit(r#gen, "\txor\teax, eax");
            emit(r#gen, "\ttest\trdx, rdx");
            emit(r#gen, &format!("\tjz\t{done}"));
            handle(r#gen, &label("handle"));
            emit(r#gen, &format!("\tmov\t{}, rax", local(0)));
            emit(r#gen, "\tmov\trcx, rax");
            emit(r#gen, "\tlea\trdx, [rsp + 72]");
            emit(r#gen, "\tcall\tGetConsoleMode");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjnz\t{}", label("console")));
            emit(r#gen, &format!("\tmov\trcx, {}", local(0)));
            emit(r#gen, "\tmov\trdx, rsi");
            emit(r#gen, &format!("\tmov\tr8, {}", saved("rdx")));
            emit(r#gen, "\tmov\teax, 0x40000000");
            emit(r#gen, "\tcmp\tr8, rax");
            emit(r#gen, "\tcmova\tr8, rax");
            emit(r#gen, "\tlea\tr9, [rsp + 72]");
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 32], 0");
            emit(r#gen, "\tcall\tWriteFile");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("failed")));
            emit(r#gen, "\tmov\teax, DWORD PTR [rsp + 72]");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("console")));
            emit(r#gen, &format!("\tmov\tr8, {}", saved("rdx")));
            emit(r#gen, &format!("\tcmp\tr8, {CONSOLE_CHUNK}"));
            emit(r#gen, &format!("\tjbe\t{}", label("chunk")));
            emit(r#gen, &format!("\tmov\tr8d, {CONSOLE_CHUNK}"));
            emit(r#gen, &format!("{}:", label("back")));
            emit(r#gen, "\tmovzx\teax, byte ptr [rsi + r8]");
            emit(r#gen, "\tand\teax, 0xc0");
            emit(r#gen, "\tcmp\teax, 0x80");
            emit(r#gen, &format!("\tjne\t{}", label("chunk")));
            emit(r#gen, "\tdec\tr8");
            emit(r#gen, &format!("\tjmp\t{}", label("back")));
            emit(r#gen, &format!("{}:", label("chunk")));
            emit(r#gen, &format!("\tmov\t{}, r8", local(2)));
            emit(r#gen, "\tmov\tr9, r8");
            emit(r#gen, "\tmov\tr8, rsi");
            emit(r#gen, "\tlea\trax, [rip + stone.wide_text]");
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 32], rax");
            emit(
                r#gen,
                &format!("\tmov\tQWORD PTR [rsp + 40], {CONSOLE_CHUNK}"),
            );
            emit(r#gen, &format!("\tmov\tecx, {CP_UTF8}"));
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tcall\tMultiByteToWideChar");
            emit(r#gen, &format!("\tmov\trcx, {}", local(0)));
            emit(r#gen, "\tlea\trdx, [rip + stone.wide_text]");
            emit(r#gen, "\tmov\tr8, rax");
            emit(r#gen, "\tlea\tr9, [rsp + 72]");
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 32], 0");
            emit(r#gen, "\tcall\tWriteConsoleW");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("failed")));
            emit(r#gen, &format!("\tmov\trax, {}", local(2)));
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("failed")));
            fail(r#gen);
        }
        Sys::Read => {
            // a pipe whose writer is gone has ended, as Rust reads it
            handle(r#gen, &label("handle"));
            emit(r#gen, "\tmov\trcx, rax");
            emit(r#gen, "\tmov\trdx, rsi");
            emit(r#gen, &format!("\tmov\tr8, {}", saved("rdx")));
            emit(r#gen, "\tmov\teax, 0x40000000");
            emit(r#gen, "\tcmp\tr8, rax");
            emit(r#gen, "\tcmova\tr8, rax");
            emit(r#gen, "\tlea\tr9, [rsp + 72]");
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 32], 0");
            emit(r#gen, "\tcall\tReadFile");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("failed")));
            emit(r#gen, "\tmov\teax, DWORD PTR [rsp + 72]");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("failed")));
            emit(r#gen, "\tcall\tGetLastError");
            emit(r#gen, "\tmov\tecx, eax");
            emit(r#gen, "\txor\teax, eax");
            emit(r#gen, "\tcmp\tecx, 109"); // ERROR_BROKEN_PIPE
            emit(r#gen, &format!("\tje\t{done}"));
            emit(r#gen, "\tcall\t.Lwin_map");
        }
        Sys::Close => {
            emit(r#gen, "\tmov\trcx, rdi");
            emit(r#gen, "\tcmp\trdi, QWORD PTR [rip + stone.find_handle]");
            emit(r#gen, &format!("\tjne\t{}", label("handle")));
            emit(r#gen, "\tmov\tQWORD PTR [rip + stone.find_handle], 0");
            emit(r#gen, "\tcall\tFindClose");
            emit(r#gen, "\txor\teax, eax");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("handle")));
            emit(r#gen, "\tcall\tCloseHandle");
            emit(r#gen, "\txor\teax, eax");
        }
        Sys::Mmap => {
            emit(r#gen, "\tmov\trdx, rsi");
            allocate(r#gen);
            emit(r#gen, "\ttest\trax, rax");
            emit(r#gen, &format!("\tjnz\t{done}"));
            emit(r#gen, "\tmov\trax, -12"); // ENOMEM
        }
        Sys::Munmap => {
            emit(r#gen, "\tmov\trcx, rdi");
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tmov\tr8d, 0x8000"); // MEM_RELEASE
            emit(r#gen, "\tcall\tVirtualFree");
            emit(r#gen, "\txor\teax, eax");
        }
        Sys::Mremap => {
            // Windows cannot grow a mapping, so the bytes move to a new one
            allocate(r#gen);
            emit(r#gen, "\ttest\trax, rax");
            emit(r#gen, &format!("\tjnz\t{}", label("moved")));
            emit(r#gen, "\tmov\trax, -12"); // ENOMEM
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("moved")));
            emit(r#gen, &format!("\tmov\t{}, rax", local(0)));
            emit(r#gen, "\tmov\trdi, rax");
            emit(r#gen, &format!("\tmov\trsi, {}", saved("rdi")));
            emit(r#gen, &format!("\tmov\trcx, {}", saved("rsi")));
            emit(r#gen, "\trep\tmovsb");
            emit(r#gen, &format!("\tmov\trcx, {}", saved("rdi")));
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tmov\tr8d, 0x8000"); // MEM_RELEASE
            emit(r#gen, "\tcall\tVirtualFree");
            emit(r#gen, &format!("\tmov\trax, {}", local(0)));
        }
        Sys::Nanosleep => {
            // milliseconds, rounded up, and at most what Sleep takes short of forever
            emit(r#gen, "\tmov\trax, QWORD PTR [rdi + 8]");
            emit(r#gen, "\tadd\trax, 999999");
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tmov\tecx, 1000000");
            emit(r#gen, "\tdiv\trcx");
            emit(r#gen, "\tmov\tr8, rax");
            emit(r#gen, "\tmov\tecx, 0xfffffffe");
            emit(r#gen, "\tmov\trax, QWORD PTR [rdi]");
            emit(r#gen, "\tcmp\trax, 4294967");
            emit(r#gen, "\tcmova\trax, rcx");
            emit(r#gen, "\timul\trax, rax, 1000");
            emit(r#gen, "\tadd\trax, r8");
            emit(r#gen, "\tcmp\trax, rcx");
            emit(r#gen, "\tcmova\trax, rcx");
            emit(r#gen, "\tmov\tecx, eax");
            emit(r#gen, "\tcall\tSleep");
            emit(r#gen, "\txor\teax, eax");
        }
        Sys::Getpid => emit(r#gen, "\tcall\tGetCurrentProcessId"),
        Sys::Uname => {
            // only the host name, which is all the runtime reads
            emit(r#gen, &format!("\tmov\t{}, 256", local(0)));
            emit(r#gen, "\tmov\tecx, 1"); // ComputerNameDnsHostname
            emit(r#gen, "\tlea\trdx, [rip + stone.wide_text]");
            emit(r#gen, "\tlea\tr8, [rsp + 64]");
            emit(r#gen, "\tcall\tGetComputerNameExW");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("failed")));
            emit(r#gen, "\tlea\trax, [rip + stone.wide_text]");
            emit(r#gen, &format!("\tlea\trcx, [rdi + {UTSNAME_FIELD}]"));
            emit(r#gen, "\tmov\tedx, DWORD PTR [rsp + 64]");
            to_utf8(r#gen, "rax", "rdx", "rcx", "64");
            emit(
                r#gen,
                &format!("\tmov\tbyte ptr [rdi + rax + {UTSNAME_FIELD}], 0"),
            );
            emit(r#gen, "\txor\teax, eax");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("failed")));
            fail(r#gen);
        }
        Sys::Getcwd => {
            emit(r#gen, &format!("\tmov\tecx, {WIDE_PATH_CHARS}"));
            emit(r#gen, "\tlea\trdx, [rip + stone.wide_path]");
            emit(r#gen, "\tcall\tGetCurrentDirectoryW");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("failed")));
            emit(r#gen, &format!("\tcmp\teax, {WIDE_PATH_CHARS}"));
            emit(r#gen, &format!("\tjae\t{}", label("long")));
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            emit(r#gen, &format!("\tmov\trdx, {}", saved("rsi")));
            emit(r#gen, "\tdec\trdx");
            to_utf8(r#gen, "rcx", "rax", "rdi", "rdx");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("long")));
            emit(r#gen, "\tmov\tbyte ptr [rdi + rax], 0");
            emit(r#gen, "\tinc\teax");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("long")));
            emit(r#gen, "\tmov\trax, -34"); // ERANGE
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("failed")));
            fail(r#gen);
        }
        Sys::Getdents64 => {
            // one linux_dirent64 at a time: its length at 16 and its name from 19
            emit(r#gen, "\tcmp\tQWORD PTR [rip + stone.find_pending], 0");
            emit(r#gen, &format!("\tjne\t{}", label("entry")));
            emit(r#gen, "\tmov\trcx, rdi");
            emit(r#gen, "\tlea\trdx, [rip + stone.find_data]");
            emit(r#gen, "\tcall\tFindNextFileW");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjnz\t{}", label("entry")));
            emit(r#gen, "\tcall\tGetLastError");
            emit(r#gen, "\tmov\tecx, eax");
            emit(r#gen, "\txor\teax, eax");
            emit(r#gen, "\tcmp\tecx, 18"); // ERROR_NO_MORE_FILES
            emit(r#gen, &format!("\tje\t{done}"));
            emit(r#gen, "\tcall\t.Lwin_map");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("entry")));
            emit(r#gen, "\tmov\tQWORD PTR [rip + stone.find_pending], 0");
            emit(
                r#gen,
                &format!("\tlea\trax, [rip + stone.find_data + {FIND_NAME}]"),
            );
            emit(r#gen, "\tlea\trcx, [rsi + 19]");
            emit(r#gen, &format!("\tmov\trdx, {}", saved("rdx")));
            emit(r#gen, "\tsub\trdx, 19");
            to_utf8(r#gen, "rax", "-1", "rcx", "rdx");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tjz\t{}", label("invalid")));
            emit(r#gen, "\tadd\teax, 19 + 7");
            emit(r#gen, "\tand\teax, -8");
            emit(r#gen, "\tmov\tword ptr [rsi + 16], ax");
            emit(r#gen, &format!("\tjmp\t{done}"));
            // a name of unpaired surrogates has no UTF-8
            emit(r#gen, &format!("{}:", label("invalid")));
            emit(r#gen, &format!("\tmov\trax, {}", -INVALID_TEXT_CODE));
        }
        Sys::ClockGettime => {
            emit(r#gen, "\ttest\tedi, edi");
            emit(r#gen, &format!("\tjnz\t{}", label("monotonic")));
            // 100-nanosecond ticks since 1601
            emit(r#gen, "\tlea\trcx, [rsp + 64]");
            emit(r#gen, "\tcall\tGetSystemTimePreciseAsFileTime");
            emit(r#gen, &format!("\tmov\trax, {}", local(0)));
            emit(r#gen, "\tmovabs\trcx, 116444736000000000");
            emit(r#gen, "\tsub\trax, rcx");
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tmov\tecx, 10000000");
            emit(r#gen, "\tdiv\trcx");
            emit(r#gen, "\timul\trdx, rdx, 100");
            emit(r#gen, &format!("\tjmp\t{}", label("store")));
            emit(r#gen, &format!("{}:", label("monotonic")));
            emit(r#gen, "\tcmp\tQWORD PTR [rip + stone.counter_frequency], 0");
            emit(r#gen, &format!("\tjne\t{}", label("counter")));
            emit(r#gen, "\tlea\trcx, [rip + stone.counter_frequency]");
            emit(r#gen, "\tcall\tQueryPerformanceFrequency");
            emit(r#gen, &format!("{}:", label("counter")));
            emit(r#gen, "\tlea\trcx, [rsp + 64]");
            emit(r#gen, "\tcall\tQueryPerformanceCounter");
            emit(r#gen, &format!("\tmov\trax, {}", local(0)));
            emit(
                r#gen,
                "\tmov\trcx, QWORD PTR [rip + stone.counter_frequency]",
            );
            emit(r#gen, "\txor\tedx, edx");
            emit(r#gen, "\tdiv\trcx");
            emit(r#gen, &format!("\tmov\t{}, rax", local(1)));
            emit(r#gen, "\tmov\trax, rdx");
            emit(r#gen, "\tmov\tr8d, 1000000000");
            emit(r#gen, "\tmul\tr8");
            emit(r#gen, "\tdiv\trcx");
            emit(r#gen, "\tmov\trdx, rax");
            emit(r#gen, &format!("\tmov\trax, {}", local(1)));
            emit(r#gen, &format!("{}:", label("store")));
            emit(r#gen, "\tmov\tQWORD PTR [rsi], rax");
            emit(r#gen, "\tmov\tQWORD PTR [rsi + 8], rdx");
            emit(r#gen, "\txor\teax, eax");
        }
        Sys::Openat => {
            emit(r#gen, "\tmov\trax, rsi");
            emit(r#gen, "\tcall\t.Lwin_wide");
            emit(r#gen, "\ttest\trax, rax");
            emit(r#gen, &format!("\tjz\t{}", label("long")));
            emit(r#gen, &format!("\tmov\t{}, rax", local(0)));
            emit(r#gen, &format!("\tmov\trdx, {}", saved("rdx")));
            emit(r#gen, &format!("\ttest\tedx, {O_DIRECTORY}"));
            emit(r#gen, &format!("\tjnz\t{}", label("directory")));
            // what Rust's OpenOptions ask for reading, writing over, and appending
            emit(r#gen, "\tmov\teax, edx");
            emit(r#gen, "\tand\teax, 3");
            emit(r#gen, &format!("\tjnz\t{}", label("writes")));
            emit(r#gen, "\tmov\tedx, 0x80000000"); // GENERIC_READ
            emit(r#gen, "\tmov\teax, 3"); // OPEN_EXISTING
            emit(r#gen, &format!("\tjmp\t{}", label("create")));
            emit(r#gen, &format!("{}:", label("writes")));
            emit(r#gen, &format!("\ttest\tedx, {O_APPEND}"));
            emit(r#gen, &format!("\tjz\t{}", label("over")));
            emit(r#gen, "\tmov\tedx, 0x120114"); // FILE_GENERIC_WRITE without FILE_WRITE_DATA
            emit(r#gen, "\tmov\teax, 4"); // OPEN_ALWAYS
            emit(r#gen, &format!("\tjmp\t{}", label("create")));
            emit(r#gen, &format!("{}:", label("over")));
            emit(r#gen, "\tmov\tedx, 0x40000000"); // GENERIC_WRITE
            emit(r#gen, "\tmov\teax, 2"); // CREATE_ALWAYS
            emit(r#gen, &format!("{}:", label("create")));
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 32], rax");
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 40], 0x80"); // FILE_ATTRIBUTE_NORMAL
            emit(r#gen, "\tmov\tQWORD PTR [rsp + 48], 0");
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            emit(r#gen, "\tmov\tr8d, 7"); // shared for reading, writing, and deleting
            emit(r#gen, "\txor\tr9d, r9d");
            emit(r#gen, "\tcall\tCreateFileW");
            emit(r#gen, "\tcmp\trax, -1");
            emit(r#gen, &format!("\tjne\t{done}"));
            emit(r#gen, "\tcall\t.Lwin_path_errno");
            emit(r#gen, &format!("\tjmp\t{done}"));
            // a directory is listed from FindFirstFileW of the path and \*, whose first entry
            // waits for getdents64
            emit(r#gen, &format!("{}:", label("directory")));
            // an empty path names no directory, though \* alone is the current drive's root
            emit(r#gen, "\tcmp\trax, 1");
            emit(r#gen, &format!("\tje\t{}", label("empty")));
            emit(r#gen, &format!("\tcmp\trax, {}", WIDE_PATH_CHARS - 2));
            emit(r#gen, &format!("\tja\t{}", label("long")));
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            emit(r#gen, "\tlea\trcx, [rcx + rax * 2 - 2]");
            emit(r#gen, "\tmov\tDWORD PTR [rcx], 0x2a005c"); // \*
            emit(r#gen, "\tmov\tword ptr [rcx + 4], 0");
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            emit(r#gen, "\tlea\trdx, [rip + stone.find_data]");
            emit(r#gen, "\tcall\tFindFirstFileW");
            emit(r#gen, "\tcmp\trax, -1");
            emit(r#gen, &format!("\tje\t{}", label("unlisted")));
            emit(r#gen, "\tmov\tQWORD PTR [rip + stone.find_handle], rax");
            emit(r#gen, "\tmov\tQWORD PTR [rip + stone.find_pending], 1");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("unlisted")));
            emit(r#gen, &format!("\tmov\trax, {}", local(0)));
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            emit(r#gen, "\tmov\tword ptr [rcx + rax * 2 - 2], 0");
            emit(r#gen, "\tcall\t.Lwin_path_errno");
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("empty")));
            emit(r#gen, "\tmov\trax, -2"); // ENOENT
            emit(r#gen, &format!("\tjmp\t{done}"));
            emit(r#gen, &format!("{}:", label("long")));
            emit(r#gen, "\tmov\trax, -36"); // ENAMETOOLONG
        }
        Sys::Mkdirat | Sys::Unlinkat | Sys::Faccessat => {
            emit(r#gen, "\tmov\trax, rsi");
            emit(r#gen, "\tcall\t.Lwin_wide");
            emit(r#gen, "\ttest\trax, rax");
            emit(r#gen, &format!("\tjz\t{}", label("long")));
            emit(r#gen, "\tlea\trcx, [rip + stone.wide_path]");
            match call {
                Sys::Mkdirat => {
                    emit(r#gen, "\txor\tedx, edx");
                    emit(r#gen, "\tcall\tCreateDirectoryW");
                }
                Sys::Unlinkat => emit(r#gen, "\tcall\tDeleteFileW"),
                _ => {
                    emit(r#gen, "\tcall\tGetFileAttributesW");
                    emit(r#gen, "\tcmp\teax, -1");
                    emit(r#gen, "\tmov\teax, 0");
                    emit(r#gen, &format!("\tjne\t{done}"));
                    emit(r#gen, "\tmov\trax, -2"); // ENOENT
                    emit(r#gen, &format!("\tjmp\t{done}"));
                }
            }
            if call != Sys::Faccessat {
                emit(r#gen, "\ttest\teax, eax");
                emit(r#gen, "\tmov\teax, 0");
                emit(r#gen, &format!("\tjnz\t{done}"));
                emit(r#gen, "\tcall\t.Lwin_path_errno");
                emit(r#gen, &format!("\tjmp\t{done}"));
            }
            emit(r#gen, &format!("{}:", label("long")));
            emit(r#gen, "\tmov\trax, -36"); // ENAMETOOLONG
        }
        Sys::Getrandom => {
            emit(r#gen, "\txor\tecx, ecx");
            emit(r#gen, "\tmov\trdx, rdi");
            emit(r#gen, "\tmov\tr8, rsi");
            emit(r#gen, "\tmov\tr9d, 2"); // BCRYPT_USE_SYSTEM_PREFERRED_RNG
            emit(r#gen, "\tcall\tBCryptGenRandom");
            emit(r#gen, "\ttest\teax, eax");
            emit(r#gen, &format!("\tmov\trax, {}", saved("rsi")));
            emit(r#gen, &format!("\tjz\t{done}"));
            emit(r#gen, "\tmov\trax, -5"); // EIO
        }
        Sys::CpuCount => {
            emit(r#gen, "\tmov\tecx, 0xffff"); // ALL_PROCESSOR_GROUPS
            emit(r#gen, "\tcall\tGetActiveProcessorCount");
        }
    }
}
