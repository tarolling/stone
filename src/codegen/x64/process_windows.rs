//! The x86-64 Windows runtime of `os.run`, which runs another program through `kernel32.dll` as
//! `stdlib::os::run` does through Rust's `std::process` for the interpreter on Windows.
//!
//! `stone.os_run` looks for the program as `stdlib::os::windows_candidates` lists its paths,
//! asking `GetFileAttributesW` about each before `CreateProcessW` starts it, with the command line
//! `stdlib::os::windows_command_line` writes, and treats the error codes it meets as
//! `stdlib::os::run` does. The program's stdin reads `NUL`, and its stdout and stderr write to
//! named pipes whose ends the routine reads with overlapped `ReadFile`s, waiting for either with
//! `WaitForMultipleObjects`, so a program that fills one while the other waits never stalls, as
//! Rust's `read2` reads them. Then it waits for the program and returns a result built with
//! `stone.enum_new`, as the Linux routine does: `ok` of an `output` whose status is the exit code,
//! or `err` of a message from `.Lfs_err`.
//!
//! Windows calls follow the Microsoft x64 convention from a 16-byte aligned stack with 32 bytes of
//! shadow space, and keep `rbx`, `rsi`, `rdi`, and `r12` through `r15`; the routine keeps its
//! state in those and in slots below `rbp`.

use super::fs::{routine_end, routine_start};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{
    FILE_BUFFER, INVALID_TEXT_CODE, NULL_IN_COMMAND_CODE, OUTPUT_KINDS, immortal_string,
};
use crate::stdlib::os::{WINDOWS_BAD_FORMAT, WINDOWS_DENIED, WINDOWS_NOT_FOUND};

/// UTF-8's code page, which `MultiByteToWideChar` takes.
const CP_UTF8: i64 = 65001;

/// The prefix of the name of each pipe, after which come 16 hexadecimal digits of the process ID
/// and a count of the pipes made.
const PIPE_PREFIX: &str = r"\\.\pipe\stone.";

/// `ERROR_BROKEN_PIPE`, what reading a pipe whose writer has closed gives.
const ERROR_BROKEN_PIPE: i64 = 109;

/// `ERROR_IO_PENDING`, what an overlapped `ReadFile` that has not finished yet gives.
const ERROR_IO_PENDING: i64 = 997;

/// How many bytes each pipe holds, as Rust's pipes hold.
const PIPE_CAPACITY: i64 = 65536;

// where the routine keeps its state, below the registers `routine_start` saves
/// The text of the arguments, each after a space, as the command line ends.
const ARGS_TEXT: i64 = 56;
const ARGS_LEN: i64 = 64;
/// The buffer each path tried is written in, and whether a directory of `PATH` is left.
const PATH: i64 = 72;
const MORE: i64 = 80;
/// The next directory of `PATH`, and whether the command is the only path.
const NEXT_DIR: i64 = 88;
const DIRECT: i64 = 96;
/// Whether a path could not be run, and the last error code met.
const DENIED: i64 = 104;
const LAST: i64 = 112;
/// The path tried and the command line in UTF-16, from `stone.mem_alloc`.
const WIDE_APP: i64 = 120;
const WIDE_LINE: i64 = 128;
/// The handles the program is given, then the ends of the pipes this routine reads and their
/// events, each 0 until made and once closed.
const NUL_HANDLE: i64 = 136;
const WRITE_ENDS: [i64; 2] = [144, 152];
const READ_ENDS: [i64; 2] = [160, 168];
const EVENTS: [i64; 2] = [176, 184];
/// Each pipe's state: 0 to read again, 1 while a read is under way, and 2 once it has ended.
const STATES: [i64; 2] = [192, 200];
/// Each pipe's output: a buffer from `stone.mem_alloc` at the slot, the bytes it holds 8 bytes
/// above, and its size 16 above.
const BUFFERS: [i64; 2] = [224, 248];
/// Each pipe's `OVERLAPPED`, 32 bytes, its event at 24.
const OVERLAPPED: [i64; 2] = [280, 312];
/// The `PROCESS_INFORMATION`, 24 bytes: the process, its thread, and their IDs.
const PROCESS_INFO: i64 = 336;
/// The `STARTUPINFOW`, 104 bytes, its flags at 60 and the standard handles at 80, 88, and 96.
const STARTUP_INFO: i64 = 440;
/// The `SECURITY_ATTRIBUTES`, 24 bytes, which let the program inherit the handles it is given.
const SECURITY: i64 = 464;
/// The bytes a read gave, the exit code, the events waited for, and which pipe each is.
const GOT: i64 = 472;
const STATUS: i64 = 480;
const WAITS: i64 = 496;
const WAITED: i64 = 512;
/// The name of a pipe in UTF-16, 32 code units.
const NAME: i64 = 576;
/// The room for the locals plus 96 bytes of arguments to calls.
const FRAME: i64 = NAME + 96;

/// Returns the slot `offset` bytes below `rbp`.
fn slot(offset: i64) -> String {
    format!("QWORD PTR [rbp - {offset}]")
}

/// Returns the 32-bit slot `offset` bytes below `rbp`.
fn dword(offset: i64) -> String {
    format!("DWORD PTR [rbp - {offset}]")
}

/// Emits `text` in UTF-16 with its terminator under `label`, as `.quad`s of four code units,
/// padded with zeros.
fn wide_literal(r#gen: &mut dyn AssemblyGenerator, label: &str, text: &str) {
    let mut units: Vec<u16> = text.encode_utf16().chain([0]).collect();
    while !units.len().is_multiple_of(4) {
        units.push(0);
    }
    r#gen.emit("\t.p2align\t3");
    r#gen.emit(&format!("{label}:"));
    for quad in units.chunks(4) {
        let value = quad
            .iter()
            .rev()
            .fold(0u64, |value, &unit| (value << 16) | unit as u64);
        r#gen.emit(&format!("\t.quad\t{value:#018x}"));
    }
}

/// Emits `stone.os_run` for Windows, which runs the program named by the string `rdi` with the
/// strings of the list `rsi` as its arguments and returns a result of an `output`, both
/// borrowed, following `stdlib::os::run`.
///
/// It needs the string, list, and enum runtimes, `stone.getenv`, the `fs` routines' `.Lfs_err`,
/// `.Lfs_check_path`, and `.Lfs_utf8`, and the shims' `.Lwin_map` and `.Lwin_errno`, which the
/// allocator's calls bring.
pub fn run_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.os_run";
    r#gen.emit("\t.data");
    wide_literal(r#gen, ".Los_run_nul", "NUL");
    wide_literal(r#gen, ".Los_run_pipe", PIPE_PREFIX);
    immortal_string(r#gen, ".Los_run_path_name", "PATH");
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    // how many pipes the program has made, so each has a name of its own
    r#gen.emit("stone.os_run_pipes:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("\t.text");
    wide(r#gen);

    // rbx holds the command and r12 the arguments
    routine_start(r#gen, label, NULL_IN_COMMAND_CODE);
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit(&format!("\tlea\trsp, [rbp - {FRAME}]"));
    r#gen.emit("\tand\trsp, -16");
    // nothing is made yet, so cleaning up can look at every slot
    r#gen.emit("\txor\teax, eax");
    for offset in [ARGS_TEXT, PATH, WIDE_APP, WIDE_LINE, NUL_HANDLE, DENIED]
        .into_iter()
        .chain(WRITE_ENDS)
        .chain(READ_ENDS)
        .chain(EVENTS)
    {
        r#gen.emit(&format!("\tmov\t{}, rax", slot(offset)));
    }
    r#gen.emit(&format!("\tmov\t{}, {}", slot(LAST), WINDOWS_NOT_FOUND[0]));

    arguments(r#gen, label);
    pipes(r#gen, label);
    search(r#gen, label);

    // the program runs: only it keeps the ends it was given
    r#gen.emit(&format!("{label}_started:"));
    for offset in [NUL_HANDLE, WRITE_ENDS[0], WRITE_ENDS[1]] {
        close_slot(r#gen, offset, "started");
    }
    r#gen.emit(&format!("\tmov\trcx, {}", slot(PROCESS_INFO - 8)));
    r#gen.emit("\tcall\tCloseHandle");
    free_slot(r#gen, WIDE_APP, "started");
    free_slot(r#gen, WIDE_LINE, "started");
    drain(r#gen, label);

    // the status is the exit code, a 32-bit number
    r#gen.emit(&format!("{label}_ended:"));
    wait_for_program(r#gen);
    r#gen.emit(&format!("\tmovsxd\tr15, {}", dword(STATUS)));
    cleanup(r#gen, "ended");
    for buffer in BUFFERS {
        r#gen.emit(&format!("\tmov\trdi, {}", slot(buffer)));
        r#gen.emit(&format!("\tmov\trsi, {}", slot(buffer - 8)));
        r#gen.emit("\tcall\t.Lfs_utf8");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjz\t{label}_invalid"));
    }
    for (buffer, reg) in [(BUFFERS[0], "r12"), (BUFFERS[1], "r13")] {
        r#gen.emit(&format!("\tmov\trdi, {}", slot(buffer)));
        r#gen.emit(&format!("\tmov\trsi, {}", slot(buffer - 8)));
        r#gen.emit("\tcall\tstone.str_slice");
        r#gen.emit(&format!("\tmov\t{reg}, rax"));
    }
    free_buffers(r#gen);
    r#gen.emit("\txor\tedi, edi"); // ok
    r#gen.emit(&format!("\tmov\tesi, {OUTPUT_KINDS}"));
    r#gen.emit("\tmov\tedx, 3");
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tQWORD PTR [rax + 16], r15");
    r#gen.emit("\tmov\tQWORD PTR [rax + 24], r12");
    r#gen.emit("\tmov\tQWORD PTR [rax + 32], r13");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    r#gen.emit(&format!("{label}_invalid:"));
    free_buffers(r#gen);
    r#gen.emit(&format!("\tmov\trdi, {INVALID_TEXT_CODE}"));
    r#gen.emit(&format!("\tjmp\t{label}_err"));

    // a failure before the program ran, with Linux's number for it in r15
    r#gen.emit(&format!("{label}_last_error:"));
    r#gen.emit("\tcall\t.Lwin_errno");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit(&format!("{label}_fail:"));
    cleanup(r#gen, "fail");
    r#gen.emit("\tmov\trdi, r15");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
    routine_end(r#gen, label);
}

/// Emits code that checks every argument for a null character, failing with
/// [`NULL_IN_COMMAND_CODE`], and writes the arguments' part of the command line into
/// [`ARGS_TEXT`], as `stdlib::os::windows_command_line` writes it.
fn arguments(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    // r13 counts the arguments and r14 the bytes the text may need: each byte at most twice,
    // plus a space and two quotes
    r#gen.emit("\txor\tr13, r13");
    r#gen.emit("\tmov\tr14, 1");
    r#gen.emit(&format!("{label}_arg:"));
    r#gen.emit("\tcmp\tr13, QWORD PTR [r12]");
    r#gen.emit(&format!("\tjae\t{label}_args_checked"));
    r#gen.emit("\tmov\trdx, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rdx + r13 * 8]");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi - 16]");
    r#gen.emit("\tlea\tr14, [r14 + rax * 2 + 3]");
    r#gen.emit("\tcall\t.Lfs_check_path");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_arg"));
    r#gen.emit(&format!("\tmov\tr15, {NULL_IN_COMMAND_CODE}"));
    r#gen.emit(&format!("\tjmp\t{label}_fail"));

    r#gen.emit(&format!("{label}_args_checked:"));
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit(&format!("\tmov\t{}, rax", slot(ARGS_TEXT)));
    // r13 counts the arguments, r14 is where the text goes on, r15 walks an argument, rsi is its
    // end, rdi counts the backslashes in a row, and rcx whether it is quoted
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\txor\tr13, r13");
    r#gen.emit(&format!("{label}_quote:"));
    r#gen.emit("\tcmp\tr13, QWORD PTR [r12]");
    r#gen.emit(&format!("\tjae\t{label}_quoted"));
    r#gen.emit("\tmov\trdx, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\tr15, QWORD PTR [rdx + r13 * 8]");
    r#gen.emit("\tinc\tr13");
    r#gen.emit("\tmov\trsi, r15");
    r#gen.emit("\tadd\trsi, QWORD PTR [r15 - 16]");
    r#gen.emit("\tmov\tbyte ptr [r14], 32"); // ' '
    r#gen.emit("\tinc\tr14");
    // an empty argument, or one with a space or tab, is quoted
    r#gen.emit("\tmov\tecx, 1");
    r#gen.emit("\tcmp\tr15, rsi");
    r#gen.emit(&format!("\tje\t{label}_open"));
    r#gen.emit("\tmov\trdx, r15");
    r#gen.emit(&format!("{label}_blank:"));
    r#gen.emit("\tmovzx\teax, byte ptr [rdx]");
    r#gen.emit("\tcmp\teax, 32");
    r#gen.emit(&format!("\tje\t{label}_open"));
    r#gen.emit("\tcmp\teax, 9");
    r#gen.emit(&format!("\tje\t{label}_open"));
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tcmp\trdx, rsi");
    r#gen.emit(&format!("\tjb\t{label}_blank"));
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(&format!("\tjmp\t{label}_unopened"));
    r#gen.emit(&format!("{label}_open:"));
    r#gen.emit("\tmov\tbyte ptr [r14], 34"); // '"'
    r#gen.emit("\tinc\tr14");
    r#gen.emit(&format!("{label}_unopened:"));
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit(&format!("{label}_char:"));
    r#gen.emit("\tcmp\tr15, rsi");
    r#gen.emit(&format!("\tjae\t{label}_close"));
    r#gen.emit("\tmovzx\teax, byte ptr [r15]");
    r#gen.emit("\tinc\tr15");
    r#gen.emit("\tcmp\teax, 92"); // '\\'
    r#gen.emit(&format!("\tjne\t{label}_not_backslash"));
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("\tjmp\t{label}_put"));
    r#gen.emit(&format!("{label}_not_backslash:"));
    r#gen.emit("\tcmp\teax, 34");
    r#gen.emit(&format!("\tjne\t{label}_plain"));
    // a quote doubles the backslashes before it and gets one of its own
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("{label}_escape:"));
    r#gen.emit("\tmov\tbyte ptr [r14], 92");
    r#gen.emit("\tinc\tr14");
    r#gen.emit("\tdec\trdi");
    r#gen.emit(&format!("\tjnz\t{label}_escape"));
    r#gen.emit(&format!("{label}_plain:"));
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit(&format!("{label}_put:"));
    r#gen.emit("\tmov\tbyte ptr [r14], al");
    r#gen.emit("\tinc\tr14");
    r#gen.emit(&format!("\tjmp\t{label}_char"));
    // a quoted argument doubles the backslashes that end it, then closes
    r#gen.emit(&format!("{label}_close:"));
    r#gen.emit("\ttest\tecx, ecx");
    r#gen.emit(&format!("\tjz\t{label}_quote"));
    r#gen.emit(&format!("{label}_trailing:"));
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit(&format!("\tjz\t{label}_closed"));
    r#gen.emit("\tmov\tbyte ptr [r14], 92");
    r#gen.emit("\tinc\tr14");
    r#gen.emit("\tdec\trdi");
    r#gen.emit(&format!("\tjmp\t{label}_trailing"));
    r#gen.emit(&format!("{label}_closed:"));
    r#gen.emit("\tmov\tbyte ptr [r14], 34");
    r#gen.emit("\tinc\tr14");
    r#gen.emit(&format!("\tjmp\t{label}_quote"));
    r#gen.emit(&format!("{label}_quoted:"));
    r#gen.emit("\tmov\tbyte ptr [r14], 0");
    r#gen.emit(&format!("\tsub\tr14, {}", slot(ARGS_TEXT)));
    r#gen.emit(&format!("\tmov\t{}, r14", slot(ARGS_LEN)));
}

/// Emits code that opens what the program is given: `NUL` for its stdin and the writing end of a
/// pipe for each of its stdout and stderr, all inheritable, keeping each pipe's reading end and an
/// event for reading it.
fn pipes(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    // SECURITY_ATTRIBUTES { nLength, lpSecurityDescriptor, bInheritHandle }
    r#gen.emit(&format!("\tmov\t{}, 24", slot(SECURITY)));
    r#gen.emit(&format!("\tmov\t{}, 0", slot(SECURITY - 8)));
    r#gen.emit(&format!("\tmov\t{}, 1", slot(SECURITY - 16)));
    r#gen.emit("\tlea\trcx, [rip + .Los_run_nul]");
    r#gen.emit("\tmov\tedx, 0x80000000"); // GENERIC_READ
    r#gen.emit("\tmov\tr8d, 7"); // share everything
    create_file(r#gen, label, NUL_HANDLE);
    for i in 0..2 {
        // the name, the prefix then 16 hexadecimal digits of (process ID << 32) + count
        for quad in 0..4 {
            r#gen.emit(&format!(
                "\tmov\trax, QWORD PTR [rip + .Los_run_pipe + {}]",
                8 * quad
            ));
            r#gen.emit(&format!("\tmov\t{}, rax", slot(NAME - 8 * quad)));
        }
        r#gen.emit("\tcall\tGetCurrentProcessId");
        r#gen.emit("\tmov\tr13d, eax");
        r#gen.emit("\tshl\tr13, 32");
        r#gen.emit("\tmov\trax, QWORD PTR [rip + stone.os_run_pipes]");
        r#gen.emit("\tinc\trax");
        r#gen.emit("\tmov\tQWORD PTR [rip + stone.os_run_pipes], rax");
        r#gen.emit("\tor\tr13, rax");
        let digits = PIPE_PREFIX.len() as i64;
        r#gen.emit(&format!("\tlea\trdi, [rbp - {NAME}]"));
        r#gen.emit(&format!("\tmov\tword ptr [rdi + {}], 0", 2 * (digits + 16)));
        r#gen.emit(&format!("\tmov\tecx, {}", digits + 15));
        r#gen.emit(&format!("{label}_hex{i}:"));
        r#gen.emit("\tmov\teax, r13d");
        r#gen.emit("\tand\teax, 15");
        r#gen.emit("\tcmp\teax, 10");
        r#gen.emit(&format!("\tjb\t{label}_digit{i}"));
        r#gen.emit("\tadd\teax, 39"); // 'a' - '0' - 10
        r#gen.emit(&format!("{label}_digit{i}:"));
        r#gen.emit("\tadd\teax, 48"); // '0'
        r#gen.emit("\tmov\tword ptr [rdi + rcx * 2], ax");
        r#gen.emit("\tshr\tr13, 4");
        r#gen.emit("\tdec\tecx");
        r#gen.emit(&format!("\tcmp\tecx, {digits}"));
        r#gen.emit(&format!("\tjge\t{label}_hex{i}"));

        // the reading end: inbound, overlapped, the first of its name, local clients only
        r#gen.emit(&format!("\tlea\trcx, [rbp - {NAME}]"));
        r#gen.emit("\tmov\tedx, 0x40080001");
        r#gen.emit("\tmov\tr8d, 8"); // PIPE_REJECT_REMOTE_CLIENTS
        r#gen.emit("\tmov\tr9d, 1");
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + 32], {PIPE_CAPACITY}"));
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + 40], {PIPE_CAPACITY}"));
        r#gen.emit("\tmov\tQWORD PTR [rsp + 48], 0");
        r#gen.emit("\tmov\tQWORD PTR [rsp + 56], 0");
        r#gen.emit("\tcall\tCreateNamedPipeW");
        r#gen.emit("\tcmp\trax, -1");
        r#gen.emit(&format!("\tje\t{label}_last_error"));
        r#gen.emit(&format!("\tmov\t{}, rax", slot(READ_ENDS[i])));
        // the writing end, which the program inherits
        r#gen.emit(&format!("\tlea\trcx, [rbp - {NAME}]"));
        r#gen.emit("\tmov\tedx, 0x40000000"); // GENERIC_WRITE
        r#gen.emit("\txor\tr8d, r8d");
        create_file(r#gen, label, WRITE_ENDS[i]);
        // a manual-reset event, not set, which each read's end sets
        r#gen.emit("\txor\tecx, ecx");
        r#gen.emit("\tmov\tedx, 1");
        r#gen.emit("\txor\tr8d, r8d");
        r#gen.emit("\txor\tr9d, r9d");
        r#gen.emit("\tcall\tCreateEventW");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjz\t{label}_last_error"));
        r#gen.emit(&format!("\tmov\t{}, rax", slot(EVENTS[i])));
    }
}

/// Emits a `CreateFileW` of the name `rcx` with the access `edx` and sharing `r8d`, inheritable,
/// of something that exists, into the slot `offset`, or a jump to `<label>_last_error`.
fn create_file(r#gen: &mut dyn AssemblyGenerator, label: &str, offset: i64) {
    r#gen.emit(&format!("\tlea\tr9, [rbp - {SECURITY}]"));
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], 3"); // OPEN_EXISTING
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], 0");
    r#gen.emit("\tcall\tCreateFileW");
    r#gen.emit("\tcmp\trax, -1");
    r#gen.emit(&format!("\tje\t{label}_last_error"));
    r#gen.emit(&format!("\tmov\t{}, rax", slot(offset)));
}

/// Emits the search for the program: each path `stdlib::os::windows_candidates` lists, written
/// into [`PATH`] and tried by `try_path`, until one starts, which goes on at
/// `<label>_started`.
fn search(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    // r13 is the command's length and r14 whether it needs .exe: its file name, after the last
    // separator, has no dot past its first byte
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx - 16]");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit(&format!("\tjz\t{label}_exhausted"));
    r#gen.emit(&format!("\tmov\t{}, 0", slot(DIRECT)));
    r#gen.emit("\tmov\tr14, 1");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit(&format!("{label}_name:"));
    r#gen.emit("\tdec\trcx");
    r#gen.emit(&format!("\tjs\t{label}_named"));
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rcx]");
    r#gen.emit("\tcmp\teax, 92");
    r#gen.emit(&format!("\tje\t{label}_separator"));
    r#gen.emit("\tcmp\teax, 47"); // '/'
    r#gen.emit(&format!("\tje\t{label}_separator"));
    r#gen.emit("\tcmp\teax, 46"); // '.'
    r#gen.emit(&format!("\tjne\t{label}_name"));
    // a dot that does not start the file name is an extension
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjz\t{label}_name"));
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rcx - 1]");
    r#gen.emit("\tcmp\teax, 92");
    r#gen.emit(&format!("\tje\t{label}_name"));
    r#gen.emit("\tcmp\teax, 47");
    r#gen.emit(&format!("\tje\t{label}_name"));
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit(&format!("\tjmp\t{label}_name"));
    // a separator ends the file name, and makes the command the only path
    r#gen.emit(&format!("{label}_separator:"));
    r#gen.emit(&format!("\tmov\t{}, 1", slot(DIRECT)));
    r#gen.emit(&format!("{label}_named:"));

    // the buffer holds a directory, a \, the command, .exe, and a terminator
    r#gen.emit(&format!("\tcmp\t{}, 0", slot(DIRECT)));
    r#gen.emit(&format!("\tjne\t{label}_sized"));
    r#gen.emit("\tlea\trdi, [rip + .Los_run_path_name]");
    r#gen.emit("\tcall\tstone.getenv");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_exhausted"));
    r#gen.emit(&format!("\tmov\t{}, rax", slot(NEXT_DIR)));
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tcall\tstone.cstr_len");
    r#gen.emit("\tadd\tr13, rax");
    r#gen.emit(&format!("{label}_sized:"));
    r#gen.emit("\tlea\trdi, [r13 + 8]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit(&format!("\tmov\t{}, rax", slot(PATH)));
    r#gen.emit(&format!("\tcmp\t{}, 0", slot(DIRECT)));
    r#gen.emit(&format!("\tje\t{label}_entry"));
    // the command is the only path: rdi is where the file name goes
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit(&format!("\tmov\t{}, 0", slot(MORE)));
    r#gen.emit(&format!("\tjmp\t{label}_file"));

    // rsi walks the next directory of PATH and rdi writes it, its quotes left out
    r#gen.emit(&format!("{label}_entry:"));
    r#gen.emit(&format!("\tmov\trsi, {}", slot(NEXT_DIR)));
    r#gen.emit(&format!("\tmov\trdi, {}", slot(PATH)));
    r#gen.emit(&format!("{label}_dir:"));
    r#gen.emit("\tmovzx\teax, byte ptr [rsi]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit(&format!("\tjz\t{label}_dir_end"));
    r#gen.emit("\tinc\trsi");
    r#gen.emit("\tcmp\teax, 59"); // ';'
    r#gen.emit(&format!("\tje\t{label}_dir_end"));
    r#gen.emit("\tcmp\teax, 34");
    r#gen.emit(&format!("\tje\t{label}_dir"));
    r#gen.emit("\tmov\tbyte ptr [rdi], al");
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("\tjmp\t{label}_dir"));
    r#gen.emit(&format!("{label}_dir_end:"));
    r#gen.emit(&format!("\tmov\t{}, rax", slot(MORE)));
    r#gen.emit(&format!("\tmov\t{}, rsi", slot(NEXT_DIR)));
    // an empty directory is skipped, and one that does not end in a separator gets a \
    r#gen.emit(&format!("\tcmp\trdi, {}", slot(PATH)));
    r#gen.emit(&format!("\tje\t{label}_next"));
    r#gen.emit("\tmovzx\teax, byte ptr [rdi - 1]");
    r#gen.emit("\tcmp\teax, 92");
    r#gen.emit(&format!("\tje\t{label}_file"));
    r#gen.emit("\tcmp\teax, 47");
    r#gen.emit(&format!("\tje\t{label}_file"));
    r#gen.emit("\tmov\tbyte ptr [rdi], 92");
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("{label}_file:"));
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx - 16]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit(&format!("\tjz\t{label}_terminated"));
    r#gen.emit("\tmov\tDWORD PTR [rdi], 0x6578652e"); // ".exe"
    r#gen.emit("\tadd\trdi, 4");
    r#gen.emit(&format!("{label}_terminated:"));
    r#gen.emit("\tmov\tbyte ptr [rdi], 0");
    r#gen.emit(&format!("\tsub\trdi, {}", slot(PATH)));
    r#gen.emit("\tmov\tr15, rdi");
    try_path(r#gen, label);

    // r15 holds the error code of the path tried: one that names nothing moves on, and so does
    // one that may not be run, remembered, while any other stops
    r#gen.emit(&format!("{label}_code:"));
    for code in WINDOWS_NOT_FOUND {
        r#gen.emit(&format!("\tcmp\tr15d, {code}"));
        r#gen.emit(&format!("\tje\t{label}_missing"));
    }
    r#gen.emit(&format!("\tcmp\tr15d, {WINDOWS_DENIED}"));
    r#gen.emit(&format!("\tjne\t{label}_stop"));
    r#gen.emit(&format!("\tmov\t{}, 1", slot(DENIED)));
    r#gen.emit(&format!("{label}_missing:"));
    r#gen.emit(&format!("\tmov\t{}, r15", slot(LAST)));
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit(&format!("\tcmp\t{}, 0", slot(MORE)));
    r#gen.emit(&format!("\tjne\t{label}_entry"));
    r#gen.emit(&format!("{label}_exhausted:"));
    r#gen.emit(&format!("\tmov\tr15, {}", slot(LAST)));
    r#gen.emit(&format!("\tcmp\t{}, 0", slot(DENIED)));
    r#gen.emit(&format!("\tje\t{label}_stop"));
    r#gen.emit(&format!("\tmov\tr15, {WINDOWS_DENIED}"));
    r#gen.emit(&format!("{label}_stop:"));
    r#gen.emit("\tmov\tecx, r15d");
    r#gen.emit("\tcall\t.Lwin_map");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit(&format!("\tjmp\t{label}_fail"));
}

/// Emits the try of the path in [`PATH`], `r15` bytes long, which jumps to `<label>_started` if
/// the program starts, and otherwise goes on with its error code in `r15`.
fn try_path(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("\tmov\trcx, {}", slot(PATH)));
    r#gen.emit("\tmov\trdx, r15");
    r#gen.emit("\tcall\t.Los_run_wide");
    r#gen.emit(&format!("\tmov\t{}, rax", slot(WIDE_APP)));
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\tcall\tGetFileAttributesW");
    r#gen.emit("\tcmp\teax, -1"); // INVALID_FILE_ATTRIBUTES
    r#gen.emit(&format!("\tjne\t{label}_exists"));
    r#gen.emit(&format!("{label}_not_started:"));
    r#gen.emit("\tcall\tGetLastError");
    r#gen.emit("\tmov\tr15d, eax");
    r#gen.emit(&format!("{label}_tried:"));
    free_slot(r#gen, WIDE_APP, "tried");
    free_slot(r#gen, WIDE_LINE, "tried");
    r#gen.emit(&format!("\tjmp\t{label}_code"));

    // a batch file, ending in .bat or .cmd in any case, is never started itself
    r#gen.emit(&format!("{label}_exists:"));
    r#gen.emit("\tcmp\tr15, 4");
    r#gen.emit(&format!("\tjb\t{label}_not_batch"));
    r#gen.emit(&format!("\tmov\trax, {}", slot(PATH)));
    r#gen.emit("\tmov\teax, DWORD PTR [rax + r15 - 4]");
    r#gen.emit("\tor\teax, 0x20202000"); // ASCII letters in lowercase, the dot kept
    r#gen.emit("\tcmp\teax, 0x7461622e"); // ".bat"
    r#gen.emit(&format!("\tje\t{label}_batch"));
    r#gen.emit("\tcmp\teax, 0x646d632e"); // ".cmd"
    r#gen.emit(&format!("\tjne\t{label}_not_batch"));
    r#gen.emit(&format!("{label}_batch:"));
    r#gen.emit(&format!("\tmov\tr15d, {WINDOWS_BAD_FORMAT}"));
    r#gen.emit(&format!("\tjmp\t{label}_tried"));

    // the command line: the path in quotes, then the arguments
    r#gen.emit(&format!("{label}_not_batch:"));
    r#gen.emit(&format!("\tmov\trdi, {}", slot(ARGS_LEN)));
    r#gen.emit("\tlea\trdi, [rdi + r15 + 3]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\tbyte ptr [rdi], 34");
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("\tmov\trsi, {}", slot(PATH)));
    r#gen.emit("\tmov\trcx, r15");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tbyte ptr [rdi], 34");
    r#gen.emit("\tinc\trdi");
    r#gen.emit(&format!("\tmov\trsi, {}", slot(ARGS_TEXT)));
    r#gen.emit(&format!("\tmov\trcx, {}", slot(ARGS_LEN)));
    r#gen.emit("\tinc\trcx"); // and its terminator
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tlea\trdx, [rdi - 1]");
    r#gen.emit("\tsub\trdx, r13");
    r#gen.emit("\tcall\t.Los_run_wide");
    r#gen.emit(&format!("\tmov\t{}, rax", slot(WIDE_LINE)));
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tcall\tstone.mem_free");

    // STARTUPINFOW: its size, the standard handles, and nothing else
    r#gen.emit("\txor\teax, eax");
    for field in (0..104).step_by(8) {
        r#gen.emit(&format!("\tmov\t{}, rax", slot(STARTUP_INFO - field)));
    }
    r#gen.emit(&format!("\tmov\t{}, 104", dword(STARTUP_INFO)));
    r#gen.emit(&format!("\tmov\t{}, 0x100", dword(STARTUP_INFO - 60))); // STARTF_USESTDHANDLES
    for (field, handle) in [(80, NUL_HANDLE), (88, WRITE_ENDS[0]), (96, WRITE_ENDS[1])] {
        r#gen.emit(&format!("\tmov\trax, {}", slot(handle)));
        r#gen.emit(&format!("\tmov\t{}, rax", slot(STARTUP_INFO - field)));
    }
    r#gen.emit(&format!("\tmov\trcx, {}", slot(WIDE_APP)));
    r#gen.emit(&format!("\tmov\trdx, {}", slot(WIDE_LINE)));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], 1"); // inherit the handles given
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 0x400"); // CREATE_UNICODE_ENVIRONMENT
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 56], 0");
    r#gen.emit(&format!("\tlea\trax, [rbp - {STARTUP_INFO}]"));
    r#gen.emit("\tmov\tQWORD PTR [rsp + 64], rax");
    r#gen.emit(&format!("\tlea\trax, [rbp - {PROCESS_INFO}]"));
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], rax");
    r#gen.emit("\tcall\tCreateProcessW");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit(&format!("\tjz\t{label}_not_started"));
    r#gen.emit(&format!("\tjmp\t{label}_started"));
}

/// Emits the reading of both pipes until both end: each pipe not being read starts an
/// overlapped read into the end of its buffer, doubled first if full, then the routine waits for
/// any read under way, and takes what it gave. A pipe ends when its writer has closed.
fn drain(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    for (i, buffer) in BUFFERS.into_iter().enumerate() {
        r#gen.emit(&format!("\tmov\t{}, 0", slot(STATES[i])));
        r#gen.emit(&format!("\tmov\trdi, {FILE_BUFFER}"));
        r#gen.emit("\tcall\tstone.mem_alloc");
        r#gen.emit(&format!("\tmov\t{}, rax", slot(buffer)));
        r#gen.emit(&format!("\tmov\t{}, 0", slot(buffer - 8)));
        r#gen.emit(&format!("\tmov\t{}, {FILE_BUFFER}", slot(buffer - 16)));
    }
    r#gen.emit(&format!("{label}_read:"));
    for (i, buffer) in BUFFERS.into_iter().enumerate() {
        let next = format!("{label}_reading{i}");
        r#gen.emit(&format!("\tcmp\t{}, 0", slot(STATES[i])));
        r#gen.emit(&format!("\tjne\t{next}"));
        r#gen.emit(&format!("\tmov\trax, {}", slot(buffer - 8)));
        r#gen.emit(&format!("\tcmp\trax, {}", slot(buffer - 16)));
        r#gen.emit(&format!("\tjne\t{label}_room{i}"));
        r#gen.emit(&format!("\tmov\trsi, {}", slot(buffer - 16)));
        r#gen.emit("\tadd\trsi, rsi");
        r#gen.emit(&format!("\tmov\t{}, rsi", slot(buffer - 16)));
        r#gen.emit(&format!("\tmov\trdi, {}", slot(buffer)));
        r#gen.emit("\tcall\tstone.mem_realloc");
        r#gen.emit(&format!("\tmov\t{}, rax", slot(buffer)));
        r#gen.emit(&format!("{label}_room{i}:"));
        // a fresh OVERLAPPED with the pipe's event
        let overlapped = OVERLAPPED[i];
        r#gen.emit("\txor\teax, eax");
        for field in [0, 8, 16] {
            r#gen.emit(&format!("\tmov\t{}, rax", slot(overlapped - field)));
        }
        r#gen.emit(&format!("\tmov\trax, {}", slot(EVENTS[i])));
        r#gen.emit(&format!("\tmov\t{}, rax", slot(overlapped - 24)));
        r#gen.emit(&format!("\tmov\trcx, {}", slot(READ_ENDS[i])));
        r#gen.emit(&format!("\tmov\trdx, {}", slot(buffer)));
        r#gen.emit(&format!("\tadd\trdx, {}", slot(buffer - 8)));
        r#gen.emit(&format!("\tmov\tr8, {}", slot(buffer - 16)));
        r#gen.emit(&format!("\tsub\tr8, {}", slot(buffer - 8)));
        r#gen.emit("\txor\tr9d, r9d");
        r#gen.emit(&format!("\tlea\trax, [rbp - {overlapped}]"));
        r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
        r#gen.emit("\tcall\tReadFile");
        // a read that finished at once has set the event too, so both wait
        r#gen.emit(&format!("\tmov\t{}, 1", slot(STATES[i])));
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit(&format!("\tjnz\t{next}"));
        r#gen.emit("\tcall\tGetLastError");
        r#gen.emit(&format!("\tcmp\teax, {ERROR_IO_PENDING}"));
        r#gen.emit(&format!("\tje\t{next}"));
        r#gen.emit(&format!("\tmov\t{}, 2", slot(STATES[i])));
        r#gen.emit(&format!("\tcmp\teax, {ERROR_BROKEN_PIPE}"));
        r#gen.emit(&format!("\tjne\t{label}_read_failed"));
        r#gen.emit(&format!("{next}:"));
    }
    // r13 counts the reads under way, whose events and pipes are listed in turn
    r#gen.emit("\txor\tr13d, r13d");
    for i in 0..2 {
        let next = format!("{label}_listed{i}");
        r#gen.emit(&format!("\tcmp\t{}, 1", slot(STATES[i])));
        r#gen.emit(&format!("\tjne\t{next}"));
        r#gen.emit(&format!("\tmov\trax, {}", slot(EVENTS[i])));
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp + r13 * 8 - {WAITS}], rax"));
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp + r13 * 8 - {WAITED}], {i}"));
        r#gen.emit("\tinc\tr13d");
        r#gen.emit(&format!("{next}:"));
    }
    r#gen.emit("\ttest\tr13d, r13d");
    r#gen.emit(&format!("\tjz\t{label}_ended"));
    r#gen.emit("\tmov\tecx, r13d");
    r#gen.emit(&format!("\tlea\trdx, [rbp - {WAITS}]"));
    r#gen.emit("\txor\tr8d, r8d"); // any of them
    r#gen.emit("\tmov\tr9d, -1"); // INFINITE
    r#gen.emit("\tcall\tWaitForMultipleObjects");
    r#gen.emit("\tcmp\teax, r13d");
    r#gen.emit(&format!("\tjae\t{label}_wait_failed"));
    r#gen.emit("\tmov\teax, eax");
    r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbp + rax * 8 - {WAITED}]"));
    for i in 0..2 {
        let next = format!("{label}_taken{i}");
        r#gen.emit(&format!("\tcmp\trax, {i}"));
        r#gen.emit(&format!("\tjne\t{next}"));
        r#gen.emit(&format!("\tmov\trcx, {}", slot(READ_ENDS[i])));
        r#gen.emit(&format!("\tlea\trdx, [rbp - {}]", OVERLAPPED[i]));
        r#gen.emit(&format!("\tlea\tr8, [rbp - {GOT}]"));
        r#gen.emit("\txor\tr9d, r9d");
        r#gen.emit("\tcall\tGetOverlappedResult");
        r#gen.emit(&format!("\tmov\t{}, 0", slot(STATES[i])));
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit(&format!("\tjz\t{label}_unread{i}"));
        r#gen.emit(&format!("\tmov\teax, {}", dword(GOT)));
        r#gen.emit(&format!("\tadd\t{}, rax", slot(BUFFERS[i] - 8)));
        r#gen.emit(&format!("\tjmp\t{label}_read"));
        r#gen.emit(&format!("{label}_unread{i}:"));
        r#gen.emit(&format!("\tmov\t{}, 2", slot(STATES[i])));
        r#gen.emit("\tcall\tGetLastError");
        r#gen.emit(&format!("\tcmp\teax, {ERROR_BROKEN_PIPE}"));
        r#gen.emit(&format!("\tje\t{label}_read"));
        r#gen.emit(&format!("\tjmp\t{label}_read_failed"));
        r#gen.emit(&format!("{next}:"));
    }

    // a failure reading: reads under way are given up and waited for, then the program is
    r#gen.emit(&format!("{label}_wait_failed:"));
    r#gen.emit("\tcall\tGetLastError");
    r#gen.emit(&format!("{label}_read_failed:"));
    r#gen.emit("\tmov\tecx, eax");
    r#gen.emit("\tcall\t.Lwin_map");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr15, rax");
    for i in 0..2 {
        let next = format!("{label}_given_up{i}");
        r#gen.emit(&format!("\tcmp\t{}, 1", slot(STATES[i])));
        r#gen.emit(&format!("\tjne\t{next}"));
        r#gen.emit(&format!("\tmov\trcx, {}", slot(READ_ENDS[i])));
        r#gen.emit("\tcall\tCancelIo");
        r#gen.emit(&format!("\tmov\trcx, {}", slot(READ_ENDS[i])));
        r#gen.emit(&format!("\tlea\trdx, [rbp - {}]", OVERLAPPED[i]));
        r#gen.emit(&format!("\tlea\tr8, [rbp - {GOT}]"));
        r#gen.emit("\tmov\tr9d, 1"); // waiting
        r#gen.emit("\tcall\tGetOverlappedResult");
        r#gen.emit(&format!("{next}:"));
    }
    // closing the pipes first lets a program writing to them finish
    for offset in READ_ENDS {
        close_slot(r#gen, offset, "drain");
    }
    wait_for_program(r#gen);
    free_buffers(r#gen);
    r#gen.emit(&format!("\tjmp\t{label}_fail"));
}

/// Emits a wait for the program to end, which leaves its exit code in [`STATUS`] and closes it.
fn wait_for_program(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(&format!("\tmov\trcx, {}", slot(PROCESS_INFO)));
    r#gen.emit("\tmov\tedx, -1"); // INFINITE
    r#gen.emit("\tcall\tWaitForSingleObject");
    r#gen.emit(&format!("\tmov\t{}, 0", slot(STATUS)));
    r#gen.emit(&format!("\tmov\trcx, {}", slot(PROCESS_INFO)));
    r#gen.emit(&format!("\tlea\trdx, [rbp - {STATUS}]"));
    r#gen.emit("\tcall\tGetExitCodeProcess");
    r#gen.emit(&format!("\tmov\trcx, {}", slot(PROCESS_INFO)));
    r#gen.emit("\tcall\tCloseHandle");
}

/// Emits code that closes every handle the routine still holds and frees its buffers but the
/// outputs', keeping `r15`, with `tag` naming its labels.
fn cleanup(r#gen: &mut dyn AssemblyGenerator, tag: &str) {
    for offset in [NUL_HANDLE]
        .into_iter()
        .chain(WRITE_ENDS)
        .chain(READ_ENDS)
        .chain(EVENTS)
    {
        close_slot(r#gen, offset, tag);
    }
    for offset in [ARGS_TEXT, PATH, WIDE_APP, WIDE_LINE] {
        free_slot(r#gen, offset, tag);
    }
}

/// Emits a `CloseHandle` of the handle in the slot `offset` unless it is 0, which it then holds,
/// with `tag` telling its label from those of other places that close the slot.
fn close_slot(r#gen: &mut dyn AssemblyGenerator, offset: i64, tag: &str) {
    let skip = format!(".Los_run_closed_{tag}_{offset}");
    r#gen.emit(&format!("\tmov\trcx, {}", slot(offset)));
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjz\t{skip}"));
    r#gen.emit("\tcall\tCloseHandle");
    r#gen.emit(&format!("\tmov\t{}, 0", slot(offset)));
    r#gen.emit(&format!("{skip}:"));
}

/// Emits a `stone.mem_free` of the block in the slot `offset` unless it is 0, which it then
/// holds, with `tag` telling its label from those of other places that free the slot.
fn free_slot(r#gen: &mut dyn AssemblyGenerator, offset: i64, tag: &str) {
    let skip = format!(".Los_run_freed_{tag}_{offset}");
    r#gen.emit(&format!("\tmov\trax, {}", slot(offset)));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{skip}"));
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(&format!("\tmov\t{}, 0", slot(offset)));
    r#gen.emit(&format!("{skip}:"));
}

/// Emits a free of both output buffers.
fn free_buffers(r#gen: &mut dyn AssemblyGenerator) {
    for buffer in BUFFERS {
        r#gen.emit(&format!("\tmov\trax, {}", slot(buffer)));
        r#gen.emit("\tcall\tstone.mem_free");
    }
}

/// Emits `.Los_run_wide`, which returns in `rax` a block from `stone.mem_alloc` holding the
/// UTF-8 text `rcx`, `rdx` bytes long and followed by a terminator, in UTF-16 with its
/// terminator. It follows the Microsoft convention.
fn wide(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the text, r12 its length, and r13 the block
    r#gen.emit(".Los_run_wide:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tsub\trsp, 48");
    r#gen.emit("\tmov\trbx, rcx");
    r#gen.emit("\tlea\tr12, [rdx + 1]");
    r#gen.emit("\tlea\trdi, [r12 + r12]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit(&format!("\tmov\tecx, {CP_UTF8}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8, rbx");
    r#gen.emit("\tmov\tr9, r12");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r13");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], r12");
    r#gen.emit("\tcall\tMultiByteToWideChar");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tadd\trsp, 48");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}
