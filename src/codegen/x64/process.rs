//! The x86-64 runtime of `os.run`, which runs another program with Linux system calls, as
//! `stdlib::os::run` does through Rust's `std::process` for the interpreter.
//!
//! `stone.os_run` forks, and the child points its stdin at `/dev/null` and its stdout and stderr
//! at pipes, then tries each path `stdlib::os::candidates` lists with `execve`. A third pipe,
//! closed on a successful `execve`, carries the error number back when no path runs. The
//! parent reads both pipes with `poll` until they end, so a program that fills one while the
//! other waits never stalls, then waits for the child and returns a result built with
//! `stone.enum_new`, as the `fs` routines do: `ok` of an `output`, or `err` of a message from
//! `.Lfs_err`.

use super::fs::{EINTR, close, routine_end, routine_start};
use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{
    FILE_BUFFER, INVALID_TEXT_CODE, NULL_IN_COMMAND_CODE, OUTPUT_KINDS, WAIT_SIGNAL_MASK,
    immortal_string,
};
use crate::stdlib::os::DEFAULT_PATH;

/// `openat`'s directory for a path relative to the working directory.
const AT_FDCWD: i64 = -100;

/// The error numbers the search for a program treats specially, as `stdlib::os::run` does.
const ENOENT: i64 = 2;
const EACCES: i64 = 13;
const ENOTDIR: i64 = 20;

/// `pipe2`'s flag that closes a descriptor when `execve` succeeds.
const O_CLOEXEC: i64 = 0o2000000;

/// What a child that could not run any path exits with, as a shell does.
const NOT_RUN: i64 = 127;

// where the routine keeps its state, below the registers `routine_start` saves
const ARGV: i64 = -56;
const PID: i64 = -64;
/// Each pipe is two 32-bit descriptors, the end to read first.
const OUT_PIPE: i64 = -72;
const ERR_PIPE: i64 = -80;
const EXEC_PIPE: i64 = -88;
/// Two `pollfd`s of 8 bytes: the descriptor, then the events asked for and those that happened,
/// 16 bits each.
const POLL: i64 = -104;
/// The output read from each pipe: a buffer from `stone.mem_alloc`, the bytes it holds, and its
/// size.
const STDOUT: i64 = -128;
const STDERR: i64 = -152;
const STATUS: i64 = -160;
/// The child's buffer for each path it tries.
const PATH: i64 = -168;
/// The child's next byte of `PATH`, 0 once the last directory is tried.
const MORE: i64 = -176;
const FRAME: i64 = 128;

/// Emits `stone.os_run`, which runs the program named by the string `rdi` with the strings of the
/// list `rsi` as its arguments and returns a result of an `output`, both borrowed, following
/// `stdlib::os::run`.
///
/// It needs the string, list, and enum runtimes, `stone.getenv`, and the `fs` routines'
/// `.Lfs_err`, `.Lfs_check_path`, and `.Lfs_utf8`.
pub fn run_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.os_run";
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Los_run_null", "/dev/null");
    immortal_string(r#gen, ".Los_run_path_name", "PATH");
    immortal_string(r#gen, ".Los_run_default_path", DEFAULT_PATH);
    r#gen.emit("\t.text");

    drain(r#gen);

    // rbx holds the command, r12 the arguments, and r13 and r14 counts
    routine_start(r#gen, label, NULL_IN_COMMAND_CODE);
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit(&format!("\tsub\trsp, {FRAME}"));
    // every descriptor starts closed, so cleaning up can close them all
    for pipe in [OUT_PIPE, ERR_PIPE, EXEC_PIPE] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], -1", -pipe));
    }

    // argv is the command, each argument, then null, and no argument may hold a null character
    r#gen.emit("\tmov\tr13, QWORD PTR [r12]");
    r#gen.emit("\tlea\trdi, [r13 + 2]");
    r#gen.emit("\tshl\trdi, 3");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", -ARGV));
    r#gen.emit("\tmov\tQWORD PTR [rax], rbx");
    r#gen.emit("\tmov\tQWORD PTR [rax + r13 * 8 + 8], 0");
    r#gen.emit("\txor\tr14, r14");
    r#gen.emit(&format!("{label}_arg:"));
    r#gen.emit("\tcmp\tr14, r13");
    r#gen.emit(&format!("\tjae\t{label}_pipes"));
    r#gen.emit("\tmov\trdx, QWORD PTR [r12 + 16]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rdx + r14 * 8]");
    r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", -ARGV));
    r#gen.emit("\tmov\tQWORD PTR [rax + r14 * 8 + 8], rdi");
    r#gen.emit("\tinc\tr14");
    r#gen.emit("\tcall\t.Lfs_check_path");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_arg"));
    r#gen.emit(&format!("\tmov\tr15, {NULL_IN_COMMAND_CODE}"));
    r#gen.emit(&format!("\tjmp\t{label}_fail"));

    // the output pipes stay open across execve, and the error pipe closes there
    r#gen.emit(&format!("{label}_pipes:"));
    for (pipe, flags) in [(OUT_PIPE, 0), (ERR_PIPE, 0), (EXEC_PIPE, O_CLOEXEC)] {
        r#gen.emit(&format!("\tlea\trdi, [rbp - {}]", -pipe));
        r#gen.emit(&format!("\tmov\tesi, {flags}"));
        r#gen.emit("\tmov\teax, 293"); // sys_pipe2
        r#gen.emit("\tsyscall");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjs\t{label}_errno"));
    }
    r#gen.emit("\tmov\teax, 57"); // sys_fork
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_errno"));
    r#gen.emit(&format!("\tjz\t{label}_child"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", -PID));

    // the parent keeps only the ends it reads
    for end in [OUT_PIPE + 4, ERR_PIPE + 4, EXEC_PIPE + 4] {
        close_slot(r#gen, end);
    }
    // the error pipe ends with no bytes once execve succeeds, or holds the child's error number
    r#gen.emit(&format!("{label}_exec_read:"));
    r#gen.emit(&format!("\tmov\tedi, DWORD PTR [rbp - {}]", -EXEC_PIPE));
    r#gen.emit(&format!("\tlea\trsi, [rbp - {}]", -STATUS));
    r#gen.emit("\tmov\tedx, 4");
    r#gen.emit("\txor\teax, eax"); // sys_read
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_exec_read"));
    r#gen.emit("\tcmp\trax, 4");
    r#gen.emit(&format!("\tjne\t{label}_started"));
    r#gen.emit(&format!("\tmov\tr15d, DWORD PTR [rbp - {}]", -STATUS));
    reap(r#gen, &format!("{label}_exec_failed"));
    r#gen.emit(&format!("\tjmp\t{label}_fail"));

    // read both pipes as they fill, a pipe whose descriptor is -1 having ended
    r#gen.emit(&format!("{label}_started:"));
    for (pipe, poll, buffer) in [(OUT_PIPE, POLL, STDOUT), (ERR_PIPE, POLL + 8, STDERR)] {
        r#gen.emit(&format!("\tmov\teax, DWORD PTR [rbp - {}]", -pipe));
        r#gen.emit(&format!("\tmov\tDWORD PTR [rbp - {}], eax", -poll));
        r#gen.emit(&format!("\tmov\tDWORD PTR [rbp - {}], 1", -(poll + 4))); // POLLIN
        r#gen.emit(&format!("\tmov\trdi, {FILE_BUFFER}"));
        r#gen.emit("\tcall\tstone.mem_alloc");
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", -buffer));
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], 0", -(buffer + 8)));
        r#gen.emit(&format!(
            "\tmov\tQWORD PTR [rbp - {}], {FILE_BUFFER}",
            -(buffer + 16)
        ));
    }
    r#gen.emit("\tmov\tr13, 2");
    r#gen.emit(&format!("{label}_poll:"));
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit(&format!("\tjz\t{label}_ended"));
    r#gen.emit(&format!("\tlea\trdi, [rbp - {}]", -POLL));
    r#gen.emit("\tmov\tesi, 2");
    r#gen.emit("\tmov\trdx, -1"); // no timeout
    r#gen.emit("\tmov\teax, 7"); // sys_poll
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_poll"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_poll_failed"));
    for (i, (poll, buffer)) in [(POLL, STDOUT), (POLL + 8, STDERR)].into_iter().enumerate() {
        let next = format!("{label}_polled{i}");
        r#gen.emit(&format!("\tmovzx\teax, word ptr [rbp - {}]", -(poll + 6)));
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit(&format!("\tjz\t{next}"));
        r#gen.emit(&format!("\tmov\tedi, DWORD PTR [rbp - {}]", -poll));
        r#gen.emit(&format!("\tlea\trsi, [rbp - {}]", -buffer));
        r#gen.emit("\tcall\t.Los_run_drain");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjg\t{next}"));
        // the end of the pipe, or an error reading it, which ends it too
        r#gen.emit(&format!("\tmov\tDWORD PTR [rbp - {}], -1", -poll));
        r#gen.emit("\tdec\tr13");
        r#gen.emit(&format!("{next}:"));
    }
    r#gen.emit(&format!("\tjmp\t{label}_poll"));
    r#gen.emit(&format!("{label}_poll_failed:"));
    // closing the pipes first lets a child writing to them finish
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr15, rax");
    close_pipes(r#gen);
    reap(r#gen, &format!("{label}_poll_reap"));
    free_buffers(r#gen);
    r#gen.emit(&format!("\tjmp\t{label}_fail"));

    // the status is the exit status, or minus the signal that stopped the child
    r#gen.emit(&format!("{label}_ended:"));
    reap(r#gen, &format!("{label}_reap"));
    r#gen.emit(&format!("\tmov\teax, DWORD PTR [rbp - {}]", -STATUS));
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit(&format!("\tand\tr15, {WAIT_SIGNAL_MASK}"));
    r#gen.emit(&format!("\tjz\t{label}_exited"));
    r#gen.emit("\tneg\tr15");
    r#gen.emit(&format!("\tjmp\t{label}_status"));
    r#gen.emit(&format!("{label}_exited:"));
    r#gen.emit("\tshr\trax, 8");
    r#gen.emit("\tmovzx\tr15d, al");
    r#gen.emit(&format!("{label}_status:"));
    close_pipes(r#gen);
    free_argv(r#gen);
    for buffer in [STDOUT, STDERR] {
        r#gen.emit(&format!("\tmov\trdi, QWORD PTR [rbp - {}]", -buffer));
        r#gen.emit(&format!("\tmov\trsi, QWORD PTR [rbp - {}]", -(buffer + 8)));
        r#gen.emit("\tcall\t.Lfs_utf8");
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjz\t{label}_invalid"));
    }
    for (buffer, reg) in [(STDOUT, "r12"), (STDERR, "r13")] {
        r#gen.emit(&format!("\tmov\trdi, QWORD PTR [rbp - {}]", -buffer));
        r#gen.emit(&format!("\tmov\trsi, QWORD PTR [rbp - {}]", -(buffer + 8)));
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

    // a failure before the child ran: r15 holds the error, or rax minus it
    r#gen.emit(&format!("{label}_errno:"));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit(&format!("{label}_fail:"));
    close_pipes(r#gen);
    free_argv(r#gen);
    r#gen.emit("\tmov\trdi, r15");
    r#gen.emit(&format!("\tjmp\t{label}_err"));

    child(r#gen, label);
    routine_end(r#gen, label);
}

/// Emits the child's side of `stone.os_run`, which never returns: it moves its descriptors into
/// place and tries each path with `execve`, then writes the error number to the error pipe and
/// exits.
fn child(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_child:"));
    // stdin reads /dev/null
    r#gen.emit(&format!("\tmov\trdi, {AT_FDCWD}"));
    r#gen.emit("\tlea\trsi, [rip + .Los_run_null]");
    r#gen.emit("\txor\tedx, edx"); // O_RDONLY
    r#gen.emit("\txor\tr10d, r10d");
    r#gen.emit("\tmov\teax, 257"); // sys_openat
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_null_open"));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit(&format!("\tjmp\t{label}_report"));
    r#gen.emit(&format!("{label}_null_open:"));
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tmov\teax, 33"); // sys_dup2
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjz\t{label}_stdin"));
    close(r#gen, "r12");
    r#gen.emit(&format!("{label}_stdin:"));
    for (end, fd) in [(OUT_PIPE + 4, 1), (ERR_PIPE + 4, 2)] {
        r#gen.emit(&format!("\tmov\tedi, DWORD PTR [rbp - {}]", -end));
        r#gen.emit(&format!("\tmov\tesi, {fd}"));
        r#gen.emit("\tmov\teax, 33"); // sys_dup2
        r#gen.emit("\tsyscall");
    }
    for end in [OUT_PIPE, OUT_PIPE + 4, ERR_PIPE, ERR_PIPE + 4, EXEC_PIPE] {
        r#gen.emit(&format!("\tmov\tedi, DWORD PTR [rbp - {}]", -end));
        r#gen.emit("\tmov\teax, 3"); // sys_close
        r#gen.emit("\tsyscall");
    }

    // r13 holds the last error, r14 whether a path could not be run, r15 the command's length,
    // and r12 the next directory of PATH
    r#gen.emit(&format!("\tmov\tr13, {ENOENT}"));
    r#gen.emit("\txor\tr14, r14");
    r#gen.emit("\tmov\tr15, QWORD PTR [rbx - 16]");
    r#gen.emit("\ttest\tr15, r15");
    r#gen.emit(&format!("\tjz\t{label}_report"));
    // a command holding / is the only path
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(&format!("{label}_slash:"));
    r#gen.emit("\tcmp\trcx, r15");
    r#gen.emit(&format!("\tjae\t{label}_search"));
    r#gen.emit("\tcmp\tbyte ptr [rbx + rcx], 47"); // '/'
    r#gen.emit(&format!("\tje\t{label}_direct"));
    r#gen.emit("\tinc\trcx");
    r#gen.emit(&format!("\tjmp\t{label}_slash"));
    r#gen.emit(&format!("{label}_direct:"));
    r#gen.emit("\tmov\trdi, rbx");
    exec(r#gen);
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit(&format!("\tjmp\t{label}_report"));

    r#gen.emit(&format!("{label}_search:"));
    r#gen.emit("\tlea\trdi, [rip + .Los_run_path_name]");
    r#gen.emit("\tcall\tstone.getenv");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjnz\t{label}_path"));
    r#gen.emit("\tlea\trax, [rip + .Los_run_default_path]");
    r#gen.emit(&format!("{label}_path:"));
    r#gen.emit("\tmov\tr12, rax");
    // a path is at most a directory, a /, the command, and its null, or ./ for an empty directory
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tcall\tstone.cstr_len");
    r#gen.emit("\tlea\trdi, [rax + r15 + 3]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", -PATH));
    r#gen.emit(&format!("{label}_entry:"));
    // rcx counts the directory's bytes, up to a : or the end
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(&format!("{label}_scan:"));
    r#gen.emit("\tmovzx\teax, byte ptr [r12 + rcx]");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit(&format!("\tjz\t{label}_scanned"));
    r#gen.emit("\tcmp\teax, 58"); // ':'
    r#gen.emit(&format!("\tje\t{label}_scanned"));
    r#gen.emit("\tinc\trcx");
    r#gen.emit(&format!("\tjmp\t{label}_scan"));
    r#gen.emit(&format!("{label}_scanned:"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], rax", -MORE));
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit("\tlea\tr12, [r12 + rcx + 1]");
    r#gen.emit(&format!("\tmov\trdi, QWORD PTR [rbp - {}]", -PATH));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit(&format!("{label}_dir:"));
    r#gen.emit("\tcmp\trdx, rcx");
    r#gen.emit(&format!("\tjae\t{label}_dir_copied"));
    r#gen.emit("\tmovzx\teax, byte ptr [rsi + rdx]");
    r#gen.emit("\tmov\tbyte ptr [rdi + rdx], al");
    r#gen.emit("\tinc\trdx");
    r#gen.emit(&format!("\tjmp\t{label}_dir"));
    r#gen.emit(&format!("{label}_dir_copied:"));
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjnz\t{label}_named"));
    r#gen.emit("\tmov\tbyte ptr [rdi], 46"); // '.'
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit(&format!("{label}_named:"));
    r#gen.emit("\tmov\tbyte ptr [rdi + rdx], 47"); // '/'
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(&format!("{label}_name:"));
    r#gen.emit("\tmovzx\teax, byte ptr [rbx + rcx]");
    r#gen.emit("\tmov\tbyte ptr [rdi + rdx], al");
    r#gen.emit("\tinc\trdx");
    r#gen.emit("\tinc\trcx");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit(&format!("\tjnz\t{label}_name"));
    exec(r#gen);
    // a path that names nothing moves on, and so does one that cannot be run, remembered
    r#gen.emit(&format!("\tcmp\trax, -{ENOENT}"));
    r#gen.emit(&format!("\tje\t{label}_next"));
    r#gen.emit(&format!("\tcmp\trax, -{ENOTDIR}"));
    r#gen.emit(&format!("\tje\t{label}_next"));
    r#gen.emit(&format!("\tcmp\trax, -{EACCES}"));
    r#gen.emit(&format!("\tjne\t{label}_stop"));
    r#gen.emit("\tmov\tr14, 1");
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit(&format!("\tcmp\tQWORD PTR [rbp - {}], 0", -MORE));
    r#gen.emit(&format!("\tjne\t{label}_entry"));
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit(&format!("\tjz\t{label}_report"));
    r#gen.emit(&format!("\tmov\tr13, {EACCES}"));
    r#gen.emit(&format!("\tjmp\t{label}_report"));
    r#gen.emit(&format!("{label}_stop:"));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");

    // the parent reads the error number's low 4 bytes
    r#gen.emit(&format!("{label}_report:"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {}], r13", -STATUS));
    r#gen.emit(&format!(
        "\tmov\tedi, DWORD PTR [rbp - {}]",
        -(EXEC_PIPE + 4)
    ));
    r#gen.emit(&format!("\tlea\trsi, [rbp - {}]", -STATUS));
    r#gen.emit("\tmov\tedx, 4");
    r#gen.emit("\tmov\teax, 1"); // sys_write
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tmov\tedi, {NOT_RUN}"));
    r#gen.emit("\tmov\teax, 231"); // sys_exit_group
    r#gen.emit("\tsyscall");
}

/// Emits an `execve` of the path `rdi` with the routine's argv and the program's environment,
/// which returns only on failure, with minus the error number in `rax`.
fn exec(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(&format!("\tmov\trsi, QWORD PTR [rbp - {}]", -ARGV));
    r#gen.emit("\tmov\trdx, QWORD PTR [rip + stone.envp]");
    r#gen.emit("\tmov\teax, 59"); // sys_execve
    r#gen.emit("\tsyscall");
}

/// Emits `.Los_run_drain`, which reads once from the descriptor `rdi` into the output `rsi`
/// points to (a buffer, the bytes it holds, and its size), doubling the buffer first if it is
/// full, made again if a signal interrupts it, and returns what `read` returned: the bytes
/// read, 0 at the end, or minus an error number.
fn drain(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the descriptor and r12 the output
    r#gen.emit(".Los_run_drain:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\trax, QWORD PTR [r12 + 8]");
    r#gen.emit("\tcmp\trax, QWORD PTR [r12 + 16]");
    r#gen.emit("\tjne\t.Los_run_drain_room");
    r#gen.emit("\tmov\trsi, QWORD PTR [r12 + 16]");
    r#gen.emit("\tadd\trsi, rsi");
    r#gen.emit("\tmov\tQWORD PTR [r12 + 16], rsi");
    r#gen.emit("\tmov\trdi, QWORD PTR [r12]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tQWORD PTR [r12], rax");
    r#gen.emit(".Los_run_drain_room:");
    r#gen.emit("\tmov\trdi, rbx");
    r#gen.emit("\tmov\trsi, QWORD PTR [r12]");
    r#gen.emit("\tmov\trax, QWORD PTR [r12 + 8]");
    r#gen.emit("\tadd\trsi, rax");
    r#gen.emit("\tmov\trdx, QWORD PTR [r12 + 16]");
    r#gen.emit("\tsub\trdx, rax");
    r#gen.emit("\txor\teax, eax"); // sys_read
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit("\tje\t.Los_run_drain_room");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjle\t.Los_run_drain_done");
    r#gen.emit("\tmov\trcx, QWORD PTR [r12 + 8]");
    r#gen.emit("\tadd\trcx, rax");
    r#gen.emit("\tmov\tQWORD PTR [r12 + 8], rcx");
    r#gen.emit(".Los_run_drain_done:");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits a `close` of the descriptor in the 32-bit slot at `offset`, which then holds -1.
fn close_slot(r#gen: &mut dyn AssemblyGenerator, offset: i64) {
    r#gen.emit(&format!("\tmov\tedi, DWORD PTR [rbp - {}]", -offset));
    r#gen.emit("\tmov\teax, 3"); // sys_close
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tmov\tDWORD PTR [rbp - {}], -1", -offset));
}

/// Emits a `close` of every pipe descriptor still open. A closed one holds -1, which `close`
/// rejects harmlessly.
fn close_pipes(r#gen: &mut dyn AssemblyGenerator) {
    for pipe in [OUT_PIPE, ERR_PIPE, EXEC_PIPE] {
        close_slot(r#gen, pipe);
        close_slot(r#gen, pipe + 4);
    }
}

/// Emits a `wait4` for the child, made again if a signal interrupts it, which leaves its status
/// in the slot `STATUS`, using `done` as a label of its own.
fn reap(r#gen: &mut dyn AssemblyGenerator, done: &str) {
    r#gen.emit(&format!("{done}:"));
    r#gen.emit(&format!("\tmov\trdi, QWORD PTR [rbp - {}]", -PID));
    r#gen.emit(&format!("\tlea\trsi, [rbp - {}]", -STATUS));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr10d, r10d");
    r#gen.emit("\tmov\teax, 61"); // sys_wait4
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{done}"));
}

/// Emits a free of the routine's argv.
fn free_argv(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", -ARGV));
    r#gen.emit("\tcall\tstone.mem_free");
}

/// Emits a free of both output buffers.
fn free_buffers(r#gen: &mut dyn AssemblyGenerator) {
    for buffer in [STDOUT, STDERR] {
        r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbp - {}]", -buffer));
        r#gen.emit("\tcall\tstone.mem_free");
    }
}
