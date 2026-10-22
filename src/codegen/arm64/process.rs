//! The arm64 runtime of `os.run`, which mirrors `x64/process.rs` under the same label and
//! contract, with the command in `x0`, the arguments in `x1`, and the result in `x0`.
//!
//! Every call into the system goes through [`system_call`]. Linux forks with `clone`, waits with
//! `ppoll`, and moves descriptors with `dup3`, since arm64 Linux has no `fork`, `poll`, or
//! `dup2`, and marks the error pipe close-on-exec through `pipe2`. macOS calls `libSystem`'s
//! `fork`, `poll`, `dup2`, and `pipe`, then `fcntl` for close-on-exec.

use super::address;
use super::builtins::{Sys, system_call};
use super::fs::{EINTR, close, load, routine_end, routine_start};
use crate::codegen::context::{
    FILE_BUFFER, INVALID_TEXT_CODE, NULL_IN_COMMAND_CODE, OUTPUT_KINDS, WAIT_SIGNAL_MASK,
    immortal_string,
};
use crate::codegen::{AssemblyGenerator, Os};
use crate::stdlib::os::DEFAULT_PATH;

/// `openat`'s directory for a path relative to the working directory, which macOS's shim drops.
const AT_FDCWD: i64 = -100;

/// The error numbers the search for a program treats specially, the same on both systems.
const ENOENT: i64 = 2;
const EACCES: i64 = 13;
const ENOTDIR: i64 = 20;

/// Linux's `pipe2` flag that closes a descriptor when `execve` succeeds.
const O_CLOEXEC: i64 = 0o2000000;

/// The signal a child sends its parent when it exits, which `clone` takes to act as `fork`.
const SIGCHLD: i64 = 17;

/// macOS's `fcntl` command that sets a descriptor's flags, and the flag that closes it on
/// `execve`.
const F_SETFD: i64 = 2;
const FD_CLOEXEC: i64 = 1;

/// What a child that could not run any path exits with, as a shell does.
const NOT_RUN: i64 = 127;

// where the routine keeps its state, from sp: each pipe is two 32-bit descriptors, the end to
// read first, each pollfd is the descriptor then the events asked for and those that happened,
// 16 bits each, and each output is a buffer from stone.mem_alloc, the bytes it holds, and its size
const OUT_PIPE: i64 = 0;
const ERR_PIPE: i64 = 8;
const EXEC_PIPE: i64 = 16;
const STATUS: i64 = 24;
const POLL: i64 = 32;
const STDOUT: i64 = 48;
const STDERR: i64 = 72;
/// The child's buffer for each path it tries.
const PATH: i64 = 96;
/// The byte after the child's directory of `PATH`, 0 once the last is tried.
const MORE: i64 = 104;
const FRAME: i64 = 112;

/// Emits `stone.os_run`, as `x64::process::run_runtime` describes it.
pub fn run_runtime(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.os_run";
    let os = r#gen.target().os;
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Los_run_null", "/dev/null");
    immortal_string(r#gen, ".Los_run_path_name", "PATH");
    immortal_string(r#gen, ".Los_run_default_path", DEFAULT_PATH);
    r#gen.emit("\t.text");

    drain(r#gen);

    // x19 holds the command, x20 the arguments, x21 argv, x22 the child, x23 a count, x24 the
    // error or status, and x25 and x26 the output strings
    routine_start(r#gen, label, NULL_IN_COMMAND_CODE);
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit(&format!("\tsub\tsp, sp, #{FRAME}"));
    // every descriptor starts closed, so cleaning up can close them all
    r#gen.emit("\tmov\tx9, #-1");
    for pipe in [OUT_PIPE, ERR_PIPE, EXEC_PIPE] {
        r#gen.emit(&format!("\tstr\tx9, [sp, #{pipe}]"));
    }

    // argv is the command, each argument, then null, and no argument may hold a null character
    r#gen.emit("\tldr\tx23, [x20]");
    r#gen.emit("\tadd\tx0, x23, #2");
    r#gen.emit("\tlsl\tx0, x0, #3");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tstr\tx19, [x21]");
    r#gen.emit("\tadd\tx9, x21, x23, lsl #3");
    r#gen.emit("\tstr\txzr, [x9, #8]");
    r#gen.emit("\tmov\tx24, #0");
    r#gen.emit(&format!("{label}_arg:"));
    r#gen.emit("\tcmp\tx24, x23");
    r#gen.emit(&format!("\tb.hs\t{label}_pipes"));
    r#gen.emit("\tldr\tx9, [x20, #16]");
    r#gen.emit("\tldr\tx0, [x9, x24, lsl #3]");
    r#gen.emit("\tadd\tx9, x21, x24, lsl #3");
    r#gen.emit("\tstr\tx0, [x9, #8]");
    r#gen.emit("\tadd\tx24, x24, #1");
    r#gen.emit("\tbl\t.Lfs_check_path");
    r#gen.emit(&format!("\tcbz\tx9, {label}_arg"));
    r#gen.emit(&format!("\tmov\tx24, #{NULL_IN_COMMAND_CODE}"));
    r#gen.emit(&format!("\tb\t{label}_fail"));

    // the output pipes stay open across execve, and the error pipe closes there
    r#gen.emit(&format!("{label}_pipes:"));
    for (pipe, flags) in [(OUT_PIPE, 0), (ERR_PIPE, 0), (EXEC_PIPE, O_CLOEXEC)] {
        r#gen.emit(&format!("\tadd\tx0, sp, #{pipe}"));
        load(r#gen, "x1", flags);
        system_call(r#gen, Sys::Pipe2);
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_errno"));
    }
    if os == Os::MacOs {
        r#gen.emit(&format!("\tldr\tw0, [sp, #{}]", EXEC_PIPE + 4));
        r#gen.emit(&format!("\tmov\tx1, #{F_SETFD}"));
        r#gen.emit(&format!("\tmov\tx2, #{FD_CLOEXEC}"));
        system_call(r#gen, Sys::Fcntl);
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_errno"));
    } else {
        r#gen.emit(&format!("\tmov\tx0, #{SIGCHLD}"));
        for reg in ["x1", "x2", "x3", "x4"] {
            r#gen.emit(&format!("\tmov\t{reg}, #0"));
        }
    }
    system_call(r#gen, Sys::Clone);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_errno"));
    r#gen.emit(&format!("\tcbz\tx0, {label}_child"));
    r#gen.emit("\tmov\tx22, x0");

    // the parent keeps only the ends it reads
    for end in [OUT_PIPE + 4, ERR_PIPE + 4, EXEC_PIPE + 4] {
        close_slot(r#gen, end);
    }
    // the error pipe ends with no bytes once execve succeeds, or holds the child's error number
    r#gen.emit(&format!("{label}_exec_read:"));
    r#gen.emit(&format!("\tldr\tw0, [sp, #{EXEC_PIPE}]"));
    r#gen.emit(&format!("\tadd\tx1, sp, #{STATUS}"));
    r#gen.emit("\tmov\tx2, #4");
    system_call(r#gen, Sys::Read);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_exec_read"));
    r#gen.emit("\tcmp\tx0, #4");
    r#gen.emit(&format!("\tb.ne\t{label}_started"));
    r#gen.emit(&format!("\tldr\tw24, [sp, #{STATUS}]"));
    reap(r#gen, &format!("{label}_exec_failed"));
    r#gen.emit(&format!("\tb\t{label}_fail"));

    // read both pipes as they fill, a pipe whose descriptor is -1 having ended
    r#gen.emit(&format!("{label}_started:"));
    for (pipe, poll, buffer) in [(OUT_PIPE, POLL, STDOUT), (ERR_PIPE, POLL + 8, STDERR)] {
        r#gen.emit(&format!("\tldr\tw9, [sp, #{pipe}]"));
        r#gen.emit(&format!("\tstr\tw9, [sp, #{poll}]"));
        r#gen.emit("\tmov\tx9, #1"); // POLLIN
        r#gen.emit(&format!("\tstr\tw9, [sp, #{}]", poll + 4));
        load(r#gen, "x0", FILE_BUFFER);
        r#gen.emit("\tbl\tstone.mem_alloc");
        r#gen.emit(&format!("\tstr\tx0, [sp, #{buffer}]"));
        r#gen.emit(&format!("\tstr\txzr, [sp, #{}]", buffer + 8));
        load(r#gen, "x9", FILE_BUFFER);
        r#gen.emit(&format!("\tstr\tx9, [sp, #{}]", buffer + 16));
    }
    r#gen.emit("\tmov\tx23, #2");
    r#gen.emit(&format!("{label}_poll:"));
    r#gen.emit(&format!("\tcbz\tx23, {label}_ended"));
    r#gen.emit(&format!("\tadd\tx0, sp, #{POLL}"));
    r#gen.emit("\tmov\tx1, #2");
    match os {
        // no timeout and no signal mask
        Os::Linux => {
            for reg in ["x2", "x3", "x4"] {
                r#gen.emit(&format!("\tmov\t{reg}, #0"));
            }
        }
        // no timeout
        Os::MacOs => r#gen.emit("\tmov\tx2, #-1"),
    }
    system_call(r#gen, Sys::Ppoll);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_poll"));
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_poll_failed"));
    for (i, (poll, buffer)) in [(POLL, STDOUT), (POLL + 8, STDERR)].into_iter().enumerate() {
        let next = format!("{label}_polled{i}");
        r#gen.emit(&format!("\tldrh\tw9, [sp, #{}]", poll + 6));
        r#gen.emit(&format!("\tcbz\tw9, {next}"));
        r#gen.emit(&format!("\tldr\tw0, [sp, #{poll}]"));
        r#gen.emit(&format!("\tadd\tx1, sp, #{buffer}"));
        r#gen.emit("\tbl\t.Los_run_drain");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit(&format!("\tb.gt\t{next}"));
        // the end of the pipe, or an error reading it, which ends it too
        r#gen.emit("\tmov\tx9, #-1");
        r#gen.emit(&format!("\tstr\tw9, [sp, #{poll}]"));
        r#gen.emit("\tsub\tx23, x23, #1");
        r#gen.emit(&format!("{next}:"));
    }
    r#gen.emit(&format!("\tb\t{label}_poll"));
    r#gen.emit(&format!("{label}_poll_failed:"));
    // closing the pipes first lets a child writing to them finish
    r#gen.emit("\tneg\tx24, x0");
    close_pipes(r#gen);
    reap(r#gen, &format!("{label}_poll_reap"));
    free_buffers(r#gen);
    r#gen.emit(&format!("\tb\t{label}_fail"));

    // the status is the exit status, or minus the signal that stopped the child
    r#gen.emit(&format!("{label}_ended:"));
    reap(r#gen, &format!("{label}_reap"));
    r#gen.emit(&format!("\tldr\tw9, [sp, #{STATUS}]"));
    r#gen.emit(&format!("\tand\tx24, x9, #{WAIT_SIGNAL_MASK}"));
    r#gen.emit(&format!("\tcbz\tx24, {label}_exited"));
    r#gen.emit("\tneg\tx24, x24");
    r#gen.emit(&format!("\tb\t{label}_status"));
    r#gen.emit(&format!("{label}_exited:"));
    r#gen.emit("\tlsr\tx24, x9, #8");
    r#gen.emit("\tand\tx24, x24, #255");
    r#gen.emit(&format!("{label}_status:"));
    close_pipes(r#gen);
    free_argv(r#gen);
    for buffer in [STDOUT, STDERR] {
        r#gen.emit(&format!("\tldr\tx0, [sp, #{buffer}]"));
        r#gen.emit(&format!("\tldr\tx1, [sp, #{}]", buffer + 8));
        r#gen.emit("\tbl\t.Lfs_utf8");
        r#gen.emit(&format!("\tcbz\tx9, {label}_invalid"));
    }
    for (buffer, reg) in [(STDOUT, "x25"), (STDERR, "x26")] {
        r#gen.emit(&format!("\tldr\tx0, [sp, #{buffer}]"));
        r#gen.emit(&format!("\tldr\tx1, [sp, #{}]", buffer + 8));
        r#gen.emit("\tbl\tstone.str_slice");
        r#gen.emit(&format!("\tmov\t{reg}, x0"));
    }
    free_buffers(r#gen);
    r#gen.emit("\tmov\tx0, #0"); // ok
    r#gen.emit(&format!("\tmov\tx1, #{OUTPUT_KINDS}"));
    r#gen.emit("\tmov\tx2, #3");
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit("\tstr\tx24, [x0, #16]");
    r#gen.emit("\tstr\tx25, [x0, #24]");
    r#gen.emit("\tstr\tx26, [x0, #32]");
    r#gen.emit(&format!("\tb\t{label}_done"));
    r#gen.emit(&format!("{label}_invalid:"));
    free_buffers(r#gen);
    r#gen.emit(&format!("\tmov\tx0, #{INVALID_TEXT_CODE}"));
    r#gen.emit(&format!("\tb\t{label}_err"));

    // a failure before the child ran: x24 holds the error, or x0 minus it
    r#gen.emit(&format!("{label}_errno:"));
    r#gen.emit("\tneg\tx24, x0");
    r#gen.emit(&format!("{label}_fail:"));
    close_pipes(r#gen);
    free_argv(r#gen);
    r#gen.emit("\tmov\tx0, x24");
    r#gen.emit(&format!("\tb\t{label}_err"));

    child(r#gen, label);
    routine_end(r#gen, label);
}

/// Emits the child's side of `stone.os_run`, as `x64::process` does, which never returns.
fn child(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_child:"));
    // stdin reads /dev/null
    r#gen.emit(&format!("\tmov\tx0, #{AT_FDCWD}"));
    address(r#gen, "x1", ".Los_run_null");
    r#gen.emit("\tmov\tx2, #0"); // O_RDONLY
    r#gen.emit("\tmov\tx3, #0");
    system_call(r#gen, Sys::Openat);
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_null_open"));
    r#gen.emit("\tneg\tx23, x0");
    r#gen.emit(&format!("\tb\t{label}_report"));
    r#gen.emit(&format!("{label}_null_open:"));
    r#gen.emit("\tmov\tx25, x0");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tmov\tx2, #0");
    system_call(r#gen, Sys::Dup3);
    r#gen.emit(&format!("\tcbz\tx25, {label}_stdin"));
    close(r#gen, "x25");
    r#gen.emit(&format!("{label}_stdin:"));
    for (end, fd) in [(OUT_PIPE + 4, 1), (ERR_PIPE + 4, 2)] {
        r#gen.emit(&format!("\tldr\tw0, [sp, #{end}]"));
        r#gen.emit(&format!("\tmov\tx1, #{fd}"));
        r#gen.emit("\tmov\tx2, #0");
        system_call(r#gen, Sys::Dup3);
    }
    for end in [OUT_PIPE, OUT_PIPE + 4, ERR_PIPE, ERR_PIPE + 4, EXEC_PIPE] {
        r#gen.emit(&format!("\tldr\tw0, [sp, #{end}]"));
        system_call(r#gen, Sys::Close);
    }

    // x23 holds the last error, x25 whether a path could not be run, x20 the command's length,
    // and x26 the next directory of PATH
    r#gen.emit(&format!("\tmov\tx23, #{ENOENT}"));
    r#gen.emit("\tmov\tx25, #0");
    r#gen.emit("\tldur\tx20, [x19, #-16]");
    r#gen.emit(&format!("\tcbz\tx20, {label}_report"));
    // a command holding / is the only path
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(&format!("{label}_slash:"));
    r#gen.emit("\tcmp\tx9, x20");
    r#gen.emit(&format!("\tb.hs\t{label}_search"));
    r#gen.emit("\tldrb\tw10, [x19, x9]");
    r#gen.emit("\tcmp\tw10, #47"); // '/'
    r#gen.emit(&format!("\tb.eq\t{label}_direct"));
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(&format!("\tb\t{label}_slash"));
    r#gen.emit(&format!("{label}_direct:"));
    r#gen.emit("\tmov\tx0, x19");
    exec(r#gen);
    r#gen.emit("\tneg\tx23, x0");
    r#gen.emit(&format!("\tb\t{label}_report"));

    r#gen.emit(&format!("{label}_search:"));
    address(r#gen, "x0", ".Los_run_path_name");
    r#gen.emit("\tbl\tstone.getenv");
    r#gen.emit(&format!("\tcbnz\tx0, {label}_path"));
    address(r#gen, "x0", ".Los_run_default_path");
    r#gen.emit(&format!("{label}_path:"));
    r#gen.emit("\tmov\tx26, x0");
    // a path is at most a directory, a /, the command, and its null, or ./ for an empty directory
    r#gen.emit("\tbl\tstone.cstr_len");
    r#gen.emit("\tadd\tx0, x0, x20");
    r#gen.emit("\tadd\tx0, x0, #3");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit(&format!("\tstr\tx0, [sp, #{PATH}]"));
    r#gen.emit(&format!("{label}_entry:"));
    // x9 counts the directory's bytes, up to a : or the end
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(&format!("{label}_scan:"));
    r#gen.emit("\tldrb\tw10, [x26, x9]");
    r#gen.emit(&format!("\tcbz\tw10, {label}_scanned"));
    r#gen.emit("\tcmp\tw10, #58"); // ':'
    r#gen.emit(&format!("\tb.eq\t{label}_scanned"));
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(&format!("\tb\t{label}_scan"));
    r#gen.emit(&format!("{label}_scanned:"));
    r#gen.emit(&format!("\tstr\tx10, [sp, #{MORE}]"));
    r#gen.emit("\tmov\tx11, x26");
    r#gen.emit("\tadd\tx26, x26, x9");
    r#gen.emit("\tadd\tx26, x26, #1");
    r#gen.emit(&format!("\tldr\tx0, [sp, #{PATH}]"));
    r#gen.emit("\tmov\tx12, #0");
    r#gen.emit(&format!("{label}_dir:"));
    r#gen.emit("\tcmp\tx12, x9");
    r#gen.emit(&format!("\tb.hs\t{label}_dir_copied"));
    r#gen.emit("\tldrb\tw10, [x11, x12]");
    r#gen.emit("\tstrb\tw10, [x0, x12]");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit(&format!("\tb\t{label}_dir"));
    r#gen.emit(&format!("{label}_dir_copied:"));
    r#gen.emit(&format!("\tcbnz\tx9, {label}_named"));
    r#gen.emit("\tmov\tx10, #46"); // '.'
    r#gen.emit("\tstrb\tw10, [x0]");
    r#gen.emit("\tmov\tx12, #1");
    r#gen.emit(&format!("{label}_named:"));
    r#gen.emit("\tmov\tx10, #47"); // '/'
    r#gen.emit("\tstrb\tw10, [x0, x12]");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(&format!("{label}_name:"));
    r#gen.emit("\tldrb\tw10, [x19, x9]");
    r#gen.emit("\tstrb\tw10, [x0, x12]");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(&format!("\tcbnz\tw10, {label}_name"));
    exec(r#gen);
    // a path that names nothing moves on, and so does one that cannot be run, remembered
    r#gen.emit(&format!("\tcmn\tx0, #{ENOENT}"));
    r#gen.emit(&format!("\tb.eq\t{label}_next"));
    r#gen.emit(&format!("\tcmn\tx0, #{ENOTDIR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_next"));
    r#gen.emit(&format!("\tcmn\tx0, #{EACCES}"));
    r#gen.emit(&format!("\tb.ne\t{label}_stop"));
    r#gen.emit("\tmov\tx25, #1");
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit("\tneg\tx23, x0");
    r#gen.emit(&format!("\tldr\tx9, [sp, #{MORE}]"));
    r#gen.emit(&format!("\tcbnz\tx9, {label}_entry"));
    r#gen.emit(&format!("\tcbz\tx25, {label}_report"));
    r#gen.emit(&format!("\tmov\tx23, #{EACCES}"));
    r#gen.emit(&format!("\tb\t{label}_report"));
    r#gen.emit(&format!("{label}_stop:"));
    r#gen.emit("\tneg\tx23, x0");

    // the parent reads the error number's low 4 bytes
    r#gen.emit(&format!("{label}_report:"));
    r#gen.emit(&format!("\tstr\tx23, [sp, #{STATUS}]"));
    r#gen.emit(&format!("\tldr\tw0, [sp, #{}]", EXEC_PIPE + 4));
    r#gen.emit(&format!("\tadd\tx1, sp, #{STATUS}"));
    r#gen.emit("\tmov\tx2, #4");
    system_call(r#gen, Sys::Write);
    r#gen.emit(&format!("\tmov\tx0, #{NOT_RUN}"));
    system_call(r#gen, Sys::Exit);
}

/// Emits an `execve` of the path `x0` with the routine's argv and the program's environment,
/// which returns only on failure, with minus the error number in `x0`.
fn exec(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tx1, x21");
    address(r#gen, "x2", "stone.envp");
    r#gen.emit("\tldr\tx2, [x2]");
    system_call(r#gen, Sys::Execve);
}

/// Emits `.Los_run_drain`, which reads once from the descriptor `x0` into the output `x1` points
/// to, as `x64::process` describes it.
fn drain(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the descriptor and x20 the output
    let saved = ["x19", "x20"];
    r#gen.emit(".Los_run_drain:");
    super::push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tldr\tx9, [x20, #8]");
    r#gen.emit("\tldr\tx10, [x20, #16]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Los_run_drain_room");
    r#gen.emit("\tadd\tx1, x10, x10");
    r#gen.emit("\tstr\tx1, [x20, #16]");
    r#gen.emit("\tldr\tx0, [x20]");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x20]");
    r#gen.emit(".Los_run_drain_room:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tldr\tx1, [x20]");
    r#gen.emit("\tldr\tx9, [x20, #8]");
    r#gen.emit("\tadd\tx1, x1, x9");
    r#gen.emit("\tldr\tx2, [x20, #16]");
    r#gen.emit("\tsub\tx2, x2, x9");
    system_call(r#gen, Sys::Read);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit("\tb.eq\t.Los_run_drain_room");
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tb.le\t.Los_run_drain_done");
    r#gen.emit("\tldr\tx9, [x20, #8]");
    r#gen.emit("\tadd\tx9, x9, x0");
    r#gen.emit("\tstr\tx9, [x20, #8]");
    r#gen.emit(".Los_run_drain_done:");
    super::pop_frame(r#gen, &saved);
}

/// Emits a `close` of the descriptor in the 32-bit slot at `offset` from `sp`, which then holds
/// -1.
fn close_slot(r#gen: &mut dyn AssemblyGenerator, offset: i64) {
    r#gen.emit(&format!("\tldr\tw0, [sp, #{offset}]"));
    system_call(r#gen, Sys::Close);
    r#gen.emit("\tmov\tx9, #-1");
    r#gen.emit(&format!("\tstr\tw9, [sp, #{offset}]"));
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
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit(&format!("\tadd\tx1, sp, #{STATUS}"));
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit("\tmov\tx3, #0");
    system_call(r#gen, Sys::Wait4);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{done}"));
}

/// Emits a free of the routine's argv.
fn free_argv(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tx9, x21");
    r#gen.emit("\tbl\tstone.mem_free");
}

/// Emits a free of both output buffers.
fn free_buffers(r#gen: &mut dyn AssemblyGenerator) {
    for buffer in [STDOUT, STDERR] {
        r#gen.emit(&format!("\tldr\tx9, [sp, #{buffer}]"));
        r#gen.emit("\tbl\tstone.mem_free");
    }
}
