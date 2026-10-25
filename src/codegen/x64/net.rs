//! The x86-64 runtime of the `net_sys` module and of the methods of sockets and listeners, which
//! make Linux system calls as `stdlib::net` does through Rust's `std::net` for the interpreter.
//!
//! A socket or listener is a handle: an object laid out as an enum value (see
//! `builtins::enum_runtime`) whose tag is [`SOCKET_TAG`] or [`LISTENER_TAG`], with no counted
//! slots, and these slots:
//!
//! - `[p + 16]`: the descriptor, or -1 once closed
//! - `[p + 24]`: a socket's buffer from `stone.mem_alloc`, or 0, or a listener's port
//! - `[p + 32]` and `[p + 40]`: where the bytes received but not read yet start and end in it
//! - `[p + 48]`: the buffer's size
//! - `[p + 56]`: 1 once the other end has closed the connection
//! - `[p + 64]`: how many milliseconds each wait may take, or -1 to wait forever
//!
//! `stone.free_enum` closes a handle's descriptor and frees its buffer when its count reaches 0.
//! Errors are made by `.Lfs_err`, as the `fs` routines make theirs: an error number or code
//! about the `name` an address was given, or, with no name, the reason alone.

use crate::codegen::AssemblyGenerator;
use crate::codegen::context::{
    CLOSED_EARLY_CODE, INVALID_ADDRESS_CODE, INVALID_TEXT_CODE, LISTENER_CLOSED_CODE,
    SOCKET_CLOSED_CODE, TIMED_OUT_CODE,
};
use crate::stdlib::net::{MAX_REPLY, READ_SIZE};

use super::fs::{EINTR, close};

/// The tag of a socket, which no enum's variant has, since tags count up from 0.
pub const SOCKET_TAG: i64 = -1;

/// The tag of a listener.
pub const LISTENER_TAG: i64 = -2;

/// The slots of a handle after its tag and kinds.
const HANDLE_SLOTS: i64 = 7;

/// `SOCK_CLOEXEC`, so a program `os.run` starts never inherits a connection.
const SOCK_CLOEXEC: i64 = 0x80000;

/// `MSG_NOSIGNAL`, so writing to a connection the other end closed fails with `EPIPE` instead
/// of killing the program with `SIGPIPE`.
const MSG_NOSIGNAL: i64 = 0x4000;

/// `MSG_DONTWAIT`, which a send with a timeout passes, so it waits only in `.Lnet_wait`.
const MSG_DONTWAIT: i64 = 0x40;

/// The error number of a send that would have to wait.
const EAGAIN: i64 = 11;

/// The error number of a connection a non-blocking `connect` started but did not finish.
const EINPROGRESS: i64 = 115;

/// `O_NONBLOCK`, which a socket connecting with a timeout has until it connects.
const O_NONBLOCK: i64 = 0x800;

/// The bytes the buffer of a socket starts with, room for two reads.
const FIRST_BUFFER: usize = 2 * READ_SIZE;

/// Emits the routines `used` lists, each following the function or method of `stdlib::net` with
/// the same name:
///
/// - `stone.net_connect`, `stone.net_listen`, and `stone.net_exchange` take an address list
///   in `rdi`, a port in `rsi`, and the name errors quote in `rdx`; `stone.net_exchange` also
///   takes the query list in `rcx` and the milliseconds to wait in `r8`
/// - `stone.net_connect` also takes the milliseconds it may wait in `rcx`, or -1
/// - `stone.net_send` takes a socket in `rdi` and a string, or bytes, in `rsi`
/// - `stone.net_read_line`, `stone.net_read`, `stone.net_read_all`, and
///   `stone.net_read_all_bytes` take a socket in `rdi`, and `stone.net_read_bytes` and
///   `stone.net_read_exact` a socket in `rdi` and a count in `rsi`
/// - `stone.net_set_timeout` takes a socket or listener in `rdi` and the seconds, a float's bits,
///   in `rsi`
/// - `stone.net_accept` and `stone.net_port` take a listener in `rdi`
/// - `stone.net_close` takes a socket or listener in `rdi`
///
/// They borrow their arguments, follow the System V calling convention, and need the pieces of
/// `fs::fs_runtime` that make results, which emits them, and the string, list, and enum
/// runtimes.
pub fn net_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    handle(r#gen);
    wait(r#gen);
    if used.iter().any(|label| {
        matches!(
            *label,
            "stone.net_read_line"
                | "stone.net_read"
                | "stone.net_read_all"
                | "stone.net_read_bytes"
                | "stone.net_read_exact"
                | "stone.net_read_all_bytes"
        )
    }) {
        receive(r#gen);
        take(r#gen);
        take_bytes(r#gen);
    }
    if uses("stone.net_set_timeout") {
        set_timeout(r#gen);
    }
    if uses("stone.net_read_bytes") {
        read_bytes(r#gen);
    }
    if uses("stone.net_read_exact") {
        read_exact(r#gen);
    }
    if uses("stone.net_read_all_bytes") {
        read_all_bytes(r#gen);
    }
    if uses("stone.net_connect") {
        connect(r#gen);
    }
    if uses("stone.net_listen") {
        listen(r#gen);
    }
    if uses("stone.net_exchange") {
        exchange(r#gen);
    }
    if uses("stone.net_accept") {
        accept(r#gen);
    }
    if uses("stone.net_port") {
        r#gen.emit("stone.net_port:");
        r#gen.emit("\tmov\trax, QWORD PTR [rdi + 24]");
        r#gen.emit("\tret");
    }
    if uses("stone.net_close") {
        close_handle(r#gen);
    }
    if uses("stone.net_send") {
        send(r#gen);
    }
    if uses("stone.net_read_line") {
        read_line(r#gen);
    }
    if uses("stone.net_read") {
        read(r#gen);
    }
    if uses("stone.net_read_all") {
        read_all(r#gen);
    }
}

/// Emits the start of a routine that saves `rbx` and `r12` through `r15` for its own use and has
/// 88 bytes below them, from `[rbp - 128]` to `[rbp - 41]`: an address at `[rbp - 80]`, and
/// scratch slots at `[rbp - 88]` through `[rbp - 128]`.
fn frame(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in ["rbx", "r12", "r13", "r14", "r15"] {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tsub\trsp, 88");
}

/// Emits the end of a routine [`frame`] began: `<label>_err`, which makes an `err` of the code
/// in `rdi` about the name in `rsi`, or of the reason alone if `rsi` is 0, and `<label>_done`,
/// which returns `rax`.
fn frame_end(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_err:"));
    r#gen.emit("\tcall\t.Lfs_err");
    r#gen.emit(&format!("{label}_done:"));
    r#gen.emit("\tlea\trsp, [rbp - 40]");
    for reg in ["r15", "r14", "r13", "r12", "rbx"] {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits a jump to `<label>_err` for the negative error number in `rax`, about the name in
/// `subject`, a register, or with the reason alone if it is `None`.
fn fail_with_errno(r#gen: &mut dyn AssemblyGenerator, label: &str, subject: Option<&str>) {
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\trdi, rax");
    match subject {
        Some(reg) => r#gen.emit(&format!("\tmov\trsi, {reg}")),
        None => r#gen.emit("\txor\tesi, esi"),
    }
    r#gen.emit(&format!("\tjmp\t{label}_err"));
}

/// Emits a jump to `<label>_err` for the error `code` with the reason alone.
fn fail_with_code(r#gen: &mut dyn AssemblyGenerator, label: &str, code: i64) {
    r#gen.emit(&format!("\tmov\trdi, {code}"));
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
}

/// Emits a system call `number`, made again while a signal interrupts it, whose arguments are
/// already in place except that `again` sets them, if given.
fn syscall(r#gen: &mut dyn AssemblyGenerator, label: &str, number: i64, setup: &[&str]) {
    r#gen.emit(&format!("{label}:"));
    for line in setup {
        r#gen.emit(&format!("\t{line}"));
    }
    r#gen.emit(&format!("\tmov\teax, {number}"));
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}"));
}

/// Emits code that writes the address list in `r12` and the port in `r13` as a `sockaddr_in` or
/// `sockaddr_in6` at `[rbp - 80]`, leaving its family in `r14` and its length in `r15`, or jumps
/// to `<label>_invalid` if the list is not 4 or 16 bytes or the port is not from 0 to 65535.
fn socket_address(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    // unsigned, so a negative port is out of range too
    r#gen.emit("\tcmp\tr13, 65535");
    r#gen.emit(&format!("\tja\t{label}_invalid"));
    r#gen.emit("\txor\teax, eax");
    for offset in [80, 72, 64, 56] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - {offset}], rax"));
    }
    // the port, most significant byte first
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tmov\tbyte ptr [rbp - 77], al");
    r#gen.emit("\tshr\trax, 8");
    r#gen.emit("\tmov\tbyte ptr [rbp - 78], al");
    r#gen.emit("\tmov\trcx, QWORD PTR [r12]");
    r#gen.emit("\tlea\trdx, [rbp - 76]");
    r#gen.emit("\tmov\tr14, 2"); // AF_INET
    r#gen.emit("\tmov\tr15, 16");
    r#gen.emit("\tcmp\trcx, 4");
    r#gen.emit(&format!("\tje\t{label}_bytes"));
    r#gen.emit("\tlea\trdx, [rbp - 72]");
    r#gen.emit("\tmov\tr14, 10"); // AF_INET6
    r#gen.emit("\tmov\tr15, 28");
    r#gen.emit("\tcmp\trcx, 16");
    r#gen.emit(&format!("\tjne\t{label}_invalid"));
    r#gen.emit(&format!("{label}_bytes:"));
    r#gen.emit("\tmov\trdi, QWORD PTR [r12 + 16]");
    r#gen.emit(&format!("{label}_byte:"));
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + rcx * 8]");
    r#gen.emit("\tcmp\trax, 255");
    r#gen.emit(&format!("\tja\t{label}_invalid"));
    r#gen.emit("\tmov\tbyte ptr [rdx + rcx], al");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjnz\t{label}_byte"));
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit("\tmov\tbyte ptr [rbp - 80], al");
}

/// Emits `.Lnet_handle`, which returns in `rax` a new handle of the descriptor `rdi` with the
/// tag `rsi` and its other slots 0.
fn handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_handle:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(&format!("\tmov\tedx, {HANDLE_SLOTS}"));
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tQWORD PTR [rax + 16], rbx");
    r#gen.emit("\txor\tecx, ecx");
    for offset in [24, 32, 40, 48, 56] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rax + {offset}], rcx"));
    }
    r#gen.emit("\tmov\tQWORD PTR [rax + 64], -1");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits `stone.net_connect`: a socket of the address's family, connected to it. With a timeout,
/// the socket connects without blocking, waits in `.Lnet_wait` until it can write, reads how the
/// connection went with `getsockopt(SO_ERROR)`, and goes back to blocking.
fn connect(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_connect";
    // rbx holds the name, r12 the address and then the descriptor, and r13 the port; [rbp - 96]
    // holds the timeout and [rbp - 104] the descriptor's flags
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdx");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 96], rcx");
    socket_address(r#gen, label);
    open_socket(r#gen, label, 1);
    r#gen.emit("\tcmp\tQWORD PTR [rbp - 96], 0");
    r#gen.emit(&format!("\tjl\t{label}_connect"));
    fcntl(r#gen, 3, "xor\tedx, edx"); // F_GETFL
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit("\tmov\tQWORD PTR [rbp - 104], rax");
    r#gen.emit(&format!("\tor\trax, {O_NONBLOCK}"));
    fcntl(r#gen, 4, "mov\trdx, rax"); // F_SETFL
    r#gen.emit(&format!("\tjs\t{label}_close"));
    syscall(
        r#gen,
        &format!("{label}_connect"),
        42, // sys_connect
        &["mov\trdi, r12", "lea\trsi, [rbp - 80]", "mov\trdx, r15"],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_blocking"));
    r#gen.emit(&format!("\tcmp\trax, -{EINPROGRESS}"));
    r#gen.emit(&format!("\tjne\t{label}_close"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\tesi, 4"); // POLLOUT
    r#gen.emit("\tmov\trdx, QWORD PTR [rbp - 96]");
    r#gen.emit("\tcall\t.Lnet_wait");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjnz\t{label}_close"));
    // how the connection went
    r#gen.emit("\tmov\tQWORD PTR [rbp - 88], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 112], 4");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\tesi, 1"); // SOL_SOCKET
    r#gen.emit("\tmov\tedx, 4"); // SO_ERROR
    r#gen.emit("\tlea\tr10, [rbp - 88]");
    r#gen.emit("\tlea\tr8, [rbp - 112]");
    r#gen.emit("\tmov\teax, 55"); // sys_getsockopt
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit("\tmov\teax, DWORD PTR [rbp - 88]");
    r#gen.emit("\tneg\trax");
    r#gen.emit(&format!("\tjnz\t{label}_close"));
    r#gen.emit(&format!("{label}_blocking:"));
    r#gen.emit("\tcmp\tQWORD PTR [rbp - 96], 0");
    r#gen.emit(&format!("\tjl\t{label}_connected"));
    fcntl(r#gen, 4, "mov\trdx, QWORD PTR [rbp - 104]"); // F_SETFL
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit(&format!("{label}_connected:"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit(&format!("\tmov\trsi, {SOCKET_TAG}"));
    r#gen.emit("\tcall\t.Lnet_handle");
    ok_handle(r#gen, label);
    closing_failure(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_listen`: a socket of the address's family, bound to it with `SO_REUSEADDR`,
/// as Rust's `TcpListener::bind` does, listening, with the port it got in slot 1.
fn listen(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_listen";
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdx");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    socket_address(r#gen, label);
    open_socket(r#gen, label, 1);
    r#gen.emit("\tmov\tQWORD PTR [rbp - 88], 1");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\tesi, 1"); // SOL_SOCKET
    r#gen.emit("\tmov\tedx, 2"); // SO_REUSEADDR
    r#gen.emit("\tlea\tr10, [rbp - 88]");
    r#gen.emit("\tmov\tr8, 4");
    r#gen.emit("\tmov\teax, 54"); // sys_setsockopt
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tlea\trsi, [rbp - 80]");
    r#gen.emit("\tmov\trdx, r15");
    r#gen.emit("\tmov\teax, 49"); // sys_bind
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\tesi, 128");
    r#gen.emit("\tmov\teax, 50"); // sys_listen
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_close"));
    // the port the system picked, if the address asked for 0
    r#gen.emit("\tmov\tQWORD PTR [rbp - 88], 32");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tlea\trsi, [rbp - 80]");
    r#gen.emit("\tlea\trdx, [rbp - 88]");
    r#gen.emit("\tmov\teax, 51"); // sys_getsockname
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_close"));
    r#gen.emit("\tmovzx\teax, byte ptr [rbp - 78]");
    r#gen.emit("\tshl\trax, 8");
    r#gen.emit("\tmovzx\tecx, byte ptr [rbp - 77]");
    r#gen.emit("\tadd\trax, rcx");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit(&format!("\tmov\trsi, {LISTENER_TAG}"));
    r#gen.emit("\tcall\t.Lnet_handle");
    r#gen.emit("\tmov\tQWORD PTR [rax + 24], r13");
    ok_handle(r#gen, label);
    closing_failure(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits an `fcntl` call with the command `command` on the descriptor in `r12`, after `third`
/// puts its argument in `rdx`, leaving the result in `rax` and the flags set by testing it.
fn fcntl(r#gen: &mut dyn AssemblyGenerator, command: i64, third: &str) {
    r#gen.emit(&format!("\t{third}"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit(&format!("\tmov\tesi, {command}"));
    r#gen.emit("\tmov\teax, 72"); // sys_fcntl
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
}

/// Emits code that opens a socket of the family in `r14` and the type `kind` (1 for a stream, 2
/// for datagrams) into `r12`, or fails with the error about the name in `rbx`, and the jump
/// target `<label>_invalid` for an address [`socket_address`] rejects.
fn open_socket(r#gen: &mut dyn AssemblyGenerator, label: &str, kind: i64) {
    r#gen.emit(&format!("\tjmp\t{label}_valid"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\trdi, {INVALID_ADDRESS_CODE}"));
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
    r#gen.emit(&format!("{label}_valid:"));
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit(&format!("\tmov\tesi, {}", kind | SOCK_CLOEXEC));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\teax, 41"); // sys_socket
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_opened"));
    fail_with_errno(r#gen, label, Some("rbx"));
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tr12, rax");
}

/// Emits the return of `ok` of the new handle in `rax`.
fn ok_handle(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\tesi, 3"); // an enum value's slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
}

/// Emits `<label>_close`, which closes the descriptor in `r12` and fails with the negative error
/// number in `rax` about the name in `rbx`.
fn closing_failure(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_close:"));
    r#gen.emit("\tmov\tr13, rax");
    close(r#gen, "r12");
    r#gen.emit("\tmov\trax, r13");
    fail_with_errno(r#gen, label, Some("rbx"));
}

/// Emits `stone.net_accept`: the next connection to the listener, as a new socket.
fn accept(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_accept";
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, QWORD PTR [rdi + 16]");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjns\t{label}_open"));
    fail_with_code(r#gen, label, LISTENER_CLOSED_CODE);
    r#gen.emit(&format!("{label}_open:"));
    wait_for(r#gen, label, "r12", 1, &format!("{label}_again"));
    syscall(
        r#gen,
        &format!("{label}_again"),
        288, // sys_accept4
        &[
            "mov\trdi, r12",
            "xor\tesi, esi",
            "xor\tedx, edx",
            &format!("mov\tr10, {SOCK_CLOEXEC}"),
        ],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_accepted"));
    fail_with_errno(r#gen, label, None);
    r#gen.emit(&format!("{label}_accepted:"));
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit(&format!("\tmov\trsi, {SOCKET_TAG}"));
    r#gen.emit("\tcall\t.Lnet_handle");
    ok_handle(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_close`, which closes the handle `rdi` unless it is closed already, freeing a
/// socket's buffer, so its methods fail from now on.
fn close_handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.net_close:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjs\t.Lnet_close_done");
    r#gen.emit("\tmov\teax, 3"); // sys_close
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], -1");
    r#gen.emit(&format!("\tcmp\tQWORD PTR [rbx], {SOCKET_TAG}"));
    r#gen.emit("\tjne\t.Lnet_close_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 24]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lnet_close_done");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], 0");
    r#gen.emit(".Lnet_close_done:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits a jump to `<label>_closed`'s error unless the socket in `rbx` is open.
fn check_open(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit(&format!("\tjge\t{label}_open"));
    fail_with_code(r#gen, label, SOCKET_CLOSED_CODE);
    r#gen.emit(&format!("{label}_open:"));
}

/// Emits `stone.net_send`, which sends all of the string `rsi`, returning `ok` of its length.
fn send(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_send";
    // r12 holds the string, r13 the descriptor, r14 the length, and r15 the bytes sent
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    check_open(r#gen, label);
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tr14, QWORD PTR [r12 - 16]");
    r#gen.emit("\txor\tr15, r15");
    // with a timeout, a send never waits but in .Lnet_wait
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - 88], {MSG_NOSIGNAL}"));
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 64], 0");
    r#gen.emit(&format!("\tjl\t{label}_more"));
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rbp - 88], {}",
        MSG_NOSIGNAL | MSG_DONTWAIT
    ));
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tcmp\tr15, r14");
    r#gen.emit(&format!("\tjae\t{label}_sent"));
    wait_for(r#gen, label, "r13", 4, &format!("{label}_again"));
    syscall(
        r#gen,
        &format!("{label}_again"),
        44, // sys_sendto
        &[
            "mov\trdi, r13",
            "lea\trsi, [r12 + r15]",
            "mov\trdx, r14",
            "sub\trdx, r15",
            "mov\tr10, QWORD PTR [rbp - 88]",
            "xor\tr8, r8",
            "xor\tr9, r9",
        ],
    );
    r#gen.emit(&format!("\tcmp\trax, -{EAGAIN}"));
    r#gen.emit(&format!("\tje\t{label}_more"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjns\t{label}_wrote"));
    fail_with_errno(r#gen, label, None);
    r#gen.emit(&format!("{label}_wrote:"));
    r#gen.emit("\tadd\tr15, rax");
    r#gen.emit(&format!("\tjmp\t{label}_more"));
    r#gen.emit(&format!("{label}_sent:"));
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    frame_end(r#gen, label);
}

/// Emits `.Lnet_receive`, which waits for more data on the socket in `rbx` and adds it to the
/// socket's buffer, first moving the bytes not read yet to its start and growing it if a read
/// would not fit, returning the bytes received in `rax`, 0 once the other end has closed the
/// connection, or a negative error number. It preserves `rbx` and `r12` through `r15`.
fn receive(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_receive:");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 56], 0");
    r#gen.emit("\tjne\t.Lnet_receive_ended");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 24]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lnet_receive_buffer");
    r#gen.emit(&format!("\tmov\trdi, {FIRST_BUFFER}"));
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], rax");
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbx + 48], {FIRST_BUFFER}"));
    r#gen.emit(".Lnet_receive_buffer:");
    // the bytes not read yet move to the start
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 32]");
    r#gen.emit("\ttest\trsi, rsi");
    r#gen.emit("\tjz\t.Lnet_receive_room");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 40]");
    r#gen.emit("\tsub\trcx, rsi");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 40], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 32], 0");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tadd\trsi, rdi");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit(".Lnet_receive_room:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 48]");
    r#gen.emit("\tsub\trax, QWORD PTR [rbx + 40]");
    r#gen.emit(&format!("\tcmp\trax, {READ_SIZE}"));
    r#gen.emit("\tjae\t.Lnet_receive_wait");
    // doubling makes room, since the bytes kept are at most the old size
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 48]");
    r#gen.emit("\tadd\trsi, rsi");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 48], rsi");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], rax");
    r#gen.emit(".Lnet_receive_wait:");
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 64]");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit("\tjs\t.Lnet_receive_again");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\tesi, 1"); // POLLIN
    r#gen.emit("\tcall\t.Lnet_wait");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjnz\t.Lnet_receive_done");
    syscall(
        r#gen,
        ".Lnet_receive_again",
        45, // sys_recvfrom
        &[
            "mov\trdi, QWORD PTR [rbx + 16]",
            "mov\trsi, QWORD PTR [rbx + 24]",
            "add\trsi, QWORD PTR [rbx + 40]",
            &format!("mov\trdx, {READ_SIZE}"),
            "xor\tr10, r10",
            "xor\tr8, r8",
            "xor\tr9, r9",
        ],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjs\t.Lnet_receive_done");
    r#gen.emit("\tjnz\t.Lnet_receive_got");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 56], 1");
    r#gen.emit(".Lnet_receive_got:");
    r#gen.emit("\tadd\tQWORD PTR [rbx + 40], rax");
    r#gen.emit(".Lnet_receive_done:");
    r#gen.emit("\tret");
    r#gen.emit(".Lnet_receive_ended:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");
}

/// Emits `.Lnet_take`, which takes the first `rdi` of the bytes not read yet from the socket in
/// `rbx` and returns in `rax` a new string of the first `rsi` of them, or 0 if those are not
/// text. It preserves `rbx` and `r12` through `r15`.
fn take(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_take:");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tmov\tr12, QWORD PTR [rbx + 24]");
    r#gen.emit("\tadd\tr12, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\tr13, rsi");
    r#gen.emit("\tadd\tQWORD PTR [rbx + 32], rdi");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\t.Lfs_utf8");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lnet_take_done");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit(".Lnet_take_done:");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tret");
}

/// Emits `.Lnet_take_bytes`, which takes the first `rdi` of the bytes not read yet from the
/// socket in `rbx` and returns them in `rax` as new bytes. It preserves `rbx` and `r12` through
/// `r15`.
fn take_bytes(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_take_bytes:");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tmov\tr12, QWORD PTR [rbx + 24]");
    r#gen.emit("\tadd\tr12, QWORD PTR [rbx + 32]");
    r#gen.emit("\tadd\tQWORD PTR [rbx + 32], rdi");
    r#gen.emit("\tmov\trsi, rdi");
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tcall\tstone.str_slice");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tret");
}

/// Emits `.Lnet_wait`, which waits until the descriptor `rdi` is ready for the `poll` events
/// `rsi`, for at most `rdx` milliseconds in all, returning 0 in `rax` when it is, the negative of
/// [`TIMED_OUT_CODE`] when the time runs out, or a negative error number. A signal that cuts the
/// wait short starts it again with the time left, from the monotonic clock, as
/// `stdlib::net::wait` does. It preserves `rbx` and `r12` through `r15`.
fn wait(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the descriptor, r12 the events, and r13 the deadline in nanoseconds; the clock
    // is read into [rsp], and the pollfd is at [rsp + 16]
    r#gen.emit(".Lnet_wait:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tpush\tr13");
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    wait_now(r#gen);
    r#gen.emit("\timul\tr13, r13, 1000000");
    r#gen.emit("\tadd\tr13, rax");
    r#gen.emit(".Lnet_wait_again:");
    wait_now(r#gen);
    r#gen.emit("\tmov\trcx, r13");
    r#gen.emit("\tsub\trcx, rax");
    r#gen.emit("\tjle\t.Lnet_wait_timeout");
    // the milliseconds left, rounded up
    r#gen.emit("\tlea\trax, [rcx + 999999]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\trcx, 1000000");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tmov\trdx, rax");
    // struct pollfd { fd, events, revents }
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tshl\trax, 32");
    r#gen.emit("\tadd\trax, rbx");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 16], rax");
    r#gen.emit("\tlea\trdi, [rsp + 16]");
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tmov\teax, 7"); // sys_poll
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit("\tje\t.Lnet_wait_again");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjs\t.Lnet_wait_done");
    // a poll that ran out may have rounded the time, so the clock decides
    r#gen.emit("\tjz\t.Lnet_wait_again");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tjmp\t.Lnet_wait_done");
    r#gen.emit(".Lnet_wait_timeout:");
    r#gen.emit(&format!("\tmov\trax, -{TIMED_OUT_CODE}"));
    r#gen.emit(".Lnet_wait_done:");
    r#gen.emit("\tadd\trsp, 32");
    r#gen.emit("\tpop\tr13");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits code that leaves the monotonic clock's time in nanoseconds in `rax`, reading it into
/// `[rsp]`, for `.Lnet_wait`.
fn wait_now(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tedi, 1"); // CLOCK_MONOTONIC
    r#gen.emit("\tmov\trsi, rsp");
    r#gen.emit("\tmov\teax, 228"); // sys_clock_gettime
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp]");
    r#gen.emit("\timul\trax, rax, 1000000000");
    r#gen.emit("\tadd\trax, QWORD PTR [rsp + 8]");
}

/// Emits code that waits, if the handle in `rbx` has a timeout, until the descriptor in `fd` is
/// ready for the `poll` events `events`, then goes on to `ready`, failing with the reason alone
/// if the wait runs out or fails.
fn wait_for(r#gen: &mut dyn AssemblyGenerator, label: &str, fd: &str, events: i64, ready: &str) {
    r#gen.emit("\tmov\trdx, QWORD PTR [rbx + 64]");
    r#gen.emit("\ttest\trdx, rdx");
    r#gen.emit(&format!("\tjs\t{ready}"));
    r#gen.emit(&format!("\tmov\trdi, {fd}"));
    r#gen.emit(&format!("\tmov\tesi, {events}"));
    r#gen.emit("\tcall\t.Lnet_wait");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{ready}"));
    fail_with_errno(r#gen, label, None);
}

/// Emits `stone.net_set_timeout`, which sets the handle's timeout to the seconds whose bits are
/// in `rsi`, counted as `stdlib::net::timeout_ms` counts them: -1 unless they are more than 0,
/// else the milliseconds rounded up, at most 2^31 - 1.
fn set_timeout(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.net_set_timeout:");
    r#gen.emit("\tmovq\txmm0, rsi");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tcvtsi2sd\txmm1, rax");
    // below, equal, or nan
    r#gen.emit("\tucomisd\txmm0, xmm1");
    r#gen.emit("\tjbe\t.Lnet_set_timeout_none");
    r#gen.emit("\tmov\teax, 1000");
    r#gen.emit("\tcvtsi2sd\txmm1, rax");
    r#gen.emit("\tmulsd\txmm0, xmm1");
    r#gen.emit("\tmov\teax, 2147483647");
    r#gen.emit("\tcvtsi2sd\txmm1, rax");
    r#gen.emit("\tucomisd\txmm0, xmm1");
    r#gen.emit("\tjae\t.Lnet_set_timeout_store");
    r#gen.emit("\tcvttsd2si\trax, xmm0");
    r#gen.emit("\tcvtsi2sd\txmm1, rax");
    r#gen.emit("\tucomisd\txmm0, xmm1");
    r#gen.emit("\tjbe\t.Lnet_set_timeout_store");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tjmp\t.Lnet_set_timeout_store");
    r#gen.emit(".Lnet_set_timeout_none:");
    r#gen.emit("\tmov\trax, -1");
    r#gen.emit(".Lnet_set_timeout_store:");
    r#gen.emit("\tmov\tQWORD PTR [rdi + 64], rax");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tret");
}

/// Emits the end of a byte reading routine: `<label>_bytes` returns `ok` of the bytes in `rax`,
/// and `<label>_failed` fails with the negative error number in `rax`.
fn bytes_result(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_bytes:"));
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\tesi, 1"); // a string slot, as bytes are
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    r#gen.emit(&format!("{label}_failed:"));
    fail_with_errno(r#gen, label, None);
}

/// Emits `stone.net_read_bytes`, which returns `ok` of what has arrived, up to `rsi` bytes,
/// waiting for something if nothing has, or of no bytes at the end or for a count of 0 or less.
fn read_bytes(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_bytes";
    // r12 holds the most to take
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    check_open(r#gen, label);
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjle\t{label}_take"));
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit(&format!("\tjnz\t{label}_some"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjnz\t{label}_again"));
    // the other end closed the connection
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit(&format!("\tjmp\t{label}_take"));
    r#gen.emit(&format!("{label}_some:"));
    r#gen.emit("\tcmp\trdi, r12");
    r#gen.emit("\tcmova\trdi, r12");
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tcall\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_exact`, which returns `ok` of the next `rsi` bytes, waiting for them,
/// or fails with `CLOSED_EARLY` if the other end closes the connection first.
fn read_exact(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_exact";
    // r12 holds how many to take
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    check_open(r#gen, label);
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjns\t{label}_again"));
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tcmp\tr13, r12");
    r#gen.emit(&format!("\tjae\t{label}_take"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjnz\t{label}_again"));
    fail_with_code(r#gen, label, CLOSED_EARLY_CODE);
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tmov\trdi, r12");
    r#gen.emit("\tcall\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_all_bytes`, which reads until the other end closes the connection and
/// returns `ok` of every byte that arrived.
fn read_all_bytes(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_all_bytes";
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjnz\t{label}_more"));
    unread(r#gen);
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tcall\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits the end of a reading routine: `<label>_text` returns `ok` of the string in `rax`, or
/// fails with `INVALID_TEXT` if it is 0, and `<label>_failed` fails with the negative error
/// number in `rax`.
fn text_result(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_text:"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_invalid"));
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\tesi, 1"); // a string slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    r#gen.emit(&format!("{label}_invalid:"));
    fail_with_code(r#gen, label, INVALID_TEXT_CODE);
    r#gen.emit(&format!("{label}_failed:"));
    fail_with_errno(r#gen, label, None);
}

/// Emits code that leaves in `r13` how many bytes the socket in `rbx` has received but not read
/// yet, and in `r14` where they start.
fn unread(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tr13, QWORD PTR [rbx + 40]");
    r#gen.emit("\tsub\tr13, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 24]");
    r#gen.emit("\tadd\tr14, QWORD PTR [rbx + 32]");
}

/// Emits `stone.net_read_line`, which returns `ok` of `some` of the next line without its `\n`
/// or `\r\n`, or `ok` of `none` once every line has been read.
fn read_line(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_line";
    // r12 is how far the bytes not read yet have been searched for a \n
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    check_open(r#gen, label);
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit(&format!("{label}_scan:"));
    unread(r#gen);
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit("\tcmp\tr12, r13");
    r#gen.emit(&format!("\tjae\t{label}_more"));
    r#gen.emit("\tcmp\tbyte ptr [r14 + r12], 10");
    r#gen.emit(&format!("\tje\t{label}_found"));
    r#gen.emit("\tinc\tr12");
    r#gen.emit(&format!("\tjmp\t{label}_next"));
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjnz\t{label}_scan"));
    // the other end closed the connection, so the rest is the last line, if any
    unread(r#gen);
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit(&format!("\tjz\t{label}_none"));
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit(&format!("\tjmp\t{label}_take"));
    r#gen.emit(&format!("{label}_found:"));
    r#gen.emit("\tlea\trdi, [r12 + 1]");
    r#gen.emit("\tmov\trsi, r12");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit(&format!("\tjz\t{label}_take"));
    r#gen.emit("\tcmp\tbyte ptr [r14 + r12 - 1], 13");
    r#gen.emit(&format!("\tjne\t{label}_take"));
    r#gen.emit("\tdec\trsi");
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tcall\t.Lnet_take");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjz\t{label}_invalid"));
    // some(line), tag 1
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\tedi, 1");
    r#gen.emit("\tmov\tesi, 1"); // a string slot
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tQWORD PTR [rax + 16], r12");
    r#gen.emit(&format!("\tjmp\t{label}_option"));
    // none, tag 0
    r#gen.emit(&format!("{label}_none:"));
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit(&format!("{label}_option:"));
    ok_handle(r#gen, label);
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read`, which returns `ok` of whatever has arrived, waiting for something if
/// nothing has, except the first bytes of a character whose last have not, or of `""` once the
/// other end has closed the connection, as `stdlib::net::complete_prefix` decides.
fn read(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read";
    // r15 is how many of the bytes not read yet end on a character boundary
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tmov\tr15, r13");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 56], 0");
    r#gen.emit(&format!("\tjne\t{label}_take"));
    // rcx counts back from the end over at most 3 bytes, the most an unfinished character has
    r#gen.emit("\tmov\trcx, 1");
    r#gen.emit(&format!("{label}_back:"));
    r#gen.emit("\tcmp\trcx, r13");
    r#gen.emit(&format!("\tja\t{label}_complete"));
    r#gen.emit("\tcmp\trcx, 3");
    r#gen.emit(&format!("\tja\t{label}_complete"));
    r#gen.emit("\tmov\trdx, r13");
    r#gen.emit("\tsub\trdx, rcx");
    r#gen.emit("\tmovzx\teax, byte ptr [r14 + rdx]");
    r#gen.emit("\tmov\tedx, eax");
    r#gen.emit("\tand\tedx, 192");
    r#gen.emit("\tcmp\tedx, 128");
    r#gen.emit(&format!("\tjne\t{label}_lead"));
    r#gen.emit("\tinc\trcx");
    r#gen.emit(&format!("\tjmp\t{label}_back"));
    // the character's width, from its first byte
    r#gen.emit(&format!("{label}_lead:"));
    r#gen.emit("\tmov\tedx, 1");
    for (bound, width) in [(192, 2), (224, 3), (240, 4)] {
        r#gen.emit(&format!("\tcmp\teax, {bound}"));
        r#gen.emit(&format!("\tjb\t{label}_width"));
        r#gen.emit(&format!("\tmov\tedx, {width}"));
    }
    r#gen.emit("\tcmp\teax, 248");
    r#gen.emit(&format!("\tjb\t{label}_width"));
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit(&format!("{label}_width:"));
    r#gen.emit("\tcmp\trdx, rcx");
    r#gen.emit(&format!("\tjbe\t{label}_complete"));
    r#gen.emit("\tsub\tr15, rcx");
    r#gen.emit(&format!("{label}_complete:"));
    r#gen.emit("\ttest\tr15, r15");
    r#gen.emit(&format!("\tjnz\t{label}_take"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjmp\t{label}_again"));
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tmov\trdi, r15");
    r#gen.emit("\tmov\trsi, r15");
    r#gen.emit("\tcall\t.Lnet_take");
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_all`, which reads until the other end closes the connection and
/// returns `ok` of everything that arrived.
fn read_all(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_all";
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdi");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tcall\t.Lnet_receive");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjnz\t{label}_more"));
    unread(r#gen);
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, r13");
    r#gen.emit("\tcall\t.Lnet_take");
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_exchange`: one UDP query to the address and the first reply with its ID, as
/// a list of bytes, waiting at most the milliseconds given, polling with what is left of them.
fn exchange(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_exchange";
    // rbx holds the name, r12 the packet sent, r13 the descriptor, r14 the reply buffer, and r15
    // the deadline in nanoseconds; [rbp - 96] holds the query, [rbp - 104] the milliseconds,
    // [rbp - 112] the error, and [rbp - 120] the reply's length
    frame(r#gen, label);
    r#gen.emit("\tmov\trbx, rdx");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 96], rcx");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 104], r8");
    r#gen.emit("\tmov\tr12, rdi");
    r#gen.emit("\tmov\tr13, rsi");
    socket_address(r#gen, label);
    // the query has an ID to fill in, and holds only bytes
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 96]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
    r#gen.emit("\tcmp\trcx, 2");
    r#gen.emit(&format!("\tjb\t{label}_invalid"));
    r#gen.emit("\tmov\trdi, QWORD PTR [rdi + 16]");
    r#gen.emit(&format!("{label}_check:"));
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tcmp\tQWORD PTR [rdi + rcx * 8], 255");
    r#gen.emit(&format!("\tja\t{label}_invalid"));
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjnz\t{label}_check"));
    // nothing to clean up yet
    r#gen.emit("\txor\tr12, r12");
    r#gen.emit("\tmov\tr13, -1");
    r#gen.emit("\txor\tr14, r14");
    r#gen.emit(&format!("\tjmp\t{label}_valid"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\trdi, {INVALID_ADDRESS_CODE}"));
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
    r#gen.emit(&format!("{label}_valid:"));
    // a random ID, as the interpreter reads it from /dev/urandom
    r#gen.emit("\tmov\tQWORD PTR [rbp - 88], 0");
    syscall(
        r#gen,
        &format!("{label}_random"),
        318, // sys_getrandom
        &["lea\trdi, [rbp - 88]", "mov\tesi, 2", "xor\tedx, edx"],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    // the packet: the query's bytes with the ID first
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 96]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rdi]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 96]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rdi]");
    r#gen.emit("\tmov\trdi, QWORD PTR [rdi + 16]");
    r#gen.emit(&format!("{label}_copy:"));
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + rcx * 8]");
    r#gen.emit("\tmov\tbyte ptr [r12 + rcx], al");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjnz\t{label}_copy"));
    r#gen.emit("\tmovzx\teax, byte ptr [rbp - 88]");
    r#gen.emit("\tmov\tbyte ptr [r12], al");
    r#gen.emit("\tmovzx\teax, byte ptr [rbp - 87]");
    r#gen.emit("\tmov\tbyte ptr [r12 + 1], al");
    r#gen.emit(&format!("\tmov\trdi, {MAX_REPLY}"));
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 80]");
    r#gen.emit("\tand\tedi, 255"); // the family
    r#gen.emit(&format!("\tmov\tesi, {}", 2 | SOCK_CLOEXEC)); // SOCK_DGRAM
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\teax, 41"); // sys_socket
    r#gen.emit("\tsyscall");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit("\tmov\tr13, rax");
    syscall(
        r#gen,
        &format!("{label}_connect"),
        42, // sys_connect
        &["mov\trdi, r13", "lea\trsi, [rbp - 80]", "mov\trdx, r15"],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    syscall(
        r#gen,
        &format!("{label}_send"),
        44, // sys_sendto
        &[
            "mov\trdi, r13",
            "mov\trsi, r12",
            "mov\trdx, QWORD PTR [rbp - 96]",
            "mov\trdx, QWORD PTR [rdx]",
            &format!("mov\tr10, {MSG_NOSIGNAL}"),
            "xor\tr8, r8",
            "xor\tr9, r9",
        ],
    );
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    // the deadline, from the monotonic clock
    now(r#gen);
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 104]");
    r#gen.emit("\timul\trax, rax, 1000000");
    r#gen.emit("\tadd\tr15, rax");
    r#gen.emit(&format!("{label}_wait:"));
    now(r#gen);
    r#gen.emit("\tmov\trcx, r15");
    r#gen.emit("\tsub\trcx, rax");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjle\t{label}_timeout"));
    // the milliseconds left, rounded up
    r#gen.emit("\tlea\trax, [rcx + 999999]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\trcx, 1000000");
    r#gen.emit("\tdiv\trcx");
    r#gen.emit("\tmov\trdx, rax");
    // struct pollfd { fd, events = POLLIN, revents }
    r#gen.emit("\tmov\trax, 1");
    r#gen.emit("\tshl\trax, 32");
    r#gen.emit("\tadd\trax, r13");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 88], rax");
    r#gen.emit("\tlea\trdi, [rbp - 88]");
    r#gen.emit("\tmov\tesi, 1");
    r#gen.emit("\tmov\teax, 7"); // sys_poll
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_wait"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    r#gen.emit(&format!("\tjz\t{label}_timeout"));
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit(&format!("\tmov\trdx, {MAX_REPLY}"));
    r#gen.emit("\txor\tr10, r10");
    r#gen.emit("\txor\tr8, r8");
    r#gen.emit("\txor\tr9, r9");
    r#gen.emit("\tmov\teax, 45"); // sys_recvfrom
    r#gen.emit("\tsyscall");
    r#gen.emit(&format!("\tcmp\trax, -{EINTR}"));
    r#gen.emit(&format!("\tje\t{label}_wait"));
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit(&format!("\tjs\t{label}_failed"));
    // another query's reply, or a stray packet, is not the answer
    r#gen.emit("\tcmp\trax, 2");
    r#gen.emit(&format!("\tjb\t{label}_wait"));
    for offset in [0, 1] {
        r#gen.emit(&format!("\tmovzx\tecx, byte ptr [r14 + {offset}]"));
        r#gen.emit(&format!("\tmovzx\tedx, byte ptr [r12 + {offset}]"));
        r#gen.emit("\tcmp\tecx, edx");
        r#gen.emit(&format!("\tjne\t{label}_wait"));
    }
    r#gen.emit("\tmov\tQWORD PTR [rbp - 120], rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit("\tcall\tstone.list_new");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbp - 120]");
    r#gen.emit("\tmov\trdi, QWORD PTR [r15 + 16]");
    r#gen.emit(&format!("{label}_fill:"));
    r#gen.emit("\tdec\trcx");
    r#gen.emit("\tmovzx\teax, byte ptr [r14 + rcx]");
    r#gen.emit("\tmov\tQWORD PTR [rdi + rcx * 8], rax");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjnz\t{label}_fill"));
    cleanup(r#gen, label, "replied");
    r#gen.emit("\tmov\trdi, r15");
    r#gen.emit("\tmov\tesi, 2"); // a list slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit(&format!("\tjmp\t{label}_done"));
    r#gen.emit(&format!("{label}_timeout:"));
    r#gen.emit(&format!("\tmov\tQWORD PTR [rbp - 112], {TIMED_OUT_CODE}"));
    r#gen.emit(&format!("\tjmp\t{label}_fail"));
    r#gen.emit(&format!("{label}_failed:"));
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tQWORD PTR [rbp - 112], rax");
    r#gen.emit(&format!("{label}_fail:"));
    cleanup(r#gen, label, "failing");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbp - 112]");
    r#gen.emit("\tmov\trsi, rbx");
    r#gen.emit(&format!("\tjmp\t{label}_err"));
    frame_end(r#gen, label);
}

/// Emits code that leaves the monotonic clock's time in nanoseconds in `rax`, using the address
/// slots at `[rbp - 80]`, which are not needed once a socket is connected.
fn now(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tedi, 1"); // CLOCK_MONOTONIC
    r#gen.emit("\tlea\trsi, [rbp - 80]");
    r#gen.emit("\tmov\teax, 228"); // sys_clock_gettime
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\trax, QWORD PTR [rbp - 80]");
    r#gen.emit("\timul\trax, rax, 1000000000");
    r#gen.emit("\tadd\trax, QWORD PTR [rbp - 72]");
}

/// Emits code that closes the descriptor in `r13` unless it is -1 and frees the buffers in `r12`
/// and `r14` unless they are 0, for `stone.net_exchange` on its way out, with labels named after
/// `way`.
fn cleanup(r#gen: &mut dyn AssemblyGenerator, label: &str, way: &str) {
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit(&format!("\tjs\t{label}_{way}_closed"));
    close(r#gen, "r13");
    r#gen.emit(&format!("{label}_{way}_closed:"));
    for reg in ["r12", "r14"] {
        r#gen.emit(&format!("\ttest\t{reg}, {reg}"));
        r#gen.emit(&format!("\tjz\t{label}_{way}_{reg}"));
        r#gen.emit(&format!("\tmov\trax, {reg}"));
        r#gen.emit("\tcall\tstone.mem_free");
        r#gen.emit(&format!("{label}_{way}_{reg}:"));
    }
}

/// Emits `.Lfree_handle`, which `stone.free_enum` calls for a handle in `rax` whose count reached
/// 0: it closes the descriptor unless closed already, and frees a socket's buffer, preserving
/// every register except `rcx` and `rdx`, as the free routines must.
pub fn free_handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfree_handle:");
    r#gen.emit("\tpush\trax");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\tr11");
    r#gen.emit("\tmov\trdi, QWORD PTR [rax + 16]");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjs\t.Lfree_handle_closed");
    r#gen.emit("\tmov\teax, 3"); // sys_close
    r#gen.emit("\tsyscall");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 16]");
    r#gen.emit(".Lfree_handle_closed:");
    r#gen.emit(&format!("\tcmp\tQWORD PTR [rax], {SOCKET_TAG}"));
    r#gen.emit("\tjne\t.Lfree_handle_done");
    r#gen.emit("\tmov\trax, QWORD PTR [rax + 24]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lfree_handle_done");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lfree_handle_done:");
    r#gen.emit("\tpop\tr11");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tpop\trax");
    r#gen.emit("\tret");
}
