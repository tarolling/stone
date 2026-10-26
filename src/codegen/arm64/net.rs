//! The arm64 runtime of the `net_sys` module and of the methods of sockets and listeners, which
//! mirrors `x64/net.rs` routine for routine, under the same labels, contracts, and handle layout,
//! with arguments in `x0` and up and results in `x0`.
//!
//! Every call into the system goes through [`system_call`], so on macOS each is a `libSystem`
//! function with Linux's contract. Where the systems differ: macOS writes a socket address's
//! length before its family and numbers IPv6 30, has no `SOCK_CLOEXEC`, `accept4`, or
//! `MSG_NOSIGNAL`, so its routines mark each descriptor close-on-exec with `fcntl` and set
//! `SO_NOSIGPIPE` on each socket instead, names `SOL_SOCKET` and `SO_REUSEADDR` differently, and
//! waits with `poll`'s milliseconds where Linux's `ppoll` takes a `timespec`.

use super::builtins::{Sys, system_call};
use super::fs::{EINTR, close, load};
use super::{pop_frame, push_frame};
use crate::codegen::context::{
    CLOSED_EARLY_CODE, INVALID_ADDRESS_CODE, INVALID_TEXT_CODE, LISTENER_CLOSED_CODE,
    SOCKET_CLOSED_CODE, TIMED_OUT_CODE,
};
use crate::codegen::x64::net::{LISTENER_TAG, SOCKET_TAG};
use crate::codegen::{AssemblyGenerator, Os};
use crate::stdlib::net::{MAX_REPLY, READ_SIZE};

/// The registers every routine keeps its values in: `x19` holds the name an error quotes, or
/// the handle a method is called on.
const SAVED: [&str; 8] = ["x19", "x20", "x21", "x22", "x23", "x24", "x25", "x26"];

/// The bytes each routine keeps below its saved registers: an address at `[sp]`, and scratch
/// slots at `[sp, #32]` through `[sp, #56]`.
const LOCALS: i64 = 64;

/// The slots of a handle after its tag and kinds.
const HANDLE_SLOTS: i64 = 7;

/// The bytes the buffer of a socket starts with, room for two reads.
const FIRST_BUFFER: usize = 2 * READ_SIZE;

/// The constants of the sockets interface, which differ by system.
struct Constants {
    inet6: i64,
    /// `SOCK_CLOEXEC`, which macOS lacks.
    cloexec: i64,
    sol_socket: i64,
    reuse_address: i64,
    /// `MSG_NOSIGNAL`, which macOS lacks.
    no_signal: i64,
    /// `MSG_DONTWAIT`, which a send with a timeout passes.
    dont_wait: i64,
    monotonic: i64,
    /// The error numbers of a send that would wait and of a connection started but not finished.
    again: i64,
    in_progress: i64,
    nonblock: i64,
    so_error: i64,
}

impl Constants {
    fn of(os: Os) -> Constants {
        match os {
            Os::Linux => Constants {
                inet6: 10,
                cloexec: 0x80000,
                sol_socket: 1,
                reuse_address: 2,
                no_signal: 0x4000,
                dont_wait: 0x40,
                monotonic: 1,
                again: 11,
                in_progress: 115,
                nonblock: 0x800,
                so_error: 4,
            },
            Os::MacOs => Constants {
                inet6: 30,
                cloexec: 0,
                sol_socket: 0xffff,
                reuse_address: 4,
                no_signal: 0,
                dont_wait: 0x80,
                monotonic: 6,
                again: 35,
                in_progress: 36,
                nonblock: 4,
                so_error: 0x1007,
            },
        }
    }
}

/// macOS's `SO_NOSIGPIPE`, which makes writing to a connection the other end closed fail with
/// `EPIPE` instead of killing the program with `SIGPIPE`, as Linux's `MSG_NOSIGNAL` does.
const SO_NOSIGPIPE: i64 = 0x1022;

/// Emits the routines `used` lists, as `x64::net::net_runtime` describes them.
pub fn net_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    handle(r#gen);
    wait(r#gen);
    if r#gen.target().os == Os::MacOs {
        prepare(r#gen);
    }
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
    if uses("stone.net_connect_unix") {
        connect_unix(r#gen);
    }
    if uses("stone.net_ready") {
        ready(r#gen);
    }
    if uses("stone.net_exchange") {
        exchange(r#gen);
    }
    if uses("stone.net_accept") {
        accept(r#gen);
    }
    if uses("stone.net_port") {
        r#gen.emit("stone.net_port:");
        r#gen.emit("\tldr\tx0, [x0, #24]");
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

/// Emits the start of a routine that saves [`SAVED`] and makes room for [`LOCALS`].
fn frame(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}:"));
    push_frame(r#gen, &SAVED);
    r#gen.emit(&format!("\tsub\tsp, sp, #{LOCALS}"));
}

/// Emits the end of a routine [`frame`] began: `<label>_err`, which makes an `err` of the code in
/// `x0` about the name in `x1`, or of the reason alone if `x1` is 0, and `<label>_done`, which
/// returns `x0`.
fn frame_end(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_err:"));
    r#gen.emit("\tbl\t.Lfs_err");
    r#gen.emit(&format!("{label}_done:"));
    pop_frame(r#gen, &SAVED);
}

/// Emits a jump to `<label>_err` for the negative error number in `x0`, about the name in
/// `subject`, a register, or with the reason alone if it is `None`.
fn fail_with_errno(r#gen: &mut dyn AssemblyGenerator, label: &str, subject: Option<&str>) {
    r#gen.emit("\tneg\tx0, x0");
    match subject {
        Some(reg) => r#gen.emit(&format!("\tmov\tx1, {reg}")),
        None => r#gen.emit("\tmov\tx1, #0"),
    }
    r#gen.emit(&format!("\tb\t{label}_err"));
}

/// Emits a jump to `<label>_err` for the error `code` with the reason alone.
fn fail_with_code(r#gen: &mut dyn AssemblyGenerator, label: &str, code: i64) {
    r#gen.emit(&format!("\tmov\tx0, #{code}"));
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(&format!("\tb\t{label}_err"));
}

/// Emits `call`, made again while a signal interrupts it, after `setup` puts its arguments in
/// place each time, starting at the label `again`.
fn retried(r#gen: &mut dyn AssemblyGenerator, again: &str, call: Sys, setup: &[&str]) {
    r#gen.emit(&format!("{again}:"));
    for line in setup {
        r#gen.emit(&format!("\t{line}"));
    }
    system_call(r#gen, call);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{again}"));
}

/// Emits code that writes the address list in `x20` and the port in `x21` as a socket address
/// at `[sp]`, leaving its family in `x22` and its length in `x23`, or jumps to `<label>_invalid`
/// if the list is not 4 or 16 bytes or the port is not from 0 to 65535.
fn socket_address(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    let os = r#gen.target().os;
    let constants = Constants::of(os);
    // unsigned, so a negative port is out of range too
    load(r#gen, "x9", 65535);
    r#gen.emit("\tcmp\tx21, x9");
    r#gen.emit(&format!("\tb.hi\t{label}_invalid"));
    for offset in [0, 8, 16, 24] {
        r#gen.emit(&format!("\tstr\txzr, [sp, #{offset}]"));
    }
    // the port, most significant byte first
    r#gen.emit("\tstrb\tw21, [sp, #3]");
    r#gen.emit("\tlsr\tx9, x21, #8");
    r#gen.emit("\tstrb\tw9, [sp, #2]");
    r#gen.emit("\tldr\tx10, [x20]");
    r#gen.emit("\tadd\tx11, sp, #4");
    r#gen.emit("\tmov\tx22, #2"); // AF_INET
    r#gen.emit("\tmov\tx23, #16");
    r#gen.emit("\tcmp\tx10, #4");
    r#gen.emit(&format!("\tb.eq\t{label}_bytes"));
    r#gen.emit("\tadd\tx11, sp, #8");
    r#gen.emit(&format!("\tmov\tx22, #{}", constants.inet6));
    r#gen.emit("\tmov\tx23, #28");
    r#gen.emit("\tcmp\tx10, #16");
    r#gen.emit(&format!("\tb.ne\t{label}_invalid"));
    r#gen.emit(&format!("{label}_bytes:"));
    r#gen.emit("\tldr\tx12, [x20, #16]");
    r#gen.emit(&format!("{label}_byte:"));
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tldr\tx9, [x12, x10, lsl #3]");
    r#gen.emit("\tcmp\tx9, #255");
    r#gen.emit(&format!("\tb.hi\t{label}_invalid"));
    r#gen.emit("\tstrb\tw9, [x11, x10]");
    r#gen.emit(&format!("\tcbnz\tx10, {label}_byte"));
    match os {
        Os::Linux => r#gen.emit("\tstrb\tw22, [sp]"),
        // the length comes first, then the family
        Os::MacOs => {
            r#gen.emit("\tstrb\tw23, [sp]");
            r#gen.emit("\tstrb\tw22, [sp, #1]");
        }
    }
}

/// Emits `.Lnet_handle`, which returns in `x0` a new handle of the descriptor `x0` with the tag
/// `x1` and its other slots 0.
fn handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_handle:");
    push_frame(r#gen, &["x19", "x20"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx0, x1");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit(&format!("\tmov\tx2, #{HANDLE_SLOTS}"));
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit("\tstr\tx19, [x0, #16]");
    for offset in [24, 32, 40, 48, 56] {
        r#gen.emit(&format!("\tstr\txzr, [x0, #{offset}]"));
    }
    r#gen.emit("\tmov\tx9, #-1");
    r#gen.emit("\tstr\tx9, [x0, #64]");
    pop_frame(r#gen, &["x19", "x20"]);
}

/// Emits `.Lnet_prepare`, which macOS calls on each new descriptor `x0`: it marks it
/// close-on-exec, as `SOCK_CLOEXEC` would, and sets `SO_NOSIGPIPE`, as Linux's routines pass
/// `MSG_NOSIGNAL`, returning 0 or a negative error number in `x0`.
fn prepare(r#gen: &mut dyn AssemblyGenerator) {
    let constants = Constants::of(Os::MacOs);
    r#gen.emit(".Lnet_prepare:");
    push_frame(r#gen, &["x19", "x20"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx1, #2"); // F_SETFD
    r#gen.emit("\tmov\tx2, #1"); // FD_CLOEXEC
    system_call(r#gen, Sys::Fcntl);
    r#gen.emit("\ttbnz\tx0, #63, .Lnet_prepare_done");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x19");
    load(r#gen, "x1", constants.sol_socket);
    load(r#gen, "x2", SO_NOSIGPIPE);
    r#gen.emit("\tmov\tx3, sp");
    r#gen.emit("\tmov\tx4, #4");
    system_call(r#gen, Sys::Setsockopt);
    r#gen.emit("\tadd\tsp, sp, #16");
    r#gen.emit(".Lnet_prepare_done:");
    pop_frame(r#gen, &["x19", "x20"]);
}

/// Emits code that opens a socket of the family in `x22` and the type `kind` (1 for a stream, 2
/// for datagrams) into `x20`, or fails with the error about the name in `x19`, and the jump
/// target `<label>_invalid` for an address [`socket_address`] rejects.
fn open_socket(r#gen: &mut dyn AssemblyGenerator, label: &str, kind: i64) {
    let os = r#gen.target().os;
    let constants = Constants::of(os);
    r#gen.emit(&format!("\tb\t{label}_valid"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\tx0, #{INVALID_ADDRESS_CODE}"));
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit(&format!("\tb\t{label}_err"));
    r#gen.emit(&format!("{label}_valid:"));
    r#gen.emit("\tmov\tx0, x22");
    load(r#gen, "x1", kind | constants.cloexec);
    r#gen.emit("\tmov\tx2, #0");
    system_call(r#gen, Sys::Socket);
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_opened"));
    fail_with_errno(r#gen, label, Some("x19"));
    r#gen.emit(&format!("{label}_opened:"));
    r#gen.emit("\tmov\tx20, x0");
    if os == Os::MacOs {
        r#gen.emit("\tbl\t.Lnet_prepare");
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    }
}

/// Emits the return of `ok` of the new handle, or option, in `x0`.
fn ok_handle(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tmov\tx1, #3"); // an enum value's slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
}

/// Emits `<label>_close`, which closes the descriptor in `x20` and fails with the negative error
/// number in `x0` about the name in `x19`.
fn closing_failure(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_close:"));
    r#gen.emit("\tmov\tx21, x0");
    close(r#gen, "x20");
    r#gen.emit("\tmov\tx0, x21");
    fail_with_errno(r#gen, label, Some("x19"));
}

/// Emits `stone.net_connect`, as `x64::net` does.
fn connect(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_connect";
    let constants = Constants::of(r#gen.target().os);
    // x19 holds the name, x20 the address and then the descriptor, x21 the port, x24 the
    // timeout, and x25 the descriptor's flags
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x2");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tmov\tx24, x3");
    socket_address(r#gen, label);
    open_socket(r#gen, label, 1);
    r#gen.emit(&format!("\ttbnz\tx24, #63, {label}_connect"));
    fcntl(r#gen, 3, "mov\tx2, #0"); // F_GETFL
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit("\tmov\tx25, x0");
    load(r#gen, "x9", constants.nonblock);
    r#gen.emit("\torr\tx2, x0, x9");
    fcntl(r#gen, 4, "mov\tx2, x2"); // F_SETFL
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    retried(
        r#gen,
        &format!("{label}_connect"),
        Sys::Connect,
        &["mov\tx0, x20", "mov\tx1, sp", "mov\tx2, x23"],
    );
    r#gen.emit(&format!("\tcbz\tx0, {label}_blocking"));
    r#gen.emit(&format!("\tcmn\tx0, #{}", constants.in_progress));
    r#gen.emit(&format!("\tb.ne\t{label}_close"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, #4"); // POLLOUT
    r#gen.emit("\tmov\tx2, x24");
    r#gen.emit("\tbl\t.Lnet_wait");
    r#gen.emit(&format!("\tcbnz\tx0, {label}_close"));
    // how the connection went
    r#gen.emit("\tstr\txzr, [sp, #32]");
    r#gen.emit("\tmov\tx9, #4");
    r#gen.emit("\tstr\tx9, [sp, #40]");
    r#gen.emit("\tmov\tx0, x20");
    load(r#gen, "x1", constants.sol_socket);
    load(r#gen, "x2", constants.so_error);
    r#gen.emit("\tadd\tx3, sp, #32");
    r#gen.emit("\tadd\tx4, sp, #40");
    system_call(r#gen, Sys::Getsockopt);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit("\tldr\tw9, [sp, #32]");
    r#gen.emit("\tneg\tx0, x9");
    r#gen.emit(&format!("\tcbnz\tx0, {label}_close"));
    r#gen.emit(&format!("{label}_blocking:"));
    r#gen.emit(&format!("\ttbnz\tx24, #63, {label}_connected"));
    fcntl(r#gen, 4, "mov\tx2, x25"); // F_SETFL
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit(&format!("{label}_connected:"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit(&format!("\tmov\tx1, #{SOCKET_TAG}"));
    r#gen.emit("\tbl\t.Lnet_handle");
    ok_handle(r#gen, label);
    closing_failure(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_connect_unix`, as `x64::net` does, for the path in `x0` and the name in `x1`.
/// A macOS `sockaddr_un` holds 104 bytes of path where Linux's holds 108, and starts with its
/// length.
fn connect_unix(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_connect_unix";
    let os = r#gen.target().os;
    let longest = match os {
        Os::Linux => 107,
        Os::MacOs => 103,
    };
    // x19 holds the name, x20 the path and then the descriptor, x21 the path's length, x22 the
    // family, x23 the address's length, and the `sockaddr_un` is at [sp]
    frame(r#gen, label);
    r#gen.emit("	sub	sp, sp, #112");
    r#gen.emit("	mov	x19, x1");
    r#gen.emit("	mov	x20, x0");
    r#gen.emit("	bl	stone.str_len");
    r#gen.emit("	mov	x21, x0");
    r#gen.emit(&format!("	cmp	x21, #{longest}"));
    r#gen.emit(&format!("	b.hi	{label}_invalid"));
    for offset in (0..112).step_by(16) {
        r#gen.emit(&format!("	stp	xzr, xzr, [sp, #{offset}]"));
    }
    r#gen.emit("	mov	x22, #1"); // AF_UNIX
    r#gen.emit("	add	x23, x21, #3");
    match os {
        Os::Linux => r#gen.emit("	strb	w22, [sp]"),
        Os::MacOs => {
            r#gen.emit("	strb	w23, [sp]");
            r#gen.emit("	strb	w22, [sp, #1]");
        }
    }
    r#gen.emit("	mov	x10, #0");
    r#gen.emit(&format!("{label}_copy:"));
    r#gen.emit("	cmp	x10, x21");
    r#gen.emit(&format!("	b.hs	{label}_copied"));
    r#gen.emit("	ldrb	w9, [x20, x10]");
    r#gen.emit("	add	x11, sp, x10");
    r#gen.emit("	strb	w9, [x11, #2]");
    r#gen.emit("	add	x10, x10, #1");
    r#gen.emit(&format!("	b	{label}_copy"));
    r#gen.emit(&format!("{label}_copied:"));
    open_socket(r#gen, label, 1);
    retried(
        r#gen,
        &format!("{label}_connect"),
        Sys::Connect,
        &["mov	x0, x20", "mov	x1, sp", "mov	x2, x23"],
    );
    r#gen.emit(&format!("	tbnz	x0, #63, {label}_close"));
    r#gen.emit("	mov	x0, x20");
    r#gen.emit(&format!("	mov	x1, #{SOCKET_TAG}"));
    r#gen.emit("	bl	.Lnet_handle");
    ok_handle(r#gen, label);
    closing_failure(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_ready`, as `x64::net` does, for the socket in `x0` and the milliseconds in
/// `x1`: through `ppoll` with a `timespec` of them, or none for -1, on Linux, and `poll` on
/// macOS.
fn ready(r#gen: &mut dyn AssemblyGenerator) {
    let os = r#gen.target().os;
    let saved = ["x19", "x20"];
    // x19 holds the descriptor and x20 the milliseconds; the pollfd is at [sp, #16], and the
    // timespec at [sp]
    r#gen.emit("stone.net_ready:");
    r#gen.emit("	ldr	x9, [x0, #16]");
    r#gen.emit("	tbnz	x9, #63, .Lnet_ready_yes");
    r#gen.emit("	ldr	x10, [x0, #40]");
    r#gen.emit("	ldr	x11, [x0, #32]");
    r#gen.emit("	cmp	x10, x11");
    r#gen.emit("	b.ne	.Lnet_ready_yes");
    r#gen.emit("	ldr	x10, [x0, #56]");
    r#gen.emit("	cbnz	x10, .Lnet_ready_yes");
    push_frame(r#gen, &saved);
    r#gen.emit("	sub	sp, sp, #32");
    r#gen.emit("	mov	x19, x9");
    load(r#gen, "x10", 2147483647);
    r#gen.emit("	cmp	x1, x10");
    r#gen.emit("	csel	x20, x10, x1, gt");
    r#gen.emit(".Lnet_ready_again:");
    // struct pollfd { fd, events, revents }
    r#gen.emit("	str	w19, [sp, #16]");
    r#gen.emit("	mov	w9, #1"); // POLLIN
    r#gen.emit("	str	w9, [sp, #20]");
    r#gen.emit("	add	x0, sp, #16");
    r#gen.emit("	mov	x1, #1");
    match os {
        Os::Linux => {
            r#gen.emit("	mov	x2, #0");
            r#gen.emit("	tbnz	x20, #63, .Lnet_ready_poll");
            r#gen.emit("	mov	x9, #1000");
            r#gen.emit("	udiv	x10, x20, x9");
            r#gen.emit("	msub	x11, x10, x9, x20");
            load(r#gen, "x9", 1_000_000);
            r#gen.emit("	mul	x11, x11, x9");
            r#gen.emit("	stp	x10, x11, [sp]");
            r#gen.emit("	mov	x2, sp");
            r#gen.emit(".Lnet_ready_poll:");
            r#gen.emit("	mov	x3, #0");
            r#gen.emit("	mov	x4, #0");
        }
        Os::MacOs => r#gen.emit("	mov	x2, x20"),
    }
    system_call(r#gen, Sys::Ppoll);
    r#gen.emit(&format!("	cmn	x0, #{EINTR}"));
    r#gen.emit("	b.eq	.Lnet_ready_again");
    r#gen.emit("	cmp	x0, #0");
    r#gen.emit("	cset	x0, ne");
    pop_frame(r#gen, &saved);
    r#gen.emit(".Lnet_ready_yes:");
    r#gen.emit("	mov	x0, #1");
    r#gen.emit("	ret");
}

/// Emits an `fcntl` call with the command `command` on the descriptor in `x20`, after `third`
/// puts its argument in `x2`, leaving the result in `x0`.
fn fcntl(r#gen: &mut dyn AssemblyGenerator, command: i64, third: &str) {
    r#gen.emit(&format!("\t{third}"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit(&format!("\tmov\tx1, #{command}"));
    system_call(r#gen, Sys::Fcntl);
}

/// Emits `stone.net_listen`, as `x64::net` does.
fn listen(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_listen";
    let constants = Constants::of(r#gen.target().os);
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x2");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    socket_address(r#gen, label);
    open_socket(r#gen, label, 1);
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [sp, #32]");
    r#gen.emit("\tmov\tx0, x20");
    load(r#gen, "x1", constants.sol_socket);
    r#gen.emit(&format!("\tmov\tx2, #{}", constants.reuse_address));
    r#gen.emit("\tadd\tx3, sp, #32");
    r#gen.emit("\tmov\tx4, #4");
    system_call(r#gen, Sys::Setsockopt);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, sp");
    r#gen.emit("\tmov\tx2, x23");
    system_call(r#gen, Sys::Bind);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, #128");
    system_call(r#gen, Sys::Listen);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    // the port the system picked, if the address asked for 0
    r#gen.emit("\tmov\tx9, #32");
    r#gen.emit("\tstr\tx9, [sp, #32]");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, sp");
    r#gen.emit("\tadd\tx2, sp, #32");
    system_call(r#gen, Sys::Getsockname);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_close"));
    r#gen.emit("\tldrb\tw9, [sp, #2]");
    r#gen.emit("\tldrb\tw10, [sp, #3]");
    r#gen.emit("\tadd\tx21, x10, x9, lsl #8");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit(&format!("\tmov\tx1, #{LISTENER_TAG}"));
    r#gen.emit("\tbl\t.Lnet_handle");
    r#gen.emit("\tstr\tx21, [x0, #24]");
    ok_handle(r#gen, label);
    closing_failure(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_accept`, as `x64::net` does.
fn accept(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_accept";
    let os = r#gen.target().os;
    let constants = Constants::of(os);
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx20, [x0, #16]");
    r#gen.emit(&format!("\ttbz\tx20, #63, {label}_open"));
    fail_with_code(r#gen, label, LISTENER_CLOSED_CODE);
    r#gen.emit(&format!("{label}_open:"));
    wait_for(r#gen, label, "x20", 1, &format!("{label}_again"));
    let mut setup = vec![
        "mov\tx0, x20".to_string(),
        "mov\tx1, #0".to_string(),
        "mov\tx2, #0".to_string(),
    ];
    if os == Os::Linux {
        setup.push(format!("mov\tx3, #{}", constants.cloexec));
    }
    let setup: Vec<&str> = setup.iter().map(String::as_str).collect();
    retried(r#gen, &format!("{label}_again"), Sys::Accept, &setup);
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_accepted"));
    fail_with_errno(r#gen, label, None);
    r#gen.emit(&format!("{label}_accepted:"));
    if os == Os::MacOs {
        r#gen.emit("\tmov\tx21, x0");
        r#gen.emit("\tbl\t.Lnet_prepare");
        r#gen.emit(&format!("\ttbz\tx0, #63, {label}_prepared"));
        r#gen.emit("\tmov\tx22, x0");
        close(r#gen, "x21");
        r#gen.emit("\tmov\tx0, x22");
        fail_with_errno(r#gen, label, None);
        r#gen.emit(&format!("{label}_prepared:"));
        r#gen.emit("\tmov\tx0, x21");
    }
    r#gen.emit(&format!("\tmov\tx1, #{SOCKET_TAG}"));
    r#gen.emit("\tbl\t.Lnet_handle");
    ok_handle(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_close`, as `x64::net` does.
fn close_handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.net_close:");
    push_frame(r#gen, &["x19", "x20"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\ttbnz\tx0, #63, .Lnet_close_done");
    system_call(r#gen, Sys::Close);
    r#gen.emit("\tmov\tx9, #-1");
    r#gen.emit("\tstr\tx9, [x19, #16]");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit(&format!("\tcmn\tx9, #{}", -SOCKET_TAG));
    r#gen.emit("\tb.ne\t.Lnet_close_done");
    r#gen.emit("\tldr\tx9, [x19, #24]");
    r#gen.emit("\tcbz\tx9, .Lnet_close_done");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tstr\txzr, [x19, #24]");
    r#gen.emit(".Lnet_close_done:");
    r#gen.emit("\tmov\tx0, #0");
    pop_frame(r#gen, &["x19", "x20"]);
}

/// Emits a jump to `<label>_closed`'s error unless the socket in `x19` is open.
fn check_open(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit(&format!("\ttbz\tx9, #63, {label}_open"));
    fail_with_code(r#gen, label, SOCKET_CLOSED_CODE);
    r#gen.emit(&format!("{label}_open:"));
}

/// Emits `stone.net_send`, as `x64::net` does.
fn send(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_send";
    let constants = Constants::of(r#gen.target().os);
    // x20 holds the string, x21 the descriptor, x22 the length, and x23 the bytes sent
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    check_open(r#gen, label);
    r#gen.emit("\tldr\tx21, [x19, #16]");
    r#gen.emit("\tldur\tx22, [x20, #-16]");
    r#gen.emit("\tmov\tx23, #0");
    // with a timeout, a send never waits but in .Lnet_wait; x24 holds the flags
    load(r#gen, "x24", constants.no_signal);
    r#gen.emit("\tldr\tx9, [x19, #64]");
    r#gen.emit(&format!("\ttbnz\tx9, #63, {label}_more"));
    load(r#gen, "x24", constants.no_signal | constants.dont_wait);
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tcmp\tx23, x22");
    r#gen.emit(&format!("\tb.hs\t{label}_sent"));
    wait_for(r#gen, label, "x21", 4, &format!("{label}_again"));
    retried(
        r#gen,
        &format!("{label}_again"),
        Sys::Sendto,
        &[
            "mov\tx0, x21",
            "add\tx1, x20, x23",
            "sub\tx2, x22, x23",
            "mov\tx3, x24",
            "mov\tx4, #0",
            "mov\tx5, #0",
        ],
    );
    r#gen.emit(&format!("\tcmn\tx0, #{}", constants.again));
    r#gen.emit(&format!("\tb.eq\t{label}_more"));
    r#gen.emit(&format!("\ttbz\tx0, #63, {label}_wrote"));
    fail_with_errno(r#gen, label, None);
    r#gen.emit(&format!("{label}_wrote:"));
    r#gen.emit("\tadd\tx23, x23, x0");
    r#gen.emit(&format!("\tb\t{label}_more"));
    r#gen.emit(&format!("{label}_sent:"));
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    frame_end(r#gen, label);
}

/// Emits `.Lnet_receive`, as `x64::net` does, for the socket in `x19`, returning in `x0` and
/// preserving `x19` through `x28`.
fn receive(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_receive:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tldr\tx9, [x19, #56]");
    r#gen.emit("\tcbnz\tx9, .Lnet_receive_ended");
    r#gen.emit("\tldr\tx9, [x19, #24]");
    r#gen.emit("\tcbnz\tx9, .Lnet_receive_buffer");
    load(r#gen, "x0", FIRST_BUFFER as i64);
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tstr\tx0, [x19, #24]");
    load(r#gen, "x9", FIRST_BUFFER as i64);
    r#gen.emit("\tstr\tx9, [x19, #48]");
    r#gen.emit(".Lnet_receive_buffer:");
    // the bytes not read yet move to the start
    r#gen.emit("\tldr\tx10, [x19, #32]");
    r#gen.emit("\tcbz\tx10, .Lnet_receive_room");
    r#gen.emit("\tldr\tx11, [x19, #40]");
    r#gen.emit("\tsub\tx11, x11, x10");
    r#gen.emit("\tstr\tx11, [x19, #40]");
    r#gen.emit("\tstr\txzr, [x19, #32]");
    r#gen.emit("\tldr\tx9, [x19, #24]");
    r#gen.emit("\tadd\tx10, x9, x10");
    r#gen.emit("\tcbz\tx11, .Lnet_receive_room");
    r#gen.emit(".Lnet_receive_move:");
    r#gen.emit("\tldrb\tw12, [x10], #1");
    r#gen.emit("\tstrb\tw12, [x9], #1");
    r#gen.emit("\tsub\tx11, x11, #1");
    r#gen.emit("\tcbnz\tx11, .Lnet_receive_move");
    r#gen.emit(".Lnet_receive_room:");
    r#gen.emit("\tldr\tx9, [x19, #48]");
    r#gen.emit("\tldr\tx10, [x19, #40]");
    r#gen.emit("\tsub\tx9, x9, x10");
    load(r#gen, "x10", READ_SIZE as i64);
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.hs\t.Lnet_receive_wait");
    // doubling makes room, since the bytes kept are at most the old size
    r#gen.emit("\tldr\tx1, [x19, #48]");
    r#gen.emit("\tadd\tx1, x1, x1");
    r#gen.emit("\tstr\tx1, [x19, #48]");
    r#gen.emit("\tldr\tx0, [x19, #24]");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x19, #24]");
    r#gen.emit(".Lnet_receive_wait:");
    r#gen.emit("\tldr\tx2, [x19, #64]");
    r#gen.emit("\ttbnz\tx2, #63, .Lnet_receive_again");
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\tmov\tx1, #1"); // POLLIN
    r#gen.emit("\tbl\t.Lnet_wait");
    r#gen.emit("\tcbnz\tx0, .Lnet_receive_done");
    let size = format!("mov\tx2, #{READ_SIZE}");
    retried(
        r#gen,
        ".Lnet_receive_again",
        Sys::Recvfrom,
        &[
            "ldr\tx0, [x19, #16]",
            "ldr\tx1, [x19, #24]",
            "ldr\tx9, [x19, #40]",
            "add\tx1, x1, x9",
            &size,
            "mov\tx3, #0",
            "mov\tx4, #0",
            "mov\tx5, #0",
        ],
    );
    r#gen.emit("\ttbnz\tx0, #63, .Lnet_receive_done");
    r#gen.emit("\tcbnz\tx0, .Lnet_receive_got");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [x19, #56]");
    r#gen.emit(".Lnet_receive_got:");
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tadd\tx9, x9, x0");
    r#gen.emit("\tstr\tx9, [x19, #40]");
    r#gen.emit(".Lnet_receive_done:");
    pop_frame(r#gen, &[]);
    r#gen.emit(".Lnet_receive_ended:");
    r#gen.emit("\tmov\tx0, #0");
    pop_frame(r#gen, &[]);
}

/// Emits `.Lnet_take`, as `x64::net` does, taking the first `x0` bytes not read yet from the
/// socket in `x19` and returning a new string of the first `x1` of them, or 0 if those are not
/// text, preserving `x19` through `x28`.
fn take(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_take:");
    push_frame(r#gen, &["x20", "x21"]);
    r#gen.emit("\tldr\tx20, [x19, #24]");
    r#gen.emit("\tldr\tx9, [x19, #32]");
    r#gen.emit("\tadd\tx20, x20, x9");
    r#gen.emit("\tadd\tx9, x9, x0");
    r#gen.emit("\tstr\tx9, [x19, #32]");
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\t.Lfs_utf8");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tcbz\tx9, .Lnet_take_done");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit(".Lnet_take_done:");
    pop_frame(r#gen, &["x20", "x21"]);
}

/// Emits `.Lnet_take_bytes`, as `x64::net` does, taking the first `x0` bytes not read yet from
/// the socket in `x19` as new bytes, preserving `x19` through `x28`.
fn take_bytes(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lnet_take_bytes:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tldr\tx9, [x19, #32]");
    r#gen.emit("\tadd\tx10, x9, x0");
    r#gen.emit("\tstr\tx10, [x19, #32]");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tldr\tx0, [x19, #24]");
    r#gen.emit("\tadd\tx0, x0, x9");
    r#gen.emit("\tbl\tstone.str_slice");
    pop_frame(r#gen, &[]);
}

/// Emits `.Lnet_wait`, as `x64::net` does, waiting until the descriptor `x0` is ready for the
/// `poll` events `x1`, for at most `x2` milliseconds in all, and returning 0, the negative of
/// [`TIMED_OUT_CODE`], or a negative error number in `x0`: through `ppoll` with a `timespec` of
/// the time left on Linux, and `poll` with its milliseconds, rounded up, on macOS.
fn wait(r#gen: &mut dyn AssemblyGenerator) {
    let os = r#gen.target().os;
    let constants = Constants::of(os);
    // x19 holds the descriptor, x20 the events, and x21 the deadline in nanoseconds; the clock is
    // read into [sp], the pollfd is at [sp, #16], and the timespec at [sp, #32]
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit(".Lnet_wait:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tsub\tsp, sp, #48");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx21, x2");
    now(r#gen, constants.monotonic);
    load(r#gen, "x9", 1_000_000);
    r#gen.emit("\tmadd\tx21, x21, x9, x0");
    r#gen.emit(".Lnet_wait_again:");
    now(r#gen, constants.monotonic);
    r#gen.emit("\tsubs\tx10, x21, x0");
    r#gen.emit("\tb.le\t.Lnet_wait_timeout");
    // struct pollfd { fd, events, revents }
    r#gen.emit("\tstr\tw19, [sp, #16]");
    r#gen.emit("\tstr\tw20, [sp, #20]");
    r#gen.emit("\tadd\tx0, sp, #16");
    r#gen.emit("\tmov\tx1, #1");
    match os {
        Os::Linux => {
            load(r#gen, "x9", 1_000_000_000);
            r#gen.emit("\tudiv\tx11, x10, x9");
            r#gen.emit("\tmsub\tx12, x11, x9, x10");
            r#gen.emit("\tstp\tx11, x12, [sp, #32]");
            r#gen.emit("\tadd\tx2, sp, #32");
            r#gen.emit("\tmov\tx3, #0");
            r#gen.emit("\tmov\tx4, #0");
        }
        Os::MacOs => {
            load(r#gen, "x9", 999_999);
            r#gen.emit("\tadd\tx10, x10, x9");
            load(r#gen, "x9", 1_000_000);
            r#gen.emit("\tudiv\tx2, x10, x9");
        }
    }
    system_call(r#gen, Sys::Ppoll);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit("\tb.eq\t.Lnet_wait_again");
    r#gen.emit("\ttbnz\tx0, #63, .Lnet_wait_done");
    // a poll that ran out may have rounded the time, so the clock decides
    r#gen.emit("\tcbz\tx0, .Lnet_wait_again");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tb\t.Lnet_wait_done");
    r#gen.emit(".Lnet_wait_timeout:");
    r#gen.emit(&format!("\tmov\tx0, #-{TIMED_OUT_CODE}"));
    r#gen.emit(".Lnet_wait_done:");
    pop_frame(r#gen, &saved);
}

/// Emits code that waits, if the handle in `x19` has a timeout, until the descriptor in `fd` is
/// ready for the `poll` events `events`, then goes on to `ready`, failing with the reason alone
/// if the wait runs out or fails.
fn wait_for(r#gen: &mut dyn AssemblyGenerator, label: &str, fd: &str, events: i64, ready: &str) {
    r#gen.emit("\tldr\tx2, [x19, #64]");
    r#gen.emit(&format!("\ttbnz\tx2, #63, {ready}"));
    r#gen.emit(&format!("\tmov\tx0, {fd}"));
    r#gen.emit(&format!("\tmov\tx1, #{events}"));
    r#gen.emit("\tbl\t.Lnet_wait");
    r#gen.emit(&format!("\tcbz\tx0, {ready}"));
    fail_with_errno(r#gen, label, None);
}

/// Emits `stone.net_set_timeout`, as `x64::net` does, for the handle in `x0` and the seconds'
/// bits in `x1`.
fn set_timeout(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.net_set_timeout:");
    r#gen.emit("\tfmov\td0, x1");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit("\tscvtf\td1, x9");
    // below, equal, or nan
    r#gen.emit("\tfcmp\td0, d1");
    r#gen.emit("\tb.le\t.Lnet_set_timeout_none");
    r#gen.emit("\tmov\tx9, #1000");
    r#gen.emit("\tscvtf\td1, x9");
    r#gen.emit("\tfmul\td0, d0, d1");
    load(r#gen, "x10", 2147483647);
    r#gen.emit("\tscvtf\td1, x10");
    r#gen.emit("\tfcmp\td0, d1");
    r#gen.emit("\tb.ge\t.Lnet_set_timeout_store");
    r#gen.emit("\tfcvtzs\tx10, d0");
    r#gen.emit("\tscvtf\td1, x10");
    r#gen.emit("\tfcmp\td0, d1");
    r#gen.emit("\tb.le\t.Lnet_set_timeout_store");
    r#gen.emit("\tadd\tx10, x10, #1");
    r#gen.emit("\tb\t.Lnet_set_timeout_store");
    r#gen.emit(".Lnet_set_timeout_none:");
    r#gen.emit("\tmov\tx10, #-1");
    r#gen.emit(".Lnet_set_timeout_store:");
    r#gen.emit("\tstr\tx10, [x0, #64]");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");
}

/// Emits the end of a byte reading routine, as `x64::net` does: `<label>_bytes` for the bytes in
/// `x0`, and `<label>_failed` for the negative error number in `x0`.
fn bytes_result(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_bytes:"));
    r#gen.emit("\tmov\tx1, #1"); // a string slot, as bytes are
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    r#gen.emit(&format!("{label}_failed:"));
    fail_with_errno(r#gen, label, None);
}

/// Emits `stone.net_read_bytes`, as `x64::net` does.
fn read_bytes(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_bytes";
    // x20 holds the most to take
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    check_open(r#gen, label);
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tcmp\tx20, #0");
    r#gen.emit(&format!("\tb.le\t{label}_take"));
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit(&format!("\tcbnz\tx21, {label}_some"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbnz\tx0, {label}_again"));
    // the other end closed the connection
    r#gen.emit(&format!("\tb\t{label}_take"));
    r#gen.emit(&format!("{label}_some:"));
    r#gen.emit("\tcmp\tx0, x20");
    r#gen.emit("\tcsel\tx0, x20, x0, hi");
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tbl\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_exact`, as `x64::net` does.
fn read_exact(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_exact";
    // x20 holds how many to take
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    check_open(r#gen, label);
    r#gen.emit(&format!("\ttbz\tx20, #63, {label}_again"));
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tcmp\tx21, x20");
    r#gen.emit(&format!("\tb.hs\t{label}_take"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbnz\tx0, {label}_again"));
    fail_with_code(r#gen, label, CLOSED_EARLY_CODE);
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_all_bytes`, as `x64::net` does.
fn read_all_bytes(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_all_bytes";
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbnz\tx0, {label}_more"));
    unread(r#gen);
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\t.Lnet_take_bytes");
    bytes_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits the end of a reading routine, as `x64::net` does: `<label>_text` for the string in
/// `x0`, and `<label>_failed` for the negative error number in `x0`.
fn text_result(r#gen: &mut dyn AssemblyGenerator, label: &str) {
    r#gen.emit(&format!("{label}_text:"));
    r#gen.emit(&format!("\tcbz\tx0, {label}_invalid"));
    r#gen.emit("\tmov\tx1, #1"); // a string slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    r#gen.emit(&format!("{label}_invalid:"));
    fail_with_code(r#gen, label, INVALID_TEXT_CODE);
    r#gen.emit(&format!("{label}_failed:"));
    fail_with_errno(r#gen, label, None);
}

/// Emits code that leaves in `x21` how many bytes the socket in `x19` has received but not read
/// yet, and in `x22` where they start.
fn unread(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tldr\tx9, [x19, #32]");
    r#gen.emit("\tldr\tx21, [x19, #40]");
    r#gen.emit("\tsub\tx21, x21, x9");
    r#gen.emit("\tldr\tx22, [x19, #24]");
    r#gen.emit("\tadd\tx22, x22, x9");
}

/// Emits `stone.net_read_line`, as `x64::net` does.
fn read_line(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_line";
    // x20 is how far the bytes not read yet have been searched for a \n
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    check_open(r#gen, label);
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit(&format!("{label}_scan:"));
    unread(r#gen);
    r#gen.emit(&format!("{label}_next:"));
    r#gen.emit("\tcmp\tx20, x21");
    r#gen.emit(&format!("\tb.hs\t{label}_more"));
    r#gen.emit("\tldrb\tw9, [x22, x20]");
    r#gen.emit("\tcmp\tx9, #10");
    r#gen.emit(&format!("\tb.eq\t{label}_found"));
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit(&format!("\tb\t{label}_next"));
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbnz\tx0, {label}_scan"));
    // the other end closed the connection, so the rest is the last line, if any
    unread(r#gen);
    r#gen.emit(&format!("\tcbz\tx21, {label}_none"));
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit(&format!("\tb\t{label}_take"));
    r#gen.emit(&format!("{label}_found:"));
    r#gen.emit("\tadd\tx0, x20, #1");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit(&format!("\tcbz\tx20, {label}_take"));
    r#gen.emit("\tadd\tx9, x22, x20");
    r#gen.emit("\tsub\tx9, x9, #1");
    r#gen.emit("\tldrb\tw9, [x9]");
    r#gen.emit("\tcmp\tx9, #13");
    r#gen.emit(&format!("\tb.ne\t{label}_take"));
    r#gen.emit("\tsub\tx1, x1, #1");
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tbl\t.Lnet_take");
    r#gen.emit(&format!("\tcbz\tx0, {label}_invalid"));
    // some(line), tag 1
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx0, #1");
    r#gen.emit("\tmov\tx1, #1"); // a string slot
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit("\tstr\tx20, [x0, #16]");
    r#gen.emit(&format!("\tb\t{label}_option"));
    // none, tag 0
    r#gen.emit(&format!("{label}_none:"));
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tmov\tx2, #0");
    r#gen.emit("\tbl\tstone.enum_new");
    r#gen.emit(&format!("{label}_option:"));
    ok_handle(r#gen, label);
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read`, as `x64::net` does.
fn read(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read";
    // x23 is how many of the bytes not read yet end on a character boundary
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_again:"));
    unread(r#gen);
    r#gen.emit("\tmov\tx23, x21");
    r#gen.emit("\tldr\tx9, [x19, #56]");
    r#gen.emit(&format!("\tcbnz\tx9, {label}_take"));
    // x10 counts back from the end over at most 3 bytes, the most an unfinished character has
    r#gen.emit("\tmov\tx10, #1");
    r#gen.emit(&format!("{label}_back:"));
    r#gen.emit("\tcmp\tx10, x21");
    r#gen.emit(&format!("\tb.hi\t{label}_complete"));
    r#gen.emit("\tcmp\tx10, #3");
    r#gen.emit(&format!("\tb.hi\t{label}_complete"));
    r#gen.emit("\tsub\tx11, x21, x10");
    r#gen.emit("\tldrb\tw9, [x22, x11]");
    r#gen.emit("\tand\tx11, x9, #0xc0");
    r#gen.emit("\tcmp\tx11, #128");
    r#gen.emit(&format!("\tb.ne\t{label}_lead"));
    r#gen.emit("\tadd\tx10, x10, #1");
    r#gen.emit(&format!("\tb\t{label}_back"));
    // the character's width, from its first byte
    r#gen.emit(&format!("{label}_lead:"));
    r#gen.emit("\tmov\tx11, #1");
    for (bound, width) in [(192, 2), (224, 3), (240, 4)] {
        r#gen.emit(&format!("\tcmp\tx9, #{bound}"));
        r#gen.emit(&format!("\tb.lo\t{label}_width"));
        r#gen.emit(&format!("\tmov\tx11, #{width}"));
    }
    r#gen.emit("\tcmp\tx9, #248");
    r#gen.emit(&format!("\tb.lo\t{label}_width"));
    r#gen.emit("\tmov\tx11, #1");
    r#gen.emit(&format!("{label}_width:"));
    r#gen.emit("\tcmp\tx11, x10");
    r#gen.emit(&format!("\tb.ls\t{label}_complete"));
    r#gen.emit("\tsub\tx23, x23, x10");
    r#gen.emit(&format!("{label}_complete:"));
    r#gen.emit(&format!("\tcbnz\tx23, {label}_take"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tb\t{label}_again"));
    r#gen.emit(&format!("{label}_take:"));
    r#gen.emit("\tmov\tx0, x23");
    r#gen.emit("\tmov\tx1, x23");
    r#gen.emit("\tbl\t.Lnet_take");
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_read_all`, as `x64::net` does.
fn read_all(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_read_all";
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x0");
    check_open(r#gen, label);
    r#gen.emit(&format!("{label}_more:"));
    r#gen.emit("\tbl\t.Lnet_receive");
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbnz\tx0, {label}_more"));
    unread(r#gen);
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit("\tbl\t.Lnet_take");
    text_result(r#gen, label);
    frame_end(r#gen, label);
}

/// Emits `stone.net_exchange`, as `x64::net` does.
fn exchange(r#gen: &mut dyn AssemblyGenerator) {
    let label = "stone.net_exchange";
    let os = r#gen.target().os;
    let constants = Constants::of(os);
    // x19 holds the name, x20 the packet sent, x21 the descriptor, x22 the reply buffer, x23 the
    // address's length and then the deadline in nanoseconds, x24 the query, x25 the
    // milliseconds, and x26 the error, then the reply
    frame(r#gen, label);
    r#gen.emit("\tmov\tx19, x2");
    r#gen.emit("\tmov\tx24, x3");
    r#gen.emit("\tmov\tx25, x4");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    socket_address(r#gen, label);
    // the query has an ID to fill in, and holds only bytes
    r#gen.emit("\tldr\tx10, [x24]");
    r#gen.emit("\tcmp\tx10, #2");
    r#gen.emit(&format!("\tb.lo\t{label}_invalid"));
    r#gen.emit("\tldr\tx12, [x24, #16]");
    r#gen.emit(&format!("{label}_check:"));
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tldr\tx9, [x12, x10, lsl #3]");
    r#gen.emit("\tcmp\tx9, #255");
    r#gen.emit(&format!("\tb.hi\t{label}_invalid"));
    r#gen.emit(&format!("\tcbnz\tx10, {label}_check"));
    // nothing to clean up yet
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit("\tmov\tx21, #-1");
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit(&format!("\tb\t{label}_valid"));
    r#gen.emit(&format!("{label}_invalid:"));
    r#gen.emit(&format!("\tmov\tx0, #{INVALID_ADDRESS_CODE}"));
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit(&format!("\tb\t{label}_err"));
    r#gen.emit(&format!("{label}_valid:"));
    // a random ID, as the interpreter reads it from /dev/urandom
    r#gen.emit("\tstr\txzr, [sp, #32]");
    retried(
        r#gen,
        &format!("{label}_random"),
        Sys::Getrandom,
        &["add\tx0, sp, #32", "mov\tx1, #2", "mov\tx2, #0"],
    );
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    // the packet: the query's bytes with the ID first
    r#gen.emit("\tldr\tx0, [x24]");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tldr\tx10, [x24]");
    r#gen.emit("\tldr\tx12, [x24, #16]");
    r#gen.emit(&format!("{label}_copy:"));
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tldr\tx9, [x12, x10, lsl #3]");
    r#gen.emit("\tstrb\tw9, [x20, x10]");
    r#gen.emit(&format!("\tcbnz\tx10, {label}_copy"));
    r#gen.emit("\tldrb\tw9, [sp, #32]");
    r#gen.emit("\tstrb\tw9, [x20]");
    r#gen.emit("\tldrb\tw9, [sp, #33]");
    r#gen.emit("\tstrb\tw9, [x20, #1]");
    load(r#gen, "x0", MAX_REPLY as i64);
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx22, x0");
    r#gen.emit(match os {
        Os::Linux => "\tldrb\tw0, [sp]",
        Os::MacOs => "\tldrb\tw0, [sp, #1]",
    });
    load(r#gen, "x1", 2 | constants.cloexec); // SOCK_DGRAM
    r#gen.emit("\tmov\tx2, #0");
    system_call(r#gen, Sys::Socket);
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit("\tmov\tx21, x0");
    if os == Os::MacOs {
        r#gen.emit("\tbl\t.Lnet_prepare");
        r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    }
    retried(
        r#gen,
        &format!("{label}_connect"),
        Sys::Connect,
        &["mov\tx0, x21", "mov\tx1, sp", "mov\tx2, x23"],
    );
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    let flags = format!("mov\tx3, #{}", constants.no_signal);
    retried(
        r#gen,
        &format!("{label}_send"),
        Sys::Sendto,
        &[
            "mov\tx0, x21",
            "mov\tx1, x20",
            "ldr\tx2, [x24]",
            &flags,
            "mov\tx4, #0",
            "mov\tx5, #0",
        ],
    );
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    // the deadline, from the monotonic clock
    now(r#gen, constants.monotonic);
    load(r#gen, "x9", 1_000_000);
    r#gen.emit("\tmadd\tx23, x25, x9, x0");
    r#gen.emit(&format!("{label}_wait:"));
    now(r#gen, constants.monotonic);
    r#gen.emit("\tsubs\tx10, x23, x0");
    r#gen.emit(&format!("\tb.le\t{label}_timeout"));
    // struct pollfd { fd, events = POLLIN, revents }
    r#gen.emit("\tstr\tw21, [sp, #32]");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tw9, [sp, #36]");
    r#gen.emit("\tadd\tx0, sp, #32");
    r#gen.emit("\tmov\tx1, #1");
    match os {
        // a timespec of what is left, and no signal mask
        Os::Linux => {
            load(r#gen, "x9", 1_000_000_000);
            r#gen.emit("\tudiv\tx11, x10, x9");
            r#gen.emit("\tmsub\tx12, x11, x9, x10");
            r#gen.emit("\tstp\tx11, x12, [sp, #40]");
            r#gen.emit("\tadd\tx2, sp, #40");
            r#gen.emit("\tmov\tx3, #0");
            r#gen.emit("\tmov\tx4, #0");
        }
        // the milliseconds left, rounded up
        Os::MacOs => {
            load(r#gen, "x9", 999_999);
            r#gen.emit("\tadd\tx10, x10, x9");
            load(r#gen, "x9", 1_000_000);
            r#gen.emit("\tudiv\tx2, x10, x9");
        }
    }
    system_call(r#gen, Sys::Ppoll);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_wait"));
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    r#gen.emit(&format!("\tcbz\tx0, {label}_timeout"));
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tmov\tx1, x22");
    load(r#gen, "x2", MAX_REPLY as i64);
    r#gen.emit("\tmov\tx3, #0");
    r#gen.emit("\tmov\tx4, #0");
    r#gen.emit("\tmov\tx5, #0");
    system_call(r#gen, Sys::Recvfrom);
    r#gen.emit(&format!("\tcmn\tx0, #{EINTR}"));
    r#gen.emit(&format!("\tb.eq\t{label}_wait"));
    r#gen.emit(&format!("\ttbnz\tx0, #63, {label}_failed"));
    // another query's reply, or a stray packet, is not the answer
    r#gen.emit("\tcmp\tx0, #2");
    r#gen.emit(&format!("\tb.lo\t{label}_wait"));
    for offset in [0, 1] {
        r#gen.emit(&format!("\tldrb\tw9, [x22, #{offset}]"));
        r#gen.emit(&format!("\tldrb\tw10, [x20, #{offset}]"));
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit(&format!("\tb.ne\t{label}_wait"));
    }
    r#gen.emit("\tmov\tx26, x0");
    r#gen.emit("\tmov\tx1, #0");
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx23, x0");
    r#gen.emit("\tldr\tx12, [x23, #16]");
    r#gen.emit(&format!("{label}_fill:"));
    r#gen.emit("\tsub\tx26, x26, #1");
    r#gen.emit("\tldrb\tw9, [x22, x26]");
    r#gen.emit("\tstr\tx9, [x12, x26, lsl #3]");
    r#gen.emit(&format!("\tcbnz\tx26, {label}_fill"));
    cleanup(r#gen, label, "replied");
    r#gen.emit("\tmov\tx0, x23");
    r#gen.emit("\tmov\tx1, #2"); // a list slot
    r#gen.emit("\tbl\t.Lfs_ok");
    r#gen.emit(&format!("\tb\t{label}_done"));
    r#gen.emit(&format!("{label}_timeout:"));
    r#gen.emit(&format!("\tmov\tx26, #{TIMED_OUT_CODE}"));
    r#gen.emit(&format!("\tb\t{label}_fail"));
    r#gen.emit(&format!("{label}_failed:"));
    r#gen.emit("\tneg\tx26, x0");
    r#gen.emit(&format!("{label}_fail:"));
    cleanup(r#gen, label, "failing");
    r#gen.emit("\tmov\tx0, x26");
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit(&format!("\tb\t{label}_err"));
    frame_end(r#gen, label);
}

/// Emits code that leaves the clock `clock`'s time in nanoseconds in `x0`, using the address
/// slots at `[sp]`, which are not needed once a socket is connected.
fn now(r#gen: &mut dyn AssemblyGenerator, clock: i64) {
    r#gen.emit(&format!("\tmov\tx0, #{clock}"));
    r#gen.emit("\tmov\tx1, sp");
    system_call(r#gen, Sys::ClockGettime);
    r#gen.emit("\tldp\tx9, x10, [sp]");
    load(r#gen, "x11", 1_000_000_000);
    r#gen.emit("\tmadd\tx0, x9, x11, x10");
}

/// Emits code that closes the descriptor in `x21` unless it is -1 and frees the buffers in `x20`
/// and `x22` unless they are 0, for `stone.net_exchange` on its way out, with labels named after
/// `way`.
fn cleanup(r#gen: &mut dyn AssemblyGenerator, label: &str, way: &str) {
    r#gen.emit(&format!("\ttbnz\tx21, #63, {label}_{way}_closed"));
    close(r#gen, "x21");
    r#gen.emit(&format!("{label}_{way}_closed:"));
    for reg in ["x20", "x22"] {
        r#gen.emit(&format!("\tcbz\t{reg}, {label}_{way}_{reg}"));
        r#gen.emit(&format!("\tmov\tx9, {reg}"));
        r#gen.emit("\tbl\tstone.mem_free");
        r#gen.emit(&format!("{label}_{way}_{reg}:"));
    }
}

/// Emits `.Lfree_handle`, which `stone.free_enum` calls for a handle in `x9` whose count reached
/// 0: it closes the descriptor unless closed already, and frees a socket's buffer, changing only
/// the registers the free routines may change.
pub fn free_handle(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lfree_handle:");
    r#gen.emit("\tstp\tx0, x9, [sp, #-16]!");
    r#gen.emit("\tstr\tx30, [sp, #-16]!");
    r#gen.emit("\tldr\tx0, [x9, #16]");
    r#gen.emit("\ttbnz\tx0, #63, .Lfree_handle_closed");
    system_call(r#gen, Sys::Close);
    r#gen.emit(".Lfree_handle_closed:");
    r#gen.emit("\tldr\tx9, [sp, #24]");
    r#gen.emit("\tldr\tx10, [x9]");
    r#gen.emit(&format!("\tcmn\tx10, #{}", -SOCKET_TAG));
    r#gen.emit("\tb.ne\t.Lfree_handle_done");
    r#gen.emit("\tldr\tx9, [x9, #24]");
    r#gen.emit("\tcbz\tx9, .Lfree_handle_done");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit(".Lfree_handle_done:");
    r#gen.emit("\tldr\tx30, [sp], #16");
    r#gen.emit("\tldp\tx0, x9, [sp], #16");
    r#gen.emit("\tret");
}
