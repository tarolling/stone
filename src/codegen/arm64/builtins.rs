//! The hand-written arm64 runtime, which mirrors `x64/builtins.rs` routine for routine, under the
//! same labels and with the same contracts.
//!
//! Routines take their arguments in `x0` and up and return in `x0`, as AAPCS64 does, keep what
//! must survive a call in `x19` and up, and save those with [`push_frame`]. Floats travel as
//! their bits in general registers, as in generated code, and only move to `d0` for arithmetic.
//! The float text routines and `stone.fmod` are in `floats.rs`.

use super::emit::CALLER_SAVED;
use super::sort::sort_runtime;
use super::{address, pop_frame, push_frame};
use crate::codegen::context::{CPU_ONLINE, PATH_BUFFER, UTSNAME_FIELD, UTSNAME_SIZE};
use crate::codegen::context::{
    HEAP_REGION, IMMORTAL, LARGEST_CLASS, LEAK_PREFIX, LEAK_SUFFIX, OUT_OF_MEMORY,
    OUT_OF_MEMORY_LENGTH, SMALLEST_CLASS, STDIN_BUFFER, TEXT_BLOCK, immortal_string,
};
use crate::codegen::{AssemblyGenerator, Os};
use crate::stdlib::os::HOSTNAME_BUFFER;

/// A call into the operating system that the runtime makes.
///
/// On Linux each is a system call made directly with `svc`. macOS has no stable system call
/// interface, so there each is a call to the `libSystem` function of the same name through a
/// shim ([`system_shims`]).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sys {
    Read,
    Write,
    Openat,
    Close,
    Mmap,
    Munmap,
    Mremap,
    Exit,
    Uname,
    Getpid,
    Getcwd,
    ClockGettime,
    Sysctl,
    Nanosleep,
    /// Fills a buffer with entropy: `getrandom` on Linux and `getentropy` on macOS, which ignores
    /// the flags in `x2`.
    Getrandom,
    Getdents64,
    /// Deletes the file named by `x1`, relative to the directory `x0`: `unlinkat` on Linux, and
    /// `unlink` on macOS, whose shim drops `x0`, as it does for the next two.
    Unlinkat,
    Mkdirat,
    Faccessat,
    Opendir,
    /// Reads the next entry of a `DIR` from `opendir`, returning null at the end, which only
    /// macOS calls.
    Readdir,
    Closedir,
    /// Makes a pipe, writing its two descriptors where `x0` points: `pipe2` with the flags `x1`
    /// on Linux, and `pipe` on macOS, which ignores them.
    Pipe2,
    /// Starts a child process: `clone` on Linux, which takes `fork`'s flags in `x0` (`SIGCHLD`)
    /// and zeros in `x1` through `x4`, and `fork` on macOS, which takes nothing.
    Clone,
    /// Moves the descriptor `x0` to `x1`: `dup3` on Linux, with the flags `x2`, and `dup2` on
    /// macOS, which ignores them.
    Dup3,
    Execve,
    /// Waits for the descriptors `x0` points to: `ppoll` on Linux, whose timeout `x2` and signal
    /// mask `x3` are pointers, and `poll` on macOS, whose timeout `x2` is milliseconds, -1
    /// for none.
    Ppoll,
    Wait4,
    /// Changes a descriptor's flags: on macOS for close-on-exec, which Linux's `pipe2` and
    /// `SOCK_CLOEXEC` set themselves, and on both for a socket connecting with a timeout.
    Fcntl,
    Socket,
    Connect,
    Bind,
    Listen,
    /// Takes the next connection to a listening socket: `accept4` on Linux, with the flags `x3`,
    /// and `accept` on macOS, which ignores them.
    Accept,
    Getsockname,
    Setsockopt,
    Sendto,
    Recvfrom,
    Getsockopt,
    /// Makes a file in memory, which only Linux has.
    MemfdCreate,
    /// Sets a file's size, which only Linux's runtime does.
    Ftruncate,
    /// Writes at a position of a file, which only Linux's runtime does.
    Pwrite,
    /// Sends a message with a descriptor, which only Linux's runtime does.
    Sendmsg,
}

impl Sys {
    /// Every call, in the order their shims are emitted.
    pub const ALL: [Sys; 43] = [
        Sys::Read,
        Sys::Write,
        Sys::Openat,
        Sys::Close,
        Sys::Mmap,
        Sys::Munmap,
        Sys::Mremap,
        Sys::Exit,
        Sys::Uname,
        Sys::Getpid,
        Sys::Getcwd,
        Sys::ClockGettime,
        Sys::Sysctl,
        Sys::Nanosleep,
        Sys::Getrandom,
        Sys::Getdents64,
        Sys::Unlinkat,
        Sys::Mkdirat,
        Sys::Faccessat,
        Sys::Opendir,
        Sys::Readdir,
        Sys::Closedir,
        Sys::Pipe2,
        Sys::Clone,
        Sys::Dup3,
        Sys::Execve,
        Sys::Ppoll,
        Sys::Wait4,
        Sys::Fcntl,
        Sys::Socket,
        Sys::Connect,
        Sys::Bind,
        Sys::Listen,
        Sys::Accept,
        Sys::Getsockname,
        Sys::Setsockopt,
        Sys::Sendto,
        Sys::Recvfrom,
        Sys::Getsockopt,
        Sys::MemfdCreate,
        Sys::Ftruncate,
        Sys::Pwrite,
        Sys::Sendmsg,
    ];

    /// Returns the call's Linux system call number, such as 64 for `write`, or `None` if the
    /// runtime only makes it on macOS.
    pub fn linux(self) -> Option<u32> {
        Some(match self {
            Sys::Read => 63,
            Sys::Write => 64,
            Sys::Openat => 56,
            Sys::Close => 57,
            Sys::Mmap => 222,
            Sys::Munmap => 215,
            Sys::Mremap => 216,
            Sys::Exit => 94, // exit_group
            Sys::Uname => 160,
            Sys::Getpid => 172,
            Sys::Getcwd => 17,
            Sys::ClockGettime => 113,
            Sys::Nanosleep => 101,
            Sys::Getrandom => 278,
            Sys::Getdents64 => 61,
            Sys::Unlinkat => 35,
            Sys::Mkdirat => 34,
            Sys::Faccessat => 48,
            Sys::Pipe2 => 59,
            Sys::Clone => 220,
            Sys::Dup3 => 24,
            Sys::Execve => 221,
            Sys::Ppoll => 73,
            Sys::Wait4 => 260,
            Sys::Socket => 198,
            Sys::Bind => 200,
            Sys::Listen => 201,
            Sys::Connect => 203,
            Sys::Getsockname => 204,
            Sys::Sendto => 206,
            Sys::Recvfrom => 207,
            Sys::Setsockopt => 208,
            Sys::Getsockopt => 209,
            Sys::Fcntl => 25,
            Sys::Accept => 242, // accept4
            Sys::MemfdCreate => 279,
            Sys::Ftruncate => 46,
            Sys::Pwrite => 68, // pwrite64
            Sys::Sendmsg => 211,
            Sys::Sysctl | Sys::Opendir | Sys::Readdir | Sys::Closedir => {
                return None;
            }
        })
    }

    /// Returns the `libSystem` function behind the call on macOS, such as `_exit` for
    /// [`Sys::Exit`], or `None` if the runtime only makes it on Linux.
    pub fn macos(self) -> Option<&'static str> {
        Some(match self {
            Sys::Read => "read",
            Sys::Write => "write",
            Sys::Mmap => "mmap",
            Sys::Munmap => "munmap",
            Sys::Exit => "_exit",
            Sys::Getpid => "getpid",
            Sys::Getcwd => "getcwd",
            Sys::ClockGettime => "clock_gettime",
            Sys::Sysctl => "sysctl",
            Sys::Nanosleep => "nanosleep",
            Sys::Getrandom => "getentropy",
            Sys::Openat => "open",
            Sys::Close => "close",
            Sys::Unlinkat => "unlink",
            Sys::Mkdirat => "mkdir",
            Sys::Faccessat => "access",
            Sys::Opendir => "opendir",
            Sys::Readdir => "readdir",
            Sys::Closedir => "closedir",
            Sys::Pipe2 => "pipe",
            Sys::Clone => "fork",
            Sys::Dup3 => "dup2",
            Sys::Execve => "execve",
            Sys::Ppoll => "poll",
            Sys::Wait4 => "wait4",
            Sys::Fcntl => "fcntl",
            Sys::Socket => "socket",
            Sys::Connect => "connect",
            Sys::Bind => "bind",
            Sys::Listen => "listen",
            Sys::Accept => "accept",
            Sys::Getsockname => "getsockname",
            Sys::Setsockopt => "setsockopt",
            Sys::Sendto => "sendto",
            Sys::Recvfrom => "recvfrom",
            Sys::Getsockopt => "getsockopt",
            Sys::Mremap | Sys::Uname | Sys::Getdents64 => return None,
            Sys::MemfdCreate | Sys::Ftruncate | Sys::Pwrite | Sys::Sendmsg => return None,
        })
    }

    /// Returns the label of the call's macOS shim, such as `stone.sys_write`.
    fn shim(self) -> String {
        format!(
            "stone.sys_{}",
            self.macos().unwrap().trim_start_matches('_')
        )
    }
}

/// Emits a call into the operating system with its arguments in `x0` and up, which returns its
/// result in `x0` and changes no other general register, as a Linux system call does.
///
/// On Linux that is `mov x8, #n` and `svc #0`. On macOS it calls the call's shim, saving `x30`
/// around it.
pub fn system_call(r#gen: &mut dyn AssemblyGenerator, call: Sys) {
    match r#gen.target().os {
        Os::Linux => {
            let number = call
                .linux()
                .expect("a system call the runtime makes on Linux");
            r#gen.emit(&format!("\tmov\tx8, #{number}"));
            r#gen.emit("\tsvc\t#0");
        }
        Os::MacOs => {
            r#gen.emit("\tstr\tx30, [sp, #-16]!");
            r#gen.emit(&format!("\tbl\t{}", call.shim()));
            r#gen.emit("\tldr\tx30, [sp], #16");
        }
    }
}

/// The registers a macOS shim saves around a `libSystem` call: every general register the call
/// may change except `x0`, which carries the result.
const SHIM_SAVED: [&str; 17] = [
    "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
    "x16", "x17",
];

/// Emits the macOS shim of every call that `assembly`, the program's text so far, makes through
/// [`system_call`]. Each shim calls the `libSystem` function, which the linker binds, and keeps
/// the contract of a Linux system call: only `x0` changes, and a failure comes back as the
/// negative error number. `libSystem` reports a failure as -1, or a null pointer from `getcwd`
/// and `opendir`, with the number in `errno`, which `__error` points to.
///
/// The calls named for Linux's `*at` system calls take a directory in `x0`, which the shim drops,
/// since every path the runtime passes is relative to the working directory. `open` and `fcntl`
/// are variadic, and Apple's arm64 calling convention passes variadic arguments on the stack, so
/// their shims store the last argument there. `readdir` returns null both at the end and on an error, so its shim
/// clears `errno` first and returns 0 at the end.
pub fn system_shims(r#gen: &mut dyn AssemblyGenerator, assembly: &str) {
    for call in Sys::ALL {
        if call.macos().is_none() || !assembly.contains(&format!("\tbl\t{}\n", call.shim())) {
            continue;
        }
        let name = call.macos().unwrap().trim_start_matches('_');
        let done = format!(".Lsys_{name}_done");
        r#gen.emit(&format!("{}:", call.shim()));
        push_frame(r#gen, &SHIM_SAVED);
        if matches!(
            call,
            Sys::Openat | Sys::Unlinkat | Sys::Mkdirat | Sys::Faccessat
        ) {
            r#gen.emit("\tmov\tx0, x1");
            r#gen.emit("\tmov\tx1, x2");
            r#gen.emit("\tmov\tx2, x3");
        }
        match call {
            Sys::Openat | Sys::Fcntl => {
                r#gen.emit("\tstr\tx2, [sp, #-16]!");
                r#gen.emit(&format!("\tbl\t{name}"));
                r#gen.emit("\tadd\tsp, sp, #16");
            }
            Sys::Readdir => {
                r#gen.emit("\tstr\tx0, [sp, #-16]!");
                r#gen.emit(&format!("\tbl\t{ERRNO_FUNCTION}"));
                r#gen.emit("\tstr\twzr, [x0]");
                r#gen.emit("\tldr\tx0, [sp], #16");
                r#gen.emit("\tbl\treaddir");
                r#gen.emit(&format!("\tcbnz\tx0, {done}"));
                errno(r#gen);
            }
            _ => r#gen.emit(&format!("\tbl\t{}", call.macos().unwrap())),
        }
        if matches!(call, Sys::Getcwd | Sys::Opendir) {
            r#gen.emit(&format!("\tcbnz\tx0, {done}"));
            errno(r#gen);
        } else if !matches!(call, Sys::Readdir | Sys::Exit | Sys::Getpid) {
            r#gen.emit("\tcmn\tx0, #1");
            r#gen.emit(&format!("\tb.ne\t{done}"));
            errno(r#gen);
        }
        r#gen.emit(&format!("{done}:"));
        pop_frame(r#gen, &SHIM_SAVED);
    }
}

/// The `libSystem` function that returns where the calling thread's `errno` is, which the shims
/// call after a failure.
pub const ERRNO_FUNCTION: &str = "__error";

/// Emits code that puts the negative of `errno` in `x0`, for a shim whose call failed.
fn errno(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(&format!("\tbl\t{ERRNO_FUNCTION}"));
    r#gen.emit("\tldr\tw0, [x0]");
    r#gen.emit("\tneg\tx0, x0");
}

/// Emits a loop that copies `count` bytes from `src` to `dst`, advancing both and counting
/// `count` down to 0. It clobbers `w16`, and uses `label` and `label_done` as its labels.
///
/// For example, `copy_bytes(gen, "x9", "x19", "x21", ".Lstr_concat_left")` copies `x21` bytes
/// from `x19` to `x9`.
pub(super) fn copy_bytes(
    r#gen: &mut dyn AssemblyGenerator,
    dst: &str,
    src: &str,
    count: &str,
    label: &str,
) {
    r#gen.emit(&format!("{label}:"));
    r#gen.emit(&format!("\tcbz\t{count}, {label}_done"));
    r#gen.emit(&format!("\tldrb\tw16, [{src}], #1"));
    r#gen.emit(&format!("\tstrb\tw16, [{dst}], #1"));
    r#gen.emit(&format!("\tsub\t{count}, {count}, #1"));
    r#gen.emit(&format!("\tb\t{label}"));
    r#gen.emit(&format!("{label}_done:"));
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

/// Emits `_start`, where the kernel starts the program, with the argument count at `[sp]`, the
/// arguments after it, then a null, then the environment. It calls `main` with the count in `x0`,
/// the arguments in `x1`, and the environment in `x2`, as a C program's `main` gets them, then
/// ends the program with the status `main` returns.
pub fn start_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.globl\t_start");
    r#gen.emit("_start:");
    if r#gen.target().os == Os::MacOs {
        // dyld calls the entry point as C calls main, with the arguments and environment in place
        r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
        r#gen.emit("\tmov\tx29, sp");
        r#gen.emit("\tbl\tmain");
        system_call(r#gen, Sys::Exit);
        return;
    }
    r#gen.emit("\tmov\tx29, #0"); // the outermost frame
    r#gen.emit("\tmov\tx30, #0");
    r#gen.emit("\tldr\tx0, [sp]");
    r#gen.emit("\tadd\tx1, sp, #8");
    r#gen.emit("\tadd\tx2, x1, x0, lsl #3");
    r#gen.emit("\tadd\tx2, x2, #8");
    r#gen.emit("\tbl\tmain");
    system_call(r#gen, Sys::Exit);
}

/// Emits the allocator, which takes memory from the system with the `mmap` syscall instead of
/// libc's `malloc`, with the same blocks, classes, and regions as x64's `allocator`.
///
/// - `stone.mem_alloc` returns at least `x0` bytes, aligned to 8
/// - `stone.mem_free` frees the bytes at `x9`
/// - `stone.mem_realloc` returns at least `x1` bytes holding what the `x0` bytes held, moving
///   them only if their block is too small
///
/// All three clobber only `x8` to `x11`, `x16`, `x17`, and `x30`, so the free routines and
/// `stone.list_copy` can call them. If the system has no memory to give, the program stops with
/// [`OUT_OF_MEMORY`].
pub fn allocator(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_out_of_memory:");
    r#gen.emit(&format!("\t.string \"{OUT_OF_MEMORY}\""));
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.heap_next:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.heap_end:");
    r#gen.emit("\t.zero\t8");
    // the first free block of each class, linked through the 8 bytes after each header
    r#gen.emit("stone.free_blocks:");
    r#gen.emit(&format!("\t.zero\t{}", 8 * (LARGEST_CLASS + 1)));
    r#gen.emit("\t.text");

    r#gen.emit("stone.mem_alloc:");
    // the class is the smallest power of two that holds the header too
    r#gen.emit("\tadd\tx9, x0, #7");
    r#gen.emit("\tclz\tx9, x9");
    r#gen.emit("\tmov\tx10, #64");
    r#gen.emit("\tsub\tx9, x10, x9");
    r#gen.emit(&format!("\tmov\tx10, #{SMALLEST_CLASS}"));
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tcsel\tx9, x9, x10, hs");
    r#gen.emit(&format!("\tcmp\tx9, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_alloc_large");
    address(r#gen, "x10", "stone.free_blocks");
    r#gen.emit("\tadd\tx10, x10, x9, lsl #3");
    r#gen.emit("\tldr\tx11, [x10]");
    r#gen.emit("\tcbz\tx11, .Lmem_alloc_cut");
    // a freed block keeps its class in its header
    r#gen.emit("\tldr\tx16, [x11, #8]");
    r#gen.emit("\tstr\tx16, [x10]");
    r#gen.emit("\tadd\tx0, x11, #8");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_alloc_cut:");
    r#gen.emit("\tadrp\tx10, stone.heap_next");
    r#gen.emit("\tldr\tx11, [x10, :lo12:stone.heap_next]");
    r#gen.emit("\tmov\tx16, #1");
    r#gen.emit("\tlsl\tx16, x16, x9");
    r#gen.emit("\tadd\tx16, x16, x11");
    r#gen.emit("\tadrp\tx17, stone.heap_end");
    r#gen.emit("\tldr\tx17, [x17, :lo12:stone.heap_end]");
    r#gen.emit("\tcmp\tx16, x17");
    r#gen.emit("\tb.hi\t.Lmem_alloc_region");
    r#gen.emit("\tstr\tx16, [x10, :lo12:stone.heap_next]");
    r#gen.emit("\tstr\tx9, [x11]");
    r#gen.emit("\tadd\tx0, x11, #8");
    r#gen.emit("\tret");
    // what is left of the old region is too small, and stays unused
    r#gen.emit(".Lmem_alloc_region:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tstr\tx0, [sp, #-16]!");
    r#gen.emit(&format!("\tmov\tx0, #{HEAP_REGION}"));
    r#gen.emit("\tbl\tstone.mem_map");
    r#gen.emit("\tadrp\tx10, stone.heap_next");
    r#gen.emit("\tstr\tx0, [x10, :lo12:stone.heap_next]");
    r#gen.emit(&format!("\tadd\tx0, x0, #{HEAP_REGION}"));
    r#gen.emit("\tadrp\tx10, stone.heap_end");
    r#gen.emit("\tstr\tx0, [x10, :lo12:stone.heap_end]");
    r#gen.emit("\tldr\tx0, [sp], #16");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tb\tstone.mem_alloc");
    r#gen.emit(".Lmem_alloc_large:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tadd\tx0, x0, #4095"); // whole pages
    r#gen.emit("\tand\tx0, x0, #-4096");
    r#gen.emit("\tstr\tx0, [sp, #-16]!");
    r#gen.emit("\tbl\tstone.mem_map");
    r#gen.emit("\tldr\tx9, [sp], #16");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_free:");
    r#gen.emit("\tsub\tx9, x9, #8");
    r#gen.emit("\tldr\tx10, [x9]");
    r#gen.emit(&format!("\tcmp\tx10, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_free_large");
    address(r#gen, "x11", "stone.free_blocks");
    r#gen.emit("\tadd\tx11, x11, x10, lsl #3");
    r#gen.emit("\tldr\tx16, [x11]");
    r#gen.emit("\tstr\tx16, [x9, #8]");
    r#gen.emit("\tstr\tx9, [x11]");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_free_large:");
    r#gen.emit("\tstp\tx0, x1, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x9");
    r#gen.emit("\tmov\tx1, x10");
    system_call(r#gen, Sys::Munmap);
    r#gen.emit("\tldp\tx0, x1, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.mem_realloc:");
    r#gen.emit("\tldur\tx9, [x0, #-8]");
    r#gen.emit("\tadd\tx10, x1, #8"); // the size the block needs
    r#gen.emit(&format!("\tcmp\tx9, #{LARGEST_CLASS}"));
    r#gen.emit("\tb.hi\t.Lmem_realloc_large");
    r#gen.emit("\tmov\tx11, #1");
    r#gen.emit("\tlsl\tx11, x11, x9");
    r#gen.emit("\tcmp\tx10, x11");
    r#gen.emit("\tb.hi\t.Lmem_realloc_move");
    r#gen.emit("\tret");
    // the old block's x11 bytes, less its header, are copied to a new one 8 at a time
    r#gen.emit(".Lmem_realloc_move:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tstp\tx0, x11, [sp, #-16]!");
    r#gen.emit("\tstr\tx1, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x1");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tldr\tx1, [sp], #16");
    r#gen.emit("\tldp\tx9, x11, [sp], #16");
    r#gen.emit("\tsub\tx11, x11, #8");
    r#gen.emit("\tmov\tx10, x0");
    r#gen.emit("\tmov\tx17, x9");
    r#gen.emit(".Lmem_realloc_copy:");
    r#gen.emit("\tldr\tx16, [x17], #8");
    r#gen.emit("\tstr\tx16, [x10], #8");
    r#gen.emit("\tsubs\tx11, x11, #8");
    r#gen.emit("\tb.ne\t.Lmem_realloc_copy");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_realloc_large:");
    r#gen.emit("\tcmp\tx10, x9");
    r#gen.emit("\tb.hi\t.Lmem_realloc_remap");
    r#gen.emit("\tret");
    r#gen.emit(".Lmem_realloc_remap:");
    if r#gen.target().os == Os::MacOs {
        // macOS has no mremap, so the bytes move to a new mapping and the old one is unmapped
        r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
        r#gen.emit("\tstp\tx0, x9, [sp, #-16]!");
        r#gen.emit("\tadd\tx0, x10, #4095");
        r#gen.emit("\tand\tx0, x0, #-4096");
        r#gen.emit("\tstr\tx0, [sp, #-16]!");
        r#gen.emit("\tbl\tstone.mem_map");
        r#gen.emit("\tldr\tx10, [sp], #16");
        r#gen.emit("\tstr\tx10, [x0]");
        r#gen.emit("\tldp\tx9, x11, [sp], #16");
        r#gen.emit("\tsub\tx11, x11, #8");
        r#gen.emit("\tadd\tx10, x0, #8");
        r#gen.emit("\tmov\tx17, x9");
        r#gen.emit(".Lmem_realloc_remap_copy:");
        r#gen.emit("\tldr\tx16, [x17], #8");
        r#gen.emit("\tstr\tx16, [x10], #8");
        r#gen.emit("\tsubs\tx11, x11, #8");
        r#gen.emit("\tb.ne\t.Lmem_realloc_remap_copy");
        r#gen.emit("\tbl\tstone.mem_free");
        r#gen.emit("\tadd\tx0, x0, #8");
        r#gen.emit("\tldp\tx29, x30, [sp], #16");
        r#gen.emit("\tret");
        return emit_mem_map(r#gen);
    }
    // the kernel moves the pages if it cannot grow the mapping where it is
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tstp\tx3, x4, [sp, #-16]!");
    r#gen.emit("\tadd\tx2, x10, #4095");
    r#gen.emit("\tand\tx2, x2, #-4096");
    r#gen.emit("\tmov\tx1, x9");
    r#gen.emit("\tsub\tx0, x0, #8");
    r#gen.emit("\tmov\tx3, #1"); // MREMAP_MAYMOVE
    system_call(r#gen, Sys::Mremap);
    r#gen.emit("\tcmn\tx0, #4095");
    r#gen.emit("\tb.hs\tstone.out_of_memory");
    r#gen.emit("\tstr\tx2, [x0]");
    r#gen.emit("\tadd\tx0, x0, #8");
    r#gen.emit("\tldp\tx3, x4, [sp], #16");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tret");
    emit_mem_map(r#gen);
}

/// Emits `stone.mem_map`, which returns `x0` new bytes of zeros, a whole number of pages,
/// clobbering only `x8`, and `stone.out_of_memory`, where it goes when the system has none.
fn emit_mem_map(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.mem_map:");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tstp\tx3, x4, [sp, #-16]!");
    r#gen.emit("\tstr\tx5, [sp, #-16]!");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, #0"); // anywhere
    r#gen.emit("\tmov\tx2, #3"); // PROT_READ | PROT_WRITE
    let flags = match r#gen.target().os {
        Os::Linux => "0x22",
        Os::MacOs => "0x1002",
    };
    r#gen.emit(&format!("\tmov\tx3, #{flags}")); // MAP_PRIVATE | MAP_ANONYMOUS
    r#gen.emit("\tmov\tx4, #-1"); // no file
    r#gen.emit("\tmov\tx5, #0");
    system_call(r#gen, Sys::Mmap);
    // the kernel returns an error as -4095 through -1, and libSystem as -1
    r#gen.emit("\tcmn\tx0, #4095");
    r#gen.emit("\tb.hs\tstone.out_of_memory");
    r#gen.emit("\tldr\tx5, [sp], #16");
    r#gen.emit("\tldp\tx3, x4, [sp], #16");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tret");

    r#gen.emit("stone.out_of_memory:");
    r#gen.emit("\tmov\tx0, #2"); // stderr
    address(r#gen, "x1", ".Lstone_out_of_memory");
    r#gen.emit(&format!("\tmov\tx2, #{OUT_OF_MEMORY_LENGTH}"));
    system_call(r#gen, Sys::Write);
    r#gen.emit("\tmov\tx0, #1");
    system_call(r#gen, Sys::Exit);
}

/// Emits the memory runtime. Every string and list `p` is counted: `[p - 8]` holds how many
/// variables, list slots, and temporaries refer to it, and it is freed when that reaches 0. A
/// string also holds its length in bytes at `[p - 16]`, so measuring it takes no scan, and its
/// bytes end with a terminator. `stone.live` counts the objects allocated and not yet freed.
///
/// - `stone.alloc` returns a new object with room for `x0` bytes and a count of 1
/// - `stone.str_new` returns a new string of length `x0` with a count of 1 and its terminator
///   written, whose bytes the caller fills in
/// - `stone.free_str` frees the string in `x9`, whose count just reached 0
/// - `stone.free_list` frees the list in `x9`, whose count just reached 0, along with its
///   elements' storage, first releasing each value its `kinds` field says is counted, through
///   `stone.free_enum` for enum values if `enums`; a list it releases that dies too waits on
///   `stone.lists_pending` for the same loop, so freeing never recurses through lists
/// - `stone.leak_check` exits with an error if the `STONE_LEAK_CHECK` environment variable is set
///   and any object is still allocated, which tests use to find leaks
///
/// The free routines are reached from inline releases that do not count as calls, so they
/// preserve every register a vreg can be in, clobbering only `x8` to `x11`, `x16`, `x17`, `x30`,
/// and the float registers. `stone.leak_check` also preserves `x0`, which holds `main`'s exit
/// status. Memory comes from [`allocator`].
pub fn memory_runtime(r#gen: &mut dyn AssemblyGenerator, enums: bool) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_leak_env:");
    r#gen.emit("\t.string \"STONE_LEAK_CHECK\"");
    r#gen.emit(".Lstone_leak_prefix:");
    r#gen.emit(&format!("\t.string \"{LEAK_PREFIX}\""));
    r#gen.emit(".Lstone_leak_suffix:");
    r#gen.emit(&format!("\t.string \"{LEAK_SUFFIX}\\n\""));
    r#gen.emit("\t.text");

    allocator(r#gen);

    r#gen.emit("stone.alloc:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tadd\tx0, x0, #8"); // room for the count
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    count_live(r#gen, 1);
    r#gen.emit("\tadd\tx0, x0, #8");
    pop_frame(r#gen, &[]);

    // the length, then the count, then the bytes and a terminator; x12 holds the length, which
    // stone.mem_alloc leaves alone
    r#gen.emit("stone.str_new:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tmov\tx12, x0");
    r#gen.emit("\tadd\tx0, x0, #17");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tstp\tx12, x9, [x0]");
    count_live(r#gen, 1);
    r#gen.emit("\tadd\tx0, x0, #16");
    r#gen.emit("\tstrb\twzr, [x0, x12]");
    pop_frame(r#gen, &[]);

    // the length is where the allocation starts
    r#gen.emit("stone.free_str:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tsub\tx9, x9, #16");
    r#gen.emit("\tbl\tstone.mem_free");
    count_live(r#gen, -1);
    pop_frame(r#gen, &[]);

    // a list whose count reached 0 joins the lists waiting on stone.lists_pending, linked through
    // their capacity, and the first call frees them in a loop, so freeing lists nested any number
    // of levels deep, as a struct holding a list of itself makes them, never recurses; x19 holds
    // the list being freed and x20 the index of the element being released
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.free_list:");
    r#gen.emit("\tadrp\tx16, stone.lists_pending");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.lists_pending]");
    r#gen.emit("\tstr\tx17, [x9, #8]");
    r#gen.emit("\tstr\tx9, [x16, :lo12:stone.lists_pending]");
    r#gen.emit("\tadrp\tx16, stone.freeing_lists");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.freeing_lists]");
    r#gen.emit("\tcbnz\tx17, .Lfree_list_queued");
    r#gen.emit("\tmov\tx17, #1");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.freeing_lists]");
    push_frame(r#gen, &saved);
    r#gen.emit(".Lfree_list_next:");
    r#gen.emit("\tadrp\tx16, stone.lists_pending");
    r#gen.emit("\tldr\tx19, [x16, :lo12:stone.lists_pending]");
    r#gen.emit("\tcbz\tx19, .Lfree_list_done");
    r#gen.emit("\tldr\tx17, [x19, #8]");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.lists_pending]");
    r#gen.emit("\tldr\tx10, [x19, #24]");
    r#gen.emit("\tcbz\tx10, .Lfree_list_storage"); // the elements are not counted
    r#gen.emit("\tldr\tx10, [x19, #32]");
    r#gen.emit("\tcmp\tx10, #1");
    r#gen.emit("\tb.ne\t.Lfree_list_wide");
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
    if enums {
        r#gen.emit("\tcmp\tx10, #3");
        r#gen.emit("\tb.eq\t.Lfree_list_enum");
    }
    // only queues the list, since this call is freeing lists already
    r#gen.emit("\tbl\tstone.free_list");
    r#gen.emit("\tb\t.Lfree_list_loop");
    r#gen.emit(".Lfree_list_str:");
    r#gen.emit("\tbl\tstone.free_str");
    r#gen.emit("\tb\t.Lfree_list_loop");
    if enums {
        // freeing an enum value only queues it while another is being freed, so this
        // recursion stays shallow too
        r#gen.emit(".Lfree_list_enum:");
        r#gen.emit("\tbl\tstone.free_enum");
        r#gen.emit("\tb\t.Lfree_list_loop");
    }
    // each element of a list of tuples is several slots, each counted as its field's kind says:
    // x20 is the slot, x21 how many fields of the element are left, and x22 the kinds of the
    // rest, two bits each
    r#gen.emit(".Lfree_list_wide:");
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit("\tmov\tx21, #0");
    r#gen.emit(".Lfree_list_wide_loop:");
    r#gen.emit("\tldr\tx10, [x19]");
    r#gen.emit("\tcmp\tx20, x10");
    r#gen.emit("\tb.ge\t.Lfree_list_storage");
    r#gen.emit("\tcbnz\tx21, .Lfree_list_wide_field");
    r#gen.emit("\tldr\tx21, [x19, #32]");
    r#gen.emit("\tldr\tx22, [x19, #24]");
    r#gen.emit(".Lfree_list_wide_field:");
    r#gen.emit("\tand\tx11, x22, #3");
    r#gen.emit("\tlsr\tx22, x22, #2");
    r#gen.emit("\tsub\tx21, x21, #1");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tldr\tx9, [x10, x20, lsl #3]");
    r#gen.emit("\tadd\tx20, x20, #1");
    r#gen.emit("\tcbz\tx11, .Lfree_list_wide_loop"); // this field is not counted
    // a removed map entry's counted values were zeroed
    r#gen.emit("\tcbz\tx9, .Lfree_list_wide_loop");
    r#gen.emit("\tldur\tx10, [x9, #-8]");
    r#gen.emit("\tsubs\tx10, x10, #1");
    r#gen.emit("\tstur\tx10, [x9, #-8]");
    r#gen.emit("\tb.ne\t.Lfree_list_wide_loop");
    r#gen.emit("\tcmp\tx11, #1");
    r#gen.emit("\tb.eq\t.Lfree_list_wide_str");
    if enums {
        r#gen.emit("\tcmp\tx11, #3");
        r#gen.emit("\tb.eq\t.Lfree_list_wide_enum");
    }
    r#gen.emit("\tbl\tstone.free_list");
    r#gen.emit("\tb\t.Lfree_list_wide_loop");
    r#gen.emit(".Lfree_list_wide_str:");
    r#gen.emit("\tbl\tstone.free_str");
    r#gen.emit("\tb\t.Lfree_list_wide_loop");
    if enums {
        r#gen.emit(".Lfree_list_wide_enum:");
        r#gen.emit("\tbl\tstone.free_enum");
        r#gen.emit("\tb\t.Lfree_list_wide_loop");
    }
    r#gen.emit(".Lfree_list_storage:");
    // a map's hash index
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbz\tx9, .Lfree_list_unindexed");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit(".Lfree_list_unindexed:");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tsub\tx9, x19, #8");
    r#gen.emit("\tbl\tstone.mem_free");
    count_live(r#gen, -1);
    r#gen.emit("\tb\t.Lfree_list_next");
    r#gen.emit(".Lfree_list_done:");
    r#gen.emit("\tadrp\tx16, stone.freeing_lists");
    r#gen.emit("\tmov\tx17, #0");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.freeing_lists]");
    pop_frame(r#gen, &saved);
    r#gen.emit(".Lfree_list_queued:");
    r#gen.emit("\tret");

    // the count's digits are written backwards into 32 bytes of stack
    r#gen.emit("stone.leak_check:");
    push_frame(r#gen, &["x0"]);
    r#gen.emit("\tadrp\tx9, stone.live");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.live]");
    r#gen.emit("\tcbz\tx9, .Lleak_check_done");
    address(r#gen, "x0", ".Lstone_leak_env");
    r#gen.emit("\tbl\tstone.getenv");
    r#gen.emit("\tcbz\tx0, .Lleak_check_done");
    write_stderr(r#gen, ".Lstone_leak_prefix", LEAK_PREFIX.len());
    r#gen.emit("\tsub\tsp, sp, #32");
    r#gen.emit("\tadd\tx1, sp, #32");
    r#gen.emit("\tadrp\tx9, stone.live");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.live]");
    r#gen.emit("\tmov\tx10, #10");
    r#gen.emit(".Lleak_check_digit:");
    r#gen.emit("\tudiv\tx11, x9, x10");
    r#gen.emit("\tmsub\tx16, x11, x10, x9");
    r#gen.emit("\tadd\tw16, w16, #48"); // '0'
    r#gen.emit("\tstrb\tw16, [x1, #-1]!");
    r#gen.emit("\tmov\tx9, x11");
    r#gen.emit("\tcbnz\tx9, .Lleak_check_digit");
    r#gen.emit("\tadd\tx2, sp, #32");
    r#gen.emit("\tsub\tx2, x2, x1");
    r#gen.emit("\tmov\tx0, #2"); // stderr
    system_call(r#gen, Sys::Write);
    write_stderr(r#gen, ".Lstone_leak_suffix", LEAK_SUFFIX.len() + 1);
    r#gen.emit("\tmov\tx0, #1");
    system_call(r#gen, Sys::Exit);
    r#gen.emit(".Lleak_check_done:");
    pop_frame(r#gen, &["x0"]);
}

/// Emits the routines for enum values, ported from the x86-64 runtime's `enum_runtime` under the
/// same labels and layout: the tag at `[p]`, the kinds of the slots at `[p + 8]`, and the slots
/// from `[p + 16]`.
///
/// - `stone.enum_new` returns a new enum value with tag `x0`, kinds `x1`, and `x2` slots, for the
///   caller to fill in
/// - `stone.free_enum` frees the enum value in `x9`, whose count just reached 0, first releasing
///   each value its kinds say is counted, and queuing enum values on `stone.free_pending` rather
///   than recursing into them, so freeing a long chain takes no more stack than freeing one. It
///   clobbers only the registers the other free routines do.
///
/// With `handles`, a value whose tag is negative is a socket or listener, whose descriptor it
/// closes first, through `net::free_handle`, before the tag becomes the queue's link.
pub fn enum_runtime(r#gen: &mut dyn AssemblyGenerator, handles: bool) {
    // x19 holds the tag and x20 the kinds while stone.alloc runs
    r#gen.emit("stone.enum_new:");
    push_frame(r#gen, &["x19", "x20"]);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tlsl\tx0, x2, #3");
    r#gen.emit("\tadd\tx0, x0, #16");
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tstp\tx19, x20, [x0]");
    pop_frame(r#gen, &["x19", "x20"]);

    // x19 holds the value being freed, x20 the kinds of its slots left, and x21 the next slot
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.free_enum:");
    if handles {
        r#gen.emit("\tldr\tx16, [x9]");
        r#gen.emit("\ttbz\tx16, #63, .Lfree_enum_value");
        r#gen.emit("\tstr\tx30, [sp, #-16]!");
        r#gen.emit("\tbl\t.Lfree_handle");
        r#gen.emit("\tldr\tx30, [sp], #16");
        r#gen.emit(".Lfree_enum_value:");
    }
    r#gen.emit("\tadrp\tx16, stone.free_pending");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tstr\tx17, [x9]");
    r#gen.emit("\tstr\tx9, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tadrp\tx16, stone.freeing");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.freeing]");
    r#gen.emit("\tcbnz\tx17, .Lfree_enum_queued");
    r#gen.emit("\tmov\tx17, #1");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.freeing]");
    push_frame(r#gen, &saved);
    r#gen.emit(".Lfree_enum_next:");
    r#gen.emit("\tadrp\tx16, stone.free_pending");
    r#gen.emit("\tldr\tx19, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tcbz\tx19, .Lfree_enum_done");
    r#gen.emit("\tldr\tx17, [x19]");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tldr\tx20, [x19, #8]");
    r#gen.emit("\tadd\tx21, x19, #16");
    r#gen.emit(".Lfree_enum_slot:");
    r#gen.emit("\tcbz\tx20, .Lfree_enum_storage"); // no counted slots are left
    r#gen.emit("\tand\tx11, x20, #3");
    r#gen.emit("\tlsr\tx20, x20, #2");
    r#gen.emit("\tldr\tx9, [x21]");
    r#gen.emit("\tadd\tx21, x21, #8");
    r#gen.emit("\tcbz\tx11, .Lfree_enum_slot"); // this slot is not counted
    r#gen.emit("\tldur\tx10, [x9, #-8]");
    r#gen.emit("\tsubs\tx10, x10, #1");
    r#gen.emit("\tstur\tx10, [x9, #-8]");
    r#gen.emit("\tb.ne\t.Lfree_enum_slot");
    r#gen.emit("\tcmp\tx11, #1");
    r#gen.emit("\tb.eq\t.Lfree_enum_str");
    r#gen.emit("\tcmp\tx11, #2");
    r#gen.emit("\tb.eq\t.Lfree_enum_list");
    // an enum value joins the queue
    if handles {
        r#gen.emit("\tldr\tx16, [x9]");
        r#gen.emit("\ttbz\tx16, #63, .Lfree_enum_queue");
        r#gen.emit("\tbl\t.Lfree_handle");
        r#gen.emit(".Lfree_enum_queue:");
    }
    r#gen.emit("\tadrp\tx16, stone.free_pending");
    r#gen.emit("\tldr\tx17, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tstr\tx17, [x9]");
    r#gen.emit("\tstr\tx9, [x16, :lo12:stone.free_pending]");
    r#gen.emit("\tb\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_str:");
    r#gen.emit("\tbl\tstone.free_str");
    r#gen.emit("\tb\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_list:");
    r#gen.emit("\tbl\tstone.free_list");
    r#gen.emit("\tb\t.Lfree_enum_slot");
    r#gen.emit(".Lfree_enum_storage:");
    r#gen.emit("\tsub\tx9, x19, #8");
    r#gen.emit("\tbl\tstone.mem_free");
    count_live(r#gen, -1);
    r#gen.emit("\tb\t.Lfree_enum_next");
    r#gen.emit(".Lfree_enum_done:");
    r#gen.emit("\tadrp\tx16, stone.freeing");
    r#gen.emit("\tmov\tx17, #0");
    r#gen.emit("\tstr\tx17, [x16, :lo12:stone.freeing]");
    pop_frame(r#gen, &saved);
    r#gen.emit(".Lfree_enum_queued:");
    r#gen.emit("\tret");
    if handles {
        super::net::free_handle(r#gen);
    }
}

/// Emits code that writes the `length` bytes at `label` to stderr, clobbering `x0` to `x2` and
/// `x8`.
fn write_stderr(r#gen: &mut dyn AssemblyGenerator, label: &str, length: usize) {
    r#gen.emit("\tmov\tx0, #2"); // stderr
    address(r#gen, "x1", label);
    r#gen.emit(&format!("\tmov\tx2, #{length}"));
    system_call(r#gen, Sys::Write);
}

/// Emits `stone.getenv`, which returns the value of the environment variable named by the
/// string in `x0`, or 0 if it is not set, like libc's `getenv`. It reads the environment `main`
/// saved in `stone.envp`, and clobbers `x9` to `x11`, `x16`, and `x17`.
pub fn env_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.getenv:");
    r#gen.emit("\tadrp\tx9, stone.envp");
    r#gen.emit("\tldr\tx9, [x9, :lo12:stone.envp]");
    r#gen.emit(".Lgetenv_entry:");
    r#gen.emit("\tldr\tx10, [x9], #8");
    r#gen.emit("\tcbz\tx10, .Lgetenv_none"); // the list ends with null
    r#gen.emit("\tmov\tx11, #0");
    // the entry must start with the name, then =
    r#gen.emit(".Lgetenv_compare:");
    r#gen.emit("\tldrb\tw16, [x0, x11]");
    r#gen.emit("\tcbz\tw16, .Lgetenv_name_end");
    r#gen.emit("\tldrb\tw17, [x10, x11]");
    r#gen.emit("\tcmp\tw16, w17");
    r#gen.emit("\tb.ne\t.Lgetenv_entry");
    r#gen.emit("\tadd\tx11, x11, #1");
    r#gen.emit("\tb\t.Lgetenv_compare");
    r#gen.emit(".Lgetenv_name_end:");
    r#gen.emit("\tldrb\tw17, [x10, x11]");
    r#gen.emit("\tcmp\tw17, #61"); // '='
    r#gen.emit("\tb.ne\t.Lgetenv_entry");
    r#gen.emit("\tadd\tx0, x10, x11");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tret");
    r#gen.emit(".Lgetenv_none:");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");
}

/// Emits the routines behind `print`, each writing one value to stdout with no newline, so a call
/// like `print("n", 1)` writes `n`, a space, `1`, and a newline with four calls.
///
/// - `stone.print_int` writes the signed integer in `x0`
/// - `stone.print_str` writes the null-terminated string `x0` points to
/// - `stone.print_bool` writes `true` if `x0` is nonzero and `false` otherwise
/// - `stone.print_char` writes the byte in `w0`
/// - `stone.print_str_quoted` writes the string `x0` points to as a printed list shows its
///   strings, quoted and escaped like Python's `repr`
///
/// They write into the text being built (see [`text_runtime`]), which `print` then writes to
/// stdout in one go and `str` makes a string of, and clobber `x0` to `x2`, `x8` to `x13`, `x16`,
/// `x17`, and `x30`. `stone.text_eprint` is emitted only if the program `eprints`.
pub fn print(r#gen: &mut dyn AssemblyGenerator, strings: bool, eprints: bool) {
    r#gen.emit("\t.section\t.rodata");
    r#gen.emit(".Lstone_true:");
    r#gen.emit("\t.string \"true\"");
    r#gen.emit(".Lstone_false:");
    r#gen.emit("\t.string \"false\"");
    r#gen.emit("\t.text");

    // digits are written backwards from the end of 32 bytes, which holds all 20 and a sign
    r#gen.emit("stone.print_int:");
    push_frame(r#gen, &[]);
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
    r#gen.emit("\tbl\tstone.text_append");
    pop_frame(r#gen, &[]);

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
    r#gen.emit("\tb\tstone.text_append");
    r#gen.emit(".Lprint_str_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.print_bool:");
    r#gen.emit("\tcmp\tx0, #0");
    address(r#gen, "x0", ".Lstone_true");
    address(r#gen, "x9", ".Lstone_false");
    r#gen.emit("\tcsel\tx0, x0, x9, ne");
    r#gen.emit("\tb\tstone.print_str");

    r#gen.emit("stone.print_char:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tsub\tsp, sp, #16");
    r#gen.emit("\tstrb\tw0, [sp]");
    r#gen.emit("\tmov\tx1, sp");
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("\tbl\tstone.text_append");
    pop_frame(r#gen, &[]);

    print_str_quoted(r#gen);
    text_runtime(r#gen, strings, eprints);
}

/// Emits the text builder that every `stone.print_*` routine writes into, as the x86-64
/// `text_runtime` does, with `stone.text` pointing to the block of the text being built.
///
/// - `stone.text_begin` starts a new text and returns the one being built before
/// - `stone.text_append` adds the `x2` bytes at `x1` to the text
/// - `stone.text_end` finishes the text as a new string with a count of 1, and goes back to
///   building the text in `x0`
/// - `stone.text_print` writes the text and a newline to stdout, frees it, and goes back to
///   building the text in `x0`
/// - `stone.text_eprint` does the same, writing to stderr
/// - `stone.write_out` writes the `x2` bytes at `x1` to stdout, however many `write` calls that
///   takes
///
/// `stone.text_print` and `stone.text_eprint` preserve the callee-saved registers, and the others
/// clobber `x0` to `x3`, `x8` to `x13`, `x16`, `x17`, and `x30`. `stone.text_end` is emitted only
/// if the program has `strings`, since it counts the string it makes in `stone.live`, and
/// `stone.text_eprint` only if the program `eprints`.
fn text_runtime(r#gen: &mut dyn AssemblyGenerator, strings: bool, eprints: bool) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.text:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("\t.text");

    r#gen.emit("stone.text_begin:");
    push_frame(r#gen, &[]);
    r#gen.emit(&format!("\tmov\tx0, #{TEXT_BLOCK}"));
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tstr\txzr, [x0]");
    r#gen.emit(&format!("\tmov\tx9, #{}", TEXT_BLOCK - 17));
    r#gen.emit("\tstr\tx9, [x0, #8]");
    r#gen.emit("\tadrp\tx9, stone.text");
    r#gen.emit("\tldr\tx10, [x9, :lo12:stone.text]");
    r#gen.emit("\tstr\tx0, [x9, :lo12:stone.text]");
    r#gen.emit("\tmov\tx0, x10");
    pop_frame(r#gen, &[]);

    // x12 holds the block and x13 the length it will have
    r#gen.emit("stone.text_append:");
    push_frame(r#gen, &[]);
    r#gen.emit("\tadrp\tx9, stone.text");
    r#gen.emit("\tldr\tx12, [x9, :lo12:stone.text]");
    r#gen.emit("\tldr\tx13, [x12]");
    r#gen.emit("\tadd\tx13, x13, x2");
    r#gen.emit("\tldr\tx9, [x12, #8]");
    r#gen.emit("\tcmp\tx13, x9");
    r#gen.emit("\tb.ls\t.Ltext_append_copy");
    r#gen.emit("\tstp\tx1, x2, [sp, #-16]!");
    r#gen.emit("\tmov\tx0, x12");
    r#gen.emit("\tadd\tx1, x13, x13");
    r#gen.emit("\tadd\tx1, x1, #17");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tldp\tx1, x2, [sp], #16");
    r#gen.emit("\tmov\tx12, x0");
    r#gen.emit("\tadd\tx9, x13, x13");
    r#gen.emit("\tstr\tx9, [x12, #8]");
    r#gen.emit("\tadrp\tx9, stone.text");
    r#gen.emit("\tstr\tx12, [x9, :lo12:stone.text]");
    r#gen.emit(".Ltext_append_copy:");
    r#gen.emit("\tldr\tx9, [x12]");
    r#gen.emit("\tstr\tx13, [x12]");
    r#gen.emit("\tadd\tx10, x12, #16");
    r#gen.emit("\tadd\tx10, x10, x9");
    copy_bytes(r#gen, "x10", "x1", "x2", ".Ltext_append_bytes");
    pop_frame(r#gen, &[]);

    if strings {
        r#gen.emit("stone.text_end:");
        r#gen.emit("\tadrp\tx9, stone.text");
        r#gen.emit("\tldr\tx10, [x9, :lo12:stone.text]");
        r#gen.emit("\tstr\tx0, [x9, :lo12:stone.text]");
        r#gen.emit("\tldr\tx11, [x10]");
        r#gen.emit("\tadd\tx0, x10, #16");
        r#gen.emit("\tstrb\twzr, [x0, x11]");
        r#gen.emit("\tmov\tx11, #1");
        r#gen.emit("\tstr\tx11, [x10, #8]");
        count_live(r#gen, 1);
        r#gen.emit("\tret");
    }

    // x19 holds the text to go back to, x20 the one written, and x21 the file it goes to
    let saved = ["x19", "x20", "x21"];
    if eprints {
        r#gen.emit("stone.text_eprint:");
        r#gen.emit("\tmov\tx3, #2"); // stderr
        r#gen.emit("\tb\t.Ltext_write");
    }
    r#gen.emit("stone.text_print:");
    r#gen.emit("\tmov\tx3, #1"); // stdout
    r#gen.emit(".Ltext_write:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx21, x3");
    r#gen.emit("\tmov\tx0, #10"); // '\n'
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tadrp\tx9, stone.text");
    r#gen.emit("\tldr\tx20, [x9, :lo12:stone.text]");
    r#gen.emit("\tadd\tx1, x20, #16");
    r#gen.emit("\tldr\tx2, [x20]");
    r#gen.emit("\tmov\tx3, x21");
    r#gen.emit("\tbl\t.Lwrite_fd");
    r#gen.emit("\tmov\tx9, x20");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tadrp\tx9, stone.text");
    r#gen.emit("\tstr\tx19, [x9, :lo12:stone.text]");
    pop_frame(r#gen, &saved);

    // a write may take fewer bytes than asked, or be interrupted before it takes any; x3 holds
    // the file, which the call keeps
    r#gen.emit("stone.write_out:");
    r#gen.emit("\tmov\tx3, #1"); // stdout
    r#gen.emit(".Lwrite_fd:");
    push_frame(r#gen, &[]);
    r#gen.emit(".Lwrite_out_next:");
    r#gen.emit("\tcbz\tx2, .Lwrite_out_done");
    r#gen.emit("\tmov\tx0, x3");
    system_call(r#gen, Sys::Write);
    r#gen.emit("\tcmn\tx0, #4"); // EINTR
    r#gen.emit("\tb.eq\t.Lwrite_out_next");
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tb.le\t.Lwrite_out_done");
    r#gen.emit("\tadd\tx1, x1, x0");
    r#gen.emit("\tsub\tx2, x2, x0");
    r#gen.emit("\tb\t.Lwrite_out_next");
    r#gen.emit(".Lwrite_out_done:");
    pop_frame(r#gen, &[]);
}

/// Emits `stone.print_str_quoted`, which prints a string inside a list the way
/// `stdlib::repr_in_list` writes it, as the x86-64 routine does. Each run of bytes that needs no
/// escape is appended at once.
fn print_str_quoted(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds where the run starts, x20 the quote, and x21 where the run ends, then the byte
    // that ended it
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.print_str_quoted:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    // x9 is set by a ' and x10 by a "
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit("\tmov\tx10, #0");
    r#gen.emit("\tmov\tx11, x0");
    r#gen.emit(".Lprint_quoted_scan:");
    r#gen.emit("\tldrb\tw12, [x11], #1");
    r#gen.emit("\tcbz\tw12, .Lprint_quoted_choose");
    r#gen.emit("\tcmp\tw12, #39"); // '\''
    r#gen.emit("\tb.ne\t.Lprint_quoted_double");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit("\tb\t.Lprint_quoted_scan");
    r#gen.emit(".Lprint_quoted_double:");
    r#gen.emit("\tcmp\tw12, #34"); // '"'
    r#gen.emit("\tb.ne\t.Lprint_quoted_scan");
    r#gen.emit("\tmov\tx10, #1");
    r#gen.emit("\tb\t.Lprint_quoted_scan");
    r#gen.emit(".Lprint_quoted_choose:");
    r#gen.emit("\tmov\tx20, #39");
    r#gen.emit("\tcbz\tx9, .Lprint_quoted_open");
    r#gen.emit("\tcbnz\tx10, .Lprint_quoted_open");
    r#gen.emit("\tmov\tx20, #34");
    r#gen.emit(".Lprint_quoted_open:");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.print_char");
    // a run ends at the terminator or a byte that needs an escape
    r#gen.emit(".Lprint_quoted_run:");
    r#gen.emit("\tmov\tx21, x19");
    r#gen.emit(".Lprint_quoted_plain:");
    r#gen.emit("\tldrb\tw9, [x21]");
    r#gen.emit("\tcmp\tw9, #92"); // '\\'
    r#gen.emit("\tb.eq\t.Lprint_quoted_flush");
    r#gen.emit("\tcmp\tw9, w20");
    r#gen.emit("\tb.eq\t.Lprint_quoted_flush");
    r#gen.emit("\tcmp\tw9, #32");
    r#gen.emit("\tb.lo\t.Lprint_quoted_flush"); // a control character or the terminator
    r#gen.emit("\tcmp\tw9, #127");
    r#gen.emit("\tb.eq\t.Lprint_quoted_flush");
    r#gen.emit("\tadd\tx21, x21, #1");
    r#gen.emit("\tb\t.Lprint_quoted_plain");
    r#gen.emit(".Lprint_quoted_flush:");
    r#gen.emit("\tsub\tx2, x21, x19");
    r#gen.emit("\tcbz\tx2, .Lprint_quoted_special");
    r#gen.emit("\tmov\tx1, x19");
    r#gen.emit("\tbl\tstone.text_append");
    r#gen.emit(".Lprint_quoted_special:");
    r#gen.emit("\tldrb\tw9, [x21]");
    r#gen.emit("\tcbz\tw9, .Lprint_quoted_close");
    r#gen.emit("\tadd\tx19, x21, #1");
    r#gen.emit("\tmov\tx21, x9");
    r#gen.emit("\tmov\tx0, #92");
    r#gen.emit("\tbl\tstone.print_char");
    // what follows the backslash: n, t, r, the byte itself, or x and two hex digits
    r#gen.emit("\tmov\tx0, #110"); // 'n'
    r#gen.emit("\tcmp\tx21, #10");
    r#gen.emit("\tb.eq\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tx0, #116"); // 't'
    r#gen.emit("\tcmp\tx21, #9");
    r#gen.emit("\tb.eq\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tx0, #114"); // 'r'
    r#gen.emit("\tcmp\tx21, #13");
    r#gen.emit("\tb.eq\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tcmp\tx21, #92");
    r#gen.emit("\tb.eq\t.Lprint_quoted_code");
    r#gen.emit("\tcmp\tx21, x20");
    r#gen.emit("\tb.eq\t.Lprint_quoted_code");
    r#gen.emit("\tmov\tx0, #120"); // 'x'
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tlsr\tx0, x21, #4");
    r#gen.emit("\tbl\t.Lprint_quoted_hex");
    r#gen.emit("\tubfx\tx0, x21, #0, #4");
    r#gen.emit("\tbl\t.Lprint_quoted_hex");
    r#gen.emit("\tb\t.Lprint_quoted_run");
    r#gen.emit(".Lprint_quoted_code:");
    r#gen.emit("\tbl\tstone.print_char");
    r#gen.emit("\tb\t.Lprint_quoted_run");
    r#gen.emit(".Lprint_quoted_close:");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.print_char");
    pop_frame(r#gen, &saved);
    // prints the hex digit in x0, from 0 to 15, in lowercase, returning to the caller's caller
    r#gen.emit(".Lprint_quoted_hex:");
    r#gen.emit("\tadd\tx9, x0, #48"); // '0'
    r#gen.emit("\tadd\tx10, x0, #87"); // 'a' - 10
    r#gen.emit("\tcmp\tx0, #10");
    r#gen.emit("\tcsel\tx0, x10, x9, hs");
    r#gen.emit("\tb\tstone.print_char");
}

/// Emits `stone.str_float`, which returns the float whose bits are in `x0` as a new string, the
/// way `stone.print_float` prints it. It needs `floats::float_runtime` and the memory runtime.
pub fn str_float_runtime(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the bits, and x20 the new string
    let saved = ["x19", "x20"];
    r#gen.emit("stone.str_float:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx0, #63"); // more than any float's text
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\tstone.format_float");
    // the text is shorter than the room it was given
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.cstr_len");
    r#gen.emit("\tstur\tx0, [x20, #-16]");
    r#gen.emit("\tmov\tx0, x20");
    pop_frame(r#gen, &saved);
}

/// Emits the string runtime. Strings are null-terminated, counted, and carry their length (see
/// [`memory_runtime`]), and `+` makes a new one with `stone.str_new`.
///
/// - `stone.str_len` returns the length in bytes of the string in `x0`, from its header, and
///   changes only `x0`
/// - `stone.cstr_len` returns the length of the null-terminated bytes at `x0`, such as a program
///   argument or a pointer into a string, by scanning them, and changes only `x0`, `x9`, and
///   `x10`
/// - `stone.str_eq` returns 1 if the strings in `x0` and `x1` hold the same bytes, else 0
/// - `stone.str_concat` returns a new string holding `x0` followed by `x1`
/// - `stone.str_slice` returns a new string holding the `x1` bytes at `x0`
pub fn string_runtime(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("stone.str_len:");
    r#gen.emit("\tldur\tx0, [x0, #-16]");
    r#gen.emit("\tret");

    r#gen.emit("stone.cstr_len:");
    r#gen.emit("\tmov\tx9, x0");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit(".Lcstr_len_loop:");
    r#gen.emit("\tldrb\tw10, [x9, x0]");
    r#gen.emit("\tcbz\tw10, .Lcstr_len_done");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tb\t.Lcstr_len_loop");
    r#gen.emit(".Lcstr_len_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.str_eq:");
    // strings of different lengths differ without a look at their bytes
    r#gen.emit("\tldur\tx9, [x0, #-16]");
    r#gen.emit("\tldur\tx10, [x1, #-16]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Lstr_eq_differ");
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
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx9, x0");
    copy_bytes(r#gen, "x9", "x19", "x21", ".Lstr_concat_left");
    r#gen.emit("\tadd\tx22, x22, #1"); // copy the terminator too
    copy_bytes(r#gen, "x9", "x20", "x22", ".Lstr_concat_right");
    pop_frame(r#gen, &saved);

    // x19 and x20 hold the bytes and their count across stone.str_new
    let saved = ["x19", "x20"];
    r#gen.emit("stone.str_slice:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx0, x1");
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx9, x0");
    copy_bytes(r#gen, "x9", "x19", "x20", ".Lstr_slice_copy");
    r#gen.emit("\tstrb\twzr, [x9]");
    pop_frame(r#gen, &saved);
}

/// Emits the list runtime. A list is a pointer to a counted 40-byte header `{len, cap, data,
/// kinds, width}` (see [`memory_runtime`]), laid out as x64's is (see
/// `x64::builtins::list_runtime`): `len` and `cap` count 8-byte slots, each element is `width`
/// of them, and `kinds` says, two bits for each value of an element, whether it is a string (1),
/// a list (2), or not counted (0).
///
/// - `stone.list_new_wide` returns a list of `x0` slots whose values the caller fills in, with
///   `kinds` set to `x1` and `width` to `x2`
/// - `stone.list_new` does the same for a list of one value each, whose `width` is 1
/// - `stone.retain_slots` adds a reference to each counted value of the `x1` slots from `x0`,
///   which start at an element and whose `kinds` and `width` are `x2` and `x3`, clobbering only
///   `x8` to `x11`, `x16`, and `x17`
/// - `stone.list_append` appends `x1` to list `x0`, doubling its capacity when full
/// - `stone.list_copy` returns in `x9` a copy of the list in `x9`, retaining each element if
///   they are counted, and drops one reference to the original, which something else still
///   holds. Like the free routines, it is reached from an inline `list_unique` that does not
///   count as a call, so it clobbers only `x8` to `x11`, `x16`, `x17`, `x30`, and the float
///   registers.
///
/// Indexing is generated inline by `Arm64Generator::list_slot` (in `emit.rs`) rather than called
/// here.
pub fn list_runtime(r#gen: &mut dyn AssemblyGenerator, maps_shrink: bool) {
    // x19 holds the list, x20 its length, x21 what its elements are, and x22 their width
    let saved = ["x19", "x20", "x21", "x22"];
    r#gen.emit("stone.list_new:");
    r#gen.emit("\tmov\tx2, #1");
    r#gen.emit("stone.list_new_wide:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx21, x1");
    r#gen.emit("\tmov\tx22, x2");
    r#gen.emit("\tmov\tx0, #48");
    r#gen.emit("\tbl\tstone.alloc");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tstr\tx20, [x19]");
    r#gen.emit("\tstr\txzr, [x19, #40]");
    r#gen.emit("\tstr\tx21, [x19, #24]");
    r#gen.emit("\tstr\tx22, [x19, #32]");
    // room for at least 4 elements, so the first appends do not reallocate
    r#gen.emit("\tmov\tx9, #4");
    r#gen.emit("\tcmp\tx20, x9");
    r#gen.emit("\tcsel\tx9, x20, x9, ge");
    r#gen.emit("\tstr\tx9, [x19, #8]");
    r#gen.emit("\tlsl\tx0, x9, #3");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit("\tmov\tx0, x19");
    pop_frame(r#gen, &saved);

    // x8 counts the slots, x9 how many fields of the element are left, and x10 the kinds of the
    // rest, two bits each
    r#gen.emit("stone.retain_slots:");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(".Lretain_slots_loop:");
    r#gen.emit("\tcmp\tx8, x1");
    r#gen.emit("\tb.ge\t.Lretain_slots_done");
    r#gen.emit("\tcbnz\tx9, .Lretain_slots_field");
    r#gen.emit("\tmov\tx9, x3");
    r#gen.emit("\tmov\tx10, x2");
    r#gen.emit(".Lretain_slots_field:");
    r#gen.emit("\tand\tx11, x10, #3");
    r#gen.emit("\tlsr\tx10, x10, #2");
    r#gen.emit("\tsub\tx9, x9, #1");
    r#gen.emit("\tldr\tx16, [x0, x8, lsl #3]");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tcbz\tx11, .Lretain_slots_loop"); // this field is not counted
    r#gen.emit("\tldur\tx17, [x16, #-8]");
    r#gen.emit("\tadd\tx17, x17, #1");
    r#gen.emit("\tstur\tx17, [x16, #-8]");
    r#gen.emit("\tb\t.Lretain_slots_loop");
    r#gen.emit(".Lretain_slots_done:");
    r#gen.emit("\tret");

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
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit(".Llist_append_store:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tstr\tx20, [x10, x9, lsl #3]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstr\tx9, [x19]");
    r#gen.emit("\tmov\tx0, #0"); // append returns none
    pop_frame(r#gen, &saved);

    // x19 holds the original, x20 the copy, x10 and x11 their elements, and x17 what they are
    let mut saved = CALLER_SAVED.to_vec();
    saved.extend(["x19", "x20"]);
    r#gen.emit("stone.list_copy:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x9");
    if maps_shrink {
        // a map's removed entries go before it is copied, since the copy has no index to tell
        r#gen.emit("\tldr\tx16, [x19, #40]");
        r#gen.emit("\tcbz\tx16, .Llist_copy_closed");
        r#gen.emit("\tbl\t.Lmap_compact");
        r#gen.emit(".Llist_copy_closed:");
    }
    r#gen.emit("\tldr\tx0, [x19]");
    r#gen.emit("\tldr\tx1, [x19, #24]");
    r#gen.emit("\tldr\tx2, [x19, #32]");
    r#gen.emit("\tbl\tstone.list_new_wide");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tldr\tx10, [x20, #16]");
    r#gen.emit("\tldr\tx11, [x19, #16]");
    r#gen.emit("\tldr\tx17, [x19, #24]");
    // the values of wider elements are retained after copying
    r#gen.emit("\tldr\tx16, [x19, #32]");
    r#gen.emit("\tcmp\tx16, #1");
    r#gen.emit("\tcsel\tx17, x17, xzr, eq");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Llist_copy_loop:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tcmp\tx8, x9");
    r#gen.emit("\tb.ge\t.Llist_copy_done");
    r#gen.emit("\tldr\tx9, [x11, x8, lsl #3]");
    r#gen.emit("\tstr\tx9, [x10, x8, lsl #3]");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tcbz\tx17, .Llist_copy_loop"); // the elements are not counted
    r#gen.emit("\tldur\tx16, [x9, #-8]");
    r#gen.emit("\tadd\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x9, #-8]");
    r#gen.emit("\tb\t.Llist_copy_loop");
    r#gen.emit(".Llist_copy_done:");
    r#gen.emit("\tldr\tx3, [x19, #32]");
    r#gen.emit("\tcmp\tx3, #1");
    r#gen.emit("\tb.eq\t.Llist_copy_retained");
    r#gen.emit("\tldr\tx0, [x20, #16]");
    r#gen.emit("\tldr\tx1, [x19]");
    r#gen.emit("\tldr\tx2, [x19, #24]");
    r#gen.emit("\tbl\tstone.retain_slots");
    r#gen.emit(".Llist_copy_retained:");
    // the variable or slot that held the original now holds the copy instead
    r#gen.emit("\tldur\tx16, [x19, #-8]");
    r#gen.emit("\tsub\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x19, #-8]");
    r#gen.emit("\tmov\tx9, x20");
    pop_frame(r#gen, &saved);
}

/// Emits the map runtime, the port of x64's `map_runtime` with the same layout and contracts: a
/// map's index holds its capacity, how many entries were removed, and whether the keys are
/// strings, then its slots, each 0 if empty, -1 if its entry was removed, or else 1 more than the
/// first slot of an entry.
///
/// - `stone.map_find` returns the first slot of the entry of key `x1` in map `x0`, or -1 if there
///   is none, with `x2` 1 for a string key
/// - `stone.map_add` adds an entry for key `x1`, which the map does not have, at the end of map
///   `x0`, retaining a string key, and returns its first slot. The caller fills in its values.
/// - `stone.map_take` removes the entry of key `x1` from map `x0` and returns its first slot, or
///   -1 if there is none. The caller takes its values and zeroes the counted ones.
/// - `stone.map_settle` closes up map `x0` if more than half of its entries were removed
/// - `stone.map_compact` closes up map `x0` if any of its entries were removed
/// - `stone.map_holes` returns how many entries were removed from map `x0` and not closed up
pub fn map_runtime(r#gen: &mut dyn AssemblyGenerator, strings: bool) {
    // x19 holds the map, x20 the key, and x21 whether it is a string
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    let enter = |r#gen: &mut dyn AssemblyGenerator| {
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tmov\tx20, x1");
        r#gen.emit("\tmov\tx21, x2");
    };
    // loads a 64-bit constant in four pieces
    let constant = |r#gen: &mut dyn AssemblyGenerator, reg: &str, value: u64| {
        r#gen.emit(&format!("\tmovz\t{reg}, #{:#x}", value & 0xffff));
        for shift in [16, 32, 48] {
            r#gen.emit(&format!(
                "\tmovk\t{reg}, #{:#x}, lsl #{shift}",
                (value >> shift) & 0xffff
            ));
        }
    };

    r#gen.emit("stone.map_find:");
    enter(r#gen);
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbnz\tx9, .Lmap_find_indexed");
    r#gen.emit("\tbl\t.Lmap_rebuild");
    r#gen.emit(".Lmap_find_indexed:");
    r#gen.emit("\tbl\t.Lmap_probe");
    // an empty slot holds 0, which becomes -1
    r#gen.emit("\tldr\tx0, [x0]");
    r#gen.emit("\tsub\tx0, x0, #1");
    pop_frame(r#gen, &saved);

    // x22 is the first slot of the new entry, and x23 counts the slots appended
    r#gen.emit("stone.map_add:");
    enter(r#gen);
    // a bigger index when one more entry would fill more than half of it: 2 * (len + width)
    // slots must be at most capacity * width
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbz\tx9, .Lmap_add_rebuild");
    r#gen.emit("\tldr\tx10, [x19]");
    r#gen.emit("\tldr\tx11, [x19, #32]");
    r#gen.emit("\tadd\tx10, x10, x11");
    r#gen.emit("\tadd\tx10, x10, x10");
    r#gen.emit("\tldr\tx16, [x9]");
    r#gen.emit("\tmul\tx16, x16, x11");
    r#gen.emit("\tcmp\tx10, x16");
    r#gen.emit("\tb.le\t.Lmap_add_room");
    r#gen.emit(".Lmap_add_rebuild:");
    // removed entries make room first
    r#gen.emit("\tbl\t.Lmap_squeeze");
    r#gen.emit("\tbl\t.Lmap_rebuild");
    r#gen.emit(".Lmap_add_room:");
    r#gen.emit("\tldr\tx22, [x19]");
    r#gen.emit("\tldr\tx23, [x19, #32]");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit(".Lmap_add_slot:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tbl\tstone.list_append");
    r#gen.emit("\tmov\tx1, #0"); // the values are filled in by the caller
    r#gen.emit("\tsub\tx23, x23, #1");
    r#gen.emit("\tcbnz\tx23, .Lmap_add_slot");
    r#gen.emit("\tcbz\tx21, .Lmap_add_key");
    r#gen.emit("\tldur\tx9, [x20, #-8]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstur\tx9, [x20, #-8]");
    r#gen.emit(".Lmap_add_key:");
    r#gen.emit("\tbl\t.Lmap_probe");
    r#gen.emit("\tadd\tx9, x22, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tmov\tx0, x22");
    pop_frame(r#gen, &saved);

    r#gen.emit("stone.map_take:");
    enter(r#gen);
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbnz\tx9, .Lmap_take_indexed");
    r#gen.emit("\tbl\t.Lmap_rebuild");
    r#gen.emit(".Lmap_take_indexed:");
    r#gen.emit("\tbl\t.Lmap_probe");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tcbz\tx9, .Lmap_take_missing");
    r#gen.emit("\tmov\tx10, #-1");
    r#gen.emit("\tstr\tx10, [x0]");
    r#gen.emit("\tldr\tx11, [x19, #40]");
    r#gen.emit("\tldr\tx12, [x11, #8]");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tstr\tx12, [x11, #8]");
    r#gen.emit(".Lmap_take_missing:");
    // an empty slot holds 0, which becomes -1
    r#gen.emit("\tsub\tx0, x9, #1");
    pop_frame(r#gen, &saved);

    r#gen.emit("stone.map_holes:");
    r#gen.emit("\tldr\tx9, [x0, #40]");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tcbz\tx9, .Lmap_holes_done");
    r#gen.emit("\tldr\tx0, [x9, #8]");
    r#gen.emit(".Lmap_holes_done:");
    r#gen.emit("\tret");

    // closes up when removed entries are more than half: 2 * removed * width > len
    r#gen.emit("stone.map_settle:");
    r#gen.emit("\tldr\tx9, [x0, #40]");
    r#gen.emit("\tcbz\tx9, .Lmap_settle_done");
    r#gen.emit("\tldr\tx9, [x9, #8]");
    r#gen.emit("\tldr\tx10, [x0, #32]");
    r#gen.emit("\tmul\tx9, x9, x10");
    r#gen.emit("\tadd\tx9, x9, x9");
    r#gen.emit("\tldr\tx10, [x0]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.gt\tstone.map_compact");
    r#gen.emit(".Lmap_settle_done:");
    r#gen.emit("\tret");

    r#gen.emit("stone.map_compact:");
    enter(r#gen);
    r#gen.emit("\tbl\t.Lmap_compact");
    pop_frame(r#gen, &saved);

    // closes up map x19 if any of its entries were removed, keeping x20 and x21 for its caller
    let kept = ["x20", "x21"];
    r#gen.emit(".Lmap_compact:");
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbz\tx9, .Lmap_compact_done");
    r#gen.emit("\tldr\tx10, [x9, #8]");
    r#gen.emit("\tcbnz\tx10, .Lmap_compact_holes");
    r#gen.emit(".Lmap_compact_done:");
    r#gen.emit("\tret");
    r#gen.emit(".Lmap_compact_holes:");
    push_frame(r#gen, &kept);
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tldr\tx21, [x9, #16]");
    r#gen.emit("\tbl\t.Lmap_squeeze");
    r#gen.emit("\tbl\t.Lmap_rebuild");
    pop_frame(r#gen, &kept);

    // moves the entries of map x19 left over removed ones, for .Lmap_rebuild to index, keeping
    // x20 for its caller: x22 is the first slot of each entry in turn, x23 where the next entry
    // kept goes, and x24 the width
    let kept = ["x20", "x22", "x23", "x24"];
    r#gen.emit(".Lmap_squeeze:");
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbz\tx9, .Lmap_squeeze_none");
    r#gen.emit("\tldr\tx10, [x9, #8]");
    r#gen.emit("\tcbnz\tx10, .Lmap_squeeze_holes");
    r#gen.emit(".Lmap_squeeze_none:");
    r#gen.emit("\tret");
    r#gen.emit(".Lmap_squeeze_holes:");
    push_frame(r#gen, &kept);
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit("\tmov\tx23, #0");
    r#gen.emit("\tldr\tx24, [x19, #32]");
    r#gen.emit(".Lmap_squeeze_next:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tcmp\tx22, x9");
    r#gen.emit("\tb.ge\t.Lmap_squeeze_done");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tldr\tx20, [x9, x22, lsl #3]");
    if strings {
        // a removed entry's string key was zeroed
        r#gen.emit("\tcbz\tx21, .Lmap_squeeze_probe");
        r#gen.emit("\tcbz\tx20, .Lmap_squeeze_skip");
        r#gen.emit(".Lmap_squeeze_probe:");
    }
    // an entry is kept if its key still finds it
    r#gen.emit("\tbl\t.Lmap_probe");
    r#gen.emit("\tldr\tx9, [x0]");
    r#gen.emit("\tadd\tx10, x22, #1");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.ne\t.Lmap_squeeze_skip");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tmov\tx11, #0");
    r#gen.emit(".Lmap_squeeze_move:");
    r#gen.emit("\tadd\tx12, x22, x11");
    r#gen.emit("\tldr\tx13, [x9, x12, lsl #3]");
    r#gen.emit("\tadd\tx12, x23, x11");
    r#gen.emit("\tstr\tx13, [x9, x12, lsl #3]");
    r#gen.emit("\tadd\tx11, x11, #1");
    r#gen.emit("\tcmp\tx11, x24");
    r#gen.emit("\tb.lt\t.Lmap_squeeze_move");
    r#gen.emit("\tadd\tx23, x23, x24");
    r#gen.emit(".Lmap_squeeze_skip:");
    r#gen.emit("\tadd\tx22, x22, x24");
    r#gen.emit("\tb\t.Lmap_squeeze_next");
    r#gen.emit(".Lmap_squeeze_done:");
    r#gen.emit("\tstr\tx23, [x19]");
    pop_frame(r#gen, &kept);

    // replaces map x19's index with one for its entries and one more, none of them removed,
    // keeping x20 and x21 for its caller: x22 is the first slot of each entry in turn, and x23
    // the capacity
    let kept = ["x20", "x22", "x23"];
    r#gen.emit(".Lmap_rebuild:");
    push_frame(r#gen, &kept);
    r#gen.emit("\tldr\tx9, [x19, #40]");
    r#gen.emit("\tcbz\tx9, .Lmap_rebuild_size");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit(".Lmap_rebuild_size:");
    r#gen.emit("\tmov\tx23, #8");
    r#gen.emit("\tldr\tx10, [x19]");
    r#gen.emit("\tldr\tx11, [x19, #32]");
    r#gen.emit("\tadd\tx10, x10, x11");
    r#gen.emit("\tadd\tx10, x10, x10");
    r#gen.emit(".Lmap_rebuild_grow:");
    r#gen.emit("\tmul\tx16, x23, x11");
    r#gen.emit("\tcmp\tx10, x16");
    r#gen.emit("\tb.le\t.Lmap_rebuild_alloc");
    r#gen.emit("\tadd\tx23, x23, x23");
    r#gen.emit("\tb\t.Lmap_rebuild_grow");
    r#gen.emit(".Lmap_rebuild_alloc:");
    r#gen.emit("\tlsl\tx0, x23, #3");
    r#gen.emit("\tadd\tx0, x0, #24");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tstr\tx0, [x19, #40]");
    r#gen.emit("\tstr\tx23, [x0]");
    r#gen.emit("\tstr\txzr, [x0, #8]");
    r#gen.emit("\tstr\tx21, [x0, #16]");
    r#gen.emit("\tadd\tx10, x0, #16");
    r#gen.emit("\tmov\tx9, x23");
    r#gen.emit(".Lmap_rebuild_zero:");
    r#gen.emit("\tstr\txzr, [x10, x9, lsl #3]");
    r#gen.emit("\tsub\tx9, x9, #1");
    r#gen.emit("\tcbnz\tx9, .Lmap_rebuild_zero");
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit(".Lmap_rebuild_next:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tcmp\tx22, x9");
    r#gen.emit("\tb.ge\t.Lmap_rebuild_done");
    r#gen.emit("\tldr\tx9, [x19, #16]");
    r#gen.emit("\tldr\tx20, [x9, x22, lsl #3]");
    // the keys differ, so this finds an empty slot
    r#gen.emit("\tbl\t.Lmap_probe");
    r#gen.emit("\tadd\tx9, x22, #1");
    r#gen.emit("\tstr\tx9, [x0]");
    r#gen.emit("\tldr\tx9, [x19, #32]");
    r#gen.emit("\tadd\tx22, x22, x9");
    r#gen.emit("\tb\t.Lmap_rebuild_next");
    r#gen.emit(".Lmap_rebuild_done:");
    pop_frame(r#gen, &kept);

    // returns in x0 the address of the slot of map x19's index that holds key x20's entry, or
    // of the empty slot where it would go, probing one slot after another and passing removed
    // entries' slots: x9 is the slot number, x13 the index's first slot, x14 the mask of a slot
    // number, x15 the slot's address, and x16 what it holds
    r#gen.emit(".Lmap_probe:");
    push_frame(r#gen, &[]);
    // the hash, FNV-1a over a string's bytes, then mixed by Fibonacci hashing
    r#gen.emit("\tmov\tx9, x20");
    if strings {
        r#gen.emit("\tcbz\tx21, .Lmap_probe_mix");
        constant(r#gen, "x9", 0xcbf2_9ce4_8422_2325);
        constant(r#gen, "x10", 0x0000_0100_0000_01b3);
        r#gen.emit("\tmov\tx11, x20");
        r#gen.emit(".Lmap_probe_byte:");
        r#gen.emit("\tldrb\tw12, [x11], #1");
        r#gen.emit("\tcbz\tw12, .Lmap_probe_mix");
        r#gen.emit("\teor\tx9, x9, x12");
        r#gen.emit("\tmul\tx9, x9, x10");
        r#gen.emit("\tb\t.Lmap_probe_byte");
    }
    r#gen.emit(".Lmap_probe_mix:");
    constant(r#gen, "x10", 0x9e37_79b9_7f4a_7c15);
    r#gen.emit("\tmul\tx9, x9, x10");
    r#gen.emit("\tlsr\tx10, x9, #32");
    r#gen.emit("\teor\tx9, x9, x10");
    r#gen.emit("\tldr\tx13, [x19, #40]");
    r#gen.emit("\tldr\tx14, [x13]");
    r#gen.emit("\tsub\tx14, x14, #1");
    r#gen.emit("\tadd\tx13, x13, #24");
    r#gen.emit(".Lmap_probe_slot:");
    r#gen.emit("\tand\tx9, x9, x14");
    r#gen.emit("\tlsl\tx15, x9, #3");
    r#gen.emit("\tadd\tx15, x13, x15");
    r#gen.emit("\tldr\tx16, [x15]");
    r#gen.emit("\tcbz\tx16, .Lmap_probe_done");
    r#gen.emit("\tcmn\tx16, #1");
    r#gen.emit("\tb.eq\t.Lmap_probe_next"); // a removed entry's
    r#gen.emit("\tldr\tx17, [x19, #16]");
    r#gen.emit("\tsub\tx16, x16, #1");
    r#gen.emit("\tldr\tx17, [x17, x16, lsl #3]");
    r#gen.emit("\tcmp\tx17, x20");
    r#gen.emit("\tb.eq\t.Lmap_probe_done"); // the same int, or the same string
    if strings {
        r#gen.emit("\tcbz\tx21, .Lmap_probe_next");
        // stone.str_eq changes only x0, x1, x9, and x10
        r#gen.emit("\tmov\tx12, x9");
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tmov\tx1, x17");
        r#gen.emit("\tbl\tstone.str_eq");
        r#gen.emit("\tmov\tx9, x12");
        r#gen.emit("\tcbnz\tx0, .Lmap_probe_done");
    }
    r#gen.emit(".Lmap_probe_next:");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tb\t.Lmap_probe_slot");
    r#gen.emit(".Lmap_probe_done:");
    r#gen.emit("\tmov\tx0, x15");
    pop_frame(r#gen, &[]);
}

/// Emits each list routine in `labels`, which are the [`LIST_ROUTINES`] a program calls, under
/// the same labels and contracts as the x64 ones, with arguments in `x0` onward.
///
/// - `stone.list_slice` returns a new list of the elements of list `x0` that the slice from `x1`
///   to `x2` by `x3` picks, as `stdlib::slice_indices` picks them, retaining them if they are
///   counted. Bit 0 of `x4` says `x1` is missing, bit 1 `x2`, and bit 2 `x3`, and the step is
///   never 0, which the caller checks.
/// - `stone.list_pop` removes the element at position `x1` of list `x0`, moving the ones after it
///   down, and returns it in `x0` with the list's reference to it. The caller checks that `x1` is
///   in range.
/// - `stone.list_insert` puts `x2` at position `x1` of list `x0`, moving the elements from there
///   on up, and the list takes the caller's reference to it. The caller checks that `x1` is at
///   most the length.
/// - `stone.list_extend` appends every element of list `x1` to list `x0`, retaining them if they
///   are counted. The two may be the same list.
/// - `stone.list_reuse` takes a reference to list `x0`, whose elements are not counted, and
///   returns it if nothing else refers to it, for arithmetic to write its result over. Otherwise
///   it drops the reference and returns a new list of length `x1` from `stone.list_new`.
/// - `stone.list_sort_int`, `stone.list_sort_float`, and `stone.list_sort_str` sort list `x0`
///   (see [`sort_runtime`]).
///
/// [`LIST_ROUTINES`]: crate::codegen::context::LIST_ROUTINES
pub fn list_methods(r#gen: &mut dyn AssemblyGenerator, labels: &[&str]) {
    sort_runtime(r#gen, labels);
    if labels.contains(&"stone.list_slice") {
        list_slice(r#gen);
    }
    if labels.contains(&"stone.list_pop") {
        list_pop(r#gen);
    }
    if labels.contains(&"stone.list_insert") {
        list_insert(r#gen);
    }
    if labels.contains(&"stone.list_extend") {
        list_extend(r#gen);
    }
    if labels.contains(&"stone.list_reuse") {
        r#gen.emit("stone.list_reuse:");
        r#gen.emit("\tldur\tx9, [x0, #-8]");
        r#gen.emit("\tcmp\tx9, #1");
        r#gen.emit("\tb.ne\t.Llist_reuse_new");
        r#gen.emit("\tret");
        r#gen.emit(".Llist_reuse_new:");
        // something else still holds it, so this only drops a reference
        r#gen.emit("\tsub\tx9, x9, #1");
        r#gen.emit("\tstur\tx9, [x0, #-8]");
        r#gen.emit("\tmov\tx0, x1");
        r#gen.emit("\tmov\tx1, #0");
        r#gen.emit("\tb\tstone.list_new");
    }
}

/// Emits `stone.list_pop` (see [`list_methods`]).
fn list_pop(r#gen: &mut dyn AssemblyGenerator) {
    // x9 holds the elements, x10 the new length, and x11 the element popped
    r#gen.emit("stone.list_pop:");
    r#gen.emit("\tldr\tx9, [x0, #16]");
    r#gen.emit("\tldr\tx11, [x9, x1, lsl #3]");
    r#gen.emit("\tldr\tx10, [x0]");
    r#gen.emit("\tsub\tx10, x10, #1");
    r#gen.emit("\tstr\tx10, [x0]");
    r#gen.emit(".Llist_pop_loop:");
    r#gen.emit("\tcmp\tx1, x10");
    r#gen.emit("\tb.ge\t.Llist_pop_done");
    r#gen.emit("\tadd\tx12, x1, #1");
    r#gen.emit("\tldr\tx13, [x9, x12, lsl #3]");
    r#gen.emit("\tstr\tx13, [x9, x1, lsl #3]");
    r#gen.emit("\tmov\tx1, x12");
    r#gen.emit("\tb\t.Llist_pop_loop");
    r#gen.emit(".Llist_pop_done:");
    r#gen.emit("\tmov\tx0, x11");
    r#gen.emit("\tret");
}

/// Emits `stone.list_insert` (see [`list_methods`]).
fn list_insert(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the list, x20 the position, and x21 the element
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.list_insert:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx21, x2");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #8]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.lt\t.Llist_insert_shift");
    r#gen.emit("\tlsl\tx10, x10, #1");
    r#gen.emit("\tstr\tx10, [x19, #8]");
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\tlsl\tx1, x10, #3");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit(".Llist_insert_shift:");
    // move each element from the end down to the position up by one
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit(".Llist_insert_loop:");
    r#gen.emit("\tcmp\tx9, x20");
    r#gen.emit("\tb.le\t.Llist_insert_place");
    r#gen.emit("\tsub\tx11, x9, #1");
    r#gen.emit("\tldr\tx12, [x10, x11, lsl #3]");
    r#gen.emit("\tstr\tx12, [x10, x9, lsl #3]");
    r#gen.emit("\tmov\tx9, x11");
    r#gen.emit("\tb\t.Llist_insert_loop");
    r#gen.emit(".Llist_insert_place:");
    r#gen.emit("\tstr\tx21, [x10, x20, lsl #3]");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tstr\tx9, [x19]");
    r#gen.emit("\tmov\tx0, #0"); // insert returns none
    pop_frame(r#gen, &saved);
}

/// Emits `stone.list_extend` (see [`list_methods`]).
fn list_extend(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the list, x20 the other list, and x21 how many elements it adds, read before
    // growing, since the other list may be this one
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.list_extend:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tldr\tx21, [x20]");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tadd\tx9, x9, x21");
    r#gen.emit("\tldr\tx10, [x19, #8]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.le\t.Llist_extend_copy");
    // double the capacity, or more if that is not enough
    r#gen.emit("\tlsl\tx10, x10, #1");
    r#gen.emit("\tcmp\tx10, x9");
    r#gen.emit("\tcsel\tx10, x9, x10, lt");
    r#gen.emit("\tstr\tx10, [x19, #8]");
    r#gen.emit("\tldr\tx0, [x19, #16]");
    r#gen.emit("\tlsl\tx1, x10, #3");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit("\tstr\tx0, [x19, #16]");
    r#gen.emit(".Llist_extend_copy:");
    // the other list's elements are read after growing, which may have moved them
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tldr\tx10, [x19, #16]");
    r#gen.emit("\tlsl\tx9, x9, #3");
    r#gen.emit("\tadd\tx10, x10, x9");
    r#gen.emit("\tldr\tx11, [x20, #16]");
    r#gen.emit("\tldr\tx17, [x19, #24]");
    // the values of wider elements are retained after copying
    r#gen.emit("\tldr\tx16, [x19, #32]");
    r#gen.emit("\tcmp\tx16, #1");
    r#gen.emit("\tcsel\tx17, x17, xzr, eq");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Llist_extend_loop:");
    r#gen.emit("\tcmp\tx8, x21");
    r#gen.emit("\tb.ge\t.Llist_extend_done");
    r#gen.emit("\tldr\tx9, [x11, x8, lsl #3]");
    r#gen.emit("\tstr\tx9, [x10, x8, lsl #3]");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tcbz\tx17, .Llist_extend_loop"); // the elements are not counted
    r#gen.emit("\tldur\tx16, [x9, #-8]");
    r#gen.emit("\tadd\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x9, #-8]");
    r#gen.emit("\tb\t.Llist_extend_loop");
    r#gen.emit(".Llist_extend_done:");
    r#gen.emit("\tldr\tx3, [x19, #32]");
    r#gen.emit("\tcmp\tx3, #1");
    r#gen.emit("\tb.eq\t.Llist_extend_retained");
    r#gen.emit("\tmov\tx0, x10");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit("\tldr\tx2, [x19, #24]");
    r#gen.emit("\tbl\tstone.retain_slots");
    r#gen.emit(".Llist_extend_retained:");
    r#gen.emit("\tldr\tx9, [x19]");
    r#gen.emit("\tadd\tx9, x9, x21");
    r#gen.emit("\tstr\tx9, [x19]");
    r#gen.emit("\tmov\tx0, #0"); // extend returns none
    pop_frame(r#gen, &saved);
}

/// Emits `stone.list_slice` (see [`list_methods`]).
fn list_slice(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the list, x20 the first index, x21 the step, x22 the count, and x23 the new list;
    // x9 holds the length while the bounds are clamped
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.list_slice:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx9, [x19]");
    // the bounds count elements, each `width` slots
    r#gen.emit("\tldr\tx10, [x19, #32]");
    r#gen.emit("\tsdiv\tx9, x9, x10");
    slice_bounds(r#gen, "list_slice");
    r#gen.emit("\tldr\tx10, [x19, #32]");
    r#gen.emit("\tcmp\tx10, #1");
    r#gen.emit("\tb.ne\t.Llist_slice_wide");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tldr\tx1, [x19, #24]");
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx23, x0");
    r#gen.emit("\tldr\tx10, [x23, #16]");
    r#gen.emit("\tldr\tx11, [x19, #16]");
    r#gen.emit("\tldr\tx17, [x19, #24]");
    r#gen.emit("\tmov\tx12, x20");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Llist_slice_loop:");
    r#gen.emit("\tcmp\tx8, x22");
    r#gen.emit("\tb.ge\t.Llist_slice_done");
    r#gen.emit("\tldr\tx9, [x11, x12, lsl #3]");
    r#gen.emit("\tstr\tx9, [x10, x8, lsl #3]");
    r#gen.emit("\tadd\tx12, x12, x21");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tcbz\tx17, .Llist_slice_loop"); // the elements are not counted
    r#gen.emit("\tldur\tx16, [x9, #-8]");
    r#gen.emit("\tadd\tx16, x16, #1");
    r#gen.emit("\tstur\tx16, [x9, #-8]");
    r#gen.emit("\tb\t.Llist_slice_loop");
    r#gen.emit(".Llist_slice_done:");
    r#gen.emit("\tmov\tx0, x23");
    pop_frame(r#gen, &saved);
    // each element picked is `width` slots, in x10, copied from x13 to x11, then retained: x12
    // is the element picked, x8 how many are copied, and x9 the slot within one
    r#gen.emit(".Llist_slice_wide:");
    r#gen.emit("\tmul\tx0, x22, x10");
    r#gen.emit("\tldr\tx1, [x19, #24]");
    r#gen.emit("\tmov\tx2, x10");
    r#gen.emit("\tbl\tstone.list_new_wide");
    r#gen.emit("\tmov\tx23, x0");
    r#gen.emit("\tldr\tx10, [x19, #32]");
    r#gen.emit("\tldr\tx11, [x23, #16]");
    r#gen.emit("\tmov\tx12, x20");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Llist_slice_wide_loop:");
    r#gen.emit("\tcmp\tx8, x22");
    r#gen.emit("\tb.ge\t.Llist_slice_wide_copied");
    r#gen.emit("\tmul\tx13, x12, x10");
    r#gen.emit("\tlsl\tx13, x13, #3");
    r#gen.emit("\tldr\tx14, [x19, #16]");
    r#gen.emit("\tadd\tx13, x14, x13");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(".Llist_slice_wide_slot:");
    r#gen.emit("\tldr\tx15, [x13, x9, lsl #3]");
    r#gen.emit("\tstr\tx15, [x11]");
    r#gen.emit("\tadd\tx11, x11, #8");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.lt\t.Llist_slice_wide_slot");
    r#gen.emit("\tadd\tx12, x12, x21");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tb\t.Llist_slice_wide_loop");
    r#gen.emit(".Llist_slice_wide_copied:");
    r#gen.emit("\tldr\tx0, [x23, #16]");
    r#gen.emit("\tldr\tx1, [x23]");
    r#gen.emit("\tldr\tx2, [x19, #24]");
    r#gen.emit("\tmov\tx3, x10");
    r#gen.emit("\tbl\tstone.retain_slots");
    r#gen.emit("\tmov\tx0, x23");
    pop_frame(r#gen, &saved);
}

/// Emits the steps of a slice routine that pick what the slice from `x1` to `x2` by `x3` takes
/// from a list or string of length `x9`, as `stdlib::slice_indices` does, leaving the first index
/// in `x20`, the step in `x21`, and the count in `x22`. Bit 0 of `x4` says `x1` is missing, bit 1
/// `x2`, and bit 2 `x3`, and the step is never 0. It clobbers `x3`, `x10`, `x11`, and `x12`, and
/// `routine` keeps the labels of each routine apart.
pub(super) fn slice_bounds(r#gen: &mut dyn AssemblyGenerator, routine: &str) {
    let label = |name: &str| format!(".L{routine}_{name}");
    r#gen.emit(&format!("\ttbz\tx4, #2, {}", label("step")));
    r#gen.emit("\tmov\tx3, #1");
    r#gen.emit(&format!("{}:", label("step")));
    // a step of MIN, the only one that doubles to 0, acts like -MAX
    r#gen.emit("\tadd\tx10, x3, x3");
    r#gen.emit(&format!("\tcbnz\tx10, {}", label("bounds")));
    r#gen.emit("\tadd\tx3, x3, #1");
    r#gen.emit(&format!("{}:", label("bounds")));
    r#gen.emit("\tmov\tx21, x3");
    // x10 is -1 for a negative step and 0 otherwise
    r#gen.emit("\tasr\tx10, x3, #63");
    // a missing start is len - 1 going backward and 0 going forward
    r#gen.emit("\tmov\tx11, x1");
    r#gen.emit(&format!("\ttbz\tx4, #0, {}", label("start")));
    r#gen.emit("\tsub\tx11, x9, #1");
    r#gen.emit("\tand\tx11, x11, x10");
    r#gen.emit(&format!("\tb\t{}", label("start_done")));
    r#gen.emit(&format!("{}:", label("start")));
    clamp_slice_bound(r#gen, &label("start"));
    r#gen.emit("\tmov\tx20, x11");
    // a missing stop is -1 going backward and len going forward
    r#gen.emit("\tmov\tx11, x2");
    r#gen.emit(&format!("\ttbz\tx4, #1, {}", label("stop")));
    r#gen.emit("\tbic\tx11, x9, x10");
    r#gen.emit("\tadd\tx11, x11, x10");
    r#gen.emit(&format!("\tb\t{}", label("stop_done")));
    r#gen.emit(&format!("{}:", label("stop")));
    clamp_slice_bound(r#gen, &label("stop"));
    // the distance to cover, start - stop going backward and stop - start going forward
    r#gen.emit("\tsub\tx11, x11, x20");
    r#gen.emit("\tmov\tx12, x21");
    r#gen.emit(&format!("\tcbz\tx10, {}", label("count")));
    r#gen.emit("\tneg\tx11, x11");
    r#gen.emit("\tneg\tx12, x12");
    r#gen.emit(&format!("{}:", label("count")));
    // count = (distance - 1) / |step| + 1 for a positive distance, else 0
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit("\tcmp\tx11, #0");
    r#gen.emit(&format!("\tb.le\t{}", label("counted")));
    r#gen.emit("\tsub\tx11, x11, #1");
    r#gen.emit("\tudiv\tx11, x11, x12");
    r#gen.emit("\tadd\tx22, x11, #1");
    r#gen.emit(&format!("{}:", label("counted")));
}

/// Emits the steps of [`slice_bounds`] that clamp the bound in `x11` to a list or string of
/// length `x9`, given, in `x10`, -1 for a negative step or 0 otherwise, as the x86-64 version
/// does. The labels start with `prefix`, and the steps end at `<prefix>_done`.
fn clamp_slice_bound(r#gen: &mut dyn AssemblyGenerator, prefix: &str) {
    r#gen.emit(&format!("\ttbz\tx11, #63, {prefix}_high"));
    // len is at most 2^60, so adding it cannot overflow
    r#gen.emit("\tadd\tx11, x11, x9");
    r#gen.emit(&format!("\ttbz\tx11, #63, {prefix}_done"));
    r#gen.emit("\tmov\tx11, x10");
    r#gen.emit(&format!("\tb\t{prefix}_done"));
    r#gen.emit(&format!("{prefix}_high:"));
    r#gen.emit("\tcmp\tx11, x9");
    r#gen.emit(&format!("\tb.lt\t{prefix}_done"));
    r#gen.emit("\tadd\tx11, x9, x10");
    r#gen.emit(&format!("{prefix}_done:"));
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
pub(super) fn jump_if_space(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tcmp\t{reg}, #32"));
    r#gen.emit(&format!("\tb.eq\t{label}"));
    // tab, newline, vertical tab, form feed, and carriage return are 9 through 13
    r#gen.emit(&format!("\tsub\tw16, {reg}, #9"));
    r#gen.emit("\tcmp\tw16, #4");
    r#gen.emit(&format!("\tb.ls\t{label}"));
}

/// Emits the input routines named in `used`, which read stdin with the `read` syscall into a
/// buffer, the same way as x64's `io_runtime`.
///
/// - `stone.input` prints the string in `x0` unless it is null, then returns the next line of
///   stdin as a new string without its newline, or 0 at the end of the input
/// - `stone.eof` returns 1 if stdin has nothing left and 0 otherwise
/// - `stone.stdin_fill` reads into the buffer from its start and returns how many bytes it
///   read, or 0 at the end of the input or on an error
///
/// `stone.input` needs `print`'s routines, the string runtime, and the allocator.
pub fn io_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.stdin_pos:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.stdin_len:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("stone.stdin_buffer:");
    r#gen.emit(&format!("\t.zero\t{STDIN_BUFFER}"));
    r#gen.emit("\t.text");

    r#gen.emit("stone.stdin_fill:");
    r#gen.emit("\tmov\tx0, #0"); // stdin
    address(r#gen, "x1", "stone.stdin_buffer");
    r#gen.emit(&format!("\tmov\tx2, #{STDIN_BUFFER}"));
    system_call(r#gen, Sys::Read);
    r#gen.emit("\tcmp\tx0, #0");
    r#gen.emit("\tcsel\tx0, x0, xzr, gt");
    r#gen.emit("\tadrp\tx9, stone.stdin_len");
    r#gen.emit("\tstr\tx0, [x9, :lo12:stone.stdin_len]");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tstr\txzr, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tret");

    if used.contains(&"stone.eof") {
        r#gen.emit("stone.eof:");
        r#gen.emit("\tadrp\tx9, stone.stdin_pos");
        r#gen.emit("\tldr\tx9, [x9, :lo12:stone.stdin_pos]");
        r#gen.emit("\tadrp\tx10, stone.stdin_len");
        r#gen.emit("\tldr\tx10, [x10, :lo12:stone.stdin_len]");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tb.lo\t.Leof_no");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.stdin_fill");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit("\tcset\tx0, eq");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Leof_no:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }

    if !used.contains(&"stone.input") {
        return;
    }

    // x19 holds the block a long line is gathered in, or 0 before there is one, x20 how many
    // bytes it holds, and x21 how many it has room for
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.input:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tcbz\tx0, .Linput_read");
    // straight to stdout, since a prompt is never part of a text being built
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tldur\tx2, [x0, #-16]");
    r#gen.emit("\tbl\tstone.write_out");
    r#gen.emit(".Linput_read:");
    r#gen.emit("\tmov\tx19, #0");
    r#gen.emit("\tmov\tx20, #0");
    r#gen.emit("\tmov\tx21, #0");
    r#gen.emit(".Linput_scan:");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tldr\tx1, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadrp\tx9, stone.stdin_len");
    r#gen.emit("\tldr\tx2, [x9, :lo12:stone.stdin_len]");
    r#gen.emit("\tcmp\tx1, x2");
    r#gen.emit("\tb.lo\t.Linput_search");
    r#gen.emit("\tbl\tstone.stdin_fill");
    r#gen.emit("\tcbnz\tx0, .Linput_scan");
    // the input ended, so what was gathered is the last line, unless nothing was
    r#gen.emit("\tcbnz\tx19, .Linput_gathered");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tb\t.Linput_done");
    // x3 looks for a newline from x1 up to x2 in the buffer at x0
    r#gen.emit(".Linput_search:");
    address(r#gen, "x0", "stone.stdin_buffer");
    r#gen.emit("\tmov\tx3, x1");
    r#gen.emit(".Linput_find:");
    r#gen.emit("\tcmp\tx3, x2");
    r#gen.emit("\tb.eq\t.Linput_partial");
    r#gen.emit("\tldrb\tw9, [x0, x3]");
    r#gen.emit("\tcmp\tw9, #10"); // newline
    r#gen.emit("\tb.eq\t.Linput_newline");
    r#gen.emit("\tadd\tx3, x3, #1");
    r#gen.emit("\tb\t.Linput_find");
    // the rest of the buffer is part of a longer line
    r#gen.emit(".Linput_partial:");
    r#gen.emit("\tadrp\tx9, stone.stdin_pos");
    r#gen.emit("\tstr\tx2, [x9, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadd\tx0, x0, x1");
    r#gen.emit("\tsub\tx1, x2, x1");
    r#gen.emit("\tbl\t.Linput_append");
    r#gen.emit("\tb\t.Linput_scan");
    // the line ends at x3, and the newline is read but left out
    r#gen.emit(".Linput_newline:");
    r#gen.emit("\tadd\tx9, x3, #1");
    r#gen.emit("\tadrp\tx10, stone.stdin_pos");
    r#gen.emit("\tstr\tx9, [x10, :lo12:stone.stdin_pos]");
    r#gen.emit("\tadd\tx0, x0, x1");
    r#gen.emit("\tsub\tx1, x3, x1");
    r#gen.emit("\tcbnz\tx19, .Linput_last_piece");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tb\t.Linput_done");
    r#gen.emit(".Linput_last_piece:");
    r#gen.emit("\tbl\t.Linput_append");
    // the gathered line becomes a string, and its block is freed
    r#gen.emit(".Linput_gathered:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit("\tmov\tx9, x19");
    r#gen.emit("\tbl\tstone.mem_free");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit(".Linput_done:");
    pop_frame(r#gen, &saved);

    // adds the x1 bytes at x0 to the block in x19, growing it to twice what it must hold
    r#gen.emit(".Linput_append:");
    r#gen.emit("\tstp\tx29, x30, [sp, #-16]!");
    r#gen.emit("\tadd\tx3, x20, x1");
    r#gen.emit("\tcmp\tx3, x21");
    r#gen.emit("\tb.ls\t.Linput_append_copy");
    r#gen.emit("\tstp\tx0, x1, [sp, #-16]!");
    r#gen.emit("\tstr\tx3, [sp, #-16]!");
    r#gen.emit("\tlsl\tx21, x3, #1");
    r#gen.emit("\tcbnz\tx19, .Linput_append_grow");
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tbl\tstone.mem_alloc");
    r#gen.emit("\tb\t.Linput_append_grown");
    r#gen.emit(".Linput_append_grow:");
    r#gen.emit("\tmov\tx0, x19");
    r#gen.emit("\tmov\tx1, x21");
    r#gen.emit("\tbl\tstone.mem_realloc");
    r#gen.emit(".Linput_append_grown:");
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldr\tx3, [sp], #16");
    r#gen.emit("\tldp\tx0, x1, [sp], #16");
    r#gen.emit(".Linput_append_copy:");
    r#gen.emit("\tadd\tx9, x19, x20");
    copy_bytes(r#gen, "x9", "x0", "x1", ".Linput_append_bytes");
    r#gen.emit("\tmov\tx20, x3");
    r#gen.emit("\tldp\tx29, x30, [sp], #16");
    r#gen.emit("\tret");
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
    r#gen.emit("\tbl\tstone.cstr_len");
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
/// `stdlib::parse_float` with the same messages, each emitted only if named in `used`. They need
/// the string runtime.
///
/// - `stone.parse_int_result` returns a new result of the int in the string in `x0`: `ok` of
///   the number (tag 0), or `err` (tag 1) of the message saying why it is none
/// - `stone.parse_float_result` does the same for a float, checking the grammar before
///   `stone.decimal_to_float` reads it
/// - `stone.fail_quoted` stops the program with the message `x0` followed by the string `x1`
///   and a closing quote
pub fn parse_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    // the pieces of a message are joined with stone.str_concat, so they are strings
    r#gen.emit("\t.data");
    for (label, text) in [
        (
            ".Lstone_int_invalid",
            "invalid literal for int() with base 10: '",
        ),
        (".Lstone_int_range", "int() argument out of range: '"),
        (
            ".Lstone_float_invalid",
            "could not convert string to float: '",
        ),
        (".Lstone_quote", "'"),
        (".Lstone_key_missing", "key not found: '"),
    ] {
        immortal_string(r#gen, label, text);
    }
    r#gen.emit("\t.section\t.rodata");
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

    if used.contains(&"stone.map_missing_str") {
        r#gen.emit("stone.map_missing_str:");
        r#gen.emit("\tmov\tx1, x0");
        address(r#gen, "x0", ".Lstone_key_missing");
        r#gen.emit("\tb\tstone.fail_quoted");
    }

    // each core returns the number in x0 and 0 in x2, or the start of the message in x2 and the
    // text in x1, for the entry points to make an err of
    let ints = used.contains(&"stone.parse_int_result");
    let floats = used.contains(&"stone.parse_float_result");
    let result_saved = ["x19", "x20"];
    for (label, core) in [
        ("stone.parse_int_result", ".Lparse_int_core"),
        ("stone.parse_float_result", ".Lparse_float_core"),
    ] {
        if used.contains(&label) {
            r#gen.emit(&format!("{label}:"));
            push_frame(r#gen, &result_saved);
            r#gen.emit(&format!("\tbl\t{core}"));
            r#gen.emit("\tb\t.Lparse_result");
        }
    }
    if used.contains(&"stone.parse_int_result") || used.contains(&"stone.parse_float_result") {
        // the frame of the entry point that branched here, with x19 holding the number, then the
        // first piece of the message, and x20 the message
        r#gen.emit(".Lparse_result:");
        r#gen.emit("\tcbnz\tx2, .Lparse_result_err");
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tmov\tx0, #0"); // ok
        r#gen.emit("\tmov\tx1, #0"); // an uncounted slot
        r#gen.emit("\tmov\tx2, #1");
        r#gen.emit("\tbl\tstone.enum_new");
        r#gen.emit("\tstr\tx19, [x0, #16]");
        r#gen.emit("\tb\t.Lparse_result_done");
        r#gen.emit(".Lparse_result_err:");
        r#gen.emit("\tmov\tx0, x2");
        r#gen.emit("\tbl\tstone.str_concat");
        r#gen.emit("\tmov\tx19, x0");
        address(r#gen, "x1", ".Lstone_quote");
        r#gen.emit("\tbl\tstone.str_concat");
        r#gen.emit("\tmov\tx20, x0");
        // the first piece was only for joining
        r#gen.emit("\tldur\tx16, [x19, #-8]");
        r#gen.emit("\tsubs\tx16, x16, #1");
        r#gen.emit("\tstur\tx16, [x19, #-8]");
        r#gen.emit("\tb.ne\t.Lparse_result_joined");
        r#gen.emit("\tmov\tx9, x19");
        r#gen.emit("\tbl\tstone.free_str");
        r#gen.emit(".Lparse_result_joined:");
        r#gen.emit("\tmov\tx0, #1"); // err
        r#gen.emit("\tmov\tx1, #1"); // a string slot
        r#gen.emit("\tmov\tx2, #1");
        r#gen.emit("\tbl\tstone.enum_new");
        r#gen.emit("\tstr\tx20, [x0, #16]");
        r#gen.emit(".Lparse_result_done:");
        pop_frame(r#gen, &result_saved);
    }

    if ints {
        // x0 keeps the text, x9 is the cursor, x12 where the digits start, x13 the value so far,
        // negated so that the most negative int fits, x11 whether a minus sign came first, and x14
        // whether the value overflowed
        r#gen.emit(".Lparse_int_core:");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit(".Lparse_int_lead:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        jump_if_space(r#gen, "w10", ".Lparse_int_lead");
        r#gen.emit("\tsub\tx9, x9, #1");
        r#gen.emit("\tmov\tx11, #0");
        r#gen.emit("\tcmp\tw10, #43"); // '+'
        r#gen.emit("\tb.eq\t.Lparse_int_sign");
        r#gen.emit("\tcmp\tw10, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lparse_int_digits");
        r#gen.emit("\tmov\tx11, #1");
        r#gen.emit(".Lparse_int_sign:");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit(".Lparse_int_digits:");
        r#gen.emit("\tmov\tx12, x9");
        r#gen.emit("\tmov\tx13, #0");
        r#gen.emit("\tmov\tx14, #0");
        r#gen.emit("\tmov\tx15, #10");
        r#gen.emit(".Lparse_int_digit:");
        r#gen.emit("\tldrb\tw10, [x9]");
        r#gen.emit("\tsub\tw10, w10, #48"); // '0'
        r#gen.emit("\tcmp\tw10, #9");
        r#gen.emit("\tb.hi\t.Lparse_int_digits_done");
        r#gen.emit("\tadd\tx9, x9, #1");
        // the product overflowed if its high half is not just the sign of its low half
        r#gen.emit("\tsmulh\tx17, x13, x15");
        r#gen.emit("\tmul\tx13, x13, x15");
        r#gen.emit("\tcmp\tx17, x13, asr #63");
        r#gen.emit("\tb.ne\t.Lparse_int_overflow");
        r#gen.emit("\tsubs\tx13, x13, x10");
        r#gen.emit("\tb.vc\t.Lparse_int_digit");
        // the digits are still read, since text after them makes the error a different one
        r#gen.emit(".Lparse_int_overflow:");
        r#gen.emit("\tmov\tx14, #1");
        r#gen.emit("\tb\t.Lparse_int_digit");
        r#gen.emit(".Lparse_int_digits_done:");
        r#gen.emit("\tcmp\tx9, x12"); // no digits
        r#gen.emit("\tb.eq\t.Lparse_int_invalid");
        // only whitespace may follow, which is checked before overflow, as in stdlib::parse_int
        r#gen.emit(".Lparse_int_trailing:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        r#gen.emit("\tcbz\tw10, .Lparse_int_end");
        jump_if_space(r#gen, "w10", ".Lparse_int_trailing");
        r#gen.emit(".Lparse_int_invalid:");
        r#gen.emit("\tmov\tx1, x0");
        address(r#gen, "x2", ".Lstone_int_invalid");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_end:");
        r#gen.emit("\tcbnz\tx14, .Lparse_int_range");
        r#gen.emit("\tcbnz\tx11, .Lparse_int_done");
        r#gen.emit("\tnegs\tx13, x13");
        r#gen.emit("\tb.vs\t.Lparse_int_range"); // 9223372036854775808 has no positive int
        r#gen.emit(".Lparse_int_done:");
        r#gen.emit("\tmov\tx0, x13");
        r#gen.emit("\tmov\tx2, #0");
        r#gen.emit("\tret");
        r#gen.emit(".Lparse_int_range:");
        r#gen.emit("\tmov\tx1, x0");
        address(r#gen, "x2", ".Lstone_int_range");
        r#gen.emit("\tret");
    }

    if floats {
        // x19 holds the text, x20 the cursor, and x21 a count of digits
        let saved = ["x19", "x20", "x21"];
        r#gen.emit(".Lparse_float_core:");
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
            // setting bit 5 lowercases a letter, and turns nothing else into one
            let next = format!("{name}_next");
            address(r#gen, "x9", name);
            r#gen.emit("\tmov\tx10, #0");
            r#gen.emit(&format!("{name}_loop:"));
            r#gen.emit("\tldrb\tw11, [x20, x10]");
            r#gen.emit("\torr\tw11, w11, #32");
            r#gen.emit("\tldrb\tw17, [x9, x10]");
            r#gen.emit("\tcmp\tw11, w17");
            r#gen.emit(&format!("\tb.ne\t{next}"));
            r#gen.emit("\tadd\tx10, x10, #1");
            r#gen.emit(&format!("\tcmp\tx10, #{length}"));
            r#gen.emit(&format!("\tb.ne\t{name}_loop"));
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
        address(r#gen, "x2", ".Lstone_float_invalid");
        r#gen.emit("\tmov\tx1, x19");
        r#gen.emit("\tb\t.Lparse_float_return");
        r#gen.emit(".Lparse_float_valid:");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.decimal_to_float");
        r#gen.emit("\tmov\tx2, #0");
        r#gen.emit(".Lparse_float_return:");
        pop_frame(r#gen, &saved);
    }
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
    // text and its size across stone.str_new
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
    r#gen.emit("\tsub\tx0, x20, #1");
    r#gen.emit("\tbl\tstone.str_new");
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
/// and `stdlib::split`, each emitted only if named in `used`. They need the string runtime, the
/// `split` routines need the list runtime too, and `stone.str_split` jumps to `empty_separator`, a
/// failure label, when given an empty separator.
///
/// - `stone.str_strip` returns the string in `x0` without whitespace at either end
/// - `stone.str_split_ws` returns a list of the pieces of `x0` between runs of whitespace
/// - `stone.str_split` returns a list of the pieces of `x0` between each `x1`
/// - `stone.str_splitlines` returns a list of the lines of `x0`, as `stdlib::splitlines` splits
///   them
/// - `stone.str_lower` and `stone.str_upper` return a new string of `x0` with its ASCII letters
///   made lowercase or uppercase
/// - `stone.str_rposition` returns the byte index where `x1` last appears in `x0`, or -1
/// - `stone.str_count` returns how many times `x1` appears in `x0` without overlapping
/// - `stone.str_replace` returns a new string of `x0` with the first `x3` matches of `x1`
///   replaced by `x2`, every match if `x3` is negative
///
/// The rest port the x86-64 routines of the same names.
pub fn string_methods(
    r#gen: &mut dyn AssemblyGenerator,
    used: &[&str],
    empty_separator: &str,
    cut_character: &str,
) {
    let uses = |label: &str| used.contains(&label);
    if uses("stone.str_strip") {
        // x19 holds the start and x20 the end
        let saved = ["x19", "x20"];
        r#gen.emit("stone.str_strip:");
        push_frame(r#gen, &saved);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tbl\tstone.str_len");
        r#gen.emit("\tadd\tx20, x19, x0");
        r#gen.emit(".Lstr_strip_lead:");
        r#gen.emit("\tldrb\tw10, [x19], #1");
        jump_if_space(r#gen, "w10", ".Lstr_strip_lead");
        r#gen.emit("\tsub\tx19, x19, #1");
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
    }
    if uses("stone.str_split_ws") {
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
    }
    if [
        "stone.str_split",
        "stone.str_position",
        "stone.str_count",
        "stone.str_replace",
    ]
    .iter()
    .any(|label| uses(label))
    {
        // returns where the string in x1 first appears in the one in x0, or 0, like strstr
        r#gen.emit("stone.str_find:");
        r#gen.emit("\tmov\tx9, #0");
        r#gen.emit(".Lstr_find_compare:");
        r#gen.emit("\tldrb\tw10, [x1, x9]");
        r#gen.emit("\tcbz\tw10, .Lstr_find_found");
        r#gen.emit("\tldrb\tw11, [x0, x9]");
        r#gen.emit("\tcmp\tw10, w11");
        r#gen.emit("\tb.ne\t.Lstr_find_next");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit("\tb\t.Lstr_find_compare");
        r#gen.emit(".Lstr_find_next:");
        r#gen.emit("\tldrb\tw11, [x0]");
        r#gen.emit("\tcbz\tw11, .Lstr_find_none");
        r#gen.emit("\tadd\tx0, x0, #1");
        r#gen.emit("\tb\tstone.str_find");
        r#gen.emit(".Lstr_find_found:");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_find_none:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }
    if uses("stone.str_split") {
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
        r#gen.emit("\tbl\tstone.str_find");
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
        r#gen.emit("\tbl\tstone.cstr_len"); // x19 points inside the string
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x20");
        r#gen.emit("\tbl\tstone.list_append");
        r#gen.emit("\tmov\tx0, x20");
        pop_frame(r#gen, &saved);
    }
    if uses("stone.str_at") || uses("stone.str_char") {
        byte_strings(r#gen);
    }
    if uses("stone.str_at") {
        r#gen.emit("stone.str_at:");
        r#gen.emit("\tldrb\tw9, [x0, x1]");
        r#gen.emit("\tcmp\tw9, #128");
        r#gen.emit(&format!("\tb.hs\t{cut_character}")); // past ascii, so part of a character
        byte_string(r#gen);
    }
    if uses("stone.str_char") {
        r#gen.emit("stone.str_char:");
        r#gen.emit("\tldrb\tw9, [x0, x1]");
        r#gen.emit("\tcmp\tw9, #128");
        r#gen.emit("\tb.hs\t.Lstr_char_long");
        byte_string(r#gen);
        // a lead byte 110xxxxx starts 2 bytes, 1110xxxx 3, and 11110xxx 4, and a byte of any other
        // form is taken alone; x10 is how many bytes are left, which bounds the step
        r#gen.emit(".Lstr_char_long:");
        r#gen.emit("\tldur\tx10, [x0, #-16]");
        r#gen.emit("\tsub\tx10, x10, x1");
        r#gen.emit("\tadd\tx0, x0, x1");
        r#gen.emit("\tmov\tx1, #1");
        for (below, width) in [(192, 2), (224, 3), (240, 4)] {
            r#gen.emit(&format!("\tcmp\tw9, #{below}"));
            r#gen.emit("\tb.lo\t.Lstr_char_copy");
            r#gen.emit(&format!("\tmov\tx1, #{width}"));
        }
        r#gen.emit(".Lstr_char_copy:");
        r#gen.emit("\tcmp\tx1, x10");
        r#gen.emit("\tcsel\tx1, x10, x1, hi");
        r#gen.emit("\tb\tstone.str_slice");
    }
    if uses("stone.str_slice_of") {
        str_slice_of(r#gen, cut_character);
    }
    if uses("stone.str_cmp") {
        r#gen.emit("stone.str_cmp:");
        r#gen.emit("\tldrb\tw9, [x0], #1");
        r#gen.emit("\tldrb\tw10, [x1], #1");
        r#gen.emit("\tcmp\tw9, w10");
        r#gen.emit("\tb.ne\t.Lstr_cmp_differ");
        r#gen.emit("\tcbnz\tw9, stone.str_cmp");
        r#gen.emit("\tmov\tx0, #0"); // both ended together
        r#gen.emit("\tret");
        // a string that ends first has the smaller byte, its terminator
        r#gen.emit(".Lstr_cmp_differ:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit("\tcneg\tx0, x0, lo");
        r#gen.emit("\tret");
    }
    if uses("stone.str_join") {
        str_join(r#gen);
    }
    if uses("stone.str_position") {
        // stone.str_find leaves x12 alone, so it keeps the start
        r#gen.emit("stone.str_position:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tmov\tx12, x0");
        r#gen.emit("\tbl\tstone.str_find");
        r#gen.emit("\tsub\tx12, x0, x12");
        r#gen.emit("\tmov\tx9, #-1");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit("\tcsel\tx0, x12, x9, ne");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.str_ends") {
        // the suffix must start where the string has exactly its length left
        r#gen.emit("stone.str_ends:");
        r#gen.emit("\tldur\tx9, [x0, #-16]");
        r#gen.emit("\tldur\tx10, [x1, #-16]");
        r#gen.emit("\tcmp\tx9, x10");
        r#gen.emit("\tb.lo\t.Lstr_starts_no"); // the suffix is longer
        r#gen.emit("\tsub\tx9, x9, x10");
        r#gen.emit("\tadd\tx0, x0, x9");
        r#gen.emit("\tb\tstone.str_starts");
    }
    if uses("stone.str_starts") || uses("stone.str_ends") {
        r#gen.emit("stone.str_starts:");
        r#gen.emit("\tldrb\tw10, [x1], #1");
        r#gen.emit("\tcbz\tw10, .Lstr_starts_yes");
        r#gen.emit("\tldrb\tw9, [x0], #1");
        r#gen.emit("\tcmp\tw9, w10");
        r#gen.emit("\tb.ne\t.Lstr_starts_no");
        r#gen.emit("\tb\tstone.str_starts");
        r#gen.emit(".Lstr_starts_yes:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit("\tret");
        r#gen.emit(".Lstr_starts_no:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }
    if uses("stone.str_splitlines") {
        str_splitlines(r#gen);
    }
    for (label, first) in [("stone.str_lower", 'A'), ("stone.str_upper", 'a')] {
        if uses(label) {
            str_case(r#gen, label, first);
        }
    }
    if uses("stone.str_rposition") {
        str_rposition(r#gen);
    }
    if uses("stone.str_count") {
        str_count(r#gen);
    }
    if uses("stone.str_replace") {
        str_replace(r#gen);
    }
}

/// Emits `stone.str_splitlines` (see [`string_methods`]), which ends a line at each `\n`, `\r\n`,
/// or `\r` and keeps the last line only if it is not empty, as `stdlib::splitlines` does.
fn str_splitlines(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the cursor, x20 the list, and x21 where the line started
    let saved = ["x19", "x20", "x21"];
    r#gen.emit("stone.str_splitlines:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tmov\tx1, #1"); // a list of strings
    r#gen.emit("\tbl\tstone.list_new");
    r#gen.emit("\tmov\tx20, x0");
    r#gen.emit(".Lstr_splitlines_scan:");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\tcbz\tw10, .Lstr_splitlines_end");
    r#gen.emit("\tcmp\tw10, #10");
    r#gen.emit("\tb.eq\t.Lstr_splitlines_line");
    r#gen.emit("\tcmp\tw10, #13");
    r#gen.emit("\tb.eq\t.Lstr_splitlines_line");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit("\tb\t.Lstr_splitlines_scan");
    r#gen.emit(".Lstr_splitlines_line:");
    str_splitlines_append(r#gen);
    // \r\n is one ending
    r#gen.emit("\tldrb\tw10, [x19], #1");
    r#gen.emit("\tcmp\tw10, #13");
    r#gen.emit("\tb.ne\t.Lstr_splitlines_next");
    r#gen.emit("\tldrb\tw10, [x19]");
    r#gen.emit("\tcmp\tw10, #10");
    r#gen.emit("\tb.ne\t.Lstr_splitlines_next");
    r#gen.emit("\tadd\tx19, x19, #1");
    r#gen.emit(".Lstr_splitlines_next:");
    r#gen.emit("\tmov\tx21, x19");
    r#gen.emit("\tb\t.Lstr_splitlines_scan");
    // a last line without an ending is a line, unless it is empty
    r#gen.emit(".Lstr_splitlines_end:");
    r#gen.emit("\tcmp\tx19, x21");
    r#gen.emit("\tb.eq\t.Lstr_splitlines_done");
    str_splitlines_append(r#gen);
    r#gen.emit(".Lstr_splitlines_done:");
    r#gen.emit("\tmov\tx0, x20");
    pop_frame(r#gen, &saved);
}

/// Emits the part of `stone.str_splitlines` that appends the line from `x21` up to `x19` to the
/// list in `x20`.
fn str_splitlines_append(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\tmov\tx0, x21");
    r#gen.emit("\tsub\tx1, x19, x21");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tmov\tx1, x0");
    r#gen.emit("\tmov\tx0, x20");
    r#gen.emit("\tbl\tstone.list_append");
}

/// Emits `label`, `stone.str_lower` or `stone.str_upper` (see [`string_methods`]), which copies
/// the string, flipping the case bit of each byte from `first`, `'A'` or `'a'`, to 25 past it.
fn str_case(r#gen: &mut dyn AssemblyGenerator, label: &str, first: char) {
    let name = label.trim_start_matches("stone.");
    // x19 holds the string across stone.str_new
    let saved = ["x19"];
    r#gen.emit(&format!("{label}:"));
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tldur\tx0, [x0, #-16]");
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx9, #0");
    r#gen.emit(&format!(".L{name}_loop:"));
    r#gen.emit("\tldrb\tw10, [x19, x9]");
    r#gen.emit(&format!("\tcbz\tw10, .L{name}_done"));
    r#gen.emit(&format!("\tsub\tw11, w10, #{}", first as u32));
    r#gen.emit("\tcmp\tw11, #25");
    r#gen.emit(&format!("\tb.hi\t.L{name}_keep"));
    r#gen.emit("\teor\tw10, w10, #32");
    r#gen.emit(&format!(".L{name}_keep:"));
    r#gen.emit("\tstrb\tw10, [x0, x9]");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit(&format!("\tb\t.L{name}_loop"));
    r#gen.emit(&format!(".L{name}_done:"));
    pop_frame(r#gen, &saved);
}

/// Emits `stone.str_rposition` (see [`string_methods`]), which tries each start from the last one
/// that leaves room for the text down to the first, so an empty text is found at the end.
fn str_rposition(r#gen: &mut dyn AssemblyGenerator) {
    // x12 points at the start being tried and x13 counts the bytes that matched
    r#gen.emit("stone.str_rposition:");
    r#gen.emit("\tldur\tx9, [x0, #-16]");
    r#gen.emit("\tldur\tx10, [x1, #-16]");
    r#gen.emit("\tcmp\tx9, x10");
    r#gen.emit("\tb.lo\t.Lstr_rposition_none"); // the text is longer
    r#gen.emit("\tsub\tx9, x9, x10");
    r#gen.emit("\tadd\tx12, x0, x9");
    r#gen.emit(".Lstr_rposition_try:");
    r#gen.emit("\tmov\tx13, #0");
    r#gen.emit(".Lstr_rposition_compare:");
    r#gen.emit("\tldrb\tw10, [x1, x13]");
    r#gen.emit("\tcbz\tw10, .Lstr_rposition_found");
    r#gen.emit("\tldrb\tw11, [x12, x13]");
    r#gen.emit("\tcmp\tw10, w11");
    r#gen.emit("\tb.ne\t.Lstr_rposition_next");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tb\t.Lstr_rposition_compare");
    r#gen.emit(".Lstr_rposition_next:");
    r#gen.emit("\tcmp\tx12, x0");
    r#gen.emit("\tb.eq\t.Lstr_rposition_none");
    r#gen.emit("\tsub\tx12, x12, #1");
    r#gen.emit("\tb\t.Lstr_rposition_try");
    r#gen.emit(".Lstr_rposition_found:");
    r#gen.emit("\tsub\tx0, x12, x0");
    r#gen.emit("\tret");
    r#gen.emit(".Lstr_rposition_none:");
    r#gen.emit("\tmov\tx0, #-1");
    r#gen.emit("\tret");
}

/// Emits `stone.str_count` (see [`string_methods`]), which goes on searching past each match, or,
/// for an empty text, counts one match before each character and one at the end.
fn str_count(r#gen: &mut dyn AssemblyGenerator) {
    // stone.str_find leaves x12 and x13 alone, so they keep the count and the text's length
    r#gen.emit("stone.str_count:");
    r#gen.emit("\tldrb\tw9, [x1]");
    r#gen.emit("\tcbz\tw9, .Lstr_count_empty");
    push_frame(r#gen, &[]);
    r#gen.emit("\tmov\tx12, #0");
    r#gen.emit("\tldur\tx13, [x1, #-16]");
    r#gen.emit(".Lstr_count_find:");
    r#gen.emit("\tbl\tstone.str_find");
    r#gen.emit("\tcbz\tx0, .Lstr_count_done");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tadd\tx0, x0, x13");
    r#gen.emit("\tb\t.Lstr_count_find");
    r#gen.emit(".Lstr_count_done:");
    r#gen.emit("\tmov\tx0, x12");
    pop_frame(r#gen, &[]);
    // every byte but a continuation byte, 10xxxxxx, starts a character
    r#gen.emit(".Lstr_count_empty:");
    r#gen.emit("\tmov\tx9, #1");
    r#gen.emit(".Lstr_count_byte:");
    r#gen.emit("\tldrb\tw10, [x0], #1");
    r#gen.emit("\tcbz\tw10, .Lstr_count_end");
    r#gen.emit("\tand\tw10, w10, #192");
    r#gen.emit("\tcmp\tw10, #128");
    r#gen.emit("\tb.eq\t.Lstr_count_byte");
    r#gen.emit("\tadd\tx9, x9, #1");
    r#gen.emit("\tb\t.Lstr_count_byte");
    r#gen.emit(".Lstr_count_end:");
    r#gen.emit("\tmov\tx0, x9");
    r#gen.emit("\tret");
}

/// Emits `stone.str_replace` (see [`string_methods`]). A first pass counts the matches it will
/// replace, at most `x3` taken as unsigned so that a negative count never stops it, which gives
/// the result's length, and a second pass finds them again, copying the bytes between them and
/// the new text in their place. An empty old text matches before each character and at the end,
/// as `stone.str_count` counts it.
fn str_replace(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the string, x20 the old text, x21 the new one, x22 the most matches to replace
    // and then how many are left to replace, and x23 the old text's length
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.str_replace:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tmov\tx21, x2");
    r#gen.emit("\tmov\tx22, x3");
    r#gen.emit("\tldur\tx23, [x1, #-16]");
    // x12 is where the next search starts and x13 counts the matches
    r#gen.emit("\tmov\tx12, x19");
    r#gen.emit("\tmov\tx13, #0");
    r#gen.emit(".Lstr_replace_count:");
    r#gen.emit("\tcmp\tx13, x22");
    r#gen.emit("\tb.hs\t.Lstr_replace_counted");
    r#gen.emit("\tmov\tx0, x12");
    r#gen.emit("\tbl\t.Lstr_replace_next");
    r#gen.emit("\tcbz\tx0, .Lstr_replace_counted");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tadd\tx12, x0, x23");
    r#gen.emit("\tcbnz\tx23, .Lstr_replace_count");
    // an empty match is followed by a character, whose end the next search starts from, unless
    // it is the match at the end
    r#gen.emit("\tldrb\tw9, [x0]");
    r#gen.emit("\tcbz\tw9, .Lstr_replace_counted");
    r#gen.emit("\tadd\tx12, x12, #1");
    r#gen.emit("\tb\t.Lstr_replace_count");
    r#gen.emit(".Lstr_replace_counted:");
    r#gen.emit("\tmov\tx22, x13");
    r#gen.emit("\tldur\tx9, [x21, #-16]");
    r#gen.emit("\tsub\tx9, x9, x23");
    r#gen.emit("\tldur\tx10, [x19, #-16]");
    r#gen.emit("\tmadd\tx0, x9, x13, x10");
    r#gen.emit("\tbl\tstone.str_new");
    // nothing but .Lstr_replace_next is called from here, so x12 to x15 keep what follows: x12
    // where the bytes not yet copied start, x13 where the next search starts, x14 where the next
    // byte goes, and x15 the result
    r#gen.emit("\tmov\tx15, x0");
    r#gen.emit("\tmov\tx14, x0");
    r#gen.emit("\tmov\tx12, x19");
    r#gen.emit("\tmov\tx13, x19");
    r#gen.emit(".Lstr_replace_copy:");
    r#gen.emit("\tcbz\tx22, .Lstr_replace_rest");
    r#gen.emit("\tsub\tx22, x22, #1");
    r#gen.emit("\tmov\tx0, x13");
    r#gen.emit("\tbl\t.Lstr_replace_next"); // a match the first pass counted
    r#gen.emit("\tsub\tx10, x0, x12");
    copy_bytes(r#gen, "x14", "x12", "x10", ".Lstr_replace_piece");
    r#gen.emit("\tmov\tx11, x21");
    r#gen.emit("\tldur\tx10, [x21, #-16]");
    copy_bytes(r#gen, "x14", "x11", "x10", ".Lstr_replace_new");
    r#gen.emit("\tadd\tx12, x0, x23");
    r#gen.emit("\tmov\tx13, x12");
    r#gen.emit("\tcbnz\tx23, .Lstr_replace_copy");
    r#gen.emit("\tadd\tx13, x13, #1");
    r#gen.emit("\tb\t.Lstr_replace_copy");
    // then whatever follows the last match; stone.str_new wrote the terminator
    r#gen.emit(".Lstr_replace_rest:");
    r#gen.emit("\tldur\tx10, [x19, #-16]");
    r#gen.emit("\tadd\tx10, x10, x19");
    r#gen.emit("\tsub\tx10, x10, x12");
    copy_bytes(r#gen, "x14", "x12", "x10", ".Lstr_replace_tail");
    r#gen.emit("\tmov\tx0, x15");
    pop_frame(r#gen, &saved);
    // returns the first match of x20 at or after x0, or 0, changing only x0, x1, and x9 to x11;
    // an empty text matches at the first byte that starts a character, or the end
    r#gen.emit(".Lstr_replace_next:");
    r#gen.emit("\tcbz\tx23, .Lstr_replace_boundary");
    r#gen.emit("\tmov\tx1, x20");
    r#gen.emit("\tb\tstone.str_find");
    r#gen.emit(".Lstr_replace_boundary:");
    r#gen.emit("\tldrb\tw9, [x0]");
    r#gen.emit("\tand\tw9, w9, #192");
    r#gen.emit("\tcmp\tw9, #128");
    r#gen.emit("\tb.ne\t.Lstr_replace_found");
    r#gen.emit("\tadd\tx0, x0, #1");
    r#gen.emit("\tb\t.Lstr_replace_boundary");
    r#gen.emit(".Lstr_replace_found:");
    r#gen.emit("\tret");
}

/// Emits the table of the 128 one-byte strings that `stone.str_at` and `stone.str_char` (see
/// [`string_methods`]) return for an ASCII byte, laid out as the x86-64 table is: entry `b` is 32
/// bytes at `32 * b`, a length of 1, an immortal reference count, the byte and its terminator,
/// and padding.
fn byte_strings(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit("\t.data");
    r#gen.emit("\t.balign\t8");
    r#gen.emit(".Lstone_bytes:");
    for byte in 0..128 {
        r#gen.emit("\t.quad\t1");
        r#gen.emit(&format!("\t.quad\t{IMMORTAL}"));
        r#gen.emit(&format!("\t.quad\t{byte}"));
        r#gen.emit("\t.quad\t0");
    }
    r#gen.emit("\t.text");
}

/// Emits the end of a routine that returns the string of the ASCII byte in `x9` from the table
/// [`byte_strings`] emits.
fn byte_string(r#gen: &mut dyn AssemblyGenerator) {
    address(r#gen, "x0", ".Lstone_bytes");
    r#gen.emit("\tadd\tx0, x0, x9, lsl #5");
    r#gen.emit("\tadd\tx0, x0, #16");
    r#gen.emit("\tret");
}

/// Emits `stone.str_slice_of` (see [`string_methods`]), which follows `stdlib::str_slice` as
/// the x86-64 routine does.
fn str_slice_of(r#gen: &mut dyn AssemblyGenerator, cut_character: &str) {
    // x19 holds the string, x20 the first index, x21 the step, x22 the count, and x23 the new
    // string; x9 holds the length while the bounds are clamped
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.str_slice_of:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    // stone.str_len only changes x0
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx9, x0");
    slice_bounds(r#gen, "str_slice_of");
    r#gen.emit("\tcbz\tx22, .Lstr_slice_of_copy"); // an empty slice cuts nothing
    r#gen.emit("\tcmp\tx21, #1");
    r#gen.emit("\tb.ne\t.Lstr_slice_of_check");
    // a byte of the form 10xxxxxx continues a character, so neither end may be one
    r#gen.emit("\tldrb\tw9, [x19, x20]");
    r#gen.emit("\tlsr\tw9, w9, #6");
    r#gen.emit("\tcmp\tw9, #2");
    r#gen.emit(&format!("\tb.eq\t{cut_character}"));
    r#gen.emit("\tadd\tx10, x20, x22");
    r#gen.emit("\tldrb\tw9, [x19, x10]");
    r#gen.emit("\tlsr\tw9, w9, #6");
    r#gen.emit("\tcmp\tw9, #2");
    r#gen.emit(&format!("\tb.eq\t{cut_character}"));
    r#gen.emit("\tadd\tx0, x19, x20");
    r#gen.emit("\tmov\tx1, x22");
    r#gen.emit("\tbl\tstone.str_slice");
    r#gen.emit("\tb\t.Lstr_slice_of_done");
    // any other step needs every byte it picks to be ascii
    r#gen.emit(".Lstr_slice_of_check:");
    r#gen.emit("\tmov\tx12, x20");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Lstr_slice_of_check_loop:");
    r#gen.emit("\tcmp\tx8, x22");
    r#gen.emit("\tb.ge\t.Lstr_slice_of_copy");
    r#gen.emit("\tldrb\tw9, [x19, x12]");
    r#gen.emit("\tcmp\tw9, #128");
    r#gen.emit(&format!("\tb.hs\t{cut_character}"));
    r#gen.emit("\tadd\tx12, x12, x21");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tb\t.Lstr_slice_of_check_loop");
    r#gen.emit(".Lstr_slice_of_copy:");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx23, x0");
    r#gen.emit("\tmov\tx12, x20");
    r#gen.emit("\tmov\tx8, #0");
    r#gen.emit(".Lstr_slice_of_loop:");
    r#gen.emit("\tcmp\tx8, x22");
    r#gen.emit("\tb.ge\t.Lstr_slice_of_end");
    r#gen.emit("\tldrb\tw9, [x19, x12]");
    r#gen.emit("\tstrb\tw9, [x23, x8]");
    r#gen.emit("\tadd\tx12, x12, x21");
    r#gen.emit("\tadd\tx8, x8, #1");
    r#gen.emit("\tb\t.Lstr_slice_of_loop");
    r#gen.emit(".Lstr_slice_of_end:");
    r#gen.emit("\tstrb\twzr, [x23, x8]");
    r#gen.emit("\tmov\tx0, x23");
    r#gen.emit(".Lstr_slice_of_done:");
    pop_frame(r#gen, &saved);
}

/// Emits `stone.str_join` (see [`string_methods`]), which measures every piece, allocates the
/// result once, and then copies the pieces and separators into it.
fn str_join(r#gen: &mut dyn AssemblyGenerator) {
    // x19 holds the separator, x20 the list, x21 the separator's length, x22 the total length and
    // then the new string, and x23 the index; x10 is where the next byte goes
    let saved = ["x19", "x20", "x21", "x22", "x23"];
    r#gen.emit("stone.str_join:");
    push_frame(r#gen, &saved);
    r#gen.emit("\tmov\tx19, x0");
    r#gen.emit("\tmov\tx20, x1");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tmov\tx21, x0");
    r#gen.emit("\tmov\tx22, #0");
    r#gen.emit("\tmov\tx23, #0");
    // each piece and a separator after it
    r#gen.emit(".Lstr_join_measure:");
    r#gen.emit("\tldr\tx9, [x20]");
    r#gen.emit("\tcmp\tx23, x9");
    r#gen.emit("\tb.ge\t.Lstr_join_measured");
    r#gen.emit("\tldr\tx9, [x20, #16]");
    r#gen.emit("\tldr\tx0, [x9, x23, lsl #3]");
    r#gen.emit("\tbl\tstone.str_len");
    r#gen.emit("\tadd\tx22, x22, x21");
    r#gen.emit("\tadd\tx22, x22, x0");
    r#gen.emit("\tadd\tx23, x23, #1");
    r#gen.emit("\tb\t.Lstr_join_measure");
    // less the separator after the last piece, if there is one
    r#gen.emit(".Lstr_join_measured:");
    r#gen.emit("\tcbz\tx23, .Lstr_join_alloc");
    r#gen.emit("\tsub\tx22, x22, x21");
    r#gen.emit(".Lstr_join_alloc:");
    r#gen.emit("\tmov\tx0, x22");
    r#gen.emit("\tbl\tstone.str_new");
    r#gen.emit("\tmov\tx22, x0");
    r#gen.emit("\tmov\tx10, x0");
    r#gen.emit("\tmov\tx23, #0");
    r#gen.emit(".Lstr_join_piece:");
    r#gen.emit("\tldr\tx9, [x20]");
    r#gen.emit("\tcmp\tx23, x9");
    r#gen.emit("\tb.ge\t.Lstr_join_done");
    r#gen.emit("\tcbz\tx23, .Lstr_join_item");
    r#gen.emit("\tmov\tx11, x19");
    r#gen.emit(".Lstr_join_separator:");
    r#gen.emit("\tldrb\tw9, [x11], #1");
    r#gen.emit("\tcbz\tw9, .Lstr_join_item");
    r#gen.emit("\tstrb\tw9, [x10], #1");
    r#gen.emit("\tb\t.Lstr_join_separator");
    r#gen.emit(".Lstr_join_item:");
    r#gen.emit("\tldr\tx9, [x20, #16]");
    r#gen.emit("\tldr\tx11, [x9, x23, lsl #3]");
    r#gen.emit(".Lstr_join_byte:");
    r#gen.emit("\tldrb\tw9, [x11], #1");
    r#gen.emit("\tcbz\tw9, .Lstr_join_next");
    r#gen.emit("\tstrb\tw9, [x10], #1");
    r#gen.emit("\tb\t.Lstr_join_byte");
    r#gen.emit(".Lstr_join_next:");
    r#gen.emit("\tadd\tx23, x23, #1");
    r#gen.emit("\tb\t.Lstr_join_piece");
    r#gen.emit(".Lstr_join_done:");
    r#gen.emit("\tstrb\twzr, [x10]");
    r#gen.emit("\tmov\tx0, x22");
    pop_frame(r#gen, &saved);
}

/// `sysctl`'s names for the kernel's and the hardware's values, `CTL_KERN` and `CTL_HW`, and for
/// the host name and the number of processors that are online in them.
const CTL_KERN: i64 = 1;
const KERN_HOSTNAME: i64 = 10;
const CTL_HW: i64 = 6;
const HW_AVAILCPU: i64 = 25;

/// Emits a macOS `sysctl` call that copies at most `size` bytes of the value named
/// `{top, second}` to the bottom of a new stack area, after which `x0` is 0 if it succeeded. The
/// area holds the value, then the two-int name and the value's length, which `sysctl` reads and
/// updates, so `size` plus 16 bytes, rounded up to keep `sp` aligned.
///
/// For example, `sysctl(gen, CTL_HW, HW_AVAILCPU, 4)` leaves the number of processors as an int
/// at `[sp]`.
fn sysctl(r#gen: &mut dyn AssemblyGenerator, top: i64, second: i64, size: usize) {
    let name = size.next_multiple_of(8);
    r#gen.emit(&format!(
        "\tsub\tsp, sp, #{}",
        (name + 16).next_multiple_of(16)
    ));
    r#gen.emit(&format!("\tmov\tx9, #{top}"));
    r#gen.emit(&format!("\tmovk\tx9, #{second}, lsl #32"));
    r#gen.emit(&format!("\tstr\tx9, [sp, #{name}]"));
    r#gen.emit(&format!("\tmov\tx9, #{size}"));
    r#gen.emit(&format!("\tstr\tx9, [sp, #{}]", name + 8));
    r#gen.emit(&format!("\tadd\tx0, sp, #{name}"));
    r#gen.emit("\tmov\tx1, #2");
    r#gen.emit("\tmov\tx2, sp");
    r#gen.emit(&format!("\tadd\tx3, sp, #{}", name + 8));
    r#gen.emit("\tmov\tx4, #0"); // nothing to set
    r#gen.emit("\tmov\tx5, #0");
    system_call(r#gen, Sys::Sysctl);
}

/// Emits the routines behind the `os` module that the program calls, each named in `used`, under
/// the same labels and contracts as `x64::builtins::os_runtime`, with arguments in `x0` and
/// results in `x0`. `cwd_failure` is the failure label for `stone.os_cwd`, needed only if it is
/// used. The routines that return strings need the string runtime.
pub fn os_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str], cwd_failure: &str) {
    let uses = |label: &str| used.contains(&label);
    let macos = r#gen.target().os == Os::MacOs;
    r#gen.emit("\t.data");
    immortal_string(r#gen, ".Lstone_os_empty", "");
    let platform = if macos { "macos" } else { "linux" };
    immortal_string(r#gen, ".Lstone_os_platform", platform);
    immortal_string(r#gen, ".Lstone_os_arch", "aarch64");
    r#gen.emit("\t.text");

    if uses("stone.os_env") || uses("stone.os_has_env") {
        // x19 holds the name across strchr
        // a name that is empty or holds = is never set, though it could match an entry
        r#gen.emit("stone.os_getenv:");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit("\tldrb\tw10, [x9]");
        r#gen.emit("\tcbz\tw10, .Los_getenv_unset");
        r#gen.emit(".Los_getenv_scan:");
        r#gen.emit("\tldrb\tw10, [x9], #1");
        r#gen.emit("\tcbz\tw10, stone.getenv");
        r#gen.emit("\tcmp\tw10, #61"); // '='
        r#gen.emit("\tb.ne\t.Los_getenv_scan");
        r#gen.emit(".Los_getenv_unset:");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tret");
    }
    if uses("stone.os_env") {
        r#gen.emit("stone.os_env:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.os_getenv");
        r#gen.emit("\tcbz\tx0, .Los_env_unset");
        r#gen.emit("\tbl\tstone.os_copy");
        // x0 is 0 for a variable that is not set
        r#gen.emit(".Los_env_unset:");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_has_env") {
        r#gen.emit("stone.os_has_env:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.os_getenv");
        r#gen.emit("\tcmp\tx0, #0");
        r#gen.emit("\tcset\tx0, ne");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_env") || uses("stone.os_hostname") || uses("stone.os_cwd") {
        // returns a counted copy of the C string in x0, which x19 holds across cstr_len
        r#gen.emit("stone.os_copy:");
        push_frame(r#gen, &["x19"]);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tbl\tstone.cstr_len");
        r#gen.emit("\tmov\tx1, x0");
        r#gen.emit("\tmov\tx0, x19");
        r#gen.emit("\tbl\tstone.str_slice");
        pop_frame(r#gen, &["x19"]);
    }
    if uses("stone.os_platform") {
        r#gen.emit("stone.os_platform:");
        address(r#gen, "x0", ".Lstone_os_platform");
        r#gen.emit("\tret");
    }
    if uses("stone.os_arch") {
        r#gen.emit("stone.os_arch:");
        address(r#gen, "x0", ".Lstone_os_arch");
        r#gen.emit("\tret");
    }
    if uses("stone.os_hostname") && macos {
        // sysctl copies kern.hostname into 256 bytes of stack, as gethostname does on macOS
        r#gen.emit("stone.os_hostname:");
        push_frame(r#gen, &[]);
        sysctl(r#gen, CTL_KERN, KERN_HOSTNAME, HOSTNAME_BUFFER);
        r#gen.emit("\tcbnz\tx0, .Los_hostname_empty");
        r#gen.emit(&format!("\tstrb\twzr, [sp, #{}]", HOSTNAME_BUFFER - 1));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Los_hostname_empty:");
        address(r#gen, "x0", ".Lstone_os_empty");
        pop_frame(r#gen, &[]);
    } else if uses("stone.os_hostname") {
        // uname fills six fields of 65 bytes, and the host name is the second
        r#gen.emit("stone.os_hostname:");
        push_frame(r#gen, &[]);
        r#gen.emit(&format!("\tsub\tsp, sp, #{UTSNAME_SIZE}"));
        r#gen.emit("\tmov\tx0, sp");
        system_call(r#gen, Sys::Uname);
        r#gen.emit("\tcbnz\tx0, .Los_hostname_empty");
        r#gen.emit(&format!("\tadd\tx0, sp, #{UTSNAME_FIELD}"));
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
        r#gen.emit(".Los_hostname_empty:");
        address(r#gen, "x0", ".Lstone_os_empty");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_cpu_count") && macos {
        // sysctl copies hw.activecpu, an int, as sysconf(_SC_NPROCESSORS_ONLN) reads it
        r#gen.emit("stone.os_cpu_count:");
        push_frame(r#gen, &[]);
        sysctl(r#gen, CTL_HW, HW_AVAILCPU, 4);
        r#gen.emit("\tcbnz\tx0, .Lcpu_count_one");
        r#gen.emit("\tldrsw\tx0, [sp]");
        r#gen.emit("\tcmp\tx0, #1");
        r#gen.emit("\tb.ge\t.Lcpu_count_return");
        r#gen.emit(".Lcpu_count_one:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit(".Lcpu_count_return:");
        pop_frame(r#gen, &[]);
    } else if uses("stone.os_cpu_count") {
        r#gen.emit("\t.section\t.rodata");
        r#gen.emit(".Lstone_cpu_online:");
        r#gen.emit(&format!("\t.string \"{CPU_ONLINE}\""));
        r#gen.emit("\t.text");
        // the file lists ranges like 0-3,6, read into 256 bytes of stack, which x9 walks
        let digits = |r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str| {
            r#gen.emit(&format!("\tmov\t{reg}, #0"));
            r#gen.emit("\tmov\tx17, #10");
            r#gen.emit(&format!("{label}:"));
            r#gen.emit("\tldrb\tw16, [x9]");
            r#gen.emit("\tsub\tw16, w16, #48"); // '0'
            r#gen.emit("\tcmp\tw16, #9");
            r#gen.emit(&format!("\tb.hi\t{label}_done"));
            r#gen.emit("\tadd\tx9, x9, #1");
            r#gen.emit(&format!("\tmadd\t{reg}, {reg}, x17, x16"));
            r#gen.emit(&format!("\tb\t{label}"));
            r#gen.emit(&format!("{label}_done:"));
        };
        r#gen.emit("stone.os_cpu_count:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tsub\tsp, sp, #256");
        r#gen.emit("\tmov\tx0, #-100"); // AT_FDCWD
        address(r#gen, "x1", ".Lstone_cpu_online");
        r#gen.emit("\tmov\tx2, #0"); // O_RDONLY
        r#gen.emit("\tmov\tx3, #0");
        system_call(r#gen, Sys::Openat);
        r#gen.emit("\ttbnz\tx0, #63, .Lcpu_count_one");
        r#gen.emit("\tmov\tx9, x0");
        r#gen.emit("\tmov\tx1, sp");
        r#gen.emit("\tmov\tx2, #255");
        system_call(r#gen, Sys::Read);
        r#gen.emit("\tmov\tx10, x0");
        r#gen.emit("\tmov\tx0, x9");
        system_call(r#gen, Sys::Close);
        r#gen.emit("\tcmp\tx10, #1");
        r#gen.emit("\tb.lt\t.Lcpu_count_one");
        r#gen.emit("\tstrb\twzr, [sp, x10]");
        r#gen.emit("\tmov\tx0, #0");
        r#gen.emit("\tmov\tx9, sp");
        r#gen.emit(".Lcpu_count_range:");
        digits(r#gen, "x10", ".Lcpu_count_first");
        r#gen.emit("\tmov\tx11, x10");
        r#gen.emit("\tldrb\tw16, [x9]");
        r#gen.emit("\tcmp\tw16, #45"); // '-'
        r#gen.emit("\tb.ne\t.Lcpu_count_add");
        r#gen.emit("\tadd\tx9, x9, #1");
        digits(r#gen, "x11", ".Lcpu_count_last");
        r#gen.emit(".Lcpu_count_add:");
        r#gen.emit("\tsub\tx11, x11, x10");
        r#gen.emit("\tadd\tx0, x0, x11");
        r#gen.emit("\tadd\tx0, x0, #1");
        r#gen.emit("\tldrb\tw16, [x9]");
        r#gen.emit("\tcmp\tw16, #44"); // ','
        r#gen.emit("\tb.ne\t.Lcpu_count_done");
        r#gen.emit("\tadd\tx9, x9, #1");
        r#gen.emit("\tb\t.Lcpu_count_range");
        r#gen.emit(".Lcpu_count_done:");
        r#gen.emit("\tcmp\tx0, #1");
        r#gen.emit("\tb.ge\t.Lcpu_count_return");
        r#gen.emit(".Lcpu_count_one:");
        r#gen.emit("\tmov\tx0, #1");
        r#gen.emit(".Lcpu_count_return:");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_pid") {
        r#gen.emit("stone.os_pid:");
        system_call(r#gen, Sys::Getpid);
        r#gen.emit("\tret");
    }
    if uses("stone.os_cwd") {
        // the kernel writes the path into a buffer on the stack, and marks a directory that is
        // no longer reachable from the root by not starting it with /, which glibc treats as
        // an error too
        r#gen.emit("stone.os_cwd:");
        push_frame(r#gen, &[]);
        r#gen.emit(&format!("\tsub\tsp, sp, #{PATH_BUFFER}"));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit(&format!("\tmov\tx1, #{PATH_BUFFER}"));
        system_call(r#gen, Sys::Getcwd);
        r#gen.emit(&format!("\ttbnz\tx0, #63, {cwd_failure}"));
        r#gen.emit("\tldrb\tw9, [sp]");
        r#gen.emit("\tcmp\tw9, #47"); // '/'
        r#gen.emit(&format!("\tb.ne\t{cwd_failure}"));
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit("\tbl\tstone.os_copy");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.os_exit") {
        r#gen.emit("stone.os_exit:");
        system_call(r#gen, Sys::Exit);
    }
}

/// Emits the routines behind the `time` module that the program calls, each named in `used`,
/// under the same labels and contracts as `x64::builtins::time_runtime`, with arguments in `x0`
/// and results in `x0`. `failures` are the labels `stone.time_sleep` jumps to for a negative
/// length or nan, and for one of 2^63 seconds or more.
pub fn time_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str], failures: (&str, &str)) {
    let uses = |label: &str| used.contains(&label);
    let macos = r#gen.target().os == Os::MacOs;
    // CLOCK_REALTIME is 0 on both systems, and CLOCK_MONOTONIC is 1 on Linux but 6 on macOS
    let monotonic = if macos { 6 } else { 1 };
    for (label, clock) in [("stone.time_now", 0), ("stone.time_clock", monotonic)] {
        if !uses(label) {
            continue;
        }
        // clock_gettime fills the seconds at [sp] and the nanoseconds at [sp, #8]
        r#gen.emit(&format!("{label}:"));
        push_frame(r#gen, &[]);
        r#gen.emit("\tsub\tsp, sp, #16");
        r#gen.emit(&format!("\tmov\tx0, #{clock}"));
        r#gen.emit("\tmov\tx1, sp");
        system_call(r#gen, Sys::ClockGettime);
        r#gen.emit("\tldp\tx9, x10, [sp]");
        r#gen.emit("\tscvtf\td0, x9");
        r#gen.emit("\tscvtf\td1, x10");
        // 1e9
        r#gen.emit("\tmovz\tx11, #0x41cd, lsl #48");
        r#gen.emit("\tmovk\tx11, #0xcd65, lsl #32");
        r#gen.emit("\tfmov\td2, x11");
        r#gen.emit("\tfdiv\td1, d1, d2");
        r#gen.emit("\tfadd\td0, d0, d1");
        r#gen.emit("\tfmov\tx0, d0");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.time_sleep") {
        let (negative, too_large) = failures;
        // the request is at [sp] and what is left after a signal at [sp, #16]
        r#gen.emit("stone.time_sleep:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tsub\tsp, sp, #32");
        r#gen.emit("\tfmov\td0, x0");
        // lt also holds for nan
        r#gen.emit("\tfcmp\td0, #0.0");
        r#gen.emit(&format!("\tb.lt\t{negative}"));
        r#gen.emit("\tmovz\tx9, #0x43e0, lsl #48"); // 2^63
        r#gen.emit("\tfmov\td1, x9");
        r#gen.emit("\tfcmp\td0, d1");
        r#gen.emit(&format!("\tb.ge\t{too_large}"));
        r#gen.emit("\tfcvtzs\tx9, d0");
        r#gen.emit("\tscvtf\td1, x9");
        r#gen.emit("\tfsub\td0, d0, d1");
        // 1e9
        r#gen.emit("\tmovz\tx10, #0x41cd, lsl #48");
        r#gen.emit("\tmovk\tx10, #0xcd65, lsl #32");
        r#gen.emit("\tfmov\td1, x10");
        r#gen.emit("\tfmul\td0, d0, d1");
        r#gen.emit("\tfcvtzs\tx10, d0");
        r#gen.emit("\tstp\tx9, x10, [sp]");
        r#gen.emit(".Ltime_sleep_again:");
        r#gen.emit("\tmov\tx0, sp");
        r#gen.emit("\tadd\tx1, sp, #16");
        system_call(r#gen, Sys::Nanosleep);
        // a signal interrupted it, so sleep for what is left: both systems return -EINTR
        r#gen.emit("\tcmn\tx0, #4");
        r#gen.emit("\tb.ne\t.Ltime_sleep_done");
        r#gen.emit("\tldp\tx9, x10, [sp, #16]");
        r#gen.emit("\tstp\tx9, x10, [sp]");
        r#gen.emit("\tb\t.Ltime_sleep_again");
        r#gen.emit(".Ltime_sleep_done:");
        pop_frame(r#gen, &[]);
    }
}

/// Emits code that puts the 64-bit constant `value` in `reg`: a `movz`, then a `movk` for each
/// other halfword that is not zero.
///
/// For example, `constant(gen, "x9", 0x3ca0_0000_0000_0000)` emits `movz x9, #0` and
/// `movk x9, #15520, lsl #48`.
fn constant(r#gen: &mut dyn AssemblyGenerator, reg: &str, value: u64) {
    r#gen.emit(&format!("\tmovz\t{reg}, #{}", value & 0xffff));
    for shift in [16, 32, 48] {
        let half = (value >> shift) & 0xffff;
        if half != 0 {
            r#gen.emit(&format!("\tmovk\t{reg}, #{half}, lsl #{shift}"));
        }
    }
}

/// Emits the runtime of the `random` module under the same labels and contracts as
/// `x64::builtins::random_runtime`, with arguments and results in `x0`. An unseeded program
/// seeds itself from `getrandom` on Linux and `getentropy` on macOS.
pub fn random_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    use crate::stdlib::random::{SPLITMIX_GAMMA, SPLITMIX_MUL1, SPLITMIX_MUL2};
    let uses = |label: &str| used.contains(&label);
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    r#gen.emit("stone.random_state:");
    r#gen.emit("\t.zero\t40");
    r#gen.emit("\t.text");

    // splitmix64, unrolled for the four words of the state
    r#gen.emit("stone.random_seed:");
    address(r#gen, "x9", "stone.random_state");
    constant(r#gen, "x10", SPLITMIX_GAMMA);
    constant(r#gen, "x11", SPLITMIX_MUL1);
    constant(r#gen, "x12", SPLITMIX_MUL2);
    for word in 0..4 {
        r#gen.emit("\tadd\tx0, x0, x10");
        r#gen.emit("\tmov\tx13, x0");
        for (shift, multiplier) in [(30, Some("x11")), (27, Some("x12")), (31, None)] {
            r#gen.emit(&format!("\tlsr\tx14, x13, #{shift}"));
            r#gen.emit("\teor\tx13, x13, x14");
            if let Some(multiplier) = multiplier {
                r#gen.emit(&format!("\tmul\tx13, x13, {multiplier}"));
            }
        }
        r#gen.emit(&format!("\tstr\tx13, [x9, #{}]", 8 * word));
    }
    r#gen.emit("\tmov\tx13, #1");
    r#gen.emit("\tstr\tx13, [x9, #32]");
    r#gen.emit("\tmov\tx0, #0");
    r#gen.emit("\tret");

    // xoshiro256**, with s0 through s3 in x10 through x13
    r#gen.emit("stone.random_next:");
    push_frame(r#gen, &[]);
    address(r#gen, "x9", "stone.random_state");
    r#gen.emit("\tldr\tx10, [x9, #32]");
    r#gen.emit("\tcbnz\tx10, .Lrandom_next_seeded");
    r#gen.emit("\tsub\tsp, sp, #16");
    r#gen.emit("\tstr\txzr, [sp]");
    r#gen.emit("\tmov\tx0, sp");
    r#gen.emit("\tmov\tx1, #8");
    r#gen.emit("\tmov\tx2, #0");
    system_call(r#gen, Sys::Getrandom);
    r#gen.emit("\tldr\tx0, [sp]");
    r#gen.emit("\tadd\tsp, sp, #16");
    r#gen.emit("\tbl\tstone.random_seed");
    r#gen.emit(".Lrandom_next_seeded:");
    r#gen.emit("\tldp\tx10, x11, [x9]");
    r#gen.emit("\tldp\tx12, x13, [x9, #16]");
    // the result is rotl(s1 * 5, 7) * 9, and rotating left by 7 is rotating right by 57
    r#gen.emit("\tlsl\tx14, x11, #2");
    r#gen.emit("\tadd\tx0, x11, x14");
    r#gen.emit("\tror\tx0, x0, #57");
    r#gen.emit("\tlsl\tx14, x0, #3");
    r#gen.emit("\tadd\tx0, x0, x14");
    r#gen.emit("\tlsl\tx14, x11, #17");
    r#gen.emit("\teor\tx12, x12, x10");
    r#gen.emit("\teor\tx13, x13, x11");
    r#gen.emit("\teor\tx11, x11, x12");
    r#gen.emit("\teor\tx10, x10, x13");
    r#gen.emit("\teor\tx12, x12, x14");
    r#gen.emit("\tror\tx13, x13, #19");
    r#gen.emit("\tstp\tx10, x11, [x9]");
    r#gen.emit("\tstp\tx12, x13, [x9, #16]");
    pop_frame(r#gen, &[]);

    if uses("stone.random_float") {
        // the top 53 bits over 2^53, which is exact
        r#gen.emit("stone.random_float:");
        push_frame(r#gen, &[]);
        r#gen.emit("\tbl\tstone.random_next");
        r#gen.emit("\tlsr\tx0, x0, #11");
        r#gen.emit("\tscvtf\td0, x0");
        constant(r#gen, "x9", (1.0f64 / (1u64 << 53) as f64).to_bits());
        r#gen.emit("\tfmov\td1, x9");
        r#gen.emit("\tfmul\td0, d0, d1");
        r#gen.emit("\tfmov\tx0, d0");
        pop_frame(r#gen, &[]);
    }
    if uses("stone.random_below") {
        // the span stays in x19 and the smallest draw to keep, 2^64 % span, in x20
        r#gen.emit("stone.random_below:");
        push_frame(r#gen, &["x19", "x20"]);
        r#gen.emit("\tmov\tx19, x0");
        r#gen.emit("\tcbz\tx19, .Lrandom_below_any");
        r#gen.emit("\tneg\tx9, x19");
        r#gen.emit("\tudiv\tx10, x9, x19");
        r#gen.emit("\tmsub\tx20, x10, x19, x9");
        r#gen.emit(".Lrandom_below_draw:");
        r#gen.emit("\tbl\tstone.random_next");
        r#gen.emit("\tcmp\tx0, x20");
        r#gen.emit("\tb.lo\t.Lrandom_below_draw");
        r#gen.emit("\tudiv\tx10, x0, x19");
        r#gen.emit("\tmsub\tx0, x10, x19, x0");
        r#gen.emit("\tb\t.Lrandom_below_done");
        r#gen.emit(".Lrandom_below_any:");
        r#gen.emit("\tbl\tstone.random_next");
        r#gen.emit(".Lrandom_below_done:");
        pop_frame(r#gen, &["x19", "x20"]);
    }
}
