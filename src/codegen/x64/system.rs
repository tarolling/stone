//! How the x86-64 runtime calls the operating system.
//!
//! On Linux the runtime makes system calls itself, with the number in `eax` and the arguments in
//! `rdi`, `rsi`, `rdx`, `r10`, `r8`, and `r9`. Windows has no stable system call interface, so
//! there the same code calls a shim per call, `stone.sys_<name>` (see `windows::windows_shims`),
//! which keeps a Linux system call's contract over `kernel32.dll`: the same arguments, the
//! result or the negative Linux error number in `rax`, and no other register changed but `rcx`
//! and `r11`, which `syscall` changes too. So the rest of the runtime is the same on both.

use crate::codegen::{AssemblyGenerator, Os};

/// A call into the operating system that the x86-64 runtime makes, named for the Linux system
/// call it is on Linux.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sys {
    Read,
    Write,
    Close,
    Mmap,
    Munmap,
    Mremap,
    Nanosleep,
    Getpid,
    Uname,
    Getcwd,
    Getdents64,
    ClockGettime,
    Exit,
    Openat,
    Mkdirat,
    Unlinkat,
    Faccessat,
    Getrandom,
    /// The number of processors, which only Windows asks for, since Linux reads it from
    /// [`CPU_ONLINE`](crate::codegen::context::CPU_ONLINE).
    CpuCount,
}

impl Sys {
    /// Every call, in the order the Windows shims are emitted.
    pub const ALL: [Sys; 19] = [
        Sys::Read,
        Sys::Write,
        Sys::Close,
        Sys::Mmap,
        Sys::Munmap,
        Sys::Mremap,
        Sys::Nanosleep,
        Sys::Getpid,
        Sys::Uname,
        Sys::Getcwd,
        Sys::Getdents64,
        Sys::ClockGettime,
        Sys::Exit,
        Sys::Openat,
        Sys::Mkdirat,
        Sys::Unlinkat,
        Sys::Faccessat,
        Sys::Getrandom,
        Sys::CpuCount,
    ];

    /// Returns the call's Linux system call number, or `None` for one Linux has not.
    ///
    /// For example, `Sys::Write.linux()` is `Some(1)`.
    pub fn linux(self) -> Option<u32> {
        Some(match self {
            Sys::Read => 0,
            Sys::Write => 1,
            Sys::Close => 3,
            Sys::Mmap => 9,
            Sys::Munmap => 11,
            Sys::Mremap => 25,
            Sys::Nanosleep => 35,
            Sys::Getpid => 39,
            Sys::Uname => 63,
            Sys::Getcwd => 79,
            Sys::Getdents64 => 217,
            Sys::ClockGettime => 228,
            Sys::Exit => 231,
            Sys::Openat => 257,
            Sys::Mkdirat => 258,
            Sys::Unlinkat => 263,
            Sys::Faccessat => 269,
            Sys::Getrandom => 318,
            Sys::CpuCount => return None,
        })
    }

    /// Returns the call's name, as Linux names its system call.
    pub fn name(self) -> &'static str {
        match self {
            Sys::Read => "read",
            Sys::Write => "write",
            Sys::Close => "close",
            Sys::Mmap => "mmap",
            Sys::Munmap => "munmap",
            Sys::Mremap => "mremap",
            Sys::Nanosleep => "nanosleep",
            Sys::Getpid => "getpid",
            Sys::Uname => "uname",
            Sys::Getcwd => "getcwd",
            Sys::Getdents64 => "getdents64",
            Sys::ClockGettime => "clock_gettime",
            Sys::Exit => "exit_group",
            Sys::Openat => "openat",
            Sys::Mkdirat => "mkdirat",
            Sys::Unlinkat => "unlinkat",
            Sys::Faccessat => "faccessat",
            Sys::Getrandom => "getrandom",
            Sys::CpuCount => "cpu_count",
        }
    }

    /// Returns the label of the call's Windows shim, such as `stone.sys_write`.
    pub fn shim(self) -> String {
        format!("stone.sys_{}", self.name())
    }
}

/// Emits a call into the operating system with its arguments where a Linux system call takes
/// them, which returns its result in `rax` and changes no other register but `rcx` and `r11`.
///
/// On Linux that is `mov eax, n` and `syscall`. On Windows it calls the call's shim.
pub fn system_call(r#gen: &mut dyn AssemblyGenerator, call: Sys) {
    match r#gen.target().os {
        Os::Windows => r#gen.emit(&format!("\tcall\t{}", call.shim())),
        _ => {
            let number = call
                .linux()
                .expect("a system call the runtime makes on Linux");
            r#gen.emit(&format!("\tmov\teax, {number}"));
            r#gen.emit("\tsyscall");
        }
    }
}
