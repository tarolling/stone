//! The internal modules behind GPU kernels on Linux.
//!
//! A program built with `--gpu` for Linux runs its list arithmetic on an NVIDIA GPU when it can,
//! talking to the kernel driver through `/dev/nvidiactl`, `/dev/nvidia-uvm`, and `/dev/nvidia0`
//! itself, with no libc and no CUDA library. That driver is written in stone, in `gpu_nv.stn`,
//! which such a build links with the program, and it reaches the system and raw memory through
//! the internal module `gpu_sys`, whose routines compiled code emits for Linux alone. The
//! interpreter never runs kernels, so it never calls them.

use super::BuiltinDoc;

/// The name of the driver module written in stone.
pub const NV_MODULE: &str = "gpu_nv";

/// The driver's stone source, which a build with `--gpu` links with the program.
pub const NV_SOURCE: &str = include_str!("gpu_nv.stn");

/// The name of the internal module through which `gpu_nv.stn` reaches the system.
pub const SYS_MODULE: &str = "gpu_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The system calls and raw memory behind GPU kernels on Linux.";

pub static SYS_DOCS: [BuiltinDoc; 14] = [
    BuiltinDoc {
        name: "gpu_sys.open",
        signature: "gpu_sys.open(path: str) -> int",
        description: "Opens the file at `path` to read and write, closed on `exec`, and \
                      returns its descriptor, or the negative error number.",
    },
    BuiltinDoc {
        name: "gpu_sys.close",
        signature: "gpu_sys.close(fd: int)",
        description: "Closes the descriptor `fd`.",
    },
    BuiltinDoc {
        name: "gpu_sys.ioctl",
        signature: "gpu_sys.ioctl(fd: int, request: int, address: int) -> int",
        description: "Makes the request `request` of the device `fd` with the memory at \
                      `address`, returning what the system gives, a negative error number on \
                      failure.",
    },
    BuiltinDoc {
        name: "gpu_sys.mmap",
        signature: "gpu_sys.mmap(address: int, length: int, protection: int, flags: int, fd: \
                    int, offset: int) -> int",
        description: "Maps `length` bytes as the system call does, returning the address, or \
                      the negative error number.",
    },
    BuiltinDoc {
        name: "gpu_sys.munmap",
        signature: "gpu_sys.munmap(address: int, length: int)",
        description: "Unmaps the `length` bytes at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.peek32",
        signature: "gpu_sys.peek32(address: int) -> int",
        description: "Returns the 32-bit word at `address`, from 0 to 4294967295.",
    },
    BuiltinDoc {
        name: "gpu_sys.peek64",
        signature: "gpu_sys.peek64(address: int) -> int",
        description: "Returns the 64-bit word at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.poke32",
        signature: "gpu_sys.poke32(address: int, value: int)",
        description: "Writes the low 32 bits of `value` at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.poke64",
        signature: "gpu_sys.poke64(address: int, value: int)",
        description: "Writes `value` at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.copy",
        signature: "gpu_sys.copy(target: int, source: int, count: int)",
        description: "Copies `count` bytes from `source` to `target`, which do not overlap.",
    },
    BuiltinDoc {
        name: "gpu_sys.fence",
        signature: "gpu_sys.fence()",
        description: "Makes every write before it visible to the GPU before any after it.",
    },
    BuiltinDoc {
        name: "gpu_sys.state",
        signature: "gpu_sys.state() -> int",
        description: "Returns the word the driver keeps for the whole run, 0 at first.",
    },
    BuiltinDoc {
        name: "gpu_sys.set_state",
        signature: "gpu_sys.set_state(value: int)",
        description: "Sets the word `state` returns.",
    },
    BuiltinDoc {
        name: "gpu_sys.image",
        signature: "gpu_sys.image() -> int",
        description: "Returns the address of the program's GPU image, the kernels it was \
                      built with.",
    },
];

#[cfg(test)]
mod tests {
    use crate::project::MapSources;
    use std::path::Path;

    #[test]
    fn the_driver_module_checks_without_errors_or_warnings() {
        let linked = crate::project::link_with(
            Path::new("main.stn"),
            "print(1)\n",
            &MapSources::default(),
            &[super::NV_MODULE],
        );
        let rendered: Vec<String> = linked
            .diagnostics
            .iter()
            .chain(crate::driver::analyze_linked(&linked).diagnostics.iter())
            .filter(|d| d.span.file != crate::span::FileId(0))
            .map(|d| linked.sources.render(d))
            .collect();
        assert!(rendered.is_empty(), "{}", rendered.join(""));
    }
}
