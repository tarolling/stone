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

/// The name of the module, written in stone, that decides where each kernel runs and calls the
/// driver.
pub const PLAN_MODULE: &str = "gpu_plan";

/// The planner's stone source, which a build with `--gpu` links with the program.
pub const PLAN_SOURCE: &str = include_str!("gpu_plan.stn");

/// The name of the module, written in stone, that computes kernels on the processor as a GPU
/// would, for a build that simulates its GPU.
pub const SIM_MODULE: &str = "gpu_sim";

/// The simulator's stone source.
pub const SIM_SOURCE: &str = include_str!("gpu_sim.stn");

/// The name of the internal module through which `gpu_nv.stn` reaches the system.
pub const SYS_MODULE: &str = "gpu_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The system calls and raw memory behind GPU kernels on Linux.";

pub static SYS_DOCS: [BuiltinDoc; 19] = [
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
    BuiltinDoc {
        name: "gpu_sys.peek_float",
        signature: "gpu_sys.peek_float(address: int) -> float",
        description: "Returns the float at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.poke_float",
        signature: "gpu_sys.poke_float(address: int, value: float)",
        description: "Writes the float `value` at `address`.",
    },
    BuiltinDoc {
        name: "gpu_sys.simulated",
        signature: "gpu_sys.simulated() -> bool",
        description: "Returns whether the build simulates its GPU for tests (`STONE_GPU_SIMULATE` \
                      set when it was built), a constant, so an `if` on it lowers one branch.",
    },
    BuiltinDoc {
        name: "gpu_sys.plan",
        signature: "gpu_sys.plan() -> int",
        description: "Returns the address of the 4096 bytes `gpu_plan` keeps for the whole \
                      run, zero at first.",
    },
    BuiltinDoc {
        name: "gpu_sys.min_lengths",
        signature: "gpu_sys.min_lengths() -> int",
        description: "Returns the address of the table of each record's shortest list worth \
                      a launch, 8 bytes per record and zero at first, which compiled code \
                      reads before each launch.",
    },
];

#[cfg(test)]
mod tests {
    use crate::interpreter::{Interpreter, Limits, Value};
    use crate::project::MapSources;
    use std::path::Path;

    /// Calls the planner's function `name` with `args` in the interpreter.
    fn call_plan(name: &str, args: Vec<Value>) -> Value {
        let linked = crate::project::link_with(
            Path::new("main.stn"),
            "print(1)\n",
            &MapSources::default(),
            &[super::PLAN_MODULE],
        );
        let analysis = crate::driver::analyze_linked(&linked);
        assert!(analysis.diagnostics.is_empty());
        let mut out = Vec::new();
        let mut interpreter =
            Interpreter::with_output(&mut out, Limits::DEFAULT).with_calls(analysis.calls());
        interpreter
            .call_function(&linked.module, &format!("gpu_plan.{name}"), args)
            .unwrap()
    }

    /// Calls the planner's function `name` with the ints `args`.
    fn call_ints(name: &str, args: &[i64]) -> Value {
        call_plan(name, args.iter().map(|&arg| Value::Int(arg)).collect())
    }

    fn int(value: Value) -> i64 {
        match value {
            Value::Int(n) => n,
            other => panic!("not an int: {other:?}"),
        }
    }

    #[test]
    fn a_kernel_is_launched_only_for_lists_long_enough_to_repay_reaching_the_gpu() {
        const NEVER: i64 = i64::MAX;
        // cpu, gpu, bytes in, bytes out, latency, up, down, all in femtoseconds
        let shortest = |args: [i64; 7]| int(call_ints("min_length", &args));
        // saving 1 ns an element repays a 10 us launch from 10 000 elements on
        assert_eq!(
            shortest([1_200_000, 200_000, 0, 0, 10_000_000_000, 0, 0]),
            10_000
        );
        // a part of an element still needs a whole one
        assert_eq!(
            shortest([1_200_000, 200_000, 0, 0, 10_000_000_001, 0, 0]),
            10_001
        );
        // moving 24 bytes an element at 80 000 each costs more than computing it saves
        assert_eq!(
            shortest([1_680_000, 60_000, 16, 8, 10_000_000_000, 80_000, 80_000]),
            NEVER
        );
        // a kernel heavy enough to repay the bus too
        assert_eq!(
            shortest([20_000_000, 400_000, 16, 8, 10_000_000_000, 80_000, 80_000]),
            566
        );
        // never fewer than one element
        assert_eq!(shortest([1_200_000, 200_000, 0, 0, 0, 0, 0]), 1);
    }

    #[test]
    fn a_probe_that_finds_a_slow_or_absent_gpu_makes_it_unusable() {
        let plausible = |args: [i64; 3]| match call_ints("plausible", &args) {
            Value::Bool(b) => b,
            other => panic!("not a bool: {other:?}"),
        };
        assert!(plausible([20_000_000_000, 90_000, 90_000]));
        // a launch that took nothing, or longer than 50 ms
        assert!(!plausible([0, 90_000, 90_000]));
        assert!(!plausible([60_000_000_000_000, 90_000, 90_000]));
        // transfers slower than 100 MB/s
        assert!(!plausible([20_000_000_000, 20_000_000, 90_000]));
        assert!(!plausible([20_000_000_000, 90_000, 0]));
    }

    #[test]
    fn stone_gpu_names_a_mode() {
        let mode = |setting: &str| {
            let setting = Value::Str(std::rc::Rc::new(setting.to_string()));
            int(call_plan("mode_of", vec![setting]))
        };
        // the costs decide unless the setting is one of the others
        assert_eq!(mode(""), 0);
        assert_eq!(mode("on"), 0);
        assert_eq!(mode("off"), 1);
        assert_eq!(mode("always"), 2);
        assert_eq!(mode("require"), 3);
    }

    #[test]
    fn the_planner_reads_the_images_layout() {
        use crate::codegen::gpu::image::{SHAPE_SIZE, VERSION};
        let source = super::PLAN_SOURCE;
        assert!(source.contains(&format!("const image_version = {VERSION}\n")));
        assert!(source.contains(&format!("const shape_size = {SHAPE_SIZE}\n")));
    }

    #[test]
    fn the_driver_module_checks_without_errors_or_warnings() {
        let linked = crate::project::link_with(
            Path::new("main.stn"),
            "print(1)\n",
            &MapSources::default(),
            &[super::PLAN_MODULE, super::NV_MODULE],
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
