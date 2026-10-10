//! The builtin `os` module, which tells a program about the machine and process it runs on.
//!
//! A program imports it like a module file, with `use os` then `os.env("HOME")`, or with
//! `use os.env` then `env("HOME")`. Linking renames each call to the function's linked name, such
//! as `os.env`, which no stone name can collide with since it holds a dot.
//!
//! The functions here are what the interpreter runs. Compiled code makes the system calls
//! behind the same libc functions from `stone.os_*` routines, reading what they read, so both
//! backends agree.

use super::BuiltinDoc;
use std::ffi::{c_char, c_int, c_long};
use std::sync::OnceLock;
use std::time::{Instant, SystemTime, UNIX_EPOCH};

/// The name a program imports the module by, as in `use os`.
pub const MODULE: &str = "os";

/// What the module is for, which editors show for `os`.
pub const MODULE_DOC: &str = "The builtin module that tells a program about the machine and \
                              process it runs on.";

/// The linked name of every function of the module.
pub static FUNCTIONS: [&str; 11] = [
    "os.env",
    "os.has_env",
    "os.platform",
    "os.arch",
    "os.hostname",
    "os.cpu_count",
    "os.pid",
    "os.cwd",
    "os.exit",
    "os.time",
    "os.clock",
];

pub static DOCS: [BuiltinDoc; 11] = [
    BuiltinDoc {
        name: "os.env",
        signature: "os.env(name: str) -> str",
        description: "Returns the value of the environment variable `name`, or `\"\"` if it is \
                      not set. A name that is empty or holds `=` is never set.",
    },
    BuiltinDoc {
        name: "os.has_env",
        signature: "os.has_env(name: str) -> bool",
        description: "Returns whether the environment variable `name` is set, which tells an \
                      empty value apart from a missing one.",
    },
    BuiltinDoc {
        name: "os.platform",
        signature: "os.platform() -> str",
        description: "Returns the operating system the program runs on, such as `\"linux\"`.",
    },
    BuiltinDoc {
        name: "os.arch",
        signature: "os.arch() -> str",
        description: "Returns the processor architecture the program runs on, `\"x86_64\"` or \
                      `\"aarch64\"`.",
    },
    BuiltinDoc {
        name: "os.hostname",
        signature: "os.hostname() -> str",
        description: "Returns the name of the machine the program runs on.",
    },
    BuiltinDoc {
        name: "os.cpu_count",
        signature: "os.cpu_count() -> int",
        description: "Returns the number of processors that are online, which is at least 1.",
    },
    BuiltinDoc {
        name: "os.pid",
        signature: "os.pid() -> int",
        description: "Returns the process ID of the running program. Under `stone run`, that \
                      is the interpreter's process.",
    },
    BuiltinDoc {
        name: "os.cwd",
        signature: "os.cwd() -> str",
        description: "Returns the absolute path of the current working directory. It stops the \
                      program if the directory cannot be read, such as after it was deleted.",
    },
    BuiltinDoc {
        name: "os.exit",
        signature: "os.exit(code: int) -> none",
        description: "Stops the program at once with the exit status `code`, of which the \
                      system keeps the low 8 bits, so `os.exit(256)` exits with 0.",
    },
    BuiltinDoc {
        name: "os.time",
        signature: "os.time() -> float",
        description: "Returns the seconds since 1970-01-01 00:00:00 UTC, with a fraction.",
    },
    BuiltinDoc {
        name: "os.clock",
        signature: "os.clock() -> float",
        description: "Returns seconds from a fixed but arbitrary point, which never go \
                      backward, so the difference of two calls times the code between them.",
    },
];

/// Returns whether `name` is the linked name of a function of the module, such as `os.env`.
pub fn is_function(name: &str) -> bool {
    FUNCTIONS.contains(&name)
}

/// Returns the value of the environment variable `name` the way libc's `getenv` sees it: only up
/// to a null character, and never set if that is empty or holds `=`.
fn lookup(name: &str) -> Option<String> {
    let name = name.split('\0').next().unwrap_or("");
    if name.is_empty() || name.contains('=') {
        return None;
    }
    std::env::var_os(name).map(|value| value.to_string_lossy().into_owned())
}

/// Returns the environment variable `name`, or `""` if it is not set.
///
/// For example, `env("HOME")` might return `"/home/ada"`, and `env("A=B")` always returns `""`.
pub fn env(name: &str) -> String {
    lookup(name).unwrap_or_default()
}

/// Returns whether the environment variable `name` is set, even to `""`.
pub fn has_env(name: &str) -> bool {
    lookup(name).is_some()
}

/// Returns the operating system, such as `"linux"` or `"macos"`.
pub fn platform() -> &'static str {
    std::env::consts::OS
}

/// Returns the processor architecture, such as `"x86_64"` or `"aarch64"`.
pub fn arch() -> &'static str {
    std::env::consts::ARCH
}

unsafe extern "C" {
    fn gethostname(name: *mut c_char, len: usize) -> c_int;
    fn sysconf(name: c_int) -> c_long;
}

/// `sysconf`'s name for the number of processors that are online.
#[cfg(target_os = "macos")]
const SC_NPROCESSORS_ONLN: c_int = 58;
#[cfg(not(target_os = "macos"))]
const SC_NPROCESSORS_ONLN: c_int = 84;

/// The size of the buffer `gethostname` fills, which is larger than any Linux host name.
pub const HOSTNAME_BUFFER: usize = 256;

/// Returns the machine's host name from libc's `gethostname`, or `""` if it fails.
pub fn hostname() -> String {
    let mut buffer = [0u8; HOSTNAME_BUFFER];
    // SAFETY: the buffer is writable for its whole length, and the last byte stays 0
    let status = unsafe { gethostname(buffer.as_mut_ptr().cast(), HOSTNAME_BUFFER - 1) };
    if status != 0 {
        return String::new();
    }
    let len = buffer.iter().position(|&b| b == 0).unwrap_or(0);
    String::from_utf8_lossy(&buffer[..len]).into_owned()
}

/// Returns the number of online processors from libc's `sysconf`, or 1 if it fails.
pub fn cpu_count() -> i64 {
    // SAFETY: sysconf only reads its argument
    let count = unsafe { sysconf(SC_NPROCESSORS_ONLN) };
    (count as i64).max(1)
}

/// Returns the process ID.
pub fn pid() -> i64 {
    std::process::id() as i64
}

/// The runtime error of `os.cwd` when the working directory cannot be read.
pub const CWD_FAILURE: &str = "could not read the current directory";

/// Returns the current working directory, or the error [`CWD_FAILURE`].
pub fn cwd() -> Result<String, String> {
    std::env::current_dir()
        .map(|path| path.to_string_lossy().into_owned())
        .map_err(|_| CWD_FAILURE.to_string())
}

/// Returns the seconds since the Unix epoch, as whole seconds plus nanoseconds over 1e9, which is
/// how compiled code turns `clock_gettime`'s result into a float.
pub fn time() -> f64 {
    let elapsed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 / 1e9
}

/// Returns the seconds since the first call in this process, which never go backward.
pub fn clock() -> f64 {
    static START: OnceLock<Instant> = OnceLock::new();
    START.get_or_init(Instant::now).elapsed().as_secs_f64()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_function_is_documented() {
        let documented: Vec<&str> = DOCS.iter().map(|doc| doc.name).collect();
        assert_eq!(documented, FUNCTIONS);
        for name in FUNCTIONS {
            assert!(name.starts_with("os."), "{name}");
            let doc = crate::stdlib::builtin_doc(name).unwrap();
            assert!(doc.signature.starts_with(&format!("{name}(")), "{name}");
            assert!(crate::stdlib::is_builtin(name), "{name}");
        }
        assert!(!is_function("os.nope"));
        assert!(!crate::stdlib::is_builtin("env"));
    }

    #[test]
    fn env_reads_the_environment() {
        // PATH is set wherever cargo runs tests
        assert!(has_env("PATH"));
        assert_eq!(env("PATH"), std::env::var("PATH").unwrap());
        assert!(!has_env("STONE_SURELY_UNSET_VARIABLE"));
        assert_eq!(env("STONE_SURELY_UNSET_VARIABLE"), "");
    }

    #[test]
    fn names_libc_cannot_look_up_are_never_set() {
        assert!(!has_env(""));
        assert!(!has_env("PATH=x"));
        assert!(!has_env("=PATH"));
        // getenv stops at the null character
        assert_eq!(env("PATH\0junk"), env("PATH"));
    }

    #[test]
    fn system_details_are_plausible() {
        assert!(!platform().is_empty());
        assert!(!arch().is_empty());
        assert!(cpu_count() >= 1);
        assert!(pid() > 0);
        assert!(cwd().unwrap().starts_with('/'));
        assert!(time() > 1.7e9);
        let start = clock();
        assert!(clock() >= start);
    }
}
