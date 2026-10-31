//! The builtin `os` module, which tells a program about the machine and process it runs on.
//!
//! A program imports it like any builtin module (see [`super::BuiltinModule`]), with `use os`
//! then `os.env("HOME")`, or with `use os.env` then `env("HOME")`.
//!
//! The functions here are what the interpreter runs. Compiled code makes the system calls
//! behind the same libc functions from `stone.os_*` routines, reading what they read, so both
//! backends agree.

use super::{BuiltinDoc, fs};
#[cfg(unix)]
use std::ffi::{c_char, c_int, c_long};
#[cfg(unix)]
use std::os::unix::process::{CommandExt, ExitStatusExt};
#[cfg(unix)]
use std::process::{Child, Command, Stdio};

/// The name a program imports the module by, as in `use os`.
pub const MODULE: &str = "os";

/// What the module is for, which editors show for `os`.
pub const MODULE_DOC: &str = "The builtin module that tells a program about the machine and \
                              process it runs on.";

pub static DOCS: [BuiltinDoc; 10] = [
    BuiltinDoc {
        name: "os.env",
        signature: "os.env(name: str) -> option[str]",
        description: "Returns `some` of the value of the environment variable `name`, or \
                      `none` if it is not set. A name that is empty or holds `=` is never set.",
    },
    BuiltinDoc {
        name: "os.has_env",
        signature: "os.has_env(name: str) -> bool",
        description: "Returns whether the environment variable `name` is set, even to `\"\"`, \
                      as `os.env(name).is_some()` does.",
    },
    BuiltinDoc {
        name: "os.platform",
        signature: "os.platform() -> str",
        description: "Returns the operating system the program runs on: `\"linux\"`, `\"macos\"`, or \
                      `\"windows\"`.",
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
        signature: "os.exit(code: int)",
        description: "Stops the program at once with the exit status `code`, of which the \
                      system keeps the low 8 bits, so `os.exit(256)` exits with 0.",
    },
    BuiltinDoc {
        name: "os.run",
        signature: "os.run(cmd: str, args: list[str]) -> result[output, str]",
        description: "Runs the program `cmd` with the arguments `args` and waits for it to \
                      finish, returning `ok` of an `output` holding its exit status (or minus \
                      the number of the signal that stopped it), and the text it wrote to \
                      stdout and to stderr. A `cmd` without `/` is looked for in each \
                      directory of `PATH`. The program reads nothing from stdin. Only a \
                      program that cannot start is an `err`, of a message such as `no such \
                      file or directory: 'gti'`, and so is output that is not UTF-8 text.",
    },
];

/// Returns the value of the environment variable `name` the way libc's `getenv` sees it: only up
/// to a null character, and never set if that is empty or holds `=`.
fn lookup(name: &str) -> Option<String> {
    let name = name.split('\0').next().unwrap_or("");
    if name.is_empty() || name.contains('=') {
        return None;
    }
    std::env::var_os(name).map(|value| value.to_string_lossy().into_owned())
}

/// Returns the environment variable `name`, or `None` if it is not set.
///
/// For example, `env("HOME")` might return `Some("/home/ada")`, and `env("A=B")` always returns
/// `None`.
pub fn env(name: &str) -> Option<String> {
    lookup(name)
}

/// Returns whether the environment variable `name` is set, even to `""`.
pub fn has_env(name: &str) -> bool {
    lookup(name).is_some()
}

/// Returns the operating system, such as `"linux"`, `"macos"`, or `"windows"`.
pub fn platform() -> &'static str {
    std::env::consts::OS
}

/// Returns the processor architecture, such as `"x86_64"` or `"aarch64"`.
pub fn arch() -> &'static str {
    std::env::consts::ARCH
}

#[cfg(unix)]
unsafe extern "C" {
    fn gethostname(name: *mut c_char, len: usize) -> c_int;
    fn sysconf(name: c_int) -> c_long;
}

#[cfg(windows)]
#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetComputerNameExW(format: i32, buffer: *mut u16, size: *mut u32) -> i32;
    fn GetActiveProcessorCount(group: u16) -> u32;
}

/// `sysconf`'s name for the number of processors that are online.
#[cfg(target_os = "macos")]
const SC_NPROCESSORS_ONLN: c_int = 58;
#[cfg(all(unix, not(target_os = "macos")))]
const SC_NPROCESSORS_ONLN: c_int = 84;

/// The size of the buffer `gethostname` fills, which is larger than any Linux host name.
pub const HOSTNAME_BUFFER: usize = 256;

/// Returns the machine's host name from libc's `gethostname`, or `""` if it fails.
#[cfg(unix)]
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

/// Returns the machine's DNS host name from `GetComputerNameExW`, as compiled code asks for it,
/// or `""` if it fails.
#[cfg(windows)]
pub fn hostname() -> String {
    let mut buffer = [0u16; HOSTNAME_BUFFER];
    let mut size = HOSTNAME_BUFFER as u32;
    // SAFETY: the buffer holds `size` code units, which the call writes at most; 1 is
    // ComputerNameDnsHostname
    let ok = unsafe { GetComputerNameExW(1, buffer.as_mut_ptr(), &mut size) };
    if ok == 0 {
        return String::new();
    }
    String::from_utf16_lossy(&buffer[..size as usize])
}

/// Returns the number of online processors from libc's `sysconf`, or 1 if it fails.
#[cfg(unix)]
pub fn cpu_count() -> i64 {
    // SAFETY: sysconf only reads its argument
    let count = unsafe { sysconf(SC_NPROCESSORS_ONLN) };
    (count as i64).max(1)
}

/// Returns the number of processors in every processor group, from `GetActiveProcessorCount`,
/// as compiled code counts them, or 1 if it fails.
#[cfg(windows)]
pub fn cpu_count() -> i64 {
    // SAFETY: the call only reads its argument; 0xffff is ALL_PROCESSOR_GROUPS
    let count = unsafe { GetActiveProcessorCount(0xffff) };
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

/// The reason `os.run` gives for a command or argument holding a null character, which no
/// program can be given.
pub const NULL_IN_COMMAND: &str = "command holds a null character";

/// The reason `os.run` gives under `cargo fuzz`, where it runs nothing, so mutated programs
/// cannot start other programs.
pub const RUN_DISABLED: &str = "running programs is disabled while fuzzing";

/// The directories a command is looked for in when `PATH` is not set, as glibc's `execvp`
/// looks.
pub const DEFAULT_PATH: &str = "/bin:/usr/bin";

/// The error numbers `os.run` treats specially while it looks for a program.
#[cfg(unix)]
const ENOENT: i32 = 2;
#[cfg(unix)]
const EACCES: i32 = 13;
#[cfg(unix)]
const ENOTDIR: i32 = 20;

/// Returns the paths `os.run` tries, in order, for the command `cmd` when `PATH` is `path`.
///
/// A command holding `/` is itself the only path, an empty one has none, and any other is
/// joined to each directory of `path` (or [`DEFAULT_PATH`]), an empty directory being the
/// working directory. For example, `candidates("ls", Some("/usr/bin::/bin"))` is
/// `["/usr/bin/ls", "./ls", "/bin/ls"]`, and `candidates("./ls", None)` is `["./ls"]`.
pub fn candidates(cmd: &str, path: Option<&str>) -> Vec<String> {
    if cmd.contains('/') {
        return vec![cmd.to_string()];
    }
    if cmd.is_empty() {
        return vec![];
    }
    path.unwrap_or(DEFAULT_PATH)
        .split(':')
        .map(|dir| match dir {
            "" => format!("./{cmd}"),
            _ => format!("{dir}/{cmd}"),
        })
        .collect()
}

/// Runs the program `cmd` with the arguments `args`, its stdin empty and its stdout and stderr
/// captured, and returns its status and what it wrote, or the message of why it could not run.
///
/// It tries each of [`candidates`] in turn, as `execvp` does: a path that names nothing (or
/// goes through a file) moves on to the next, and so does one that may not be run, though
/// that error is remembered, while any other error stops. The error is then `permission
/// denied` if any path gave it, else the last path's. The status is the exit status, or minus
/// the number of the signal that stopped the program, and output that is not UTF-8 text, or
/// holds a null byte, is an error.
///
/// For example, `run("sh", &["-c".into(), "echo hi; exit 2".into()])` returns
/// `Ok((2, "hi\n", ""))`, and `run("gti", &[])` returns
/// `Err("no such file or directory: 'gti'")`.
#[cfg(unix)]
pub fn run(cmd: &str, args: &[String]) -> Result<(i64, String, String), String> {
    if cmd.contains('\0') || args.iter().any(|arg| arg.contains('\0')) {
        return Err(fs::message(NULL_IN_COMMAND, cmd));
    }
    if cfg!(fuzzing) {
        return Err(fs::message(RUN_DISABLED, cmd));
    }
    let path = std::env::var_os("PATH").map(|path| path.to_string_lossy().into_owned());
    let mut denied = false;
    let mut last = ENOENT;
    for candidate in candidates(cmd, path.as_deref()) {
        let spawned = Command::new(&candidate)
            .arg0(cmd)
            .args(args)
            .stdin(Stdio::null())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn();
        let error = match spawned {
            Ok(child) => return finish(cmd, child),
            Err(error) => error,
        };
        match error.raw_os_error() {
            Some(errno @ (ENOENT | ENOTDIR)) => last = errno,
            Some(EACCES) => {
                denied = true;
                last = EACCES;
            }
            Some(_) | None => return Err(fs::io_message(&error, cmd)),
        }
    }
    let errno = if denied { EACCES } else { last };
    Err(fs::io_message(
        &std::io::Error::from_raw_os_error(errno),
        cmd,
    ))
}

/// Fails, since `os.run` is not supported on Windows yet, where a program that calls it is an
/// error before it runs (`driver::unsupported`).
#[cfg(not(unix))]
pub fn run(cmd: &str, _: &[String]) -> Result<(i64, String, String), String> {
    Err(fs::message(
        "os.run is not supported on this system yet",
        cmd,
    ))
}

/// Waits for the program `child` that `cmd` started and returns its status and output.
#[cfg(unix)]
fn finish(cmd: &str, child: Child) -> Result<(i64, String, String), String> {
    let output = child
        .wait_with_output()
        .map_err(|error| fs::io_message(&error, cmd))?;
    let status = match output.status.code() {
        Some(code) => code as i64,
        None => -(output.status.signal().unwrap_or(0) as i64),
    };
    let text = |bytes: Vec<u8>| match fs::is_text(&bytes) {
        true => Ok(String::from_utf8(bytes).unwrap_or_default()),
        false => Err(fs::message(fs::INVALID_TEXT, cmd)),
    };
    Ok((status, text(output.stdout)?, text(output.stderr)?))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn env_reads_the_environment() {
        // PATH is set wherever cargo runs tests
        assert!(has_env("PATH"));
        assert_eq!(env("PATH"), std::env::var("PATH").ok());
        assert!(!has_env("STONE_SURELY_UNSET_VARIABLE"));
        assert_eq!(env("STONE_SURELY_UNSET_VARIABLE"), None);
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
    fn commands_are_looked_for_in_each_directory_of_path() {
        assert_eq!(candidates("./tool", Some("/bin")), ["./tool"]);
        assert_eq!(candidates("a/b", None), ["a/b"]);
        assert_eq!(candidates("", Some("/bin")), Vec::<String>::new());
        assert_eq!(
            candidates("ls", Some("/usr/bin::/bin/")),
            ["/usr/bin/ls", "./ls", "/bin//ls"]
        );
        assert_eq!(candidates("ls", Some("")), ["./ls"]);
        assert_eq!(candidates("ls", None), ["/bin/ls", "/usr/bin/ls"]);
    }

    #[test]
    #[cfg(unix)]
    fn run_captures_status_and_output() {
        let script = "echo out; echo err >&2; exit 3";
        let ran = run("sh", &["-c".into(), script.into()]).unwrap();
        assert_eq!(ran, (3, "out\n".to_string(), "err\n".to_string()));
        // the program's name comes first, and stdin holds nothing
        let ran = run(
            "sh",
            &[
                "-c".into(),
                "echo $0 $1; cat".into(),
                "x".into(),
                "y".into(),
            ],
        );
        assert_eq!(ran.unwrap(), (0, "x y\n".to_string(), String::new()));
        let killed = run("sh", &["-c".into(), "kill -9 $$".into()]).unwrap();
        assert_eq!(killed.0, -9);
    }

    #[test]
    #[cfg(unix)]
    fn run_fails_when_the_program_cannot_start() {
        assert_eq!(
            run("stone-surely-missing-command", &[]),
            Err("no such file or directory: 'stone-surely-missing-command'".to_string())
        );
        assert_eq!(
            run("./stone/surely/missing", &[]),
            Err("no such file or directory: './stone/surely/missing'".to_string())
        );
        assert_eq!(
            run("", &[]),
            Err("no such file or directory: ''".to_string())
        );
        assert_eq!(
            run("sh", &["a\0b".into()]),
            Err(format!("{NULL_IN_COMMAND}: 'sh'"))
        );
        assert_eq!(
            run("sh", &["-c".into(), "printf '\\377'".into()]),
            Err(format!("{}: 'sh'", fs::INVALID_TEXT))
        );
    }

    #[test]
    #[cfg(unix)]
    fn run_cannot_start_a_file_that_is_not_executable() {
        let dir = std::env::temp_dir().join(format!("stone-os-run-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let file = dir.join("data.txt");
        std::fs::write(&file, "text").unwrap();
        for path in [file.to_str().unwrap(), dir.to_str().unwrap()] {
            assert_eq!(run(path, &[]), Err(format!("permission denied: '{path}'")));
        }
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn system_details_are_plausible() {
        assert!(!platform().is_empty());
        assert!(!arch().is_empty());
        assert!(cpu_count() >= 1);
        assert!(pid() > 0);
        let cwd = cwd().unwrap();
        assert!(std::path::Path::new(&cwd).is_absolute(), "{cwd}");
    }
}
