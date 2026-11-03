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
#[cfg(any(unix, windows))]
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
    fn GetFileAttributesW(name: *const u16) -> u32;
    fn GetLastError() -> u32;
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

/// Returns the paths `os.run` tries on Windows, in order, for the command `cmd` when `PATH` is
/// `path`.
///
/// A command holding `\` or `/` is itself the only path, an empty one has none, and any other is
/// joined to each directory of `path`, which `;` separates, its quotes dropped and empty
/// directories skipped, as `std::env::split_paths` reads it; with no `PATH` there are no
/// directories, and the working directory is never searched. Each path gets `.exe` when the
/// command's file name has no extension. For example,
/// `windows_candidates("git", Some("C:\\bin;D:\\"))` is `["C:\\bin\\git.exe", "D:\\git.exe"]`,
/// and `windows_candidates(".\\run.com", None)` is `[".\\run.com"]`.
pub fn windows_candidates(cmd: &str, path: Option<&str>) -> Vec<String> {
    if cmd.is_empty() {
        return vec![];
    }
    let name = cmd.rsplit(['\\', '/']).next().unwrap_or(cmd);
    let file = match name.rfind('.') {
        Some(dot) if dot > 0 => cmd.to_string(),
        _ => format!("{cmd}.exe"),
    };
    if cmd.contains(['\\', '/']) {
        return vec![file];
    }
    path.unwrap_or("")
        .split(';')
        .map(|dir| dir.replace('"', ""))
        .filter(|dir| !dir.is_empty())
        .map(|dir| match dir.ends_with(['\\', '/']) {
            true => format!("{dir}{file}"),
            false => format!("{dir}\\{file}"),
        })
        .collect()
}

/// Returns whether `path` names a batch file, which `os.run` never starts itself, since
/// Windows would run it through `cmd.exe` by rules of quoting of its own. It is the error
/// `exec format error`; `os.run("cmd", ["/c", "build.bat"])` runs one.
///
/// For example, `is_batch("make.CMD")` is true.
pub fn is_batch(path: &str) -> bool {
    let lower = path.to_ascii_lowercase();
    lower.ends_with(".bat") || lower.ends_with(".cmd")
}

/// Returns the command line `os.run` starts `program` with on Windows, as Rust's
/// `std::process::Command` writes it, which is what a program's C runtime and Rust's
/// `std::env::args` split back into `args`.
///
/// The program is always quoted. An argument is quoted when it is empty or holds a space or a
/// tab, a `"` becomes `\"` with every backslash before it doubled, and so do the backslashes
/// that end a quoted argument; other backslashes stay as they are. For example,
/// `windows_command_line("t.exe", &["a b".into(), "c\\".into()])` is `"t.exe" "a b" c\`.
pub fn windows_command_line(program: &str, args: &[String]) -> String {
    let mut line = format!("\"{program}\"");
    for arg in args {
        line.push(' ');
        let quote = arg.is_empty() || arg.contains([' ', '\t']);
        if quote {
            line.push('"');
        }
        let mut backslashes = 0;
        for c in arg.chars() {
            if c == '\\' {
                backslashes += 1;
            } else {
                if c == '"' {
                    line.extend(std::iter::repeat_n('\\', backslashes + 1));
                }
                backslashes = 0;
            }
            line.push(c);
        }
        if quote {
            line.extend(std::iter::repeat_n('\\', backslashes));
            line.push('"');
        }
    }
    line
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

/// The Windows error codes of a path `os.run` finds nothing at, so it moves on to the next:
/// `ERROR_FILE_NOT_FOUND`, `ERROR_PATH_NOT_FOUND`, and `ERROR_INVALID_NAME`.
pub const WINDOWS_NOT_FOUND: [i64; 3] = [2, 3, 123];

/// The Windows error code of a path `os.run` may not run, `ERROR_ACCESS_DENIED`, which it
/// remembers and moves on from.
pub const WINDOWS_DENIED: i64 = 5;

/// The Windows error code of a file that is no program, `ERROR_BAD_EXE_FORMAT`, which a batch
/// file gives too.
pub const WINDOWS_BAD_FORMAT: i64 = 193;

/// Runs the program `cmd` as [`run`] does on Unix, trying each of [`windows_candidates`] in
/// turn with the same rules: a path whose file attributes cannot be read because it names
/// nothing moves on, and so does one that `CreateProcessW` may not run, remembered, while any
/// other error stops, a batch file ([`is_batch`]) failing as `exec format error`. The status
/// is the exit code, sign-extended from its 32 bits, and the program is started with the
/// command line [`windows_command_line`] writes, whose first word is the path tried.
///
/// For example, `run("cmd", &["/c".into(), "exit /b 2".into()])` returns `Ok((2, "", ""))`.
#[cfg(windows)]
pub fn run(cmd: &str, args: &[String]) -> Result<(i64, String, String), String> {
    if cmd.contains('\0') || args.iter().any(|arg| arg.contains('\0')) {
        return Err(fs::message(NULL_IN_COMMAND, cmd));
    }
    if cfg!(fuzzing) {
        return Err(fs::message(RUN_DISABLED, cmd));
    }
    let path = std::env::var_os("PATH").map(|path| path.to_string_lossy().into_owned());
    let mut denied = false;
    let mut last = WINDOWS_NOT_FOUND[0];
    for candidate in windows_candidates(cmd, path.as_deref()) {
        let code = match attributes_error(&candidate) {
            Some(code) => code,
            None if is_batch(&candidate) => WINDOWS_BAD_FORMAT,
            None => {
                let spawned = Command::new(&candidate)
                    .args(args)
                    .stdin(Stdio::null())
                    .stdout(Stdio::piped())
                    .stderr(Stdio::piped())
                    .spawn();
                match spawned {
                    Ok(child) => return finish(cmd, child),
                    Err(error) => match error.raw_os_error() {
                        Some(code) => code as i64,
                        None => return Err(fs::io_message(&error, cmd)),
                    },
                }
            }
        };
        match code {
            code if WINDOWS_NOT_FOUND.contains(&code) => last = code,
            WINDOWS_DENIED => {
                denied = true;
                last = code;
            }
            code => return Err(windows_failure(code, cmd)),
        }
    }
    let code = if denied { WINDOWS_DENIED } else { last };
    Err(windows_failure(code, cmd))
}

/// Returns the message of the Windows error `code` about the command `cmd`, worded by Linux's
/// number for it with none of the path rules of [`fs::windows_errno`], as compiled code words
/// it.
#[cfg(windows)]
fn windows_failure(code: i64, cmd: &str) -> String {
    fs::error_message(fs::host(), fs::windows_errno(code, None), cmd)
}

/// Returns the error code `GetFileAttributesW` gives for `path`, or `None` if it names
/// something, as compiled code asks before it starts a program.
#[cfg(windows)]
fn attributes_error(path: &str) -> Option<i64> {
    let wide: Vec<u16> = path.encode_utf16().chain([0]).collect();
    // SAFETY: the name is a null-terminated UTF-16 string that lives through the call
    let attributes = unsafe { GetFileAttributesW(wide.as_ptr()) };
    match attributes {
        // INVALID_FILE_ATTRIBUTES
        u32::MAX => Some(unsafe { GetLastError() } as i64),
        _ => None,
    }
}

/// Fails, since `os.run` has no implementation on this system.
#[cfg(not(any(unix, windows)))]
pub fn run(cmd: &str, _: &[String]) -> Result<(i64, String, String), String> {
    Err(fs::message(
        "os.run is not supported on this system yet",
        cmd,
    ))
}

/// Waits for the program `child` that `cmd` started and returns its status and output.
#[cfg(any(unix, windows))]
fn finish(cmd: &str, child: Child) -> Result<(i64, String, String), String> {
    let output = child
        .wait_with_output()
        .map_err(|error| fs::io_message(&error, cmd))?;
    #[cfg(unix)]
    let status = match output.status.code() {
        Some(code) => code as i64,
        None => -(output.status.signal().unwrap_or(0) as i64),
    };
    #[cfg(windows)]
    let status = output.status.code().unwrap_or(0) as i64;
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
    fn windows_commands_are_looked_for_in_each_directory_of_path() {
        let path = Some(r#"C:\bin;;"C:\Program Files\x";D:\tools\"#);
        assert_eq!(
            windows_candidates("git", path),
            [
                r"C:\bin\git.exe",
                r"C:\Program Files\x\git.exe",
                r"D:\tools\git.exe"
            ]
        );
        // a command with an extension is tried as it is
        assert_eq!(
            windows_candidates("run.com", Some(r"C:\bin")),
            [r"C:\bin\run.com"]
        );
        // a command holding a separator is the only path, and an empty one has none
        assert_eq!(windows_candidates(r".\tool", path), [r".\tool.exe"]);
        assert_eq!(windows_candidates("a/b.txt", path), ["a/b.txt"]);
        assert_eq!(windows_candidates(r"dir.d\tool", None), [r"dir.d\tool.exe"]);
        assert_eq!(
            windows_candidates(".hidden", Some("C:/")),
            ["C:/.hidden.exe"]
        );
        assert_eq!(windows_candidates("", path), Vec::<String>::new());
        assert_eq!(windows_candidates("git", None), Vec::<String>::new());
    }

    #[test]
    fn batch_files_are_never_run_directly() {
        assert!(is_batch(r"C:\x\build.BAT"));
        assert!(is_batch("make.cmd"));
        assert!(!is_batch("cmd.exe"));
        assert!(!is_batch(r"C:\cmd\tool.exe"));
    }

    #[test]
    fn windows_command_lines_quote_as_rust_does() {
        let line = |args: &[&str]| {
            let args: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
            windows_command_line(r"C:\bin\tool.exe", &args)
        };
        assert_eq!(line(&[]), r#""C:\bin\tool.exe""#);
        assert_eq!(line(&["a", "b c", ""]), r#""C:\bin\tool.exe" a "b c" """#);
        // backslashes are literal unless a quote follows them
        assert_eq!(line(&[r"a\b", r"a\\"]), r#""C:\bin\tool.exe" a\b a\\"#);
        assert_eq!(line(&[r#"say "hi""#]), r#""C:\bin\tool.exe" "say \"hi\"""#);
        assert_eq!(line(&[r#"a\"b"#]), r#""C:\bin\tool.exe" a\\\"b"#);
        assert_eq!(line(&[r"dir with\ "]), r#""C:\bin\tool.exe" "dir with\ ""#);
        assert_eq!(line(&[r"end\ \"]), r#""C:\bin\tool.exe" "end\ \\""#);
        assert_eq!(line(&["tab\there"]), "\"C:\\bin\\tool.exe\" \"tab\there\"");
    }

    #[test]
    #[cfg(windows)]
    fn run_captures_status_and_output_on_windows() {
        let script = "echo out& echo err 1>&2& exit /b 3";
        let ran = run("cmd", &["/c".into(), script.into()]).unwrap();
        assert_eq!(ran, (3, "out\r\n".to_string(), "err \r\n".to_string()));
        assert_eq!(
            run("stone-surely-missing-command", &[]),
            Err("no such file or directory: 'stone-surely-missing-command'".to_string())
        );
        assert_eq!(
            run("", &[]),
            Err("no such file or directory: ''".to_string())
        );
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
