//! The builtin `fs` module, which reads and writes files and directories.
//!
//! A program imports it like any builtin module (see [`super::BuiltinModule`]), with `use fs`
//! then `fs.read("notes.txt")`, or with `use fs.read` then `read("notes.txt")`.
//!
//! Every function but `fs.exists` returns a result whose error is a message naming the path, such
//! as `no such file or directory: 'notes.txt'`, so a program decides what a missing file means,
//! or passes the error on with `?`. The functions here are what the interpreter runs. Compiled
//! code makes the same system calls from `stone.fs_*` routines and words their errors from the
//! same tables ([`COMMON_ERRORS`], [`LINUX_ERRORS`], and [`MACOS_ERRORS`]), so both backends agree.
//! Windows reports its own error codes, which both backends turn into Linux's numbers first
//! ([`windows_errno`]), so a program's errors read the same on every system.

use super::BuiltinDoc;
use crate::codegen::Os;
use std::io::{ErrorKind, Write};

/// The name a program imports the module by, as in `use fs`.
pub const MODULE: &str = "fs";

/// What the module is for, which editors show for `fs`.
pub const MODULE_DOC: &str = "The builtin module that reads and writes files and directories.";

pub static DOCS: [BuiltinDoc; 11] = [
    BuiltinDoc {
        name: "fs.read",
        signature: "fs.read(path: str) -> result[str, str]",
        description: "Returns `ok` of the whole text of the file at `path`, or `err` of a \
                      message such as `no such file or directory: 'notes.txt'`. A file that \
                      is not UTF-8 text, or holds a null byte, is an error too.",
    },
    BuiltinDoc {
        name: "fs.read_lines",
        signature: "fs.read_lines(path: str) -> result[list[str], str]",
        description: "Returns `ok` of the lines of the file at `path`, without their line \
                      endings (`\\n` or `\\r\\n`), or `err` of a message as `fs.read` does. \
                      A file that ends with a line ending has no empty last line.",
    },
    BuiltinDoc {
        name: "fs.write",
        signature: "fs.write(path: str, text: str) -> result[int, str]",
        description: "Writes `text` to the file at `path`, creating it or replacing what it \
                      held, and returns `ok` of the number of bytes written, or `err` of a \
                      message.",
    },
    BuiltinDoc {
        name: "fs.append",
        signature: "fs.append(path: str, text: str) -> result[int, str]",
        description: "Writes `text` to the end of the file at `path`, creating it if there is \
                      none, and returns `ok` of the number of bytes written, or `err` of a \
                      message.",
    },
    BuiltinDoc {
        name: "fs.read_bytes",
        signature: "fs.read_bytes(path: str) -> result[bytes, str]",
        description: "Returns `ok` of every byte of the file at `path`, whatever they are, or \
                      `err` of a message.",
    },
    BuiltinDoc {
        name: "fs.write_bytes",
        signature: "fs.write_bytes(path: str, data: bytes) -> result[int, str]",
        description: "Writes `data` to the file at `path` as `fs.write` writes text, and returns \
                      `ok` of the number of bytes written, or `err` of a message.",
    },
    BuiltinDoc {
        name: "fs.append_bytes",
        signature: "fs.append_bytes(path: str, data: bytes) -> result[int, str]",
        description: "Writes `data` to the end of the file at `path` as `fs.append` appends \
                      text, and returns `ok` of the number of bytes written, or `err` of a \
                      message.",
    },
    BuiltinDoc {
        name: "fs.list_dir",
        signature: "fs.list_dir(path: str) -> result[list[str], str]",
        description: "Returns `ok` of the names in the directory at `path`, without `.` and \
                      `..`, sorted by their bytes, or `err` of a message.",
    },
    BuiltinDoc {
        name: "fs.exists",
        signature: "fs.exists(path: str) -> bool",
        description: "Returns whether there is a file or directory at `path`, following \
                      symbolic links.",
    },
    BuiltinDoc {
        name: "fs.remove",
        signature: "fs.remove(path: str) -> result[str, str]",
        description: "Deletes the file at `path` and returns `ok` of `path`, or `err` of a \
                      message. It does not delete directories.",
    },
    BuiltinDoc {
        name: "fs.make_dir",
        signature: "fs.make_dir(path: str) -> result[str, str]",
        description: "Creates the directory `path`, whose parent must exist, and returns `ok` \
                      of `path`, or `err` of a message, such as `file exists: 'out'`.",
    },
];

/// The reason an error gives for each error number that Linux and macOS share, as
/// `strerror` words it, in lowercase.
pub const COMMON_ERRORS: [(i64, &str); 20] = [
    (1, "operation not permitted"),
    (2, "no such file or directory"),
    (5, "input/output error"),
    (7, "argument list too long"),
    (8, "exec format error"),
    (9, "bad file descriptor"),
    (12, "cannot allocate memory"),
    (13, "permission denied"),
    (16, "device or resource busy"),
    (17, "file exists"),
    (20, "not a directory"),
    (21, "is a directory"),
    (22, "invalid argument"),
    (23, "too many open files in system"),
    (24, "too many open files"),
    (26, "text file busy"),
    (27, "file too large"),
    (28, "no space left on device"),
    (30, "read-only file system"),
    (32, "broken pipe"),
];

/// The reasons for error numbers that only Linux uses.
pub const LINUX_ERRORS: [(i64, &str); 16] = [
    (11, "resource temporarily unavailable"),
    (36, "file name too long"),
    (39, "directory not empty"),
    (40, "too many levels of symbolic links"),
    (97, "address family not supported"),
    (98, "address already in use"),
    (99, "cannot assign requested address"),
    (100, "network is down"),
    (101, "network is unreachable"),
    (103, "connection aborted"),
    (104, "connection reset by peer"),
    (107, "socket is not connected"),
    (110, "connection timed out"),
    (111, "connection refused"),
    (113, "no route to host"),
    (122, "disk quota exceeded"),
];

/// The reasons for the same errors under macOS's numbers.
pub const MACOS_ERRORS: [(i64, &str); 16] = [
    (35, "resource temporarily unavailable"),
    (47, "address family not supported"),
    (48, "address already in use"),
    (49, "cannot assign requested address"),
    (50, "network is down"),
    (51, "network is unreachable"),
    (53, "connection aborted"),
    (54, "connection reset by peer"),
    (57, "socket is not connected"),
    (60, "connection timed out"),
    (61, "connection refused"),
    (62, "too many levels of symbolic links"),
    (63, "file name too long"),
    (65, "no route to host"),
    (66, "directory not empty"),
    (69, "disk quota exceeded"),
];

/// macOS's number for "socket operation on non-socket", which its `connect` gives for a Unix
/// socket path naming a file that is not a socket.
pub const MACOS_ENOTSOCK: i64 = 38;

/// macOS's number for "connection refused", which Linux's `connect` gives for a Unix socket path
/// naming a file that is not a socket, so `net.connect_unix` reports `MACOS_ENOTSOCK` as this.
pub const MACOS_ECONNREFUSED: i64 = 61;

/// The reason an error gives for a file, or a name in a directory, that is not UTF-8 text.
pub const INVALID_TEXT: &str = "not valid UTF-8 text";

/// The reason an error gives for a path holding a null character, which no system call can
/// take.
pub const NULL_IN_PATH: &str = "path holds a null character";

/// Returns every error number the system `os` reports with its reason. Windows errors are
/// turned into Linux's numbers ([`windows_errno`]), so Windows shares Linux's table.
pub fn errors(os: Os) -> impl Iterator<Item = (i64, &'static str)> {
    let own: &[(i64, &str)] = match os {
        Os::Linux | Os::Windows => &LINUX_ERRORS,
        Os::MacOs => &MACOS_ERRORS,
    };
    COMMON_ERRORS.iter().chain(own).copied()
}

/// Returns the system the interpreter runs on, whose error numbers it sees.
pub(crate) fn host() -> Os {
    if cfg!(target_os = "macos") {
        Os::MacOs
    } else if cfg!(windows) {
        Os::Windows
    } else {
        Os::Linux
    }
}

/// Linux's numbers for the Windows error codes that mean the same, which [`windows_errno`]
/// gives: the code first, then the number.
///
/// For example, `ERROR_ALREADY_EXISTS` (183) is `EEXIST` (17).
pub const WINDOWS_ERRNO: [(i64, i64); 17] = [
    (2, 2),    // ERROR_FILE_NOT_FOUND: ENOENT
    (3, 2),    // ERROR_PATH_NOT_FOUND: ENOENT
    (4, 24),   // ERROR_TOO_MANY_OPEN_FILES: EMFILE
    (5, 13),   // ERROR_ACCESS_DENIED: EACCES
    (8, 12),   // ERROR_NOT_ENOUGH_MEMORY: ENOMEM
    (14, 12),  // ERROR_OUTOFMEMORY: ENOMEM
    (15, 2),   // ERROR_INVALID_DRIVE: ENOENT
    (19, 30),  // ERROR_WRITE_PROTECT: EROFS
    (32, 16),  // ERROR_SHARING_VIOLATION: EBUSY
    (80, 17),  // ERROR_FILE_EXISTS: EEXIST
    (87, 22),  // ERROR_INVALID_PARAMETER: EINVAL
    (109, 32), // ERROR_BROKEN_PIPE: EPIPE
    (112, 28), // ERROR_DISK_FULL: ENOSPC
    (145, 39), // ERROR_DIR_NOT_EMPTY: ENOTEMPTY
    (183, 17), // ERROR_ALREADY_EXISTS: EEXIST
    (206, 36), // ERROR_FILENAME_EXCED_RANGE: ENAMETOOLONG
    (267, 20), // ERROR_DIRECTORY: ENOTDIR
];

/// What [`windows_errno`] adds to a Windows error code that has no Linux number, so it stays
/// apart from every Linux number and is still worded as `os error <code>`.
pub const WINDOWS_ERROR_BASE: i64 = 1 << 16;

/// Returns Linux's number for the Windows error `code` of a call about `path`, if it has one,
/// so both backends word Windows errors as they word Linux's, or `code` plus
/// [`WINDOWS_ERROR_BASE`] otherwise.
///
/// Windows tells less than Linux about some failures, so the path decides two of them, as
/// Linux's own numbers would: a missing file under a path one of whose leading parts is a file
/// (`out/a.txt/inner`) is `ENOTDIR` (20), and a file denied because it is a directory (reading
/// or removing `out`) is `EISDIR` (21). Compiled code asks the same questions with
/// `GetFileAttributesW`.
///
/// For example, `windows_errno(183, None)` is 17 (`EEXIST`), and `windows_errno(1450, None)` is
/// `1450 + WINDOWS_ERROR_BASE`.
pub fn windows_errno(code: i64, path: Option<&str>) -> i64 {
    match (code, path) {
        (2 | 3, Some(path)) if names_through_a_file(path) => 20,
        (5, Some(path)) if is_directory(path) => 21,
        _ => WINDOWS_ERRNO
            .iter()
            .find(|&&(windows, _)| windows == code)
            .map_or(code + WINDOWS_ERROR_BASE, |&(_, errno)| errno),
    }
}

/// Returns whether `path`, or a part of it up to a `/` or `\\`, names something that is not a
/// directory, without following a symbolic link at its end, as `GetFileAttributesW` does not.
///
/// For example, with a file `out/a.txt`, `names_through_a_file("out/a.txt/inner")` is true.
fn names_through_a_file(path: &str) -> bool {
    let ends = path
        .char_indices()
        .filter(|&(_, c)| c == '/' || c == '\\')
        .map(|(i, _)| i)
        .chain([path.len()]);
    ends.filter(|&end| end > 0)
        .any(|end| std::fs::symlink_metadata(&path[..end]).is_ok_and(|metadata| !metadata.is_dir()))
}

/// Returns whether `path` names a directory, without following a symbolic link at its end.
fn is_directory(path: &str) -> bool {
    std::fs::symlink_metadata(path).is_ok_and(|metadata| metadata.is_dir())
}

/// Returns the message of an error with `reason` about `path`.
///
/// For example, `message("file exists", "out")` is `file exists: 'out'`.
pub fn message(reason: &str, path: &str) -> String {
    format!("{reason}: '{path}'")
}

/// Returns the message for the error number `errno` under `os` about `path`, which names an
/// error number with no reason in the tables.
///
/// For example, `error_message(Os::Linux, 2, "a.txt")` is
/// `no such file or directory: 'a.txt'`, and an unknown 99 gives `os error 99: 'a.txt'`.
pub fn error_message(os: Os, errno: i64, path: &str) -> String {
    match errors(os).find(|&(number, _)| number == errno) {
        Some((_, reason)) => message(reason, path),
        None if os == Os::Windows && errno >= WINDOWS_ERROR_BASE => {
            message(&format!("os error {}", errno - WINDOWS_ERROR_BASE), path)
        }
        None => message(&format!("os error {errno}"), path),
    }
}

/// Returns the number the tables know an I/O error about `path` by: the system's own, or
/// Linux's on Windows ([`windows_errno`]).
pub(super) fn errno_of(code: i32, path: Option<&str>) -> i64 {
    match host() {
        Os::Windows => windows_errno(code as i64, path),
        _ => code as i64,
    }
}

/// Returns the message for an I/O error about `path`.
pub(super) fn io_message(error: &std::io::Error, path: &str) -> String {
    match error.raw_os_error() {
        Some(code) => error_message(host(), errno_of(code, Some(path)), path),
        None if error.kind() == ErrorKind::InvalidData => message(INVALID_TEXT, path),
        None => message(&error.to_string(), path),
    }
}

/// Fails with [`NULL_IN_PATH`] if `path` holds a null character, which compiled code checks by
/// comparing a string's length with where its first null is.
fn check_path(path: &str) -> Result<(), String> {
    match path.contains('\0') {
        true => Err(message(NULL_IN_PATH, path)),
        false => Ok(()),
    }
}

/// Returns whether `bytes` are UTF-8 text without a null byte, which is what a stone string can
/// hold, since compiled strings end at their first null.
///
/// For example, `is_text(b"caf\xc3\xa9")` is true, and `is_text(b"\xc0\x80")` (an overlong null)
/// and `is_text(b"a\0b")` are false.
pub fn is_text(bytes: &[u8]) -> bool {
    !bytes.contains(&0) && std::str::from_utf8(bytes).is_ok()
}

/// Returns the lines of `text` as Rust's `str::lines` splits them: at each `\n`, dropping a `\r`
/// before it, with no empty line after a final line ending.
///
/// For example, `lines("a\r\nb\n")` is `["a", "b"]` and `lines("\n")` is `[""]`.
pub fn lines(text: &str) -> Vec<&str> {
    text.lines().collect()
}

/// Returns the text of the file at `path`.
pub fn read(path: &str) -> Result<String, String> {
    check_path(path)?;
    let bytes = std::fs::read(path).map_err(|e| io_message(&e, path))?;
    match is_text(&bytes) {
        true => Ok(String::from_utf8(bytes).unwrap_or_default()),
        false => Err(message(INVALID_TEXT, path)),
    }
}

/// Returns the lines of the file at `path`, as [`lines`] splits them.
pub fn read_lines(path: &str) -> Result<Vec<String>, String> {
    let text = read(path)?;
    Ok(lines(&text).into_iter().map(str::to_string).collect())
}

/// Writes `text` to the file at `path`, at its end if `append` is set and over what it held
/// otherwise, returning the number of bytes written.
fn write_to(path: &str, text: &[u8], append: bool) -> Result<i64, String> {
    check_path(path)?;
    let mut options = std::fs::OpenOptions::new();
    match append {
        true => options.append(true),
        false => options.write(true).truncate(true),
    };
    let mut file = options
        .create(true)
        .open(path)
        .map_err(|e| io_message(&e, path))?;
    file.write_all(text).map_err(|e| io_message(&e, path))?;
    Ok(text.len() as i64)
}

/// Writes `text` to the file at `path`, replacing what it held, and returns the bytes written.
pub fn write(path: &str, text: &str) -> Result<i64, String> {
    write_to(path, text.as_bytes(), false)
}

/// Writes `data` to the file at `path`, replacing what it held, and returns the bytes written.
pub fn write_bytes(path: &str, data: &[u8]) -> Result<i64, String> {
    write_to(path, data, false)
}

/// Writes `data` to the end of the file at `path` and returns the bytes written.
pub fn append_bytes(path: &str, data: &[u8]) -> Result<i64, String> {
    write_to(path, data, true)
}

/// Returns every byte of the file at `path`.
pub fn read_bytes(path: &str) -> Result<Vec<u8>, String> {
    check_path(path)?;
    std::fs::read(path).map_err(|e| io_message(&e, path))
}

/// Writes `text` to the end of the file at `path` and returns the bytes written.
pub fn append(path: &str, text: &str) -> Result<i64, String> {
    write_to(path, text.as_bytes(), true)
}

/// Returns the names in the directory at `path`, sorted by their bytes.
pub fn list_dir(path: &str) -> Result<Vec<String>, String> {
    check_path(path)?;
    // Windows would list the current directory for "", where Linux finds nothing
    if path.is_empty() {
        return Err(error_message(host(), 2, path));
    }
    let entries = std::fs::read_dir(path).map_err(|e| io_message(&e, path))?;
    let mut names = Vec::new();
    for entry in entries {
        let entry = entry.map_err(|e| io_message(&e, path))?;
        let name = entry.file_name();
        // a name that is not text has no UTF-8, as a Windows name of unpaired surrogates has none
        let text = name.to_str().filter(|text| is_text(text.as_bytes()));
        let Some(text) = text else {
            return Err(message(INVALID_TEXT, path));
        };
        names.push(text.to_string());
    }
    names.sort();
    Ok(names)
}

/// Returns whether anything is at `path`, following symbolic links.
pub fn exists(path: &str) -> bool {
    check_path(path).is_ok() && std::path::Path::new(path).exists()
}

/// Deletes the file at `path`, returning `path`.
pub fn remove(path: &str) -> Result<String, String> {
    check_path(path)?;
    std::fs::remove_file(path).map_err(|e| io_message(&e, path))?;
    Ok(path.to_string())
}

/// Creates the directory `path`, returning `path`.
pub fn make_dir(path: &str) -> Result<String, String> {
    check_path(path)?;
    std::fs::create_dir(path).map_err(|e| io_message(&e, path))?;
    Ok(path.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    /// Returns a new empty directory for one test.
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("stone-fs-{}-{name}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn errors_are_worded_from_the_tables() {
        assert_eq!(
            error_message(Os::Linux, 2, "a.txt"),
            "no such file or directory: 'a.txt'"
        );
        assert_eq!(error_message(Os::Linux, 36, "x"), "file name too long: 'x'");
        assert_eq!(error_message(Os::MacOs, 63, "x"), "file name too long: 'x'");
        assert_eq!(error_message(Os::MacOs, 36, "x"), "os error 36: 'x'");
        assert_eq!(error_message(Os::Linux, 999, "x"), "os error 999: 'x'");
        assert_eq!(error_message(Os::Linux, 8, "x"), "exec format error: 'x'");
        assert_eq!(
            error_message(Os::MacOs, 35, "x"),
            "resource temporarily unavailable: 'x'"
        );
    }

    #[test]
    fn windows_errors_take_linux_numbers() {
        assert_eq!(windows_errno(183, None), 17);
        assert_eq!(windows_errno(2, None), 2);
        assert_eq!(windows_errno(267, None), 20);
        assert_eq!(windows_errno(5, None), 13);
        assert_eq!(windows_errno(1450, None), 1450 + WINDOWS_ERROR_BASE);
        assert_eq!(
            error_message(Os::Windows, windows_errno(1450, None), "x"),
            "os error 1450: 'x'"
        );
        assert_eq!(
            error_message(Os::Windows, windows_errno(80, None), "x"),
            "file exists: 'x'"
        );

        let dir = scratch("windows-errno");
        let file = dir.join("a.txt");
        std::fs::write(&file, "a").unwrap();
        let dir_path = dir.to_str().unwrap();
        let file_path = file.to_str().unwrap();
        assert_eq!(windows_errno(5, Some(dir_path)), 21);
        assert_eq!(windows_errno(5, Some(file_path)), 13);
        assert_eq!(windows_errno(3, Some(&format!("{file_path}/inner"))), 20);
        assert_eq!(windows_errno(3, Some(&format!("{file_path}\\inner"))), 20);
        assert_eq!(windows_errno(3, Some(file_path)), 20);
        assert_eq!(windows_errno(3, Some(&format!("{dir_path}/missing/x"))), 2);
        assert_eq!(windows_errno(2, Some("")), 2);
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn text_is_utf8_without_nulls() {
        assert!(is_text(b""));
        assert!(is_text("caf\u{e9} \u{1f600}".as_bytes()));
        for bad in [
            &b"a\0b"[..],
            b"\xc0\x80",
            b"\xe0\x80\x80",
            b"\xf0\x80\x80\x80",
            b"\xed\xa0\x80",
            b"\xf4\x90\x80\x80",
            b"\xf5",
            b"\xe2\x82",
            b"\x80",
        ] {
            assert!(!is_text(bad), "{bad:?}");
        }
    }

    #[test]
    fn lines_split_like_rust() {
        assert_eq!(lines(""), Vec::<&str>::new());
        assert_eq!(lines("a"), ["a"]);
        assert_eq!(lines("a\n"), ["a"]);
        assert_eq!(lines("a\r\nb"), ["a", "b"]);
        assert_eq!(lines("\n\n"), ["", ""]);
        assert_eq!(lines("a\rb\n"), ["a\rb"]);
    }

    #[test]
    fn files_round_trip() {
        let dir = scratch("round-trip");
        let path = dir.join("notes.txt");
        let path = path.to_str().unwrap();
        assert!(!exists(path));
        assert_eq!(write(path, "one\n"), Ok(4));
        assert_eq!(append(path, "two\r\nthree"), Ok(10));
        assert!(exists(path));
        assert_eq!(read(path).unwrap(), "one\ntwo\r\nthree");
        assert_eq!(read_lines(path).unwrap(), ["one", "two", "three"]);
        assert_eq!(write(path, "x"), Ok(1));
        assert_eq!(read(path).unwrap(), "x");
        assert_eq!(remove(path), Ok(path.to_string()));
        assert_eq!(read(path), Err(message("no such file or directory", path)));
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn directories_list_sorted() {
        let dir = scratch("listing");
        let root = dir.to_str().unwrap();
        let sub = format!("{root}/sub");
        assert_eq!(make_dir(&sub), Ok(sub.clone()));
        assert_eq!(make_dir(&sub), Err(message("file exists", &sub)));
        for name in ["b", "a", "C"] {
            write(&format!("{root}/{name}"), "").unwrap();
        }
        assert_eq!(list_dir(root).unwrap(), ["C", "a", "b", "sub"]);
        assert_eq!(read(&sub), Err(message("is a directory", &sub)));
        let file = format!("{root}/a");
        assert_eq!(list_dir(&file), Err(message("not a directory", &file)));
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn invalid_text_and_null_paths_are_errors() {
        let dir = scratch("invalid");
        let path = dir.join("bin");
        std::fs::write(&path, b"\xff").unwrap();
        let path = path.to_str().unwrap();
        assert_eq!(read(path), Err(message(INVALID_TEXT, path)));
        assert_eq!(read("a\0b"), Err(message(NULL_IN_PATH, "a\0b")));
        assert!(!exists("a\0b"));
        std::fs::remove_dir_all(dir).unwrap();
    }
}
