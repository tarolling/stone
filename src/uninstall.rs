//! `stone uninstall`: removes the running binary.
//!
//! It asks first when stdin is a terminal, needs `--yes` otherwise, and leaves a binary that
//! `cargo install` put in `$CARGO_HOME/bin` to `cargo uninstall`, so cargo's records stay right.
//! Windows cannot delete a program while it runs, so there the binary is renamed aside at once
//! and a detached `cmd` deletes it once stone has exited.

use std::io::{BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};

/// Removes the stone binary that is running, after confirming unless `yes` is set.
pub fn run(yes: bool) -> Result<(), String> {
    let exe = std::env::current_exe()
        .and_then(|exe| exe.canonicalize())
        .map(plain)
        .map_err(|e| format!("cannot find the stone binary: {e}"))?;
    if installed_by_cargo(&exe, cargo_home().as_deref()) {
        return Err("stone was installed by cargo; run `cargo uninstall stone` instead".into());
    }
    if !yes {
        let stdin = std::io::stdin();
        if !stdin.is_terminal() {
            return Err("pass --yes to uninstall without a terminal".into());
        }
        eprint!("remove {}? [y/N] ", exe.display());
        std::io::stderr().flush().ok();
        let mut answer = String::new();
        stdin
            .lock()
            .read_line(&mut answer)
            .map_err(|e| format!("cannot read the answer: {e}"))?;
        if !confirmed(&answer) {
            return Ok(());
        }
    }
    remove(&exe).map_err(|e| format!("cannot remove {}: {e}", exe.display()))?;
    println!("removed {}", exe.display());
    Ok(())
}

/// Removes the running binary `exe`.
#[cfg(not(windows))]
fn remove(exe: &Path) -> std::io::Result<()> {
    std::fs::remove_file(exe)
}

/// Removes the running binary `exe`: a running program can be renamed but not deleted, so it
/// moves to `<exe>.old` in the same directory, and a `cmd` started without a console deletes that
/// once this process has exited, trying every second for a minute (an `if` takes the rest of its
/// line, so it comes last).
#[cfg(windows)]
fn remove(exe: &Path) -> std::io::Result<()> {
    use std::os::windows::process::CommandExt;
    const CREATE_NO_WINDOW: u32 = 0x0800_0000;
    const DETACHED_PROCESS: u32 = 0x0000_0008;
    let mut old = exe.as_os_str().to_owned();
    old.push(".old");
    let old = PathBuf::from(old);
    let _ = std::fs::remove_file(&old);
    std::fs::rename(exe, &old)?;
    // no Windows file name holds a quote, so quoting it is enough
    let old = old.display();
    let script = format!(
        "for /l %i in (1,1,60) do @(ping -n 2 127.0.0.1 >nul & del /f /q \"{old}\" >nul 2>nul \
         & if not exist \"{old}\" exit /b)"
    );
    std::process::Command::new("cmd")
        .args(["/d", "/c"])
        .raw_arg(script)
        .stdin(std::process::Stdio::null())
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .creation_flags(CREATE_NO_WINDOW | DETACHED_PROCESS)
        .spawn()?;
    Ok(())
}

/// Returns `path` without the `\\?\` that Windows puts before a canonical path, which `cmd`
/// does not take and people do not write.
///
/// For example, `plain(r"\\?\C:\bin\stone.exe".into())` returns `C:\bin\stone.exe`, and every
/// other path is returned as it is.
fn plain(path: PathBuf) -> PathBuf {
    match path.to_str().and_then(|p| p.strip_prefix(r"\\?\")) {
        Some(rest) if !rest.starts_with("UNC") => PathBuf::from(rest),
        _ => path,
    }
}

/// Returns where cargo keeps its installed binaries' home: `$CARGO_HOME`, else `.cargo` in the
/// home directory, `$HOME` (or `%USERPROFILE%` on Windows, as cargo finds it).
fn cargo_home() -> Option<PathBuf> {
    if let Some(home) = std::env::var_os("CARGO_HOME") {
        return Some(PathBuf::from(home));
    }
    let home = std::env::var_os("HOME").or_else(|| {
        if cfg!(windows) {
            std::env::var_os("USERPROFILE")
        } else {
            None
        }
    })?;
    Some(PathBuf::from(home).join(".cargo"))
}

/// Returns whether `exe` sits in `cargo_home`'s `bin` directory, where `cargo install` puts it.
///
/// For example, with a cargo home of `/home/ada/.cargo`, `/home/ada/.cargo/bin/stone` returns
/// `true` and `/home/ada/.local/bin/stone` returns `false`.
fn installed_by_cargo(exe: &Path, cargo_home: Option<&Path>) -> bool {
    let Some(home) = cargo_home else {
        return false;
    };
    // the cargo home may be reached through a symlink, like the canonical, plain `exe`
    let bin = home.join("bin");
    let bin = bin.canonicalize().map(plain).unwrap_or(bin);
    exe.parent() == Some(bin.as_path())
}

/// Returns whether `answer` to a `[y/N]` prompt means yes.
///
/// For example, `confirmed("Yes\n")` returns `true`, and `confirmed("")` returns `false`.
fn confirmed(answer: &str) -> bool {
    let answer = answer.trim();
    answer.eq_ignore_ascii_case("y") || answer.eq_ignore_ascii_case("yes")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn installed_by_cargo_finds_the_cargo_bin_directory() {
        let home = Path::new("/nonexistent/ada/.cargo");
        assert!(installed_by_cargo(
            Path::new("/nonexistent/ada/.cargo/bin/stone"),
            Some(home)
        ));
        assert!(!installed_by_cargo(
            Path::new("/nonexistent/ada/.local/bin/stone"),
            Some(home)
        ));
    }

    #[test]
    fn installed_by_cargo_is_false_without_a_cargo_home() {
        assert!(!installed_by_cargo(
            Path::new("/nonexistent/ada/.cargo/bin/stone"),
            None
        ));
    }

    #[test]
    fn installed_by_cargo_matches_a_canonical_binary_in_cargo_bin() {
        let home = std::env::temp_dir().join(format!("stone-uninstall-{}", std::process::id()));
        let bin = home.join("bin");
        std::fs::create_dir_all(&bin).unwrap();
        let file = bin.join("stone");
        std::fs::write(&file, "").unwrap();
        // run gives the binary's path canonical and plain
        let exe = plain(file.canonicalize().unwrap());
        assert!(installed_by_cargo(&exe, Some(&home)));
        std::fs::remove_dir_all(home).unwrap();
    }

    #[test]
    fn plain_drops_the_verbatim_prefix_of_a_drive_path() {
        assert_eq!(
            plain(PathBuf::from(r"\\?\C:\bin\stone.exe")),
            PathBuf::from(r"C:\bin\stone.exe")
        );
        assert_eq!(
            plain(PathBuf::from(r"\\?\UNC\host\share\stone.exe")),
            PathBuf::from(r"\\?\UNC\host\share\stone.exe")
        );
        assert_eq!(
            plain(PathBuf::from("/usr/bin/stone")),
            PathBuf::from("/usr/bin/stone")
        );
    }

    #[test]
    fn confirmed_accepts_only_yes() {
        for answer in ["y", "Yes\n", " YES "] {
            assert!(confirmed(answer), "{answer:?}");
        }
        for answer in ["", "\n", "n", "no", "yess"] {
            assert!(!confirmed(answer), "{answer:?}");
        }
    }
}
