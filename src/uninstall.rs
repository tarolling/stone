//! `stone uninstall`: removes the running binary.
//!
//! It asks first when stdin is a terminal, needs `--yes` otherwise, and leaves a binary that
//! `cargo install` put in `$CARGO_HOME/bin` to `cargo uninstall`, so cargo's records stay right.

use std::io::{BufRead, IsTerminal, Write};
use std::path::{Path, PathBuf};

/// Removes the stone binary that is running, after confirming unless `yes` is set.
pub fn run(yes: bool) -> Result<(), String> {
    let exe = std::env::current_exe()
        .and_then(|exe| exe.canonicalize())
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
    std::fs::remove_file(&exe).map_err(|e| format!("cannot remove {}: {e}", exe.display()))?;
    println!("removed {}", exe.display());
    Ok(())
}

/// Returns where cargo keeps its installed binaries' home: `$CARGO_HOME`, else `$HOME/.cargo`.
fn cargo_home() -> Option<PathBuf> {
    match std::env::var_os("CARGO_HOME") {
        Some(home) => Some(PathBuf::from(home)),
        None => std::env::var_os("HOME").map(|home| PathBuf::from(home).join(".cargo")),
    }
}

/// Returns whether `exe` sits in `cargo_home`'s `bin` directory, where `cargo install` puts it.
///
/// For example, with a cargo home of `/home/ada/.cargo`, `/home/ada/.cargo/bin/stone` returns
/// `true` and `/home/ada/.local/bin/stone` returns `false`.
fn installed_by_cargo(exe: &Path, cargo_home: Option<&Path>) -> bool {
    let Some(home) = cargo_home else {
        return false;
    };
    // the cargo home may be reached through a symlink, like the canonical `exe`
    let bin = home.join("bin");
    let bin = bin.canonicalize().unwrap_or(bin);
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
    fn confirmed_accepts_only_yes() {
        for answer in ["y", "Yes\n", " YES "] {
            assert!(confirmed(answer), "{answer:?}");
        }
        for answer in ["", "\n", "n", "no", "yess"] {
            assert!(!confirmed(answer), "{answer:?}");
        }
    }
}
