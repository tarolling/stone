//! `os.run` on Windows, where a program gets its arguments as one command line that `os.run`
//! writes and the program splits again, and `net`'s use of the system's resolver there,
//! natively on Windows and from Linux under WSL. Elsewhere the tests skip.

use super::windows_gui::{Host, build};
use super::*;

/// A program that prints each of its arguments in brackets.
const ECHO: &str = "\
for arg in args():
    print(\"[{arg}]\")
";

/// Arguments that need quoting and escaping on a Windows command line.
const HARD: [&str; 11] = [
    "plain",
    "two words",
    "",
    "quote\"inside",
    "back\\slash",
    "trail\\",
    "trail space\\",
    "both \\\"x\\\" \\",
    "tab\there",
    "\\\\server\\share",
    "caf\u{e9} \u{1f600}",
];

#[test]
fn arguments_reach_a_program_os_run_starts_as_they_were() {
    let Some(host) = Host::find() else {
        eprintln!("skipped: this machine runs no Windows programs");
        return;
    };
    let dir = host.work_dir().join("os-run-arguments");
    std::fs::create_dir_all(&dir).unwrap();
    build(&dir, "echo", ECHO);
    let args: Vec<String> = HARD
        .iter()
        .map(|arg| {
            format!(
                "\"{}\"",
                arg.replace('\\', "\\\\")
                    .replace('"', "\\\"")
                    .replace('\t', "\\t")
            )
        })
        .collect();
    let runner = format!(
        "use os\n\nout = os.run(\".\\\\echo\", [{}]).unwrap()\nprint(out.status)\nprint(out.stdout)\n",
        args.join(", ")
    );
    let expected: String = std::iter::once("0\n".to_string())
        .chain(HARD.iter().map(|arg| format!("[{arg}]\n")))
        .chain(["\n".to_string()])
        .collect();
    let exe = build(&dir, "runner", &runner);
    let compiled = Command::new(&exe)
        .current_dir(&dir)
        .env("STONE_LEAK_CHECK", "1")
        .env("WSLENV", "STONE_LEAK_CHECK")
        .output()
        .unwrap();
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    assert_eq!(String::from_utf8_lossy(&compiled.stdout), expected);
    // the interpreter starts programs only on Windows itself
    if host == Host::Windows {
        let interpreted = Command::new(env!("CARGO_BIN_EXE_stone"))
            .arg("run")
            .arg(dir.join("runner.stn"))
            .current_dir(&dir)
            .output()
            .unwrap();
        assert_eq!(
            String::from_utf8_lossy(&interpreted.stdout),
            expected,
            "{}",
            String::from_utf8_lossy(&interpreted.stderr)
        );
    }
}

/// A program that finds addresses through the system's resolver, as `net` does on Windows.
const RESOLVE: &str = "\
use net
use os
print(net.resolve(\"localhost\"))
print(net.resolve(\"127.0.0.1\"))
print(net.resolve(os.hostname()).is_ok())
print(net.resolve(\"no-such-host.invalid\"))
";

#[test]
fn windows_programs_ask_the_system_for_addresses() {
    let Some(host) = Host::find() else {
        eprintln!("skipped: this machine runs no Windows programs");
        return;
    };
    let dir = host.work_dir().join("net-resolve");
    std::fs::create_dir_all(&dir).unwrap();
    let exe = build(&dir, "resolve", RESOLVE);
    let compiled = Command::new(&exe)
        .current_dir(&dir)
        .env("STONE_LEAK_CHECK", "1")
        .env("WSLENV", "STONE_LEAK_CHECK")
        .output()
        .unwrap();
    assert!(
        compiled.status.success(),
        "{}",
        String::from_utf8_lossy(&compiled.stderr)
    );
    assert_eq!(
        String::from_utf8_lossy(&compiled.stdout),
        "ok(['127.0.0.1', '::1'])\nok(['127.0.0.1'])\ntrue\n\
         err(\"unknown host: 'no-such-host.invalid'\")\n"
    );
    // the interpreter asks the same resolver only on Windows itself, where the machine's own
    // name gives the same addresses in the same order
    if host == Host::Windows {
        let exe = build(
            &dir,
            "hostname",
            "use net\nuse os\nprint(net.resolve(os.hostname()))\n",
        );
        let compiled = Command::new(&exe).current_dir(&dir).output().unwrap();
        let interpreted = Command::new(env!("CARGO_BIN_EXE_stone"))
            .arg("run")
            .arg(dir.join("hostname.stn"))
            .current_dir(&dir)
            .output()
            .unwrap();
        assert_eq!(
            String::from_utf8_lossy(&interpreted.stdout),
            String::from_utf8_lossy(&compiled.stdout),
            "{}",
            String::from_utf8_lossy(&interpreted.stderr)
        );
    }
}
