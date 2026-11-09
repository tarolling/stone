//! The `gui` module's Win32 windows, which `windows_gui.ps1` drives through PowerShell: natively
//! on Windows, and from Linux under WSL, which runs Windows programs, PowerShell included,
//! directly. Elsewhere the test skips.

use super::*;
use std::io::Read;
use std::process::Stdio;
use std::time::{Duration, Instant};

/// A program that opens a window titled by its argument, shows it in red, and prints its scale
/// and every event until it is asked to close.
const EVENTS: &str = "\
use gui

match gui.open(args()[0], 200, 100):
    case ok(w):
        w.show(gui.blank(200, 100, gui.rgb(255, 0, 0)))
        print(\"open at {w.scale()}\")
        while w.is_open():
            for e in w.wait():
                print(e)
                if e == gui.event.close:
                    w.close()
    case err(reason):
        print(reason)
";

/// What [`EVENTS`] prints for the input `windows_gui.ps1` gives it: a key held so it repeats
/// goes down once, a control character types nothing, the two halves of a character outside the
/// Basic Multilingual Plane type it once, a drag goes on outside the window, and two half steps
/// of the wheel make one.
const EXPECTED: &str = "\
event.key_down('f5')
event.key_up('f5')
event.key_down('left')
event.key_up('left')
event.text('\u{e9}')
event.text('\u{1f600}')
event.mouse_move(10, 20)
event.mouse_down('left', 11, 21)
event.mouse_up('left', 11, 21)
event.mouse_move(-5, 7)
event.scroll(1)
event.scroll(-1)
event.resize(300, 150)
event.close
";

/// How this machine runs Windows programs and PowerShell, if it does.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum Host {
    Windows,
    Wsl,
}

impl Host {
    pub(super) fn find() -> Option<Host> {
        if cfg!(windows) {
            return Some(Host::Windows);
        }
        let binfmt = Path::new("/proc/sys/fs/binfmt_misc");
        let interop = ["WSLInterop", "WSLInterop-late"]
            .iter()
            .any(|name| binfmt.join(name).exists());
        let powershell = Command::new("powershell.exe")
            .args(["-NoProfile", "-Command", "exit 0"])
            .current_dir("/")
            .output()
            .is_ok_and(|output| output.status.success());
        (interop && powershell).then_some(Host::Wsl)
    }

    /// Returns the directory programs work in: under WSL, one on Windows' own disk, since its
    /// share of Linux files refuses to open files now and then.
    pub(super) fn work_dir(self) -> PathBuf {
        let target = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join("windows_gui");
        if self == Host::Windows {
            return target;
        }
        let output = Command::new("cmd.exe")
            .args(["/c", "echo %TEMP%"])
            .current_dir("/")
            .output()
            .unwrap();
        let temp = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let output = Command::new("wslpath")
            .arg("-u")
            .arg(temp)
            .output()
            .unwrap();
        PathBuf::from(String::from_utf8_lossy(&output.stdout).trim()).join("stone-windows-gui")
    }

    /// Returns `path` as Windows names it.
    fn windows_path(self, path: &Path) -> String {
        if self == Host::Windows {
            return path.display().to_string();
        }
        let output = Command::new("wslpath")
            .arg("-w")
            .arg(path)
            .output()
            .unwrap();
        String::from_utf8_lossy(&output.stdout).trim().to_string()
    }
}

/// A program that opens a window titled by its argument and draws it in another color at each
/// frame until it is asked to close.
const FRAMES: &str = "\
use gui

w = gui.open(args()[0], 64, 64).unwrap()
n = 0
while w.is_open():
    for e in w.next_frame():
        if e == gui.event.close:
            w.close()
    n += 1
    w.show(gui.blank(64, 64, gui.rgb(n * 7 % 256, 255, 0)))
print(\"closed\")
";

/// Held while a test runs a program with a window, since tests run at once and a window that
/// opens takes activation from another, which ends Windows' loop of moving that one.
static WINDOWS: std::sync::Mutex<()> = std::sync::Mutex::new(());

/// Waits until no other test runs a program with a window, and returns the turn, which ends
/// when it is dropped. A test that failed with its turn leaves it to the next.
fn take_turn() -> std::sync::MutexGuard<'static, ()> {
    WINDOWS
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

/// Runs `program` (with its arguments) in `dir`, with `STONE_GUI_SCALE` set to `scale` unless
/// it is empty, while `windows_gui.ps1` drives the window titled `title` with the arguments
/// `driving`, and returns what the driver and then the program printed.
fn drive(
    host: Host,
    dir: &Path,
    program: &mut Command,
    (title, scale): (&str, &str),
    driving: &[&str],
) -> (String, String) {
    let _turn = take_turn();
    if !scale.is_empty() {
        program.env("STONE_GUI_SCALE", scale);
    }
    let mut child = program
        .current_dir(dir)
        .env("STONE_LEAK_CHECK", "1")
        .env("WSLENV", "STONE_LEAK_CHECK:STONE_GUI_SCALE")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap();
    let script = dir.join("windows_gui.ps1");
    std::fs::copy(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/cli/windows_gui.ps1"),
        &script,
    )
    .unwrap();
    let powershell = match host {
        Host::Windows => "powershell",
        Host::Wsl => "powershell.exe",
    };
    let driver = Command::new(powershell)
        .args(["-NoProfile", "-ExecutionPolicy", "Bypass", "-File"])
        .arg(host.windows_path(&script))
        .args(["-Title", title])
        .args(driving)
        .current_dir(dir)
        .output()
        .unwrap();
    let deadline = Instant::now() + Duration::from_secs(30);
    while child.try_wait().unwrap().is_none() {
        if Instant::now() > deadline {
            child.kill().unwrap();
            break;
        }
        std::thread::sleep(Duration::from_millis(50));
    }
    let mut stdout = String::new();
    child
        .stdout
        .take()
        .unwrap()
        .read_to_string(&mut stdout)
        .unwrap();
    let mut stderr = String::new();
    child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();
    let status = child.wait().unwrap();
    assert!(status.success(), "{status}: {stderr}");
    let driven = String::from_utf8_lossy(&driver.stdout).replace("\r\n", "\n");
    (driven, stdout)
}

/// Builds the stone program `source` for Windows as `name.exe` in `dir`, returning its path.
pub(super) fn build(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.stn"));
    std::fs::write(&path, source).unwrap();
    let exe = dir.join(format!("{name}.exe"));
    let built = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&path)
        .args(["--target", "x86_64-windows", "-o"])
        .arg(&exe)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    exe
}

/// Checks what [`EVENTS`] prints, as `program` runs it, at the scale `scale` (or the system's if
/// empty): the scale the driver found, and the same events in logical pixels at any scale.
fn check_events(host: Host, dir: &Path, program: &mut Command, title: &str, scale: &str) {
    let color = 0xff0000.to_string();
    let scale_arg = if scale.is_empty() { "0" } else { scale };
    let driving = [
        "-Color", &color, "-Scale", scale_arg, "-Width", "200", "-Height", "100",
    ];
    let (driven, printed) = drive(host, dir, program, (title, scale), &driving);
    let found = driven
        .lines()
        .next()
        .and_then(|line| line.strip_prefix("scale "))
        .and_then(|scale| scale.parse::<f64>().ok())
        .unwrap_or_else(|| panic!("{driven}"));
    assert_eq!(
        driven,
        format!("scale {}\nok\n", driven[6..].lines().next().unwrap())
    );
    assert_eq!(
        printed.replace("\r\n", "\n"),
        format!("open at {found:?}\n{EXPECTED}")
    );
}

#[test]
fn win32_windows_report_input_in_order_under_both_backends() {
    let Some(host) = Host::find() else {
        eprintln!("skipping: this machine runs no Windows programs");
        return;
    };
    let dir = host.work_dir();
    std::fs::create_dir_all(&dir).unwrap();
    let exe = build(&dir, "events", EVENTS);
    let id = std::process::id();
    for scale in ["", "1.5"] {
        let title = format!("stone gui test {id} {scale}");
        check_events(host, &dir, Command::new(&exe).arg(&title), &title, scale);
        // the interpreter runs on Windows only there
        if host == Host::Windows {
            let title = format!("stone gui run {id} {scale}");
            let mut run = Command::new(env!("CARGO_BIN_EXE_stone"));
            run.arg("run")
                .arg(dir.join("events.stn"))
                .arg("--")
                .arg(&title);
            check_events(host, &dir, &mut run, &title, scale);
        }
    }
}

#[test]
fn win32_windows_go_on_drawing_while_moved() {
    let Some(host) = Host::find() else {
        eprintln!("skipping: this machine runs no Windows programs");
        return;
    };
    let dir = host.work_dir().join("frames");
    std::fs::create_dir_all(&dir).unwrap();
    let exe = build(&dir, "frames", FRAMES);
    let title = format!("stone gui frames {}", std::process::id());
    let (driven, printed) = drive(
        host,
        &dir,
        Command::new(&exe).arg(&title),
        (&title, ""),
        &["-Mode", "move"],
    );
    assert!(
        driven.ends_with("\nok\n") && driven.lines().count() == 2,
        "{driven}"
    );
    assert_eq!(printed.replace("\r\n", "\n"), "closed\n");
    if host == Host::Windows {
        let title = format!("stone gui frames run {}", std::process::id());
        let mut run = Command::new(env!("CARGO_BIN_EXE_stone"));
        run.arg("run")
            .arg(dir.join("frames.stn"))
            .arg("--")
            .arg(&title);
        let (driven, printed) = drive(host, &dir, &mut run, (&title, ""), &["-Mode", "move"]);
        assert!(
            driven.ends_with("\nok\n") && driven.lines().count() == 2,
            "{driven}"
        );
        assert_eq!(printed.replace("\r\n", "\n"), "closed\n");
    }
}

#[test]
fn programs_with_windows_run_without_a_console() {
    let Some(host) = Host::find() else {
        eprintln!("skipping: this machine runs no Windows programs");
        return;
    };
    let dir = host.work_dir().join("no_console");
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    let source = dir.join("quiet.stn");
    let program = "use fs\n\nprint(\"out\")\neprint(\"err\")\nprint(input())\n\
                   fs.write(\"ran.txt\", \"yes\").unwrap()\n";
    std::fs::write(&source, program).unwrap();
    let exe = dir.join("quiet.exe");
    let built = Command::new(env!("CARGO_BIN_EXE_stone"))
        .arg("build")
        .arg(&source)
        .args(["--target", "x86_64-windows", "--windows-gui", "-o"])
        .arg(&exe)
        .output()
        .unwrap();
    assert!(
        built.status.success(),
        "{}",
        String::from_utf8_lossy(&built.stderr)
    );
    // Start-Process goes through ShellExecute, which gives a program with windows no console
    // and no standard handles, as Explorer does
    let powershell = match host {
        Host::Windows => "powershell",
        Host::Wsl => "powershell.exe",
    };
    let command = format!(
        "exit (Start-Process -FilePath '{}' -WorkingDirectory '{}' -Wait -PassThru).ExitCode",
        host.windows_path(&exe),
        host.windows_path(&dir)
    );
    let turn = take_turn();
    let ran = Command::new(powershell)
        .args(["-NoProfile", "-Command", &command])
        .current_dir(&dir)
        .output()
        .unwrap();
    drop(turn);
    assert!(ran.status.success(), "{ran:?}");
    assert_eq!(std::fs::read_to_string(dir.join("ran.txt")).unwrap(), "yes");
}
