//! The x86-64 Windows runtime of `gui_sys`'s Win32 windows, which `stdlib::gui`'s Windows half
//! does through Rust for the interpreter: the same calls of `user32.dll`, `gdi32.dll`, and
//! `dwmapi.dll`, and the same records of messages, so `gui_win32.stn` sees the same bytes.
//!
//! Each window lives on a thread of its own, which creates it and handles its messages, so the
//! loop Windows runs while a window is moved or resized holds up only that thread and the program
//! goes on drawing. The two threads share the window's handle (see `net`'s for the layout of
//! one), whose tag is [`WINDOW_TAG`], with no counted slots, and these slots:
//!
//! - `[p + 16]`: the window, or 0 until it is open and once it is closed
//! - `[p + 24]`, `[p + 32]`, and `[p + 40]`: the block from the process heap that the window's
//!   procedure records messages in, or 0, how many bytes of it are recorded, and its size
//! - `[p + 48]`: the block from the process heap holding the last image shown, or 0, which the
//!   procedure paints, and `[p + 56]`, `[p + 64]`, and `[p + 72]`: its width, height, and the
//!   block's size
//! - `[p + 80]`: the `SRWLOCK` that guards the records and the image
//! - `[p + 88]`: an event, set while records wait to be taken, once the window is open
//! - `[p + 96]`: the window's thread
//! - `[p + 104]`: the DPI the window is drawn at, which only its thread changes
//! - `[p + 112]`: the DPI `STONE_GUI_SCALE` fixes, or 0 for the monitor's
//! - `[p + 120]`: the title in UTF-16, from `stone.mem_alloc`, while the thread opens the window
//! - `[p + 128]` and `[p + 136]`: the width and height asked for, in logical pixels
//! - `[p + 144]`: why opening the window failed, a Win32 error code
//!
//! The window's thread calls nothing of the runtime's, whose allocator is not safe to call from
//! two threads: its blocks come from the process heap. The window's `GWLP_USERDATA` points to
//! the handle, so its procedure, `stone.win_proc`, finds it. The routines the program calls
//! follow the System V convention, as the rest of the runtime does; the thread, the procedure,
//! and the helpers they share follow Microsoft's. Every Windows call is made from a 16-byte
//! aligned stack with its 32 bytes of shadow space.

use crate::codegen::AssemblyGenerator;

/// The tag of a window, after those of a socket and a listener.
pub const WINDOW_TAG: i64 = -3;

/// The slots of a window after its tag and kinds.
const WINDOW_SLOTS: i64 = 17;

/// UTF-8's code page, which `MultiByteToWideChar` takes.
const CP_UTF8: i64 = 65001;

/// `WS_OVERLAPPEDWINDOW`: a title bar, a frame to resize by, and the buttons to minimize,
/// maximize, and close.
const OVERLAPPED_WINDOW: i64 = 0x00cf_0000;

/// `GWLP_USERDATA`, the index of the pointer a window keeps for its program.
const GWLP_USERDATA: i64 = -21;

/// The message the program's thread posts to have a window's own thread close it, the first of
/// `WM_APP`'s, as `stdlib::gui`'s `WM_CLOSE_WINDOW`.
const WM_CLOSE_WINDOW: i64 = 0x8000;

/// The messages the procedure records with 0 after them, returning 0: `WM_CLOSE` and `WM_CHAR`.
const RECORDED: [i64; 2] = [0x10, 0x102];

/// The messages the procedure records with the window's DPI after them, returning 0: `WM_SIZE`,
/// `WM_MOUSEMOVE`, and `WM_MOUSEWHEEL`.
const SIZED: [i64; 3] = [0x05, 0x200, 0x20a];

/// The key messages, which the procedure records with the character the key types and leaves
/// to `DefWindowProcW` too: `WM_KEYDOWN`, `WM_KEYUP`, `WM_SYSKEYDOWN`, and `WM_SYSKEYUP`.
const KEYS: [i64; 4] = [0x100, 0x101, 0x104, 0x105];

/// The messages of a mouse button going down, for the left, right, and middle buttons, each
/// followed by the one of it coming up.
const BUTTONS_DOWN: [i64; 3] = [0x201, 0x204, 0x207];

/// The flags of `SetWindowPos` that keep a window where it is in the stack of windows and leave
/// it inactive, plus `SWP_NOMOVE` to keep where it is on the screen too.
const SWP_NOZORDER_NOACTIVATE: i64 = 0x14;
const SWP_NOMOVE: i64 = 0x2;

/// The size of each window thread's stack that is committed at first, which a window's messages
/// stay far within.
const THREAD_STACK: i64 = 0x40000;

/// Emits the routines `used` lists, each doing what the function or method of
/// `stdlib::gui::NativeWindow` with the same name does:
///
/// - `stone.win_open` opens a window titled `rdi` whose inside is `rsi` by `rdx` logical pixels,
///   at `rcx` DPI or the monitor's for 0, and returns a result of it
/// - `stone.win_events` waits up to `rsi` milliseconds (forever if negative) for a message to
///   the window `rdi` if none was recorded, and returns the records as bytes
/// - `stone.win_frame` waits for the desktop to put its windows on the screen, and returns
///   whether it could
/// - `stone.win_show` keeps the image `rsi` by `rdx` whose pixels are the bytes `rcx`, and paints
///   it in the window `rdi`
/// - `stone.win_set_title` sets the title of `rdi` to `rsi`
/// - `stone.win_close` closes the window `rdi`
///
/// They need the fs routines' `.Lfs_ok` and `.Lfs_err`, the shims' `.Lwin_map` and
/// `.Lwin_errno`, the string and enum runtimes, and the allocator. The window's thread, its
/// procedure, and the class it is registered by come along.
pub fn window_runtime(r#gen: &mut dyn AssemblyGenerator, used: &[&str]) {
    let uses = |label: &str| used.contains(&label);
    r#gen.emit("\t.bss");
    r#gen.emit("\t.p2align\t3");
    // whether the class is registered, which the first window does
    r#gen.emit("stone.win_class:");
    r#gen.emit("\t.zero\t8");
    r#gen.emit("\t.data");
    r#gen.emit("\t.p2align\t3");
    // the class's name, `stone` in UTF-16
    r#gen.emit("stone.win_class_name:");
    r#gen.emit("\t.quad\t0x006e006f00740073");
    r#gen.emit("\t.quad\t0x0000000000000065");
    r#gen.emit("\t.text");
    thread(r#gen);
    procedure(r#gen);
    record(r#gen);
    paint(r#gen);
    wide(r#gen);
    if uses("stone.win_open") {
        open(r#gen);
    }
    if uses("stone.win_events") {
        events(r#gen);
    }
    if uses("stone.win_frame") {
        r#gen.emit("stone.win_frame:");
        enter(r#gen, &[]);
        r#gen.emit("\tsub\trsp, 32");
        r#gen.emit("\tcall\tDwmFlush");
        r#gen.emit("\ttest\teax, eax");
        r#gen.emit("\tsete\tal");
        r#gen.emit("\tmovzx\teax, al");
        leave(r#gen, &[]);
    }
    if uses("stone.win_show") {
        show(r#gen);
    }
    if uses("stone.win_set_title") {
        set_title(r#gen);
    }
    if uses("stone.win_close") {
        r#gen.emit("stone.win_close:");
        enter(r#gen, &["rbx"]);
        r#gen.emit("\tsub\trsp, 32");
        r#gen.emit("\tmov\trbx, rdi");
        destroy(r#gen, "rbx", ".Lwin_close");
        leave(r#gen, &["rbx"]);
    }
}

/// Emits the start of a routine's frame: `rbp` and then each of `saved` pushed, and the stack
/// aligned to 16 bytes, for [`leave`] to end.
fn enter(r#gen: &mut dyn AssemblyGenerator, saved: &[&str]) {
    r#gen.emit("\tpush\trbp");
    r#gen.emit("\tmov\trbp, rsp");
    for reg in saved {
        r#gen.emit(&format!("\tpush\t{reg}"));
    }
    r#gen.emit("\tand\trsp, -16");
}

/// Emits the end of a frame [`enter`] began with the same `saved` registers, and a return.
fn leave(r#gen: &mut dyn AssemblyGenerator, saved: &[&str]) {
    r#gen.emit(&format!("\tlea\trsp, [rbp - {}]", 8 * saved.len()));
    for reg in saved.iter().rev() {
        r#gen.emit(&format!("\tpop\t{reg}"));
    }
    r#gen.emit("\tpop\trbp");
    r#gen.emit("\tret");
}

/// Emits code that takes (`AcquireSRWLockExclusive`) or gives back (`ReleaseSRWLockExclusive`)
/// the lock of the handle in `reg`, changing what a Windows call changes.
fn lock(r#gen: &mut dyn AssemblyGenerator, reg: &str, function: &str) {
    r#gen.emit(&format!("\tlea\trcx, [{reg} + 80]"));
    r#gen.emit(&format!("\tcall\t{function}"));
}

/// Emits code that closes the window of the handle in `reg`, a register a Windows call keeps,
/// unless it is closed already: it asks the window's thread to, which destroys the window and
/// ends, and waits for the thread to end. It changes what a Windows call changes.
fn destroy(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [{reg} + 16]"));
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjz\t{label}_closed"));
    r#gen.emit(&format!("\tmov\tedx, {WM_CLOSE_WINDOW}"));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tcall\tPostMessageW");
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [{reg} + 96]"));
    r#gen.emit("\tmov\tedx, -1"); // INFINITE
    r#gen.emit("\tcall\tWaitForSingleObject");
    r#gen.emit(&format!("\tmov\tQWORD PTR [{reg} + 16], 0"));
    r#gen.emit(&format!("{label}_closed:"));
}

/// Emits `.Lwin_thread`, the thread of a window, which Windows starts with the handle in `rcx`
/// and the Microsoft convention. It creates the window, sizes its inside to the logical size
/// asked for at the window's DPI, shows it, records its size, says it is open (or why it is not)
/// by setting the handle's event, and then handles the window's messages until it is closed.
fn thread(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "rsi", "rdi", "r12", "r13", "r14"];
    // rbx holds the handle, r12 the window, r13 the monitor's DPI, and r14 the window's; a MSG
    // goes at [rsp + 48] and a RECT at [rsp + 96]
    r#gen.emit(".Lwin_thread:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 112");
    r#gen.emit("\tmov\trbx, rcx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tGetModuleHandleW");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 80], rax");
    // CW_USEDEFAULT for where it goes and how big it is, until its DPI is known
    r#gen.emit("\tmov\teax, 2147483648");
    for offset in [32, 40, 48, 56] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], rax"));
    }
    r#gen.emit("\tmov\tQWORD PTR [rsp + 64], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 88], rbx");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tlea\trdx, [rip + stone.win_class_name]");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 120]");
    r#gen.emit(&format!("\tmov\tr9d, {OVERLAPPED_WINDOW}"));
    r#gen.emit("\tcall\tCreateWindowExW");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_thread_failed");
    // the frame is drawn at the monitor's DPI, and the inside at the window's
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tcall\tGetDpiForWindow");
    r#gen.emit("\tmov\tr13d, eax");
    r#gen.emit("\tmov\tr14, QWORD PTR [rbx + 112]");
    r#gen.emit("\ttest\tr14, r14");
    r#gen.emit("\tcmovz\tr14, r13");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 104], r14");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 96], 0");
    for (asked, offset) in [(128, 104), (136, 108)] {
        // the physical pixels, to the nearest
        r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbx + {asked}]"));
        r#gen.emit("\timul\trax, r14");
        r#gen.emit("\tadd\trax, 48");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tmov\tecx, 96");
        r#gen.emit("\tdiv\trcx");
        r#gen.emit(&format!("\tmov\tDWORD PTR [rsp + {offset}], eax"));
    }
    r#gen.emit("\tlea\trcx, [rsp + 96]");
    r#gen.emit(&format!("\tmov\tedx, {OVERLAPPED_WINDOW}"));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r13");
    r#gen.emit("\tcall\tAdjustWindowRectExForDpi");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 104]");
    r#gen.emit("\tsub\teax, DWORD PTR [rsp + 96]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 108]");
    r#gen.emit("\tsub\teax, DWORD PTR [rsp + 100]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], rax");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rsp + 48], {}",
        SWP_NOZORDER_NOACTIVATE | SWP_NOMOVE
    ));
    r#gen.emit("\tcall\tSetWindowPos");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tmov\tedx, 5"); // SW_SHOW
    r#gen.emit("\tcall\tShowWindow");
    // the size it has, with its DPI, for gui_win32 to start from
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tlea\trdx, [rsp + 96]");
    r#gen.emit("\tcall\tGetClientRect");
    r#gen.emit("\tmovzx\tr9d, WORD PTR [rsp + 108]");
    r#gen.emit("\tshl\tr9d, 16");
    r#gen.emit("\tmovzx\teax, WORD PTR [rsp + 104]");
    r#gen.emit("\tor\tr9d, eax");
    r#gen.emit("\tmov\trcx, rbx");
    r#gen.emit("\tmov\tedx, 5"); // WM_SIZE
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r14");
    r#gen.emit("\tcall\t.Lwin_record");
    // open: from now on a record sets the event too
    lock(r#gen, "rbx", "AcquireSRWLockExclusive");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 16], r12");
    lock(r#gen, "rbx", "ReleaseSRWLockExclusive");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 88]");
    r#gen.emit("\tcall\tSetEvent");
    r#gen.emit(".Lwin_thread_next:");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tcall\tGetMessageW");
    // 0 once the window is destroyed, and -1 for an error
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjle\t.Lwin_thread_done");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\tcall\tTranslateMessage");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\tcall\tDispatchMessageW");
    r#gen.emit("\tjmp\t.Lwin_thread_next");
    r#gen.emit(".Lwin_thread_failed:");
    r#gen.emit("\tcall\tGetLastError");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 144], rax");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 88]");
    r#gen.emit("\tcall\tSetEvent");
    r#gen.emit(".Lwin_thread_done:");
    r#gen.emit("\txor\teax, eax");
    leave(r#gen, &saved);
}

/// Emits `stone.win_proc`, the procedure of every window, which Windows calls on the window's
/// thread with the Microsoft convention: the window in `rcx`, the message in `edx`, `wParam` in
/// `r8`, and `lParam` in `r9`. It records the messages `gui_win32.stn` reads, paints the last
/// image shown, follows the window to a monitor of another DPI, closes the window when the
/// program asks, and leaves the rest to `DefWindowProcW`, keeping every register that
/// convention keeps.
///
/// `WM_NCCREATE`, the first message, brings the handle in the `CREATESTRUCTW` that `lParam`
/// points to, whose first field is `CreateWindowExW`'s last argument.
fn procedure(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "rsi", "rdi", "r12", "r13", "r14"];
    // r12 holds the window, r13 the message, r14 wParam, rbx lParam, and rsi the handle; a
    // PAINTSTRUCT goes at [rsp + 48]
    r#gen.emit("stone.win_proc:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 128");
    r#gen.emit("\tmov\tr12, rcx");
    r#gen.emit("\tmov\tr13d, edx");
    r#gen.emit("\tmov\tr14, r8");
    r#gen.emit("\tmov\trbx, r9");
    r#gen.emit("\tcmp\tr13d, 129"); // WM_NCCREATE
    r#gen.emit("\tjne\t.Lwin_proc_handle");
    r#gen.emit(&format!("\tmov\tedx, {GWLP_USERDATA}"));
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx]");
    r#gen.emit("\tcall\tSetWindowLongPtrW");
    r#gen.emit("\tjmp\t.Lwin_proc_default");
    r#gen.emit(".Lwin_proc_handle:");
    r#gen.emit(&format!("\tmov\tedx, {GWLP_USERDATA}"));
    r#gen.emit("\tcall\tGetWindowLongPtrW");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_proc_default");
    r#gen.emit("\tmov\trsi, rax");
    for message in RECORDED {
        r#gen.emit(&format!("\tcmp\tr13d, {message}"));
        r#gen.emit("\tje\t.Lwin_proc_record");
    }
    for message in SIZED {
        r#gen.emit(&format!("\tcmp\tr13d, {message}"));
        r#gen.emit("\tje\t.Lwin_proc_sized");
    }
    for message in KEYS {
        r#gen.emit(&format!("\tcmp\tr13d, {message}"));
        r#gen.emit("\tje\t.Lwin_proc_key");
    }
    for message in BUTTONS_DOWN {
        r#gen.emit(&format!("\tcmp\tr13d, {message}"));
        r#gen.emit("\tje\t.Lwin_proc_down");
        r#gen.emit(&format!("\tcmp\tr13d, {}", message + 1));
        r#gen.emit("\tje\t.Lwin_proc_up");
    }
    r#gen.emit(&format!("\tcmp\tr13d, {WM_CLOSE_WINDOW}"));
    r#gen.emit("\tje\t.Lwin_proc_close");
    r#gen.emit("\tcmp\tr13d, 736"); // WM_DPICHANGED
    r#gen.emit("\tje\t.Lwin_proc_dpi");
    // the procedure paints all of the inside, so nothing needs erasing first
    r#gen.emit("\tmov\teax, 1");
    r#gen.emit("\tcmp\tr13d, 20"); // WM_ERASEBKGND
    r#gen.emit("\tje\t.Lwin_proc_done");
    r#gen.emit("\tcmp\tr13d, 15"); // WM_PAINT
    r#gen.emit("\tjne\t.Lwin_proc_default");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tlea\trdx, [rsp + 48]");
    r#gen.emit("\tcall\tBeginPaint");
    r#gen.emit("\tmov\trdi, rax");
    lock(r#gen, "rsi", "AcquireSRWLockExclusive");
    r#gen.emit("\tmov\trcx, rdi");
    r#gen.emit("\tmov\trdx, rsi");
    r#gen.emit("\tmov\tr8, r12");
    r#gen.emit("\tcall\t.Lwin_paint");
    lock(r#gen, "rsi", "ReleaseSRWLockExclusive");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tlea\trdx, [rsp + 48]");
    r#gen.emit("\tcall\tEndPaint");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    // the program asked to close the window, which this thread made, so it destroys it here
    r#gen.emit(".Lwin_proc_close:");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit(&format!("\tmov\tedx, {GWLP_USERDATA}"));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tcall\tSetWindowLongPtrW");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tcall\tDestroyWindow");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tPostQuitMessage");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    // on a monitor of another DPI, the window keeps its logical size at the new DPI, which
    // lParam's RECT has room for, unless its scale is fixed
    r#gen.emit(".Lwin_proc_dpi:");
    r#gen.emit("\tcmp\tQWORD PTR [rsi + 112], 0");
    r#gen.emit("\tjne\t.Lwin_proc_zero");
    r#gen.emit("\tmovzx\teax, r14w");
    r#gen.emit("\tmov\tQWORD PTR [rsi + 104], rax");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8d, DWORD PTR [rbx]");
    r#gen.emit("\tmov\tr9d, DWORD PTR [rbx + 4]");
    r#gen.emit("\tmov\teax, DWORD PTR [rbx + 8]");
    r#gen.emit("\tsub\teax, DWORD PTR [rbx]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\teax, DWORD PTR [rbx + 12]");
    r#gen.emit("\tsub\teax, DWORD PTR [rbx + 4]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], rax");
    r#gen.emit(&format!(
        "\tmov\tQWORD PTR [rsp + 48], {SWP_NOZORDER_NOACTIVATE}"
    ));
    r#gen.emit("\tcall\tSetWindowPos");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    r#gen.emit(".Lwin_proc_record:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tcall\t.Lwin_proc_append");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");
    r#gen.emit(".Lwin_proc_sized:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 104]");
    r#gen.emit("\tcall\t.Lwin_proc_append");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    // a key also gives the character it types, and goes on to DefWindowProcW, which turns
    // Alt+F4 into WM_CLOSE
    r#gen.emit(".Lwin_proc_key:");
    r#gen.emit("\tmov\tecx, r14d");
    r#gen.emit("\tmov\tedx, 2"); // MAPVK_VK_TO_CHAR
    r#gen.emit("\tcall\tMapVirtualKeyW");
    r#gen.emit("\tcall\t.Lwin_proc_append");
    r#gen.emit("\tjmp\t.Lwin_proc_default");

    // the window takes the mouse while a button is held, so a drag goes on outside it
    r#gen.emit(".Lwin_proc_down:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 104]");
    r#gen.emit("\tcall\t.Lwin_proc_append");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tcall\tSetCapture");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");
    r#gen.emit(".Lwin_proc_up:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 104]");
    r#gen.emit("\tcall\t.Lwin_proc_append");
    r#gen.emit("\ttest\tr14, 19"); // MK_LBUTTON, MK_RBUTTON, and MK_MBUTTON
    r#gen.emit("\tjnz\t.Lwin_proc_zero");
    r#gen.emit("\tcall\tReleaseCapture");

    r#gen.emit(".Lwin_proc_zero:");
    r#gen.emit("\txor\teax, eax");
    r#gen.emit("\tjmp\t.Lwin_proc_done");
    r#gen.emit(".Lwin_proc_default:");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tmov\tedx, r13d");
    r#gen.emit("\tmov\tr8, r14");
    r#gen.emit("\tmov\tr9, rbx");
    r#gen.emit("\tcall\tDefWindowProcW");
    r#gen.emit(".Lwin_proc_done:");
    leave(r#gen, &saved);

    // records the message r13, wParam r14, lParam rbx, and eax for the handle rsi, called with
    // the stack 8 bytes off the procedure's alignment
    r#gen.emit(".Lwin_proc_append:");
    r#gen.emit("\tsub\trsp, 56");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\tmov\tedx, r13d");
    r#gen.emit("\tmov\tr8, r14");
    r#gen.emit("\tmov\tr9, rbx");
    r#gen.emit("\tcall\t.Lwin_record");
    r#gen.emit("\tadd\trsp, 56");
    r#gen.emit("\tret");
}

/// Emits `.Lwin_record`, which appends a record of the message `edx` with `wParam` `r8`,
/// `lParam` `r9`, and its fifth argument, on the stack, to the records of the handle `rcx`,
/// growing their block, and sets the handle's event if the window is open, all with the lock
/// taken. It follows the Microsoft convention. A record that finds no memory is dropped.
fn record(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "rsi", "rdi", "r12", "r13"];
    // rbx holds the handle, esi the message, rdi wParam, r12 lParam, and r13 the fourth word
    r#gen.emit(".Lwin_record:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 40]");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 48");
    r#gen.emit("\tmov\trbx, rcx");
    r#gen.emit("\tmov\tesi, edx");
    r#gen.emit("\tmov\trdi, r8");
    r#gen.emit("\tmov\tr12, r9");
    r#gen.emit("\tmov\tr13, rax");
    lock(r#gen, "rbx", "AcquireSRWLockExclusive");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 32]");
    r#gen.emit("\tadd\trax, 16");
    r#gen.emit("\tcmp\trax, QWORD PTR [rbx + 40]");
    r#gen.emit("\tjbe\t.Lwin_record_room");
    r#gen.emit("\tcall\tGetProcessHeap");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr9, QWORD PTR [rbx + 40]");
    r#gen.emit("\tadd\tr9, r9");
    r#gen.emit("\tmov\teax, 256");
    r#gen.emit("\tcmp\tr9, rax");
    r#gen.emit("\tcmovb\tr9, rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r9");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 24]");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjnz\t.Lwin_record_grow");
    r#gen.emit("\tmov\tr8, r9");
    r#gen.emit("\tcall\tHeapAlloc");
    r#gen.emit("\tjmp\t.Lwin_record_grown");
    r#gen.emit(".Lwin_record_grow:");
    r#gen.emit("\tcall\tHeapReAlloc");
    r#gen.emit(".Lwin_record_grown:");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_record_done");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 24], rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 32]");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 40], rax");
    r#gen.emit(".Lwin_record_room:");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 24]");
    r#gen.emit("\tadd\trax, QWORD PTR [rbx + 32]");
    r#gen.emit("\tmov\tDWORD PTR [rax], esi");
    r#gen.emit("\tmov\tDWORD PTR [rax + 4], edi");
    r#gen.emit("\tmov\tDWORD PTR [rax + 8], r12d");
    r#gen.emit("\tmov\tDWORD PTR [rax + 12], r13d");
    r#gen.emit("\tadd\tQWORD PTR [rbx + 32], 16");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit("\tje\t.Lwin_record_done");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 88]");
    r#gen.emit("\tcall\tSetEvent");
    r#gen.emit(".Lwin_record_done:");
    lock(r#gen, "rbx", "ReleaseSRWLockExclusive");
    leave(r#gen, &saved);
}

/// Emits `.Lwin_paint`, which paints the image the handle `rdx` keeps at the top left of the
/// device context `rcx` of the window `r8`, each canvas pixel covering the screen's pixels at
/// the window's DPI, the nearest one to each, and the rest of the inside black. A canvas as big
/// as the inside, in logical pixels, covers all of it. The caller holds the lock; it follows the
/// Microsoft convention.
fn paint(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "rsi", "rdi", "r12", "r13", "r14", "r15"];
    // rbx holds the handle, rsi the device context, rdi the window, r12 and r13 the inside's
    // width and height, and r14 and r15 how far across and down the image reaches; the
    // BITMAPINFO goes at [rsp + 104] and the inside's RECT at [rsp + 160]
    r#gen.emit(".Lwin_paint:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 176");
    r#gen.emit("\tmov\trbx, rdx");
    r#gen.emit("\tmov\trsi, rcx");
    r#gen.emit("\tmov\trdi, r8");
    r#gen.emit("\tmov\trcx, rdi");
    r#gen.emit("\tlea\trdx, [rsp + 160]");
    r#gen.emit("\tcall\tGetClientRect");
    r#gen.emit("\tmov\tr12d, DWORD PTR [rsp + 168]");
    r#gen.emit("\tmov\tr13d, DWORD PTR [rsp + 172]");
    r#gen.emit("\txor\tr14d, r14d");
    r#gen.emit("\txor\tr15d, r15d");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 48], 0");
    r#gen.emit("\tje\t.Lwin_paint_black");
    // how far the image reaches: its size times the inside's physical size over its logical
    // size, which is the physical size at the DPI, to the nearest
    for (inside, image, reach) in [("r12", 56, "r14"), ("r13", 64, "r15")] {
        r#gen.emit(&format!("\tmov\trax, {inside}"));
        r#gen.emit("\timul\trax, rax, 96");
        r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 104]");
        r#gen.emit("\tmov\trdx, rcx");
        r#gen.emit("\tshr\trdx, 1");
        r#gen.emit("\tadd\trax, rdx");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tdiv\trcx");
        r#gen.emit("\tmov\trcx, rax");
        r#gen.emit("\ttest\trcx, rcx");
        r#gen.emit("\tjz\t.Lwin_paint_black");
        r#gen.emit(&format!("\tmov\trax, QWORD PTR [rbx + {image}]"));
        r#gen.emit(&format!("\timul\trax, {inside}"));
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit("\tdiv\trcx");
        r#gen.emit(&format!("\tmov\t{reach}, rax"));
    }
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\tmov\tedx, 3"); // COLORONCOLOR, the nearest pixel
    r#gen.emit("\tcall\tSetStretchBltMode");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 104], 40");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 56]");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 108], eax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 64]");
    r#gen.emit("\tneg\teax"); // negative for rows from the top
    r#gen.emit("\tmov\tDWORD PTR [rsp + 112], eax");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 116], 2097153"); // 1 plane, 32 bits a pixel
    for offset in [120, 128, 136, 144] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], 0"));
    }
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tr9, r14");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r15");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], 0");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 56]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 56], rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 64]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 64], rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 48]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], rax");
    r#gen.emit("\tlea\trax, [rsp + 104]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 80], rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 88], 0"); // DIB_RGB_COLORS
    r#gen.emit("\tmov\tQWORD PTR [rsp + 96], 13369376"); // SRCCOPY
    r#gen.emit("\tcall\tStretchDIBits");
    // what the image leaves of the inside, right of it and then under it, is black
    r#gen.emit(".Lwin_paint_black:");
    r#gen.emit("\tcmp\tr14, r12");
    r#gen.emit("\tjge\t.Lwin_paint_right");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\tmov\tedx, r14d");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tr9d, r12d");
    r#gen.emit("\tsub\tr9d, r14d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r13");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 66"); // BLACKNESS
    r#gen.emit("\tcall\tPatBlt");
    r#gen.emit(".Lwin_paint_right:");
    r#gen.emit("\tcmp\tr15, r13");
    r#gen.emit("\tjge\t.Lwin_paint_done");
    r#gen.emit("\tcmp\tr14, r12");
    r#gen.emit("\tcmovg\tr14, r12");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8d, r15d");
    r#gen.emit("\tmov\tr9d, r14d");
    r#gen.emit("\tmov\trax, r13");
    r#gen.emit("\tsub\trax, r15");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 66");
    r#gen.emit("\tcall\tPatBlt");
    r#gen.emit(".Lwin_paint_done:");
    leave(r#gen, &saved);
}

/// Emits `.Lwin_text`, which returns in `rax` a block from `stone.mem_alloc` holding the string
/// `rcx` as null-terminated UTF-16, for the caller to free, called from a 16-byte aligned stack
/// and changing what a Windows call changes.
fn wide(r#gen: &mut dyn AssemblyGenerator) {
    // rbx holds the string and r12 the block; a string of n bytes is at most n code units
    r#gen.emit(".Lwin_text:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tpush\tr12");
    r#gen.emit("\tsub\trsp, 56");
    r#gen.emit("\tmov\trbx, rcx");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx - 16]");
    r#gen.emit("\tlea\trdi, [rdi + rdi + 2]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit(&format!("\tmov\tecx, {CP_UTF8}"));
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8, rbx");
    r#gen.emit("\tmov\tr9, -1"); // through the terminator
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], r12");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx - 16]");
    r#gen.emit("\tinc\trax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], rax");
    r#gen.emit("\tcall\tMultiByteToWideChar");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tadd\trsp, 56");
    r#gen.emit("\tpop\tr12");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits `stone.win_open`. The first window makes the process aware of each monitor's DPI and
/// registers the class every window shares. Then it starts the window's thread and waits for it
/// to say whether the window is open.
fn open(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    // rbx holds the title, r12 the width, r13 the height, r14 the handle, and r15 the DPI; the
    // class's WNDCLASSEXW goes at [rsp + 112]
    r#gen.emit("stone.win_open:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 208");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\tr15, rcx");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.win_class], 0");
    r#gen.emit("\tjne\t.Lwin_open_registered");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.win_class], 1");
    r#gen.emit("\tmov\trcx, -4"); // DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2
    r#gen.emit("\tcall\tSetProcessDpiAwarenessContext");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 112], 80"); // its size
    r#gen.emit("\tmov\tDWORD PTR [rsp + 116], 0"); // no style
    r#gen.emit("\tlea\trax, [rip + stone.win_proc]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 120], rax");
    for offset in [128, 144, 168, 184] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], 0"));
    }
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tGetModuleHandleW");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 136], rax");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tmov\tedx, 32512"); // IDC_ARROW
    r#gen.emit("\tcall\tLoadCursorW");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 152], rax");
    r#gen.emit("\tmov\tecx, 4"); // BLACK_BRUSH
    r#gen.emit("\tcall\tGetStockObject");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 160], rax");
    r#gen.emit("\tlea\trax, [rip + stone.win_class_name]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 176], rax");
    r#gen.emit("\tlea\trcx, [rsp + 112]");
    r#gen.emit("\tcall\tRegisterClassExW");
    r#gen.emit(".Lwin_open_registered:");

    // the handle, which the window's thread fills in
    r#gen.emit(&format!("\tmov\trdi, {WINDOW_TAG}"));
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(&format!("\tmov\tedx, {WINDOW_SLOTS}"));
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tr14, rax");
    for slot in 0..WINDOW_SLOTS {
        r#gen.emit(&format!("\tmov\tQWORD PTR [r14 + {}], 0", 16 + 8 * slot));
    }
    r#gen.emit("\tmov\tQWORD PTR [r14 + 112], r15");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 128], r12");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 136], r13");
    r#gen.emit("\tmov\trcx, rbx");
    r#gen.emit("\tcall\t.Lwin_text");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 120], rax");
    // a manual-reset event, not set
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tmov\tedx, 1");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tcall\tCreateEventW");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 88], rax");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_open_system");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit(&format!("\tmov\tedx, {THREAD_STACK}"));
    r#gen.emit("\tlea\tr8, [rip + .Lwin_thread]");
    r#gen.emit("\tmov\tr9, r14");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], 0");
    r#gen.emit("\tcall\tCreateThread");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 96], rax");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_open_system");
    r#gen.emit("\tmov\trcx, QWORD PTR [r14 + 88]");
    r#gen.emit("\tmov\tedx, -1"); // INFINITE
    r#gen.emit("\tcall\tWaitForSingleObject");
    r#gen.emit("\tmov\trax, QWORD PTR [r14 + 120]");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 120], 0");
    r#gen.emit("\tcmp\tQWORD PTR [r14 + 16], 0");
    r#gen.emit("\tje\t.Lwin_open_refused");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\tesi, 3"); // an enum value's slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit("\tjmp\t.Lwin_open_done");
    // the window's thread could not open it, and ends, which is waited for so nothing of it
    // reaches the handle once freed
    r#gen.emit(".Lwin_open_refused:");
    r#gen.emit("\tmov\trcx, QWORD PTR [r14 + 96]");
    r#gen.emit("\tmov\tedx, -1");
    r#gen.emit("\tcall\tWaitForSingleObject");
    r#gen.emit("\tmov\tecx, DWORD PTR [r14 + 144]");
    r#gen.emit("\tcall\t.Lwin_map");
    r#gen.emit("\tjmp\t.Lwin_open_failed");
    r#gen.emit(".Lwin_open_system:");
    r#gen.emit("\tcall\t.Lwin_errno");
    // the error, then the handle, which has no window, freed
    r#gen.emit(".Lwin_open_failed:");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit("\tdec\tQWORD PTR [rax - 8]");
    r#gen.emit("\tcall\tstone.free_enum");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\txor\tesi, esi"); // the reason alone
    r#gen.emit("\tcall\t.Lfs_err");
    r#gen.emit(".Lwin_open_done:");
    leave(r#gen, &saved);
}

/// Emits `stone.win_events`. A wait only waits when no record is waiting, and the event, which
/// the window's thread sets with each record, is reset as the records are taken, both with the
/// lock taken, so a record never goes unnoticed.
fn events(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12"];
    // rbx holds the handle, and r12 the timeout and then the bytes
    r#gen.emit("stone.win_events:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    lock(r#gen, "rbx", "AcquireSRWLockExclusive");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit("\tje\t.Lwin_events_take");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 0");
    r#gen.emit("\tjne\t.Lwin_events_take");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Lwin_events_take");
    lock(r#gen, "rbx", "ReleaseSRWLockExclusive");
    // INFINITE for a negative timeout, and at most one millisecond less otherwise
    r#gen.emit("\tmov\tedx, -1");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjs\t.Lwin_events_wait");
    r#gen.emit("\tmov\teax, 4294967294");
    r#gen.emit("\tcmp\tr12, rax");
    r#gen.emit("\tcmova\tr12, rax");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit(".Lwin_events_wait:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 88]");
    r#gen.emit("\tcall\tWaitForSingleObject");
    lock(r#gen, "rbx", "AcquireSRWLockExclusive");
    // the records become bytes, and the block is empty again
    r#gen.emit(".Lwin_events_take:");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 32], 0");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 88]");
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit("\tjz\t.Lwin_events_reset");
    r#gen.emit("\tcall\tResetEvent");
    r#gen.emit(".Lwin_events_reset:");
    lock(r#gen, "rbx", "ReleaseSRWLockExclusive");
    r#gen.emit("\tmov\trax, r12");
    leave(r#gen, &saved);
}

/// Emits `stone.win_show`. An image with fewer bytes than its size needs is not shown, nor one
/// in a closed window. The image is kept and painted with the lock taken, so the window's thread
/// never paints half of it.
fn show(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    // rbx holds the handle, r12 the width, r13 the height, r14 the pixels and then the device
    // context, and r15 the bytes the image takes
    r#gen.emit("stone.win_show:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 48");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tmov\tr14, rcx");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit("\tje\t.Lwin_show_done");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjle\t.Lwin_show_done");
    r#gen.emit("\ttest\tr13, r13");
    r#gen.emit("\tjle\t.Lwin_show_done");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\timul\trax, r13");
    r#gen.emit("\tjo\t.Lwin_show_done");
    r#gen.emit("\tadd\trax, rax");
    r#gen.emit("\tjo\t.Lwin_show_done");
    r#gen.emit("\tadd\trax, rax");
    r#gen.emit("\tjo\t.Lwin_show_done");
    r#gen.emit("\tcmp\trax, QWORD PTR [r14 - 16]");
    r#gen.emit("\tjg\t.Lwin_show_done");
    r#gen.emit("\tmov\tr15, rax");
    lock(r#gen, "rbx", "AcquireSRWLockExclusive");
    // a bigger image needs a new block, since nothing of the old one is kept
    r#gen.emit("\tcmp\tr15, QWORD PTR [rbx + 72]");
    r#gen.emit("\tjbe\t.Lwin_show_room");
    r#gen.emit("\tcall\tGetProcessHeap");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 48]");
    r#gen.emit("\ttest\tr8, r8");
    r#gen.emit("\tjz\t.Lwin_show_new");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tcall\tHeapFree");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 48], 0");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 72], 0");
    r#gen.emit(".Lwin_show_new:");
    r#gen.emit("\tmov\trcx, QWORD PTR [rsp + 32]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\tmov\tr8, r15");
    r#gen.emit("\tcall\tHeapAlloc");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_show_unlock");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 48], rax");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 72], r15");
    r#gen.emit(".Lwin_show_room:");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 56], r12");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 64], r13");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 48]");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit("\tmov\trcx, r15");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tGetDC");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\tmov\trdx, rbx");
    r#gen.emit("\tmov\tr8, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\t.Lwin_paint");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trdx, r14");
    r#gen.emit("\tcall\tReleaseDC");
    r#gen.emit(".Lwin_show_unlock:");
    lock(r#gen, "rbx", "ReleaseSRWLockExclusive");
    r#gen.emit(".Lwin_show_done:");
    leave(r#gen, &saved);
}

/// Emits `stone.win_set_title`.
fn set_title(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12"];
    // rbx holds the handle and r12 the title in UTF-16
    r#gen.emit("stone.win_set_title:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 32");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit("\tje\t.Lwin_set_title_done");
    r#gen.emit("\tmov\trcx, rsi");
    r#gen.emit("\tcall\t.Lwin_text");
    r#gen.emit("\tmov\tr12, rax");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trdx, r12");
    r#gen.emit("\tcall\tSetWindowTextW");
    r#gen.emit("\tmov\trax, r12");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lwin_set_title_done:");
    leave(r#gen, &saved);
}

/// Emits `.Lfree_handle` for Windows, where the only handles are windows: `stone.free_enum`
/// calls it for a window in `rax` whose count reached 0, and it closes the window unless closed
/// already, which ends its thread, then closes the thread and the event and frees the blocks,
/// preserving every register except `rcx` and `rdx`, as the free routines must, `xmm3` to `xmm5`
/// included, which a Windows call may change.
pub fn free_handle(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rax", "rdi", "r8", "r9", "r10", "r11"];
    r#gen.emit(".Lfree_handle:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 64");
    for xmm in 3..6 {
        r#gen.emit(&format!(
            "\tmovq\tQWORD PTR [rsp + {}], xmm{xmm}",
            32 + 8 * (xmm - 3)
        ));
    }
    r#gen.emit("\tmov\trdi, rax");
    destroy(r#gen, "rdi", ".Lfree_handle");
    for slot in [96, 88] {
        r#gen.emit(&format!("\tmov\trcx, QWORD PTR [rdi + {slot}]"));
        r#gen.emit("\ttest\trcx, rcx");
        r#gen.emit(&format!("\tjz\t.Lfree_handle_{slot}"));
        r#gen.emit("\tcall\tCloseHandle");
        r#gen.emit(&format!(".Lfree_handle_{slot}:"));
    }
    for slot in [24, 48] {
        r#gen.emit(&format!("\tmov\tr8, QWORD PTR [rdi + {slot}]"));
        r#gen.emit("\ttest\tr8, r8");
        r#gen.emit(&format!("\tjz\t.Lfree_handle_{slot}"));
        r#gen.emit("\tcall\tGetProcessHeap");
        r#gen.emit("\tmov\trcx, rax");
        r#gen.emit("\txor\tedx, edx");
        r#gen.emit(&format!("\tmov\tr8, QWORD PTR [rdi + {slot}]"));
        r#gen.emit("\tcall\tHeapFree");
        r#gen.emit(&format!(".Lfree_handle_{slot}:"));
    }
    // the title, if opening the window failed before it was freed
    r#gen.emit("\tmov\trax, QWORD PTR [rdi + 120]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lfree_handle_title");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lfree_handle_title:");
    for xmm in 3..6 {
        r#gen.emit(&format!(
            "\tmovq\txmm{xmm}, QWORD PTR [rsp + {}]",
            32 + 8 * (xmm - 3)
        ));
    }
    leave(r#gen, &saved);
}
