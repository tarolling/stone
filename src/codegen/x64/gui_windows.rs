//! The x86-64 Windows runtime of `gui_sys`'s Win32 windows, which `stdlib::gui`'s Windows half
//! does through Rust for the interpreter: the same calls of `user32.dll`, `gdi32.dll`, and
//! `dwmapi.dll`, and the same records of messages, so `gui_win32.stn` sees the same bytes.
//!
//! A window is a handle (see `net`'s for the layout of one) whose tag is [`WINDOW_TAG`], with no
//! counted slots, and these slots:
//!
//! - `[p + 16]`: the window, or 0 once closed
//! - `[p + 24]`, `[p + 32]`, and `[p + 40]`: the block from `stone.mem_alloc` that the window's
//!   procedure records messages in, or 0, how many bytes of it are recorded, and its size
//! - `[p + 48]`: the block holding the last image shown, or 0, which the procedure paints, and
//!   `[p + 56]`, `[p + 64]`, and `[p + 72]`: its width, height, and the block's size
//!
//! The window's `GWLP_USERDATA` points to the handle, so its procedure, `stone.win_proc`, finds
//! it. Each routine follows the System V convention, as the rest of the runtime does, and makes
//! its Windows calls from a 16-byte aligned stack with their 32 bytes of shadow space.

use crate::codegen::AssemblyGenerator;

/// The tag of a window, after those of a socket and a listener.
pub const WINDOW_TAG: i64 = -3;

/// The slots of a window after its tag and kinds.
const WINDOW_SLOTS: i64 = 8;

/// UTF-8's code page, which `MultiByteToWideChar` takes.
const CP_UTF8: i64 = 65001;

/// `WS_OVERLAPPEDWINDOW`: a title bar, a frame to resize by, and the buttons to minimize,
/// maximize, and close.
const OVERLAPPED_WINDOW: i64 = 0x00cf_0000;

/// `GWLP_USERDATA`, the index of the pointer a window keeps for its program.
const GWLP_USERDATA: i64 = -21;

/// The messages the procedure records with nothing more, returning 0: `WM_SIZE`, `WM_CLOSE`,
/// `WM_CHAR`, `WM_MOUSEMOVE`, and `WM_MOUSEWHEEL`.
const RECORDED: [i64; 5] = [0x05, 0x10, 0x102, 0x200, 0x20a];

/// The key messages, which the procedure records with the character the key types and leaves
/// to `DefWindowProcW` too: `WM_KEYDOWN`, `WM_KEYUP`, `WM_SYSKEYDOWN`, and `WM_SYSKEYUP`.
const KEYS: [i64; 4] = [0x100, 0x101, 0x104, 0x105];

/// The messages of a mouse button going down, for the left, right, and middle buttons, each
/// followed by the one of it coming up.
const BUTTONS_DOWN: [i64; 3] = [0x201, 0x204, 0x207];

/// Emits the routines `used` lists, each doing what the method of `stdlib::gui::NativeWindow`
/// with the same name does:
///
/// - `stone.win_open` opens a window titled `rdi` whose inside is `rsi` by `rdx` pixels and
///   returns a result of it
/// - `stone.win_events` waits up to `rsi` milliseconds (forever if negative) for a message to
///   the window `rdi` if none was recorded, handles every message waiting, and returns the
///   records as bytes
/// - `stone.win_frame` waits for the desktop to put its windows on the screen, and returns
///   whether it could
/// - `stone.win_show` keeps the image `rsi` by `rdx` whose pixels are the bytes `rcx`, and paints
///   it in the window `rdi`
/// - `stone.win_set_title` sets the title of `rdi` to `rsi`
/// - `stone.win_close` closes the window `rdi`
///
/// They need the fs routines' `.Lfs_ok` and `.Lfs_err`, the shims' `.Lwin_errno`, the string and
/// enum runtimes, and the allocator. The window procedure and the class it is registered by come
/// along.
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
    procedure(r#gen);
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

/// Emits code that closes the window of the handle in `reg`, a register a Windows call keeps,
/// unless it is closed already: its procedure stops finding the handle first, so the messages
/// destroying it sends are not recorded. It changes what a Windows call changes.
fn destroy(r#gen: &mut dyn AssemblyGenerator, reg: &str, label: &str) {
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [{reg} + 16]"));
    r#gen.emit("\ttest\trcx, rcx");
    r#gen.emit(&format!("\tjz\t{label}_closed"));
    r#gen.emit(&format!("\tmov\tedx, {GWLP_USERDATA}"));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tcall\tSetWindowLongPtrW");
    r#gen.emit(&format!("\tmov\trcx, QWORD PTR [{reg} + 16]"));
    r#gen.emit("\tcall\tDestroyWindow");
    r#gen.emit(&format!("\tmov\tQWORD PTR [{reg} + 16], 0"));
    r#gen.emit(&format!("{label}_closed:"));
}

/// Emits `stone.win_proc`, the procedure of every window, which Windows calls with the
/// Microsoft convention: the window in `rcx`, the message in `edx`, `wParam` in `r8`, and
/// `lParam` in `r9`. It records the messages `gui_win32.stn` reads, paints the last image shown,
/// and leaves the rest to `DefWindowProcW`, keeping every register that convention keeps.
///
/// `WM_NCCREATE`, the first message, brings the handle in the `CREATESTRUCTW` that `lParam`
/// points to, whose first field is `CreateWindowExW`'s last argument.
fn procedure(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "rsi", "rdi", "r12", "r13", "r14"];
    // r12 holds the window, r13 the message, r14 wParam, rbx lParam, and rsi the handle; a
    // PAINTSTRUCT goes at [rsp + 32]
    r#gen.emit("stone.win_proc:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 112");
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
    r#gen.emit("\tcmp\tr13d, 15"); // WM_PAINT
    r#gen.emit("\tjne\t.Lwin_proc_default");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tlea\trdx, [rsp + 32]");
    r#gen.emit("\tcall\tBeginPaint");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\tmov\trdx, rsi");
    r#gen.emit("\tcall\t.Lwin_paint");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tlea\trdx, [rsp + 32]");
    r#gen.emit("\tcall\tEndPaint");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    r#gen.emit(".Lwin_proc_record:");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tcall\t.Lwin_record");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");

    // a key also gives the character it types, and goes on to DefWindowProcW, which turns
    // Alt+F4 into WM_CLOSE
    r#gen.emit(".Lwin_proc_key:");
    r#gen.emit("\tmov\tecx, r14d");
    r#gen.emit("\tmov\tedx, 2"); // MAPVK_VK_TO_CHAR
    r#gen.emit("\tcall\tMapVirtualKeyW");
    r#gen.emit("\tmov\tedi, eax");
    r#gen.emit("\tcall\t.Lwin_record");
    r#gen.emit("\tjmp\t.Lwin_proc_default");

    // the window takes the mouse while a button is held, so a drag goes on outside it
    r#gen.emit(".Lwin_proc_down:");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tcall\t.Lwin_record");
    r#gen.emit("\tmov\trcx, r12");
    r#gen.emit("\tcall\tSetCapture");
    r#gen.emit("\tjmp\t.Lwin_proc_zero");
    r#gen.emit(".Lwin_proc_up:");
    r#gen.emit("\txor\tedi, edi");
    r#gen.emit("\tcall\t.Lwin_record");
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

    // appends the message r13, wParam r14, lParam rbx, and edi to the records of the handle
    // rsi, growing them, changing rax, rcx, and rdx
    r#gen.emit(".Lwin_record:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 32]");
    r#gen.emit("\tadd\trax, 16");
    r#gen.emit("\tcmp\trax, QWORD PTR [rsi + 40]");
    r#gen.emit("\tjbe\t.Lwin_record_room");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 40]");
    r#gen.emit("\tadd\trax, rax");
    r#gen.emit("\tmov\tecx, 256");
    r#gen.emit("\tcmp\trax, rcx");
    r#gen.emit("\tcmovb\trax, rcx");
    r#gen.emit("\tmov\tQWORD PTR [rsi + 40], rax");
    r#gen.emit("\tpush\trdi");
    r#gen.emit("\tpush\trsi");
    r#gen.emit("\tmov\trdi, QWORD PTR [rsi + 24]");
    r#gen.emit("\tmov\trsi, rax");
    r#gen.emit("\ttest\trdi, rdi");
    r#gen.emit("\tjnz\t.Lwin_record_grow");
    r#gen.emit("\tmov\trdi, rsi");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tjmp\t.Lwin_record_grown");
    r#gen.emit(".Lwin_record_grow:");
    r#gen.emit("\tcall\tstone.mem_realloc");
    r#gen.emit(".Lwin_record_grown:");
    r#gen.emit("\tpop\trsi");
    r#gen.emit("\tpop\trdi");
    r#gen.emit("\tmov\tQWORD PTR [rsi + 24], rax");
    r#gen.emit(".Lwin_record_room:");
    r#gen.emit("\tmov\trax, QWORD PTR [rsi + 24]");
    r#gen.emit("\tadd\trax, QWORD PTR [rsi + 32]");
    r#gen.emit("\tmov\tDWORD PTR [rax], r13d");
    r#gen.emit("\tmov\tDWORD PTR [rax + 4], r14d");
    r#gen.emit("\tmov\tDWORD PTR [rax + 8], ebx");
    r#gen.emit("\tmov\tDWORD PTR [rax + 12], edi");
    r#gen.emit("\tadd\tQWORD PTR [rsi + 32], 16");
    r#gen.emit("\tret");
}

/// Emits `.Lwin_paint`, which paints the image the handle `rdx` keeps, if any, at the top left
/// of the device context `rcx`, called from a 16-byte aligned stack and changing what a Windows
/// call changes. The `BITMAPINFO` of a 32-bit image with rows from the top goes at
/// `[rsp + 96]`.
fn paint(r#gen: &mut dyn AssemblyGenerator) {
    r#gen.emit(".Lwin_paint:");
    r#gen.emit("\tpush\trbx");
    r#gen.emit("\tsub\trsp, 144");
    r#gen.emit("\tmov\trbx, rdx");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 48], 0");
    r#gen.emit("\tje\t.Lwin_paint_done");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 96], 40");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 56]");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 100], eax");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 64]");
    r#gen.emit("\tneg\teax"); // negative for rows from the top
    r#gen.emit("\tmov\tDWORD PTR [rsp + 104], eax");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 108], 2097153"); // 1 plane, 32 bits a pixel
    for offset in [112, 120, 128] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], 0"));
    }
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tr9, QWORD PTR [rbx + 56]");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 64]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    for offset in [40, 48, 56, 88] {
        r#gen.emit(&format!("\tmov\tQWORD PTR [rsp + {offset}], 0"));
    }
    r#gen.emit("\tmov\tQWORD PTR [rsp + 64], rax"); // every row
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 48]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], rax");
    r#gen.emit("\tlea\trax, [rsp + 96]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 80], rax");
    r#gen.emit("\tcall\tSetDIBitsToDevice");
    r#gen.emit(".Lwin_paint_done:");
    r#gen.emit("\tadd\trsp, 144");
    r#gen.emit("\tpop\trbx");
    r#gen.emit("\tret");
}

/// Emits `.Lwin_wide`'s sibling for text of any length, `.Lwin_text`, which returns in `rax` a
/// block from `stone.mem_alloc` holding the string `rcx` as null-terminated UTF-16, for the
/// caller to free, called from a 16-byte aligned stack and changing what a Windows call changes.
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

/// Emits `stone.win_open`. The first window makes the process aware of each monitor's DPI, so
/// a canvas's pixel is the screen's, and registers the class every window shares.
fn open(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12", "r13", "r14", "r15"];
    // rbx holds the title, r12 the width, r13 the height, r14 the handle, and r15 the title in
    // UTF-16; the window's RECT goes at [rsp + 96] and the class's WNDCLASSEXW at [rsp + 112]
    r#gen.emit("stone.win_open:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 208");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tmov\tr13, rdx");
    r#gen.emit("\tcmp\tQWORD PTR [rip + stone.win_class], 0");
    r#gen.emit("\tjne\t.Lwin_open_registered");
    r#gen.emit("\tmov\tQWORD PTR [rip + stone.win_class], 1");
    r#gen.emit("\tmov\trcx, -4"); // DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2
    r#gen.emit("\tcall\tSetProcessDpiAwarenessContext");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 112], 80"); // its size
    r#gen.emit("\tmov\tDWORD PTR [rsp + 116], 32"); // CS_OWNDC
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

    // the handle, which its window's procedure records into from the first message
    r#gen.emit(&format!("\tmov\trdi, {WINDOW_TAG}"));
    r#gen.emit("\txor\tesi, esi");
    r#gen.emit(&format!("\tmov\tedx, {WINDOW_SLOTS}"));
    r#gen.emit("\tcall\tstone.enum_new");
    r#gen.emit("\tmov\tr14, rax");
    for slot in 0..WINDOW_SLOTS {
        r#gen.emit(&format!("\tmov\tQWORD PTR [r14 + {}], 0", 16 + 8 * slot));
    }
    // the window's size for an inside of the size asked for
    r#gen.emit("\tmov\tQWORD PTR [rsp + 96], 0");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 104], r12d");
    r#gen.emit("\tmov\tDWORD PTR [rsp + 108], r13d");
    r#gen.emit("\tlea\trcx, [rsp + 96]");
    r#gen.emit(&format!("\tmov\tedx, {OVERLAPPED_WINDOW}"));
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tcall\tAdjustWindowRectEx");
    r#gen.emit("\tmov\trcx, rbx");
    r#gen.emit("\tcall\t.Lwin_text");
    r#gen.emit("\tmov\tr15, rax");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tcall\tGetModuleHandleW");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 80], rax");
    // CW_USEDEFAULT for where it goes
    r#gen.emit("\tmov\teax, 2147483648");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 40], rax");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 104]");
    r#gen.emit("\tsub\teax, DWORD PTR [rsp + 96]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 48], rax");
    r#gen.emit("\tmov\teax, DWORD PTR [rsp + 108]");
    r#gen.emit("\tsub\teax, DWORD PTR [rsp + 100]");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 56], rax");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 64], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 72], 0");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 88], r14");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\tlea\trdx, [rip + stone.win_class_name]");
    r#gen.emit("\tmov\tr8, r15");
    r#gen.emit(&format!("\tmov\tr9d, {OVERLAPPED_WINDOW}"));
    r#gen.emit("\tcall\tCreateWindowExW");
    r#gen.emit("\tmov\tQWORD PTR [r14 + 16], rax");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_open_failed");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trcx, QWORD PTR [r14 + 16]");
    r#gen.emit("\tmov\tedx, 5"); // SW_SHOW
    r#gen.emit("\tcall\tShowWindow");
    r#gen.emit("\tmov\trdi, r14");
    r#gen.emit("\tmov\tesi, 3"); // an enum value's slot
    r#gen.emit("\tcall\t.Lfs_ok");
    r#gen.emit("\tjmp\t.Lwin_open_done");
    // the error, then the handle, which has no window, freed
    r#gen.emit(".Lwin_open_failed:");
    r#gen.emit("\tcall\t.Lwin_errno");
    r#gen.emit("\tneg\trax");
    r#gen.emit("\tmov\tr13, rax");
    r#gen.emit("\tmov\trax, r15");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit("\tmov\trax, r14");
    r#gen.emit("\tdec\tQWORD PTR [rax - 8]");
    r#gen.emit("\tcall\tstone.free_enum");
    r#gen.emit("\tmov\trdi, r13");
    r#gen.emit("\txor\tesi, esi"); // the reason alone
    r#gen.emit("\tcall\t.Lfs_err");
    r#gen.emit(".Lwin_open_done:");
    leave(r#gen, &saved);
}

/// Emits `stone.win_events`. A wait for a message only waits when none was recorded, and ends at
/// the first that comes for any window.
fn events(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12"];
    // rbx holds the handle and r12 the timeout; a MSG goes at [rsp + 48]
    r#gen.emit("stone.win_events:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 112");
    r#gen.emit("\tmov\trbx, rdi");
    r#gen.emit("\tmov\tr12, rsi");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 16], 0");
    r#gen.emit("\tje\t.Lwin_events_take");
    r#gen.emit("\tcmp\tQWORD PTR [rbx + 32], 0");
    r#gen.emit("\tjne\t.Lwin_events_pump");
    r#gen.emit("\ttest\tr12, r12");
    r#gen.emit("\tjz\t.Lwin_events_pump");
    // INFINITE for a negative timeout, and at most one millisecond less otherwise
    r#gen.emit("\tmov\tr9d, -1");
    r#gen.emit("\tjs\t.Lwin_events_wait");
    r#gen.emit("\tmov\teax, 4294967294");
    r#gen.emit("\tcmp\tr12, rax");
    r#gen.emit("\tcmova\tr12, rax");
    r#gen.emit("\tmov\tr9, r12");
    r#gen.emit(".Lwin_events_wait:");
    r#gen.emit("\txor\tecx, ecx");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], 1279"); // QS_ALLINPUT
    r#gen.emit("\tcall\tMsgWaitForMultipleObjects");
    r#gen.emit(".Lwin_events_pump:");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\txor\tedx, edx");
    r#gen.emit("\txor\tr8d, r8d");
    r#gen.emit("\txor\tr9d, r9d");
    r#gen.emit("\tmov\tQWORD PTR [rsp + 32], 1"); // PM_REMOVE
    r#gen.emit("\tcall\tPeekMessageW");
    r#gen.emit("\ttest\teax, eax");
    r#gen.emit("\tjz\t.Lwin_events_take");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\tcall\tTranslateMessage");
    r#gen.emit("\tlea\trcx, [rsp + 48]");
    r#gen.emit("\tcall\tDispatchMessageW");
    r#gen.emit("\tjmp\t.Lwin_events_pump");
    // the records become bytes, and the block is empty again
    r#gen.emit(".Lwin_events_take:");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 32]");
    r#gen.emit("\tcall\tstone.str_new");
    r#gen.emit("\tmov\trdi, rax");
    r#gen.emit("\tmov\trsi, QWORD PTR [rbx + 24]");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 32]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 32], 0");
    leave(r#gen, &saved);
}

/// Emits `stone.win_show`. An image with fewer bytes than its size needs is not shown, nor one
/// in a closed window.
fn show(r#gen: &mut dyn AssemblyGenerator) {
    let saved = ["rbx", "r12", "r13", "r14"];
    // rbx holds the handle, r12 the width, r13 the height, r14 the pixels and then the device
    // context, and [rsp + 96] the bytes the image takes
    r#gen.emit("stone.win_show:");
    enter(r#gen, &saved);
    r#gen.emit("\tsub\trsp, 112");
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
    r#gen.emit("\tmov\tQWORD PTR [rsp + 96], rax");
    // a bigger image needs a new block, since nothing of the old one is kept
    r#gen.emit("\tcmp\trax, QWORD PTR [rbx + 72]");
    r#gen.emit("\tjbe\t.Lwin_show_room");
    r#gen.emit("\tmov\trax, QWORD PTR [rbx + 48]");
    r#gen.emit("\ttest\trax, rax");
    r#gen.emit("\tjz\t.Lwin_show_new");
    r#gen.emit("\tcall\tstone.mem_free");
    r#gen.emit(".Lwin_show_new:");
    r#gen.emit("\tmov\trdi, QWORD PTR [rsp + 96]");
    r#gen.emit("\tcall\tstone.mem_alloc");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 48], rax");
    r#gen.emit("\tmov\trax, QWORD PTR [rsp + 96]");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 72], rax");
    r#gen.emit(".Lwin_show_room:");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 56], r12");
    r#gen.emit("\tmov\tQWORD PTR [rbx + 64], r13");
    r#gen.emit("\tmov\trdi, QWORD PTR [rbx + 48]");
    r#gen.emit("\tmov\trsi, r14");
    r#gen.emit("\tmov\trcx, QWORD PTR [rsp + 96]");
    r#gen.emit("\trep\tmovsb");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tcall\tGetDC");
    r#gen.emit("\tmov\tr14, rax");
    r#gen.emit("\tmov\trcx, rax");
    r#gen.emit("\tmov\trdx, rbx");
    r#gen.emit("\tcall\t.Lwin_paint");
    r#gen.emit("\tmov\trcx, QWORD PTR [rbx + 16]");
    r#gen.emit("\tmov\trdx, r14");
    r#gen.emit("\tcall\tReleaseDC");
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
/// already and frees its blocks, preserving every register except `rcx` and `rdx`, as the free
/// routines must, `xmm3` to `xmm5` included, which a Windows call may change.
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
    for slot in [24, 48] {
        r#gen.emit(&format!("\tmov\trax, QWORD PTR [rdi + {slot}]"));
        r#gen.emit("\ttest\trax, rax");
        r#gen.emit(&format!("\tjz\t.Lfree_handle_{slot}"));
        r#gen.emit("\tcall\tstone.mem_free");
        r#gen.emit(&format!(".Lfree_handle_{slot}:"));
    }
    for xmm in 3..6 {
        r#gen.emit(&format!(
            "\tmovq\txmm{xmm}, QWORD PTR [rsp + {}]",
            32 + 8 * (xmm - 3)
        ));
    }
    leave(r#gen, &saved);
}
