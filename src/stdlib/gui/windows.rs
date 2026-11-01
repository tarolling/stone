//! Win32 windows for the `gui` module under `stone run` on Windows, as compiled code makes them
//! in `x64::gui_windows`.
//!
//! Each window lives on a thread of its own, which creates it and handles its messages, so the
//! loop Windows runs while the window is moved or resized holds up only that thread, and the
//! program goes on drawing. The window procedure records the messages `gui_win32.stn` reads, 16
//! bytes each (the message, `wParam`, `lParam`, and a fourth word: for a key the character it
//! types, and for a size or the mouse the window's DPI, as little-endian 32-bit words), and paints
//! the last image shown, which it keeps, scaled from the canvas's logical pixels to the screen's.

use crate::stdlib::fs::io_message;
use std::ffi::c_void;
use std::ptr::{null, null_mut};
use std::sync::mpsc::channel;
use std::sync::{Arc, Condvar, Mutex, MutexGuard, Once};
use std::thread::JoinHandle;
use std::time::Duration;

type Handle = *mut c_void;
type Procedure = unsafe extern "system" fn(Handle, u32, usize, isize) -> isize;

#[repr(C)]
struct WindowClass {
    size: u32,
    style: u32,
    procedure: Procedure,
    class_extra: i32,
    window_extra: i32,
    instance: Handle,
    icon: Handle,
    cursor: Handle,
    background: Handle,
    menu_name: *const u16,
    class_name: *const u16,
    small_icon: Handle,
}

#[repr(C)]
struct Message {
    window: Handle,
    message: u32,
    wparam: usize,
    lparam: isize,
    time: u32,
    x: i32,
    y: i32,
    private: u32,
}

#[repr(C)]
#[derive(Default)]
struct Rect {
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
}

#[repr(C)]
struct Paint {
    dc: Handle,
    erase: i32,
    rect: Rect,
    restore: i32,
    include_update: i32,
    reserved: [u8; 32],
}

/// A `BITMAPINFO` of a 32-bit top-down image with no color table.
#[repr(C)]
struct BitmapInfo {
    size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bit_count: u16,
    compression: u32,
    size_image: u32,
    x_per_meter: i32,
    y_per_meter: i32,
    colors_used: u32,
    colors_important: u32,
    colors: [u32; 1],
}

#[link(name = "user32")]
unsafe extern "system" {
    fn RegisterClassExW(class: *const WindowClass) -> u16;
    fn CreateWindowExW(
        ex_style: u32,
        class: *const u16,
        title: *const u16,
        style: u32,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        parent: Handle,
        menu: Handle,
        instance: Handle,
        param: *mut c_void,
    ) -> Handle;
    fn DefWindowProcW(window: Handle, message: u32, wparam: usize, lparam: isize) -> isize;
    fn GetMessageW(message: *mut Message, window: Handle, min: u32, max: u32) -> i32;
    fn TranslateMessage(message: *const Message) -> i32;
    fn DispatchMessageW(message: *const Message) -> isize;
    fn PostMessageW(window: Handle, message: u32, wparam: usize, lparam: isize) -> i32;
    fn PostQuitMessage(code: i32);
    fn GetWindowLongPtrW(window: Handle, index: i32) -> isize;
    fn SetWindowLongPtrW(window: Handle, index: i32, value: isize) -> isize;
    fn AdjustWindowRectExForDpi(
        rect: *mut Rect,
        style: u32,
        menu: i32,
        ex_style: u32,
        dpi: u32,
    ) -> i32;
    fn GetDpiForWindow(window: Handle) -> u32;
    fn SetWindowPos(
        window: Handle,
        after: Handle,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        flags: u32,
    ) -> i32;
    fn GetClientRect(window: Handle, rect: *mut Rect) -> i32;
    fn ShowWindow(window: Handle, show: i32) -> i32;
    fn SetWindowTextW(window: Handle, text: *const u16) -> i32;
    fn DestroyWindow(window: Handle) -> i32;
    fn GetDC(window: Handle) -> Handle;
    fn ReleaseDC(window: Handle, dc: Handle) -> i32;
    fn BeginPaint(window: Handle, paint: *mut Paint) -> Handle;
    fn EndPaint(window: Handle, paint: *const Paint) -> i32;
    fn LoadCursorW(instance: Handle, name: *const u16) -> Handle;
    fn SetCapture(window: Handle) -> Handle;
    fn ReleaseCapture() -> i32;
    fn MapVirtualKeyW(code: u32, kind: u32) -> u32;
    fn SetProcessDpiAwarenessContext(context: Handle) -> i32;
}

#[link(name = "gdi32")]
unsafe extern "system" {
    fn StretchDIBits(
        dc: Handle,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
        source_x: i32,
        source_y: i32,
        source_width: i32,
        source_height: i32,
        bits: *const c_void,
        info: *const BitmapInfo,
        usage: u32,
        operation: u32,
    ) -> i32;
    fn SetStretchBltMode(dc: Handle, mode: i32) -> i32;
    fn PatBlt(dc: Handle, x: i32, y: i32, width: i32, height: i32, operation: u32) -> i32;
    fn GetStockObject(object: i32) -> Handle;
}

#[link(name = "dwmapi")]
unsafe extern "system" {
    fn DwmFlush() -> i32;
}

#[link(name = "kernel32")]
unsafe extern "system" {
    fn GetModuleHandleW(name: *const u16) -> Handle;
}

const WM_NCCREATE: u32 = 0x81;
const WM_SIZE: u32 = 0x05;
const WM_PAINT: u32 = 0x0f;
const WM_CLOSE: u32 = 0x10;
const WM_ERASEBKGND: u32 = 0x14;
const WM_KEYDOWN: u32 = 0x100;
const WM_KEYUP: u32 = 0x101;
const WM_CHAR: u32 = 0x102;
const WM_SYSKEYDOWN: u32 = 0x104;
const WM_SYSKEYUP: u32 = 0x105;
const WM_MOUSEMOVE: u32 = 0x200;
const WM_LBUTTONDOWN: u32 = 0x201;
const WM_LBUTTONUP: u32 = 0x202;
const WM_RBUTTONDOWN: u32 = 0x204;
const WM_RBUTTONUP: u32 = 0x205;
const WM_MBUTTONDOWN: u32 = 0x207;
const WM_MBUTTONUP: u32 = 0x208;
const WM_MOUSEWHEEL: u32 = 0x20a;
const WM_DPICHANGED: u32 = 0x2e0;

/// The message the program's thread posts to have a window's own thread close it, the first of
/// `WM_APP`'s.
pub const WM_CLOSE_WINDOW: u32 = 0x8000;

/// The buttons of `wParam`'s mouse state (`MK_LBUTTON`, `MK_RBUTTON`, `MK_MBUTTON`), which say
/// whether one is still held when another comes up.
const BUTTONS_HELD: usize = 0x13;

/// `WS_OVERLAPPEDWINDOW`: a title bar, a frame to resize by, and the buttons to minimize,
/// maximize, and close.
const OVERLAPPED_WINDOW: u32 = 0x00cf_0000;
const CW_USEDEFAULT: i32 = i32::MIN;
const GWLP_USERDATA: i32 = -21;
const SW_SHOW: i32 = 5;
const SWP_NOMOVE: u32 = 0x2;
const SWP_NOZORDER: u32 = 0x4;
const SWP_NOACTIVATE: u32 = 0x10;
const IDC_ARROW: usize = 32512;
const BLACK_BRUSH: i32 = 4;
const MAPVK_VK_TO_CHAR: u32 = 2;
const COLORONCOLOR: i32 = 3;
const SRCCOPY: u32 = 0x00cc_0020;
const BLACKNESS: u32 = 0x0000_0042;

/// The DPI of a screen at 100%, where a logical pixel is a physical one.
const BASE_DPI: i64 = 96;

/// `DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2`, so the window learns each monitor's DPI and
/// `gui` scales its canvases itself.
const PER_MONITOR_AWARE_V2: isize = -4;

/// The name every window's class is registered by, `stone`.
const CLASS: [u16; 6] = [
    b's' as u16,
    b't' as u16,
    b'o' as u16,
    b'n' as u16,
    b'e' as u16,
    0,
];

/// What a window's thread and the program's share: the window's state, and a signal that a
/// message was recorded. The window procedure reaches it through `GWLP_USERDATA`.
struct Shared {
    state: Mutex<State>,
    recorded: Condvar,
    /// The DPI `STONE_GUI_SCALE` fixes, or 0 for the monitor's.
    fixed_dpi: u32,
}

#[derive(Default)]
struct State {
    records: Vec<u8>,
    image: Image,
    /// The DPI the window is drawn at.
    dpi: u32,
}

#[derive(Default)]
struct Image {
    width: i32,
    height: i32,
    pixels: Vec<u8>,
}

/// A window on the screen, which closes when dropped.
pub struct NativeWindow {
    /// The window, or 0 once closed (a number, since a window may be used from any thread).
    window: usize,
    shared: Arc<Shared>,
    thread: Option<JoinHandle<()>>,
}

impl std::fmt::Debug for NativeWindow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("NativeWindow")
    }
}

/// Returns `text` as UTF-16 with a terminator.
fn wide(text: &str) -> Vec<u16> {
    text.encode_utf16().chain([0]).collect()
}

/// Returns the reason alone for the last error of a Win32 call, as compiled code words it.
fn last_reason() -> String {
    let full = io_message(&std::io::Error::last_os_error(), "");
    full.strip_suffix(": ''").unwrap_or(&full).to_string()
}

/// Returns the physical pixels `logical` pixels take at `dpi`, rounded to the nearest.
///
/// For example, `physical(300, 144)` is `450`.
fn physical(logical: i64, dpi: u32) -> i32 {
    ((logical * dpi as i64 + BASE_DPI / 2) / BASE_DPI) as i32
}

/// Opens a window titled `title` whose inside is `width` by `height` logical pixels, drawn at
/// `dpi` dots per inch, or at its monitor's if `dpi` is 0.
///
/// The first window makes the process aware of each monitor's DPI and registers the class all
/// windows share.
pub fn open(title: &str, width: i64, height: i64, dpi: i64) -> Result<NativeWindow, String> {
    static REGISTER: Once = Once::new();
    REGISTER.call_once(|| unsafe {
        SetProcessDpiAwarenessContext(PER_MONITOR_AWARE_V2 as Handle);
        let class = WindowClass {
            size: size_of::<WindowClass>() as u32,
            style: 0,
            procedure,
            class_extra: 0,
            window_extra: 0,
            instance: GetModuleHandleW(null()),
            icon: null_mut(),
            cursor: LoadCursorW(null_mut(), IDC_ARROW as *const u16),
            background: GetStockObject(BLACK_BRUSH),
            menu_name: null(),
            class_name: CLASS.as_ptr(),
            small_icon: null_mut(),
        };
        RegisterClassExW(&class);
    });
    let shared = Arc::new(Shared {
        state: Mutex::default(),
        recorded: Condvar::new(),
        fixed_dpi: dpi.clamp(0, u32::MAX as i64) as u32,
    });
    let title = wide(title);
    let (opened, result) = channel();
    let own = Arc::clone(&shared);
    let thread = std::thread::spawn(move || {
        let created = unsafe { create(&own, &title, width, height) };
        let window = match created {
            Ok(window) => window,
            Err(reason) => {
                let _ = opened.send(Err(reason));
                return;
            }
        };
        let _ = opened.send(Ok(window as usize));
        let mut message = std::mem::MaybeUninit::<Message>::uninit();
        while unsafe { GetMessageW(message.as_mut_ptr(), null_mut(), 0, 0) } > 0 {
            unsafe {
                TranslateMessage(message.as_ptr());
                DispatchMessageW(message.as_ptr());
            }
        }
        // `own` lives until here, as long as the window procedure may reach it
        drop(own);
    });
    match result.recv() {
        Ok(Ok(window)) => Ok(NativeWindow {
            window,
            shared,
            thread: Some(thread),
        }),
        Ok(Err(reason)) => {
            let _ = thread.join();
            Err(reason)
        }
        Err(_) => Err("the window's thread failed".to_string()),
    }
}

/// Creates and shows the window of `shared` on the calling thread, which handles its messages
/// from then on, its inside `width` by `height` logical pixels, and records its size and DPI as
/// a first `WM_SIZE`.
unsafe fn create(
    shared: &Shared,
    title: &[u16],
    width: i64,
    height: i64,
) -> Result<Handle, String> {
    unsafe {
        let window = CreateWindowExW(
            0,
            CLASS.as_ptr(),
            title.as_ptr(),
            OVERLAPPED_WINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            null_mut(),
            null_mut(),
            GetModuleHandleW(null()),
            shared as *const Shared as *mut c_void,
        );
        if window.is_null() {
            return Err(last_reason());
        }
        // the frame is drawn at the monitor's DPI, and the inside at the window's
        let monitor = GetDpiForWindow(window);
        let dpi = match shared.fixed_dpi {
            0 => monitor,
            fixed => fixed,
        };
        lock(shared).dpi = dpi;
        let mut rect = Rect {
            right: physical(width, dpi),
            bottom: physical(height, dpi),
            ..Rect::default()
        };
        AdjustWindowRectExForDpi(&mut rect, OVERLAPPED_WINDOW, 0, 0, monitor);
        SetWindowPos(
            window,
            null_mut(),
            0,
            0,
            rect.right - rect.left,
            rect.bottom - rect.top,
            SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE,
        );
        ShowWindow(window, SW_SHOW);
        let mut client = Rect::default();
        GetClientRect(window, &mut client);
        let size = (client.bottom as u32) << 16 | client.right as u32 & 0xffff;
        record(shared, WM_SIZE, 0, size as isize, dpi);
        Ok(window)
    }
}

/// Locks the state of `shared`, which a panic while holding it leaves usable.
fn lock(shared: &Shared) -> MutexGuard<'_, State> {
    shared
        .state
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

impl NativeWindow {
    /// Waits at most `timeout` milliseconds (forever if negative) for a message if none has
    /// been recorded, and returns the records since the last call.
    pub fn events(&mut self, timeout: i64) -> Vec<u8> {
        let mut state = lock(&self.shared);
        if self.window != 0 && state.records.is_empty() && timeout != 0 {
            let empty = |state: &mut State| state.records.is_empty();
            let recorded = &self.shared.recorded;
            state = if timeout < 0 {
                recorded
                    .wait_while(state, empty)
                    .unwrap_or_else(|poisoned| poisoned.into_inner())
            } else {
                let wait = Duration::from_millis(timeout as u64);
                recorded
                    .wait_timeout_while(state, wait, empty)
                    .unwrap_or_else(|poisoned| poisoned.into_inner())
                    .0
            };
        }
        std::mem::take(&mut state.records)
    }

    /// Waits for the desktop to put its windows on the screen next, returning whether it could.
    pub fn frame(&mut self) -> bool {
        unsafe { DwmFlush() == 0 }
    }

    /// Keeps the image `width` by `height` of 32-bit pixels, blue first, and paints it. An image
    /// with fewer bytes than its size needs is not shown.
    pub fn show(&mut self, width: i64, height: i64, pixels: &[u8]) {
        if self.window == 0
            || width <= 0
            || height <= 0
            || width > i32::MAX as i64
            || height > i32::MAX as i64
            || (pixels.len() as i64) < width.saturating_mul(height).saturating_mul(4)
        {
            return;
        }
        let mut state = lock(&self.shared);
        state.image.width = width as i32;
        state.image.height = height as i32;
        state.image.pixels.clear();
        state
            .image
            .pixels
            .extend_from_slice(&pixels[..(width * height * 4) as usize]);
        let window = self.window as Handle;
        unsafe {
            let dc = GetDC(window);
            paint(window, dc, &state);
            ReleaseDC(window, dc);
        }
    }

    /// Sets the window's title.
    pub fn set_title(&mut self, title: &str) {
        if self.window != 0 {
            unsafe { SetWindowTextW(self.window as Handle, wide(title).as_ptr()) };
        }
    }

    /// Closes the window, if it is open, and waits for its thread to end.
    pub fn close(&mut self) {
        if self.window != 0 {
            unsafe { PostMessageW(self.window as Handle, WM_CLOSE_WINDOW, 0, 0) };
            if let Some(thread) = self.thread.take() {
                let _ = thread.join();
            }
            self.window = 0;
        }
    }
}

impl Drop for NativeWindow {
    fn drop(&mut self) {
        self.close();
    }
}

/// Paints the image of `state` at the top left of `dc`, the device context of `window`, each
/// canvas pixel covering the screen's pixels at the window's DPI, and the rest of the inside
/// black.
///
/// A canvas as big as the window's inside, in logical pixels, covers all of it, so at a scale
/// such as 150% its pixels are one or two screen pixels across, the nearest one to each.
unsafe fn paint(window: Handle, dc: Handle, state: &State) {
    let mut client = Rect::default();
    unsafe { GetClientRect(window, &mut client) };
    let (width, height) = (client.right as i64, client.bottom as i64);
    let dpi = state.dpi.max(1) as i64;
    let logical = |size: i64| (size * BASE_DPI + dpi / 2) / dpi;
    let image = &state.image;
    let (mut across, mut down) = (0, 0);
    if image.width > 0 && logical(width) > 0 && logical(height) > 0 {
        across = (image.width as i64 * width / logical(width)) as i32;
        down = (image.height as i64 * height / logical(height)) as i32;
        let info = BitmapInfo {
            size: 40,
            width: image.width,
            // negative for rows from the top
            height: -image.height,
            planes: 1,
            bit_count: 32,
            compression: 0,
            size_image: 0,
            x_per_meter: 0,
            y_per_meter: 0,
            colors_used: 0,
            colors_important: 0,
            colors: [0],
        };
        unsafe {
            SetStretchBltMode(dc, COLORONCOLOR);
            StretchDIBits(
                dc,
                0,
                0,
                across,
                down,
                0,
                0,
                image.width,
                image.height,
                image.pixels.as_ptr().cast(),
                &info,
                0,
                SRCCOPY,
            );
        }
    }
    let (width, height) = (width as i32, height as i32);
    unsafe {
        if across < width {
            PatBlt(dc, across, 0, width - across, height, BLACKNESS);
        }
        if down < height {
            PatBlt(dc, 0, down, across.min(width), height - down, BLACKNESS);
        }
    }
}

/// Records a message to the window of `shared` as `gui_win32.stn` reads it, and wakes the
/// program if it waits for one.
fn record(shared: &Shared, message: u32, wparam: usize, lparam: isize, extra: u32) {
    let mut state = lock(shared);
    for word in [message, wparam as u32, lparam as u32, extra] {
        state.records.extend_from_slice(&word.to_le_bytes());
    }
    shared.recorded.notify_all();
}

/// The procedure of every window, which runs on the window's own thread: it records what `gui`
/// reports, paints the last image, follows the window to a monitor of another DPI, closes the
/// window when the program asks, and leaves the rest to `DefWindowProcW`.
unsafe extern "system" fn procedure(
    window: Handle,
    message: u32,
    wparam: usize,
    lparam: isize,
) -> isize {
    unsafe {
        if message == WM_NCCREATE {
            // the shared state comes as CreateWindowExW's last argument, the first field of the
            // CREATESTRUCTW `lParam` points to
            let shared = *(lparam as *const *const Shared);
            SetWindowLongPtrW(window, GWLP_USERDATA, shared as isize);
            return DefWindowProcW(window, message, wparam, lparam);
        }
        let shared = GetWindowLongPtrW(window, GWLP_USERDATA) as *const Shared;
        let Some(shared) = shared.as_ref() else {
            return DefWindowProcW(window, message, wparam, lparam);
        };
        let dpi = || lock(shared).dpi;
        match message {
            WM_CLOSE_WINDOW => {
                SetWindowLongPtrW(window, GWLP_USERDATA, 0);
                DestroyWindow(window);
                PostQuitMessage(0);
                0
            }
            WM_CLOSE | WM_CHAR => {
                record(shared, message, wparam, lparam, 0);
                0
            }
            WM_ERASEBKGND => 1,
            WM_PAINT => {
                let mut painting = std::mem::MaybeUninit::<Paint>::uninit();
                let dc = BeginPaint(window, painting.as_mut_ptr());
                paint(window, dc, &lock(shared));
                EndPaint(window, painting.as_ptr());
                0
            }
            WM_DPICHANGED => {
                // the window keeps its logical size on the new monitor, unless its scale is fixed
                if shared.fixed_dpi == 0 {
                    lock(shared).dpi = wparam as u32 & 0xffff;
                    let rect = &*(lparam as *const Rect);
                    SetWindowPos(
                        window,
                        null_mut(),
                        rect.left,
                        rect.top,
                        rect.right - rect.left,
                        rect.bottom - rect.top,
                        SWP_NOZORDER | SWP_NOACTIVATE,
                    );
                }
                0
            }
            WM_SIZE | WM_MOUSEMOVE | WM_MOUSEWHEEL => {
                record(shared, message, wparam, lparam, dpi());
                0
            }
            WM_KEYDOWN | WM_KEYUP | WM_SYSKEYDOWN | WM_SYSKEYUP => {
                let typed = MapVirtualKeyW(wparam as u32, MAPVK_VK_TO_CHAR);
                record(shared, message, wparam, lparam, typed);
                DefWindowProcW(window, message, wparam, lparam)
            }
            WM_LBUTTONDOWN | WM_RBUTTONDOWN | WM_MBUTTONDOWN => {
                record(shared, message, wparam, lparam, dpi());
                SetCapture(window);
                0
            }
            WM_LBUTTONUP | WM_RBUTTONUP | WM_MBUTTONUP => {
                record(shared, message, wparam, lparam, dpi());
                if wparam & BUTTONS_HELD == 0 {
                    ReleaseCapture();
                }
                0
            }
            _ => DefWindowProcW(window, message, wparam, lparam),
        }
    }
}
