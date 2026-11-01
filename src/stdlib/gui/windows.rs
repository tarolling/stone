//! Win32 windows for the `gui` module under `stone run` on Windows, as compiled code makes them
//! in `x64::gui_windows`: the window procedure records the messages `gui_win32.stn` reads, 16
//! bytes each (the message, `wParam`, `lParam`, and for a key the character it types, as
//! little-endian 32-bit words), and paints the last image shown, which it keeps.

use crate::stdlib::fs::io_message;
use std::cell::RefCell;
use std::ffi::c_void;
use std::ptr::{null, null_mut};
use std::sync::Once;

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
    fn PeekMessageW(message: *mut Message, window: Handle, min: u32, max: u32, remove: u32) -> i32;
    fn TranslateMessage(message: *const Message) -> i32;
    fn DispatchMessageW(message: *const Message) -> isize;
    fn MsgWaitForMultipleObjects(
        count: u32,
        handles: *const Handle,
        all: i32,
        milliseconds: u32,
        wake: u32,
    ) -> u32;
    fn GetWindowLongPtrW(window: Handle, index: i32) -> isize;
    fn SetWindowLongPtrW(window: Handle, index: i32, value: isize) -> isize;
    fn AdjustWindowRectEx(rect: *mut Rect, style: u32, menu: i32, ex_style: u32) -> i32;
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
    fn SetDIBitsToDevice(
        dc: Handle,
        x: i32,
        y: i32,
        width: u32,
        height: u32,
        source_x: i32,
        source_y: i32,
        start: u32,
        lines: u32,
        bits: *const c_void,
        info: *const BitmapInfo,
        usage: u32,
    ) -> i32;
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

/// The buttons of `wParam`'s mouse state (`MK_LBUTTON`, `MK_RBUTTON`, `MK_MBUTTON`), which say
/// whether one is still held when another comes up.
const BUTTONS_HELD: usize = 0x13;

/// `WS_OVERLAPPEDWINDOW`: a title bar, a frame to resize by, and the buttons to minimize,
/// maximize, and close.
const OVERLAPPED_WINDOW: u32 = 0x00cf_0000;
const CS_OWNDC: u32 = 0x20;
const CW_USEDEFAULT: i32 = i32::MIN;
const GWLP_USERDATA: i32 = -21;
const SW_SHOW: i32 = 5;
const PM_REMOVE: u32 = 1;
const QS_ALLINPUT: u32 = 0x04ff;
const INFINITE: u32 = u32::MAX;
const IDC_ARROW: usize = 32512;
const BLACK_BRUSH: i32 = 4;
const MAPVK_VK_TO_CHAR: u32 = 2;

/// `DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2`, so a pixel of a canvas is a pixel of the
/// screen on every monitor.
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

/// What a window's procedure reaches through `GWLP_USERDATA`: the messages it recorded and the
/// image it paints. It lives in a box of its own, so it stays where the window points while
/// messages are dispatched, and nothing borrows it across a call that may dispatch one.
#[derive(Default)]
struct State {
    records: RefCell<Vec<u8>>,
    image: RefCell<Image>,
}

#[derive(Default)]
struct Image {
    width: i32,
    height: i32,
    pixels: Vec<u8>,
}

/// A window on the screen, which closes when dropped.
pub struct NativeWindow {
    /// The window, or null once closed.
    window: Handle,
    state: Box<State>,
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

/// Opens a window titled `title` whose inside is `width` by `height` pixels.
///
/// The first window makes the process aware of each monitor's DPI and registers the class all
/// windows share.
pub fn open(title: &str, width: i64, height: i64) -> Result<NativeWindow, String> {
    static REGISTER: Once = Once::new();
    REGISTER.call_once(|| unsafe {
        SetProcessDpiAwarenessContext(PER_MONITOR_AWARE_V2 as Handle);
        let class = WindowClass {
            size: size_of::<WindowClass>() as u32,
            style: CS_OWNDC,
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
    let mut rect = Rect {
        left: 0,
        top: 0,
        right: width as i32,
        bottom: height as i32,
    };
    let state = Box::<State>::default();
    let title = wide(title);
    let window = unsafe {
        AdjustWindowRectEx(&mut rect, OVERLAPPED_WINDOW, 0, 0);
        CreateWindowExW(
            0,
            CLASS.as_ptr(),
            title.as_ptr(),
            OVERLAPPED_WINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            rect.right - rect.left,
            rect.bottom - rect.top,
            null_mut(),
            null_mut(),
            GetModuleHandleW(null()),
            &*state as *const State as *mut c_void,
        )
    };
    if window.is_null() {
        return Err(last_reason());
    }
    unsafe { ShowWindow(window, SW_SHOW) };
    Ok(NativeWindow { window, state })
}

impl NativeWindow {
    /// Waits at most `timeout` milliseconds (forever if negative) for a message if none has
    /// been recorded, handles every message waiting, and returns the records since the last
    /// call.
    pub fn events(&mut self, timeout: i64) -> Vec<u8> {
        if self.window.is_null() {
            return std::mem::take(&mut self.state.records.borrow_mut());
        }
        let waiting = !self.state.records.borrow().is_empty();
        if !waiting && timeout != 0 {
            let milliseconds = if timeout < 0 {
                INFINITE
            } else {
                timeout.min(INFINITE as i64 - 1) as u32
            };
            unsafe { MsgWaitForMultipleObjects(0, null(), 0, milliseconds, QS_ALLINPUT) };
        }
        let mut message = std::mem::MaybeUninit::<Message>::uninit();
        while unsafe { PeekMessageW(message.as_mut_ptr(), null_mut(), 0, 0, PM_REMOVE) } != 0 {
            unsafe {
                TranslateMessage(message.as_ptr());
                DispatchMessageW(message.as_ptr());
            }
        }
        std::mem::take(&mut self.state.records.borrow_mut())
    }

    /// Waits for the desktop to put its windows on the screen next, returning whether it could.
    pub fn frame(&mut self) -> bool {
        unsafe { DwmFlush() == 0 }
    }

    /// Keeps the image `width` by `height` of 32-bit pixels, blue first, and paints it. An image
    /// with fewer bytes than its size needs is not shown.
    pub fn show(&mut self, width: i64, height: i64, pixels: &[u8]) {
        if self.window.is_null()
            || width <= 0
            || height <= 0
            || (pixels.len() as i64) < width.saturating_mul(height).saturating_mul(4)
        {
            return;
        }
        {
            let mut image = self.state.image.borrow_mut();
            image.width = width as i32;
            image.height = height as i32;
            image.pixels.clear();
            image
                .pixels
                .extend_from_slice(&pixels[..(width * height * 4) as usize]);
        }
        unsafe {
            let dc = GetDC(self.window);
            paint(dc, &self.state.image.borrow());
            ReleaseDC(self.window, dc);
        }
    }

    /// Sets the window's title.
    pub fn set_title(&mut self, title: &str) {
        if !self.window.is_null() {
            unsafe { SetWindowTextW(self.window, wide(title).as_ptr()) };
        }
    }

    /// Closes the window, if it is open.
    pub fn close(&mut self) {
        if !self.window.is_null() {
            unsafe {
                SetWindowLongPtrW(self.window, GWLP_USERDATA, 0);
                DestroyWindow(self.window);
            }
            self.window = null_mut();
        }
    }
}

impl Drop for NativeWindow {
    fn drop(&mut self) {
        self.close();
    }
}

/// Paints `image` at the top left of `dc`.
unsafe fn paint(dc: Handle, image: &Image) {
    if image.width == 0 {
        return;
    }
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
        SetDIBitsToDevice(
            dc,
            0,
            0,
            image.width as u32,
            image.height as u32,
            0,
            0,
            0,
            image.height as u32,
            image.pixels.as_ptr().cast(),
            &info,
            0,
        );
    }
}

/// Records a message to the window of `state` as `gui_win32.stn` reads it.
fn record(state: &State, message: u32, wparam: usize, lparam: isize, extra: u32) {
    let mut records = state.records.borrow_mut();
    for word in [message, wparam as u32, lparam as u32, extra] {
        records.extend_from_slice(&word.to_le_bytes());
    }
}

/// The procedure of every window: it records what `gui` reports, paints the last image, and
/// leaves the rest to `DefWindowProcW`.
unsafe extern "system" fn procedure(
    window: Handle,
    message: u32,
    wparam: usize,
    lparam: isize,
) -> isize {
    unsafe {
        if message == WM_NCCREATE {
            // the state comes as CreateWindowExW's last argument, the first field of the
            // CREATESTRUCTW `lParam` points to
            let state = *(lparam as *const *const State);
            SetWindowLongPtrW(window, GWLP_USERDATA, state as isize);
            return DefWindowProcW(window, message, wparam, lparam);
        }
        let state = GetWindowLongPtrW(window, GWLP_USERDATA) as *const State;
        let Some(state) = state.as_ref() else {
            return DefWindowProcW(window, message, wparam, lparam);
        };
        match message {
            WM_CLOSE => {
                record(state, message, wparam, lparam, 0);
                0
            }
            WM_PAINT => {
                let mut painting = std::mem::MaybeUninit::<Paint>::uninit();
                let dc = BeginPaint(window, painting.as_mut_ptr());
                paint(dc, &state.image.borrow());
                EndPaint(window, painting.as_ptr());
                0
            }
            WM_SIZE | WM_CHAR | WM_MOUSEMOVE | WM_MOUSEWHEEL => {
                record(state, message, wparam, lparam, 0);
                0
            }
            WM_KEYDOWN | WM_KEYUP | WM_SYSKEYDOWN | WM_SYSKEYUP => {
                let typed = MapVirtualKeyW(wparam as u32, MAPVK_VK_TO_CHAR);
                record(state, message, wparam, lparam, typed);
                DefWindowProcW(window, message, wparam, lparam)
            }
            WM_LBUTTONDOWN | WM_RBUTTONDOWN | WM_MBUTTONDOWN => {
                record(state, message, wparam, lparam, 0);
                SetCapture(window);
                0
            }
            WM_LBUTTONUP | WM_RBUTTONUP | WM_MBUTTONUP => {
                record(state, message, wparam, lparam, 0);
                if wparam & BUTTONS_HELD == 0 {
                    ReleaseCapture();
                }
                0
            }
            _ => DefWindowProcW(window, message, wparam, lparam),
        }
    }
}
