//! Win32 windows where there are none: every system but Windows, where `gui.stn` opens windows
//! on X11 or Wayland instead and never calls these, since `gui_sys.on_windows()` is false.
//! Nothing makes a [`NativeWindow`], so its methods never run.

/// What opening a window fails with.
const UNSUPPORTED: &str = "Win32 windows are only on Windows";

/// A Win32 window, which nothing makes here.
#[derive(Debug)]
pub enum NativeWindow {}

/// Fails, since there are no Win32 windows here.
pub fn open(_: &str, _: i64, _: i64) -> Result<NativeWindow, String> {
    Err(UNSUPPORTED.to_string())
}

impl NativeWindow {
    pub fn events(&mut self, _: i64) -> Vec<u8> {
        match *self {}
    }

    pub fn frame(&mut self) -> bool {
        match *self {}
    }

    pub fn show(&mut self, _: i64, _: i64, _: &[u8]) {
        match *self {}
    }

    pub fn set_title(&mut self, _: &str) {
        match *self {}
    }

    pub fn close(&mut self) {
        match *self {}
    }
}
