//! The builtin `gui` module: canvases to draw on, and windows that show them.
//!
//! Like `net`, `gui` is written in stone, in `gui.stn`, which a program that says `use gui` is
//! linked with like a module file of its own. It speaks the Wayland or the X11 protocol itself
//! over a socket from `net`, or on Windows opens Win32 windows, and reaches the system through
//! the internal module `gui_sys`. Its functions [`pack`] and [`pack_scaled`] turn pixels into the
//! bytes a display or an image file takes, which a loop in stone would do too slowly under
//! `stone run`, and the rest share memory with a display server, take descriptors it sends, and
//! drive Win32 windows. Compiled code does the same in `stone.gui_pack` and the routines beside
//! it.

use super::BuiltinDoc;

// Win32 windows, which `gui_win32.stn` shows its canvases in on Windows, and none elsewhere
#[cfg(windows)]
#[path = "gui/windows.rs"]
mod system;
#[cfg(not(windows))]
#[path = "gui/unsupported.rs"]
mod system;

pub use system::{NativeWindow, open};

/// The name a program imports the module by, as in `use gui`.
pub const MODULE: &str = "gui";

/// The module's stone source, which `use gui` links with the program.
pub const SOURCE: &str = include_str!("gui.stn");

/// The modules written in stone that `gui.stn` uses and only stone's own modules may: X11,
/// Wayland, and Win32 windows, keymaps and the keysym table, and the byte helpers the display
/// protocols share.
pub const INTERNAL_SOURCES: [(&str, &str); 6] = [
    ("gui_x11", include_str!("gui_x11.stn")),
    ("gui_wayland", include_str!("gui_wayland.stn")),
    ("gui_win32", include_str!("gui_win32.stn")),
    ("gui_keys", include_str!("gui_keys.stn")),
    ("gui_keysyms", include_str!("gui_keysyms.stn")),
    ("gui_wire", include_str!("gui_wire.stn")),
];

/// The name of the internal module through which `gui.stn` reaches the system.
pub const SYS_MODULE: &str = "gui_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The native routines behind the builtin `gui` module.";

pub static SYS_DOCS: [BuiltinDoc; 13] = [
    BuiltinDoc {
        name: "gui_sys.pack",
        signature: "gui_sys.pack(pixels: list[int], start: int, count: int, format: int) -> \
                    bytes",
        description: "Returns the bytes of `count` colors of `pixels` from `start`, as \
                      `format` lays them out: 0 for blue, green, red, and a 0 byte, 1 for a 0 \
                      byte, red, green, and blue, 2 for red, green, and blue, or 3 for blue, \
                      green, red, and the alpha in bits 24 to 31.",
    },
    BuiltinDoc {
        name: "gui_sys.take_fd",
        signature: "gui_sys.take_fd(s: socket, size: int) -> result[bytes, str]",
        description: "Returns at most `size` bytes of the file whose descriptor came on `s` \
                      last, and closes it.",
    },
    BuiltinDoc {
        name: "gui_sys.shm_create",
        signature: "gui_sys.shm_create(s: socket, size: int) -> result[int, str]",
        description: "Makes `size` bytes of memory to share through `s`, in place of any \
                      before.",
    },
    BuiltinDoc {
        name: "gui_sys.shm_write",
        signature: "gui_sys.shm_write(s: socket, offset: int, data: bytes) -> result[int, str]",
        description: "Writes `data` to the memory `s` shares, from `offset`.",
    },
    BuiltinDoc {
        name: "gui_sys.shm_send",
        signature: "gui_sys.shm_send(s: socket, data: bytes) -> result[int, str]",
        description: "Sends `data` on `s` with the descriptor of the memory it shares.",
    },
    BuiltinDoc {
        name: "gui_sys.pack_scaled",
        signature: "gui_sys.pack_scaled(pixels: list[int], width: int, height: int, out_width: \
                    int, out_height: int, format: int) -> bytes",
        description: "Returns the bytes of the image `width` by `height` whose colors are \
                      `pixels`, scaled to `out_width` by `out_height`, each pixel taking the \
                      color of the one it falls in, laid out in `format` as `pack` does.",
    },
    BuiltinDoc {
        name: "gui_sys.on_windows",
        signature: "gui_sys.on_windows() -> bool",
        description: "Returns whether the program runs on Windows, which compiled code knows \
                      as it is built, so an `if` on it keeps only the code for its system.",
    },
    BuiltinDoc {
        name: "gui_sys.window_open",
        signature: "gui_sys.window_open(title: str, width: int, height: int, dpi: int) -> \
                    result[native_window, str]",
        description: "Opens a Win32 window titled `title` whose inside is `width` by `height` \
                      logical pixels, on a thread of its own, drawn at `dpi` dots per inch, or \
                      at its monitor's (following it to another) if `dpi` is 0.",
    },
    BuiltinDoc {
        name: "gui_sys.window_events",
        signature: "gui_sys.window_events(w: native_window, timeout_ms: int) -> bytes",
        description: "Waits at most `timeout_ms` milliseconds (-1 for ever) for a message to \
                      `w` if none has come, and returns those to `w` since the last call, 16 \
                      bytes each.",
    },
    BuiltinDoc {
        name: "gui_sys.window_frame",
        signature: "gui_sys.window_frame(w: native_window) -> bool",
        description: "Waits until the desktop next puts its windows on the screen, and \
                      returns false if it cannot tell.",
    },
    BuiltinDoc {
        name: "gui_sys.window_show",
        signature: "gui_sys.window_show(w: native_window, width: int, height: int, pixels: \
                    bytes)",
        description: "Shows the image `width` by `height` whose pixels `pack` laid out in \
                      format 0 in `w`, each pixel a logical one, scaled to the window's DPI, \
                      and keeps it to paint again.",
    },
    BuiltinDoc {
        name: "gui_sys.window_set_title",
        signature: "gui_sys.window_set_title(w: native_window, title: str)",
        description: "Sets the title of `w`.",
    },
    BuiltinDoc {
        name: "gui_sys.window_close",
        signature: "gui_sys.window_close(w: native_window)",
        description: "Closes `w`.",
    },
];

/// Returns the bytes of the colors `0xRRGGBB` of `pixels` from `start` for `count` of them, as
/// `format` lays each out: 0 as blue, green, red, and 0 (a 32-bit pixel whose least significant
/// byte comes first, as X11 takes it), 1 as 0, red, green, and blue (most significant first), 3
/// as blue, green, red, and the alpha in bits 24 to 31 (Wayland's ARGB8888, for a frame with
/// transparent edges), or any other as red, green, and blue (as a PPM image holds it). A range that runs past either
/// end of `pixels` is cut to the part inside it.
///
/// For example, `pack(&[0x102030], 0, 1, 0)` is `[0x30, 0x20, 0x10, 0]`.
pub fn pack(pixels: &[i64], start: i64, count: i64, format: i64) -> Vec<u8> {
    let start = start.clamp(0, pixels.len() as i64) as usize;
    let count = count.clamp(0, (pixels.len() - start) as i64) as usize;
    let mut out = Vec::with_capacity(count * bytes_per_pixel(format));
    for &color in &pixels[start..start + count] {
        put(&mut out, color, format);
    }
    out
}

/// The largest width or height [`pack_scaled`] takes, before or after scaling, which is far
/// past any window's.
pub const MAX_SCALED: i64 = 1 << 18;

/// Returns the bytes of the image `width` by `height` whose colors are `pixels`, a row at a
/// time, scaled to `out_width` by `out_height`, as [`pack`] lays each out in `format`. Each
/// pixel of the new image takes the color of the one it falls in: pixel `x` of a row takes
/// pixel `x * width / out_width` of its row, rounded down, and row `y` takes row
/// `y * height / out_height`. A pixel past the end of `pixels` is black. Any size below 1 or
/// above [`MAX_SCALED`] gives no bytes.
///
/// For example, scaling a row of two pixels to three gives the first pixel twice and the second
/// once: `pack_scaled(&[1, 2], 2, 1, 3, 1, 2)` is `[0, 0, 1, 0, 0, 1, 0, 0, 2]`.
pub fn pack_scaled(
    pixels: &[i64],
    width: i64,
    height: i64,
    out_width: i64,
    out_height: i64,
    format: i64,
) -> Vec<u8> {
    if [width, height, out_width, out_height]
        .iter()
        .any(|&size| !(1..=MAX_SCALED).contains(&size))
    {
        return Vec::new();
    }
    let mut out = Vec::with_capacity((out_width * out_height) as usize * bytes_per_pixel(format));
    for y in 0..out_height {
        let row = y * height / out_height * width;
        // the source pixel moves on by one each time `width` steps add up to `out_width`
        let (mut source, mut step) = (0, 0);
        for _ in 0..out_width {
            let color = pixels.get((row + source) as usize).copied().unwrap_or(0);
            put(&mut out, color, format);
            step += width;
            while step >= out_width {
                step -= out_width;
                source += 1;
            }
        }
    }
    out
}

/// The bytes a pixel takes in `format`, as [`pack`] lays it out.
fn bytes_per_pixel(format: i64) -> usize {
    if matches!(format, 0 | 1 | 3) { 4 } else { 3 }
}

/// Appends the bytes of `color` in `format` to `out`, as [`pack`] lays them out.
fn put(out: &mut Vec<u8>, color: i64, format: i64) {
    let [b, g, r] = [color as u8, (color >> 8) as u8, (color >> 16) as u8];
    match format {
        0 => out.extend_from_slice(&[b, g, r, 0]),
        1 => out.extend_from_slice(&[0, r, g, b]),
        3 => out.extend_from_slice(&[b, g, r, (color >> 24) as u8]),
        _ => out.extend_from_slice(&[r, g, b]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pixels_pack_in_each_format() {
        let pixels = [0x102030, 0xffffff, 0x0a0b0c];
        assert_eq!(pack(&pixels, 0, 1, 0), [0x30, 0x20, 0x10, 0]);
        assert_eq!(pack(&pixels, 2, 1, 1), [0, 0x0a, 0x0b, 0x0c]);
        assert_eq!(pack(&pixels, 1, 2, 2), [0xff, 0xff, 0xff, 0x0a, 0x0b, 0x0c]);
        // the bits above a color's 24 are left out
        assert_eq!(pack(&[0x7f00_0000_0102_0304], 0, 1, 2), [0x02, 0x03, 0x04]);
        assert_eq!(pack(&[-1], 0, 1, 0), [0xff, 0xff, 0xff, 0]);
        // with the alpha of bits 24 to 31, for a buffer with an alpha channel
        assert_eq!(pack(&pixels, 0, 1, 3), [0x30, 0x20, 0x10, 0]);
        assert_eq!(pack(&[0x80a0_b0c0], 0, 1, 3), [0xc0, 0xb0, 0xa0, 0x80]);
    }

    #[test]
    fn images_scale_to_the_nearest_pixel() {
        // doubled, each pixel becomes two by two
        let doubled = pack_scaled(&[1, 2, 3, 4], 2, 2, 4, 4, 2);
        let rows: Vec<Vec<u8>> = doubled
            .chunks(12)
            .map(|row| row.iter().skip(2).step_by(3).copied().collect())
            .collect();
        assert_eq!(
            rows,
            [[1, 1, 2, 2], [1, 1, 2, 2], [3, 3, 4, 4], [3, 3, 4, 4]]
        );
        // at 150%, pixels are one or two across, unevenly
        let blues = |packed: Vec<u8>| -> Vec<u8> { packed.iter().step_by(4).copied().collect() };
        assert_eq!(blues(pack_scaled(&[1, 2], 2, 1, 3, 1, 0)), [1, 1, 2]);
        assert_eq!(
            blues(pack_scaled(&[1, 2, 3, 4], 4, 1, 6, 1, 0)),
            [1, 1, 2, 3, 3, 4]
        );
        // halved, every other pixel
        assert_eq!(blues(pack_scaled(&[1, 2, 3, 4], 4, 1, 2, 1, 0)), [1, 3]);
        // the same size is `pack` of all of it, in every format
        let pixels = [0x102030, 0x80a0_b0c0, 0x0a0b0c, 0xffffff];
        for format in 0..4 {
            assert_eq!(
                pack_scaled(&pixels, 2, 2, 2, 2, format),
                pack(&pixels, 0, 4, format)
            );
        }
    }

    #[test]
    fn scaling_tolerates_odd_sizes() {
        // a pixel past the end is black
        assert_eq!(
            pack_scaled(&[0xffffff], 2, 1, 2, 1, 2),
            [255, 255, 255, 0, 0, 0]
        );
        for (width, height, out_width, out_height) in
            [(0, 1, 1, 1), (1, 1, -1, 1), (1, 1, 1, MAX_SCALED + 1)]
        {
            assert!(pack_scaled(&[1], width, height, out_width, out_height, 0).is_empty());
        }
    }

    #[test]
    fn a_range_past_the_pixels_is_cut() {
        let pixels = [0x010203, 0x040506];
        assert_eq!(pack(&pixels, -5, 1, 2), [1, 2, 3]);
        assert_eq!(pack(&pixels, 1, 9, 2), [4, 5, 6]);
        assert_eq!(pack(&pixels, 7, 1, 2), Vec::<u8>::new());
        assert_eq!(pack(&pixels, 0, -1, 2), Vec::<u8>::new());
    }
}
