//! The builtin `gui` module: canvases to draw on, and windows that show them.
//!
//! Like `net`, `gui` is written in stone, in `gui.stn`, which a program that says `use gui` is
//! linked with like a module file of its own. It speaks the X11 protocol itself over a socket
//! from `net`, and reaches the system through the internal module `gui_sys`, whose one function,
//! [`pack`], turns pixels into the bytes a display or an image file takes, which a loop in stone
//! would do too slowly under `stone run`. Compiled code does the same in `stone.gui_pack`.

use super::BuiltinDoc;

/// The name a program imports the module by, as in `use gui`.
pub const MODULE: &str = "gui";

/// The module's stone source, which `use gui` links with the program.
pub const SOURCE: &str = include_str!("gui.stn");

/// The modules written in stone that `gui.stn` uses and only stone's own modules may: X11 and
/// Wayland windows, keymaps and the keysym table, and the byte helpers both display protocols share.
pub const INTERNAL_SOURCES: [(&str, &str); 5] = [
    ("gui_x11", include_str!("gui_x11.stn")),
    ("gui_wayland", include_str!("gui_wayland.stn")),
    ("gui_keys", include_str!("gui_keys.stn")),
    ("gui_keysyms", include_str!("gui_keysyms.stn")),
    ("gui_wire", include_str!("gui_wire.stn")),
];

/// The name of the internal module through which `gui.stn` reaches the system.
pub const SYS_MODULE: &str = "gui_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The native routines behind the builtin `gui` module.";

pub static SYS_DOCS: [BuiltinDoc; 5] = [
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
    let width = if matches!(format, 0 | 1 | 3) { 4 } else { 3 };
    let mut out = Vec::with_capacity(count * width);
    for &color in &pixels[start..start + count] {
        let [b, g, r] = [color as u8, (color >> 8) as u8, (color >> 16) as u8];
        match format {
            0 => out.extend_from_slice(&[b, g, r, 0]),
            1 => out.extend_from_slice(&[0, r, g, b]),
            3 => out.extend_from_slice(&[b, g, r, (color >> 24) as u8]),
            _ => out.extend_from_slice(&[r, g, b]),
        }
    }
    out
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
    fn a_range_past_the_pixels_is_cut() {
        let pixels = [0x010203, 0x040506];
        assert_eq!(pack(&pixels, -5, 1, 2), [1, 2, 3]);
        assert_eq!(pack(&pixels, 1, 9, 2), [4, 5, 6]);
        assert_eq!(pack(&pixels, 7, 1, 2), Vec::<u8>::new());
        assert_eq!(pack(&pixels, 0, -1, 2), Vec::<u8>::new());
    }
}
