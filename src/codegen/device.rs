//! GPUs that a program's code may run on beside its [`Target`](super::Target).
//!
//! No backend generates GPU code yet. A device is not part of a target name, since one program
//! may carry code for several GPUs: a build will take one target plus any number of devices, as
//! in `stone build --target x86_64-linux --gpu gfx1100 --gpu sm_90`.

use std::fmt;
use std::str::FromStr;

/// A GPU, named as its vendor's compilers name it.
///
/// For example, `"gfx90a".parse::<Device>()` is `Ok(Device::Amdgcn { gfx: 0x90a })`, and
/// `"sm_90a"` is `Ok(Device::Nvptx { sm: 90, specific: true })`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Device {
    /// An AMD GPU running the amdgcn instruction set, such as `gfx1100`. Its version digits are
    /// hexadecimal (the last is the stepping, as in `gfx90a`), so `gfx` holds them as written.
    Amdgcn { gfx: u16 },
    /// An NVIDIA GPU of compute capability `sm / 10`.`sm % 10`, such as `sm_90`. A `specific`
    /// device (`sm_90a`) may use features that later GPUs lack.
    Nvptx { sm: u16, specific: bool },
}

impl fmt::Display for Device {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Device::Amdgcn { gfx } => write!(f, "gfx{gfx:x}"),
            Device::Nvptx { sm, specific } => {
                write!(f, "sm_{sm}{}", if *specific { "a" } else { "" })
            }
        }
    }
}

impl FromStr for Device {
    type Err = String;

    /// Parses `gfx` followed by 3 or 4 lowercase hexadecimal digits that start with a nonzero
    /// decimal one (AMD), or `sm_` followed by a 2 or 3 digit number and an optional `a`
    /// (NVIDIA).
    fn from_str(name: &str) -> Result<Self, Self::Err> {
        let device = if let Some(digits) = name.strip_prefix("gfx") {
            let valid = (3..=4).contains(&digits.len())
                && digits.starts_with(|c: char| matches!(c, '1'..='9'))
                && digits
                    .bytes()
                    .all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'));
            valid
                .then(|| u16::from_str_radix(digits, 16).ok())
                .flatten()
                .map(|gfx| Device::Amdgcn { gfx })
        } else if let Some(rest) = name.strip_prefix("sm_") {
            let (digits, specific) = match rest.strip_suffix('a') {
                Some(digits) => (digits, true),
                None => (rest, false),
            };
            let valid = (2..=3).contains(&digits.len())
                && !digits.starts_with('0')
                && digits.bytes().all(|b| b.is_ascii_digit());
            valid
                .then(|| digits.parse().ok())
                .flatten()
                .map(|sm| Device::Nvptx { sm, specific })
        } else {
            None
        };
        device.ok_or_else(|| {
            format!(
                "unknown GPU '{name}' (expected an AMD GPU such as gfx1100 or an NVIDIA GPU such \
                 as sm_90)"
            )
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn devices_parse_by_their_vendors_names_and_display_them() {
        for (name, device) in [
            ("gfx1100", Device::Amdgcn { gfx: 0x1100 }),
            ("gfx90a", Device::Amdgcn { gfx: 0x90a }),
            ("gfx942", Device::Amdgcn { gfx: 0x942 }),
            (
                "sm_90",
                Device::Nvptx {
                    sm: 90,
                    specific: false,
                },
            ),
            (
                "sm_90a",
                Device::Nvptx {
                    sm: 90,
                    specific: true,
                },
            ),
            (
                "sm_120",
                Device::Nvptx {
                    sm: 120,
                    specific: false,
                },
            ),
        ] {
            assert_eq!(name.parse(), Ok(device), "{name}");
            assert_eq!(device.to_string(), name);
        }
        for name in [
            "", "cuda", "gfx", "gfx9", "gfx0900", "gfx11000", "gfx90A", "gfx90g", "sm_", "sm_9",
            "sm_090", "sm_1000", "sm_90b", "sm90", "SM_90",
        ] {
            assert_eq!(
                name.parse::<Device>(),
                Err(format!(
                    "unknown GPU '{name}' (expected an AMD GPU such as gfx1100 or an NVIDIA GPU \
                     such as sm_90)"
                )),
                "{name}"
            );
        }
    }
}
