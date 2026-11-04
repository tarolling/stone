//! GPUs that a program's code may run on beside its [`Target`](super::Target).
//!
//! A device is not part of a target name, since one program may carry code for several GPUs: a
//! build takes one target plus any number of devices, as in
//! `stone build --target x86_64-linux --gpu sm_75 --gpu sm_90`. Only NVIDIA GPUs have a backend
//! yet ([`gpu::ptx`](super::gpu::ptx)), and nothing launches its kernels.

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

/// The PTX ISA version that first supports each NVIDIA GPU stone generates code for, by its
/// compute capability. A GPU older than `sm_50` is past the drivers that still load PTX for it.
const PTX_VERSIONS: [(u16, (u32, u32)); 16] = [
    (50, (4, 0)),
    (52, (4, 1)),
    (53, (4, 2)),
    (60, (5, 0)),
    (61, (5, 0)),
    (62, (5, 0)),
    (70, (6, 0)),
    (72, (6, 1)),
    (75, (6, 3)),
    (80, (7, 0)),
    (86, (7, 1)),
    (87, (7, 4)),
    (89, (7, 8)),
    (90, (7, 8)),
    (100, (8, 6)),
    (120, (8, 7)),
];

impl Device {
    /// Returns the oldest PTX ISA version, as major and minor, that can target this GPU, which
    /// is what its PTX declares, so the oldest driver that knows the GPU loads it. A GPU that
    /// stone cannot generate code for is an error.
    ///
    /// For example, `sm_75` is `Ok((6, 3))`, `sm_90a` is `Ok((8, 0))`, since its specific
    /// features came later than `sm_90`, and `gfx1100` is the error
    /// `AMD GPUs (gfx1100) are not supported yet`.
    pub fn ptx_version(self) -> Result<(u32, u32), String> {
        let (sm, specific) = match self {
            Device::Amdgcn { .. } => {
                return Err(format!("AMD GPUs ({self}) are not supported yet"));
            }
            Device::Nvptx { sm, specific } => (sm, specific),
        };
        let unknown = || {
            format!(
                "unsupported NVIDIA GPU '{self}' (stone generates code for sm_50 through \
                 sm_120, and for sm_90a and later with 'a')"
            )
        };
        let (_, version) = PTX_VERSIONS
            .iter()
            .find(|(known, _)| *known == sm)
            .ok_or_else(unknown)?;
        match (specific, sm) {
            (false, _) => Ok(*version),
            (true, 90) => Ok((8, 0)),
            (true, 100..) => Ok(*version),
            (true, _) => Err(unknown()),
        }
    }
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

    #[test]
    fn each_nvidia_gpu_declares_the_oldest_ptx_that_targets_it() {
        for (name, version) in [
            ("sm_50", (4, 0)),
            ("sm_75", (6, 3)),
            ("sm_86", (7, 1)),
            ("sm_90", (7, 8)),
            ("sm_90a", (8, 0)),
            ("sm_100a", (8, 6)),
            ("sm_120", (8, 7)),
        ] {
            let device: Device = name.parse().unwrap();
            assert_eq!(device.ptx_version(), Ok(version), "{name}");
        }
        let unknown = |name: &str| {
            format!(
                "unsupported NVIDIA GPU '{name}' (stone generates code for sm_50 through sm_120, \
                 and for sm_90a and later with 'a')"
            )
        };
        for name in ["sm_35", "sm_74", "sm_80a", "sm_130"] {
            let device: Device = name.parse().unwrap();
            assert_eq!(device.ptx_version(), Err(unknown(name)), "{name}");
        }
        let amd: Device = "gfx1100".parse().unwrap();
        assert_eq!(
            amd.ptx_version(),
            Err("AMD GPUs (gfx1100) are not supported yet".to_string())
        );
    }
}
