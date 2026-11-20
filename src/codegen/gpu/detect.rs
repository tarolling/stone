//! What a build knows of the GPU it writes code for: the devices it carries code for, and what
//! measuring this machine's GPU found, which `stone build` bakes into the program.
//!
//! By default a build for this machine asks `nvidia-smi` for its NVIDIA GPU, measures once how
//! long a launch takes and how fast lists move to the GPU and back (by building and running a
//! tiny program), and keeps the result in `<stone home>/gpu/`, keyed by the GPU, its driver,
//! stone's version, and the target ([`cache_name`]). The program then knows from its first
//! instruction which arithmetic on lists is worth a launch. `--no-gpu` builds no GPU code at all,
//! and [`DEVICES_VARIABLE`] names devices instead, whose programs measure the GPU when they first
//! reach one, for tests and builds for other machines.
//!
//! Everything here is plain data and text, so the driver, which runs the tools and the probe,
//! stays the only part that touches the machine.

use crate::codegen::device::Device;

/// The environment variable that names the GPUs a build writes code for instead of the one it
/// finds, separated by commas, such as `sm_75,sm_89`, or none when empty.
pub const DEVICES_VARIABLE: &str = "STONE_GPU_DEVICES";

/// The environment variable that names the `nvidia-smi` to run instead of the system's, as
/// `STONE_PTXAS` names `ptxas`.
pub const SMI_VARIABLE: &str = "STONE_NVIDIA_SMI";

/// The arguments that make `nvidia-smi` write one line per GPU: its name, compute capability,
/// driver version, and PCI bus, separated by commas.
pub const SMI_QUERY: [&str; 2] = [
    "--query-gpu=name,compute_cap,driver_version,pci.bus_id",
    "--format=csv,noheader",
];

/// The GPUs a build writes code for, and what measuring the GPU of this machine found.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Gpu {
    pub devices: Vec<Device>,
    /// What the build measured of the GPU it found, or `None`, when the program measures the GPU
    /// itself on its first launch.
    pub measured: Option<Measured>,
}

/// What measuring a GPU found: its compute capability times 10, such as 75, the time a launch
/// takes to start and finish, and the time per byte of moving a list to it and back, all in
/// femtoseconds.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Measured {
    pub sm: u32,
    pub latency: u64,
    pub up: u64,
    pub down: u64,
}

/// A GPU as `nvidia-smi` describes it with [`SMI_QUERY`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SmiGpu {
    pub name: String,
    /// The compute capability times 10, such as 75 for 7.5.
    pub sm: u32,
    pub driver: String,
    pub bus: String,
}

impl SmiGpu {
    /// Returns the device whose code this GPU runs best, such as `sm_75`.
    pub fn device(&self) -> Result<Device, String> {
        format!("sm_{}", self.sm).parse()
    }
}

/// Reads a line `nvidia-smi` wrote for [`SMI_QUERY`], or `None` if it is not one. A GPU's name
/// may hold commas, so the other fields are taken from the end.
///
/// For example, `Quadro T1000, 7.5, 596.36, 00000000:01:00.0` is a GPU named `Quadro T1000` of
/// compute capability 75.
pub fn parse_smi(line: &str) -> Option<SmiGpu> {
    let mut fields = line.trim().rsplitn(4, ',').map(str::trim);
    let bus = fields.next()?;
    let driver = fields.next()?;
    let capability = fields.next()?;
    let name = fields.next()?;
    let (major, minor) = capability.split_once('.')?;
    let (major, minor) = (major.parse::<u32>().ok()?, minor.parse::<u32>().ok()?);
    if name.is_empty() || driver.is_empty() || minor > 9 {
        return None;
    }
    Some(SmiGpu {
        name: name.to_string(),
        sm: major * 10 + minor,
        driver: driver.to_string(),
        bus: bus.to_string(),
    })
}

/// Returns the devices `value`, the setting of [`DEVICES_VARIABLE`], names, each one stone can
/// write code for, or why not.
///
/// For example, `sm_75,sm_89` gives two devices, an empty value none, and `gfx1100` the error
/// `STONE_GPU_DEVICES: AMD GPUs (gfx1100) are not supported yet`.
pub fn forced_devices(value: &str) -> Result<Vec<Device>, String> {
    let mut devices = Vec::new();
    for name in value
        .split(',')
        .map(str::trim)
        .filter(|name| !name.is_empty())
    {
        let device: Device = name
            .parse()
            .map_err(|error| format!("{DEVICES_VARIABLE}: {error}"))?;
        device
            .ptx_version()
            .map_err(|error| format!("{DEVICES_VARIABLE}: {error}"))?;
        if !devices.contains(&device) {
            devices.push(device);
        }
    }
    Ok(devices)
}

/// What measuring a GPU gave: its measurements, or the refusal code of a GPU that could not be
/// used, its kind in the low four bits and a detail above them, as the planner's drivers give
/// it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Probed {
    Measured(Measured),
    Refused(i64),
}

impl Probed {
    /// Reads what the probe program printed, or a cache file: `ok <sm> <latency> <up> <down>`,
    /// or `none <refusal code>`.
    ///
    /// For example, `ok 75 9000000000 80000 90000\n` is a measured `sm_75`, and `none 1` a
    /// machine without NVIDIA's driver.
    pub fn parse(text: &str) -> Option<Probed> {
        let words: Vec<&str> = text.split_whitespace().collect();
        match words.as_slice() {
            ["ok", sm, latency, up, down] => Some(Probed::Measured(Measured {
                sm: sm.parse().ok()?,
                latency: latency.parse().ok()?,
                up: up.parse().ok()?,
                down: down.parse().ok()?,
            })),
            ["none", code] => Some(Probed::Refused(code.parse().ok()?)),
            _ => None,
        }
    }

    /// Returns the text [`Probed::parse`] reads back, with a newline.
    pub fn text(&self) -> String {
        match self {
            Probed::Measured(m) => format!("ok {} {} {} {}\n", m.sm, m.latency, m.up, m.down),
            Probed::Refused(code) => format!("none {code}\n"),
        }
    }

    /// Returns whether the outcome stays true for the machine, so later builds may reuse it: a
    /// measurement, or a refusal for a missing driver, a missing GPU, or code it cannot run, and
    /// not one that may pass, such as a GPU that was busy or slow to answer.
    pub fn lasts(&self) -> bool {
        match self {
            Probed::Measured(_) => true,
            Probed::Refused(code) => matches!(code & 15, 1..=3),
        }
    }
}

/// Returns why a GPU that refused with `code` gets no code, for the note a build prints.
///
/// For example, `refusal_reason(1)` is `NVIDIA's driver is missing or unsupported`, and
/// `refusal_reason(4 + 16 * 999)` is `the GPU's driver failed (status 999)`.
pub fn refusal_reason(code: i64) -> String {
    match code & 15 {
        1 => "NVIDIA's driver is missing or unsupported".into(),
        2 => "there is no CUDA device".into(),
        3 => "the GPU cannot run the code stone writes for it".into(),
        4 => format!("the GPU's driver failed (status {})", code >> 4),
        5 => "a kernel takes more values than a launch passes".into(),
        6 => "the GPU did not finish in time".into(),
        8 => "the GPU was too slow to reach to be worth it".into(),
        _ => format!("the GPU could not be used (refusal {code})"),
    }
}

/// Returns the name of the file that keeps what measuring a GPU gave, without its directory: the
/// SHA-256 of the line `nvidia-smi` wrote for it, stone's `version`, and the `target`, in
/// hexadecimal, then `.txt`. Another GPU, driver, or version of stone measures again.
///
/// For example, `cache_name("Quadro T1000, 7.5, 596.36, 00000000:01:00.0", "0.9.0",
/// "x86_64-windows")` is 64 hexadecimal digits and `.txt`.
pub fn cache_name(line: &str, version: &str, target: &str) -> String {
    let key = format!("{}\n{version}\n{target}\n", line.trim());
    let digest = crate::codegen::sha256::sha256(key.as_bytes());
    let hex: String = digest.iter().map(|b| format!("{b:02x}")).collect();
    format!("{hex}.txt")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_line_of_nvidia_smi_names_a_gpu_and_its_capability() {
        let gpu = parse_smi("Quadro T1000, 7.5, 596.36, 00000000:01:00.0\n").unwrap();
        assert_eq!(gpu.name, "Quadro T1000");
        assert_eq!(gpu.sm, 75);
        assert_eq!(gpu.driver, "596.36");
        assert_eq!(gpu.bus, "00000000:01:00.0");
        assert_eq!(gpu.device().unwrap().to_string(), "sm_75");
        // a name may hold commas
        let gpu = parse_smi("NVIDIA RTX 4000, Ada, 8.9, 595.10, 00000000:65:00.0").unwrap();
        assert_eq!((gpu.name.as_str(), gpu.sm), ("NVIDIA RTX 4000, Ada", 89));
        assert_eq!(parse_smi("NVIDIA H100, 12.0, 595.10, 0:1").unwrap().sm, 120);
        // what a failing nvidia-smi writes is no GPU
        assert_eq!(parse_smi(""), None);
        assert_eq!(parse_smi("No devices were found"), None);
        assert_eq!(parse_smi("Quadro T1000, [N/A], 596.36, 0:1"), None);
    }

    #[test]
    fn forced_devices_are_ones_stone_writes_code_for() {
        let names = |value: &str| -> Vec<String> {
            forced_devices(value)
                .unwrap()
                .iter()
                .map(Device::to_string)
                .collect()
        };
        assert_eq!(names("sm_75,sm_89"), ["sm_75", "sm_89"]);
        assert_eq!(names(" sm_75 , sm_75 "), ["sm_75"]);
        assert!(names("").is_empty());
        assert_eq!(
            forced_devices("sm_75,gfx1100").unwrap_err(),
            "STONE_GPU_DEVICES: AMD GPUs (gfx1100) are not supported yet"
        );
        assert!(
            forced_devices("cuda")
                .unwrap_err()
                .starts_with("STONE_GPU_DEVICES: unknown GPU 'cuda'")
        );
    }

    #[test]
    fn what_a_probe_found_reads_back_from_its_text() {
        let measured = Probed::Measured(Measured {
            sm: 75,
            latency: 9_000_000_000,
            up: 80_000,
            down: 90_000,
        });
        assert_eq!(measured.text(), "ok 75 9000000000 80000 90000\n");
        assert_eq!(Probed::parse(&measured.text()), Some(measured));
        let refused = Probed::Refused(1);
        assert_eq!(refused.text(), "none 1\n");
        assert_eq!(Probed::parse(&refused.text()), Some(refused));
        assert_eq!(Probed::parse("ok 75 1 2"), None);
        assert_eq!(Probed::parse("maybe"), None);
        assert_eq!(Probed::parse(""), None);
    }

    #[test]
    fn only_lasting_outcomes_are_kept_for_later_builds() {
        assert!(Probed::Measured(Measured::default()).lasts());
        // no driver, no device, no code
        assert!(Probed::Refused(1).lasts());
        assert!(Probed::Refused(2).lasts());
        assert!(Probed::Refused(3 + 16 * 0x705).lasts());
        // a failing driver, a timeout, or a slow GPU may pass
        assert!(!Probed::Refused(4 + 16 * 999).lasts());
        assert!(!Probed::Refused(6).lasts());
        assert!(!Probed::Refused(8).lasts());
        assert_eq!(
            refusal_reason(4 + 16 * 999),
            "the GPU's driver failed (status 999)"
        );
    }

    #[test]
    fn another_gpu_driver_version_or_target_is_measured_again() {
        let line = "Quadro T1000, 7.5, 596.36, 00000000:01:00.0";
        let name = cache_name(line, "0.9.0", "x86_64-windows");
        assert_eq!(name.len(), 64 + 4);
        assert!(name.ends_with(".txt"));
        assert_eq!(
            name,
            cache_name(&format!("{line}\n"), "0.9.0", "x86_64-windows")
        );
        let other_driver = "Quadro T1000, 7.5, 597.01, 00000000:01:00.0";
        assert_ne!(name, cache_name(other_driver, "0.9.0", "x86_64-windows"));
        assert_ne!(name, cache_name(line, "0.9.1", "x86_64-windows"));
        assert_ne!(name, cache_name(line, "0.9.0", "x86_64v3-windows"));
    }
}
