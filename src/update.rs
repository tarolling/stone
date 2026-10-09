//! `stone update`: replaces the running binary with a release from GitHub.
//!
//! It downloads the same `stone-<target>.tar.gz` that `install.sh` does, and checks it against
//! the release's `stone-<target>.tar.gz.sha256` (plus the digest GitHub publishes for each asset)
//! before installing it.

use self_update::ReleaseAsset;
use self_update::backends::github::Update;
use std::error::Error;
use std::path::Path;
use std::process::Command;

/// Returns the release archive for `target`, as `release.yml` names it.
///
/// For example, `asset_name("x86_64-unknown-linux-musl")` returns
/// `"stone-x86_64-unknown-linux-musl.tar.gz"`.
fn asset_name(target: &str) -> String {
    format!("stone-{target}.tar.gz")
}

/// Picks the archive for `target` out of a release's assets by its exact name.
///
/// `self_update` otherwise takes the first asset whose name contains the target, which could be
/// the archive's `.sha256` file.
fn pick_asset(assets: &[ReleaseAsset], target: &str) -> Option<ReleaseAsset> {
    let name = asset_name(target);
    assets.iter().find(|asset| asset.name() == name).cloned()
}

/// Updates stone to the newest release, or to `tag` (such as `v0.2.0`) if given, even when that
/// is older. With `check`, it only reports whether a newer release exists.
pub fn run(check: bool, tag: Option<&str>) -> Result<(), Box<dyn Error>> {
    let current = env!("CARGO_PKG_VERSION");
    // the triple this binary was built for, which is the one to download
    let target = self_update::get_target();
    let mut builder = Update::configure();
    builder
        .repo_owner("tarolling")
        .repo_name("stone")
        .tag_prefix("v")
        .bin_name("stone")
        .target(target)
        .current_version(current)
        .asset_matcher(move |assets| pick_asset(assets, target))
        .checksum_from_asset(format!("{}.sha256", asset_name(target)))
        .verify_binary(check_binary)
        .show_download_progress(true)
        .show_output(false)
        .no_confirm(true);
    if let Some(tag) = tag {
        builder.release_tag(tag);
    }
    let updater = builder.build()?;

    if check {
        match updater.is_update_available()? {
            Some(release) => println!(
                "stone {} is available (you have {current}); run `stone update`",
                release.version()
            ),
            None => println!("stone {current} is up to date"),
        }
        return Ok(());
    }
    let status = updater.update()?;
    if status.is_updated() {
        println!("updated stone {current} -> {}", status.version());
    } else {
        println!("stone {current} is up to date");
    }
    Ok(())
}

/// Runs the downloaded `stone --version` before it replaces this one, so a binary that cannot
/// run here is never installed.
fn check_binary(exe: &Path) -> self_update::Result<()> {
    let output = Command::new(exe)
        .arg("--version")
        .output()
        .map_err(|e| self_update::Error::verification_rejected(format!("cannot run it: {e}")))?;
    if output.status.success() && output.stdout.starts_with(b"stone ") {
        Ok(())
    } else {
        Err(self_update::Error::verification_rejected(
            "`stone --version` failed",
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn asset(name: &str) -> ReleaseAsset {
        ReleaseAsset::new(name, format!("https://example.com/{name}"))
    }

    #[test]
    fn asset_name_matches_the_release_archives() {
        assert_eq!(
            asset_name("aarch64-apple-darwin"),
            "stone-aarch64-apple-darwin.tar.gz"
        );
    }

    #[test]
    fn pick_asset_skips_the_checksum_file() {
        let target = "x86_64-unknown-linux-musl";
        let assets = [
            asset("stone-x86_64-unknown-linux-musl.tar.gz.sha256"),
            asset("stone-aarch64-unknown-linux-musl.tar.gz"),
            asset("stone-x86_64-unknown-linux-musl.tar.gz"),
        ];
        let picked = pick_asset(&assets, target).unwrap();
        assert_eq!(picked.name(), "stone-x86_64-unknown-linux-musl.tar.gz");
    }

    #[test]
    fn pick_asset_finds_nothing_for_an_unreleased_target() {
        let assets = [asset("stone-x86_64-unknown-linux-musl.tar.gz")];
        assert!(pick_asset(&assets, "x86_64-unknown-linux-gnu").is_none());
    }
}
