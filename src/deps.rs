//! `stone add`, `stone remove`, `stone fetch`, and `stone tree`, which manage the packages a
//! program uses (see `stone::package`), and the downloading `run`, `build`, and `check` do first.
//!
//! Downloading needs the `fetch` feature, which release builds have. Without it, `remove` and
//! `tree` still work, and so does building a program whose packages are already in the store.

use std::path::{Path, PathBuf};
use std::process::ExitCode;
use stone::diagnostic::Severity;
use stone::package::MANIFEST;
use stone::package::edit;
use stone::project::{self, DEFAULT_ENTRY, FsSources, Linked, PackageInfo, Sources};

/// What a build without the `fetch` feature says when asked to download.
#[cfg(not(feature = "fetch"))]
const NO_FETCH: &str = "this build of stone cannot download packages; install a release of stone \
                        from https://github.com/tarolling/stone/releases, or build it with \
                        `cargo install --features fetch`";

/// Returns the entry file of the program `path` names: the file itself, or the `main.stn` of a
/// directory, or of the program the current directory is in if there is no path (the current
/// directory itself if it is in none). Only its manifest matters, so it need not exist.
fn entry_of(path: Option<&Path>) -> PathBuf {
    match path {
        None => root().join(DEFAULT_ENTRY),
        Some(path) if path.is_dir() => path.join(DEFAULT_ENTRY),
        Some(path) => path.to_path_buf(),
    }
}

/// Returns the root of the program the current directory is in, relative to it, or the current
/// directory (an empty path) if it is in none, which is where `stone add` makes a `stone.toml`.
fn root() -> PathBuf {
    stone::driver::program_dir().unwrap_or_default()
}

/// Downloads every package the program whose entry file is `entry` uses that is not in the
/// store yet, before `run`, `build`, or `check` links it, unless `offline`. Any error is printed,
/// and linking then reports which packages are missing.
pub fn prepare(entry: &Path, offline: bool) {
    let has_manifest = entry
        .parent()
        .map_or(Path::new(MANIFEST).to_path_buf(), |dir| dir.join(MANIFEST))
        .is_file();
    #[cfg(feature = "fetch")]
    if !offline && has_manifest {
        let mut client = stone::package::fetch::Client::new();
        if let Err(e) = stone::package::fetch::fetch_missing(entry, &mut client, &mut log) {
            eprintln!("error: {e}");
        }
    }
    #[cfg(not(feature = "fetch"))]
    let _ = (offline, has_manifest);
}

/// Prints a line about downloading.
#[cfg(feature = "fetch")]
fn log(line: String) {
    eprintln!("{line}");
}

/// Prints the errors of a link of manifests and writes its lockfile if it should change,
/// returning whether there were no errors.
fn finish(linked: &Linked) -> bool {
    let errors: Vec<_> = linked
        .diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    let rendered: String = errors.iter().map(|d| linked.sources.render(d)).collect();
    eprint!("{rendered}");
    if !errors.is_empty() {
        return false;
    }
    if let Some((path, text)) = &linked.lock
        && let Err(e) = FsSources.write(path, text)
    {
        eprintln!("error: {e}");
        return false;
    }
    true
}

/// Turns the outcome of a command into its exit code, printing its error.
fn exit(result: Result<bool, String>) -> ExitCode {
    match result {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::FAILURE,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}

/// Runs `stone add`: finds the release `spec` asks for, downloads it, names it in the current
/// directory's `stone.toml` (made if there is none) under `name` or the package's own name, and
/// downloads whatever else the program now uses.
pub fn add(spec: &str, name: Option<&str>, host: Option<&str>) -> ExitCode {
    exit(add_package(spec, name, host))
}

#[cfg(not(feature = "fetch"))]
fn add_package(_: &str, _: Option<&str>, _: Option<&str>) -> Result<bool, String> {
    Err(NO_FETCH.to_string())
}

#[cfg(feature = "fetch")]
fn add_package(spec: &str, name: Option<&str>, host: Option<&str>) -> Result<bool, String> {
    use stone::package::{Manifest, dependency_name_error};
    use stone::span::FileId;

    let (source, wanted) = edit::parse_spec(spec, host)?;
    let mut client = stone::package::fetch::Client::new();
    let release = client.newest(&source, &wanted)?;
    let version = release.version;
    let store = FsSources
        .store()
        .ok_or("there is no directory to keep packages in; set STONE_HOME")?;
    let dir = stone::package::store::package_dir(&store, &source, &version);
    if !dir.join(MANIFEST).is_file() {
        log(format!("downloading {source} {version}"));
    }
    let dir = client
        .download(&store, &source, &version)
        .map_err(|e| format!("cannot download {source} {version}: {e}"))?;
    let text = std::fs::read_to_string(dir.join(MANIFEST)).map_err(|e| e.to_string())?;
    let (manifest, _) = Manifest::parse(&text, FileId(0));
    let name = match (name, manifest.name) {
        (Some(name), _) => name.to_string(),
        (None, Some((name, _))) => name,
        (None, None) => {
            return Err(format!(
                "{source} {version} does not name itself in its {MANIFEST}, so give it a name \
                 with --name"
            ));
        }
    };
    if let Some(message) = dependency_name_error(&name) {
        return Err(format!("{message}, with --name"));
    }
    let root = root();
    let manifest = root.join(MANIFEST);
    let old = std::fs::read_to_string(&manifest).unwrap_or_default();
    let new = edit::set_dependency(&old, &name, &edit::dependency_value(&source, &version))?;
    std::fs::write(&manifest, new)
        .map_err(|e| format!("cannot write {}: {e}", manifest.display()))?;
    let linked =
        stone::package::fetch::fetch_missing(&root.join(DEFAULT_ENTRY), &mut client, &mut log)?;
    let ok = finish(&linked);
    if ok {
        println!("added {name} {version} ({source})");
    }
    Ok(ok)
}

/// Runs `stone remove`: takes the dependency `name` out of the current directory's
/// `stone.toml`, and out of `stone.lock` with any package only it used.
pub fn remove(name: &str) -> ExitCode {
    exit((|| {
        let root = root();
        let manifest = root.join(MANIFEST);
        let old = std::fs::read_to_string(&manifest)
            .map_err(|_| format!("there is no {MANIFEST} in this directory or any above it"))?;
        let new = edit::remove_dependency(&old, name)?;
        std::fs::write(&manifest, new)
            .map_err(|e| format!("cannot write {}: {e}", manifest.display()))?;
        let ok = finish(&project::link(&root.join(DEFAULT_ENTRY), "", &FsSources));
        if ok {
            println!("removed {name}");
        }
        Ok(ok)
    })())
}

/// Runs `stone fetch`: downloads every package the program in `path` uses and brings its
/// `stone.lock` up to date, which with `locked` must already be.
pub fn fetch(path: Option<&Path>, locked: bool) -> ExitCode {
    let entry = entry_of(path);
    exit(fetch_all(&entry, locked))
}

#[cfg(not(feature = "fetch"))]
fn fetch_all(_: &Path, _: bool) -> Result<bool, String> {
    Err(NO_FETCH.to_string())
}

#[cfg(feature = "fetch")]
fn fetch_all(entry: &Path, locked: bool) -> Result<bool, String> {
    let mut client = stone::package::fetch::Client::new();
    let linked = stone::package::fetch::fetch_missing(entry, &mut client, &mut log)?;
    if locked && let Some((path, _)) = &linked.lock {
        let errors = linked
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error);
        if !errors {
            return Err(format!(
                "{} is not up to date, and --locked keeps it as it is; run `stone fetch` to \
                 update it",
                path.display()
            ));
        }
    }
    Ok(finish(&linked))
}

/// Runs `stone tree`: prints the packages the program in `path` uses, and the ones they use, as
/// a tree. A package seen before is marked `(*)` and not shown again.
pub fn tree(path: Option<&Path>) -> ExitCode {
    let entry = entry_of(path);
    let linked = project::link(&entry, "", &FsSources);
    if linked.packages.is_empty() {
        return ExitCode::SUCCESS;
    }
    let root = &linked.packages[0];
    let mut title = match root.name.as_str() {
        "" => std::path::absolute(entry)
            .ok()
            .and_then(|entry| Some(entry.parent()?.file_name()?.to_string_lossy().into_owned()))
            .unwrap_or("program".to_string()),
        name => name.to_string(),
    };
    if let Some(version) = &root.version {
        title.push_str(&format!(" {version}"));
    }
    let mut lines = vec![title];
    let mut seen = vec![false; linked.packages.len()];
    seen[0] = true;
    branches(&linked.packages, 0, "", &mut seen, &mut lines);
    for line in lines {
        println!("{line}");
    }
    match finish(&linked) {
        true => ExitCode::SUCCESS,
        false => ExitCode::FAILURE,
    }
}

/// Adds the lines of the packages `packages[at]` uses, each after `indent`.
fn branches(
    packages: &[PackageInfo],
    at: usize,
    indent: &str,
    seen: &mut [bool],
    lines: &mut Vec<String>,
) {
    let deps = &packages[at].deps;
    for (n, (key, i)) in deps.iter().enumerate() {
        let last = n + 1 == deps.len();
        let package = &packages[*i];
        let mut line = format!("{indent}{}", if last { "└── " } else { "├── " });
        if *key != package.name {
            line.push_str(&format!("{key} = "));
        }
        line.push_str(&package.name);
        if let Some(version) = &package.version {
            line.push_str(&format!(" {version}"));
        }
        if let Some(source) = &package.source {
            line.push_str(&format!(" ({source})"));
        }
        if seen[*i] {
            line.push_str(" (*)");
            lines.push(line);
            continue;
        }
        seen[*i] = true;
        lines.push(line);
        let deeper = format!("{indent}{}", if last { "    " } else { "│   " });
        branches(packages, *i, &deeper, seen, lines);
    }
}
