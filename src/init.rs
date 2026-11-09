//! `stone init`, which starts a project: a directory holding a `stone.toml` that names it, a
//! `main.stn` (or, for a library, a `lib.stn`) to start from, and a `.gitignore` that leaves out
//! the build directory, all at its root, which is where every module is found from.
//!
//! For example, `stone init hello` makes `hello/stone.toml`, `hello/main.stn`, and
//! `hello/.gitignore`, after which `stone run` in `hello` prints `Hello, world!`.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use crate::driver::BUILD_DIR;
use crate::package::{LOCK, MANIFEST, identifier_error, taken_name};
use crate::project::{DEFAULT_ENTRY, LIB_ENTRY};

/// The `main.stn` a new program starts with.
pub const PROGRAM: &str = include_str!("templates/main.stn");

/// The `lib.stn` a new library starts with.
pub const LIBRARY: &str = include_str!("templates/lib.stn");

/// The files that make a directory a project already, which `stone init` never writes over.
const PROJECT_FILES: [&str; 4] = [MANIFEST, LOCK, DEFAULT_ENTRY, LIB_ENTRY];

/// What `stone init` makes.
#[derive(Debug, Clone, Default)]
pub struct Options {
    /// Whether to start a library, with a `lib.stn`, rather than a program with a `main.stn`.
    pub lib: bool,
    /// The package's name, instead of the directory's.
    pub name: Option<String>,
    /// Whether to make the directory a git repository if it is not in one already.
    pub git: bool,
}

/// What [`init`] did.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Created {
    /// The package's name, as `stone.toml` gives it.
    pub name: String,
    /// The files written, in the order they were.
    pub files: Vec<PathBuf>,
    /// Whether `git init` made the directory a repository.
    pub git: bool,
}

/// Starts a project in `dir`, making the directory if there is none, and returns what it wrote.
/// A directory that already has a `stone.toml`, `stone.lock`, `main.stn`, or `lib.stn` is an
/// error, and nothing is written. An existing `.gitignore` is kept, gaining a `build/` line if it
/// has none.
///
/// For example, `init(Path::new("my-app"), &Options::default())` writes `my-app/stone.toml`
/// naming the package `my_app`, then `my-app/main.stn` and `my-app/.gitignore`.
pub fn init(dir: &Path, options: &Options) -> Result<Created, String> {
    let shown = |path: &Path| path.display().to_string();
    if let Some(found) = PROJECT_FILES
        .iter()
        .map(|file| dir.join(file))
        .find(|path| path.exists())
    {
        return Err(format!(
            "{} already exists, so stone init left {} alone",
            shown(&found),
            match dir.as_os_str().is_empty() {
                true => "this directory".to_string(),
                false => shown(dir),
            }
        ));
    }
    let name = match &options.name {
        Some(name) => name.clone(),
        None => directory_name(dir)?,
    };
    if let Some(message) = name_error(&name, options.lib) {
        return Err(match options.name {
            Some(_) => message,
            None => format!("{message}; give the package a name with --name"),
        });
    }
    std::fs::create_dir_all(dir).map_err(|e| format!("cannot make {}: {e}", shown(dir)))?;
    let mut files = vec![];
    let mut write = |file: &str, text: &str| {
        let path = dir.join(file);
        std::fs::write(&path, text).map_err(|e| format!("cannot write {}: {e}", shown(&path)))?;
        files.push(path);
        Ok::<(), String>(())
    };
    write(MANIFEST, &manifest(&name))?;
    match options.lib {
        true => write(LIB_ENTRY, LIBRARY)?,
        false => write(DEFAULT_ENTRY, PROGRAM)?,
    }
    let ignore = dir.join(".gitignore");
    let ignored = format!("{BUILD_DIR}/");
    match std::fs::read_to_string(&ignore) {
        Ok(old) if old.lines().any(|line| ignores_build(line.trim())) => {}
        Ok(old) => {
            let separator = match old.is_empty() || old.ends_with('\n') {
                true => "",
                false => "\n",
            };
            write(".gitignore", &format!("{old}{separator}{ignored}\n"))?;
        }
        Err(_) => write(".gitignore", &format!("{ignored}\n"))?,
    }
    let git = options.git && git_init(dir);
    Ok(Created { name, files, git })
}

/// Returns the text of a new package's `stone.toml`, which needs the stone running now.
///
/// For example, `manifest("hello")` starts with `[package]` and `name = "hello"`.
pub fn manifest(name: &str) -> String {
    format!(
        "[package]\nname = \"{name}\"\nversion = \"0.1.0\"\nstone = \"{}\"\n",
        env!("CARGO_PKG_VERSION")
    )
}

/// Returns why a new package cannot be named `name`, if it cannot. A library's name is also
/// the name programs use it under, so no builtin module, the prelude, or a builtin may take it.
///
/// For example, `name_error("json", true)` says `json` is a builtin module, and
/// `name_error("json", false)` is `None`.
pub fn name_error(name: &str, lib: bool) -> Option<String> {
    if let Some(message) = identifier_error(name, "a package's name") {
        return Some(message);
    }
    match taken_name(name) {
        Some(what) if lib => Some(format!(
            "'{name}' is {what}, so no program could use a library named it"
        )),
        _ => None,
    }
}

/// Returns the name a package in `dir` takes from it: the directory's own name, with each `-`
/// written as `_`, as Cargo writes a crate's.
///
/// For example, a directory `my-app` gives `my_app`, and `.` gives the current directory's name.
fn directory_name(dir: &Path) -> Result<String, String> {
    let absolute = std::path::absolute(dir).map_err(|e| e.to_string())?;
    // `.` and `a/..` name a directory only once resolved
    let resolved = std::fs::canonicalize(&absolute).unwrap_or(absolute);
    let name = resolved
        .file_name()
        .ok_or("this directory has no name to give the package; give it one with --name")?;
    Ok(name.to_string_lossy().replace('-', "_"))
}

/// Returns whether a line of a `.gitignore` already leaves out the build directory.
fn ignores_build(line: &str) -> bool {
    let line = line.trim_start_matches('/').trim_end_matches('/');
    line == BUILD_DIR
}

/// Makes `dir` a git repository unless it is in one already or git is missing, returning
/// whether it did. A `git init` that fails prints a warning, since the project is made anyway.
fn git_init(dir: &Path) -> bool {
    let inside = Command::new("git")
        .arg("-C")
        .arg(dir)
        .args(["rev-parse", "--is-inside-work-tree"])
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status();
    match inside {
        // no git on PATH, so there is nothing to do
        Err(_) => false,
        Ok(status) if status.success() => false,
        Ok(_) => {
            let made = Command::new("git")
                .arg("init")
                .arg("-q")
                .arg(dir)
                .stdout(Stdio::null())
                .output();
            match made {
                Ok(output) if output.status.success() => true,
                Ok(output) => {
                    eprintln!(
                        "warning: git init failed: {}",
                        String::from_utf8_lossy(&output.stderr).trim()
                    );
                    false
                }
                Err(e) => {
                    eprintln!("warning: git init failed: {e}");
                    false
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostic::Diagnostic;
    use crate::project::FsSources;

    /// Makes an empty directory named `name` under the system's temporary directory, unique to
    /// this process.
    fn scratch(name: &str) -> PathBuf {
        let dir = std::env::temp_dir()
            .join(format!("stone-init-{}", std::process::id()))
            .join(name);
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn a_program_gets_a_manifest_a_main_and_a_gitignore() {
        let dir = scratch("program").join("my-app");
        let created = init(&dir, &Options::default()).unwrap();
        assert_eq!(created.name, "my_app");
        assert_eq!(
            created.files,
            [
                dir.join("stone.toml"),
                dir.join("main.stn"),
                dir.join(".gitignore")
            ]
        );
        assert!(!created.git);
        let manifest = std::fs::read_to_string(dir.join("stone.toml")).unwrap();
        assert!(manifest.starts_with("[package]\nname = \"my_app\"\nversion = \"0.1.0\"\n"));
        assert_eq!(
            std::fs::read_to_string(dir.join("main.stn")).unwrap(),
            PROGRAM
        );
        assert_eq!(
            std::fs::read_to_string(dir.join(".gitignore")).unwrap(),
            "build/\n"
        );
    }

    #[test]
    fn a_library_gets_a_lib_and_may_be_named() {
        let dir = scratch("library");
        let options = Options {
            lib: true,
            name: Some("geo".to_string()),
            ..Options::default()
        };
        let created = init(&dir, &options).unwrap();
        assert_eq!(created.name, "geo");
        assert!(dir.join("lib.stn").is_file());
        assert!(!dir.join("main.stn").exists());
    }

    #[test]
    fn a_project_is_never_written_over() {
        let dir = scratch("existing");
        std::fs::write(dir.join("main.stn"), "print(1)\n").unwrap();
        let error = init(&dir, &Options::default()).unwrap_err();
        assert_eq!(
            error,
            format!(
                "{} already exists, so stone init left {} alone",
                dir.join("main.stn").display(),
                dir.display()
            )
        );
        assert!(!dir.join("stone.toml").exists());
        assert_eq!(
            std::fs::read_to_string(dir.join("main.stn")).unwrap(),
            "print(1)\n"
        );
    }

    #[test]
    fn a_gitignore_is_kept_and_learns_the_build_directory() {
        let dir = scratch("gitignore");
        std::fs::write(dir.join(".gitignore"), "*.log").unwrap();
        init(&dir, &Options::default()).unwrap();
        assert_eq!(
            std::fs::read_to_string(dir.join(".gitignore")).unwrap(),
            "*.log\nbuild/\n"
        );
        let dir = scratch("gitignore_build");
        std::fs::write(dir.join(".gitignore"), "/build\n").unwrap();
        let created = init(&dir, &Options::default()).unwrap();
        assert!(!created.files.contains(&dir.join(".gitignore")));
        assert_eq!(
            std::fs::read_to_string(dir.join(".gitignore")).unwrap(),
            "/build\n"
        );
    }

    #[test]
    fn names_must_be_stone_names() {
        let dir = scratch("names").join("2fast");
        let error = init(&dir, &Options::default()).unwrap_err();
        assert_eq!(
            error,
            "'2fast' cannot be a package's name, which is a name of letters, digits, and '_'; \
             give the package a name with --name"
        );
        assert!(!dir.exists());
        assert_eq!(name_error("json", false), None);
        assert_eq!(
            name_error("json", true),
            Some("'json' is a builtin module, so no program could use a library named it".into())
        );
        assert_eq!(
            name_error("if", false),
            Some("'if' is a keyword, so it cannot be a package's name".into())
        );
    }

    /// Returns every diagnostic `stone check` gives for the project in `dir`.
    fn check(dir: &Path, entry: &str) -> Vec<Diagnostic> {
        let path = dir.join(entry);
        let source = std::fs::read_to_string(&path).unwrap();
        crate::driver::check_program(&path, &source, &FsSources).1
    }

    #[test]
    fn both_templates_check_without_a_warning() {
        let dir = scratch("program_checks");
        init(&dir, &Options::default()).unwrap();
        assert_eq!(check(&dir, "main.stn"), []);
        let dir = scratch("library_checks");
        let options = Options {
            lib: true,
            name: Some("greet".to_string()),
            ..Options::default()
        };
        init(&dir, &options).unwrap();
        assert_eq!(check(&dir, "lib.stn"), []);
    }
}
