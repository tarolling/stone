//! Programs of several files: loading the modules a program uses and linking them into one.
//!
//! The entry file's directory is the program's root, and every other file is a module named by
//! its path from there. For example, running `app/main.stn` makes `app/` the root, so
//! `use geometry.shapes` loads `app/geometry/shapes.stn` as the module `geometry.shapes`. There are
//! no files that only declare or re-export modules: a directory is just part of a module's name.
//!
//! [`link`] merges every module into a single [`Mod`] in which each library function and enum is
//! named by its module, such as `geometry.shapes.area`, and every use of it is rewritten to that
//! name. The checker and both backends then run on it as they would on one file.
//!
//! Every program is also linked with the prelude, [`crate::stdlib::PRELUDE`], whose `option`
//! enum comes first in the linked module under its own name. Its variants are written alone in
//! every file, so `some(x)` and `none` are rewritten to `option.some(x)` and `option.none`, which
//! the checker and both backends treat like any other enum's variants.
//!
//! Builtin modules such as `os` have no file (see [`crate::stdlib::MODULES`]). Calls to their
//! functions are rewritten to their linked names too, such as `os.env`, which the checker and
//! both backends treat as builtins.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use crate::ast::{
    Expr, ExprContext, ExprKind, Mod, Pattern, PatternKind, Stmt, StmtKind, TraitName, TypeExpr,
    TypeExprKind, TypeParam,
};
use crate::checker::{
    DISPLAY, EQ, FROM, OPERATOR_TRAITS, OPTION, ORD, RESULT, TRY_FROM, collect_assigned,
};
use crate::diagnostic::{Diagnostic, excerpt, location};
use crate::explain::Code;
use crate::lexer::Lexer;
use crate::package::lock::{Lock, Locked};
use crate::package::resolve::{self, Requirement};
use crate::package::store::{content_hash, package_dir};
use crate::package::{LIB, LOCK, MANIFEST, Manifest, Source, Version, join_normal};
use crate::parser::Parser;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{
    BuiltinModule, PRELUDE, PRELUDE_VARIANTS, is_builtin, is_reserved, module_function,
    prelude_variant,
};

#[cfg(test)]
mod tests;

/// Where module files are read from: the filesystem, or an in-memory map in tests and editors.
pub trait Sources {
    /// Returns the contents of the file at `path`, or `None` if there is no such file.
    fn read(&self, path: &Path) -> Option<String>;
    /// Returns whether `path` is a directory.
    fn is_dir(&self, path: &Path) -> bool;
    /// Returns the files and directories directly in the directory `dir`, for editors to suggest
    /// modules.
    fn entries(&self, dir: &Path) -> Vec<PathBuf>;
    /// Returns the directory packages from hosts are downloaded into (see
    /// [`crate::package::store`]), if there is one.
    fn store(&self) -> Option<PathBuf> {
        crate::package::store::default_dir()
    }
    /// Writes `text` to the file at `path`, as linking does to keep `stone.lock` current. Sources
    /// that only read, as an editor's, write nothing.
    fn write(&self, path: &Path, text: &str) -> Result<(), String> {
        let _ = (path, text);
        Ok(())
    }
}

/// Reads modules from the filesystem.
pub struct FsSources;

impl Sources for FsSources {
    fn read(&self, path: &Path) -> Option<String> {
        if !path.is_file() {
            return None;
        }
        std::fs::read_to_string(path).ok()
    }

    fn is_dir(&self, path: &Path) -> bool {
        path.is_dir()
    }

    fn entries(&self, dir: &Path) -> Vec<PathBuf> {
        let Ok(entries) = std::fs::read_dir(dir) else {
            return vec![];
        };
        entries.filter_map(|e| e.ok().map(|e| e.path())).collect()
    }

    fn write(&self, path: &Path, text: &str) -> Result<(), String> {
        std::fs::write(path, text).map_err(|e| format!("cannot write {}: {e}", path.display()))
    }
}

/// Reads modules from a map of paths to sources, where a directory is any path that one of the
/// files is under.
///
/// For example, after `sources.insert("geometry/vec.stn", "...")`, `geometry` is a directory.
#[derive(Debug, Default, Clone)]
pub struct MapSources {
    files: HashMap<PathBuf, String>,
    /// The package store's directory, which is none unless set.
    store: Option<PathBuf>,
    /// Every file written, in order, shared with clones.
    written: std::sync::Arc<std::sync::Mutex<Vec<(PathBuf, String)>>>,
}

impl MapSources {
    pub fn insert(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) {
        self.files.insert(path.into(), source.into());
    }

    /// Makes `dir` the package store, whose packages are files under it like any other.
    pub fn set_store(&mut self, dir: impl Into<PathBuf>) {
        self.store = Some(dir.into());
    }

    /// Returns every file written through [`Sources::write`], in order.
    pub fn written(&self) -> Vec<(PathBuf, String)> {
        self.written.lock().expect("not poisoned").clone()
    }
}

impl Sources for MapSources {
    fn read(&self, path: &Path) -> Option<String> {
        self.files.get(path).cloned()
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.files.keys().any(|p| p != path && p.starts_with(path))
    }

    fn store(&self) -> Option<PathBuf> {
        self.store.clone()
    }

    fn write(&self, path: &Path, text: &str) -> Result<(), String> {
        self.written
            .lock()
            .expect("not poisoned")
            .push((path.to_path_buf(), text.to_string()));
        Ok(())
    }

    fn entries(&self, dir: &Path) -> Vec<PathBuf> {
        let mut entries: Vec<PathBuf> = self
            .files
            .keys()
            .filter_map(|p| p.strip_prefix(dir).ok()?.components().next())
            .map(|first| dir.join(first))
            .collect();
        entries.sort();
        entries.dedup();
        entries
    }
}

/// One file of a program.
#[derive(Debug, Clone, PartialEq)]
pub struct SourceFile {
    /// The path diagnostics show, which is the root joined with the module's path, such as
    /// `app/geometry/shapes.stn`.
    pub path: PathBuf,
    /// The module's dotted name, such as `geometry.shapes`, or empty for the entry file and a
    /// manifest. A module of a dependency is named by its package's linked prefix, as
    /// `geo.1.shapes` (see [`crate::package`]).
    pub module: String,
    pub source: String,
    /// The name of the package the file belongs to, if it is a dependency's rather than the
    /// program's own, such as `geo`.
    pub package: Option<String>,
}

impl SourceFile {
    /// Returns whether the file is part of stone, written in stone, such as the `net` module's,
    /// rather than one of the program's, which is named by its module in angle brackets, as
    /// `<net>`.
    pub fn is_builtin(&self) -> bool {
        crate::stdlib::source_module(&self.module).is_some()
            && self.path == Path::new(&format!("<{}>", self.module))
    }

    /// Returns whether the file belongs to a package the program depends on, whose code the
    /// program's author did not write.
    pub fn is_dependency(&self) -> bool {
        self.package.is_some()
    }

    /// Returns whether the file is a package's `stone.toml` or `stone.lock` rather than stone
    /// source.
    pub fn is_package_file(&self) -> bool {
        let name = self.path.file_name();
        name == Some(std::ffi::OsStr::new(MANIFEST)) || name == Some(std::ffi::OsStr::new(LOCK))
    }
}

/// Every file of a program, indexed by [`FileId`], with the entry file first.
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

/// The prelude as a file, which every [`SourceMap`] returns for [`FileId::PRELUDE`].
static PRELUDE_FILE: LazyLock<SourceFile> = LazyLock::new(|| SourceFile {
    path: PathBuf::from("<prelude>"),
    module: String::new(),
    source: PRELUDE.to_string(),
    package: None,
});

impl SourceMap {
    /// Returns the file `id` names, which is the prelude for [`FileId::PRELUDE`], or the entry
    /// file if there is no such file.
    pub fn file(&self, id: FileId) -> &SourceFile {
        if id == FileId::PRELUDE {
            return &PRELUDE_FILE;
        }
        self.files
            .get(id.0 as usize)
            .unwrap_or_else(|| &self.files[0])
    }

    /// Returns every file of the program with its id, the entry file first. The prelude is not
    /// one of them.
    pub fn files(&self) -> impl Iterator<Item = (FileId, &SourceFile)> {
        self.files
            .iter()
            .enumerate()
            .map(|(i, file)| (FileId(i as u32), file))
    }

    /// Returns the id of the file at `path`, if it is part of the program.
    pub fn find(&self, path: &Path) -> Option<FileId> {
        self.files
            .iter()
            .position(|file| file.path == path)
            .map(|i| FileId(i as u32))
    }

    /// Formats a diagnostic for a terminal, naming the file its span is in.
    ///
    /// For example, an error in `util.stn` renders as `util.stn:2:9: error: ...` followed by the
    /// line and a caret.
    pub fn render(&self, diagnostic: &Diagnostic) -> String {
        let files = |id| {
            let file = self.file(id);
            (file.path.display().to_string(), file.source.as_str())
        };
        if diagnostic
            .calls()
            .iter()
            .any(|call| !self.shows_call(*call))
        {
            return diagnostic
                .keeping_calls(|call| self.shows_call(call))
                .render_in(files);
        }
        diagnostic.render_in(files)
    }

    /// Returns whether a runtime error shows a call at `span` among the calls that led to it:
    /// only a call the program writes, not one in the prelude or a builtin module written in
    /// stone, such as `xs.map(f)`'s call of `f`, and not one stone adds, which has no place.
    pub fn shows_call(&self, span: Span) -> bool {
        span.start.line > 0
            && span.file != FileId::PRELUDE
            && self
                .files
                .get(span.file.0 as usize)
                .is_some_and(|file| !file.is_builtin())
    }

    /// Returns what a diagnostic at `span` renders before and after its `error: message` line:
    /// the location, such as `util.stn:2:9: `, and the excerpt of the source line with carets.
    /// Compiled programs print a runtime error from these, so it reads exactly as
    /// [`SourceMap::render`] would show it.
    pub fn site(&self, span: Span) -> (String, String) {
        let file = self.file(span.file);
        (
            location(&file.path.display().to_string(), span),
            excerpt(&file.source, span),
        )
    }
}

/// What a `use` binds its name to.
#[derive(Debug, Clone, PartialEq)]
pub enum Target {
    /// A module, whose functions are called as `name.f()`.
    Module(FileId),
    /// A library function, by its linked name, such as `geometry.vec.dot`, or a function of a
    /// builtin module, such as `os.env`.
    Function(String),
    /// A builtin module, which has no file, by its name, such as `os`.
    BuiltinModule(String),
    /// A library enum, by its linked name, such as `geometry.Shape`.
    Enum(String),
    /// A library struct, by its linked name, such as `geometry.Point`.
    Struct(String),
    /// A library trait, by its linked name, such as `geometry.Shape`.
    Trait(String),
}

/// A name a `use` binds, for editor tooling.
#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    /// The name it binds, which is the name after `as` if there is one.
    pub name: String,
    /// The span of the last name in the `use` path, such as `dot` in `use geometry.vec.dot`.
    pub span: Span,
    /// The span of the name it binds, which is the name after `as` if there is one.
    pub name_span: Span,
    pub target: Target,
    /// Whether the file names it anywhere, which `stone check` warns about when it does not.
    pub used: bool,
}

/// A program linked into one module.
#[derive(Debug)]
pub struct Linked {
    pub module: Mod,
    /// The directory module paths start from.
    pub root: PathBuf,
    pub sources: SourceMap,
    /// Lex, syntax, and import errors, sorted by file and position. Type errors are left to the
    /// checker.
    pub diagnostics: Vec<Diagnostic>,
    /// Every `use` that resolved, in file order.
    pub imports: Vec<Import>,
    /// The linked name of every `pub` function of a library module, such as `geometry.vec.dot`.
    pub public: HashSet<String>,
    /// The packages from hosts the program uses that are not downloaded, which are errors too.
    pub missing: Vec<(Source, Version)>,
    /// An error at each `use` of a dependency that could not be read, saying why, for editors.
    /// These are not in `diagnostics`, which report each such dependency once, where its
    /// manifest names it.
    pub unavailable: Vec<Diagnostic>,
    /// The package each file belongs to, by index into `packages`.
    pub file_packages: Vec<usize>,
    /// The program's `stone.lock` and the text to write to it, if it should change.
    pub lock: Option<(PathBuf, String)>,
    /// The program, then every package it uses, for `stone tree`.
    pub packages: Vec<PackageInfo>,
    /// The test blocks of the entry file, in order, which `stone test` runs.
    pub tests: Vec<Test>,
}

/// A test block of a file, such as `test "adds one":`, which linking makes a function of
/// ([`crate::lift::lift_tests`]).
#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    /// What the test says it checks, as `adds one`.
    pub description: String,
    /// The linked name of the function it is, as `test.4.1`.
    pub function: String,
    /// Where its description is written.
    pub span: Span,
}

/// A package a program is linked with, as `stone tree` shows it.
#[derive(Debug, Clone, PartialEq)]
pub struct PackageInfo {
    /// Its name, which is empty for a program whose manifest names none.
    pub name: String,
    /// The version used, if it has one.
    pub version: Option<Version>,
    /// Where it comes from, which is `None` for the program.
    pub source: Option<Source>,
    /// Each package it uses, by the name it uses it under and its index in
    /// [`Linked::packages`], sorted by name.
    pub deps: Vec<(String, usize)>,
    /// The directory its module paths start from.
    pub root: PathBuf,
    /// What its modules' linked names start with, as `geo.1`, or empty for the program.
    pub prefix: String,
}

/// Returns `text` with each dependency's linked prefix written as the package's name, the way
/// the program's files name it, wherever the prefix starts a name.
///
/// For example, with `geo` 1.2.0 a dependency, `fn geo.1.shapes.area(p: geo.1.Point)` becomes
/// `fn geo.shapes.area(p: geo.Point)`. A prefix has a segment of digits, which no name of the
/// program's own has, so nothing else changes. Two packages of one name keep their prefixes,
/// which are what tell them apart.
pub fn shown_names(text: &str, packages: &[PackageInfo]) -> String {
    let dependencies = || packages.iter().filter(|p| !p.prefix.is_empty());
    let prefixes: Vec<(&str, &str)> = dependencies()
        .filter(|p| dependencies().filter(|q| q.name == p.name).count() == 1)
        .map(|p| (p.prefix.as_str(), p.name.as_str()))
        .collect();
    if prefixes.is_empty() {
        return text.to_string();
    }
    let is_word = |c: char| c.is_alphanumeric() || c == '_';
    let mut shown = String::with_capacity(text.len());
    let mut rest = text;
    let mut previous: Option<char> = None;
    while let Some(c) = rest.chars().next() {
        let starts_name = previous.is_none_or(|p| !is_word(p) && p != '.');
        let found = prefixes.iter().find(|(prefix, _)| {
            rest.starts_with(prefix)
                && rest[prefix.len()..]
                    .chars()
                    .next()
                    .is_none_or(|n| !is_word(n))
        });
        if let Some((prefix, name)) = found.filter(|_| starts_name) {
            shown.push_str(name);
            rest = &rest[prefix.len()..];
            previous = name.chars().last();
            continue;
        }
        shown.push(c);
        rest = &rest[c.len_utf8()..];
        previous = Some(c);
    }
    shown
}

/// The entry file's name for programs given as a single source string, such as by
/// [`crate::driver::interpret`].
pub const DEFAULT_ENTRY: &str = "main.stn";

/// The entry file of a library, which a package's `use` of its own name loads (see
/// [`crate::package::LIB`]), and which `stone check` checks in a directory with no `main.stn`.
pub const LIB_ENTRY: &str = "lib.stn";

/// Loads the program whose entry file is at `entry` and holds `source`, reading the modules it
/// uses from `sources`, and links them into one module.
///
/// For example, linking `main.stn` holding `use util` and `print(util.one())` with a `util.stn`
/// holding `pub fn one() -> int: ret 1` gives a module that defines `util.one` and calls it.
pub fn link(entry: &Path, source: &str, sources: &dyn Sources) -> Linked {
    let root = entry.parent().unwrap_or(Path::new(""));
    link_in(root, entry, source, sources)
}

/// Links like [`link`], but with `root` as the program's root instead of the entry file's
/// directory, so an editor can check a module that the program does not use yet.
///
/// For example, with `p` as the root, the entry file `p/geometry/shapes.stn` can use
/// `geometry.vec`, and is itself the module `geometry.shapes`.
pub fn link_in(root: &Path, entry: &Path, source: &str, sources: &dyn Sources) -> Linked {
    let relative = entry.strip_prefix(root).unwrap_or(entry).with_extension("");
    let entry_names = relative
        .components()
        .map(|c| c.as_os_str().to_string_lossy().into_owned())
        .collect();
    let mut loader = Loader {
        sources,
        root: root.to_path_buf(),
        entry_names,
        files: vec![],
        modules: vec![],
        by_name: HashMap::new(),
        uses: vec![],
        own: HashSet::new(),
        packages: vec![Package {
            root: root.to_path_buf(),
            prefix: String::new(),
            name: String::new(),
            deps: HashMap::new(),
            unavailable: HashMap::new(),
            source: None,
            version: None,
        }],
        file_package: vec![],
        nodes: HashMap::new(),
        named: HashMap::new(),
        missing: vec![],
        failed: HashMap::new(),
        unavailable: vec![],
        lock: None,
        tests: vec![],
        diagnostics: vec![],
    };
    loader.add(entry.to_path_buf(), String::new(), source.to_string(), 0);
    loader.load_packages();

    // each file's uses can add files, which are resolved in turn
    let mut next = 0;
    while next < loader.files.len() {
        loader.resolve_uses(FileId(next as u32));
        next += 1;
    }
    loader.link()
}

/// The error for a variant of the prelude written with its enum's name, as in `option.some(1)`.
/// Returns the error for a variant of the prelude's enum `name` written with the enum's name,
/// which is written alone.
fn prelude_qualified(name: &str) -> &'static str {
    match name {
        OPTION => "write 'some(x)' or 'none' without 'option.'",
        RESULT => "write 'ok(x)' or 'err(e)' without 'result.'",
        _ => "write 'less', 'equal', or 'greater' without 'ordering.'",
    }
}

/// Returns whether `name` is an enum of the prelude, whose variants are written alone.
fn is_prelude_enum(name: &str) -> bool {
    PRELUDE_VARIANTS
        .iter()
        .any(|(_, enum_name)| *enum_name == name)
}

/// What a name a file binds with `use` refers to while linking.
enum Binding {
    Module(FileId),
    /// A builtin module, such as `os`.
    BuiltinModule(&'static BuiltinModule),
    Function(String),
    /// A library enum, struct, or trait, by its linked name.
    Type(String),
    /// A `use` that failed to resolve, which was already reported, so its uses are left alone.
    Unresolved,
}

/// One resolved or failed `use` statement of a file.
struct Use {
    name: String,
    name_span: Span,
    /// The span of the last name in the path.
    last_span: Span,
    target: Option<Target>,
}

/// A package whose files are part of the program: the program itself, or one it depends on,
/// directly or not.
struct Package {
    /// The directory its module paths start from.
    root: PathBuf,
    /// What its modules' linked names start with, as `geo.1` (see [`Loader::load_packages`]),
    /// or empty for the program, whose modules are named by their paths alone.
    prefix: String,
    /// Its name, from its manifest, as `geo`.
    name: String,
    /// Each package its files use, by the name they use it under.
    deps: HashMap<String, usize>,
    /// The names of its dependencies that could not be read, whose errors are reported where
    /// the manifest names them, so a `use` of one says nothing more, with the reason, if one
    /// was given, for editors (see [`Linked::unavailable`]).
    unavailable: HashMap<String, Option<String>>,
    /// Where it comes from and its version, for `stone tree`.
    source: Option<Source>,
    version: Option<Version>,
}

/// A package whose manifest was read while selecting versions.
#[derive(Clone)]
struct Node {
    dir: PathBuf,
    name: String,
    /// The version it was released as, or its manifest's for a directory.
    version: Version,
    /// The version its manifest gives, if any.
    written: Option<Version>,
}

/// State for loading every file a program uses.
struct Loader<'a> {
    sources: &'a dyn Sources,
    root: PathBuf,
    /// The module path that would refer to the entry file, such as `["main"]`.
    entry_names: Vec<String>,
    files: Vec<SourceFile>,
    modules: Vec<Mod>,
    /// Each loaded module's file, by dotted name.
    by_name: HashMap<String, FileId>,
    /// Each file's `use` statements, in order.
    uses: Vec<Vec<Use>>,
    /// The files that are part of stone, written in stone, such as `net`'s, which alone may use
    /// the internal builtin modules.
    own: HashSet<FileId>,
    /// The program first, then every package it depends on, directly or not.
    packages: Vec<Package>,
    /// Each file's package, by index into `packages`.
    file_package: Vec<usize>,
    /// Every package read while selecting versions.
    nodes: HashMap<resolve::Target, Node>,
    /// Where each target was first named, by which dependency, and as what: its directory as
    /// written, or the dependency's name for a package from a host.
    named: HashMap<resolve::Target, (Span, String, String)>,
    /// The packages from hosts that are not in the store.
    missing: Vec<(Source, Version)>,
    /// Why each package that could not be read could not be, when that is said where it is
    /// named.
    failed: HashMap<resolve::Target, String>,
    /// The errors at each `use` of a dependency that could not be read.
    unavailable: Vec<Diagnostic>,
    /// The lockfile to write, if it should change.
    lock: Option<(PathBuf, String)>,
    /// The entry file's tests.
    tests: Vec<Test>,
    diagnostics: Vec<Diagnostic>,
}

impl Loader<'_> {
    /// Parses a file of the package `package` and adds it to the program, recording its syntax
    /// errors.
    fn add(&mut self, path: PathBuf, module: String, source: String, package: usize) -> FileId {
        let id = FileId(self.files.len() as u32);
        let (tokens, lex_errors) = Lexer::with_file(&source, id).lex_recovering();
        let (mut parsed, parse_errors) = Parser::new(&tokens).parse_recovering();
        // a line the lexer cut short fails to parse too, which says nothing more
        let cut: HashSet<usize> = lex_errors.iter().map(|e| e.span.start.line).collect();
        let mut errors: Vec<Diagnostic> = lex_errors.into_iter().map(Diagnostic::from).collect();
        errors.extend(
            parse_errors
                .into_iter()
                .filter(|e| !cut.contains(&e.span.start.line)),
        );
        let Mod::Module { body } = &mut parsed;
        if package == 0 {
            let tests = crate::lift::lift_tests(body, &mut errors);
            if id == FileId::ENTRY {
                self.tests = tests;
            }
        } else {
            // a package's tests are its own, so its users never check or build them
            body.retain(|stmt| !matches!(stmt.kind, StmtKind::Test { .. }));
        }
        errors.sort_by_key(|d| (d.span.start.line, d.span.start.col));
        crate::lift::lift_lambdas(body);
        self.diagnostics.extend(errors);
        self.files.push(SourceFile {
            path,
            module,
            source,
            package: (package != 0).then(|| self.packages[package].name.clone()),
        });
        self.modules.push(parsed);
        self.uses.push(vec![]);
        self.file_package.push(package);
        id
    }

    /// Adds a package's `stone.toml` or `stone.lock` to the program's files, so its errors
    /// render like any other, with no statements. `package` names the dependency it belongs to,
    /// if it is not the program's.
    fn add_manifest(&mut self, path: PathBuf, source: String, package: Option<String>) -> FileId {
        let id = FileId(self.files.len() as u32);
        self.files.push(SourceFile {
            path,
            module: String::new(),
            source,
            package,
        });
        self.modules.push(Mod::Module { body: vec![] });
        self.uses.push(vec![]);
        self.file_package.push(0);
        id
    }

    /// Reads the program's `stone.toml`, if it has one, selects the version of every package it
    /// depends on, directly or not (see [`crate::package::resolve`]), and adds each package used.
    /// A package from a host is read from the store, and checked against `stone.lock`.
    ///
    /// A dependency's modules are linked under a prefix of its name and the compatibility class
    /// of its version, as `geo.1` for `geo` 1.2.0, so `use geo.shapes` in the program and
    /// `use shapes` in `geo` both load the module `geo.1.shapes`. A module's name never starts
    /// with a digit, so no prefix is ever a module of the program. Two packages with the same
    /// name and class, which can only be different directories, are told apart by a number, as
    /// `geo.1.2`.
    fn load_packages(&mut self) {
        let root = self.packages[0].root.clone();
        let path = root.join(MANIFEST);
        let Some(source) = self.sources.read(&path) else {
            return;
        };
        let file = self.add_manifest(path, source.clone(), None);
        let (manifest, errors) = Manifest::parse(&source, file);
        self.diagnostics.extend(errors);
        if let Some((name, _)) = &manifest.name {
            self.packages[0].name = name.clone();
        }
        self.packages[0].version = manifest.version.as_ref().map(|(v, _)| v.clone());
        let root_requirements = self.requirements(&root, &manifest, false);
        let selection = resolve::select(&root_requirements, |target| self.read_target(target));

        // every package used, in the order first reached
        let mut index: HashMap<resolve::Target, usize> = HashMap::new();
        for target in selection.used() {
            let Some(node) = self.nodes.get(target) else {
                continue;
            };
            let class = format!("{}.{}", node.name, node.version.class());
            let mut prefix = class.clone();
            let mut n = 1;
            while self.packages.iter().any(|p| p.prefix == prefix) {
                n += 1;
                prefix = format!("{class}.{n}");
            }
            let (source, version) = match target {
                resolve::Target::Path(dir) => (Source::Path(dir.clone()), node.written.clone()),
                resolve::Target::Remote(source, version) => (source.clone(), Some(version.clone())),
            };
            self.packages.push(Package {
                root: node.dir.clone(),
                prefix,
                name: node.name.clone(),
                deps: HashMap::new(),
                unavailable: HashMap::new(),
                source: Some(source),
                version,
            });
            index.insert(target.clone(), self.packages.len() - 1);
        }
        let failed = std::mem::take(&mut self.failed);
        let bind = |package: &mut Package, requirements: &[Requirement]| {
            for r in requirements {
                let target = selection.resolve(&r.target);
                match index.get(&target) {
                    Some(&i) => {
                        package.deps.insert(r.name.clone(), i);
                    }
                    None => {
                        let reason = failed.get(&target).or_else(|| failed.get(&r.target));
                        package.unavailable.insert(r.name.clone(), reason.cloned());
                    }
                }
            }
        };
        bind(&mut self.packages[0], &root_requirements);
        for (target, &i) in &index {
            if let Some(requirements) = selection.requirements.get(target) {
                bind(&mut self.packages[i], requirements);
            }
        }
        self.check_lock(&root, &selection);
    }

    /// Returns the dependencies of the package in `dir` with `manifest` as requirements, which
    /// for a package from a host (`remote`) cannot be directories.
    fn requirements(&mut self, dir: &Path, manifest: &Manifest, remote: bool) -> Vec<Requirement> {
        let mut requirements = vec![];
        for dependency in &manifest.dependencies {
            let names = [dependency.name.clone()];
            let local = self.sources.read(&dir.join(module_path(&names))).is_some()
                || self.sources.is_dir(&dir.join(&dependency.name));
            if local {
                let whose = match dir == self.packages[0].root {
                    true => "program",
                    false => "package",
                };
                self.diagnostics.push(Code::Dependency.error(
                    dependency.span,
                    format!(
                        "'{}' is both a dependency and a module of this {whose}, so rename one",
                        dependency.name
                    ),
                ));
                continue;
            }
            let (target, written) = match (&dependency.source, &dependency.version) {
                (Source::Path(path), _) if remote => {
                    self.diagnostics.push(Code::Dependency.error(
                        dependency.span,
                        "a package from a host cannot use a directory, since the directory is \
                         not part of its release",
                    ));
                    continue;
                }
                (Source::Path(path), _) => (
                    resolve::Target::Path(join_normal(dir, path)),
                    path.display().to_string(),
                ),
                (source, Some((version, _))) => (
                    resolve::Target::Remote(source.clone(), version.clone()),
                    dependency.name.clone(),
                ),
                // already reported
                (_, None) => continue,
            };
            self.named
                .entry(target.clone())
                .or_insert_with(|| (dependency.span, dependency.name.clone(), written));
            requirements.push(Requirement {
                name: dependency.name.clone(),
                span: dependency.span,
                target,
            });
        }
        requirements
    }

    /// Reads the manifest of the package `target` names, from its directory or the store,
    /// returning its requirements, or `None` with the error if it cannot.
    fn read_target(&mut self, target: &resolve::Target) -> Option<Vec<Requirement>> {
        let (span, key, written) = self.named.get(target)?.clone();
        let dir = match target {
            resolve::Target::Path(dir) => dir.clone(),
            resolve::Target::Remote(source, version) => match self.sources.store() {
                Some(store) => package_dir(&store, source, version),
                None => {
                    let message = format!(
                        "{key} {version} ({source}) cannot be downloaded, since there is no \
                         directory to keep packages in; set STONE_HOME"
                    );
                    self.failed.insert(target.clone(), message.clone());
                    self.diagnostics.push(Code::Dependency.error(span, message));
                    return None;
                }
            },
        };
        let path = dir.join(MANIFEST);
        let Some(source) = self.sources.read(&path) else {
            let (message, hint) = match target {
                resolve::Target::Path(_) => (
                    format!("'{written}' has no {MANIFEST}, so it is not a package"),
                    "",
                ),
                resolve::Target::Remote(source, version) => {
                    self.missing.push((source.clone(), version.clone()));
                    (
                        format!("{key} {version} ({source}) is not downloaded"),
                        "; run `stone fetch`",
                    )
                }
            };
            self.failed
                .insert(target.clone(), format!("{message}{hint}"));
            self.diagnostics.push(Code::Dependency.error(span, message));
            return None;
        };
        let file = self.add_manifest(path, source.clone(), None);
        let (manifest, errors) = Manifest::parse(&source, file);
        let clean = errors.is_empty();
        self.diagnostics.extend(errors);
        let Some((name, _)) = manifest.name.clone() else {
            // a mistake in the manifest may be why it has no name
            if clean {
                self.diagnostics.push(Code::Manifest.error(
                    Span::empty(Pos::new(1, 1)).in_file(file),
                    format!(
                        "a package that others use has a name, as name = \"{key}\" under \
                         [package]"
                    ),
                ));
            }
            return None;
        };
        self.files[file.0 as usize].package = Some(name.clone());
        let written_version = manifest.version.as_ref().map(|(v, _)| v.clone());
        let written = written_version.clone();
        let version = match target {
            resolve::Target::Remote(_, released) => {
                if let Some((said, span)) = &manifest.version
                    && said != released
                {
                    self.diagnostics.push(Code::Dependency.error(
                        *span,
                        format!(
                            "this package says it is version {said}, but it was released as \
                             {released}"
                        ),
                    ));
                }
                released.clone()
            }
            resolve::Target::Path(_) => written_version.unwrap_or(Version::new(0, 0, 0)),
        };
        let remote = matches!(target, resolve::Target::Remote(..));
        let requirements = self.requirements(&dir, &manifest, remote);
        self.nodes.insert(
            target.clone(),
            Node {
                dir,
                name,
                version,
                written,
            },
        );
        Some(requirements)
    }

    /// Checks every package from a host that `selection` uses against the program's
    /// `stone.lock`, noting the lockfile to write if it lacks one or holds one no longer used.
    fn check_lock(&mut self, root: &Path, selection: &resolve::Selection) {
        let path = root.join(LOCK);
        let existing = self.sources.read(&path);
        let used: Vec<(Source, Version, Node)> = selection
            .used()
            .filter_map(|target| match target {
                resolve::Target::Remote(source, version) => self
                    .nodes
                    .get(target)
                    .map(|node| (source.clone(), version.clone(), node.clone())),
                resolve::Target::Path(_) => None,
            })
            .collect();
        if used.is_empty() && existing.is_none() {
            return;
        }
        let lock = match &existing {
            Some(text) => {
                let file = self.add_manifest(path.clone(), text.clone(), None);
                let (lock, errors) = Lock::parse(text, file);
                if !errors.is_empty() {
                    self.diagnostics.extend(errors);
                    return;
                }
                lock
            }
            None => Lock::default(),
        };
        let mut entries = vec![];
        let mut changed = false;
        for (source, version, node) in used {
            let hash = content_hash(self.sources, &node.dir);
            if let Some(locked) = lock.find(&source, &version)
                && locked.hash != hash
            {
                self.diagnostics.push(Code::Dependency.error(
                    locked.span,
                    format!(
                        "{} {version} ({source}) does not hold what it held when it was locked: \
                         its hash was {}, and is now {hash}; if its release changed on \
                         purpose, remove its entry from {LOCK}",
                        node.name, locked.hash
                    ),
                ));
                changed = true;
                continue;
            }
            entries.push(Locked {
                name: node.name,
                source,
                version,
                hash,
                span: Span::default(),
            });
        }
        if changed {
            return;
        }
        let text = Lock { entries }.write();
        if existing.as_deref() != Some(text.as_str()) {
            self.lock = Some((path, text));
        }
    }

    /// Returns the linked name of the module of `package` at the path `names` from its root.
    ///
    /// For example, in a package with prefix `geo.1`, `["shapes"]` is `geo.1.shapes` and
    /// `["lib"]` is `geo.1`, while in the program `["shapes"]` is `shapes`.
    fn linked_module(&self, package: usize, names: &[String]) -> String {
        let prefix = &self.packages[package].prefix;
        if package == 0 {
            names.join(".")
        } else if names == [LIB] {
            prefix.clone()
        } else {
            format!("{prefix}.{}", names.join("."))
        }
    }

    /// Returns the file of the module of `package` with these dotted names, loading it if it
    /// exists. With `builtin`, a module written in stone, such as `net`, takes its name.
    ///
    /// For example, `["geometry", "shapes"]` loads `<root>/geometry/shapes.stn`.
    fn module(&mut self, package: usize, names: &[String], builtin: bool) -> Option<FileId> {
        // a module written in stone has no file of the program, and no modules inside it
        if builtin && let Some(source) = crate::stdlib::source_module(&names[0]) {
            if names.len() != 1 {
                return None;
            }
            let name = names[0].clone();
            if let Some(&id) = self.by_name.get(&name) {
                return Some(id);
            }
            let id = self.add(
                PathBuf::from(format!("<{name}>")),
                name.clone(),
                source.to_string(),
                0,
            );
            self.own.insert(id);
            self.by_name.insert(name, id);
            return Some(id);
        }
        if package == 0 && self.is_entry(names) {
            return None;
        }
        let name = self.linked_module(package, names);
        if let Some(&id) = self.by_name.get(&name) {
            return Some(id);
        }
        let path = self.packages[package].root.join(module_path(names));
        let source = self.sources.read(&path)?;
        let id = self.add(path, name.clone(), source, package);
        self.by_name.insert(name, id);
        Some(id)
    }

    /// Returns whether a module name refers to the entry file, such as `main` for `main.stn`,
    /// which a module that is part of stone, such as `net`, never does.
    fn is_entry(&self, names: &[String]) -> bool {
        names == self.entry_names
            && !(names.len() == 1 && crate::stdlib::source_module(&names[0]).is_some())
    }

    /// Resolves the `use` statements at the top level of `file`, loading the modules they name.
    fn resolve_uses(&mut self, file: FileId) {
        let Mod::Module { body } = &self.modules[file.0 as usize];
        let uses: Vec<Stmt> = body
            .iter()
            .filter(|stmt| matches!(stmt.kind, StmtKind::Use { .. }))
            .cloned()
            .collect();

        for stmt in uses {
            let StmtKind::Use { path, alias } = stmt.kind else {
                continue;
            };
            let Some(last) = path.last().cloned() else {
                continue;
            };
            let unavailable = self.packages[self.file_package[file.0 as usize]]
                .unavailable
                .get(&path[0].0)
                .cloned();
            let target = match (self.resolve(file, &path), unavailable) {
                (Ok(target), _) => Some(target),
                // the manifest's error says why, and editors show it at the `use` too
                (Err(_), Some(reason)) => {
                    let name = &path[0].0;
                    let message = match reason {
                        Some(reason) => format!("'{name}' cannot be used: {reason}"),
                        None => format!(
                            "'{name}' cannot be used, because of an error in its {MANIFEST}"
                        ),
                    };
                    let span = path[0].1.to(path[path.len() - 1].1);
                    self.unavailable.push(Code::Dependency.error(span, message));
                    None
                }
                (Err(e), None) => {
                    self.diagnostics.push(e);
                    None
                }
            };
            let (name, name_span) = alias.unwrap_or_else(|| last.clone());
            self.uses[file.0 as usize].push(Use {
                name,
                name_span,
                last_span: last.1,
                target,
            });
        }
    }

    /// Finds what a `use` path in `file` names: a module file if there is one, otherwise a public
    /// function of the module the path ends in. A path that starts with the name of a dependency
    /// of the file's package names a module of that package.
    ///
    /// For example, `use util.pad` is the module `util/pad.stn` if it exists, and otherwise the
    /// function `pad` of `util.stn`.
    fn resolve(&mut self, file: FileId, path: &[(String, Span)]) -> Result<Target, Diagnostic> {
        let names: Vec<String> = path.iter().map(|(name, _)| name.clone()).collect();
        let span = path[0].1.to(path[path.len() - 1].1);
        let package = self.file_package[file.0 as usize];
        if let Some(&dependency) = self.packages[package].deps.get(&names[0]) {
            return self.resolve_in(file, dependency, path, 1);
        }
        // the prelude's functions are linked as if in this module
        if names == [PRELUDE_MODULE] {
            return Err(Code::Reserved.error(
                span,
                format!(
                    "'{PRELUDE_MODULE}' is the name of stone's prelude, which every program has, \
                     so rename {PRELUDE_MODULE}.stn"
                ),
            ));
        }
        if crate::stdlib::is_internal_module(&names[0]) && !self.own.contains(&file) {
            return Err(
                Code::UnknownModule.error(span, format!("no module named '{}'", names.join(".")))
            );
        }
        if let Some(module) = crate::stdlib::module(&names[0]) {
            return self.resolve_builtin_module(package, module, path, span);
        }
        if crate::stdlib::source_module(&names[0]).is_some() {
            self.check_builtin_name(package, &names[0], span)?;
        }
        self.resolve_in(file, package, path, 0)
    }

    /// Finds what a `use` path in `file` names in `package`, whose module paths are the path's
    /// names after the first `skip`: the first name is a dependency's when `skip` is 1, and then
    /// no more names is the package's `lib.stn`.
    fn resolve_in(
        &mut self,
        file: FileId,
        package: usize,
        path: &[(String, Span)],
        skip: usize,
    ) -> Result<Target, Diagnostic> {
        let names: Vec<String> = path.iter().map(|(name, _)| name.clone()).collect();
        let span = path[0].1.to(path[path.len() - 1].1);
        let builtin = skip == 0;
        let lib_or = |names: &[String]| match names {
            [] => vec![LIB.to_string()],
            names => names.to_vec(),
        };
        let entry = self.entry_names.join(".");
        let entry_error = || {
            Code::UnknownModule.error(
                span,
                format!("'{entry}' is the entry file, so it cannot be imported"),
            )
        };

        let within = &names[skip..];
        if package == 0 && self.is_entry(within) {
            return Err(entry_error());
        }
        if let Some(id) = self.module(package, &lib_or(within), builtin) {
            return Ok(Target::Module(id));
        }
        if within.is_empty() {
            return Err(Code::UnknownModule.error(
                span,
                format!(
                    "package '{}' has no {LIB}.stn, so use one of its modules",
                    names[0]
                ),
            ));
        }
        if let [parent @ .., (function, function_span)] = path
            && !parent.is_empty()
        {
            let parent_within = &names[skip..parent.len()];
            if package == 0 && self.is_entry(parent_within) {
                return Err(entry_error());
            }
            if let Some(id) = self.module(package, &lib_or(parent_within), builtin) {
                let linked_module = self.files[id.0 as usize].module.clone();
                // errors name the module as the file wrote it
                let module = names[..parent.len()].join(".");
                let parsed = &self.modules[id.0 as usize];
                if let Some(&public) = public_types(parsed).get(function) {
                    let linked = format!("{linked_module}.{function}");
                    return if !(public || id == file) {
                        Err(Code::Private.error(
                            *function_span,
                            format!("'{function}' is private to module '{module}'"),
                        ))
                    } else if is_struct(parsed, function) {
                        Ok(Target::Struct(linked))
                    } else if is_trait(parsed, function) {
                        Ok(Target::Trait(linked))
                    } else {
                        Ok(Target::Enum(linked))
                    };
                }
                return match public_functions(parsed).get(function) {
                    Some(&public) if public || id == file => {
                        Ok(Target::Function(format!("{linked_module}.{function}")))
                    }
                    Some(_) => Err(Code::Private.error(
                        *function_span,
                        format!("'{function}' is private to module '{module}'"),
                    )),
                    None => Err(Code::NoMember.error(
                        *function_span,
                        format!("module '{module}' has no function '{function}'"),
                    )),
                };
            }
        }

        let dotted = names.join(".");
        let dir = self.packages[package]
            .root
            .join(within.iter().collect::<PathBuf>());
        if self.sources.is_dir(&dir) {
            return Err(Code::UnknownModule.error(
                span,
                format!("'{dotted}' is a directory, so use a module inside it"),
            ));
        }
        Err(Code::UnknownModule.error(span, format!("no module named '{dotted}'")))
    }

    /// Finds what a `use` path starting with the name of a builtin module names: the module, or
    /// one of its functions, by its linked name.
    ///
    /// For example, `use os` is the module and `use os.env` is the function `os.env`. A file
    /// `os.stn` next to the entry file would be ambiguous, so it is an error to import it.
    fn resolve_builtin_module(
        &self,
        package: usize,
        module: &'static BuiltinModule,
        path: &[(String, Span)],
        span: Span,
    ) -> Result<Target, Diagnostic> {
        let name = module.name;
        self.check_builtin_name(package, name, span)?;
        match path {
            [_] => Ok(Target::BuiltinModule(name.to_string())),
            [_, (function, function_span)] => {
                let linked = format!("{name}.{function}");
                if module_function(&linked).is_some() {
                    Ok(Target::Function(linked))
                } else {
                    Err(Code::NoMember.error(
                        *function_span,
                        format!("module '{name}' has no function '{function}'"),
                    ))
                }
            }
            _ => {
                let dotted: Vec<&str> = path.iter().map(|(name, _)| name.as_str()).collect();
                Err(Code::UnknownModule
                    .error(span, format!("no module named '{}'", dotted.join("."))))
            }
        }
    }

    /// Fails if a file at the root of `package` takes the name of the builtin module `name`,
    /// which would make `use` ambiguous.
    fn check_builtin_name(&self, package: usize, name: &str, span: Span) -> Result<(), Diagnostic> {
        let names = [name.to_string()];
        let taken = (package != 0 || names != *self.entry_names)
            && self
                .sources
                .read(&self.packages[package].root.join(module_path(&names)))
                .is_some();
        match taken {
            true => Err(Code::Reserved.error(
                span,
                format!("'{name}' is a builtin module, so rename {name}.stn"),
            )),
            false => Ok(()),
        }
    }

    /// Checks every file's top-level rules and names, then merges the files into one module.
    fn link(mut self) -> Linked {
        let functions: Vec<HashMap<String, bool>> =
            self.modules.iter().map(public_functions).collect();
        let types: Vec<HashMap<String, bool>> = self.modules.iter().map(public_types).collect();
        // each file's structs with their type parameters, and its traits, for `impl` blocks
        let structs: Vec<HashMap<String, Vec<TypeParam>>> =
            self.modules.iter().map(struct_params).collect();
        let traits: Vec<HashSet<String>> = self.modules.iter().map(trait_names).collect();
        let module_names: Vec<String> = self.files.iter().map(|f| f.module.clone()).collect();
        // every constant by its linked name, so an import can tell one from a function
        let file_constants: Vec<HashSet<String>> =
            self.modules.iter().map(constant_names).collect();
        let linked_constants: HashSet<String> = file_constants
            .iter()
            .enumerate()
            .flat_map(|(i, names)| {
                let module = &module_names[i];
                names.iter().map(move |name| match i {
                    0 => name.clone(),
                    _ => format!("{module}.{name}"),
                })
            })
            .collect();
        let mut imports = vec![];
        let mut library_body = vec![];
        let mut entry_body = vec![];

        for i in 0..self.files.len() {
            let file = FileId(i as u32);
            let library = i != 0;
            let Mod::Module { body } =
                std::mem::replace(&mut self.modules[i], Mod::Module { body: vec![] });
            check_top_level(&body, library, &mut self.diagnostics);

            let mut defined: HashSet<String> = functions[i].keys().cloned().collect();
            defined.extend(types[i].keys().cloned());
            if !library {
                let mut globals = vec![];
                collect_assigned(&body, &mut globals);
                defined.extend(globals.into_iter().map(|(name, _)| name));
            }
            let mut bindings = HashMap::new();
            let first_import = imports.len();
            for u in &self.uses[i] {
                let error = if is_reserved(&u.name) {
                    Some(format!("'{}' is a builtin, so rename it with 'as'", u.name))
                } else if bindings.contains_key(&u.name) {
                    Some(format!("'{}' is already imported", u.name))
                } else if defined.contains(&u.name) {
                    Some(format!("'{}' is already defined in this file", u.name))
                } else {
                    None
                };
                if let Some(message) = error {
                    self.diagnostics
                        .push(Code::Duplicate.error(u.name_span, message));
                    continue;
                }
                let binding = match &u.target {
                    Some(target) => {
                        imports.push(Import {
                            name: u.name.clone(),
                            span: u.last_span,
                            name_span: u.name_span,
                            target: target.clone(),
                            used: false,
                        });
                        match target {
                            Target::Module(id) => Binding::Module(*id),
                            Target::Function(name) => Binding::Function(name.clone()),
                            Target::Enum(name) | Target::Struct(name) | Target::Trait(name) => {
                                Binding::Type(name.clone())
                            }
                            Target::BuiltinModule(name) => Binding::BuiltinModule(
                                crate::stdlib::module(name).expect("resolved to a builtin module"),
                            ),
                        }
                    }
                    None => Binding::Unresolved,
                };
                bindings.insert(u.name.clone(), binding);
            }

            // each struct with its type parameters, which its impl blocks' functions can name
            let own_structs = &structs[i];
            let mut globals = vec![];
            if !library {
                collect_assigned(&body, &mut globals);
            }
            let mut constants = file_constants[i].clone();
            constants.extend(bindings.iter().filter_map(|(name, binding)| match binding {
                Binding::Function(linked) if linked_constants.contains(linked) => {
                    Some(name.clone())
                }
                _ => None,
            }));
            let mut rewriter = Rewriter {
                file,
                library,
                module: &module_names[i],
                own: &functions[i],
                own_types: &types[i],
                bindings: &bindings,
                used: RefCell::new(HashSet::new()),
                functions: &functions,
                types: &types,
                module_names: &module_names,
                locals: HashSet::new(),
                globals: globals.into_iter().map(|(name, _)| name).collect(),
                type_params: HashSet::new(),
                constants,
                diagnostics: &mut self.diagnostics,
            };
            for mut stmt in body {
                match &mut stmt.kind {
                    StmtKind::Use { .. } => continue,
                    StmtKind::FunctionDef { name, .. } => {
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                        rewriter.function(&mut stmt, &[]);
                    }
                    StmtKind::TraitDef {
                        name,
                        type_params,
                        methods,
                        ..
                    } => {
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                        // the trait's parameters are its methods' too, as a struct's are
                        rewriter.set_type_params(&[], type_params);
                        rewriter.bounds(type_params);
                        methods
                            .iter_mut()
                            .for_each(|m| rewriter.function(m, type_params));
                    }
                    StmtKind::ImplDef {
                        name,
                        name_span,
                        trait_name: Some(trait_name),
                        methods,
                    } => {
                        let own_trait = traits[i].contains(&trait_name.name);
                        let found = rewriter.impl_type(name, &structs, &traits, &module_names);
                        // the trait's arguments may name the struct's type parameters
                        let outer = found.as_ref().map_or(vec![], |(_, outer, _)| outer.clone());
                        rewriter.set_type_params(&outer, &[]);
                        rewriter.trait_name(trait_name);
                        let names_own_struct = trait_name
                            .args
                            .iter()
                            .any(|arg| names_struct(arg, own_structs, library, &module_names[i]));
                        let error = match &found {
                            Err(message) => Some(message.clone()),
                            // the checker says a builtin type already implements the prelude's,
                            // and which of its conversions a program may give one
                            Ok((linked, _, _))
                                if [EQ, ORD, DISPLAY, FROM, TRY_FROM]
                                    .iter()
                                    .chain(OPERATOR_TRAITS)
                                    .any(|t| *t == trait_name.name)
                                    && matches!(
                                        linked.as_str(),
                                        "int" | "float" | "bool" | "str"
                                    ) =>
                            {
                                None
                            }
                            Ok((_, _, own_type))
                                if !own_type && !own_trait && !names_own_struct =>
                            {
                                Some(format!(
                                    "'{}' and '{name}' are both defined in other files, so their \
                                 impl block goes in the file of either",
                                    trait_name.name.rsplit('.').next().unwrap_or_default()
                                ))
                            }
                            Ok(_) => None,
                        };
                        if let Some(message) = error {
                            rewriter
                                .diagnostics
                                .push(Code::ImplNotAllowed.error(*name_span, message));
                            continue;
                        }
                        let Ok((linked, outer, _)) = found else {
                            continue;
                        };
                        // a type implements a generic trait once for each set of arguments,
                        // so each block's functions are named by them too
                        if !trait_name.args.is_empty() {
                            let this = match outer.as_slice() {
                                [] => linked.clone(),
                                params => {
                                    let params: Vec<&str> =
                                        params.iter().map(|p| p.name.as_str()).collect();
                                    format!("{linked}.{}", params.join("_"))
                                }
                            };
                            let suffix = method_suffix(&trait_name.args, &this);
                            for method in methods.iter_mut() {
                                if let StmtKind::FunctionDef { name, .. } = &mut method.kind {
                                    name.push_str(&suffix);
                                }
                            }
                        }
                        *name = linked;
                        methods
                            .iter_mut()
                            .for_each(|m| rewriter.function(m, &outer));
                    }
                    StmtKind::ImplDef {
                        name,
                        name_span,
                        methods,
                        ..
                    } => {
                        let error = if own_structs.contains_key(name.as_str()) {
                            None
                        } else if traits[i].contains(name.as_str()) {
                            Some(format!(
                                "'{name}' is a trait, so name the type it is for, as in \
                                 'impl {name} for Point:'"
                            ))
                        } else if types[i].contains_key(name.as_str()) {
                            Some(format!(
                                "'{name}' is an enum, and only a struct has an impl block for now"
                            ))
                        } else if let Some(Binding::Type(linked)) = bindings.get(name.as_str()) {
                            let module = linked.rsplit_once('.').map_or("", |(m, _)| m);
                            Some(format!(
                                "'{name}' is defined in module '{module}', so its impl block \
                                 goes in that file"
                            ))
                        } else {
                            Some(format!("no struct named '{name}' is defined in this file"))
                        };
                        if let Some(message) = error {
                            rewriter
                                .diagnostics
                                .push(Code::ImplNotAllowed.error(*name_span, message));
                            continue;
                        }
                        let outer = own_structs[name.as_str()].clone();
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                        methods
                            .iter_mut()
                            .for_each(|m| rewriter.function(m, &outer));
                    }
                    StmtKind::EnumDef {
                        name,
                        type_params,
                        variants,
                        ..
                    } => {
                        rewriter.locals.clear();
                        rewriter.set_type_params(&[], type_params);
                        rewriter.bounds(type_params);
                        for variant in variants.iter_mut() {
                            variant
                                .fields
                                .iter_mut()
                                .for_each(|f| rewriter.type_expr(f));
                        }
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                    }
                    StmtKind::StructDef {
                        name,
                        type_params,
                        fields,
                        ..
                    } => {
                        rewriter.locals.clear();
                        rewriter.set_type_params(&[], type_params);
                        rewriter.bounds(type_params);
                        fields
                            .iter_mut()
                            .for_each(|f| rewriter.type_expr(&mut f.annotation));
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                    }
                    StmtKind::ConstDef { name, .. } => {
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                        rewriter.locals.clear();
                        rewriter.type_params.clear();
                        rewriter.stmt(&mut stmt);
                    }
                    // already reported, and leaving it out keeps a module from running code
                    _ if library => continue,
                    _ => {
                        rewriter.locals.clear();
                        rewriter.type_params.clear();
                        rewriter.stmt(&mut stmt);
                    }
                }
                if library {
                    library_body.push(stmt);
                } else {
                    entry_body.push(stmt);
                }
            }
            let used = rewriter.used.into_inner();
            for import in &mut imports[first_import..] {
                import.used = used.contains(&import.name);
            }
        }

        report_method_collisions(&library_body, &entry_body, &mut self.diagnostics);
        let packages = self
            .packages
            .iter()
            .map(|package| {
                let mut deps: Vec<(String, usize)> = package
                    .deps
                    .iter()
                    .map(|(name, &i)| (name.clone(), i))
                    .collect();
                deps.sort();
                PackageInfo {
                    name: package.name.clone(),
                    version: package.version.clone(),
                    source: package.source.clone(),
                    deps,
                    root: package.root.clone(),
                    prefix: package.prefix.clone(),
                }
            })
            .collect();
        let mut body = prelude(&mut self.diagnostics);
        let public = functions
            .iter()
            .zip(&module_names)
            .skip(1)
            .flat_map(|(functions, module)| {
                functions
                    .iter()
                    .filter(|(_, public)| **public)
                    .map(move |(name, _)| format!("{module}.{name}"))
            })
            .collect();
        body.extend(library_body);
        body.extend(entry_body);
        self.diagnostics
            .sort_by_key(|d| (d.span.file, d.span.start));
        Linked {
            root: self.root,
            module: Mod::Module { body },
            sources: SourceMap { files: self.files },
            diagnostics: self.diagnostics,
            imports,
            public,
            missing: self.missing,
            unavailable: self.unavailable,
            file_packages: self.file_package,
            lock: self.lock,
            packages,
            tests: self.tests,
        }
    }
}

/// The module name the prelude's functions are linked under, as `prelude.sort`, which no file
/// may have.
pub const PRELUDE_MODULE: &str = "prelude";

/// Parses the prelude, whose definitions every program is linked with and which keep their own
/// names, such as `option`, except its functions, which are named as if in a module `prelude`. It always parses, so an error in it is a bug in the compiler, but one
/// is still reported rather than lost.
fn prelude(diagnostics: &mut Vec<Diagnostic>) -> Vec<Stmt> {
    let parsed = Lexer::with_file(PRELUDE, FileId::PRELUDE)
        .lex()
        .map_err(Diagnostic::from)
        .and_then(|tokens| Parser::new(&tokens).parse());
    match parsed {
        Ok(Mod::Module { mut body }) => {
            // a function calls no other of the prelude's, so only its name changes, and its
            // variants written alone become its enums', as in any file
            let none = HashMap::new();
            let mut rewriter = Rewriter {
                file: FileId::PRELUDE,
                library: false,
                module: PRELUDE_MODULE,
                own: &none,
                own_types: &none,
                bindings: &HashMap::new(),
                used: RefCell::new(HashSet::new()),
                functions: &[],
                types: &[],
                module_names: &[],
                locals: HashSet::new(),
                globals: HashSet::new(),
                type_params: HashSet::new(),
                constants: HashSet::new(),
                diagnostics,
            };
            for stmt in &mut body {
                if let StmtKind::FunctionDef { name, .. } = &mut stmt.kind {
                    *name = format!("{PRELUDE_MODULE}.{name}");
                    rewriter.function(stmt, &[]);
                }
            }
            body
        }
        Err(error) => {
            diagnostics.push(error);
            vec![]
        }
    }
}

/// Reports a library function whose linked name is also the name of a method, which is its
/// struct's linked name and the method's, as `geo.Point.origin` is for both `origin` of the
/// module `geo.Point` and the method `origin` of the struct `Point` of the module `geo`.
fn report_method_collisions(library: &[Stmt], entry: &[Stmt], diagnostics: &mut Vec<Diagnostic>) {
    let mut methods = HashMap::new();
    for stmt in library.iter().chain(entry) {
        if let StmtKind::ImplDef {
            name, methods: m, ..
        }
        | StmtKind::TraitDef {
            name, methods: m, ..
        } = &stmt.kind
        {
            for method in m {
                if let StmtKind::FunctionDef { name: method, .. } = &method.kind {
                    methods.insert(format!("{name}.{method}"), name.clone());
                }
            }
        }
    }
    for stmt in library {
        if let StmtKind::FunctionDef {
            name, name_span, ..
        } = &stmt.kind
            && let Some(owner) = methods.get(name)
        {
            let owner = match owner.rsplit_once('.') {
                Some((module, short)) => format!("'{short}' in module '{module}'"),
                None => format!("'{owner}'"),
            };
            diagnostics.push(Code::Duplicate.error(
                *name_span,
                format!("'{name}' is both a function of this module and a method of {owner}"),
            ));
        }
    }
}

/// Returns the path of a module's file relative to the root, such as `geometry/shapes.stn` for
/// `["geometry", "shapes"]`.
fn module_path(names: &[String]) -> PathBuf {
    let mut path: PathBuf = names.iter().collect();
    path.set_extension("stn");
    path
}

/// Returns each enum, struct, and trait a module defines at the top level, mapped to whether it
/// is `pub`.
fn public_types(module: &Mod) -> HashMap<String, bool> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::EnumDef { name, public, .. }
            | StmtKind::StructDef { name, public, .. }
            | StmtKind::TraitDef { name, public, .. } => Some((name.clone(), *public)),
            _ => None,
        })
        .collect()
}

/// Returns each struct a module defines at the top level, mapped to its type parameters.
/// Returns what follows the name of a function of an `impl` block of a generic trait with the
/// type arguments `args`, written after linking: `..` and the arguments as labels write types,
/// `Self` being `this`, as the checker's `method_suffix` writes them.
///
/// For example, `add[int]` gives `..int`, so its `add` is named `add..int`, and
/// `add[list[geo.Point]]` gives `..list_geo.Point`.
pub fn method_suffix(args: &[TypeExpr], this: &str) -> String {
    let args: Vec<String> = args.iter().map(|a| mangle(a, this)).collect();
    format!("..{}", args.join("_"))
}

/// Returns `ty`, a type written after linking, as a label writes it, `Self` being `this`.
fn mangle(ty: &TypeExpr, this: &str) -> String {
    let all = |tys: &[TypeExpr]| tys.iter().map(|t| mangle(t, this)).collect::<Vec<_>>();
    match &ty.kind {
        TypeExprKind::Int => "int".to_string(),
        TypeExprKind::Float => "float".to_string(),
        TypeExprKind::Bool => "bool".to_string(),
        TypeExprKind::Str => "str".to_string(),
        TypeExprKind::Bytes => "bytes".to_string(),
        TypeExprKind::List(elem) => format!("list_{}", mangle(elem, this)),
        TypeExprKind::Set(elem) => format!("set_{}", mangle(elem, this)),
        TypeExprKind::Map(key, value) => {
            format!("map_{}_{}", mangle(key, this), mangle(value, this))
        }
        TypeExprKind::Tuple(fields) => format!("tuple{}_{}", fields.len(), all(fields).join("_")),
        TypeExprKind::Named { name, args } if name == "Self" && args.is_empty() => this.to_string(),
        TypeExprKind::Named { name, args } if args.is_empty() => name.clone(),
        TypeExprKind::Named { name, args } => format!("{name}.{}", all(args).join("_")),
        TypeExprKind::Function { params, ret } => {
            let ret = match ret {
                Some(ret) => mangle(ret, this),
                None => "unit".to_string(),
            };
            format!("fn{}_{}_{ret}", params.len(), all(params).join("_"))
        }
    }
}

/// Returns whether `ty`, a type written after linking, names anywhere in it a struct of
/// `structs`, those of the file of module `module`, which is a `library`.
///
/// For example, `list[geo.Point]` names `Point` of module `geo`.
fn names_struct(
    ty: &TypeExpr,
    structs: &HashMap<String, Vec<TypeParam>>,
    library: bool,
    module: &str,
) -> bool {
    let names = |ty: &TypeExpr| names_struct(ty, structs, library, module);
    match &ty.kind {
        TypeExprKind::List(elem) | TypeExprKind::Set(elem) => names(elem),
        TypeExprKind::Map(key, value) => names(key) || names(value),
        TypeExprKind::Tuple(fields) => fields.iter().any(names),
        TypeExprKind::Function { params, ret } => {
            params.iter().any(names) || ret.as_deref().is_some_and(names)
        }
        TypeExprKind::Named { name, args } => {
            let short = match library {
                true => name.strip_prefix(module).and_then(|n| n.strip_prefix('.')),
                false => Some(name.as_str()),
            };
            short.is_some_and(|short| structs.contains_key(short)) || args.iter().any(names)
        }
        TypeExprKind::Int
        | TypeExprKind::Float
        | TypeExprKind::Bool
        | TypeExprKind::Str
        | TypeExprKind::Bytes => false,
    }
}

fn struct_params(module: &Mod) -> HashMap<String, Vec<TypeParam>> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::StructDef {
                name, type_params, ..
            } => Some((name.clone(), type_params.clone())),
            _ => None,
        })
        .collect()
}

/// Returns the name of each trait a module defines at the top level.
fn trait_names(module: &Mod) -> HashSet<String> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::TraitDef { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// Returns whether a module defines a trait called `name` at the top level.
fn is_trait(module: &Mod, name: &str) -> bool {
    trait_names(module).contains(name)
}

/// Returns whether a module defines a struct called `name` at the top level.
fn is_struct(module: &Mod, name: &str) -> bool {
    let Mod::Module { body } = module;
    body.iter()
        .any(|stmt| matches!(&stmt.kind, StmtKind::StructDef { name: n, .. } if n == name))
}

/// Returns the constants a module defines, such as `port` for `const port = 53`.
fn constant_names(module: &Mod) -> HashSet<String> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::ConstDef { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect()
}

/// Returns each function and constant a module defines at the top level, mapped to whether it
/// is `pub`. Both are linked alike: `use m.f` binds either, `m.f` reads either, and a library
/// renames both to `<module>.<name>`.
fn public_functions(module: &Mod) -> HashMap<String, bool> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef { name, public, .. }
            | StmtKind::ConstDef { name, public, .. } => Some((name.clone(), *public)),
            _ => None,
        })
        .collect()
}

/// Reports a `use` after other statements, and in a library, any top-level statement that is not
/// a `use`, `fn`, `const`, `enum`, `struct`, `impl`, or `trait`.
fn check_top_level(body: &[Stmt], library: bool, diagnostics: &mut Vec<Diagnostic>) {
    let mut after_use = false;
    for stmt in body {
        match &stmt.kind {
            StmtKind::Use { .. } if after_use => diagnostics
                .push(Code::Placement.error(stmt.span, "use must come before other statements")),
            StmtKind::Use { .. } => {}
            StmtKind::FunctionDef { .. }
            | StmtKind::ConstDef { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. } => after_use = true,
            _ => {
                after_use = true;
                if library {
                    diagnostics.push(Code::Placement.error(
                        stmt.span,
                        "only use, fn, const, enum, struct, impl, and trait are allowed at the top \
                         level of a module",
                    ));
                }
            }
        }
    }
}

/// Rewrites the names in one file's code to the names they have in the linked module.
struct Rewriter<'a> {
    file: FileId,
    /// Whether the file is a library module rather than the entry file.
    library: bool,
    /// The file's module name, such as `geometry.shapes`.
    module: &'a str,
    /// The functions the file defines, which in a library are renamed to `<module>.<name>`.
    own: &'a HashMap<String, bool>,
    /// The enums and structs the file defines, which in a library are renamed like its
    /// functions.
    own_types: &'a HashMap<String, bool>,
    bindings: &'a HashMap<String, Binding>,
    /// The names of `bindings` the file has looked up, so far, which are the imports it uses.
    used: RefCell<HashSet<String>>,
    /// Every file's functions, mapped to whether they are `pub`.
    functions: &'a [HashMap<String, bool>],
    /// Every file's enums and structs, mapped to whether they are `pub`.
    types: &'a [HashMap<String, bool>],
    module_names: &'a [String],
    /// The parameters and locals of the function being rewritten, which shadow everything else.
    locals: HashSet<String>,
    /// The names the entry file's top-level code assigns, which, like locals, may take the name
    /// of a prelude enum, as in `result = []`.
    globals: HashSet<String>,
    /// The type parameters of the function being rewritten, such as `T` in
    /// `fn first[T](xs: list[T]) -> T:`, which its types never rename.
    type_params: HashSet<String>,
    /// The names that mean a constant in this file, its own and those it imports, which no
    /// parameter or variable of its functions may take.
    constants: HashSet<String>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl<'a> Rewriter<'a> {
    /// Returns what `name` is bound to by a `use`, if anything, noting that the file uses it.
    ///
    /// For example, with `use util` in the file, `binding("util")` gives the module and marks
    /// that import used.
    fn binding(&self, name: &str) -> Option<&'a Binding> {
        let binding = self.bindings.get(name);
        if binding.is_some() {
            self.used.borrow_mut().insert(name.to_string());
        }
        binding
    }

    /// Rewrites a function definition's signature and body, with its parameters and assigned
    /// names as locals, leaving its name alone.
    fn function(&mut self, stmt: &mut Stmt, outer: &[TypeParam]) {
        let StmtKind::FunctionDef {
            type_params,
            args,
            returns,
            body,
            ..
        } = &mut stmt.kind
        else {
            return;
        };
        self.set_type_params(outer, type_params);
        self.bounds(type_params);
        let mut locals = vec![];
        collect_assigned(body, &mut locals);
        let params = args.args.iter().map(|arg| (arg.arg.clone(), arg.span));
        for (name, span) in params.clone().chain(locals.iter().cloned()) {
            if self.constants.contains(&name) {
                self.diagnostics.push(Code::Constant.error(
                    span,
                    format!("'{name}' is a constant, so a variable cannot take its name"),
                ));
            }
        }
        self.locals = locals.into_iter().map(|(name, _)| name).collect();
        self.locals.extend(params.map(|(name, _)| name));
        for annotation in args.args.iter_mut().filter_map(|a| a.annotation.as_mut()) {
            self.type_expr(annotation);
        }
        if let Some(returns) = returns {
            self.type_expr(returns);
        }
        self.block(body);
    }

    /// Makes `outer`, the type parameters of the struct whose `impl` block is being rewritten,
    /// and `own`, those of the definition being rewritten, the names its types never rename,
    /// reporting any of `own` named like a type this file imports or, in a library, defines.
    fn set_type_params(&mut self, outer: &[TypeParam], own: &[TypeParam]) {
        self.type_params.clear();
        for param in own {
            // the checker reports the file's own types in the entry file, whose names stay
            if self.enum_name(&param.name).is_some() {
                self.diagnostics.push(
                    Code::Duplicate
                        .error(param.span, format!("'{}' is already a type", param.name)),
                );
            }
        }
        self.type_params
            .extend(outer.iter().chain(own).map(|p| p.name.clone()));
    }

    /// Rewrites the traits each of `params` is bounded by to their linked names.
    fn bounds(&mut self, params: &mut [TypeParam]) {
        for bound in params.iter_mut().flat_map(|p| &mut p.bounds) {
            self.trait_name(bound);
        }
    }

    /// Rewrites a trait named in a bound or an `impl` block to its linked name, as `type_expr`
    /// does a type. A name that is no trait this file can see is left for the checker.
    fn trait_name(&mut self, name: &mut TraitName) {
        name.args.iter_mut().for_each(|arg| self.type_expr(arg));
        let linked = match name.name.rsplit_once('.') {
            Some((module, short)) => self.module_type(module, short, name.span, "trait"),
            None => self.enum_name(&name.name),
        };
        if let Some(linked) = linked {
            name.name = linked;
        }
    }

    /// Finds the type `name` that an `impl` block of a trait is for, in this file or imported:
    /// its linked name, its type parameters, and whether this file defines it. A builtin type,
    /// such as `int`, is defined in no file. The error says why it cannot have the block.
    fn impl_type(
        &self,
        name: &str,
        structs: &[HashMap<String, Vec<TypeParam>>],
        traits: &[HashSet<String>],
        module_names: &[String],
    ) -> Result<(String, Vec<TypeParam>, bool), String> {
        let own = &structs[self.file.0 as usize];
        if let Some(params) = own.get(name) {
            let linked = match self.library {
                true => format!("{}.{name}", self.module),
                false => name.to_string(),
            };
            return Ok((linked, params.clone(), true));
        }
        if matches!(name, "int" | "float" | "bool" | "str") {
            return Ok((name.to_string(), vec![], false));
        }
        let enum_error =
            format!("'{name}' is an enum, and only a struct has an impl block for now");
        if traits[self.file.0 as usize].contains(name) {
            return Err(format!(
                "'{name}' is a trait, so it cannot have an impl block"
            ));
        }
        if self.own_types.contains_key(name) {
            return Err(enum_error);
        }
        if let Some(Binding::Type(linked)) = self.binding(name)
            && let Some((module, short)) = linked.rsplit_once('.')
            && let Some(file) = module_names.iter().position(|m| m == module)
        {
            return match structs[file].get(short) {
                Some(params) => Ok((linked.clone(), params.clone(), false)),
                None => Err(enum_error),
            };
        }
        Err(format!("no struct named '{name}'"))
    }

    fn block(&mut self, body: &mut [Stmt]) {
        for stmt in body {
            self.stmt(stmt);
        }
    }

    fn stmt(&mut self, stmt: &mut Stmt) {
        match &mut stmt.kind {
            StmtKind::FunctionDef { body, .. } => self.block(body),
            StmtKind::Return { value } => {
                if let Some(value) = value {
                    self.expr(value);
                }
            }
            StmtKind::Assign { targets, value } => {
                targets.iter_mut().for_each(|t| self.expr(t));
                self.expr(value);
            }
            StmtKind::AugAssign { target, value, .. } => {
                self.expr(target);
                self.expr(value);
            }
            StmtKind::For { target, iter, body } => {
                self.expr(target);
                self.expr(iter);
                self.block(body);
            }
            StmtKind::While { test, body } => {
                self.expr(test);
                self.block(body);
            }
            StmtKind::If { test, body, orelse } => {
                self.expr(test);
                self.block(body);
                self.block(orelse);
            }
            StmtKind::Expr { value } | StmtKind::ConstDef { value, .. } => self.expr(value),
            StmtKind::Assert { test, msg } => {
                self.expr(test);
                if let Some(msg) = msg {
                    self.expr(msg);
                }
            }
            StmtKind::Match { subject, cases } => {
                self.expr(subject);
                for case in cases {
                    self.pattern(&mut case.pattern);
                    self.block(&mut case.body);
                }
            }
            StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. }
            | StmtKind::Use { .. }
            | StmtKind::Test { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    /// Rewrites the enums and structs a type written in a signature, variant, or field names to
    /// their linked names.
    ///
    /// For example, `list[Shape]` becomes `list[geometry.Shape]` after `use geometry.Shape`, and
    /// `shapes.Shape` becomes `geometry.shapes.Shape` after `use geometry.shapes`.
    fn type_expr(&mut self, ty: &mut TypeExpr) {
        match &mut ty.kind {
            TypeExprKind::List(elem) | TypeExprKind::Set(elem) => self.type_expr(elem),
            TypeExprKind::Map(key, value) => {
                self.type_expr(key);
                self.type_expr(value);
            }
            TypeExprKind::Tuple(fields) => fields.iter_mut().for_each(|f| self.type_expr(f)),
            TypeExprKind::Function { params, ret } => {
                params.iter_mut().for_each(|p| self.type_expr(p));
                if let Some(ret) = ret {
                    self.type_expr(ret);
                }
            }
            TypeExprKind::Named { name, args } => {
                args.iter_mut().for_each(|a| self.type_expr(a));
                match name.rsplit_once('.') {
                    Some((module, type_name)) => {
                        if let Some(linked) = self.module_type(module, type_name, ty.span, "type") {
                            *name = linked;
                        }
                    }
                    // `Self` names the type of a trait or impl block, never a module's type
                    None if self.type_params.contains(name) || name == "Self" => {}
                    None => {
                        if let Some(linked) = self.enum_name(name) {
                            *name = linked;
                        }
                    }
                }
            }
            TypeExprKind::Int
            | TypeExprKind::Float
            | TypeExprKind::Bool
            | TypeExprKind::Str
            | TypeExprKind::Bytes => {}
        }
    }

    /// Returns the linked name of the enum or struct this file calls `name`, if it is one it
    /// imports or, in a library, one it defines.
    fn enum_name(&self, name: &str) -> Option<String> {
        match self.binding(name) {
            Some(Binding::Type(linked)) => Some(linked.clone()),
            _ if self.library && self.own_types.contains_key(name) => {
                Some(format!("{}.{name}", self.module))
            }
            _ => None,
        }
    }

    /// Returns the linked name of the enum or struct `enum_name` of the module this file imports
    /// as `module`, reporting at `span` if it is private or missing, as a `what`, such as
    /// `enum`. Returns `None` if `module` is not an imported module.
    fn module_type(
        &mut self,
        module: &str,
        enum_name: &str,
        span: Span,
        what: &str,
    ) -> Option<String> {
        let Some(Binding::Module(target)) = self.binding(module) else {
            return None;
        };
        let module = &self.module_names[target.0 as usize];
        match self.types[target.0 as usize].get(enum_name) {
            Some(&public) if public || *target == self.file => {
                Some(format!("{module}.{enum_name}"))
            }
            Some(_) => {
                self.diagnostics.push(Code::Private.error(
                    span,
                    format!("'{enum_name}' is private to module '{module}'"),
                ));
                None
            }
            None => {
                self.diagnostics.push(Code::NoMember.error(
                    span,
                    format!("module '{module}' has no {what} '{enum_name}'"),
                ));
                None
            }
        }
    }

    /// Rewrites `value`, the enum in front of a variant, from `module.Enum` to the enum's linked
    /// name, and returns whether it did.
    ///
    /// For example, `shapes.Shape` in `shapes.Shape.Circle(1.0)` becomes `geometry.shapes.Shape`.
    fn qualified_enum(&mut self, value: &mut Expr) -> bool {
        let ExprKind::Attribute {
            value: module,
            attr,
            ..
        } = &value.kind
        else {
            return false;
        };
        let ExprKind::Name { id, .. } = &module.kind else {
            return false;
        };
        if self.locals.contains(id) || !matches!(self.binding(id), Some(Binding::Module(_))) {
            return false;
        }
        let span = value.span;
        if let Some(linked) = self.module_type(&id.clone(), &attr.clone(), span, "type") {
            *value = Expr::new(
                ExprKind::Name {
                    id: linked,
                    ctx: ExprContext::Load,
                },
                span,
            );
        }
        true
    }

    /// Rewrites the enum each variant in `pattern` names to its linked name.
    fn pattern(&mut self, pattern: &mut Pattern) {
        // a prelude variant without values, such as `less`, is written as a name, which can only
        // be the variant, since its name is reserved
        if let PatternKind::Name(name) = &pattern.kind
            && let Some(enum_name) = prelude_variant(name)
        {
            let span = pattern.span;
            let unwritten = Span::new(span.start, span.start).in_file(span.file);
            pattern.kind = PatternKind::Variant {
                path: vec![(enum_name.to_string(), unwritten), (name.clone(), span)],
                fields: vec![],
            };
            return;
        }
        if let PatternKind::Or(alternatives) = &mut pattern.kind {
            alternatives.iter_mut().for_each(|a| self.pattern(a));
            return;
        }
        if let PatternKind::As { pattern, .. } = &mut pattern.kind {
            self.pattern(pattern);
            return;
        }
        let PatternKind::Variant { path, fields } = &mut pattern.kind else {
            return;
        };
        fields.iter_mut().for_each(|f| self.pattern(&mut f.pattern));
        match path.as_mut_slice() {
            [(name, span), ..] if is_prelude_enum(name) => {
                self.diagnostics
                    .push(Code::Undefined.error(*span, prelude_qualified(name)));
            }
            [(name, span)] if let Some(enum_name) = prelude_variant(name) => {
                let unwritten = Span::new(span.start, span.start).in_file(span.file);
                path.insert(0, (enum_name.to_string(), unwritten));
            }
            // a struct, which a pattern of a trait value tests for, as `Circle(r)`
            [(name, _)] => {
                if let Some(linked) = self.enum_name(name) {
                    *name = linked;
                }
            }
            // a struct of an imported module, as `geo.Point(x)`
            [(module, module_span), (type_name, type_span)]
                if matches!(self.binding(module), Some(Binding::Module(_))) =>
            {
                let span = module_span.to(*type_span);
                if let Some(linked) = self.module_type(module, type_name, span, "struct") {
                    path.splice(0..2, [(linked, span)]);
                }
            }
            [(name, _), _] => {
                if let Some(linked) = self.enum_name(name) {
                    *name = linked;
                }
            }
            [(module, module_span), (enum_name, enum_span), _] => {
                let span = module_span.to(*enum_span);
                if let Some(linked) = self.module_type(module, enum_name, span, "enum") {
                    path.splice(0..2, [(linked, span)]);
                }
            }
            _ => {}
        }
    }

    fn expr(&mut self, expr: &mut Expr) {
        if self.prelude_variant(expr) {
            return;
        }
        if let ExprKind::Attribute { value, .. } = &expr.kind
            && let ExprKind::Name { id, .. } = &value.kind
            && is_prelude_enum(id)
            && !self.locals.contains(id)
            && !self.globals.contains(id)
        {
            self.diagnostics
                .push(Code::Undefined.error(expr.span, prelude_qualified(id)));
            return;
        }
        match &mut expr.kind {
            // lifted before rewriting, by `lift_lambdas`
            ExprKind::Lambda { .. } => {}
            ExprKind::Closure { function, captures } => {
                self.name(function, expr.span, false);
                captures.iter_mut().for_each(|c| self.expr(c));
            }
            ExprKind::Call {
                func,
                args,
                keywords,
            } => {
                if !self.prelude_variant(func) && !self.module_member(func) {
                    match &mut func.kind {
                        ExprKind::Name { id, .. } => self.name(id, func.span, true),
                        _ => self.expr(func),
                    }
                }
                args.iter_mut().for_each(|a| self.expr(a));
                keywords.iter_mut().for_each(|k| self.expr(&mut k.value));
            }
            ExprKind::Name { id, .. } => self.name(id, expr.span, false),
            ExprKind::BoolOp { values, .. } | ExprKind::JoinedStr { values } => {
                values.iter_mut().for_each(|v| self.expr(v))
            }
            ExprKind::BinOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::UnaryOp { operand, .. }
            | ExprKind::Try { value: operand }
            | ExprKind::FormattedValue { value: operand, .. } => self.expr(operand),
            ExprKind::Compare {
                left, comparators, ..
            } => {
                self.expr(left);
                comparators.iter_mut().for_each(|c| self.expr(c));
            }
            ExprKind::Subscript { value, slice, .. } => {
                self.expr(value);
                self.expr(slice);
            }
            ExprKind::Attribute { value, .. } => {
                // a module's function named as a value, as in `xs.map(shapes.area)`
                if !self.qualified_enum(value) && !self.module_member(expr) {
                    let ExprKind::Attribute { value, .. } = &mut expr.kind else {
                        unreachable!("checked above")
                    };
                    self.expr(value);
                }
            }
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } | ExprKind::Set { elts } => {
                elts.iter_mut().for_each(|e| self.expr(e))
            }
            ExprKind::Dict { keys, values } => {
                keys.iter_mut().for_each(|e| self.expr(e));
                values.iter_mut().for_each(|e| self.expr(e));
            }
            ExprKind::Slice { lower, upper, step } => [lower, upper, step]
                .into_iter()
                .flatten()
                .for_each(|b| self.expr(b)),
            ExprKind::Constant { .. } => {}
        }
    }

    /// Rewrites `expr` if it is a variant of the prelude written alone, such as `some` in
    /// `some(1)` or `none`, to that variant of its enum, `option.some` or `option.none`, and
    /// returns whether it did. The enum's name was never written, so its span covers no text,
    /// and editors find only the variant there.
    fn prelude_variant(&mut self, expr: &mut Expr) -> bool {
        let ExprKind::Name {
            id,
            ctx: ExprContext::Load,
        } = &expr.kind
        else {
            return false;
        };
        // a parameter may shadow it, as one may shadow a builtin
        let Some(enum_name) = prelude_variant(id) else {
            return false;
        };
        if self.locals.contains(id) {
            return false;
        }
        let span = expr.span;
        let unwritten = Span::new(span.start, span.start).in_file(span.file);
        let enum_name = Expr::new(
            ExprKind::Name {
                id: enum_name.to_string(),
                ctx: ExprContext::Load,
            },
            unwritten,
        );
        *expr = Expr::new(
            ExprKind::Attribute {
                value: Box::new(enum_name),
                attr: id.clone(),
                ctx: ExprContext::Load,
            },
            span,
        );
        true
    }

    /// Rewrites `func` if it names a function of an imported module, such as `shapes.area` in
    /// `shapes.area(2)` or in `xs.map(shapes.area)`, to that function's linked name, and returns
    /// whether it did.
    fn module_member(&mut self, func: &mut Expr) -> bool {
        let ExprKind::Attribute { value, attr, .. } = &func.kind else {
            return false;
        };
        let ExprKind::Name { id, .. } = &value.kind else {
            return false;
        };
        if self.locals.contains(id) {
            return false;
        }
        let target = match self.binding(id) {
            Some(Binding::Module(target)) => target,
            Some(Binding::BuiltinModule(module)) => {
                let attr_span = self.attr_span(func.span, attr);
                let linked = format!("{}.{attr}", module.name);
                if module_function(&linked).is_some() {
                    *func = Expr::new(
                        ExprKind::Name {
                            id: linked,
                            ctx: ExprContext::Load,
                        },
                        attr_span,
                    );
                } else {
                    self.diagnostics.push(Code::NoMember.error(
                        attr_span,
                        format!("module '{}' has no function '{attr}'", module.name),
                    ));
                }
                return true;
            }
            _ => return false,
        };

        let attr_span = self.attr_span(func.span, attr);
        let module = &self.module_names[target.0 as usize];
        // a struct of the module, as in `geometry.Point(x=1, y=2)`, or an enum used wrongly
        if self.types[target.0 as usize].contains_key(attr.as_str()) {
            let (id, attr) = (id.clone(), attr.clone());
            if let Some(linked) = self.module_type(&id, &attr, attr_span, "type") {
                *func = Expr::new(
                    ExprKind::Name {
                        id: linked,
                        ctx: ExprContext::Load,
                    },
                    func.span,
                );
            }
            return true;
        }
        match self.functions[target.0 as usize].get(attr) {
            Some(&public) if public || *target == self.file => {
                *func = Expr::new(
                    ExprKind::Name {
                        id: format!("{module}.{attr}"),
                        ctx: ExprContext::Load,
                    },
                    attr_span,
                );
            }
            Some(_) => self.diagnostics.push(Code::Private.error(
                attr_span,
                format!("'{attr}' is private to module '{module}'"),
            )),
            None => self.diagnostics.push(Code::NoMember.error(
                attr_span,
                format!("module '{module}' has no function '{attr}'"),
            )),
        }
        true
    }

    /// Returns the span of `attr`, the function's name, which is the last token of a call's
    /// `func` spanning `span`.
    fn attr_span(&self, span: Span, attr: &str) -> Span {
        let end = span.end;
        let start = Pos::new(end.line, end.col.saturating_sub(attr.chars().count()));
        Span::new(start, end).in_file(self.file)
    }

    /// Rewrites a name to what it refers to: an imported function's linked name, or in a library,
    /// one of the module's own functions. In a library, a name that is none of those, nor a local
    /// or builtin, is undefined, since a module cannot see the entry file's names.
    fn name(&mut self, id: &mut String, span: Span, called: bool) {
        if self.locals.contains(id) {
            return;
        }
        match self.binding(id) {
            Some(Binding::Function(name) | Binding::Type(name)) => *id = name.clone(),
            Some(Binding::Module(_) | Binding::BuiltinModule(_)) => self
                .diagnostics
                .push(Code::NotAValue.error(span, format!("'{id}' is a module, not a value"))),
            Some(Binding::Unresolved) => {}
            None if !self.library => {}
            None if self.own.contains_key(id) || self.own_types.contains_key(id) => {
                *id = format!("{}.{id}", self.module)
            }
            None if is_builtin(id) => {}
            None => {
                let what = if called { "function" } else { "name" };
                self.diagnostics
                    .push(Code::Undefined.error(span, format!("undefined {what} '{id}'")));
            }
        }
    }
}
