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

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::LazyLock;

use crate::ast::{
    Expr, ExprContext, ExprKind, Mod, Pattern, PatternKind, Stmt, StmtKind, TraitName, TypeExpr,
    TypeExprKind, TypeParam,
};
use crate::checker::{DISPLAY, EQ, OPTION, ORD, RESULT, collect_assigned};
use crate::diagnostic::{Diagnostic, excerpt, location};
use crate::lexer::Lexer;
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
}

/// Reads modules from a map of paths to sources, where a directory is any path that one of the
/// files is under.
///
/// For example, after `sources.insert("geometry/vec.stn", "...")`, `geometry` is a directory.
#[derive(Debug, Default, Clone)]
pub struct MapSources {
    files: HashMap<PathBuf, String>,
}

impl MapSources {
    pub fn insert(&mut self, path: impl Into<PathBuf>, source: impl Into<String>) {
        self.files.insert(path.into(), source.into());
    }
}

impl Sources for MapSources {
    fn read(&self, path: &Path) -> Option<String> {
        self.files.get(path).cloned()
    }

    fn is_dir(&self, path: &Path) -> bool {
        self.files.keys().any(|p| p != path && p.starts_with(path))
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
    /// The module's dotted name, such as `geometry.shapes`, or empty for the entry file.
    pub module: String,
    pub source: String,
}

impl SourceFile {
    /// Returns whether the file is part of stone, written in stone, such as the `net` module's,
    /// rather than one of the program's, which is named by its module in angle brackets, as
    /// `<net>`.
    pub fn is_builtin(&self) -> bool {
        crate::stdlib::source_module(&self.module).is_some()
            && self.path == Path::new(&format!("<{}>", self.module))
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
        let file = self.file(diagnostic.span.file);
        diagnostic.render(&file.path.display().to_string(), &file.source)
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
    pub target: Target,
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
}

/// The entry file's name for programs given as a single source string, such as by
/// [`crate::driver::interpret`].
pub const DEFAULT_ENTRY: &str = "main.stn";

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
        diagnostics: vec![],
    };
    loader.add(entry.to_path_buf(), String::new(), source.to_string());

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
    diagnostics: Vec<Diagnostic>,
}

impl Loader<'_> {
    /// Parses a file and adds it to the program, recording its syntax errors.
    fn add(&mut self, path: PathBuf, module: String, source: String) -> FileId {
        let id = FileId(self.files.len() as u32);
        let (parsed, errors) = match Lexer::with_file(&source, id).lex() {
            Ok(tokens) => Parser::new(&tokens).parse_recovering(),
            Err(e) => (Mod::Module { body: vec![] }, vec![e.into()]),
        };
        self.diagnostics.extend(errors);
        self.files.push(SourceFile {
            path,
            module,
            source,
        });
        self.modules.push(parsed);
        self.uses.push(vec![]);
        id
    }

    /// Returns the file of the module with these dotted names, loading it if it exists.
    ///
    /// For example, `["geometry", "shapes"]` loads `<root>/geometry/shapes.stn`.
    fn module(&mut self, names: &[String]) -> Option<FileId> {
        if self.is_entry(names) {
            return None;
        }
        let name = names.join(".");
        if let Some(&id) = self.by_name.get(&name) {
            return Some(id);
        }
        // a module written in stone has no file of the program, and no modules inside it
        if let Some(source) = crate::stdlib::source_module(&names[0]) {
            if names.len() != 1 {
                return None;
            }
            let id = self.add(
                PathBuf::from(format!("<{name}>")),
                name.clone(),
                source.to_string(),
            );
            self.own.insert(id);
            self.by_name.insert(name, id);
            return Some(id);
        }
        let path = self.root.join(module_path(names));
        let source = self.sources.read(&path)?;
        let id = self.add(path, name.clone(), source);
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
            let target = match self.resolve(file, &path) {
                Ok(target) => Some(target),
                Err(e) => {
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
    /// function of the module the path ends in.
    ///
    /// For example, `use util.pad` is the module `util/pad.stn` if it exists, and otherwise the
    /// function `pad` of `util.stn`.
    fn resolve(&mut self, file: FileId, path: &[(String, Span)]) -> Result<Target, Diagnostic> {
        let names: Vec<String> = path.iter().map(|(name, _)| name.clone()).collect();
        let span = path[0].1.to(path[path.len() - 1].1);
        // the prelude's functions are linked as if in this module
        if names == [PRELUDE_MODULE] {
            return Err(Diagnostic::error(
                span,
                format!(
                    "'{PRELUDE_MODULE}' is the name of stone's prelude, which every program has, \
                     so rename {PRELUDE_MODULE}.stn"
                ),
            ));
        }
        let internal = crate::stdlib::INTERNAL_MODULES
            .iter()
            .any(|module| module.name == names[0]);
        if internal && !self.own.contains(&file) {
            return Err(Diagnostic::error(
                span,
                format!("no module named '{}'", names.join(".")),
            ));
        }
        if let Some(module) = crate::stdlib::module(&names[0]) {
            return self.resolve_builtin_module(module, path, span);
        }
        if crate::stdlib::source_module(&names[0]).is_some() {
            self.check_builtin_name(&names[0], span)?;
        }
        let entry = self.entry_names.join(".");
        let entry_error = || {
            Diagnostic::error(
                span,
                format!("'{entry}' is the entry file, so it cannot be imported"),
            )
        };

        if self.is_entry(&names) {
            return Err(entry_error());
        }
        if let Some(id) = self.module(&names) {
            return Ok(Target::Module(id));
        }
        if let [parent @ .., (function, function_span)] = path
            && !parent.is_empty()
        {
            let parent_names = &names[..parent.len()];
            if self.is_entry(parent_names) {
                return Err(entry_error());
            }
            if let Some(id) = self.module(parent_names) {
                let module = parent_names.join(".");
                let parsed = &self.modules[id.0 as usize];
                if let Some(&public) = public_types(parsed).get(function) {
                    let linked = format!("{module}.{function}");
                    return if !(public || id == file) {
                        Err(Diagnostic::error(
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
                        Ok(Target::Function(format!("{module}.{function}")))
                    }
                    Some(_) => Err(Diagnostic::error(
                        *function_span,
                        format!("'{function}' is private to module '{module}'"),
                    )),
                    None => Err(Diagnostic::error(
                        *function_span,
                        format!("module '{module}' has no function '{function}'"),
                    )),
                };
            }
        }

        let dotted = names.join(".");
        let dir = self.root.join(names.iter().collect::<PathBuf>());
        if self.sources.is_dir(&dir) {
            return Err(Diagnostic::error(
                span,
                format!("'{dotted}' is a directory, so use a module inside it"),
            ));
        }
        Err(Diagnostic::error(
            span,
            format!("no module named '{dotted}'"),
        ))
    }

    /// Finds what a `use` path starting with the name of a builtin module names: the module, or
    /// one of its functions, by its linked name.
    ///
    /// For example, `use os` is the module and `use os.env` is the function `os.env`. A file
    /// `os.stn` next to the entry file would be ambiguous, so it is an error to import it.
    fn resolve_builtin_module(
        &self,
        module: &'static BuiltinModule,
        path: &[(String, Span)],
        span: Span,
    ) -> Result<Target, Diagnostic> {
        let name = module.name;
        self.check_builtin_name(name, span)?;
        match path {
            [_] => Ok(Target::BuiltinModule(name.to_string())),
            [_, (function, function_span)] => {
                let linked = format!("{name}.{function}");
                if module_function(&linked).is_some() {
                    Ok(Target::Function(linked))
                } else {
                    Err(Diagnostic::error(
                        *function_span,
                        format!("module '{name}' has no function '{function}'"),
                    ))
                }
            }
            _ => {
                let dotted: Vec<&str> = path.iter().map(|(name, _)| name.as_str()).collect();
                Err(Diagnostic::error(
                    span,
                    format!("no module named '{}'", dotted.join(".")),
                ))
            }
        }
    }

    /// Fails if a file next to the entry file takes the name of the builtin module `name`, which
    /// would make `use` ambiguous.
    fn check_builtin_name(&self, name: &str, span: Span) -> Result<(), Diagnostic> {
        let names = [name.to_string()];
        let taken = names != *self.entry_names
            && self
                .sources
                .read(&self.root.join(module_path(&names)))
                .is_some();
        match taken {
            true => Err(Diagnostic::error(
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
                        .push(Diagnostic::error(u.name_span, message));
                    continue;
                }
                let binding = match &u.target {
                    Some(target) => {
                        imports.push(Import {
                            name: u.name.clone(),
                            span: u.last_span,
                            target: target.clone(),
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
            let mut rewriter = Rewriter {
                file,
                library,
                module: &module_names[i],
                own: &functions[i],
                own_types: &types[i],
                bindings: &bindings,
                functions: &functions,
                types: &types,
                module_names: &module_names,
                locals: HashSet::new(),
                globals: globals.into_iter().map(|(name, _)| name).collect(),
                type_params: HashSet::new(),
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
                    StmtKind::TraitDef { name, methods, .. } => {
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                        methods.iter_mut().for_each(|m| rewriter.function(m, &[]));
                    }
                    StmtKind::ImplDef {
                        name,
                        name_span,
                        trait_name: Some(trait_name),
                        methods,
                    } => {
                        let own_trait = traits[i].contains(&trait_name.name);
                        rewriter.trait_name(trait_name);
                        let found = rewriter.impl_type(name, &structs, &traits, &module_names);
                        let error = match &found {
                            Err(message) => Some(message.clone()),
                            // the checker says a builtin type already implements the prelude's
                            Ok((linked, _, _))
                                if [EQ, ORD, DISPLAY].contains(&trait_name.name.as_str())
                                    && matches!(
                                        linked.as_str(),
                                        "int" | "float" | "bool" | "str"
                                    ) =>
                            {
                                None
                            }
                            Ok((_, _, own_type)) if !own_type && !own_trait => Some(format!(
                                "'{}' and '{name}' are both defined in other files, so their \
                                 impl block goes in the file of either",
                                trait_name.name.rsplit('.').next().unwrap_or_default()
                            )),
                            Ok(_) => None,
                        };
                        if let Some(message) = error {
                            rewriter
                                .diagnostics
                                .push(Diagnostic::error(*name_span, message));
                            continue;
                        }
                        let Ok((linked, outer, _)) = found else {
                            continue;
                        };
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
                                .push(Diagnostic::error(*name_span, message));
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
        }

        report_method_collisions(&library_body, &entry_body, &mut self.diagnostics);
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
                functions: &[],
                types: &[],
                module_names: &[],
                locals: HashSet::new(),
                globals: HashSet::new(),
                type_params: HashSet::new(),
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
            diagnostics.push(Diagnostic::error(
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

/// Returns each function a module defines at the top level, mapped to whether it is `pub`.
fn public_functions(module: &Mod) -> HashMap<String, bool> {
    let Mod::Module { body } = module;
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef { name, public, .. } => Some((name.clone(), *public)),
            _ => None,
        })
        .collect()
}

/// Reports a `use` after other statements, and in a library, any top-level statement that is not
/// a `use`, `fn`, `enum`, `struct`, `impl`, or `trait`.
fn check_top_level(body: &[Stmt], library: bool, diagnostics: &mut Vec<Diagnostic>) {
    let mut after_use = false;
    for stmt in body {
        match &stmt.kind {
            StmtKind::Use { .. } if after_use => diagnostics.push(Diagnostic::error(
                stmt.span,
                "use must come before other statements",
            )),
            StmtKind::Use { .. } => {}
            StmtKind::FunctionDef { .. }
            | StmtKind::EnumDef { .. }
            | StmtKind::StructDef { .. }
            | StmtKind::ImplDef { .. }
            | StmtKind::TraitDef { .. } => after_use = true,
            _ => {
                after_use = true;
                if library {
                    diagnostics.push(Diagnostic::error(
                        stmt.span,
                        "only use, fn, enum, struct, impl, and trait are allowed at the top level \
                         of a module",
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
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl Rewriter<'_> {
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
        self.locals = locals.into_iter().map(|(name, _)| name).collect();
        self.locals
            .extend(args.args.iter().map(|arg| arg.arg.clone()));
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
                self.diagnostics.push(Diagnostic::error(
                    param.span,
                    format!("'{}' is already a type", param.name),
                ));
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
        if let Some(Binding::Type(linked)) = self.bindings.get(name)
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
            StmtKind::Expr { value } => self.expr(value),
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
            TypeExprKind::List(elem) => self.type_expr(elem),
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
            TypeExprKind::Int | TypeExprKind::Float | TypeExprKind::Bool | TypeExprKind::Str => {}
        }
    }

    /// Returns the linked name of the enum or struct this file calls `name`, if it is one it
    /// imports or, in a library, one it defines.
    fn enum_name(&self, name: &str) -> Option<String> {
        match self.bindings.get(name) {
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
        let Some(Binding::Module(target)) = self.bindings.get(module) else {
            return None;
        };
        let module = &self.module_names[target.0 as usize];
        match self.types[target.0 as usize].get(enum_name) {
            Some(&public) if public || *target == self.file => {
                Some(format!("{module}.{enum_name}"))
            }
            Some(_) => {
                self.diagnostics.push(Diagnostic::error(
                    span,
                    format!("'{enum_name}' is private to module '{module}'"),
                ));
                None
            }
            None => {
                self.diagnostics.push(Diagnostic::error(
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
        if self.locals.contains(id) || !matches!(self.bindings.get(id), Some(Binding::Module(_))) {
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
        let PatternKind::Variant { path, fields } = &mut pattern.kind else {
            return;
        };
        fields.iter_mut().for_each(|f| self.pattern(f));
        match path.as_mut_slice() {
            [(name, span), ..] if is_prelude_enum(name) => {
                self.diagnostics
                    .push(Diagnostic::error(*span, prelude_qualified(name)));
            }
            [(name, span)] if let Some(enum_name) = prelude_variant(name) => {
                let unwritten = Span::new(span.start, span.start).in_file(span.file);
                path.insert(0, (enum_name.to_string(), unwritten));
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
                .push(Diagnostic::error(expr.span, prelude_qualified(id)));
            return;
        }
        match &mut expr.kind {
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
            ExprKind::UnaryOp { operand, .. } | ExprKind::Try { value: operand } => {
                self.expr(operand)
            }
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
            ExprKind::List { elts, .. } | ExprKind::Tuple { elts, .. } => {
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
        let target = match self.bindings.get(id) {
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
                    self.diagnostics.push(Diagnostic::error(
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
            Some(_) => self.diagnostics.push(Diagnostic::error(
                attr_span,
                format!("'{attr}' is private to module '{module}'"),
            )),
            None => self.diagnostics.push(Diagnostic::error(
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
        match self.bindings.get(id) {
            Some(Binding::Function(name) | Binding::Type(name)) => *id = name.clone(),
            Some(Binding::Module(_) | Binding::BuiltinModule(_)) => self.diagnostics.push(
                Diagnostic::error(span, format!("'{id}' is a module, not a value")),
            ),
            Some(Binding::Unresolved) => {}
            None if !self.library => {}
            None if self.own.contains_key(id) || self.own_types.contains_key(id) => {
                *id = format!("{}.{id}", self.module)
            }
            None if is_builtin(id) => {}
            None => {
                let what = if called { "function" } else { "name" };
                self.diagnostics
                    .push(Diagnostic::error(span, format!("undefined {what} '{id}'")));
            }
        }
    }
}
