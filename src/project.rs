//! Programs of several files: loading the modules a program uses and linking them into one.
//!
//! The entry file's directory is the program's root, and every other file is a module named by
//! its path from there. For example, running `app/main.st` makes `app/` the root, so
//! `use geometry.shapes` loads `app/geometry/shapes.st` as the module `geometry.shapes`. There are
//! no files that only declare or re-export modules: a directory is just part of a module's name.
//!
//! [`link`] merges every module into a single [`Mod`] in which each library function is named by
//! its module, such as `geometry.shapes.area`, and every use of it is rewritten to that name. The
//! checker and both backends then run on it as they would on one file.
//!
//! `os` is a builtin module with no file (see [`crate::stdlib::os`]). Calls to its functions are
//! rewritten to their linked names too, such as `os.env`, which the checker and both backends
//! treat as builtins.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::ast::{Expr, ExprContext, ExprKind, Mod, Stmt, StmtKind};
use crate::checker::collect_assigned;
use crate::diagnostic::Diagnostic;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::span::{FileId, Pos, Span};
use crate::stdlib::{BUILTINS, is_builtin, os};

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
/// For example, after `sources.insert("geometry/vec.st", "...")`, `geometry` is a directory.
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
    /// `app/geometry/shapes.st`.
    pub path: PathBuf,
    /// The module's dotted name, such as `geometry.shapes`, or empty for the entry file.
    pub module: String,
    pub source: String,
}

/// Every file of a program, indexed by [`FileId`], with the entry file first.
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    files: Vec<SourceFile>,
}

impl SourceMap {
    /// Returns the file `id` names, or the entry file if there is no such file.
    pub fn file(&self, id: FileId) -> &SourceFile {
        self.files
            .get(id.0 as usize)
            .unwrap_or_else(|| &self.files[0])
    }

    /// Returns every file with its id, the entry file first.
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
    /// For example, an error in `util.st` renders as `util.st:2:9: error: ...` followed by the
    /// line and a caret.
    pub fn render(&self, diagnostic: &Diagnostic) -> String {
        let file = self.file(diagnostic.span.file);
        diagnostic.render(&file.path.display().to_string(), &file.source)
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
pub const DEFAULT_ENTRY: &str = "main.st";

/// Loads the program whose entry file is at `entry` and holds `source`, reading the modules it
/// uses from `sources`, and links them into one module.
///
/// For example, linking `main.st` holding `use util` and `print(util.one())` with a `util.st`
/// holding `pub def one(); ret 1` gives a module that defines `util.one` and calls it.
pub fn link(entry: &Path, source: &str, sources: &dyn Sources) -> Linked {
    let root = entry.parent().unwrap_or(Path::new(""));
    link_in(root, entry, source, sources)
}

/// Links like [`link`], but with `root` as the program's root instead of the entry file's
/// directory, so an editor can check a module that the program does not use yet.
///
/// For example, with `p` as the root, the entry file `p/geometry/shapes.st` can use
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

/// What a name a file binds with `use` refers to while linking.
enum Binding {
    Module(FileId),
    /// The builtin `os` module.
    BuiltinModule,
    Function(String),
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
    /// For example, `["geometry", "shapes"]` loads `<root>/geometry/shapes.st`.
    fn module(&mut self, names: &[String]) -> Option<FileId> {
        if self.is_entry(names) {
            return None;
        }
        let name = names.join(".");
        if let Some(&id) = self.by_name.get(&name) {
            return Some(id);
        }
        let path = self.root.join(module_path(names));
        let source = self.sources.read(&path)?;
        let id = self.add(path, name.clone(), source);
        self.by_name.insert(name, id);
        Some(id)
    }

    /// Returns whether a module name refers to the entry file, such as `main` for `main.st`.
    fn is_entry(&self, names: &[String]) -> bool {
        names == self.entry_names
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
    /// For example, `use util.pad` is the module `util/pad.st` if it exists, and otherwise the
    /// function `pad` of `util.st`.
    fn resolve(&mut self, file: FileId, path: &[(String, Span)]) -> Result<Target, Diagnostic> {
        let names: Vec<String> = path.iter().map(|(name, _)| name.clone()).collect();
        let span = path[0].1.to(path[path.len() - 1].1);
        if names[0] == os::MODULE {
            return self.resolve_os(path, span);
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
                return match public_functions(&self.modules[id.0 as usize]).get(function) {
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

    /// Finds what a `use` path starting with `os` names: the builtin module, or one of its
    /// functions, by its linked name.
    ///
    /// For example, `use os` is the module and `use os.env` is the function `os.env`. A file
    /// `os.st` next to the entry file would be ambiguous, so it is an error to import it.
    fn resolve_os(&self, path: &[(String, Span)], span: Span) -> Result<Target, Diagnostic> {
        let module = [os::MODULE.to_string()];
        if !self.is_entry(&module)
            && self
                .sources
                .read(&self.root.join(module_path(&module)))
                .is_some()
        {
            return Err(Diagnostic::error(
                span,
                format!("'{}' is a builtin module, so rename os.st", os::MODULE),
            ));
        }
        match path {
            [_] => Ok(Target::BuiltinModule(os::MODULE.to_string())),
            [_, (function, function_span)] => {
                let linked = format!("{}.{function}", os::MODULE);
                if os::is_function(&linked) {
                    Ok(Target::Function(linked))
                } else {
                    Err(Diagnostic::error(
                        *function_span,
                        format!("module '{}' has no function '{function}'", os::MODULE),
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

    /// Checks every file's top-level rules and names, then merges the files into one module.
    fn link(mut self) -> Linked {
        let functions: Vec<HashMap<String, bool>> =
            self.modules.iter().map(public_functions).collect();
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
            if !library {
                let mut globals = vec![];
                collect_assigned(&body, &mut globals);
                defined.extend(globals.into_iter().map(|(name, _)| name));
            }
            let mut bindings = HashMap::new();
            for u in &self.uses[i] {
                let error = if BUILTINS.contains(&u.name.as_str()) {
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
                            Target::BuiltinModule(_) => Binding::BuiltinModule,
                        }
                    }
                    None => Binding::Unresolved,
                };
                bindings.insert(u.name.clone(), binding);
            }

            let mut rewriter = Rewriter {
                file,
                library,
                module: &module_names[i],
                own: &functions[i],
                bindings: &bindings,
                functions: &functions,
                module_names: &module_names,
                locals: HashSet::new(),
                diagnostics: &mut self.diagnostics,
            };
            for mut stmt in body {
                match &mut stmt.kind {
                    StmtKind::Use { .. } => continue,
                    StmtKind::FunctionDef {
                        name, args, body, ..
                    } => {
                        let mut locals = vec![];
                        collect_assigned(body, &mut locals);
                        rewriter.locals = locals.into_iter().map(|(name, _)| name).collect();
                        rewriter
                            .locals
                            .extend(args.args.iter().map(|arg| arg.arg.clone()));
                        rewriter.block(body);
                        if library {
                            *name = format!("{}.{name}", module_names[i]);
                        }
                    }
                    // already reported, and leaving it out keeps a module from running code
                    _ if library => continue,
                    _ => {
                        rewriter.locals.clear();
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
        library_body.extend(entry_body);
        self.diagnostics
            .sort_by_key(|d| (d.span.file, d.span.start));
        Linked {
            root: self.root,
            module: Mod::Module { body: library_body },
            sources: SourceMap { files: self.files },
            diagnostics: self.diagnostics,
            imports,
            public,
        }
    }
}

/// Returns the path of a module's file relative to the root, such as `geometry/shapes.st` for
/// `["geometry", "shapes"]`.
fn module_path(names: &[String]) -> PathBuf {
    let mut path: PathBuf = names.iter().collect();
    path.set_extension("st");
    path
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
/// a `use` or `def`.
fn check_top_level(body: &[Stmt], library: bool, diagnostics: &mut Vec<Diagnostic>) {
    let mut after_use = false;
    for stmt in body {
        match &stmt.kind {
            StmtKind::Use { .. } if after_use => diagnostics.push(Diagnostic::error(
                stmt.span,
                "use must come before other statements",
            )),
            StmtKind::Use { .. } => {}
            StmtKind::FunctionDef { .. } => after_use = true,
            _ => {
                after_use = true;
                if library {
                    diagnostics.push(Diagnostic::error(
                        stmt.span,
                        "only use and def are allowed at the top level of a module",
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
    bindings: &'a HashMap<String, Binding>,
    /// Every file's functions, mapped to whether they are `pub`.
    functions: &'a [HashMap<String, bool>],
    module_names: &'a [String],
    /// The parameters and locals of the function being rewritten, which shadow everything else.
    locals: HashSet<String>,
    diagnostics: &'a mut Vec<Diagnostic>,
}

impl Rewriter<'_> {
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
            StmtKind::Use { .. } | StmtKind::Break | StmtKind::Continue => {}
        }
    }

    fn expr(&mut self, expr: &mut Expr) {
        match &mut expr.kind {
            ExprKind::Call { func, args } => {
                if !self.module_call(func) {
                    match &mut func.kind {
                        ExprKind::Name { id, .. } => self.name(id, func.span, true),
                        _ => self.expr(func),
                    }
                }
                args.iter_mut().for_each(|a| self.expr(a));
            }
            ExprKind::Name { id, .. } => self.name(id, expr.span, false),
            ExprKind::BoolOp { values, .. } => values.iter_mut().for_each(|v| self.expr(v)),
            ExprKind::BinOp { left, right, .. } => {
                self.expr(left);
                self.expr(right);
            }
            ExprKind::UnaryOp { operand, .. } => self.expr(operand),
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
            ExprKind::Attribute { value, .. } => self.expr(value),
            ExprKind::List { elts, .. } => elts.iter_mut().for_each(|e| self.expr(e)),
            ExprKind::Constant { .. } => {}
        }
    }

    /// Rewrites `func` if it calls a function of an imported module, such as `shapes.area` in
    /// `shapes.area(2)`, to that function's linked name, and returns whether it did.
    fn module_call(&mut self, func: &mut Expr) -> bool {
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
            Some(Binding::BuiltinModule) => {
                let attr_span = self.attr_span(func.span, attr);
                let linked = format!("{}.{attr}", os::MODULE);
                if os::is_function(&linked) {
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
                        format!("module '{}' has no function '{attr}'", os::MODULE),
                    ));
                }
                return true;
            }
            _ => return false,
        };

        let attr_span = self.attr_span(func.span, attr);
        let module = &self.module_names[target.0 as usize];
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
            Some(Binding::Function(name)) => *id = name.clone(),
            Some(Binding::Module(_) | Binding::BuiltinModule) => self.diagnostics.push(
                Diagnostic::error(span, format!("'{id}' is a module, not a value")),
            ),
            Some(Binding::Unresolved) => {}
            None if !self.library => {}
            None if self.own.contains_key(id) => *id = format!("{}.{id}", self.module),
            None if is_builtin(id) => {}
            None => {
                let what = if called { "function" } else { "name" };
                self.diagnostics
                    .push(Diagnostic::error(span, format!("undefined {what} '{id}'")));
            }
        }
    }
}
