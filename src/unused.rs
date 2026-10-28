//! Warnings about what a program defines but never uses: variables, parameters, functions,
//! methods, structs, enums, traits, and imports.
//!
//! `stone check` and the language server report these, but `stone run` and `stone build` do not,
//! so a script under construction runs quietly. The pass reads what the checker already recorded,
//! every [`Symbol`] and each [`Reference`](crate::checker::Reference) to it, plus one walk over
//! the linked module for what a reference alone does not tell, such as which appearances of a
//! name only assign it. For example, in
//!
//! ```text
//! fn area(w: int, h: int) -> int:
//!     ret w * 2
//! ```
//!
//! `h` is never used, and neither is `area`. A name starting with `_`, such as `_h`, is never
//! reported, which is how a program says it means to leave one unused.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, ExprKind, Mod, Stmt, StmtKind, is_lambda};
use crate::checker::{Analysis, SymbolKind, struct_display};
use crate::diagnostic::{Diagnostic, Severity};
use crate::project::Linked;
use crate::span::{FileId, Span};

/// Returns a warning for each thing `linked` defines and never uses, sorted by file and
/// position, given the checker's `analysis` of it.
///
/// For example, a program `x = 1\n` gets `variable 'x' is never used; remove it or name it
/// '_x'` at the `x`.
pub fn find(linked: &Linked, analysis: &Analysis) -> Vec<Diagnostic> {
    // a file that did not parse or link leaves the checker too little to go on
    if linked
        .diagnostics
        .iter()
        .any(|d| d.severity == Severity::Error)
    {
        return vec![];
    }
    let Mod::Module { body } = &linked.module;
    let mut facts = Facts::default();
    // an impl block may come before its struct's definition
    for stmt in body {
        if let StmtKind::StructDef {
            name, public: true, ..
        } = &stmt.kind
        {
            facts.public_structs.insert(name.clone());
        }
    }
    facts.block(body, Owner::Free);

    // the spans where each symbol's name is read, rather than defined or assigned
    let mut reads: Vec<Vec<Span>> = vec![vec![]; analysis.symbols.len()];
    for reference in &analysis.references {
        let symbol = &analysis.symbols[reference.symbol];
        if reference.span != symbol.span && !facts.writes.contains(&reference.span) {
            reads[reference.symbol].push(reference.span);
        }
    }

    let skipped =
        |span: Span| span.file == FileId::PRELUDE || linked.sources.file(span.file).is_builtin();
    let mut warnings = vec![];
    for (symbol, reads) in analysis.symbols.iter().zip(&reads) {
        let short = symbol.name.rsplit('.').next().unwrap_or(&symbol.name);
        if skipped(symbol.span) || short.starts_with('_') || facts.exempt.contains(&symbol.span) {
            continue;
        }
        // a reference inside the definition itself, such as a recursive call, is no use, nor is
        // one in a struct's own impl blocks
        let mut inside: Vec<Span> = facts
            .definitions
            .get(&symbol.span)
            .copied()
            .into_iter()
            .collect();
        if symbol.kind == SymbolKind::Struct {
            inside.extend(facts.impls.get(&symbol.name).into_iter().flatten());
        }
        let outside = reads
            .iter()
            .any(|read| !inside.iter().any(|outer| contains(*outer, *read)));
        let message = match symbol.kind {
            SymbolKind::Local | SymbolKind::Global if reads.is_empty() => {
                format!("variable '{short}' is never used; remove it or name it '_{short}'")
            }
            SymbolKind::Parameter if reads.is_empty() && symbol.name != "self" => {
                format!("parameter '{short}' is never used; remove it or name it '_{short}'")
            }
            SymbolKind::Function if !is_lambda(&symbol.name) => {
                if outside {
                    continue;
                }
                format!("function '{short}' is never used")
            }
            SymbolKind::Method => {
                if outside {
                    continue;
                }
                let owner = symbol.scope.as_deref().unwrap_or_default();
                format!(
                    "method '{}' of '{}' is never used",
                    symbol.name,
                    struct_display(owner)
                )
            }
            SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait => {
                if outside {
                    continue;
                }
                let what = match symbol.kind {
                    SymbolKind::Struct => "struct",
                    SymbolKind::Enum => "enum",
                    _ => "trait",
                };
                format!("{what} '{}' is never used", struct_display(&symbol.name))
            }
            _ => continue,
        };
        warnings.push(Diagnostic::warning(symbol.span, message));
    }
    for import in &linked.imports {
        if !import.used && !skipped(import.name_span) && !import.name.starts_with('_') {
            warnings.push(Diagnostic::warning(
                import.name_span,
                format!("'{}' is imported but never used", import.name),
            ));
        }
    }
    warnings.sort_by_key(|d| (d.span.file, d.span.start));
    warnings
}

/// Returns whether `inner` lies within `outer`, in the same file.
///
/// For example, a call on line 3 lies within a function defined from line 2 to line 5.
fn contains(outer: Span, inner: Span) -> bool {
    outer.file == inner.file && outer.start <= inner.start && inner.end <= outer.end
}

/// What a function's definition is part of, which decides whether its signature is the
/// program's to change.
#[derive(Clone, Copy, PartialEq)]
enum Owner {
    /// A top-level function or a function of a struct's own `impl` block.
    Free,
    /// A method of a `pub struct`, which other modules may call.
    PublicStruct,
    /// A function of a `trait`, or of an `impl` block implementing one, whose signature the
    /// trait fixes and whose calls may go through the trait.
    Trait,
}

/// What [`find`] learns from one walk over the linked module.
#[derive(Default)]
struct Facts {
    /// The spans of names that are only assigned, such as `x` in `x = 1`, `for x in xs`, or
    /// `case some(x)`, but not in `x += 1`, which reads `x` too.
    writes: HashSet<Span>,
    /// The span of each function's, struct's, enum's, and trait's whole definition, by the
    /// span of its name.
    definitions: HashMap<Span, Span>,
    /// The span of every `impl` block of each struct, by the struct's linked name.
    impls: HashMap<String, Vec<Span>>,
    /// The spans of names never reported: `pub` definitions, functions and parameters a trait
    /// fixes.
    exempt: HashSet<Span>,
    /// The linked names of the structs defined `pub`, whose methods other modules may call.
    public_structs: HashSet<String>,
}

impl Facts {
    fn block(&mut self, body: &[Stmt], owner: Owner) {
        for stmt in body {
            self.stmt(stmt, owner);
        }
    }

    fn stmt(&mut self, stmt: &Stmt, owner: Owner) {
        match &stmt.kind {
            StmtKind::FunctionDef {
                name_span,
                public,
                args,
                body,
                ..
            } => {
                self.definitions.insert(*name_span, stmt.span);
                if *public || owner != Owner::Free {
                    self.exempt.insert(*name_span);
                }
                if owner == Owner::Trait {
                    self.exempt.extend(args.args.iter().map(|arg| arg.span));
                }
                self.block(body, Owner::Free);
            }
            StmtKind::StructDef {
                name_span, public, ..
            }
            | StmtKind::EnumDef {
                name_span, public, ..
            } => {
                self.definitions.insert(*name_span, stmt.span);
                if *public {
                    self.exempt.insert(*name_span);
                }
            }
            StmtKind::TraitDef {
                name_span,
                public,
                methods,
                ..
            } => {
                self.definitions.insert(*name_span, stmt.span);
                if *public {
                    self.exempt.insert(*name_span);
                }
                self.block(methods, Owner::Trait);
            }
            StmtKind::ImplDef {
                name,
                trait_name,
                methods,
                ..
            } => {
                self.impls.entry(name.clone()).or_default().push(stmt.span);
                let owner = if trait_name.is_some() {
                    Owner::Trait
                } else if self.public_structs.contains(name) {
                    Owner::PublicStruct
                } else {
                    Owner::Free
                };
                self.block(methods, owner);
            }
            StmtKind::Assign { targets, .. } => {
                targets.iter().for_each(|target| self.target(target));
            }
            StmtKind::For { target, body, .. } => {
                self.target(target);
                self.block(body, owner);
            }
            StmtKind::While { body, .. } => self.block(body, owner),
            StmtKind::If { body, orelse, .. } => {
                self.block(body, owner);
                self.block(orelse, owner);
            }
            StmtKind::Match { cases, .. } => {
                for case in cases {
                    let mut names = vec![];
                    case.pattern.names(&mut names);
                    self.writes.extend(names.into_iter().map(|(_, span)| span));
                    self.block(&case.body, owner);
                }
            }
            StmtKind::Return { .. }
            | StmtKind::AugAssign { .. }
            | StmtKind::Expr { .. }
            | StmtKind::Use { .. }
            | StmtKind::Break
            | StmtKind::Continue => {}
        }
    }

    /// Notes the names an assignment target only assigns, through any tuples it unpacks into.
    fn target(&mut self, target: &Expr) {
        match &target.kind {
            ExprKind::Name { .. } => {
                self.writes.insert(target.span);
            }
            ExprKind::Tuple { elts, .. } => elts.iter().for_each(|elt| self.target(elt)),
            _ => {}
        }
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;
    use crate::driver::analyze_linked;
    use crate::project::{MapSources, link};

    /// Links and checks a program whose entry file is the first of `files`, and returns each
    /// unused warning as its file's path, its message, and where it starts.
    fn unused_in(files: &[(&str, &str)]) -> Vec<(String, String, usize, usize)> {
        let mut sources = MapSources::default();
        for (path, source) in &files[1..] {
            sources.insert(path, *source);
        }
        let (entry, source) = files[0];
        let linked = link(Path::new(entry), source, &sources);
        let analysis = analyze_linked(&linked);
        let errors: Vec<&Diagnostic> = analysis.diagnostics.iter().collect();
        assert_eq!(errors, Vec::<&Diagnostic>::new(), "{source}");
        find(&linked, &analysis)
            .into_iter()
            .map(|d| {
                let path = linked.sources.file(d.span.file).path.display().to_string();
                (path, d.message, d.span.start.line, d.span.start.col)
            })
            .collect()
    }

    /// Returns each unused warning of a single-file program as its message and position.
    fn unused(source: &str) -> Vec<(String, usize, usize)> {
        unused_in(&[("main.stn", source)])
            .into_iter()
            .map(|(_, message, line, col)| (message, line, col))
            .collect()
    }

    fn warning(message: &str, line: usize, col: usize) -> (String, usize, usize) {
        (message.to_string(), line, col)
    }

    #[test]
    fn a_used_program_has_no_warnings() {
        let source = "\
fn double(x: int) -> int:
    ret x * 2

total = 0
for i in range(3):
    total += double(i)
print(total)
";
        assert_eq!(unused(source), []);
    }

    #[test]
    fn a_variable_that_is_never_read_is_reported() {
        assert_eq!(
            unused("x = 1\nx = 2\n"),
            [warning(
                "variable 'x' is never used; remove it or name it '_x'",
                1,
                1
            )]
        );
        assert_eq!(
            unused("fn f():\n    y = 1\n    print(\"hi\")\n\nf()\n"),
            [warning(
                "variable 'y' is never used; remove it or name it '_y'",
                2,
                5
            )]
        );
    }

    #[test]
    fn unpacked_looped_and_matched_names_are_variables_too() {
        let source = "\
q, r = (1, 2)
print(q)
for i in range(2):
    print(\"x\")
o = some(1)
match o:
    case some(v):
        print(\"some\")
    case none:
        print(\"none\")
";
        assert_eq!(
            unused(source),
            [
                warning(
                    "variable 'r' is never used; remove it or name it '_r'",
                    1,
                    4
                ),
                warning(
                    "variable 'i' is never used; remove it or name it '_i'",
                    3,
                    5
                ),
                warning(
                    "variable 'v' is never used; remove it or name it '_v'",
                    7,
                    15
                ),
            ]
        );
    }

    #[test]
    fn an_augmented_assignment_or_a_change_reads_its_variable() {
        assert_eq!(unused("n = 0\nn += 1\nxs = [1]\nxs.append(2)\n"), []);
    }

    #[test]
    fn a_global_read_only_inside_a_function_is_used() {
        assert_eq!(
            unused("limit = 3\nfn f() -> int:\n    ret limit\n\nprint(f())\n"),
            []
        );
    }

    #[test]
    fn a_leading_underscore_silences_a_warning() {
        let source = "\
_x = 1
fn f(_a: int):
    print(\"f\")

fn _helper():
    print(\"h\")

f(1)
for _ in range(2):
    print(\"y\")
";
        assert_eq!(unused(source), []);
    }

    #[test]
    fn a_parameter_that_is_never_read_is_reported() {
        let source = "fn area(w: int, h: int) -> int:\n    ret w * 2\n\nprint(area(1, 2))\n";
        assert_eq!(
            unused(source),
            [warning(
                "parameter 'h' is never used; remove it or name it '_h'",
                1,
                17
            )]
        );
    }

    #[test]
    fn a_function_used_only_by_itself_is_reported() {
        let source = "\
fn count(n: int) -> int:
    if n == 0:
        ret 0
    ret count(n - 1)

fn unused():
    print(\"u\")

fn used():
    print(\"used\")

used()
";
        assert_eq!(
            unused(source),
            [
                warning("function 'count' is never used", 1, 4),
                warning("function 'unused' is never used", 6, 4),
            ]
        );
    }

    #[test]
    fn a_function_named_as_a_value_is_used() {
        let source = "\
fn double(x: int) -> int:
    ret x * 2

print([1, 2].map(double))
print([1].map(|x| x + 1))
";
        assert_eq!(unused(source), []);
    }

    #[test]
    fn methods_and_types_that_are_never_used_are_reported() {
        let source = "\
struct Point:
    x: int

impl Point:
    fn origin() -> Point:
        ret Point(x=0)

    fn shifted(self, dx: int) -> Point:
        ret Point(x=self.x + dx)

struct Unused:
    y: int

impl Unused:
    fn make() -> Unused:
        ret Unused(y=1)

enum Color:
    Red

trait Named:
    fn name(self) -> str

print(Point.origin())
";
        assert_eq!(
            unused(source),
            [
                warning("method 'shifted' of 'Point' is never used", 8, 8),
                warning("struct 'Unused' is never used", 11, 8),
                warning("method 'make' of 'Unused' is never used", 15, 8),
                warning("enum 'Color' is never used", 18, 6),
                warning("trait 'Named' is never used", 21, 7),
            ]
        );
    }

    #[test]
    fn a_trait_fixes_its_functions_and_their_parameters() {
        let source = "\
trait Shape:
    fn area(self) -> int
    fn scaled(self, k: int) -> int:
        ret 0

struct Square:
    side: int

impl Shape for Square:
    fn area(self) -> int:
        ret self.side * self.side

impl display for Square:
    fn display(self) -> str:
        ret \"square\"

fn total[T: Shape](xs: list[T]) -> int:
    ret xs[0].area()

print(total([Square(side=2)]))
";
        assert_eq!(unused(source), []);
    }

    #[test]
    fn imports_that_are_never_used_are_reported() {
        let files = [
            (
                "main.stn",
                "use util\nuse util.twice as double\nuse math\nprint(util.twice(1))\n",
            ),
            (
                "util.stn",
                "pub fn twice(x: int) -> int:\n    ret helper(x) * 2\n\nfn helper(x: int) -> \
                 int:\n    ret x\n\nfn dead():\n    print(\"d\")\n\npub struct Box:\n    v: \
                 int\n\nimpl Box:\n    fn get(self) -> int:\n        ret self.v\n",
            ),
        ];
        assert_eq!(
            unused_in(&files),
            [
                (
                    "main.stn".to_string(),
                    "'double' is imported but never used".to_string(),
                    2,
                    19
                ),
                (
                    "main.stn".to_string(),
                    "'math' is imported but never used".to_string(),
                    3,
                    5
                ),
                (
                    "util.stn".to_string(),
                    "function 'dead' is never used".to_string(),
                    7,
                    4
                ),
            ]
        );
    }

    #[test]
    fn a_program_that_fails_to_link_gets_no_warnings() {
        let linked = link(
            Path::new("main.stn"),
            "x = 1\ny = (\n",
            &MapSources::default(),
        );
        let analysis = analyze_linked(&linked);
        assert_eq!(find(&linked, &analysis), []);
    }

    #[test]
    fn the_prelude_and_builtin_modules_are_never_reported() {
        assert_eq!(
            unused("use net\nr = net.resolve(\"localhost\")\nprint(r.is_ok())\n"),
            []
        );
    }
}
