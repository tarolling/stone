//! The language features, each a function from a [`Document`] and a position to an LSP result.
//!
//! The work is done by [`stone::driver::analyze_linked`] over the whole program a document
//! belongs to. These functions only find what is at a position and convert it to LSP types, so
//! they are tested without running a server.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use lsp_types::{
    CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, DocumentSymbol, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, Position, PrepareRenameResponse, Range,
    SymbolKind, TextEdit, Uri, WorkspaceEdit,
};
use stone::checker::{Analysis, Symbol, SymbolKind as StoneSymbolKind, Type};
use stone::diagnostic::Severity;
use stone::project::{
    DEFAULT_ENTRY, Import, Linked, MapSources, SourceMap, Sources, Target, link, link_in,
};
use stone::span::{FileId, Pos, Span};
use stone::stdlib::{
    BUILTIN_DOCS, BUILTINS, BuiltinDoc, METHOD_DOCS, METHODS, builtin_doc, builtins_reference,
    method_doc,
};
use stone::token::RESERVED_KEYWORDS;

use crate::line_index::{Encoding, LineIndex};
use crate::uri::file_uri;

#[cfg(test)]
mod tests;

/// An open document and everything the checker learned about the program it belongs to.
pub struct Document {
    pub text: String,
    pub analysis: Analysis,
    pub index: LineIndex,
    /// The document's file in its program.
    pub file: FileId,
    /// Every file of the program.
    pub sources: SourceMap,
    /// Every name the program's files bind with `use`.
    pub imports: Vec<Import>,
    /// The linked names of the program's `pub` functions, such as `util.twice`.
    pub public: HashSet<String>,
    /// The directory module paths start from.
    pub root: PathBuf,
    encoding: Encoding,
}

impl Document {
    /// Analyzes `text` on its own, as a program of one file, keeping the result for answering
    /// requests until the text changes.
    pub fn new(text: String, encoding: Encoding) -> Self {
        let linked = link(Path::new(DEFAULT_ENTRY), &text, &MapSources::default());
        Self::from_linked(linked, FileId(0), encoding)
    }

    /// Analyzes the file at `path` as part of its program, reading every file from `sources`, or
    /// returns `None` if there is no file at `path`.
    ///
    /// The program is the one whose entry file is the nearest `main.st` in the file's directory or
    /// one above it. If that program does not use the file yet, the file is checked as an entry
    /// file of its own, from the program's root if it is in a directory under it.
    pub fn in_program(path: &Path, sources: &dyn Sources, encoding: Encoding) -> Option<Self> {
        let text = sources.read(path)?;
        let linked = program_of(path, &text, sources);
        let file = linked.sources.find(path).unwrap_or(FileId(0));
        Some(Self::from_linked(linked, file, encoding))
    }

    fn from_linked(linked: Linked, file: FileId, encoding: Encoding) -> Self {
        let analysis = stone::driver::analyze_linked(&linked);
        let text = linked.sources.file(file).source.clone();
        let index = LineIndex::new(&text, encoding);
        Document {
            text,
            analysis,
            index,
            file,
            sources: linked.sources,
            imports: linked.imports,
            public: linked.public,
            root: linked.root,
            encoding,
        }
    }

    /// Returns the symbol whose name is at `position`, with the span of that appearance.
    fn symbol_at(&self, position: Position) -> Option<(usize, Span)> {
        let reference = self
            .analysis
            .reference_at(self.file, self.index.pos(position))?;
        Some((reference.symbol, reference.span))
    }

    /// Returns the `use` of this document whose last name is at `position`.
    fn import_at(&self, position: Position) -> Option<&Import> {
        let pos = self.index.pos(position);
        self.imports
            .iter()
            .find(|i| i.span.file == self.file && i.span.contains(pos))
    }

    /// Returns where `span` is, given that this document is at `uri`.
    fn location(&self, span: Span, uri: &Uri) -> Option<Location> {
        if span.file == self.file {
            return Some(Location::new(uri.clone(), self.index.range(span)));
        }
        let file = self.sources.file(span.file);
        let index = LineIndex::new(&file.source, self.encoding);
        Some(Location::new(file_uri(&file.path)?, index.range(span)))
    }

    /// Returns the name a symbol has in its own file, which for a module's function leaves out
    /// the module, such as `twice` for `util.twice`.
    fn local_name<'a>(&self, symbol: &'a Symbol) -> &'a str {
        let module = &self.sources.file(symbol.span.file).module;
        if module.is_empty() || symbol.scope.is_some() {
            return &symbol.name;
        }
        symbol
            .name
            .strip_prefix(module.as_str())
            .and_then(|rest| rest.strip_prefix('.'))
            .unwrap_or(&symbol.name)
    }

    /// Returns the text `span` covers, if it is on one line.
    fn text_at(&self, span: Span) -> Option<String> {
        let line = self
            .sources
            .file(span.file)
            .source
            .lines()
            .nth(span.start.line.checked_sub(1)?)?;
        (span.start.line == span.end.line).then(|| {
            line.chars()
                .skip(span.start.col - 1)
                .take(span.end.col - span.start.col)
                .collect()
        })
    }

    /// Returns the symbol of the function with this linked name, such as `util.twice`.
    fn function(&self, name: &str) -> Option<&Symbol> {
        self.analysis
            .symbols
            .iter()
            .find(|s| s.kind == StoneSymbolKind::Function && s.name == name)
    }
}

/// Links the program the file at `path` belongs to. See [`Document::in_program`].
fn program_of(path: &Path, text: &str, sources: &dyn Sources) -> Linked {
    let dir = path.parent().unwrap_or(Path::new(""));
    let root = dir
        .ancestors()
        .find(|d| sources.read(&d.join(DEFAULT_ENTRY)).is_some());
    let Some(root) = root else {
        return link(path, text, sources);
    };
    let main = root.join(DEFAULT_ENTRY);
    if main == path {
        return link(path, text, sources);
    }
    let linked = link(&main, &sources.read(&main).unwrap_or_default(), sources);
    if linked.sources.find(path).is_some() {
        return linked;
    }
    // not used by the program yet
    if dir == root {
        link(path, text, sources)
    } else {
        link_in(root, path, text, sources)
    }
}

/// Returns the document's problems as LSP diagnostics.
pub fn diagnostics(doc: &Document) -> Vec<Diagnostic> {
    doc.analysis
        .diagnostics
        .iter()
        .filter(|d| d.span.file == doc.file)
        .map(|d| Diagnostic {
            range: doc.index.range(d.span),
            severity: Some(match d.severity {
                Severity::Error => DiagnosticSeverity::ERROR,
                Severity::Warning => DiagnosticSeverity::WARNING,
            }),
            source: Some("stone".to_string()),
            message: d.message.clone(),
            ..Diagnostic::default()
        })
        .collect()
}

/// Describes a symbol the way stone would declare it, such as `def add(a: int) -> int` or
/// `(local) sum: int`.
fn signature(analysis: &Analysis, symbol: &Symbol) -> String {
    match (&symbol.kind, &symbol.ty) {
        (StoneSymbolKind::Function, Type::Function { params, ret }) => {
            let names = analysis.symbols.iter().filter(|s| {
                s.kind == StoneSymbolKind::Parameter && s.scope.as_deref() == Some(&symbol.name)
            });
            let params: Vec<String> = names
                .zip(params)
                .map(|(param, ty)| format!("{}: {ty}", param.name))
                .collect();
            format!("def {}({}) -> {ret}", symbol.name, params.join(", "))
        }
        (kind, ty) => {
            let kind = match kind {
                StoneSymbolKind::Parameter => "parameter",
                StoneSymbolKind::Local => "local",
                StoneSymbolKind::Global | StoneSymbolKind::Function => "global",
            };
            format!("({kind}) {}: {ty}", symbol.name)
        }
    }
}

/// Returns the `//` comment lines directly above a function's or variable's definition, without
/// their `//` and the space after it, as Markdown for hover.
///
/// For example, for `f` in `"// adds one\n// to n\ndef f(n);\n    ret n + 1\n"` this returns
/// `"adds one\nto n"`. A line that is only `//` becomes a paragraph break. A blank line between
/// the comment and the definition detaches it, parameters are never documented, and neither is a
/// definition that does not start its line, such as `i` in `for i in range(3);`.
fn doc_comment(text: &str, symbol: &Symbol) -> Option<String> {
    if symbol.kind == StoneSymbolKind::Parameter {
        return None;
    }
    let lines: Vec<&str> = text.lines().collect();
    let line = symbol.span.start.line - 1;
    let before: String = lines
        .get(line)?
        .chars()
        .take(symbol.span.start.col - 1)
        .collect();
    if !matches!(before.trim(), "" | "def" | "pub def") {
        return None;
    }
    let comment: Vec<&str> = lines[..line]
        .iter()
        .rev()
        .map_while(|line| line.trim_start().strip_prefix("//"))
        .map(|line| line.strip_prefix(' ').unwrap_or(line).trim_end())
        .collect();
    if comment.is_empty() {
        return None;
    }
    let lines: Vec<&str> = comment.into_iter().rev().collect();
    Some(lines.join("\n"))
}

/// Returns the `//` comment lines at the very top of a module's file, without their `//` and the
/// space after it, as Markdown for hover.
///
/// For example, for `"// summary statistics\n\npub def mean(xs);\n..."` this returns
/// `"summary statistics"`. A comment directly above a definition documents that definition
/// instead (see [`doc_comment`]), so only one followed by a blank line or the end of the file
/// counts.
fn module_comment(text: &str) -> Option<String> {
    let lines: Vec<&str> = text.lines().collect();
    let comment: Vec<&str> = lines
        .iter()
        .map_while(|line| line.trim_start().strip_prefix("//"))
        .map(|line| line.strip_prefix(' ').unwrap_or(line).trim_end())
        .collect();
    let after = lines.get(comment.len());
    if comment.is_empty() || after.is_some_and(|line| !line.trim().is_empty()) {
        return None;
    }
    Some(comment.join("\n"))
}

/// Returns the module named at `position` and the span of its name: the last name of a
/// `use` path that binds a module, or a name bound to a module right before a `.`, as `stats`
/// is in `stats.mean(xs)`.
fn module_at(doc: &Document, position: Position) -> Option<(FileId, Span)> {
    if let Some(import) = doc.import_at(position) {
        return match import.target {
            Target::Module(file) => Some((file, import.span)),
            Target::Function(_) => None,
        };
    }
    let (word, span) = word_at(doc, doc.index.pos(position))?;
    let next = doc
        .text
        .lines()
        .nth(span.end.line - 1)?
        .chars()
        .nth(span.end.col - 1);
    if next != Some('.') {
        return None;
    }
    doc.imports.iter().find_map(|i| match i.target {
        Target::Module(file) if i.span.file == doc.file && i.name == word => Some((file, span)),
        _ => None,
    })
}

/// Returns the identifier the position is on or just after, and its span.
fn word_at(doc: &Document, pos: Pos) -> Option<(String, Span)> {
    let line: Vec<char> = doc.text.lines().nth(pos.line - 1)?.chars().collect();
    let is_word = |i: usize| {
        line.get(i)
            .is_some_and(|c| c.is_alphanumeric() || *c == '_')
    };
    let cursor = pos.col - 1;
    let mut start = if is_word(cursor) {
        cursor
    } else if cursor > 0 && is_word(cursor - 1) {
        cursor - 1
    } else {
        return None;
    };
    while start > 0 && is_word(start - 1) {
        start -= 1;
    }
    let mut end = start;
    while is_word(end) {
        end += 1;
    }
    let word = line[start..end].iter().collect();
    let span = Span::new(Pos::new(pos.line, start + 1), Pos::new(pos.line, end + 1));
    Some((word, span))
}

/// Returns whether the text right before `pos` is a `.`, so a name there is a method, as `len` is
/// in `xs.len()`.
fn follows_dot(doc: &Document, pos: Pos) -> bool {
    pos.col > 1
        && doc
            .text
            .lines()
            .nth(pos.line - 1)
            .and_then(|line| line.chars().nth(pos.col - 2))
            == Some('.')
}

fn markdown(code: &str, text: Option<&str>) -> HoverContents {
    let mut value = format!("```stone\n{code}\n```");
    if let Some(text) = text {
        value.push('\n');
        value.push_str(text);
    }
    HoverContents::Markup(MarkupContent {
        kind: MarkupKind::Markdown,
        value,
    })
}

/// Describes what is at `position`: a symbol's declaration with the comment above its definition
/// (see [`doc_comment`]), a builtin's documentation, or the type of the innermost expression there.
pub fn hover(doc: &Document, position: Position) -> Option<Hover> {
    let pos = doc.index.pos(position);
    if let Some((symbol, span)) = doc.symbol_at(position) {
        let symbol = &doc.analysis.symbols[symbol];
        let text = &doc.sources.file(symbol.span.file).source;
        let comment = doc_comment(text, symbol);
        return Some(Hover {
            contents: markdown(&signature(&doc.analysis, symbol), comment.as_deref()),
            range: Some(doc.index.range(span)),
        });
    }
    // a local that shadows a module is a symbol, so this only finds real modules
    if let Some((file, span)) = module_at(doc, position) {
        let module = doc.sources.file(file);
        return Some(Hover {
            contents: markdown(
                &format!("module {}", module.module),
                module_comment(&module.source).as_deref(),
            ),
            range: Some(doc.index.range(span)),
        });
    }
    if let Some((word, span)) = word_at(doc, pos)
        && let Some(builtin) = if follows_dot(doc, span.start) {
            method_doc(&word)
        } else {
            builtin_doc(&word)
        }
    {
        return Some(Hover {
            contents: markdown(builtin.signature, Some(builtin.description)),
            range: Some(doc.index.range(span)),
        });
    }
    // only on the text of an expression, not the whitespace around it
    let on_text = doc
        .text
        .lines()
        .nth(pos.line - 1)
        .and_then(|line| line.chars().nth(pos.col - 1))
        .is_some_and(|ch| !ch.is_whitespace());
    if !on_text && word_at(doc, pos).is_none() {
        return None;
    }
    // the innermost expression, which starts last among those that contain the position
    let (span, ty) = doc
        .analysis
        .types
        .iter()
        .filter(|(span, _)| span.file == doc.file && span.contains(pos))
        .max_by_key(|(span, _)| (span.start, std::cmp::Reverse(span.end)))?;
    Some(Hover {
        contents: markdown(&ty.to_string(), None),
        range: Some(doc.index.range(*span)),
    })
}

/// Where builtins are documented: the file [`builtins_reference`] generates, written out by the
/// server so editors can open it.
pub struct Builtins {
    uri: Uri,
    /// The line and column of each builtin function's name in its signature, counted from 0.
    functions: HashMap<&'static str, (u32, u32)>,
    /// The same for each builtin method, such as `len` in `// (str | list[T]).len() -> int`.
    methods: HashMap<&'static str, (u32, u32)>,
}

impl Builtins {
    pub fn new(uri: Uri) -> Self {
        let text = builtins_reference();
        // the signature's line, and the column of `name` where it is followed by a `(`
        let find = |doc: &BuiltinDoc| {
            let signature = format!("// {}", doc.signature);
            let line = text.lines().position(|line| line == signature)?;
            let col = signature.find(&format!("{}(", doc.name))?;
            Some((
                doc.name,
                (line as u32, signature[..col].chars().count() as u32),
            ))
        };
        let functions = BUILTIN_DOCS.iter().filter_map(find).collect();
        let methods = METHOD_DOCS.iter().filter_map(find).collect();
        Builtins {
            uri,
            functions,
            methods,
        }
    }

    /// Returns the location of the builtin function, or with `method` the builtin method, named
    /// `name` in its signature line.
    fn location(&self, name: &str, method: bool) -> Option<Location> {
        let names = if method {
            &self.methods
        } else {
            &self.functions
        };
        let &(line, start) = names.get(name)?;
        let end = start + name.chars().count() as u32;
        let range = Range::new(Position::new(line, start), Position::new(line, end));
        Some(Location::new(self.uri.clone(), range))
    }
}

/// Returns where the symbol at `position` is defined, or for a builtin, where `builtins`
/// documents it.
pub fn definition(
    doc: &Document,
    uri: &Uri,
    position: Position,
    builtins: Option<&Builtins>,
) -> Option<Location> {
    if let Some((symbol, _)) = doc.symbol_at(position) {
        return doc.location(doc.analysis.symbols[symbol].span, uri);
    }
    if let Some(import) = doc.import_at(position) {
        return match &import.target {
            Target::Module(file) => {
                let start = Range::new(Position::new(0, 0), Position::new(0, 0));
                Some(Location::new(
                    file_uri(&doc.sources.file(*file).path)?,
                    start,
                ))
            }
            Target::Function(name) => doc.location(doc.function(name)?.span, uri),
        };
    }
    let (word, span) = word_at(doc, doc.index.pos(position))?;
    builtins?.location(&word, follows_dot(doc, span.start))
}

/// Returns every appearance of the symbol at `position`, with or without its definition.
pub fn references(
    doc: &Document,
    uri: &Uri,
    position: Position,
    include_declaration: bool,
) -> Vec<Location> {
    let Some((symbol, _)) = doc.symbol_at(position) else {
        return vec![];
    };
    let definition = doc.analysis.symbols[symbol].span;
    doc.analysis
        .references_to(symbol)
        .filter(|r| include_declaration || r.span != definition)
        .filter_map(|r| doc.location(r.span, uri))
        .collect()
}

/// Returns the range of the name at `position` if it can be renamed.
pub fn prepare_rename(doc: &Document, position: Position) -> Option<PrepareRenameResponse> {
    let (_, span) = doc.symbol_at(position)?;
    Some(PrepareRenameResponse::Range(doc.index.range(span)))
}

/// Returns whether `name` could be lexed as a single name.
fn is_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(char::is_alphabetic) && chars.all(|c| c.is_alphanumeric() || c == '_')
}

/// Renames the symbol at `position` everywhere it appears, or explains why it cannot be.
///
/// The new name must be a valid name that is not a keyword or builtin, and must not already mean
/// something else anywhere the symbol is used.
pub fn rename(
    doc: &Document,
    uri: &Uri,
    position: Position,
    new_name: &str,
) -> Result<WorkspaceEdit, String> {
    let Some((index, _)) = doc.symbol_at(position) else {
        return Err("nothing to rename here".to_string());
    };
    if RESERVED_KEYWORDS.contains(&new_name) {
        return Err(format!("'{new_name}' is a keyword"));
    }
    if BUILTINS.contains(&new_name) {
        return Err(format!("'{new_name}' is a builtin"));
    }
    if !is_name(new_name) {
        return Err(format!("'{new_name}' is not a valid name"));
    }
    let symbol = &doc.analysis.symbols[index];
    let name = doc.local_name(symbol);
    // an alias from `use ... as` keeps its own name
    let mut uses: Vec<Span> = doc
        .analysis
        .references_to(index)
        .map(|r| r.span)
        .filter(|span| doc.text_at(*span).as_deref() == Some(name))
        .collect();
    let target = Target::Function(symbol.name.clone());
    uses.extend(
        doc.imports
            .iter()
            .filter(|i| i.target == target)
            .map(|i| i.span),
    );
    let taken = uses.iter().any(|span| {
        doc.analysis
            .visible_at(span.file, span.start)
            .any(|s| doc.local_name(s) == new_name)
    });
    if taken {
        return Err(format!("'{new_name}' is already defined here"));
    }
    let mut changes: Vec<(Uri, Vec<TextEdit>)> = vec![];
    for span in uses {
        let Some(location) = doc.location(span, uri) else {
            return Err("cannot find a file to rename in".to_string());
        };
        let edit = TextEdit::new(location.range, new_name.to_string());
        match changes.iter_mut().find(|(uri, _)| *uri == location.uri) {
            Some((_, edits)) => edits.push(edit),
            None => changes.push((location.uri, vec![edit])),
        }
    }
    Ok(WorkspaceEdit {
        changes: Some(changes.into_iter().collect()),
        ..WorkspaceEdit::default()
    })
}

#[allow(deprecated)] // `DocumentSymbol::deprecated` must still be set
fn document_symbol(doc: &Document, symbol: &Symbol, range: Span) -> DocumentSymbol {
    DocumentSymbol {
        name: doc.local_name(symbol).to_string(),
        detail: Some(signature(&doc.analysis, symbol)),
        kind: match symbol.kind {
            StoneSymbolKind::Function => SymbolKind::FUNCTION,
            _ => SymbolKind::VARIABLE,
        },
        tags: None,
        deprecated: None,
        range: doc.index.range(range),
        selection_range: doc.index.range(symbol.span),
        children: None,
    }
}

/// Returns an outline of the document: its globals and functions, with each function's parameters
/// and locals inside it.
pub fn document_symbols(doc: &Document) -> Vec<DocumentSymbol> {
    let analysis = &doc.analysis;
    let mut outline: Vec<DocumentSymbol> = analysis
        .symbols
        .iter()
        .filter(|s| s.scope.is_none() && s.span.file == doc.file)
        .map(|symbol| {
            let Some(&extent) = analysis.functions.get(&symbol.name) else {
                return document_symbol(doc, symbol, symbol.span);
            };
            let mut outline = document_symbol(doc, symbol, extent);
            let children = analysis
                .symbols
                .iter()
                .filter(|s| s.scope.as_deref() == Some(&symbol.name))
                .map(|s| document_symbol(doc, s, s.span))
                .collect();
            outline.children = Some(children);
            outline
        })
        .collect();
    outline.sort_by_key(|s| (s.range.start.line, s.range.start.character));
    outline
}

/// Returns the names that can be written at `position`: symbols in scope, imported names,
/// builtins, and keywords. Right after a `.`, it is the public functions of an imported module, or
/// otherwise the builtin methods, and in a `use`, the modules and directories there are.
///
/// The client filters them by what has been typed so far.
pub fn completion(
    doc: &Document,
    position: Position,
    sources: &dyn Sources,
) -> Vec<CompletionItem> {
    let pos = doc.index.pos(position);
    let start = word_at(doc, pos).map_or(pos, |(_, span)| span.start);
    let before: String = doc
        .text
        .lines()
        .nth(pos.line - 1)
        .unwrap_or("")
        .chars()
        .take(start.col - 1)
        .collect();
    if let Some(path) = before.trim_start().strip_prefix("use ") {
        return use_completion(doc, path.trim_start(), sources);
    }
    // after a `.`, only a module's function or a method can come next, as in `xs.le`
    if let Some(receiver) = before.strip_suffix('.') {
        let receiver: String = receiver
            .chars()
            .rev()
            .take_while(|c| c.is_alphanumeric() || *c == '_')
            .collect::<Vec<char>>()
            .into_iter()
            .rev()
            .collect();
        let module = doc.imports.iter().find_map(|i| match i.target {
            Target::Module(file) if i.span.file == doc.file && i.name == receiver => Some(file),
            _ => None,
        });
        if let Some(file) = module {
            return module_functions(doc, file);
        }
        return METHODS
            .iter()
            .map(|method| CompletionItem {
                label: method.to_string(),
                kind: Some(CompletionItemKind::METHOD),
                detail: method_doc(method).map(|doc| doc.signature.to_string()),
                ..CompletionItem::default()
            })
            .collect();
    }
    let mut items: Vec<CompletionItem> = vec![];
    // locals come first, so they win over a global with the same name
    let mut visible: Vec<&Symbol> = doc.analysis.visible_at(doc.file, pos).collect();
    visible.sort_by_key(|s| s.scope.is_none());
    for symbol in visible {
        let label = doc.local_name(symbol);
        if items.iter().any(|item| item.label == label) {
            continue;
        }
        items.push(CompletionItem {
            label: label.to_string(),
            kind: Some(match symbol.kind {
                StoneSymbolKind::Function => CompletionItemKind::FUNCTION,
                _ => CompletionItemKind::VARIABLE,
            }),
            detail: Some(signature(&doc.analysis, symbol)),
            ..CompletionItem::default()
        });
    }
    for import in doc.imports.iter().filter(|i| i.span.file == doc.file) {
        let (kind, detail) = match &import.target {
            Target::Module(file) => (
                CompletionItemKind::MODULE,
                Some(format!("module {}", doc.sources.file(*file).module)),
            ),
            Target::Function(name) => (
                CompletionItemKind::FUNCTION,
                doc.function(name).map(|s| signature(&doc.analysis, s)),
            ),
        };
        items.push(CompletionItem {
            label: import.name.clone(),
            kind: Some(kind),
            detail,
            ..CompletionItem::default()
        });
    }
    for builtin in BUILTINS {
        items.push(CompletionItem {
            label: builtin.to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: builtin_doc(builtin).map(|doc| doc.signature.to_string()),
            ..CompletionItem::default()
        });
    }
    for keyword in RESERVED_KEYWORDS {
        items.push(CompletionItem {
            label: keyword.to_string(),
            kind: Some(CompletionItemKind::KEYWORD),
            ..CompletionItem::default()
        });
    }
    items
}

/// Returns the public functions of the module in `file`, by their own names.
fn module_functions(doc: &Document, file: FileId) -> Vec<CompletionItem> {
    doc.analysis
        .symbols
        .iter()
        .filter(|s| s.kind == StoneSymbolKind::Function && s.span.file == file)
        .filter(|s| doc.public.contains(&s.name))
        .map(|symbol| CompletionItem {
            label: doc.local_name(symbol).to_string(),
            kind: Some(CompletionItemKind::FUNCTION),
            detail: Some(signature(&doc.analysis, symbol)),
            ..CompletionItem::default()
        })
        .collect()
}

/// Returns what can come next in a `use` whose path so far is `path`, such as `geometry.`: the
/// modules and directories in the directory it names, and the public functions of the module it
/// names, if the program has loaded it.
fn use_completion(doc: &Document, path: &str, sources: &dyn Sources) -> Vec<CompletionItem> {
    let parent = path.rsplit_once('.').map_or("", |(parent, _)| parent);
    let names: Vec<&str> = parent.split('.').filter(|n| !n.is_empty()).collect();
    let dir = names.iter().fold(doc.root.clone(), |dir, n| dir.join(n));
    let mut items: Vec<CompletionItem> = vec![];
    for entry in sources.entries(&dir) {
        let Some(name) = entry.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };
        let kind = if entry.extension().is_some_and(|ext| ext == "st") {
            // the entry file cannot be imported
            if doc.sources.find(&entry) == Some(FileId(0)) {
                continue;
            }
            CompletionItemKind::MODULE
        } else if sources.is_dir(&entry) {
            CompletionItemKind::FOLDER
        } else {
            continue;
        };
        if items.iter().any(|i| i.label == name) {
            continue;
        }
        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(kind),
            ..CompletionItem::default()
        });
    }
    if !names.is_empty() {
        let module = names.join(".");
        if let Some((file, _)) = doc.sources.files().find(|(_, f)| f.module == module) {
            items.extend(module_functions(doc, file));
        }
    }
    items
}
