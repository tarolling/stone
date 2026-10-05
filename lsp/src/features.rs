//! The language features, each a function from a [`Document`] and a position to an LSP result.
//!
//! The work is done by [`stone::driver::analyze`]. These functions only find what is at a
//! position and convert it to LSP types, so they are tested without running a server.

use std::collections::HashMap;

use lsp_types::{
    CompletionItem, CompletionItemKind, Diagnostic, DiagnosticSeverity, DocumentSymbol, Hover,
    HoverContents, Location, MarkupContent, MarkupKind, Position, PrepareRenameResponse, Range,
    SymbolKind, TextEdit, Uri, WorkspaceEdit,
};
use stone::checker::{Analysis, Symbol, SymbolKind as StoneSymbolKind, Type};
use stone::diagnostic::Severity;
use stone::span::{Pos, Span};
use stone::stdlib::{
    BUILTIN_DOCS, BUILTINS, BuiltinDoc, METHOD_DOCS, METHODS, builtin_doc, builtins_reference,
    method_doc,
};
use stone::token::RESERVED_KEYWORDS;

use crate::line_index::{Encoding, LineIndex};

#[cfg(test)]
mod tests;

/// An open document and everything the checker learned about it.
pub struct Document {
    pub text: String,
    pub analysis: Analysis,
    pub index: LineIndex,
}

impl Document {
    /// Analyzes `text`, keeping the result for answering requests until the text changes.
    pub fn new(text: String, encoding: Encoding) -> Self {
        let analysis = stone::driver::analyze(&text);
        let index = LineIndex::new(&text, encoding);
        Document {
            text,
            analysis,
            index,
        }
    }

    /// Returns the symbol whose name is at `position`, with the span of that appearance.
    fn symbol_at(&self, position: Position) -> Option<(usize, Span)> {
        let reference = self.analysis.reference_at(self.index.pos(position))?;
        Some((reference.symbol, reference.span))
    }
}

/// Returns the document's problems as LSP diagnostics.
pub fn diagnostics(doc: &Document) -> Vec<Diagnostic> {
    doc.analysis
        .diagnostics
        .iter()
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

/// Describes what is at `position`: a symbol's declaration, a builtin's documentation, or the
/// type of the innermost expression there.
pub fn hover(doc: &Document, position: Position) -> Option<Hover> {
    let pos = doc.index.pos(position);
    if let Some((symbol, span)) = doc.symbol_at(position) {
        let symbol = &doc.analysis.symbols[symbol];
        return Some(Hover {
            contents: markdown(&signature(&doc.analysis, symbol), None),
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
        .filter(|(span, _)| span.contains(pos))
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
    /// The same for each builtin method, such as `len` in `# (str | list[T]).len() -> int`.
    methods: HashMap<&'static str, (u32, u32)>,
}

impl Builtins {
    pub fn new(uri: Uri) -> Self {
        let text = builtins_reference();
        // the signature's line, and the column of `name` where it is followed by a `(`
        let find = |doc: &BuiltinDoc| {
            let signature = format!("# {}", doc.signature);
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
        let span = doc.analysis.symbols[symbol].span;
        return Some(Location::new(uri.clone(), doc.index.range(span)));
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
        .map(|r| Location::new(uri.clone(), doc.index.range(r.span)))
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
    let Some((symbol, _)) = doc.symbol_at(position) else {
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
    let uses: Vec<Span> = doc.analysis.references_to(symbol).map(|r| r.span).collect();
    let taken = uses.iter().any(|span| {
        doc.analysis
            .visible_at(span.start)
            .any(|s| s.name == new_name)
    });
    if taken {
        return Err(format!("'{new_name}' is already defined here"));
    }
    let edits = uses
        .into_iter()
        .map(|span| TextEdit::new(doc.index.range(span), new_name.to_string()))
        .collect();
    Ok(WorkspaceEdit {
        changes: Some(HashMap::from([(uri.clone(), edits)])),
        ..WorkspaceEdit::default()
    })
}

#[allow(deprecated)] // `DocumentSymbol::deprecated` must still be set
fn document_symbol(doc: &Document, symbol: &Symbol, range: Span) -> DocumentSymbol {
    DocumentSymbol {
        name: symbol.name.clone(),
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
        .filter(|s| s.scope.is_none())
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

/// Returns the names that can be written at `position`: symbols in scope, builtins, and keywords,
/// or right after a `.`, the builtin methods.
///
/// The client filters them by what has been typed so far.
pub fn completion(doc: &Document, position: Position) -> Vec<CompletionItem> {
    let pos = doc.index.pos(position);
    // after a `.`, only a method can come next, as in `xs.le`
    let start = word_at(doc, pos).map_or(pos, |(_, span)| span.start);
    if follows_dot(doc, start) {
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
    let mut visible: Vec<&Symbol> = doc.analysis.visible_at(pos).collect();
    visible.sort_by_key(|s| s.scope.is_none());
    for symbol in visible {
        if items.iter().any(|item| item.label == symbol.name) {
            continue;
        }
        items.push(CompletionItem {
            label: symbol.name.clone(),
            kind: Some(match symbol.kind {
                StoneSymbolKind::Function => CompletionItemKind::FUNCTION,
                _ => CompletionItemKind::VARIABLE,
            }),
            detail: Some(signature(&doc.analysis, symbol)),
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
