//! The MCP server's tools, each answering in text an agent reads easily.
//!
//! Every tool that is about code on disk analyzes the file as part of its program, as the
//! language server does for an open document ([`Document::in_program`]), so a module sees the
//! program that uses it and its packages. A position is a 1-based `line` plus either `symbol`,
//! text on that line, or `column`, its 1-based character, as stone's own errors count them, and a
//! position in an answer is written `file:line:col: <the line's text>`, the file relative to the
//! server's working directory when it is inside it.

use std::collections::HashSet;
use std::fmt::Write as _;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::mpsc;
use std::time::Duration;

use lsp_types::{DocumentSymbol, HoverContents, Location, Position, SymbolKind, Uri};
use serde_json::{Value, json};
use stone::diagnostic::{Diagnostic, Diagnostics, Severity, distance};
use stone::interpreter::{Exit, Limits};
use stone::project::{DEFAULT_ENTRY, FsSources, MapSources, SourceMap, Sources};
use stone::span::FileId;
use stone::span::Pos;
use stone::stdlib::{BUILTIN_DOCS, BuiltinDoc, METHOD_DOCS, MODULES, TYPE_DOCS};

use super::Context;
use crate::features::{self, Document};
use crate::line_index::{Encoding, LineIndex};
use crate::uri::{file_path, file_uri};

/// What a tool answered: its text, and whether that text says why it failed.
#[derive(Debug)]
pub struct ToolResult {
    pub text: String,
    pub is_error: bool,
}

/// The guide for coding agents that teaches the language, which `stone_guide` returns.
const GUIDE: &str = include_str!("../../../docs/agents/stone.md");

/// How much of a snippet's stdout or stderr an answer keeps, since a loop that prints could
/// otherwise fill the agent's context.
const MAX_OUTPUT: usize = 64 * 1024;

/// A tool: its name, what it is for, its arguments' JSON Schema, whether it changes nothing, and
/// what runs it.
struct Tool {
    name: &'static str,
    title: &'static str,
    description: &'static str,
    schema: fn() -> Value,
    read_only: bool,
    run: fn(&Args, &Context) -> Result<String, String>,
}

/// The schema of the arguments that name a position.
fn position_properties() -> serde_json::Map<String, Value> {
    let properties = json!({
        "path": {
            "type": "string",
            "description": "The .stn file, absolute or relative to the server's working directory.",
        },
        "line": {
            "type": "integer",
            "minimum": 1,
            "description": "The 1-based line.",
        },
        "symbol": {
            "type": "string",
            "description": "The name to look at, as written on that line; its first whole-word \
                            appearance is used. Give this or 'column'.",
        },
        "column": {
            "type": "integer",
            "minimum": 1,
            "description": "The 1-based column, in characters, if 'symbol' is not given.",
        },
    });
    match properties {
        Value::Object(map) => map,
        _ => unreachable!(),
    }
}

fn position_schema() -> Value {
    json!({
        "type": "object",
        "properties": position_properties(),
        "required": ["path", "line"],
    })
}

fn references_schema() -> Value {
    let mut properties = position_properties();
    properties.insert(
        "include_declaration".to_string(),
        json!({
            "type": "boolean",
            "description": "Whether to list the definition too (the default).",
        }),
    );
    json!({ "type": "object", "properties": properties, "required": ["path", "line"] })
}

fn rename_schema() -> Value {
    let mut properties = position_properties();
    properties.insert(
        "new_name".to_string(),
        json!({ "type": "string", "description": "The symbol's new name." }),
    );
    json!({ "type": "object", "properties": properties, "required": ["path", "line", "new_name"] })
}

fn path_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "path": {
                "type": "string",
                "description": "A .stn file or a directory, absolute or relative to the server's \
                                working directory.",
            },
        },
        "required": ["path"],
    })
}

fn code_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "code": { "type": "string", "description": "A whole stone program." },
        },
        "required": ["code"],
    })
}

fn run_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "code": { "type": "string", "description": "A whole stone program." },
            "stdin": {
                "type": "string",
                "description": "What the program reads with input(); empty if not given.",
            },
            "args": {
                "type": "array",
                "items": { "type": "string" },
                "description": "What args() returns.",
            },
        },
        "required": ["code"],
    })
}

fn name_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "name": {
                "type": "string",
                "description": "A builtin function (print), method (split or str.split), type \
                                (int), module (math), or module function (math.sqrt).",
            },
        },
        "required": ["name"],
    })
}

fn no_schema() -> Value {
    json!({ "type": "object", "properties": {} })
}

static TOOLS: [Tool; 10] = [
    Tool {
        name: "stone_guide",
        title: "stone language guide",
        description: "Returns the guide to writing stone for coding agents: syntax, types, value \
                      semantics, the standard library, and what Python has that stone does not. \
                      Read it once before writing or changing stone code.",
        schema: no_schema,
        read_only: true,
        run: guide,
    },
    Tool {
        name: "stone_diagnostics",
        title: "Check stone files",
        description: "Type checks a .stn file, or every .stn file under a directory, as part of \
                      its program, and returns every error and warning as `stone check` writes \
                      them (file:line:col: error: message, the line, and a caret), then a count. \
                      Run it after every edit.",
        schema: path_schema,
        read_only: true,
        run: diagnostics,
    },
    Tool {
        name: "stone_check_snippet",
        title: "Check stone code",
        description: "Type checks a stone program given as text, which is not on disk, and \
                      returns its errors and warnings, or 'no problems found'.",
        schema: code_schema,
        read_only: true,
        run: check_snippet,
    },
    Tool {
        name: "stone_run_snippet",
        title: "Run stone code",
        description: "Runs a stone program given as text with the interpreter, in the server's \
                      working directory, and returns its stdout, its stderr, and how it ended: \
                      success, an exit status, or a runtime error at its position. It runs with \
                      a step limit, so a loop that never ends stops with 'ran out of fuel'.",
        schema: run_schema,
        read_only: false,
        run: run_snippet,
    },
    Tool {
        name: "stone_hover",
        title: "Describe a stone symbol",
        description: "Describes what is at a position: a symbol's declaration and type with the \
                      comment above its definition, a builtin's documentation, or the type of \
                      the expression there.",
        schema: position_schema,
        read_only: true,
        run: hover,
    },
    Tool {
        name: "stone_definition",
        title: "Find a stone definition",
        description: "Returns where the symbol at a position is defined, in any file of its \
                      program, or for a builtin, the line documenting it.",
        schema: position_schema,
        read_only: true,
        run: definition,
    },
    Tool {
        name: "stone_references",
        title: "Find stone references",
        description: "Lists every use of the symbol at a position, across every file of its \
                      program, one file:line:col per line.",
        schema: references_schema,
        read_only: true,
        run: references,
    },
    Tool {
        name: "stone_symbols",
        title: "Outline a stone file",
        description: "Outlines a .stn file: its structs with their fields and methods, enums with \
                      their variants, traits, functions with their signatures, and globals, each \
                      with its line.",
        schema: file_schema,
        read_only: true,
        run: symbols,
    },
    Tool {
        name: "stone_rename",
        title: "Rename a stone symbol",
        description: "Renames the symbol at a position everywhere its program uses it, writing \
                      every changed file, and says how many changes each got. It refuses, \
                      changing nothing, if the new name is a keyword or builtin or would clash.",
        schema: rename_schema,
        read_only: false,
        run: rename,
    },
    Tool {
        name: "stone_docs",
        title: "Look up a stone builtin",
        description: "Returns the signature and documentation of a builtin function, method, \
                      type, module, or module function, such as print, split, int, math, or \
                      math.sqrt.",
        schema: name_schema,
        read_only: true,
        run: docs,
    },
];

fn file_schema() -> Value {
    json!({
        "type": "object",
        "properties": {
            "path": {
                "type": "string",
                "description": "The .stn file, absolute or relative to the server's working \
                                directory.",
            },
        },
        "required": ["path"],
    })
}

/// Returns every tool as `tools/list` describes it.
pub fn list() -> Vec<Value> {
    TOOLS
        .iter()
        .map(|tool| {
            json!({
                "name": tool.name,
                "title": tool.title,
                "description": tool.description,
                "inputSchema": (tool.schema)(),
                "annotations": {
                    "title": tool.title,
                    "readOnlyHint": tool.read_only,
                    "destructiveHint": false,
                    "openWorldHint": false,
                },
            })
        })
        .collect()
}

/// Runs the tool `name` with `arguments`, or returns `None` if there is no such tool.
pub fn call(name: &str, arguments: &Value, context: &Context) -> Option<ToolResult> {
    let tool = TOOLS.iter().find(|tool| tool.name == name)?;
    Some(match (tool.run)(&Args(arguments), context) {
        Ok(text) => ToolResult {
            text,
            is_error: false,
        },
        Err(text) => ToolResult {
            text,
            is_error: true,
        },
    })
}

/// A tool's arguments, read with errors that name what is wrong.
struct Args<'a>(&'a Value);

impl Args<'_> {
    fn get(&self, key: &str) -> Option<&Value> {
        self.0.get(key).filter(|v| !v.is_null())
    }

    fn str(&self, key: &str) -> Result<&str, String> {
        self.optional_str(key)?
            .ok_or_else(|| format!("missing argument '{key}'"))
    }

    fn optional_str(&self, key: &str) -> Result<Option<&str>, String> {
        match self.get(key) {
            None => Ok(None),
            Some(value) => value
                .as_str()
                .map(Some)
                .ok_or_else(|| format!("argument '{key}' must be a string")),
        }
    }

    /// Reads a number at least 1, which positions count from.
    fn optional_count(&self, key: &str) -> Result<Option<usize>, String> {
        match self.get(key) {
            None => Ok(None),
            Some(value) => match value.as_u64() {
                Some(n) if n >= 1 => Ok(Some(n as usize)),
                _ => Err(format!(
                    "argument '{key}' must be a whole number of at least 1"
                )),
            },
        }
    }

    fn count(&self, key: &str) -> Result<usize, String> {
        self.optional_count(key)?
            .ok_or_else(|| format!("missing argument '{key}'"))
    }

    fn bool_or(&self, key: &str, default: bool) -> Result<bool, String> {
        match self.get(key) {
            None => Ok(default),
            Some(value) => value
                .as_bool()
                .ok_or_else(|| format!("argument '{key}' must be true or false")),
        }
    }

    fn strings(&self, key: &str) -> Result<Vec<String>, String> {
        let Some(value) = self.get(key) else {
            return Ok(vec![]);
        };
        let error = || format!("argument '{key}' must be a list of strings");
        value
            .as_array()
            .ok_or_else(error)?
            .iter()
            .map(|item| item.as_str().map(str::to_string).ok_or_else(error))
            .collect()
    }
}

/// Returns `path` as an absolute path, resolving it against the working directory.
fn absolute(context: &Context, path: &str) -> Result<PathBuf, String> {
    let joined = context.cwd.join(path);
    joined
        .canonicalize()
        .map(|path| crate::uri::plain(&path))
        .map_err(|_| format!("no file or directory at '{path}'"))
}

/// Writes `path` as an answer shows it: relative to the working directory when inside it.
fn shown_path(context: &Context, path: &Path) -> String {
    let relative = path.strip_prefix(&context.cwd).unwrap_or(path);
    relative.display().to_string()
}

fn guide(_: &Args, _: &Context) -> Result<String, String> {
    Ok(GUIDE.to_string())
}

/// Renders `diagnostic`, from file `file` of `doc`'s program, as `stone check` does.
fn render(context: &Context, doc: &Document, file: FileId, diagnostic: &Diagnostic) -> String {
    let shown = Diagnostic {
        span: diagnostic.span.in_file(file),
        ..diagnostic.map_texts(|text| doc.shown(text))
    };
    shown.render_in(|id| {
        let source = doc.sources.file(id);
        (shown_path(context, &source.path), source.source.as_str())
    })
}

/// Counts errors and warnings for a summary line.
#[derive(Default)]
struct Tally {
    errors: usize,
    warnings: usize,
}

impl Tally {
    fn add(&mut self, diagnostic: &Diagnostic) {
        match diagnostic.severity {
            Severity::Error => self.errors += 1,
            Severity::Warning => self.warnings += 1,
        }
    }

    /// Writes the counts, such as `1 error, 2 warnings`, or `None` if there are none.
    fn counts(&self) -> Option<String> {
        let plural = |n: usize, word: &str| match n {
            1 => format!("1 {word}"),
            _ => format!("{n} {word}s"),
        };
        match (self.errors, self.warnings) {
            (0, 0) => None,
            (errors, 0) => Some(plural(errors, "error")),
            (0, warnings) => Some(plural(warnings, "warning")),
            (errors, warnings) => Some(format!(
                "{}, {}",
                plural(errors, "error"),
                plural(warnings, "warning")
            )),
        }
    }
}

fn diagnostics(args: &Args, context: &Context) -> Result<String, String> {
    let written = args.str("path")?;
    let path = absolute(context, written)?;
    let files = if path.is_dir() {
        let files = stone::driver::stone_files(&path)
            .map_err(|e| format!("cannot list '{written}': {e}"))?;
        if files.is_empty() {
            return Err(format!("no .stn files under '{written}'"));
        }
        files
    } else {
        vec![path]
    };
    let mut text = String::new();
    let mut tally = Tally::default();
    // several files of one program find the same errors in its manifest
    let mut package_errors = HashSet::new();
    for file in &files {
        let doc = Document::in_program(file, &FsSources, Encoding::Utf8)
            .ok_or_else(|| format!("cannot read '{}'", shown_path(context, file)))?;
        for (diagnostic, _) in doc.problems() {
            tally.add(diagnostic);
            text.push_str(&render(context, &doc, doc.file, diagnostic));
        }
        let package_files = doc.sources.files().filter(|(_, f)| f.is_package_file());
        for (id, _) in package_files {
            let found = doc
                .analysis
                .diagnostics
                .iter()
                .filter(|d| d.span.file == id);
            for diagnostic in found {
                let rendered = render(context, &doc, id, diagnostic);
                if package_errors.insert(rendered.clone()) {
                    tally.add(diagnostic);
                    text.push_str(&rendered);
                }
            }
        }
    }
    let files = match files.len() {
        1 => "1 file".to_string(),
        n => format!("{n} files"),
    };
    match tally.counts() {
        Some(counts) => {
            text.push('\n');
            write!(text, "{counts} in {files}").unwrap();
        }
        None => write!(text, "no problems found in {files}").unwrap(),
    }
    Ok(text)
}

fn check_snippet(args: &Args, context: &Context) -> Result<String, String> {
    let code = args.str("code")?;
    let doc = Document::new(code.to_string(), Encoding::Utf8);
    let problems = doc.problems();
    if problems.is_empty() {
        return Ok("no problems found".to_string());
    }
    let mut tally = Tally::default();
    let mut text: String = problems
        .iter()
        .map(|(diagnostic, _)| {
            tally.add(diagnostic);
            render(context, &doc, doc.file, diagnostic)
        })
        .collect();
    text.push('\n');
    text.push_str(&tally.counts().unwrap_or_default());
    Ok(text)
}

/// A writer that keeps the first [`MAX_OUTPUT`] bytes and counts the rest.
#[derive(Default)]
struct Capped {
    kept: Vec<u8>,
    dropped: usize,
}

impl Write for Capped {
    fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
        let room = MAX_OUTPUT.saturating_sub(self.kept.len());
        let taken = room.min(bytes.len());
        self.kept.extend_from_slice(&bytes[..taken]);
        self.dropped += bytes.len() - taken;
        Ok(bytes.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

impl Capped {
    /// Writes what was kept as a labeled section, or nothing if nothing was written.
    fn section(&self, label: &str) -> String {
        if self.kept.is_empty() {
            return String::new();
        }
        let mut text = format!("{label}:\n{}", String::from_utf8_lossy(&self.kept));
        if !text.ends_with('\n') {
            text.push('\n');
        }
        if self.dropped > 0 {
            writeln!(text, "... and {} more bytes not shown", self.dropped).unwrap();
        }
        text.push('\n');
        text
    }
}

fn run_snippet(args: &Args, context: &Context) -> Result<String, String> {
    let code = args.str("code")?.to_string();
    let stdin = args.optional_str("stdin")?.unwrap_or_default().to_string();
    let program_args = args.strings("args")?;
    let fuel = context.fuel;
    let (send, receive) = mpsc::channel();
    // a detached thread, so a program blocked on the network cannot hold the server up
    std::thread::Builder::new()
        .name("stone-mcp-run".to_string())
        .spawn(move || {
            let _ = send.send(run_program(&code, &stdin, &program_args, fuel));
        })
        .map_err(|e| format!("could not start the interpreter: {e}"))?;
    receive.recv_timeout(context.timeout).or_else(|_| {
        Ok(format!(
            "the program was still running after {} seconds, so it was left running in the \
             background; a program that waits for input it never gets, or on the network, \
             runs until it is answered",
            context.timeout.as_secs()
        ))
    })
}

/// Checks and runs `code`, describing its output and how it ended.
fn run_program(code: &str, stdin: &str, args: &[String], fuel: u64) -> String {
    let (sources, checked) =
        stone::driver::load(Path::new(DEFAULT_ENTRY), code, &MapSources::default());
    let render_all = |sources: &SourceMap, diagnostics: &[Diagnostic]| -> String {
        diagnostics.iter().map(|d| sources.render(d)).collect()
    };
    let checked = match checked {
        Ok(checked) => checked,
        Err(Diagnostics(diagnostics)) => {
            return format!(
                "the program did not pass the checker:\n{}",
                render_all(&sources, &diagnostics)
            );
        }
    };
    let mut out = Capped::default();
    let mut errors = Capped::default();
    let limits = Limits {
        fuel,
        ..Limits::DEFAULT
    };
    let ended = stone::driver::run_module_with_errors(
        &checked.module,
        &checked.calls,
        &mut stdin.as_bytes(),
        &mut out,
        &mut errors,
        args,
        limits,
    );
    let mut text = String::new();
    if !checked.warnings.is_empty() {
        text.push_str("warnings:\n");
        text.push_str(&render_all(&sources, &checked.warnings));
        text.push('\n');
    }
    text.push_str(&out.section("stdout"));
    text.push_str(&errors.section("stderr"));
    match ended {
        Ok(()) => text.push_str("exited successfully"),
        Err(error) => {
            if let Some(Exit(status)) = error.downcast_ref::<Exit>() {
                match status {
                    0 => text.push_str("exited successfully"),
                    _ => write!(text, "exited with status {status}").unwrap(),
                }
            } else if let Some(Diagnostics(diagnostics)) = error.downcast_ref::<Diagnostics>() {
                text.push_str(&render_all(&sources, diagnostics));
            } else if let Some(diagnostic) = error.downcast_ref::<Diagnostic>() {
                text.push_str(&sources.render(diagnostic));
            } else {
                let message = error.to_string();
                write!(text, "error: {message}").unwrap();
                if message.contains("fuel") {
                    write!(
                        text,
                        " (a snippet may take {fuel} loop turns and calls; run longer programs \
                         with `stone run`)"
                    )
                    .unwrap();
                }
            }
        }
    }
    text.trim_end().to_string()
}

/// A position a tool was asked about, in the document of its file.
struct At {
    doc: Document,
    uri: Uri,
    position: Position,
    /// The position as the tool was given it, `main.stn:4:9`, for messages.
    shown: String,
}

/// Reads the `path`, `line`, and `symbol` or `column` arguments, analyzing the file.
fn at(args: &Args, context: &Context) -> Result<At, String> {
    let written = args.str("path")?;
    let line = args.count("line")?;
    let symbol = args.optional_str("symbol")?;
    let column = args.optional_count("column")?;
    let path = absolute(context, written)?;
    let doc = Document::in_program(&path, &FsSources, Encoding::Utf8)
        .ok_or_else(|| format!("cannot read '{written}'"))?;
    let lines = doc.text.lines().count();
    let Some(text) = doc.text.lines().nth(line - 1) else {
        return Err(format!(
            "line {line} is past the end of '{written}', which has {lines} lines"
        ));
    };
    let col = match (symbol, column) {
        (Some(symbol), _) => find_symbol(text, symbol).ok_or_else(|| {
            format!(
                "'{symbol}' is not on line {line}, which is: {}",
                text.trim()
            )
        })?,
        (None, Some(column)) => column,
        (None, None) => return Err("give 'symbol' or 'column'".to_string()),
    };
    let uri = file_uri(&path).ok_or_else(|| format!("cannot name '{written}' as a URI"))?;
    let position = doc.index.position(Pos::new(line, col));
    Ok(At {
        doc,
        uri,
        position,
        shown: format!("{}:{line}:{col}", shown_path(context, &path)),
    })
}

/// Returns the 1-based column of the first appearance of `symbol` on `line` that is a whole
/// word, so `add` is not found inside `address`.
///
/// For example, `find_symbol("total = add(1, 2)", "add")` returns `Some(9)`.
fn find_symbol(line: &str, symbol: &str) -> Option<usize> {
    let word = |ch: char| ch.is_alphanumeric() || ch == '_';
    if symbol.is_empty() {
        return None;
    }
    let first = symbol.chars().next()?;
    let last = symbol.chars().last()?;
    line.match_indices(symbol)
        .find(|(start, _)| {
            let before = line[..*start].chars().next_back();
            let after = line[start + symbol.len()..].chars().next();
            !(word(first) && before.is_some_and(word)) && !(word(last) && after.is_some_and(word))
        })
        .map(|(start, _)| line[..start].chars().count() + 1)
}

/// Writes a location as `file:line:col: <the line's text>`.
fn shown_location(context: &Context, location: &Location) -> String {
    let Some(path) = file_path(&location.uri) else {
        return location.uri.as_str().to_string();
    };
    let text = FsSources.read(&path).unwrap_or_default();
    let pos = LineIndex::new(&text, Encoding::Utf8).pos(location.range.start);
    let line = text.lines().nth(pos.line - 1).unwrap_or_default().trim();
    format!(
        "{}:{}:{}: {line}",
        shown_path(context, &path),
        pos.line,
        pos.col
    )
}

fn hover(args: &Args, context: &Context) -> Result<String, String> {
    let at = at(args, context)?;
    let hover = features::hover(&at.doc, at.position)
        .ok_or_else(|| format!("nothing to describe at {}", at.shown))?;
    Ok(match hover.contents {
        HoverContents::Markup(markup) => markup.value,
        HoverContents::Scalar(text) => format!("{text:?}"),
        HoverContents::Array(texts) => format!("{texts:?}"),
    })
}

fn definition(args: &Args, context: &Context) -> Result<String, String> {
    let at = at(args, context)?;
    let location = features::definition(&at.doc, &at.uri, at.position, context.builtins.as_ref())
        .ok_or_else(|| format!("no definition found at {}", at.shown))?;
    Ok(shown_location(context, &location))
}

fn references(args: &Args, context: &Context) -> Result<String, String> {
    let at = at(args, context)?;
    let include = args.bool_or("include_declaration", true)?;
    let mut found: Vec<(String, u32, u32, String)> =
        features::references(&at.doc, &at.uri, at.position, include)
            .iter()
            .map(|location| {
                let start = location.range.start;
                let path = file_path(&location.uri)
                    .map(|p| shown_path(context, &p))
                    .unwrap_or_default();
                (
                    path,
                    start.line,
                    start.character,
                    shown_location(context, location),
                )
            })
            .collect();
    if found.is_empty() {
        return Err(format!("no symbol to find references to at {}", at.shown));
    }
    found.sort();
    found.dedup();
    let lines: Vec<String> = found.into_iter().map(|(.., shown)| shown).collect();
    Ok(lines.join("\n"))
}

fn symbols(args: &Args, context: &Context) -> Result<String, String> {
    let written = args.str("path")?;
    let path = absolute(context, written)?;
    let doc = Document::in_program(&path, &FsSources, Encoding::Utf8)
        .ok_or_else(|| format!("cannot read '{written}'"))?;
    let outline = features::document_symbols(&doc);
    if outline.is_empty() {
        return Ok(format!("'{written}' defines nothing"));
    }
    let mut text = String::new();
    write_outline(&mut text, &doc, &outline, 0);
    Ok(text.trim_end().to_string())
}

/// Writes each symbol as its declaration and line, its members indented below it, leaving out a
/// function's parameters and locals.
fn write_outline(text: &mut String, doc: &Document, symbols: &[DocumentSymbol], depth: usize) {
    for symbol in symbols {
        let line = doc.index.pos(symbol.selection_range.start).line;
        // a struct's or enum's declaration goes on to list its members, which follow anyway
        let detail = symbol.detail.as_deref().and_then(|d| d.lines().next());
        let declaration = detail.unwrap_or(&symbol.name);
        writeln!(text, "{}{declaration} (line {line})", "  ".repeat(depth)).unwrap();
        let function = matches!(symbol.kind, SymbolKind::FUNCTION | SymbolKind::METHOD);
        if let Some(children) = &symbol.children {
            let members: Vec<DocumentSymbol> = children
                .iter()
                .filter(|child| !function || child.kind != SymbolKind::VARIABLE)
                .filter(|child| child.kind != SymbolKind::TYPE_PARAMETER)
                .cloned()
                .collect();
            write_outline(text, doc, &members, depth + 1);
        }
    }
}

fn rename(args: &Args, context: &Context) -> Result<String, String> {
    let new_name = args.str("new_name")?.to_string();
    let at = at(args, context)?;
    let edit = features::rename(&at.doc, &at.uri, at.position, &new_name)?;
    let mut files: Vec<(PathBuf, String, usize)> = vec![];
    for (uri, mut edits) in edit.changes.unwrap_or_default() {
        let path = file_path(&uri).ok_or_else(|| format!("cannot write {}", uri.as_str()))?;
        let mut text = FsSources
            .read(&path)
            .ok_or_else(|| format!("cannot read '{}'", shown_path(context, &path)))?;
        // from the end, so each edit leaves the offsets of those before it alone
        edits.sort_by_key(|e| std::cmp::Reverse((e.range.start.line, e.range.start.character)));
        let count = edits.len();
        for edit in edits {
            let start = offset(&text, edit.range.start);
            let end = offset(&text, edit.range.end);
            text.replace_range(start..end, &edit.new_text);
        }
        files.push((path, text, count));
    }
    if files.is_empty() {
        return Err(format!("nothing to rename at {}", at.shown));
    }
    files.sort();
    // every file is worked out before any is written, so a failure changes nothing
    for (path, text, _) in &files {
        std::fs::write(path, text)
            .map_err(|e| format!("cannot write '{}': {e}", shown_path(context, path)))?;
    }
    let changes: Vec<String> = files
        .iter()
        .map(|(path, _, count)| {
            let changes = match count {
                1 => "1 change".to_string(),
                n => format!("{n} changes"),
            };
            format!("{changes} in {}", shown_path(context, path))
        })
        .collect();
    Ok(format!("renamed to '{new_name}': {}", changes.join(", ")))
}

/// Returns the byte offset in `text` of a UTF-8 position, as the server's documents use.
fn offset(text: &str, position: Position) -> usize {
    let start: usize = text
        .split_inclusive('\n')
        .take(position.line as usize)
        .map(str::len)
        .sum();
    let line = &text[start..];
    let within = (position.character as usize).min(line.find('\n').unwrap_or(line.len()));
    // a position inside a character, which UTF-8 positions never are, rounds down
    let within = (0..=within)
        .rev()
        .find(|&i| line.is_char_boundary(i))
        .unwrap_or(0);
    start + within
}

/// Writes a builtin's documentation, its signature and then what it does.
fn shown_doc(doc: &BuiltinDoc) -> String {
    format!("{}\n\n{}", doc.signature, doc.description)
}

fn docs(args: &Args, _: &Context) -> Result<String, String> {
    let name = args.str("name")?.trim();
    if let Some(module) = MODULES.iter().find(|m| m.name == name) {
        let mut text = format!("module {}: {}\n\nfunctions:", module.name, module.doc);
        for function in module.functions {
            write!(text, "\n- {}: {}", function.signature, function.description).unwrap();
        }
        return Ok(text);
    }
    let mut found: Vec<String> = vec![];
    let functions = MODULES.iter().flat_map(|m| m.functions);
    found.extend(functions.filter(|d| d.name == name).map(shown_doc));
    found.extend(
        BUILTIN_DOCS
            .iter()
            .filter(|d| d.name == name)
            .map(shown_doc),
    );
    found.extend(TYPE_DOCS.iter().filter(|d| d.name == name).map(shown_doc));
    // a method by its name alone, or after the type it is called on, as in `str.split`
    let method = name.rsplit('.').next().unwrap_or(name);
    if found.is_empty() || method == name {
        found.extend(
            METHOD_DOCS
                .iter()
                .filter(|d| d.name == method)
                .map(shown_doc),
        );
    }
    if !found.is_empty() {
        return Ok(found.join("\n\n---\n\n"));
    }
    let mut known: Vec<&str> = BUILTIN_DOCS
        .iter()
        .chain(&TYPE_DOCS)
        .chain(&METHOD_DOCS)
        .chain(MODULES.iter().flat_map(|m| m.functions))
        .map(|d| d.name)
        .chain(MODULES.iter().map(|m| m.name))
        .collect();
    known.sort();
    known.dedup();
    let similar: Vec<&str> = known
        .iter()
        .copied()
        .filter(|known| {
            let short = known.rsplit('.').next().unwrap_or(known);
            short.contains(method) || method.contains(short) || distance(short, method) <= 2
        })
        .collect();
    Err(match similar.is_empty() {
        true => format!(
            "no builtin named '{name}'; the builtins are: {}",
            known.join(", ")
        ),
        false => format!(
            "no builtin named '{name}'; similar names: {}",
            similar.join(", ")
        ),
    })
}

/// How long a snippet may run before its answer says it is still running.
pub const SNIPPET_TIMEOUT: Duration = Duration::from_secs(30);

/// How many loop turns and calls a snippet may take, about a few seconds of work.
pub const SNIPPET_FUEL: u64 = 20_000_000;
