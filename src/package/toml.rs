//! A reader for the part of TOML that `stone.toml` and `stone.lock` use, since stone depends on no
//! TOML library.
//!
//! It reads comments, `[table]` and `[[array of tables]]` headers with one name, `key = value`
//! lines with bare or quoted keys, and values that are strings (basic or literal, on one line),
//! integers, booleans, arrays (which may span lines), and inline tables. Every key and value keeps
//! its span, so errors point at the text, and `stone add` can edit a manifest in place.
//!
//! For example, reading `[dependencies]\njson = { github = "alice/json", version = "1.2.0" }\n`
//! gives a table with one entry, `dependencies`, whose table has one entry, `json`.

use crate::diagnostic::Diagnostic;
use crate::span::{FileId, Pos, Span};

/// A value with the span of its text.
#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub value: Value,
    pub span: Span,
}

/// A TOML value.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Str(String),
    Int(i64),
    Bool(bool),
    Array(Vec<Item>),
    Table(Table),
}

impl Value {
    /// Returns what the value is called in an error, such as `a string`.
    pub fn kind(&self) -> &'static str {
        match self {
            Value::Str(_) => "a string",
            Value::Int(_) => "an integer",
            Value::Bool(_) => "a boolean",
            Value::Array(_) => "an array",
            Value::Table(_) => "a table",
        }
    }
}

/// A table: its entries in the order they were written.
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Table {
    pub entries: Vec<Entry>,
}

impl Table {
    /// Returns the entry named `key`, if there is one.
    pub fn get(&self, key: &str) -> Option<&Entry> {
        self.entries.iter().find(|entry| entry.key == key)
    }
}

/// One `key = value` of a table, or a `[key]` table or `[[key]]` array of tables of the document.
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    pub key: String,
    pub key_span: Span,
    pub item: Item,
}

/// Reads a TOML document in `file`, returning its top-level table or the first error.
///
/// For example, `parse("name = \"geo\"\n", file)` has one entry, `name`, holding the string
/// `geo`, and `parse("name = geo\n", file)` fails at `geo`.
pub fn parse(source: &str, file: FileId) -> Result<Table, Diagnostic> {
    let mut reader = Reader {
        chars: source.chars().collect(),
        at: 0,
        pos: Pos::new(1, 1),
        file,
    };
    reader.document()
}

/// Where a `[header]` puts the lines after it.
enum Current {
    Root,
    /// The top-level entry the header made, by index.
    Table(usize),
    /// The last table of the top-level array of tables, by index.
    ArrayTable(usize),
}

struct Reader {
    chars: Vec<char>,
    at: usize,
    pos: Pos,
    file: FileId,
}

impl Reader {
    fn peek(&self) -> Option<char> {
        self.chars.get(self.at).copied()
    }

    fn bump(&mut self) -> Option<char> {
        let c = self.peek()?;
        self.at += 1;
        if c == '\n' {
            self.pos = Pos::new(self.pos.line + 1, 1);
        } else {
            self.pos.col += 1;
        }
        Some(c)
    }

    fn span_from(&self, start: Pos) -> Span {
        Span::new(start, self.pos).in_file(self.file)
    }

    fn error(&self, message: impl Into<String>) -> Diagnostic {
        Diagnostic::error(
            Span::new(self.pos, Pos::new(self.pos.line, self.pos.col + 1)).in_file(self.file),
            message,
        )
    }

    /// Returns what is next, for errors: the character in quotes, or `end of line`.
    fn found(&self) -> String {
        match self.peek() {
            None => "end of file".to_string(),
            Some('\n' | '\r') => "end of line".to_string(),
            Some(c) => format!("'{c}'"),
        }
    }

    /// Skips spaces and tabs.
    fn blanks(&mut self) {
        while matches!(self.peek(), Some(' ' | '\t')) {
            self.bump();
        }
    }

    /// Skips blanks and a comment, then requires the end of the line, which it skips too.
    fn end_of_line(&mut self) -> Result<(), Diagnostic> {
        self.blanks();
        if self.peek() == Some('#') {
            while !matches!(self.peek(), None | Some('\n')) {
                self.bump();
            }
        }
        if self.peek() == Some('\r') {
            self.bump();
        }
        match self.peek() {
            None => Ok(()),
            Some('\n') => {
                self.bump();
                Ok(())
            }
            _ => Err(self.error(format!(
                "expected the end of the line, found {}",
                self.found()
            ))),
        }
    }

    /// Skips blank lines, comments, and newlines, as between the values of an array.
    fn whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(' ' | '\t' | '\r' | '\n') => {
                    self.bump();
                }
                Some('#') => {
                    while !matches!(self.peek(), None | Some('\n')) {
                        self.bump();
                    }
                }
                _ => return,
            }
        }
    }

    fn document(&mut self) -> Result<Table, Diagnostic> {
        let mut root = Table::default();
        let mut current = Current::Root;
        loop {
            self.blanks();
            match self.peek() {
                None => return Ok(root),
                Some('\n' | '\r' | '#') => self.end_of_line()?,
                Some('[') => {
                    current = self.header(&mut root)?;
                    self.end_of_line()?;
                }
                Some(_) => {
                    let entry = self.key_value()?;
                    let table = match current {
                        Current::Root => &mut root,
                        Current::Table(i) => match &mut root.entries[i].item.value {
                            Value::Table(table) => table,
                            _ => unreachable!("a header makes a table"),
                        },
                        Current::ArrayTable(i) => match &mut root.entries[i].item.value {
                            Value::Array(items) => match &mut items.last_mut().unwrap().value {
                                Value::Table(table) => table,
                                _ => unreachable!("an array header makes tables"),
                            },
                            _ => unreachable!("an array header makes an array"),
                        },
                    };
                    insert(table, entry)?;
                    self.end_of_line()?;
                }
            }
        }
    }

    /// Reads a `[name]` or `[[name]]` header, adding its table to `root`.
    fn header(&mut self, root: &mut Table) -> Result<Current, Diagnostic> {
        let start = self.pos;
        self.bump();
        let array = self.peek() == Some('[');
        if array {
            self.bump();
        }
        self.blanks();
        let (key, key_span) = self.key()?;
        self.blanks();
        if self.peek() == Some('.') {
            return Err(self.error("dotted table names are not supported, so write one name"));
        }
        for _ in 0..if array { 2 } else { 1 } {
            if self.peek() != Some(']') {
                return Err(self.error(format!("expected ']', found {}", self.found())));
            }
            self.bump();
        }
        let span = self.span_from(start);
        let table = Item {
            value: Value::Table(Table::default()),
            span,
        };
        let existing = root.entries.iter().position(|entry| entry.key == key);
        match (existing, array) {
            (None, false) => {
                root.entries.push(Entry {
                    key,
                    key_span,
                    item: table,
                });
                Ok(Current::Table(root.entries.len() - 1))
            }
            (None, true) => {
                root.entries.push(Entry {
                    key,
                    key_span,
                    item: Item {
                        value: Value::Array(vec![table]),
                        span,
                    },
                });
                Ok(Current::ArrayTable(root.entries.len() - 1))
            }
            (Some(i), true)
                if matches!(&root.entries[i].item.value, Value::Array(items)
                if items.iter().all(|item| matches!(item.value, Value::Table(_)))) =>
            {
                if let Value::Array(items) = &mut root.entries[i].item.value {
                    items.push(table);
                }
                Ok(Current::ArrayTable(i))
            }
            (Some(_), _) => Err(Diagnostic::error(
                key_span,
                format!("'{key}' is already defined"),
            )),
        }
    }

    /// Reads a bare key (letters, digits, `_`, and `-`) or a quoted one.
    fn key(&mut self) -> Result<(String, Span), Diagnostic> {
        let start = self.pos;
        let key = match self.peek() {
            Some('"') => self.basic_string()?,
            Some('\'') => self.literal_string()?,
            Some(c) if is_bare(c) => {
                let mut key = String::new();
                while let Some(c) = self.peek().filter(|c| is_bare(*c)) {
                    key.push(c);
                    self.bump();
                }
                key
            }
            _ => return Err(self.error(format!("expected a key, found {}", self.found()))),
        };
        Ok((key, self.span_from(start)))
    }

    /// Reads `key = value`.
    fn key_value(&mut self) -> Result<Entry, Diagnostic> {
        let (key, key_span) = self.key()?;
        self.blanks();
        if self.peek() == Some('.') {
            return Err(self.error("dotted keys are not supported, so write one name"));
        }
        if self.peek() != Some('=') {
            return Err(self.error(format!(
                "expected '=' after the key, found {}",
                self.found()
            )));
        }
        self.bump();
        self.blanks();
        let item = self.value()?;
        Ok(Entry {
            key,
            key_span,
            item,
        })
    }

    fn value(&mut self) -> Result<Item, Diagnostic> {
        let start = self.pos;
        let value = match self.peek() {
            Some('"') => {
                if self.chars[self.at..].starts_with(&['"', '"', '"']) {
                    return Err(self.error("multi-line strings are not supported"));
                }
                Value::Str(self.basic_string()?)
            }
            Some('\'') => {
                if self.chars[self.at..].starts_with(&['\'', '\'', '\'']) {
                    return Err(self.error("multi-line strings are not supported"));
                }
                Value::Str(self.literal_string()?)
            }
            Some('[') => self.array()?,
            Some('{') => self.inline_table()?,
            Some(c) if c.is_ascii_digit() || c == '-' || c == '+' => self.integer()?,
            Some(c) if c.is_ascii_alphabetic() => {
                let mut word = String::new();
                while let Some(c) = self.peek().filter(|c| c.is_ascii_alphanumeric()) {
                    word.push(c);
                    self.bump();
                }
                match word.as_str() {
                    "true" => Value::Bool(true),
                    "false" => Value::Bool(false),
                    _ => {
                        return Err(Diagnostic::error(
                            self.span_from(start),
                            format!("expected a value, found '{word}' (write a string in quotes)"),
                        ));
                    }
                }
            }
            _ => return Err(self.error(format!("expected a value, found {}", self.found()))),
        };
        Ok(Item {
            value,
            span: self.span_from(start),
        })
    }

    fn integer(&mut self) -> Result<Value, Diagnostic> {
        let start = self.pos;
        let mut text = String::new();
        if let Some(sign @ ('-' | '+')) = self.peek() {
            text.push(sign);
            self.bump();
        }
        while let Some(c) = self.peek().filter(|c| c.is_ascii_digit() || *c == '_') {
            if c != '_' {
                text.push(c);
            }
            self.bump();
        }
        text.parse().map(Value::Int).map_err(|_| {
            Diagnostic::error(
                self.span_from(start),
                "expected an integer (floats and dates are not supported)",
            )
        })
    }

    fn array(&mut self) -> Result<Value, Diagnostic> {
        self.bump();
        let mut items = vec![];
        loop {
            self.whitespace();
            if self.peek() == Some(']') {
                self.bump();
                return Ok(Value::Array(items));
            }
            items.push(self.value()?);
            self.whitespace();
            match self.peek() {
                Some(',') => {
                    self.bump();
                }
                Some(']') => {}
                _ => return Err(self.error(format!("expected ',' or ']', found {}", self.found()))),
            }
        }
    }

    fn inline_table(&mut self) -> Result<Value, Diagnostic> {
        self.bump();
        let mut table = Table::default();
        self.blanks();
        if self.peek() == Some('}') {
            self.bump();
            return Ok(Value::Table(table));
        }
        loop {
            self.blanks();
            let entry = self.key_value()?;
            insert(&mut table, entry)?;
            self.blanks();
            match self.peek() {
                Some(',') => {
                    self.bump();
                }
                Some('}') => {
                    self.bump();
                    return Ok(Value::Table(table));
                }
                _ => {
                    return Err(self.error(format!(
                        "expected ',' or '}}' in an inline table, which stays on one line, \
                         found {}",
                        self.found()
                    )));
                }
            }
        }
    }

    fn basic_string(&mut self) -> Result<String, Diagnostic> {
        self.bump();
        let mut text = String::new();
        loop {
            match self.peek() {
                None | Some('\n' | '\r') => return Err(self.error("this string is not closed")),
                Some('"') => {
                    self.bump();
                    return Ok(text);
                }
                Some('\\') => {
                    self.bump();
                    let escaped = match self.peek() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some('t') => '\t',
                        Some('r') => '\r',
                        Some('b') => '\u{8}',
                        Some('f') => '\u{c}',
                        Some(c @ ('u' | 'U')) => {
                            self.bump();
                            let digits = if c == 'u' { 4 } else { 8 };
                            let mut code = 0;
                            for _ in 0..digits {
                                let Some(digit) = self.peek().and_then(|c| c.to_digit(16)) else {
                                    return Err(self.error("expected a hexadecimal digit"));
                                };
                                code = code * 16 + digit;
                                self.bump();
                            }
                            let Some(c) = char::from_u32(code) else {
                                return Err(self.error("this escape is not a character"));
                            };
                            text.push(c);
                            continue;
                        }
                        _ => return Err(self.error(format!("unknown escape {}", self.found()))),
                    };
                    self.bump();
                    text.push(escaped);
                }
                Some(c) => {
                    self.bump();
                    text.push(c);
                }
            }
        }
    }

    fn literal_string(&mut self) -> Result<String, Diagnostic> {
        self.bump();
        let mut text = String::new();
        loop {
            match self.peek() {
                None | Some('\n' | '\r') => return Err(self.error("this string is not closed")),
                Some('\'') => {
                    self.bump();
                    return Ok(text);
                }
                Some(c) => {
                    self.bump();
                    text.push(c);
                }
            }
        }
    }
}

/// Adds `entry` to `table`, unless it already has the key.
fn insert(table: &mut Table, entry: Entry) -> Result<(), Diagnostic> {
    if table.get(&entry.key).is_some() {
        return Err(Diagnostic::error(
            entry.key_span,
            format!("'{}' is already defined", entry.key),
        ));
    }
    table.entries.push(entry);
    Ok(())
}

/// Returns whether `c` may be in a bare key.
fn is_bare(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_' || c == '-'
}

/// Writes `text` as a TOML basic string, quoted and escaped.
///
/// For example, `quote("a\"b")` returns `"a\"b"` with the quote escaped.
pub fn quote(text: &str) -> String {
    let mut quoted = String::from("\"");
    for c in text.chars() {
        match c {
            '"' => quoted.push_str("\\\""),
            '\\' => quoted.push_str("\\\\"),
            '\n' => quoted.push_str("\\n"),
            '\t' => quoted.push_str("\\t"),
            '\r' => quoted.push_str("\\r"),
            c if c.is_control() => quoted.push_str(&format!("\\u{:04X}", c as u32)),
            c => quoted.push(c),
        }
    }
    quoted.push('"');
    quoted
}

#[cfg(test)]
mod tests {
    use super::*;

    fn read(source: &str) -> Table {
        parse(source, FileId(1)).unwrap_or_else(|e| panic!("should parse: {e}"))
    }

    fn error(source: &str) -> (String, usize, usize) {
        let e = parse(source, FileId(1)).expect_err("should fail");
        (e.message, e.span.start.line, e.span.start.col)
    }

    fn string(table: &Table, key: &str) -> String {
        match &table.get(key).expect("key").item.value {
            Value::Str(s) => s.clone(),
            other => panic!("not a string: {other:?}"),
        }
    }

    fn table<'a>(table: &'a Table, key: &str) -> &'a Table {
        match &table.get(key).expect("key").item.value {
            Value::Table(t) => t,
            other => panic!("not a table: {other:?}"),
        }
    }

    #[test]
    fn keys_hold_strings_integers_and_booleans() {
        let doc =
            read("name = \"geo\"  # a comment\ncount = 3\nyes = true\n'quoted key' = 'raw\\'\n");
        assert_eq!(string(&doc, "name"), "geo");
        assert_eq!(doc.get("count").unwrap().item.value, Value::Int(3));
        assert_eq!(doc.get("yes").unwrap().item.value, Value::Bool(true));
        assert_eq!(string(&doc, "quoted key"), "raw\\");
    }

    #[test]
    fn headers_hold_the_lines_after_them() {
        let doc = read(
            "top = \"t\"\n\n[package]\nname = \"geo\"\n\n[dependencies]\n\
             json = { github = \"alice/json\", version = \"1.2.0\" }\n",
        );
        assert_eq!(string(&doc, "top"), "t");
        assert_eq!(string(table(&doc, "package"), "name"), "geo");
        let json = table(table(&doc, "dependencies"), "json");
        assert_eq!(string(json, "github"), "alice/json");
        assert_eq!(string(json, "version"), "1.2.0");
    }

    #[test]
    fn array_headers_make_a_table_each() {
        let doc = read("[[package]]\nname = \"a\"\n[[package]]\nname = \"b\"\n");
        let Value::Array(items) = &doc.get("package").unwrap().item.value else {
            panic!("an array");
        };
        let names: Vec<String> = items
            .iter()
            .map(|item| match &item.value {
                Value::Table(t) => string(t, "name"),
                _ => panic!("tables"),
            })
            .collect();
        assert_eq!(names, ["a", "b"]);
    }

    #[test]
    fn arrays_may_span_lines() {
        let doc = read("xs = [\n  \"a\", # first\n  \"b\",\n]\n");
        let Value::Array(items) = &doc.get("xs").unwrap().item.value else {
            panic!("an array");
        };
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn strings_read_escapes() {
        let doc = read("s = \"a\\\"b\\u00e9\\n\"\n");
        assert_eq!(string(&doc, "s"), "a\"b\u{e9}\n");
        assert_eq!(quote("a\"b\n"), "\"a\\\"b\\n\"");
    }

    #[test]
    fn keys_and_values_keep_their_spans() {
        let doc = read("[dependencies]\njson = { version = \"1.0\" }\n");
        let deps = table(&doc, "dependencies");
        let json = deps.get("json").unwrap();
        assert_eq!(
            (json.key_span.start, json.key_span.end),
            (Pos::new(2, 1), Pos::new(2, 5))
        );
        assert_eq!(json.key_span.file, FileId(1));
        let version = table(deps, "json").get("version").unwrap();
        assert_eq!(version.item.span.start, Pos::new(2, 20));
    }

    #[test]
    fn errors_point_at_the_mistake() {
        assert_eq!(
            error("name = geo\n"),
            (
                "expected a value, found 'geo' (write a string in quotes)".to_string(),
                1,
                8
            )
        );
        assert_eq!(
            error("name \"geo\"\n"),
            ("expected '=' after the key, found '\"'".to_string(), 1, 6)
        );
        assert_eq!(
            error("a = 1\na = 2\n"),
            ("'a' is already defined".to_string(), 2, 1)
        );
        assert_eq!(
            error("[package]\n[package]\n"),
            ("'package' is already defined".to_string(), 2, 2)
        );
        assert_eq!(
            error("s = \"open\n"),
            ("this string is not closed".to_string(), 1, 10)
        );
        assert_eq!(
            error("a = 1 2\n"),
            ("expected the end of the line, found '2'".to_string(), 1, 7)
        );
        assert_eq!(
            error("t = { a = 1,\n b = 2 }\n").0,
            "expected a key, found end of line"
        );
        assert_eq!(
            error("a.b = 1\n").0,
            "dotted keys are not supported, so write one name"
        );
    }

    #[test]
    fn windows_line_endings_are_read() {
        let doc = read("a = 1\r\nb = 2\r\n");
        assert_eq!(doc.entries.len(), 2);
    }
}
