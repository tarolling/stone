use super::*;
use lsp_types::{CompletionItemKind, DiagnosticSeverity, HoverContents, Range, SymbolKind};
use std::path::Path;
use stone::project::MapSources;

const SOURCE: &str = "\
total = 0
fn add(a: int, b: int) -> int:
    sum = a + b
    ret sum
for i in range(3):
    total = add(total, i)
print(total, \"é\".len())
";

fn doc(text: &str) -> Document {
    Document::new(text.to_string(), Encoding::Utf16)
}

fn uri() -> Uri {
    "file:///demo.stn".parse().unwrap()
}

/// Returns an LSP position from 1-based line and col, which are easier to read against source.
fn at(line: u32, col: u32) -> Position {
    Position::new(line - 1, col - 1)
}

fn hover_text(doc: &Document, pos: Position) -> Option<String> {
    let hover = hover(doc, pos)?;
    let HoverContents::Markup(markup) = hover.contents else {
        panic!("expected markup");
    };
    Some(markup.value)
}

#[test]
fn diagnostics_have_ranges_and_messages() {
    let doc = doc("x = 1\nx = \"s\"\n");
    let diagnostics = diagnostics(&doc);
    assert_eq!(diagnostics.len(), 1);
    assert_eq!(
        diagnostics[0].range,
        Range::new(Position::new(1, 4), Position::new(1, 7))
    );
    assert_eq!(diagnostics[0].severity, Some(DiagnosticSeverity::ERROR));
    assert_eq!(
        diagnostics[0].message,
        "cannot assign str to 'x', which is int"
    );
    assert_eq!(diagnostics[0].source.as_deref(), Some("stone"));
}

#[test]
fn a_valid_document_has_no_diagnostics() {
    assert_eq!(diagnostics(&doc(SOURCE)), []);
}

#[test]
fn hover_shows_a_function_signature() {
    assert_eq!(
        hover_text(&doc(SOURCE), at(6, 14)).unwrap(),
        "```stone\nfn add(a: int, b: int) -> int\n```"
    );
}

#[test]
fn hover_shows_variables_with_their_kind() {
    let doc = doc(SOURCE);
    assert_eq!(
        hover_text(&doc, at(3, 11)).unwrap(),
        "```stone\n(parameter) a: int\n```"
    );
    assert_eq!(
        hover_text(&doc, at(4, 9)).unwrap(),
        "```stone\n(local) sum: int\n```"
    );
    assert_eq!(
        hover_text(&doc, at(1, 1)).unwrap(),
        "```stone\n(global) total: int\n```"
    );
}

const DOCUMENTED: &str = "\
// the largest value tried
limit = 3

// how many times `n` halves
// before it reaches 1
//
// `n` must be positive.
fn halvings(n: int) -> int:
    // the count so far
    count = 0
    while n > 1:
        n = n / 2
        count = count + 1
    ret count
print(halvings(limit))
";

#[test]
fn hover_shows_the_comment_above_a_function() {
    let doc = doc(DOCUMENTED);
    assert_eq!(
        hover_text(&doc, at(8, 5)).unwrap(),
        "```stone\nfn halvings(n: int) -> int\n```\n\
         how many times `n` halves\nbefore it reaches 1\n\n`n` must be positive."
    );
}

#[test]
fn hover_shows_the_comment_above_a_variable() {
    let doc = doc(DOCUMENTED);
    assert_eq!(
        hover_text(&doc, at(2, 1)).unwrap(),
        "```stone\n(global) limit: int\n```\nthe largest value tried"
    );
    assert_eq!(
        hover_text(&doc, at(10, 5)).unwrap(),
        "```stone\n(local) count: int\n```\nthe count so far"
    );
}

#[test]
fn hover_on_a_use_shows_the_definitions_comment() {
    let doc = doc(DOCUMENTED);
    assert_eq!(
        hover_text(&doc, at(15, 16)).unwrap(),
        "```stone\n(global) limit: int\n```\nthe largest value tried"
    );
    assert!(
        hover_text(&doc, at(15, 7))
            .unwrap()
            .ends_with("`n` must be positive."),
    );
}

#[test]
fn a_blank_line_detaches_a_comment() {
    let doc = doc("// about nothing\n\nx = 1\n");
    assert_eq!(
        hover_text(&doc, at(3, 1)).unwrap(),
        "```stone\n(global) x: int\n```"
    );
}

#[test]
fn a_trailing_comment_is_not_documentation() {
    let doc = doc("y = 1 // about y\nx = 1\n");
    assert_eq!(
        hover_text(&doc, at(2, 1)).unwrap(),
        "```stone\n(global) x: int\n```"
    );
}

#[test]
fn only_a_definition_that_starts_its_line_is_documented() {
    let doc = doc("// about the loop\nfor i in range(3):\n    print(i)\n");
    assert_eq!(
        hover_text(&doc, at(2, 5)).unwrap(),
        "```stone\n(global) i: int\n```"
    );
}

#[test]
fn parameters_have_no_documentation() {
    let doc = doc("// about f\nfn f(n: int) -> int:\n    ret n\nprint(f(1))\n");
    assert_eq!(
        hover_text(&doc, at(2, 7)).unwrap(),
        "```stone\n(parameter) n: int\n```"
    );
}

#[test]
fn hover_documents_builtins() {
    let text = hover_text(&doc(SOURCE), at(7, 2)).unwrap();
    assert!(
        text.starts_with("```stone\nprint(values...)\n```\n"),
        "{text}"
    );
}

#[test]
fn hover_documents_methods() {
    // the `len` in `"é".len()`
    let text = hover_text(&doc(SOURCE), at(7, 18)).unwrap();
    assert!(
        text.starts_with("```stone\n(str | list[T] | map[K, V]).len() -> int\n```\n"),
        "{text}"
    );
    // a variable named like a method is still a variable
    let text = hover_text(&doc("len = 1\nx = len\n"), at(2, 5)).unwrap();
    assert_eq!(text, "```stone\n(global) len: int\n```");
}

#[test]
fn hover_shows_the_type_of_other_expressions() {
    // the `+` in `a + b`
    assert_eq!(
        hover_text(&doc(SOURCE), at(3, 13)).unwrap(),
        "```stone\nint\n```"
    );
}

#[test]
fn hover_on_nothing_is_empty() {
    assert_eq!(hover(&doc(SOURCE), at(2, 1)), None);
}

const VALUES: &str = "\
fn double(x: int) -> int:
    ret x * 2
fn apply(f: fn(int) -> int, x: int) -> int:
    ret f(x)
g = double
print(apply(double, 1), g(2))
";

#[test]
fn function_values_hover_with_their_type_and_lead_to_their_function() {
    let doc = doc(VALUES);
    assert_eq!(
        hover_text(&doc, at(3, 10)).unwrap(),
        "```stone\n(parameter) f: fn(int) -> int\n```"
    );
    assert_eq!(
        hover_text(&doc, at(5, 1)).unwrap(),
        "```stone\n(global) g: fn(int) -> int\n```"
    );
    // `double` named as a value is the function
    assert_eq!(
        hover_text(&doc, at(6, 14)).unwrap(),
        "```stone\nfn double(x: int) -> int\n```"
    );
    let location = definition(&doc, &uri(), at(5, 6), None).unwrap();
    assert_eq!(
        location.range,
        Range::new(Position::new(0, 3), Position::new(0, 9))
    );
    let starts: Vec<Position> = references(&doc, &uri(), at(1, 5), false)
        .into_iter()
        .map(|l| l.range.start)
        .collect();
    assert_eq!(starts, [Position::new(4, 4), Position::new(5, 12)]);
}

#[test]
fn definition_jumps_to_the_name() {
    let doc = doc(SOURCE);
    let location = definition(&doc, &uri(), at(6, 14), None).unwrap();
    assert_eq!(
        location.range,
        Range::new(Position::new(1, 3), Position::new(1, 6))
    );
    assert_eq!(location.uri, uri());
    // without the reference file, builtins have nowhere to go
    assert_eq!(definition(&doc, &uri(), at(7, 2), None), None);
}

#[test]
fn definition_of_a_builtin_opens_the_reference_file() {
    let reference: Uri = "file:///cache/builtins.stn".parse().unwrap();
    let builtins = Builtins::new(reference.clone());
    let doc = doc(SOURCE);

    let location = definition(&doc, &uri(), at(7, 2), Some(&builtins)).unwrap();
    assert_eq!(location.uri, reference);
    // the `print` in the reference's `// print(values...) -> none` line
    let text = stone::stdlib::builtins_reference();
    let line = text
        .lines()
        .position(|l| l.starts_with("// print("))
        .unwrap() as u32;
    assert_eq!(
        location.range,
        Range::new(Position::new(line, 3), Position::new(line, 8))
    );

    // the `len` in the reference's `// (str | list[T] | map[K, V]).len() -> int` line
    let len = definition(&doc, &uri(), at(7, 18), Some(&builtins)).unwrap();
    let line = text
        .lines()
        .position(|l| l.starts_with("// (str | list[T] | map[K, V]).len("))
        .unwrap() as u32;
    assert_eq!(
        len.range,
        Range::new(Position::new(line, 31), Position::new(line, 34))
    );
}

#[test]
fn references_list_every_use() {
    let doc = doc(SOURCE);
    let starts = |include: bool| -> Vec<Position> {
        references(&doc, &uri(), at(6, 5), include)
            .into_iter()
            .map(|l| l.range.start)
            .collect()
    };
    assert_eq!(
        starts(true),
        [
            Position::new(0, 0),
            Position::new(5, 4),
            Position::new(5, 16),
            Position::new(6, 6)
        ]
    );
    assert_eq!(starts(false).len(), 3);
}

#[test]
fn rename_edits_every_reference() {
    let doc = doc(SOURCE);
    let edit = rename(&doc, &uri(), at(3, 5), "result").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    assert_eq!(starts, [Position::new(2, 4), Position::new(3, 8)]);
    assert!(edits.iter().all(|e| e.new_text == "result"));
}

#[test]
fn rename_edits_an_augmented_name_once() {
    let doc = doc("total = 0\ntotal += 2\nprint(total)\n");
    let edit = rename(&doc, &uri(), at(2, 1), "sum").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    assert_eq!(
        starts,
        [
            Position::new(0, 0),
            Position::new(1, 0),
            Position::new(2, 6)
        ]
    );
}

#[test]
fn names_inside_a_strings_values_work_like_any_other() {
    let doc = doc("count = 2\nprint(\"{count} and {count + 1}\")\n");
    assert_eq!(
        hover_text(&doc, at(2, 9)).unwrap(),
        "```stone\n(global) count: int\n```"
    );
    let location = definition(&doc, &uri(), at(2, 21), None).unwrap();
    assert_eq!(
        location.range,
        Range::new(Position::new(0, 0), Position::new(0, 5))
    );
    let edit = rename(&doc, &uri(), at(2, 9), "n").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    assert_eq!(
        starts,
        [
            Position::new(0, 0),
            Position::new(1, 8),
            Position::new(1, 20)
        ]
    );
}

#[test]
fn rename_rejects_bad_names() {
    let doc = doc(SOURCE);
    assert_eq!(
        rename(&doc, &uri(), at(3, 5), "ret").unwrap_err(),
        "'ret' is a keyword"
    );
    assert_eq!(
        rename(&doc, &uri(), at(3, 5), "2x").unwrap_err(),
        "'2x' is not a valid name"
    );
    assert_eq!(
        rename(&doc, &uri(), at(3, 5), "print").unwrap_err(),
        "'print' is a builtin"
    );
    // `a` is already a parameter where `sum` is used
    assert_eq!(
        rename(&doc, &uri(), at(3, 5), "a").unwrap_err(),
        "'a' is already defined here"
    );
    assert_eq!(
        rename(&doc, &uri(), at(2, 1), "x").unwrap_err(),
        "nothing to rename here"
    );
}

#[test]
fn prepare_rename_finds_the_name() {
    let doc = doc(SOURCE);
    assert_eq!(
        prepare_rename(&doc, at(6, 14)),
        Some(PrepareRenameResponse::Range(Range::new(
            Position::new(5, 12),
            Position::new(5, 15)
        )))
    );
    assert_eq!(prepare_rename(&doc, at(7, 2)), None);
}

#[test]
fn document_symbols_nest_locals_in_functions() {
    let symbols = document_symbols(&doc(SOURCE));
    let outline: Vec<(String, SymbolKind, Vec<String>)> = symbols
        .into_iter()
        .map(|s| {
            let children = s
                .children
                .unwrap_or_default()
                .into_iter()
                .map(|c| c.name)
                .collect();
            (s.name, s.kind, children)
        })
        .collect();
    assert_eq!(
        outline,
        [
            ("total".to_string(), SymbolKind::VARIABLE, vec![]),
            (
                "add".to_string(),
                SymbolKind::FUNCTION,
                vec!["a".to_string(), "b".to_string(), "sum".to_string()]
            ),
            ("i".to_string(), SymbolKind::VARIABLE, vec![]),
        ]
    );
}

#[test]
fn completion_offers_what_is_in_scope() {
    let doc = doc(SOURCE);
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        completion(&doc, pos, &MapSources::default())
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect()
    };
    let inside = labels(at(3, 5));
    assert!(inside.contains(&("sum".to_string(), Some(CompletionItemKind::VARIABLE))));
    assert!(inside.contains(&("a".to_string(), Some(CompletionItemKind::VARIABLE))));
    assert!(inside.contains(&("add".to_string(), Some(CompletionItemKind::FUNCTION))));
    assert!(inside.contains(&("print".to_string(), Some(CompletionItemKind::FUNCTION))));
    assert!(inside.contains(&("while".to_string(), Some(CompletionItemKind::KEYWORD))));

    let outside = labels(at(7, 1));
    assert!(!outside.iter().any(|(label, _)| label == "sum"));
    assert!(outside.iter().any(|(label, _)| label == "total"));
}

#[test]
fn completion_after_a_dot_offers_methods() {
    let untyped = doc("ys.\n");
    let doc = doc("xs = []\nxs.\nxs.le\n");
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        completion(&doc, pos, &MapSources::default())
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect()
    };
    let methods: Vec<_> = [
        "len",
        "append",
        "pop",
        "insert",
        "extend",
        "sort",
        "sort_by_key",
        "map",
        "filter",
        "any",
        "all",
        "min_by_key",
        "max_by_key",
        "strip",
        "split",
        "join",
        "find",
        "startswith",
        "endswith",
        "is_some",
        "is_none",
        "unwrap",
        "unwrap_or",
        "is_ok",
        "is_err",
        "unwrap_err",
        "ok",
        "err",
        "parse_int",
        "parse_float",
        "contains",
        "get",
        "keys",
        "values",
        "items",
        "eq",
        "cmp",
        "display",
        "send",
        "read_line",
        "read",
        "read_all",
        "accept",
        "port",
        "close",
    ]
    .iter()
    .map(|name| (name.to_string(), Some(CompletionItemKind::METHOD)))
    .collect();
    // a list offers the list methods, even once a name is started, and display, as every
    // value does
    let mut list_methods = methods[..13].to_vec();
    list_methods.push(("contains".to_string(), Some(CompletionItemKind::METHOD)));
    list_methods.push(("display".to_string(), Some(CompletionItemKind::METHOD)));
    assert_eq!(labels(at(2, 4)), list_methods);
    assert_eq!(labels(at(3, 6)), list_methods);
    // and a receiver of unknown type offers every method
    let unknown: Vec<_> = completion(&untyped, at(1, 4), &MapSources::default())
        .into_iter()
        .map(|item| (item.label, item.kind))
        .collect();
    assert_eq!(unknown, methods);
    // and methods are not offered as functions
    let outside = labels(at(1, 1));
    assert!(!outside.iter().any(|(label, _)| label == "len"));
}

#[test]
fn completion_after_a_dot_offers_the_methods_of_the_receivers_type() {
    let doc = doc(
        "xs = [1]\nxs.\ns = \"a\"\ns.\nm = {\"a\": 1}\nm.\no = some(1)\no.\nn = 1\nn.\n\
         r = \"1\".parse_int()\nr.\n",
    );
    let labels = |pos: Position| -> Vec<String> {
        completion(&doc, pos, &MapSources::default())
            .into_iter()
            .map(|item| item.label)
            .collect()
    };
    assert_eq!(
        labels(at(2, 4)),
        [
            "len",
            "append",
            "pop",
            "insert",
            "extend",
            "sort",
            "sort_by_key",
            "map",
            "filter",
            "any",
            "all",
            "min_by_key",
            "max_by_key",
            "contains",
            "display"
        ]
    );
    assert_eq!(
        labels(at(4, 3)),
        [
            "len",
            "strip",
            "split",
            "join",
            "find",
            "startswith",
            "endswith",
            "parse_int",
            "parse_float",
            "contains",
            "eq",
            "cmp",
            "display"
        ]
    );
    assert_eq!(
        labels(at(6, 3)),
        [
            "len", "pop", "contains", "get", "keys", "values", "items", "display"
        ]
    );
    assert_eq!(
        labels(at(8, 3)),
        ["is_some", "is_none", "unwrap", "unwrap_or", "eq", "display"]
    );
    // a number has the methods of the prelude's traits
    assert_eq!(labels(at(10, 3)), ["eq", "cmp", "display"]);
    assert_eq!(
        labels(at(12, 3)),
        [
            "unwrap",
            "unwrap_or",
            "is_ok",
            "is_err",
            "unwrap_err",
            "ok",
            "err",
            "eq",
            "display"
        ]
    );
}

#[test]
fn completion_details_show_types() {
    let doc = doc(SOURCE);
    let add = completion(&doc, at(7, 1), &MapSources::default())
        .into_iter()
        .find(|item| item.label == "add")
        .unwrap();
    assert_eq!(add.detail.as_deref(), Some("fn add(a: int, b: int) -> int"));
}

#[test]
fn a_function_returning_none_shows_no_return_type() {
    let doc = doc("fn greet(name: str):\n    print(name)\ngreet(\"bo\")\n");
    assert_eq!(
        hover_text(&doc, at(3, 2)).unwrap(),
        "```stone\nfn greet(name: str)\n```"
    );
}

#[test]
fn features_still_work_around_a_syntax_error() {
    let doc = doc("fn f(a: int) -> int:\n    ret a\nx = \nprint(f(1))\n");
    assert_eq!(diagnostics(&doc).len(), 1);
    assert_eq!(
        hover_text(&doc, at(4, 7)).unwrap(),
        "```stone\nfn f(a: int) -> int\n```"
    );
}

#[test]
fn features_work_inside_a_function_with_a_syntax_error() {
    let doc = doc("fn f(a: int) -> int:\n    x = \n    ret a\nprint(f(1))\n");
    assert_eq!(diagnostics(&doc).len(), 1);
    assert_eq!(
        hover_text(&doc, at(3, 9)).unwrap(),
        "```stone\n(parameter) a: int\n```"
    );
}

// programs of several files

const MAIN: &str = "\
use util
use util.twice as double

print(util.twice(1), double(2))
";

const UTIL: &str = "\
// doubles x
pub fn twice(x: int) -> int:
    ret helper(x) * 2

fn helper(x: int) -> int:
    ret x
";

/// Builds in-memory sources for a project under `/p`.
fn project(files: &[(&str, &str)]) -> MapSources {
    let mut sources = MapSources::default();
    for (path, text) in files {
        sources.insert(format!("/p/{path}"), *text);
    }
    sources
}

/// Opens the file at `/p/<path>` of a project, as part of the program it belongs to.
fn project_doc(path: &str, files: &[(&str, &str)]) -> Document {
    let path = format!("/p/{path}");
    Document::in_program(Path::new(&path), &project(files), Encoding::Utf16)
        .expect("the file should exist")
}

fn uri_of(path: &str) -> Uri {
    format!("file:///p/{path}").parse().unwrap()
}

#[test]
fn diagnostics_only_cover_their_own_file() {
    let files = [
        ("main.stn", MAIN),
        ("util.stn", "pub fn twice(x):\n    ret x +\n"),
    ];
    assert_eq!(diagnostics(&project_doc("main.stn", &files)), []);
    let in_util = diagnostics(&project_doc("util.stn", &files));
    assert_eq!(in_util.len(), 1);
    assert_eq!(in_util[0].range.start, Position::new(1, 11));
}

#[test]
fn a_module_is_checked_as_part_of_the_program_that_uses_it() {
    // `twice` is only called with an int from main.stn, so `x` is an int in util.stn
    let doc = project_doc("util.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    assert_eq!(diagnostics(&doc), []);
    assert_eq!(
        hover_text(&doc, at(3, 16)).unwrap(),
        "```stone\n(parameter) x: int\n```"
    );
}

#[test]
fn a_module_the_program_does_not_use_yet_is_checked_from_the_root() {
    let files = [
        ("main.stn", "print(1)\n"),
        ("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
        (
            "geometry/shapes.stn",
            "use geometry.vec\n\npub fn area() -> int:\n    ret vec.zero()\n",
        ),
    ];
    assert_eq!(diagnostics(&project_doc("geometry/shapes.stn", &files)), []);
}

#[test]
fn definition_jumps_into_another_file() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let location = definition(&doc, &uri_of("main.stn"), at(4, 13), None).unwrap();
    assert_eq!(location.uri, uri_of("util.stn"));
    assert_eq!(
        location.range,
        Range::new(Position::new(1, 7), Position::new(1, 12))
    );
}

#[test]
fn definition_of_a_use_path_opens_what_it_names() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let module = definition(&doc, &uri_of("main.stn"), at(1, 6), None).unwrap();
    assert_eq!(module.uri, uri_of("util.stn"));
    assert_eq!(module.range.start, Position::new(0, 0));

    let function = definition(&doc, &uri_of("main.stn"), at(2, 11), None).unwrap();
    assert_eq!(function.uri, uri_of("util.stn"));
    assert_eq!(function.range.start, Position::new(1, 7));
}

#[test]
fn hover_on_an_imported_function_shows_its_comment() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    assert_eq!(
        hover_text(&doc, at(4, 13)).unwrap(),
        "```stone\nfn util.twice(x: int) -> int\n```\ndoubles x"
    );
}

#[test]
fn references_span_every_file() {
    let doc = project_doc("util.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let locations: Vec<(Uri, Position)> = references(&doc, &uri_of("util.stn"), at(2, 10), true)
        .into_iter()
        .map(|l| (l.uri, l.range.start))
        .collect();
    assert_eq!(
        locations,
        [
            (uri_of("main.stn"), Position::new(3, 11)),
            (uri_of("main.stn"), Position::new(3, 21)),
            (uri_of("util.stn"), Position::new(1, 7)),
        ]
    );
}

#[test]
fn rename_edits_every_file_but_leaves_aliases() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let edit = rename(&doc, &uri_of("main.stn"), at(4, 13), "triple").unwrap();
    let starts = |path: &str| -> Vec<Position> {
        let mut starts: Vec<Position> = edit.changes.as_ref().unwrap()[&uri_of(path)]
            .iter()
            .map(|e| e.range.start)
            .collect();
        starts.sort_by_key(|p| (p.line, p.character));
        starts
    };
    assert_eq!(starts("util.stn"), [Position::new(1, 7)]);
    // the `twice` of `use util.twice as double` and of `util.twice(1)`, but not `double(2)`
    assert_eq!(
        starts("main.stn"),
        [Position::new(1, 9), Position::new(3, 11)]
    );
}

#[test]
fn rename_checks_names_in_the_definitions_file() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    assert_eq!(
        rename(&doc, &uri_of("main.stn"), at(4, 13), "helper").unwrap_err(),
        "'helper' is already defined here"
    );
}

#[test]
fn document_symbols_of_a_module_use_its_own_names() {
    let doc = project_doc("util.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let names: Vec<String> = document_symbols(&doc).into_iter().map(|s| s.name).collect();
    assert_eq!(names, ["twice", "helper"]);
    let names: Vec<String> = document_symbols(&project_doc(
        "main.stn",
        &[("main.stn", MAIN), ("util.stn", UTIL)],
    ))
    .into_iter()
    .map(|s| s.name)
    .collect();
    assert_eq!(names, Vec::<String>::new());
}

#[test]
fn completion_offers_imported_names() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let items = completion(&doc, at(4, 1), &MapSources::default());
    let kind_of = |label: &str| items.iter().find(|i| i.label == label).and_then(|i| i.kind);
    assert_eq!(kind_of("util"), Some(CompletionItemKind::MODULE));
    assert_eq!(kind_of("double"), Some(CompletionItemKind::FUNCTION));
    assert_eq!(kind_of("helper"), None);
}

#[test]
fn completion_after_a_module_offers_its_public_functions() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let labels: Vec<String> = completion(&doc, at(4, 12), &MapSources::default())
        .into_iter()
        .map(|i| i.label)
        .collect();
    assert_eq!(labels, ["twice"]);
}

#[test]
fn completion_in_a_use_offers_modules_and_directories() {
    let files = [
        ("main.stn", "use \nuse geometry.\n"),
        ("util.stn", UTIL),
        ("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
    ];
    let doc = project_doc("main.stn", &files);
    let sources = project(&files);
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        let mut items: Vec<(String, Option<CompletionItemKind>)> = completion(&doc, pos, &sources)
            .into_iter()
            .map(|i| (i.label, i.kind))
            .collect();
        items.sort_by(|a, b| a.0.cmp(&b.0));
        items
    };
    // the files and directories, plus every builtin module
    let mut expected = vec![
        ("geometry".to_string(), Some(CompletionItemKind::FOLDER)),
        ("util".to_string(), Some(CompletionItemKind::MODULE)),
    ];
    for module in &stone::stdlib::MODULES {
        expected.push((module.name.to_string(), Some(CompletionItemKind::MODULE)));
    }
    expected.sort_by(|a, b| a.0.cmp(&b.0));
    assert_eq!(labels(at(1, 5)), expected);
    assert_eq!(
        labels(at(2, 14)),
        [("vec".to_string(), Some(CompletionItemKind::MODULE))]
    );
}

const STATS: &str = "\
// summary statistics
// for lists of ints

pub fn mean(xs: list[int]) -> int:
    ret xs[0]
";

#[test]
fn hover_on_a_module_shows_the_comment_at_the_top_of_its_file() {
    let main = "use stats\nuse stats as s\nprint(stats.mean([1]), s.mean([2]))\n";
    let doc = project_doc("main.stn", &[("main.stn", main), ("stats.stn", STATS)]);
    let expected = "```stone\nmodule stats\n```\nsummary statistics\nfor lists of ints";
    // the `use` path, the module before a `.`, and an alias for it
    assert_eq!(hover_text(&doc, at(1, 6)).unwrap(), expected);
    assert_eq!(hover_text(&doc, at(3, 8)).unwrap(), expected);
    assert_eq!(hover_text(&doc, at(3, 24)).unwrap(), expected);
}

#[test]
fn a_comment_right_above_a_definition_is_not_the_modules() {
    let doc = project_doc("main.stn", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    assert_eq!(
        hover_text(&doc, at(4, 8)).unwrap(),
        "```stone\nmodule util\n```"
    );
}

const OS: &str = "use os\nuse os.env as getenv\nprint(os.pid(), getenv(\"HOME\"))\n";

#[test]
fn hover_documents_the_os_module() {
    let doc = doc(OS);
    // the `pid` in `os.pid()`, and `getenv`, bound to `os.env`
    let text = hover_text(&doc, at(3, 10)).unwrap();
    assert!(
        text.starts_with("```stone\nos.pid() -> int\n```\n"),
        "{text}"
    );
    let text = hover_text(&doc, at(3, 18)).unwrap();
    assert!(
        text.starts_with("```stone\nos.env(name: str) -> option[str]\n```\n"),
        "{text}"
    );
    // the module, in its `use` and before a `.`
    let expected = format!(
        "```stone\nmodule os\n```\n{}",
        stone::stdlib::os::MODULE_DOC
    );
    assert_eq!(hover_text(&doc, at(1, 5)).unwrap(), expected);
    assert_eq!(hover_text(&doc, at(3, 7)).unwrap(), expected);
}

#[test]
fn definition_of_the_os_module_opens_the_reference_file() {
    let reference: Uri = "file:///cache/builtins.stn".parse().unwrap();
    let builtins = Builtins::new(reference.clone());
    let doc = doc(OS);
    let text = stone::stdlib::builtins_reference();
    let line_of = |prefix: &str| text.lines().position(|l| l.starts_with(prefix)).unwrap() as u32;
    let range = |position: Position| {
        let location = definition(&doc, &uri(), position, Some(&builtins)).unwrap();
        assert_eq!(location.uri, reference);
        location.range
    };

    // `os.pid` in its signature line
    let line = line_of("// os.pid(");
    let pid = Range::new(Position::new(line, 3), Position::new(line, 9));
    assert_eq!(range(at(3, 10)), pid);
    // `os.env`, from the name it is bound to and from its `use`
    let line = line_of("// os.env(");
    let env = Range::new(Position::new(line, 3), Position::new(line, 9));
    assert_eq!(range(at(3, 18)), env);
    assert_eq!(range(at(2, 8)), env);
    // the module, from its `use` and before a `.`, goes to its heading
    let line = line_of("// The os module");
    let module = Range::new(Position::new(line, 7), Position::new(line, 9));
    assert_eq!(range(at(1, 5)), module);
    assert_eq!(range(at(3, 7)), module);
}

#[test]
fn completion_after_os_offers_its_functions() {
    let doc = doc("use os as system\nsystem.\n");
    let items = completion(&doc, at(2, 8), &MapSources::default());
    let labels: Vec<&str> = items.iter().map(|i| i.label.as_str()).collect();
    let expected: Vec<&str> = stone::stdlib::os::DOCS
        .iter()
        .map(|doc| doc.name.strip_prefix("os.").unwrap())
        .collect();
    assert_eq!(labels, expected);
    assert_eq!(items[0].kind, Some(CompletionItemKind::FUNCTION));
    assert_eq!(
        items[0].detail.as_deref(),
        Some("os.env(name: str) -> option[str]")
    );
}

#[test]
fn completion_offers_os_imports() {
    let doc = doc(OS);
    let items = completion(&doc, at(4, 1), &MapSources::default());
    let item = |label: &str| items.iter().find(|i| i.label == label).unwrap();
    assert_eq!(item("os").kind, Some(CompletionItemKind::MODULE));
    assert_eq!(item("os").detail.as_deref(), Some("module os"));
    assert_eq!(item("getenv").kind, Some(CompletionItemKind::FUNCTION));
    assert_eq!(
        item("getenv").detail.as_deref(),
        Some("os.env(name: str) -> option[str]")
    );
}

#[test]
fn completion_in_a_use_of_os_offers_its_functions() {
    let files = [("main.stn", "use os.\n")];
    let doc = project_doc("main.stn", &files);
    let labels: Vec<String> = completion(&doc, at(1, 8), &project(&files))
        .into_iter()
        .map(|i| i.label)
        .collect();
    assert_eq!(labels.len(), stone::stdlib::os::DOCS.len());
    assert!(labels.contains(&"env".to_string()), "{labels:?}");
}

#[test]
fn every_builtin_module_has_hover_completion_and_a_definition() {
    let reference: Uri = "file:///cache/builtins.stn".parse().unwrap();
    let builtins = Builtins::new(reference.clone());
    for module in &stone::stdlib::MODULES {
        let name = module.name;
        let function = module.functions[0]
            .name
            .strip_prefix(&format!("{name}."))
            .unwrap();
        let text = format!("use {name} as m\nm.\n");
        let doc = doc(&text);
        // the module, in its `use`
        let expected = format!("```stone\nmodule {name}\n```\n{}", module.doc);
        assert_eq!(
            hover_text(&doc, at(1, 5)).as_deref(),
            Some(expected.as_str())
        );
        let location = definition(&doc, &uri(), at(1, 5), Some(&builtins)).unwrap();
        assert_eq!(location.uri, reference, "{name}");
        // its functions after the name it is bound to
        let labels: Vec<String> = completion(&doc, at(2, 3), &MapSources::default())
            .into_iter()
            .map(|i| i.label)
            .collect();
        assert_eq!(labels.len(), module.functions.len(), "{name}: {labels:?}");
        assert!(labels.iter().any(|l| l == function), "{name}: {labels:?}");
    }
}

const ENUMS: &str = "\
enum Shape:
    Circle(float)
    Empty
s = Shape.Circle(1.0)
match s:
    case Shape.Circle(r):
        print(r)
    case Shape.Empty:
        print(0)
";

#[test]
fn hover_shows_an_enum_with_its_variants() {
    let doc = doc(ENUMS);
    assert_eq!(
        hover_text(&doc, at(1, 6)).unwrap(),
        "```stone\nenum Shape\n    Circle(float)\n    Empty\n```"
    );
    assert_eq!(
        hover_text(&doc, at(4, 12)).unwrap(),
        "```stone\nShape.Circle(float)\n```"
    );
    assert_eq!(
        hover_text(&doc, at(8, 16)).unwrap(),
        "```stone\nShape.Empty\n```"
    );
}

#[test]
fn a_variant_leads_to_its_definition_and_renames_everywhere() {
    let doc = doc(ENUMS);
    let location = definition(&doc, &uri(), at(6, 16), None).unwrap();
    assert_eq!(location.range.start, Position::new(1, 4));
    let edit = rename(&doc, &uri(), at(2, 5), "Round").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let mut starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    starts.sort_by_key(|p| (p.line, p.character));
    assert_eq!(
        starts,
        [
            Position::new(1, 4),
            Position::new(3, 10),
            Position::new(5, 15)
        ]
    );
}

#[test]
fn an_enum_is_outlined_with_its_variants() {
    let symbols = document_symbols(&doc(ENUMS));
    let shape = symbols.iter().find(|s| s.name == "Shape").unwrap();
    assert_eq!(shape.kind, SymbolKind::ENUM);
    let children: Vec<(&str, SymbolKind)> = shape
        .children
        .as_ref()
        .unwrap()
        .iter()
        .map(|c| (c.name.as_str(), c.kind))
        .collect();
    assert_eq!(
        children,
        [
            ("Circle", SymbolKind::ENUM_MEMBER),
            ("Empty", SymbolKind::ENUM_MEMBER)
        ]
    );
}

#[test]
fn completion_after_an_enum_offers_its_variants() {
    let doc = doc(&format!("{ENUMS}t = Shape.\n"));
    let labels: Vec<(String, Option<CompletionItemKind>)> =
        completion(&doc, at(10, 11), &MapSources::default())
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect();
    assert_eq!(
        labels,
        [
            ("Circle".to_string(), Some(CompletionItemKind::ENUM_MEMBER)),
            ("Empty".to_string(), Some(CompletionItemKind::ENUM_MEMBER))
        ]
    );
}

const STRUCTS: &str = "\
struct Point:
    x: float
    y: float
p = Point(x=1.0, y=2.0)
p.x += p.y
print(p.x)
";

#[test]
fn hover_shows_a_struct_with_its_fields() {
    let doc = doc(STRUCTS);
    assert_eq!(
        hover_text(&doc, at(1, 8)).unwrap(),
        "```stone\nstruct Point\n    x: float\n    y: float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(4, 5)).unwrap(),
        "```stone\nstruct Point\n    x: float\n    y: float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(5, 3)).unwrap(),
        "```stone\nPoint.x: float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(4, 18)).unwrap(),
        "```stone\nPoint.y: float\n```"
    );
}

#[test]
fn a_field_leads_to_its_definition_and_renames_everywhere() {
    let doc = doc(STRUCTS);
    let location = definition(&doc, &uri(), at(6, 9), None).unwrap();
    assert_eq!(location.range.start, Position::new(1, 4));
    let edit = rename(&doc, &uri(), at(2, 5), "left").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let mut starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    starts.sort_by_key(|p| (p.line, p.character));
    assert_eq!(
        starts,
        [
            Position::new(1, 4),
            Position::new(3, 10),
            Position::new(4, 2),
            Position::new(5, 8)
        ]
    );
}

#[test]
fn a_struct_is_outlined_with_its_fields() {
    let symbols = document_symbols(&doc(STRUCTS));
    let point = symbols.iter().find(|s| s.name == "Point").unwrap();
    assert_eq!(point.kind, SymbolKind::STRUCT);
    let children: Vec<(&str, SymbolKind)> = point
        .children
        .as_ref()
        .unwrap()
        .iter()
        .map(|c| (c.name.as_str(), c.kind))
        .collect();
    assert_eq!(
        children,
        [("x", SymbolKind::FIELD), ("y", SymbolKind::FIELD)]
    );
}

#[test]
fn completion_after_a_struct_value_offers_its_fields() {
    let doc = doc(&format!("{STRUCTS}q = p.\n"));
    let labels: Vec<(String, Option<CompletionItemKind>)> =
        completion(&doc, at(7, 7), &MapSources::default())
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect();
    assert_eq!(
        labels,
        [
            ("x".to_string(), Some(CompletionItemKind::FIELD)),
            ("y".to_string(), Some(CompletionItemKind::FIELD))
        ]
    );
}

#[test]
fn completion_offers_the_fields_of_what_os_run_returns() {
    let doc = doc("use os\nout = os.run(\"ls\", []).unwrap()\nq = out.\n");
    let labels: Vec<String> = completion(&doc, at(3, 9), &MapSources::default())
        .into_iter()
        .map(|item| item.label)
        .collect();
    assert_eq!(labels, ["status", "stdout", "stderr"]);
}

const METHODS_SOURCE: &str = "\
struct Point:
    x: float
impl Point:
    fn origin() -> Point:
        ret Point(x=0.0)
    fn shift(self, dx: float):
        self.x += dx
p = Point.origin()
p.shift(1.0)
";

#[test]
fn hover_shows_a_method_or_associated_function_with_its_struct() {
    let doc = doc(METHODS_SOURCE);
    let shift = "```stone\nfn Point.shift(self, dx: float)\n```";
    assert_eq!(hover_text(&doc, at(9, 3)).unwrap(), shift);
    assert_eq!(hover_text(&doc, at(6, 8)).unwrap(), shift);
    assert_eq!(
        hover_text(&doc, at(8, 11)).unwrap(),
        "```stone\nfn Point.origin() -> Point\n```"
    );
    assert_eq!(
        hover_text(&doc, at(7, 9)).unwrap(),
        "```stone\n(parameter) self: Point\n```"
    );
}

#[test]
fn a_method_leads_to_its_definition_and_renames_everywhere() {
    let doc = doc(METHODS_SOURCE);
    let location = definition(&doc, &uri(), at(9, 3), None).unwrap();
    assert_eq!(location.range.start, Position::new(5, 7));
    let edit = rename(&doc, &uri(), at(6, 9), "move").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let mut starts: Vec<Position> = edits.iter().map(|e| e.range.start).collect();
    starts.sort_by_key(|p| (p.line, p.character));
    assert_eq!(starts, [Position::new(5, 7), Position::new(8, 2)]);
}

#[test]
fn a_struct_is_outlined_with_its_fields_and_methods() {
    let symbols = document_symbols(&doc(METHODS_SOURCE));
    let point = symbols.iter().find(|s| s.name == "Point").unwrap();
    let children: Vec<(&str, SymbolKind)> = point
        .children
        .as_ref()
        .unwrap()
        .iter()
        .map(|c| (c.name.as_str(), c.kind))
        .collect();
    assert_eq!(
        children,
        [
            ("x", SymbolKind::FIELD),
            ("origin", SymbolKind::FUNCTION),
            ("shift", SymbolKind::METHOD)
        ]
    );
    let shift = &point.children.as_ref().unwrap()[2];
    let locals: Vec<&str> = shift
        .children
        .as_ref()
        .unwrap()
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(locals, ["self", "dx"]);
    assert!(!symbols.iter().any(|s| s.name == "shift"));
}

#[test]
fn completion_after_a_struct_value_offers_its_methods_and_after_the_struct_its_functions() {
    let labels =
        |source: String, position: Position| -> Vec<(String, Option<CompletionItemKind>)> {
            completion(&doc(&source), position, &MapSources::default())
                .into_iter()
                .map(|item| (item.label, item.kind))
                .collect()
        };
    assert_eq!(
        labels(format!("{METHODS_SOURCE}q = p.\n"), at(10, 7)),
        [
            ("x".to_string(), Some(CompletionItemKind::FIELD)),
            ("shift".to_string(), Some(CompletionItemKind::METHOD))
        ]
    );
    assert_eq!(
        labels(format!("{METHODS_SOURCE}q = Point.\n"), at(10, 11)),
        [("origin".to_string(), Some(CompletionItemKind::FUNCTION))]
    );
}

const GENERIC: &str = "\
fn first[T](xs: list[T]) -> T:
    ret xs[0]
x = first([\"a\"])
";

#[test]
fn hover_shows_a_generic_functions_type_parameters() {
    let doc = doc(GENERIC);
    assert_eq!(
        hover_text(&doc, at(3, 6)).unwrap(),
        "```stone\nfn first[T](xs: list[T]) -> T\n```"
    );
    assert_eq!(
        hover_text(&doc, at(1, 10)).unwrap(),
        "```stone\n(type parameter) T\n```"
    );
    assert_eq!(
        hover_text(&doc, at(1, 15)).unwrap(),
        "```stone\n(parameter) xs: list[T]\n```"
    );
    assert_eq!(
        hover_text(&doc, at(3, 1)).unwrap(),
        "```stone\n(global) x: str\n```"
    );
}

#[test]
fn a_type_parameter_renames_everywhere_in_its_function() {
    let doc = doc(GENERIC);
    let edit = rename(&doc, &uri(), at(1, 29), "Item").unwrap();
    let edits = &edit.changes.unwrap()[&uri()];
    let mut ranges: Vec<(u32, u32)> = edits
        .iter()
        .map(|e| (e.range.start.line, e.range.start.character))
        .collect();
    ranges.sort();
    assert_eq!(ranges, [(0, 9), (0, 21), (0, 28)]);
}

#[test]
fn an_outline_lists_a_functions_type_parameters() {
    let doc = doc(GENERIC);
    let outline = document_symbols(&doc);
    let children = outline[0].children.as_ref().unwrap();
    assert_eq!(children[0].name, "T");
    assert_eq!(children[0].kind, SymbolKind::TYPE_PARAMETER);
}

const GENERIC_TYPES: &str = "\
struct Pair[A, B]:
    first: A
    second: B
enum Tree[T]:
    Leaf
    Node(Tree[T], T)
p = Pair(first=1, second=\"s\")
";

#[test]
fn hover_shows_a_generic_types_parameters() {
    let doc = doc(GENERIC_TYPES);
    assert_eq!(
        hover_text(&doc, at(1, 8)).unwrap(),
        "```stone\nstruct Pair[A, B]\n    first: A\n    second: B\n```"
    );
    assert_eq!(
        hover_text(&doc, at(4, 6)).unwrap(),
        "```stone\nenum Tree[T]\n    Leaf\n    Node(Tree[T], T)\n```"
    );
    assert_eq!(
        hover_text(&doc, at(7, 1)).unwrap(),
        "```stone\n(global) p: Pair[int, str]\n```"
    );
}

#[test]
fn completion_offers_a_generic_structs_fields() {
    let source = format!("{GENERIC_TYPES}x = p.\n");
    let doc = doc(&source);
    let items = completion(&doc, at(8, 7), &MapSources::default());
    let labels: Vec<(&str, Option<&str>)> = items
        .iter()
        .map(|i| (i.label.as_str(), i.detail.as_deref()))
        .collect();
    assert_eq!(
        labels,
        [
            ("first", Some("Pair.first: A")),
            ("second", Some("Pair.second: B"))
        ]
    );
}

const OPTIONS: &str =
    "fn f(o: option[int]) -> int:\n    ret o.unwrap_or(0)\nx = some(1)\ny = none\n";

/// Returns the range of `name` on the first line of the prelude that holds it.
fn in_prelude(name: &str) -> Range {
    let (line, text) = stone::stdlib::PRELUDE
        .lines()
        .enumerate()
        .find(|(_, l)| l.contains(name) && !l.starts_with("//"))
        .unwrap();
    let col = text.find(name).unwrap() as u32;
    let line = line as u32;
    Range::new(
        Position::new(line, col),
        Position::new(line, col + name.len() as u32),
    )
}

#[test]
fn the_preludes_names_lead_to_the_prelude_file() {
    let reference: Uri = "file:///cache/builtins.stn".parse().unwrap();
    let builtins = Builtins::new(reference);
    let prelude: Uri = "file:///cache/prelude.stn".parse().unwrap();
    let doc = doc(OPTIONS);
    let target = |position: Position| definition(&doc, &uri(), position, Some(&builtins));

    let some = target(at(3, 6)).unwrap();
    assert_eq!(
        (some.uri.clone(), some.range),
        (prelude.clone(), in_prelude("some"))
    );
    let none = target(at(4, 6)).unwrap();
    assert_eq!(
        (none.uri.clone(), none.range),
        (prelude.clone(), in_prelude("none"))
    );
    let option = target(at(1, 10)).unwrap();
    assert_eq!((option.uri, option.range), (prelude, in_prelude("option")));
    // without the files written out, the prelude has nowhere to go
    assert_eq!(definition(&doc, &uri(), at(3, 6), None), None);
    // and its uses are only found in the program's own files
    let uses = references(&doc, &uri(), at(3, 6), true);
    assert_eq!(uses.len(), 1);
    assert!(uses.iter().all(|l| l.uri == uri()));
}

#[test]
fn the_preludes_names_cannot_be_renamed() {
    let doc = doc(OPTIONS);
    assert_eq!(prepare_rename(&doc, at(3, 6)), None);
    assert_eq!(prepare_rename(&doc, at(1, 10)), None);
    assert_eq!(
        rename(&doc, &uri(), at(3, 6), "maybe").unwrap_err(),
        "'some' is a builtin"
    );
    assert_eq!(
        rename(&doc, &uri(), at(3, 1), "some").unwrap_err(),
        "'some' is a builtin"
    );
}

#[test]
fn hover_shows_the_preludes_variants_as_they_are_written() {
    let doc = doc(OPTIONS);
    assert_eq!(
        hover_text(&doc, at(3, 6)).unwrap(),
        "```stone\nsome(T)\n```"
    );
    assert!(
        hover_text(&doc, at(1, 10))
            .unwrap()
            .starts_with("```stone\nenum option[T]\n    none\n    some(T)\n```"),
        "{:?}",
        hover_text(&doc, at(1, 10))
    );
}

#[test]
fn completion_offers_the_preludes_variants() {
    let doc = doc("x = \n");
    let labels: Vec<String> = completion(&doc, at(1, 5), &MapSources::default())
        .into_iter()
        .map(|i| i.label)
        .collect();
    for (name, _) in stone::stdlib::PRELUDE_VARIANTS {
        assert_eq!(
            labels.iter().filter(|l| *l == name).count(),
            1,
            "{name}: {labels:?}"
        );
    }
}

const TRAITS: &str = "\
trait Shape:
    fn area(self) -> float
    fn half(self) -> float:
        ret self.area() / 2.0
struct Sq:
    s: float
impl Shape for Sq:
    fn area(self) -> float:
        ret self.s * self.s
fn total[T: Shape](xs: list[T]) -> float:
    t = 0.0
    for x in xs:
        t += x.area()
    ret t
q = Sq(s=1.0)
";

#[test]
fn hover_shows_a_trait_its_methods_and_bounds() {
    let doc = doc(TRAITS);
    assert_eq!(diagnostics(&doc), []);
    assert_eq!(
        hover_text(&doc, at(1, 7)).unwrap(),
        "```stone\ntrait Shape\n    fn area(self) -> float\n    fn half(self) -> float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(13, 16)).unwrap(),
        "```stone\nfn Shape.area(self) -> float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(10, 4)).unwrap(),
        "```stone\nfn total[T: Shape](xs: list[T]) -> float\n```"
    );
    assert_eq!(
        hover_text(&doc, at(10, 10)).unwrap(),
        "```stone\n(type parameter) T: Shape\n```"
    );
    // the trait named in a bound and an impl block
    assert!(
        hover_text(&doc, at(10, 13))
            .unwrap()
            .starts_with("```stone\ntrait Shape")
    );
    assert!(
        hover_text(&doc, at(7, 7))
            .unwrap()
            .starts_with("```stone\ntrait Shape")
    );
}

#[test]
fn a_call_through_a_bound_leads_to_the_traits_method() {
    let doc = doc(TRAITS);
    let location = definition(&doc, &uri(), at(13, 16), None).unwrap();
    assert_eq!(location.range.start, Position::new(1, 7));
    let location = definition(&doc, &uri(), at(10, 13), None).unwrap();
    assert_eq!(location.range.start, Position::new(0, 6));
}

#[test]
fn a_trait_is_outlined_with_its_methods() {
    let symbols = document_symbols(&doc(TRAITS));
    let shape = symbols.iter().find(|s| s.name == "Shape").unwrap();
    assert_eq!(shape.kind, SymbolKind::INTERFACE);
    let children: Vec<(&str, SymbolKind)> = shape
        .children
        .as_ref()
        .unwrap()
        .iter()
        .map(|c| (c.name.as_str(), c.kind))
        .collect();
    assert_eq!(
        children,
        [("area", SymbolKind::METHOD), ("half", SymbolKind::METHOD)]
    );
}

#[test]
fn completion_offers_the_methods_of_a_bound_and_a_traits_defaults() {
    let labels = |source: String, position: Position| -> Vec<String> {
        completion(&doc(&source), position, &MapSources::default())
            .into_iter()
            .map(|item| item.label)
            .collect()
    };
    assert_eq!(
        labels(format!("{TRAITS}r = q.\n"), at(16, 7)),
        ["s", "area", "half"]
    );
    assert_eq!(
        labels(
            format!("{TRAITS}fn f[T: Shape](x: T) -> float:\n    y = x.\n    ret 1.0\n"),
            at(17, 11)
        ),
        ["area", "half"]
    );
}

#[test]
fn completion_after_a_socket_or_listener_offers_their_methods() {
    let doc = doc("use net\nfn f(c: socket, l: listener):\n    c.\n    l.\n");
    let labels = |pos: Position| -> Vec<String> {
        completion(&doc, pos, &MapSources::default())
            .into_iter()
            .map(|item| item.label)
            .collect()
    };
    assert_eq!(
        labels(at(3, 7)),
        ["display", "send", "read_line", "read", "read_all", "close"]
    );
    assert_eq!(labels(at(4, 7)), ["display", "accept", "port", "close"]);
}

#[test]
fn the_net_modules_names_lead_to_its_file_and_cannot_be_renamed() {
    let reference: Uri = "file:///cache/builtins.stn".parse().unwrap();
    let builtins = Builtins::new(reference);
    let net: Uri = "file:///cache/net.stn".parse().unwrap();
    let doc = doc("use net\nc = net.connect(\"a\", 1)\n");
    let connect = definition(&doc, &uri(), at(2, 10), Some(&builtins)).unwrap();
    let line = stone::stdlib::net::SOURCE
        .lines()
        .position(|line| line.starts_with("pub fn connect("))
        .unwrap() as u32;
    assert_eq!(
        (connect.uri, connect.range),
        (
            net.clone(),
            Range::new(Position::new(line, 7), Position::new(line, 14))
        )
    );
    // the module itself leads to the top of the file
    let module = definition(&doc, &uri(), at(1, 6), Some(&builtins)).unwrap();
    assert_eq!((module.uri, module.range.start), (net, Position::new(0, 0)));
    assert_eq!(prepare_rename(&doc, at(2, 10)), None);
    assert_eq!(
        rename(&doc, &uri(), at(2, 10), "join").unwrap_err(),
        "'connect' is a builtin"
    );
}
