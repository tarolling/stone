use super::*;
use lsp_types::{CompletionItemKind, DiagnosticSeverity, HoverContents, Range, SymbolKind};

const SOURCE: &str = "\
total = 0
def add(a, b);
    sum = a + b
    ret sum
for i in range(3);
    total = add(total, i)
print(total, \"é\".len())
";

fn doc(text: &str) -> Document {
    Document::new(text.to_string(), Encoding::Utf16)
}

fn uri() -> Uri {
    "file:///demo.st".parse().unwrap()
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
        "```stone\ndef add(a: int, b: int) -> int\n```"
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
def halvings(n);
    // the count so far
    count = 0
    while n > 1;
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
        "```stone\ndef halvings(n: int) -> int\n```\n\
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
    let doc = doc("// about the loop\nfor i in range(3);\n    print(i)\n");
    assert_eq!(
        hover_text(&doc, at(2, 5)).unwrap(),
        "```stone\n(global) i: int\n```"
    );
}

#[test]
fn parameters_have_no_documentation() {
    let doc = doc("// about f\ndef f(n);\n    ret n\nprint(f(1))\n");
    assert_eq!(
        hover_text(&doc, at(2, 7)).unwrap(),
        "```stone\n(parameter) n: int\n```"
    );
}

#[test]
fn hover_documents_builtins() {
    let text = hover_text(&doc(SOURCE), at(7, 2)).unwrap();
    assert!(
        text.starts_with("```stone\nprint(values...) -> none\n```\n"),
        "{text}"
    );
}

#[test]
fn hover_documents_methods() {
    // the `len` in `"é".len()`
    let text = hover_text(&doc(SOURCE), at(7, 18)).unwrap();
    assert!(
        text.starts_with("```stone\n(str | list[T]).len() -> int\n```\n"),
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

#[test]
fn definition_jumps_to_the_name() {
    let doc = doc(SOURCE);
    let location = definition(&doc, &uri(), at(6, 14), None).unwrap();
    assert_eq!(
        location.range,
        Range::new(Position::new(1, 4), Position::new(1, 7))
    );
    assert_eq!(location.uri, uri());
    // without the reference file, builtins have nowhere to go
    assert_eq!(definition(&doc, &uri(), at(7, 2), None), None);
}

#[test]
fn definition_of_a_builtin_opens_the_reference_file() {
    let reference: Uri = "file:///cache/builtins.st".parse().unwrap();
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

    // the `len` in the reference's `// (str | list[T]).len() -> int` line
    let len = definition(&doc, &uri(), at(7, 18), Some(&builtins)).unwrap();
    let line = text
        .lines()
        .position(|l| l.starts_with("// (str | list[T]).len("))
        .unwrap() as u32;
    assert_eq!(
        len.range,
        Range::new(Position::new(line, 19), Position::new(line, 22))
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
        completion(&doc, pos)
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
    let doc = doc("xs = []\nxs.\nxs.le\n");
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        completion(&doc, pos)
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect()
    };
    let methods = [
        ("len".to_string(), Some(CompletionItemKind::METHOD)),
        ("append".to_string(), Some(CompletionItemKind::METHOD)),
    ];
    assert_eq!(labels(at(2, 4)), methods);
    assert_eq!(labels(at(3, 6)), methods);
    // and methods are not offered as functions
    let outside = labels(at(1, 1));
    assert!(!outside.iter().any(|(label, _)| label == "len"));
}

#[test]
fn completion_details_show_types() {
    let doc = doc(SOURCE);
    let add = completion(&doc, at(7, 1))
        .into_iter()
        .find(|item| item.label == "add")
        .unwrap();
    assert_eq!(
        add.detail.as_deref(),
        Some("def add(a: int, b: int) -> int")
    );
}

#[test]
fn features_still_work_around_a_syntax_error() {
    let doc = doc("def f(a);\n    ret a\nx = \nprint(f(1))\n");
    assert_eq!(diagnostics(&doc).len(), 1);
    assert_eq!(
        hover_text(&doc, at(4, 7)).unwrap(),
        "```stone\ndef f(a: int) -> int\n```"
    );
}

#[test]
fn features_work_inside_a_function_with_a_syntax_error() {
    let doc = doc("def f(a);\n    x = \n    ret a\nprint(f(1))\n");
    assert_eq!(diagnostics(&doc).len(), 1);
    assert_eq!(
        hover_text(&doc, at(3, 9)).unwrap(),
        "```stone\n(parameter) a: int\n```"
    );
}
