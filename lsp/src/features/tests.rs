use super::*;
use lsp_types::{CompletionItemKind, DiagnosticSeverity, HoverContents, Range, SymbolKind};
use std::path::Path;
use stone::project::MapSources;

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
    let doc = doc("xs = []\nxs.\nxs.le\n");
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        completion(&doc, pos, &MapSources::default())
            .into_iter()
            .map(|item| (item.label, item.kind))
            .collect()
    };
    let methods: Vec<_> = ["len", "append", "strip", "split"]
        .iter()
        .map(|name| (name.to_string(), Some(CompletionItemKind::METHOD)))
        .collect();
    assert_eq!(labels(at(2, 4)), methods);
    assert_eq!(labels(at(3, 6)), methods);
    // and methods are not offered as functions
    let outside = labels(at(1, 1));
    assert!(!outside.iter().any(|(label, _)| label == "len"));
}

#[test]
fn completion_details_show_types() {
    let doc = doc(SOURCE);
    let add = completion(&doc, at(7, 1), &MapSources::default())
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

// programs of several files

const MAIN: &str = "\
use util
use util.twice as double

print(util.twice(1), double(2))
";

const UTIL: &str = "\
// doubles x
pub def twice(x);
    ret helper(x) * 2

def helper(x);
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
        ("main.st", MAIN),
        ("util.st", "pub def twice(x);\n    ret x +\n"),
    ];
    assert_eq!(diagnostics(&project_doc("main.st", &files)), []);
    let in_util = diagnostics(&project_doc("util.st", &files));
    assert_eq!(in_util.len(), 1);
    assert_eq!(in_util[0].range.start, Position::new(1, 11));
}

#[test]
fn a_module_is_checked_as_part_of_the_program_that_uses_it() {
    // `twice` is only called with an int from main.st, so `x` is an int in util.st
    let doc = project_doc("util.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    assert_eq!(diagnostics(&doc), []);
    assert_eq!(
        hover_text(&doc, at(3, 16)).unwrap(),
        "```stone\n(parameter) x: int\n```"
    );
}

#[test]
fn a_module_the_program_does_not_use_yet_is_checked_from_the_root() {
    let files = [
        ("main.st", "print(1)\n"),
        ("geometry/vec.st", "pub def zero();\n    ret 0\n"),
        (
            "geometry/shapes.st",
            "use geometry.vec\n\npub def area();\n    ret vec.zero()\n",
        ),
    ];
    assert_eq!(diagnostics(&project_doc("geometry/shapes.st", &files)), []);
}

#[test]
fn definition_jumps_into_another_file() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let location = definition(&doc, &uri_of("main.st"), at(4, 13), None).unwrap();
    assert_eq!(location.uri, uri_of("util.st"));
    assert_eq!(
        location.range,
        Range::new(Position::new(1, 8), Position::new(1, 13))
    );
}

#[test]
fn definition_of_a_use_path_opens_what_it_names() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let module = definition(&doc, &uri_of("main.st"), at(1, 6), None).unwrap();
    assert_eq!(module.uri, uri_of("util.st"));
    assert_eq!(module.range.start, Position::new(0, 0));

    let function = definition(&doc, &uri_of("main.st"), at(2, 11), None).unwrap();
    assert_eq!(function.uri, uri_of("util.st"));
    assert_eq!(function.range.start, Position::new(1, 8));
}

#[test]
fn hover_on_an_imported_function_shows_its_comment() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    assert_eq!(
        hover_text(&doc, at(4, 13)).unwrap(),
        "```stone\ndef util.twice(x: int) -> int\n```\ndoubles x"
    );
}

#[test]
fn references_span_every_file() {
    let doc = project_doc("util.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let locations: Vec<(Uri, Position)> = references(&doc, &uri_of("util.st"), at(2, 10), true)
        .into_iter()
        .map(|l| (l.uri, l.range.start))
        .collect();
    assert_eq!(
        locations,
        [
            (uri_of("main.st"), Position::new(3, 11)),
            (uri_of("main.st"), Position::new(3, 21)),
            (uri_of("util.st"), Position::new(1, 8)),
        ]
    );
}

#[test]
fn rename_edits_every_file_but_leaves_aliases() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let edit = rename(&doc, &uri_of("main.st"), at(4, 13), "triple").unwrap();
    let starts = |path: &str| -> Vec<Position> {
        let mut starts: Vec<Position> = edit.changes.as_ref().unwrap()[&uri_of(path)]
            .iter()
            .map(|e| e.range.start)
            .collect();
        starts.sort_by_key(|p| (p.line, p.character));
        starts
    };
    assert_eq!(starts("util.st"), [Position::new(1, 8)]);
    // the `twice` of `use util.twice as double` and of `util.twice(1)`, but not `double(2)`
    assert_eq!(
        starts("main.st"),
        [Position::new(1, 9), Position::new(3, 11)]
    );
}

#[test]
fn rename_checks_names_in_the_definitions_file() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    assert_eq!(
        rename(&doc, &uri_of("main.st"), at(4, 13), "helper").unwrap_err(),
        "'helper' is already defined here"
    );
}

#[test]
fn document_symbols_of_a_module_use_its_own_names() {
    let doc = project_doc("util.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let names: Vec<String> = document_symbols(&doc).into_iter().map(|s| s.name).collect();
    assert_eq!(names, ["twice", "helper"]);
    let names: Vec<String> = document_symbols(&project_doc(
        "main.st",
        &[("main.st", MAIN), ("util.st", UTIL)],
    ))
    .into_iter()
    .map(|s| s.name)
    .collect();
    assert_eq!(names, Vec::<String>::new());
}

#[test]
fn completion_offers_imported_names() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let items = completion(&doc, at(4, 1), &MapSources::default());
    let kind_of = |label: &str| items.iter().find(|i| i.label == label).and_then(|i| i.kind);
    assert_eq!(kind_of("util"), Some(CompletionItemKind::MODULE));
    assert_eq!(kind_of("double"), Some(CompletionItemKind::FUNCTION));
    assert_eq!(kind_of("helper"), None);
}

#[test]
fn completion_after_a_module_offers_its_public_functions() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
    let labels: Vec<String> = completion(&doc, at(4, 12), &MapSources::default())
        .into_iter()
        .map(|i| i.label)
        .collect();
    assert_eq!(labels, ["twice"]);
}

#[test]
fn completion_in_a_use_offers_modules_and_directories() {
    let files = [
        ("main.st", "use \nuse geometry.\n"),
        ("util.st", UTIL),
        ("geometry/vec.st", "pub def zero();\n    ret 0\n"),
    ];
    let doc = project_doc("main.st", &files);
    let sources = project(&files);
    let labels = |pos: Position| -> Vec<(String, Option<CompletionItemKind>)> {
        let mut items: Vec<(String, Option<CompletionItemKind>)> = completion(&doc, pos, &sources)
            .into_iter()
            .map(|i| (i.label, i.kind))
            .collect();
        items.sort_by(|a, b| a.0.cmp(&b.0));
        items
    };
    assert_eq!(
        labels(at(1, 5)),
        [
            ("geometry".to_string(), Some(CompletionItemKind::FOLDER)),
            ("os".to_string(), Some(CompletionItemKind::MODULE)),
            ("util".to_string(), Some(CompletionItemKind::MODULE)),
        ]
    );
    assert_eq!(
        labels(at(2, 14)),
        [("vec".to_string(), Some(CompletionItemKind::MODULE))]
    );
}

const STATS: &str = "\
// summary statistics
// for lists of ints

pub def mean(xs);
    ret xs[0]
";

#[test]
fn hover_on_a_module_shows_the_comment_at_the_top_of_its_file() {
    let main = "use stats\nuse stats as s\nprint(stats.mean([1]), s.mean([2]))\n";
    let doc = project_doc("main.st", &[("main.st", main), ("stats.st", STATS)]);
    let expected = "```stone\nmodule stats\n```\nsummary statistics\nfor lists of ints";
    // the `use` path, the module before a `.`, and an alias for it
    assert_eq!(hover_text(&doc, at(1, 6)).unwrap(), expected);
    assert_eq!(hover_text(&doc, at(3, 8)).unwrap(), expected);
    assert_eq!(hover_text(&doc, at(3, 24)).unwrap(), expected);
}

#[test]
fn a_comment_right_above_a_definition_is_not_the_modules() {
    let doc = project_doc("main.st", &[("main.st", MAIN), ("util.st", UTIL)]);
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
        text.starts_with("```stone\nos.env(name: str) -> str\n```\n"),
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
    let reference: Uri = "file:///cache/builtins.st".parse().unwrap();
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
    let expected: Vec<&str> = stone::stdlib::os::FUNCTIONS
        .iter()
        .map(|name| name.strip_prefix("os.").unwrap())
        .collect();
    assert_eq!(labels, expected);
    assert_eq!(items[0].kind, Some(CompletionItemKind::FUNCTION));
    assert_eq!(items[0].detail.as_deref(), Some("os.env(name: str) -> str"));
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
        Some("os.env(name: str) -> str")
    );
}

#[test]
fn completion_in_a_use_of_os_offers_its_functions() {
    let files = [("main.st", "use os.\n")];
    let doc = project_doc("main.st", &files);
    let labels: Vec<String> = completion(&doc, at(1, 8), &project(&files))
        .into_iter()
        .map(|i| i.label)
        .collect();
    assert_eq!(labels.len(), stone::stdlib::os::FUNCTIONS.len());
    assert!(labels.contains(&"env".to_string()), "{labels:?}");
}
