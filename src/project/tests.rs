use super::*;
use crate::driver;
use crate::interpreter::Limits;
use crate::span::Pos;

/// Builds in-memory sources from `(path, source)` pairs.
fn sources_of(files: &[(&str, &str)]) -> MapSources {
    let mut sources = MapSources::default();
    for (path, source) in files {
        sources.insert(path, *source);
    }
    sources
}

/// Loads, links, and checks a program whose entry file is the first of `files`, and returns what
/// it prints, panicking with the rendered errors if it has any.
///
/// For example, `output(&[("main.st", "use util\nprint(util.one())\n"), ("util.st", "pub def
/// one(); ret 1\n")])` returns `"1\n"`.
fn output(files: &[(&str, &str)]) -> String {
    let (entry, source) = files[0];
    let (map, result) = driver::load(Path::new(entry), source, &sources_of(&files[1..]));
    let ast = result.unwrap_or_else(|errors| {
        let rendered: String = errors.0.iter().map(|d| map.render(d)).collect();
        panic!("program should have no errors:\n{rendered}")
    });
    let mut out = vec![];
    driver::run_module(&ast, &mut std::io::empty(), &mut out, &[], Limits::DEFAULT)
        .expect("program should run");
    String::from_utf8(out).unwrap()
}

/// Loads, links, and checks a program like [`output`], which must fail, and returns each error as
/// its file's path, its message, and where it starts.
fn errors(files: &[(&str, &str)]) -> Vec<(String, String, usize, usize)> {
    let (entry, source) = files[0];
    let (map, result) = driver::load(Path::new(entry), source, &sources_of(&files[1..]));
    let Err(errors) = result else {
        panic!("program should have errors");
    };
    errors
        .0
        .iter()
        .map(|d| {
            let path = map.file(d.span.file).path.display().to_string();
            (path, d.message.clone(), d.span.start.line, d.span.start.col)
        })
        .collect()
}

/// Asserts that a program has exactly one error, with this message at this place.
fn assert_error(files: &[(&str, &str)], path: &str, message: &str, line: usize, col: usize) {
    assert_eq!(
        errors(files),
        [(path.to_string(), message.to_string(), line, col)]
    );
}

const UTIL: (&str, &str) = (
    "util.st",
    "pub def twice(x);\n    ret helper(x) * 2\n\ndef helper(x);\n    ret x\n",
);

#[test]
fn a_module_import_calls_its_public_functions() {
    assert_eq!(
        output(&[("main.st", "use util\nprint(util.twice(2))\n"), UTIL]),
        "4\n"
    );
}

#[test]
fn a_function_import_is_called_by_its_name() {
    assert_eq!(
        output(&[("main.st", "use util.twice\nprint(twice(2))\n"), UTIL]),
        "4\n"
    );
}

#[test]
fn as_renames_an_import() {
    assert_eq!(
        output(&[
            (
                "main.st",
                "use util.twice as double\nuse util as u\nprint(double(2), u.twice(3))\n"
            ),
            UTIL
        ]),
        "4 6\n"
    );
}

#[test]
fn modules_nest_in_directories() {
    assert_eq!(
        output(&[
            ("main.st", "use geometry.shapes\nprint(shapes.area(2))\n"),
            (
                "geometry/shapes.st",
                "use geometry.vec.dot\n\npub def area(r);\n    ret 3 * dot([r], [r])\n"
            ),
            (
                "geometry/vec.st",
                "pub def dot(a, b);\n    ret a[0] * b[0]\n"
            ),
        ]),
        "12\n"
    );
}

#[test]
fn a_module_may_sit_next_to_a_directory_of_the_same_name() {
    assert_eq!(
        output(&[
            (
                "main.st",
                "use geometry\nuse geometry.vec\nprint(geometry.unit(), vec.zero())\n"
            ),
            ("geometry.st", "pub def unit();\n    ret 1\n"),
            ("geometry/vec.st", "pub def zero();\n    ret 0\n"),
        ]),
        "1 0\n"
    );
}

#[test]
fn a_module_file_wins_over_a_function_of_the_same_path() {
    assert_eq!(
        output(&[
            ("main.st", "use geometry.vec\nprint(vec.zero())\n"),
            ("geometry.st", "pub def vec();\n    ret 1\n"),
            ("geometry/vec.st", "pub def zero();\n    ret 0\n"),
        ]),
        "0\n"
    );
}

#[test]
fn private_functions_of_different_modules_do_not_collide() {
    let a = (
        "a.st",
        "def helper();\n    ret 1\npub def get();\n    ret helper()\n",
    );
    let b = (
        "b.st",
        "def helper();\n    ret 2\npub def get();\n    ret helper()\n",
    );
    assert_eq!(
        output(&[("main.st", "use a\nuse b\nprint(a.get(), b.get())\n"), a, b]),
        "1 2\n"
    );
}

#[test]
fn modules_may_import_each_other() {
    let even = (
        "even.st",
        "use odd\n\npub def even(n);\n    if n == 0;\n        ret true\n    ret odd.odd(n - 1)\n",
    );
    let odd = (
        "odd.st",
        "use even\n\npub def odd(n);\n    if n == 0;\n        ret false\n    ret even.even(n - 1)\n",
    );
    assert_eq!(
        output(&[("main.st", "use even\nprint(even.even(10))\n"), even, odd]),
        "true\n"
    );
}

#[test]
fn a_local_may_shadow_an_import() {
    assert_eq!(
        output(&[
            (
                "main.st",
                "use util.twice\n\ndef f(twice);\n    ret twice + 1\n\nprint(f(1), twice(1))\n"
            ),
            UTIL
        ]),
        "2 2\n"
    );
}

#[test]
fn pub_is_ignored_in_the_entry_file() {
    assert_eq!(
        output(&[("main.st", "pub def f();\n    ret 1\nprint(f())\n")]),
        "1\n"
    );
}

#[test]
fn library_types_are_inferred_from_their_callers() {
    assert_eq!(
        output(&[
            ("main.st", "use util.first\nprint(first([\"a\", \"b\"]))\n"),
            ("util.st", "pub def first(xs);\n    ret xs[0]\n"),
        ]),
        "a\n"
    );
}

#[test]
fn a_missing_module_is_an_error() {
    assert_error(
        &[("main.st", "use nope\n")],
        "main.st",
        "no module named 'nope'",
        1,
        5,
    );
    assert_error(
        &[("main.st", "use a.b.c\n")],
        "main.st",
        "no module named 'a.b.c'",
        1,
        5,
    );
}

#[test]
fn a_directory_cannot_be_imported() {
    assert_error(
        &[
            ("main.st", "use geometry\n"),
            ("geometry/vec.st", "pub def zero();\n    ret 0\n"),
        ],
        "main.st",
        "'geometry' is a directory, so use a module inside it",
        1,
        5,
    );
}

#[test]
fn a_missing_function_is_an_error() {
    assert_error(
        &[("main.st", "use util.nope\n"), UTIL],
        "main.st",
        "module 'util' has no function 'nope'",
        1,
        10,
    );
    assert_error(
        &[("main.st", "use util\nutil.nope()\n"), UTIL],
        "main.st",
        "module 'util' has no function 'nope'",
        2,
        6,
    );
}

#[test]
fn a_private_function_cannot_be_used_from_another_file() {
    assert_error(
        &[("main.st", "use util.helper\n"), UTIL],
        "main.st",
        "'helper' is private to module 'util'",
        1,
        10,
    );
    assert_error(
        &[("main.st", "use util\nprint(util.helper(1))\n"), UTIL],
        "main.st",
        "'helper' is private to module 'util'",
        2,
        12,
    );
}

#[test]
fn a_module_is_not_a_value() {
    assert_error(
        &[("main.st", "use util\nx = util\n"), UTIL],
        "main.st",
        "'util' is a module, not a value",
        2,
        5,
    );
}

#[test]
fn an_imported_name_must_be_unique_in_its_file() {
    assert_error(
        &[
            ("main.st", "use util.twice\nuse other.twice\n"),
            UTIL,
            ("other.st", "pub def twice();\n    ret 2\n"),
        ],
        "main.st",
        "'twice' is already imported",
        2,
        11,
    );
    assert_error(
        &[
            ("main.st", "use util.twice\n\ndef twice();\n    ret 1\n"),
            UTIL,
        ],
        "main.st",
        "'twice' is already defined in this file",
        1,
        10,
    );
    assert_error(
        &[("main.st", "use util\nutil = 1\n"), UTIL],
        "main.st",
        "'util' is already defined in this file",
        1,
        5,
    );
}

#[test]
fn an_import_cannot_shadow_a_builtin() {
    assert_error(
        &[
            ("main.st", "use str\n"),
            ("str.st", "pub def f();\n    ret 1\n"),
        ],
        "main.st",
        "'str' is a builtin, so rename it with 'as'",
        1,
        5,
    );
}

#[test]
fn use_comes_before_other_statements() {
    assert_error(
        &[("main.st", "x = 1\nuse util\n"), UTIL],
        "main.st",
        "use must come before other statements",
        2,
        1,
    );
}

#[test]
fn the_entry_file_cannot_be_imported() {
    let message = "'main' is the entry file, so it cannot be imported";
    assert_error(&[("main.st", "use main\n")], "main.st", message, 1, 5);
    assert_error(
        &[
            ("main.st", "use util\n\ndef f();\n    ret 1\n"),
            ("util.st", "use main.f\n"),
        ],
        "util.st",
        message,
        1,
        5,
    );
}

#[test]
fn a_module_only_holds_use_and_def() {
    assert_error(
        &[
            ("main.st", "use util\n"),
            ("util.st", "print(1)\n\npub def f();\n    ret 1\n"),
        ],
        "util.st",
        "only use and def are allowed at the top level of a module",
        1,
        1,
    );
}

#[test]
fn a_module_cannot_see_the_entry_files_names() {
    assert_error(
        &[
            ("main.st", "use util\ncount = 1\nprint(util.get())\n"),
            ("util.st", "pub def get();\n    ret count\n"),
        ],
        "util.st",
        "undefined name 'count'",
        2,
        9,
    );
    assert_error(
        &[
            (
                "main.st",
                "use util\n\ndef helper();\n    ret 1\n\nprint(util.get())\n",
            ),
            ("util.st", "pub def get();\n    ret helper()\n"),
        ],
        "util.st",
        "undefined function 'helper'",
        2,
        9,
    );
}

#[test]
fn errors_point_into_the_file_they_are_in() {
    let errors = errors(&[("main.st", "use util\n"), ("util.st", "pub def f(;\n")]);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0, "util.st");
}

#[test]
fn type_errors_across_files_point_at_the_caller() {
    // library functions are checked first, so their types are settled before the call
    assert_error(
        &[("main.st", "use util.twice\nprint(twice(\"a\"))\n"), UTIL],
        "main.st",
        "expected int, found str",
        2,
        13,
    );
}

#[test]
fn library_paths_are_shown_from_the_entry_files_directory() {
    let (entry, source) = ("app/main.st", "use util\nprint(util.twice(1))\n");
    let sources = sources_of(&[("app/util.st", UTIL.1)]);
    let (map, result) = driver::load(Path::new(entry), source, &sources);
    assert!(result.is_ok());
    assert_eq!(map.file(FileId(1)).path, Path::new("app/util.st"));
    assert_eq!(map.file(FileId(1)).module, "util");
}

#[test]
fn library_functions_are_named_by_their_module() {
    let linked = link(
        Path::new("main.st"),
        "use geometry.vec\nprint(vec.zero())\n",
        &sources_of(&[("geometry/vec.st", "pub def zero();\n    ret 0\n")]),
    );
    let analysis = driver::analyze_linked(&linked);
    assert!(
        analysis
            .symbols
            .iter()
            .any(|s| s.name == "geometry.vec.zero" && s.span.file == FileId(1))
    );
}

#[test]
fn lookups_by_position_stay_in_their_file() {
    let linked = link(
        Path::new("main.st"),
        "use util\n\ndef main_only(n);\n    ret util.twice(n)\n",
        &sources_of(&[UTIL]),
    );
    let analysis = driver::analyze_linked(&linked);
    // line 4 col 9 is inside `util` in main.st, and inside `helper` in util.st
    let in_util = analysis.reference_at(FileId(1), Pos::new(2, 9)).unwrap();
    assert_eq!(analysis.symbols[in_util.symbol].name, "util.helper");
    let in_main = analysis.reference_at(FileId(0), Pos::new(4, 18)).unwrap();
    assert_eq!(analysis.symbols[in_main.symbol].name, "util.twice");

    let mut visible: Vec<&str> = analysis
        .visible_at(FileId(1), Pos::new(2, 5))
        .map(|s| s.name.as_str())
        .collect();
    visible.sort();
    assert_eq!(visible, ["util.helper", "util.twice", "x"]);
    // at the top level of main.st, nothing from util.st is visible by its own name
    let visible: Vec<&str> = analysis
        .visible_at(FileId(0), Pos::new(2, 1))
        .map(|s| s.name.as_str())
        .collect();
    assert_eq!(visible, ["main_only"]);
}

#[test]
fn a_module_can_be_linked_as_the_entry_from_the_project_root() {
    let sources = sources_of(&[
        ("p/geometry/vec.st", "pub def zero();\n    ret 0\n"),
        ("p/main.st", "print(1)\n"),
    ]);
    let linked = link_in(
        Path::new("p"),
        Path::new("p/geometry/shapes.st"),
        "use geometry.vec\nuse geometry.shapes\n\npub def area();\n    ret vec.zero()\n",
        &sources,
    );
    let messages: Vec<&str> = linked
        .diagnostics
        .iter()
        .map(|d| d.message.as_str())
        .collect();
    assert_eq!(
        messages,
        ["'geometry.shapes' is the entry file, so it cannot be imported"]
    );
    assert_eq!(
        linked.sources.find(Path::new("p/geometry/vec.st")),
        Some(FileId(1))
    );
    assert_eq!(linked.sources.find(Path::new("p/main.st")), None);
}

#[test]
fn links_record_imports_and_public_functions() {
    let linked = link(
        Path::new("main.st"),
        "use util\nuse util.twice as double\n",
        &sources_of(&[UTIL]),
    );
    let names: Vec<(&str, &Target)> = linked
        .imports
        .iter()
        .map(|i| (i.name.as_str(), &i.target))
        .collect();
    assert_eq!(
        names,
        [
            ("util", &Target::Module(FileId(1))),
            ("double", &Target::Function("util.twice".to_string())),
        ]
    );
    assert!(linked.public.contains("util.twice"));
    assert!(!linked.public.contains("util.helper"));
}

#[test]
fn map_sources_list_directories() {
    let sources = sources_of(&[
        ("p/main.st", ""),
        ("p/geometry/vec.st", ""),
        ("p/geometry/shapes.st", ""),
    ]);
    assert_eq!(
        sources.entries(Path::new("p")),
        [PathBuf::from("p/geometry"), PathBuf::from("p/main.st")]
    );
    assert!(sources.is_dir(Path::new("p/geometry")));
    assert!(!sources.is_dir(Path::new("p/main.st")));
}

#[test]
fn the_os_module_is_builtin() {
    let platform = crate::stdlib::os::platform();
    assert_eq!(
        output(&[("main.st", "use os\nprint(os.platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.st", "use os.platform\nprint(platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.st", "use os as system\nprint(system.platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.st", "use os.platform as p\nprint(p())\n")]),
        format!("{platform}\n")
    );
}

#[test]
fn a_library_module_can_use_os() {
    assert_eq!(
        output(&[
            ("main.st", "use util\nprint(util.ok())\n"),
            ("util.st", "use os\n\npub def ok();\n    ret os.pid() > 0\n"),
        ]),
        "true\n"
    );
}

#[test]
fn os_links_to_builtin_names() {
    let linked = link(
        Path::new("main.st"),
        "use os\nuse os.pid\nprint(os.cwd(), pid())\n",
        &MapSources::default(),
    );
    assert_eq!(linked.diagnostics, []);
    let Mod::Module { body } = &linked.module;
    let StmtKind::Expr { value } = &body[0].kind else {
        panic!("expected a call");
    };
    let ExprKind::Call { args, .. } = &value.kind else {
        panic!("expected a call");
    };
    let names: Vec<&str> = args
        .iter()
        .map(|arg| match &arg.kind {
            ExprKind::Call { func, .. } => match &func.kind {
                ExprKind::Name { id, .. } => id.as_str(),
                _ => "",
            },
            _ => "",
        })
        .collect();
    assert_eq!(names, ["os.cwd", "os.pid"]);
    let targets: Vec<(&str, &Target)> = linked
        .imports
        .iter()
        .map(|i| (i.name.as_str(), &i.target))
        .collect();
    assert_eq!(
        targets,
        [
            ("os", &Target::BuiltinModule("os".to_string())),
            ("pid", &Target::Function("os.pid".to_string())),
        ]
    );
}

#[test]
fn os_has_only_its_own_functions() {
    assert_error(
        &[("main.st", "use os.nope\n")],
        "main.st",
        "module 'os' has no function 'nope'",
        1,
        8,
    );
    assert_error(
        &[("main.st", "use os\nos.nope()\n")],
        "main.st",
        "module 'os' has no function 'nope'",
        2,
        4,
    );
    assert_error(
        &[("main.st", "use os.env.x\n")],
        "main.st",
        "no module named 'os.env.x'",
        1,
        5,
    );
}

#[test]
fn os_is_not_a_value() {
    assert_error(
        &[("main.st", "use os\nx = os\n")],
        "main.st",
        "'os' is a module, not a value",
        2,
        5,
    );
}

#[test]
fn os_cannot_be_called_without_importing_it() {
    let errors = errors(&[("main.st", "print(os.pid())\n")]);
    assert_eq!(
        errors[0],
        (
            "main.st".to_string(),
            "undefined name 'os'".to_string(),
            1,
            7
        )
    );
}

#[test]
fn a_file_cannot_take_the_builtin_os_modules_name() {
    assert_error(
        &[
            ("main.st", "use os\nprint(os.pid() > 0)\n"),
            ("os.st", "pub def pid();\n    ret 0\n"),
        ],
        "main.st",
        "'os' is a builtin module, so rename os.st",
        1,
        5,
    );
}
