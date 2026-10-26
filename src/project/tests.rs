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
/// For example, `output(&[("main.stn", "use util\nprint(util.one())\n"), ("util.stn", "pub fn
/// one(); ret 1\n")])` returns `"1\n"`.
fn output(files: &[(&str, &str)]) -> String {
    let (entry, source) = files[0];
    let (map, result) = driver::load(Path::new(entry), source, &sources_of(&files[1..]));
    let checked = result.unwrap_or_else(|errors| {
        let rendered: String = errors.0.iter().map(|d| map.render(d)).collect();
        panic!("program should have no errors:\n{rendered}")
    });
    let mut out = vec![];
    driver::run_module(
        &checked.module,
        &checked.calls,
        &mut std::io::empty(),
        &mut out,
        &[],
        Limits::DEFAULT,
    )
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
    "util.stn",
    "pub fn twice(x: int) -> int:\n    ret helper(x) * 2\n\nfn helper(x: int) -> int:\n    ret x\n",
);

#[test]
fn a_module_import_calls_its_public_functions() {
    assert_eq!(
        output(&[("main.stn", "use util\nprint(util.twice(2))\n"), UTIL]),
        "4\n"
    );
}

#[test]
fn a_function_import_is_called_by_its_name() {
    assert_eq!(
        output(&[("main.stn", "use util.twice\nprint(twice(2))\n"), UTIL]),
        "4\n"
    );
}

#[test]
fn as_renames_an_import() {
    assert_eq!(
        output(&[
            (
                "main.stn",
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
            ("main.stn", "use geometry.shapes\nprint(shapes.area(2))\n"),
            (
                "geometry/shapes.stn",
                "use geometry.vec.dot\n\npub fn area(r: int) -> int:\n    ret 3 * dot([r], [r])\n"
            ),
            (
                "geometry/vec.stn",
                "pub fn dot(a: list[int], b: list[int]) -> int:\n    ret a[0] * b[0]\n"
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
                "main.stn",
                "use geometry\nuse geometry.vec\nprint(geometry.unit(), vec.zero())\n"
            ),
            ("geometry.stn", "pub fn unit() -> int:\n    ret 1\n"),
            ("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
        ]),
        "1 0\n"
    );
}

#[test]
fn a_module_file_wins_over_a_function_of_the_same_path() {
    assert_eq!(
        output(&[
            ("main.stn", "use geometry.vec\nprint(vec.zero())\n"),
            ("geometry.stn", "pub fn vec() -> int:\n    ret 1\n"),
            ("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
        ]),
        "0\n"
    );
}

#[test]
fn private_functions_of_different_modules_do_not_collide() {
    let a = (
        "a.stn",
        "fn helper() -> int:\n    ret 1\npub fn get() -> int:\n    ret helper()\n",
    );
    let b = (
        "b.stn",
        "fn helper() -> int:\n    ret 2\npub fn get() -> int:\n    ret helper()\n",
    );
    assert_eq!(
        output(&[
            ("main.stn", "use a\nuse b\nprint(a.get(), b.get())\n"),
            a,
            b
        ]),
        "1 2\n"
    );
}

#[test]
fn modules_may_import_each_other() {
    let even = (
        "even.stn",
        "use odd\n\npub fn even(n: int) -> bool:\n    if n == 0:\n        ret true\n    ret odd.odd(n - 1)\n",
    );
    let odd = (
        "odd.stn",
        "use even\n\npub fn odd(n: int) -> bool:\n    if n == 0:\n        ret false\n    ret even.even(n - 1)\n",
    );
    assert_eq!(
        output(&[("main.stn", "use even\nprint(even.even(10))\n"), even, odd]),
        "true\n"
    );
}

#[test]
fn a_local_may_shadow_an_import() {
    assert_eq!(
        output(&[
            (
                "main.stn",
                "use util.twice\n\nfn f(twice: int) -> int:\n    ret twice + 1\n\nprint(f(1), twice(1))\n"
            ),
            UTIL
        ]),
        "2 2\n"
    );
}

#[test]
fn pub_is_ignored_in_the_entry_file() {
    assert_eq!(
        output(&[("main.stn", "pub fn f() -> int:\n    ret 1\nprint(f())\n")]),
        "1\n"
    );
}

#[test]
fn library_functions_take_their_written_types() {
    assert_eq!(
        output(&[
            ("main.stn", "use util.first\nprint(first([\"a\", \"b\"]))\n"),
            (
                "util.stn",
                "pub fn first(xs: list[str]) -> str:\n    ret xs[0]\n"
            ),
        ]),
        "a\n"
    );
}

#[test]
fn a_missing_module_is_an_error() {
    assert_error(
        &[("main.stn", "use nope\n")],
        "main.stn",
        "no module named 'nope'",
        1,
        5,
    );
    assert_error(
        &[("main.stn", "use a.b.c\n")],
        "main.stn",
        "no module named 'a.b.c'",
        1,
        5,
    );
}

#[test]
fn a_directory_cannot_be_imported() {
    assert_error(
        &[
            ("main.stn", "use geometry\n"),
            ("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
        ],
        "main.stn",
        "'geometry' is a directory, so use a module inside it",
        1,
        5,
    );
}

#[test]
fn a_missing_function_is_an_error() {
    assert_error(
        &[("main.stn", "use util.nope\n"), UTIL],
        "main.stn",
        "module 'util' has no function 'nope'",
        1,
        10,
    );
    assert_error(
        &[("main.stn", "use util\nutil.nope()\n"), UTIL],
        "main.stn",
        "module 'util' has no function 'nope'",
        2,
        6,
    );
}

#[test]
fn a_private_function_cannot_be_used_from_another_file() {
    assert_error(
        &[("main.stn", "use util.helper\n"), UTIL],
        "main.stn",
        "'helper' is private to module 'util'",
        1,
        10,
    );
    assert_error(
        &[("main.stn", "use util\nprint(util.helper(1))\n"), UTIL],
        "main.stn",
        "'helper' is private to module 'util'",
        2,
        12,
    );
}

#[test]
fn a_module_is_not_a_value() {
    assert_error(
        &[("main.stn", "use util\nx = util\n"), UTIL],
        "main.stn",
        "'util' is a module, not a value",
        2,
        5,
    );
}

#[test]
fn an_imported_name_must_be_unique_in_its_file() {
    assert_error(
        &[
            ("main.stn", "use util.twice\nuse other.twice\n"),
            UTIL,
            ("other.stn", "pub fn twice() -> int:\n    ret 2\n"),
        ],
        "main.stn",
        "'twice' is already imported",
        2,
        11,
    );
    assert_error(
        &[
            (
                "main.stn",
                "use util.twice\n\nfn twice() -> int:\n    ret 1\n",
            ),
            UTIL,
        ],
        "main.stn",
        "'twice' is already defined in this file",
        1,
        10,
    );
    assert_error(
        &[("main.stn", "use util\nutil = 1\n"), UTIL],
        "main.stn",
        "'util' is already defined in this file",
        1,
        5,
    );
}

#[test]
fn an_import_cannot_shadow_a_builtin() {
    assert_error(
        &[
            ("main.stn", "use str\n"),
            ("str.stn", "pub fn f() -> int:\n    ret 1\n"),
        ],
        "main.stn",
        "'str' is a builtin, so rename it with 'as'",
        1,
        5,
    );
}

#[test]
fn use_comes_before_other_statements() {
    assert_error(
        &[("main.stn", "x = 1\nuse util\n"), UTIL],
        "main.stn",
        "use must come before other statements",
        2,
        1,
    );
}

#[test]
fn the_entry_file_cannot_be_imported() {
    let message = "'main' is the entry file, so it cannot be imported";
    assert_error(&[("main.stn", "use main\n")], "main.stn", message, 1, 5);
    assert_error(
        &[
            ("main.stn", "use util\n\nfn f() -> int:\n    ret 1\n"),
            ("util.stn", "use main.f\n"),
        ],
        "util.stn",
        message,
        1,
        5,
    );
}

#[test]
fn a_module_only_holds_use_and_def() {
    assert_error(
        &[
            ("main.stn", "use util\n"),
            ("util.stn", "print(1)\n\npub fn f() -> int:\n    ret 1\n"),
        ],
        "util.stn",
        "only use, fn, enum, struct, impl, and trait are allowed at the top level of a module",
        1,
        1,
    );
}

#[test]
fn a_module_cannot_see_the_entry_files_names() {
    assert_error(
        &[
            ("main.stn", "use util\ncount = 1\nprint(util.get())\n"),
            ("util.stn", "pub fn get() -> int:\n    ret count\n"),
        ],
        "util.stn",
        "undefined name 'count'",
        2,
        9,
    );
    assert_error(
        &[
            (
                "main.stn",
                "use util\n\nfn helper() -> int:\n    ret 1\n\nprint(util.get())\n",
            ),
            ("util.stn", "pub fn get() -> int:\n    ret helper()\n"),
        ],
        "util.stn",
        "undefined function 'helper'",
        2,
        9,
    );
}

#[test]
fn errors_point_into_the_file_they_are_in() {
    let errors = errors(&[("main.stn", "use util\n"), ("util.stn", "pub fn f(:\n")]);
    assert_eq!(errors.len(), 1);
    assert_eq!(errors[0].0, "util.stn");
}

#[test]
fn type_errors_across_files_point_at_the_caller() {
    // library functions are checked first, so their types are settled before the call
    assert_error(
        &[("main.stn", "use util.twice\nprint(twice(\"a\"))\n"), UTIL],
        "main.stn",
        "expected int, found str",
        2,
        13,
    );
}

#[test]
fn library_paths_are_shown_from_the_entry_files_directory() {
    let (entry, source) = ("app/main.stn", "use util\nprint(util.twice(1))\n");
    let sources = sources_of(&[("app/util.stn", UTIL.1)]);
    let (map, result) = driver::load(Path::new(entry), source, &sources);
    assert!(result.is_ok());
    assert_eq!(map.file(FileId(1)).path, Path::new("app/util.stn"));
    assert_eq!(map.file(FileId(1)).module, "util");
}

#[test]
fn library_functions_are_named_by_their_module() {
    let linked = link(
        Path::new("main.stn"),
        "use geometry.vec\nprint(vec.zero())\n",
        &sources_of(&[("geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n")]),
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
        Path::new("main.stn"),
        "use util\n\nfn main_only(n: int) -> int:\n    ret util.twice(n)\n",
        &sources_of(&[UTIL]),
    );
    let analysis = driver::analyze_linked(&linked);
    // line 4 col 9 is inside `util` in main.stn, and inside `helper` in util.stn
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
    // at the top level of main.stn, nothing from util.stn is visible by its own name
    let visible: Vec<&str> = analysis
        .visible_at(FileId(0), Pos::new(2, 1))
        .map(|s| s.name.as_str())
        .collect();
    assert_eq!(visible, ["main_only"]);
}

#[test]
fn a_module_can_be_linked_as_the_entry_from_the_project_root() {
    let sources = sources_of(&[
        ("p/geometry/vec.stn", "pub fn zero() -> int:\n    ret 0\n"),
        ("p/main.stn", "print(1)\n"),
    ]);
    let linked = link_in(
        Path::new("p"),
        Path::new("p/geometry/shapes.stn"),
        "use geometry.vec\nuse geometry.shapes\n\npub fn area() -> int:\n    ret vec.zero()\n",
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
        linked.sources.find(Path::new("p/geometry/vec.stn")),
        Some(FileId(1))
    );
    assert_eq!(linked.sources.find(Path::new("p/main.stn")), None);
}

#[test]
fn links_record_imports_and_public_functions() {
    let linked = link(
        Path::new("main.stn"),
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
        ("p/main.stn", ""),
        ("p/geometry/vec.stn", ""),
        ("p/geometry/shapes.stn", ""),
    ]);
    assert_eq!(
        sources.entries(Path::new("p")),
        [PathBuf::from("p/geometry"), PathBuf::from("p/main.stn")]
    );
    assert!(sources.is_dir(Path::new("p/geometry")));
    assert!(!sources.is_dir(Path::new("p/main.stn")));
}

#[test]
fn the_os_module_is_builtin() {
    let platform = crate::stdlib::os::platform();
    assert_eq!(
        output(&[("main.stn", "use os\nprint(os.platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.stn", "use os.platform\nprint(platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.stn", "use os as system\nprint(system.platform())\n")]),
        format!("{platform}\n")
    );
    assert_eq!(
        output(&[("main.stn", "use os.platform as p\nprint(p())\n")]),
        format!("{platform}\n")
    );
}

#[test]
fn a_library_module_can_use_os() {
    assert_eq!(
        output(&[
            ("main.stn", "use util\nprint(util.ok())\n"),
            (
                "util.stn",
                "use os\n\npub fn ok() -> bool:\n    ret os.pid() > 0\n"
            ),
        ]),
        "true\n"
    );
}

#[test]
fn os_links_to_builtin_names() {
    let linked = link(
        Path::new("main.stn"),
        "use os\nuse os.pid\nprint(os.cwd(), pid())\n",
        &MapSources::default(),
    );
    assert_eq!(linked.diagnostics, []);
    let Mod::Module { body } = &linked.module;
    let StmtKind::Expr { value } = &body.last().unwrap().kind else {
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
        &[("main.stn", "use os.nope\n")],
        "main.stn",
        "module 'os' has no function 'nope'",
        1,
        8,
    );
    assert_error(
        &[("main.stn", "use os\nos.nope()\n")],
        "main.stn",
        "module 'os' has no function 'nope'",
        2,
        4,
    );
    assert_error(
        &[("main.stn", "use os.env.x\n")],
        "main.stn",
        "no module named 'os.env.x'",
        1,
        5,
    );
}

#[test]
fn os_is_not_a_value() {
    assert_error(
        &[("main.stn", "use os\nx = os\n")],
        "main.stn",
        "'os' is a module, not a value",
        2,
        5,
    );
}

#[test]
fn os_cannot_be_called_without_importing_it() {
    let errors = errors(&[("main.stn", "print(os.pid())\n")]);
    assert_eq!(
        errors[0],
        (
            "main.stn".to_string(),
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
            ("main.stn", "use os\nprint(os.pid() > 0)\n"),
            ("os.stn", "pub fn pid() -> int:\n    ret 0\n"),
        ],
        "main.stn",
        "'os' is a builtin module, so rename os.stn",
        1,
        5,
    );
}

#[test]
fn every_builtin_module_can_be_used_like_os() {
    assert_eq!(
        output(&[("main.stn", "use math\nprint(math.max(1, 2))\n")]),
        "2\n"
    );
    assert_eq!(
        output(&[("main.stn", "use math.sqrt as root\nprint(root(4))\n")]),
        "2.0\n"
    );
    assert_eq!(
        output(&[
            ("main.stn", "use util\nprint(util.biggest([3, 9, 4]))\n"),
            (
                "util.stn",
                "use math\n\npub fn biggest(xs: list[int]) -> int:\n    ret math.max(xs)\n"
            ),
        ]),
        "9\n"
    );
    assert_error(
        &[("main.stn", "use math\nmath.nope()\n")],
        "main.stn",
        "module 'math' has no function 'nope'",
        2,
        6,
    );
}

#[test]
fn a_file_cannot_take_a_builtin_modules_name() {
    for module in &crate::stdlib::MODULES {
        let name = module.name;
        let file = format!("{name}.stn");
        let entry = format!("use {name}\n");
        assert_error(
            &[
                (DEFAULT_ENTRY, &entry),
                (&file, "pub fn f() -> int:\n    ret 0\n"),
            ],
            DEFAULT_ENTRY,
            &format!("'{name}' is a builtin module, so rename {name}.stn"),
            1,
            5,
        );
    }
}

#[test]
fn a_builtin_modules_functions_do_not_reserve_their_names() {
    assert_eq!(
        output(&[(
            "main.stn",
            "use math\nmin = 3\nmax = math.max(min, 5)\nprint(min, max)\n"
        )]),
        "3 5\n"
    );
}

const ZOO: (&str, &str) = (
    "zoo.stn",
    "pub enum Animal:\n    Dog(str)\n    Fish\nenum Secret:\n    Hidden\n\
     pub fn name(a: Animal) -> str:\n    match a:\n        case Animal.Dog(n):\n            ret n\n\
     \x20       case Animal.Fish:\n            ret \"fish\"\n",
);

#[test]
fn a_public_enum_is_used_by_name_or_through_its_module() {
    let main = "use zoo\nuse zoo.Animal\nfn twice(a: zoo.Animal) -> list[Animal]:\n    ret [a, a]\n\
                print(twice(Animal.Dog(\"rex\")), zoo.name(zoo.Animal.Fish))\n\
                match twice(Animal.Fish)[0]:\n    case zoo.Animal.Fish:\n        print(\"fish\")\n\
                \x20   case Animal.Dog(_):\n        print(\"dog\")\n";
    assert_eq!(
        output(&[("main.stn", main), ZOO]),
        "[Animal.Dog('rex'), Animal.Dog('rex')] fish\nfish\n"
    );
}

#[test]
fn a_private_enum_cannot_be_used_from_another_file() {
    assert_error(
        &[("main.stn", "use zoo.Secret\n"), ZOO],
        "main.stn",
        "'Secret' is private to module 'zoo'",
        1,
        9,
    );
    assert_error(
        &[("main.stn", "use zoo\nx = zoo.Secret.Hidden\n"), ZOO],
        "main.stn",
        "'Secret' is private to module 'zoo'",
        2,
        5,
    );
    assert_error(
        &[("main.stn", "use zoo\nfn f(x: zoo.Bird):\n    ret\n"), ZOO],
        "main.stn",
        "module 'zoo' has no type 'Bird'",
        2,
        9,
    );
}

#[test]
fn an_imported_enum_cannot_share_a_name_with_one_the_file_defines() {
    assert_error(
        &[("main.stn", "use zoo.Animal\nenum Animal:\n    Cat\n"), ZOO],
        "main.stn",
        "'Animal' is already defined in this file",
        1,
        9,
    );
}

const GEO: (&str, &str) = (
    "geo.stn",
    "pub struct Point:\n    x: int\n    y: int\nstruct Secret:\n    code: int\n\
     pub fn shift(p: Point, dx: int) -> Point:\n    p.x += dx\n    ret p\n\
     pub fn origin() -> Point:\n    ret Point(x=0, y=0)\n",
);

#[test]
fn a_public_struct_is_used_by_name_or_through_its_module() {
    let main = "use geo\nuse geo.Point\nfn pair(a: geo.Point) -> list[Point]:\n    ret [a, a]\n\
                p = geo.Point(x=1, y=2)\nq = Point(y=4, x=3)\nq.y = 5\n\
                print(pair(p), geo.shift(q, 10), geo.origin().y)\n";
    assert_eq!(
        output(&[("main.stn", main), GEO]),
        "[Point(x=1, y=2), Point(x=1, y=2)] Point(x=13, y=5) 0\n"
    );
}

#[test]
fn a_struct_import_binds_the_struct() {
    let linked = link(
        Path::new("main.stn"),
        "use geo.Point\n",
        &sources_of(&[GEO]),
    );
    assert!(linked.diagnostics.is_empty());
    assert_eq!(
        linked.imports[0].target,
        Target::Struct("geo.Point".to_string())
    );
}

#[test]
fn a_private_struct_cannot_be_used_from_another_file() {
    assert_error(
        &[("main.stn", "use geo.Secret\n"), GEO],
        "main.stn",
        "'Secret' is private to module 'geo'",
        1,
        9,
    );
    assert_error(
        &[("main.stn", "use geo\nx = geo.Secret(code=1)\n"), GEO],
        "main.stn",
        "'Secret' is private to module 'geo'",
        2,
        9,
    );
    assert_error(
        &[("main.stn", "use geo\nfn f(p: geo.Spot):\n    ret\n"), GEO],
        "main.stn",
        "module 'geo' has no type 'Spot'",
        2,
        9,
    );
}

#[test]
fn an_imported_struct_cannot_share_a_name_with_one_the_file_defines() {
    assert_error(
        &[
            ("main.stn", "use geo.Point\nstruct Point:\n    z: int\n"),
            GEO,
        ],
        "main.stn",
        "'Point' is already defined in this file",
        1,
        9,
    );
}

const SHAPES: (&str, &str) = (
    "shapes.stn",
    "pub struct Square:\n    side: int\nimpl Square:\n    fn unit() -> Square:\n        \
     ret Square(side=one())\n    fn area(self) -> int:\n        ret self.side * self.side\n\
     fn one() -> int:\n    ret 1\n",
);

#[test]
fn an_impl_in_a_library_names_its_struct_and_functions_by_their_linked_names() {
    let linked = link(
        Path::new("main.stn"),
        "use shapes\nprint(shapes.Square.unit())\n",
        &sources_of(&[SHAPES]),
    );
    assert_eq!(linked.diagnostics, []);
    let Mod::Module { body } = &linked.module;
    let Some(StmtKind::ImplDef { name, methods, .. }) = body
        .iter()
        .map(|stmt| &stmt.kind)
        .find(|kind| matches!(kind, StmtKind::ImplDef { .. }))
    else {
        panic!("expected an impl block");
    };
    assert_eq!(name, "shapes.Square");
    let StmtKind::FunctionDef {
        name, body: unit, ..
    } = &methods[0].kind
    else {
        panic!("expected a method");
    };
    assert_eq!(name, "unit");
    let StmtKind::Return { value: Some(value) } = &unit[0].kind else {
        panic!("expected a return");
    };
    let ExprKind::Call { func, keywords, .. } = &value.kind else {
        panic!("expected a call");
    };
    assert!(matches!(&func.kind, ExprKind::Name { id, .. } if id == "shapes.Square"));
    assert!(matches!(
        &keywords[0].value.kind,
        ExprKind::Call { func, .. } if matches!(&func.kind, ExprKind::Name { id, .. } if id == "shapes.one")
    ));

    // the call through the module names the struct by its linked name
    let StmtKind::Expr { value } = &body.last().unwrap().kind else {
        panic!("expected a call");
    };
    let ExprKind::Call { args, .. } = &value.kind else {
        panic!("expected a call");
    };
    let ExprKind::Call { func, .. } = &args[0].kind else {
        panic!("expected a call");
    };
    let ExprKind::Attribute { value, attr, .. } = &func.kind else {
        panic!("expected an associated function");
    };
    assert_eq!(attr, "unit");
    assert!(matches!(&value.kind, ExprKind::Name { id, .. } if id == "shapes.Square"));
}

#[test]
fn an_impl_is_for_a_struct_of_its_own_file() {
    assert_error(
        &[("main.stn", "impl Point:\n    fn f(self):\n        ret\n")],
        "main.stn",
        "no struct named 'Point' is defined in this file",
        1,
        6,
    );
    assert_error(
        &[(
            "main.stn",
            "enum Shape:\n    Dot\nimpl Shape:\n    fn f(self):\n        ret\n",
        )],
        "main.stn",
        "'Shape' is an enum, and only a struct has an impl block for now",
        3,
        6,
    );
    assert_error(
        &[
            (
                "main.stn",
                "use geo.Point\nimpl Point:\n    fn f(self):\n        ret\n",
            ),
            GEO,
        ],
        "main.stn",
        "'Point' is defined in module 'geo', so its impl block goes in that file",
        2,
        6,
    );
}

#[test]
fn a_method_cannot_share_its_linked_name_with_a_module_function() {
    assert_error(
        &[
            ("main.stn", "use shapes\nuse shapes.Square.unit\n"),
            SHAPES,
            ("shapes/Square.stn", "pub fn unit() -> int:\n    ret 1\n"),
        ],
        "shapes/Square.stn",
        "'shapes.Square.unit' is both a function of this module and a method of 'Square' in \
         module 'shapes'",
        1,
        8,
    );
}

#[test]
fn a_library_may_define_generic_functions() {
    assert_eq!(
        output(&[
            (
                "main.stn",
                "use util.first\nuse util\nprint(first([1, 2]))\nprint(util.first([\"a\"]))\n"
            ),
            (
                "util.stn",
                "pub fn first[T](xs: list[T]) -> T:\n    ret only(xs[0])\n\nfn only[T](x: T) -> T:\n    ret x\n"
            ),
        ]),
        "1\na\n"
    );
}

#[test]
fn a_type_parameter_cannot_reuse_a_type_name() {
    assert_error(
        &[
            (
                "main.stn",
                "use geo.Point\nfn f[Point](p: Point):\n    print(p)\n",
            ),
            ("geo.stn", "pub struct Point:\n    x: int\n"),
        ],
        "main.stn",
        "'Point' is already a type",
        2,
        6,
    );
    assert_error(
        &[
            ("main.stn", "use geo\nprint(1)\n"),
            (
                "geo.stn",
                "enum Shape:\n    Dot\n\nfn f[Shape](s: Shape):\n    print(s)\n",
            ),
        ],
        "geo.stn",
        "'Shape' is already a type",
        4,
        6,
    );
}

#[test]
fn every_program_links_the_preludes_option_enum() {
    let linked = link(Path::new("main.stn"), "x = 1\n", &MapSources::default());
    assert!(linked.diagnostics.is_empty(), "{:?}", linked.diagnostics);
    let Mod::Module { body } = &linked.module;
    let StmtKind::EnumDef {
        name,
        type_params,
        variants,
        ..
    } = &body[0].kind
    else {
        panic!("the prelude's enum should come first: {:?}", body[0]);
    };
    assert_eq!(name, "option");
    assert_eq!(type_params.len(), 1);
    let names: Vec<&str> = variants.iter().map(|v| v.name.as_str()).collect();
    assert_eq!(names, ["none", "some"]);
    assert_eq!(body[0].span.file, FileId::PRELUDE);
    assert_eq!(linked.sources.file(FileId::PRELUDE).source, PRELUDE);
    // the prelude is not one of the program's own files
    assert!(linked.sources.files().all(|(id, _)| id != FileId::PRELUDE));
}

#[test]
fn bare_some_and_none_name_the_preludes_variants() {
    let linked = link(
        Path::new("main.stn"),
        "x = some(1)\ny = none\nmatch x:\n    case some(v):\n        print(v)\n    case none:\n        print(y)\n",
        &MapSources::default(),
    );
    assert!(linked.diagnostics.is_empty(), "{:?}", linked.diagnostics);
    let Mod::Module { body } = &linked.module;
    // the prelude's own statements come first, then the entry file's, kept as if from index 1
    let skipped = body
        .iter()
        .take_while(|stmt| stmt.span.file == FileId::PRELUDE)
        .count();
    let body = &body[skipped - 1..];
    let variant = |expr: &Expr| match &expr.kind {
        ExprKind::Attribute { value, attr, .. } => match &value.kind {
            ExprKind::Name { id, .. } => (id.clone(), attr.clone(), value.span),
            _ => panic!("{expr:?}"),
        },
        _ => panic!("{expr:?}"),
    };
    let StmtKind::Assign { value, .. } = &body[1].kind else {
        panic!("{:?}", body[1]);
    };
    let ExprKind::Call { func, .. } = &value.kind else {
        panic!("{value:?}");
    };
    let (enum_name, attr, span) = variant(func);
    assert_eq!((enum_name.as_str(), attr.as_str()), ("option", "some"));
    // the enum's name was never written, so it covers no text
    assert_eq!(span.start, span.end);
    let StmtKind::Assign { value, .. } = &body[2].kind else {
        panic!("{:?}", body[2]);
    };
    let (enum_name, attr, _) = variant(value);
    assert_eq!((enum_name.as_str(), attr.as_str()), ("option", "none"));
    let StmtKind::Match { cases, .. } = &body[3].kind else {
        panic!("{:?}", body[3]);
    };
    let paths: Vec<Vec<&str>> = cases
        .iter()
        .map(|case| match &case.pattern.kind {
            PatternKind::Variant { path, .. } => path.iter().map(|(n, _)| n.as_str()).collect(),
            other => panic!("{other:?}"),
        })
        .collect();
    assert_eq!(paths, [["option", "some"], ["option", "none"]]);
}

#[test]
fn a_library_module_uses_options_like_the_entry_file() {
    assert_eq!(
        output(&[
            (
                "main.stn",
                "use util\nprint(util.first([3, 4]), util.first([]))\n"
            ),
            (
                "util.stn",
                "pub fn first(xs: list[int]) -> option[int]:\n    if xs.len() == 0:\n        ret none\n    ret some(xs[0])\n",
            ),
        ]),
        "some(3) none\n"
    );
}

#[test]
fn option_variants_are_written_without_the_enums_name() {
    let message = "write 'some(x)' or 'none' without 'option.'";
    assert_error(
        &[("main.stn", "x = option.some(1)\n")],
        "main.stn",
        message,
        1,
        5,
    );
    assert_error(
        &[(
            "main.stn",
            "x = some(1)\nmatch x:\n    case option.some(v):\n        print(v)\n    case _:\n        print(0)\n",
        )],
        "main.stn",
        message,
        3,
        10,
    );
}

#[test]
fn an_import_cannot_take_a_prelude_variants_name() {
    assert_error(
        &[("main.stn", "use util.twice as some\n"), UTIL],
        "main.stn",
        "'some' is a builtin, so rename it with 'as'",
        1,
        19,
    );
}

#[test]
fn only_the_prelude_names_a_variant_none() {
    assert_error(
        &[("main.stn", "enum E:\n    none\n")],
        "main.stn",
        "expected a variant name, found 'none'",
        2,
        5,
    );
}

const ROUND: (&str, &str) = (
    "round.stn",
    "pub trait Shape:\n    fn area(self) -> int\n    fn twice(self) -> int:\n        \
     ret self.area() * 2\ntrait Secret:\n    fn code(self) -> int\n\
     pub fn total[T: Shape](xs: list[T]) -> int:\n    t = 0\n    for x in xs:\n        \
     t += x.area()\n    ret t\n",
);

/// Returns the linked module's statements of `files`, which must link without errors.
fn linked_body(files: &[(&str, &str)]) -> Vec<Stmt> {
    let (entry, source) = files[0];
    let linked = link(Path::new(entry), source, &sources_of(&files[1..]));
    assert_eq!(linked.diagnostics, []);
    let Mod::Module { body } = linked.module;
    body
}

/// Returns the bounds of the first type parameter of the linked function `name`.
fn bounds_of(body: &[Stmt], name: &str) -> Vec<String> {
    body.iter()
        .find_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef {
                name: n,
                type_params,
                ..
            } if n == name => Some(
                type_params[0]
                    .bounds
                    .iter()
                    .map(|b| b.name.clone())
                    .collect(),
            ),
            _ => None,
        })
        .expect("expected the function")
}

#[test]
fn a_library_trait_is_named_by_its_linked_name() {
    let body = linked_body(&[
        (
            "main.stn",
            "use round\nuse round.Shape\nstruct Sq:\n    s: int\nimpl Shape for Sq:\n    \
             fn area(self) -> int:\n        ret self.s\nfn f[T: Shape](x: T) -> int:\n    \
             ret x.area()\nfn g[T: round.Shape](x: T) -> int:\n    ret x.area()\n",
        ),
        ROUND,
    ]);
    assert!(body.iter().any(
        |stmt| matches!(&stmt.kind, StmtKind::TraitDef { name, .. } if name == "round.Shape")
    ));
    assert_eq!(bounds_of(&body, "round.total"), ["round.Shape"]);
    assert_eq!(bounds_of(&body, "f"), ["round.Shape"]);
    assert_eq!(bounds_of(&body, "g"), ["round.Shape"]);
    let Some(StmtKind::ImplDef {
        name, trait_name, ..
    }) = body
        .iter()
        .map(|stmt| &stmt.kind)
        .find(|kind| matches!(kind, StmtKind::ImplDef { .. }))
    else {
        panic!("expected an impl block");
    };
    assert_eq!(name, "Sq");
    assert_eq!(trait_name.as_ref().unwrap().name, "round.Shape");
}

#[test]
fn a_trait_import_binds_the_trait() {
    let linked = link(
        Path::new("main.stn"),
        "use round.Shape\n",
        &sources_of(&[ROUND]),
    );
    assert_eq!(
        linked.imports[0].target,
        Target::Trait("round.Shape".to_string())
    );
}

#[test]
fn a_private_trait_cannot_be_used_from_another_file() {
    assert_error(
        &[("main.stn", "use round.Secret\n"), ROUND],
        "main.stn",
        "'Secret' is private to module 'round'",
        1,
        11,
    );
    assert_error(
        &[
            (
                "main.stn",
                "use round\nfn f[T: round.Secret](x: T):\n    ret\n",
            ),
            ROUND,
        ],
        "main.stn",
        "'Secret' is private to module 'round'",
        2,
        9,
    );
    assert_error(
        &[
            (
                "main.stn",
                "use round\nfn f[T: round.Round](x: T):\n    ret\n",
            ),
            ROUND,
        ],
        "main.stn",
        "module 'round' has no trait 'Round'",
        2,
        9,
    );
}

#[test]
fn an_impl_of_a_trait_is_in_the_file_of_the_trait_or_the_type() {
    // the trait's file may implement it for another file's struct
    let body = linked_body(&[
        ("main.stn", "use named\n"),
        GEO,
        (
            "named.stn",
            "use geo.Point\npub trait Named:\n    fn name(self) -> str\nimpl Named for Point:\n    \
             fn name(self) -> str:\n        ret \"p\"\n",
        ),
    ]);
    let impls: Vec<(String, String)> = body
        .iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::ImplDef {
                name,
                trait_name: Some(trait_name),
                ..
            } => Some((trait_name.name.clone(), name.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(
        impls,
        [("named.Named".to_string(), "geo.Point".to_string())]
    );

    assert_error(
        &[
            (
                "main.stn",
                "use geo.Point\nuse round.Shape\nimpl Shape for Point:\n    \
                 fn area(self) -> int:\n        ret 1\n",
            ),
            GEO,
            ROUND,
        ],
        "main.stn",
        "'Shape' and 'Point' are both defined in other files, so their impl block goes in the \
         file of either",
        3,
        16,
    );
    assert_error(
        &[(
            "main.stn",
            "trait Shape:\n    fn area(self) -> int\nimpl Shape for Point:\n    \
             fn area(self) -> int:\n        ret 1\n",
        )],
        "main.stn",
        "no struct named 'Point'",
        3,
        16,
    );
    assert_error(
        &[(
            "main.stn",
            "enum E:\n    A\ntrait Shape:\n    fn area(self) -> int\nimpl Shape for E:\n    \
             fn area(self) -> int:\n        ret 1\n",
        )],
        "main.stn",
        "'E' is an enum, and only a struct has an impl block for now",
        5,
        16,
    );
}

#[test]
fn a_default_method_cannot_share_its_linked_name_with_a_module_function() {
    assert_error(
        &[
            ("main.stn", "use round\nuse round.Shape.twice\n"),
            ROUND,
            ("round/Shape.stn", "pub fn twice() -> int:\n    ret 1\n"),
        ],
        "round/Shape.stn",
        "'round.Shape.twice' is both a function of this module and a method of 'Shape' in \
         module 'round'",
        1,
        8,
    );
}

#[test]
fn no_module_can_be_named_like_the_prelude() {
    assert_error(
        &[
            ("main.stn", "use prelude\n"),
            ("prelude.stn", "pub fn sort() -> int:\n    ret 1\n"),
        ],
        "main.stn",
        "'prelude' is the name of stone's prelude, which every program has, so rename \
         prelude.stn",
        1,
        5,
    );
}

#[test]
fn the_preludes_variants_are_written_alone() {
    assert_eq!(
        output(&[(
            "main.stn",
            "o = less\nmatch o:\n    case greater:\n        print(1)\n    case _:\n        \
             print(o, equal)\n",
        )]),
        "less equal\n"
    );
    assert_error(
        &[("main.stn", "o = ordering.less\n")],
        "main.stn",
        "write 'less', 'equal', or 'greater' without 'ordering.'",
        1,
        5,
    );
}

#[test]
fn result_variants_are_written_alone() {
    assert_eq!(
        output(&[(
            "main.stn",
            "r = ok(1)\nmatch r:\n    case ok(v):\n        print(v, err(\"no\"))\n    case err(e):\n        \
             print(e)\n",
        )]),
        "1 err('no')\n"
    );
    assert_error(
        &[("main.stn", "r = result.ok(1)\n")],
        "main.stn",
        "write 'ok(x)' or 'err(e)' without 'result.'",
        1,
        5,
    );
}

#[test]
fn net_is_a_module_of_stone_source_that_any_program_can_use() {
    assert_eq!(
        output(&[("main.stn", "use net\nprint(net.connect)\n")]),
        "fn(str, int) -> result[socket, str]\n"
    );
    assert_eq!(
        output(&[("main.stn", "use net.listen\nprint(listen)\n")]),
        "fn(str, int) -> result[listener, str]\n"
    );
}

#[test]
fn a_file_cannot_take_the_net_modules_name() {
    assert_error(
        &[
            ("main.stn", "use net\n"),
            ("net.stn", "pub fn f() -> int:\n    ret 0\n"),
        ],
        "main.stn",
        "'net' is a builtin module, so rename net.stn",
        1,
        5,
    );
}

#[test]
fn only_stones_own_modules_can_use_net_sys() {
    assert_error(
        &[("main.stn", "use net_sys\n")],
        "main.stn",
        "no module named 'net_sys'",
        1,
        5,
    );
}

#[test]
fn only_stones_own_modules_can_use_its_internal_modules_written_in_stone() {
    for module in ["gui_x11", "gui_wire"] {
        assert_error(
            &[("main.stn", &format!("use {module}\n"))],
            "main.stn",
            &format!("no module named '{module}'"),
            1,
            5,
        );
    }
    // gui uses them itself
    assert_eq!(
        output(&[("main.stn", "use gui\nprint(gui.rgb(1, 2, 3))\n")]),
        "66051\n"
    );
}

/// Returns the name of every function the linked module defines from `files`, outside the
/// prelude, in order.
fn defined_functions(files: &[(&str, &str)]) -> Vec<String> {
    let (entry, source) = files[0];
    let linked = link(Path::new(entry), source, &sources_of(&files[1..]));
    assert_eq!(linked.diagnostics, []);
    let Mod::Module { body } = &linked.module;
    body.iter()
        .filter(|stmt| stmt.span.file != FileId::PRELUDE)
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef { name, .. } => Some(name.clone()),
            _ => None,
        })
        .collect()
}

#[test]
fn a_lambda_becomes_a_function_before_its_statement() {
    let (_, result) = driver::load(
        Path::new("main.stn"),
        "x = 1\nys = [1].map(|x| x + 1)\n",
        &MapSources::default(),
    );
    let checked = result.unwrap();
    let Mod::Module { body } = &checked.module;
    let user: Vec<&Stmt> = body
        .iter()
        .filter(|stmt| stmt.span.file != FileId::PRELUDE)
        .collect();
    let StmtKind::FunctionDef { name, body, .. } = &user[1].kind else {
        panic!(
            "expected the lambda's function second, got {:?}",
            user[1].kind
        );
    };
    assert_eq!(name, "lambda.2.14");
    assert!(matches!(body[0].kind, StmtKind::Return { value: Some(_) }));
    let StmtKind::Assign { value, .. } = &user[2].kind else {
        panic!("expected the assignment last");
    };
    let ExprKind::Call { args, .. } = &value.kind else {
        panic!("expected a call");
    };
    assert!(matches!(&args[0].kind, ExprKind::Name { id, .. } if id == "lambda.2.14"));
}

#[test]
fn lambdas_inside_lambdas_and_functions_are_lifted_too() {
    assert_eq!(
        defined_functions(&[(
            "main.stn",
            "fn f(xs: list[list[int]]) -> list[list[int]]:\n    ret xs.map(|row| row.map(|x| x * 2))\n"
        )]),
        ["lambda.2.30", "lambda.2.16", "f"]
    );
}

#[test]
fn a_library_lambda_is_named_by_its_module() {
    assert_eq!(
        output(&[
            ("main.stn", "use util\nprint(util.doubled([1, 2]))\n"),
            (
                "util.stn",
                "pub fn doubled(xs: list[int]) -> list[int]:\n    ret xs.map(|x| x * 2)\n"
            ),
        ]),
        "[2, 4]\n"
    );
    assert!(
        defined_functions(&[
            ("main.stn", "use util\nprint(util.doubled([1, 2]))\n"),
            (
                "util.stn",
                "pub fn doubled(xs: list[int]) -> list[int]:\n    ret xs.map(|x| x * 2)\n"
            ),
        ])
        .contains(&"util.lambda.2.16".to_string())
    );
}

const CONV: (&str, &str) = (
    "conv.stn",
    "pub struct Point:\n    x: int\npub trait scale[R]:\n    fn scale(self, by: R) -> Self\n\
     impl scale[int] for Point:\n    fn scale(self, by: int) -> Self:\n        \
     ret Point(x=self.x * by)\nimpl scale[Point] for Point:\n    \
     fn scale(self, by: Point) -> Self:\n        ret Point(x=self.x * by.x)\n\
     pub trait make[T]:\n    fn make(self) -> T\n",
);

/// Returns the type arguments of each trait an `impl` block of `body` implements, as written
/// after linking, in order.
fn impl_args(body: &[Stmt]) -> Vec<Vec<TypeExprKind>> {
    body.iter()
        .filter_map(|stmt| match &stmt.kind {
            StmtKind::ImplDef {
                trait_name: Some(trait_name),
                ..
            } => Some(trait_name.args.iter().map(|a| a.kind.clone()).collect()),
            _ => None,
        })
        .collect()
}

fn named(name: &str) -> TypeExprKind {
    TypeExprKind::Named {
        name: name.to_string(),
        args: vec![],
    }
}

#[test]
fn a_generic_trait_names_its_arguments_by_their_linked_names() {
    let body = linked_body(&[
        (
            "main.stn",
            "use conv\nuse conv.Point\nfn f[T: conv.scale[Point]](x: T) -> T:\n    ret x\n",
        ),
        CONV,
    ]);
    assert_eq!(
        impl_args(&body),
        [vec![TypeExprKind::Int], vec![named("conv.Point")]]
    );
    let bound = body
        .iter()
        .find_map(|stmt| match &stmt.kind {
            StmtKind::FunctionDef {
                name, type_params, ..
            } if name == "f" => Some(type_params[0].bounds[0].clone()),
            _ => None,
        })
        .expect("expected the function");
    assert_eq!(bound.name, "conv.scale");
    assert_eq!(
        bound
            .args
            .iter()
            .map(|a| a.kind.clone())
            .collect::<Vec<_>>(),
        [named("conv.Point")]
    );
    // the trait's own parameter is never renamed
    let Some(StmtKind::TraitDef { methods, .. }) = body
        .iter()
        .map(|stmt| &stmt.kind)
        .find(|kind| matches!(kind, StmtKind::TraitDef { name, .. } if name == "conv.scale"))
    else {
        panic!("expected the trait");
    };
    let StmtKind::FunctionDef { args, .. } = &methods[0].kind else {
        panic!("expected a method");
    };
    assert_eq!(
        args.args[1].annotation.as_ref().map(|a| a.kind.clone()),
        Some(named("R"))
    );
}

#[test]
fn an_impl_of_another_files_trait_for_a_builtin_type_may_go_beside_a_struct_it_names() {
    let body = linked_body(&[
        (
            "main.stn",
            "use conv.make\nstruct C:\n    d: int\nimpl make[C] for int:\n    \
             fn make(self) -> C:\n        ret C(d=self)\nimpl make[list[C]] for str:\n    \
             fn make(self) -> list[C]:\n        ret []\n",
        ),
        CONV,
    ]);
    // conv's own two blocks come first
    assert_eq!(
        impl_args(&body)[2..],
        [
            vec![named("C")],
            vec![TypeExprKind::List(Box::new(TypeExpr {
                kind: named("C"),
                span: Span::new(Pos { line: 7, col: 16 }, Pos { line: 7, col: 17 }),
            }))]
        ]
    );
    assert_error(
        &[
            (
                "main.stn",
                "use conv.make\nimpl make[bool] for int:\n    fn make(self) -> bool:\n        \
                 ret true\n",
            ),
            CONV,
        ],
        "main.stn",
        "'make' and 'int' are both defined in other files, so their impl block goes in the \
         file of either",
        2,
        21,
    );
}

const SHADES: (&str, &str) = (
    "shades.stn",
    "struct pair:\n    a: int\n\npub fn sum(n: int) -> int:\n    color = pair(a=n)\n    \
     point = |x: int| x + 1\n    ret point(color.a)\n",
);

#[test]
fn a_library_can_name_a_variable_like_a_type_of_the_entry_file() {
    let main = (
        "main.stn",
        "use shades\n\nenum color:\n    Red\n\nstruct point:\n    x: int\n\n\
         print(shades.sum(2), color.Red, point(x=1))\n",
    );
    assert_eq!(output(&[main, SHADES]), "3 color.Red point(x=1)\n");
}

#[test]
fn a_library_cannot_name_a_type_of_the_entry_file() {
    let main = (
        "main.stn",
        "use shapes\n\nstruct point:\n    x: int\n\nprint(1)\n",
    );
    let shapes = ("shapes.stn", "pub fn f(p: point) -> int:\n    ret 1\n");
    assert_error(
        &[main, shapes],
        "shapes.stn",
        "there is no type 'point'",
        1,
        13,
    );
}

#[test]
fn a_library_can_name_a_type_parameter_like_a_type_of_the_entry_file() {
    let main = (
        "main.stn",
        "use shapes\n\nstruct point:\n    x: int\n\nprint(shapes.first([1, 2]))\n",
    );
    let shapes = (
        "shapes.stn",
        "pub fn first[point](xs: list[point]) -> point:\n    ret xs[0]\n",
    );
    assert_eq!(output(&[main, shapes]), "1\n");
}

#[test]
fn a_type_of_the_entry_file_is_no_type_in_a_builtin_module() {
    let main = (
        "main.stn",
        "use net\n\nstruct address:\n    x: int\n\nprint(net.resolve(\"localhost\").is_ok())\n",
    );
    let (_, result) = driver::load(Path::new(main.0), main.1, &sources_of(&[]));
    assert!(result.is_ok());
}

const ENTRY_TRAIT: (&str, &str) = (
    "main.stn",
    "use shapes\n\ntrait show:\n    fn show(self) -> str\n\nstruct point:\n    x: int\n\nprint(1)\n",
);

#[test]
fn a_library_cannot_bound_by_a_trait_of_the_entry_file() {
    let shapes = ("shapes.stn", "pub fn f[T: show](x: T) -> int:\n    ret 1\n");
    assert_error(
        &[ENTRY_TRAIT, shapes],
        "shapes.stn",
        "there is no trait 'show'",
        1,
        13,
    );
    let shapes = (
        "shapes.stn",
        "pub fn f[T: point](x: T) -> int:\n    ret 1\n",
    );
    assert_error(
        &[ENTRY_TRAIT, shapes],
        "shapes.stn",
        "there is no trait 'point'",
        1,
        13,
    );
}

#[test]
fn a_library_cannot_implement_a_trait_of_the_entry_file() {
    let shapes = (
        "shapes.stn",
        "struct pair:\n    a: int\n\nimpl show for pair:\n    fn show(self) -> str:\n        ret \"\"\n",
    );
    assert_error(
        &[ENTRY_TRAIT, shapes],
        "shapes.stn",
        "there is no trait 'show'",
        4,
        6,
    );
}

#[test]
fn a_library_cannot_name_a_trait_of_the_entry_file_as_a_type() {
    let shapes = ("shapes.stn", "pub fn f(x: show) -> int:\n    ret 1\n");
    assert_error(
        &[ENTRY_TRAIT, shapes],
        "shapes.stn",
        "there is no type 'show'",
        1,
        13,
    );
}
