use super::*;
use serde_json::{Value, json};
use std::path::{Path, PathBuf};

/// Runs the server over `requests`, one JSON message per line, and returns its replies.
fn exchange(requests: &[Value]) -> Vec<Value> {
    let input: String = requests.iter().map(|r| format!("{r}\n")).collect();
    let mut output = vec![];
    serve(input.as_bytes(), &mut output).unwrap();
    String::from_utf8(output)
        .unwrap()
        .lines()
        .map(|line| serde_json::from_str(line).unwrap())
        .collect()
}

fn request(id: u64, method: &str, params: Value) -> Value {
    json!({ "jsonrpc": "2.0", "id": id, "method": method, "params": params })
}

/// Makes a fresh directory holding `files`, as (path, text) pairs.
fn project(name: &str, files: &[(&str, &str)]) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("stone-mcp-test-{}-{name}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for (path, text) in files {
        let path = dir.join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }
    dir.canonicalize().unwrap()
}

/// Calls tool `name` from `dir`, returning its text and whether it is an error.
fn call_in(dir: &Path, name: &str, args: Value) -> (String, bool) {
    let mut context = Context::new(dir.to_path_buf());
    // enough for every snippet here, and quick to run out of in a loop that never ends
    context.fuel = 200_000;
    let result = tools::call(name, &args, &context).expect("the tool should exist");
    (result.text, result.is_error)
}

fn ok(dir: &Path, name: &str, args: Value) -> String {
    let (text, is_error) = call_in(dir, name, args);
    assert!(!is_error, "{name} failed: {text}");
    text
}

const MAIN: &str = "\
use util
fn add(a: int, b: int) -> int:
    ret a + b
total = add(1, 2)
print(util.twice(total))
";

const UTIL: &str = "\
pub fn twice(n: int) -> int:
    ret n * 2
";

#[test]
fn initialize_answers_with_tools_and_instructions() {
    let replies = exchange(&[request(
        1,
        "initialize",
        json!({ "protocolVersion": "2025-06-18", "capabilities": {}, "clientInfo": { "name": "t", "version": "1" } }),
    )]);
    assert_eq!(replies.len(), 1);
    let result = &replies[0]["result"];
    assert_eq!(replies[0]["id"], 1);
    assert_eq!(result["protocolVersion"], "2025-06-18");
    assert_eq!(result["serverInfo"]["name"], "stone");
    assert!(result["capabilities"]["tools"].is_object());
    assert!(
        result["instructions"]
            .as_str()
            .unwrap()
            .contains("stone_guide")
    );
}

#[test]
fn initialize_offers_its_own_version_for_one_it_does_not_know() {
    let replies = exchange(&[request(
        1,
        "initialize",
        json!({ "protocolVersion": "1999-01-01", "capabilities": {} }),
    )]);
    assert_eq!(replies[0]["result"]["protocolVersion"], PROTOCOL_VERSION);
}

#[test]
fn notifications_get_no_reply_and_unknown_methods_an_error() {
    let replies = exchange(&[
        json!({ "jsonrpc": "2.0", "method": "notifications/initialized" }),
        request(2, "ping", json!({})),
        request(3, "resources/list", json!({})),
    ]);
    assert_eq!(replies.len(), 2);
    assert_eq!(replies[0]["id"], 2);
    assert_eq!(replies[0]["result"], json!({}));
    assert_eq!(replies[1]["id"], 3);
    assert_eq!(replies[1]["error"]["code"], -32601);
}

#[test]
fn a_line_that_is_not_json_is_a_parse_error() {
    let mut output = vec![];
    serve("{oops\n".as_bytes(), &mut output).unwrap();
    let reply: Value = serde_json::from_slice(&output).unwrap();
    assert_eq!(reply["error"]["code"], -32700);
    assert_eq!(reply["id"], Value::Null);
}

#[test]
fn tools_list_names_every_tool_with_a_schema() {
    let replies = exchange(&[request(1, "tools/list", json!({}))]);
    let tools = replies[0]["result"]["tools"].as_array().unwrap();
    let names: Vec<&str> = tools.iter().map(|t| t["name"].as_str().unwrap()).collect();
    assert_eq!(
        names,
        [
            "stone_guide",
            "stone_diagnostics",
            "stone_check_snippet",
            "stone_run_snippet",
            "stone_hover",
            "stone_definition",
            "stone_references",
            "stone_symbols",
            "stone_rename",
            "stone_docs",
        ]
    );
    for tool in tools {
        assert_eq!(tool["inputSchema"]["type"], "object", "{}", tool["name"]);
        assert!(!tool["description"].as_str().unwrap().is_empty());
    }
}

#[test]
fn tools_call_wraps_the_text_as_content() {
    let replies = exchange(&[request(
        1,
        "tools/call",
        json!({ "name": "stone_check_snippet", "arguments": { "code": "print(1)\n" } }),
    )]);
    let result = &replies[0]["result"];
    assert_eq!(result["isError"], false);
    assert_eq!(result["content"][0]["type"], "text");
    assert_eq!(result["content"][0]["text"], "no problems found");
}

#[test]
fn an_unknown_tool_is_an_invalid_params_error() {
    let replies = exchange(&[request(
        1,
        "tools/call",
        json!({ "name": "stone_nothing", "arguments": {} }),
    )]);
    assert_eq!(replies[0]["error"]["code"], -32602);
}

#[test]
fn a_missing_argument_is_a_tool_error() {
    let dir = project("missing", &[]);
    let (text, is_error) = call_in(&dir, "stone_hover", json!({ "line": 1 }));
    assert!(is_error);
    assert_eq!(text, "missing argument 'path'");
}

#[test]
fn diagnostics_render_as_stone_check_does() {
    let dir = project(
        "diagnostics",
        &[("main.stn", "x = 1\nx = \"s\"\nprint(x)\n")],
    );
    let text = ok(&dir, "stone_diagnostics", json!({ "path": "main.stn" }));
    assert_eq!(
        text,
        "main.stn:2:5: error: cannot assign str to 'x', which is int\n  |\n\
         1 | x = 1\n  | - 'x' is first assigned here\n\
         2 | x = \"s\"\n  |     ^^^\n\n1 error in 1 file"
    );
}

#[test]
fn diagnostics_of_a_directory_cover_every_file_with_relative_paths() {
    let dir = project(
        "diagnostics_dir",
        &[
            ("main.stn", "use util\nprint(util.twice(1))\n"),
            (
                "util.stn",
                "pub fn twice(n: int) -> int:\n    ret n * \"2\"\n",
            ),
        ],
    );
    let text = ok(&dir, "stone_diagnostics", json!({ "path": "." }));
    assert!(
        text.starts_with("util.stn:2:13: error: expected int, found str"),
        "{text}"
    );
    assert!(text.ends_with("1 error in 2 files"), "{text}");
}

#[test]
fn a_clean_file_has_no_problems() {
    let dir = project("clean", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let text = ok(&dir, "stone_diagnostics", json!({ "path": "main.stn" }));
    assert_eq!(text, "no problems found in 1 file");
}

#[test]
fn check_snippet_reports_errors() {
    let dir = project("check_snippet", &[]);
    let (text, is_error) = call_in(
        &dir,
        "stone_check_snippet",
        json!({ "code": "fn f(a: int) -> int:\n    ret a\nprint(f(\"s\"))\n" }),
    );
    assert!(!is_error);
    assert!(text.starts_with("main.stn:3:9: error:"), "{text}");
}

#[test]
fn run_snippet_captures_output_and_errors() {
    let dir = project("run_snippet", &[]);
    let text = ok(
        &dir,
        "stone_run_snippet",
        json!({ "code": "print(\"hi\")\neprint(\"oops\")\nxs = [1]\nprint(xs[3])\n" }),
    );
    assert_eq!(
        text,
        "stdout:\nhi\n\nstderr:\noops\n\n\
         main.stn:4:7: error: list index out of range (index 3, length 1)\n  |\n\
         4 | print(xs[3])\n  |       ^^^^^"
    );
}

#[test]
fn run_snippet_reads_stdin_and_args() {
    let dir = project("run_snippet_io", &[]);
    let text = ok(
        &dir,
        "stone_run_snippet",
        json!({
            "code": "print(input().unwrap(), args())\n",
            "stdin": "typed\n",
            "args": ["a", "b"],
        }),
    );
    assert_eq!(text, "stdout:\ntyped ['a', 'b']\n\nexited successfully");
}

#[test]
fn run_snippet_stops_a_loop_that_never_ends() {
    let dir = project("run_snippet_loop", &[]);
    let text = ok(
        &dir,
        "stone_run_snippet",
        json!({ "code": "x = 0\nwhile true:\n    x += 1\n" }),
    );
    assert!(text.contains("ran out of fuel"), "{text}");
}

#[test]
fn run_snippet_does_not_run_a_program_with_errors() {
    let dir = project("run_snippet_bad", &[]);
    let text = ok(
        &dir,
        "stone_run_snippet",
        json!({ "code": "print(1 + \"s\")\n" }),
    );
    assert!(
        text.starts_with("the program did not pass the checker:\n"),
        "{text}"
    );
}

#[test]
fn run_snippet_reports_an_exit_status() {
    let dir = project("run_snippet_exit", &[]);
    let text = ok(
        &dir,
        "stone_run_snippet",
        json!({ "code": "use os\nos.exit(3)\n" }),
    );
    assert_eq!(text, "exited with status 3");
}

#[test]
fn hover_finds_a_symbol_on_a_line_or_a_column() {
    let dir = project("hover", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let by_symbol = ok(
        &dir,
        "stone_hover",
        json!({ "path": "main.stn", "line": 4, "symbol": "add" }),
    );
    let by_column = ok(
        &dir,
        "stone_hover",
        json!({ "path": "main.stn", "line": 4, "column": 9 }),
    );
    assert_eq!(by_symbol, by_column);
    assert!(
        by_symbol.contains("fn add(a: int, b: int) -> int"),
        "{by_symbol}"
    );
}

#[test]
fn a_symbol_not_on_its_line_is_an_error_showing_the_line() {
    let dir = project("hover_missing", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let (text, is_error) = call_in(
        &dir,
        "stone_hover",
        json!({ "path": "main.stn", "line": 4, "symbol": "ad" }),
    );
    assert!(is_error);
    assert_eq!(text, "'ad' is not on line 4, which is: total = add(1, 2)");
}

#[test]
fn definition_goes_to_another_file() {
    let dir = project("definition", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let text = ok(
        &dir,
        "stone_definition",
        json!({ "path": "main.stn", "line": 5, "symbol": "twice" }),
    );
    assert_eq!(text, "util.stn:1:8: pub fn twice(n: int) -> int:");
}

#[test]
fn references_list_every_use() {
    let dir = project("references", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let text = ok(
        &dir,
        "stone_references",
        json!({ "path": "util.stn", "line": 1, "symbol": "twice" }),
    );
    assert_eq!(
        text,
        "main.stn:5:12: print(util.twice(total))\nutil.stn:1:8: pub fn twice(n: int) -> int:"
    );
    let without = ok(
        &dir,
        "stone_references",
        json!({ "path": "util.stn", "line": 1, "symbol": "twice", "include_declaration": false }),
    );
    assert_eq!(without, "main.stn:5:12: print(util.twice(total))");
}

#[test]
fn symbols_outline_a_file() {
    let dir = project(
        "symbols",
        &[(
            "main.stn",
            "struct Point:\n    x: float\nfn origin() -> Point:\n    ret Point(x=0.0)\nprint(origin())\n",
        )],
    );
    let text = ok(&dir, "stone_symbols", json!({ "path": "main.stn" }));
    assert_eq!(
        text,
        "struct Point (line 1)\n  Point.x: float (line 2)\nfn origin() -> Point (line 3)"
    );
}

#[test]
fn rename_rewrites_every_file() {
    let dir = project("rename", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let text = ok(
        &dir,
        "stone_rename",
        json!({ "path": "main.stn", "line": 5, "symbol": "twice", "new_name": "double" }),
    );
    assert_eq!(
        text,
        "renamed to 'double': 1 change in main.stn, 1 change in util.stn"
    );
    let main = std::fs::read_to_string(dir.join("main.stn")).unwrap();
    let util = std::fs::read_to_string(dir.join("util.stn")).unwrap();
    assert!(main.contains("util.double(total)"), "{main}");
    assert!(util.starts_with("pub fn double(n: int)"), "{util}");
}

#[test]
fn rename_to_a_keyword_changes_nothing() {
    let dir = project("rename_keyword", &[("main.stn", MAIN), ("util.stn", UTIL)]);
    let (text, is_error) = call_in(
        &dir,
        "stone_rename",
        json!({ "path": "main.stn", "line": 2, "symbol": "add", "new_name": "while" }),
    );
    assert!(is_error);
    assert_eq!(text, "'while' is a keyword");
    assert_eq!(std::fs::read_to_string(dir.join("main.stn")).unwrap(), MAIN);
}

#[test]
fn docs_find_builtins_methods_types_and_module_functions() {
    let dir = project("docs", &[]);
    let print = ok(&dir, "stone_docs", json!({ "name": "print" }));
    assert!(print.starts_with("print(values...)\n\nPrints"), "{print}");
    let split = ok(&dir, "stone_docs", json!({ "name": "split" }));
    assert!(split.contains(".split("), "{split}");
    let sqrt = ok(&dir, "stone_docs", json!({ "name": "math.sqrt" }));
    assert!(sqrt.contains("sqrt("), "{sqrt}");
    let module = ok(&dir, "stone_docs", json!({ "name": "math" }));
    assert!(module.contains("math.sqrt"), "{module}");
    let ty = ok(&dir, "stone_docs", json!({ "name": "int" }));
    assert!(!ty.is_empty());
}

#[test]
fn docs_of_an_unknown_name_suggest_what_exists() {
    let dir = project("docs_unknown", &[]);
    let (text, is_error) = call_in(&dir, "stone_docs", json!({ "name": "printf" }));
    assert!(is_error);
    assert!(text.starts_with("no builtin named 'printf'"), "{text}");
    assert!(text.contains("print"), "{text}");
}

#[test]
fn guide_is_the_agent_guide() {
    let dir = project("guide", &[]);
    let text = ok(&dir, "stone_guide", json!({}));
    assert_eq!(text, include_str!("../../../docs/agents/stone.md"));
}
