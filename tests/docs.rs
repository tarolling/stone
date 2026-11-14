//! Tests that the documentation site in `docs/` keeps up with the language.
//!
//! For example, adding a builtin to `stdlib::BUILTIN_DOCS`, `stdlib::METHOD_DOCS`, or
//! a builtin module's functions in `stdlib::MODULES` fails these tests until `docs/reference/builtins.md` documents its
//! signature.

use std::path::Path;
use stone::stdlib::{BUILTIN_DOCS, METHOD_DOCS, MODULES};
use stone::token::RESERVED_KEYWORDS;

/// Returns the contents of a file under `docs/`.
fn read_doc(path: &str) -> String {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("docs")
        .join(path);
    std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("{}: {e}", path.display()))
}

#[test]
fn builtins_reference_documents_every_builtin() {
    let page = read_doc("reference/builtins.md");
    let functions = MODULES.iter().flat_map(|module| module.functions);
    for doc in BUILTIN_DOCS.iter().chain(&METHOD_DOCS).chain(functions) {
        assert!(
            page.contains(doc.signature),
            "docs/reference/builtins.md does not mention `{}`",
            doc.signature
        );
    }
}

#[test]
fn builtins_reference_documents_every_function_of_the_modules_written_in_stone() {
    let page = read_doc("reference/builtins.md");
    for (module, source) in &stone::stdlib::SOURCE_MODULES {
        for line in source.lines() {
            let Some(signature) = line.strip_prefix("pub fn ") else {
                continue;
            };
            let signature = format!("{module}.{}", signature.trim_end_matches(':'));
            assert!(
                page.contains(&signature),
                "docs/reference/builtins.md does not mention `{signature}`"
            );
        }
    }
}

#[test]
fn syntax_reference_lists_every_keyword() {
    let page = read_doc("reference/syntax.md");
    for keyword in RESERVED_KEYWORDS {
        assert!(
            page.contains(&format!("`{keyword}`")),
            "docs/reference/syntax.md does not list the keyword `{keyword}`"
        );
    }
}

/// Returns the ```` ```stone ```` blocks of a Markdown page, each with the line it starts on.
///
/// A block indented under a list item loses the fence's indentation, as Markdown renders it. For
/// example, a page holding one block that opens on line 3 gives `[(4, "print(1)\n")]`.
fn stone_blocks(page: &str) -> Vec<(usize, String)> {
    let mut blocks = Vec::new();
    let mut current: Option<(usize, usize, String)> = None;
    for (i, line) in page.lines().enumerate() {
        let indent = line.len() - line.trim_start().len();
        match &mut current {
            None if line.trim() == "```stone" => current = Some((i + 2, indent, String::new())),
            None => {}
            Some((start, _, text)) if line.trim() == "```" => {
                blocks.push((*start, std::mem::take(text)));
                current = None;
            }
            Some((_, fence, text)) => {
                text.push_str(line.get(*fence..).unwrap_or("").trim_end());
                text.push('\n');
            }
        }
    }
    blocks
}

#[test]
fn agent_prompts_have_valid_programs() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/agents");
    let mut pages: Vec<_> = std::fs::read_dir(&dir)
        .unwrap_or_else(|e| panic!("{}: {e}", dir.display()))
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "md"))
        .collect();
    pages.sort();
    assert!(!pages.is_empty(), "docs/agents has no pages");
    let scratch = Path::new(env!("CARGO_TARGET_TMPDIR")).join("agent_prompts");
    std::fs::create_dir_all(&scratch).unwrap();
    for page in pages {
        let name = page.file_stem().unwrap().to_string_lossy().into_owned();
        let blocks = stone_blocks(&std::fs::read_to_string(&page).unwrap());
        assert!(
            !blocks.is_empty(),
            "docs/agents/{name}.md has no stone blocks"
        );
        for (line, source) in blocks {
            let file = scratch.join(format!("{name}_{line}.stn"));
            std::fs::write(&file, &source).unwrap();
            for command in ["check", "run"] {
                let output = std::process::Command::new(env!("CARGO_BIN_EXE_stone"))
                    .arg(command)
                    .arg(&file)
                    // a block that writes files writes them here, not in the repository
                    .current_dir(&scratch)
                    .stdin(std::process::Stdio::null())
                    .output()
                    .expect("stone should run");
                let stderr = String::from_utf8_lossy(&output.stderr);
                // os.run, net, and gui are refused on Windows
                if cfg!(windows) && stderr.contains("is not supported on Windows yet") {
                    break;
                }
                assert!(
                    output.status.success() && !stderr.contains("warning"),
                    "stone {command} failed on the block at docs/agents/{name}.md:{line}:\n{stderr}"
                );
            }
        }
    }
}

#[test]
fn error_codes_page_is_current() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/reference/errors.md");
    let want = stone::explain::markdown();
    if std::env::var_os("STONE_UPDATE_DOCS").is_some() {
        std::fs::write(&path, &want).unwrap();
    }
    let page = std::fs::read_to_string(&path).unwrap_or_default();
    assert!(
        page == want,
        "docs/reference/errors.md is out of date with src/explain.rs; rewrite it with \
         `STONE_UPDATE_DOCS=1 cargo test --test docs error_codes_page_is_current`"
    );
}

/// Writes the files of an example into a new directory `dir`, returning the path of its
/// `main.stn`.
fn write_example(dir: &Path, files: &[(&str, &str)]) -> std::path::PathBuf {
    let _ = std::fs::remove_dir_all(dir);
    for (path, source) in files {
        let path = dir.join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(&path, source).unwrap();
    }
    dir.join("main.stn")
}

/// Checks the example whose entry is `main`, or builds it for `target`, returning whether it
/// passed and what stone wrote to stderr.
fn check_example(main: &Path, target: Option<&str>) -> (bool, String) {
    let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_stone"));
    match target {
        None => command.arg("check").arg("--offline").arg(main),
        Some(target) => command
            .arg("build")
            .arg("--offline")
            .arg(main)
            .arg("--target")
            .arg(target)
            .arg("-o")
            .arg(main.with_file_name("built")),
    };
    let output = command
        .current_dir(main.parent().unwrap())
        .stdin(std::process::Stdio::null())
        .output()
        .expect("stone should run");
    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
    (output.status.success(), stderr)
}

#[test]
fn every_error_code_has_examples_that_behave() {
    let scratch = Path::new(env!("CARGO_TARGET_TMPDIR")).join("error_codes");
    for code in stone::explain::Code::ALL {
        let explanation = code.explanation();
        let id = explanation.id;
        assert!(!explanation.text.is_empty(), "{id} has no explanation");
        assert!(
            !explanation.failing.is_empty(),
            "{id} has no failing example"
        );
        assert!(!explanation.fixed.is_empty(), "{id} has no fixed example");
        let main = write_example(&scratch.join(id).join("failing"), explanation.failing);
        let (passed, stderr) = check_example(&main, explanation.target);
        let first = stderr.lines().find(|line| line.contains(": error"));
        assert!(
            !passed && first.is_some_and(|line| line.contains(&format!("error[{id}]: "))),
            "{id}'s failing example should fail with {id} first, but stone wrote:\n{stderr}"
        );
        let main = write_example(&scratch.join(id).join("fixed"), explanation.fixed);
        let (passed, stderr) = check_example(&main, explanation.target);
        assert!(
            passed && !stderr.contains("warning"),
            "{id}'s fixed example should pass with no warning, but stone wrote:\n{stderr}"
        );
    }
}

#[test]
fn lints_page_is_current() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("docs/reference/lints.md");
    let want = stone::explain::lints_markdown();
    if std::env::var_os("STONE_UPDATE_DOCS").is_some() {
        std::fs::write(&path, &want).unwrap();
    }
    let page = std::fs::read_to_string(&path).unwrap_or_default();
    assert!(
        page == want,
        "docs/reference/lints.md is out of date with src/explain.rs; rewrite it with \
         `STONE_UPDATE_DOCS=1 cargo test --test docs lints_page_is_current`"
    );
}

#[test]
fn every_lint_has_examples_that_behave() {
    let scratch = Path::new(env!("CARGO_TARGET_TMPDIR")).join("lints");
    for lint in stone::explain::Lint::ALL {
        let explanation = lint.explanation();
        let id = explanation.id;
        assert!(!explanation.text.is_empty(), "{id} has no explanation");
        let main = write_example(&scratch.join(id).join("failing"), explanation.failing);
        let (passed, stderr) = check_example(&main, None);
        let first = stderr.lines().find(|line| line.contains(": warning"));
        assert!(
            passed && first.is_some_and(|line| line.contains(&format!("warning[{id}]: "))),
            "{id}'s failing example should warn with {id} first, but stone wrote:\n{stderr}"
        );
        let main = write_example(&scratch.join(id).join("fixed"), explanation.fixed);
        let (passed, stderr) = check_example(&main, None);
        assert!(
            passed && !stderr.contains("warning"),
            "{id}'s fixed example should pass with no warning, but stone wrote:\n{stderr}"
        );
    }
}
