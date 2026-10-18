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
                    .stdin(std::process::Stdio::null())
                    .output()
                    .expect("stone should run");
                let stderr = String::from_utf8_lossy(&output.stderr);
                assert!(
                    output.status.success() && !stderr.contains("warning"),
                    "stone {command} failed on the block at docs/agents/{name}.md:{line}:\n{stderr}"
                );
            }
        }
    }
}
