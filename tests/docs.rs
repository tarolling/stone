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
