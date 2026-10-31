//! Checks that every stone file in the repository is in the layout `stone fmt` writes, so the
//! formatter's rules and the code written by hand never drift apart.

use std::path::Path;
use stone::driver::stone_files;
use stone::format::format;

/// The directories whose `.stn` files must be formatted.
const DIRECTORIES: [&str; 5] = [
    "examples",
    "tests/programs",
    "docs/examples",
    "src",
    "bench",
];

/// Files left as they are on purpose.
const UNFORMATTED: [&str; 1] = [
    // indents comments oddly to test that the lexer ignores where they are
    "tests/programs/comments.stn",
];

#[test]
fn every_stone_file_is_formatted() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut unformatted = vec![];
    let mut count = 0;
    for dir in DIRECTORIES {
        for file in stone_files(&root.join(dir)).unwrap() {
            // windows writes its own separator
            let name = file
                .strip_prefix(root)
                .unwrap()
                .display()
                .to_string()
                .replace('\\', "/");
            if UNFORMATTED.contains(&name.as_str()) {
                continue;
            }
            count += 1;
            let source = std::fs::read_to_string(&file).unwrap();
            match format(&source) {
                Ok(formatted) if formatted == source => {}
                Ok(_) => unformatted.push(name),
                Err(e) => unformatted.push(format!("{name}: {e}")),
            }
        }
    }
    assert!(count > 300, "only found {count} files");
    assert!(
        unformatted.is_empty(),
        "run `cargo run -- fmt` on these:\n{}",
        unformatted.join("\n")
    );
}
