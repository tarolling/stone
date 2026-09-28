//! Lexes arbitrary text, which must produce tokens or a `LexError` but never panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use stone::lexer::Lexer;

fuzz_target!(|source: &str| {
    let _ = Lexer::new(source).lex();
});
