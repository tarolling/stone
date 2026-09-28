//! Lexes and parses arbitrary text, which must produce a module or an error but never panic,
//! overflow the stack, or take exponential time.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|source: &str| {
    let _ = stone::driver::parse(source);
});
