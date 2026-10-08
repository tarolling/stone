//! Generates x86-64 and arm64 assembly for any text that parses, without invoking gcc. Code
//! generation must succeed or return an error, never panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use stone::codegen::Architecture;

fuzz_target!(|source: &str| {
    if let Ok(module) = stone::driver::parse(source) {
        for arch in Architecture::ALL {
            let _ = arch.generator().assemble(&module);
        }
    }
});
