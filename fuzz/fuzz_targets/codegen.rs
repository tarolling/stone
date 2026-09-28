//! Generates x86-64 assembly for any text that parses, without invoking gcc. Code generation must
//! succeed or return an error, never panic.

#![no_main]

use libfuzzer_sys::fuzz_target;
use stone::codegen::x64::X64Generator;

fuzz_target!(|source: &str| {
    if let Ok(module) = stone::driver::parse(source) {
        let _ = X64Generator::new().assemble(&module);
    }
});
