//! Feeds grammar-generated programs, which reach deeper than raw bytes usually do, through the
//! parser, interpreter, and code generator. Every stage must succeed or return an error.

#![no_main]

use libfuzzer_sys::arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use stone::codegen::x64::X64Generator;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = stone_fuzz::generate(&mut Unstructured::new(data)) else {
        return;
    };
    let module = stone::driver::parse(&source)
        .unwrap_or_else(|e| panic!("generated program failed to parse ({e}):\n{source}"));
    let _ = stone_fuzz::interpret(&source);
    if let Err(e) = X64Generator::new().assemble(&module) {
        panic!("generated program failed to assemble ({e}):\n{source}");
    }
});
