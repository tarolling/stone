//! Feeds grammar-generated programs, which reach deeper than raw bytes usually do, through the
//! parser, interpreter, and both code generators. Every stage must succeed or return an error.

#![no_main]

use libfuzzer_sys::arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use stone::codegen::Architecture;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = stone_fuzz::generate(&mut Unstructured::new(data)) else {
        return;
    };
    let module = stone::driver::parse(&source)
        .unwrap_or_else(|e| panic!("generated program failed to parse ({e}):\n{source}"));
    let _ = stone_fuzz::interpret(&source);
    for arch in Architecture::ALL {
        if let Err(e) = arch.generator().assemble(&module) {
            panic!("generated program failed to assemble for {arch} ({e}):\n{source}");
        }
    }
});
