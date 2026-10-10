//! Feeds grammar-generated programs, which reach deeper than raw bytes usually do, through the
//! parser, interpreter, and the code generator of every target, then the built-in assembler and
//! linkers. Every stage must succeed or return an error.

#![no_main]

use libfuzzer_sys::arbitrary::Unstructured;
use libfuzzer_sys::fuzz_target;
use stone::codegen::{Target, executable};

fuzz_target!(|data: &[u8]| {
    let Ok(source) = stone_fuzz::generate(&mut Unstructured::new(data)) else {
        return;
    };
    let module = stone::driver::parse(&source)
        .unwrap_or_else(|e| panic!("generated program failed to parse ({e}):\n{source}"));
    let _ = stone_fuzz::interpret(&source);
    for target in Target::ALL {
        let text = target.generator().assemble(&module).unwrap_or_else(|e| {
            panic!("generated program failed to compile for {target} ({e}):\n{source}")
        });
        if let Err(e) = executable(&text, target, "out") {
            panic!("generated program failed to assemble for {target} ({e}):\n{source}");
        }
    }
});
