//! Interprets arbitrary text under small limits, discarding output. Every input must finish with
//! `Ok` or `Err` without panicking or hanging.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|source: &str| {
    let _ = stone::driver::interpret_with(source, &mut std::io::sink(), stone_fuzz::LIMITS);
});
