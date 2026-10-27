//! Formats arbitrary text, which must give the same layout when formatted again, or an error,
//! but never panic. `stone::format::format` itself checks that formatting changes no token.

#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|source: &str| {
    if let Ok(formatted) = stone::format::format(source) {
        assert_eq!(stone::format::format(&formatted).as_ref(), Ok(&formatted));
    }
});
