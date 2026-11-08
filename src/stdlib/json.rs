//! The builtin `json` module: reading and writing JSON text.
//!
//! Like `net`, it is written in stone, in `json.stn`, which a program that says `use json` is
//! linked with like a module file of its own, so both backends parse and write alike. It needs
//! no system calls, so it has no internal `_sys` module.

/// The name a program imports the module by, as in `use json`.
pub const MODULE: &str = "json";

/// The module's stone source, which `use json` links with the program.
pub const SOURCE: &str = include_str!("json.stn");
