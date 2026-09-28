//! Names of the builtin functions shared by the interpreter and the compiler.

///
/// `range` is only valid as the iterable of a `for` loop, such as `for i in range(3);`.
pub static BUILTINS: [&str; 4] = ["print", "len", "range", "append"];
