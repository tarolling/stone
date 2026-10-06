# Style guide

These rules apply to code, comments, documentation, and commit messages.

- Never use em dashes.
- Never use emojis.
- Use American English, such as "initialize", "color", and "behavior".
- Write doc comments (`///`, `//!`) in full sentences, with examples wherever practical:

  ```rust
  /// Rounds a frame size up to the 16-byte stack alignment required by the System V ABI.
  ///
  /// For example, `align16(20)` returns `32` and `align16(32)` returns `32`.
  fn align16(size: i32) -> i32 {
  ```

- Start inline comments (`//`) lowercase, and almost never as full sentences, as in
  `// globals live in main's frame`.
- Format with `cargo fmt`, and keep `cargo clippy --all-targets -- -D warnings` clean.

## Writing stone

These apply to stone programs, such as the examples, tests, and benchmarks.

- Document a function or variable with `//` comments on the lines directly above its definition,
  with no blank line between. Editors show them when hovering over the name, under its
  signature, as Markdown. Use a line holding only `//` to start a new paragraph:

  ```stone
  // how many times `n` can be halved before it reaches 1
  //
  // `n` must be positive.
  def halvings(n);
      count = 0
      while n > 1;
          n = n / 2
          count = count + 1
      ret count
  ```

- A comment at the end of a line, or one separated from the next line by a blank line, is an
  ordinary comment and documents nothing.

## Changing the language

- Keep `docs/grammar/stone.gram` and `docs/grammar/stone.asdl` in sync with the parser and AST.
  Parser functions mirror the grammar's rule names, such as `parse_t_primary`.
- The checker should only accept what both backends run identically.
- Runtime error messages must match between backends.
- Adding a builtin touches `BUILTINS` and `BUILTIN_DOCS` in `src/stdlib.rs`, its type rule in
  `Inference::infer_builtin`, `Interpreter::call`, its lowering in `Lowerer::call` (plus an IR
  instruction and its x86-64 emission if it is not a runtime call), a program in
  `tests/programs/`, and `docs/reference/builtins.md`.
- New keywords go in `docs/reference/syntax.md`. `tests/docs.rs` fails until both pages are
  updated.
