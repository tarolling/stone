# stone

stone is a small, statically checked language with Python-like syntax. One binary both interprets
a program and compiles it to a native x86-64 executable, and the two always agree on what a
program prints.

```stone
def fib(n);
    if n < 2;
        ret n
    ret fib(n - 1) + fib(n - 2)

for i in range(10);
    print(fib(i))
```

```sh
stone run fib.st                # interpret it right away
stone build fib.st -o fib && ./fib   # or compile it to a native executable
```

## Why stone

**Fewer Shift presses.** Blocks open with `;` instead of `:`, functions return with `ret`, and
loops skip ahead with `cont`. The constants are `true`, `false`, and `none`, all lowercase.

**Two backends, one behavior.** `stone run` walks the syntax tree, and `stone build` lowers it
to an IR, allocates registers, and emits assembly. Every program in the test suite, including
every example in this documentation, runs through both and must print the same thing, down to
float formatting and runtime error messages.

**Errors before running.** A type checker infers every type, so `1 + "a"`, mixing ints and
floats, or reading a variable that might not be assigned yet is reported with its line and
column before anything runs.

**Editor support.** The `stone-lsp` language server, packaged as a VS Code extension, shows
errors as you type, inferred types on hover, go to definition, references, rename, and
completion.

```{toctree}
:maxdepth: 2
:caption: Getting started

installation
tutorial/index
```

```{toctree}
:maxdepth: 2
:caption: Reference

reference/syntax
reference/modules
reference/types
reference/builtins
reference/runtime-errors
reference/cli
reference/editor
```

```{toctree}
:maxdepth: 2
:caption: Development

development/building
development/architecture
development/testing
development/fuzzing
development/benchmarks
development/language-server
development/releasing
development/style
```
