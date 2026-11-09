# Tutorial

This tutorial walks through stone from a first program to lists, functions, and input. If you
know Python, most of it will look familiar: the differences are mostly a few changes in syntax
and a type checker that runs before your program does.

Every program in this tutorial is part of stone's test suite, which runs it under both the
interpreter and the compiler and checks that each prints exactly the output shown.

```{toctree}
:maxdepth: 1

values
control-flow
functions
lists
maps
structs
enums
generics
traits
function-values
input
modules
errors
networking
windows
```

## Your first program

Save this as `hello.stn`:

```{literalinclude} ../examples/hello.stn
:language: stone
```

stone gives you three ways to handle it:

```sh
stone run hello.stn          # interpret it
stone build hello.stn && ./build/hello   # compile a native executable
stone check hello.stn        # only look for errors
```

`stone hello.stn` is shorthand for `stone run hello.stn`. Either way, it prints:

```{literalinclude} ../examples/hello.out
:language: text
```

A single file is enough for small programs. For anything larger, `stone init` starts a
project, a directory with a `stone.toml` that names it and a `main.stn` to start from:

```sh
stone init hello             # makes hello/stone.toml, hello/main.stn, hello/.gitignore
cd hello
stone run                    # prints Hello, world!
stone build && ./build/hello
stone test                   # runs the test block main.stn starts with
```

Inside a project, `stone run`, `stone build`, and `stone check` need no file: they use the
project's `main.stn`, from its directory or any directory inside it. See
{doc}`../reference/cli` for the details.

## Blocks and indentation

Like Python, stone groups statements by indentation. A line that opens a block ends in `:`, as
in Python, and the indented lines after it are the block:

```stone
if 2 > 1:
    print("math still works")
```

A block with a single statement can also follow the `:` on the same line, as in
`if done: break`. Comments start with `//` and run to the end of the line.

Next, {doc}`values`.
