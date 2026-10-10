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
input
modules
errors
```

## Your first program

Save this as `hello.st`:

```{literalinclude} ../examples/hello.st
:language: stone
```

stone gives you three ways to handle it:

```sh
stone run hello.st          # interpret it
stone build hello.st && ./build/hello   # compile a native executable
stone check hello.st        # only look for errors
```

`stone hello.st` is shorthand for `stone run hello.st`. Either way, it prints:

```{literalinclude} ../examples/hello.out
:language: text
```

## Blocks and indentation

Like Python, stone groups statements by indentation. A line that opens a block ends in `;`
where Python would use `:`, and the indented lines after it are the block:

```stone
if 2 > 1;
    print("math still works")
```

A block with a single statement can also follow the `;` on the same line, as in
`if done; break`. Comments start with `//` and run to the end of the line.

Next, {doc}`values`.
