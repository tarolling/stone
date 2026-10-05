# Functions

```{literalinclude} ../examples/functions.st
:language: stone
```

prints

```{literalinclude} ../examples/functions.out
:language: text
```

## Defining and calling

`def name(params);` opens a function, and `ret value` returns from it. `ret` on its own, or
reaching the end of the body, returns `none`. Functions must be defined at the top level, not
inside other functions or blocks, and they can be called from anywhere in the file, even above
their definition.

Like variables, parameters have no type annotations. The checker infers one type per parameter
and per return value from how the function is used, so a function called with an `int` in one
place cannot be called with a `str` in another. A function that returns a value on some paths
must return one on every path.

Calls can nest 1,000 deep. The 1,001st nested call stops the program with
`recursion is too deep (more than 1000 nested calls)`.

## Scope

stone resolves names the way Python does, without `global`:

- A function's parameters, and every name it assigns anywhere in its body, are local to it.
- Any other name it reads is a global: a variable assigned at the top level of the file,
  including inside top-level `if` and loop blocks.
- A function sees its own locals and the globals, never its caller's locals.

A function can read a global that is assigned later in the file, as long as the assignment has
run by the time the function is called. If it has not, the program stops with
`'name' is used before it is assigned`.

Next, {doc}`lists`.
