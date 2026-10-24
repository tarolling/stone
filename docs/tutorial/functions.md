# Functions

```{literalinclude} ../examples/functions.stn
:language: stone
```

prints

```{literalinclude} ../examples/functions.out
:language: text
```

## Defining and calling

`fn name(param: type, ...) -> type:` opens a function, and `ret value` returns from it. Each
parameter is written with its type, and the type after `->` is the type of the value the
function returns. A function without `->` returns no value: `ret` on its own, or reaching the
end of the body, leaves it, and a call to it can only be a statement of its own, as
`greet("ada")` is above. Functions must be defined at the top level, not inside other functions
or blocks, and they can be called from anywhere in the file, even above their definition.

The signature is the only place you write types. The checker infers the type of every variable
inside the function, as it does at the top level, and checks each call against the signature,
so `area("3", 4)` is an error at `"3"`. A function with a return type must return a value of
that type on every path.

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

Functions are values too, which can be passed to other functions and stored; see
{doc}`function-values`.

Next, {doc}`lists`.
