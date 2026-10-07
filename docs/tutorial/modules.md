# Modules

A program can be split across files. Each file is a module named by its path, so there is
nothing to declare and no file that only lists other files. This program has three:

```text
modules/
  main.st
  stats.st
  text/
    pad.st
```

`main.st` is the entry file, the one you run:

```{literalinclude} ../examples/modules/main.st
:language: stone
```

`stats.st` is the module `stats`:

```{literalinclude} ../examples/modules/stats.st
:language: stone
```

and `text/pad.st` is the module `text.pad`:

```{literalinclude} ../examples/modules/text/pad.st
:language: stone
```

`stone run modules/main.st` prints

```{literalinclude} ../examples/modules/main.out
:language: text
```

## Using a module

The entry file's directory is the program's root, and a module's name is its path from there,
with `.` between the parts and without `.st`. `use` loads a module by that name:

- `use stats` binds the module, and you call its functions as `stats.mean(scores)`.
- `use text.pad.left` binds one function, `left`, which you call directly.
- `as` picks another name, as in `use text.pad.left as pad_left`.

A name is always the whole path from the root, whichever file the `use` is in, so moving a file
only changes how others name it. `use` statements come first in a file.

## pub

A function is private to its file unless it starts with `pub`. `main.st` can call
`stats.mean` and `stats.best`, but not `stats.total`, which only `stats.st` itself can use. Two
modules can each have a private function with the same name without clashing.

## What a module holds

A module other than the entry file holds only `use` statements and functions. Using a module
never runs any code, so two modules may use each other. A module's functions see their own
parameters and locals, the functions of their own file, and what that file uses, but never the
entry file's variables.

The checker infers types across every file together, so a function in a module gets its types
from how the program calls it.

See {doc}`../reference/modules` for every rule.

Next, {doc}`errors`.
