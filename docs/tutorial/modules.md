# Modules

A program can be split across files. Each file is a module named by its path, so there is
nothing to declare and no file that only lists other files. This program has three:

```text
modules/
  main.stn
  stats.stn
  text/
    pad.stn
```

`main.stn` is the entry file, the one you run:

```{literalinclude} ../examples/modules/main.stn
:language: stone
```

`stats.stn` is the module `stats`:

```{literalinclude} ../examples/modules/stats.stn
:language: stone
```

and `text/pad.stn` is the module `text.pad`:

```{literalinclude} ../examples/modules/text/pad.stn
:language: stone
```

`stone run modules/main.stn` prints

```{literalinclude} ../examples/modules/main.out
:language: text
```

## Using a module

The entry file's directory is the program's root, and a module's name is its path from there,
with `.` between the parts and without `.stn`. `use` loads a module by that name:

- `use stats` binds the module, and you call its functions as `stats.mean(scores)`.
- `use text.pad.left` binds one function, `left`, which you call directly.
- `as` picks another name, as in `use text.pad.left as pad_left`.

A name is always the whole path from the root, whichever file the `use` is in, so moving a file
only changes how others name it. `use` statements come first in a file.

## pub

A function is private to its file unless it starts with `pub`. `main.stn` can call
`stats.mean` and `stats.best`, but not `stats.total`, which only `stats.stn` itself can use. Two
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
