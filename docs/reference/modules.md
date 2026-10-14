# Modules

A program is an entry file plus every module it uses, directly or through other modules. The
{doc}`../tutorial/modules` tutorial walks through an example.

## Names and files

The **root** is the directory of the entry file, the file given to `stone run`, `stone build`, or
`stone check`. Every other `.stn` file under the root is a module, named by its path from the root
with `.` between the parts and without `.stn`:

| file | module |
| --- | --- |
| `app/main.stn` (entry) | none; it cannot be used |
| `app/util.stn` | `util` |
| `app/geometry/shapes.stn` | `geometry.shapes` |
| `app/geometry.stn` | `geometry` |

A directory is only part of a module's name, never a module itself, so there are no files that
only declare or re-export others. A module and a directory can share a name, as `geometry.stn` and
`geometry/` do above. File and directory names must be valid stone names to be used.

## use

```stone
use geometry.shapes
use geometry.shapes.area
use geometry.shapes.area as shape_area
```

`use` takes a dotted path from the root and binds one name in the file:

- If the path names a module file, `use` binds the module by its last name, and its functions
  are called as `shapes.area(r)`. A module is not a value, so `x = shapes` is an error.
- Otherwise, if everything before the last name is a module, `use` binds that module's function
  by its own name, and it is called as `area(r)`.
- If that module defines an enum by the last name, `use` binds the enum, as in
  `use geometry.shapes.Shape`, and its values are written `Shape.Circle(1.0)`. Through the module,
  they are `shapes.Shape.Circle(1.0)`, and its type is `shapes.Shape`.
- If that module defines a struct by the last name, `use` binds the struct, as in
  `use geometry.shapes.Point`, and its values are written `Point(x=1.0, y=2.0)`. Through the
  module, they are `shapes.Point(x=1.0, y=2.0)`, and its type is `shapes.Point`.
- `as` binds the name after it instead.

So when both `geometry/shapes.stn` and a function `shapes` in `geometry.stn` exist,
`use geometry.shapes` is the module. Use `use geometry` and call `geometry.shapes()` to reach the
function.

The rules for `use`:

| rule | error |
| --- | --- |
| the path must name a module or a function in one | `no module named 'a.b'` |
| a directory alone cannot be used | `'geometry' is a directory, so use a module inside it` |
| the function must exist | `module 'util' has no function 'nope'` |
| the function must be `pub` | `'helper' is private to module 'util'` |
| the entry file cannot be used | `'main' is the entry file, so it cannot be imported` |
| each name is bound once per file | `'twice' is already imported` |
| a name cannot be both used and defined in a file | `'twice' is already defined in this file` |
| a builtin's name cannot be bound | `'str' is a builtin, so rename it with 'as'` |
| `use` comes before other statements | `use must come before other statements` |
| `use` is only allowed at the top level | `use is only allowed at the top level of a file` |

A function's parameters and locals may shadow a name bound by `use`, just as they shadow
functions.

## Builtin modules

`os`, `math`, `random`, and `time` are builtin modules with no file, so `use math`,
`use math.sqrt`, and `use math as m` work in any file without a `math.stn`. A program cannot have
a module of its own with one of those names: using it is the error
`'math' is a builtin module, so rename math.stn`. See [the builtins](builtins.md) for their
functions:

| module | for |
| --- | --- |
| [`os`](builtins.md#the-os-module) | the machine and process a program runs on |
| [`math`](builtins.md#the-math-module) | absolute values, least and greatest, square roots, and rounding down |
| [`random`](builtins.md#the-random-module) | pseudo-random numbers, the same in every backend after a seed |
| [`time`](builtins.md#the-time-module) | the time of day, timing code, and sleeping |

A builtin module's functions are only reachable through it, so a program can still name its
own variables and functions `min`, `max`, `sleep`, or `env`.

## pub

`pub fn` makes a function usable from other files, `pub enum` an enum, and `pub struct` a struct.
Every other function, enum, and struct is private to the file it is in. `pub` makes no difference in the entry file, so a
module can also be run on its own.

## Module bodies

A module other than the entry file holds only `use` statements, `fn`s, `enum`s, and `struct`s.
Any other top-level statement is an error: `only use, fn, enum, and struct are allowed at the top
level of a module`. So using a module never runs code, and modules may use each other in a cycle.

A module's functions see their parameters and locals, their own file's functions, builtins, and
the names their file binds with `use`. They never see the entry file's variables or functions:
reading one is `undefined name` or `undefined function`, as for any unknown name.

## Types

The checker checks the whole program at once, the same way it does within one file. A module's
functions have the types their signatures write, so `pub fn first(xs: list[str]) -> str; ret xs[0]`
can be called from any file with a `list[str]`, and a call with anything else is an error in the
calling file. Errors point at the file they are in, as in
`app/geometry/shapes.stn:4:12: error: ...`.
