# Syntax

stone's grammar is derived from Python's, trimmed down and changed in a few places. The full
grammar is
[`docs/grammar/stone.gram`](https://github.com/tarolling/stone/blob/main/docs/grammar/stone.gram).

## Compared with Python

| Python | stone |
| --- | --- |
| `if x:` / `while x:` / `for i in xs:` | `if x;` / `while x;` / `for i in xs;` |
| `def f(a, b):` | `def f(a, b);` |
| `else:` / `elif x:` | `else;` / `elif x;` |
| `return x` | `ret x` |
| `continue` | `cont` |
| `True`, `False`, `None` | `true`, `false`, `none` |
| `'single'` or `"double"` quotes | `"double"` quotes only |
| `-7 // 2 == -4` | `-7 / 2 == -3` (truncates) |
| `-7 % 2 == 1` | `-7 % 2 == -1` (takes the dividend's sign, like `/`) |
| `2 ** -1 == 0.5`, `2 ** 0.5` | `2.0 ** -1 == 0.5`; the exponent is always an `int` |

## Keywords

These names are reserved and cannot be used for variables or functions:

| keyword | meaning |
| --- | --- |
| `def` | defines a function |
| `pub` | lets other files use the function after it, as in `pub def f();` |
| `use`, `as` | uses a module or one of its functions, optionally under another name |
| `ret` | returns from a function, or ends the program at the top level |
| `if`, `elif`, `else` | conditional blocks |
| `while` | loops while a condition holds |
| `for`, `in` | loops over a list or a `range` |
| `break` | leaves the innermost loop |
| `cont` | skips to the next iteration of the innermost loop |
| `and`, `or`, `not` | boolean operators, with short-circuiting `and` and `or` |
| `true`, `false` | the `bool` values |
| `none` | the only value of type `none` |

## Lines and blocks

A statement ends at the end of its line. A statement that opens a block (`def`, `if`, `elif`,
`else`, `while`, `for`) ends in `;`, followed either by an indented block on the next lines or
by a single simple statement on the same line:

```stone
for i in range(3);
    print(i)
while true; break
```

Indent with spaces or tabs, as long as each block is consistent. Comments start with `//` outside
a string and run to the end of the line, and blank and comment-only lines may sit at any
indentation. Comment lines directly above a function or variable definition document it, and
editors show them when you hover over its name. Comment lines at the very top of a module's file,
followed by a blank line, document the module, and editors show them when you hover over the
module's name in a `use` or before a `.`.

## Literals

| literal | examples |
| --- | --- |
| int | `0`, `42`, `9223372036854775807` |
| float | `1.5`, `2.5e-3`, `1e9`, `1E+9` |
| string | `"hello"`, `""` |
| bool and none | `true`, `false`, `none` |
| list | `[]`, `[1, 2, 3]`, `[[1.0], []]` |

Negative numbers are the unary `-` applied to a literal, as in `-7`. Strings use double quotes,
must close on the line they open on, and have no escape sequences, so `"a\tb"` holds a backslash
and a `t`.

## Statements

| statement | example |
| --- | --- |
| expression | `print(x)` |
| assignment | `x = 1`, `a = b = 0`, `items[i] = x`, `grid[r][c] = 0` |
| function definition | `def f(a, b);` plus a block |
| `ret` | `ret`, `ret x` |
| `if` / `elif` / `else` | `if x < 0;` ... `elif x == 0;` ... `else;` ... |
| `while` | `while i < 10;` plus a block |
| `for` | `for x in items;`, `for i in range(n);`, `for i in range(a, b);` |
| `break`, `cont` | inside a loop only |
| `use` | `use util`, `use geometry.shapes.area as area` |

A `for` loop's variable must be a plain name. Functions can only be defined at the top level.
`use` statements come first in a file. See [Modules](modules.md) for how `use` finds files.

## Operators

From lowest to highest precedence:

| operators | operands |
| --- | --- |
| `or` | `int` or `bool` conditions |
| `and` | `int` or `bool` conditions |
| `not` | an `int` or `bool` condition |
| `==` `!=` `<` `<=` `>` `>=` | numbers; `==` and `!=` also strings, bools, and `none` |
| `+` `-` | two ints or two floats; `+` also joins two strings |
| `*` `/` `%` | two ints or two floats |
| unary `+` `-` | an int or a float |
| `**` | an int or float base and an `int` exponent |
| calls `f(x)`, method calls `xs.len()`, indexing `xs[i]` | |

Comparisons chain, so `a < b <= c` means `a < b and b <= c` with `b` evaluated once.
`**` groups right to left and binds tighter than a unary sign on its left, as in Python, so
`2 ** 3 ** 2 == 512` and `-2 ** 2 == -4`, while `2 ** -1` raises to `-1`.
Parentheses group as usual. Operands are evaluated left to right.

A method call such as `xs.len()` evaluates the value before the `.` first, then the
arguments. A change, `xs[i][j] = v` or `xs[i].append(v)`, evaluates the assigned value, then
each index from the outermost, then the arguments, and only then checks the indexes against the
lists and makes the change. The builtin methods are `len` and `append`, documented in
[Builtins](builtins.md#methods). A `.` right after an int starts a method call, so `1.` is not a
float; write `1.0`.
