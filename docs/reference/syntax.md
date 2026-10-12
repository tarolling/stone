# Syntax

stone's grammar is derived from Python's, trimmed down and changed in a few places. The full
grammar is
[`docs/grammar/stone.gram`](https://github.com/tarolling/stone/blob/main/docs/grammar/stone.gram).

## Compared with Python

| Python | stone |
| --- | --- |
| `if x:` / `while x:` / `for i in xs:` | `if x;` / `while x;` / `for i in xs;` |
| `def f(a, b):`, annotations optional | `fn f(a: int, b: int) -> int;`, parameter types required |
| `else:` / `elif x:` | `else;` / `elif x;` |
| `return x` | `ret x` |
| `continue` | `cont` |
| `True`, `False`, `None` | `true`, `false`, `none` |
| `'single'` or `"double"` quotes | `"double"` quotes only |
| `-7 // 2 == -4` | `-7 / 2 == -3` (truncates) |
| `-7 % 2 == 1` | `-7 % 2 == -1` (takes the dividend's sign, like `/`) |
| `2 ** -1 == 0.5`, `2 ** 0.5` | `2.0 ** -1 == 0.5`; the exponent is always an `int` |
| `[1, 2] + [3]` joins, `[0] * 3` repeats | `+ - * / % **` on lists work element by element, as in numpy; join with `xs.extend(ys)` |
| `xs.insert(9, x)` clamps the index | an index past either end is an error |

## Keywords

These names are reserved and cannot be used for variables or functions:

| keyword | meaning |
| --- | --- |
| `fn` | defines a function |
| `pub` | lets other files use the function after it, as in `pub fn f() -> int;` |
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

A statement ends at the end of its line. A statement that opens a block (`fn`, `if`, `elif`,
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

Negative numbers are the unary `-` applied to a literal, as in `-7`. Strings use double quotes
and must close on the line they open on. A backslash starts an escape sequence:

| escape | means |
| --- | --- |
| `\"` | a double quote |
| `\'` | a single quote |
| `\\` | a backslash |
| `\n` | a line break |
| `\t` | a tab |
| `\r` | a carriage return |
| `\xhh` | the ASCII character with hex code `hh`, from `\x01` to `\x7f` |

So `"say \"hi\"\n"` holds `say "hi"` and a line break. Any other escape is an error, and so is
`\x00`, since a string cannot hold a null byte. Characters outside ASCII are written as
themselves, as in `"café"`.

## Statements

| statement | example |
| --- | --- |
| expression | `print(x)` |
| assignment | `x = 1`, `a = b = 0`, `items[i] = x`, `grid[r][c] = 0` |
| function definition | `fn f(a: int, b: int) -> int;` plus a block |
| `ret` | `ret`, `ret x` |
| `if` / `elif` / `else` | `if x < 0;` ... `elif x == 0;` ... `else;` ... |
| `while` | `while i < 10;` plus a block |
| `for` | `for x in items;`, `for c in text;`, `for i in range(n);`, `for i in range(a, b);` |
| `break`, `cont` | inside a loop only |
| `use` | `use util`, `use geometry.shapes.area as area` |

A `for` loop's variable must be a plain name. Functions can only be defined at the top level.
`use` statements come first in a file. See [Modules](modules.md) for how `use` finds files.

## Function signatures

Every parameter is written with its type after a colon, and a function that returns a value
writes its type after `->`:

```stone
fn mean(xs: list[float]) -> float;
    total = 0.0
    for x in xs;
        total = total + x
    ret total / float(xs.len())

fn report(name: str, score: float);
    print(name, score)
```

A type is `int`, `float`, `bool`, `str`, `none`, or `list[T]` for any type `T`, such as
`list[list[int]]`. A function without `->` returns `none`, the same as writing `-> none`.
Types are written only in signatures: variables, including a function's locals, never have
them. See [Types](types.md).

## Operators

From lowest to highest precedence:

| operators | operands |
| --- | --- |
| `or` | `int` or `bool` conditions |
| `and` | `int` or `bool` conditions |
| `not` | an `int` or `bool` condition |
| `==` `!=` `<` `<=` `>` `>=` | numbers; `==` and `!=` also strings, bools, and `none` |
| `+` `-` | two ints or two floats, or lists of them; `+` also joins two strings |
| `*` `/` `%` | two ints or two floats, or lists of them |
| unary `+` `-` | an int or a float, or a list of them |
| `**` | an int or float base and an `int` exponent, or lists of them |
| calls `f(x)`, method calls `xs.len()`, indexing `xs[i]`, slicing `xs[i:j:k]` | |

Comparisons chain, so `a < b <= c` means `a < b and b <= c` with `b` evaluated once.
`**` groups right to left and binds tighter than a unary sign on its left, as in Python, so
`2 ** 3 ** 2 == 512` and `-2 ** 2 == -4`, while `2 ** -1` raises to `-1`.
Parentheses group as usual. Operands are evaluated left to right.

A method call such as `xs.len()` evaluates the value before the `.` first, then the
arguments. A change, `xs[i][j] = v` or `xs[i].append(v)`, evaluates the assigned value, then
each index from the outermost, then the arguments, and only then checks the indexes against the
lists and makes the change. The builtin methods are `len`, `append`, `pop`, `insert`, `extend`,
`strip`, and `split`, documented in [Builtins](builtins.md#methods). A `.` right after an int starts a method call, so `1.` is not a
float; write `1.0`.

## Slices

`xs[start:stop]` is a new list of the elements from `start` up to but not including `stop`, and
`xs[start:stop:step]` takes every `step`th one, as in Python. Any of the three can be left out:
`start` defaults to the beginning and `stop` to the end, or the other way around for a negative
step, and `step` defaults to 1, so `xs[1:]` drops the first element and `xs[::-1]` is reversed.
A negative `start` or `stop` counts from the end, and one past either end is clamped, so
`xs[-2:]` is the last two elements and `xs[:100]` is all of a shorter list. A step of 0 stops
the program with `slice step cannot be zero`. The list is evaluated first, then the bounds left
to right.

A slice is a new list, so it cannot be assigned to or changed: `xs[1:3] = ys` and
`xs[1:].append(x)` are errors.

## Arithmetic on lists

The arithmetic operators work on lists of ints or floats element by element, as numpy's arrays
do. Both lists must have the same length, and a number on either side applies to every element:

```stone
a = [1.0, 2.0, 3.0]
b = [10.0, 20.0, 30.0]
print(a * 2.0 + b)   // [12.0, 24.0, 36.0]
print(-a, a ** 2)    // [-1.0, -2.0, -3.0] [1.0, 4.0, 9.0]
```

Each element follows the rules for single numbers: int arithmetic wraps, `/` on ints truncates,
and dividing by zero stops the program. Lists of different lengths stop it with
`lists have different lengths`. The operands that are not themselves arithmetic on lists are
evaluated once, left to right, then the lengths are checked, then the whole expression is
computed for the first element, then the second, and so on, so `a / b + c / d` reports the first
element where either division fails.

Because of this, `[0] * n` is a list of one element, `[0 * n]`, rather than `n` zeros as in
Python, and the checker warns about it. Join two lists with `xs.extend(ys)`.

Arithmetic on lists compiles to one loop over the elements, with no list for any intermediate
result, two elements at a time where the processor allows. When the result replaces a list
nothing else holds, as in `xs = xs * 2.0 + ys`, the loop writes over it in place.
