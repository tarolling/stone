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
| `True`, `False` | `true`, `false` |
| `None`, `Optional[int]` | `none` and `some(x)`, of type `option[int]` |
| `class Shape(Enum):` | `enum Shape;`, whose variants can hold values, as in Rust |
| `@dataclass class Point:` | `struct Point;` with a typed field per line, made as `Point(x=1.0, y=2.0)` |
| `match x:` / `case P:` | `match x;` / `case P;`, which must cover every value |
| `'single'` or `"double"` quotes | `"double"` quotes only |
| `-7 // 2 == -4` | `-7 / 2 == -3` (truncates) |
| `-7 % 2 == 1` | `-7 % 2 == -1` (takes the dividend's sign, like `/`) |
| `2 ** -1 == 0.5`, `2 ** 0.5` | `2.0 ** -1 == 0.5`; the exponent is always an `int` |
| `[1, 2] + [3]` joins, `[0] * 3` repeats | `+ - * / % **` on lists work element by element, as in numpy; join with `xs.extend(ys)` |
| `xs.insert(9, x)` clamps the index | an index past either end is an error |
| `t = 1, 2`, `return a, b` | `t = (1, 2)`, `ret (a, b)`; a tuple's values always need parentheses |
| `()`, `(x,)` | no empty or one-value tuples; a tuple has two or more values |
| `t[i]`, `t[1:]` | `t[1]`, `t[-1]`: a constant index only, and no slices |

## Keywords

These names are reserved and cannot be used for variables or functions:

| keyword | meaning |
| --- | --- |
| `fn` | defines a function |
| `enum` | defines an enum |
| `struct` | defines a struct |
| `impl` | defines the methods and associated functions of a struct |
| `pub` | lets other files use the function, enum, or struct after it, as in `pub fn f() -> int;` |
| `use`, `as` | uses a module or one of its functions, optionally under another name |
| `ret` | returns from a function, or ends the program at the top level |
| `if`, `elif`, `else` | conditional blocks |
| `while` | loops while a condition holds |
| `for`, `in` | loops over a list or a `range` |
| `match`, `case` | runs the first case whose pattern fits a value |
| `break` | leaves the innermost loop |
| `cont` | skips to the next iteration of the innermost loop |
| `and`, `or`, `not` | boolean operators, with short-circuiting `and` and `or` |
| `true`, `false` | the `bool` values |
| `none` | the option that holds no value |

## Lines and blocks

A statement ends at the end of its line. A statement that opens a block (`fn`, `if`, `elif`,
`else`, `while`, `for`, `case`) ends in `;`, followed either by an indented block on the next
lines or by a single simple statement on the same line. `enum`, `struct`, `impl`, and `match`
end in `;` too, and are always followed by an indented block: of variants or fields, one per
line, of `fn`s, or of `case`s:

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
| bool | `true`, `false` |
| option | `none`, `some(1)` |
| list | `[]`, `[1, 2, 3]`, `[[1.0], []]` |
| map | `{}`, `{"a": 1, "b": 2}`, `{1: [2.5], 3: [],}` |
| tuple | `(1, "a")`, `(x, y, z,)`, `(1, ("a", [2.5]))` |
| struct | `Point(x=1.0, y=2.0)`, `Line(end=q, start=p)` |

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
| assignment | `x = 1`, `a = b = 0`, `items[i] = x`, `grid[r][c] = 0`, `p.x = 1.0`, `ps[i].y = 0.0` |
| unpacking | `q, r = divide(7, 2)`, `a, b = (b, a)`, `(x, (y, z)) = t` |
| augmented assignment | `x += 1`, `total -= cost`, `items[i] *= 2`, `grid[r][c] **= 2`, `p.x += 0.5` |
| function definition | `fn f(a: int, b: int) -> int;` plus a block |
| enum definition | `enum Shape;` plus a block of variants, such as `Circle(float)` and `Empty` |
| struct definition | `struct Point;` plus a block of fields, such as `x: float` and `y: float` |
| `impl` block | `impl Point;` plus a block of `fn`s, such as `fn norm(self) -> float;` plus a block |
| `match` | `match s;` plus a block of cases, such as `case Shape.Circle(r);` plus a block |
| `ret` | `ret`, `ret x`, `ret (a, b)` |
| `if` / `elif` / `else` | `if x < 0;` ... `elif x == 0;` ... `else;` ... |
| `while` | `while i < 10;` plus a block |
| `for` | `for x in items;`, `for c in text;`, `for i in range(n);`, `for name, score in pairs;` |
| `break`, `cont` | inside a loop only |
| `use` | `use util`, `use geometry.shapes.area as area` |

A `for` loop's variable is a plain name, or names to unpack each tuple of a list into. Unpacking
takes names only, each once, as many as the tuple has values; an element such as `xs[0]` is
assigned on a line of its own. Functions, enums, structs, and `impl` blocks can only be defined
at the top level.
`use` statements come first in a file. See [Modules](modules.md) for how `use` finds files.

## Augmented assignment

`x op= v`, where `op=` is one of `+=`, `-=`, `*=`, `/=`, `%=`, and `**=`, means
`x = x op v`, with the same types and errors as `op`:

```stone
total = 0
for price in [3, 4, 5];
    total += price
total *= 2
print(total)   // 24
```

The target can be a variable, an element, or a field, as in `counts[i] += 1`,
`grid[r][c] **= 2`, or `ps[i].x += 0.5`. An element's indexes are evaluated once, from the
outermost, then the element is read, so an index
out of range stops the program before the value runs, then the value is evaluated and the result
stored. So `counts[random.randint(0, 5)] += 1` picks one position and adds 1 to it. A variable
assigned with `op=` is assigned like any other, so inside a function it is a local, and
`x += 1` in a function with a global `x` is an error, as in Python.

`+=` on lists adds element by element, as `+` does: `xs += ys` is `xs = xs + ys`, and
`xs += 1` adds 1 to every element. It never appends; use `xs.append(x)` or `xs.extend(ys)` for
that. Because the result replaces the list, it is computed in place when nothing else holds it.
Strings join with `s += t`, the same as `s = s + t`.

## Function signatures

Every parameter is written with its type after a colon, and a function that returns a value
writes its type after `->`:

```stone
fn mean(xs: list[float]) -> float;
    total = 0.0
    for x in xs;
        total += x
    ret total / float(xs.len())

fn report(name: str, score: float);
    print(name, score)
```

A type is `int`, `float`, `bool`, `str`, `list[T]` for any type `T`, such as
`list[list[int]]`, `map[K, V]` with keys of type `K` and values of type `V`, such as
`map[str, list[int]]`, `tuple[T1, T2, ...]` with a type for each of two or more values, such as
`tuple[str, list[int]]`, `option[T]`, or a struct's or enum's name, such as `Shape`, or
`shapes.Shape` for one of a module. A function without `->` returns no value, so a call to it can only be a
statement of its own, and its `ret` takes no value. Types are written only in signatures:
variables, including a function's locals, never have them. See [Types](types.md).

## Operators

From lowest to highest precedence:

| operators | operands |
| --- | --- |
| `or` | `bool` conditions |
| `and` | `bool` conditions |
| `not` | a `bool` condition |
| `==` `!=` `<` `<=` `>` `>=` | numbers and strings; `==` and `!=` also bools |
| `+` `-` | two ints or two floats, or lists of them; `+` also joins two strings |
| `*` `/` `%` | two ints or two floats, or lists of them |
| unary `+` `-` | an int or a float, or a list of them |
| `**` | an int or float base and an `int` exponent, or lists of them |
| calls `f(x)`, method calls `xs.len()`, fields `p.x`, indexing `xs[i]`, slicing `xs[i:j:k]` | |

Comparisons chain, so `a < b <= c` means `a < b and b <= c` with `b` evaluated once.
`**` groups right to left and binds tighter than a unary sign on its left, as in Python, so
`2 ** 3 ** 2 == 512` and `-2 ** 2 == -4`, while `2 ** -1` raises to `-1`.
Parentheses group as usual. Operands are evaluated left to right.

A method call such as `xs.len()` evaluates the value before the `.` first, then the
arguments. A change, `xs[i][j] = v`, `ps[i].x = v`, or `xs[i].append(v)`, evaluates the assigned
value, then each index from the outermost, then the arguments, and only then checks the indexes
against the lists and makes the change. A struct's named values are evaluated in the order they
are written. The builtin methods, such as `len`, `append`, `split`, and an option's
`unwrap`, are documented in [Builtins](builtins.md#methods). A `.` right after an int starts a method call, so `1.` is not a
float; write `1.0`.

## Enums and match

An enum lists its variants, one per line, each with the types of the values it holds in
parentheses, or none:

```stone
enum Shape;
    Circle(float)
    Rect(float, float)
    Empty
```

`Shape.Circle(1.0)` and `Shape.Empty` make values of it. `match` runs the first case whose pattern
fits a value:

```stone
match s;
    case Shape.Circle(r);
        print("a circle of radius", r)
    case Shape.Rect(w, _);
        print("a rectangle", w, "wide")
    case _;
        print("empty")
```

A pattern is a variant with a pattern for each of its values, such as `Shape.Rect(w, _)` or
`some(Shape.Circle(r))`, a name, which matches any value and binds it, or `_`, which matches any
value and binds nothing. `some(p)` and `none` are the patterns of an option. The cases must cover
every value. See [Types](types.md#enums).

## Structs

A struct lists its fields, one per line, each with its type:

```stone
struct Point;
    x: float
    y: float
```

`Point(x=1.0, y=2.0)` makes a value of it, naming a value for every field once, in any order.
`p.x` reads a field, and `p.x = 3.0`, `p.x += 1.0`, and `ps[i].y = 0.0` change one, as long as
the struct is stored in a variable. See [Types](types.md#structs).

An `impl` block, in the same file as the struct, holds its functions:

```stone
impl Point;
    fn origin() -> Point;
        ret Point(x=0.0, y=0.0)

    fn shift(self, dx: float);
        self.x += dx
```

A function whose first parameter is `self`, with no type, is a method, called on a value as
`p.shift(1.0)`; any other is an associated function, called on the struct as `Point.origin()`.
`self` is the only parameter written without a type, and only as a method's first. See
[Types](types.md#methods).

## Maps

A map literal is a key, `:`, and a value for each entry, separated by commas, in braces:
`{"ann": 31, "bob": 27}`. `m[k]` reads the value of `k`'s entry, and `m[k] = v` sets it, adding
the entry if `m` has none. See [Types](types.md#maps).

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
nothing else holds, as in `xs = xs * 2.0 + ys` or `xs += ys`, the loop writes over it in place.
