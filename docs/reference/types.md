# Types

stone is statically typed. You write the type of each function's parameters and return value,
and the checker infers every other type, for each variable and expression, before the program
runs, reporting every place they conflict.

## The types

| type | values |
| --- | --- |
| `int` | 64-bit signed integers. `+`, `-`, `*`, and `**` wrap around on overflow |
| `float` | IEEE 754 doubles, including `inf`, `-inf`, and `nan` |
| `bool` | `true` and `false` |
| `str` | immutable byte strings, ordered byte by byte |
| `none` | `none`, also what a function without `ret value` returns |
| `list[T]` | growable lists of `T`, which are values: assigning or passing one copies it |
| `tuple[T1, T2, ...]` | two or more values of fixed types, such as `(1, "a")`, which never change |
| functions | each function has one signature, such as `fn(int, list[str]) -> bool` |

## Writing types

Types appear only in function signatures, after each parameter's name and after `->`:

```stone
fn scale(xs: list[float], k: float) -> list[float];
    ret xs * k
```

`int`, `float`, `bool`, `str`, and `none` are written as their names, a list as `list[T]`, so
a grid of floats is `list[list[float]]`, and a tuple as `tuple[` and the type of each value, so
a name and a score is `tuple[str, int]`. A function without `->` returns `none`, the same as
writing `-> none`, so returning a value from it is an error that suggests the `->` to add. A
parameter without a type is an error too, which suggests the type the checker would infer for
it.

Writing signatures keeps each function's contract in one place: a call with a wrong argument is
an error at that argument, and a mistake inside the function is reported inside it, against the
signature, rather than wherever the function happens to be called.

## Inference

Each name gets one type for its whole scope, from the first thing assigned to it and every use
after. Assigning a different type later is an error:

```text
x = 1
x = "one"   # error: cannot assign str to 'x', which is int
```

An empty list `[]` starts with an unknown element type, which is filled in by what is appended
to it, assigned into it, or what it is passed to.

Functions are monomorphic: a function has exactly the types its signature writes, so one that
takes `xs: list[int]` cannot also be called with a `list[str]`.

Arithmetic works on numbers and, element by element, on lists of them, so an operand whose type
is not known yet can be either. For example, after `xs = []`, nothing is known about `xs[0]`,
and `xs[0] * 2` makes it an `int` or a `list[int]`. The operand still has to be a number or a
list of numbers that fits the other operand, so a later `xs.append("a")` is an error at `"a"`.
If nothing ever settles it, it is the same type as the other operand.

## Strings

A string is a sequence of bytes holding UTF-8 text, so a character outside ASCII, such as `é`,
is more than one byte. `s.len()` counts bytes, and indexes count bytes too: `s[i]` is the
one-byte string at index `i`, counting a negative index from the end, and `s[a:b:c]` slices
bytes the way a list slice picks elements. Neither may cut a character in two, which stops the
program with `cannot cut a string inside a character`: `s[i]` must be an ASCII byte, a slice
with a step of 1 must start and stop between characters, and a slice with any other step must
pick only ASCII bytes. Text that is all ASCII therefore indexes and slices exactly as in
Python:

```stone
s = "naïve"
print(s[0], s[-1], s[:2], s[2:4], s.len())   // n e na ï 6
```

A `for` loop over a string takes one character at a time, each a string of the one to four
bytes that make it up, so it never cuts one, and `for c in "naïve"` runs five times:

```stone
for c in "aé€";
    print(c, c.len())   // a 1, then é 2, then € 3
```

A string knows its length, so `s.len()` takes the same time for any string.

`<`, `<=`, `>`, and `>=` order strings byte by byte, so a string orders before any longer string
it starts, and the order of characters is the order of their Unicode code points, as in Python.
Strings cannot be changed in place, so `s[0] = "x"` is an error; build a new string instead,
as in `s = "x" + s[1:]`.

## Tuples

A tuple holds two or more values, each of its own type, written in parentheses. It is how a
function returns several values:

```stone
fn divide(a: int, b: int) -> tuple[int, int];
    ret (a / b, a % b)

q, r = divide(17, 5)
print(q, r, divide(9, 2))   // 3 2 (4, 1)
```

Names separated by commas, on the left of `=` or after `for`, unpack a tuple, one name for each
value and at any depth, as in `a, (b, c) = t`. The whole value is computed before any name is
assigned, so `a, b = (b, a)` swaps. A list of tuples unpacks one element at a time:

```stone
scores = [("ann", 3), ("bo", 5)]
for name, score in scores;
    print(name, score)
```

`t[0]` reads a tuple's first value and `t[-1]` its last. The index must be a number written in
the code, so the checker knows the value's type, and one past the tuple's size is an error
before the program runs. A tuple's values are only read: `t[0] = v` and `t[0].append(v)` are
errors, and a new tuple replaces the old one, as in `t = (v, t[1])`.

Values separated by commas need their parentheses everywhere else, so `x = 1, 2` and
`ret a, b` are errors that suggest `(1, 2)` and `ret (a, b)`. A tuple has at least two values:
`(x)` is `x` in parentheses, and `()` and `(x,)` are errors.

A tuple prints as Python prints one, `(1, 'a')`, with strings inside it quoted. It cannot be
compared, iterated, converted with `str`, or used in arithmetic or as a condition.

A tuple is not stored anywhere of its own: the compiler keeps each of its values where it would
keep a variable, passes and returns them as separate values, and stores the values of a list's
tuples side by side in the list, so a tuple costs nothing over writing its values out by hand.
An element of a list can hold at most 32 values, counting each value of its tuples.

## Rules the checker enforces

These keep the two backends identical and catch common mistakes:

- **No implicit conversion.** Arithmetic and comparisons never mix `int` and `float`; use
  `int()` or `float()`. The one exception is `**`, whose exponent is always an `int`. The same
  holds element by element, so `list[int] + list[float]` and `[1, 2] * 0.5` are errors. Strings
  never mix with numbers either: `int("3")` and `float("2.5")` read numbers from strings, and
  `str(3)` writes them.
- **Conditions are `bool`.** `if`, `elif`, `while`, `and`, `or`, and `not` reject ints,
  floats, strings, lists, and `none`, so `if 1 or 0;` is an error; write `n != 0` instead of `n`.
- **Lists and tuples are not comparable.** `==` and `!=` cannot compare lists or tuples, and
  `<`, `<=`, `>`, and `>=` compare only numbers and strings.
- **Tuples are indexed by constants and never change.** `t[i]` needs `i` written as a number,
  and nothing changes a tuple's values or what they hold, so `t[0] = v` is an error.
- **Arithmetic on lists needs numbers.** `+ - * / % **` apply to a list only if it holds ints
  or floats, so `["a"] + ["b"]` and `[[1]] * [[2]]` are errors.
- **Changes are statements, made to variables.** Since lists are values, `xs.append(v)` and
  `xs[i] = v` change only the variable they start from. Each is a statement of its own, so
  `ys = xs.append(1)` is an error, and the list must be in a variable or an element of one, so
  `f().append(1)` and `xs[1:].append(1)` are errors too, since the change would be lost. `pop`,
  which also returns what it removes, may instead be the whole value of an assignment, as in
  `x = xs.pop()`.
- **Functions change only their own variables.** A function can read a global but cannot
  change it, with `append` or an index, just as it cannot assign it. It takes the list as an
  argument and returns the changed one instead. A function that changes a parameter and then
  never uses it gets a warning, since its caller cannot see the change.
- **`range` is only a `for` iterable.** It cannot be stored or passed around.
- **Functions are top level, and only called.** A function cannot be assigned, stored, or
  passed as a value, and a builtin's name cannot be assigned to.
- **Signatures are written.** Every parameter has a type, and a function that returns a value
  writes its type after `->`.
- **Calls match.** Each call passes as many arguments as the function has parameters, each of
  the parameter's type.
- **Returns match the signature.** Every `ret` in a function returns the type after its `->`,
  and a function with a return type returns a value on every path.
- **Variables are assigned before they are read.** At the top level and inside each function,
  every path to a read must assign the name first. A function reading a global is checked when
  it runs instead, since it may be called before or after the assignment.
