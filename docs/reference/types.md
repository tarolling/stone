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
| `str` | immutable byte strings |
| `none` | `none`, also what a function without `ret value` returns |
| `list[T]` | growable lists of `T`, which are values: assigning or passing one copies it |
| functions | each function has one signature, such as `fn(int, list[str]) -> bool` |

## Writing types

Types appear only in function signatures, after each parameter's name and after `->`:

```stone
fn scale(xs: list[float], k: float) -> list[float];
    ret xs * k
```

`int`, `float`, `bool`, `str`, and `none` are written as their names, and a list as `list[T]`,
so a grid of floats is `list[list[float]]`. A function without `->` returns `none`, the same as
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

## Rules the checker enforces

These keep the two backends identical and catch common mistakes:

- **No implicit conversion.** Arithmetic and comparisons never mix `int` and `float`; use
  `int()` or `float()`. The one exception is `**`, whose exponent is always an `int`. The same
  holds element by element, so `list[int] + list[float]` and `[1, 2] * 0.5` are errors. Strings
  never mix with numbers either: `int("3")` and `float("2.5")` read numbers from strings, and
  `str(3)` writes them.
- **Conditions are `int` or `bool`.** `if`, `elif`, `while`, `and`, `or`, and `not` reject
  floats, strings, lists, and `none`.
- **Lists are not comparable.** `==` and `!=` cannot compare lists.
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
