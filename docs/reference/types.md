# Types

stone is statically typed, but you never write a type. The checker infers one type for every
variable, parameter, and return value before the program runs, and reports every place they
conflict.

## The types

| type | values |
| --- | --- |
| `int` | 64-bit signed integers. `+`, `-`, and `*` wrap around on overflow |
| `float` | IEEE 754 doubles, including `inf`, `-inf`, and `nan` |
| `bool` | `true` and `false` |
| `str` | immutable byte strings |
| `none` | `none`, also what a function without `ret value` returns |
| `list[T]` | growable lists of `T`, shared by reference |
| functions | each function has one signature, such as `(int, list[str]) -> bool` |

## Inference

Each name gets one type for its whole scope, from the first thing assigned to it and every use
after. Assigning a different type later is an error:

```text
x = 1
x = "one"   # error: cannot assign str to 'x', which is int
```

An empty list `[]` starts with an unknown element type, which is filled in by what is appended
to it, assigned into it, or what it is passed to.

Functions are monomorphic: each parameter has a single type inferred from the function's body
and its calls. A function called as `f(1)` cannot also be called as `f("a")`.

## Rules the checker enforces

These keep the two backends identical and catch common mistakes:

- **No implicit conversion.** Arithmetic and comparisons never mix `int` and `float`; use
  `int()` or `float()`.
- **Conditions are `int` or `bool`.** `if`, `elif`, `while`, `and`, `or`, and `not` reject
  floats, strings, lists, and `none`.
- **Lists are not comparable.** `==` and `!=` cannot compare lists.
- **`range` is only a `for` iterable.** It cannot be stored or passed around.
- **Functions are top level, and only called.** A function cannot be assigned, stored, or
  passed as a value, and a builtin's name cannot be assigned to.
- **Calls match.** Each call passes as many arguments as the function has parameters.
- **Returns are consistent.** A function that returns a value on one path must return a value
  of the same type on every path.
- **Variables are assigned before they are read.** At the top level and inside each function,
  every path to a read must assign the name first. A function reading a global is checked when
  it runs instead, since it may be called before or after the assignment.
