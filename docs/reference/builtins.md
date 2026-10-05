# Builtins

These functions are part of the language and available everywhere. Their names cannot be
assigned to. The same descriptions appear on hover in the editor.

## `print`

```text
print(values...) -> none
```

Prints the values separated by spaces, followed by a newline. `print()` prints an empty line.
Floats print like Python's `repr`, bools as `true` and `false`, and lists with their elements
in brackets, with strings inside a list in single quotes:

```stone
print("total", 3, 0.5, true, none, [1, 2], ["a"])   # total 3 0.5 true none [1, 2] ['a']
```

## `len`

```text
len(value: str | list[T]) -> int
```

Returns the number of bytes in a string or elements in a list.

```stone
print(len("stone"), len([1, 2, 3]), len([]))   # 5 3 0
```

## `range`

```text
range(end) | range(start, end)
```

Counts from `start`, or 0, up to but not including `end`. It can only be the iterable of a
`for` loop, and its bounds are evaluated once, before the loop starts.

```stone
for i in range(2, 5);
    print(i)   # 2, then 3, then 4
```

## `append`

```text
append(items: list[T], item: T) -> none
```

Adds `item` to the end of `items`.

```stone
names = []
append(names, "ada")
print(names)   # ['ada']
```

## `int`

```text
int(value: int | float) -> int
```

Converts a number to an int, dropping any fraction, so `int(-2.5)` is -2. It stops the program
with `cannot convert float to int (nan or out of range)` if `value` is nan or does not fit in an
int.

```stone
print(int(2.9), int(-2.9), int(7))   # 2 -2 7
```

## `float`

```text
float(value: int | float) -> float
```

Converts a number to a float, rounding to the nearest float if needed.

```stone
print(float(3), float(9007199254740993))   # 3.0 9007199254740992.0
```
