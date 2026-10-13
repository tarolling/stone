# Lists

```{literalinclude} ../examples/lists.stn
:language: stone
```

prints

```{literalinclude} ../examples/lists.out
:language: text
```

## Working with lists

A list holds values of a single type, written `list[int]`, `list[str]`, `list[list[float]]`,
and so on. An empty list `[]` gets its type from what is later appended to it or assigned
from it.

- `items[i]` reads an element, counting from `0`. A negative index counts from the end, so
  `items[-1]` is the last element.
- `items[i] = value` replaces an element.
- `items.append(value)` adds to the end, and `items.len()` counts the elements. `append`
  returns nothing, so it is always a statement of its own.
- `items.pop()` removes the last element and returns it, and `items.pop(i)` the one at `i`.
  `items.insert(i, value)` puts `value` at `i`, and `items.extend(more)` adds every element of
  `more` to the end.
- `items[start:stop]` is a new list of part of `items`, as in Python.
- `for item in items;` walks the list in order, as it was when the loop started. If the body
  appends to the list, the loop does not visit the new elements.

An index past either end stops the program with `list index out of range`.

## Slices, more methods, and arithmetic

```{literalinclude} ../examples/list_math.stn
:language: stone
```

prints

```{literalinclude} ../examples/list_math.out
:language: text
```

A slice `xs[start:stop:step]` works as in Python: any part can be left out, negative bounds
count from the end, and bounds past the end are clamped. It is a new list, so changing it never
changes `xs`.

Arithmetic on lists of ints or floats works element by element, like numpy's arrays, so
`prices * counts` multiplies matching elements and `totals * 1.08` multiplies each one. Both
lists must have the same length. This is the one place lists behave differently from Python,
where `+` joins lists and `*` repeats them: use `extend` to join lists. stone compiles each such
expression to a single loop with no intermediate lists, computing two elements at a time where
it can, so it can outrun the same numpy code, which builds a temporary array for each operator. See [syntax](../reference/syntax.md#arithmetic-on-lists)
for the details.

## Lists are values

A list is a value, like a number: assigning it to another variable, passing it to a function,
or putting it in another list gives a copy, so a change through one name never shows through
another. This is where stone differs from Python, which shares the list instead.

```stone
a = [1, 2]
b = a
b.append(3)
print(a, b)  // [1, 2] [1, 2, 3]
```

A function that changes a list it was given changes its own copy, so it returns the list, and
the caller assigns the result:

```stone
fn with_total(xs: list[int]) -> list[int];
    total = 0
    for x in xs;
        total += x
    xs.append(total)
    ret xs

scores = [3, 4]
scores = with_total(scores)
print(scores)  // [3, 4, 7]
```

For the same reason, a function can read a global list but not change it; it takes the list as
an argument and returns the new one instead. The checker points out each of these.

The copies cost nothing until they are needed. Copies share their elements, and a list is only
copied when it is about to change while another variable still holds it, so changing a list
that only one variable holds happens in place. That includes `scores = with_total(scores)`
above: since `scores` is replaced by the result, stone hands the list itself to the function,
which appends to it in place. The same goes for any list at its last use, such as `xs` in
`ys = xs` when nothing reads `xs` afterward. See
[value semantics](../philosophy.md#value-semantics) for why stone works this way.

## Tuples

A tuple groups a few values of different types, such as a name and a score:

```{literalinclude} ../examples/tuples.stn
:language: stone
```

prints

```{literalinclude} ../examples/tuples.out
:language: text
```

A tuple's type lists the type of each value, as in `tuple[str, int]`, and a function that
returns one, like `min_max` above, gives back several values at once. Names separated by
commas unpack a tuple into variables, after `=` or `for`, and `t[0]` or `t[-1]` reads one
value. A tuple never changes: to change a value, build a new tuple. A tuple's values always
need their parentheses, so `x = 1, 2` is an error that suggests `x = (1, 2)`. See
[Types](../reference/types.md#tuples) for the details.

## Putting it together

This program finds primes with the sieve of Eratosthenes, using a `list[bool]` of candidates:

```{literalinclude} ../examples/sieve.stn
:language: stone
```

prints

```{literalinclude} ../examples/sieve.out
:language: text
```

Next, {doc}`enums`.
