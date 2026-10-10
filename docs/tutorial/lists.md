# Lists

```{literalinclude} ../examples/lists.st
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
- `for item in items;` walks the list in order, as it was when the loop started. If the body
  appends to the list, the loop does not visit the new elements.

An index past either end stops the program with `list index out of range`.

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
def with_total(xs);
    total = 0
    for x in xs;
        total = total + x
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

## Putting it together

This program finds primes with the sieve of Eratosthenes, using a `list[bool]` of candidates:

```{literalinclude} ../examples/sieve.st
:language: stone
```

prints

```{literalinclude} ../examples/sieve.out
:language: text
```

Next, {doc}`input`.
