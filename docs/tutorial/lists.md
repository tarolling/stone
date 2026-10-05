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
- `items.append(value)` adds to the end, and `items.len()` counts the elements.
- `for item in items;` walks the list in order. If the body appends to the list, the loop also
  visits the new elements.

An index past either end stops the program with `list index out of range`.

Lists are shared, not copied: assigning a list to another variable, or passing it to a
function, gives a second name for the same list, so changes through one name show through the
other.

## Putting it together

This program finds primes with the sieve of Eratosthenes, using a `list[bool]` of candidates:

```{literalinclude} ../examples/sieve.st
:language: stone
```

prints

```{literalinclude} ../examples/sieve.out
:language: text
```

Next, {doc}`errors`.
