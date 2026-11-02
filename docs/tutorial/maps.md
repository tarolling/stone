# Maps

```{literalinclude} ../examples/maps.stn
:language: stone
```

prints

```{literalinclude} ../examples/maps.out
:language: text
```

## Working with maps

A map finds a value by its key, as a Python `dict` does. Its type names both, as in
`map[str, int]`, and its keys are ints, strings, or bools. `{}` is an empty map, which gets its
types from what is later stored in it, and `{"a": 1, "b": 2}` writes one with entries.

- `m[k]` reads the value for `k`. A key the map has no entry for stops the program with
  `key not found: 'k'`, so use `m.get(k)`, which gives an option, when a key may be missing.
- `m[k] = v` adds an entry for `k`, or replaces its value if the map has one. `m[k] += 1` and
  other changes need the entry to be there already.
- `k in m` and `k not in m` test for a key, as `m.contains(k)` does, and `m.len()` counts the
  entries.
- `m.pop(k)` removes the entry for `k` and gives its value as an option, `none` if there was no
  entry.
- `for k in m:` walks the keys, and `m.keys()`, `m.values()`, and `m.items()` give them, the
  values, and both as tuples, in lists.

A map keeps its entries in the order they were added, so walking it and printing it always give
the same order, and adding a key again after `pop` puts it last.

A map is a value, like a list: assigning it, passing it, or storing it makes a copy as far as
the program can tell, so a function changes its caller's map only by returning it, as in
`counts = add(counts, word)`, which stone turns into a change in place. An entry changes through
any place that starts at a variable, such as `groups["even"].append(i)` or `p.counts[k] += 1`.
Two maps cannot be compared with `==`. See [Types](../reference/types.md#maps) for the details,
and [the builtins](../reference/builtins.md) for each method.

Next, {doc}`structs`.
