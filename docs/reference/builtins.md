# Builtins

These functions are part of the language and available everywhere. Their names cannot be
assigned to. The builtin [methods](#methods) are called on a value instead, as in `xs.len()`.
The builtin modules, [`os`](#the-os-module), [`math`](#the-math-module),
[`random`](#the-random-module), [`time`](#the-time-module), [`fs`](#the-fs-module), and
[`net`](#the-net-module), are imported like module files, as in `use math`. The same descriptions appear on hover in the
editor.

## `print`

```text
print(values...)
```

Prints the values separated by spaces, followed by a newline. `print()` prints an empty line.
Floats print like Python's `repr`, bools as `true` and `false`, lists with their elements in
brackets, with strings inside a list in single quotes, enum values as they are written, and
struct values with their fields named, as in `Point(x=1.0, y=2.0)`, unless the struct
implements the trait `display`, whose `display` method then says how it is written. The whole
line is built before any of it is written, so a print that fails writes nothing:

```stone
print("total", 3, 0.5, true, some(1), [1, 2], ["a"])   // total 3 0.5 true some(1) [1, 2] ['a']
```

`print` returns no value, so a call to it is a statement of its own.

## `eprint`

```text
eprint(values...)
```

Writes the values to standard error exactly as `print` writes them to standard output. Use it
for messages about the run, such as warnings and progress, so they stay out of the output when it
is piped into another program:

```stone
eprint("warning: skipping line", 3)   // on stderr: warning: skipping line 3
```

Each line is written as soon as it is built, so when both streams go to the same place, as with
`2>&1`, lines appear in the order the program wrote them.

## `range`

```text
range(end) | range(start, end)
```

Counts from `start`, or 0, up to but not including `end`. It can only be the iterable of a
`for` loop, and its bounds are evaluated once, before the loop starts.

```stone
for i in range(2, 5):
    print(i)   // 2, then 3, then 4
```

## `int`

```text
int(value: int | float | str) -> int
```

Converts a number to an int, dropping any fraction, so `int(-2.5)` is -2. It stops the program
with `cannot convert float to int (nan or out of range)` if `value` is nan or does not fit in an
int.

Given a string, it reads a decimal int: an optional `+` or `-`, then one or more digits, with
spaces, tabs, and line breaks allowed at either end. Anything else, such as `"1.5"`, `"0x10"`, or
`""`, stops the program with `invalid literal for int() with base 10: '...'`, and a number that
does not fit in an int with `int() argument out of range: '...'`.

```stone
print(int(2.9), int(-2.9), int(7))   // 2 -2 7
print(int(" -42 "), int("+7"))       // -42 7
```

## `float`

```text
float(value: int | float | str) -> float
```

Converts a number to a float, rounding to the nearest float if needed.

Given a string, it reads a decimal float such as `2.5`, `-.5`, `3.`, or `6.02e23`, or `inf`,
`infinity`, or `nan` in any case, each with an optional sign and with whitespace allowed at
either end. A number too large for a float is `inf`. Anything else, such as `"1e"` or the hex
float `"0x1p3"`, stops the program with `could not convert string to float: '...'`.

```stone
print(float(3), float(9007199254740993))   // 3.0 9007199254740992.0
print(float("2.5e3"), float("-inf"))       // 2500.0 -inf
```

## `str`

```text
str(value) -> str
```

Returns the text `print` would write for `value`, of any type, so strings inside a list or
another value are quoted and a string on its own is not. A struct that implements the trait
`display` is written as its `display` method says, wherever it appears.

```stone
print("x = " + str(1.5) + ", done = " + str(true))   // x = 1.5, done = true
print(str([1, 2]) + str(some("a")))                  // [1, 2]some('a')
```

## `bytes`

```text
bytes(values: list[int]) -> bytes
```

Returns the bytes whose values are `values`, each from 0 to 255. A value outside that range
stops the program with `byte out of range (value 300)`, pointing at the call. See
[Bytes](types.md#bytes) for the type itself.

```stone
print(bytes([104, 105, 0]))   // b'hi\x00'
```

## `input`

```text
input() | input(prompt: str) -> result[str, str]
```

Writes `prompt`, if given, with no newline, then reads the next line from standard input and
returns `ok` of it without its newline. Only `\n` is removed, so a line ending in `\r\n` keeps
its `\r` (`strip` removes it), and a last line with no newline is still a line. Once the input is
over, `input` returns `err("end of input")`, so an empty line, `ok("")`, never looks like the
end. In a function that returns a `result[T, str]`, `?` passes the end on:

```stone
fn read_number() -> result[int, str]:
    line = input("number? ")?
    ret line.strip().parse_int()

print(read_number())   // ok(42) for the line 42, err('end of input') with no input
```

## `eof`

```text
eof() -> bool
```

Returns whether standard input has nothing left to read, so whether `input` would return an
`err`. If no input has arrived yet, as when someone is typing, it waits for some. The usual way
to read every line is:

```stone
while not eof():
    line = input().unwrap()
    print(line.len())
```

## `args`

```text
args() -> list[str]
```

Returns the command-line arguments given after the program, without the program's own name.
`stone run sum.stn 1 2` and a built `./sum 1 2` both see `["1", "2"]`. Each call returns a new
list.

```stone
total = 0
for a in args():
    total += int(a)
print(total)
```

## Methods

A method is called on a value with a `.`, as in `items.append(4)`. The value before the `.` is
evaluated first, then the arguments, left to right, except that a method that changes its value
evaluates the indexes of its value, then the arguments, then makes the change. A method must be called: `items.len` on its
own is an error. Method names are not reserved, so they also work as ordinary names.

### `len`

```text
(str | bytes | list[T] | map[K, V]).len() -> int
```

Returns the number of bytes in a string or bytes, elements in a list, or entries in a map.

```stone
print("stone".len(), [1, 2, 3].len(), {"a": 1}.len())   // 5 3 1
```

### `append`

```text
list[T].append(item: T)
```

Adds `item` to the end of the list. Since lists are values, this changes only the variable
`append` is called on, or the element of one, as in `grid[0].append(1)`, and never another
variable that was assigned from it. It is always a statement of its own.

```stone
names = []
names.append("ada")
print(names)   // ['ada']
```

### `pop`

```text
list[T].pop() -> option[T] | list[T].pop(index: int) -> T | map[K, V].pop(key: K) -> option[V]
```

Without `index`, removes the last element and returns it as `some(x)`, or returns `none` if the
list is empty. With `index`, removes the element there and returns it; a negative `index` counts
from the end, as in indexing, and the program stops with `pop from empty list` if the list is
empty and with `pop index out of range` if `index` is past either end. Like `append`, it changes
only the variable it is called on. Unlike `append`, it may be inside any expression, as in
`print(xs.pop().unwrap())` or `while xs.pop().is_some():`, as long as nothing else in that
statement uses the list it changes: `print(xs, xs.pop())` is an error, since `xs` would be read
around the change, so write `x = xs.pop()` first. The whole value of an assignment, such as
`xs[0] = xs.pop(1)`, may use it anywhere, since the value is evaluated before the target.

On a map, `pop` removes the entry for `key` and returns `some` of its value, or `none` if the map
has no entry for it. The other entries keep their order, and adding `key` again puts it last.
Removing an entry moves the ones after it, so it takes time in proportion to the map's size.

```stone
stack = [1, 2, 3]
top = stack.pop()
first = stack.pop(0)
print(top, first, stack)   // some(3) 1 [2]
stack = []
nothing = stack.pop()
print(nothing)             // none
ages = {"ann": 31, "bob": 27}
gone = ages.pop("ann")
missing = ages.pop("cy")
print(gone, missing, ages)   // some(31) none {'bob': 27}
```

### `insert`

```text
list[T].insert(index: int, item: T)
```

Puts `item` at `index`, moving the elements from there on up by one, so `xs.insert(0, x)` adds
`x` at the front and `xs.insert(xs.len(), x)` at the end. A negative `index` counts from the end.
Unlike Python, which quietly clamps the index, it stops the program with `list index out of range`
if `index` is below `-len` or above `len`. It is a statement of its own.

```stone
xs = [1, 3]
xs.insert(1, 2)
xs.insert(-3, 0)
print(xs)   // [0, 1, 2, 3]
```

### `extend`

```text
list[T].extend(items: list[T])
```

Adds every element of `items` to the end of the list, in order. A list can extend itself. It is
a statement of its own. Since `+` on lists works element by element, `extend` is how to join two
lists.

```stone
xs = [1, 2]
xs.extend([3, 4])
xs.extend(xs)
print(xs)   // [1, 2, 3, 4, 1, 2, 3, 4]
```

### `sort`

```text
list[T].sort()
```

Puts the elements of a list of `int`, `float`, `str`, or a type that implements `ord` in
ascending order, in place. Strings compare byte by byte, so `"Z"` comes before `"a"` and a prefix
before the longer string. Floats go in the order `-inf`, the negative numbers, `-0.0`, `0.0`, the
positive numbers, `inf`, then every `nan`, so no two values a program can tell apart tie and the
result never depends on how the list was sorted. Values of a struct that implements `ord` go in
the order its `cmp` gives, and values it finds equal keep the order they were in. It is a
statement of its own; to keep the list as it was, sort a copy (`ys = xs`, then `ys.sort()`).

```stone
xs = [3.5, -1.0, 0.0, -0.0]
xs.sort()
print(xs)   // [-1.0, -0.0, 0.0, 3.5]
```

Compiled programs sort a list that is already in order, or in reverse order, in one pass. Long
lists of ints or floats are sorted by radix, a pass per byte in which the values differ, after
subtracting the smallest, so ints from a small range take one or two passes; this borrows memory
as large as the list until it is done. Other lists of numbers and strings use a quicksort that
never takes more than `n log n` time. A list of a struct is sorted by a merge sort written in
stone in the prelude, which calls `cmp` about `n log n` times.

### `sort_by_key`

```text
list[T].sort_by_key(key: fn(T) -> K)
```

Puts the elements in ascending order of what `key` gives for each, in place. The keys must be of
a type `<` orders: `int`, `float`, `str`, or a type that implements `ord`, and they compare as
`sort` compares them. `key` is called once for each element, in order, before anything moves,
and elements with equal keys keep the order they were in. It is a statement of its own.

```stone
fn length(s: str) -> int:
    ret s.len()

words = ["pear", "fig", "kiwi"]
words.sort_by_key(length)
print(words)   // ['fig', 'pear', 'kiwi']
```

The methods from here to `max_by_key` take a function, and are written in stone in the
prelude. A call that passes a function by its name, as `words.sort_by_key(length)` does,
compiles to a copy of the method that calls that function directly.

### `map`

```text
list[T].map(f: fn(T) -> U) -> list[U]
```

Returns a new list of what `f` gives for each element, in order.

```stone
fn double(x: int) -> int:
    ret x * 2

print([1, 2, 3].map(double))   // [2, 4, 6]
```

### `filter`

```text
list[T].filter(keep: fn(T) -> bool) -> list[T]
```

Returns a new list of the elements `keep` gives true for, in order.

### `any`

```text
list[T].any(test: fn(T) -> bool) -> bool
```

Returns whether `test` gives true for any element. It calls `test` on each element in order and
stops at the first that gives true, so an empty list gives false.

### `all`

```text
list[T].all(test: fn(T) -> bool) -> bool
```

Returns whether `test` gives true for every element. It calls `test` on each element in order
and stops at the first that gives false, so an empty list gives true.

### `min_by_key`

```text
list[T].min_by_key(key: fn(T) -> K) -> option[T]
```

Returns `some` of the element with the least key, the first of several with an equal key, or
`none` for an empty list. It calls `key` once for each element, in order, and the keys compare as
for `sort_by_key`.

### `max_by_key`

```text
list[T].max_by_key(key: fn(T) -> K) -> option[T]
```

Returns `some` of the element with the greatest key, the last of several with an equal key, or
`none` for an empty list. With `min_by_key`, it gives the two ends of what `sort_by_key` would
put in order.

```stone
fn length(s: str) -> int:
    ret s.len()

words = ["fig", "pear", "kiwi"]
print(words.min_by_key(length), words.max_by_key(length))   // some('fig') some('kiwi')
```

### `strip`

```text
str.strip() -> str
```

Returns the string without the spaces, tabs, and line breaks (`\t`, `\n`, `\v`, `\f`, and `\r`)
at either end.

```stone
print("[" + "  a b  ".strip() + "]")   // [a b]
```

### `split`

```text
str.split() | str.split(separator: str) -> list[str]
```

With a separator, splits the string at each occurrence of it, from left to right, keeping empty
pieces, so there is always one more piece than separators. An empty separator stops the program
with `empty separator`. Without one, it splits at runs of whitespace and drops empty pieces, so
leading and trailing whitespace make no difference.

```stone
print("a,,b".split(","), "a::b".split("::"))   // ['a', '', 'b'] ['a', 'b']
print("  3 4   5 ".split())                    // ['3', '4', '5']
```

### `splitlines`

```text
str.splitlines() -> list[str]
```

Splits the string into lines at each `\n`, `\r\n`, or `\r`, without their endings. A line ending
at the very end ends the last line rather than starting an empty one, so a file's text splits into
its lines whether or not it ends with a newline, and an empty string has no lines. Unlike Python,
other characters Python counts as line breaks, such as `\x0c`, stay in the line.

```stone
print("one\r\ntwo\nthree\n".splitlines())   // ['one', 'two', 'three']
print("a\n\nb".splitlines(), "".splitlines())   // ['a', '', 'b'] []
```

### `join`

```text
str.join(items: list[str]) -> str
```

Returns the strings in `items` with this string between each pair, so joining one item gives
that item and joining none gives `""`. It undoes `split` with the same separator.

```stone
print(", ".join(["a", "b", "c"]))      // a, b, c
print("-".join("1 2 3".split()))        // 1-2-3
```

### `find`

```text
str.find(text: str) -> option[int]
```

Returns `some` of the index where `text` first appears in the string, or `none` if it never
does. Like `len` and indexes, it counts bytes. An empty `text` is found at `0`.

```stone
line = "key=value"
match line.find("="):
    case some(at):
        print(at, line[:at], line[at + 1:])   // 3 key value
    case none:
        print("no =")
print(line.find("?"))                       // none
```

### `rfind`

```text
str.rfind(text: str) -> option[int]
```

Returns `some` of the byte index where `text` last appears in the string, or `none` if it never
does. An empty `text` is found at the end, at `s.len()`.

```stone
path = "src/stdlib/net.stn"
match path.rfind("/"):
    case some(at):
        print(path[:at], path[at + 1:])   // src/stdlib net.stn
    case none:
        print(path)
```

### `count`

```text
str.count(text: str) -> int
```

Returns how many times `text` appears in the string, searching from the start and going on after
the end of each match, so matches never overlap. An empty `text` is found between every two
characters and at both ends, so it counts one more than the characters.

```stone
print("banana".count("a"), "aaaa".count("aa"), "café".count(""))   // 3 2 5
```

### `replace`

```text
str.replace(old: str, new: str) | str.replace(old: str, new: str, count: int) -> str
```

Returns the string with each match of `old`, found as `count` finds them, replaced by `new`. With
`count`, only the first `count` matches are replaced, or every one if `count` is negative. An
empty `old` matches between every two characters and at both ends, never inside a character.

```stone
print("a-b-c".replace("-", "+"), "a-b-c".replace("-", "", 1))   // a+b+c ab-c
print("ab".replace("", "."))                                      // .a.b.
```

### `lower`

```text
str.lower() -> str
```

Returns the string with each ASCII letter `A` to `Z` made lowercase. Unlike Python, letters
outside ASCII stay as they are, so `"ÉTÉ".lower()` is `"ÉtÉ"`.

```stone
print("Hello, World!".lower())   // hello, world!
```

### `upper`

```text
str.upper() -> str
```

Returns the string with each ASCII letter `a` to `z` made uppercase. Unlike Python, letters
outside ASCII stay as they are, so `"été".upper()` is `"éTé"`.

```stone
print("Hello, World!".upper())   // HELLO, WORLD!
```

### `startswith`

```text
str.startswith(prefix: str) -> bool
```

Returns whether the string begins with `prefix`. Every string starts with `""`.

```stone
print("stone".startswith("st"), "stone".startswith("one"))   // true false
```

### `endswith`

```text
str.endswith(suffix: str) -> bool
```

Returns whether the string ends with `suffix`. Every string ends with `""`.

```stone
print("main.stn".endswith(".stn"), "main.stn".endswith("main"))   // true false
```

### `parse_int`

```text
str.parse_int() -> result[int, str]
```

Reads the string as `int` does, but returns a result instead of stopping the program: `ok` of the
int, or `err` of the message `int` would stop with. Use it for text that may not be a number,
such as input, and `?` to pass the error on.

```stone
print("42".parse_int(), " -7\n".parse_int())   // ok(42) ok(-7)
print("4.5".parse_int())   // err("invalid literal for int() with base 10: '4.5'")
```

### `parse_float`

```text
str.parse_float() -> result[float, str]
```

Reads the string as `float` does, returning `ok` of the float, or `err` of the message `float`
would stop with.

```stone
print("2.5".parse_float(), "x".parse_float())   // ok(2.5) err("could not convert string to float: 'x'")
```

### `bytes`

```text
str.bytes() -> bytes
```

Returns the string's UTF-8 bytes. It copies nothing, since a string's bytes never change.

```stone
print("café".bytes())   // b'caf\xc3\xa9'
```

### `text`

```text
bytes.text() -> result[str, str]
```

Returns `ok` of the bytes as a string if they are UTF-8 text with no 0 byte, which a string
cannot hold, or `err("not valid UTF-8 text")`.

```stone
print(b"caf\xc3\xa9".text(), b"\xff".text())   // ok('café') err('not valid UTF-8 text')
```

### `is_some`

```text
option[T].is_some() -> bool
```

Returns whether the option holds a value.

### `is_none`

```text
option[T].is_none() -> bool
```

Returns whether the option is `none`.

```stone
print(some(1).is_some(), some(1).is_none(), none.is_none())   // true false true
```

### `unwrap`

```text
(option[T] | result[T, E]).unwrap() -> T
```

Returns the value the option or the `ok` holds. It stops the program with `cannot unwrap none` if
the option is `none`, or, if the result is an `err`, with `cannot unwrap err: ` and the error as
`print` writes it, such as `cannot unwrap err: invalid literal for int() with base 10: 'x'`. Use
it where that cannot happen, and `match`, `unwrap_or`, or `?` elsewhere.

```stone
print(some("a").unwrap(), ok(2).unwrap())   // a 2
```

### `unwrap_or`

```text
(option[T] | result[T, E]).unwrap_or(default: T) -> T
```

Returns the value the option or the `ok` holds, or `default` if it is `none` or an `err`.

```stone
print(some(3).unwrap_or(0), none.unwrap_or(0), "x".parse_int().unwrap_or(-1))   // 3 0 -1
```

### `is_ok`

```text
result[T, E].is_ok() -> bool
```

Returns whether the result is an `ok`.

### `is_err`

```text
result[T, E].is_err() -> bool
```

Returns whether the result is an `err`.

```stone
r = "12".parse_int()
print(r.is_ok(), r.is_err())   // true false
```

### `unwrap_err`

```text
result[T, E].unwrap_err() -> E
```

Returns the error the `err` holds. It stops the program with `cannot unwrap_err ok: ` and the
value, as `print` writes it, if the result is an `ok`, such as `cannot unwrap_err ok: 12`.

```stone
print("x".parse_float().unwrap_err())   // could not convert string to float: 'x'
```

### `ok`

```text
result[T, E].ok() -> option[T]
```

Returns `some` of the value an `ok` holds, or `none` for an `err`, which turns a result into an
option, as a function that returns an option needs before `?`.

### `err`

```text
result[T, E].err() -> option[E]
```

Returns `some` of the error an `err` holds, or `none` for an `ok`.

```stone
r = "7".parse_int()
print(r.ok(), r.err())   // some(7) none
```

### `contains`

```text
str.contains(text: str) -> bool | bytes.contains(part: bytes) -> bool | list[T].contains(value: T) -> bool | map[K, V].contains(key: K) -> bool
```

Returns whether the string has `text` in it (the empty string is in every string), the bytes
`part` in them, the list an
element equal to `value`, or the map an entry for `key`. A list compares its elements with `value`
in order, as `value == element` does, and stops at the first that is equal, so its elements must
be values `==` can compare, and a struct with its own `eq` compares by it. The operators `in` and
`not in` make the same test: `value in c` is `c.contains(value)`, except that it evaluates
`value` first.

```stone
ages = {"ann": 31}
print(ages.contains("ann"), ages.contains("bob"))   // true false
print([3, 1, 4].contains(4), "hello".contains("ell"))   // true true
```

### `get`

```text
map[K, V].get(key: K) -> option[V]
```

Returns `some` of the value of `key`'s entry, or `none` if the map has no entry for it. Unlike
`m[key]`, which stops the program with `key not found`, it never fails.

```stone
ages = {"ann": 31}
print(ages.get("ann"), ages.get("bob"))   // some(31) none
print(ages.get("bob").unwrap_or(0))      // 0
```

### `keys`

```text
map[K, V].keys() -> list[K]
```

Returns a new list of the map's keys, in the order they were first added. A `for` loop over a
map goes through the same keys without making a list.

```stone
ages = {"bob": 27, "ann": 31}
print(ages.keys())   // ['bob', 'ann']
for name in ages:
    print(name)      // bob, then ann
```

### `values`

```text
map[K, V].values() -> list[V]
```

Returns a new list of the map's values, in the order their keys were first added.

```stone
ages = {"bob": 27, "ann": 31}
print(ages.values())   // [27, 31]
```

### `items`

```text
map[K, V].items() -> list[tuple[K, V]]
```

Returns a new list of the map's entries as `(key, value)` tuples, in the order their keys were
first added, so `for k, v in m.items()` goes through both. It shares the map's storage until one
of them changes, so it copies nothing.

```stone
ages = {"bob": 27, "ann": 31}
print(ages.items())   // [('bob', 27), ('ann', 31)]
for name, age in ages.items():
    print(name, age)
```

### `eq`

```text
T.eq(other: T) -> bool
```

Returns whether two values are equal, as `==` does, for any type that implements the trait
`eq`: every type `==` compares part by part, and a struct whose `impl eq` block says what equal
means for it.

```stone
print(3.eq(3), (1, "a").eq((1, "b")))   // true false
```

### `cmp`

```text
T.cmp(other: T) -> ordering
```

Returns `less`, `equal`, or `greater` as the value comes before, with, or after `other`, for any
type that implements the trait `ord`: `int`, `float`, `str`, and a struct with an `impl ord`
block. Floats are in the order `sort` uses, so unlike `<`, `cmp` puts `-0.0` before `0.0` and
`nan` after every other float, with all `nan`s equal.

```stone
print(1.cmp(2), "b".cmp("a"), (-0.0).cmp(0.0))   // less greater less
```

### `min`

```text
T.min(other: T) -> T
```

Returns the lesser of the value and `other`, for any type that implements `ord`, keeping the
value itself when they are equal. It compares with `<`, as `math.min` does, so `0.0.min(-0.0)`
is `0.0` and a `nan` receiver stays `nan`.

```stone
print(5.min(3), "pear".min("apple"))   // 3 apple
```

### `max`

```text
T.max(other: T) -> T
```

Returns the greater of the value and `other`, for any type that implements `ord`, keeping the
value itself when they are equal. It compares with `>`, as `math.max` does.

```stone
print(5.max(3), 1.5.max(2.5))   // 5 2.5
```

### `clamp`

```text
T.clamp(lo: T, hi: T) -> T
```

Returns `lo` if the value is less than `lo`, `hi` if it is greater than `hi`, and the value
itself otherwise, for any type that implements `ord`. It stops the program if `lo` is greater
than `hi`, or if either is `nan`, as Rust's `clamp` does.

```stone
print(12.clamp(0, 10), (-3).clamp(0, 10), 0.5.clamp(0.0, 1.0))   // 10 0 0.5
```

### `display`

```text
T.display() -> str
```

Returns the text `print` writes for the value, as `str` does, for any type: every type
implements the trait `display`, and a struct whose `impl display` block says how to write it
uses that.

```stone
print([1, 2].display().len())   // 6
```

### `send`

```text
socket.send(text: str) -> result[int, str]
```

Sends all of `text` over the connection and returns `ok` of the number of bytes sent, or `err`
of the reason it could not, such as `broken pipe` or `connection reset by peer` once the other
end has closed the connection.

### `read_line`

```text
socket.read_line() -> result[option[str], str]
```

Waits for the next line from the connection and returns `ok` of `some` of it, without its `\n`
or `\r\n`, or `ok(none)` once the other end has closed the connection and every line has been
read. A last line without a line ending is still a line. Data that is not UTF-8 text is the
error `not valid UTF-8 text`.

```stone
use net

fn greeting(host: str, port: int) -> result[str, str]:
    c = net.connect(host, port)?
    ret ok(c.read_line()?.unwrap_or(""))
```

### `read`

```text
socket.read() -> result[str, str]
```

Waits for data from the connection and returns `ok` of whatever has arrived, or `ok("")` once
the other end has closed the connection. A character whose bytes arrive in two parts comes whole
with the second read, so the text is never cut inside a character.

### `read_all`

```text
socket.read_all() -> result[str, str]
```

Reads from the connection until the other end closes it, and returns `ok` of everything that
arrived.

### `accept`

```text
listener.accept() -> result[socket, str]
```

Waits for the next connection to the listener's port and returns `ok` of a socket for it.

### `port`

```text
listener.port() -> int
```

Returns the port the listener waits on, which is the port the system picked if
`net.listen` was given 0.

### `close`

```text
(socket | listener).close()
```

Closes the connection, or stops listening, now. A socket or listener also closes by itself
right after the last statement that uses it, so `close` is only needed to close one sooner, as
when the other end reads until the connection closes. Once closed, a socket's methods return
`err("socket is closed")`, and `accept` returns `err("listener is closed")`.

### `set_timeout`

```text
(socket | listener).set_timeout(seconds: float)
```

Makes each later wait on the socket or listener give up after `seconds` with `err("timed out")`:
waiting for data to read, for room to send, or for a connection to accept. The time is per wait,
so `read_all` keeps going for as long as data keeps arriving. It counts in whole milliseconds,
rounded up, and zero, a negative number, or nan waits forever, as a new socket or listener does.

```stone
use net

l = net.listen("127.0.0.1", 0).unwrap()
l.set_timeout(0.1)
print(l.accept())   // err('timed out'), since nothing connects
```

### `send_bytes`

```text
socket.send_bytes(data: bytes) -> result[int, str]
```

Sends all of `data` over the connection, as `send` sends text, and returns `ok` of the number
of bytes sent.

### `read_bytes`

```text
socket.read_bytes(max: int) -> result[bytes, str]
```

Waits for data from the connection and returns `ok` of what has arrived, at most `max` bytes,
or `ok(b"")` once the other end has closed it, or at once for a `max` of 0 or less. Text and
byte reads take from the same data, so a protocol can read a line of headers with `read_line`
and then a body with `read_exact`.

### `read_exact`

```text
socket.read_exact(n: int) -> result[bytes, str]
```

Waits for exactly `n` bytes and returns `ok` of them, or `err("connection closed early")` if the
other end closes the connection first, leaving the bytes that did arrive to read.

```stone
use net

fn body(c: socket) -> result[bytes, str]:
    length = c.read_line()?.unwrap_or("0").parse_int()?
    ret c.read_exact(length)
```

### `read_all_bytes`

```text
socket.read_all_bytes() -> result[bytes, str]
```

Reads from the connection until the other end closes it, and returns `ok` of every byte that
arrived.

## The `os` module

`os` is a builtin module: it has no file, and a program imports it like any other module, with
`use os` to call `os.env("HOME")`, `use os.env` to call `env("HOME")`, or `use os as system` to
call `system.env("HOME")`. Its names are only reserved once imported, so a program can still
have its own `env` or `pid`. A file named `os.stn` next to the entry file cannot be imported,
since `use os` always means this module.

Both `stone run` and a built program answer from the same C library calls, so they agree on
every machine. The examples show values from one machine.

### `os.env`

```text
os.env(name: str) -> option[str]
```

Returns `some` of the value of the environment variable `name`, or `none` if it is not set, so
an empty value, `some("")`, differs from a missing one. A name that is empty or holds `=` is never
set.

```stone
use os

print(os.env("HOME"))                         // some('/home/ada')
print(os.env("NO_SUCH_VARIABLE").unwrap_or(""))   // prints an empty line
```

### `os.has_env`

```text
os.has_env(name: str) -> bool
```

Returns whether the environment variable `name` is set, even to `""`, as
`os.env(name).is_some()` does.

```stone
use os

if os.has_env("DEBUG"):
    print("debugging")
```

### `os.platform`

```text
os.platform() -> str
```

Returns the operating system the program runs on, such as `"linux"`.

### `os.arch`

```text
os.arch() -> str
```

Returns the processor architecture the program runs on, `"x86_64"` or `"aarch64"`. A program
built with `--target aarch64` reports `"aarch64"` wherever it runs.

### `os.hostname`

```text
os.hostname() -> str
```

Returns the name of the machine the program runs on.

### `os.cpu_count`

```text
os.cpu_count() -> int
```

Returns the number of processors that are online, which is at least 1.

### `os.pid`

```text
os.pid() -> int
```

Returns the process ID of the running program. Under `stone run`, that is the interpreter's
process.

### `os.cwd`

```text
os.cwd() -> str
```

Returns the absolute path of the current working directory, the one the program was started
from unless something changed it. It stops the program with
`could not read the current directory` if the directory cannot be read, such as after it was
deleted.

### `os.exit`

```text
os.exit(code: int)
```

Stops the program at once with the exit status `code`. Nothing after it runs, and it prints
nothing. The system keeps only the low 8 bits of the status, so `os.exit(256)` exits with 0 and
`os.exit(-1)` with 255. In an interactive session, it ends the session.

```stone
use os

if args().len() == 0:
    print("usage: greet NAME")
    os.exit(2)
print("hello", args()[0])
```

### `os.run`

```text
os.run(cmd: str, args: list[str]) -> result[output, str]
```

Runs the program `cmd` with the arguments `args`, waits for it to finish, and returns `ok` of an
`output`, a struct from the prelude with three fields:

- `status: int`, the status the program exited with, or minus the number of the signal that
  stopped it (`-9` for `kill -9`), as Python's `subprocess` gives it
- `stdout: str`, everything it wrote to stdout
- `stderr: str`, everything it wrote to stderr

The program gets the environment and working directory of the one that runs it, and reads
nothing from stdin, as if it were `/dev/null`. No shell is involved, so each argument reaches the
program exactly as written, spaces and `*` included; to run a shell command, run
`os.run("sh", ["-c", "ls *.stn | wc -l"])`.

A `cmd` holding `/` names the program's file, relative to the working directory unless it starts
with `/`. Any other `cmd` is looked for in each directory of `PATH` in turn, the working directory
for an empty one, or `/bin` and `/usr/bin` if `PATH` is not set, as a shell looks. A directory
where the file is missing is skipped, and so is one where it cannot be run, though that error is
kept in case nothing else runs.

A program that exits with a status other than 0 has still run, so its result is `ok`: check
`out.status`. Only a program that cannot start is an `err`, of a message naming `cmd`, such as
`no such file or directory: 'gti'` or `permission denied: './notes.txt'`, and so is a program
whose stdout or stderr is not UTF-8 text, or holds a null byte (`not valid UTF-8 text: 'cmd'`),
and a `cmd` or argument holding a null character. A file with no `#!` line is not run through a
shell, so it gives `exec format error`.

```stone
use os

out = os.run("git", ["rev-parse", "--short", "HEAD"]).unwrap()
if out.status == 0:
    print("at", out.stdout.strip())
else:
    print("not a repository:", out.stderr.strip())

// ? passes on an err, as anywhere else
fn word_count(path: str) -> result[int, str]:
    out = os.run("wc", ["-w", path])?
    ret ok(out.stdout.split()[0].parse_int().unwrap_or(0))
```

## The `math` module

`math` is a builtin module like [`os`](#the-os-module): `use math` to call `math.sqrt(2)`, or
`use math.sqrt` to call `sqrt(2)`. Since its functions are only reachable through the module,
a program can still name its own variables `min`, `max`, or `abs`. A file named `math.stn` next
to the entry file cannot be imported.

Each function takes ints or floats, but never a mix of the two in one call: convert with
`float()` or `int()` first, as for arithmetic. Both `stone run` and a built program compute
every result to the same bit.

### `math.abs`

```text
math.abs(x: int | float) -> int | float
```

Returns `x` without its sign, as the same type, so `math.abs(-2.5)` is `2.5` and
`math.abs(-0.0)` is `0.0`. The smallest int, `-9223372036854775808`, has no positive
counterpart, so its absolute value stops the program with `integer overflow in abs`.

```stone
use math

print(math.abs(-3), math.abs(2.5))   // 3 2.5
```

### `math.min`

```text
math.min(values: int | float...) | math.min(values: list[int | float])
```

Returns the least of two or more numbers of the same type, or of the numbers in one list.
Equal values keep the first, so `math.min(0.0, -0.0)` is `0.0`. As in Python, a later value
only replaces the one kept so far when it is less, so a nan is kept only if it comes first.
The least of an empty list stops the program with `min of an empty list`.

```stone
use math

print(math.min(3, 1, 2))          // 1
print(math.min([2.5, 0.5, 1.5]))  // 0.5
```

### `math.max`

```text
math.max(values: int | float...) | math.max(values: list[int | float])
```

Returns the greatest of two or more numbers of the same type, or of the numbers in one list,
with the same rules as `math.min`. The greatest of an empty list stops the program with
`max of an empty list`.

```stone
use math

fn clamp(x: int, low: int, high: int) -> int:
    ret math.min(math.max(x, low), high)

print(clamp(12, 0, 10), math.max([4, 8, 7]))   // 10 8
```

### `math.sqrt`

```text
math.sqrt(x: int | float) -> float
```

Returns the square root of `x`, correctly rounded, so `math.sqrt(2)` is `1.4142135623730951`.
An int is converted to a float first. The square root of a negative number stops the program
with `math domain error`, as in Python, while `math.sqrt(-0.0)` is `-0.0` and the square root
of nan is nan.

```stone
use math

fn distance(x: float, y: float) -> float:
    ret math.sqrt(x * x + y * y)

print(distance(3.0, 4.0))   // 5.0
```

### `math.floor`

```text
math.floor(x: int | float) -> int
```

Returns the greatest int that is at most `x`, so `math.floor(2.5)` is `2` and
`math.floor(-2.5)` is `-3`, where `int(-2.5)` would drop the fraction and give `-2`. An int
comes back unchanged. A float that is nan or does not fit in an int stops the program with
`cannot convert float to int (nan or out of range)`, as `int` does.

```stone
use math

print(math.floor(7.0 / 2.0), math.floor(-0.5))   // 3 -1
```

## The `random` module

`random` is a builtin module like [`os`](#the-os-module): `use random` to call
`random.randint(1, 6)`, or `use random.randint` to call `randint(1, 6)`. A file named
`random.stn` next to the entry file cannot be imported.

The numbers come from xoshiro256**, a fast generator whose numbers look random but follow from
its seed, so they are fine for games and simulations but not for passwords or keys. After
`random.seed(n)`, `stone run` and a built program draw exactly the same numbers on every
machine. Without a seed, the first draw seeds the generator from the system's entropy, so each
run differs.

### `random.seed`

```text
random.seed(n: int)
```

Starts the numbers over from `n`, so the same seed always gives the same numbers.

```stone
use random

random.seed(42)
print(random.randint(1, 6), random.randint(1, 6), random.randint(1, 6))   // 1 1 6
```

### `random.random`

```text
random.random() -> float
```

Returns a float from 0.0 up to but not including 1.0, each of the 2^53 multiples of 2^-53 in
that range equally likely.

### `random.randint`

```text
random.randint(low: int, high: int) -> int
```

Returns an int from `low` to `high`, including both, each equally likely, as Python's
`random.randint` does. A `low` greater than `high` stops the program with
`empty range for randint`.

```stone
use random

roll = random.randint(1, 6)
print("you rolled", roll)
```

### `random.choice`

```text
random.choice(items: list[T]) -> T
```

Returns an element of the list, each equally likely, drawing the same number as
`random.randint(0, items.len() - 1)` would. Since lists are values, the result is a copy that
changes independently of the list. An empty list stops the program with
`cannot choose from an empty list`.

```stone
use random

print(random.choice(["rock", "paper", "scissors"]))
```

## The `time` module

`time` is a builtin module like [`os`](#the-os-module): `use time` to call `time.sleep(1)`, or
`use time.sleep` to call `sleep(1)`. A file named `time.stn` next to the entry file cannot be
imported. In version 0.2.0, `time.now` and `time.clock` were `os.time` and `os.clock`.

### `time.now`

```text
time.now() -> float
```

Returns the seconds since 1970-01-01 00:00:00 UTC, with a fraction.

```stone
use time

print(time.now())   // 1791512687.8459256
```

### `time.clock`

```text
time.clock() -> float
```

Returns seconds from a fixed but arbitrary point, which never go backward, even if the system's
time changes. Only the difference between two calls means anything, which makes it the way to
time code:

```stone
use time

start = time.clock()
total = 0
for i in range(1000000):
    total += i
print("took", time.clock() - start, "seconds")
```

### `time.sleep`

```text
time.sleep(seconds: int | float)
```

Waits for at least `seconds`, which may have a fraction, so `time.sleep(0.25)` waits a quarter
of a second. The system may wake the program a little later than asked, never earlier. A
negative length or nan stops the program with `sleep length must be non-negative`, and one of
2^63 seconds or more with `sleep length is too large`.

```stone
use time

for i in range(3):
    print(3 - i)
    time.sleep(1)
print("go")
```

## The `fs` module

`fs` is a builtin module like [`os`](#the-os-module): `use fs` to call `fs.read("notes.txt")`,
or `use fs.read` to call `read("notes.txt")`. A file named `fs.stn` next to the entry file
cannot be imported.

Every function but `fs.exists` returns a `result` whose error is a message naming the path,
such as `no such file or directory: 'notes.txt'`, so a program decides what a missing file
means, or passes the error on with `?` from a function that returns `result[..., str]`:

```stone
use fs

fn total(path: str) -> result[int, str]:
    sum = 0
    for line in fs.read_lines(path)?:
        sum += line.parse_int()?
    ret ok(sum)

print(total("numbers.txt"))   // ok(46), or err("no such file or directory: 'numbers.txt'")
```

A path is relative to the working directory unless it starts with `/`. The messages are the
same under `stone run` and a built program: the reason the system gives, in lowercase (such as
`permission denied`, `is a directory`, `not a directory`, or `file exists`), or
`os error N` for an error number with no reason in stone's table. A file, or a name in a
directory, that is not UTF-8 text is the error `not valid UTF-8 text`, and so is a file holding
a null byte, since a string cannot hold one.

### `fs.read`

```text
fs.read(path: str) -> result[str, str]
```

Returns `ok` of the whole text of the file at `path`, or `err` of a message.

```stone
use fs

match fs.read("notes.txt"):
    case ok(text):
        print(text.len(), "bytes")
    case err(message):
        print("cannot read:", message)
```

### `fs.read_lines`

```text
fs.read_lines(path: str) -> result[list[str], str]
```

Returns `ok` of the lines of the file at `path`, without their line endings, or `err` of a
message. A line ends at `\n` or `\r\n`, and a file that ends with a line ending has no empty
last line, so `"a\nb\n"` and `"a\r\nb"` both read as `["a", "b"]`.

```stone
use fs

for line in fs.read_lines("notes.txt").unwrap_or([]):
    print(line)
```

### `fs.write`

```text
fs.write(path: str, text: str) -> result[int, str]
```

Writes `text` to the file at `path`, creating it or replacing what it held, and returns `ok` of
the number of bytes written, or `err` of a message. A new file can be read and written by
everyone the system's umask allows.

```stone
use fs

fs.write("greeting.txt", "hello\n").unwrap()
print(fs.read("greeting.txt"))   // ok('hello\n')
```

### `fs.append`

```text
fs.append(path: str, text: str) -> result[int, str]
```

Writes `text` to the end of the file at `path`, creating it if there is none, and returns `ok`
of the number of bytes written, or `err` of a message.

```stone
use fs

for i in range(3):
    fs.append("log.txt", "line " + str(i) + "\n").unwrap()
print(fs.read_lines("log.txt"))   // ok(['line 0', 'line 1', 'line 2'])
```

### `fs.read_bytes`

```text
fs.read_bytes(path: str) -> result[bytes, str]
```

Returns `ok` of every byte of the file at `path`, text or not, or `err` of a message.

### `fs.write_bytes`

```text
fs.write_bytes(path: str, data: bytes) -> result[int, str]
```

Writes `data` to the file at `path`, as `fs.write` writes text, and returns `ok` of the number of
bytes written, or `err` of a message.

### `fs.append_bytes`

```text
fs.append_bytes(path: str, data: bytes) -> result[int, str]
```

Writes `data` to the end of the file at `path`, as `fs.append` appends text.

```stone
use fs

fs.write_bytes("data.bin", b"\x00\x01").unwrap()
fs.append_bytes("data.bin", bytes([255])).unwrap()
print(fs.read_bytes("data.bin"))   // ok(b'\x00\x01\xff')
```

### `fs.list_dir`

```text
fs.list_dir(path: str) -> result[list[str], str]
```

Returns `ok` of the names in the directory at `path`, without `.` and `..`, sorted by their
bytes (so `"B"` comes before `"a"`), or `err` of a message. The names are not paths: join one to
`path` to use it.

```stone
use fs

for name in fs.list_dir(".").unwrap():
    if name.endswith(".txt"):
        print(name)
```

### `fs.exists`

```text
fs.exists(path: str) -> bool
```

Returns whether there is a file or directory at `path`, following symbolic links, so a link to
nothing does not exist.

### `fs.remove`

```text
fs.remove(path: str) -> result[str, str]
```

Deletes the file at `path` and returns `ok` of `path`, or `err` of a message. It does not
delete directories.

### `fs.make_dir`

```text
fs.make_dir(path: str) -> result[str, str]
```

Creates the directory `path`, whose parent must exist, and returns `ok` of `path`, or `err` of
a message, such as `file exists: 'out'` if something is already there.

```stone
use fs

if not fs.exists("out"):
    fs.make_dir("out").unwrap()
fs.write("out/report.txt", "done\n").unwrap()
```

## The `net` module

`net` makes TCP connections and finds the addresses of hosts. Import it like a builtin module,
with `use net` to call `net.connect("example.com", 80)`, or with `use net.connect` to call
`connect("example.com", 80)`. Unlike the other builtin modules, it is written in stone, and a
program that uses it is linked with its source, so its functions behave alike under `stone run`
and in a built program, and an editor can show their code. A file named `net.stn` next to the
entry file cannot be imported. TLS (`https`) is not supported.

A connection is a `socket`, and a port waiting for connections a `listener`. Both are types a
signature can name, as in `fn serve(l: listener)`. A copy of one refers to the same connection,
since a connection cannot be copied, and they cannot be compared with `==`. Each closes by
itself right after the last statement that uses it, at the same point under both backends, or
sooner with [`close`](#close). Their methods, [`send`](#send), `read_line`,
[`read`](#read), `read_all`, [`accept`](#accept), and [`port`](#port), are listed
with the other methods. Text is sent and read as UTF-8; there is no type for other bytes yet.

```stone
use net

fn ask(host: str, port: int, question: str) -> result[str, str]:
    c = net.connect(host, port)?
    c.send("{question}\n")?
    ret c.read_all()

print(ask("localhost", 7777, "hello"))
```

A program is one thread, so a server handles one connection at a time. A connection to a port
the same program listens on completes before the program accepts it, so a program can talk to
itself:

```stone
use net

l = net.listen("127.0.0.1", 0).unwrap()
c = net.connect("127.0.0.1", l.port()).unwrap()
s = l.accept().unwrap()
c.send("ping\n").unwrap()
print(s.read_line())   // ok(some('ping'))
```

Errors are messages, as the [`fs`](#the-fs-module) module's are: the reason the system gives,
in lowercase, and the host and port as the program wrote them, such as
`connection refused: 'localhost:7777'`, or the reason alone for an error on an open
connection, such as `connection reset by peer`.

### Finding addresses

A host is an address, `127.0.0.1` or `::1`, or a name. `net` finds a name's addresses itself,
the same way under both backends:

1. A name in `/etc/hosts` has the addresses the file gives it, compared without regard to case.
2. Otherwise `localhost`, and any name ending in `.localhost`, is `127.0.0.1` and `::1`.
3. Otherwise the DNS servers named by `nameserver` lines in `/etc/resolv.conf` are asked, as
   the C library asks them: in turn, waiting `timeout` seconds for each (5 unless an `options`
   line says otherwise), going through them `attempts` times (2), and trying a name with fewer
   dots than `ndots` (1) in each `search` domain before trying it alone. With no `nameserver`
   line, the server on this machine, `127.0.0.1`, is asked.

IPv4 addresses come before IPv6 ones. The environment variables `STONE_HOSTS` and
`STONE_RESOLV_CONF` name other files to read instead, and a `nameserver` line may give a port
of its own, as in `nameserver 127.0.0.1:5353`, which tests use for a DNS server of their own.
A name that no server knows is the error `unknown host: 'name'`.

### `net.connect`

```text
net.connect(host: str, port: int) -> result[socket, str]
```

Connects to `port` on `host`, trying each of the host's addresses in the order
[Finding addresses](#finding-addresses) gives them until one connects, and returns `ok` of the
socket, or `err` of the last address's error.

### `net.connect_timeout`

```text
net.connect_timeout(host: str, port: int, seconds: float) -> result[socket, str]
```

Connects as `net.connect` does, but gives each address at most `seconds`, counted as
`set_timeout` counts them, after which it is `err("timed out: 'host:port'")`.
The socket it returns waits forever until its own `set_timeout`.

### `net.listen`

```text
net.listen(host: str, port: int) -> result[listener, str]
```

Listens for connections to `port` at the first address of `host`: `127.0.0.1` for this machine
alone, `0.0.0.0` for every IPv4 address, or `::` for every address. A port of 0 lets the system
pick a free one, which the listener's [`port`](#port) gives. A port another listener holds is
the error `address already in use`.

```stone
use net

l = net.listen("0.0.0.0", 8080).unwrap()
while true:
    match l.accept():
        case ok(c):
            c.send("hello\n").unwrap_or(0)
        case err(e):
            eprint(e)
```

### `net.resolve`

```text
net.resolve(host: str) -> result[list[str], str]
```

Returns `ok` of the addresses of `host`, as text, in the order `net.connect`
tries them, or `err` of a message.

```stone
use net

print(net.resolve("localhost"))   // ok(['127.0.0.1', '::1'])
print(net.resolve("::FFFF:1.2.3.4"))   // ok(['::ffff:1.2.3.4'])
```
