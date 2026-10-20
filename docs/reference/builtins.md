# Builtins

These functions are part of the language and available everywhere. Their names cannot be
assigned to. The builtin [methods](#methods) are called on a value instead, as in `xs.len()`.
The builtin modules, [`os`](#the-os-module), [`math`](#the-math-module),
[`random`](#the-random-module), and [`time`](#the-time-module), are imported like module files,
as in `use math`. The same descriptions appear on hover in the editor.

## `print`

```text
print(values...)
```

Prints the values separated by spaces, followed by a newline. `print()` prints an empty line.
Floats print like Python's `repr`, bools as `true` and `false`, lists with their elements in
brackets, with strings inside a list in single quotes, enum values as they are written, and
struct values with their fields named, as in `Point(x=1.0, y=2.0)`:

```stone
print("total", 3, 0.5, true, some(1), [1, 2], ["a"])   // total 3 0.5 true some(1) [1, 2] ['a']
```

`print` returns no value, so a call to it is a statement of its own.

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
str(value: int | float | bool | str) -> str
```

Returns the text `print` would write for `value`, which is how to build a string from numbers.

```stone
print("x = " + str(1.5) + ", done = " + str(true))   // x = 1.5, done = true
```

## `input`

```text
input() | input(prompt: str) -> str
```

Writes `prompt`, if given, with no newline, then reads the next line from standard input and
returns it without its newline. Only `\n` is removed, so a line ending in `\r\n` keeps its `\r`
(`strip` removes it). At the end of the input, `input` returns `""`, which looks the same as an
empty line, so check `eof` to tell them apart.

```stone
name = input("name? ")
print("hello, " + name)
```

## `eof`

```text
eof() -> bool
```

Returns whether standard input has nothing left to read. If no input has arrived yet, as when
someone is typing, it waits for some. The usual way to read every line is:

```stone
while not eof():
    line = input()
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
(str | list[T] | map[K, V]).len() -> int
```

Returns the number of bytes in a string, elements in a list, or entries in a map.

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
the option is `none`, or `cannot unwrap err` if the result is an `err`, so use it where that
cannot happen, and `match`, `unwrap_or`, or `?` elsewhere.

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

Returns the error the `err` holds. It stops the program with `cannot unwrap_err ok` if the result
is an `ok`.

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
map[K, V].contains(key: K) -> bool
```

Returns whether the map has an entry for `key`.

```stone
ages = {"ann": 31}
print(ages.contains("ann"), ages.contains("bob"))   // true false
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
