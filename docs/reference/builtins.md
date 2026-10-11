# Builtins

These functions are part of the language and available everywhere. Their names cannot be
assigned to. The builtin [methods](#methods) are called on a value instead, as in `xs.len()`.
The builtin modules, [`os`](#the-os-module), [`math`](#the-math-module),
[`random`](#the-random-module), and [`time`](#the-time-module), are imported like module files,
as in `use math`. The same descriptions appear on hover in the editor.

## `print`

```text
print(values...) -> none
```

Prints the values separated by spaces, followed by a newline. `print()` prints an empty line.
Floats print like Python's `repr`, bools as `true` and `false`, and lists with their elements
in brackets, with strings inside a list in single quotes:

```stone
print("total", 3, 0.5, true, none, [1, 2], ["a"])   // total 3 0.5 true none [1, 2] ['a']
```

## `range`

```text
range(end) | range(start, end)
```

Counts from `start`, or 0, up to but not including `end`. It can only be the iterable of a
`for` loop, and its bounds are evaluated once, before the loop starts.

```stone
for i in range(2, 5);
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
while not eof();
    line = input()
    print(line.len())
```

## `args`

```text
args() -> list[str]
```

Returns the command-line arguments given after the program, without the program's own name.
`stone run sum.st 1 2` and a built `./sum 1 2` both see `["1", "2"]`. Each call returns a new
list.

```stone
total = 0
for a in args();
    total = total + int(a)
print(total)
```

## Methods

A method is called on a value with a `.`, as in `items.append(4)`. The value before the `.` is
evaluated first, then the arguments, left to right, except that a method that changes its value
evaluates the indexes of its value, then the arguments, then makes the change. A method must be called: `items.len` on its
own is an error. Method names are not reserved, so they also work as ordinary names.

### `len`

```text
(str | list[T]).len() -> int
```

Returns the number of bytes in a string or elements in a list.

```stone
print("stone".len(), [1, 2, 3].len(), [].len())   // 5 3 0
```

### `append`

```text
list[T].append(item: T) -> none
```

Adds `item` to the end of the list. Since lists are values, this changes only the variable
`append` is called on, or the element of one, as in `grid[0].append(1)`, and never another
variable that was assigned from it. It is always a statement of its own.

```stone
names = []
names.append("ada")
print(names)   // ['ada']
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

## The `os` module

`os` is a builtin module: it has no file, and a program imports it like any other module, with
`use os` to call `os.env("HOME")`, `use os.env` to call `env("HOME")`, or `use os as system` to
call `system.env("HOME")`. Its names are only reserved once imported, so a program can still
have its own `env` or `pid`. A file named `os.st` next to the entry file cannot be imported,
since `use os` always means this module.

Both `stone run` and a built program answer from the same C library calls, so they agree on
every machine. The examples show values from one machine.

### `os.env`

```text
os.env(name: str) -> str
```

Returns the value of the environment variable `name`, or `""` if it is not set. Use `os.has_env`
to tell an empty value from a missing one. A name that is empty or holds `=` is never set.

```stone
use os

print(os.env("HOME"))   // /home/ada
```

### `os.has_env`

```text
os.has_env(name: str) -> bool
```

Returns whether the environment variable `name` is set, even to `""`.

```stone
use os

if os.has_env("DEBUG");
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
os.exit(code: int) -> none
```

Stops the program at once with the exit status `code`. Nothing after it runs, and it prints
nothing. The system keeps only the low 8 bits of the status, so `os.exit(256)` exits with 0 and
`os.exit(-1)` with 255. In an interactive session, it ends the session.

```stone
use os

if args().len() == 0;
    print("usage: greet NAME")
    os.exit(2)
print("hello", args()[0])
```

## The `math` module

`math` is a builtin module like [`os`](#the-os-module): `use math` to call `math.sqrt(2)`, or
`use math.sqrt` to call `sqrt(2)`. Since its functions are only reachable through the module,
a program can still name its own variables `min`, `max`, or `abs`. A file named `math.st` next
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

def clamp(x, low, high);
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

def distance(x, y);
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
`random.st` next to the entry file cannot be imported.

The numbers come from xoshiro256**, a fast generator whose numbers look random but follow from
its seed, so they are fine for games and simulations but not for passwords or keys. After
`random.seed(n)`, `stone run` and a built program draw exactly the same numbers on every
machine. Without a seed, the first draw seeds the generator from the system's entropy, so each
run differs.

### `random.seed`

```text
random.seed(n: int) -> none
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
`use time.sleep` to call `sleep(1)`. A file named `time.st` next to the entry file cannot be
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
for i in range(1000000);
    total = total + i
print("took", time.clock() - start, "seconds")
```

### `time.sleep`

```text
time.sleep(seconds: int | float) -> none
```

Waits for at least `seconds`, which may have a fraction, so `time.sleep(0.25)` waits a quarter
of a second. The system may wake the program a little later than asked, never earlier. A
negative length or nan stops the program with `sleep length must be non-negative`, and one of
2^63 seconds or more with `sleep length is too large`.

```stone
use time

for i in range(3);
    print(3 - i)
    time.sleep(1)
print("go")
```
