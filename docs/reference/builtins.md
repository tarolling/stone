# Builtins

These functions are part of the language and available everywhere. Their names cannot be
assigned to. The builtin [methods](#methods) are called on a value instead, as in `xs.len()`,
and the functions of the [`os` module](#the-os-module) are imported with `use os`. The same
descriptions appear on hover in the editor.

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
have its own `env` or `time`. A file named `os.st` next to the entry file cannot be imported,
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

### `os.time`

```text
os.time() -> float
```

Returns the seconds since 1970-01-01 00:00:00 UTC, with a fraction.

```stone
use os

print(os.time())   // 1791512687.8459256
```

### `os.clock`

```text
os.clock() -> float
```

Returns seconds from a fixed but arbitrary point, which never go backward, even if the system's
time changes. Only the difference between two calls means anything, which makes it the way to
time code:

```stone
use os

start = os.clock()
total = 0
for i in range(1000000);
    total = total + i
print("took", os.clock() - start, "seconds")
```
