<!-- markdownlint-configure-file { "MD024": { "siblings_only": true } } -->

# Changelog

All notable changes to stone: the language, the `stone` binary, its interpreter, and its
compiler. The VS Code extension and the `stone-lsp` language server it ships have their own
changelog in `editors/vscode/CHANGELOG.md`. The format follows
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and versions follow
[semantic versioning](https://semver.org/).

## [Unreleased]

### Added

- Generic traits: a trait may have type parameters, as `trait add[R]:`, which a type implements
  once for each set of type arguments, as `impl add[int] for V:` and `impl add[V] for V:`. A
  call such as `v.add(2)` calls the impl its arguments fit, and a bound gives the trait its
  arguments, which may name other type parameters, as `fn plus[T: add[U], U](x: T, y: U) -> T:`.
  Blocks whose arguments could be the same are an error.

- Conversions through the prelude's traits `from[T]` (`fn from(value: T) -> Self`) and
  `try_from[T]` (`fn try_from(value: T) -> result[Self, str]`): a type called with one value,
  `T(x)`, converts it, as `Fahrenheit(c)` after `impl from[Celsius] for Fahrenheit:`, giving a
  `T` through `from` or a `result[T, str]` through `try_from`. A builtin type may convert from a
  struct in the struct's file (`impl from[Celsius] for float:`), and a bound lets generic code
  convert to its type parameter, as `T(text)?` with `[T: try_from[str]]`. The builtin
  conversions `int`, `float`, `str`, and `bytes` are impls of the same traits.

- The methods `a.min(b)`, `a.max(b)`, and `x.clamp(lo, hi)` on every type that implements
  `ord`: `int`, `float`, `str`, `bytes`, a struct with an `impl ord` block, and a type parameter
  bounded by `ord`. `min` and `max` keep the receiver of equal values and compare with `<` and
  `>`, as `math.min` and `math.max` do, and `clamp` stops the program if `lo` is greater than
  `hi` or either is nan, as Rust's does. A struct's own method of the same name wins.

- Lambdas: a function written where it is used, as `xs.map(|x| x * 2)`,
  `people.sort_by_key(|p| p.age)`, or `|a: int, b| a + b`, its parameters between bars and its
  body one expression on the same line. Its types are inferred from where it is used, and a
  parameter may still be given one. A lambda is a function value like a named function, which
  `stone build` calls directly when it is passed to `map` and the other list helpers, but it may
  use only its own parameters: a variable from outside it, global or local, is an error, since
  lambdas capture nothing yet. A lambda inside a generic function cannot have a type that depends
  on the function's type parameters yet.

- The string methods `replace(old, new)` and `replace(old, new, count)`, `count`, `rfind`
  (an option, as `find` is), `splitlines` (at `\n`, `\r\n`, or `\r`, without the endings), and
  `lower` and `upper`, as Python has them, except that `lower` and `upper` change only ASCII
  letters. An empty text matches between every two characters and at both ends, as in Python, so
  `"ab".replace("", "-")` is `"-a-b-"` and `"ab".count("")` is 3.

### Changed

- **Breaking:** every conversion that can fail returns a `result[T, str]` instead of stopping
  the program: `int(x)` of a float (nan or out of range) or a `str`, `float(s)` of a `str`, and
  `bytes(xs)` of a `list[int]` (a value outside 0 to 255). Write `int(s)?` to pass the error on,
  or `int(s).unwrap()`, which stops with `cannot unwrap err:` and the old message. `float(n)`
  of an int, `str(x)`, and a type converted to itself give the value as before.

### Removed

- **Breaking:** the string methods `parse_int` and `parse_float`; `int(s)` and `float(s)` return
  the same results.

## [0.3.4] - 2026-10-24

### Added

- Functions are values. A function's name without parentheses, as in `apply(double, 3)`, is a
  value of its type, written `fn(int) -> int` (or `fn(str)` for one that returns no value), which
  can be passed, returned, kept in lists, maps, structs, and enums, and called: `f(x)`,
  `fs[0](x)`, or `p.run(x)` for a struct field that holds one. A generic function takes its types
  from where it is used, and `print` writes a function value as its type. Builtins, methods, and
  variants are not values yet, and closures are not supported yet.
- Lists' methods that take a function, written in stone in the prelude: `map`, `filter`, `any`,
  `all`, `sort_by_key` (in place and stable, calling the key once per element), and `min_by_key`
  and `max_by_key`, which return options. A call that passes a function by its name compiles to a
  copy that calls it directly.
- `eprint(values...)` writes its values to standard error exactly as `print` writes them to
  standard output, so a script's messages stay out of its output when that is piped into another
  program. Each line is written at once, so with `2>&1` lines appear in the order the program
  wrote them, under both `stone run` and a built program.
- The `net` module makes TCP connections: `net.connect(host, port)`, `net.listen(host, port)`,
  and `net.resolve(host)`. A connection is a `socket`, with `send`, `read_line`, `read`,
  `read_all`, and `close`, and a server a `listener`, with `accept`, `port`, and `close`; both
  are types a signature can name. Each closes by itself right after the last statement that
  uses it, at the same point under `stone run` and in a built program. Names are found in
  `/etc/hosts` and then from the DNS servers in `/etc/resolv.conf`, by stone itself rather than
  the C library, so both backends agree. `net` is the first builtin module written in stone,
  which an editor can open from `use net`. TLS is not supported.
- `bytes`, a type for data that is not text: literals such as `b"GET\x00\xff"`, which may hold
  any byte, `b[i]` giving an int, slices, `+`, `==`, `<`, `in`, `for` over its values, and
  `sort`. `print` writes bytes as Python does, as `b'GET\x00\xff'`. `"text".bytes()`,
  `data.text()` (a result, since the bytes may not be UTF-8 text), and `bytes([104, 105])`
  convert. `fs.read_bytes`, `fs.write_bytes`, and `fs.append_bytes` read and write any file.
- Sockets send and read bytes: `send_bytes`, `read_bytes(max)`, `read_exact(n)`, and
  `read_all_bytes()`, sharing one buffer with the text reads, so a protocol can read a line of
  headers and then a body.
- Timeouts: `set_timeout(seconds)` on a socket or listener makes each later wait give up with
  `err("timed out")`, and `net.connect_timeout(host, port, seconds)` connects the same way.

### Changed

- A map's `pop` takes constant time on average, under `stone run` and in built programs, instead
  of moving every later entry and rebuilding the map's hash index: popping half of 400,000 keys,
  which took minutes, takes a few hundredths of a second. The entries left keep their order.
- Built programs run faster:
  - Division and remainder by any constant multiply by a magic number instead of dividing.
  - A global that the program assigns one literal, once, at its top level reads as that literal,
    so `x % modulus` with `modulus = 2147483648` shifts like `x % 2147483648`. The `globals`
    benchmark runs about four times as fast.
  - An index check or a check that a list is not shared, repeated in a function, is done once,
    and one that every turn of a loop repeats moves before the loop, failing exactly where the
    loop would.
  - Small functions that call no others are inlined, so `bodies[i].drift(dt)` changes the
    struct's fields in place.
  - The fields of a list element are read and written at fixed offsets from where it starts.
  - Floats stay in the processor's floating-point registers (`xmm` on x86-64, `d` on arm64)
    instead of moving through general registers for every operation, which makes the `nbody`
    benchmark more than twice as fast.
  - A loop reads an element that does not change across its turns, such as `a[i]` in
    `for k in range(n): s += a[i][k]`, once before it starts, loads where a list's elements are
    once rather than for every element, and tests whether to go on at the bottom of each turn.
- **Breaking:** `input()` returns a `result[str, str]`: `ok` of the line, or
  `err("end of input")` once the input is over, instead of `""`, which an empty line also gave.
  Write `input().unwrap()` where a line must be there (after checking `eof()`), `input()?` in a
  function that returns a `result[T, str]`, or `match` it.
- A failed `unwrap()` of an `err` shows the error, as in
  `cannot unwrap err: invalid literal for int() with base 10: 'x'`, written as `print` writes it,
  and a failed `unwrap_err()` of an `ok` shows the value, as in `cannot unwrap_err ok: 12`.

### Fixed

- `stone update` installs the newest release. It used to stop at the newest patch of the
  installed minor version, so 0.2.6 updated only to 0.2.7 while 0.3.1 was out, though
  `stone update --check` named 0.3.1.

## [0.3.3] - 2026-10-21

### Added

- `os.run(cmd, args)` runs another program and waits for it, returning a
  `result[output, str]`: `ok` of an `output`, a new prelude struct holding the program's
  `status` (minus the signal's number if one stopped it), `stdout`, and `stderr`, or `err` of a
  message such as `no such file or directory: 'gti'` if it cannot start. A nonzero status is
  still `ok`. No shell is involved: a `cmd` without `/` is looked for in each directory of
  `PATH`, the arguments reach the program as written, and its stdin is empty. Output that is
  not UTF-8 text is an `err`, as for `fs.read`. Values may still be named `output`.

- `in` and `not in`: `k in m` tests whether a map has the key `k`, `x in xs` whether a list has
  an element equal to `x` (compared as `==` compares, from the first, stopping at the first
  match, so a struct with its own `eq` uses it), and `t in s` whether the string `s` has the
  text `t` in it. Unlike in Python, they never chain with other comparisons: `x in xs == b` is
  an error that suggests `(x in xs) == b`.
- `contains` works on lists and strings too, as `xs.contains(x)` and `s.contains(t)`, making the
  same test as `in`.
- Values in strings: every string writes the value of each expression in braces inside it, as
  `"{name} has {xs.len()} items: {xs}"`, with no prefix. A value is any expression, including
  fields, method calls, indexes, and strings of its own, and is written as `print` writes it.
  The values are evaluated left to right before any is written. `{{` and `}}` write braces, and a
  lone `}`, an empty `{}`, and a format spec such as `{x:.2f}` (not supported yet) are errors.
- `str(x)` takes any value, giving the text `print` writes for it, so `str([1, 2])` is
  `"[1, 2]"`, and `x.display()` is the same.
- The prelude trait `display`, which `print`, `str`, and values in strings use. Every type
  implements it, and a struct can implement it itself, as `impl display for Money:` with
  `fn display(self) -> str`, to say how it is written wherever it appears, inside lists, maps,
  options, and other structs too.

### Changed

- `print` builds its whole line before writing it, so a print that fails, such as one whose
  value is nested too deeply, writes nothing rather than part of its line, and compiled code
  writes each line with one system call rather than one for each value and separator.

## [0.3.2] - 2026-10-19

### Added

- More of maps: `for k in m` goes through a map's keys in insertion order, over the map as it
  was when the loop started; `m.keys()`, `m.values()`, and `m.items()` return new lists, the
  last of `(key, value)` tuples, which compiled code makes without copying anything until one of
  them changes; and `m.pop(k)` removes an entry, returning `some` of its value or `none`, and
  keeps the other entries in order.
- A map entry changes in place like a list element, through any place that starts at a
  variable: `m[k] += 1`, `m[k].append(x)`, `m[k][i] = v`, `m[k].x = v`, `m[a][b] = v`, a
  struct's field such as `p.counts[k] += 1` or `p.counts.pop(k)`, and a method that changes
  `self.counts`. A plain `m[k] = v` adds a missing key; anywhere else the key must exist, or the
  program stops with `key not found` at its subscript, in both backends.
- `result[T, E]`, a prelude enum like `option[T]` whose variants `ok(x)` and `err(e)` are written
  alone, with the methods `is_ok`, `is_err`, `unwrap`, `unwrap_or`, `unwrap_err`, `ok()`, and
  `err()`, the last two turning it into an option.
- `?` after a result gives the value an `ok` holds, and on an `err` returns it from the function
  at once, anywhere in an expression, freeing what the statement made so far. It needs a function
  that returns a result with the same error type; on an option, it returns `none` from a function
  that returns an option. `x = f(x)?` moves `x` into the call, as `x = f(x)` does.
- `s.parse_int()` and `s.parse_float()` return a `result[int, str]` and a `result[float, str]`,
  whose error is the message `int(s)` and `float(s)` stop the program with.
- The builtin module `fs` reads and writes files: `fs.read(path)`, `fs.read_lines(path)`,
  `fs.write(path, text)`, `fs.append(path, text)`, `fs.list_dir(path)` (sorted names),
  `fs.exists(path)`, `fs.remove(path)`, and `fs.make_dir(path)`. Each but `exists` returns a
  result whose error is a message naming the path, such as
  `no such file or directory: 'notes.txt'`, worded the same under `stone run` and a built
  program, so errors pass on with `?`. A file that is not UTF-8 text, or holds a null byte, is
  an error too.

### Changed

- `ok` and `err` are reserved, like `some` and `none`, so a program that used them as names must
  rename them. A variable, parameter, or function may now be named `result`, `option`, or
  `ordering`, since their variants never follow their names; types in signatures still mean the
  enums.
- `pop`, and a method that changes `self` and returns a value, may be inside any expression,
  not only a statement of its own or the whole value of an assignment: `xs.pop().unwrap()`,
  `while xs.pop().is_some():`, `ret self.items.pop().unwrap_or(0)`, and `m.pop(k).unwrap_or(0)`
  all work. Nothing else in the same statement may use what the call changes, so
  `print(xs, xs.pop())` is an error pointing at the other `xs`, while `self.offset` beside
  `self.items.pop()` is fine.

## [0.3.1] - 2026-10-17

### Added

- Prompts for coding agents in `docs/agents/`: `stone.md` teaches an agent to write stone, and
  `python-to-stone.md` guides one through converting Python, with a translation of each idiom,
  the differences that change output without an error, and a worked example. The README links
  them for people coming from Python, and a test checks and runs every program in them.
- `xs.sort()` puts a list of `int`, `float`, or `str` in ascending order, in place. Strings
  compare byte by byte, and floats in a total order, `-inf`, the negative numbers, `-0.0`, `0.0`,
  the positive numbers, `inf`, then every `nan`, so the result is the same in `stone run` and
  `stone build` on every target. Like `append`, it is a statement of its own. Compiled programs
  sort a list already in order, or in reverse order, in one pass, and sort long lists of ints
  and floats by radix, one pass per byte in which the values differ after subtracting the
  smallest, splitting large lists by their highest byte first so each part is sorted in cache;
  other lists use a quicksort that falls back to heapsort, so it never takes more than
  `n log n` time. The new `sort` benchmark, which sorts 2 million ints and 2 million floats,
  runs 2.3 to 3.4 times faster than the same program in C with `qsort`.
- Traits: `trait Shape:` lists methods, written without a body (`fn area(self) -> float`), or
  with one as a default, and `impl Shape for Circle:` implements them for a struct, or for
  `int`, `float`, `bool`, or `str`. Bounds give a generic function's type parameters the methods
  of traits, as in `fn total[T: Shape](xs: list[T]) -> float:`, and each call checks that its
  types implement them. `stone build` compiles a copy per type, so a call through a bound calls
  the type's method directly. Inside a trait and any `impl` block, `Self` names the type. A
  trait's method changes `self` only if its default body does, and a type's method then changes
  its receiver in place too. A library's `pub trait` is imported like a struct.
- The prelude's traits `eq` and `ord`, and its enum `ordering`, whose variants `less`, `equal`,
  and `greater` are written and printed alone. Every type `==` compares part by part implements
  `eq`, and a struct can implement it itself with `fn eq(self, other: Self) -> bool`, which then
  decides `==` and `!=` for it wherever it is compared, and lets a struct holding a list be
  compared. `int`, `float`, and `str` implement `ord`, and a struct can with
  `fn cmp(self, other: Self) -> ordering`, which `<`, `<=`, `>`, `>=`, and `sort` then use;
  sorting a list of a struct keeps equal values in their order. `[T: eq]` and `[T: ord]` let a
  generic function compare and order its values. The methods `x.eq(y)` and `x.cmp(y)` work on
  every type that implements them, with floats in the order `sort` uses.

### Changed

- `trait` is a keyword, and `less`, `equal`, and `greater` are reserved, like `some`. A module
  cannot be named `prelude`.

## [0.3.0] - 2026-10-17

### Added

- Generic functions: type parameters in brackets after a function's name, as in
  `fn first[T](xs: list[T]) -> T:`. Each call infers its own types for them, and the body is
  checked once, with `T` standing for any type, so a value of type `T` can be moved, stored in
  lists, tuples, options, and map values, and printed, but not used in arithmetic, compared, or
  given methods. `stone build` compiles a copy of the function for each set of types it is
  called with. A library's `pub fn` can be generic too.
- Generic structs and enums: `struct Pair[A, B]:` and `enum Tree[T]:` take type parameters, which
  their fields and variants use and a value fills in from what it holds, so
  `Pair(first=1, second="a")` is a `Pair[int, str]`. Their types are written with their types
  filled in, as in `Tree[str]`. An `impl` block of a generic struct uses its type parameters
  without brackets, and `stone build` compiles its methods once for each set of types.
- `m.get(k)` reads a map's value for `k` as an option: `some` of the value, or `none` if the map
  has no entry for `k`, where `m[k]` stops the program.
- Benchmarks of strings (`strings`), maps (`wordcount`), structs and methods (`nbody`), and enums
  (`binary_trees`). Every benchmark reads its workload size from a `.args` file that the runner
  passes to each version, and the existing programs use `%` and `+=`.

### Changed

- `option[T]` is an ordinary generic enum, `pub enum option[T]:` with the variants `none` and
  `some(T)`, defined in stone by a prelude that every program is linked with, rather than a type
  the compiler special-cases. Programs are unchanged: `some(x)` and `none` are still written
  without `option.`, which is now an error to write (`option.some(1)`), and `some` is a variant
  rather than a builtin function.
- `%` by a constant power of two compiles to shifts instead of `idiv`, as `/` already did, which
  makes the `lcg` benchmark about 3.7 times as fast.
- An index into a list of structs or tuples is checked without dividing the list's length by the
  element's width, which makes the `nbody` benchmark about 3.9 times as fast.
- Blocks open with `:`, as in Python, instead of `;`: `if x:`, `fn add(a: int, b: int) -> int:`,
  `enum Shape:`, `struct Point:`, `impl Point:`, `match s:`, and `case Shape.Circle(r):`. `;` is
  no longer valid anywhere and reports `unexpected character ';'`. Since `;` only ever opened a
  block, `sed -i 's/;\( *\/\/.*\)\?$/:\1/' *.stn` converts most programs; a block
  written on the same line, as in `if done; break`, needs its `;` changed by hand.
- Builtins that could find nothing return an option instead of a value that means "missing":
  `s.find(t)` is `option[int]`, `none` rather than `-1` when `t` never appears; `xs.pop()` is
  `option[T]`, `none` rather than stopping the program when the list is empty (`xs.pop(i)` still
  returns the element and still stops the program on an empty list or an index out of range);
  and `os.env(name)` is `option[str]`, `none` rather than `""` when the variable is not set.

### Fixed

- `some` of a value with more than 32 values, such as a tuple of 34 ints, is a checker error, as
  it is for any enum's variant. `stone run` used to accept it and `stone build` to crash.
- `stone build` no longer fails to assemble a program that prints an option holding a map, such
  as `print(some({"a": 1}))`.
- `stone build` no longer fails with `undefined label 'stone.print_list_int'` for a program that
  prints an enum or option that may hold a list but never makes a list itself.
- `stone build` no longer fails with `undefined label 'stone.str_eq'` for a program that
  compares options or enums that may hold a string, such as two `option[str]` parameters, but
  never makes a string itself.
- The differential fuzzer generates structs with `impl` methods, enums, `match`, options, maps,
  and `==` on them, which found the three bugs above.

## [0.2.7] - 2026-10-15

### Added

- Methods: an `impl Point;` block, in the struct's own file, holds functions for a struct. One
  whose first parameter is `self`, written without a type, is a method, called on a value as
  `p.norm()`; any other is an associated function, called on the struct as `Point.origin()` or
  `geo.Point.origin()`. A method that changes `self` changes the value it is called on, as
  `append` changes a list, with no keyword: `s.push(1)` and `ps[i].shift(1.0)` change `s` and
  `ps[i]` in place, and a call of one is a statement of its own or the whole value of an
  assignment, as in `x = s.pop_top()`. Methods belong to their struct, so one can be named like a
  builtin method, and they can be used wherever their struct can, with no `pub`.
- `impl` is a keyword.
- `==` and `!=` compare tuples, structs, options, and enum values, part by part: tuples and
  structs value by value, and enum values by variant and then by the values they hold, so
  `(1, "a") == t`, `p != Point(x=0.0, y=0.0)`, `x == none`, and `s == Shape.Circle(1.0)` work.
  Floats inside compare as they do alone, so `(nan, 1) != (nan, 1)`. A value holding a list or a
  map anywhere still cannot be compared, and the error names the struct field that holds it.
  Comparing enum values nested more than 10,000 deep stops the program with
  `value is nested too deeply to compare (more than 10000 levels)`.

### Changed

- `driver::load` returns a `Checked` program, holding the module, its warnings, and how its method
  calls were resolved, which `driver::run_module` takes too.

## [0.2.6] - 2026-10-13

### Added

- Maps: `map[K, V]` with `int`, `str`, or `bool` keys, written `{"a": 1, "b": 2}`, or `{}` for an
  empty one. `m[k]` reads an entry, failing with `key not found: 'k'` if there is none,
  `m[k] = v` adds or changes one, `m.len()` counts them, and `m.contains(k)` tests for a key.
  Maps keep their entries in insertion order, print like Python's dicts, and are values like
  lists. Compiled maps are lists of entries with a hash table beside them.
- Structs, like Rust's: `struct Point;` lists its fields, one per line, each with its type, as in
  `x: float`. `Point(x=1.0, y=2.0)` makes a value, naming a value for every field once, in any
  order, and the values run in the order they are written. `p.x` reads a field, and `p.x = v`,
  `p.x += v`, `ps[i].x = v`, and `p.items.append(v)` change one in a variable. Structs are values,
  like lists: assigning or passing one copies it, so a function changes its caller's struct only
  by returning it, and `p.items = f(p.items)` changes the list in place. A struct prints with its
  fields named, as in `Point(x=1.0, y=2.0)`, can hold itself through a list, an option, or an
  enum, and a library module shares one with `pub struct`, imported with `use shapes.Point`.
- Compiled structs are never allocated: the compiler keeps each field as a variable of its own and
  stores the fields of a list's structs side by side in the list, as it does for tuples. Freeing
  lists nested any number of levels deep, as a struct holding a list of itself makes them, takes
  constant stack, and `print` counts struct values toward its limit of 10,000 levels.

### Changed

- `struct` is a keyword, so it can no longer name a variable or function.

## [0.2.5] - 2026-10-12

### Added

- Enums, like Rust's: `enum Shape;` lists its variants, one per line, each holding values of the
  types in its parentheses, as in `Circle(float)`, or none, as in `Empty`. `Shape.Circle(1.0)`
  and `Shape.Empty` make values, which never change and print as they are written. A variant can
  hold any type, including its own enum, so enums make trees and linked lists, and a library
  module shares one with `pub enum`, imported with `use shapes.Shape`.
- `match`, which runs the first `case` whose pattern fits a value: a variant with a pattern for
  each of its values, as in `case Shape.Rect(w, _);` or `case some(Shape.Circle(r));`, a name,
  which binds the value, or `_`. The checker requires the cases to cover every value, naming
  one they miss, and warns about a case that can never run.
- Options: `option[T]` is either `some(x)` or `none`, so a value that may be missing has a type
  that says so. Options have the methods `is_some`, `is_none`, `unwrap`, which stops the program
  with `cannot unwrap none` on `none`, and `unwrap_or(default)`.
- `match`, `case`, and `enum` are keywords, and names may start with `_`.
- Compiled enum values are counted like strings and lists, and freeing one, however long the
  chain of values it holds, takes constant stack. `print` stops at an enum value nested more
  than 10,000 deep, with `value is nested too deeply to print`, in both backends.

### Changed

- `none` is now only the option that holds no value, of type `option[T]`, rather than a value of
  a type of its own. A function without `->` returns no value, so a call to it can only be a
  statement of its own: `x = f()` and `print(f())` are errors, and `-> none` is no longer a
  type.
- `==` and `!=` no longer compare `none`; use `is_none()` or `match`.

## [0.2.4] - 2026-10-11

### Added

- Tuples, stone's first record type: `(1, "a")` holds two or more values of their own types,
  written `tuple[int, str]` in a signature, so a function can return several values, as in
  `ret (q, r)`. Names separated by commas unpack one, as in `q, r = divide(7, 2)`,
  `a, (b, c) = t`, and `for name, score in pairs;`, and `a, b = (b, a)` swaps. `t[0]` and
  `t[-1]` read a value with a constant index. Tuples never change, print as Python prints them,
  and cannot be compared, iterated, or used in arithmetic.
- Compiled code never allocates a tuple: each value lives in a register or spill slot of its
  own, a tuple argument is passed as its values, a tuple result comes back in several registers
  (three on x86-64 and eight on arm64, then on the stack), and a list of tuples stores the
  values of each element side by side, as many as 32 of them.
- Values separated by commas without parentheses, as in `x = 1, 2` or `ret a, b`, are an error
  that suggests the parentheses, as are `()`, `(x,)`, and `tuple[int]`.

### Changed

- Conditions must be `bool`. `if`, `elif`, `while`, and the operands of `and`, `or`, and `not`
  no longer accept an `int`, so `if 1 or 0;` and `while n;` are errors (`a condition must be
  bool, found int`); write `n != 0` or `while true;` instead. `a and b` and `a or b` are always
  `bool`.
- A compiled program's `list index out of range` and `string index out of range` errors give
  the index and the length, as in `list index out of range (index 5, length 2)`, and
  `lists have different lengths` gives both lengths, as in `(3 and 2)`, the same text
  `stone run` prints.

## [0.2.3] - 2026-10-11

### Added

- String literals take escape sequences: `\"`, `\'`, `\\`, `\n`, `\t`, `\r`, and `\xhh` for an
  ASCII character, so `print("say \"hi\"")` prints a quote. Any other escape is an error, as is
  `\x00`.
- Strings can be indexed and sliced like lists, as in `s[0]`, `s[-1]`, `s[1:]`, and `s[::-1]`.
  Indexes count bytes, as `len` does, and a result that would cut a character outside ASCII in
  two stops the program with `cannot cut a string inside a character`. An index past either end
  is `string index out of range`.
- `<`, `<=`, `>`, and `>=` compare strings, byte by byte, which orders characters by code point
  as Python does.
- The string methods `join`, `find`, `startswith`, and `endswith`, as in `", ".join(words)`,
  `line.find("=")`, which returns a byte index or -1, `name.startswith("#")`, and
  `path.endswith(".stn")`.
- A `for` loop walks a string one character at a time, as in `for c in "naïve";`, where each
  character is a string of the one to four bytes that make it up.
- Augmented assignment: `x += 1`, `total -= cost`, `xs[i] *= 2`, `grid[r][c] **= 2`, and `/=`
  and `%=`, each meaning `x = x op v`. An element's indexes are evaluated once, then the element
  is read, then the value. On lists, `+=` adds element by element as `+` does, never appending,
  and writes over the list in place when nothing else holds it.

### Changed

- A compiled string records its length, so `len`, comparing strings of different lengths with
  `==`, and the bounds check of `s[i]` take the same time for a string of any length, and a loop
  over `range(s.len())` that indexes `s` is no longer quadratic.
- A string inside a printed list is written as Python's `repr` writes it, so `["it's", "a\nb"]`
  prints as `["it's", 'a\nb']` rather than splitting the line.

## [0.2.2] - 2026-10-11

### Changed

- Runtime errors print like checker errors, with the file, line, and column, then the source
  line with carets under the expression that failed, under both `stone run` and a compiled
  executable, which prints exactly the same text. `recursion is too deep` points at the call
  that went one level too deep, and a change such as `grid[i][j] = v` at the subscript whose
  index is out of range.

- Source files use the extension `.stn` instead of `.st`, which Smalltalk already uses. A
  program's entry file is `main.stn`, and `use a.b` loads `a/b.stn`. Rename existing files with
  `for f in *.st; do mv "$f" "${f%.st}.stn"; done`.
- Functions are defined with `fn` instead of `def`, as in `fn add(a: int, b: int) -> int;`. `fn`
  is a keyword, and `def` is now an ordinary name.
- Every function parameter has a type, and a function that returns a value writes its type after
  `->`, as in `fn mean(xs: list[float]) -> float;`. A function without `->` returns `none`. Types
  are `int`, `float`, `bool`, `str`, `none`, and `list[T]`, and they are written only in
  signatures: variables are still inferred. A call is checked against the signature, so a wrong
  argument is an error at that argument. A parameter without a type is an error that suggests
  the type the checker infers for it, and returning a value from a function without `->` is an
  error that suggests the `->` to add.

## [0.2.1] - 2026-10-10

### Added

- The builtin `math` module: `math.abs`, `math.min`, `math.max`, `math.sqrt`, and `math.floor`.
  `min` and `max` take two or more numbers or one list of them. Since they live in a module,
  programs can still name their own variables `min` and `max`. `math.sqrt` of a negative number
  stops the program with `math domain error`, as in Python, and `math.floor` returns an int.
- The builtin `random` module: `random.seed`, `random.random`, `random.randint`, and
  `random.choice`. After `random.seed(n)`, `stone run` and a built program draw exactly the same
  numbers on every machine. Without a seed, each run seeds itself from the system's entropy.
- The builtin `time` module: `time.now`, `time.clock`, and `time.sleep`, which waits for a number
  of seconds that may have a fraction.
- Slices, as in Python: `xs[1:3]`, `xs[:2]`, `xs[-2:]`, `xs[::-1]`, and `xs[a:b:step]` give a
  new list. A step of 0 stops the program with `slice step cannot be zero`.
- The list methods `pop`, `insert`, and `extend`. `x = xs.pop()` takes the last element off `xs`,
  `xs.pop(i)` the one at `i`, `xs.insert(i, x)` puts `x` at `i`, and `xs.extend(ys)` adds every
  element of `ys`. Unlike Python's, `insert` stops the program when the index is out of range.
- Arithmetic on lists of ints or floats, element by element, as with numpy's arrays:
  `a * 2.0 + b` multiplies each element of `a` by 2 and adds the matching element of `b`. Both
  lists must have the same length, or the program stops with `lists have different lengths`.
  Compiled programs compute each such expression in one loop with no intermediate lists, two
  elements at a time with SSE2 or NEON where the operations allow, and in place when the result
  replaces a list nothing else holds. The checker warns about `[0] * n`, which multiplies the
  element rather than repeating it as Python does.
- The benchmark runner takes `--numpy PYTHON` and times a benchmark's numpy version, such as the
  new `vector` benchmark's `vector.np.py`, alongside the others.

### Changed

- `os.time()` and `os.clock()` moved to the new `time` module as `time.now()` and
  `time.clock()`, and the `os` module no longer has them.
- A program can no longer have a module of its own named `math`, `random`, or `time`, since
  `use math` always means the builtin module. Rename a `math.st`, `random.st`, or `time.st` next
  to the entry file.

## [0.2.0] - 2026-10-10

### Added

- Warnings. The first one points out a function that changes a parameter and never uses it
  afterward, since the caller cannot see the change. `stone run` and `stone build` print
  warnings and still run or build the program.
- `stone build` writes executables for arm64 macOS, the default on an Apple silicon Mac, and
  `--target aarch64-macos` builds one from any machine. stone writes and signs the Mach-O file
  itself, and the program calls the system only through `libSystem`, so it needs nothing
  installed to build or run.
- `--target` takes a processor level, such as `x86_64v3-linux` or `aarch64v8.2-macos`, naming
  the features a program may assume (stone does not use them yet), and accepts Rust's names for
  the same machines, such as `x86_64-unknown-linux-musl`.
- `stone build` takes several files, building each into its own executable, and `--target` can
  be given more than once to build for several targets in one run.
- `stone clean [PATH]` removes the build directory of the program in `PATH` (an entry file or a
  directory, the current one by default). It only removes a `build` directory that
  `stone build` made.

### Changed

- `stone build` writes to a `build` directory next to the entry file instead of `build/out` in
  the current directory, and names the executable after the file, so
  `stone build examples/basics.st` writes `examples/build/basics`. A `main.st` is named after
  its directory, and a target other than the machine stone runs on builds into
  `build/<target>/`, such as `build/aarch64-macos/basics`. `-o` still picks a path.
- Targets display by their full names, `x86_64-linux` and `aarch64-linux`, in messages and help.
  The short names `x86_64` and `aarch64` still work.
- Release archives are named by stone's target names, such as `stone-x86_64-linux.tar.gz` and
  `stone-aarch64-macos.tar.gz`, instead of Rust's. `install.sh` and `stone update` look for the
  new names, so `stone update` from an earlier version cannot find this release; reinstall with
  `install.sh` instead.
- stone has a new design philosophy, described in the documentation's Philosophy page: Python's
  simplicity with Rust's memory safety, speed, and error reporting, with the complexity moved
  into the compiler, and compiled programs that need no libc. The syntax is unchanged.
- Lists are values. Assigning a list, passing it to a function, or putting it in another list
  behaves as a copy, so after `b = a`, `b.append(3)` leaves `a` unchanged. Compiled code copies a
  list only when it is about to change while something else still refers to it, so a list held
  by one variable is still changed in place.
- `xs = f(xs)` and `ret f(xs)` change the list in place: stone moves `xs` into the call instead
  of copying it, unless `f` could read `xs` as a global, so building a list through a
  function no longer copies it on every call.
- More generally, a list moves at its last use instead of being copied, in both `stone run` and
  `stone build`: `ys = add(xs, 1)`, `ys = xs`, and `rows.append(row)` hand over the list itself
  when nothing reads `xs` or `row` afterward, so a later change to it happens in place.
- A function changes its caller's list by returning it, as in `xs = add(xs, 1)`, and can read
  a global but no longer change it with `append` or an index.
- `append` is a statement of its own (`ys = xs.append(1)` is an error), and it and `xs[i] = v`
  must change a variable or an element of one (`f().append(1)` is an error).
- A `for` loop walks its list as it was when the loop started, so appending to the list inside
  the loop no longer makes the loop run longer.
- A change such as `grid[i][j] = v` evaluates every index before checking any against the
  lists, so an out-of-range `i` now stops the program after `j` is evaluated.
- Compiled programs no longer need a C library. They start at their own entry point and call
  the Linux kernel directly, with their own memory allocator, input buffering, and float
  formatting and parsing, so `stone build` makes a static executable that runs on any Linux
  machine with the same processor.
- `stone build` no longer needs gcc. It assembles and links programs itself, so installing
  stone is all it takes to build them, for either processor from any machine:
  `--target aarch64` on x86-64 (or `x86_64` on arm64) needs no cross compiler. The machine
  code is the same, byte for byte, as GNU's assembler makes, and the executable's symbol table
  names each function, so tools such as `gdb`, `perf`, and `objdump` show `fn.area` and `main`,
  though `gdb` can no longer step through the lines of the `.s` file.
- Compiled programs print floats several times faster.

### Fixed

- A compiled program that called `strip` but used no lists, or `eof` but never `input`, failed
  to link.
- A compiled program that used lists but never printed anything failed to link.

## [0.1.3] - 2026-10-09

### Added

- An interactive session: `stone` with no file reads entries one at a time, runs each as soon as
  it is complete, and prints the value of an expression on its own. Variables, functions, and
  modules from earlier entries stay available, and a function can be defined again.
- `stone build` compiles for arm64 Linux as well as x86-64, targeting the host by default.
  `--target x86_64` or `--target aarch64` (also `x64` and `arm64`) cross-compiles, using
  `x86_64-linux-gnu-gcc` or `aarch64-linux-gnu-gcc` to assemble and link. Both backends print the
  same output and runtime errors as `stone run`.
- The builtin `os` module, which needs no file: `os.env`, `os.has_env`, `os.platform`, `os.arch`,
  `os.hostname`, `os.cpu_count`, `os.pid`, `os.cwd`, `os.exit`, `os.time`, and `os.clock`. Use it
  as `use os`, `use os.env`, or `use os as system`.
- `stone uninstall` deletes the `stone` binary after asking `[y/N]`, or right away with `--yes`
  (required without a terminal). A `stone` installed with `cargo install` is left alone, with a
  pointer to `cargo uninstall stone`.

### Changed

- `stone self-update` is now `stone update`, with the same `--check` and `--version` options.
- A program can no longer have a module of its own named `os`. An `os.st` in the program's
  directory is the error `'os' is a builtin module, so rename os.st`.

## [0.1.2] - 2026-10-06

### Added

- Modules. A program's entry file can `use` other `.st` files, found from its directory, so
  `use geometry.shapes` loads `geometry/shapes.st` and `shapes.area()` calls its `area`.
  `use geometry.shapes.area` binds a single function, `as` renames either, and only `pub def`
  functions can be used from another file. Library modules hold only `use` and `def`, so using one
  runs nothing, and modules may use each other in a cycle. Comment lines at the top of a module's
  file, followed by a blank line, document it.
- `stone run`, `stone build`, and `stone check` read and check every module the entry file uses,
  and report errors in any of them with that file's name.

### Changed

- Compiled programs free strings and lists when they are no longer used, through reference
  counting, instead of keeping every allocation until the program exits.

### Removed

- The undocumented `del` statement, which only unbound a name in the interpreter.

## [0.1.1] - 2026-10-05

### Added

- The `%` and `**` operators on ints and floats. `%` takes the dividend's sign, like `/`, and
  `**` takes an int exponent and groups right to left. `x % 0` is `division by zero`, and an int
  raised to a negative power is the runtime error `negative exponent`.
- Input and output: `input()` reads a line from standard input, `eof()` says whether it has
  ended, and `args()` returns the program's command-line arguments, as `stone run file.st a b` or
  a built `./file a b`.
- `str(x)` converts an int, float, or bool to a string, and `int(s)` and `float(s)` parse one,
  stopping the program with Python's error message if the text is not a number.
- The string methods `s.strip()` and `s.split()` (on whitespace) or `s.split(sep)`.
- A logo.

### Changed

- Comments start with `//` instead of `#`.
- `len` and `append` are methods: write `xs.len()` and `xs.append(x)` instead of `len(xs)` and
  `append(xs, x)`. `s.len()` works on strings too.

## [0.1.0] - 2026-10-04

### Added

- The first release of stone, a statically checked language with Python-like syntax that
  minimizes Shift-key use: blocks open with `;`, functions return with `ret`, and loops skip
  ahead with `cont`.
- Types `int`, `float`, `bool`, `str`, `none`, and `list[T]`, all inferred, with builtins
  `print`, `len`, `range`, `append`, `int`, and `float`.
- `stone run` interprets a program, and `stone build` compiles it to a native x86-64 Linux
  executable through an IR and a linear-scan register allocator, with `gcc` assembling and
  linking it. Both print the same output, including float formatting and runtime error messages.
- `stone check` reports every syntax and type error without running the program, as
  `file:line:col: error: message` with the source line and a caret.
- `stone self-update` replaces the binary with a newer release after verifying its checksum.
- Prebuilt binaries for x86-64 and arm64 Linux and macOS, and an `install.sh` that installs the
  right one into `~/.local/bin`.
- Documentation, with a tutorial and a language reference, at
  [tarolling.github.io/stone](https://tarolling.github.io/stone/).
