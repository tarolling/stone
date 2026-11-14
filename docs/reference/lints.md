# Lints

A warning points out code that is valid but probably not what was meant, such as a change that
is lost or a statement that never runs. The program still runs, and `stone check` still passes.
Each kind of warning is a lint, with an id that a warning shows after its severity, as in
`main.stn:2:5: warning[W0004]: changes to 'xs' are lost when 'push' returns`, and a name, such as
`lost_change`. `stone explain W0004` (or `stone explain lost_change`) prints what a lint finds,
with an example and its fix, and editors link each warning to its lint below.

`stone run`, `stone build`, and `stone test` print warnings before they go on, except
[W0001](#w0001) and [W0002](#w0002), unused names and imports, which only `stone check` and
editors report, so a program under construction runs quietly. No warning is reported in stone's
own modules or in the packages a program uses.

## Levels

A program's `stone.toml` sets how each lint is reported in a `[lints]` table, by name:

```toml
[lints]
unused_name = "allow"
lost_change = "deny"
```

`"allow"` reports nothing, `"warn"`, the default, reports a warning, and `"deny"` reports an
error, which keeps the program from running or building and still shows the lint's id, as in
`error[W0004]`. The name `warnings` sets every lint the table does not name, so
`warnings = "deny"` with `unused_name = "warn"` denies every lint but that one. A name that is
no lint is an error at its key.

`stone check --deny warnings` reports every warning that is left as an error, so that CI fails on
any of them.

(w0001)=
## W0001: a name that is never used

Name: `unused_name`

A variable, parameter, function, method, struct, enum, trait, or constant is defined, but
nothing uses it, which often means a typo or code left behind by a change. Remove it, or, for a
variable or parameter meant to go unused, start its name with `_`, as in `_h`, which is never
reported. A `pub` definition is never reported either, since other modules may use it, and
neither are the parameters of a trait's methods, which each type's own method may need.

Only `stone check` and editors report this, so a program under construction runs quietly.

This program warns with W0001:

```stone
fn area(w: int, h: int) -> int:
    ret w * w

print(area(3, 4))
```

This one is fixed:

```stone
fn area(w: int, h: int) -> int:
    ret w * h

print(area(3, 4))
```

(w0002)=
## W0002: an import that is never used

Name: `unused_import`

A `use` binds a module, or a function, type, or constant of one, that nothing in the file
uses. Remove the `use`, or start the name it binds with `_` (`use math as _math`).

Only `stone check` and editors report this, so a program under construction runs quietly.

This program warns with W0002:

```stone
use math

print(1 + 2)
```

This one is fixed:

```stone
print(1 + 2)
```

(w0003)=
## W0003: a list literal multiplied by an int

Name: `list_repetition`

`*` between a list and a number multiplies each element of the list, so `[0] * 3` is `[0]`, a
list of one element, and not three copies of it as in Python. To build a longer list, append to it
in a loop.

This program warns with W0003:

```stone
zeros = [0] * 3
print(zeros)
```

This one is fixed:

```stone
zeros = []
for _ in range(3):
    zeros.append(0)
print(zeros)
```

(w0004)=
## W0004: a change to a parameter that is lost

Name: `lost_change`

A function changes a parameter, such as by appending to a list, but never reads it again or
returns it. Parameters are copies of the values a function is called with, so the change is lost
when the function returns, and the caller's value stays as it was. Return the changed value and
assign it where the function is called, as in `xs = push(xs)`, which changes the list in place
without copying it.

This program warns with W0004:

```stone
fn push(xs: list[int]):
    xs.append(1)

nums = [0]
push(nums)
print(nums)
```

This one is fixed:

```stone
fn push(xs: list[int]) -> list[int]:
    xs.append(1)
    ret xs

nums = [0]
nums = push(nums)
print(nums)
```

(w0005)=
## W0005: a case that never runs

Name: `unreachable_case`

The cases before a case of a `match` already match every value its pattern would, so it never
runs, or an alternative of a `|` pattern never matches for the same reason. A `match` tries its
cases in order, so put the more particular case first, or remove the one that never runs.

This program warns with W0005:

```stone
x = 3
match x:
    case _:
        print("any")
    case 3:
        print("three")
```

This one is fixed:

```stone
x = 3
match x:
    case 3:
        print("three")
    case _:
        print("any")
```

(w0006)=
## W0006: code that never runs

Name: `unreachable_code`

A statement comes after one that always leaves the block it is in: a `ret`, `break`, or `cont`,
an `if` or `match` each of whose branches leaves, or a `while true:` loop with no `break`. Nothing
after it in the block ever runs. Move the code before the statement that leaves, or remove it.
Only the first statement that never runs is reported, and definitions, such as a `fn` after a
loop that never ends, are not, since a definition takes effect wherever it is written.

This program warns with W0006:

```stone
fn first_even(xs: list[int]) -> int:
    for x in xs:
        if x % 2 == 0:
            ret x
            print("found it")
    ret -1

print(first_even([3, 4]))
```

This one is fixed:

```stone
fn first_even(xs: list[int]) -> int:
    for x in xs:
        if x % 2 == 0:
            print("found it")
            ret x
    ret -1

print(first_even([3, 4]))
```

(w0007)=
## W0007: a result that is never used

Name: `ignored_result`

A call that gives a `result` is a statement of its own, so if it fails, its `err` is dropped
and the program goes on as if it had worked, as when a file cannot be written. Add `?` to return
the error from the function, use `.unwrap()` to stop the program with it, or `match` on the
result to handle it. To ignore it on purpose, assign it to a name that starts with `_`, which is
never reported as unused.

This program warns with W0007:

```stone
use fs

fs.write("notes.txt", "hello")
print("saved")
```

This one is fixed:

```stone
use fs

fs.write("notes.txt", "hello").unwrap()
print("saved")
```

(w0008)=
## W0008: an expression whose value is never used

Name: `useless_expression`

A statement is an expression that calls nothing, such as `x + 1` or `count == 1`, so it
computes a value and then drops it. Often `==` was written for `=`, which assigns; otherwise use
the value or remove the statement. A string alone on a line is not a comment in stone: comments
start with `//`.

This program warns with W0008:

```stone
count = 0
count == 1
print(count)
```

This one is fixed:

```stone
count = 0
count = 1
print(count)
```

(w0009)=
## W0009: a conversion to the type a value already has

Name: `identity_conversion`

A conversion such as `int(n)` or `str(s)` is given a value of the type it converts to, so it
gives the value back unchanged. Use the value itself.

This program warns with W0009:

```stone
fn shout(s: str) -> str:
    ret str(s).upper()

print(shout("hi"))
```

This one is fixed:

```stone
fn shout(s: str) -> str:
    ret s.upper()

print(shout("hi"))
```

(w0010)=
## W0010: a bool compared with true or false

Name: `bool_comparison`

`done == true` is `done`, and `done == false` is `not done`, so the comparison says nothing the
bool does not. Write the bool, or `not` and the bool.

This program warns with W0010:

```stone
done = false
if done == false:
    print("working")
```

This one is fixed:

```stone
done = false
if not done:
    print("working")
```

(w0011)=
## W0011: a value compared with itself

Name: `self_comparison`

Both sides of a comparison are the same int, bool, str, or bytes, written alike, so its answer is
always the same: `x == x` is always true and `x < x` always false. One side is usually a typo for
another value. Floats are not reported, since `x != x` is true for a nan.

This program warns with W0011:

```stone
fn is_sorted(xs: list[int]) -> bool:
    for i in range(1, xs.len()):
        if xs[i] < xs[i]:
            ret false
    ret true

print(is_sorted([1, 2, 3]))
```

This one is fixed:

```stone
fn is_sorted(xs: list[int]) -> bool:
    for i in range(1, xs.len()):
        if xs[i] < xs[i - 1]:
            ret false
    ret true

print(is_sorted([1, 2, 3]))
```

(w0012)=
## W0012: a variable or place assigned to itself

Name: `self_assignment`

An assignment such as `x = x` or `p.x = p.x` gives a place the value it already holds, so it
changes nothing, and the value meant was usually another. `self = self` in a method is not
reported, since it is how a method says it changes `self`, as a trait's default method may.

This program warns with W0012:

```stone
fn larger(a: int, b: int) -> int:
    best = a
    if b > a:
        best = best
    ret best

print(larger(1, 2))
```

This one is fixed:

```stone
fn larger(a: int, b: int) -> int:
    best = a
    if b > a:
        best = b
    ret best

print(larger(1, 2))
```

(w0013)=
## W0013: a key given twice in a literal

Name: `duplicate_key`

A map literal gives the same key twice, so the value given later replaces the one given first,
which is lost; or a set literal gives the same element twice. Keys are compared as constants,
so `{k: 1, "a": 2}` with `const k = "a"` is reported too.

This program warns with W0013:

```stone
ages = {"ann": 31, "bob": 27, "ann": 32}
print(ages)
```

This one is fixed:

```stone
ages = {"ann": 31, "bob": 27, "cy": 32}
print(ages)
```

(w0014)=
## W0014: a condition tested twice in one chain

Name: `repeated_condition`

An `elif` tests a condition written exactly like one an `if` or `elif` before it in the same
chain tested. If it were true, the earlier branch would have run instead, so this branch never
runs. A condition that calls a function is not reported, since a call may answer differently each
time.

This program warns with W0014:

```stone
fn grade(score: int) -> str:
    if score >= 90:
        ret "A"
    elif score >= 80:
        ret "B"
    elif score >= 80:
        ret "C"
    ret "F"

print(grade(85))
```

This one is fixed:

```stone
fn grade(score: int) -> str:
    if score >= 90:
        ret "A"
    elif score >= 80:
        ret "B"
    elif score >= 70:
        ret "C"
    ret "F"

print(grade(85))
```

(w0015)=
## W0015: an operation that fails whenever it runs

Name: `always_fails`

An int or float is divided by zero, or its remainder taken by zero, or an int is shifted by a
count outside 0 to 63, and the right side is a constant, so the operation fails with a runtime
error every time it runs. It is a warning rather than an error since the code may never run. An
operation of literals alone, as `1 / 0`, is not reported, since it is written so on purpose.

This program warns with W0015:

```stone
fn half(total: int) -> int:
    ret total / 0

print(half(10))
```

This one is fixed:

```stone
fn half(total: int) -> int:
    ret total / 2

print(half(10))
```
