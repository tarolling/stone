# Testing

stone tests live next to the code they test, in test blocks, and `stone test` runs them.

## Writing tests

A test block is `test`, a description in quotes, and a block, at the top level of any file:

```stone
fn area(width: int, height: int) -> int:
    ret width * height

test "area of a square":
    assert area(3, 3) == 9

test "area of a rectangle":
    assert area(2, 4) == 6
```

`assert` stops the test with an error unless its condition holds. When the condition is a single
comparison, the error shows both sides, so a wrong expectation such as the second test's reads

```text
shapes.stn:8:12: error: assertion failed: 8 == 6
  |
8 |     assert area(2, 4) == 6
  |            ^^^^^^^^^^^^^^^
```

A message after a comma replaces the sides: `assert total > 0, "total is {total}"`. A test
passes if it runs to its end without an error. It may call anything the file defines or uses,
print, and fail in any way a program can: an index out of range or an `unwrap` of an `err` fails
it just as an `assert` does.

Each description is a plain string, without values in braces, and no two tests of a file share
one. `test` is a keyword only where a test block opens, so it may still name a variable or a
function.

## Running tests

```sh
stone test
```

finds every file with test blocks under the current directory and runs its tests. Give files or
directories to test only those, and `--filter` to run only the tests whose description contains
some text:

```sh
stone test shapes.stn geo
stone test --filter "area of"
```

The report lists each file's tests, then every failure with what the test printed and its error,
then a count:

```text
shapes.stn
  ok      area of a square
  FAILED  area of a rectangle

failures:

shapes.stn: area of a rectangle
shapes.stn:8:12: error: assertion failed: 8 == 6
  |
8 |     assert area(2, 4) == 6
  |            ^^^^^^^^^^^^^^^

1 passed, 1 failed in 1 file
```

`stone test` exits with status 1 if any test failed or any file has errors, and prints
`no tests found` and succeeds if there was nothing to run.

## What a test sees

Each test runs alone: none of the file's top-level statements run, and no test sees what
another one did. So a test cannot read a global, even through a function it calls, since the
code that would assign it never runs; the checker reports one that might:

```text
limits.stn:7:6: error: this test reads the global 'LIMIT' (through 'clamp'), but stone test runs no top-level code
```

Make what a test needs inside it, or pass it to the functions as a parameter. A test gets no
arguments from `args()`, and its input is empty, so `input()` gives `err("end of input")`.

`stone run` and `stone build` never run tests, and a built program holds none of their code, so
tests cost nothing outside `stone test`.

## Tests in modules and packages

A test may be in any file, a library module included. `stone test` checks each file as part of
the program it belongs to (the nearest directory with a `main.stn` or a `stone.toml`), so its
`use`s resolve as they do when the program runs. A package's tests are its own: a program that
uses the package never checks or builds them.

## Compiled tests

```sh
stone test --build
```

compiles each file with test blocks into a native executable under the `build/test/` directory
of its program and runs each test as a run of it. A compiled program and the interpreter agree
on everything, so the report is the same; `--build` checks that they do for your code, and runs
tests that do a lot of work faster.
