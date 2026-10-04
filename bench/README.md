# stone benchmarks

This crate times stone's compiled output (`stone build`) against the same programs written in C and Python. The interpreter is not benchmarked.

```sh
cargo run --release --manifest-path bench/Cargo.toml -- [--runs 5] [--filter NAME] [--stone PATH] [--min]
```

For each benchmark, the runner builds stone in release mode, compiles the `.st` with `stone build` and the `.c` with `gcc -O2 -fwrapv` and `gcc -O0 -fwrapv`, and runs the `.py` with `python3`. It runs each implementation once untimed and requires its stdout to match the `.out` file exactly. It then times `--runs` more runs, checking the output of each, and prints a Markdown table of the median wall times, or the fastest with `--min`. Progress and warnings go to stderr, so `> results.md` captures only the table. It needs `gcc` and `python3` on `PATH`.

`cargo test --manifest-path bench/Cargo.toml` checks the statistics and table code, and that every benchmark still passes stone's checker, without running any workloads.

## Reading the table

- `stone / C -O2` is how many times slower compiled stone is than optimized C. Lower is better for stone.
- `Python / stone` is how many times faster compiled stone is than CPython. Higher is better for stone.
- `C -O0` is a useful anchor. Like stone's backend, gcc at `-O0` keeps every variable in memory and does no optimization, but it still picks better instructions.
- The geometric mean row summarizes the ratios, treating 2x faster and 2x slower symmetrically.
- Times include process startup, which is about 1 ms for the native programs and tens of milliseconds for Python.

## Benchmarks

| name | what it stresses | workload |
| --- | --- | --- |
| `fib` | calls and returns | recursive `fib(35)` |
| `lcg` | integer arithmetic and division | 20 million steps of a linear congruential generator |
| `globals` | reading globals from a function | `lcg`, but with its constants in globals |
| `collatz` | branches and division | the longest Collatz chain for starts below 300,000 |
| `sieve` | list append, indexed load and store | counting primes up to 5,000,000 |
| `matmul` | nested list indexing | multiplying two 300x300 `list[list[int]]` |
| `matmul_float` | float arithmetic and nested list indexing | multiplying two 300x300 `list[list[float]]` |

Stone programs cannot read arguments or input, so each workload size is a constant written into all three versions. Sizes are chosen so `gcc -O2` takes at least about 40 ms and the whole suite finishes in a few minutes. The runner warns when any time is under 10 ms, which usually means gcc optimized the work away.

The versions are meant to be written naturally in each language, not to mimic each other instruction by instruction. The main difference comes from stone having no `%` operator: stone computes `a % b` as `a - (a / b) * b`, while C and Python use `%`. Every program prints only ints and keeps values nonnegative and within 64 bits, so truncating and floor division, and Python's big ints, give the same output. `matmul_float` prints its float checksum scaled by a million and converted to an int, so C need not print floats the way stone does, and it adds in the same order in all three versions, so the floats round identically.

To add a benchmark, write `name.st`, `name.c`, and `name.py`, put the hot loop inside a function, and save the shared output as `name.out`.

## Baseline

Median of 5 runs on 2026-09-29, on an Intel Core i7-10750H under WSL2, with gcc and Python 3 from the distribution, after inlining list index checks. Absolute times on a machine like this can differ by 2x between sessions, as CPU boost and background load change, so compare the ratio columns, which come from the same session, rather than times from different sessions. For steadier numbers, close other programs and pin the runner to one core with `taskset -c 2`.

| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | stone / C -O2 | Python / stone |
|---|---:|---:|---:|---:|---:|---:|
| collatz | 55.9 | 126.0 | 231.7 | 2229.9 | 4.1x | 9.6x |
| fib | 18.6 | 59.2 | 107.3 | 1130.0 | 5.8x | 10.5x |
| globals | 217.4 | 229.3 | 341.1 | 4877.8 | 1.6x | 14.3x |
| lcg | 49.3 | 113.9 | 155.2 | 5287.0 | 3.2x | 34.1x |
| matmul | 16.3 | 70.4 | 216.5 | 1930.1 | 13.3x | 8.9x |
| sieve | 76.3 | 96.8 | 155.4 | 866.3 | 2.0x | 5.6x |
| geometric mean | | | | | 3.8x | 11.6x |

Compiled stone is about 3.8x slower than `gcc -O2` and about 11.6x faster than CPython, as a geometric mean. It is slower than `gcc -O0` on every benchmark.

- `sieve` is close to C because allocating and touching 40 MB of memory dominates all three native versions.
- `globals` looks close to C only because C's version is also slow: once the modulus is a variable instead of a constant, gcc has to use `idiv` too. Compare stone's `globals` with its own `lcg` to see what reading globals costs.

### History

| change | collatz | fib | globals | lcg | matmul | sieve | geometric mean |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| first baseline | 12.2x | 4.9x | 2.3x | 9.8x | 15.8x | 1.4x | 5.6x |
| constant divisors skip checks, powers of two shift | 4.4x | 5.7x | 1.5x | 3.2x | 15.1x | 1.7x | 3.8x |
| list index checks inline instead of calling `stone.list_slot` | 4.1x | 5.8x | 1.6x | 3.2x | 13.3x | 2.0x | 3.8x |

Each cell is stone's time over `gcc -O2`'s from the same session. Benchmarks a change does not touch still move by a few tenths between rows, which shows the noise. For a single change, timing the old and new stone binaries against each other in one session is more reliable: inlining index checks made `matmul` 1.25x faster that way, and left `sieve` unchanged, since memory traffic dominates it.

## Where the time goes

The generated assembly for each benchmark is left at `bench/target/programs/<name>-stone.s`. The x64 backend is a simple stack machine, so the gap to C comes mostly from:

- **Every operation goes through memory.** Each binary operator pushes its left side, evaluates the right into `rax`, moves it to `rbx`, and pops the left side back, and every local lives in the stack frame. There is no register allocation.
- **Division by a variable uses `idiv`.** A literal divisor that is a power of two compiles to shifts, and any other literal except 0 and -1 skips the zero and overflow checks. A divisor held in a variable still gets both checks and an `idiv`, while C has no checks at all, since dividing by zero is undefined behavior there.
- **Calls do extra bookkeeping.** Every call increments `stone.call_depth`, compares it with 1,000, and decrements it on return, so recursion stays within the language's depth limit.
- **List access chases two pointers and checks bounds.** An index loads the list's header, adjusts a negative index, compares it with the length, then loads the data pointer and the element. For `list[list[int]]` that happens twice per `a[i][k]`, where C's flat array needs one load.
- **Globals are checked on every read in a function.** `globals` compared with `lcg` shows the cost of the `g.<name>.set` check.
