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

Median of 5 runs on 2026-10-04, on an Intel Core i7-10750H under WSL2, with gcc and Python 3 from the distribution, after adding register allocation. Absolute times on a machine like this can differ by 2x between sessions, as CPU boost and background load change, so compare the ratio columns, which come from the same session, rather than times from different sessions. For steadier numbers, close other programs and pin the runner to one core with `taskset -c 2`.

| program | C -O2 (ms) | C -O0 (ms) | stone (ms) | Python (ms) | stone / C -O2 | Python / stone |
|---|---:|---:|---:|---:|---:|---:|
| collatz | 62.7 | 132.5 | 113.5 | 2430.3 | 1.8x | 21.4x |
| fib | 20.8 | 64.7 | 77.2 | 1256.5 | 3.7x | 16.3x |
| globals | 231.1 | 258.0 | 259.7 | 5236.5 | 1.1x | 20.2x |
| lcg | 50.5 | 66.3 | 70.3 | 5103.7 | 1.4x | 72.6x |
| matmul | 19.1 | 77.4 | 84.4 | 2416.2 | 4.4x | 28.6x |
| matmul_float | 28.0 | 75.9 | 93.9 | 1434.8 | 3.4x | 15.3x |
| sieve | 112.9 | 128.8 | 109.2 | 1006.4 | 1.0x | 9.2x |
| geometric mean | | | | | 2.0x | 21.4x |

Compiled stone is about 2.0x slower than `gcc -O2` and about 21x faster than CPython, as a geometric mean. It is faster than `gcc -O0` on `collatz` and `sieve`, and within about 25% of it on the rest.

- `sieve` is close to C because allocating and touching 40 MB of memory dominates all three native versions.
- `globals` looks close to C only because C's version is also slow: once the modulus is a variable instead of a constant, gcc has to use `idiv` too. Compare stone's `globals` with its own `lcg` to see what reading globals costs.

### History

| change | collatz | fib | globals | lcg | matmul | sieve | geometric mean |
| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| first baseline | 12.2x | 4.9x | 2.3x | 9.8x | 15.8x | 1.4x | 5.6x |
| constant divisors skip checks, powers of two shift | 4.4x | 5.7x | 1.5x | 3.2x | 15.1x | 1.7x | 3.8x |
| list index checks inline instead of calling `stone.list_slot` | 4.1x | 5.8x | 1.6x | 3.2x | 13.3x | 2.0x | 3.8x |
| IR, linear-scan register allocation, and compare-and-branch conditions | 1.8x | 3.7x | 1.1x | 1.4x | 4.4x | 1.0x | 1.9x |

Each cell is stone's time over `gcc -O2`'s from the same session, and the geometric mean covers the six columns shown. Benchmarks a change does not touch still move by a few tenths between rows, which shows the noise. For a single change, timing the old and new stone binaries against each other in one session is more reliable: inlining index checks made `matmul` 1.25x faster that way, and left `sieve` unchanged, since memory traffic dominates it. Register allocation, timed the same way with `--stone`, made `collatz` 2.6x faster, `matmul` and `matmul_float` 2.5x, `lcg` 2.3x, `fib` 1.6x, `globals` 1.35x, and `sieve` 1.17x.

## Where the time goes

The generated assembly for each benchmark is left at `bench/target/programs/<name>-stone.s`, and `STONE_DEBUG=1` with a debug build prints each function's IR and register assignment. Values live in registers, and loop tests compare and jump directly, so the remaining gap to C comes mostly from:

- **Only five registers survive calls.** A value that lives across a call needs one of the callee-saved registers `rbx` and `r12` to `r15`, and the rest spill to the stack, the least-used first. `matmul`'s `multiply` keeps the matrices and inner counters in registers, but its outer loop counters live in memory, since `append` is called inside that loop. Each vreg also gets one location for its whole life, with no splitting around calls.
- **Division by a variable uses `idiv`.** A literal divisor that is a power of two compiles to shifts, and any other literal except 0 and -1 skips the zero and overflow checks. A divisor held in a variable still gets both checks and an `idiv`, while C has no checks at all, since dividing by zero is undefined behavior there.
- **Calls do extra bookkeeping.** Every call increments `stone.call_depth`, compares it with 1,000, and decrements it on return, so recursion stays within the language's depth limit. This is most of what separates `fib` from C.
- **List access chases two pointers and checks bounds.** An index loads the list's header, adjusts a negative index, compares it with the length, then loads the data pointer and the element. For `list[list[int]]` that happens twice per `a[i][k]`, where C's flat array needs one load.
- **Globals are checked on every read in a function.** `globals` compared with `lcg` shows the cost of the `g.<name>.set` check, plus the load itself, since globals always live in memory.
- **Loops test at the top.** Each iteration jumps back to the test, then branches into the body, where gcc puts the test at the bottom so an iteration takes one branch.
