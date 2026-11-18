# Running code on the GPU

A program built with `--gpu` runs its arithmetic on whole lists on an NVIDIA GPU, with the same
results it gives on the processor. Nothing in the program changes: each piece of arithmetic such
as `xs * 2.0 + ys` becomes a kernel.

## What you need

- An NVIDIA GPU and its driver. Linux executables speak to the driver themselves, with no
  libcuda, and so far know driver version 595 and the Ampere (`sm_86`) and Ada
  (`sm_89`) generations. Windows executables use `nvcuda.dll` and run on any GPU it supports.
  `nvidia-smi` shows the driver version, and
  `nvidia-smi --query-gpu=compute_cap --format=csv` the GPU's compute capability (8.9 is
  `sm_89`).
- NVIDIA's `ptxas`, which compiles the kernels to machine code. Linux executables need it, since
  they run only machine code; Windows ones can leave the PTX to the driver. It needs no GPU and
  comes in a Python wheel:

  ```sh
  python3 -m venv ~/.local/share/stone-gpu/venv
  ~/.local/share/stone-gpu/venv/bin/pip install nvidia-cuda-nvcc-cu12
  export STONE_PTXAS=$(echo ~/.local/share/stone-gpu/venv/lib/python3.*/site-packages/nvidia/cuda_nvcc/bin/ptxas)
  ```

  A `ptxas` on `PATH` works too, so `STONE_PTXAS` is only needed when it is elsewhere.

## Building

Take a program that works on a million numbers:

```stone
n = 1000000
xs = []
ys = []
for i in range(n):
    xs.append(float(i))
    ys.append(float(n - i))
zs = xs * 2.0 + ys
print(zs[0], zs[1], zs[n - 1])
total = 0.0
for z in zs:
    total += z
print(total)
```

Build it for your GPU with `--gpu` (give it more than once for several GPUs):

```console
$ stone build saxpy.stn --gpu sm_89
Compiled build/saxpy
$ ls build
saxpy  saxpy.s  saxpy.sm_89.cubin  saxpy.sm_89.ptx
```

The `.ptx` and `.cubin` files are the kernels as text and as machine code; the executable
carries both, so it is all you need to run.

## Running

```console
$ ./build/saxpy
1000000.0 1000001.0 1999999.0
1499999500000.0
```

When the program cannot use a GPU (no driver, a driver or GPU it does not know, no machine code
for the GPU), it computes on the processor instead and prints the same thing. The `STONE_GPU`
environment variable changes this:

- `STONE_GPU=off` always computes on the processor.
- `STONE_GPU=always` launches every kernel on the GPU, however short its lists.
- `STONE_GPU=require` launches every kernel as `always` does, and stops with an error at the
  arithmetic when no GPU ran it, such as
  `no GPU ran this program's kernels: NVIDIA's driver is missing or unsupported`. Use it to
  check that the GPU really runs your program:

  ```console
  $ STONE_GPU=require ./build/saxpy
  1000000.0 1000001.0 1999999.0
  1499999500000.0
  ```

On Linux, `strace` shows the program opening the driver's devices:

```console
$ strace -e trace=openat ./build/saxpy 2>&1 | grep nvidia
openat(AT_FDCWD, "/dev/nvidiactl", O_RDWR|O_CLOEXEC) = 3
openat(AT_FDCWD, "/dev/nvidia-uvm", O_RDWR|O_CLOEXEC) = 4
...
```

A zero divisor in a float division is still reported at the element and position `stone run`
reports, since the processor computes again any arithmetic in which the GPU found one.

## When the GPU computes

A GPU computes arithmetic on lists far faster than a processor, but reaching it costs time: a
launch takes microseconds to start and finish, and a list must cross to the GPU and back. For
one piece of arithmetic such as `xs * 2.0 + ys`, those copies alone take about as long as the
processor takes to compute it, so stone decides where each piece runs:

- `stone build` records, for each kernel, how many bytes an element moves each way and how long
  an element takes on the processor (counting how many its vector instructions compute at a
  time at the target's level) and on the GPU.
- The first time the program reaches such arithmetic, it starts the GPU and measures it: the
  time a launch that does nothing takes, and how fast lists move. A GPU that cannot finish that
  test in time, or moves lists too slowly, counts as one the program cannot use.
- From both, it works out for each kernel the shortest list worth a launch. Before each launch
  the program compares the list's length with it, so a short list costs one compare and never a
  call.

Under `STONE_GPU` unset, a kernel whose element is cheaper on the processor than the time it
takes to move it never runs on the GPU, however long its lists. `STONE_GPU=always` and
`STONE_GPU=require` launch every kernel anyway.

## Lists that stay on the GPU

What makes a GPU pay is computing on lists that are already there. stone keeps a list on the
GPU while the code only computes on it:

- through a run of statements of arithmetic on lists, such as `a = xs * 2.0 + ys` then
  `b = a * a`, where `a` never comes back to the processor;
- through every turn of a loop whose body only computes on those lists, such as
  `for _ in range(k): x = x * 0.99 + v`, where `x` and `v` cross once.

Each list comes back once, where the program reads it next, and a list no later statement reads
does not come back at all. Anything else that names one of those lists, such as `print(x[0])`
or `x.append(1.0)`, ends the stretch before it, and so, in top-level code, does any call, since
a function may read a global. The program decides once, when it reaches the stretch, whether
keeping its lists on the GPU is worth it. If the GPU stops computing partway, for a zero divisor
or anything else, the program brings every list back and computes the rest on the processor.

On Linux the lists kept on the GPU live in memory the GPU reads over the bus, not yet in its
own, so keeping them there saves copies but each kernel still reads them over the bus.

To test all of this on a machine without a GPU, build with `STONE_GPU_SIMULATE=1`: the program
then computes each kernel on the processor, as the GPU would, and runs everything around it as
it does with a GPU. Run it with `STONE_GPU=require` to send every kernel there.

## Limits

- Only arithmetic on whole lists of ints or floats (`+`, `-`, `*`, float `/`, and negation)
  runs on the GPU; everything else runs on the processor.
- Windows executables still launch every such piece of arithmetic, however short its lists,
  and keep no list on the GPU between launches.
- `stone run` never uses a GPU, and macOS executables carry no GPU code.
