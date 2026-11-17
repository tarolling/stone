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
- `STONE_GPU=require` stops with an error at the arithmetic when no GPU ran it, such as
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

## Limits

- Only arithmetic on whole lists of ints or floats (`+`, `-`, `*`, float `/`, and negation)
  runs on the GPU; everything else runs on the processor.
- Every such piece of arithmetic goes to the GPU, however short its lists, so a program with
  only small lists may be slower than on the processor.
- `stone run` never uses a GPU, and macOS executables carry no GPU code.
