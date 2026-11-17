#!/usr/bin/env python3
"""Runs a stone kernel on an NVIDIA GPU through raw ioctls to /dev/nvidia*, with no CUDA library.

This is the sequence the `gpu_nv` runtime module speaks, written with the Python standard library
alone so it can be tried and changed quickly: a root client, the GPU's device and subdevice, the
usermode doorbell, a VA space registered with UVM, host memory mapped for the GPU, a channel group
with a GPFIFO channel and the compute class, then constant bank 0, a QMD (version 3, Ampere and
Ada), and a push buffer that launches the kernel and signals a semaphore. The struct layouts are
those of NVIDIA's open-gpu-kernel-modules for driver 595.

The kernel is `stone_k0` of the saxpy program `y = a * x + y`, whose parameters are the result,
the length, `a`, `x`, and `y`. Build one with ptxas on PATH (or STONE_PTXAS naming it):

    stone build --gpu sm_89 saxpy.stn        # writes build/saxpy.sm_89.cubin

Usage: python3 scripts/nv-probe.py CUBIN [--dump DIR]

It prints `ok` when the GPU computed what the processor does. `--dump DIR` writes the constant
bank, the QMD, and each push buffer, as words, for comparing with what the runtime writes.
"""

import argparse
import ctypes
import mmap
import os
import struct
import sys
import time

libc = ctypes.CDLL(None, use_errno=True)
libc.ioctl.argtypes = [ctypes.c_int, ctypes.c_ulong, ctypes.c_void_p]
libc.ioctl.restype = ctypes.c_int
libc.mmap.argtypes = [
    ctypes.c_void_p,
    ctypes.c_size_t,
    ctypes.c_int,
    ctypes.c_int,
    ctypes.c_int,
    ctypes.c_long,
]
libc.mmap.restype = ctypes.c_void_p

PROT_RW = 3
MAP_SHARED = 1
MAP_FIXED = 0x10
MAP_ANONYMOUS = 0x20

# classes, newest first
USERMODE = [0xC661, 0xC461]
GPFIFO = [0xC96F, 0xC56F]
COMPUTE = [0xC9C0, 0xC7C0]  # Ada, Ampere: QMD version 3
BLOCK = 256
# what the driver gives every launch in constant bank 0 after the dimensions
SHARED_WINDOW = 0x729300000000
LOCAL_WINDOW = 0x729400000000


def round_up(x, a):
    return (x + a - 1) // a * a


class Buf:
    """A zeroed byte buffer with little-endian field access."""

    def __init__(self, size):
        self.b = ctypes.create_string_buffer(size)
        self.size = size

    def addr(self):
        return ctypes.addressof(self.b)

    def set(self, fmt, off, *v):
        struct.pack_into("<" + fmt, self.b, off, *v)
        return self

    def get(self, fmt, off):
        return struct.unpack_from("<" + fmt, self.b, off)[0]


def ioctl(fd, req, buf):
    if libc.ioctl(fd, req, buf.addr()) != 0:
        e = ctypes.get_errno()
        raise OSError(e, f"ioctl {req:#x}: {os.strerror(e)}")


def rm_request(nr, size):
    return (3 << 30) | ((size & 0x1FFF) << 16) | (ord("F") << 8) | nr


def fence():
    """Returns a function that runs a full memory barrier, so the doorbell follows the ring."""
    page = mmap.mmap(-1, 4096, prot=mmap.PROT_READ | mmap.PROT_WRITE | mmap.PROT_EXEC)
    machine = os.uname().machine
    if machine == "x86_64":
        page.write(bytes([0x0F, 0xAE, 0xF0, 0xC3]))  # mfence; ret
    else:
        page.write(struct.pack("<II", 0xD5033BBF, 0xD65F03C0))  # dmb ish; ret
    function = ctypes.CFUNCTYPE(None)(ctypes.addressof(ctypes.c_char.from_buffer(page)))
    function.page = page
    return function


def method(subchannel, offset, *values):
    """Returns a push buffer method writing `values` from `offset` on, incrementing."""
    return [(2 << 28) | (len(values) << 16) | (subchannel << 13) | (offset >> 2), *values]


def high_first(v):
    return [(v >> 32) & 0xFFFFFFFF, v & 0xFFFFFFFF]


def low_first(v):
    return [v & 0xFFFFFFFF, (v >> 32) & 0xFFFFFFFF]


class Memory:
    def __init__(self, va, size, handle):
        self.va, self.size, self.handle = va, size, handle


class Gpu:
    def __init__(self):
        self.memory = 0x1000000000
        self.handle = 0x1000
        self.dumps = {}
        self.barrier = fence()
        self.ctl = os.open("/dev/nvidiactl", os.O_RDWR | os.O_CLOEXEC)
        self.uvm_fd = os.open("/dev/nvidia-uvm", os.O_RDWR | os.O_CLOEXEC)
        self.root = 0
        self.root = self.alloc_object(0, 0x41)

        version = self.control(self.root, 0x13E, Buf(1032))
        print("driver", ctypes.string_at(version.addr()).decode())

        self.uvm(0x30000001, Buf(16), 8)
        # UVM's memory manager is a second file of the same device
        manager = os.open("/dev/nvidia-uvm", os.O_RDWR | os.O_CLOEXEC)
        ioctl(manager, 0x4B, Buf(8).set("i", 0, self.uvm_fd))

        cards = Buf(0x1200)
        ioctl(self.ctl, rm_request(0xC8, 0x1200), cards)
        card = next(i for i in range(64) if cards.get("B", i * 72))
        gpu_id, minor = cards.get("I", card * 72 + 16), cards.get("I", card * 72 + 56)
        self.dev = self.device_file(minor)

        info = self.control(self.root, 0x205, Buf(32).set("I", 0, gpu_id))
        self.device = self.alloc_object(
            self.root, 0x80, Buf(56).set("II", 0, info.get("I", 8), self.root)
        )
        self.subdevice = self.alloc_object(self.device, 0x2080, Buf(4))
        self.virtmem = self.alloc_object(self.device, 0x70, Buf(24).set("Q", 8, 0x1FFFFFFFFFFFF))

        count = self.control(self.device, 0x800201, Buf(16)).get("I", 0)
        listed = Buf(count * 4)
        self.control(self.device, 0x800201, Buf(16).set("IIQ", 0, count, 0, listed.addr()))
        classes = {listed.get("I", 4 * i) for i in range(count)}

        def pick(candidates, what):
            found = [c for c in candidates if c in classes]
            if not found:
                raise RuntimeError(f"this GPU has no {what} class stone knows")
            return found[0]

        self.compute = pick(COMPUTE, "compute")
        usermode = self.alloc_object(self.subdevice, pick(USERMODE, "usermode"))
        self.doorbell = self.map_object(usermode, 0x10000, minor) + 0x90

        space = Buf(56).set("IIQ", 0, 0, 0x48, 0x1FFFFFB000000).set("Q", 40, 0x1000)
        self.vaspace = self.alloc_object(self.device, 0x90F1, space)
        uuid = self.control(self.subdevice, 0x2080014A, Buf(268).set("III", 0, 0, 2, 16))
        self.uuid = bytes(uuid.b.raw[12:28])
        register = Buf(40)
        register.b[0:16] = self.uuid
        self.uvm(0x25, register.set("ii", 24, -1, 0), 36)
        register = Buf(32)
        register.b[0:16] = self.uuid
        self.uvm(0x19, register.set("iII", 16, self.ctl, self.root, self.vaspace), 28)

        group = self.alloc_object(self.device, 0xA06C, Buf(20).set("I", 12, 1))
        entries = 0x10000
        area = self.alloc(0x300000)
        notifier = self.alloc(0x1000)
        share = self.alloc_object(group, 0x9067, Buf(12).set("II", 0, self.vaspace, 1))
        channel = Buf(368).set("IIQI", 0, notifier.handle, area.handle, area.va, entries)
        channel.set("I", 24, share).set("I", 32, area.handle).set("Q", 64, entries * 8)
        gpfifo = self.alloc_object(group, pick(GPFIFO, "GPFIFO"), channel)
        self.alloc_object(gpfifo, self.compute)
        token = self.control(gpfifo, 0xC36F0108, Buf(4).set("I", 0, 0xFFFFFFFF))
        self.token = token.get("I", 0)
        register = Buf(56)
        register.b[0:16] = self.uuid
        register.set("iII", 16, self.ctl, self.root, gpfifo)
        self.uvm(0x1B, register.set("QQ", 32, self.reserve(0x4000000), 0x4000000), 48)
        self.control(group, 0xA06C0101, Buf(3).set("B", 0, 1))
        self.ring, self.entries, self.put = area, entries, 0
        self.gp_put = area.va + entries * 8 + 0x8C

        items = Buf(8).set("I", 0, 12)
        self.control(self.subdevice, 0x20801201, Buf(32).set("IIQ", 0, 1, 0, items.addr()))
        self.sm = items.get("I", 4)

        self.pushes = self.alloc(0x10000)
        self.semaphore = self.alloc(0x1000)
        self.value = 1
        setup = method(1, 0x0, self.compute)
        setup += method(1, 0x7B0, *high_first(SHARED_WINDOW))
        setup += method(1, 0x2A0, *high_first(LOCAL_WINDOW))
        self.run(setup, "setup")

    def device_file(self, minor):
        fd = os.open(f"/dev/nvidia{minor}", os.O_RDWR | os.O_CLOEXEC)
        ioctl(fd, rm_request(0xC9, 4), Buf(4).set("i", 0, self.ctl))
        return fd

    def alloc_object(self, parent, cls, params=None):
        b = Buf(32).set("IIII", 0, self.root, parent, 0, cls)
        if params is not None:
            b.set("Q", 16, params.addr())
        ioctl(self.ctl, rm_request(0x2B, 32), b)
        if b.get("I", 28):
            raise RuntimeError(f"allocating class {cls:#x} gave status {b.get('I', 28):#x}")
        return b.get("I", 8)

    def control(self, obj, command, params):
        b = Buf(32).set("IIII", 0, self.root, obj, command, 0)
        b.set("QI", 16, params.addr(), params.size)
        ioctl(self.ctl, rm_request(0x2A, 32), b)
        if b.get("I", 28):
            raise RuntimeError(f"control {command:#x} gave status {b.get('I', 28):#x}")
        return params

    def uvm(self, command, b, status):
        ioctl(self.uvm_fd, command, b)
        if b.get("I", status):
            raise RuntimeError(f"UVM {command:#x} gave status {b.get('I', status):#x}")

    def map_object(self, handle, size, minor):
        fd = self.device_file(minor)
        b = Buf(56).set("III", 0, self.root, self.device, handle).set("QQ", 16, 0, size)
        b.set("i", 48, fd)
        ioctl(self.ctl, rm_request(0x4E, 56), b)
        if b.get("I", 40):
            raise RuntimeError(f"mapping gave status {b.get('I', 40):#x}")
        return libc.mmap(None, size, PROT_RW, MAP_SHARED, fd, 0)

    def reserve(self, size):
        va = round_up(self.memory, 0x1000)
        self.memory = va + size
        return va

    def alloc(self, size):
        """Returns host memory the processor and the GPU see at the same address."""
        size = round_up(size, 0x1000)
        va = self.reserve(size)
        if libc.mmap(va, size, PROT_RW, MAP_FIXED | MAP_SHARED | MAP_ANONYMOUS, -1, 0) != va:
            raise OSError(ctypes.get_errno(), "mmap failed")
        self.handle += 1
        flags = (1 << 4) | (1 << 12) | (1 << 30)
        b = Buf(56).set("IIIII", 0, self.root, self.device, self.handle, 0x71, flags)
        ioctl(self.dev, rm_request(0x27, 56), b.set("QQ", 24, va, size - 1).set("i", 48, -1))
        if b.get("I", 40):
            raise RuntimeError(f"host memory gave status {b.get('I', 40):#x}")
        self.uvm(0x49, Buf(24).set("QQ", 0, va, size), 16)
        b = Buf(64).set("IIII", 0, self.root, self.device, self.virtmem, self.handle)
        b.set("QQI", 16, 0, size, (1 << 8) | (1 << 4) | (1 << 15)).set("Q", 48, va)
        ioctl(self.ctl, rm_request(0x57, 64), b)
        if b.get("I", 56) or b.get("Q", 48) != va:
            raise RuntimeError(f"mapping for the GPU gave status {b.get('I', 56):#x}")
        external = Buf(9264).set("QQQ", 0, va, size, 0)
        external.b[24:40] = self.uuid
        external.set("I", 40, 1).set("Q", 0x2418, 1).set("iII", 0x2420, self.ctl, self.root, self.handle)
        self.uvm(0x21, external, 0x242C)
        return Memory(va, size, self.handle)

    def run(self, words, what, timeout=5.0):
        """Pushes `words` after a wait for the last value and before a signal of the next."""
        address = low_first(self.semaphore.va)
        target = self.value
        words = (
            method(0, 0x5C, *address, *low_first(target - 1), 3 | (1 << 24))
            + words
            + method(0, 0x5C, *address, *low_first(target), 1 | (1 << 20) | (1 << 24) | (1 << 25))
            + method(0, 0x20, 0)
        )
        self.dumps[what] = words
        ctypes.memmove(self.pushes.va, struct.pack(f"<{len(words)}I", *words), 4 * len(words))
        entry = ((self.pushes.va // 4) << 2) | (len(words) << 42) | (1 << 41)
        ctypes.c_uint64.from_address(self.ring.va + 8 * (self.put % self.entries)).value = entry
        self.put += 1
        ctypes.c_uint32.from_address(self.gp_put).value = self.put % self.entries
        self.barrier()
        ctypes.c_uint32.from_address(self.doorbell).value = self.token
        start = time.monotonic()
        signal = ctypes.c_uint64.from_address(self.semaphore.va)
        while signal.value < target:
            if time.monotonic() - start > timeout:
                raise TimeoutError(f"{what}: the GPU did not signal {target}")
        self.value += 1


def kernel_code(cubin, name):
    """Returns the machine code of kernel `name` and its register count from a cubin."""
    shoff = struct.unpack_from("<Q", cubin, 0x28)[0]
    size, count, names = struct.unpack_from("<HHH", cubin, 0x3A)
    sections = {}
    headers = [struct.unpack_from("<IIQQQQ", cubin, shoff + i * size) for i in range(count)]
    strings = headers[names][4]
    for name_at, _, _, _, offset, length in headers:
        title = cubin[strings + name_at : cubin.index(b"\0", strings + name_at)].decode()
        sections[title] = cubin[offset : offset + length]
    info, registers, at = sections[".nv.info"], 0, 0
    while at + 4 <= len(info):
        form, attribute, length = struct.unpack_from("<BBH", info, at)
        at += 4
        if form == 4:
            if attribute == 0x2F:
                registers = struct.unpack_from("<I", info, at + 4)[0]
            at += length
    return sections[f".text.{name}"], registers


def set_bits(qmd, high, low, value):
    width = high - low + 1
    assert value >> width == 0, f"{value:#x} does not fit bits {high}:{low}"
    first, last = low // 8, high // 8 + 1
    current = int.from_bytes(qmd[first:last], "little")
    mask = ((1 << width) - 1) << (low % 8)
    current = (current & ~mask) | (value << (low % 8))
    qmd[first:last] = current.to_bytes(last - first, "little")


def qmd_v3(sm, program, size, registers, bank, bank_size, grid):
    """Returns the 256-byte QMD that launches `grid` blocks of BLOCK threads."""
    qmd = bytearray(256)
    shared = 0x400
    config = min(c for c in (32 << 10, 64 << 10, 100 << 10) if c >= shared) // 4096 + 1
    for high, low, value in [
        (583, 580, 3),  # QMD major version
        (134, 134, 1),  # API visible call limit: no check
        (133, 128, 0x3F),  # sampler index: via header index
        (1567, 1536, program & 0xFFFFFFFF),
        (1584, 1568, program >> 32),
        (287, 256, (program >> 8) & 0xFFFFFFFF),  # prefetch
        (1640, 1632, program >> 40),
        (1649, 1641, min(size >> 8, 0x1FF)),
        (561, 544, shared),
        (656, 648, max(registers, 16)),
        (1623, 1600, 0),  # local memory per thread
        (186, 186, 1),  # invalidate caches
        (187, 187, 1),
        (188, 188, 1),
        (189, 189, 1),
        (378, 378, 1),  # release membar: sys
        (382, 382, 1),
        (767, 763, 1),
        (369, 368, 1),
        (567, 562, config),
        (662, 657, config),
        (574, 569, 0x1A),
        (1663, 1656, ((sm & 0xF00) >> 4) | (sm & 0xF)),
        (415, 384, grid),
        (431, 416, 1),
        (463, 448, 1),
        (607, 592, BLOCK),
        (623, 608, 1),
        (639, 624, 1),
        (1074, 1074, 1),
        (640, 640, 1),  # constant bank 0
        (1055, 1024, bank & 0xFFFFFFFF),
        (1072, 1056, bank >> 32),
        (1087, 1075, bank_size),
    ]:
        set_bits(qmd, high, low, value)
    return bytes(qmd)


def main():
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("cubin")
    parser.add_argument("--dump")
    options = parser.parse_args()
    code, registers = kernel_code(open(options.cubin, "rb").read(), "stone_k0")

    gpu = Gpu()
    program = gpu.alloc(len(code))
    ctypes.memmove(program.va, code, len(code))
    n, a = 1000, 0.5
    xs = [float(i % 7) for i in range(n)]
    ys = [float(i % 5) for i in range(n)]
    out, x, y = gpu.alloc(8 * n), gpu.alloc(8 * n), gpu.alloc(8 * n)
    ctypes.memmove(x.va, struct.pack(f"<{n}d", *xs), 8 * n)
    ctypes.memmove(y.va, struct.pack(f"<{n}d", *ys), 8 * n)

    grid = min(-(-n // BLOCK), 0xFFFF)
    # the dimensions a grid-stride loop reads, then the windows, then the parameters at 0x160
    words = [BLOCK, 1, 1, grid, 1, 1]
    words += low_first(LOCAL_WINDOW) + low_first(SHARED_WINDOW) + low_first(0xFFFDC0)
    bank = struct.pack("<12I", *words).ljust(0x160, b"\0")
    bank += struct.pack("<QQdQQ", out.va, n, a, x.va, y.va)
    launch = gpu.alloc(0x1000)
    ctypes.memmove(launch.va, bank, len(bank))
    qmd = qmd_v3(gpu.sm, program.va, len(code), registers, launch.va, len(bank), grid)
    ctypes.memmove(launch.va + 0x200, qmd, len(qmd))

    words = method(1, 0x1698, 1 | (1 << 4) | (1 << 12))  # invalidate shader caches
    words += method(1, 0x2B4, (launch.va + 0x200) >> 8)  # send the QMD
    words += method(1, 0x2C0, 9)
    start = time.monotonic()
    gpu.run(words, "launch")
    print(f"the kernel ran in {(time.monotonic() - start) * 1e3:.2f} ms")

    got = struct.unpack(f"<{n}d", ctypes.string_at(out.va, 8 * n))
    if options.dump:
        os.makedirs(options.dump, exist_ok=True)
        with open(os.path.join(options.dump, "bank0.bin"), "wb") as f:
            f.write(bank)
        with open(os.path.join(options.dump, "qmd.bin"), "wb") as f:
            f.write(qmd)
        for what, pushed in gpu.dumps.items():
            with open(os.path.join(options.dump, f"{what}.txt"), "w") as f:
                f.write("".join(f"{word:#010x}\n" for word in pushed))
    wrong = [i for i in range(n) if got[i] != a * xs[i] + ys[i]]
    if wrong:
        i = wrong[0]
        print(f"{len(wrong)} elements differ, the first {i}: {got[i]} != {a * xs[i] + ys[i]}")
        sys.exit(1)
    print("ok")


if __name__ == "__main__":
    main()
