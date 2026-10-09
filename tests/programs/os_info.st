// prints only what is the same on every machine the tests run on
use os

print(os.platform())
arch = os.arch()
print(arch == "x86_64" or arch == "aarch64")
print(os.hostname().len() > 0, os.hostname() == os.hostname())
print(os.cpu_count() >= 1, os.pid() > 0, os.pid() == os.pid())
print(os.cwd().len() > 0, os.cwd() == os.cwd())

// seconds since 1970, which passed 1.7e9 in 2023
print(os.time() > 1700000000.0)

// the clock never goes backward
start = os.clock()
total = 0
for i in range(100000);
    total = total + i
print(total, os.clock() >= start)
