// prints only what is the same on every machine the tests run on
use os

platform = os.platform()
print(platform == "linux" or platform == "macos")
arch = os.arch()
print(arch == "x86_64" or arch == "aarch64")
print(os.hostname().len() > 0, os.hostname() == os.hostname())
print(os.cpu_count() >= 1, os.pid() > 0, os.pid() == os.pid())
print(os.cwd().len() > 0, os.cwd() == os.cwd())
