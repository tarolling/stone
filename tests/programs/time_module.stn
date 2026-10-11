// prints only what is the same on every run and every machine
use time
use time.sleep as nap

// seconds since 1970, which passed 1.7e9 in 2023
print(time.now() > 1700000000.0)

// the clock never goes backward
start = time.clock()
total = 0
for i in range(100000);
    total = total + i
print(total, time.clock() >= start)

// sleeping takes at least as long as asked, in floats or ints
start = time.clock()
time.sleep(0.02)
print(time.clock() - start >= 0.02)
start = time.clock()
nap(0)
time.sleep(0.0)
time.sleep(-0.0)
print(time.clock() >= start)
