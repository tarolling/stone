// prints floats whose shortest digits are hard to find, so both backends must agree on every one
print(0.0, -0.0, 1.0, -1.0, 0.1, 0.2, 0.1 + 0.2, 1.0 / 3.0, 2.0 / 3.0)
print(5e-324, 1e-323, 2.2250738585072014e-308, 2.225073858507201e-308, 1.7976931348623157e308)
print(1e15, 1e16, 123456789012345680.0, 9007199254740992.0, 9007199254740993.0)
print(1e-4, 1e-5, 0.0001234, 0.00001234, 1e22, 1e23, 5e-310, 2.5e-7)
// the shortest digits end in a tie that rounds to even
print(1462468587316101.25, 0.3, 100.0, 1234.5, 999999999999999.9)
print(1e308 * 10.0, -1e308 * 10.0, 1e308 * 10.0 - 1e308 * 10.0)

// powers of two straddle the boundaries where the gap below a float is half the gap above
x = 1.0
for i in range(1100);
    if i % 50 == 0 or i > 1070;
        print(x, -x / 3.0)
    x = x / 2.0

// random mantissas scaled by powers of two and ten, each checked to read back as itself
seed = 12345
def next(seed);
    seed = seed * 6364136223846793005 % 9223372036854775807 + 1442695040888963407
    if seed < 0;
        seed = -(seed + 1)
    ret seed

def scaled(m, e);
    x = float(m)
    while e > 0;
        step = e
        if step > 900;
            step = 900
        x = x * 2.0 ** step
        e = e - step
    while e < 0;
        step = -e
        if step > 900;
            step = 900
        x = x * 0.5 ** step
        e = e + step
    ret x

failures = 0
for i in range(6000);
    seed = next(seed)
    m = seed % 9007199254740992
    seed = next(seed)
    x = scaled(m, seed % 2150 - 1126)
    seed = next(seed)
    y = float(m % 100000000) * 10.0 ** (seed % 640 - 330)
    if float(str(x)) != x or float(str(y)) != y;
        failures = failures + 1
    if i % 6 == 0;
        print(x, y)
print("round trip failures:", failures)
