// float % is exact and takes the dividend's sign, even for huge, tiny, and subnormal floats
inf = 1e308 * 10.0
print(5.5 % 2.0, -5.5 % 2.0, 5.5 % -2.0, -5.5 % -2.0, 6.0 % 2.0, -6.0 % 3.0)
print(1e300 % 3.0, 1e-300 % 3.0, 3.0 % 1e-300, 1e308 % 1e-308, 0.1 % 0.01)
print(5e-324 % 3e-324, 2.2250738585072014e-308 % 5e-324, 1e-310 % 3e-320, 7e-320 % 1e-310)
print(2.0 % inf, -2.0 % inf, inf % 2.0, (inf - inf) % 2.0, 2.0 % (inf - inf), 0.0 % 5.0, -0.0 % 5.0)
print(9007199254740993.0 % 2.0, 1.7976931348623157e308 % 0.7, 123456789.0 % 0.001)

// random pairs across the whole range, each checked against the definition
seed = 2024
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

for i in range(600);
    seed = next(seed)
    a = scaled(seed % 9007199254740992, seed % 2000 - 1100)
    seed = next(seed)
    b = scaled(seed % 9007199254740992 + 1, seed % 2000 - 1100)
    if seed % 2 == 0;
        a = -a
    if i % 3 == 0;
        print(a % b)
