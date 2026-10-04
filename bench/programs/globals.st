# the lcg benchmark, but reading its constants from globals inside the loop,
# which compiled stone checks are assigned on every read
multiplier = 1103515245
increment = 12345
modulus = 2147483648

def lcg(rounds);
    x = 1
    total = 0
    i = 0
    while i < rounds;
        x = x * multiplier + increment
        x = x - (x / modulus) * modulus
        total = total + x / 65536
        i = i + 1
    ret total

print(lcg(20000000))
