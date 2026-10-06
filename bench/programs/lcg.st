// integer arithmetic in a tight loop: a linear congruential generator
// stone has no %, so the modulo is written with / instead
def lcg(rounds);
    x = 1
    total = 0
    i = 0
    while i < rounds;
        x = x * 1103515245 + 12345
        x = x - (x / 2147483648) * 2147483648
        total = total + x / 65536
        i = i + 1
    ret total

print(lcg(20000000))
