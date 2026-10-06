// compiled code divides by constants without idiv or runtime checks where it can,
// so these must still round toward zero like the interpreter

def show(x);
    print(x, x / 1, x / 2, x / -2, x / 8, x / -8, x / 3, x / -7)

show(0)
show(1)
show(-1)
show(7)
show(-7)
show(16)
show(-17)
show(1000001)
show(-1000001)

// the extremes, including the one dividend that overflows when divided by -1
biggest = 9223372036854775807
smallest = -9223372036854775807 - 1
show(biggest)
show(smallest)
print(biggest / 4611686018427387904, smallest / 4611686018427387904)
print(smallest / -4611686018427387904, smallest / 2147483648)

// the checks stay for 0 and -1, which can fail
print(biggest / -1)
