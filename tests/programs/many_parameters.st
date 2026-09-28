# functions take any number of parameters, not just the six that fit in registers
def weigh(a, b, c, d, e, f, g, h);
    ret a + 2 * b + 3 * c + 4 * d + 5 * e + 6 * f + 7 * g + 8 * h
print(weigh(1, 1, 1, 1, 1, 1, 1, 1))
print(weigh(0, 0, 0, 0, 0, 0, 1, 0), weigh(0, 0, 0, 0, 0, 0, 0, 1))

def last_of_nine(a, b, c, d, e, f, g, h, i);
    ret i
print(last_of_nine(1, 2, 3, 4, 5, 6, 7, 8, weigh(8 / 8, 0, 0, 0, 0, 0, 0, 0)))
