// ** binds tighter than unary minus on its left and groups right to left
print(2 ** 10, 0 ** 0, 5 ** 0, 0 ** 5, 1 ** 100, -2 ** 2, (0 - 2) ** 2, (0 - 2) ** 3)
print(2 ** 3 ** 2, (2 ** 3) ** 2, 2 * 3 ** 2, 2 ** -(-3), 10 % 3 ** 2)

// int powers wrap around like *
print(2 ** 62, 2 ** 63, 2 ** 64, 3 ** 50, (0 - 3) ** 41)
print(1 ** 9223372036854775807, (0 - 1) ** 9223372036854775807, 0 ** 9223372036854775807)

// exponents from variables and calls
def power(base, exp);
    ret base ** exp
total = 0
for n in range(8);
    total = total + power(3, n)
print(total, power(7, 3), power(0 - 7, 3))

// a float base takes an int exponent, and a negative one gives a reciprocal
print(2.5 ** 3, 2.0 ** -2, 1.5 ** 0, 0.5 ** 1, (0.0 - 2.0) ** 3, -2.0 ** 2)
zero = 0.0
print(0.0 ** -1, (-zero) ** -1, 10.0 ** 400, 10.0 ** -400, 1.1 ** 100)
min = 0 - 9223372036854775807 - 1
inf = 1e308 * 10.0
nan = inf - inf
print(1.0 ** min, 2.0 ** min, 1.0 ** (0 - 1), nan ** 0, nan ** 1, inf ** -1)

def float_power(base, exp);
    ret base ** exp
print(float_power(1.1, 100), float_power(2.0, 0 - 3), float_power(0.0, 0 - 1))

// powers between values that stay live across them
def mixed(a, b, c, x, y);
    p = a ** b + c ** 2
    q = x ** b - y ** -2
    print([a, b, c, p, a ** c % 1000, b], [x, y, q])
mixed(3, 4, 5, 1.5, 2.0)
