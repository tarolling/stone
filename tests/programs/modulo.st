// % truncates like /, so the remainder takes the dividend's sign
print(7 % 3, -7 % 3, 7 % -3, -7 % -3, 0 % 5)
a = 0 - 7
b = 3
print(a % b, a % (0 - b), (a / b) * b + a % b == a)

// constant divisors, including powers of two
def by_constants(x);
    ret [x % 1, x % 2, x % 8, x % -8, x % 10, x % -10, x % 4096]
print(by_constants(12345), by_constants(0 - 12345))

// % -1 is always 0, even for the smallest int, which / -1 cannot divide
min = 0 - 9223372036854775807 - 1
minus_one = 0 - 1
print(min % minus_one, 5 % minus_one, min % -1, min % 10, min % min)

// % binds like * and /, left to right
print(17 % 5 * 2, 2 * 17 % 5, 100 / 7 % 4, 1 + 10 % 4)

// floats take the dividend's sign too, and the result is exact
print(7.5 % 2.0, -7.5 % 2.0, 7.5 % -2.0, -7.5 % -2.0)
print(0.0 % 3.0, -0.0 % 3.0, 6.0 % 3.0, -6.0 % 3.0)
print(1e300 % 3.0, 1e300 % 7.25, 5e-324 % 1.0, 0.3 % 0.1)
inf = 1e308 * 10.0
nan = inf - inf
print(2.5 % inf, -2.5 % inf, inf % 2.0, nan % 2.0, 2.0 % nan)

// a remainder in a loop, with live values on both sides
def digit_sum(n);
    total = 0
    while n > 0;
        total = total + n % 10
        n = n / 10
    ret total
print(digit_sum(9876543210), digit_sum(7))
