// float comparisons follow IEEE 754, so nan is unequal to everything
a = 1.5
b = 2.5
print(a < b, a <= b, a > b, a >= b, a == b, a != b)
print(a < a, a <= a, a > a, a >= a, a == a, a != a)
print(0.0 == -0.0, 0.0 < -0.0, -0.0 <= 0.0)
inf = 1e308 * 10.0
nan = inf - inf
print(nan == nan, nan != nan, nan < 1.0, nan <= 1.0, nan > 1.0, nan >= 1.0)
print(1.0 < nan, 1.0 >= nan, inf > 1e308, -inf < -1e308)
print(1.0 < 2.0 < 3.0, 1.0 < 3.0 < 2.0)
if a < b;
    print("less")
while a < 10.0;
    a = a * 2.0
print(a)
