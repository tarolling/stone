// float literals, arithmetic, and how print shows them
a = 1.5
b = 0.25
print(a + b, a - b, a * b, a / b)
print(-a, +b, -(-a))
print(0.1 + 0.2, 1.0, 100.0, 1e14, 1e15, 1e16, 1.5e16)
print(0.0001, 0.00012, 1e-5, 1.5e-7, 1e300 * 1e8, 5e-324)
print(0.0, -0.0, 0.0 * -1.0, 1462468587316101.0 + 0.25)
big = 1e308
print(big * 10.0, -big * 10.0, big * 10.0 - big * 10.0)
print(1.0 / 3.0, 2.0 / 3.0, 10.0 / 4.0, 7.0 / 2.0)
xs = [0.5, 1.0, 2.5e-10]
xs.append(3.0)
print(xs, xs.len(), xs[-1])
xs[0] = xs[0] * 4.0
print(xs[0])
total = 0.0
for x in xs;
    total = total + x
print(total)

def mean(values);
    sum = 0.0
    for v in values;
        sum = sum + v
    ret sum / float(values.len())

print(mean(xs))
grid = [[1.0, 2.0], [0.5, -0.5]]
print(grid)
