// the math module, whose results must match to the bit between backends
use math
use math.max as biggest

print(math.abs(-3), math.abs(4), math.abs(0))
inf = 1e308 * 10.0
nan = inf - inf
print(math.abs(-2.5), math.abs(-0.0), math.abs(0.0), math.abs(-inf), math.abs(nan))

// several numbers, folded from the left like Python
print(math.min(3, 1, 2), math.max(3, 1, 2), math.max(-5, -9))
print(math.min(2.5, -1.5), math.max(0.0, -0.0), math.max(-0.0, 0.0), math.min(-0.0, 0.0))
print(math.max(nan, 1.0), math.max(1.0, nan), math.min(2.0, nan, 1.0))

// a list of numbers
print(math.min([4, 8, -2, 7]), math.max([4, 8, -2, 7]), math.max([9]))
print(math.min([1.5, 0.25]), math.max([1.5, 0.25]), math.min([2.0, nan, 1.0]))
rows = [[5, 1], [7, 3]]
print(math.max(rows[1]), math.min(rows[0]) + math.max(rows[0]))

print(math.sqrt(2), math.sqrt(16.0), math.sqrt(0.0), math.sqrt(-0.0), math.sqrt(nan))
print(math.sqrt(inf), math.sqrt(1e-320), math.sqrt(0.5))

print(math.floor(2.5), math.floor(-2.5), math.floor(-2.0), math.floor(7), math.floor(-0.5))
print(math.floor(1e15 + 0.5), math.floor(-4503599627370495.5), math.floor(-0.0))

print(biggest(1, 2), biggest([1.5]))

xs = [3, 5]
xs.append(math.max(xs) * 2)
print(xs)

def norm(x, y);
    ret math.sqrt(x * x + y * y)

def clamp(x, low, high);
    ret math.min(math.max(x, low), high)

print(norm(3.0, 4.0), clamp(12, 0, 10), clamp(-4, 0, 10), clamp(5, 0, 10))

// arguments are evaluated left to right before comparing
def noisy(x);
    print("arg", x)
    ret x

print(math.max(noisy(1), noisy(3), noisy(2)))
