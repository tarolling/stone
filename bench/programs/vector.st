// arithmetic on lists: y = a * x + y over a million floats, repeated
def fill(n, k);
    xs = []
    for i in range(n);
        xs.append(float(i % k))
    ret xs

def saxpy(a, x, y, reps);
    for r in range(reps);
        y = a * x + y
    ret y

def total(xs);
    sum = 0.0
    for v in xs;
        sum = sum + v
    ret sum

n = 1000000
y = saxpy(0.5, fill(n, 7), fill(n, 5), 50)
// every value is a multiple of 0.5, so the sum is exact in any order
print(int(total(y)))
