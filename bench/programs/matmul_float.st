# float arithmetic in nested lists: multiplying two matrices of floats from a generator
def matrix(n, seed);
    rows = []
    x = seed
    for i in range(n);
        row = []
        for j in range(n);
            x = x * 1103515245 + 12345
            x = x - (x / 2147483648) * 2147483648
            row.append(float(x) / 2147483648.0)
        rows.append(row)
    ret rows

def multiply(a, b, n);
    c = []
    for i in range(n);
        row = []
        for j in range(n);
            total = 0.0
            for k in range(n);
                total = total + a[i][k] * b[k][j]
            row.append(total)
        c.append(row)
    ret c

def checksum(c, n);
    total = 0.0
    for i in range(n);
        for j in range(n);
            total = total + c[i][j] * float(i + j + 1)
    ret total

n = 300
# scaled to an int, so C need not print floats the way stone does
print(int(checksum(multiply(matrix(n, 1), matrix(n, 2), n), n) * 1000000.0))
