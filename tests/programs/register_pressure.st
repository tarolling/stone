# more values live at once than there are registers, across calls of every kind
def id(x);
    ret x

def many(n);
    a = n + 1
    b = n + 2
    c = n + 3
    d = n + 4
    e = n + 5
    f = n + 6
    g = n + 7
    h = n + 8
    i = n + 9
    j = n + 10
    k = n + 11
    l = n + 12
    m = n + 13
    total = 0
    for step in range(3);
        total = total + id(a) + b * c - d + e * f - g + h * i - j + k * l - m + step
        print(a, b, c, d, e, f, g, h, i, j, k, l, m)
    ret total + a + b + c + d + e + f + g + h + i + j + k + l + m

print(many(5))

# each frame keeps values in registers across the recursive call
def depth(n, acc);
    if n == 0;
        ret acc
    x = n * 3
    y = x + acc
    r = depth(n - 1, y)
    ret r + x - y + n

print(depth(50, 1))

# values live across append, string concatenation, len, and print
def mixed(n);
    xs = []
    s = "a"
    t = 0
    u = 1.5
    for i in range(n);
        append(xs, i * i)
        s = s + "b"
        t = t + len(xs) + len(s)
        u = u * 2.0
    print(xs, s, t, u)
    ret t

print(mixed(4))

# an assignment whose right side reads the variable being assigned
def reads_target(x, y);
    x = y - x
    y = x * y + x
    x = [x, y][1] - x
    ret x * 1000 + y

print(reads_target(3, 10))
