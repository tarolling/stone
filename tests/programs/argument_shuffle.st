# arguments passed in a different order than the caller received them, which needs the
# register moves at each call ordered so none overwrites a value another still needs
def pair(a, b);
    ret a * 10 + b

def swap(a, b);
    ret pair(b, a)

def triple(a, b, c);
    ret a * 100 + b * 10 + c

def rotate(a, b, c);
    ret triple(c, a, b)

def seven(a, b, c, d, e, f, g);
    ret a - b + c - d + e - f + g * 1000

def shuffle7(a, b, c, d, e, f, g);
    ret seven(g, f, e, d, c, b, a) + seven(b, c, d, e, f, g, a)

def eight(a, b, c, d, e, f, g, h);
    ret ((((((a * 2 + b) * 2 + c) * 2 + d) * 2 + e) * 2 + f) * 2 + g) * 2 + h

def reverse8(a, b, c, d, e, f, g, h);
    ret eight(h, g, f, e, d, c, b, a)

print(swap(1, 2), rotate(1, 2, 3))
print(shuffle7(1, 2, 3, 4, 5, 6, 7))
print(reverse8(1, 0, 0, 0, 0, 0, 0, 0), reverse8(0, 0, 0, 0, 0, 0, 0, 1))
print(pair(pair(1, 2), pair(3, 4)))
big = 5000000000
print(pair(big, 9000000000), triple(big, 1, -big))
