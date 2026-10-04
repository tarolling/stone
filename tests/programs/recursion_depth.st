# up to 1000 calls can be active at once
def depth(n);
    if n == 0;
        ret 0
    ret 1 + depth(n - 1)
print(depth(999))

# returning unwinds, so later calls can go just as deep again
print(depth(999) + depth(999))
