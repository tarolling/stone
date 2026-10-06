// the 1001st active call is an error rather than a crash
def depth(n);
    if n == 0;
        ret 0
    ret 1 + depth(n - 1)
print("start")
print(depth(1000))
