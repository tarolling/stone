// recursive calls and returns
def fib(n);
    if n < 2;
        ret n
    ret fib(n - 1) + fib(n - 2)

print(fib(35))
