# operands and arguments are evaluated left to right, which shows when they print
def say(n);
    print(n)
    ret n

x = say(2) - say(1)
print(x)
y = say(3) * say(4) + say(5) / say(6)
print(y)
def pair(a, b);
    ret a - b
print(pair(say(8), say(7)))

