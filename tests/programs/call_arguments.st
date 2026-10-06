// every argument keeps its value while later arguments are evaluated, even ones that divide
def digits(a, b, c, d);
    ret a * 1000 + b * 100 + c * 10 + d
print(digits(1, 2, 3, 8 / 2))
print(digits(1, 2, 3 / 1, 4))
print(digits(1, 2, 3, digits(0, 0, 0, 4)))
print(digits(6 / 6, 4 / 2, 9 / 3, 16 / 4))
