# prints floats of many sizes, so both backends must agree on every digit
x = 12345
scale = 1e-30
for i in range(400);
    x = x * 1103515245 + 12345
    x = x - (x / 2147483648) * 2147483648
    value = float(x) / 2147483648.0
    print(value * scale, float(x) * 3.0 / 7.0, -value / 3.0, value * 1e-310)
    scale = scale * 1.7
