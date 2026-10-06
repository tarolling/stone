// int and float read numbers from strings, ignoring whitespace at either end
print(int("42"), int("  -7 "), int("+0012"), int(input()))
print(int("9223372036854775807"), int("-9223372036854775808"))
print(float("2.5"), float(" -.5 "), float("3."), float("1e3"), float("1E-2"), float(input()))
print(float("6.02e+23"), float("1e999"), float("-1e999"), float("1e-999"))
print(float("inf"), float("-Infinity"), float("NaN"), float("0.1") + float("0.2"))
print(float("123456789012345678901234567890"), float("4.9e-324"))
// numbers still convert as before
print(int(2.9), float(3))
total = 0
for word in "10 20 30".split();
    total = total + int(word)
print(total)
