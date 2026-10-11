// each element is computed in turn, so element 1 divides by zero before element 2 divides MIN
// by -1, even though a / b comes first
a = [1, 1, -9223372036854775807 - 1]
b = [1, 1, -1]
c = [1, 1, 1]
d = [1, 0, 1]
print(a / b + c / d)
