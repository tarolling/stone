print("hi", 1, 2)
print(3 - 5)
print(-7)
print(true)
print(false)

# every type prints the same in both backends
print(none)
print(0)
print(9223372036854775807)
print(0 - 9223372036854775807 - 1)
print(1 < 2, 2 < 1)
print("with spaces", "", "and after an empty string")
print()

# a function without ret returns none
def nothing();
    x = 1
print(nothing())

# arguments are all evaluated before anything on the line is printed
def loud(n);
    print("loud", n)
    ret n
print("line", loud(1), loud(2))

# printing inside a call's arguments keeps the outer arguments intact
def pair(a, b);
    ret a * 10 + b
print(pair(4, loud(3)))
