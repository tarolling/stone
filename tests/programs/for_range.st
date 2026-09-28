# range(n) counts from 0 up to but not including n
for i in range(3);
    print(i)

# range(a, b) counts from a up to but not including b
for i in range(-2, 2);
    print("from -2:", i)

# an empty range runs no iterations
for i in range(5, 5);
    print("never")
for i in range(0);
    print("never")

# break and cont
for i in range(10);
    if i == 1;
        cont
    if i == 4;
        break
    print("kept", i)

# the bounds are evaluated once, before the first iteration
n = 3
for i in range(n);
    n = n + 1
    print("n is", n)

# reassigning the variable does not change the next iteration
i = 0
for i in range(3);
    print("got", i)
    i = 100

# the variable keeps its last value after the loop
print("after", i)

# nested loops, including inside a function
def table(size);
    total = 0
    for row in range(1, size + 1);
        for col in range(1, size + 1);
            total = total + row * col
    ret total
print(table(3))

def first_square_over(limit);
    for k in range(limit);
        if k * k > limit;
            ret k
    ret -1
print(first_square_over(20))
print(first_square_over(0))
