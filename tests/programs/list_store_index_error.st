# storing past the end is an error, even inside a function
def fill(items, n);
    for i in range(n);
        items[i] = i * 10
        print(items[i])

fill([0, 0, 0], 5)
print("after")
