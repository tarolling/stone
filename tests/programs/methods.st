# len and append are methods, called on a value
def push(xs, x);
    xs.append(x)

def tag(xs, label);
    print(label)
    ret xs

def value(n);
    print("argument")
    ret n

def words();
    ret ["a", "bb"]

xs = []
push(xs, 1)
# the receiver is evaluated before the argument
tag(xs, "receiver").append(value(2))
print(xs, xs.len(), (xs).len())

grid = [[1], []]
grid[0].append(2)
grid[1].append(grid[0].len())
print(grid)

print(words().len(), words()[1].len(), "".len(), [].len(), "héllo".len())

floats = []
floats.append(0.5)
print(floats, floats.len())

# len and append are ordinary names now
len = 3
def append(a, b);
    ret a + b
print(len, append(len, 4))
