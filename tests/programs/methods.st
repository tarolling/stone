// len and append are methods, called on a value
def push(xs, x);
    xs.append(x)
    ret xs

def index(n);
    print("index")
    ret n

def value(n);
    print("argument")
    ret n

def words();
    ret ["a", "bb"]

xs = []
xs = push(xs, 1)
// the receiver's index is evaluated before the argument
rows = [[0], []]
rows[index(1)].append(value(2))
print(xs, xs.len(), (xs).len(), rows)

grid = [[1], []]
grid[0].append(2)
grid[1].append(grid[0].len())
print(grid)

print(words().len(), words()[1].len(), "".len(), [].len(), "héllo".len())

floats = []
floats.append(0.5)
print(floats, floats.len())

// len and append are ordinary names now
len = 3
def append(a, b);
    ret a + b
print(len, append(len, 4))
