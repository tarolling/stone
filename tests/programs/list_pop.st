// pop removes an element and returns it
xs = [1, 2, 3, 4]
last = xs.pop()
first = xs.pop(0)
print(last, first, xs)
xs.pop(-1)
print(xs)

// popping from a copy leaves the original alone
ys = xs
y = ys.pop()
print(xs, ys, y)

// strings and lists move out of the list
words = ["a", "b", "c"]
w = words.pop(1)
print(w, words)
grid = [[1], [2, 3]]
row = grid.pop()
row.append(4)
print(row, grid)

// the value is popped before the element it goes into is found
grid = [[1], [2]]
grid[0] = grid.pop()
print(grid)

// a nested list can be popped from
grid = [[1, 2], [3, 4]]
x = grid[1].pop(0)
print(x, grid)

// a function pops from its own copy and returns it
def drop_last(items);
    items.pop()
    ret items
zs = [5, 6, 7]
zs = drop_last(zs)
print(zs, drop_last([8, 9]))

// a stack
stack = []
for i in range(5);
    stack.append(i * i)
total = 0
while stack.len() > 0;
    top = stack.pop()
    total = total + top
print(total, stack)
