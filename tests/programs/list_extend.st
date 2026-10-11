// extend adds every element of another list
xs = [1, 2]
xs.extend([3, 4])
xs.extend([])
print(xs)

// a list can extend itself
xs.extend(xs)
print(xs)

// strings and lists are shared, not changed
words = ["a"]
more = ["b", "c"]
words.extend(more)
more[0] = "z"
print(words, more)
grid = [[1]]
grid.extend([[2], [3]])
grid[1].append(9)
print(grid)
grid.extend(grid)
grid[0].append(8)
print(grid)

// a list nothing reads afterward moves in
tail = ["x", "y"]
words.extend(tail)
print(words)

// extending over and over grows the list
big = []
for i in range(100);
    big.extend([i, i])
print(big.len(), big[199])
