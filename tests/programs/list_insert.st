// insert puts an element before the one at an index, which may also be the length
xs = [2, 4]
xs.insert(0, 1)
xs.insert(2, 3)
xs.insert(xs.len(), 5)
xs.insert(-1, 9)
xs.insert(-6, 0)
print(xs)

// strings and lists keep their own references
words = []
words.insert(0, "b")
words.insert(0, "a")
w = "c"
words.insert(2, w)
print(words, w)
grid = [[1]]
row = [0]
grid.insert(0, row)
row.append(5)
grid[1].insert(0, 7)
print(grid, row)

// inserting at the front over and over grows the list
ys = []
for i in range(20);
    ys.insert(0, i)
print(ys)

// inserting into a copy leaves the original alone
zs = ys
zs.insert(0, -1)
print(zs.len(), ys.len())
