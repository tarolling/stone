// assigning a list copies it, so changing one variable never changes another
a = [1, 2]
b = a
b.append(3)
print(a, b)
b[0] = 10
print(a, b)

// a list read from another list is a copy too
grid = [[1, 2], [3, 4]]
row = grid[0]
row.append(5)
grid[1][0] = 30
print(grid, row)
copy = grid
copy[0][0] = 100
print(grid, copy)

// and so is a list put into another list
items = [7]
nested = [items, items]
nested[0].append(8)
items.append(9)
print(items, nested)

// a function gets a copy of its argument, and gives back what it changed
def with_zero(xs);
    xs.append(0)
    ret xs
original = [5]
changed = with_zero(original)
print(original, changed)
original = with_zero(original)
print(original)

def bumped(grid);
    grid[0][0] = grid[0][0] + 1
    ret grid
small = [[1], [2]]
print(bumped(small), small)

// a loop variable is a copy of the element
def rows_with(grid, x);
    out = []
    for row in grid;
        row.append(x)
        out.append(row)
    ret out
print(rows_with(small, 9), small)

// a loop walks the list as it was when the loop started
grow = [1, 2]
for g in grow;
    grow.append(g * 10)
print(grow)

// strings in copied lists are shared until replaced
words = ["a" + "b"]
more = words
more[0] = "c" + "d"
more.append(words[0])
print(words, more)

// a function reads globals, and changes lists of its own
total = [100]
def summed(xs);
    out = [0]
    for x in xs;
        out[0] = out[0] + x
    ret out[0] + total[0]
print(summed([1, 2, 3]), total)
