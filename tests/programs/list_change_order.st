// a change evaluates the value and every index before it walks to the list it changes
def say(n);
    print(n)
    ret n

grid = [[1]]
grid[say(0)][say(0)] = say(7)
print(grid)
grid[say(3)][say(0)] = say(8)
print("unreachable")
