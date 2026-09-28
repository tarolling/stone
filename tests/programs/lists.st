# literals, indexing, and len
nums = [3, 1, 4]
print(nums, len(nums), nums[0], nums[2])
print([], [[1, 2], []], ["a", "b c"], [true, false], [none])

# negative indexes count from the end
print(nums[-1], nums[-3])

# assigning through an index
nums[1] = 10
nums[-1] = nums[0] + nums[1]
print(nums)

# append grows a list well past its first allocation
squares = []
for i in range(20);
    append(squares, i * i)
print(len(squares), squares[19])
print(squares)

# lists are shared, not copied
alias = nums
append(alias, 99)
print(nums)

# iterating, including over a list that grows while it is being iterated
total = 0
for n in nums;
    total = total + n
print(total)
grow = [1]
for g in grow;
    if g < 4;
        append(grow, g + 1)
print(grow)

# lists of lists
grid = [[0, 0], [0, 0]]
grid[1][0] = 5
print(grid, grid[1])

# lists pass through functions by reference
def fill(items, n);
    for i in range(n);
        append(items, i)
    ret items
built = fill([], 3)
print(built, fill(built, 2))

def words();
    ret ["x", "y"]
for w in words();
    print(w)
