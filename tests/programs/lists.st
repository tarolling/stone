// literals, indexing, and len
nums = [3, 1, 4]
print(nums, nums.len(), nums[0], nums[2])
print([], [[1, 2], []], ["a", "b c"], [true, false], [none])

// negative indexes count from the end
print(nums[-1], nums[-3])

// assigning through an index
nums[1] = 10
nums[-1] = nums[0] + nums[1]
print(nums)

// append grows a list well past its first allocation
squares = []
for i in range(20);
    squares.append(i * i)
print(squares.len(), squares[19])
print(squares)

// assigning a list copies it
alias = nums
alias.append(99)
print(nums, alias)

// iterating, including over a list that grows while it is being iterated, which walks the
// list as it was when the loop started
total = 0
for n in nums;
    total = total + n
print(total)
grow = [1]
for g in grow;
    if g < 4;
        grow.append(g + 1)
print(grow)

// lists of lists
grid = [[0, 0], [0, 0]]
grid[1][0] = 5
print(grid, grid[1])

// a function changes its own copy of a list, and gives it back
def fill(items, n);
    for i in range(n);
        items.append(i)
    ret items
built = fill([], 3)
print(built, fill(built, 2))

def words();
    ret ["x", "y"]
for w in words();
    print(w)
