# a list holds values of one type
primes = [2, 3, 5]
print(primes, len(primes))

# indexes start at 0, and negative ones count from the end
print(primes[0], primes[-1])
primes[0] = 1
print(primes)

# append adds to the end
append(primes, 7)
print(primes)

# for walks a list in order
total = 0
for p in primes;
    total = total + p
print(total)

# lists are shared, not copied
alias = primes
append(alias, 11)
print(primes)

# lists can nest
grid = [[1, 2], [3, 4]]
grid[1][0] = 30
print(grid, grid[1][0])
