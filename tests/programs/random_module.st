// a seed gives the same numbers in every backend on every machine
use random
use random.choice as pick

random.seed(42)
print(random.random(), random.random(), random.random())

random.seed(42)
rolls = []
for i in range(10);
    rolls.append(random.randint(1, 6))
print(rolls)

random.seed(0 - 7)
smallest = 0 - 9223372036854775807 - 1
print(random.randint(smallest, 9223372036854775807), random.randint(5, 5), random.randint(-3, -1))

// choice draws like randint(0, len - 1), and its result is a copy like any element
words = ["red", "green", "blue"]
first = pick(words)
print(first, pick(words), random.choice([[1, 2], [3]]), random.choice([2.5]))
rows = [[1], [2, 3]]
row = random.choice(rows)
row.append(9)
print(row, rows)

// seeding again repeats the numbers
random.seed(2026)
a = random.randint(0, 1000000)
random.seed(2026)
print(a == random.randint(0, 1000000))

// arguments are evaluated left to right, then the number is drawn
def noisy(x);
    print("arg", x)
    ret x

random.seed(1)
print(random.randint(noisy(10), noisy(20)))
