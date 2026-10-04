# conditions branch on comparisons directly, left to right, stopping as soon as the result is known
def note(label, v);
    print(label)
    ret v

def classify(a, b, c);
    if a < b < c;
        ret 1
    if a > b > c;
        ret 2
    if a == b and not b == c;
        ret 3
    if a == c or b == c;
        ret 4
    ret 0

print(classify(1, 2, 3), classify(3, 2, 1), classify(2, 2, 3), classify(1, 2, 1), classify(2, 1, 3))

if note("first", 0) and note("skipped", 1);
    print("no")
if note("second", 1) or note("skipped", 1);
    print("yes")
if not note("third", 0) and note("fourth", 1) < note("fifth", 2) < note("sixth", 1);
    print("no")
else;
    print("chain stopped")
if 5 > note("seventh", 3) >= 3 != 4;
    print("constant on the left")

def count_down(n);
    steps = 0
    while n > 0 and not n == 3;
        n = n - 1
        steps = steps + 1
    ret steps

print(count_down(10), count_down(2))

# comparisons with nan are false, except !=
inf = 1e308 * 10.0
nan = inf - inf

def nan_branches(x);
    hits = 0
    if x == x;
        hits = hits + 1
    if x != x;
        hits = hits + 10
    if x < 1.0 or x <= 1.0 or x > 1.0 or x >= 1.0;
        hits = hits + 100
    if not x < 1.0;
        hits = hits + 1000
    ret hits

print(nan_branches(nan), nan_branches(0.5))
n = 0
while nan < 1.0;
    n = n + 1
print(n)

# a string comparison in a condition
def greet(name);
    if name == "stone" and name != "rock";
        ret "hi " + name
    ret "who?"

print(greet("stone"), greet("rock"))
