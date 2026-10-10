// a value moves at its last use, and every copy that is still read stays as it was
def add(xs, x);
    xs.append(x)
    ret xs

// moved into a call, into another variable, and into a list
a = [1]
b = add(a, 2)
c = b
rows = []
rows.append(c)
rows[0].append(3)
print(rows)

// a read on one branch keeps the value
d = [4]
e = add(d, 5)
if e.len() > 5;
    print(d)
else;
    print(e)

// a later iteration keeps the value
grow = [0]
snapshot = []
i = 0
while i < 3;
    grow = add(grow, i)
    snapshot = grow
    grow = add(grow, 10)
    i = i + 1
print(grow, snapshot)

// a function called later reads the global, so the global keeps its copy
seen = [7]
def first_seen();
    ret seen[0]
copied = add(seen, 8)
print(copied, first_seen())

// break and cont
items = [1, 2, 3, 4]
kept = []
for item in items;
    row = [item]
    if item == 2;
        cont
    if item == 4;
        break
    kept.append(row)
print(kept, items)

// strings move the same way
word = "a" + "b"
other = word
other = other + "c"
print(other)

// in a function, locals move into calls, lists, and the result
def build(n);
    out = []
    for i in range(n);
        row = add([i], i)
        out.append(row)
    copy = out
    copy.append([])
    ret copy
print(build(3))
