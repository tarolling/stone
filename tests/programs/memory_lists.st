// lists, and lists of strings and lists, are freed with what they hold
xs = [1, 2, 3]
xs = [4, 5]
words = ["a" + "b", "c" + "d"]
words[0] = words[1]
words[1] = "e" + "f"
words[-1] = words[-1]
words.append(words[0] + "!")
print(xs, words)

grid = [["a" + "1", "b"], [], ["c"]]
grid[1].append("d" + "2")
grid[0] = grid[2]
grid.append([str(7)])
print(grid, grid[0][0])
deep = [[["x" + "y"]], [[]]]
deep[1][0].append(deep[0][0][0])
print(deep)

// a list being iterated stays alive when its variable is reassigned
items = ["p" + "", "q" + "", "r" + ""]
for item in items;
    items = []
    print(item)
print(items)

// returning from inside loops releases the lists they iterate
def find(groups, wanted);
    for group in groups;
        for name in group.split(",");
            if name == wanted;
                ret name + "?"
    ret "none"
print(find(["a,b", "c,d"], "c"), find(["a"], "z"), find("x,y z".split(), "y"))

// breaking out of a loop over a temporary list
for word in "one two three".split();
    if word == "two";
        break
    print(word)

// a function that builds and returns a list
def build(n);
    out = []
    for i in range(n);
        out.append(str(i) + "!")
    ret out
print(build(3), build(2)[1], build(4).len())

// a list passed in and changed by a function
def grow(list, item);
    list.append(item)
    list = ["replaced"]
    ret list
kept = ["k"]
print(grow(kept, "m" + "n"), kept)

// strip and split make new strings and lists
line = "  a b  c "
print(line.strip(), line.split(), line.split(" ").len(), line.strip().split("b"))

// a row stays alive while the index calls a function that replaces it in a copy
table = [["r0" + "", "s0"], ["r1" + "", "s1"]]
def replace_row(rows, i);
    rows[i] = ["new" + ""]
    ret rows
def first(rows);
    ret rows[0].len() - 1
print(table[0][first(replace_row(table, 0))], replace_row(table, 1), table[0])

// a row of a temporary list stays alive while it is read
def fresh_table();
    ret [["f" + "", "g"], ["h"]]
print(fresh_table()[0][1], fresh_table()[1].len(), fresh_table()[0][0] + "!")
table[1].append(table[1][0] + "!")
table[1][0] = table[0][0]
print(table)
