// a call whose result is assigned back to its argument changes the list in place, while every
// other copy stays as it was
def add(xs, x);
    xs.append(x)
    ret xs

xs = []
for i in range(5);
    xs = add(xs, i)
print(xs)

kept = xs
xs = add(xs, 5)
print(kept, xs)

// a function that reads the global sees it unchanged during the call
def add_first(ys);
    ys.append(xs[0] + 100)
    ret ys
xs = add_first(xs)
print(xs)

// the same list passed twice is two copies
def joined(a, b);
    a.append(b.len())
    b.append(-1)
    ret a
row = [7]
row = joined(row, row)
print(row)

// returning a call passes the list on, and the caller's copy stays as it was
def fill(xs, n);
    if n == 0;
        ret xs
    xs.append(n)
    ret fill(xs, n - 1)
start = [0]
print(fill(start, 3), start)

// a function that replaces the list it was given
def replaced(xs);
    xs = ["new" + ""]
    ret xs
words = ["old" + ""]
words = replaced(words)
print(words)

// strings are passed the same way
def shout(s);
    s = s + "!"
    ret s
greeting = "hi" + ""
greeting = shout(greeting)
copy_of = greeting
greeting = shout(greeting)
print(greeting, copy_of)

// a local is moved too
def build(n);
    out = []
    for i in range(n);
        out = add(out, i * i)
    ret out
print(build(4))

// a loop keeps the list it walks
items = [1, 2]
for item in items;
    items = add(items, item * 10)
print(items)
