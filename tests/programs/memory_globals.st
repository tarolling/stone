// a global list passed to a function, which gives back the changed list
names = []
def add(names, name);
    names.append(name + "")
    ret names

def reset();
    names = ["fresh"]

for i in range(5);
    names = add(names, str(i))
print(names)

// assigning in a function makes a local that shadows the global
current = "old" + ""
def change(text);
    current = "new" + text
    ret current
print(change(current), current)
current = change(current) + current
print(current)

// a function changes its own copy of a global list while the caller holds an element
shared = ["first" + ""]
def clobber();
    mine = shared
    mine[0] = "second" + ""
    ret mine
print(shared[0], clobber(), shared[0])

// ret at the top level still frees everything
leftover = ["a" + "b"]
for x in [leftover];
    ret
print("unreachable")
