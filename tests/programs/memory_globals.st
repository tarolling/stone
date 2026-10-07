// globals read and written from functions
names = []
def add(name);
    names.append(name + "")

def reset();
    names = ["fresh"]

for i in range(5);
    add(str(i))
print(names)

// assigning in a function makes a local that shadows the global
current = "old" + ""
def change(text);
    current = "new" + text
    ret current
print(change(current), current)
current = change(current) + current
print(current)

// a global list changed by a function while the caller holds an element
shared = ["first" + ""]
def clobber();
    shared[0] = "second" + ""
    ret 1
print(shared[0], clobber(), shared[0])

// ret at the top level still frees everything
leftover = ["a" + "b"]
for x in [leftover];
    ret
print("unreachable")
