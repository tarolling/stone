// strings are freed once nothing refers to them
s = ""
for i in range(1000);
    s = s + "x"
    if s.len() > 10;
        s = ""
print(s, s.len())

// reassigning a variable to itself keeps the string alive
t = "a" + "b"
t = t
u = t
t = t + t
print(t, u)

// strings through functions: borrowed parameters, assigned parameters, and results
def shout(word);
    ret word + "!"

def pad(word, n);
    for i in range(n);
        word = word + "."
    ret word

def same(word);
    ret word

def first(word);
    if word.len() > 3;
        ret word
    ret "short"

print(shout("hi"), pad("a", 3), same(u), first(u), first("long word"))
print(shout(shout(str(42))), str(1.5) + str(true))

// chained comparisons of strings, including ones that stop early
def make(text);
    ret text + ""
print(make("a") == make("a") == make("a"), make("a") == make("b") == make("c"))
print("a" != make("a"), make("x") == "x")


// a string read through a global from a function
greeting = "hello" + " world"
def greet();
    ret greeting + "!"
print(greet())
greeting = greet()
print(greeting)

// expression statements drop their result
shout("ignored")
