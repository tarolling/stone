# def opens a function, and ret returns from it
def area(width, height);
    ret width * height

print(area(3, 4))

# functions can call themselves, and be called before they are defined
print(fib(10))

def fib(n);
    if n < 2;
        ret n
    ret fib(n - 1) + fib(n - 2)

# a function without ret returns none
def greet(name);
    print("hi " + name)

print(greet("ada"))

# assigning inside a function makes a local; other names are globals
counter = 10
def bump();
    counter = 1
    ret counter
print(bump(), counter)

def read_counter();
    ret counter
print(read_counter())
