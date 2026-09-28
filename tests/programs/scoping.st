# functions can be called before their definition
print(twice(2))

def twice(n);
    ret n * 2

# functions read globals, even ones assigned after the function is defined
def read_limit();
    ret limit
limit = 10
print(read_limit())
limit = 11
print(read_limit())

# assigning inside a function makes a local, leaving the global alone
x = 1
def shadow();
    x = 2
    ret x
print(shadow())
print(x)

# a callee sees globals, not its caller's locals
def inner();
    ret x
def outer();
    x = 3
    ret inner()
print(outer())

# a parameter shadows a global of the same name
def param(x);
    ret x + 100
print(param(5))
print(x)

# globals assigned inside top-level blocks are still globals
if x;
    from_block = 7
def read_block();
    ret from_block
print(read_block())
