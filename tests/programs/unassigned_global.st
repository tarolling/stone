// a function can read a global that is assigned later, but not before it is assigned
def read_total();
    ret total
print("start")
print(read_total())
total = 1
