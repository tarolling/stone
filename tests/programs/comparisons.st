// each comparison selects a branch, printing 1 when it holds and 0 when it does not
def show(cond);
    if cond;
        print(1)
    else;
        print(0)

show(1 == 1)
show(1 == 2)
show(1 != 2)
show(2 != 2)
show(1 < 2)
show(2 < 1)
show(2 <= 2)
show(3 <= 2)
show(3 > 2)
show(2 > 3)
show(2 >= 2)
show(1 >= 2)
show(0 - 5 < 3)

// chains mean each neighboring pair holds
show(1 < 2 < 3)
show(1 < 3 < 2)
show(3 > 2 >= 2 == 2)

// the middle of a chain is evaluated once, and a false link stops the chain
def counted(n);
    print(n)
    ret n
show(1 < counted(2) < 3)
show(5 < counted(2) < counted(9))

// precedence: arithmetic before comparison, comparison before not/and/or
show(1 + 1 == 2)
show(not 1 == 2)
show(1 < 2 and 2 < 3)
show(2 < 1 or 3 < 2)
show((1 + 2) * 3 == 9)

i = 0
while i < 3;
    print(i)
    i = i + 1
