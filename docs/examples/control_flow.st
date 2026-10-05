# if, elif, and else open their blocks with ;
temperature = 18
if temperature < 10;
    print("cold")
elif temperature < 20;
    print("mild")
else;
    print("warm")

# while repeats as long as its condition holds
n = 1
while n < 100;
    n = n * 2
print(n)

# for counts with range, which stops before its end
for i in range(3);
    print("lap", i)
for i in range(10, 13);
    print(i)

# cont skips to the next iteration and break leaves the loop
for i in range(10);
    if i == 2;
        cont
    if i == 5;
        break
    print("kept", i)

# comparisons chain, and and/or/not combine conditions
x = 5
print(1 < x < 10, x == 5 and x != 6, not (x > 3 or x < 0))
