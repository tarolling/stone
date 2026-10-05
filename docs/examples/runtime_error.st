def average(values);
    total = 0
    for v in values;
        total = total + v
    ret total / len(values)

print(average([4, 8, 9]))
print(average([]))
print("never printed")
