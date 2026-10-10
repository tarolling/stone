// allocates small and large lists and strings, growing, copying, and freeing them many times
def build(n);
    xs = []
    for i in range(n);
        xs.append(i * i % 1000)
    ret xs

def total(xs);
    sum = 0
    for x in xs;
        sum = sum + x
    ret sum

// lists that outgrow every size class and keep growing in place
big = build(300000)
print(big.len(), total(big))

// a copy shares the list until one of them changes
copy = big
copy[0] = 12345
print(big[0], copy[0], total(copy) - total(big))

// strings of every size, freed and allocated again
text = ""
lengths = []
for round in range(3);
    text = ""
    for i in range(20000);
        text = text + "ab"
    lengths.append(text.len())
    words = (text + " " + text).split()
    lengths.append(words.len())
print(lengths)

// many small lists that come and go
rows = []
for i in range(2000);
    row = build(i % 50)
    rows.append(row)
    if rows.len() > 100;
        rows = []
print(rows.len(), total(rows[rows.len() - 1]))
