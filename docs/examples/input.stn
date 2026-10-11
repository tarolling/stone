// reads "name score" lines from stdin and reports the best score
best_name = ""
best = 0.0
count = 0
total = 0.0
while not eof();
    line = input().strip()
    if line == "";
        cont
    fields = line.split()
    score = float(fields[1])
    total = total + score
    count = count + 1
    if count == 1 or score > best;
        best_name = fields[0]
        best = score
print("read " + str(count) + " scores")
print("best:", best_name, best)
print("average:", total / float(count))
