// reads every line, including empty ones and a last line with no newline
name = input("name? ")
print("hello, " + name)
count = 0
while not eof();
    line = input()
    count = count + 1
    print(count, "[" + line + "]", line.len())
// past the end, input returns an empty string
print(eof(), input() == "", eof())
