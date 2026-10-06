// concatenation
greeting = "hello" + ", " + "world"
print(greeting, greeting.len())
print("" + "", "".len(), "a" + "")

// building a string in a loop
stars = ""
for i in range(5);
    stars = stars + "*"
print(stars, stars.len())

// equality compares contents, not where the string is stored
joined = "ab" + "c"
print(joined == "abc", joined != "abc", joined == "abd", "abc" == joined)
print("a" == "a", "a" != "b", "" == "", "abc" == "ab")
same = joined
print(same == joined)

// strings from functions and lists
def shout(word);
    ret word + "!"
names = ["ann", "bo"]
for name in names;
    print(shout(name))
print(shout(names[1]) == "bo!")

// chained equality
print("x" == "x" == "x", "x" == "x" == "y")
