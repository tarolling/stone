// os.exit stops at once, even with strings and lists still allocated
use os

def finish(words, code);
    print("finishing with", words.len(), "words")
    os.exit(code)
    print("never printed")

words = "a b c".split()
greeting = "hello" + " " + "world"
print(greeting)
finish(words, 0)
print("never printed either")
