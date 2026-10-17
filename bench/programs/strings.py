import sys


def sentence(n, seed):
    words = []
    x = seed
    for _ in range(n):
        x = (x * 1103515245 + 12345) % 2147483648
        words.append(str(x // 65536))
    return " ".join(words)


def score(text):
    total = 0
    for word in text.split():
        total += len(word)
        if word.startswith("1"):
            total += 1
        if word.endswith("7"):
            total += 2
        if word[0] == word[-1]:
            total += 3
    return total


def run(rounds, n):
    total = 0
    for r in range(rounds):
        text = sentence(n, r + 1)
        total += score(text) + text.find("4242")
    return total


print(run(int(sys.argv[1]), int(sys.argv[2])))
