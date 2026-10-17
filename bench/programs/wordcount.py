import sys


def count_words(n, k):
    counts = {}
    x = 1
    for _ in range(n):
        x = (x * 1103515245 + 12345) % 2147483648
        word = str(x // 64 % k)
        counts[word] = counts.get(word, 0) + 1
    return counts


def checksum(counts, k):
    total = len(counts)
    for j in range(k):
        word = str(j)
        if word in counts:
            total += counts[word] * (j % 7 + 1)
    return total


k = int(sys.argv[2])
print(checksum(count_words(int(sys.argv[1]), k), k))
