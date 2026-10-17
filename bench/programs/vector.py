import sys


def saxpy(a, x, y, reps):
    for _ in range(reps):
        y = [a * xi + yi for xi, yi in zip(x, y)]
    return y


n = int(sys.argv[1])
reps = int(sys.argv[2])
x = [float(i % 7) for i in range(n)]
y = [float(i % 5) for i in range(n)]
print(int(sum(saxpy(0.5, x, y, reps))))
