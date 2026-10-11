import numpy as np


def saxpy(a, x, y, reps):
    for _ in range(reps):
        y = a * x + y
    return y


n = 1000000
x = (np.arange(n) % 7).astype(np.float64)
y = (np.arange(n) % 5).astype(np.float64)
print(int(saxpy(0.5, x, y, 50).sum()))
