import sys

import numpy as np


def saxpy(a, x, y, reps):
    for _ in range(reps):
        y = a * x + y
    return y


n = int(sys.argv[1])
reps = int(sys.argv[2])
x = (np.arange(n) % 7).astype(np.float64)
y = (np.arange(n) % 5).astype(np.float64)
print(int(saxpy(0.5, x, y, reps).sum()))
