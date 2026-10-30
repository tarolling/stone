import math
import sys


def walk(steps):
    x = 1.0
    y = 0.0
    total = 0.0
    for i in range(steps):
        t = float(i) * 0.001
        r = 1.0 + 0.5 * math.sin(t * 3.0)
        x = r * math.cos(t)
        y = r * math.sin(t)
        total += math.atan2(y, x) + math.asin(math.sin(t) * 0.5)
    return total


print(int(walk(int(sys.argv[1])) * 1000.0))
