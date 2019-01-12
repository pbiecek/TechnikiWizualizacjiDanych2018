import math
import numpy as np
import pandas as pd

sides = [
    (0, 1, 0),
    (-4, 1, 0),
    (4, 1, -40)
]


def distance(x, y, side):
    (a, b, c) = side
    return abs(a * x + y * b + c) / math.sqrt(a ** 2 + b ** 2)


def under(x, y, side):
    (a, b, c) = side
    return a * x + b * y + c < 0


def in_triangle(x, y):
    return under(x, y, sides[1]) and under(x, y, sides[2])


def inside(x, y, result):
    return any((x - r[0]) ** 2 + (y - r[1]) ** 2 < r[2] ** 2 for r in result)


def min_distance(x, y, result):
    partial = [math.sqrt((x - r[0]) ** 2 + (y - r[1]) ** 2) - r[2] for r in result]
    partial.extend(distance(x, y, s) for s in sides)
    return min(*partial)


area = 10 * 20 / 2
coveredArea = 0
circles = []

while coveredArea < area * 0.7:
    [x, y] = np.random.rand(2)
    x *= 10
    y *= 20

    if in_triangle(x, y) and not inside(x, y, circles):
        dist = min_distance(x, y, circles)
        if dist > 0.5:
            r = dist / 4
        else:
            r = dist
        circles.append((x, y, r))
        coveredArea += math.pi * r ** 2

df = pd.DataFrame(circles, columns=["x", "y", "r"])
df.to_csv("out.csv", index=False)
