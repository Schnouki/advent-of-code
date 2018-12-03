#!/usr/bin/env python3

import collections as coll
import itertools as it
import doctest
import sys


def count_overlaps(claims):
    """Count how many square inches of fabric are within two or more claims.

    >>> count_overlaps([(1, 3, 4, 4), (3, 1, 4, 4), (5, 5, 2, 2)])
    4
    """
    # Slow and inefficient!
    claimed = coll.defaultdict(lambda: None)
    max_x, max_y = 0, 0
    for id_, rect in enumerate(claims):
        x1, y1, x2, y2 = rect[0], rect[1], rect[0]+rect[2], rect[1]+rect[3]
        max_x, max_y = max(max_x, x2), max(max_y, y2)
        for x, y in it.product(range(x1, x2), range(y1, y2)):
            if claimed[x, y] is None:
                claimed[x, y] = id_ + 1
            else:
                claimed[x, y] = "X"

    # Debug!
    if False:
        for y in range(max_y):
            line = ""
            for x in range(max_x):
                val = claimed[x, y]
                if val is None:
                    val = "."
                line += str(val)
            print(line)

    return coll.Counter(claimed.values())["X"]


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = []
            for line in fin:
                words = line.split()
                x, y = words[2].split(",")
                w, h = words[3].split("x")
                data.append((int(x), int(y.rstrip(":")), int(w), int(h)))

        print("Overlaps for %s: %d" % (fn, count_overlaps(data)))
