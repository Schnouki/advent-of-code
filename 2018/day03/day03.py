#!/usr/bin/env python3

import collections as coll
import itertools as it
import doctest
import sys


def count_claims(claims):
    """Count how many square inches are available in each claim.

    Overlapping claims are noted as "X".

    >>> dict(count_claims([("1", 1, 3, 4, 4), ("2", 3, 1, 4, 4), ("3", 5, 5, 2, 2)]))
    {'1': 12, 'X': 4, '2': 12, '3': 4}
    """
    # Slow and inefficient!
    claimed = coll.defaultdict(lambda: None)
    max_x, max_y = 0, 0
    for rect in claims:
        id_ = rect[0]
        x1, y1, x2, y2 = rect[1], rect[2], rect[1]+rect[3], rect[2]+rect[4]
        max_x, max_y = max(max_x, x2), max(max_y, y2)
        for x, y in it.product(range(x1, x2), range(y1, y2)):
            if claimed[x, y] is None:
                claimed[x, y] = id_
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

    return dict(coll.Counter(claimed.values()))


def count_overlaps(claims):
    """Count how many square inches of fabric are within two or more claims.

    >>> count_overlaps([("1", 1, 3, 4, 4), ("2", 3, 1, 4, 4), ("3", 5, 5, 2, 2)])
    4
    """
    return count_claims(claims)["X"]


def find_non_overlapping_claim(claims):
    """Find the ID of the only claim that doesn't overlap.

    >>> find_non_overlapping_claim([("1", 1, 3, 4, 4), ("2", 3, 1, 4, 4), ("3", 5, 5, 2, 2)])
    '3'
    """
    cnt = count_claims(claims)
    for claim in claims:
        id_ = claim[0]
        expected_size = claim[3] * claim[4]
        if id_ in cnt and cnt[id_] == expected_size:
            return id_


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
                data.append((words[0].lstrip("#"), int(
                    x), int(y.rstrip(":")), int(w), int(h)))

        print("Overlaps for %s: %d" % (fn, count_overlaps(data)))
        print("Non-overlapping claim for %s: %s" %
              (fn, find_non_overlapping_claim(data)))
