#!/usr/bin/env python3

import doctest
import sys

from day10 import knot_hash


def squares_used(key: str) -> int:
    """Count the number of squares used for defragmentation.

    >>> squares_used('flqrgnkx')
    8108
    """
    used = 0
    for n in range(128):
        hkey = "%s-%d" % (key, n)
        knot = knot_hash(hkey)
        for c in knot:
            b = bin(int(c, 16))
            used += b.count("1")
    return used


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    KEY = 'uugsqrei'
    print("Squares used for %s: %d" % (KEY, squares_used(KEY)))
