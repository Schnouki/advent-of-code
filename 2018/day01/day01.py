#!/usr/bin/env python3

import doctest
import sys


def compute_frequency(changes):
    """Compute the frequency after a sequence of changes.

    >>> compute_frequency([1, -2, 3, 1])
    3
    >>> compute_frequency([1, 1, 1])
    3
    >>> compute_frequency([1, 1, -2])
    0
    >>> compute_frequency([-1, -2, -3])
    -6
    """
    freq = 0
    for change in changes:
        freq += change
    return freq


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = [int(line) for line in fin]

        print("Frequency for %s: %d" % (fn, compute_frequency(data)))
