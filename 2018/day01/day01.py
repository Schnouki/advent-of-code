#!/usr/bin/env python3

import itertools as it
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


def first_freq_reached_twice(changes):
    """Find out the first frequency reached twice.

    >>> first_freq_reached_twice([1, -2, 3, 1])
    2
    >>> first_freq_reached_twice([+1, -1])
    0
    >>> first_freq_reached_twice([+3, +3, +4, -2, -4])
    10
    >>> first_freq_reached_twice([-6, +3, +8, +5, -6])
    5
    >>> first_freq_reached_twice([+7, +7, -2, -7, -4])
    14
    """
    freq = 0
    freqs = {0}
    for change in it.cycle(changes):
        freq += change
        if freq in freqs:
            return freq
        freqs.add(freq)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = [int(line) for line in fin]

        print("Frequency for %s: %d" % (fn, compute_frequency(data)))
        print("First frequency reached twice for %s: %d" %
              (fn, first_freq_reached_twice(data)))
