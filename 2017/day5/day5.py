#!/usr/bin/env python3

import doctest
import sys
import typing


def steps(data: typing.List[int], stranger: bool = False) -> int:
    """Compute the number of steps to exit a jump maze.

    >>> steps([0, 3, 0, 1, -3])
    5
    >>> steps([0, 3, 0, 1, -3], True)
    10
    """
    data = data.copy()
    ptr = 0
    steps = 0
    while 0 <= ptr < len(data):
        offset = data[ptr]
        if stranger and offset >= 3:
            data[ptr] -= 1
        else:
            data[ptr] += 1
        ptr += offset
        steps += 1
    return steps


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()
        data = [int(v) for v in data.splitlines()]

        print("Number of steps for %s: %d" % (fn, steps(data)))
        print("Number of stranger steps for %s: %d" % (fn, steps(data, True)))
