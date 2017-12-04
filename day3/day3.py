#!/usr/bin/env python3

import doctest
import itertools as it
import sys
import typing


def spiral(target: int) -> typing.List[typing.List[int]]:
    """Build a "spiral memory" grid up to target.

    >>> spiral(1)
    [[1]]
    >>> spiral(2)
    [[4, 3], [1, 2]]
    >>> spiral(3)
    [[4, 3], [1, 2]]
    >>> spiral(8)
    [[5, 4, 3], [6, 1, 2], [7, 8, 9]]
    >>> spiral(23)
    [[17, 16, 15, 14, 13], [18, 5, 4, 3, 12], [19, 6, 1, 2, 11], [20, 7, 8, 9, 10], [21, 22, 23, 24, 25]]
    """
    memory = [[1]]
    last, size = 1, 1

    while last < target:
        size += 1
        if size % 2 == 0:
            # Add a right column
            for n in range(size - 1):
                last += 1
                memory[-n - 1].append(last)

            # Add a top row
            row = range(last + 1, last + 1 + size)
            memory.insert(0, list(row)[::-1])
            last = memory[0][0]

        else:
            # Add a left column
            for n in range(size - 1):
                last += 1
                memory[n].insert(0, last)

            # Add a bottom row
            row = range(last + 1, last + 1 + size)
            memory.append(list(row))
            last = memory[-1][-1]

    return memory


def steps(square: int) -> int:
    """Compute the number of steps to access a "square".

    >>> steps(1)
    0
    >>> steps(12)
    3
    >>> steps(23)
    2
    >>> steps(1024)
    31
    """
    memory = spiral(square)

    def _find(target):
        for y, line in enumerate(memory):
            if target in line:
                x = line.index(target)
                return (x, y)
    x1, y1 = _find(1)
    xs, ys = _find(square)

    dx = abs(x1 - xs)
    dy = abs(y1 - ys)
    return dx + dy


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    data1 = 289326
    print("Steps for %d: %d" % (data1, steps(data1)))
