#!/usr/bin/env python3

import doctest

INPUT = 394


def spinlock(steps: int) -> int:
    """Day 17 spinlock.

    >>> spinlock(3)
    638
    """
    buf = [0]
    pos = 0
    for n in range(2017):
        pos = (pos + steps) % len(buf) + 1
        buf.insert(pos, n + 1)

    end_pos = (pos + 1) % len(buf)
    return buf[end_pos]


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    print("Value after %d steps: %d" % (INPUT, spinlock(INPUT)))
