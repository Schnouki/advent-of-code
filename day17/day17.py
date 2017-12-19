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


def angry_spinlock(steps: int, target: int = 50000000) -> int:
    """Day 17 part 2: angry spinlock.

    >>> angry_spinlock(3, 10)
    9
    """
    buf1 = -1
    pos = 0
    for n in range(target):
        if n > 0 and n % 1000000 == 0:
            print(n, end=" \r")
        pos = (pos + steps) % (n + 1) + 1
        if pos == 1:
            buf1 = n + 1
    return buf1


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    print("Value after %d steps: %d" % (INPUT, spinlock(INPUT)))
    print("Angry spinlock value for %s: %d" % (INPUT, angry_spinlock(INPUT)))
