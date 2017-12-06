#!/usr/bin/env python3

import doctest
import sys
import typing


def cycles(banks: typing.List[int], cycle_len: bool = False) -> int:
    """Compute the number of cycles before an infinite loop is detected.

    >>> cycles([0, 2, 7, 0])
    5
    >>> cycles([0, 2, 7, 0], True)
    4
    """
    seen = []
    cycles = 0

    tbanks = tuple(banks)
    while tbanks not in seen:
        seen.append(tbanks)
        cycles += 1

        top = max(banks)
        idx = banks.index(top)
        banks[idx] = 0
        while top > 0:
            idx = (idx + 1) % len(banks)
            banks[idx] += 1
            top -= 1
        tbanks = tuple(banks)

    if cycle_len:
        return len(seen) - seen.index(tbanks)
    else:
        return cycles


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()
        data = [int(v) for v in data.split()]

        print("Number of cycles for %s: %d" % (fn, cycles(data)))
        print("Cycle length for %s: %d" % (fn, cycles(data, True)))
