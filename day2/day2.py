#!/usr/bin/env python3

import doctest
import itertools as it
import sys


def checksum_diff(data: str) -> int:
    r"""Compute the "diff" checksum for a spreadsheet.

    >>> checksum_diff("5 1 9 5\n7 5 3\n2 4 6 8")
    18
    """
    cksum = 0
    for line in data.splitlines():
        tab = [int(x) for x in line.split()]
        diff = max(tab) - min(tab)
        cksum += diff
    return cksum


def checksum_div(data: str) -> int:
    r"""Compute the "div" checksum for a spreadsheet.

    >>> checksum_div("5 9 2 8\n9 4 7 3\n3 8 6 5")
    9
    """
    cksum = 0
    for line in data.splitlines():
        tab = [int(x) for x in line.split()]
        for n, m in it.permutations(range(len(tab)), 2):
            x, y = tab[n], tab[m]
            if x % y == 0:
                cksum += x // y
                break
            if y % x == 0:
                cksum += y // x
                break
    return cksum


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()
        print("Diff checksum for {}: {}".format(fn, checksum_diff(data)))
        print("Div checksum for {}: {}".format(fn, checksum_div(data)))
