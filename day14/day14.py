#!/usr/bin/env python3

import itertools as it
import doctest
import sys
import typing

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


def regions(key: str) -> int:
    """Count the number of regions in the disk data.

    >>> regions('flqrgnkx')
    1242
    """
    # First build the whole disk data. Internally, we'll use "." for empty
    # squares (0 bits in the hashes), "#" for used squares that are not yet
    # counted in a region, and "@" for squares that are already counted in a
    # region.
    disk = []
    for n in range(128):
        knot = knot_hash("%s-%d" % (key, n))
        bits = bin(int(knot, 16))[2:]
        bits = ("0" * (128 - len(bits))) + bits
        bits = bits.replace("0", ".").replace("1", "#")
        disk.append(list(bits))

    # Some helpers…
    def _show(N=128):
        for n in range(N):
            print("".join(disk[n][:N]))

    def find_uncounted() -> typing.Tuple[int, int]:
        for y, x in it.product(range(128), repeat=2):
            if disk[y][x] == "#":
                return (x, y)
        return (-1, -1)

    def fill(x, y):
        if disk[y][x] != "#":
            return
        disk[y][x] = "@"
        others = ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
        for nx, ny in others:
            if 0 <= nx < 128 and 0 <= ny < 128 and disk[ny][nx] == "#":
                fill(nx, ny)

    # Now count the regions!
    regions = 0
    ux, uy = find_uncounted()
    while ux >= 0 and uy >= 0:
        regions += 1
        # print("\n\nREGION %d (start: %d, %d)" % (regions, ux, uy))
        fill(ux, uy)
        # _show(8)
        ux, uy = find_uncounted()

    return regions


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    KEY = 'uugsqrei'
    print("Squares used for %s: %d" % (KEY, squares_used(KEY)))
    print("Regions for %s: %d" % (KEY, regions(KEY)))
