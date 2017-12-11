#!/usr/bin/env python3

import attr
import doctest
import enum
import sys
import typing

# https://www.redblobgames.com/grids/hexagons/: "odd-q" vertical layour


@enum.unique
class Direction(enum.Enum):
    N = (0, -1)
    NE = (1, -1)
    SE = (1, 0)
    S = (0, 1)
    SW = (-1, 1)
    NW = (-1, 0)


@attr.s
class Hex:
    q: int = attr.ib()  # "column"
    r: int = attr.ib()  # "row"

    @staticmethod
    def distance(a: 'Hex', b: 'Hex') -> int:
        return (abs(a.q - b.q)
                + abs(a.q + a.r - b.q - b.r)
                + abs(a.r - b.r)) // 2

    def __add__(self, dir_: Direction) -> 'Hex':
        if not isinstance(dir_, Direction):
            raise TypeError(dir_)
        return Hex(self.q + dir_.value[0],
                   self.r + dir_.value[1])

    @property
    def distance_from_origin(self):
        zero = Hex(0, 0)
        return Hex.distance(self, zero)


def walk(data: str) -> typing.List[typing.Tuple[int, int]]:
    """Return the coordinates after every step in the path."""
    coords = Hex(0, 0)
    for step in data.split(","):
        coords += Direction[step.upper()]
        yield coords


def steps(data: str) -> int:
    """Number of steps required to reach the same place as path.

    >>> steps('ne,ne,ne')
    3
    >>> steps('ne,ne,sw,sw')
    0
    >>> steps('ne,ne,s,s')
    2
    >>> steps('se,sw,se,sw,sw')
    3
    """
    for coord in walk(data):
        pass
    return coord.distance_from_origin


def max_distance(data: str) -> int:
    """Max distance the boy got from the starting position."""
    return max(coord.distance_from_origin for coord in walk(data))


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Steps for %s: %d" % (fn, steps(data)))
        print("Max distance for %s: %d" % (fn, max_distance(data)))
