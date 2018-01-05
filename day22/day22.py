#!/usr/bin/env python3

import doctest
import enum
import itertools as it
import sys

TEST_GRID = "..#\n#..\n..."


@enum.unique
class Direction(enum.Enum):
    N = (0, -1)
    E = (+1, 0)
    S = (0, +1)
    W = (-1, 0)

    def offset(self, n):
        dirs = list(Direction)
        idx = dirs.index(self)
        idx = (idx + n) % len(dirs)
        return dirs[idx]

    @property
    def right(self):
        return self.offset(1)

    @property
    def left(self):
        return self.offset(-1)


class Cluster:
    def __init__(self, grid: str):
        self.direction = Direction.N
        self.pos = (0, 0)

        self.infected = set()
        self.infections = 0

        lines = grid.splitlines()
        w, h = len(lines[0]), len(lines)
        x0, y0 = -(w // 2), -(h // 2)
        for x, y in it.product(range(w), range(h)):
            if lines[y][x] == "#":
                self.infected.add((x + x0, y + y0))

    def step(self):
        if self.pos in self.infected:
            self.direction = self.direction.right
            self.infected.remove(self.pos)

        else:
            self.direction = self.direction.left
            self.infected.add(self.pos)
            self.infections += 1

        move = self.direction.value
        self.pos = (self.pos[0] + move[0], self.pos[1] + move[1])


def count_infections(grid: str, steps: int) -> int:
    """Count the number of infections after a given number of steps.

    >>> count_infections(TEST_GRID, 70)
    41
    >>> count_infections(TEST_GRID, 10000)
    5587
    """
    cluster = Cluster(grid)
    for _ in range(steps):
        cluster.step()
    return cluster.infections


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Infections after 10000 steps for %s: %d" %
              (fn, count_infections(data, 10000)))
