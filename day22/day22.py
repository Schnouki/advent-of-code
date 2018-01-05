#!/usr/bin/env python3

import doctest
import enum
import itertools as it
import sys

TEST_GRID = "..#\n#..\n..."

STEPS_1 = 10000
STEPS_2 = 10000000


class EnumOrderMixin:
    def offset(self, n):
        items = list(type(self))
        idx = items.index(self)
        idx = (idx + n) % len(items)
        return items[idx]


@enum.unique
class Direction(EnumOrderMixin, enum.Enum):
    N = (0, -1)
    E = (+1, 0)
    S = (0, +1)
    W = (-1, 0)


@enum.unique
class State(EnumOrderMixin, enum.Enum):
    C = "."
    W = "W"
    I = "#"
    F = "F"


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
            self.direction = self.direction.offset(1)
            self.infected.remove(self.pos)

        else:
            self.direction = self.direction.offset(-1)
            self.infected.add(self.pos)
            self.infections += 1

        move = self.direction.value
        self.pos = (self.pos[0] + move[0], self.pos[1] + move[1])


class ClusterEvolved:
    def __init__(self, grid: str):
        self.direction = Direction.N
        self.pos = (0, 0)

        self.grid = {}
        self.infections = 0

        lines = grid.splitlines()
        w, h = len(lines[0]), len(lines)
        x0, y0 = -(w // 2), -(h // 2)
        for x, y in it.product(range(w), range(h)):
            if lines[y][x] == "#":
                self.grid[(x + x0, y + y0)] = State.I

    def step(self):
        state = self.grid.get(self.pos, State.C)

        # Direction
        if state == State.C:
            self.direction = self.direction.offset(-1)
        elif state == State.I:
            self.direction = self.direction.offset(1)
        elif state == State.F:
            self.direction = self.direction.offset(2)

        # Update state
        state = state.offset(1)
        if state == state.I:
            self.infections += 1
        if state == State.C:
            del self.grid[self.pos]
        else:
            self.grid[self.pos] = state

        # Move
        move = self.direction.value
        self.pos = (self.pos[0] + move[0], self.pos[1] + move[1])


def count_infections(grid: str, steps: int) -> int:
    """Count the number of infections after a given number of steps.

    >>> count_infections(TEST_GRID, 70)
    41
    >>> count_infections(TEST_GRID, STEPS_1)
    5587
    """
    cluster = Cluster(grid)
    for _ in range(steps):
        cluster.step()
    return cluster.infections


def count_infections_evolved(grid: str, steps: int) -> int:
    """Count the number of infections (evolved) after a given number of steps.

    >>> count_infections_evolved(TEST_GRID, 100)
    26
    >>> count_infections_evolved(TEST_GRID, STEPS_2)
    2511944
    """
    cluster = ClusterEvolved(grid)
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

        print("Infections after %d steps for %s: %d" %
              (STEPS_1, fn, count_infections(data, STEPS_1)))
        print("Infections after %d evolved steps for %s: %d" %
              (STEPS_2, fn, count_infections_evolved(data, STEPS_2)))
