#!/usr/bin/env python3

import doctest
import itertools as it
import sys
import typing


class BaseSpiral:
    def __init__(self):
        self.memory = [[1]]
        self.size = 1

    def next_value(self, x, y):
        raise NotImplemented

    def grow(self):
        self.size += 1
        if self.size % 2 == 0:
            self._grow_even()
        else:
            self._grow_odd()

    def _grow_even(self):
        # Add an empty top row
        self.memory.insert(0, [None for _ in range(self.size)])

        # Add an empty right column
        for n in range(1, self.size):
            self.memory[n].append(None)

        # Now fill the right column
        x = self.size - 1
        for y in range(self.size - 1, -1, -1):
            self.memory[y][x] = self.next_value(x, y)

        # And fill the top row
        y = 0
        for x in range(self.size - 2, -1, -1):
            self.memory[y][x] = self.next_value(x, y)

    def _grow_odd(self):
        # Add an empty left column
        for n in range(self.size - 1):
            self.memory[n].insert(0, None)

        # Add an empty bottom row
        self.memory.append([None for _ in range(self.size)])

        # Now fill the left column
        x = 0
        for y in range(self.size):
            self.memory[y][x] = self.next_value(x, y)

        # And fill the bottom row
        y = self.size - 1
        for x in range(1, self.size):
            self.memory[y][x] = self.next_value(x, y)


class SimpleSpiral(BaseSpiral):
    def __init__(self):
        super().__init__()
        self.last = 1

    def next_value(self, x, y):
        self.last += 1
        return self.last


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

    s = SimpleSpiral()
    while s.last < target:
        s.grow()
    return s.memory


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


class AdjacentSpiral(BaseSpiral):
    class TargetReached(Exception):
        pass

    def run(self, target):
        self.target = target
        try:
            while True:
                self.grow()
        except self.TargetReached:
            pass

    def next_value(self, x, y):
        val = 0
        for dx, dy in it.product((-1, 0, 1), repeat=2):
            if dx == 0 and dy == 0:
                pass
            v = None
            xx, yy = x + dx, y + dy
            if xx >= 0 and yy >= 0:
                try:
                    v = self.memory[yy][xx]
                except IndexError:
                    pass
            if v is not None:
                val += v
        if val > self.target:
            self.last_value = val
            raise self.TargetReached()
        return val


def first_gt(target: int) -> int:
    """Compute the first value written that is larger than the target.

    This time the spiral memory is filled using the "largest adjacent value" method.

    >>> first_gt(2)
    4
    >>> first_gt(8)
    10
    >>> first_gt(150)
    304
    >>> first_gt(800)
    806
    """

    s = AdjacentSpiral()
    s.run(target)
    return s.last_value


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    data1 = 289326
    print("Steps for %d: %d" % (data1, steps(data1)))
    print("First adjacent value > %d: %d" % (data1, first_gt(data1)))
