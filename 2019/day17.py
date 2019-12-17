#!/usr/bin/env python3

from typing import List

import attr

from aoc import IntcodePuzzle, run
from intcode import Computer


TEST_MAP = """..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^.."""


def print_outputs(outputs: List[int]):
    out = "".join(chr(n) for n in outputs)
    print(out)


def find_scaffolds(outputs: List[int]):
    """Iterate over the scaffolds positions.

    >>> len(list(find_scaffolds(ord(c) for c in TEST_MAP)))
    38
    """
    x, y = 0, 0
    for n in outputs:
        if n == 10:
            x, y = 0, y + 1
            continue
        if n == 35:
            yield (x, y)
        x += 1


def find_intersections(outputs: List[int]):
    """Iterate over the scaffold interesections.

    >>> list(find_intersections(ord(c) for c in TEST_MAP))
    [(2, 2), (2, 4), (6, 4), (10, 4)]
    """
    scaffolds = list(find_scaffolds(outputs))
    for (x, y) in scaffolds:
        if (
            (x - 1, y) in scaffolds
            and (x + 1, y) in scaffolds
            and (x, y - 1) in scaffolds
            and (x, y + 1) in scaffolds
        ):
            yield (x, y)


class Day17(IntcodePuzzle):
    def run_part1(self, computer):
        computer = computer.copy()
        computer.run()

        intersections = find_intersections(computer.outputs)
        return str(sum(x * y for (x, y) in intersections))


run(obj=Day17())
