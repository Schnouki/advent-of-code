#!/usr/bin/env python3

from aoc import Puzzle, run


def fuel(mass: int) -> int:
    """Calculate fuel required to launch a module.

    >>> fuel(12)
    2
    >>> fuel(14)
    2
    >>> fuel(1969)
    654
    >>> fuel(100756)
    33583
    """
    return int(mass / 3) - 2


def full_fuel(mass: int) -> int:
    """Calculate fuel required to launch a module + its fuel.

    >>> full_fuel(12)
    2
    >>> full_fuel(1969)
    966
    >>> full_fuel(100756)
    50346
    """
    total_fuel = 0
    extra = fuel(mass)
    while extra > 0:
        total_fuel += extra
        extra = fuel(extra)
    return total_fuel


class Day01(Puzzle):
    def prepare_data(self, data):
        return [int(line) for line in data.splitlines()]

    def run_part1(self, data):
        return sum(fuel(m) for m in data)

    def run_part2(self, data):
        return sum(full_fuel(m) for m in data)


run(obj=Day01())
