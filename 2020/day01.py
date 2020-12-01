#!/usr/bin/env python3

import itertools as it

from aoc import Puzzle, run



class Day01(Puzzle):
    test_data = ["1721\n979\n366\n299\n675\n1456"]
    test_result_part1 = [514579]
    test_result_part2 = [241861950]

    def prepare_data(self, data):
        return set(int(line) for line in data.splitlines())

    def run_part1(self, data):
        # Find a pair that sums to 2020, and return its product
        for a in data:
            b = 2020 - a
            if b in data:
                return a*b

    def run_part2(self, data):
        # Find 3 numbers that sum to 2020, and return their product
        for a, b in it.combinations(data, 2):
            c = 2020 - a - b
            if c in data:
                return a*b*c


if __name__ == "__main__":
    run(obj=Day01())
