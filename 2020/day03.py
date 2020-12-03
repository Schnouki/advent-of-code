#!/usr/bin/env python3

from dataclasses import dataclass
from typing import Set, Tuple

from aoc import Puzzle, run

Coord = Tuple[int, int]

@dataclass
class Map:
    w: int
    h: int
    trees: Set[Coord]

    def add_line(self, line: str):
        w = len(line)
        if self.w == 0:
            self.w = w
        elif self.w != w:
            raise ValueError()

        for x in range(w):
            if line[x] == "#":
                self.trees.add((x, self.h))
        self.h += 1

    def wrap(self, pos: Coord) -> Coord:
        x, y = pos
        return (x % self.w, y)

    def count_trees(self, right: int, down: int) -> int:
        n = 0
        x = 0
        for y in range(down, self.h, down):
            x = (x + right) % self.w
            if (x, y) in self.trees:
                n += 1
        return n


class Day03(Puzzle):
    test_data = ["""..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"""]
    test_result_part1 = [7]
    test_result_part2 = [336]

    def prepare_data(self, data) -> Map:
        map_ = Map(0, 0, set())
        for line in data.splitlines():
            map_.add_line(line)
        return map_

    def run_part1(self, data: Map):
        return data.count_trees(3, 1)

    def run_part2(self, data: Map):
        slopes = ((1, 1),
                  (3, 1),
                  (5, 1),
                  (7, 1),
                  (1, 2))
        res = 1
        for right, down in slopes:
            res *= data.count_trees(right, down)
        return res


if __name__ == "__main__":
    run(obj=Day03())
