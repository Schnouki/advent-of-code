#!/usr/bin/env python3

from collections import Counter
import itertools as it
import math
from typing import List, Tuple

import attr

from aoc import Puzzle, run

TEST_MAP_1 = ".#..#\n.....\n#####\n....#\n...##"
TEST_MAP_2 = (
    "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n"
    "..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
)
TEST_MAP_3 = (
    ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n"
    ".###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n"
    "####################\n#.####....###.#.#.##\n##.#################\n"
    "#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n"
    ".#####..#.######.###\n##...#.##########...\n#.##########.#######\n"
    ".####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n"
    "#.#.#.#####.####.###\n###.##.####.##.#..##"
)


@attr.s
class Point:
    x: int = attr.ib()
    y: int = attr.ib()

    def direction(self, other):
        """Return the direction of other compared to self

        >>> Point(0, 0).direction(Point(1, 2))
        (1, 2)
        >>> Point(0, 0).direction(Point(-1, 2))
        (-1, 2)
        >>> Point(1, 1).direction(Point(0, 3))
        (-1, 2)
        >>> Point(1, 1).direction(Point(3, 5))
        (1, 2)
        >>> Point(1, 1).direction(Point(0, 0))
        (-1, -1)
        """
        dx, dy = other.x - self.x, other.y - self.y
        if dx == 0:
            return (0, 1 if dy > 0 else -1)
        if dy == 0:
            return (1 if dx > 0 else -1, 0)
        g = math.gcd(abs(dx), abs(dy))
        return (dx // g, dy // g)

    def count_directions(self, others: List["Point"]) -> Counter:
        """Count how many points are visible in each direction."""
        cnt = Counter()
        for other in others:
            if other == self:
                continue
            dir_ = self.direction(other)
            cnt[dir_] += 1
        return cnt


@attr.s
class Map:
    asteroids: List[Point] = attr.ib()

    @classmethod
    def from_text(cls, data: str) -> "Map":
        asteroids = []
        for y, line in enumerate(data.splitlines()):
            line = line.strip()
            for x, char in enumerate(line):
                if char == "#":
                    asteroids.append(Point(x, y))
        return cls(asteroids)

    def find_best_monitoring_spot(self) -> int:
        """Find the best monitoring spot.

        >>> Map.from_text(TEST_MAP_1).find_best_monitoring_spot()
        8
        >>> Map.from_text(TEST_MAP_2).find_best_monitoring_spot()
        33
        >>> Map.from_text(TEST_MAP_3).find_best_monitoring_spot()
        210
        """
        best = -1
        for ast in self.asteroids:
            cnt = ast.count_directions(self.asteroids)
            visible = len(cnt)
            best = max(best, visible)
        return best


class Day10(Puzzle):
    def prepare_data(self, raw_data):
        return Map.from_text(raw_data)

    def run_part1(self, data):
        return str(data.find_best_monitoring_spot())


run(obj=Day10())
