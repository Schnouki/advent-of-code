#!/usr/bin/env python3

import bisect
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

    def distance(self, other):
        """Manhattan distance between 2 points."""
        return abs(other.x - self.x) + abs(other.y - self.y)

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

    def find_visible(self, others: List["Point"]) -> List["LineOfSight"]:
        los = {}
        for other in others:
            if other == self:
                continue
            dir_ = self.direction(other)
            if dir_ in los:
                los[dir_].add_asteroid(other)
            else:
                los[dir_] = LineOfSight.from_origin(self, other, dir_[0], dir_[1])
        return list(los.values())


def dir_to_angle(dx: int, dy: int) -> float:
    """Convert a direction to an angle.

    >>> dir_to_angle(0, -1)
    0.0
    >>> dir_to_angle(1, -1)
    45.0
    >>> dir_to_angle(1, 0)
    90.0
    >>> dir_to_angle(-1, 1)
    225.0
    """
    angle = math.degrees(math.atan2(dx, -dy))
    if angle < 0:
        angle += 360
    return angle


@attr.s
class LineOfSight:
    origin: Point = attr.ib()
    dx: int = attr.ib()
    dy: int = attr.ib()
    angle: float = attr.ib()
    asteroids: List[Point] = attr.ib()
    distances: List[int] = attr.ib()

    @classmethod
    def from_origin(
        cls, origin: Point, other: Point, dx: int, dy: int
    ) -> "LineOfSight":
        angle = dir_to_angle(dx, dy)
        return cls(origin, dx, dy, angle, [other], [origin.distance(other)])

    def add_asteroid(self, asteroid: Point):
        dist = self.origin.distance(asteroid)
        idx = bisect.bisect(self.distances, dist)
        self.distances.insert(idx, dist)
        self.asteroids.insert(idx, asteroid)


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

    def find_best_spot(self) -> Tuple[Point, int, List[LineOfSight]]:
        """Find the best monitoring spot."""
        best = (None, -1, None)
        for ast in self.asteroids:
            visible = ast.find_visible(self.asteroids)
            nb_visible = len(visible)
            if nb_visible > best[1]:
                best = (ast, nb_visible, visible)
        return best


class Day10(Puzzle):
    test_data_part1 = [
        TEST_MAP_1,
        TEST_MAP_2,
        TEST_MAP_3,
    ]
    test_result_part1 = ["8", "33", "210"]

    test_data_part2 = [TEST_MAP_3]
    test_result_part2 = ["802"]

    def prepare_data(self, raw_data):
        return Map.from_text(raw_data)

    def run_part1(self, data):
        return str(data.find_best_spot()[1])

    def run_part2(self, data):
        shot, last = 0, None
        loss = data.find_best_spot()[2]
        loss.sort(key=lambda l: l.angle)
        while True:
            for los in loss:
                if los.asteroids:
                    last = los.asteroids.pop(0)
                    shot += 1

                    if shot == 200:
                        return str(100 * last.x + last.y)


run(obj=Day10())
