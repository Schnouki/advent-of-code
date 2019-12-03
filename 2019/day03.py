#!/usr/bin/env python3

import itertools as it
from typing import List, Optional

import attr

from aoc import Puzzle, run


"""
Crossed Wires
=============

Coordinate system
-----------------

The "central port" is at (x, y) = (0, 0). Up means -y, down +y, left -x, right
+x.
 """


@attr.s
class Pos:
    x: int = attr.ib()
    y: int = attr.ib()

    @property
    def dist_central(self) -> int:
        return abs(self.x) + abs(self.y)


@attr.s
class Wire:
    p1: Pos = attr.ib()
    p2: Pos = attr.ib()

    def __attrs_post_init__(self):
        # Normalize points to make sure that p1.x < p2.x if the wire is
        # horizontal, and p1.y < p2.y if it's vertical.
        direction = self.direction
        if (direction == "H" and self.p1.x > self.p2.x) or (
            direction == "V" and self.p1.y > self.p2.y
        ):
            self.p1, self.p2 = self.p2, self.p1

    @property
    def direction(self):
        return "V" if self.p1.x == self.p2.x else "H"

    def intersection(self, other: "Wire") -> Optional[Pos]:
        # Same direction: they don't intersect.
        if self.direction == other.direction:
            return None
        # Ensure "self" is the horizontal one.
        if self.direction != "H":
            return other.intersection(self)

        # self is horizontal, other is vertical
        if (
            other.p1.y <= self.p1.y <= other.p2.y
            and self.p1.x <= other.p1.x <= self.p2.x
        ):
            return Pos(other.p1.x, self.p1.y)
        return None


def parse_path(path: str) -> List[Wire]:
    start = Pos(0, 0)
    wires = []
    for piece in path.split(","):
        direction = piece[0]
        length = int(piece[1:])
        if direction == "U":
            end = Pos(start.x, start.y - length)
        elif direction == "D":
            end = Pos(start.x, start.y + length)
        elif direction == "L":
            end = Pos(start.x - length, start.y)
        elif direction == "R":
            end = Pos(start.x + length, start.y)
        else:
            raise ValueError(f"Bad wire piece {piece}")
        wires.append(Wire(start, end))
        start = end
    return wires


class Day03(Puzzle):
    test_data = [
        "R8,U5,L5,D3\nU7,R6,D4,L4",
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83",
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
    ]
    test_result_part1 = ["6", "159", "135"]

    def prepare_data(self, data):
        return [parse_path(path) for path in data.splitlines()]

    def run_part1(self, data):
        path1, path2 = data[0], data[1]
        intersections = []
        for w1, w2 in it.product(path1, path2):
            pos = w1.intersection(w2)
            if pos and not (pos.x == pos.y == 0):
                intersections.append(pos)
        return str(min(pos.dist_central for pos in intersections))


run(obj=Day03())
