#!/usr/bin/env python3

from typing import List, Optional

from aoc import Puzzle, run

import attr


@attr.s
class Object:
    name: str = attr.ib()
    depth: int = attr.ib(default=0)
    satellites: List["Object"] = attr.ib(factory=lambda: [])

    @classmethod
    def from_input(cls, lines):
        objects = {}
        for name_center, name_satellite in lines:
            if name_center not in objects:
                objects[name_center] = Object(name_center)
            if name_satellite not in objects:
                objects[name_satellite] = Object(name_satellite)
        for name_center, name_satellite in lines:
            center, satellite = objects[name_center], objects[name_satellite]
            center.satellites.append(satellite)
        com = objects["COM"]
        com.update_depth(0)
        return com

    def update_depth(self, depth):
        self.depth = depth
        for sat in self.satellites:
            sat.update_depth(depth + 1)

    def iter_all(self):
        yield self
        for sat in self.satellites:
            yield from sat.iter_all()

    def find_object(self, name: str) -> Optional["Object"]:
        for obj in self.iter_all():
            if obj.name == name:
                return obj

    def count_orbits(self) -> int:
        return sum(obj.depth for obj in self.iter_all())


class Day06(Puzzle):
    test_data = ["COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"]
    test_result_part1 = ["3,7,0,42"]

    def prepare_data(self, data):
        return Object.from_input([line.split(")") for line in data.splitlines()])

    def run_part1(self, data):
        if self.test_mode:
            D = data.find_object("D")
            L = data.find_object("L")
            COM = data
            return "%d,%d,%d,%d" % (D.depth, L.depth, COM.depth, data.count_orbits())
        return str(data.count_orbits())


run(obj=Day06())
