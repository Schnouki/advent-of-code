#!/usr/bin/env python3

from typing import List, Optional

from aoc import Puzzle, run

import attr


@attr.s
class Object:
    name: str = attr.ib()
    center: Optional["Object"] = attr.ib(default=None)
    satellites: List["Object"] = attr.ib(factory=lambda: [])

    @property
    def depth(self) -> int:
        if self.center is None:
            return 0
        return 1 + self.center.depth

    @property
    def centers(self) -> List[str]:
        if self.center is None:
            return []
        return [self.name] + self.center.centers

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
            satellite.center = center
            center.satellites.append(satellite)
        com = objects["COM"]
        return com

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
    test_result_part2 = ["4"]

    def prepare_data(self, data):
        return Object.from_input([line.split(")") for line in data.splitlines()])

    def run_part1(self, data):
        if self.test_mode:
            D = data.find_object("D")
            L = data.find_object("L")
            COM = data
            return "%d,%d,%d,%d" % (D.depth, L.depth, COM.depth, data.count_orbits())
        return str(data.count_orbits())

    def run_part2(self, data):
        if self.test_mode:
            you_center = data.find_object("K")
            san_center = data.find_object("I")
        else:
            you_center = data.find_object("YOU").center
            san_center = data.find_object("SAN").center

        you_centers = you_center.centers
        san_centers = san_center.centers

        # Find 1st common parent
        common_center = None
        for center in you_centers:
            if center in san_centers:
                common_center = center
                break
        # Distance between each center and the common parent
        dist = you_centers.index(common_center) + san_centers.index(common_center)
        return str(dist)


run(obj=Day06())
