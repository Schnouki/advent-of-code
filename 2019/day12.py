#!/usr/bin/env python3

import itertools as it
from typing import List

import attr

from aoc import Puzzle, run


@attr.s
class Vec3:
    x: int = attr.ib()
    y: int = attr.ib()
    z: int = attr.ib()

    def __add__(self, other):
        return Vec3(self.x + other.x, self.y + other.y, self.z + other.z)

    def __iadd__(self, other):
        self.x += other.x
        self.y += other.y
        self.z += other.z
        return self

    def __abs__(self):
        return abs(self.x) + abs(self.y) + abs(self.z)


@attr.s
class Moon:
    pos: Vec3 = attr.ib()
    vel: Vec3 = attr.ib(factory=lambda: Vec3(0, 0, 0))

    @property
    def potential_energy(self):
        return abs(self.pos)

    @property
    def kinetic_energy(self):
        return abs(self.vel)

    @property
    def total_energy(self):
        return self.potential_energy * self.kinetic_energy


@attr.s
class System:
    moons: List[Moon] = attr.ib()
    max_steps: int = attr.ib()

    def step(self):
        self.apply_gravity()
        self.apply_velocity()

    def apply_gravity(self):
        for m1, m2 in it.combinations(self.moons, 2):
            if m1.pos.x > m2.pos.x:
                m1.vel.x -= 1
                m2.vel.x += 1
            elif m1.pos.x < m2.pos.x:
                m1.vel.x += 1
                m2.vel.x -= 1
            if m1.pos.y > m2.pos.y:
                m1.vel.y -= 1
                m2.vel.y += 1
            elif m1.pos.y < m2.pos.y:
                m1.vel.y += 1
                m2.vel.y -= 1
            if m1.pos.z > m2.pos.z:
                m1.vel.z -= 1
                m2.vel.z += 1
            elif m1.pos.z < m2.pos.z:
                m1.vel.z += 1
                m2.vel.z -= 1

    def apply_velocity(self):
        for m in self.moons:
            m.pos += m.vel

    @property
    def total_energy(self):
        return sum(m.total_energy for m in self.moons)


class Day12(Puzzle):
    test_data = [
        "steps=10\n<x=-1, y=0, z=2>\n<x=2, y=-10, z=-7>\n<x=4, y=-8, z=8>\n<x=3, y=5, z=-1>",
        "steps=100\n<x=-8, y=-10, z=0>\n<x=5, y=5, z=10>\n<x=2, y=-7, z=3>\n<x=9, y=-8, z=-3>",
    ]
    test_result_part1 = ["179", "1940"]

    def prepare_data(self, data):
        lines = data.splitlines()

        moons = []
        steps = 1000

        if lines[0].startswith("steps="):
            steps = int(lines.pop(0)[6:])

        for line in lines:
            comps = line.split(",")
            coords = [int(comp.split("=")[1].rstrip(">")) for comp in comps]
            moons.append(Moon(Vec3(*coords)))

        return System(moons, steps)

    def run_part1(self, data):
        for step in range(data.max_steps):
            data.step()
        return str(data.total_energy)


run(obj=Day12())
