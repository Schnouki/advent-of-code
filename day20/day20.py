#!/usr/bin/env python3

import attr
import doctest
import sys
import typing

TEST_DATA = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"""


@attr.s
class Vec3:
    x: int = attr.ib()
    y: int = attr.ib()
    z: int = attr.ib()

    def __iadd__(self, other):
        self.x += other.x
        self.y += other.y
        self.z += other.z
        return self


@attr.s
class Particle:
    p: Vec3 = attr.ib()
    v: Vec3 = attr.ib()
    a: Vec3 = attr.ib()

    def tick(self):
        self.v += self.a
        self.p += self.v

    @property
    def dist_origin(self):
        return abs(self.p.x) + abs(self.p.y) + abs(self.p.z)


def parse(data: str) -> typing.List[Particle]:
    res = []
    for line in data.splitlines():
        pbeg = line.index("p=<") + 3
        pend = line.index(">", pbeg)
        pval = [int(v) for v in line[pbeg:pend].split(",")]
        pvec = Vec3(*pval)

        vbeg = line.index("v=<") + 3
        vend = line.index(">", vbeg)
        vval = [int(v) for v in line[vbeg:vend].split(",")]
        vvec = Vec3(*vval)

        abeg = line.index("a=<") + 3
        aend = line.index(">", abeg)
        aval = [int(v) for v in line[abeg:aend].split(",")]
        avec = Vec3(*aval)

        res.append(Particle(pvec, vvec, avec))
    return res


def closest_to_origin(data: str) -> int:
    """Find the particle that will stay closest to the origin in the long term.

    >>> closest_to_origin(TEST_DATA)
    0
    """
    particles = parse(data)

    ticks = 0
    all_incr = False
    while not (ticks % 1000 == 0 and all_incr):
        # print(f"TICK {ticks}", file=sys.stderr)
        ticks += 1
        dists = []
        for p in particles:
            db = p.dist_origin
            p.tick()
            da = p.dist_origin
            dists.append((db, da))
        all_incr = all(da > db for (db, da) in dists)

    return min(range(len(particles)), key=lambda n: particles[n].dist_origin)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Particle closest to origin in %s: %d" %
              (fn, closest_to_origin(data)))
