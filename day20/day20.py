#!/usr/bin/env python3

import attr
import collections
import doctest
import itertools as it
import sys
import typing

TEST_DATA_1 = """p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>"""

TEST_DATA_2 = """p=<-6,0,0>, v=< 3,0,0>, a=< 0,0,0>
p=<-4,0,0>, v=< 2,0,0>, a=< 0,0,0>
p=<-2,0,0>, v=< 1,0,0>, a=< 0,0,0>
p=< 3,0,0>, v=<-1,0,0>, a=< 0,0,0>"""


# Initial: (a, v, p)
# Tick 1: (a, v+a, p+v+a)
# Tick 2: (a, v+2a, p+2v+3a)
# Tick 3: (a, v+3a, p+3v+6a)
# Tick 4: (a, v+4a, p+4v+10a)
# Tick 5: (a, v+5a, p+5v+15a)
# ...
# Tick n: (a, v+na, p+nv+(n(n+1)a)/2)

@attr.s
class Vec3:
    x = attr.ib()
    y = attr.ib()
    z = attr.ib()

    def __add__(self, other):
        if not isinstance(other, Vec3):
            return NotImplemented
        return Vec3(self.x + other.x,
                    self.y + other.y,
                    self.z + other.z)

    def __sub__(self, other):
        if not isinstance(other, Vec3):
            return NotImplemented
        return Vec3(self.x - other.x,
                    self.y - other.y,
                    self.z - other.z)

    def __rmul__(self, n: int):
        if type(n) is not int:
            return NotImplemented
        return Vec3(n * self.x,
                    n * self.y,
                    n * self.z)


@attr.s
class Particle:
    p = attr.ib()
    v = attr.ib()
    a = attr.ib()

    def tick(self):
        self.v += self.a
        self.p += self.v

    def at_tick(self, n: int) -> 'Particle':
        v = self.v + n * self.a
        p = self.p + n * self.v + ((n * (n + 1)) // 2) * self.a
        return Particle(p, v, self.a)

    @property
    def dist_origin(self):
        return abs(self.p.x) + abs(self.p.y) + abs(self.p.z)

    def collide(self, other: 'Particle') -> int:
        """Find at which tick the particles will collide.

        Returns -1 if they enver collide.

        >>> ps = parse(TEST_DATA_2); ps[0].collide(ps[1])
        2
        >>> ps = parse(TEST_DATA_2); ps[0].collide(ps[3])
        -1
        """
        # Particle 1 and particle 2 collide at tick n means that:
        # p1(n) == p2(n)
        # <=> p1z + n*v1z + (n(n+1)/2)*a1z == p2z + n*v2z + (n(n+1)/2)*a2z
        # <=> (p1z - p2z) + n*(v1z - v2z) + (n(n+1)/2)*(a1z - a2z) == (0, 0, 0)
        # So some virtual particle is exactly at the origin at some point.
        pp, vv, aa = self.p - other.p, self.v - other.v, self.a - other.a
        part = Particle(pp, vv, aa)

        dist = part.dist_origin
        if dist == 0:
            return 0
        prev_dist = dist + 1
        tick = 0
        while dist < prev_dist:
            prev_dist = dist
            tick += 1
            part.tick()
            dist = part.dist_origin
            if dist == 0:
                return tick
        return -1


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

    >>> closest_to_origin(TEST_DATA_1)
    0
    """
    particles = parse(data)

    ticks = 0
    all_incr = False
    while not all_incr:
        ticks += 1000
        dists = []
        for p0 in particles:
            pt = p0.at_tick(ticks)
            db = pt.dist_origin
            pt.tick()
            da = pt.dist_origin
            dists.append((db, da))
        all_incr = all(da > db for (db, da) in dists)

    # print(f"FINAL TICK {ticks}", file=sys.stderr)

    return min(range(len(particles)), key=lambda n: particles[n].at_tick(ticks).dist_origin)


def boom(data: str) -> int:
    """Find how many particle don't collide each other.

    >>> boom(TEST_DATA_2)
    1
    """
    particles = parse(data)

    next_tick = 0
    while next_tick >= 0:
        particles = [p.at_tick(next_tick) for p in particles]
        collisions = collections.defaultdict(lambda: set())
        for n, m in it.combinations(range(len(particles)), 2):
            p1, p2 = particles[n], particles[m]
            ctick = p1.collide(p2)
            if ctick >= 0:
                collisions[ctick].add(n)
                collisions[ctick].add(m)
        if len(collisions) == 0:
            break
        next_tick = min(collisions.keys())
        particles = [p for idx, p in enumerate(particles)
                     if idx not in collisions[next_tick]]
    return len(particles)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Particle closest to origin in %s: %d" %
              (fn, closest_to_origin(data)))
        print("Particles left after collisions in %s: %d" % (fn, boom(data)))
