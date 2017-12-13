#!/usr/bin/env python3

import doctest
import sys

TEST_DATA = """0: 3
1: 2
4: 4
6: 4"""


class Layer:
    def __init__(self, depth, size):
        self.depth = depth
        self.size = size
        self._cycle_size = 2 * (size - 1)

    def is_caught(self, clock):
        return clock % self._cycle_size == 0

    def get_severity(self, clock):
        if self.is_caught(clock):
            return self.depth * self.size
        return 0


class Firewall:
    def __init__(self, data: str):
        self.layers = {}
        for line in data.splitlines():
            depth, size = line.split(":")
            depth, size = int(depth), int(size)
            self.layers[depth] = Layer(depth, size)

    def full_travel(self) -> int:
        """Make a packet travel through the firewall, and return its strip severity."""
        severity = 0
        for pos in range(max(self.layers.keys()) + 1):
            layer = self.layers.get(pos, None)
            if layer:
                severity += layer.get_severity(pos)
        return severity


def severity(data: str) -> int:
    """Compute the severity for a whole trip.

    >>> severity(TEST_DATA)
    24
    """
    fw = Firewall(data)
    return fw.full_travel()


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Severity for %s: %d" % (fn, severity(data)))
