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
        self.cycle = 2 * (size - 1)

    def is_caught(self, clock):
        return clock % self.cycle == 0

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

    def is_travel_safe(self, delay: int) -> bool:
        """Check if a travel with the specified delay is save."""
        for pos in range(max(self.layers.keys()) + 1):
            layer = self.layers.get(pos, None)
            if layer:
                if layer.is_caught(pos + delay):
                    return False
        return True


def severity(data: str) -> int:
    """Compute the severity for a whole trip.

    >>> severity(TEST_DATA)
    24
    """
    fw = Firewall(data)
    return fw.full_travel()


def safe_delay(data: str) -> int:
    """Find the smallest delay that guarantees a safe travel through the firewall.

    >>> safe_delay(TEST_DATA)
    10
    """
    delay = 0
    fw = Firewall(data)
    while not fw.is_travel_safe(delay):
        delay += 1
    return delay


def safe_delay_fast(data: str) -> int:
    """Find the smallest delay for a safe travel, without brute-forcing…

    >>> safe_delay_fast(TEST_DATA)
    10
    """
    # Let's call the delay D. For each layer, we're caught when
    # (D + depth) % cycle == 0
    # <=> D % cycle + depth % cycle == 0
    # <=> D % cycle = -depth % cycle
    # (<=> D = cycle*N - depth)
    fw = Firewall(data)
    constraints = [(layer.cycle, layer.depth) for layer in fw.layers.values()]
    delay = -1
    bad = True
    while bad:
        delay += 1
        bad = False
        for cycle, depth in constraints:
            if (delay + depth) % cycle == 0:
                bad = True
                break
    return delay


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Severity for %s: %d" % (fn, severity(data)))
        print("Safe delay for %s: %d" % (fn, safe_delay_fast(data)))
