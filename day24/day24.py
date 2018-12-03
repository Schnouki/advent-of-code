#!/usr/bin/env python3

import doctest
import sys


def bridge_strength(comps) -> int:
    """Compute a bridge strength.

    >>> bridge_strength(((0, 3), (3, 7), (7, 4)))
    24
    >>> bridge_strength(((0, 1), (10, 1), (9, 10)))
    31
    """
    return sum(sum(c) for c in comps)


def valid_bridges(comps):
    """Iterate over all the valid bridges.

    >>> list(valid_bridges(((0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)))) # doctest: +NORMALIZE_WHITESPACE
    [[(0, 1)],
     [(0, 1), (10, 1)],
     [(0, 1), (10, 1), (9, 10)],
     [(0, 2)],
     [(0, 2), (2, 3)],
     [(0, 2), (2, 3), (3, 4)],
     [(0, 2), (2, 3), (3, 5)],
     [(0, 2), (2, 2)],
     [(0, 2), (2, 2), (2, 3)],
     [(0, 2), (2, 2), (2, 3), (3, 4)],
     [(0, 2), (2, 2), (2, 3), (3, 5)]]
    """

    def _iter(base: list, last: int, rest: set):
        if len(base) > 0:
            yield base
        for comp in rest:
            if last in comp:
                new_base = base + [comp]
                new_rest = rest - {comp}
                new_last = comp[1] if last == comp[0] else comp[0]
                yield from _iter(new_base, new_last, new_rest)

    yield from _iter([], 0, set(list(comps)))


def strongest_bridge(comps):
    """Find the strongest possible bridge.

    >>> strongest_bridge(((0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)))
    31
    """
    return max(bridge_strength(bridge) for bridge in valid_bridges(comps))


def strongest_longest_bridge(comps):
    """Find the strongest longest bridge.

    >>> strongest_longest_bridge(((0, 2), (2, 2), (2, 3), (3, 4), (3, 5), (0, 1), (10, 1), (9, 10)))
    19
    """
    bridges = list(valid_bridges(comps))
    max_len = max(len(bridge) for bridge in bridges)
    long_bridges = [bridge for bridge in bridges if len(bridge) == max_len]
    return max(bridge_strength(bridge) for bridge in long_bridges)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = []
            for line in fin:
                comp = line.split("/")
                data.append((int(comp[0]), int(comp[1])))
            data = tuple(data)

        print("Strongest bridge for %s: %d" % (fn, strongest_bridge(data)))
        print("Strongest longest bridge for %s: %d" %
              (fn, strongest_longest_bridge(data)))
