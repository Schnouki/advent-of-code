#!/usr/bin/env python3

import attr
import doctest
import enum
import sys
import typing

TEST_DATA = """0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5"""

Group = typing.Set[int]
Connections = typing.Dict[int, typing.Set[int]]


def parse_connections(data: str) -> Connections:
    """Parse a list of connections.

    >>> parse_connections(TEST_DATA)
    {0: {2}, 1: {1}, 2: {0, 3, 4}, 3: {2, 4}, 4: {2, 3, 6}, 5: {6}, 6: {4, 5}}
    """
    conns = {}
    for line in data.splitlines():
        line = line.split("<->", 1)
        source = int(line[0])
        targets = [int(n) for n in line[1].split(",")]
        conns[source] = set(targets)
    return conns


def programs_in_group(data: str, leader: int = 0) -> int:
    """Count how many programs are in a group.

    >>> programs_in_group(TEST_DATA, 0)
    6
    """
    old_size = 0
    group = {leader}
    connections = parse_connections(data)
    while len(group) != old_size:
        old_size = len(group)
        for n in group.copy():
            group.update(connections[n])
    return len(group)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Prgrams in group 0 for %s: %d" % (fn, programs_in_group(data)))
