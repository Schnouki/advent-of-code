#!/usr/bin/env python3

from collections import Counter
import doctest
import enum
import sys
import typing


@enum.unique
class Direction(enum.Enum):
    N = 0
    NE = 1
    SE = 2
    S = 3
    SW = 4
    NW = 5


def _simplify(path: typing.List[Direction]) -> typing.List[Direction]:
    cnt = Counter(path)

    # 1st pass: remove steps that cancel each other
    for n in range(3):
        dir_a = Direction(n)
        dir_b = Direction(n + 3)
        to_rm = min(cnt[dir_a], cnt[dir_b])
        cnt[dir_a] -= to_rm
        cnt[dir_b] -= to_rm

    # 2nd pass: shortcuts! N + SE == NE, etc.
    for n in range(6):
        dir_a = Direction(n)
        dir_b = Direction((n + 2) % 6)
        dir_c = Direction((n + 1) % 6)
        to_rm = min(cnt[dir_a], cnt[dir_b])
        cnt[dir_a] -= to_rm
        cnt[dir_b] -= to_rm
        cnt[dir_c] += to_rm

    # Back to a list!
    path = []
    for dir_, nb in cnt.items():
        path += [dir_] * nb
    return path


def steps(data: str) -> int:
    """Number of steps required to reach the same place as path.

    >>> steps('ne,ne,ne')
    3
    >>> steps('ne,ne,sw,sw')
    0
    >>> steps('ne,ne,s,s')
    2
    >>> steps('se,sw,se,sw,sw')
    3
    """
    path = [Direction[step.upper()] for step in data.split(",")]
    new_path = _simplify(path)
    while new_path != path:
        path = new_path
        new_path = _simplify(path)
    return len(path)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Steps for %s: %d" % (fn, steps(data)))
