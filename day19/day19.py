#!/usr/bin/env python3

import collections
import doctest
import sys
import typing

TEST_DATA = """
     |
     |  +--+
     A  |  C
 F---|----E|--+
     |  |  |  D
     +B-+  +--+
""".strip("\n")


DIRECTIONS = {
    "up": (0, -1),
    "down": (0, 1),
    "left": (-1, 0),
    "right": (1, 0),
}


def walk_pipes(data: str) -> typing.Tuple[int, str]:
    """Walk along pipes and collect letters.

    >>> walk_pipes(TEST_DATA)
    (38, 'ABCDEF')
    """
    data = data.splitlines()
    letters = []
    direction = "down"
    steps = 0

    # Initial position
    y = 0
    x = data[y].index("|")

    # Previous position
    px, py = None, None

    while data[y][x] != " ":
        # print(f"(x, y) = ({x}, {y})\tcar={data[y][x]}", file=sys.stderr)
        car = data[y][x]
        if car.isalpha():
            letters.append(car)
        elif car == "+":
            for new_direction, new_offset in DIRECTIONS.items():
                nx, ny = x + new_offset[0], y + new_offset[1]
                if (nx, ny) == (px, py) or not (0 <= ny < len(data)) or not (0 <= nx < len(data[ny])):
                    continue
                ncar = data[ny][nx]
                # print(f"ncar ({new_direction}): '{ncar}'", file=sys.stderr)
                if ncar in (" ", "+"):
                    continue
                direction = new_direction
                break

        offset = DIRECTIONS[direction]
        px, py = x, y
        x += offset[0]
        y += offset[1]
        steps += 1

    return (steps, "".join(letters))


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read()

        print("Letters for %s: %s" % (fn, walk_pipes(data)))
