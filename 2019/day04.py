#!/usr/bin/env python3

import itertools as it

import attr
import tqdm

from aoc import InlinePuzzle, run


def is_valid_password(pw: str) -> bool:
    """Check if a password is valid:

    - 6 digits
    - within range (not checked)
    - has 2 identical adjacent digits
    - left to right: digits never decrease

    >>> [is_valid_password(pw) for pw in ("122345", "111123", "145679", "111111", "223450")]
    [True, True, False, True, False]
    """
    digits = [int(c) for c in pw]
    if len(digits) != 6:
        return False
    prev = -1
    has_double = False
    for d in digits:
        if d < prev:
            return False
        if d == prev:
            has_double = True
        prev = d
    return has_double


class Day04(InlinePuzzle):
    puzzle_input = "153517-630395"

    def prepare_data(self, data):
        return tuple(int(piece) for piece in data.split("-"))

    def run_part1(self, data):
        count = 0
        for pw in tqdm.trange(data[0], data[1] + 1):
            if is_valid_password(str(pw)):
                count += 1
        return count


run(obj=Day04())
