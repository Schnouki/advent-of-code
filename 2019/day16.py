#!/usr/bin/env python3

import itertools as it
from typing import List

import numpy as np

from aoc import Puzzle, run


BASE_PATTERN = (0, 1, 0, -1)


def pattern(pos: int):
    pattern = it.cycle(BASE_PATTERN)
    while True:
        v = next(pattern)
        for _ in range(pos):
            yield v


class Day16(Puzzle):
    test_data = [
        "80871224585914546619083218645595",
        "19617804207202209144916044189917",
        "69317163492948606335995924319873",
    ]
    test_result_part1 = [
        "24176176",
        "73745418",
        "52432133",
    ]

    test_result_part2 = []

    def prepare_data(self, data: str) -> List[int]:
        return [int(c) for c in data.strip()]

    def run_part1(self, data: List[int]) -> str:
        matrix = np.matrix(
            [
                list(it.islice(pattern(pos), 1, len(data) + 1))
                for pos in range(1, len(data) + 1)
            ]
        )
        vector = np.array(data)
        for _ in range(100):
            res = matrix @ vector
            res = [int(str(m)[-1]) for m in res.tolist()[0]]
            vector = np.array(res)
        return "".join(str(n) for n in res[:8])


run(obj=Day16())
