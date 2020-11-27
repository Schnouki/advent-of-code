#!/usr/bin/env python3

import array
import itertools as it
from typing import List

import numpy as np

from aoc import Puzzle, run

import pyximport

pyximport.install()
import cday16


BASE_PATTERN = (0, 1, 0, -1)


def pattern(pos: int):
    pattern = it.cycle(BASE_PATTERN)
    while True:
        v = next(pattern)
        for _ in range(pos):
            yield v


class Day16(Puzzle):
    test_data_part1 = [
        "80871224585914546619083218645595",
        "19617804207202209144916044189917",
        "69317163492948606335995924319873",
    ]
    test_result_part1 = [
        "24176176",
        "73745418",
        "52432133",
    ]

    test_data_part2 = [
        "03036732577212944063491565474664",
        "02935109699940807407585447034323",
        "03081770884921959731165446850517",
    ]
    test_result_part2 = ["84462026", "78725270", "53553731"]

    def prepare_data(self, data: str) -> List[int]:
        return [int(c) for c in data.strip()]

    def run_part1(self, data: List[int]) -> str:
        matrix = np.matrix(
            [
                list(it.islice(pattern(pos), 1, len(data) + 1))
                for pos in range(1, len(data) + 1)
            ],
        )
        vec = np.array(data)
        for _ in range(100):
            vec = np.array(matrix @ vec)
            vec = np.abs(vec[0]) % 10
        return "".join(map(str, vec[:8]))

    def run_part2(self, data: List[int]) -> str:
        data = data * 10000

        offset = int("".join(map(str, data[:7])))
        assert offset >= (len(data) // 2)

        # We only care about data starting at offset!
        data = data[: offset - 1 : -1]
        vec = array.array("b", data)
        vec[0] = vec[0] % 10

        for phase in range(100):
            cday16.fft2_phase(vec)

        res = vec[:-9:-1]
        return "".join(map(str, res))


run(obj=Day16())
