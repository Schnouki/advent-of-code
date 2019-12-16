#!/usr/bin/env python3

from typing import List

from aoc import Puzzle, run


def fft_phase(data: List[int]) -> List[int]:
    """Compute one phase of Flawed Frequency Transmission

    >>> fft_phase([1, 2, 3, 4, 5, 6, 7, 8])
    [4, 8, 2, 2, 6, 1, 5, 8]
    >>> fft_phase([4, 8, 2, 2, 6, 1, 5, 8])
    [3, 4, 0, 4, 0, 4, 3, 8]
    """
    output = []
    for pos in range(1, len(data) + 1):
        pattern = ([0] * pos) + ([1] * pos) + ([0] * pos) + ([-1] * pos)
        rep = (len(data) + 1) // len(pattern)
        pattern += pattern * rep
        pattern = pattern[1:]
        m = sum(data[i] * pattern[i] for i in range(len(data)))
        output.append(int(str(m)[-1]))
    return output


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
        for _ in range(100):
            data = fft_phase(data)
        return "".join(str(n) for n in data[:8])


run(obj=Day16())
