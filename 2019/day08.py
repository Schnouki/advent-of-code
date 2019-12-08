#!/usr/bin/env python3

from collections import Counter
from typing import List

import attr

from aoc import Puzzle, run


@attr.s
class Image:
    layers: List[List[int]] = attr.ib()
    counters: List[Counter] = attr.ib()

    @classmethod
    def from_list(cls, data: List[int], width: int, height: int) -> "Image":
        stride = width * height
        layers, counters = [], []
        for n in range(0, len(data), stride):
            layer = data[n : n + stride]
            counter = Counter(layer)
            layers.append(layer)
            counters.append(counter)
        return Image(layers, counters)

    def find_idx_fewest_0(self) -> int:
        return min(range(len(self.layers)), key=lambda idx: self.counters[idx][0])


class Day08(Puzzle):
    def prepare_data(self, data):
        int_data = [int(c) for c in data.strip()]
        return Image.from_list(int_data, 25, 6)

    def run_part1(self, data):
        idx = data.find_idx_fewest_0()
        cnt = data.counters[idx]
        res = cnt[1] * cnt[2]
        return str(res)


run(obj=Day08())
