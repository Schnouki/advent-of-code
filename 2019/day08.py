#!/usr/bin/env python3

from collections import Counter
from typing import List

import attr

from aoc import Puzzle, run

PIXELS = {
    (0, 0, 0, 0): " ",
    (0, 0, 0, 1): "▗",
    (0, 0, 1, 0): "▖",
    (0, 1, 0, 0): "▝",
    (1, 0, 0, 0): "▘",
    (0, 0, 1, 1): "▄",
    (1, 1, 0, 0): "▀",
    (0, 1, 0, 1): "▐",
    (1, 0, 1, 0): "▌",
    (1, 0, 0, 1): "▚",
    (0, 1, 1, 0): "▞",
    (0, 1, 1, 1): "▟",
    (1, 0, 1, 1): "▙",
    (1, 1, 0, 1): "▜",
    (1, 1, 1, 0): "▛",
    (1, 1, 1, 1): "█",
}


@attr.s
class Image:
    width: int = attr.ib()
    height: int = attr.ib()
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
        return Image(width, height, layers, counters)

    def find_idx_fewest_0(self) -> int:
        return min(range(len(self.layers)), key=lambda idx: self.counters[idx][0])

    def get_color(self, x: int, y: int) -> int:
        if x > self.width or y > self.height:
            return 0
        pos = y * self.width + x
        for layer in self.layers:
            color = layer[pos]
            if color != 2:
                return color
        raise RuntimeError(f"Could not find color at ({x}, {y})!")

    def decode(self) -> str:
        res = ""
        for y in range(self.height):
            for x in range(self.width):
                color = self.get_color(x, y)
                res += "█" if color == 1 else " "
            res += "\n"
        return res.rstrip()

    def decode_mini(self) -> str:
        res = ""
        for y in range(int(self.height / 2)):
            for x in range(int(self.width / 2)):
                arr = (
                    self.get_color(x * 2, y * 2),
                    self.get_color(x * 2 + 1, y * 2),
                    self.get_color(x * 2, y * 2 + 1),
                    self.get_color(x * 2 + 1, y * 2 + 1),
                )
                res += PIXELS[arr]
            res += "\n"
        return res.rstrip()


class Day08(Puzzle):
    def prepare_data(self, data):
        int_data = [int(c) for c in data.strip()]
        return Image.from_list(int_data, 25, 6)

    def run_part1(self, data):
        idx = data.find_idx_fewest_0()
        cnt = data.counters[idx]
        res = cnt[1] * cnt[2]
        return str(res)

    def run_part2(self, data):
        return "\n" + data.decode_mini()


run(obj=Day08())
