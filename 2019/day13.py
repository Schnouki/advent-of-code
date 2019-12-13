#!/usr/bin/env python3

from collections import Counter
import enum
from typing import Dict, Tuple

import attr

from aoc import IntcodePuzzle, run

class Tile(enum.Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    PADDLE = 3
    BALL = 4


class Day13(IntcodePuzzle):
    def run_part1(self, computer):
        computer = computer.copy()
        computer.run()

        screen: Dict[Tuple[int, int], Tile] = {}
        while computer.outputs:
            x = computer.outputs.pop(0)
            y = computer.outputs.pop(0)
            tile = Tile(computer.outputs.pop(0))
            screen[(x, y)] = tile
        cnt = Counter(screen.values())
        return str(cnt[Tile.BLOCK])


run(obj=Day13())
