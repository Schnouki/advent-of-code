#!/usr/bin/env python3

from typing import List

import attr

from aoc import Puzzle, run
from typing import List


@attr.s
class IntcodeComputer:
    mem: List[int] = attr.ib()
    idx: int = attr.ib(default=0)
    halted: bool = attr.ib(default=False)

    def _ensure_size(self, size):
        if len(self.mem) > size:
            return
        self.mem += [0] * (size - len(self.mem))

    def get(self, pos):
        self._ensure_size(pos)
        return self.mem[pos]

    def set(self, pos, value):
        self._ensure_size(pos)
        self.mem[pos] = value

    def step(self):
        if self.halted:
            return

        op = self.get(self.idx)

        if op == 1:
            pos1 = self.get(self.idx + 1)
            pos2 = self.get(self.idx + 2)
            pos3 = self.get(self.idx + 3)
            self.set(pos3, self.get(pos1) + self.get(pos2))
            self.idx += 4
        elif op == 2:
            pos1 = self.get(self.idx + 1)
            pos2 = self.get(self.idx + 2)
            pos3 = self.get(self.idx + 3)
            self.set(pos3, self.get(pos1) * self.get(pos2))
            self.idx += 4
        elif op == 99:
            self.halted = True
        else:
            raise ValueError(op)

    def run(self):
        while not self.halted:
            self.step()


class Day02(Puzzle):
    test_data = [
        "1,9,10,3,2,3,11,0,99,30,40,50",
        "1,0,0,0,99",
        "2,3,0,3,99",
        "2,4,4,5,99,0",
        "1,1,1,4,99,5,6,0,99",
    ]
    test_result_part1 = ["3500", "2", "2", "2", "30"]

    def prepare_data(self, data):
        mem = [int(x) for x in data.split(",")]
        return IntcodeComputer(mem)

    def run_part1(self, data):
        if not self.test_mode:
            data.mem[1] = 12
            data.mem[2] = 2
        data.run()
        return str(data.mem[0])


run(obj=Day02())
