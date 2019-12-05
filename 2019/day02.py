#!/usr/bin/env python3

from typing import List

import attr

from aoc import Puzzle, run
from typing import List


@attr.s
class IntcodeComputer:
    mem: List[int] = attr.ib()
    ip: int = attr.ib(default=0)
    halted: bool = attr.ib(default=False)

    def copy(self):
        return IntcodeComputer(self.mem[:], 0, False)

    def get(self, pos):
        return self.mem[pos]

    def set(self, pos, value):
        self.mem[pos] = value

    def step(self):
        if self.halted:
            return
        op = self.get(self.ip)

        if op == 1:
            addr1 = self.get(self.ip + 1)
            addr2 = self.get(self.ip + 2)
            addr3 = self.get(self.ip + 3)
            self.set(addr3, self.get(addr1) + self.get(addr2))
            self.ip += 4
        elif op == 2:
            addr1 = self.get(self.ip + 1)
            addr2 = self.get(self.ip + 2)
            addr3 = self.get(self.ip + 3)
            self.set(addr3, self.get(addr1) * self.get(addr2))
            self.ip += 4
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
        data = data.copy()
        if not self.test_mode:
            data.mem[1] = 12
            data.mem[2] = 2
        data.run()
        return str(data.mem[0])

    def run_part2(self, orig_data):
        for noun in range(101):
            for verb in range(101):
                data = orig_data.copy()
                data.mem[1] = noun
                data.mem[2] = verb
                data.run()
                if data.mem[0] == 19690720:
                    return 100*noun + verb


run(obj=Day02())
