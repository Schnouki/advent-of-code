#!/usr/bin/env python3

from aoc import Puzzle, run
from intcode import Computer


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
        return Computer(mem)

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
                    return 100 * noun + verb


run(obj=Day02())
