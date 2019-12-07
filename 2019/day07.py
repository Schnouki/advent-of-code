#!/usr/bin/env python3

import itertools as it

from aoc import IntcodePuzzle, run


class Day07(IntcodePuzzle):
    test_data = [
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
        "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
    ]
    test_result_part1 = ["43210", "54321", "65210"]

    def run_part1(self, data):
        max_signal = -1e6
        for settings in it.permutations(range(5), 5):
            last_output = 0
            for amp_setting in settings:
                amp = data.copy()
                amp.inputs = [amp_setting, last_output]
                amp.run()
                last_output = amp.outputs[0]
            max_signal = max(max_signal, last_output)
        return str(max_signal)


run(obj=Day07())
