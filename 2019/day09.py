#!/usr/bin/env python3

from collections import Counter
from typing import List

import attr

from aoc import IntcodePuzzle, run


class Day09(IntcodePuzzle):
    test_data_extra = [
        "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
        "104,1125899906842624,99",
    ]
    test_result_extra = [
        "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99",
        "1125899906842624",
    ]

    def test_extra(self, data):
        data.run()
        return ",".join(str(o) for o in data.outputs)

    def run_part1(self, data):
        data = data.copy()
        data.inputs = [1]
        data.run()
        return str(data.outputs[0])

    def run_part2(self, data):
        data = data.copy()
        data.inputs = [2]
        data.run()
        return str(data.outputs[0])


run(obj=Day09())
