#!/usr/bin/env python3

from aoc import IntcodePuzzle, run


class Day05(IntcodePuzzle):
    def run_part1(self, data):
        data = data.copy()
        data.inputs = [1]
        data.run()
        for n in range(len(data.outputs) - 1):
            assert data.outputs[n] == 0
        return data.outputs[-1]


run(obj=Day05())
