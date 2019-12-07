#!/usr/bin/env python3

from aoc import IntcodePuzzle, run


class Day05(IntcodePuzzle):
    def run_part1(self, data):
        data = data.copy()
        data.inputs = [1]
        data.run()
        for n in range(len(data.outputs) - 1):
            assert data.outputs[n] == 0
        return str(data.outputs[-1])

    def run_part2(self, data):
        data = data.copy()
        data.inputs = [5]
        data.run()
        return str(data.outputs[0])


run(obj=Day05())
