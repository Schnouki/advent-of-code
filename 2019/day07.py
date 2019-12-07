#!/usr/bin/env python3

import itertools as it

from aoc import IntcodePuzzle, run


class Amplifiers:
    def __init__(self, program, settings):
        self.amps = []
        for setting in settings:
            amp = program.copy()
            amp.break_on_output = True
            amp.inputs = [setting]
            self.amps.append(amp)

    @property
    def halted(self):
        return any(amp.halted for amp in self.amps)

    def run_once(self, first_input=0):
        last_output = first_input
        for amp in self.amps:
            amp.inputs.append(last_output)
            amp.run()
            last_output = amp.outputs[-1]
        return last_output

    def run_loop(self, first_input=0):
        last_output = first_input
        while not self.halted:
            last_output = self.run_once(last_output)
        return last_output


class Day07(IntcodePuzzle):
    test_data_part1 = [
        "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0",
        "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
        "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0",
    ]
    test_result_part1 = ["43210", "54321", "65210"]

    test_data_part2 = [
        "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
        "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,"
        "-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,"
        "53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10",
    ]
    test_result_part2 = ["139629729", "18216"]

    def run_amps(self, data, settings_range, loop):
        max_signal = -1e6
        for settings in it.permutations(settings_range, 5):
            amps = Amplifiers(data, settings)
            output = amps.run_loop() if loop else amps.run_once()
            max_signal = max(max_signal, output)
        return str(max_signal)

    def run_part1(self, data):
        return self.run_amps(data, range(5), False)

    def run_part2(self, data):
        return self.run_amps(data, range(5, 10), True)


run(obj=Day07())
