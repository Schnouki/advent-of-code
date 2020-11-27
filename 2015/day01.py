#!/usr/bin/env python3

from aoc import Puzzle, run


class Day01(Puzzle):
    test_data_part1 = [
        "(())",
        "()()",
        "(((",
        "(()(()(",
        "))(((((",
        "())",
        "))(",
        ")))",
        ")())())",
    ]
    test_result_part1 = [0, 0, 3, 3, 3, -1, -1, -3, -3]

    test_data_part2 = [")", "()())"]
    test_result_part2 = [1, 5]

    def run_part1(self, data):
        return data.count("(") - data.count(")")

    def run_part2(self, data):
        level = 0
        for idx, char in enumerate(data, 1):
            level += 1 if char == "(" else -1
            if level < 0:
                return idx


run(obj=Day01())
