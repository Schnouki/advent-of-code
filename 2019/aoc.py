"""Utility tools for Advent of Code puzzles.
"""
import abc
import doctest
from pathlib import Path
from typing import Any, List

import click


class BasePuzzle(abc.ABC):
    test_data: List[str] = []
    test_result_part1 = []
    test_result_part2 = []

    test_mode = False

    @abc.abstractmethod
    def get_input(self) -> str:
        ...

    def prepare_data(self, raw_data: str) -> Any:
        return raw_data

    def run_part1(self, data: Any) -> str:
        return "not implemented"

    def run_part2(self, data: Any) -> str:
        return "not implemented"

    def run_test(self, runner, test_result) -> bool:
        self.test_mode = True
        for idx, raw_data in enumerate(self.test_data):
            data = self.prepare_data(raw_data)
            res = runner(data)
            if res != test_result[idx]:
                print(f"TEST FAILED! Expected {test_result[idx]}, got {res}")
                return False
        return True


class Puzzle(BasePuzzle):
    def get_input(self):
        filename = Path(".") / "inputs" / self.__class__.__name__.lower()
        with filename.open() as input_file:
            return input_file.read()


class InlinePuzzle(BasePuzzle):
    puzzle_input = ""

    def get_input(self):
        return self.puzzle_input


@click.command()
@click.option("-1", "p1", help="Run part 1", is_flag=True)
@click.option("-2", "p2", help="Run part 2", is_flag=True)
@click.option("-t", "--test", help="Run on test data", is_flag=True)
@click.pass_context
def run(ctx, p1, p2, test):
    ctx.ensure_object(BasePuzzle)
    puzzle = ctx.obj

    if test:
        print("RUNNING DOCTESTS")
        dt_failed, dt_total = doctest.testmod()
        print(f"  failed: {dt_failed}/{dt_total}")
        if p1:
            print("RUNNING TESTS FOR PART 1")
            puzzle.run_test(puzzle.run_part1, puzzle.test_result_part1)
        if p2:
            print("RUNNING TESTS FOR PART 2")
            puzzle.run_test(puzzle.run_part2, puzzle.test_result_part2)
    else:
        raw_data = puzzle.get_input()
        data = puzzle.prepare_data(raw_data)
        if p1:
            print("RUNNING PART 1")
            res1 = puzzle.run_part1(data)
            print(f"Result for part 1: {res1}")
        if p2:
            print("RUNNING PART 2")
            res2 = puzzle.run_part2(data)
            print(f"Result for part 2: {res2}")
