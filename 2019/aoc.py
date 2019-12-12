"""Utility tools for Advent of Code puzzles.
"""
import abc
import doctest
from pathlib import Path
import time
from typing import Any, List

import click

import intcode


class BasePuzzle(abc.ABC):
    test_data: List[str] = []
    test_data_part1: List[str] = []
    test_data_part2: List[str] = []
    test_data_extra: List[str] = []
    test_result_part1: List[str] = []
    test_result_part2: List[str] = []
    test_result_extra: List[str] = []

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

    def test_extra(self, data: Any) -> str:
        return "not implemented"

    def run_test(self, runner, test_data, test_result) -> bool:
        self.test_mode = True
        for idx, raw_data in enumerate(test_data):
            data = self.prepare_data(raw_data)
            res = runner(data)
            if res != test_result[idx]:
                click.secho(
                    f"expected {test_result[idx]}, got {res} 🤯 ", nl=False, fg="red"
                )
                return False
        return True


class Puzzle(BasePuzzle):
    def get_input(self):
        filename = Path(".") / "inputs" / self.__class__.__name__.lower()
        with filename.open() as input_file:
            return input_file.read()


class IntcodePuzzle(Puzzle):
    def prepare_data(self, data):
        mem = [int(x) for x in data.split(",")]
        return intcode.Computer(mem)


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

    name = puzzle.__class__.__name__
    click.secho(f"Running {name} 🚀", fg="bright_magenta")

    if test:
        click.secho("Test mode! 🎉", fg="bright_white")
        click.echo(" ↳ Running doctests... ", nl=False)
        dt_failed, dt_total = doctest.testmod()
        if dt_total == 0:
            click.secho("no doctests 🙈", fg="yellow")
        elif dt_failed == 0:
            click.secho("success! 🤩", fg="green")
        else:
            click.secho(f"failed {dt_failed}/{dt_total} 😱", fg="red")
        if puzzle.test_data_extra:
            click.echo(" ↳ Running extra tests... ", nl=False)
            test_data = puzzle.test_data_extra
            success = puzzle.run_test(
                puzzle.test_extra, puzzle.test_data_extra, puzzle.test_result_extra
            )
            if success:
                click.secho("success! 🤩", fg="green")
        if p1:
            click.echo(" ↳ Testing part 1... ", nl=False)
            test_data = puzzle.test_data + puzzle.test_data_part1
            if not test_data:
                click.secho("no test data 🙉", fg="yellow")
            else:
                success = puzzle.run_test(
                    puzzle.run_part1, test_data, puzzle.test_result_part1
                )
                if success:
                    click.secho("success! 🤩", fg="green")
        if p2:
            click.echo(" ↳ Testing part 2... ", nl=False)
            test_data = puzzle.test_data + puzzle.test_data_part2
            if not test_data:
                click.secho("no test data 🙊", fg="yellow")
            else:
                success = puzzle.run_test(
                    puzzle.run_part2, test_data, puzzle.test_result_part2
                )
                if success:
                    click.secho("success! 🤩", fg="green")
    else:
        raw_data = puzzle.get_input()
        data = puzzle.prepare_data(raw_data)
        if p1:
            click.echo("Running part 1... ", nl=False)
            start1 = time.perf_counter()
            res1 = puzzle.run_part1(data)
            end1 = time.perf_counter()
            click.echo(
                "took %.2f ms; result: %s"
                % (((end1 - start1) * 1000), click.style(res1, fg="bright_cyan"))
            )
        if p2:
            click.echo("Running part 2... ", nl=False)
            start2 = time.perf_counter()
            res2 = puzzle.run_part2(data)
            end2 = time.perf_counter()
            click.echo(
                "took %.2f ms; result: %s"
                % (((end2 - start2) * 1000), click.style(res2, fg="bright_cyan"))
            )
