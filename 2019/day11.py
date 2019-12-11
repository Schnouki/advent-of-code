#!/usr/bin/env python3

import enum
from typing import Dict, Tuple

import attr

from aoc import IntcodePuzzle, run
from intcode import Computer

BLACK = "█"
WHITE = " "


class Direction(enum.Enum):
    UP = 0
    LEFT = 1
    DOWN = 2
    RIGHT = 3

    def move(self, origin: Tuple[int, int]) -> Tuple[int, int]:
        x, y = origin
        if self == Direction.UP:
            return x, y - 1
        if self == Direction.DOWN:
            return x, y + 1
        if self == Direction.LEFT:
            return x - 1, y
        if self == Direction.RIGHT:
            return x + 1, y
        raise RuntimeError(f"Invalid direction {self}")

    def turn(self, x: int) -> "Direction":
        new_value = self.value + (1 if x == 0 else -1)
        return Direction(new_value % 4)


@attr.s
class HullPaintingRobot:
    program: Computer = attr.ib()
    painted: Dict[Tuple[int, int], int] = attr.ib(factory=lambda: {})
    position: Tuple[int, int] = attr.ib(default=(0, 0))
    direction: Direction = attr.ib(default=Direction.UP)

    def step(self):
        # Set the current panel color as program input
        current_color = self.painted.get(self.position, 0)
        self.program.inputs.append(current_color)

        # Run the program and get 2 outputs
        self.program.run()
        if self.program.halted:
            return
        new_color = self.program.outputs.pop(0)
        self.program.run()
        direction_change = self.program.outputs.pop(0)

        # Use these outputs
        self.painted[self.position] = new_color
        self.direction = self.direction.turn(direction_change)
        self.position = self.direction.move(self.position)

    def run(self):
        while not self.program.halted:
            self.step()

    def get_picture(self) -> str:
        min_x = min(self.painted.keys(), key=lambda p: p[0])[0]
        max_x = max(self.painted.keys(), key=lambda p: p[0])[0]
        min_y = min(self.painted.keys(), key=lambda p: p[1])[1]
        max_y = max(self.painted.keys(), key=lambda p: p[1])[1]

        out = "\n"
        for y in range(min_y, max_y + 1):
            for x in range(min_x, max_x + 1):
                color = self.painted.get((x, y), 0)
                out += BLACK if color == 1 else WHITE
            out += "\n"

        return out.rstrip()


class Day11(IntcodePuzzle):
    def run_part1(self, computer):
        robot = HullPaintingRobot(computer.copy())
        robot.program.break_on_output = True
        robot.run()
        return str(len(robot.painted))

    def run_part2(self, computer):
        robot = HullPaintingRobot(computer)
        robot.program.break_on_output = True
        robot.painted[(0, 0)] = 1
        robot.run()
        return robot.get_picture()


run(obj=Day11())
