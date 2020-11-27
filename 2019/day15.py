#!/usr/bin/env python3

from collections import defaultdict
import enum
import time
from typing import Dict, List, Tuple

import attr
import click

from aoc import IntcodePuzzle, run
from intcode import Computer


class Direction(enum.Enum):
    NORTH = 1
    SOUTH = 2
    WEST = 3
    EAST = 4

    def move(self, pos: Tuple[int, int]) -> Tuple[int, int]:
        if self == Direction.NORTH:
            return (pos[0], pos[1] - 1)
        elif self == Direction.SOUTH:
            return (pos[0], pos[1] + 1)
        elif self == Direction.WEST:
            return (pos[0] - 1, pos[1])
        elif self == Direction.EAST:
            return (pos[0] + 1, pos[1])
        raise ValueError()

    @property
    def reverse(self) -> "Direction":
        if self == Direction.NORTH:
            return Direction.SOUTH
        elif self == Direction.SOUTH:
            return Direction.NORTH
        elif self == Direction.WEST:
            return Direction.EAST
        elif self == Direction.EAST:
            return Direction.WEST
        raise ValueError()


class Tile(enum.Enum):
    UNKNOWN = -1
    WALL = 0
    EMPTY = 1
    OXYSYS = 2

    def __str__(self):
        if self == Tile.UNKNOWN:
            return " "
        elif self == Tile.WALL:
            return "█"
        elif self == Tile.EMPTY:
            return "·"
        elif self == Tile.OXYSYS:
            return "@"
        raise ValueError()


@attr.s
class Robot:
    program: Computer = attr.ib()
    position: Tuple[int, int] = attr.ib(default=(0, 0))
    map: Dict[Tuple[int, int], Tile] = attr.ib(
        factory=lambda: defaultdict(lambda: Tile.UNKNOWN, {(0, 0): Tile.EMPTY})
    )
    visited: Dict[Tuple[int, int], bool] = attr.ib(
        factory=lambda: defaultdict(lambda: False, {(0, 0): True})
    )

    def draw(self):
        min_x = min((p[0] for p in self.map.keys()), default=0)
        max_x = max((p[0] for p in self.map.keys()), default=0)
        min_y = min((p[1] for p in self.map.keys()), default=0)
        max_y = max((p[1] for p in self.map.keys()), default=0)

        min_x = min(min_x, self.position[0])
        max_x = max(max_x, self.position[0])
        min_y = min(min_y, self.position[1])
        max_y = max(max_y, self.position[1])

        res = f"Position: ({self.position[0]}, {self.position[1]})\n\n"
        for y in range(min_y, max_y + 1):
            for x in range(min_x, max_x + 1):
                pos = (x, y)
                if pos == self.position:
                    res += "+"
                else:
                    res += str(self.map[pos])
            res += "\n"
        click.clear()
        print(res)

    def step(self, direc: Direction):
        target_pos = direc.move(self.position)
        self.program.inputs.append(direc.value)
        self.program.run()
        tile = Tile(self.program.outputs.pop())
        self.map[target_pos] = tile
        if tile != Tile.WALL:
            self.position = target_pos

    def look_around(self):
        for direc in Direction.__members__.values():
            pos = self.position
            target_pos = direc.move(self.position)
            if self.map[target_pos] != Tile.UNKNOWN:
                continue
            self.step(direc)
            if self.position != pos:
                self.step(direc.reverse)

    def explore_for_oxygen(self, draw=False):
        steps: List[Direction] = []
        while self.map[self.position] != Tile.OXYSYS:
            self.visited[self.position] = True
            self.look_around()
            if draw:
                self.draw()

            # Find spots to visit around me...
            next_direcs: List[Direction] = []
            for direc in Direction.__members__.values():
                target_pos = direc.move(self.position)
                if self.map[target_pos] == Tile.OXYSYS:
                    next_direcs = [direc]
                    break
                elif (
                    self.map[target_pos] == Tile.EMPTY and not self.visited[target_pos]
                ):
                    next_direcs.append(direc)

            # Use first possible direction, or backtrack
            if next_direcs:
                direc = next_direcs[0]
                steps.append(direc)
                self.step(direc)
            else:
                direc = steps.pop().reverse
                self.step(direc)
        return steps

    def explore_all(self, draw=False):
        steps: List[Direction] = []
        while True:
            self.visited[self.position] = True
            self.look_around()
            if draw:
                self.draw()

            # Find spots to visit around me...
            next_direcs: List[Direction] = []
            for direc in Direction.__members__.values():
                target_pos = direc.move(self.position)
                if self.map[target_pos] != Tile.WALL and not self.visited[target_pos]:
                    next_direcs.append(direc)

            # Use first possible direction, or backtrack
            if next_direcs:
                direc = next_direcs[0]
                steps.append(direc)
                self.step(direc)
            elif steps:
                direc = steps.pop().reverse
                self.step(direc)
            else:
                return

    def fill_oxygen(self):
        # Find all empty and oxygen tiles
        empties = [pos for pos, tile in self.map.items() if tile == Tile.EMPTY]
        if len(empties) == 0:
            return False
        oxys = [pos for pos, tile in self.map.items() if tile == Tile.OXYSYS]

        # Spread oxygen
        for pos in oxys:
            for direc in Direction.__members__.values():
                npos = direc.move(pos)
                if npos in empties:
                    self.map[npos] = Tile.OXYSYS
                    empties.remove(npos)
        return True


class Day15(IntcodePuzzle):
    def run_part1(self, computer: Computer):
        computer = computer.copy()
        computer.break_on_output = True
        robot = Robot(computer)

        steps = robot.explore_for_oxygen()
        return str(len(steps))

    def run_part2(self, computer: Computer):
        computer = computer.copy()
        computer.break_on_output = True
        robot = Robot(computer)

        robot.explore_all()
        # robot.draw()
        minutes = 0
        while robot.fill_oxygen():
            minutes += 1
            # robot.draw()
        return str(minutes)


if __name__ == "__main__":
    run(obj=Day15())
