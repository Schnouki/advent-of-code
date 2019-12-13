#!/usr/bin/env python3

from collections import Counter
import enum
import time
from typing import Dict, Tuple

import attr
import click

from aoc import IntcodePuzzle, run
from intcode import Computer


class Tile(enum.Enum):
    EMPTY = 0
    WALL = 1
    BLOCK = 2
    PADDLE = 3
    BALL = 4

    def draw(self):
        _chars = " █#—●"
        return _chars[self.value]


@attr.s
class Game:
    program: Computer = attr.ib()
    score: int = attr.ib(default=-1)

    screen: Dict[Tuple[int, int], Tile] = attr.ib(factory=dict)
    width: int = attr.ib(default=0)
    height: int = attr.ib(default=0)

    ball_x: int = attr.ib(default=-1)
    paddle_x: int = attr.ib(default=-1)

    def step(self):
        self.program.run()

        while self.program.outputs:
            x = self.program.outputs.pop(0)
            y = self.program.outputs.pop(0)
            out = self.program.outputs.pop(0)
            if (x, y) == (-1, 0):
                self.score = out
            else:
                tile = Tile(out)
                self.screen[(x, y)] = tile
                self.width = max(self.width, x)
                self.height = max(self.height, y)

                if tile == Tile.BALL:
                    self.ball_x = x
                elif tile == Tile.PADDLE:
                    self.paddle_x = x

    def draw(self):
        out = (
            f"Score: {self.score}  -- ball: {self.ball_x}, paddle: {self.paddle_x}\n\n"
        )
        for y in range(self.height + 1):
            for x in range(self.width + 1):
                out += self.screen[(x, y)].draw()
            out += "\n"
        print(out)


class Day13(IntcodePuzzle):
    def run_part1(self, computer):
        computer = computer.copy()
        computer.run()

        screen: Dict[Tuple[int, int], Tile] = {}
        while computer.outputs:
            x = computer.outputs.pop(0)
            y = computer.outputs.pop(0)
            tile = Tile(computer.outputs.pop(0))
            screen[(x, y)] = tile
        cnt = Counter(screen.values())
        return str(cnt[Tile.BLOCK])

    def run_part2(self, computer):
        computer = computer.copy()
        computer.break_on_input = True
        computer.mem[0] = 2
        game = Game(computer)

        debug = False

        while not game.program.halted:
            game.step()
            if debug:
                click.clear()
                game.draw()
                time.sleep(0.025)

            # Super-smart AI, totally powered by deep learning
            joystick_pos = 0
            if game.ball_x < game.paddle_x:
                joystick_pos = -1
            elif game.ball_x > game.paddle_x:
                joystick_pos = +1
            game.program.inputs.append(joystick_pos)

        return str(game.score)


run(obj=Day13())
