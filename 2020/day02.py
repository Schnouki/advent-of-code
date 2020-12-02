#!/usr/bin/env python3

from dataclasses import dataclass
import re

from aoc import Puzzle, run

RE_LINE = re.compile(r"^(?P<v1>\d+)-(?P<v2>\d+) (?P<char>\w): (?P<password>.+)$")


@dataclass
class Password:
    v1: int
    v2: int
    char: str
    password: str

    @classmethod
    def from_line(cls, line: str) -> "Password":
        mtch = RE_LINE.match(line)
        return cls(
            v1=int(mtch["v1"]),
            v2=int(mtch["v2"]),
            char=mtch["char"],
            password=mtch["password"],
        )

    @property
    def is_valid_p1(self) -> bool:
        return self.v1 <= self.password.count(self.char) <= self.v2

    @property
    def is_valid_p2(self) -> bool:
        ok1 = self.password[self.v1 - 1] == self.char
        ok2 = self.password[self.v2 - 1] == self.char
        return ok1 ^ ok2


class Day02(Puzzle):
    test_data = ["1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"]
    test_result_part1 = [2]
    test_result_part2 = [1]

    def prepare_data(self, data):
        return [Password.from_line(line) for line in data.splitlines()]

    def run_part1(self, data):
        return sum(1 for password in data if password.is_valid_p1)

    def run_part2(self, data):
        return sum(1 for password in data if password.is_valid_p2)


if __name__ == "__main__":
    run(obj=Day02())
