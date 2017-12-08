#!/usr/bin/env python3

import attr
import collections
import doctest
import enum
import operator as op
import re
import sys
import typing

TEST_DATA = """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""


@enum.unique
class OP(enum.Enum):
    INCR = "inc"
    DECR = "dec"


@enum.unique
class COND(enum.Enum):
    EQ = "=="
    NEQ = "!="
    GT = ">"
    GTE = ">="
    LT = "<"
    LTE = "<="

    @property
    def op(self):
        _ops = {
            "EQ": op.eq, "NEQ": op.ne,
            "GT": op.gt, "GTE": op.ge,
            "LT": op.lt, "LTE": op.le,
        }
        return _ops[self.name]


@attr.s
class Instruction:
    register: str = attr.ib()
    op: OP = attr.ib()
    value: int = attr.ib()
    cond_register: str = attr.ib()
    cond: COND = attr.ib()
    cond_value: int = attr.ib()


class CPU:
    def __init__(self):
        self.registers = collections.defaultdict(lambda: 0)
        self.highest = None

    def run(self, line: str, debug: bool = False):
        instr = self._parse(line)
        if debug:
            print("-" * 20)
            print("INSTR", line)
            print("BEFORE", dict(self.registers))
        if instr.cond.op(self.registers[instr.cond_register], instr.cond_value):
            value = instr.value * (1 if instr.op == OP.INCR else -1)
            if debug:
                print("VALUE", value)
            self.registers[instr.register] += value
        if debug:
            print("AFTER", dict(self.registers))

        if self.highest is None:
            self.highest = self.max_register
        else:
            self.highest = max(self.highest, self.max_register)

    @staticmethod
    def _parse(line: str) -> Instruction:
        mtch = re.match(r'^(\w+) (\w+) ([0-9-]+) if (\w+) (\S+) ([0-9-]+)$',
                        line)
        if not mtch:
            raise ValueError(line)
        grps = mtch.groups()

        return Instruction(
            register=grps[0], op=OP(grps[1]), value=int(grps[2]),
            cond_register=grps[3], cond=COND(grps[4]), cond_value=int(grps[5]),
        )

    @property
    def max_register(self):
        return max(self.registers.values())


def run_program(program: str) -> CPU:
    cpu = CPU()
    for line in program.splitlines():
        cpu.run(line.strip())
    return cpu


def largest_value(program: str) -> int:
    """Find the largest value in any register after running program.

    >>> largest_value(TEST_DATA)
    1
    """
    cpu = run_program(program)
    return cpu.max_register


def highest_value(program: str) -> int:
    """Find the highest value held in any register during the program execution.

    >>> highest_value(TEST_DATA)
    10
    """
    cpu = run_program(program)
    return cpu.highest


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read()

        print("Largest value for %s: %d" % (fn, largest_value(data)))
        print("Highest value during %s: %d" % (fn, highest_value(data)))
