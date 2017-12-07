#!/usr/bin/env python3

import attr
import doctest
import re
import sys
import typing

TEST_DATA = """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""


@attr.s
class Program:
    name: str = attr.ib()
    weight: int = attr.ib()
    leaves: typing.List[str] = attr.ib()


def parse_programs(data: str) -> typing.List[Program]:
    programs = []
    PROGRAM_RE = re.compile(r"^(\w+) \((\d+)\)(?: -> (\w+(, \w+)*))?")
    for line in data.splitlines():
        mtch = PROGRAM_RE.match(line.strip())
        if not mtch:
            raise ValueError(line)
        grps = mtch.groups()
        leaves = grps[2] or ""
        leaves = [s.strip() for s in leaves.split(",")]
        programs.append(Program(grps[0], int(grps[1]), leaves))
    return programs


def bottom_program(data: str) -> str:
    """Find the name of the bottom program.

    >>> bottom_program(TEST_DATA)
    'tknk'
    """
    programs = parse_programs(data)
    for program in programs:
        # Is it "above" any other program?
        for other_program in programs:
            if program.name in other_program.leaves:
                break
        else:
            return program.name


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read()

        print("Bottom program for %s: %s" % (fn, bottom_program(data)))
