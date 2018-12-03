#!/usr/bin/env python3

import attr
import collections
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


@attr.s
class Node:
    program: Program = attr.ib()
    leaves: typing.List['Node'] = attr.ib()

    @property
    def name(self):
        return self.program.name

    @property
    def own_weight(self):
        return self.program.weight

    @property
    def leaves_weights(self):
        return [leaf.total_weight for leaf in self.leaves]

    @property
    def total_weight(self):
        return self.own_weight + sum(self.leaves_weights)

    @property
    def is_balanced(self):
        return len(set(self.leaves_weights)) == 1

    def print_(self, indent=0):
        prefix = "  " * indent
        print("%s%s (own=%d, total=%d)" %
              (prefix, self.name, self.own_weight, self.total_weight))
        for leaf in self.leaves:
            leaf.print_(indent + 1)


def parse_programs(data: str) -> typing.List[Program]:
    programs = []
    PROGRAM_RE = re.compile(r"^(\w+) \((\d+)\)(?: -> (\w+(, \w+)*))?")
    for line in data.splitlines():
        mtch = PROGRAM_RE.match(line.strip())
        if not mtch:
            raise ValueError(line)
        grps = mtch.groups()
        if grps[2]:
            leaves = [s.strip() for s in grps[2].split(",")]
        else:
            leaves = []
        programs.append(Program(grps[0], int(grps[1]), leaves))
    return programs


def _find_root(programs: typing.List[Program]) -> Program:
    for program in programs:
        # Is it "above" any other program?
        for other_program in programs:
            if program.name in other_program.leaves:
                break
        else:
            return program


def bottom_program(data: str) -> str:
    """Find the name of the bottom program.

    >>> bottom_program(TEST_DATA)
    'tknk'
    """
    programs = parse_programs(data)
    return _find_root(programs).name


def _build_tree(root: str, programs: typing.Dict[str, Program]) -> Node:
    program = programs[root]
    leaves = [_build_tree(leaf, programs) for leaf in program.leaves]
    return Node(program, leaves)


def _fix_tree(node: Node) -> int:
    weights = collections.Counter(node.leaves_weights)
    if len(weights) == 1:
        return -1
    good_weight, bad_weight = weights.most_common(2)
    good_weight, bad_weight = good_weight[0], bad_weight[0]
    for leaf in node.leaves:
        if leaf.total_weight != bad_weight:
            continue
        fix = _fix_tree(leaf)
        if fix >= 0:
            return fix
        return leaf.own_weight - (bad_weight - good_weight)


def fix_weight(data: str) -> int:
    """Find the correct weight for the wrong program.

    >>> fix_weight(TEST_DATA)
    60
    """
    raw_programs = parse_programs(data)
    programs = {program.name: program for program in raw_programs}
    tree = _build_tree(_find_root(raw_programs).name, programs)
    # tree.print_()
    return _fix_tree(tree)


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
        print("Fixed weight for %s: %d" % (fn, fix_weight(data)))
