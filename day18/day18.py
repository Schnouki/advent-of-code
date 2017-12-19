#!/usr/bin/env python3

import collections
import doctest
import sys
import typing

TEST_DATA_1 = """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""

TEST_DATA_2 = """snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d"""


def parse_program(data: str) -> typing.List[typing.Tuple[str, str, str]]:
    ops = []
    for op in data.splitlines():
        code, *args = (op + " -1").split()
        x, y = args[0], args[1]
        ops.append((code, x, y))
    return ops


class Machine:
    def __init__(self):
        self.reg = collections.defaultdict(lambda: 0)
        self.last_snd = None
        self.last_rcv = None

    def run(self, data: str):
        ops = parse_program(data)
        eip = 0
        while 0 <= eip < len(ops):
            code, x, y = ops[eip]
            eip += self.run_op(code, x, y)

    def run_op(self, code, x, y) -> int:
        op_func = getattr(self, "op_" + code)
        offset = op_func(x, y)
        if offset is None:
            return 1
        return offset

    def value(self, val: str):
        if val.isalpha():
            return self.reg[val]
        return int(val)

    def op_snd(self, x, y):
        self.last_snd = self.value(x)

    def op_set(self, x, y):
        self.reg[x] = self.value(y)

    def op_add(self, x, y):
        self.reg[x] += self.value(y)

    def op_mul(self, x, y):
        self.reg[x] *= self.value(y)

    def op_mod(self, x, y):
        self.reg[x] %= self.value(y)

    def op_rcv(self, x, y):
        if self.value(x) != 0:
            self.last_rcv = self.last_snd

    def op_jgz(self, x, y):
        if self.value(x) > 0:
            return self.value(y)
        return 1


class RcvMachine(Machine):
    def op_rcv(self, x, y):
        if self.value(x) != 0:
            self.last_rcv = self.last_snd
            return 1e8


def first_recovered_frequency(data: str) -> int:
    """Get the first recovered frequency for the given program.

    >>> first_recovered_frequency(TEST_DATA_1)
    4
    """
    machine = RcvMachine()
    machine.run(data)
    return machine.last_rcv


class ComMachine(Machine):
    def __init__(self, program_id: int, other: 'ComMachine' = None):
        super().__init__()
        self.pid = program_id
        self.reg["p"] = program_id
        self.other = other
        self.locked = False
        self.queue = []

        self.cnt_snd = 0

    def op_snd(self, x, y):
        self.other.queue.append(self.value(x))
        self.cnt_snd += 1
        self.other.locked = False

    def op_rcv(self, x, y):
        self.locked = len(self.queue) == 0
        if self.locked:
            return 0
        else:
            self.reg[x] = self.queue.pop(0)


class Scheduler:
    def __init__(self):
        prog0 = ComMachine(0)
        prog1 = ComMachine(1, other=prog0)
        prog0.other = prog1
        self.progs = (prog0, prog1)

    @property
    def deadlock(self) -> bool:
        return self.progs[0].locked and self.progs[1].locked

    def run(self, data: str):
        ops = parse_program(data)
        eip = [0, 0]
        active = 0
        while 0 <= eip[0] < len(ops) and 0 <= eip[1] < len(ops) and not self.deadlock:
            prog = self.progs[active]
            code, x, y = ops[eip[active]]
            eip[active] += prog.run_op(code, x, y)
            if prog.locked:
                active = 1 - active


def count_sends(data: str) -> int:
    """Count how many times program 1 sends a value.

    >>> count_sends(TEST_DATA_2)
    3
    """
    sched = Scheduler()
    sched.run(data)
    prog1 = sched.progs[1]
    return prog1.cnt_snd


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("First recovered frequency for %s: %d" %
              (fn, first_recovered_frequency(data)))
        print("Send counter(1) for %s: %d" % (fn, count_sends(data)))
