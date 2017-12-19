#!/usr/bin/env python3

import collections
import doctest
import sys

TEST_DATA = """set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2"""


class Machine:
    def __init__(self):
        self.reg = collections.defaultdict(lambda: 0)
        self.last_snd = None
        self.last_rcv = None

    def run(self, data: str):
        ops = []
        for op in data.splitlines():
            code, *args = (op + " -1").split()
            x, y = args[0], args[1]
            ops.append((code, x, y))

        eip = 0
        while 0 <= eip < len(ops):
            code, x, y = ops[eip]
            op_func = getattr(self, "op_" + code)
            offset = op_func(x, y)
            if offset is None:
                offset = 1
            eip += offset

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

    >>> first_recovered_frequency(TEST_DATA)
    4
    """
    machine = RcvMachine()
    machine.run(data)
    return machine.last_rcv


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("First recovered frequency for %s: %d" %
              (fn, first_recovered_frequency(data)))
