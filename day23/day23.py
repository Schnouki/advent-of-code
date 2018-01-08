#!/usr/bin/env python3

import doctest
import sys


class Processor:
    def __init__(self, program: str):
        self.reg = {k: 0 for k in "abcdefgh"}
        self.cnt = {k: 0 for k in ("set", "sub", "mul", "jnz")}

        self.program = []
        for line in program.splitlines():
            self.program.append(line.split())
        self.eip = 0

    def val(self, x: str) -> int:
        if x.isalpha():
            return self.reg[x]
        return int(x)

    def op_set(self, x: str, y: str) -> None:
        self.reg[x] = self.val(y)

    def op_sub(self, x: str, y: str) -> None:
        self.reg[x] -= self.val(y)

    def op_mul(self, x: str, y: str) -> None:
        self.reg[x] *= self.val(y)

    def op_jnz(self, x: str, y: str) -> int:
        return 1 if self.val(x) == 0 else self.val(y)

    def step(self) -> None:
        op, x, y = self.program[self.eip]
        self.cnt[op] += 1
        func = getattr(self, "op_" + op)
        off = func(x, y)
        if off is None:
            off = 1
        self.eip += off

    def run(self) -> None:
        while 0 <= self.eip < len(self.program):
            self.step()


def count_mul(program: str) -> int:
    """Count how many times is the mul instruction is invoked."""
    proc = Processor(program)
    proc.run()
    return proc.cnt["mul"]


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Number of 'mul' for %s: %d" % (fn, count_mul(data)))
