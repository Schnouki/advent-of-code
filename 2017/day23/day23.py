#!/usr/bin/env python3

import doctest
import sys


class Processor:
    def __init__(self, program: str, val_a: int = 0):
        self.reg = {k: 0 for k in "abcdefgh"}
        self.reg["a"] = val_a
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


def find_b(program: str) -> int:
    """Find the initial value of b."""
    proc = Processor(program)
    proc.step()
    return proc.reg["b"]


def count_mul(program: str, val_a: int = 0) -> int:
    """Count how many times is the mul instruction is invoked."""
    proc = Processor(program, val_a)
    proc.run()
    return proc.cnt["mul"]


def value_h(program: str, val_a: int = 0) -> int:
    """Find the value of h after running the program."""
    proc = Processor(program, val_a)
    proc.run()
    return proc.reg["h"]


def is_prime(n):
    """Check if n is prime."""
    if n < 2:
        return False
    if n % 2 == 0:
        return n == 2
    k = 3
    while k * k <= n:
        if n % k == 0:
            return False
        k += 2
    return True


def optim_h(a: int, b: int) -> int:
    """Get the value of h."""
    c = b
    if a != 0:
        b = b * 100 + 100000
        c = b + 17000
    h = 0
    for bb in range(b, c + 1, 17):
        if not is_prime(bb):
            h += 1
    return h


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        REG_B = find_b(data)

        print("----- a=0, b=%d -----" % REG_B)
        print("Number of 'mul' for %s: %d" % (fn, count_mul(data)))
        print("Value of 'h' for %s: %d" % (fn, value_h(data)))
        print("Optimized value of 'h' for %s: %d" % (fn, optim_h(0, REG_B)))

        print("----- a=1, b=%d -----" % REG_B)
        print("Optimized value of 'h' for %s: %d" % (fn, optim_h(1, REG_B)))
