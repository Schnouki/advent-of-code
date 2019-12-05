from typing import List

import attr


@attr.s
class Computer:
    mem: List[int] = attr.ib()
    ip: int = attr.ib(default=0)
    halted: bool = attr.ib(default=False)

    def copy(self):
        return self.__class__(self.mem[:], 0, False)

    def run(self):
        while not self.halted:
            self.step()

    def step(self):
        if self.halted:
            return
        op = self.mem[self.ip]

        op_func_name = f"op_{op}"
        if not hasattr(self, op_func_name):
            raise ValueError(f"Invalid opcode {op} at address {self.ip}")

        self.ip += getattr(self, op_func_name)()

    def op_1(self):
        """Addition. *(ip+3) = *(ip+1) + *(ip+2)."""
        addrs = self.mem[self.ip + 1 : self.ip + 4]
        self.mem[addrs[2]] = self.mem[addrs[0]] + self.mem[addrs[1]]
        return 4

    def op_2(self):
        """Multiplication. *(ip+3) = *(ip+1) * *(ip+2)."""
        addrs = self.mem[self.ip + 1 : self.ip + 4]
        self.mem[addrs[2]] = self.mem[addrs[0]] * self.mem[addrs[1]]
        return 4

    def op_99(self):
        """Halt."""
        self.halted = True
        return 1
