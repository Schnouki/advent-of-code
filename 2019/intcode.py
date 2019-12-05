from typing import List

import attr


@attr.s
class Computer:
    mem: List[int] = attr.ib()
    ip: int = attr.ib(default=0)
    halted: bool = attr.ib(default=False)

    def copy(self):
        return self.__class__(self.mem[:], 0, False)

    def get(self, pos):
        return self.mem[pos]

    def set(self, pos, value):
        self.mem[pos] = value

    def step(self):
        if self.halted:
            return
        op = self.get(self.ip)

        if op == 1:
            addr1 = self.get(self.ip + 1)
            addr2 = self.get(self.ip + 2)
            addr3 = self.get(self.ip + 3)
            self.set(addr3, self.get(addr1) + self.get(addr2))
            self.ip += 4
        elif op == 2:
            addr1 = self.get(self.ip + 1)
            addr2 = self.get(self.ip + 2)
            addr3 = self.get(self.ip + 3)
            self.set(addr3, self.get(addr1) * self.get(addr2))
            self.ip += 4
        elif op == 99:
            self.halted = True
        else:
            raise ValueError(op)

    def run(self):
        while not self.halted:
            self.step()
