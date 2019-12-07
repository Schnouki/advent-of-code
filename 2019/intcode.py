import enum
import functools
import inspect
from typing import List

import attr


class ParameterMode(enum.Enum):
    POSITION = 0
    IMMEDIATE = 1


@attr.s
class Computer:
    mem: List[int] = attr.ib()
    inputs: List[int] = attr.ib(factory=lambda: [])
    outputs: List[int] = attr.ib(factory=lambda: [])
    ip: int = attr.ib(default=0)
    halted: bool = attr.ib(default=False)

    breakpoint = False
    break_on_output = False

    def copy(self):
        return self.__class__(self.mem[:], self.inputs[:], self.outputs[:], 0, False)

    def run(self):
        while not (self.halted or self.breakpoint):
            self.step()
        self.breakpoint = False

    def step(self):
        """Run a single step of the program."""
        if self.halted:
            return

        op_str = str(self.mem[self.ip])
        func = self.decode_op(op_str)
        func(self)

    @classmethod
    @functools.lru_cache
    def decode_op(cls, op_str):
        op = int(op_str[-2:])

        # Find opcode function
        op_func_name = f"op_{op}"
        if not hasattr(cls, op_func_name):
            raise ValueError(f"Invalid opcode {op}")
        op_func = getattr(cls, op_func_name)

        # How many parameters does it have?
        sig = inspect.signature(op_func)
        nb_params = (len(sig.parameters) - 1) >> 1

        # Get modes
        op_str = op_str.zfill(nb_params + 2)
        modes = [ParameterMode(int(d)) for d in op_str[:-2]]

        # Prepare function to call
        def func(self):
            params = []
            for idx, mode in enumerate(modes[::-1], 1):
                params += [mode, self.mem[self.ip + idx]]
            ip_before = self.ip
            op_func(self, *params)
            if self.ip == ip_before:
                self.ip += nb_params + 1

        return func

    def get(self, mode, value):
        if mode == ParameterMode.POSITION:
            return self.mem[value]
        if mode == ParameterMode.IMMEDIATE:
            return value
        raise ValueError(f"Invalid parameter mode {mode}")

    def op_1(self, m1, v1, m2, v2, m3, v3):
        """Addition. v3 = v1 + v2"""
        self.mem[v3] = self.get(m1, v1) + self.get(m2, v2)

    def op_2(self, m1, v1, m2, v2, m3, v3):
        """Multiplication. v3 = v1 * v2"""
        self.mem[v3] = self.get(m1, v1) * self.get(m2, v2)

    def op_3(self, m1, v1):
        """Input. v1 = i[0]"""
        self.mem[v1] = self.inputs.pop(0)

    def op_4(self, m1, v1):
        """Output. o += v1"""
        self.outputs.append(self.get(m1, v1))
        if self.break_on_output:
            self.breakpoint = True

    def op_5(self, m1, v1, m2, v2):
        """Jump-if-true. ip = v2 if v1 != 0"""
        if self.get(m1, v1) != 0:
            self.ip = self.get(m2, v2)

    def op_6(self, m1, v1, m2, v2):
        """Jump-if-false. ip = v2 if v1 == 0"""
        if self.get(m1, v1) == 0:
            self.ip = self.get(m2, v2)

    def op_7(self, m1, v1, m2, v2, m3, v3):
        """Less-than. v3 = 1 if v1 < v2 else 0"""
        self.mem[v3] = 1 if self.get(m1, v1) < self.get(m2, v2) else 0

    def op_8(self, m1, v1, m2, v2, m3, v3):
        """Equals. v3 = 1 if v1 == v2 else 0"""
        self.mem[v3] = 1 if self.get(m1, v1) == self.get(m2, v2) else 0

    def op_99(self):
        """Halt."""
        self.halted = True
