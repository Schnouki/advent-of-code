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

    def copy(self):
        return self.__class__(self.mem[:], self.inputs[:], self.outputs[:], 0, False)

    def run(self):
        while not self.halted:
            self.step()

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
            op_func(self, *params)
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

    def op_99(self):
        """Halt."""
        self.halted = True