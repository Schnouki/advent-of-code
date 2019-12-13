import enum
import functools
import inspect
from typing import List

import attr


class Opcode(enum.Enum):
    ADD = 1
    MUL = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    ADJ_RB = 9
    HALT = 99


class ParameterMode(enum.Enum):
    POSITION = 0
    IMMEDIATE = 1
    RELATIVE = 2


@attr.s
class Computer:
    mem: List[int] = attr.ib()
    inputs: List[int] = attr.ib(factory=lambda: [])
    outputs: List[int] = attr.ib(factory=lambda: [])
    ip: int = attr.ib(default=0)  # instruction pointer
    rb: int = attr.ib(default=0)  # relative base
    halted: bool = attr.ib(default=False)

    debug = False
    breakpoint = False
    break_on_input = False
    break_on_output = False

    def copy(self):
        return self.__class__(
            self.mem[:], self.inputs[:], self.outputs[:], 0, self.rb, False
        )

    def run(self):
        while not (self.halted or self.breakpoint):
            self.step()
        self.breakpoint = False

    def step(self):
        """Run a single step of the program."""
        if self.halted:
            return

        op_str = str(self.mem[self.ip])
        if self.debug:
            print(f"\nopcode={op_str} IP={self.ip} RB={self.rb} SZMEM={len(self.mem)}")
        func = self.decode_op(op_str)
        func(self)

    @classmethod
    @functools.lru_cache()
    def decode_op(cls, op_str):
        op = Opcode(int(op_str[-2:]))

        # Find opcode function
        op_func_name = f"op_{op.name}"
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
            if self.debug:
                print(op.name, params)
            keep_ip = op_func(self, *params)
            if self.ip == ip_before and not keep_ip:
                self.ip += nb_params + 1

        return func

    def ensure_size(self, size):
        if len(self.mem) <= size:
            self.mem += [0] * (size - len(self.mem) + 1)

    def _get(self, mode, value):
        if mode == ParameterMode.POSITION:
            self.ensure_size(value)
            return self.mem[value]
        if mode == ParameterMode.IMMEDIATE:
            return value
        if mode == ParameterMode.RELATIVE:
            pos = self.rb + value
            self.ensure_size(pos)
            return self.mem[pos]
        raise ValueError(f"Invalid parameter mode for get: {mode}")

    def get(self, mode, value):
        res = self._get(mode, value)
        if self.debug:
            print(f"  < {mode.name} {value} --> {res}")
        return res

    def set(self, out_mode, out_value, new_value):
        pos = None
        if out_mode == ParameterMode.POSITION:
            pos = out_value
        elif out_mode == ParameterMode.RELATIVE:
            pos = self.rb + out_value
        else:
            raise ValueError(f"Invalid parameter mode for set: {mode}")
        if self.debug:
            print(f"  > {out_mode.name} {out_value} --> {new_value}")
        self.ensure_size(pos)
        self.mem[pos] = new_value

    def op_ADD(self, m1, v1, m2, v2, m3, v3):
        """Addition. v3 = v1 + v2"""
        self.set(m3, v3, self.get(m1, v1) + self.get(m2, v2))

    def op_MUL(self, m1, v1, m2, v2, m3, v3):
        """Multiplication. v3 = v1 * v2"""
        self.set(m3, v3, self.get(m1, v1) * self.get(m2, v2))

    def op_INPUT(self, m1, v1):
        """Input. v1 = i[0]"""
        if len(self.inputs) == 0 and self.break_on_input:
            self.breakpoint = True
            return True
        self.set(m1, v1, self.inputs.pop(0))

    def op_OUTPUT(self, m1, v1):
        """Output. o += v1"""
        self.outputs.append(self.get(m1, v1))
        if self.break_on_output:
            self.breakpoint = True

    def op_JUMP_IF_TRUE(self, m1, v1, m2, v2):
        """Jump-if-true. ip = v2 if v1 != 0"""
        if self.get(m1, v1) != 0:
            self.ip = self.get(m2, v2)

    def op_JUMP_IF_FALSE(self, m1, v1, m2, v2):
        """Jump-if-false. ip = v2 if v1 == 0"""
        if self.get(m1, v1) == 0:
            self.ip = self.get(m2, v2)

    def op_LESS_THAN(self, m1, v1, m2, v2, m3, v3):
        """Less-than. v3 = 1 if v1 < v2 else 0"""
        self.set(m3, v3, 1 if self.get(m1, v1) < self.get(m2, v2) else 0)

    def op_EQUALS(self, m1, v1, m2, v2, m3, v3):
        """Equals. v3 = 1 if v1 == v2 else 0"""
        self.set(m3, v3, 1 if self.get(m1, v1) == self.get(m2, v2) else 0)

    def op_ADJ_RB(self, m1, v1):
        """Adjust relative base. rb += v1"""
        self.rb += self.get(m1, v1)

    def op_HALT(self):
        """Halt."""
        self.halted = True
