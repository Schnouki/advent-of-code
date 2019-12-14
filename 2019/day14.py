#!/usr/bin/env python3

from collections import defaultdict
import math
from typing import Dict, List

import attr

from aoc import Puzzle, run


@attr.s
class Chemical:
    qty: int = attr.ib()
    name: str = attr.ib()


@attr.s
class Receipt:
    inputs: List[Chemical] = attr.ib()
    output: Chemical = attr.ib()

    @classmethod
    def from_string(cls, line):
        inps, outp = line.split("=>")

        outp = outp.strip().split()
        output = Chemical(int(outp[0]), outp[1])

        inputs = []
        for inp in inps.split(","):
            inp = inp.strip().split()
            input = Chemical(int(inp[0]), inp[1])
            inputs.append(input)

        return cls(inputs, output)


@attr.s
class System:
    receipts: List[Receipt] = attr.ib()
    inventory: Dict[str, int] = attr.ib(factory=lambda: defaultdict(lambda: 0))

    @property
    def needed(self) -> List[str]:
        res = []
        for name, qty in self.inventory.items():
            if qty < 0 and name != "ORE":
                res.append(name)
        return res

    def find_receipt(self, output: str) -> Receipt:
        for receipt in self.receipts:
            if receipt.output.name == output:
                return receipt
        raise ValueError(f"No receipt for {output}")

    def make_one(self, name: str, qty: int):
        # Find matching receipt
        rec = self.find_receipt(name)

        # How many time do we need to apply it?
        nb = 1
        if qty > rec.output.qty:
            nb = int(math.ceil(qty / rec.output.qty))

        self.inventory[rec.output.name] += nb * rec.output.qty
        for input in rec.inputs:
            self.inventory[input.name] -= nb * input.qty

    def make(self, name: str, qty: int):
        self.make_one(name, qty)

        needed = self.needed
        while needed:
            name = needed[0]
            qty = -self.inventory[name]
            self.make_one(name, qty)
            needed = self.needed


class Day14(Puzzle):
    test_data = [
        """10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL""",
        """9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL""",
        """171 ORE => 8 CNZTR
7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
114 ORE => 4 BHXH
14 VRPVC => 6 BMBT
6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
5 BMBT => 4 WPTQ
189 ORE => 9 KTJDG
1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
12 VRPVC, 27 CNZTR => 2 XDBXC
15 KTJDG, 12 BHXH => 5 XCVML
3 BHXH, 2 VRPVC => 7 MZWV
121 ORE => 7 VRPVC
7 XCVML => 6 RJRHP
5 BHXH, 4 VRPVC => 5 LTCX""",
    ]
    test_result_part1 = ["31", "165", "2210736"]

    def prepare_data(self, data):
        receipts = []
        for line in data.splitlines():
            receipts.append(Receipt.from_string(line))
        return System(receipts)

    def run_part1(self, system: System):
        system.make("FUEL", 1)
        return str(-system.inventory["ORE"])


run(obj=Day14())
