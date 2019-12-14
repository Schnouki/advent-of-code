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

    def make(self, name: str, qty: int) -> int:
        self.inventory.clear()
        self.make_one(name, qty)

        needed = self.needed
        while needed:
            name = needed[0]
            qty = -self.inventory[name]
            self.make_one(name, qty)
            needed = self.needed

        return -self.inventory["ORE"]

    def make_max(self, max_ore: int) -> int:
        cur_try, min_try, max_try = 1, 1, 1
        res = 0
        while True:
            max_try = cur_try * 2
            res = self.make("FUEL", cur_try)
            if res > max_ore:
                break
            min_try = cur_try
            cur_try *= 2
            max_try = cur_try

        while max_try - min_try > 1:
            cur_try = (min_try + max_try) // 2
            res = self.make("FUEL", cur_try)
            if res == max_ore:
                break
            elif res > max_ore:
                max_try = cur_try
            else:
                min_try = cur_try

        fuel, ore = self.inventory["FUEL"], -self.inventory["ORE"]
        if ore <= max_ore:
            return fuel
        else:
            return fuel - 1


class Day14(Puzzle):
    test_data = [
        """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""",
        """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF""",
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
    test_result_part1 = ["13312", "180697", "2210736"]
    test_result_part2 = ["82892753", "5586022", "460664"]

    def prepare_data(self, data):
        receipts = []
        for line in data.splitlines():
            receipts.append(Receipt.from_string(line))
        return System(receipts)

    def run_part1(self, system: System):
        return str(system.make("FUEL", 1))

    def run_part2(self, system: System):
        goal = 1_000_000_000_000
        return str(system.make_max(goal))


run(obj=Day14())
