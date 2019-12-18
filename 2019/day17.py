#!/usr/bin/env python3

from typing import List, Tuple

from aoc import IntcodePuzzle, run
from intcode import Computer


TEST_MAP = """..#..........
..#..........
#######...###
#.#...#...#.#
#############
..#...#...#..
..#####...^.."""


def print_outputs(outputs: List[int]):
    out = "".join(chr(n) for n in outputs)
    print(out)


def find_scaffolds(outputs: List[int]):
    """Iterate over the scaffolds positions.

    >>> len(list(find_scaffolds(ord(c) for c in TEST_MAP)))
    32
    """
    x, y = 0, 0
    for n in outputs:
        if n == 10:
            x, y = 0, y + 1
            continue
        if n in (35, 94):
            yield (x, y, n)
        x += 1


def find_intersections(outputs: List[int]):
    """Iterate over the scaffold interesections.

    >>> list(find_intersections(ord(c) for c in TEST_MAP))
    [(2, 2), (2, 4), (6, 4), (10, 4)]
    """
    scaffolds = list(find_scaffolds(outputs))
    for (x, y, _) in scaffolds:
        if (
            (x - 1, y) in scaffolds
            and (x + 1, y) in scaffolds
            and (x, y - 1) in scaffolds
            and (x, y + 1) in scaffolds
        ):
            yield (x, y)


def _next_pos(pos, d):
    x, y = pos
    if d == "^":
        return (x, y - 1)
    elif d == "v":
        return (x, y + 1)
    elif d == "<":
        return (x - 1, y)
    elif d == ">":
        return (x + 1, y)
    raise RuntimeError()


def _turn(d, s):
    dirs = "^>v<"
    idx = dirs.index(d)
    return dirs[(idx + s) % 4]


def find_path(positions: List[Tuple[int, int, int]]):
    out = []
    d = "^"
    steps = 0
    pos = None
    for (x, y, t) in positions:
        if t == 94:
            pos = (x, y)
            break
    else:
        raise RuntimeError()

    scaffolds = [(x, y) for (x, y, t) in positions if t == 35]

    while True:
        # Can we move forward?
        npos = _next_pos(pos, d)
        if npos in scaffolds:
            # Yes!
            steps += 1
            pos = npos
            continue

        # No: try to turn.
        dr, dl = _turn(d, 1), _turn(d, -1)
        if _next_pos(pos, dr) in scaffolds:
            out += [steps, "R"]
            d = dr
            steps = 0
        elif _next_pos(pos, dl) in scaffolds:
            out += [steps, "L"]
            d = dl
            steps = 0
        else:
            break

    out += [steps]
    return out[1:]


def _move(path, moves):
    out = path.copy()
    for i, m in enumerate(moves):
        if len(out) == 0:
            return None
        n = out[0]
        if type(m) is not type(n):
            return None
        if type(m) is int:
            n -= m
            if n < 0:
                return None
            if n == 0:
                out.pop(0)
            elif i == len(moves) - 1:
                out[0] = n
                return out
            else:
                return None
        else:
            if m == out[0]:
                out.pop(0)
            else:
                return None
    return out


def build_seq(path, bits):
    ret = []
    found = True
    while len(path) > 0 and found:
        found = False
        # print("LEN(PATH) =", len(path), "--", "".join(str(c) for c in path))
        for name, seq in bits.items():
            new_p = _move(path, seq)
            if new_p is None:
                continue
            found = True
            ret.append(name)
            path = new_p
    return ret, (len(path) == 0)


def _shorten_seq(seq):
    if type(seq[-1]) is int and seq[-1] > 1:
        return seq[:-1] + [seq[-1] - 1]
    else:
        return seq[:-1]


def find_moves(path, max_length=10):
    a, b, c = path[:max_length], None, None

    while a:
        pa = path
        while True:
            npa = _move(pa, a)
            if not npa:
                break
            pa = npa

        b = pa[:max_length]

        while b:
            pb = pa
            while True:
                npb = _move(pb, b)
                if not npb:
                    break
                pb = npb

            c = pb[:max_length]

            while c:
                moves = {"A": a, "B": b, "C": c}
                res, success = build_seq(path, moves)
                # print(success, moves, res)
                if success:
                    return (a, b, c, res)
                c = _shorten_seq(c)

            b = _shorten_seq(b)

        a = _shorten_seq(a)

    print("Fail.")


def to_str(seq):
    return ",".join(str(s) for s in seq)


class Day17(IntcodePuzzle):
    def run_part1(self, computer):
        computer = computer.copy()
        computer.run()

        intersections = find_intersections(computer.outputs)
        return str(sum(x * y for (x, y) in intersections))

    def run_part2(self, computer):
        step1 = computer.copy()
        step1.run()

        scaffolds = list(find_scaffolds(step1.outputs))
        path = find_path(scaffolds)

        # print()
        # print_outputs(step1.outputs)
        # print(to_str(path))

        a, b, c, seq = find_moves(path)
        inputs = "\n".join(to_str(s) for s in (seq, a, b, c)) + "\nn\n"

        step2 = computer.copy()
        step2.mem[0] = 2
        step2.inputs = [ord(c) for c in inputs]
        step2.run()
        return str(step2.outputs[-1])


run(obj=Day17())
