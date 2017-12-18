#!/usr/bin/env python3

import doctest
import functools as fn
import itertools as it
import sys
import typing

FACTOR_A = 16807
FACTOR_B = 48271
TEST_A = 65
TEST_B = 8921
PAIRS = 40000000


def generator(factor: int, start: int) -> typing.Iterator[int]:
    """Day 15 generator.

    >>> list(it.islice(generator(FACTOR_A, TEST_A), 5))
    [1092455, 1181022009, 245556042, 1744312007, 1352636452]
    >>> list(it.islice(generator(FACTOR_B, TEST_B), 5))
    [430625591, 1233683848, 1431495498, 137874439, 285222916]
    """
    value = start
    while True:
        value = (value * factor) % 2147483647
        yield value


gen_a = fn.partial(generator, FACTOR_A)
gen_b = fn.partial(generator, FACTOR_B)


def lower16(n: int) -> int:
    return n & 0xFFFF


def judge(genA: typing.Iterator[int], genB: typing.Iterator[int], steps: int) -> int:
    """Day 15 duel judge.

    >>> judge(gen_a(TEST_A), gen_b(TEST_B), 5)
    1
    >>> judge(gen_a(TEST_A), gen_b(TEST_B), PAIRS)
    588
    """
    res = 0
    for na, nb in it.islice(zip(genA, genB), steps):
        la, lb = lower16(na), lower16(nb)
        if la == lb:
            res += 1
    return res


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    START_A = 116
    START_B = 299
    print("Judge count after %d pairs: %d" %
          (PAIRS, judge(gen_a(START_A), gen_b(START_B), PAIRS)))
