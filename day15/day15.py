#!/usr/bin/env python3

import doctest
import functools as fn
import itertools as it
import typing

FACTOR_A = 16807
FACTOR_B = 48271
MULT_A = 4
MULT_B = 8
TEST_A = 65
TEST_B = 8921
PAIRS1 = 40000000
PAIRS2 = 5000000


def no_test(_: int) -> bool:
    return True


def is_multiple_of(m: int) -> typing.Callable[[int], bool]:
    return lambda n: n % m == 0


def generator(factor: int, test: typing.Callable[[int], bool],
              start: int) -> typing.Iterator[int]:
    """Day 15 generator.

    >>> list(it.islice(generator(FACTOR_A, no_test, TEST_A), 5))
    [1092455, 1181022009, 245556042, 1744312007, 1352636452]
    >>> list(it.islice(generator(FACTOR_B, no_test, TEST_B), 5))
    [430625591, 1233683848, 1431495498, 137874439, 285222916]
    >>> list(it.islice(generator(FACTOR_A, is_multiple_of(MULT_A), TEST_A), 5))
    [1352636452, 1992081072, 530830436, 1980017072, 740335192]
    >>> list(it.islice(generator(FACTOR_B, is_multiple_of(MULT_B), TEST_B), 5))
    [1233683848, 862516352, 1159784568, 1616057672, 412269392]
    """
    value = start
    while True:
        value = (value * factor) % 2147483647
        if test(value):
            yield value


gen_a = fn.partial(generator, FACTOR_A)
gen_b = fn.partial(generator, FACTOR_B)

gen_a1 = fn.partial(gen_a, no_test)
gen_b1 = fn.partial(gen_b, no_test)

gen_a2 = fn.partial(gen_a, is_multiple_of(MULT_A))
gen_b2 = fn.partial(gen_b, is_multiple_of(MULT_B))


def lower16(n: int) -> int:
    return n & 0xFFFF


def judge(genA: typing.Iterator[int], genB: typing.Iterator[int], steps: int) -> int:
    """Day 15 duel judge.

    >>> judge(gen_a1(TEST_A), gen_b1(TEST_B), 5)
    1
    >>> judge(gen_a1(TEST_A), gen_b1(TEST_B), PAIRS1)
    588
    >>> judge(gen_a2(TEST_A), gen_b2(TEST_B), 1056)
    1
    >>> judge(gen_a2(TEST_A), gen_b2(TEST_B), PAIRS2)
    309
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
    print("Judge count (1) after %d pairs: %d" %
          (PAIRS1, judge(gen_a1(START_A), gen_b1(START_B), PAIRS1)))
    print("Judge count (2) after %d pairs: %d" %
          (PAIRS2, judge(gen_a2(START_A), gen_b2(START_B), PAIRS2)))
