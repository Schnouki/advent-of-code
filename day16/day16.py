#!/usr/bin/env python3

import doctest
import functools as fn
import itertools as it
import sys
import typing

Programs = typing.List[str]


def spin(programs: Programs, x: str) -> Programs:
    """Make X programs move from the end to the front.

    >>> "".join(spin(list("abcde"), '3'))
    'cdeab'
    """
    x = int(x)
    front, back = programs[:-x], programs[-x:]
    return back + front


def exchange(programs: Programs, a: str, b: str) -> Programs:
    """Swap programs at positions A and B."""
    a, b = int(a), int(b)
    programs[a], programs[b] = programs[b], programs[a]
    return programs


def partner(programs: Programs, a: str, b: str) -> Programs:
    """Partner prorams A and B."""
    pa, pb = programs.index(a), programs.index(b)
    return exchange(programs, pa, pb)


def dance(data: str, nb_programs: int = 16) -> str:
    """Let programs dance!

    >>> dance("s1,x3/4,pe/b", 5)
    'baedc'
    """
    programs = [chr(ord("a") + i) for i in range(nb_programs)]
    ops = {"s": spin, "x": exchange, "p": partner}
    for op in data.split(","):
        op_fn = ops[op[0]]
        op_args = op[1:].split("/")
        programs = op_fn(programs, *op_args)
    return "".join(programs)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Order after dancing %s: %s" % (fn, dance(data)))
