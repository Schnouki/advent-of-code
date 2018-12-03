#!/usr/bin/env python3

import doctest
import sys
import typing


def score(data: str) -> int:
    """Compute the score for an input stream.

    >>> score('{}')
    1
    >>> score('{{{}}}')
    6
    >>> score('{{},{}}')
    5
    >>> score('{{{},{},{{}}}}')
    16
    >>> score('{<a>,<a>,<a>,<a>}')
    1
    >>> score('{{<ab>},{<ab>},{<ab>},{<ab>}}')
    9
    >>> score('{{<!!>},{<!!>},{<!!>},{<!!>}}')
    9
    >>> score('{{<a!>},{<a!>},{<a!>},{<ab>}}')
    3
    """
    stream = iter(data)
    c = next(stream)
    if c != "{":
        raise ValueError(c)
    return _process_group(stream, 1)[0]


def count_garbage(data: str) -> int:
    """Count the garbage in an input stream.

    >>> count_garbage('{<>}')
    0
    >>> count_garbage('{<random characters>}')
    17
    >>> count_garbage('{<<<<>}')
    3
    >>> count_garbage('{<{!>}>}')
    2
    >>> count_garbage('{<!!>}')
    0
    >>> count_garbage('{<!!!>>}')
    0
    >>> count_garbage('{<{o"i!a,<{i<a>}')
    10
    """
    stream = iter(data)
    c = next(stream)
    if c != "{":
        raise ValueError(c)
    return _process_group(stream, 1)[1]


def _process_group(stream, base) -> typing.Tuple[int, int]:
    """Compute the score and garbage count for a group."""
    score, garbage = base, 0
    in_garbage = False

    while True:
        c = next(stream)
        if in_garbage:
            if c == ">":
                in_garbage = False
            elif c == "!":
                next(stream)
            else:
                garbage += 1
        elif c == "<":
            in_garbage = True
        elif c == "}":
            return score, garbage
        elif c == "{":
            group_data = _process_group(stream, base + 1)
            score += group_data[0]
            garbage += group_data[1]


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Score for %s: %d" % (fn, score(data)))
        print("Garbage for %s: %d" % (fn, count_garbage(data)))
