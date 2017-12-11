#!/usr/bin/env python3

import doctest
import sys


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
    return _score_group(stream, 1)


def _score_group(stream, base) -> int:
    """Compute the score for a group."""
    score = base
    in_garbage = False

    while True:
        c = next(stream)
        if in_garbage:
            if c == ">":
                in_garbage = False
            elif c == "!":
                next(stream)
        elif c == "<":
            in_garbage = True
        elif c == "}":
            return score
        elif c == "{":
            score += _score_group(stream, base + 1)


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
