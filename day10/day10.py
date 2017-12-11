#!/usr/bin/env python3

import doctest
import sys
import typing


def knot_hash(lengths: typing.List[int], nb_numbers: int) -> typing.List[int]:
    """Compute a knot hash.

    >>> knot_hash([3, 4, 1, 5], 5)
    [3, 4, 2, 1, 0]
    """
    numbers = list(range(nb_numbers))
    pos = 0
    skip_size = 0

    for length in lengths:
        if length > len(numbers):
            raise ValueError(length)

        # Reverse the order of that length of elements in the list, starting
        # with the element at the current position.
        la, lb = pos + length, 0
        if la >= len(numbers):
            lb = la - len(numbers)
            la = len(numbers)
        lc = la - pos
        nbs_to_reverse = numbers[pos:la] + numbers[0:lb]
        nbs_reversed = nbs_to_reverse[::-1]
        numbers[pos:la] = nbs_reversed[:lc]
        numbers[0:lb] = nbs_reversed[lc:]

        # Move the current position forward by that length plus the skip size.
        pos += length + skip_size
        pos %= len(numbers)

        # Increase the skip size by one.
        skip_size += 1

    return numbers


def k1xk2(data: typing.List[int], hash_size: int = 256) -> int:
    """Product the first 2 numbers of the knot hash of the input data.

    >>> k1xk2([3, 4, 1, 5], 5)
    12
    """
    kk = knot_hash(data, hash_size)
    return kk[0] * kk[1]


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = [int(n) for n in fin.read().split(",")]

        print("k[1]×k[2] for %s: %d" % (fn, k1xk2(data)))
