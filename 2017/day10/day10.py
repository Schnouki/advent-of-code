#!/usr/bin/env python3

import doctest
from functools import reduce
import operator as op
import sys
import typing


def sparse_knot_hash(lengths: typing.List[int], nb_numbers: int,
                     rounds: int)-> typing.List[int]:
    """Compute a sparse knot hash.

    >>> sparse_knot_hash([3, 4, 1, 5], 5, 1)
    [3, 4, 2, 1, 0]
    """
    numbers = list(range(nb_numbers))
    pos = 0
    skip_size = 0

    for _ in range(rounds):
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

            # Move the current position forward by that length plus the skip
            # size.
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
    kk = sparse_knot_hash(data, hash_size, 1)
    return kk[0] * kk[1]


def knot_hash(data: str) -> str:
    """Compute the Knot Hash of the input data.

    >>> knot_hash("")
    'a2582a3a0e66e6e86e3812dcb672a272'
    >>> knot_hash("AoC 2017")
    '33efeb34ea91902bb2f59c9920caa6cd'
    >>> knot_hash("1,2,3")
    '3efbe78a8d82f29979031a4aa0b16a9d'
    >>> knot_hash("1,2,4")
    '63960835bcdc130f0b66d7ff4f6a5a8e'
    """
    lengths = [ord(c) for c in data]
    lengths += [17, 31, 73, 47, 23]

    sparse_hash = sparse_knot_hash(lengths, 256, 64)
    blocks = (reduce(op.xor, sparse_hash[16 * n: 16 * (n + 1)])
              for n in range(16))
    dense = "".join("%02x" % block for block in blocks)
    return dense


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()
            numbers = [int(n) for n in data.split(",")]

        print("k[1]×k[2] for %s: %d" % (fn, k1xk2(numbers)))
        print("Knot Hash for %s: %s" % (fn, knot_hash(data)))
