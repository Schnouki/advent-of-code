#!/usr/bin/env python3

import collections
import itertools as it
import doctest
import sys


def count_letters(s):
    """Count letters that appear 2 or 3 times.

    >>> count_letters("abcdef")
    (0, 0)
    >>> count_letters("bababc")
    (1, 1)
    >>> count_letters("abbcde")
    (1, 0)
    >>> count_letters("abcccd")
    (0, 1)
    >>> count_letters("aabcdd")
    (1, 0)
    >>> count_letters("abcdee")
    (1, 0)
    >>> count_letters("ababab")
    (0, 1)
    """
    two, three = 0, 0
    for letter, cnt in collections.Counter(s).items():
        if cnt == 2:
            two = 1
        elif cnt == 3:
            three = 1
    return two, three


def checksum(ids):
    """Compute the "checksum" for a list of IDs.

    >>> checksum(["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"])
    12
    """
    two, three = 0, 0
    for id_ in ids:
        a, b = count_letters(id_)
        two += a
        three += b
    return two * three


def find_correct_boxes(ids):
    """Find boxes with IDs that differ by exactly 1 character.

    >>> find_correct_boxes(["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"])
    ('fghij', 'fguij')
    """
    for a, b in it.combinations(ids, 2):
        diff = 0
        for n in range(len(a)):
            if a[n] != b[n]:
                diff += 1
        if diff == 1:
            return a, b


def find_correct_common_letters(ids):
    """Find common letters between correct box IDs.

    >>> find_correct_common_letters(["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"])
    'fgij'
    """
    a, b = find_correct_boxes(ids)
    return "".join(a[n] if a[n] == b[n] else "" for n in range(len(a)))


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip().split()

        print("Checksum for %s: %d" % (fn, checksum(data)))
        print("Common letters between correct box IDs for %s: %s" %
              (fn, find_correct_common_letters(data)))
