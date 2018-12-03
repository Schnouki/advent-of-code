#!/usr/bin/env python3

import doctest
import itertools as it
import sys
import typing


def is_passphrase_valid(phrase: str, allow_anagrams: bool = True) -> bool:
    """Check if a passphrase is valid.

    >>> is_passphrase_valid("aa bb cc dd ee")
    True
    >>> is_passphrase_valid("aa bb cc dd aa")
    False
    >>> is_passphrase_valid("aa bb cc dd aaa")
    True


    >>> is_passphrase_valid("abcde fghij", False)
    True
    >>> is_passphrase_valid("abcde xyz ecdab", False)
    False
    >>> is_passphrase_valid("a ab abc abd abf abj", False)
    True
    >>> is_passphrase_valid("iiii oiii ooii oooi oooo", False)
    True
    >>> is_passphrase_valid("oiii ioii iioi iiio", False)
    False
    """
    words = [w.strip() for w in phrase.split()]
    if not allow_anagrams:
        words = [str(sorted(word)) for word in words]
    unique_words = set(words)
    return len(words) == len(unique_words)


def count_valid_passphrases(data: str, allow_anagrams: bool = True) -> int:
    """Count the number of valid passphrases in an input."""
    return sum(1 if is_passphrase_valid(line, allow_anagrams) else 0
               for line in data.splitlines())


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))
    else:
        sys.exit(1)

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Valid passphrases (allowing anagrams) in %s: %d" %
              (fn, count_valid_passphrases(data)))
        print("Valid passphrases (forbidding anagrams) in %s: %d" %
              (fn, count_valid_passphrases(data, False)))
