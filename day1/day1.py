#!/usr/bin/env python3

import doctest
import sys


def inv_captcha(data: str) -> int:
    """Compute the "inverse captcha" using the "next" digit.

    >>> inv_captcha("1122")
    3
    >>> inv_captcha("1111")
    4
    >>> inv_captcha("1234")
    0
    >>> inv_captcha("91212129")
    9
    """
    digits = [int(c) for c in data]
    digits.append(digits[0])
    total, prev = 0, None
    for digit in digits:
        if digit == prev:
            total += digit
        prev = digit
    return total


def inv_captcha_hwa(data: str) -> int:
    """Compute the "inverse captcha" using the digit halfway around.

    >>> inv_captcha_hwa("1212")
    6
    >>> inv_captcha_hwa("1221")
    0
    >>> inv_captcha_hwa("123425")
    4
    >>> inv_captcha_hwa("123123")
    12
    >>> inv_captcha_hwa("12131415")
    4
    """
    digits = [int(c) for c in data]
    N = len(digits)
    hwa = N // 2
    total = 0
    for n in range(N):
        digit = digits[n]
        other = digits[(n + hwa) % N]
        if digit == other:
            total += digit
    return total


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()
        print('Captcha "next" for {}: {}'.format(fn, inv_captcha(data)))
        print('Captcha "HWA" for {}: {}'.format(fn, inv_captcha_hwa(data)))
