#!/usr/bin/env python3

import doctest
import itertools as it
import sys
import typing

START_GRID = [['.', '#', '.'],
              ['.', '.', '#'],
              ['#', '#', '#']]

TEST_RULES = """../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#"""


Grid = typing.List[typing.List[str]]
Rules = typing.Dict[str, str]


def pattern_to_grid(pattern: str) -> Grid:
    """Convert a pattern description to a grid.

    >>> pattern_to_grid('../.#')
    [['.', '.'], ['.', '#']]
    """
    return [list(line) for line in pattern.split("/")]


def grid_to_pattern(grid: Grid) -> str:
    """Convert a grid to a pattern.

    >>> grid_to_pattern([['.', '.'], ['.', '#']])
    '../.#'
    """
    return "/".join(["".join(line) for line in grid])


def rotate_grid(grid: Grid) -> Grid:
    """Rotate a grid clockwise by 90 degrees.

    >>> rotate_grid([['.', '.'], ['.', '#']])
    [['.', '.'], ['#', '.']]
    """
    size = len(grid)
    new_grid = [[''] * size for _ in range(size)]
    for x, y in it.product(range(size), repeat=2):
        ny = x
        nx = size - y - 1
        new_grid[ny][nx] = grid[y][x]
    return new_grid


def flip_grid(grid: Grid) -> Grid:
    """Flip a grid.

    >>> flip_grid([['.', '#'], ['.', '#']])
    [['#', '.'], ['#', '.']]
    """
    return [line[::-1] for line in grid]


def rotate_pattern(pattern: str) -> str:
    """Rotate a pattern clockwise by 90 degrees.

    >>> rotate_pattern('../.#')
    '../#.'
    >>> rotate_pattern('.#./..#/###')
    '#../#.#/##.'
    """
    return grid_to_pattern(rotate_grid(pattern_to_grid(pattern)))


def flip_pattern(pattern: str) -> str:
    """Flip a pattern.

    >>> flip_pattern('.#./..#/###')
    '.#./#../###'
    """
    return grid_to_pattern(flip_grid(pattern_to_grid(pattern)))


def parse_rules(data: str) -> Rules:
    """Parse enhancement rules.

    >>> list(parse_rules('../.# => ##./#../...').keys())
    ['../.#', '../#.', '#./..', '.#/..']
    """
    rules = {}
    for line in data.splitlines():
        pattern, res = line.split(" => ")
        for _ in range(4):
            rules[pattern] = res
            rules[flip_pattern(pattern)] = res
            pattern = rotate_pattern(pattern)
    return rules


def subgrid(grid: Grid, size: int, x: int, y: int) -> Grid:
    """Get a subgrid."""
    sgrid = [[''] * size for _ in range(size)]
    for xx, yy in it.product(range(size), repeat=2):
        sgrid[yy][xx] = grid[y + yy][x + xx]
    return sgrid


def fractal_iteration(grid: Grid, rules: Rules) -> Grid:
    """Perform a single iteration of the fractal art process.

    >>> grid_to_pattern(fractal_iteration(START_GRID, parse_rules(TEST_RULES)))
    '#..#/..../..../#..#'
    """
    size = len(grid)

    if size % 2 == 0:
        sqsz = 2
    elif size % 3 == 0:
        sqsz = 3
    else:
        return grid

    # Split in squares
    new_sqsz = sqsz + 1
    new_size = (size // sqsz) * new_sqsz
    new_grid = [[''] * new_size for _ in range(new_size)]
    for x, y in it.product(range(size // sqsz), repeat=2):
        square = [[''] * sqsz for _ in range(sqsz)]
        for xx, yy in it.product(range(sqsz), repeat=2):
            square[yy][xx] = grid[y * sqsz + yy][x * sqsz + xx]

        patt = grid_to_pattern(square)
        new_square = pattern_to_grid(rules[patt])
        for xx, yy in it.product(range(new_sqsz), repeat=2):
            new_grid[y * new_sqsz + yy][x * new_sqsz + xx] = new_square[yy][xx]

    return new_grid


def count_pixels(data: str, iterations: int = 5) -> int:
    """Count active pixels after a few iterations.

    >>> count_pixels(TEST_RULES, 2)
    12
    """
    rules = parse_rules(data)
    grid = START_GRID
    for _ in range(iterations):
        grid = fractal_iteration(grid, rules)
    pattern = grid_to_pattern(grid)
    return pattern.count('#')


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :]".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        print("Pixels on for %s: %d" % (fn, count_pixels(data)))
