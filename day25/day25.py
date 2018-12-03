#!/usr/bin/env python3

import collections
import doctest
import sys

TEST_INPUT = """Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A."""


class Machine:
    def __init__(self, blueprint: str):
        """Initialize a Turing machine.

        >>> Machine(TEST_INPUT).state
        'A'
        >>> Machine(TEST_INPUT).max_steps
        6
        >>> Machine(TEST_INPUT).states
        {'A': (1, 1, 'B', 0, -1, 'B'), 'B': (1, -1, 'A', 1, 1, 'A')}
        """

        self.tape_ones = set()
        self.cursor = 0
        self.states = {}
        self.state = ""
        self.max_steps = 0

        # Parse the blueprint!
        lines = [line.strip().rstrip(".").rstrip(":").split()
                 for line in blueprint.splitlines()]
        self.state = lines[0][-1]
        self.max_steps = int(lines[1][-2])

        def _state_val(m):
            return (
                int(lines[m+1][-1]),
                +1 if lines[m+2][-1] == "right" else -1,
                lines[m+3][-1],
            )

        n = 3
        while n < len(lines):
            _state = lines[n][-1]
            _op = (*_state_val(n+1), *_state_val(n+5))
            self.states[_state] = _op
            n += 10

    def run(self, progress=None):
        """Run the blueprint."""
        for n in range(self.max_steps):
            if progress and n % progress == 0:
                print("\r%d/%d" % (n, self.max_steps), end="", flush=True)
            self.run_step()

    def run_step(self):
        """Run a blueprint step."""
        op = self.states[self.state]
        value = 1 if self.cursor in self.tape_ones else 0
        n = 0 if value == 0 else 3
        new_val, cursor_move, next_state = op[n], op[n+1], op[n+2]
        if new_val == 1:
            self.tape_ones.add(self.cursor)
        else:
            self.tape_ones.discard(self.cursor)
        self.cursor += cursor_move
        self.state = next_state

    def diagnostic(self):
        """Count how many 1s are o =n the tape.

        >>> m = Machine(TEST_INPUT); m.run(); m.diagnostic()
        3
        """
        return len(self.tape_ones)


if __name__ == "__main__":
    err, tot = doctest.testmod()
    if err == 0:
        print("{} tests OK :)".format(tot))

    for fn in sys.argv[1:]:
        with open(fn, "r") as fin:
            data = fin.read().strip()

        machine = Machine(data)
        machine.run(progress=100000)
        print("\rDiagnostic for %s: %d" % (fn, machine.diagnostic()))
