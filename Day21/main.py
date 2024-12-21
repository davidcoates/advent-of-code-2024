from dataclasses import dataclass, field
from enum import Enum, auto
from functools import lru_cache


@dataclass
class Vec2:
    x: int
    y: int

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)

    def __add__(self, other):
        return Vec2(self.x + other.x, self.y + other.y)

    def directions_to(self, other):
        if self.x < other.x:
            yield Direction.DOWN
        elif self.x > other.x:
            yield Direction.UP
        if self.y < other.y:
            yield Direction.RIGHT
        elif self.y > other.y:
            yield Direction.LEFT


class Direction(Enum):
    UP = -1, 0, '^'
    RIGHT = 0, 1, '>'
    DOWN = 1, 0, 'v'
    LEFT = 0, -1, '<'

    def __init__(self, dx, dy, symbol):
        self.delta = Vec2(dx, dy)
        self.symbol = symbol

    def move(self, point: Vec2) -> Vec2:
        return point + self.delta


numeric_keypad_button_positions = {
    '7': Vec2(0, 0),
    '8': Vec2(0, 1),
    '9': Vec2(0, 2),
    '4': Vec2(1, 0),
    '5': Vec2(1, 1),
    '6': Vec2(1, 2),
    '1': Vec2(2, 0),
    '2': Vec2(2, 1),
    '3': Vec2(2, 2),
    '0': Vec2(3, 1),
    'A': Vec2(3, 2),
}

directional_keypad_button_positions = {
    '^': Vec2(0, 1),
    'A': Vec2(0, 2),
    '<': Vec2(1, 0),
    'v': Vec2(1, 1),
    '>': Vec2(1, 2),
}

class Keypad(Enum):
    DIRECTIONAL = 0
    NUMERIC = 1

    @property
    def button_positions(self):
        match self:
            case Keypad.DIRECTIONAL:
                return directional_keypad_button_positions
            case Keypad.NUMERIC:
                return numeric_keypad_button_positions

    def button_position(self, button):
        return self.button_positions[button]

    def paths_between(self, curr, dest):
        if curr == dest:
            yield []
        else:
            for direction in curr.directions_to(dest):
                next = direction.move(curr)
                if next not in self.button_positions.values():
                    continue
                for path in self.paths_between(next, dest):
                    yield [direction.symbol] + path


def cheapest_keypad_sequence(keypad, sequence, num_robots):
    if num_robots == 0:
        return len(sequence)
    positions = [ keypad.button_position('A') ] + list(map(keypad.button_position, sequence))
    return sum(cheapest_keypad_move(keypad, positions[i], positions[i+1], num_robots) for i in range(len(positions) - 1))

@lru_cache(maxsize=None)
def cheapest_keypad_move(keypad, curr, dest, num_robots):
    return min(cheapest_keypad_sequence(Keypad.DIRECTIONAL, path + ['A'], num_robots - 1) for path in keypad.paths_between(curr, dest))

def solve_puzzle(num_robots):
    with open("input.txt") as file:
        sequences = [ line.rstrip('\n') for line in file.readlines() ]
    def compute_complexity(sequence):
        sequence_as_number = int(sequence[:sequence.index('A')])
        return sequence_as_number * cheapest_keypad_sequence(Keypad.NUMERIC, sequence, num_robots)
    return sum(compute_complexity(sequence) for sequence in sequences)

def part1():
    print(solve_puzzle(3))

def part2():
    print(solve_puzzle(26))

if __name__ == "__main__":
    part1()
    part2()
