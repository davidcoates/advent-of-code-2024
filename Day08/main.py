from collections import defaultdict
from dataclasses import dataclass
import itertools
import math


type Position = tuple[int, int]

@dataclass
class Grid:
    bounds: Position
    antennae_positions_by_frequency: dict[str, set[Position]]

    def __init__(self, grid: list[str]):
        self.bounds = (len(grid), len(grid[0]))
        self.antennae_positions_by_frequency = defaultdict(set)
        for x, row in enumerate(grid):
            for y, frequency in enumerate(row):
                if frequency != '.':
                    self.antennae_positions_by_frequency[frequency].add((x, y))


def read_input():
    with open("input.txt") as file:
        grid = [ line.rstrip('\n') for line in file.readlines() ]
    return Grid(grid)


def part1():
    grid = read_input()
    (r, c) = grid.bounds

    def calculate_antinode_positions(positions: set[Position]) -> set[Position]:
        antinode_positions = set()
        for (x0, y0), (x1, y1) in itertools.combinations(positions, 2):
            (xd, yd) = (x1 - x0, y1 - y0)
            antinode_positions.add((x0 - xd, y0 - yd))
            antinode_positions.add((x1 + xd, y1 + yd))
        return { (x, y) for (x, y) in antinode_positions if 0 <= x < r and 0 <= y < c }

    antinode_positions = set()
    for frequency, positions in grid.antennae_positions_by_frequency.items():
        antinode_positions.update(calculate_antinode_positions(positions))
    print(len(antinode_positions))


def part2():
    grid = read_input()
    (r, c) = grid.bounds

    def calculate_antinode_positions(positions: set[Position]) -> set[Position]:
        antinode_positions = set()
        for (x0, y0), (x1, y1) in itertools.combinations(positions, 2):
            (xd, yd) = (x1 - x0, y1 - y0)
            assert math.gcd(xd, yd) == 1 # note: otherwise we'd need to divide xd and yd by the gcd
            (x, y) = (x0, y0)
            while 0 <= x < r and 0 <= y < c:
                antinode_positions.add((x, y))
                (x, y) = (x - xd, y - yd)
            (x, y) = (x0, y0)
            while 0 <= x < r and 0 <= y < c:
                antinode_positions.add((x, y))
                (x, y) = (x + xd, y + yd)
        return antinode_positions

    antinode_positions = set()
    for frequency, positions in grid.antennae_positions_by_frequency.items():
        antinode_positions.update(calculate_antinode_positions(positions))
    print(len(antinode_positions))


if __name__ == "__main__":
    part1()
    part2()
