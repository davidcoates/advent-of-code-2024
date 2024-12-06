from enum import Enum, auto
from os import getresgid

type Position = tuple[int, int]

class Direction(Enum):
    UP = auto()
    RIGHT = auto()
    DOWN = auto()
    LEFT = auto()

    def move(self, i, j) -> Position:
        match self:
            case Direction.UP:
                return (i - 1, j)
            case Direction.RIGHT:
                return (i, j + 1)
            case Direction.DOWN:
                return (i + 1, j)
            case Direction.LEFT:
                return (i, j - 1)

    def rotate(self):
        match self:
            case Direction.UP:
                return Direction.RIGHT
            case Direction.RIGHT:
                return Direction.DOWN
            case Direction.DOWN:
                return Direction.LEFT
            case Direction.LEFT:
                return Direction.UP


def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    return lines

def patrol(grid) -> tuple[set[Position], bool]:
    direction = Direction.UP
    (i, j) = next((i, row.index('^')) for (i, row) in enumerate(grid) if '^' in row)
    r = len(grid)
    c = len(grid[0])
    positions = set()
    seen = set()
    while True:
        if (i, j, direction) in seen:
            return (positions, True)
        seen.add((i, j, direction))
        if (i, j) not in positions:
            positions.add((i, j))
        (next_i, next_j) = direction.move(i, j)
        if 0 <= next_i < r and 0 <= next_j < c:
            if grid[next_i][next_j] == '#':
                direction = direction.rotate()
                continue
            else:
                (i, j) = (next_i, next_j)
        else:
            return (positions, False)

def part1():
    grid = read_input()
    (positions, _) = patrol(grid)
    print(len(positions))

def part2():
    grid = read_input()
    r = len(grid)
    c = len(grid[0])
    answer = 0
    for i in range(r):
        for j in range(c):
            if grid[i][j] != '.':
                continue
            tmp = grid[i]
            grid[i] = grid[i][:j] + '#' + grid[i][j+1:]
            (_, stuck) = patrol(grid)
            if stuck:
                answer += 1
            grid[i] = tmp
    print(answer)

if __name__ == "__main__":
    part1()
    part2()
