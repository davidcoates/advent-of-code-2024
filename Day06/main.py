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
    (x, y) = next((x, row.index('^')) for (x, row) in enumerate(grid) if '^' in row)
    r = len(grid)
    c = len(grid[0])
    positions = set()
    seen = set()
    while True:
        if (x, y, direction) in seen:
            return (positions, True)
        seen.add((x, y, direction))
        if (x, y) not in positions:
            positions.add((x, y))
        (x_next, y_next) = direction.move(x, y)
        if 0 <= x_next < r and 0 <= y_next < c:
            if grid[x_next][y_next] == '#':
                direction = direction.rotate()
                continue
            else:
                (x, y) = (x_next, y_next)
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
    for x in range(r):
        for y in range(c):
            if grid[x][y] != '.':
                continue
            tmp = grid[x]
            grid[x] = grid[x][:y] + '#' + grid[x][y+1:]
            (_, stuck) = patrol(grid)
            if stuck:
                answer += 1
            grid[x] = tmp
    print(answer)

if __name__ == "__main__":
    part1()
    part2()
