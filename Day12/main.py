from dataclasses import dataclass
from enum import Enum, auto


# grid utils

type Point = tuple[int, int]
type Edge = tuple[Point, Point]

class Direction(Enum):
    UP = (-1, 0)
    RIGHT = (0, 1)
    DOWN = (1, 0)
    LEFT = (0, -1)

    def __init__(self, dx, dy):
        self.dx = dx
        self.dy = dy

    @staticmethod
    def from_edge(edge: Edge):
        (cell0, cell1) = edge
        return Direction(cell1[0] - cell0[0], cell1[1] - cell0[1])

    def move_point(self, point: Point) -> Point:
        (x, y) = point
        return (x + self.dx, y + self.dy)

    def move_edge(self, edge: Edge) -> Edge:
        (cell0, cell1) = edge
        return (self.move_point(cell0), self.move_point(cell1))

    def rotate(self):
        return Direction(self.dy, -self.dx)

    def flip(self):
        return Direction(-self.dx, -self.dy)


def neighbours(point: Point):
    for direction in Direction:
        yield direction.move_point(point)

def neighbours_in_grid(point: Point, grid):
    r = len(grid)
    c = len(grid[0])
    for (x, y) in neighbours(point):
        if 0 <= x < r and 0 <= y < c:
            yield (x, y)

def connected_cells(grid, cell) -> set[Point]:
    seen = {cell}
    cells = [cell]
    while cells:
        cell0 = cells.pop()
        for cell1 in neighbours_in_grid(cell0, grid):
            (x0, y0) = cell0
            (x1, y1) = cell1
            if grid[x0][y0] == grid[x1][y1] and cell1 not in seen:
                seen.add(cell1)
                cells.append(cell1)
    return seen


# a connected region of one type

@dataclass
class Region:
    ty: str
    cells: set[Point]

    def __post_init__(self):
        self.boundary = { (cell0, cell1) for cell0 in self.cells for cell1 in neighbours(cell0) if cell1 not in self.cells }

    @property
    def area(self):
        return len(self.cells)

    @property
    def perimeter(self):
        return len(list(self.boundary))

    def side_from_edge_in_direction(self, edge, direction):
        while (edge := direction.move_edge(edge)) in self.boundary:
            yield edge

    def side_from_edge(self, edge):
        yield edge
        yield from self.side_from_edge_in_direction(edge, Direction.from_edge(edge).rotate())
        yield from self.side_from_edge_in_direction(edge, Direction.from_edge(edge).rotate().flip())

    @property
    def sides(self):
        sides = 0
        edges = set()
        for edge in self.boundary:
            if edge in edges:
                continue
            (cell0, cell1) = edge
            sides += 1
            edges.update(self.side_from_edge(edge))
        return sides


def regions(grid):
    r = len(grid)
    c = len(grid[0])
    regions = []
    seen = set()
    for i in range(r):
        for j in range(c):
            if (i, j) in seen:
                continue
            region = Region(grid[i][j], connected_cells(grid, (i, j)))
            regions.append(region)
            seen.update(region.cells)
    return regions

def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    return lines

def calculate_total_cost(region_cost) -> int:
    grid = read_input()
    return sum(region_cost(region) for region in regions(grid))

def part1():
    print(calculate_total_cost(lambda region: region.area * region.perimeter))

def part2():
    print(calculate_total_cost(lambda region: region.area * region.sides))

if __name__ == "__main__":
    part1()
    part2()
