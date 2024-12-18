from dataclasses import dataclass, field
from enum import Enum, auto
from heapq import heappop, heappush
from typing import Any


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


class Direction(Enum):
    UP = -1, 0
    RIGHT = 0, 1
    DOWN = 1, 0
    LEFT = 0, -1

    def __init__(self, dx, dy):
        self.delta = Vec2(dx, dy)

    def move(self, point: Vec2) -> Vec2:
        return point + self.delta

    def rotate_anticlockwise(self):
        return Direction(-self.delta.y, self.delta.x)

    def rotate_clockwise(self):
        return Direction(self.delta.y, -self.delta.x)


class Graph:

    def __init__(self, nodes, edges):
        self.nodes = nodes
        self.edges = edges
        self.neighbours = { node: set() for node in self.nodes }
        for (u, v) in self.edges.keys():
            self.neighbours[u].add(v)

    @dataclass(order = True)
    class Item:
        priority: int
        node: Any = field(compare=False)

    def shortest_path(self, start, end):
        distances = { node: float('inf') for node in self.nodes }
        queue = [Graph.Item(0, start)]
        distances[start] = 0
        while queue:
            u = heappop(queue).node
            for v in self.neighbours[u]:
                score = distances[u] + self.edges[(u, v)]
                if score < distances[v]:
                    distances[v] = score
                    heappush(queue, Graph.Item(distances[v], v))
        distance = distances[end]
        return distance if distance != float('inf') else None


def read_input():
    def parse_line(line):
        return tuple(map(int, line.split(',')))
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    return [ parse_line(line) for line in lines ]

def calculate_shortest_path(corrupted_bytes, grid_size) -> float | None:
    corrupted_nodes = { Vec2(x, y) for (x, y) in corrupted_bytes }
    nodes = { node for i in range(grid_size) for j in range(grid_size) if (node := Vec2(i, j)) not in corrupted_nodes }
    edges = dict()
    for u in nodes:
        for direction in Direction:
            v = direction.move(u)
            if v in nodes:
                edges[(u, v)] = 1
    graph = Graph(nodes, edges)
    start = Vec2(0, 0)
    end = Vec2(grid_size - 1, grid_size - 1)
    return graph.shortest_path(start, end)

SIZE = 71

def part1():
    bytes = read_input()
    print(calculate_shortest_path(bytes[:1024], SIZE))

def part2():
    bytes = read_input()
    # find the first corrupted byte for which inclusion results in no path
    # invariant: bytes[:low] has a path but bytes[:high] does not
    low = 1024
    high = len(bytes)
    while low + 1 < high:
        mid = (low + high) // 2
        if calculate_shortest_path(bytes[:mid], SIZE) is None:
            high = mid
        else:
            low = mid
    (i, j) = bytes[low]
    print(f"{i},{j}")

if __name__ == "__main__":
    part1()
    part2()
