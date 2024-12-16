from heapq import heappush, heappop
from enum import Enum
from dataclasses import dataclass, field
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

    def __init__(self, start, end, nodes, edges):
        self.start = start
        self.end = end
        self.nodes = nodes
        self.edges = edges
        self.neighbours = { node: set() for node in self.nodes }
        for (u, v) in self.edges.keys():
            self.neighbours[u].add(v)

    @dataclass(order = True)
    class Item:
        priority: int
        node: Any = field(compare=False)

    def _shortest_paths(self):
        start = (self.start, Direction.RIGHT)

        distances = { node: float('inf') for node in self.nodes }
        prev = { node: set() for node in self.nodes }
        queue = [Graph.Item(0, start)]
        distances[start] = 0

        while queue:
            u = heappop(queue).node
            for v in self.neighbours[u]:
                score = distances[u] + self.edges[(u, v)]
                if score < distances[v]:
                    distances[v] = score
                    heappush(queue, Graph.Item(distances[v], v))
                    prev[v] = {u}
                if score == distances[v]:
                    prev[v].add(u)
        return (prev, distances)

    def shortest_path_cost(self):
        prev, distances = self._shortest_paths()
        min_distance = min(distances[(self.end, direction)] for direction in Direction)
        return min_distance

    def find_nodes_in_path(self, start, end, prev, nodes):
        if end in nodes:
            return
        nodes.add(end)
        if start == end:
            return
        for node in prev[end]:
            self.find_nodes_in_path(start, node, prev, nodes)

    def shortest_path_tiles(self):
        prev, distances = self._shortest_paths()
        min_distance = min(distances[(self.end, direction)] for direction in Direction)
        nodes = set()
        for direction in Direction:
            start = (self.start, Direction.RIGHT)
            end = (self.end, direction)
            if distances[end] == min_distance:
                self.find_nodes_in_path(start, end, prev, nodes)
        tiles = { node[0] for node in nodes }
        return len(tiles)


def read_input():
    with open("input.txt") as file:
        grid = [ line.rstrip('\n') for line in file.readlines() ]
    r = len(grid)
    c = len(grid[0])
    nodes = set()
    edges = dict()
    for i in range(r):
        for j in range(c):
            if grid[i][j] == '#':
                continue
            for direction in Direction:
                u = (Vec2(i, j), direction)
                nodes.add(u)
                v = (Vec2(i, j), direction.rotate_clockwise())
                edges[(u, v)] = 1000
                v = (Vec2(i, j), direction.rotate_anticlockwise())
                edges[(u, v)] = 1000
                move = direction.move(Vec2(i, j))
                if grid[move.x][move.y] != '#':
                    v = (move, direction)
                    edges[(u, v)] = 1
    start = next(Vec2(i, j) for i in range(r) for j in range(c) if grid[i][j] == 'S')
    end = next(Vec2(i, j) for i in range(r) for j in range(c) if grid[i][j] == 'E')
    return Graph(start, end, nodes, edges)


def part1():
    graph = read_input()
    print(graph.shortest_path_cost())

def part2():
    graph = read_input()
    print(graph.shortest_path_tiles())

if __name__ == "__main__":
    part1()
    part2()
