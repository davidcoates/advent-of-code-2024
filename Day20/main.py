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
        distances = self.shortest_paths(start)
        distance = distances[end]
        return distance if distance != float('inf') else None

    def shortest_paths(self, start):
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
        return distances

def read_input():
    def parse_line(line):
        return tuple(map(int, line.split(',')))
    with open("input.txt") as file:
        grid = [ line.rstrip('\n') for line in file.readlines() ]
    return grid

def create_graph(grid):
    r = len(grid)
    c = len(grid[0])
    nodes = set()
    edges = {}
    tiles = [ Vec2(i, j) for i in range(r) for j in range(c) ]
    for u in tiles:
        if grid[u.x][u.y] != '#':
            nodes.add(u)
            for direction in Direction:
                v = direction.move(u)
                if 0 <= v.x < r and 0 <= v.y < c and grid[v.x][v.y] != '#':
                    edges[(u, v)] = 1
    [start] = [ u for u in tiles if grid[u.x][u.y] == 'S' ]
    [end]   = [ u for u in tiles if grid[u.x][u.y] == 'E' ]
    return start, end, Graph(nodes, edges)

def find_cheats(max_cheat_length, saving_threshold):
    grid = read_input()
    r = len(grid)
    c = len(grid[0])
    start, end, graph = create_graph(grid)
    distances_start = graph.shortest_paths(start)
    distances_end = graph.shortest_paths(end)
    answer = 0
    for u in graph.nodes:
        for v in graph.nodes:
            if u == v:
                continue
            cheat_length = abs(v.x - u.x) + abs(v.y - u.y)
            if cheat_length > max_cheat_length:
                continue
            saving = distances_start[end] - (distances_start[u] + distances_end[v] + cheat_length)
            if saving >= saving_threshold:
                answer += 1
    return answer

def part1():
    print(find_cheats(2, 100))

def part2():
    print(find_cheats(20, 100))

if __name__ == "__main__":
    part1()
    part2()
