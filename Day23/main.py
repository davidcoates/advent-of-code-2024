import itertools
from functools import lru_cache
import random


class Graph:

    def __init__(self, nodes, edges):
        self.nodes = nodes
        self.edges = edges
        self.neighbours = { node: set() for node in self.nodes }
        for (u, v) in self.edges.keys():
            self.neighbours[u].add(v)

    def cliques_3(self):
        cliques = set()
        for node1 in self.nodes:
            for (node2, node3) in itertools.combinations(self.neighbours[node1], 2):
                if (node2, node3) in self.edges:
                    cliques.add(frozenset((node1, node2, node3)))
        return cliques

    # Bron-Kerbosch
    def _maximal_cliques(self, R, P, X):
        if not P and not X:
            yield R
            return
        u = next(iter(P | X))
        for v in list(P - self.neighbours[u]):
            yield from self._maximal_cliques(R | {v}, P & self.neighbours[v], X & self.neighbours[v])
            P -= {v}
            X |= {v}

    def maximal_cliques(self):
        return self._maximal_cliques(set(), set(self.nodes), set())

    def largest_clique(self):
        return max(self.maximal_cliques(), key=len)


def read_input():
    def parse_line(line):
        return tuple(line.split('-'))
    with open("input.txt") as file:
        connections = [ parse_line(line.rstrip('\n')) for line in file.readlines() ]
    edges = dict()
    nodes = set()
    for (node1, node2) in connections:
        nodes.add(node1)
        nodes.add(node2)
        edges[(node1, node2)] = 1
        edges[(node2, node1)] = 1
    graph = Graph(nodes, edges)
    return graph

def part1():
    graph = read_input()
    cliques = [ clique for clique in graph.cliques_3() if any(node.startswith('t') for node in clique) ]
    print(len(cliques))

def part2():
    graph = read_input()
    clique = graph.largest_clique()
    print(','.join(sorted(clique)))

if __name__ == "__main__":
    part1()
    part2()
