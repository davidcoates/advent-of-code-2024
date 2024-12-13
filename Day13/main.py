import re
from dataclasses import dataclass


type Number = int | float

@dataclass
class Matrix:
    a: Number
    b: Number
    c: Number
    d: Number

    @staticmethod
    def from_columns(column0, column1):
        (a, c) = column0
        (b, d) = column1
        return Matrix(a, b, c, d)

    @property
    def determinant(self):
        return self.a*self.d - self.b*self.c

    def inverse(self):
        assert self.determinant != 0
        det = self.determinant
        return Matrix(self.d / det, -self.b / det, -self.c / det, self.a / det)

    def __mul__(self, other):
        (x, y) = other
        return (self.a * x + self.b * y, self.c * x + self.d * y)


@dataclass
class Machine:
    buttonA: tuple[int, int]
    buttonB: tuple[int, int]
    prize: tuple[int, int]

    def solution(self) -> int | None:
        A = Matrix.from_columns(self.buttonA, self.buttonB)
        if A.determinant == 0:
            return None
        (n, m) = A.inverse() * self.prize
        n, m = round(n), round(m)
        if A * (n, m) == self.prize:
            return 3*n + m
        else:
            return None

def read_input():
    PATTERN = re.compile(r'.*: X.(\d+), Y.(\d+)')
    def parse_line(line):
        (x, y) = PATTERN.match(line).groups()
        return (int(x), int(y))
    machines = []
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
        i = 0
        while i < len(lines):
            machines.append(Machine(
                parse_line(lines[i]),
                parse_line(lines[i+1]),
                parse_line(lines[i+2])
            ))
            i += 4
    return machines

def part1():
    machines = read_input()
    print(sum(machine.solution() or 0 for machine in machines))

def part2():
    machines = read_input()
    for machine in machines:
        (x, y) = machine.prize
        machine.prize = (10000000000000 + x, 10000000000000 + y)
    print(sum(machine.solution() or 0 for machine in machines))

if __name__ == "__main__":
    part1()
    part2()
