from enum import Enum, auto
import itertools
import math

type Equation = tuple[int, list[int]]

def parse_equations() -> list[Equation]:
    def parse_line(line: str):
        [value, operands] = line.rstrip('\n').split(': ')
        return int(value), list(map(int, operands.split(' ')))
    with open("input.txt") as file:
        lines = [ parse_line(line) for line in file.readlines() ]
    return lines

class Operator(Enum):
    PLUS = auto()
    TIMES = auto()
    CONCAT = auto()

    def eval(self, a, b):
        match self:
            case Operator.PLUS:
                return a + b
            case Operator.TIMES:
                return a * b
            case Operator.CONCAT:
                n = math.floor(math.log(b, 10)) + 1 if b > 0 else 1
                return a * 10**n + b


def is_satisfiable(equation: Equation, allowed_operators: list[Operator]) -> bool:
    (value, operands) = equation
    n = len(operands)
    for operators in itertools.product(allowed_operators, repeat=(n - 1)):
        answer = operands[0]
        for i in range(n - 1):
            answer = operators[i].eval(answer, operands[i + 1])
        if answer == value:
            return True
    return False

def part1():
    equations = parse_equations()
    answer = sum( equation[0] for equation in equations if is_satisfiable(equation, [Operator.PLUS, Operator.TIMES]) )
    print(answer)

def part2():
    equations = parse_equations()
    answer = sum( equation[0] for equation in equations if is_satisfiable(equation, [Operator.PLUS, Operator.TIMES, Operator.CONCAT]) )
    print(answer)

if __name__ == "__main__":
    part1()
    part2()
