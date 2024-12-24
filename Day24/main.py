from os import WCOREDUMP
import re
from enum import Enum, auto
from dataclasses import dataclass
from collections import defaultdict
import random


class Operation(Enum):
    XOR = auto()
    OR = auto()
    AND = auto()

    def eval(self, in0, in1):
        match self:
            case Operation.XOR:
                return in0 ^ in1
            case Operation.OR:
                return in0 | in1
            case Operation.AND:
                return in0 & in1


@dataclass(frozen=True)
class Wire:
    name: str
    index: int | None

    @staticmethod
    def from_string(wire):
        if '0' <= wire[-1] <= '9':
            return Wire(wire[0], int(wire[1:]))
        else:
            return Wire(wire, None)

    def __lt__(self, other):
        assert self.name == other.name
        return self.index < other.index

    def __str__(self):
        if self.index is None:
            return self.name
        else:
            return f"{self.name}{self.index:02d}"


@dataclass(frozen=True)
class Gate:
    in0: Wire
    op: Operation
    in1: Wire
    out: Wire

    PATTERN = re.compile(r'(\w+) (\w+) (\w+) -> (\w+)')

    @staticmethod
    def from_string(gate):
        [in0, op, in1, out] = Gate.PATTERN.match(gate).groups()
        return Gate(
            Wire.from_string(in0),
            Operation[op],
            Wire.from_string(in1),
            Wire.from_string(out),
        )


class Values:

    def __init__(self):
        self.values_by_wire = {}

    def __getitem__(self, key):
        if isinstance(key, Wire):
            return self.values_by_wire[key]
        else:
            value = 0
            for wire in reversed(key):
                value = 2*value + self.values_by_wire[wire]
            return value

    def __setitem__(self, key, value):
        if isinstance(key, Wire):
            self.values_by_wire[key] = value
        else:
            for wire in key:
                self.values_by_wire[wire] = value % 2
                value = value >> 1

    def __contains__(self, key):
        if isinstance(key, Wire):
            return key in self.values_by_wire
        else:
            assert False


class Circuit:

    def __init__(self, gates):

        self.gates = gates

        self.gates_by_input = defaultdict(list)
        self.wires = set()
        for gate in gates:
            self.wires.add(gate.in0)
            self.wires.add(gate.in1)
            self.wires.add(gate.out)
            assert gate.in0 != gate.in1
            self.gates_by_input[gate.in0].append(gate)
            self.gates_by_input[gate.in1].append(gate)

        self.x = self._create_wire_group('x')
        self.y = self._create_wire_group('y')
        self.z = self._create_wire_group('z')


    def _create_wire_group(self, name):
        wires = [ wire for wire in self.wires if wire.name == name]
        wires.sort()
        return wires

    def eval(self, x: int, y: int) -> int:
        values = Values()
        values[self.x] = x
        values[self.y] = y
        wires_to_check = self.x + self.y + self.z
        while wires_to_check:
            wire = wires_to_check.pop()
            for gate in self.gates_by_input[wire]:
                if gate.in0 in values and gate.in1 in values:
                    in0 = values[gate.in0]
                    in1 = values[gate.in1]
                    out = gate.op.eval(in0, in1)
                    values[gate.out] = out
                    wires_to_check.append(gate.out)
        return values[self.z]


def read_input():

    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]

    i = lines.index('')

    values = Values()
    for line in lines[:i]:
        [wire, value] = line.split(': ')
        wire, value = Wire.from_string(wire), int(value)
        values[wire] = value

    gates = [ Gate.from_string(line) for line in lines[i+1:] ]

    return values, Circuit(gates)


def part1():
    values, circuit = read_input()
    z = circuit.eval(values[circuit.x], values[circuit.y])
    print(z)


def part2():

    _, circuit = read_input()

    wrong = []
    for gate in circuit.gates:

        if gate.out.name == 'z' and gate.out != circuit.z[-1] and gate.op != Operation.XOR:
            wrong.append(gate.out)
            continue

        if gate.op == Operation.XOR:
            if gate.in0.name not in ['x', 'y','z'] and gate.in1.name not in ['x', 'y', 'z'] and gate.out.name not in ['x', 'y', 'z']:
                wrong.append(gate.out)
                continue
            if any(next.op == Operation.OR for next in circuit.gates_by_input[gate.out]):
                wrong.append(gate.out)
                continue

        if gate.op == Operation.AND and Wire('x', 0) not in [ gate.in0, gate.in1 ]:
            if any(other.op != Operation.OR for other in circuit.gates_by_input[gate.out]):
                wrong.append(gate.out)
                continue

    print(','.join(sorted(map(str, wrong))))


if __name__ == "__main__":
    part1()
    part2()
