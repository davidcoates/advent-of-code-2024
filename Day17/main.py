import re

REGISTER_PATTERN = re.compile(r"Register \w: (\d+)")

def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    i = lines.index("")
    def parse_register(line):
        value = REGISTER_PATTERN.match(line).group(1)
        return int(value)
    registers = tuple(map(parse_register, lines[:i]))
    program = list(map(int, lines[i+1].split(" ")[1].split(",")))
    return registers, program

def run_temp(registers, program):
    ( register_A, register_B, register_C ) = registers
    out = []
    ip = 0
    while ip < len(program):
        opcode = program[ip]
        operand = program[ip + 1]
        match operand:
            case 0:
                combo = operand
            case 1:
                combo = operand
            case 2:
                combo = operand
            case 3:
                combo = operand
            case 4:
                combo = register_A
            case 5:
                combo = register_B
            case 6:
                combo = register_C
        jump = False
        match opcode:
            case 0:
                print('A >>= combo')
                register_A = register_A >> combo
            case 1:
                print('B ^ {}'.format(operand))
                register_B = register_B ^ operand
            case 2:
                print("B = combo % 8")
                register_B = combo % 8
            case 3:
                if register_A != 0:
                    ip = operand
                    jump = True
            case 4:
                print("B = B ^ C")
                register_B = register_B ^ register_C
            case 5:
                print("emit combo")
                out.append(combo % 8)
            case 6:
                print("B = A >> combo")
                register_B = register_A >> combo
            case 7:
                print("C = A >> combo")
                register_C = register_A >> combo
        if not jump:
            ip += 2
    return out

def run(registers, _):
    ( A, _, _ ) = registers
    out = []
    X = A >> ((A % 8) ^ 7)
    Y = (A % 8) ^ X
    out.append(Y % 8)
    A = A >> 3
    while A != 0:
        X = A >> ((A % 8) ^ 7)
        Y = (A % 8) ^ X
        out.append(Y % 8)
        A = A >> 3
    return out

def part1():
    registers, program = read_input()
    out = run(registers, program)
    print(','.join(map(str, out)))


"""
our program is:

2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0

i.e.

do
    B = A % 8
    B = B ^ 7
    C = A >> B
    B = B ^ 7
    B = B ^ C
    A = A >> 3
    emit (B % 8)
while A != 0

we can further rewrite this:

do
    L = A % 8
    X = L ^ ((A >> (7 - L)) % 8)
    emit X
    A = A >> 3
while A != 0

so the value emitted:

    by the first iteration depends only on bits 0 to 9 of A,

    by the second iteration depends only on bits 3 to 12 of A,

    by the third iteraiton depends only on bits 6 to 15 of A,

    and so on.

"""

def find_quines_rec(program, i, low, low_bits):
    for high in range(2**3):
        a = (high << low_bits) + low
        out = run((a, 0, 0), program)
        if i < len(out) and out[i] == program[i]:
            if i == len(program) - 1:
                yield a
            else:
                yield from find_quines_rec(program, i + 1, a, low_bits + 3)

def find_quines(program):
    for a in range(2**10):
        if run((a, 0, 0), program)[0] == program[0]:
            yield from find_quines_rec(program, 1, a, 10)

def part2():
    _, program = read_input()
    print(min(find_quines(program)))

if __name__ == "__main__":
    part1()
    part2()
