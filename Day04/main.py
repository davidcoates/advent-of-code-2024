def read_lines():
    with open("input.txt") as file:
        input = file.read()
    return input.rstrip('\n').split('\n')

def transpose(lines):
    return [ ''.join(line) for line in zip(*lines) ]

def rotate(lines):
    return [ ''.join(line) for line in zip(*lines[::-1]) ]

def lower_diagonals(lines):
    n = len(lines)
    return [ ''.join([ lines[i][j] for (i, j) in zip(range(n - 1 - x, n), range(0, n)) ]) for x in range(0, n -1) ]

def upper_diagonals(lines):
    return lower_diagonals(transpose(lines))

def diagonals(lines):
    n = len(lines)
    return [ ''.join([lines[i][i] for i in range(n)]) ] + lower_diagonals(lines) + upper_diagonals(lines)

def symmetries(lines):
    line_symmetries = [ lines, transpose(lines), diagonals(lines), diagonals(rotate(lines)) ]
    return [ line for lines in line_symmetries for line in lines ] + [ line[::-1] for lines in line_symmetries for line in lines ]

def part1():
    lines = read_lines()
    answer = sum(line.count("XMAS") for line in symmetries(lines))
    print(answer)

def part2():
    lines = read_lines()
    n = len(lines)
    answer = 0
    for i in range(n - 2):
        for j in range(n - 2):
            if lines[i + 1][j + 1] != 'A':
                continue
            if { lines[i][j], lines[i + 2][j + 2] } != { 'M', 'S' }:
                continue
            if { lines[i + 2][j], lines[i][j + 2] } != { 'M', 'S' }:
                continue
            answer += 1
    print(answer)

if __name__ == '__main__':
    part1()
    part2()
