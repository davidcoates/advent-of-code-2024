import re

def read_input():
    with open("input.txt") as file:
        return file.read()

def part1():
    input = read_input()
    PATTERN = re.compile(r'mul\((\d{1,3}),(\d{1,3})\)')
    answer = sum(int(match[0]) * int(match[1]) for match in PATTERN.findall(input))
    print(answer)

def part2():
    input = read_input()
    enabled = True
    answer = 0
    PATTERN = re.compile(r"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)")
    while match := PATTERN.search(input):
        match match.group(0):
            case "do()":
                enabled = True
            case "don't()":
                enabled = False
            case _:
                if enabled:
                 answer += int(match.group(1)) * int(match.group(2))
        input = input[match.end():]
    print(answer)

if __name__ == '__main__':
    part1()
    part2()
