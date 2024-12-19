import functools

def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    i = lines.index('')
    patterns = lines[i-1].split(', ')
    designs = lines[i+1:]
    return patterns, designs

def memoize(fn):
    cache = {}
    @functools.wraps(fn)
    def wrapper(patterns, design):
        if design not in cache:
            cache[design] = fn(patterns, design)
        return cache[design]
    return wrapper

@memoize
def ways_possible(patterns, design):
    if len(design) == 0:
        return 1
    return sum(ways_possible(patterns, design[len(pattern):]) for pattern in patterns if design.startswith(pattern))

def part1():
    patterns, designs = read_input()
    answer = 0
    for design in designs:
        if ways_possible(patterns, design) > 0:
            answer += 1
    print(answer)

def part2():
    patterns, designs = read_input()
    print(sum(ways_possible(patterns, design) for design in designs))

if __name__ == "__main__":
    part1()
    part2()
