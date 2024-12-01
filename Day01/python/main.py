def read_input_as_columns() -> (list[int], list[int]):
    with open("input.txt") as file:
        lines = file.read().rstrip().split('\n')
    def convert_line(line):
        [x, y] = line.split()
        return [int(x), int(y)]
    lines = map(convert_line, lines)
    return tuple(map(list, zip(*lines)))

def part1():
    (column0, column1) = read_input_as_columns()
    side_by_side = zip(sorted(column0), sorted(column1))
    total_distance = sum(abs(x - y) for (x, y) in side_by_side)
    print(total_distance)

def part2():
    (column0, column1) = read_input_as_columns()
    def similarity(x):
        return x*column1.count(x)
    total_similarity = sum(similarity(x) for x in column0)
    print(total_similarity)

if __name__ == '__main__':
    #part1()
    part2()
