
def read_input():

    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]

    keys = []
    locks = []

    def parse_block(lines):
        pins = [ column.count('#') - 1 for column in zip(*lines) ]
        if lines[0][0] == '#':
            locks.append(pins)
        else:
            keys.append(pins)

    block_length = lines.index('')
    while lines:
        parse_block(lines[:block_length])
        lines = lines[block_length+1:]

    return locks, keys


def key_fits_lock(key, lock):
    return all( x + y < 6 for (x, y) in zip(key, lock) )

def part1():
    locks, keys = read_input()
    print(sum(1 for lock in locks for key in keys if key_fits_lock(key, lock)))

if __name__ == "__main__":
    part1()
