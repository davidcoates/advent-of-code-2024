from collections import defaultdict

def read_input():
    with open("input.txt") as file:
        line = file.read().rstrip('\n')
    return list(map(int, line.split(' ')))

def blink(stone: int) -> list[int]:
    if stone == 0:
        return [1]
    elif (n := (len(stone_str := str(stone)))) % 2 == 0:
        return [ int(stone_str[:n//2]), int(stone_str[n//2:]) ]
    else:
        return [ stone * 2024 ]

def count_stones(blink_times: int) -> int:
    stones = read_input()
    stone_counts = { stone: 1 for stone in stones }
    for _ in range(blink_times):
        new_stone_counts = defaultdict(int)
        for stone, count in stone_counts.items():
            for new_stone in blink(stone):
                new_stone_counts[new_stone] += count
        stone_counts = new_stone_counts
    return sum(stone_counts.values())

def part1():
    print(count_stones(25))

def part2():
    print(count_stones(75))

if __name__ == "__main__":
    part1()
    part2()
