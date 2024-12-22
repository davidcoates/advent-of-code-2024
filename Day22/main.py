import itertools
import sys

sys.setrecursionlimit(10000)

def read_input():
    def parse_line(line):
        return tuple(map(int, line.split(',')))
    with open("input.txt") as file:
        lines = [ int(line.rstrip('\n')) for line in file.readlines() ]
    return lines

def evolve(x):
    x = ((x * 64) ^ x) % 16777216
    x = ((x // 32) ^ x) % 16777216
    x = ((x * 2048) ^ x) % 16777216
    return x

def evolve_repeated(x, n):
    return x if n == 0 else evolve_repeated(evolve(x), n - 1)

def part1():
    numbers = read_input()
    print(sum(evolve_repeated(number, 2000) for number in numbers))

def calculate_prices(x, n):
    prices = [x % 10]
    for _ in range(n):
        x = evolve(x)
        prices.append(x % 10)
    return prices

type Changes = tuple[int, int, int, int]

def calculate_payoff_table(prices) -> dict[Changes, int]:
    payoff = {}
    changes = [ prices[i+1] - prices[i] for i in range(len(prices) - 1) ]
    for i in range(len(changes) - 3):
        change = tuple(changes[i:i+4])
        if change not in payoff:
            payoff[change] = prices[i+4]
    return payoff

def part2():
    numbers = read_input()
    prices_by_buyer = [ calculate_prices(number, 2000) for number in numbers ]
    payoff_by_buyer = {  buyer : calculate_payoff_table(prices_by_buyer[buyer]) for buyer in range(len(prices_by_buyer)) }
    def calculate_payoff(changes):
        return sum(payoff_by_buyer[buyer].get(changes, 0) for buyer in range(len(prices_by_buyer)))
    print(max(calculate_payoff(changes) for changes in itertools.product(range(-9, 10), repeat=4)))

if __name__ == "__main__":
    part1()
    part2()
