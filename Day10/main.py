def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    grid = [ list(map(int, line)) for line in lines ]
    return grid

def find_trails(grid):
    """ Returns a new grid where the value at (i, j) is the set of distinct trails starting from (i, j) and ending in a 9. """
    r = len(grid)
    c = len(grid[0])
    trails = [ [ set() for _ in range(c) ] for _ in range(r) ]
    # base case:
    # every tile of height 9 has exactly one trail to a tile of height 9 (itself)
    for i in range(r):
        for j in range(c):
            if grid[i][j] == 9:
                trails[i][j].add(((i, j),))
    # recursive case:
    # the trails from a tile of height h are those through adjacent tiles of height h+1.
    for height in reversed(range(0, 9)):
        for i in range(r):
            for j in range(c):
                if grid[i][j] != height:
                    continue
                for (dx, dy) in [ (-1, 0), (1, 0), (0, 1), (0, -1) ]:
                    (x, y) = (i + dx, j + dy)
                    if 0 <= x < r and 0 <= y < c and grid[x][y] == height + 1:
                        trails[i][j].update(((i, j),) + trail for trail in trails[x][y])
    return trails

def calculate_total_trails_score(calculate_trails_score):
    grid = read_input()
    trails = find_trails(grid)
    r = len(grid)
    c = len(grid[0])
    return sum(calculate_trails_score(trails[i][j]) for i in range(r) for j in range(c) if grid[i][j] == 0)

def part1():
    def number_of_distinct_endpoints(trails):
        return len({ trail[-1] for trail in trails })
    print(calculate_total_trails_score(number_of_distinct_endpoints))

def part2():
    def number_of_distinct_trails(trails):
        return len(trails)
    print(calculate_total_trails_score(number_of_distinct_trails))

if __name__ == "__main__":
    part1()
    part2()
