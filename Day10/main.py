def read_input():
    with open("input.txt") as file:
        grid = [ list(map(int, line.rstrip('\n'))) for line in file.readlines() ]
    return grid

def find_trails(grid):
    r = len(grid)
    c = len(grid[0])
    trails = [ [ set() for _ in range(c) ] for _ in range(r) ]
    for i in range(r):
        for j in range(c):
            if grid[i][j] == 9:
                trails[i][j].add(((i, j),))
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

def calculate_trail_score(trails_score):
    grid = read_input()
    trails = find_trails(grid)
    r = len(grid)
    c = len(grid[0])
    answer = 0
    for i in range(r):
        for j in range(c):
            if grid[i][j] == 0:
                answer += trails_score(trails[i][j])
    return answer

def part1():
    print(calculate_trail_score(lambda trails: len({trail[-1] for trail in trails})))

def part2():
    print(calculate_trail_score(lambda trails: len(trails)))

if __name__ == "__main__":
    part1()
    part2()
