import re
from dataclasses import dataclass
from collections import defaultdict


@dataclass
class Point:
    x: int
    y: int

WIDTH = 101
HEIGHT = 103

#WIDTH = 11
#HEIGHT = 7

def calculate_quadrant(position: Point):
    if position.x < WIDTH // 2 and position.y < HEIGHT // 2:
        return 1
    if position.x > WIDTH // 2 and position.y < HEIGHT // 2:
        return 2
    if position.x < WIDTH // 2 and position.y > HEIGHT // 2:
        return 3
    if position.x > WIDTH // 2 and position.y > HEIGHT // 2:
        return 4
    return None

@dataclass
class Robot:
    position: Point
    velocity: Point

    def step(self):
        self.position.x = (self.position.x + self.velocity.x) % WIDTH
        self.position.y = (self.position.y + self.velocity.y) % HEIGHT


PATTERN = re.compile(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)')

def read_input():
    def parse_line(line):
        (px, py, vx, vy) = map(int, PATTERN.match(line).groups())
        return Robot(Point(px, py), Point(vx, vy))
    with open("input.txt") as file:
        lines = [ parse_line(line.rstrip('\n')) for line in file.readlines() ]
    return lines

def part1():
    robots = read_input()
    for i in range(100):
        for robot in robots:
            robot.step()
    robots_by_quadrant = defaultdict(int)
    for robot in robots:
        quadrant = calculate_quadrant(robot.position)
        if quadrant is not None:
            robots_by_quadrant[quadrant] += 1
    answer = 1
    for value in robots_by_quadrant.values():
        answer *= value
    print(answer)

# check for horizontal symmetry
SYMMETRY_THRESHOLD = 100
def is_christmas_tree(robots):
    robots_by_position = defaultdict(int)
    for robot in robots:
        robots_by_position[(robot.position.x, robot.position.y)] += 1
    symmetry = 0
    for ((x, y), n) in robots_by_position.items():
        m = robots_by_position.get((WIDTH - 1 - x, y), 0)
        if (n > 0) == (m > 0):
            symmetry += 1
    if symmetry < SYMMETRY_THRESHOLD:
        return False
    lines = []
    for j in range(HEIGHT):
        line = []
        for i in range(WIDTH):
            if robots_by_position[(i, j)] == 0:
                line.append(' ')
            else:
                line.append(str(robots_by_position[(i, j)]))
        lines.append(''.join(line))
    print('\n'.join(lines))
    return True

def part2():
    robots = read_input()
    i = 0
    while not is_christmas_tree(robots):
        for robot in robots:
            robot.step()
        i += 1
    print(i)

if __name__ == "__main__":
    part1()
    part2()
