from enum import Enum, auto
from dataclasses import dataclass

@dataclass
class Vec2:
    x: int
    y: int

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)

    def __add__(self, other):
        return Vec2(self.x + other.x, self.y + other.y)


class Direction(Enum):
    UP = -1, 0
    RIGHT = 0, 1
    DOWN = 1, 0
    LEFT = 0, -1

    def __init__(self, dx, dy):
        self.delta = Vec2(dx, dy)

    def move(self, point: Vec2) -> Vec2:
        return point + self.delta

    def flip(self):
        return Direction(-self.delta.x, -self.delta.y)

    @staticmethod
    def from_symbol(symbol):
        match symbol:
            case '^': return Direction.UP
            case '>': return Direction.RIGHT
            case '<': return Direction.LEFT
            case 'v': return Direction.DOWN
            case _: assert False


class ObjectType(Enum):
    ROBOT = auto()
    BOX = auto()
    WALL = auto()

@dataclass
class Object:
    ty: ObjectType
    position: Vec2
    size: Vec2

    def contains(self, point):
        return self.position.x <= point.x < self.position.x + self.size.x and self.position.y <= point.y < self.position.y + self.size.y

    def overlaps(self, other):
        for dx in range(self.size.x):
            for dy in range(self.size.y):
                if other.contains(Vec2(self.position.x + dx, self.position.y + dy)):
                    return True
        return False

    def move(self, direction):
        self.position = direction.move(self.position)

    def move_copy(self, direction):
        return Object(self.ty, direction.move(self.position), self.size)

    def pushes(self, direction, other):
        if self == other:
            return False
        return self.move_copy(direction).overlaps(other)

    def points(self):
        for dx in range(self.size.x):
            for dy in range(self.size.y):
                yield self.position + Vec2(dx, dy)

    def __eq__(self, other):
        return self.position == other.position

    def __hash__(self):
        return hash(self.position)

class Warehouse:

    def __init__(self, objects):
        self._objects = objects
        self._objects_by_position = dict()
        for object in self._objects:
            for point in object.points():
                self._objects_by_position[point] = object
        self._robot = next(object for object in self._objects if object.ty == ObjectType.ROBOT)

    def _move(self, objects, direction):
        for object in objects:
            for point in object.points():
                del self._objects_by_position[point]
        for object in objects:
            object.move(direction)
        for object in objects:
            for point in object.points():
                assert point not in self._objects_by_position
                self._objects_by_position[point] = object

    def find(self, position):
        return self._objects_by_position.get(position)

    def __iter__(self):
        return iter(self._objects)

    def boxes(self):
        for object in self._objects:
            if object.ty == ObjectType.BOX:
                yield object

    def calculate_gps(self):
        gps = 0
        for box in self.boxes():
            gps += 100*box.position.x + box.position.y
        return gps

    def try_move(self, direction):
        objects = [ self._robot ]
        seen = { self._robot }
        while objects:
            object = objects.pop()
            for point in object.points():
                if (pushed := self.find(direction.move(point))) is not None and pushed != object:
                    match pushed.ty:
                        case ObjectType.BOX:
                            if pushed not in seen:
                                seen.add(pushed)
                                objects.append(pushed)
                        case ObjectType.WALL:
                            return
                        case _:
                            assert False
        self._move(seen, direction)


def read_input():
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
    i = lines.index("")
    grid = list(map(list, lines[:i]))
    directions = list(map(Direction.from_symbol, [ char for line in lines[i+1:] for char in line ]))
    return grid, directions

def part1():
    grid, directions = read_input()
    r = len(grid)
    c = len(grid[0])

    objects = []
    for i in range(r):
        for j in range(c):
            position = Vec2(i, j)
            match grid[i][j]:
                case '@':
                    object = Object(ObjectType.ROBOT, position, size = Vec2(1, 1))
                case '#':
                    object = Object(ObjectType.WALL, position, size = Vec2(1, 1))
                case 'O':
                    object = Object(ObjectType.BOX, position, size = Vec2(1, 1))
                case _:
                    continue
            objects.append(object)

    warehouse = Warehouse(objects)

    for direction in directions:
        warehouse.try_move(direction)

    print(warehouse.calculate_gps())


def part2():
    grid, directions = read_input()
    r = len(grid)
    c = len(grid[0])

    objects = []
    for i in range(r):
        for j in range(c):
            position = Vec2(i, 2*j)
            match grid[i][j]:
                case '@':
                    object = Object(ObjectType.ROBOT, position, size = Vec2(1, 1))
                case '#':
                    object = Object(ObjectType.WALL, position, size = Vec2(1, 2))
                case 'O':
                    object = Object(ObjectType.BOX, position, size = Vec2(1, 2))
                case _:
                    continue
            objects.append(object)

    warehouse = Warehouse(objects)

    for direction in directions:
        warehouse.try_move(direction)

    print(warehouse.calculate_gps())


if __name__ == "__main__":
    part1()
    part2()
