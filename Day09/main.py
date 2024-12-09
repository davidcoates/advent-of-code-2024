from dataclasses import dataclass


@dataclass
class File:
    id: int
    length: int

@dataclass
class FreeSpace:
    length: int

@dataclass
class Disk:
    items: list[File | FreeSpace]

    def files(self):
        return [ item for item in self.items if isinstance(item, File) ]

    def calculate_checksum(self) -> int:
        checksum = 0
        position = 0
        for item in self.items:
            if isinstance(file := item, File):
                for _ in range(file.length):
                    checksum += file.id * position
                    position += 1
            elif isinstance(free_space := item, FreeSpace):
                position += free_space.length
            else:
                assert False
        return checksum

    def move_file(self, file: File, min_chunk_length: int):

        assert file.length > 0
        file_index = self.items.index(file)
        finished = False

        for free_space_index, free_space in enumerate(self.items):

            if not isinstance(free_space, FreeSpace):
                continue

            # only move the file backwards
            if free_space_index >= file_index:
                break

            # check the free space is sufficiently big
            chunk_length = min(file.length, free_space.length)
            if not (chunk_length >= min_chunk_length):
                continue

            # cut chunk from file
            if file.length == chunk_length:
                chunk = file
                self.items[file_index] = FreeSpace(chunk_length)
                finished = True
            else:
                file.length -= chunk_length
                chunk = File(file.id, chunk_length)

            # paste chunk over free space
            if free_space.length == chunk_length:
                self.items[free_space_index] = chunk
            else:
                free_space.length -= chunk_length
                self.items.insert(free_space_index, chunk)
                # note: since free_space_index < file_index, we need to adjust file_index
                file_index += 1

            if finished:
                return


def read_input() -> Disk:
    with open("input.txt") as file:
        compressed_disk = list(map(int, file.read().rstrip('\n')))
    items = []
    i = 0
    while i < len(compressed_disk):
        id = i // 2
        items.append(File(id, compressed_disk[i]))
        i += 1
        if i < len(compressed_disk):
            items.append(FreeSpace(compressed_disk[i]))
            i += 1
    return Disk(items)

def part1():
    disk = read_input()
    for file in reversed(disk.files()):
        disk.move_file(file, min_chunk_length=1)
    print(disk.calculate_checksum())

def part2():
    disk = read_input()
    for file in reversed(disk.files()):
        disk.move_file(file, min_chunk_length=file.length)
    print(disk.calculate_checksum())

if __name__ == "__main__":
    part1()
    part2()
