def read_reports() -> list[list[int]]:
    with open("input.txt") as file:
        lines = file.read().rstrip().split('\n')
    def convert_line(line):
        return list(map(int, line.split()))
    reports = list(map(convert_line, lines))
    return reports

def is_safe_increasing(report: list[int]) -> bool:
    if len(report) <= 1:
        return True
    for i in range(len(report) - 1):
        if not (report[i] + 1 <= report[i+1] <= report[i] + 3):
            return False
    return True

def part1():
    reports = read_reports()
    def is_safe(report: list[int]) -> bool:
        return is_safe_increasing(report) or is_safe_increasing(report[::-1])
    num_safe_reports = len([report for report in reports if is_safe(report)])
    print(num_safe_reports)

def part2():
    reports = read_reports()
    def tolerant_is_safe_increasing(report: list[int]) -> bool:
        if is_safe_increasing(report):
            return True
        else:
            for i in range(len(report)):
                if is_safe_increasing(report[:i] + report[i+1:]):
                    return True
            return False
    def tolerant_is_safe(report: list[int]) -> bool:
        return tolerant_is_safe_increasing(report) or tolerant_is_safe_increasing(report[::-1])
    num_safe_reports = len([report for report in reports if tolerant_is_safe(report)])
    print(num_safe_reports)    
    
if __name__ == '__main__':
    part1()
    part2()
