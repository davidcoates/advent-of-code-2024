
# quick and dirty

def read_input() -> (list[(int, int)], list[list[int]]):
    with open("input.txt") as file:
        lines = [ line.rstrip('\n') for line in file.readlines() ]
        i = lines.index('')
        rules = lines[:i]
        rules = [ tuple(map(int, rule.split('|'))) for rule in rules ]
        pages_list = lines[i+1:]
        pages_list = [ list(map(int, pages.split(','))) for pages in pages_list ]
    return (rules, pages_list)

def find_violating_rules(rules, pages):
    n = len(pages)
    return ( rule for i in range(n) for j in range(i + 1, n) if (rule := (pages[j], pages[i])) in rules )
    
def is_correctly_ordered(rules, pages) -> bool:
    return not any(find_violating_rules(rules, pages))

def part1():
    (rules, pages_list) = read_input()
    answer = 0
    for pages in pages_list:
        if is_correctly_ordered(rules, pages):
            answer += pages[len(pages)//2]
    print(answer)
        
def part2():
    (rules, pages_list) = read_input()
    answer = 0
    for pages in pages_list:
        if is_correctly_ordered(rules, pages):
            continue
        n = len(pages)    
        while not is_correctly_ordered(rules, pages):
            rule = next(find_violating_rules(rules, pages))
            i, j = pages.index(rule[0]), pages.index(rule[1])
            pages[i], pages[j] = pages[j], pages[i]
        answer += pages[len(pages)//2]
    print(answer)

if __name__ == '__main__':
    part1()
    part2()
