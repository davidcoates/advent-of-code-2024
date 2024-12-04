use regex::Regex;
use std::io;

fn evaluate_mul(captures: regex::Captures) -> i32 {
    captures
        .iter()
        .skip(1)
        .map(|capture| capture.unwrap().as_str().parse::<i32>().unwrap())
        .product::<i32>()
}

fn part1(input: &str) {
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();
    let answer = re.captures_iter(input).map(evaluate_mul).sum::<i32>();
    println!("{}", answer);
}

fn part2(input: &str) {
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)|don't\(\)|do\(\)").unwrap();
    let mut answer: i32 = 0;
    let mut enabled = true;
    for captures in re.captures_iter(input) {
        match captures.get(0).unwrap().as_str() {
            "don't()" => {
                enabled = false;
            }
            "do()" => {
                enabled = true;
            }
            _ => {
                if enabled {
                    answer += evaluate_mul(captures);
                }
            }
        }
    }
    println!("{}", answer);
}

fn main() {
    let input = io::read_to_string(io::stdin()).unwrap();
    part1(&input);
    part2(&input);
}
