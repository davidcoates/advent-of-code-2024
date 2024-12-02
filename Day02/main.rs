use std::fs;

fn read_input_as_rows() -> Vec<Vec<i32>> {
    fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(|line| {
            line.split(" ")
                .map(|word| word.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

fn is_safe_inc<'a, I>(mut report: I) -> bool
where
    I: Iterator<Item = &'a i32>,
{
    let x = report.next();
    if x.is_none() {
        return true;
    }
    let mut x = x.unwrap();
    for y in report {
        if !(1 <= y - x && y - x <= 3) {
            return false;
        }
        x = y;
    }
    return true;
}

fn is_safe<'a, I>(report: I) -> bool
where
    I: DoubleEndedIterator<Item = &'a i32> + Clone,
{
    is_safe_inc(report.clone()) || is_safe_inc(report.rev())
}

fn part1() {
    let reports = read_input_as_rows();
    let num_safe_reports = reports
        .iter()
        .filter(|&report| is_safe(report.iter()))
        .count();
    println!("{}", num_safe_reports);
}

fn is_safe_excluding_one(report: &Vec<i32>) -> bool {
    for i in 0..report.len() {
        let report_excluding_i = report
            .iter()
            .enumerate()
            .filter(|(j, _)| i != *j)
            .map(|(_, x)| x);
        if is_safe(report_excluding_i) {
            return true;
        }
    }
    return false;
}

fn part2() {
    let reports = read_input_as_rows();
    let num_safe_reports = reports
        .iter()
        .filter(|&report| is_safe(report.iter()) || is_safe_excluding_one(report))
        .count();
    println!("{}", num_safe_reports);
}

fn main() {
    part1();
    part2();
}
