use std::collections::HashMap;
use std::fs;
use std::iter::zip;

fn read_input_as_columns() -> (Vec<i32>, Vec<i32>) {
    let mut column_left = Vec::new();
    let mut column_right = Vec::new();
    let contents = fs::read_to_string("input.txt").unwrap();
    for line in contents.lines() {
        let mut words = line.split("   ");
        let left = words.next().unwrap().parse::<i32>().unwrap();
        column_left.push(left);
        let right = words.next().unwrap().parse::<i32>().unwrap();
        column_right.push(right);
    }
    return (column_left, column_right);
}

fn part1() {
    let (mut column_left, mut column_right) = read_input_as_columns();
    column_left.sort();
    column_right.sort();
    let total_distance: i32 = zip(column_left, column_right)
        .map(|(x, y)| (x - y).abs())
        .sum();
    println!("{}", total_distance);
}

fn part2() {
    let (column_left, column_right) = read_input_as_columns();
    let column_right_freq: HashMap<i32, i32> =
        column_right
            .into_iter()
            .fold(HashMap::new(), |mut map, number| {
                *map.entry(number).or_default() += 1;
                map
            });
    let total_similarity: i32 = column_left
        .iter()
        .map(|x| x * column_right_freq.get(x).cloned().unwrap_or_default())
        .sum();
    println!("{}", total_similarity);
}

fn main() {
    part1();
    part2();
}
