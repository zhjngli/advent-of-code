use std::fs;

fn parse() -> (Vec<i64>, Vec<i64>) {
    let mut ls = Vec::new();
    let mut rs = Vec::new();
    let result = fs::read_to_string("data/01.txt");
    result
        .unwrap_or_else(|error| {
            eprintln!("Error: {}", error);
            "1 1\n".to_string()
        })
        .lines()
        .for_each(|line| {
            let nums: Vec<i64> = line
                .split_whitespace()
                .filter_map(|num| num.parse::<i64>().ok())
                .collect();
            if nums.len() == 2 {
                ls.push(nums[0]);
                rs.push(nums[1]);
            } else {
                eprintln!("Skipping line: {}", line);
            }
        });
    (ls, rs)
}

fn solve1() -> i64 {
    let (mut ls, mut rs) = parse();
    ls.sort();
    rs.sort();

    ls.into_iter()
        .zip(rs)
        .map(|(l, r)| l.abs_diff(r))
        .sum::<u64>() as i64
}

fn solve2() -> i64 {
    let (ls, rs) = parse();
    ls.into_iter()
        .map(|l| rs.iter().filter(|&&r| r == l).count() as i64 * l)
        .sum()
}

pub fn solve() {
    println!("2024.01.1: {}", solve1());
    println!("2024.01.2: {}", solve2());
}
