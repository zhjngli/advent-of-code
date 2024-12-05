use std::fs;

fn parse(s: &str) -> (Vec<i64>, Vec<i64>) {
    s.lines()
        .map(|line| {
            let mut nums = line
                .split_whitespace()
                .filter_map(|num| num.parse::<i64>().ok());
            (nums.next().unwrap(), nums.next().unwrap())
        })
        .unzip()
}

fn solve1(s: &str) -> i64 {
    let (mut ls, mut rs) = parse(s);
    ls.sort();
    rs.sort();

    ls.into_iter()
        .zip(rs)
        .map(|(l, r)| l.abs_diff(r))
        .sum::<u64>() as i64
}

fn solve2(s: &str) -> i64 {
    let (ls, rs) = parse(s);
    ls.into_iter()
        .map(|l| rs.iter().filter(|&&r| r == l).count() as i64 * l)
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/01.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("Error: {}", error);
        "1 1\n".to_string()
    });
    let input = binding.as_str();

    println!("2024.01.1: {}", solve1(input));
    println!("2024.01.2: {}", solve2(input));
}
