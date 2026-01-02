use iter_first_max::IterFirstMaxExt;
use std::fs;

fn parse(s: &str) -> Vec<Vec<i8>> {
    s.lines()
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as i8)
                .collect::<Vec<i8>>()
        })
        .collect::<Vec<Vec<i8>>>()
}

fn solve1(s: &Vec<Vec<i8>>) -> i64 {
    _solve(s, 2)
}

fn solve2(s: &Vec<Vec<i8>>) -> i64 {
    _solve(s, 12)
}

fn _solve(s: &Vec<Vec<i8>>, digits: usize) -> i64 {
    s.iter()
        .map(|row| {
            let mut joltages: Vec<i8> = Vec::new();
            let mut last_index = 0;
            for i in (0..digits).rev() {
                let (ji, j) = row[last_index..row.len() - i]
                    .iter()
                    .enumerate()
                    .first_max_by_key(|&(_, val)| val)
                    .unwrap();
                joltages.push(*j);
                last_index += ji + 1;
            }
            joltages.iter().fold(0i64, |acc, &j| acc * 10 + j as i64)
        })
        .sum::<i64>()
}

pub fn solve() {
    let result = fs::read_to_string("data/03.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let input = parse(binding.as_str());

    println!("2025.03.1: {}", solve1(&input));
    println!("2025.03.2: {}", solve2(&input));
}
