use std::fs;

fn parse(s: &str) -> Vec<i64> {
    s.lines()
        .map(|line| {
            let (dir, dist) = line.split_at(1);
            match dir {
                "L" => -dist.parse::<i64>().unwrap(),
                "R" => dist.parse::<i64>().unwrap(),
                _ => panic!("Invalid direction: {}", dir),
            }
        })
        .collect()
}

fn solve1(s: &str) -> i64 {
    let dirs = parse(s);
    let mut pos = 50;
    let mut zeros = 0;
    for dir in dirs {
        pos += dir;
        if pos < 0 || pos > 99 {
            let r = pos % 100;
            pos = if r < 0 { r + 100 } else { r };
        }
        if pos == 0 {
            zeros += 1;
        }
    }
    zeros
}

fn solve2(s: &str) -> i64 {
    let dirs = parse(s);
    let mut pos = 50;
    let mut zeros = 0;
    for dir in dirs {
        let new_pos = pos + dir;
        zeros += (new_pos / 100).abs();
        if new_pos <= 0 && pos != 0 {
            zeros += 1;
        }
        pos = new_pos;
        if pos < 0 || pos > 99 {
            let r = pos % 100;
            pos = if r < 0 { r + 100 } else { r };
        }
    }
    zeros
}

pub fn solve() {
    let result = fs::read_to_string("data/01.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let input = binding.as_str();

    println!("2025.01.1: {}", solve1(input));
    println!("2025.01.2: {}", solve2(input));
}
