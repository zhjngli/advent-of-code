use std::{collections::HashSet, fs};

fn parse(s: &str) -> Vec<(&str, &str)> {
    s.split(',')
        .map(|range| {
            let dash_idx = range.find('-').unwrap();
            (&range[..dash_idx], &range[dash_idx + 1..])
        })
        .collect()
}

fn solve1(s: &str) -> i64 {
    let ranges = parse(s);
    ranges
        .iter()
        .map(|(l, r)| {
            let lower = l.parse::<i64>().unwrap();
            let upper = r.parse::<i64>().unwrap();
            if r.len() - l.len() > 1 {
                // need to handle extra large range
                panic!("Extra large range: {} - {}", l, r);
            }
            let mut dup = if l.len() == 1 {
                1
            } else {
                l.split_at(l.len() / 2).0.parse::<i64>().unwrap()
            };
            let mut invalid_ids = vec![];
            let mut current = format!("{}{}", dup, dup).parse::<i64>().unwrap();
            while current <= upper {
                if current >= lower && current <= upper {
                    invalid_ids.push(current);
                }
                dup += 1;
                current = format!("{}{}", dup, dup).parse::<i64>().unwrap();
            }
            invalid_ids.iter().sum::<i64>()
        })
        .sum()
}

fn solve2(s: &str) -> i64 {
    let ranges = parse(s);
    ranges
        .iter()
        .map(|(l, r)| {
            let lower = l.parse::<i64>().unwrap();
            let upper = r.parse::<i64>().unwrap();
            let mut invalid_ids = HashSet::new();

            for s in 2..=r.len() {
                let mut dup = format!(
                    "1{}",
                    "0".repeat(if l.len() / s == 0 { 0 } else { l.len() / s - 1 })
                )
                .parse::<i64>()
                .unwrap();
                let mut current = dup.to_string().repeat(s).parse::<i64>().unwrap();
                while current <= upper {
                    if current >= lower && current <= upper {
                        invalid_ids.insert(current);
                    }
                    dup += 1;
                    current = dup.to_string().repeat(s).parse::<i64>().unwrap();
                }
            }
            invalid_ids.iter().sum::<i64>()
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/02.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let input = binding.as_str();

    println!("2025.02.1: {}", solve1(input));
    println!("2025.02.2: {}", solve2(input));
}
