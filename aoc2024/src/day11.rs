use cached::proc_macro::cached;
use std::fs;

fn blink_stone_once(n: u64) -> Vec<u64> {
    match n {
        0 => vec![1],
        _ if n.to_string().len() % 2 == 0 => {
            let s = n.to_string();
            let mid = s.len() / 2;
            let (l, r) = s.split_at(mid);
            vec![l.parse::<u64>().unwrap(), r.parse::<u64>().unwrap()]
        }
        _ => vec![n * 2024],
    }
}

#[cached]
fn blink_stone(n: u64, blinks: usize) -> usize {
    match blinks {
        0 => 1,
        _ => blink_stone_once(n)
            .iter()
            .map(|stone| blink_stone(*stone, blinks - 1))
            .sum(),
    }
}

fn _solve(stones: &Vec<u64>, blinks: usize) -> usize {
    stones.iter().map(|s| blink_stone(*s, blinks)).sum()
}

fn solve1(stones: &Vec<u64>) -> usize {
    _solve(stones, 25)
}

fn solve2(stones: &Vec<u64>) -> usize {
    _solve(stones, 75)
}

pub fn solve() {
    let result = fs::read_to_string("data/11.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let stones = binding
        .as_str()
        .lines()
        .flat_map(|l| l.split_whitespace().map(|n| n.parse::<u64>().unwrap()))
        .collect();

    println!("2024.11.1: {}", solve1(&stones));
    println!("2024.11.2: {}", solve2(&stones));
}
