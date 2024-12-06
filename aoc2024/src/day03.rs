use nom::{
    branch::alt,
    bytes::complete::{take_until, take_while},
    IResult,
};
use regex::Regex;
use std::fs;

fn solve1(s: &str) -> i64 {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    re.captures_iter(s)
        .map(|r| {
            let n1 = r[1].parse::<i64>().unwrap();
            let n2 = r[2].parse::<i64>().unwrap();
            n1 * n2
        })
        .sum()
}

// take_while(|_| true) is kind of a hacky way to parse until eof
// couldn't get things like take_until("") or eof() to work as expected
fn parse_do(s: &str) -> IResult<&str, &str> {
    alt((take_until("don't()"), take_while(|_| true)))(s)
}

fn parse_dont(s: &str) -> IResult<&str, &str> {
    alt((take_until("do()"), take_while(|_| true)))(s)
}

fn solve2(s: &str) -> i64 {
    let mut sum = 0;
    let (mut remaining, mut parsed) = parse_do(s).unwrap();
    sum += solve1(parsed);
    while !remaining.is_empty() {
        (remaining, _) = parse_dont(remaining).unwrap();
        (remaining, parsed) = parse_do(remaining).unwrap();
        sum += solve1(parsed);
    }
    sum
}

pub fn solve() {
    let result = fs::read_to_string("data/03.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "mul(1,1)\n".to_string()
    });
    let input = binding.as_str();
    println!("2024.03.1: {}", solve1(input));
    println!("2024.03.2: {}", solve2(input));
}
