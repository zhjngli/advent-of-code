use std::{
    collections::{HashSet, VecDeque},
    fs,
};

fn possible_values(
    vals: &mut VecDeque<u64>,
    possibilities: HashSet<u64>,
    new_values: fn(&u64, &u64) -> Vec<u64>,
) -> HashSet<u64> {
    if vals.len() == 0 {
        return possibilities;
    }
    let v = vals.pop_front().unwrap();
    if possibilities.len() == 0 {
        return possible_values(vals, HashSet::from([v]), new_values);
    }
    possible_values(
        vals,
        possibilities
            .iter()
            .flat_map(|p| new_values(p, &v))
            .collect(),
        new_values,
    )
}

fn _solve(calibrations: &Vec<(u64, VecDeque<u64>)>, new_values: fn(&u64, &u64) -> Vec<u64>) -> u64 {
    calibrations
        .iter()
        .filter_map(|(c, vs)| {
            let possibilities = HashSet::new();
            if possible_values(&mut vs.clone(), possibilities, new_values).contains(c) {
                Some(c)
            } else {
                None
            }
        })
        .sum()
}

fn create_new_values1(p: &u64, v: &u64) -> Vec<u64> {
    vec![p + v, p * v]
}

fn solve1(calibrations: &Vec<(u64, VecDeque<u64>)>) -> u64 {
    _solve(calibrations, create_new_values1)
}

fn create_new_values2(p: &u64, v: &u64) -> Vec<u64> {
    let concatenated = format!("{}{}", p, v);
    vec![p + v, p * v, concatenated.parse::<u64>().unwrap()]
}

fn solve2(calibrations: &Vec<(u64, VecDeque<u64>)>) -> u64 {
    _solve(calibrations, create_new_values2)
}

pub fn solve() {
    let result = fs::read_to_string("data/07.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let calibrations: Vec<(u64, VecDeque<u64>)> = binding
        .as_str()
        .lines()
        .map(|l| {
            let (val, equation) = l.split_once(':').unwrap_or(("1", "1"));
            (
                val.parse::<u64>().unwrap(),
                equation
                    .split_ascii_whitespace()
                    .map(|v| v.parse::<u64>().unwrap())
                    .collect(),
            )
        })
        .collect();

    println!("2024.07.1: {}", solve1(&calibrations));
    println!("2024.07.2: {}", solve2(&calibrations));
}
