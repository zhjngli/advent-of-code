use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn parse(s: &str) -> Vec<&str> {
    s.lines().collect()
}

fn solve1(manifold: &Vec<&str>) -> usize {
    let mut beams = HashSet::new();
    beams.insert(manifold[0].find('S').unwrap());
    manifold
        .iter()
        .fold((0, beams), |(mut splits, beams), row| {
            let mut new_beams = HashSet::new();
            for &b in beams.iter() {
                match row.chars().nth(b).unwrap() {
                    '.' | 'S' => {
                        new_beams.insert(b);
                    }
                    '^' => {
                        new_beams.insert(b - 1);
                        new_beams.insert(b + 1);
                        splits += 1;
                    }
                    c => panic!("unknown char {} at beam {}", c, b),
                }
            }
            (splits, new_beams)
        })
        .0
}

fn solve2(manifold: &Vec<&str>) -> usize {
    let mut timelines = HashMap::new();
    timelines.insert(manifold[0].find('S').unwrap(), 1);
    manifold
        .iter()
        .fold(timelines, |timelines, row| {
            let mut new_timelines = HashMap::new();
            for (&b, &count) in timelines.iter() {
                match row.chars().nth(b).unwrap() {
                    '.' | 'S' => {
                        new_timelines
                            .entry(b)
                            .and_modify(|e| *e += count)
                            .or_insert(count);
                    }
                    '^' => {
                        new_timelines
                            .entry(b - 1)
                            .and_modify(|e| *e += count)
                            .or_insert(count);
                        new_timelines
                            .entry(b + 1)
                            .and_modify(|e| *e += count)
                            .or_insert(count);
                    }
                    c => panic!("unknown char {} at beam {}", c, b),
                }
            }
            new_timelines
        })
        .values()
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/07.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let manifold = parse(binding.as_str());

    println!("2025.07.1: {}", solve1(&manifold));
    println!("2025.07.2: {}", solve2(&manifold));
}
