use std::{collections::HashSet, fs};

fn parse(s: &str) -> Vec<Vec<char>> {
    s.lines().map(|line| line.chars().collect()).collect()
}

fn solve1(s: &Vec<Vec<char>>) -> usize {
    let rows = s.len();
    let cols = s[0].len();
    s.iter()
        .enumerate()
        .map(|(r, row)| {
            row.iter()
                .enumerate()
                .map(|(c, ch)| {
                    if ch != &'@' {
                        0
                    } else {
                        let adjacent_rolls = [
                            (-1, -1),
                            (-1, 0),
                            (-1, 1),
                            (0, -1),
                            (0, 1),
                            (1, -1),
                            (1, 0),
                            (1, 1),
                        ]
                        .iter()
                        .filter_map(|(dr, dc)| {
                            let new_r = r as isize + dr;
                            let new_c = c as isize + dc;
                            if new_r >= 0
                                && new_r < rows as isize
                                && new_c >= 0
                                && new_c < cols as isize
                                && s[new_r as usize][new_c as usize] == '@'
                            {
                                Some(s[new_r as usize][new_c as usize])
                            } else {
                                None
                            }
                        })
                        .count();
                        if adjacent_rolls < 4 { 1 } else { 0 }
                    }
                })
                .sum::<usize>()
        })
        .sum::<usize>()
}

fn solve2(s: &Vec<Vec<char>>) -> usize {
    let rows = s.len();
    let cols = s[0].len();
    let mut new_rolls = true;
    let mut moved_rolls = HashSet::new();
    while new_rolls {
        new_rolls = false;
        for (r, row) in s.iter().enumerate() {
            for (c, ch) in row.iter().enumerate() {
                if ch != &'@' || moved_rolls.get(&(r, c)).is_some() {
                    continue;
                }
                let adjacent_rolls = [
                    (-1, -1),
                    (-1, 0),
                    (-1, 1),
                    (0, -1),
                    (0, 1),
                    (1, -1),
                    (1, 0),
                    (1, 1),
                ]
                .iter()
                .filter_map(|(dr, dc)| {
                    let new_r = r as isize + dr;
                    let new_c = c as isize + dc;
                    if new_r >= 0
                        && new_r < rows as isize
                        && new_c >= 0
                        && new_c < cols as isize
                        && moved_rolls.get(&(new_r as usize, new_c as usize)).is_none()
                        && s[new_r as usize][new_c as usize] == '@'
                    {
                        Some((new_r, new_c))
                    } else {
                        None
                    }
                })
                .collect::<Vec<(isize, isize)>>();
                if adjacent_rolls.len() < 4 {
                    moved_rolls.insert((r, c));
                    new_rolls = true;
                }
            }
        }
    }
    moved_rolls.len()
}

pub fn solve() {
    let result = fs::read_to_string("data/04.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let input = parse(binding.as_str());

    println!("2025.04.1: {}", solve1(&input));
    println!("2025.04.2: {}", solve2(&input));
}
