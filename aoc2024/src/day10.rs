use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn trailheads(map: &HashMap<(i64, i64), u8>) -> Vec<(i64, i64)> {
    map.iter()
        .filter(|(_, d)| **d == 0)
        .map(|(p, _)| *p)
        .collect()
}

fn solve1(map: &HashMap<(i64, i64), u8>) -> usize {
    let trailheads = trailheads(map);
    let dirs: Vec<(i64, i64)> = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    trailheads
        .iter()
        .map(|(r, c)| {
            let mut seen = HashSet::new();
            let mut boundary = vec![(*r, *c)];
            while boundary.len() > 0 {
                let (nr, nc) = boundary.pop().unwrap();
                let nh = map.get(&(nr, nc)).unwrap();
                dirs.iter().for_each(|(dr, dc)| {
                    let neighbor = map.get(&(nr + dr, nc + dc));
                    match neighbor {
                        Some(height) => {
                            if nh + 1 == *height {
                                boundary.push((nr + dr, nc + dc));
                            }
                        }
                        None => (),
                    }
                });
                seen.insert((nr, nc));
            }
            seen.iter()
                .filter(|(r, c)| {
                    let h = map.get(&(*r, *c));
                    match h {
                        Some(9) => true,
                        _ => false,
                    }
                })
                .count()
        })
        .sum()
}

fn solve2(map: &HashMap<(i64, i64), u8>) -> usize {
    let trailheads = trailheads(map);
    let dirs: Vec<(i64, i64)> = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut seen = HashMap::new();
    trailheads.iter().for_each(|(r, c)| {
        let mut boundary = vec![(*r, *c)];
        while boundary.len() > 0 {
            let (nr, nc) = boundary.pop().unwrap();
            let nh = map.get(&(nr, nc)).unwrap();
            dirs.iter().for_each(|(dr, dc)| {
                let neighbor = map.get(&(nr + dr, nc + dc));
                match neighbor {
                    Some(height) => {
                        if nh + 1 == *height {
                            boundary.push((nr + dr, nc + dc));
                        }
                    }
                    None => (),
                }
            });
            seen.entry((nr, nc))
                .and_modify(|paths| *paths += 1)
                .or_insert(1);
        }
    });
    seen.iter()
        .filter_map(|((r, c), paths)| {
            let h = map.get(&(*r, *c));
            match h {
                Some(9) => Some(paths),
                _ => None,
            }
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/10.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let map = binding
        .as_str()
        .lines()
        .enumerate()
        .flat_map(|(r, row)| {
            row.chars()
                .into_iter()
                .enumerate()
                .map(move |(c, ch)| ((r as i64, c as i64), ch.to_digit(10).unwrap() as u8))
        })
        .collect();

    println!("2024.10.1: {}", solve1(&map));
    println!("2024.10.2: {}", solve2(&map));
}
