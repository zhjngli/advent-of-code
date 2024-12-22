use std::{collections::HashSet, fs};

use pathfinding::prelude::dijkstra;

fn find_path(map: &HashSet<(isize, isize)>, size: isize) -> Option<(Vec<(isize, isize)>, usize)> {
    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    dijkstra(
        &(0, 0),
        |&(r, c)| {
            dirs.iter()
                .filter_map(|&(dr, dc)| {
                    let nr = r + dr;
                    let nc = c + dc;
                    match map.get(&(nr, nc)) {
                        Some(_) => None,
                        None => {
                            if nr < 0 || nr > size || nc < 0 || nc > size {
                                None
                            } else {
                                Some(((nr, nc), 1))
                            }
                        }
                    }
                })
                .collect::<Vec<((isize, isize), usize)>>()
        },
        |&(r, c)| r == size && c == size,
    )
}

fn solve1(bytes: &Vec<(isize, isize)>, size: isize, first_bytes: usize) -> usize {
    let mut map = HashSet::new();
    (0..first_bytes).for_each(|i| {
        map.insert(bytes[i]);
    });
    let goal = find_path(&map, size);
    match goal {
        Some((_, c)) => c,
        None => panic!("Couldn't reach the end at ({}, {})", size, size),
    }
}

fn solve2(bytes: &Vec<(isize, isize)>, size: isize) -> String {
    let mut map = HashSet::new();
    let mut breaking_byte = bytes[0];
    for b in bytes.iter() {
        map.insert(*b);
        match find_path(&map, size) {
            Some(_) => continue,
            None => {
                breaking_byte = *b;
                break;
            }
        }
    }
    // 1 then 0 since I change the indices from X,Y to R,C
    format!("{},{}", breaking_byte.1, breaking_byte.0)
}

pub fn solve() {
    let result = fs::read_to_string("data/18.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut bytes = Vec::new();
    binding.as_str().lines().into_iter().for_each(|l| {
        if let Some((x, y)) = l.split_once(',') {
            let r = y.trim().parse::<isize>().unwrap();
            let c = x.trim().parse::<isize>().unwrap();
            bytes.push((r, c));
        }
    });
    println!("2024.18.1: {}", solve1(&bytes, 70, 1024));
    println!("2024.18.2: {}", solve2(&bytes, 70));
}
