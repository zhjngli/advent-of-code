use std::{collections::HashSet, fs};

use pathfinding::prelude::dijkstra;

fn _solve(
    map: &Vec<Vec<char>>,
    (sr, sc): (isize, isize),
    (er, ec): (isize, isize),
    cheat: isize,
) -> usize {
    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let (path, _) = dijkstra(
        &(sr, sc),
        |&(r, c)| {
            dirs.iter()
                .filter_map(|&(dr, dc)| {
                    let nr = r + dr;
                    let nc = c + dc;
                    match map[nr as usize][nc as usize] {
                        '.' | 'S' | 'E' => Some(((nr, nc), 1)),
                        '#' => None,
                        ch => panic!("Unrecognized char: {}", ch),
                    }
                })
                .collect::<Vec<((isize, isize), usize)>>()
        },
        |&(r, c)| r == er && c == ec,
    )
    .unwrap();

    let mut cheat_diffs: HashSet<(isize, isize)> = HashSet::new();
    for c in 2..=cheat {
        for x in -c..=c {
            let y = c - x.abs();
            cheat_diffs.insert((x, y));
            cheat_diffs.insert((x, -y));
        }
    }

    let mut cheats = 0;
    (0..path.len()).for_each(|i| {
        (i + 1..path.len()).for_each(|j| {
            let (ir, ic) = path[i];
            let (jr, jc) = path[j];
            let (dr, dc) = (ir - jr, ic - jc);
            if cheat_diffs.contains(&(dr, dc)) {
                if j - i >= 100 + dr.abs() as usize + dc.abs() as usize {
                    cheats += 1;
                }
            }
        });
    });
    cheats
}

fn solve1(map: &Vec<Vec<char>>, s: (isize, isize), e: (isize, isize)) -> usize {
    _solve(map, s, e, 2)
}

fn solve2(map: &Vec<Vec<char>>, s: (isize, isize), e: (isize, isize)) -> usize {
    _solve(map, s, e, 20)
}

pub fn solve() {
    let result = fs::read_to_string("data/20.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let map: Vec<Vec<char>> = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    let mut s_pos = (0, 0);
    let mut e_pos = (0, 0);
    for (r, row) in map.iter().enumerate() {
        for (c, ch) in row.iter().enumerate() {
            match *ch {
                'S' => s_pos = (r as isize, c as isize),
                'E' => e_pos = (r as isize, c as isize),
                _ => continue,
            }
        }
    }
    println!("2024.20.1: {}", solve1(&map, s_pos, e_pos));
    println!("2024.20.2: {}", solve2(&map, s_pos, e_pos));
}
