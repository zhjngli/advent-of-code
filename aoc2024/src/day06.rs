use std::{collections::HashSet, fs};

fn find_start(map: &Vec<Vec<char>>) -> Option<(usize, usize)> {
    map.iter()
        .enumerate()
        .flat_map(|(r, row)| {
            row.iter().enumerate().filter_map(
                move |(c, ch)| {
                    if *ch == '^' {
                        Some((r, c))
                    } else {
                        None
                    }
                },
            )
        })
        .next()
}

fn guard_path(map: &Vec<Vec<char>>, start: (usize, usize)) -> HashSet<(usize, usize)> {
    let (mut r, mut c) = start;
    let mut positions = HashSet::from([(r, c)]);
    let (mut dr, mut dc): (i64, i64) = (-1, 0);
    let mut nr = r as i64 + dr;
    let mut nc = c as i64 + dc;
    while (nr as usize) < map.len() && nr >= 0 && (nc as usize) < map[0].len() && nc >= 0 {
        if map[nr as usize][nc as usize] == '#' {
            (dr, dc) = (dc, dr * -1); // turn right
        } else {
            (r, c) = (nr as usize, nc as usize)
        }
        nr = r as i64 + dr;
        nc = c as i64 + dc;
        positions.insert((r, c));
    }
    positions
}

fn solve1(map: &Vec<Vec<char>>) -> usize {
    let (r, c) = find_start(map).unwrap_or((0, 0));
    guard_path(map, (r, c)).len()
}

fn solve2(map: &Vec<Vec<char>>) -> usize {
    let start = find_start(map).unwrap_or((0, 0));
    let mut obstacles = guard_path(map, start);
    obstacles.remove(&start);
    obstacles
        .iter()
        .filter(|(x, y)| {
            let mut new_map = map.clone();
            new_map[*x][*y] = '#';

            let (mut r, mut c) = start;
            let (mut dr, mut dc) = (-1, 0);
            let mut path = HashSet::from([(r, c, dr, dc)]);
            let mut nr = r as i64 + dr;
            let mut nc = c as i64 + dc;
            while (nr as usize) < new_map.len()
                && nr >= 0
                && (nc as usize) < new_map[0].len()
                && nc >= 0
            {
                if new_map[nr as usize][nc as usize] == '#' {
                    (dr, dc) = (dc, dr * -1); // turn right
                } else {
                    (r, c) = (nr as usize, nc as usize)
                }
                nr = r as i64 + dr;
                nc = c as i64 + dc;

                if path.contains(&(r, c, dr, dc)) {
                    return true;
                }
                path.insert((r, c, dr, dc));
            }
            false
        })
        .count()
}

pub fn solve() {
    let result = fs::read_to_string("data/06.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let map = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();

    println!("2024.06.1: {}", solve1(&map));
    println!("2024.06.2: {}", solve2(&map));
}
