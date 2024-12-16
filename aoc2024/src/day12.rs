use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn regions(farm: &HashMap<(isize, isize), char>) -> Vec<(char, HashSet<(isize, isize)>)> {
    let mut farm_seen = HashSet::new();
    let mut regions = Vec::new();
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    farm.iter().for_each(|((r, c), ch)| {
        if farm_seen.contains(&(*r, *c)) {
            return;
        }
        let mut seen = HashSet::new();
        let mut boundary = vec![(*r, *c)];
        let mut region = HashSet::from([(*r, *c)]);
        while boundary.len() > 0 {
            let (nr, nc) = boundary.pop().unwrap();
            dirs.iter().for_each(|(dr, dc)| {
                let neighbor_pos = (nr + dr, nc + dc);
                if seen.contains(&neighbor_pos) {
                    return;
                }
                let nch = farm.get(&neighbor_pos);
                match nch {
                    Some(n) if n == ch => {
                        boundary.push(neighbor_pos);
                        seen.insert(neighbor_pos);
                        farm_seen.insert(neighbor_pos);
                        region.insert(neighbor_pos);
                    }
                    _ => (),
                }
            })
        }
        regions.push((*ch, region));
    });
    regions
}

fn solve1(farm: &HashMap<(isize, isize), char>) -> usize {
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    regions(farm)
        .into_iter()
        .map(|(_, region)| {
            let area = region.len();
            let mut perimeter = 0;
            region.iter().for_each(|(r, c)| {
                dirs.iter().for_each(|(dr, dc)| {
                    let neighbor_pos = (r + dr, c + dc);
                    if !region.contains(&neighbor_pos) {
                        perimeter += 1;
                    }
                });
            });
            area * perimeter
        })
        .sum()
}

fn solve2(farm: &HashMap<(isize, isize), char>) -> usize {
    let dirs: Vec<(isize, isize)> = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let diags: Vec<(isize, isize)> = vec![(-1, -1), (-1, 1), (1, -1), (1, 1)];
    regions(farm)
        .into_iter()
        .map(|(_, region)| {
            let area = region.len();
            let mut doubled_region = HashSet::new();
            region.iter().for_each(|(r, c)| {
                doubled_region.insert((2 * r, 2 * c));
                doubled_region.insert((2 * r + 1, 2 * c));
                doubled_region.insert((2 * r, 2 * c + 1));
                doubled_region.insert((2 * r + 1, 2 * c + 1));
            });

            let mut sides = 0; // count corners, same as sides
            doubled_region.iter().for_each(|(r, c)| {
                let diag_neighbors: Vec<Option<&(isize, isize)>> = diags
                    .iter()
                    .map(|(dr, dc)| {
                        let neighbor_pos = (r + dr, c + dc);
                        doubled_region.get(&neighbor_pos)
                    })
                    .collect();
                let dir_neighbors: Vec<Option<&(isize, isize)>> = dirs
                    .iter()
                    .map(|(dr, dc)| {
                        let neighbor_pos = (r + dr, c + dc);
                        doubled_region.get(&neighbor_pos)
                    })
                    .collect();
                match (diag_neighbors.as_slice(), dir_neighbors.as_slice()) {
                    // convex corners have 2 regular neighbors
                    (_, &[Some(_), Some(_), None, None]) => sides += 1,
                    (_, &[Some(_), None, None, Some(_)]) => sides += 1,
                    (_, &[None, Some(_), Some(_), None]) => sides += 1,
                    (_, &[None, None, Some(_), Some(_)]) => sides += 1,
                    // concave corners have 3 diag neighbors and all 4 regular neighbors
                    (&[None, Some(_), Some(_), Some(_)], &[Some(_), Some(_), Some(_), Some(_)]) => {
                        sides += 1
                    }
                    (&[Some(_), None, Some(_), Some(_)], &[Some(_), Some(_), Some(_), Some(_)]) => {
                        sides += 1
                    }
                    (&[Some(_), Some(_), None, Some(_)], &[Some(_), Some(_), Some(_), Some(_)]) => {
                        sides += 1
                    }
                    (&[Some(_), Some(_), Some(_), None], &[Some(_), Some(_), Some(_), Some(_)]) => {
                        sides += 1
                    }
                    _ => (),
                }
            });

            area * sides
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/12.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let farm = binding
        .as_str()
        .lines()
        .enumerate()
        .flat_map(|(r, l)| {
            l.chars()
                .enumerate()
                .map(move |(c, ch)| ((r as isize, c as isize), ch))
        })
        .collect();
    println!("2024.12.1: {}", solve1(&farm));
    println!("2024.12.2: {}", solve2(&farm));
}
