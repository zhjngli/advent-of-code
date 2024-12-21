use std::{
    collections::{HashMap, HashSet},
    fs,
};

use crate::common::dijkstra_predecessors;

type Pos = (isize, isize);
type Dir = (isize, isize);

fn get_predecessors(map: &Vec<Vec<char>>) -> HashMap<(Pos, Dir), (HashSet<(Pos, Dir)>, usize)> {
    let s_pos: Pos = (map.len() as isize - 2, 1 as isize);
    let s_dir: Dir = (0, 1);
    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    dijkstra_predecessors(vec![(s_pos, s_dir)], |&((r, c), (pdr, pdc))| {
        dirs.iter()
            .filter_map(|&(dr, dc)| {
                match map[(r + dr) as usize][(c + dc) as usize] {
                    '.' | 'S' | 'E' => {
                        if pdr == dr && pdc == dc {
                            Some((((r + dr, c + dc), (dr, dc)), 1))
                        } else if pdr == dc * -1 && pdc == dr {
                            // turn cw 90 deg
                            Some((((r + dr, c + dc), (dr, dc)), 1001))
                        } else if dr == pdc * -1 && dc == pdr {
                            // turn ccw 90 deg
                            Some((((r + dr, c + dc), (dr, dc)), 1001))
                        } else {
                            // u-turn or something else
                            None
                        }
                    }
                    _ => None,
                }
            })
            .collect::<Vec<((Pos, Dir), usize)>>()
    })
}

fn solve1(map: &Vec<Vec<char>>) -> usize {
    let predecessors = get_predecessors(map);

    let (er, ec) = (1 as isize, map[0].len() as isize - 2);
    let end_states = vec![((er, ec), (-1, 0)), ((er, ec), (0, 1))];
    let least_cost = end_states
        .iter()
        .filter_map(|s| predecessors.get(s))
        .min_by(|(_, c1), (_, c2)| c1.cmp(c2))
        .map(|(_, sc)| sc)
        .unwrap();

    *least_cost
}

fn solve2(map: &Vec<Vec<char>>) -> usize {
    let predecessors = get_predecessors(map);

    let (er, ec) = (1 as isize, map[0].len() as isize - 2);
    let mut best_spots: HashSet<Pos> = HashSet::from([(er, ec)]);
    let mut preds = Vec::new();
    let end_states = vec![((er, ec), (-1, 0)), ((er, ec), (0, 1))];
    let least_cost = end_states
        .iter()
        .filter_map(|s| predecessors.get(s))
        .min_by(|(_, c1), (_, c2)| c1.cmp(c2))
        .map(|(_, sc)| sc)
        .unwrap();
    let least_cost_predecessors: HashSet<(Pos, Dir)> = end_states
        .iter()
        .filter_map(|s| predecessors.get(s))
        .filter(|(_, sc)| sc == least_cost)
        .flat_map(|(sp, _)| sp.clone())
        .collect();
    least_cost_predecessors.iter().for_each(|(p, _)| {
        best_spots.insert(*p);
    });
    preds.push(least_cost_predecessors);

    while let Some(states) = preds.pop() {
        let ps: HashSet<(Pos, Dir)> = states
            .iter()
            .filter_map(|s| predecessors.get(s))
            .flat_map(|(sp, _)| sp.clone())
            .collect();
        if ps.len() > 0 {
            ps.iter().for_each(|(p, _)| {
                best_spots.insert(*p);
            });
            preds.push(ps);
        }
    }
    best_spots.len()
}

pub fn solve() {
    let result = fs::read_to_string("data/16.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let map: Vec<Vec<char>> = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    println!("2024.16.1: {}", solve1(&map));
    println!("2024.16.2: {}", solve2(&map));
}
