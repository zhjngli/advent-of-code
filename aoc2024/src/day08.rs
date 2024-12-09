use std::{
    cmp::max,
    collections::{HashMap, HashSet},
    fs,
    ops::Range,
};

fn _solve(
    antennas: &HashMap<char, HashSet<(usize, usize)>>,
    (maxr, maxc): (i64, i64),
    iters: Range<i64>,
) -> usize {
    antennas
        .iter()
        .fold(
            HashSet::new(),
            |mut antinodes: HashSet<(i64, i64)>, (_, positions)| {
                let pairs: Vec<((usize, usize), (usize, usize))> = positions
                    .iter()
                    .flat_map(|&x| {
                        positions
                            .iter()
                            .filter(move |&&y| x < y)
                            .map(move |&y| (x, y))
                    })
                    .collect();
                pairs.iter().for_each(|((r1, c1), (r2, c2))| {
                    let dr: i64 = *r2 as i64 - *r1 as i64;
                    let dc: i64 = *c2 as i64 - *c1 as i64;
                    iters.clone().for_each(|i| {
                        antinodes.insert((*r2 as i64 + dr * i, *c2 as i64 + dc * i));
                        antinodes.insert((*r1 as i64 - dr * i, *c1 as i64 - dc * i));
                    })
                });
                antinodes
            },
        )
        .iter()
        .filter(|(r, c)| *r >= 0 && *r < maxr && *c >= 0 && *c < maxc)
        .count()
}

fn solve1(map: &Vec<Vec<char>>, antennas: &HashMap<char, HashSet<(usize, usize)>>) -> usize {
    let (maxr, maxc) = (map.len() as i64, map[0].len() as i64);
    _solve(antennas, (maxr, maxc), 1..2) // only care about 1 antinode
}

fn solve2(map: &Vec<Vec<char>>, antennas: &HashMap<char, HashSet<(usize, usize)>>) -> usize {
    let (maxr, maxc) = (map.len() as i64, map[0].len() as i64);
    _solve(antennas, (maxr, maxc), 0..max(maxr, maxc)) // want all antinodes from 0 to out of the map
}

pub fn solve() {
    let result = fs::read_to_string("data/08.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let map: Vec<Vec<char>> = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    let antennas: HashMap<char, HashSet<(usize, usize)>> = map
        .iter()
        .enumerate()
        .flat_map(|(r, row)| row.iter().enumerate().map(move |(c, ch)| (ch, (r, c))))
        .fold(HashMap::new(), |mut ants, (ch, pos)| {
            if *ch != '.' {
                ants.entry(*ch).or_insert_with(HashSet::new).insert(pos);
            }
            ants
        });

    println!("2024.08.1: {}", solve1(&map, &antennas));
    println!("2024.08.2: {}", solve2(&map, &antennas));
}
