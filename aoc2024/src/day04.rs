use std::fs;

fn solve1(w: &Vec<Vec<char>>) -> u64 {
    let deltas: [(i64, i64); 8] = [
        (0, 1),
        (1, 1),
        (1, 0),
        (1, -1),
        (0, -1),
        (-1, -1),
        (-1, 0),
        (-1, 1),
    ];
    let xmas: [char; 4] = ['X', 'M', 'A', 'S'];
    let mut count = 0;
    for r in 0..w.len() as i64 {
        for c in 0..w[0].len() as i64 {
            for (dr, dc) in deltas {
                let mut found = 1;
                for i in 0..xmas.len() as i64 {
                    let nr = r + dr * i;
                    let nc = c + dc * i;
                    if nr as usize >= w.len()
                        || nr < 0
                        || nc as usize >= w[0].len()
                        || nc < 0
                        || w[nr as usize][nc as usize] != xmas[i as usize]
                    {
                        found = 0;
                        break;
                    }
                }
                count += found;
            }
        }
    }
    count
}

fn solve2(w: &Vec<Vec<char>>) -> u64 {
    let mut count = 0;
    let m_deltas: [((i64, i64), (i64, i64)); 4] = [
        ((-1, -1), (-1, 1)),
        ((-1, 1), (1, 1)),
        ((1, 1), (1, -1)),
        ((1, -1), (-1, -1)),
    ];
    for r in 0..w.len() as i64 {
        for c in 0..w[0].len() as i64 {
            if w[r as usize][c as usize] != 'A' {
                continue;
            }
            for ((dr1, dc1), (dr2, dc2)) in m_deltas {
                let m1r = r + dr1;
                let m1c = c + dc1;
                let m2r = r + dr2;
                let m2c = c + dc2;
                let s1r = r + dr1 * -1;
                let s1c = c + dc1 * -1;
                let s2r = r + dr2 * -1;
                let s2c = c + dc2 * -1;
                if (m1r as usize) < w.len()
                    && m1r >= 0
                    && (m1c as usize) < w[0].len()
                    && m1c >= 0
                    && (m2r as usize) < w.len()
                    && m2r >= 0
                    && (m2c as usize) < w[0].len()
                    && m2c >= 0
                    && (s1r as usize) < w.len()
                    && s1r >= 0
                    && (s1c as usize) < w[0].len()
                    && s1c >= 0
                    && (s2r as usize) < w.len()
                    && s2r >= 0
                    && (s2c as usize) < w[0].len()
                    && s2c >= 0
                    && w[m1r as usize][m1c as usize] == 'M'
                    && w[m2r as usize][m2c as usize] == 'M'
                    && w[s1r as usize][s1c as usize] == 'S'
                    && w[s2r as usize][s2c as usize] == 'S'
                {
                    count += 1;
                }
            }
        }
    }
    count
}

pub fn solve() {
    let result = fs::read_to_string("data/04.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "mul(1,1)\n".to_string()
    });
    let input = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    println!("2024.04.1: {}", solve1(&input));
    println!("2024.04.2: {}", solve2(&input));
}
