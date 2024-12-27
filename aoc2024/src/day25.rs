use crate::common::transpose;
use std::fs;

type Schematic = Vec<Vec<char>>;

fn _solve(schematics: &Vec<Schematic>) -> usize {
    let mut keys = Vec::new();
    let mut locks = Vec::new();
    for s in schematics {
        if s[0].iter().all(|c| *c == '#') {
            let l: Vec<usize> = transpose(s.clone())
                .iter()
                .map(|r| r.iter().take_while(|&&x| x == '#').count() - 1)
                .collect();
            locks.push(l);
        } else if s[0].iter().all(|c| *c == '.') {
            let k: Vec<usize> = transpose(s.clone())
                .iter()
                .map(|r| r.iter().take_while(|&&x| x == '.').count() - 1)
                .collect();
            keys.push(k);
        } else {
            panic!("Don't recognize this schematic: {:?}", s);
        }
    }
    let mut fits = 0;
    for k in keys.iter() {
        for l in locks.iter() {
            assert!(
                k.len() == l.len(),
                "keys and locks should have the same length!"
            );
            let mut does_fit = true;
            for i in 0..k.len() {
                if k[i] + l[i] > 5 {
                    does_fit = false;
                    break;
                }
            }
            fits += if does_fit { 1 } else { 0 };
        }
    }
    fits
}

pub fn solve() {
    let result = fs::read_to_string("data/25.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let schematics = binding
        .as_str()
        .split("\n\n")
        .map(|s| s.lines().map(|l| l.chars().collect()).collect())
        .collect();
    println!("2024.25: {}", _solve(&schematics));
}
