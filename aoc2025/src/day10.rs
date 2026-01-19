use std::{collections::HashSet, fs};

use crate::common::dijkstra_predecessors;

#[derive(Debug)]
struct Machine {
    buttons: Vec<bool>,
    presses: Vec<HashSet<usize>>,
    joltage: Vec<usize>,
}

fn parse(s: &str) -> Vec<Machine> {
    s.lines()
        .map(|line| {
            let data: Vec<&str> = line.split(' ').collect();
            Machine {
                buttons: data[0][1..data[0].len() - 1]
                    .chars()
                    .map(|c| c == '#')
                    .collect(),
                presses: data[1..data.len() - 1]
                    .iter()
                    .map(|d| {
                        d[1..d.len() - 1]
                            .split(',')
                            .map(|n| n.trim().parse().unwrap())
                            .collect()
                    })
                    .collect(),
                joltage: data[data.len() - 1][1..data[data.len() - 1].len() - 1]
                    .split(',')
                    .map(|n| n.trim().parse().unwrap())
                    .collect(),
            }
        })
        .collect()
}

fn print_buttons(buttons: &Vec<bool>) {
    print!("[");
    for b in buttons {
        print!("{}", if *b { "#" } else { "." });
    }
    print!("]");
}

fn solve1(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            let initial_state = vec![false; m.buttons.len()];
            let predecessors = dijkstra_predecessors(vec![initial_state], |state| {
                m.presses
                    .iter()
                    .map(|press| {
                        let mut s = state.clone();
                        press.iter().for_each(|p| s[*p] = !s[*p]);
                        s
                    })
                    .map(|s| (s, 1))
                    .collect::<Vec<(Vec<bool>, usize)>>()
            });
            let (_, c) = predecessors.get(&m.buttons).unwrap();
            *c
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/10.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let machines = parse(binding.as_str());

    println!("2025.10.1: {}", solve1(&machines));
}
