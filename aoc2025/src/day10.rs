use std::{collections::HashSet, fs};

use z3::ast::Ast;

use crate::common::dijkstra_predecessors;

#[derive(Debug)]
struct Machine {
    lights: Vec<bool>,
    buttons: Vec<HashSet<usize>>,
    joltage: Vec<usize>,
}

fn parse(s: &str) -> Vec<Machine> {
    s.lines()
        .map(|line| {
            let data: Vec<&str> = line.split(' ').collect();
            Machine {
                lights: data[0][1..data[0].len() - 1]
                    .chars()
                    .map(|c| c == '#')
                    .collect(),
                buttons: data[1..data.len() - 1]
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

fn print_lights(lights: &Vec<bool>) {
    print!("[");
    for b in lights {
        print!("{}", if *b { "#" } else { "." });
    }
    print!("]");
}

fn solve1(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            let initial_state = vec![false; m.lights.len()];
            let predecessors = dijkstra_predecessors(vec![initial_state], |state| {
                m.buttons
                    .iter()
                    .map(|button| {
                        let mut s = state.clone();
                        button.iter().for_each(|bi| s[*bi] = !s[*bi]);
                        s
                    })
                    .map(|s| (s, 1))
                    .collect::<Vec<(Vec<bool>, usize)>>()
            });
            let (_, c) = predecessors.get(&m.lights).unwrap();
            *c
        })
        .sum()
}

fn solve2(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            let ctx = z3::Context::new(&z3::Config::new());
            let optimizer = z3::Optimize::new(&ctx);

            let vars: Vec<z3::ast::Int> = (0..m.buttons.len())
                .map(|i| z3::ast::Int::new_const(&ctx, format!("x{}", i)))
                .collect();
            for v in vars.iter() {
                optimizer.assert(&v.ge(&z3::ast::Int::from_u64(&ctx, 0)));
            }

            for i in 0..m.joltage.len() {
                let joltagei = &z3::ast::Int::from_u64(&ctx, m.joltage[i] as u64);
                let mut sum = z3::ast::Int::from_u64(&ctx, 0);
                for (bi, button) in m.buttons.iter().enumerate() {
                    for j in button.iter() {
                        if *j == i {
                            sum += &vars[bi];
                        }
                    }
                }
                optimizer.assert(&sum._eq(joltagei));
            }

            let mut total_presses = z3::ast::Int::from_u64(&ctx, 0);
            for v in vars.iter() {
                total_presses += v;
            }
            optimizer.minimize(&total_presses);

            match optimizer.check(&[]) {
                z3::SatResult::Sat => {
                    let model = optimizer.get_model().unwrap();
                    let total_presses_num = model.eval(&total_presses, true).unwrap();
                    total_presses_num.as_u64().unwrap() as usize
                }
                z3::SatResult::Unsat => panic!("Unsat"),
                z3::SatResult::Unknown => panic!("Unknown"),
            }
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
    println!("2025.10.2: {}", solve2(&machines));
}
