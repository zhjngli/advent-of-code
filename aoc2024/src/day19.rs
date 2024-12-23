use cached::proc_macro::cached;
use std::{
    collections::{HashSet, VecDeque},
    fs,
    hash::{Hash, Hasher},
};

#[derive(Eq, PartialEq, Clone)]
struct Towels(HashSet<String>);

#[derive(Eq, PartialEq, Clone)]
struct Design(VecDeque<char>);

impl Hash for Towels {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|s| s.hash(state));
    }
}

impl Hash for Design {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.iter().for_each(|s| s.hash(state));
    }
}

#[cached]
fn possible_design(ts: Towels, d: Design) -> usize {
    let Towels(towels) = ts;
    let Design(mut design) = d;
    let mut design_beginning = Vec::new();
    (0..design.len())
        .map(|_| {
            if let Some(ch) = design.pop_front() {
                design_beginning.push(ch);
                let beginning: String = design_beginning.iter().collect::<String>();
                if towels.contains(&beginning) {
                    if design.len() == 0 {
                        1
                    } else {
                        possible_design(Towels(towels.clone()), Design(design.clone()))
                    }
                } else {
                    0
                }
            } else {
                0
            }
        })
        .sum()
}

fn solve1(towels: &Towels, designs: &Vec<&str>) -> usize {
    designs
        .iter()
        .filter(|d| {
            possible_design(
                towels.clone(),
                Design(d.chars().collect::<VecDeque<char>>().clone()),
            ) != 0
        })
        .count()
}

fn solve2(towels: &Towels, designs: &Vec<&str>) -> usize {
    designs
        .iter()
        .map(|d| {
            possible_design(
                towels.clone(),
                Design(d.chars().collect::<VecDeque<char>>().clone()),
            )
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/19.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut input = binding.as_str().lines();
    let towels = Towels(
        input
            .next()
            .unwrap()
            .split(", ")
            .map(String::from)
            .collect::<HashSet<String>>(),
    );
    input.next();
    let designs = input.into_iter().collect();
    println!("2024.19.1: {}", solve1(&towels, &designs));
    println!("2024.19.2: {}", solve2(&towels, &designs));
}
