use std::{
    collections::{HashMap, HashSet},
    fs,
    hash::{Hash, Hasher},
};

use crate::common::bron_kerbosch;

#[derive(Eq, PartialEq, Debug)]
struct Triangle<'a>(HashSet<&'a str>);

impl Hash for Triangle<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut ns: Vec<&str> = self.0.iter().map(|s| *s).collect();
        ns.sort();
        ns.join(",").hash(state);
    }
}

fn solve1(graph: &HashMap<&str, HashSet<&str>>) -> usize {
    let mut triangles = HashSet::new();
    for n in graph.keys() {
        let ns: Vec<&str> = graph.get(n).unwrap().iter().map(|s| *s).collect();
        for i in 0..ns.len() {
            let n1 = ns[i];
            for j in i + 1..ns.len() {
                let n2 = ns[j];
                if graph.get(&n1).unwrap().contains(&n2) {
                    triangles.insert(Triangle(HashSet::from([*n, n1, n2])));
                }
            }
        }
    }
    triangles
        .iter()
        .filter(|t| t.0.iter().any(|s| s.starts_with('t')))
        .count()
}

fn solve2(graph: &HashMap<&str, HashSet<&str>>) -> String {
    let mut mc: Vec<&str> = bron_kerbosch(
        &HashSet::new(),
        &mut graph.keys().cloned().collect(),
        &mut HashSet::new(),
        graph,
    )
    .into_iter()
    .collect();
    mc.sort();
    mc.join(",")
}

pub fn solve() {
    let result = fs::read_to_string("data/23.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut graph: HashMap<&str, HashSet<&str>> = HashMap::new();
    binding.as_str().lines().for_each(|l| {
        let (n1, n2) = l.split_once('-').unwrap();
        graph
            .entry(n1)
            .and_modify(|neighbors| {
                neighbors.insert(n2);
            })
            .or_insert(HashSet::from([n2]));
        graph
            .entry(n2)
            .and_modify(|neighbors| {
                neighbors.insert(n1);
            })
            .or_insert(HashSet::from([n1]));
    });
    println!("2024.23.1: {}", solve1(&graph));
    println!("2024.23.2: {}", solve2(&graph));
}
