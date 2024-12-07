use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

fn ordered(rs: &HashMap<i64, HashSet<i64>>, u: &Vec<i64>) -> bool {
    (0..u.len() - 1).all(|i| rs.get(&u[i]).unwrap_or(&HashSet::new()).contains(&u[i + 1]))
}

fn solve1(rs: &HashMap<i64, HashSet<i64>>, us: &Vec<Vec<i64>>) -> i64 {
    us.iter()
        .filter(|u| ordered(rs, u))
        .map(|u| u[u.len() / 2])
        .sum()
}

fn topological_sort(g: &HashMap<i64, HashSet<i64>>) -> Vec<i64> {
    let mut in_degrees = HashMap::new();
    g.iter().for_each(|(k, vs)| {
        in_degrees.entry(k.clone()).or_insert(0);
        vs.iter().for_each(|v| {
            in_degrees
                .entry(v.clone())
                .and_modify(|d| {
                    *d += 1;
                })
                .or_insert(1);
        });
    });

    let mut q = VecDeque::new();
    in_degrees.iter().for_each(|(n, d)| {
        if *d == 0 {
            q.push_back(n.clone());
        }
    });

    let mut sorted = Vec::new();
    while let Some(node) = q.pop_front() {
        sorted.push(node.clone());
        if let Some(ns) = g.get(&node) {
            ns.iter().for_each(|n| {
                if let Some(d) = in_degrees.get_mut(n) {
                    *d -= 1;
                    if *d == 0 {
                        q.push_back(n.clone());
                    }
                }
            });
        }
    }

    if sorted.len() < g.len() {
        panic!("Cycle exists in graph: {:?}", g);
    }

    sorted
}

fn solve2(rs: &HashMap<i64, HashSet<i64>>, us: &Vec<Vec<i64>>) -> i64 {
    us.iter()
        .filter(|u| !ordered(rs, u))
        .map(|u| {
            // need to take subset of the whole graph since the whole graph may have cycles, but the subset doesn't
            let mut rs_subset: HashMap<i64, HashSet<i64>> = HashMap::new();
            rs.iter()
                .filter(|(e, _)| u.contains(e))
                .for_each(|(e, vs)| {
                    rs_subset.insert(e.clone(), vs.clone());
                });
            let sorted_nodes = topological_sort(&rs_subset);

            let sorted_u: Vec<i64> = sorted_nodes.into_iter().filter(|n| u.contains(n)).collect();
            sorted_u[sorted_u.len() / 2]
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/05.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let manual: Vec<&str> = binding.split("\n\n").collect();
    let mut rules: HashMap<i64, HashSet<i64>> = HashMap::new();
    manual[0]
        .lines()
        .filter_map(|l| {
            l.split_once('|').map(|(before, after)| {
                (
                    before.parse::<i64>().unwrap(),
                    after.parse::<i64>().unwrap(),
                )
            })
        })
        .for_each(|(p1, p2)| {
            rules
                .entry(p1)
                .and_modify(|s| {
                    s.insert(p2);
                })
                .or_insert(HashSet::from([p2]));
        });
    let updates: Vec<Vec<i64>> = manual[1]
        .lines()
        .map(|l| {
            l.split(',')
                .map(|n| n.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .collect();

    println!("2024.05.1: {}", solve1(&rules, &updates));
    println!("2024.05.2: {}", solve2(&rules, &updates));
}
