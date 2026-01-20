use std::{
    collections::{HashMap, HashSet},
    fs, vec,
};

use crate::common::topological_sort;

fn parse(s: &str) -> HashMap<&str, Vec<&str>> {
    s.lines()
        .map(|line| {
            let parts = line.split(":").collect::<Vec<&str>>();
            let key = parts[0].trim();
            let values = parts[1].trim().split(" ").collect();
            (key, values)
        })
        .collect()
}

// fn paths(cables: &HashMap<&str, Vec<&str>>, start: &str, end: &str) -> usize {
//     let predecessors: HashMap<&str, (HashSet<&str>, usize)> =
//         dijkstra_predecessors(vec![start], |c| {
//             cables
//                 .get(c)
//                 .unwrap_or(&vec![])
//                 .iter()
//                 .map(|&n| (n, 1))
//                 .collect::<Vec<(&str, usize)>>()
//         });

//     let mut paths = vec![vec![end]];
//     while !paths.iter().all(|p| p.last().unwrap() == &start) {
//         let mut new_paths = vec![];
//         for path in paths.iter() {
//             let last = path.last().unwrap();
//             if *last == start {
//                 new_paths.push(path.clone());
//                 continue;
//             }
//             let (preds, _) = predecessors.get(last).unwrap();
//             for pred in preds.iter() {
//                 let mut new_path = path.clone();
//                 if !new_path.contains(pred) {
//                     new_path.push(pred);
//                     new_paths.push(new_path);
//                 }
//             }
//         }
//         paths.clear();
//         paths.extend(new_paths);
//     }
//     paths.len()
// }

fn paths_dfs(cables: &HashMap<&str, Vec<&str>>, start: &str, end: &str) -> usize {
    fn dfs<'a>(
        cables: &HashMap<&str, Vec<&'a str>>,
        u: &'a str,
        end: &str,
        memo: &mut HashMap<&'a str, usize>,
    ) -> usize {
        if u == end {
            return 1;
        }
        let mut total_paths = 0;
        for v in cables.get(u).unwrap_or(&vec![]) {
            total_paths += dfs(cables, v, end, memo);
        }
        memo.insert(u, total_paths);
        total_paths
    }

    let mut memo = HashMap::new();
    dfs(cables, start, end, &mut memo)
}

fn solve1(cables: &HashMap<&str, Vec<&str>>) -> usize {
    paths_dfs(cables, "you", "out")
}

fn paths_toposort(
    cables: &HashMap<&str, Vec<&str>>,
    sort: &Vec<&str>,
    start: &str,
    end: &str,
) -> usize {
    let si = sort.iter().position(|&x| x == start).unwrap();
    let ei = sort.iter().position(|&x| x == end).unwrap();
    let mut paths = vec![0; sort.len()];
    paths[si] = 1;

    for i in 0..sort.len() {
        if i < si {
            continue;
        }
        for v in cables.get(sort[i]).unwrap_or(&vec![]) {
            let vi = sort.iter().position(|&x| x == *v).unwrap();
            paths[vi] += paths[i];
        }
    }
    paths[ei]
}

fn solve2(cables: &HashMap<&str, Vec<&str>>) -> usize {
    let mut map = HashMap::new();
    cables.iter().for_each(|(k, v)| {
        map.insert(*k, HashSet::from_iter(v.clone().iter().cloned()));
    });
    let topo_sort = topological_sort(&map);

    let ffti = topo_sort.iter().position(|&x| x == "fft").unwrap();
    let daci = topo_sort.iter().position(|&x| x == "dac").unwrap();

    if ffti < daci {
        let svr_fft_paths = paths_toposort(cables, &topo_sort, "svr", "fft");
        let fft_dac_paths = paths_toposort(cables, &topo_sort, "fft", "dac");
        let dac_out_paths = paths_toposort(cables, &topo_sort, "dac", "out");
        svr_fft_paths * fft_dac_paths * dac_out_paths
    } else {
        let svr_dac_paths = paths_toposort(cables, &topo_sort, "svr", "dac");
        let dac_fft_paths = paths_toposort(cables, &topo_sort, "dac", "fft");
        let fft_out_paths = paths_toposort(cables, &topo_sort, "fft", "out");
        svr_dac_paths * dac_fft_paths * fft_out_paths
    }
}

pub fn solve() {
    let result = fs::read_to_string("data/11.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let cables = parse(binding.as_str());

    println!("2025.11.1: {}", solve1(&cables));
    println!("2025.11.2: {}", solve2(&cables));
}
