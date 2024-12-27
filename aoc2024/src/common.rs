use pathfinding::num_traits::Zero;
use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashMap, HashSet};
use std::hash::Hash;

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone)]
struct PQ<S, C>
where
    C: Zero + Eq + Ord + Copy,
    S: Eq + Ord + Hash + Clone,
{
    cost: C,
    state: S,
}

/// Runs dijkstra on the starting states and calculates a predecessors map.
/// Maps all reachable states to all possible predecessors along with their cost.
/// This allows us to find all possible paths of shortest cost, instead of a single one.
/// More inefficient than a normal dijkstra's run.
pub fn dijkstra_predecessors<IX, FN, C, IN>(
    sources: Vec<IX>,
    mut successors: FN,
) -> HashMap<IX, (HashSet<IX>, C)>
where
    IX: Eq + Hash + Clone + Ord,
    FN: FnMut(&IX) -> IN,
    C: Zero + Ord + Copy,
    IN: IntoIterator<Item = (IX, C)>,
{
    let mut predecessors: HashMap<IX, (HashSet<IX>, C)> = HashMap::new();
    let mut distances: HashMap<IX, C> = sources.iter().map(|s| (s.clone(), Zero::zero())).collect();
    let mut p_queue = BinaryHeap::new();
    distances.iter().for_each(|(ix, c)| {
        p_queue.push(Reverse(PQ {
            cost: *c,
            state: ix.clone(),
        }))
    });

    while let Some(Reverse(PQ { cost: c, state: ix })) = p_queue.pop() {
        successors(&ix).into_iter().for_each(|(nix, nc)| {
            let alt = c + nc;
            let dist_nix = distances.get(&nix);
            match dist_nix {
                Some(d) if alt > *d => (), // do nothing
                _ => {
                    distances.insert(nix.clone(), alt);
                    let pred = predecessors.get(&nix);
                    match pred {
                        Some((preds, pred_c)) => {
                            if alt == *pred_c {
                                let mut new_preds = preds.clone();
                                new_preds.insert(ix.clone());
                                predecessors.insert(nix.clone(), (new_preds, alt));
                            } else if alt < *pred_c {
                                predecessors
                                    .insert(nix.clone(), (HashSet::from([ix.clone()]), alt));
                            }
                        }
                        None => {
                            predecessors.insert(nix.clone(), (HashSet::from([ix.clone()]), alt));
                        }
                    }
                    p_queue.push(Reverse(PQ {
                        cost: alt,
                        state: nix.clone(),
                    }));
                }
            }
        });
    }

    predecessors
}

// /// Starting from a parent position, and predecessors, calculate all possible paths ending at the parent
// /// This is in reverse order: the parent comes first in the resulting Vecs, and goes to the starting state
// pub fn calculate_paths<IX, C>(preds: &HashMap<IX, (HashSet<IX>, C)>, parent: IX) -> Vec<Vec<IX>>
// where
//     IX: Eq + Hash + Clone + Ord,
//     C: Zero + Ord + Copy,
// {
//     if let Some((children, _)) = preds.get(&parent) {
//         children
//             .iter()
//             .flat_map(|c| {
//                 let child_paths = calculate_paths(preds, c.clone());
//                 child_paths
//                     .iter()
//                     .map(|cp| {
//                         let mut new_path = Vec::from([parent.clone()]);
//                         cp.iter().for_each(|c| {
//                             new_path.push(c.clone());
//                         });
//                         new_path
//                     })
//                     .collect::<Vec<Vec<IX>>>()
//             })
//             .collect()
//     } else {
//         Vec::from([Vec::from([parent])])
//     }
// }

/// Bron-kerbosch algorithm to find a maximal clique in a graph
/// r is the current clique
/// p is the set of potential nodes to be added to the clique
/// x is the set of nodes that cannot be added to the clique
pub fn bron_kerbosch<N>(
    r: &HashSet<N>,
    p: &mut HashSet<N>,
    x: &mut HashSet<N>,
    graph: &HashMap<N, HashSet<N>>,
) -> HashSet<N>
where
    N: Eq + Hash + Clone + Ord,
{
    if p.is_empty() && x.is_empty() {
        return r.clone();
    }

    let mut max_clique = HashSet::new();
    for v in p.clone() {
        let mut new_r = r.clone();
        new_r.insert(v.clone());

        let ns = graph.get(&v).unwrap();
        let mut new_p = p.intersection(ns).cloned().collect();
        let mut new_x = x.intersection(ns).cloned().collect();
        let clique = bron_kerbosch(&new_r, &mut new_p, &mut new_x, graph);
        if clique.len() > max_clique.len() {
            max_clique = clique;
        }

        p.remove(&v);
        x.insert(v);
    }

    max_clique
}

/// Transpose a 2D vec
pub fn transpose<T: Clone>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    if matrix.is_empty() {
        return Vec::new();
    }
    
    let rows = matrix.len();
    let cols = matrix[0].len();

    assert!(matrix.iter().all(|r| r.len() == cols), "All rows must have the same length!");

    let mut transposed = vec![Vec::with_capacity(rows); cols];
    for r in matrix {
        for (i, v) in r.into_iter().enumerate() {
            transposed[i].push(v);
        }
    }

    transposed
}
