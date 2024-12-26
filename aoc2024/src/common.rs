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

/// Starting from a parent position, and predecessors, calculate all possible paths ending at the parent
/// This is in reverse order: the parent comes first in the resulting Vecs, and goes to the starting state
pub fn calculate_paths<IX, C>(preds: &HashMap<IX, (HashSet<IX>, C)>, parent: IX) -> Vec<Vec<IX>>
where
    IX: Eq + Hash + Clone + Ord,
    C: Zero + Ord + Copy,
{
    if let Some((children, _)) = preds.get(&parent) {
        children
            .iter()
            .flat_map(|c| {
                let child_paths = calculate_paths(preds, c.clone());
                child_paths
                    .iter()
                    .map(|cp| {
                        let mut new_path = Vec::from([parent.clone()]);
                        cp.iter().for_each(|c| {
                            new_path.push(c.clone());
                        });
                        new_path
                    })
                    .collect::<Vec<Vec<IX>>>()
            })
            .collect()
    } else {
        Vec::from([Vec::from([parent])])
    }
}
