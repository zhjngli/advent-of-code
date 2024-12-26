use std::{
    collections::{HashMap, HashSet, VecDeque},
    fs,
};

use crate::common::{calculate_paths, dijkstra_predecessors};

type Pos = (isize, isize);
type Path = Vec<Pos>;
type Instr = Vec<char>;

/// Get all dijkstra predecessors when starting from each char in the given chars
fn get_preds_from_chars(
    map: &HashMap<Pos, char>,
    reverse_map: &HashMap<char, Pos>,
    chars: &[char],
) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
    let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
    let mut all_preds = HashMap::new();
    chars.iter().for_each(|ch| {
        if let Some(pos) = reverse_map.get(ch) {
            let preds = dijkstra_predecessors(vec![*pos], |&(r, c)| {
                dirs.iter()
                    .filter_map(|&(dr, dc)| {
                        let nr = r + dr;
                        let nc = c + dc;
                        match map.get(&(nr, nc)) {
                            Some(_) => Some(((nr, nc), 1)),
                            None => None,
                        }
                    })
                    .collect::<Vec<(Pos, usize)>>()
            });
            all_preds.insert(*ch, preds);
        } else {
            panic!("Couldn't find position of char: {}", ch);
        }
    });
    all_preds
}

fn get_numpad_preds(
    numpad: &HashMap<Pos, char>,
    reverse_numpad: &HashMap<char, Pos>,
) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
    let numpad_chars = ['A', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    get_preds_from_chars(numpad, reverse_numpad, &numpad_chars)
}

fn get_dpad_preds(
    dpad: &HashMap<Pos, char>,
    reverse_dpad: &HashMap<char, Pos>,
) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
    let dpad_chars = ['A', '<', '^', 'v', '>'];
    get_preds_from_chars(dpad, reverse_dpad, &dpad_chars)
}

fn flatten_possibilities<T: Clone>(mut possibilities: VecDeque<Vec<Vec<T>>>) -> Vec<Vec<T>> {
    if let Some(ps) = possibilities.pop_front() {
        let mut result = Vec::new();
        let flattened_rest = flatten_possibilities(possibilities);
        ps.iter().for_each(|p| {
            flattened_rest.iter().for_each(|f| {
                let mut possibility = (*p).clone();
                possibility.extend(f.clone());
                result.push(possibility);
            });
        });
        result
    } else {
        Vec::from([Vec::new()])
    }
}

fn find_shortest_instructions(
    all_preds: &HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>>,
    reverse_map: &HashMap<char, Pos>,
    seq: Vec<char>,
) -> Vec<Instr> {
    // all possible paths in between each character in the sequence
    let possible_paths: Vec<Vec<Path>> = seq
        .windows(2)
        .map(|w| {
            let end = w[1];
            let start = w[0];
            let end_pos = reverse_map.get(&end).unwrap();
            if let Some(preds_from_start) = all_preds.get(&start) {
                calculate_paths(preds_from_start, *end_pos)
                    .iter()
                    .map(|path| path.iter().rev().map(|pos| *pos).collect::<Vec<Pos>>())
                    .collect::<Vec<Vec<Pos>>>()
            } else {
                panic!(
                    "Can't find predecessors for {} when processing sequence: {:?}",
                    start, seq
                );
            }
        })
        .collect();
    // convert the paths into instructions on the directional pad
    let possible_instructions: Vec<Vec<Instr>> = possible_paths
        .iter()
        .map(|paths| {
            paths
                .iter()
                .map(|path| {
                    path.windows(2)
                        .map(|w| {
                            let (sr, sc) = w[0];
                            let (er, ec) = w[1];
                            match (er - sr, ec - sc) {
                                (-1, 0) => '^',
                                (1, 0) => 'v',
                                (0, -1) => '<',
                                (0, 1) => '>',
                                (0, 0) => 'A', // robot doesn't need to move, just press the button again
                                (r, c) => panic!("Can't find direction {} {}", r, c),
                            }
                        })
                        .collect()
                })
                .collect()
        })
        .collect();
    // add 'A' in between all instructions so that the robot presses the button
    let mut possible_instructions_with_a: VecDeque<Vec<Instr>> = VecDeque::new();
    for (i, ps) in possible_instructions.iter().enumerate() {
        if i > 0 {
            possible_instructions_with_a.push_back(Vec::from([Vec::from(['A'])]));
        }
        possible_instructions_with_a.push_back(ps.clone());
        if i == possible_instructions.len() - 1 {
            possible_instructions_with_a.push_back(Vec::from([Vec::from(['A'])]));
        }
    }
    // connect the instructions into final sequences
    let connected_instructions = flatten_possibilities(possible_instructions_with_a);
    // if let Some(len) = connected_instructions.first().map(|v| v.len()) {
    //     for instr in connected_instructions.iter() {
    //         assert_eq!(
    //             instr.len(),
    //             len,
    //             "All instructions should have the same length"
    //         );
    //     }
    // }
    connected_instructions
}

fn solve1(
    numpad: &HashMap<Pos, char>,
    reverse_numpad: &HashMap<char, Pos>,
    dpad: &HashMap<Pos, char>,
    reverse_dpad: &HashMap<char, Pos>,
    codes: &Vec<Vec<char>>,
) -> usize {
    let numpad_preds = get_numpad_preds(numpad, reverse_numpad);
    let dpad_preds = get_dpad_preds(dpad, reverse_dpad);
    codes
        .iter()
        .map(|code| {
            let code_val = code
                .iter()
                .cloned()
                .take(code.len() - 1)
                .collect::<String>()
                .parse::<usize>()
                .unwrap();
            let mut shortest_instr = usize::MAX;

            let mut seq = Vec::from(['A']);
            seq.extend(code.clone());
            let numpad_shortest = find_shortest_instructions(&numpad_preds, reverse_numpad, seq);

            let mut dpad1_shortest = Vec::new();
            numpad_shortest.iter().for_each(|i| {
                let mut seq = Vec::from(['A']);
                seq.extend(i);
                dpad1_shortest.extend(find_shortest_instructions(&dpad_preds, reverse_dpad, seq));
            });

            let mut dpad2_shortest = Vec::new();
            dpad1_shortest.iter().for_each(|i| {
                let mut seq = Vec::from(['A']);
                seq.extend(i);
                dpad2_shortest.extend(find_shortest_instructions(&dpad_preds, reverse_dpad, seq));
            });

            dpad2_shortest.iter().for_each(|i| {
                if i.len() < shortest_instr {
                    shortest_instr = i.len();
                }
            });

            code_val * shortest_instr
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/21.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    #[rustfmt::skip]
    let numpad = HashMap::from([
        ((0, 0), '7'), ((0, 1), '8'), ((0, 2), '9'),
        ((1, 0), '4'), ((1, 1), '5'), ((1, 2), '6'),
        ((2, 0), '1'), ((2, 1), '2'), ((2, 2), '3'),
                       ((3, 1), '0'), ((3, 2), 'A'),
    ]);
    let reverse_numpad: HashMap<char, Pos> = numpad.iter().map(|(p, c)| (*c, *p)).collect();
    #[rustfmt::skip]
    let dpad = HashMap::from([
                       ((0, 1), '^'), ((0, 2), 'A'),
        ((1, 0), '<'), ((1, 1), 'v'), ((1, 2), '>')
    ]);
    let reverse_dpad: HashMap<char, Pos> = dpad.iter().map(|(p, c)| (*c, *p)).collect();

    let codes: Vec<Vec<char>> = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    println!(
        "2024.21.1: {}",
        solve1(&numpad, &reverse_numpad, &dpad, &reverse_dpad, &codes)
    );
    // println!("2024.21.2: {}", solve2(&secrets));
}
