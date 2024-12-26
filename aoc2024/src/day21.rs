use std::{collections::HashMap, fs};

type Pos = (isize, isize);

fn sequence_to_num_instructions(
    cache: &HashMap<(usize, char, char), usize>,
    robot: usize,
    seq: String,
) -> usize {
    seq.chars()
        .collect::<Vec<char>>()
        .windows(2)
        .map(|w| {
            let s = w[0];
            let e = w[1];
            if robot == 0 {
                // you are robot 0, it takes 1 instruction to press a key
                1
            } else {
                match cache.get(&(robot, s, e)) {
                    Some(n) => *n,
                    None => panic!("Couldn't find ({}, {}, {}) in cache", robot, s, e),
                }
            }
        })
        .sum()
}

fn cache_robot_keypresses(
    cache: &mut HashMap<(usize, char, char), usize>,
    robot: usize,
    pad: &HashMap<char, Pos>,
    reverse_pad: &HashMap<Pos, char>,
) {
    for (s, (sr, sc)) in pad.iter() {
        for (e, (er, ec)) in pad.iter() {
            let dr = er - sr;
            let dc = ec - sc;

            // avoid mixing left/right and up/down, it's always better to do all rows then all cols, or vice versa
            let dr_keys: String = (0..dr.abs())
                .map(|_| if dr < 0 { '^' } else { 'v' })
                .collect();
            let dc_keys: String = (0..dc.abs())
                .map(|_| if dc < 0 { '<' } else { '>' })
                .collect();

            // robots start and end at A
            let row_first_seq = format!("A{dr_keys}{dc_keys}A");
            let col_first_seq = format!("A{dc_keys}{dr_keys}A");

            // make sure path doesn't go through a non existent key on the pad
            let row_first_num_instrs = if reverse_pad.contains_key(&(*er, *sc)) {
                sequence_to_num_instructions(cache, robot - 1, row_first_seq)
            } else {
                usize::MAX
            };
            let col_first_num_instrs = if reverse_pad.contains_key(&(*sr, *ec)) {
                sequence_to_num_instructions(cache, robot - 1, col_first_seq)
            } else {
                usize::MAX
            };

            cache.insert(
                (robot, *s, *e),
                row_first_num_instrs.min(col_first_num_instrs),
            );
        }
    }
}

fn build_cache(
    robots: usize,
    numpad: &HashMap<char, Pos>,
    reverse_numpad: &HashMap<Pos, char>,
    dpad: &HashMap<char, Pos>,
    reverse_dpad: &HashMap<Pos, char>,
) -> HashMap<(usize, char, char), usize> {
    let mut cache = HashMap::new();

    (1..robots).for_each(|r| {
        cache_robot_keypresses(&mut cache, r, dpad, reverse_dpad);
    });
    cache_robot_keypresses(&mut cache, robots, numpad, reverse_numpad);

    cache
}

fn _solve(
    numpad: &HashMap<char, Pos>,
    reverse_numpad: &HashMap<Pos, char>,
    dpad: &HashMap<char, Pos>,
    reverse_dpad: &HashMap<Pos, char>,
    codes: &Vec<Vec<char>>,
    robots: usize,
) -> usize {
    let cache = build_cache(robots, numpad, reverse_numpad, dpad, reverse_dpad);
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

            let num_instrs = sequence_to_num_instructions(
                &cache,
                robots,
                format!("A{}", code.iter().collect::<String>()),
            );

            code_val * num_instrs
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
        ('7', (0, 0)), ('8', (0, 1)), ('9', (0, 2)),
        ('4', (1, 0)), ('5', (1, 1)), ('6', (1, 2)),
        ('1', (2, 0)), ('2', (2, 1)), ('3', (2, 2)),
                       ('0', (3, 1)), ('A', (3, 2)),
    ]);
    let reverse_numpad: HashMap<Pos, char> = numpad.iter().map(|(c, p)| (*p, *c)).collect();
    #[rustfmt::skip]
    let dpad = HashMap::from([
                       ('^', (0, 1)), ('A', (0, 2)),
        ('<', (1, 0)), ('v', (1, 1)), ('>', (1, 2))
    ]);
    let reverse_dpad: HashMap<Pos, char> = dpad.iter().map(|(c, p)| (*p, *c)).collect();

    let codes: Vec<Vec<char>> = binding
        .as_str()
        .lines()
        .map(|l| l.chars().collect())
        .collect();
    println!(
        "2024.21.1: {}",
        _solve(&numpad, &reverse_numpad, &dpad, &reverse_dpad, &codes, 3)
    );
    println!(
        "2024.21.2: {}",
        _solve(&numpad, &reverse_numpad, &dpad, &reverse_dpad, &codes, 26)
    );
    // println!(
    //     "2024.21.1 slow: {}",
    //     _solve_slow(2, &reverse_numpad, &numpad, &reverse_dpad, &dpad, &codes)
    // );
}

// use std::collections::{HashSet, VecDeque};

// use crate::common::{calculate_paths, dijkstra_predecessors};

// type Path = Vec<Pos>;
// type Instr = Vec<char>;

// /// Get all dijkstra predecessors when starting from each char in the given chars
// fn get_preds_from_chars(
//     map: &HashMap<Pos, char>,
//     reverse_map: &HashMap<char, Pos>,
//     chars: &[char],
// ) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
//     let dirs = vec![(-1, 0), (0, 1), (1, 0), (0, -1)];
//     let mut all_preds = HashMap::new();
//     chars.iter().for_each(|ch| {
//         if let Some(pos) = reverse_map.get(ch) {
//             let preds = dijkstra_predecessors(vec![*pos], |&(r, c)| {
//                 dirs.iter()
//                     .filter_map(|&(dr, dc)| {
//                         let nr = r + dr;
//                         let nc = c + dc;
//                         match map.get(&(nr, nc)) {
//                             Some(_) => Some(((nr, nc), 1)),
//                             None => None,
//                         }
//                     })
//                     .collect::<Vec<(Pos, usize)>>()
//             });
//             all_preds.insert(*ch, preds);
//         } else {
//             panic!("Couldn't find position of char: {}", ch);
//         }
//     });
//     all_preds
// }

// fn get_numpad_preds(
//     numpad: &HashMap<Pos, char>,
//     reverse_numpad: &HashMap<char, Pos>,
// ) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
//     let numpad_chars = ['A', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
//     get_preds_from_chars(numpad, reverse_numpad, &numpad_chars)
// }

// fn get_dpad_preds(
//     dpad: &HashMap<Pos, char>,
//     reverse_dpad: &HashMap<char, Pos>,
// ) -> HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>> {
//     let dpad_chars = ['A', '<', '^', 'v', '>'];
//     get_preds_from_chars(dpad, reverse_dpad, &dpad_chars)
// }

// fn flatten_possibilities<T: Clone>(mut possibilities: VecDeque<Vec<Vec<T>>>) -> Vec<Vec<T>> {
//     if let Some(ps) = possibilities.pop_front() {
//         let mut result = Vec::new();
//         let flattened_rest = flatten_possibilities(possibilities);
//         ps.iter().for_each(|p| {
//             flattened_rest.iter().for_each(|f| {
//                 let mut possibility = (*p).clone();
//                 possibility.extend(f.clone());
//                 result.push(possibility);
//             });
//         });
//         result
//     } else {
//         Vec::from([Vec::new()])
//     }
// }

// fn find_shortest_instructions(
//     all_preds: &HashMap<char, HashMap<Pos, (HashSet<Pos>, usize)>>,
//     reverse_map: &HashMap<char, Pos>,
//     seq: Vec<char>,
// ) -> Vec<Instr> {
//     // all possible paths in between each character in the sequence
//     let possible_paths: Vec<Vec<Path>> = seq
//         .windows(2)
//         .map(|w| {
//             let end = w[1];
//             let start = w[0];
//             let end_pos = reverse_map.get(&end).unwrap();
//             if let Some(preds_from_start) = all_preds.get(&start) {
//                 calculate_paths(preds_from_start, *end_pos)
//                     .iter()
//                     .map(|path| path.iter().rev().map(|pos| *pos).collect::<Vec<Pos>>())
//                     .collect::<Vec<Vec<Pos>>>()
//             } else {
//                 panic!(
//                     "Can't find predecessors for {} when processing sequence: {:?}",
//                     start, seq
//                 );
//             }
//         })
//         .collect();
//     // convert the paths into instructions on the directional pad
//     let possible_instructions: Vec<Vec<Instr>> = possible_paths
//         .iter()
//         .map(|paths| {
//             paths
//                 .iter()
//                 .map(|path| {
//                     path.windows(2)
//                         .map(|w| {
//                             let (sr, sc) = w[0];
//                             let (er, ec) = w[1];
//                             match (er - sr, ec - sc) {
//                                 (-1, 0) => '^',
//                                 (1, 0) => 'v',
//                                 (0, -1) => '<',
//                                 (0, 1) => '>',
//                                 (0, 0) => 'A', // robot doesn't need to move, just press the button again
//                                 (r, c) => panic!("Can't find direction {} {}", r, c),
//                             }
//                         })
//                         .collect()
//                 })
//                 .collect()
//         })
//         .collect();
//     // add 'A' in between all instructions so that the robot presses the button
//     let mut possible_instructions_with_a: VecDeque<Vec<Instr>> = VecDeque::new();
//     for (i, ps) in possible_instructions.iter().enumerate() {
//         if i > 0 {
//             possible_instructions_with_a.push_back(Vec::from([Vec::from(['A'])]));
//         }
//         possible_instructions_with_a.push_back(ps.clone());
//         if i == possible_instructions.len() - 1 {
//             possible_instructions_with_a.push_back(Vec::from([Vec::from(['A'])]));
//         }
//     }
//     // connect the instructions into final sequences
//     let connected_instructions = flatten_possibilities(possible_instructions_with_a);
//     // if let Some(len) = connected_instructions.first().map(|v| v.len()) {
//     //     for instr in connected_instructions.iter() {
//     //         assert_eq!(
//     //             instr.len(),
//     //             len,
//     //             "All instructions should have the same length"
//     //         );
//     //     }
//     // }
//     connected_instructions
// }

// fn _solve_slow(
//     robots: usize,
//     numpad: &HashMap<Pos, char>,
//     reverse_numpad: &HashMap<char, Pos>,
//     dpad: &HashMap<Pos, char>,
//     reverse_dpad: &HashMap<char, Pos>,
//     codes: &Vec<Vec<char>>,
// ) -> usize {
//     let numpad_preds = get_numpad_preds(numpad, reverse_numpad);
//     let dpad_preds = get_dpad_preds(dpad, reverse_dpad);
//     codes
//         .iter()
//         .map(|code| {
//             let code_val = code
//                 .iter()
//                 .cloned()
//                 .take(code.len() - 1)
//                 .collect::<String>()
//                 .parse::<usize>()
//                 .unwrap();
//             let mut shortest_instr = usize::MAX;

//             let mut seq = Vec::from(['A']);
//             seq.extend(code.clone());
//             let numpad_shortest = find_shortest_instructions(&numpad_preds, reverse_numpad, seq);

//             let mut i_shortest = numpad_shortest;
//             for _ in 0..robots {
//                 let mut next_i_shortest = Vec::new();
//                 i_shortest.iter().for_each(|i| {
//                     let mut seq = Vec::from(['A']);
//                     seq.extend(i);
//                     next_i_shortest.extend(find_shortest_instructions(
//                         &dpad_preds,
//                         reverse_dpad,
//                         seq,
//                     ));
//                 });
//                 i_shortest = next_i_shortest;
//             }

//             i_shortest.iter().for_each(|i| {
//                 if i.len() < shortest_instr {
//                     shortest_instr = i.len();
//                 }
//             });

//             code_val * shortest_instr
//         })
//         .sum()
// }
