use std::{collections::HashMap, fs};

fn move_in_map(
    do_move: Option<bool>,
    mut move_stack: Vec<(char, isize, isize)>,
    map: &mut HashMap<(isize, isize), char>,
    (dr, dc): (isize, isize),
    (sr, sc): (isize, isize),
) -> (isize, isize) {
    match do_move {
        Some(true) => {
            while move_stack.len() > 0 {
                let (ch, r, c) = move_stack.pop().unwrap();
                map.insert((r + dr, c + dc), ch);
                map.insert((r, c), '.');
            }
            (sr + dr, sc + dc)
        }
        Some(false) => (sr, sc),
        None => panic!("Exited while None"),
    }
}

fn simple_push(
    map: &mut HashMap<(isize, isize), char>,
    (sr, sc): (isize, isize),
    (dr, dc): (isize, isize),
) -> (isize, isize) {
    let mut move_stack = vec![('@', sr, sc)];
    let mut do_move = None;
    let (mut wr, mut wc) = (sr, sc);
    while do_move == None {
        let (nr, nc) = (wr + dr, wc + dc);
        match map.get(&(nr, nc)) {
            Some('.') => do_move = Some(true),
            Some('#') => do_move = Some(false),
            Some('O') => move_stack.push(('O', nr, nc)),
            Some(ch) => {
                panic!("Unrecognized char while moving: {}", ch);
            }
            None => panic!("Moved out of the map: {}, {}", nr, nc),
        }
        (wr, wc) = (nr, nc);
    }
    move_in_map(do_move, move_stack, map, (dr, dc), (sr, sc))
}

fn solve1(map: &mut HashMap<(isize, isize), char>, instrs: &Vec<char>) -> isize {
    let (mut rr, mut rc) = (0, 0);
    for ((r, c), ch) in map.iter() {
        if *ch == '@' {
            rr = *r;
            rc = *c;
            break;
        }
    }
    instrs.iter().for_each(|i| {
        match i {
            '^' => (rr, rc) = simple_push(map, (rr, rc), (-1, 0)),
            'v' => (rr, rc) = simple_push(map, (rr, rc), (1, 0)),
            '<' => (rr, rc) = simple_push(map, (rr, rc), (0, -1)),
            '>' => (rr, rc) = simple_push(map, (rr, rc), (0, 1)),
            c => panic!("Unrecognized char: {}", c),
        };
    });
    map.iter()
        .map(|((r, c), ch)| match ch {
            'O' => 100 * r + c,
            _ => 0,
        })
        .sum::<isize>()
}

fn push_vert(
    map: &mut HashMap<(isize, isize), char>,
    (sr, sc): (isize, isize),
    up: bool,
) -> (isize, isize) {
    let (dr, dc) = if up { (-1, 0) } else { (1, 0) };
    let mut move_stack = vec![('@', sr, sc)];
    let mut do_move = None;
    let mut frontier = vec![(sr, sc)];
    while do_move == None {
        let next_pos: Vec<(isize, isize)> =
            frontier.iter().map(|(r, c)| (*r + dr, *c + dc)).collect();
        let mut all_empty = true;
        let mut next_frontier = Vec::new();
        for (r, c) in next_pos.iter() {
            match map.get(&(*r, *c)) {
                Some('.') => next_frontier.push((*r - dr, *c - dc)),
                Some('#') => {
                    all_empty = false;
                    do_move = Some(false)
                }
                Some('[') => {
                    all_empty = false;
                    next_frontier.push((*r, *c));
                    next_frontier.push((*r, *c + 1));
                    move_stack.push(('[', *r, *c));
                    move_stack.push((']', *r, *c + 1));
                }
                Some(']') => {
                    all_empty = false;
                    next_frontier.push((*r, *c));
                    next_frontier.push((*r, *c - 1));
                    move_stack.push((']', *r, *c));
                    move_stack.push(('[', *r, *c - 1));
                }
                Some(ch) => panic!("Unrecognized char while moving vert: {}", ch),
                None => panic!("Moved vert out of the map: {}, {}", r, c),
            }
        }
        if all_empty {
            do_move = Some(true);
        }
        frontier = next_frontier;
    }
    move_in_map(do_move, move_stack, map, (dr, dc), (sr, sc))
}

fn push_horiz(
    map: &mut HashMap<(isize, isize), char>,
    (sr, sc): (isize, isize),
    left: bool,
) -> (isize, isize) {
    let (dr, dc) = if left { (0, -1) } else { (0, 1) };
    let box_char = if left { ']' } else { '[' };
    let closing_box_char = if left { '[' } else { ']' };

    let mut move_stack = vec![('@', sr, sc)];
    let mut do_move = None;
    let (mut wr, mut wc) = (sr, sc);
    while do_move == None {
        let (mut nr, mut nc) = (wr + dr, wc + dc);
        match map.get(&(nr, nc)) {
            Some('.') => do_move = Some(true),
            Some('#') => do_move = Some(false),
            Some(ch) if *ch == box_char => {
                move_stack.push((box_char, nr, nc));
                nr += dr;
                nc += dc;
                move_stack.push((closing_box_char, nr, nc));
            }
            Some(ch) => {
                panic!("Unrecognized char while moving horiz: {}", ch);
            }
            None => panic!("Moved horiz out of the map: {}, {}", nr, nc),
        }
        (wr, wc) = (nr, nc);
    }
    move_in_map(do_move, move_stack, map, (dr, dc), (sr, sc))
}

fn solve2(map: &mut HashMap<(isize, isize), char>, instrs: &Vec<char>) -> isize {
    let (mut rr, mut rc) = (0, 0);
    for ((r, c), ch) in map.iter() {
        if *ch == '@' {
            rr = *r;
            rc = *c;
            break;
        }
    }
    instrs.iter().for_each(|i| {
        match i {
            '^' => (rr, rc) = push_vert(map, (rr, rc), true),
            'v' => (rr, rc) = push_vert(map, (rr, rc), false),
            '<' => (rr, rc) = push_horiz(map, (rr, rc), true),
            '>' => (rr, rc) = push_horiz(map, (rr, rc), false),
            c => panic!("Unrecognized char: {}", c),
        };
    });
    map.iter()
        .map(|((r, c), ch)| match ch {
            '[' => 100 * r + c,
            _ => 0,
        })
        .sum::<isize>()
}

// fn print_map(map: &HashMap<(isize, isize), char>, doubled: bool) {
//     let (rows, cols);
//     if doubled {
//         rows = (map.len() as f64 / 2.).sqrt() as isize;
//         cols = rows * 2. as isize;
//     } else {
//         rows = (map.len() as f64).sqrt() as isize;
//         cols = rows;
//     }
//     (0..rows).for_each(|r| {
//         (0..cols).for_each(|c| {
//             print!("{}", map.get(&(r, c)).unwrap());
//         });
//         print!("\n");
//     });
// }

pub fn solve() {
    let result = fs::read_to_string("data/15.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut map_vecs: Vec<Vec<char>> = Vec::new();
    let mut instrs = Vec::new();
    let mut split = false;
    binding.as_str().lines().into_iter().for_each(|l| {
        if l.is_empty() {
            split = true;
        } else if split {
            instrs.extend(l.chars());
        } else {
            map_vecs.push(l.chars().collect());
        }
    });

    let mut map = map_vecs
        .iter()
        .enumerate()
        .flat_map(|(r, row)| {
            row.iter()
                .enumerate()
                .map(move |(c, ch)| ((r as isize, c as isize), *ch))
        })
        .collect();
    println!("2024.15.1: {}", solve1(&mut map, &instrs));

    let mut map2 = map_vecs
        .iter()
        .enumerate()
        .flat_map(|(r, row)| {
            row.iter().enumerate().flat_map(move |(c, ch)| match ch {
                '#' => [
                    ((r as isize, 2 * c as isize), '#'),
                    ((r as isize, 2 * c as isize + 1), '#'),
                ],
                '.' => [
                    ((r as isize, 2 * c as isize), '.'),
                    ((r as isize, 2 * c as isize + 1), '.'),
                ],
                '@' => [
                    ((r as isize, 2 * c as isize), '@'),
                    ((r as isize, 2 * c as isize + 1), '.'),
                ],
                'O' => [
                    ((r as isize, 2 * c as isize), '['),
                    ((r as isize, 2 * c as isize + 1), ']'),
                ],
                _ => panic!("Unrecognized char when doubling map: {}", ch),
            })
        })
        .collect();
    println!("2024.15.2: {}", solve2(&mut map2, &instrs));
}
