use std::{cmp::Ordering, collections::HashMap, fs};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Gate {
    XOR,
    AND,
    OR,
}

#[derive(Debug, Clone)]
struct Wire {
    r1: String,
    g: Gate,
    r2: String,
    r3: String,
}

fn gate_result(b1: usize, b2: usize, g: Gate) -> usize {
    match g {
        Gate::XOR => {
            if (b1 == 1 && b2 == 0) || (b1 == 0 && b2 == 1) {
                1
            } else {
                0
            }
        }
        Gate::AND => {
            if b1 == 1 && b2 == 1 {
                1
            } else {
                0
            }
        }
        Gate::OR => {
            if b1 == 1 || b2 == 1 {
                1
            } else {
                0
            }
        }
    }
}

fn get_num(registers: &HashMap<String, usize>, c: char) -> usize {
    let mut n = 0;
    let mut i = 0;
    while let Some(b) = registers.get(&format!("{}{:02}", c, i)) {
        n += b * 2_usize.pow(i);
        i += 1;
    }
    n
}

fn solve_wires(rs: &HashMap<String, usize>, wires: &Vec<Wire>) -> usize {
    let mut registers = rs.clone();

    let mut iter_wires = wires.clone();
    while iter_wires.len() > 0 {
        let mut new_wires = Vec::new();
        for w in iter_wires.iter() {
            match (registers.get(&w.r1), registers.get(&w.r2)) {
                (Some(b1), Some(b2)) => {
                    registers.insert(w.r3.clone(), gate_result(*b1, *b2, w.g));
                }
                _ => new_wires.push(w.clone()),
            }
        }
        iter_wires = new_wires.clone();
    }

    get_num(&registers, 'z')
}

fn solve1(registers: &HashMap<String, usize>, wires: &Vec<Wire>) -> usize {
    solve_wires(registers, wires)
}

fn solve2(wires: &Vec<Wire>) -> String {
    let mut bad_regs = Vec::new();

    let mut last_z_bit = 0;
    for w in wires.iter() {
        if w.r3.starts_with('z') {
            let i = w.r3[1..].parse::<isize>().unwrap();
            if i > last_z_bit {
                last_z_bit = i;
            }
        }
    }

    // https://en.wikipedia.org/wiki/Adder_(electronics)#/media/File:Fulladder.gif
    for w in wires.iter() {
        if w.r3.starts_with('z') && w.r3 != format!("z{}", last_z_bit) {
            // z bits are always calculated from an XOR, except for the last z bit, which is a carry over bit
            if w.g != Gate::XOR {
                bad_regs.push(w.r3.clone());
            }
        } else if !w.r3.starts_with('z')
            && (!w.r1.starts_with('x') && !w.r2.starts_with('y'))
            && (!w.r1.starts_with('y') && !w.r2.starts_with('x'))
        {
            // if the input bits are not x and y and if the output is not a z bit, then the gate must not be XOR
            if w.g == Gate::XOR {
                bad_regs.push(w.r3.clone());
            }
        } else if (w.r1.starts_with('x') && w.r2.starts_with('y'))
            || (w.r1.starts_with('y') && w.r2.starts_with('x'))
        {
            if w.r1.ends_with("00") || w.r2.ends_with("00") {
                continue;
            }
            // except for x00 and y00 which is the first input with no carry bit input
            // xXX AND yXX -> reg means reg must be ORed in another wire
            // xXX XOR yXX -> reg means reg must be XORed in another wire
            let mut out_gates = Vec::new();
            for d in wires.iter() {
                if d.r1 == w.r3 || d.r2 == w.r3 {
                    out_gates.push(d.g);
                }
            }
            if (w.g == Gate::XOR && !out_gates.contains(&Gate::XOR))
                || (w.g == Gate::AND && !out_gates.contains(&Gate::OR))
            {
                bad_regs.push(w.r3.clone());
            }
        }
    }

    bad_regs.sort();
    bad_regs.join(",")
}

fn cmp_register(r1: &str, r2: &str) -> Ordering {
    match (r1.chars().next(), r2.chars().next()) {
        (Some('x'), Some('x')) => r1.cmp(r2),
        (Some('x'), Some('y')) => r1.cmp(r2),
        (Some('x'), Some('z')) => r1.cmp(r2),
        (Some('x'), Some(_)) => Ordering::Less,
        (Some('y'), Some('x')) => r1.cmp(r2),
        (Some('y'), Some('y')) => r1.cmp(r2),
        (Some('y'), Some('z')) => r1.cmp(r2),
        (Some('y'), Some(_)) => Ordering::Less,
        (Some('z'), Some('x')) => r1.cmp(r2),
        (Some('z'), Some('y')) => r1.cmp(r2),
        (Some('z'), Some('z')) => r1.cmp(r2),
        (Some('z'), Some(_)) => Ordering::Greater,
        (Some(_), Some('x')) => Ordering::Greater,
        (Some(_), Some('y')) => Ordering::Greater,
        (Some(_), Some('z')) => Ordering::Less,
        (Some(_), Some(_)) => r1.cmp(r2),
        (None, Some(_)) => Ordering::Less,
        (Some(_), None) => Ordering::Greater,
        (None, None) => Ordering::Equal,
    }
}

pub fn solve() {
    let result = fs::read_to_string("data/24.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut registers = HashMap::new();
    let mut wires = Vec::new();
    let mut split = false;
    binding.as_str().lines().into_iter().for_each(|l| {
        if l.is_empty() {
            split = true;
        } else if split {
            let wire: Vec<_> = l.split(" ").collect::<Vec<&str>>();
            wires.push(Wire {
                r1: wire[0].to_string(),
                g: match wire[1] {
                    "XOR" => Gate::XOR,
                    "AND" => Gate::AND,
                    "OR" => Gate::OR,
                    s => panic!("Unrecognized gate: {}", s),
                },
                r2: wire[2].to_string(),
                r3: wire[4].to_string(),
            });
        } else {
            let r = l.split(": ").collect::<Vec<&str>>();
            registers.insert(r[0].to_string(), r[1].parse::<usize>().unwrap());
        }
    });
    wires.sort_by(|a, b| match cmp_register(&a.r1, &b.r1) {
        Ordering::Equal => match cmp_register(&a.r2, &b.r2) {
            Ordering::Equal => cmp_register(&a.r3, &b.r3),
            c => c,
        },
        c => c,
    });
    println!("2024.24.1: {}", solve1(&registers, &wires));
    println!("2024.24.2: {}", solve2(&wires));
}
