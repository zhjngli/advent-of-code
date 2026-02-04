use std::{
    collections::{HashMap, HashSet},
    fs,
};

use z3::ast::Ast;

#[derive(Debug)]
struct Machine {
    lights: Vec<bool>,
    buttons: Vec<HashSet<usize>>,
    joltage: Vec<usize>,
}

fn parse(s: &str) -> Vec<Machine> {
    s.lines()
        .map(|line| {
            let data: Vec<&str> = line.split(' ').collect();
            Machine {
                lights: data[0][1..data[0].len() - 1]
                    .chars()
                    .map(|c| c == '#')
                    .collect(),
                buttons: data[1..data.len() - 1]
                    .iter()
                    .map(|d| {
                        d[1..d.len() - 1]
                            .split(',')
                            .map(|n| n.trim().parse().unwrap())
                            .collect()
                    })
                    .collect(),
                joltage: data[data.len() - 1][1..data[data.len() - 1].len() - 1]
                    .split(',')
                    .map(|n| n.trim().parse().unwrap())
                    .collect(),
            }
        })
        .collect()
}

fn solve1(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            // all possible combinations of buttons to press
            let mut all_combinations = vec![Vec::new()];
            for bi in 0..m.buttons.len() {
                let mut new_combinations = Vec::new();
                for combo in all_combinations.iter() {
                    new_combinations.push(combo.clone());

                    let mut combo_with_button = combo.clone();
                    combo_with_button.push(bi);
                    new_combinations.push(combo_with_button);
                }
                all_combinations = new_combinations;
            }

            let mut min_presses = m.buttons.len();
            for combo in all_combinations {
                let mut lights = vec![false; m.lights.len()];
                for bi in &combo {
                    for li in &m.buttons[*bi] {
                        lights[*li] = !lights[*li];
                    }
                }
                if lights == m.lights {
                    min_presses = min_presses.min(combo.len());
                }
            }
            min_presses
        })
        .sum()
}

fn solve2_rec(
    parity_to_combo_map: &HashMap<Vec<bool>, Vec<(Vec<usize>, Vec<usize>)>>,
    num_lights: usize,
    joltage: Vec<usize>,
    buttons: &Vec<HashSet<usize>>,
    memo: &mut HashMap<Vec<usize>, Option<usize>>,
) -> Option<usize> {
    if joltage.iter().all(|&j| j == 0) {
        return Some(0);
    }
    if let Some(&result) = memo.get(&joltage) {
        return result;
    }

    let joltage_parity: Vec<bool> = joltage.iter().map(|j| j % 2 == 1).collect();
    if !parity_to_combo_map.contains_key(&joltage_parity) {
        panic!("No combos for joltage_parity: {:?}", joltage_parity);
    }
    let parity_combos = parity_to_combo_map.get(&joltage_parity).unwrap();

    let mut min_cost: Option<usize> = None;
    for (combo, contribution) in parity_combos {
        if (0..joltage.len()).any(|i| contribution[i] > joltage[i]) {
            continue;
        }

        let mut remaining_joltage = Vec::new();
        for i in 0..joltage.len() {
            remaining_joltage.push((joltage[i] - contribution[i]) / 2);
        }

        let recursive_cost = solve2_rec(
            parity_to_combo_map,
            num_lights,
            remaining_joltage.clone(),
            buttons,
            memo,
        );

        if let Some(rec_cost) = recursive_cost {
            let cost = combo.len() + 2 * rec_cost;
            min_cost = Some(match min_cost {
                Some(mc) => mc.min(cost),
                None => cost,
            });
        }
    }

    memo.insert(joltage.to_vec(), min_cost);
    min_cost
}

// this is a lot slower than the alt method, not sure why yet
fn solve2(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            // all possible combinations of buttons to press
            let mut all_combinations = vec![Vec::new()];
            for bi in 0..m.buttons.len() {
                let mut new_combinations = Vec::new();
                for combo in all_combinations.iter() {
                    new_combinations.push(combo.clone());

                    let mut combo_with_button = combo.clone();
                    combo_with_button.push(bi);
                    new_combinations.push(combo_with_button);
                }
                all_combinations = new_combinations;
            }

            // precalculate all possible parity patterns and map it to combos that produce it
            let mut parity_to_combo_map: HashMap<Vec<bool>, Vec<(Vec<usize>, Vec<usize>)>> =
                HashMap::new();
            for i in 0..(1 << m.lights.len()) {
                // calculate parity pattern
                let mut parity_pattern = Vec::new();
                for l in 0..m.lights.len() {
                    parity_pattern.push((i >> l) & 1 == 1);
                }

                // calculate all combos that produce this parity pattern
                let mut parity_combos = Vec::new();
                for combo in &all_combinations {
                    let mut lights = vec![false; m.lights.len()];
                    for bi in combo {
                        for li in &m.buttons[*bi] {
                            lights[*li] = !lights[*li];
                        }
                    }
                    if lights == parity_pattern {
                        let mut contribution = vec![0; m.joltage.len()];
                        for bi in combo {
                            for li in &m.buttons[*bi] {
                                contribution[*li] += 1;
                            }
                        }
                        parity_combos.push((combo.clone(), contribution));
                    }
                }

                parity_to_combo_map.insert(parity_pattern, parity_combos);
            }

            let mut memo = HashMap::new();
            match solve2_rec(
                &parity_to_combo_map,
                m.lights.len(),
                m.joltage.clone(),
                &m.buttons,
                &mut memo,
            ) {
                Some(cost) => cost,
                None => panic!(
                    "No solution found for {}, {:?}, {:?}\n\n\n",
                    show_lights(&m.lights),
                    m.buttons,
                    m.joltage
                ),
            }
        })
        .sum()
}

fn show_lights(lights: &Vec<bool>) -> String {
    let mut s = String::new();
    s.push('[');
    for b in lights {
        s.push(if *b { '#' } else { '.' });
    }
    s.push(']');
    s
}

fn solve2_z3(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            let ctx = z3::Context::new(&z3::Config::new());
            let optimizer = z3::Optimize::new(&ctx);

            let vars: Vec<z3::ast::Int> = (0..m.buttons.len())
                .map(|i| z3::ast::Int::new_const(&ctx, format!("x{}", i)))
                .collect();
            for v in vars.iter() {
                optimizer.assert(&v.ge(&z3::ast::Int::from_u64(&ctx, 0)));
            }

            for i in 0..m.joltage.len() {
                let joltagei = &z3::ast::Int::from_u64(&ctx, m.joltage[i] as u64);
                let mut sum = z3::ast::Int::from_u64(&ctx, 0);
                for (bi, button) in m.buttons.iter().enumerate() {
                    for j in button.iter() {
                        if *j == i {
                            sum += &vars[bi];
                        }
                    }
                }
                optimizer.assert(&sum._eq(joltagei));
            }

            let mut total_presses = z3::ast::Int::from_u64(&ctx, 0);
            for v in vars.iter() {
                total_presses += v;
            }
            optimizer.minimize(&total_presses);

            match optimizer.check(&[]) {
                z3::SatResult::Sat => {
                    let model = optimizer.get_model().unwrap();
                    let mut v_presses = Vec::new();
                    for v in vars.iter() {
                        let presses = model.eval(v, true).unwrap();
                        v_presses.push(presses.as_u64().unwrap() as usize);
                    }
                    let total_presses_num = model.eval(&total_presses, true).unwrap();
                    println!(
                        "\n\nSat: {}, {:?}, {:?}",
                        show_lights(&m.lights),
                        m.buttons,
                        m.joltage
                    );
                    println!("button presses: {:?}", v_presses);
                    println!("total presses: {}", total_presses_num);

                    total_presses_num.as_u64().unwrap() as usize
                }
                z3::SatResult::Unsat => panic!(
                    "Unsat: {}, {:?}, {:?}",
                    show_lights(&m.lights),
                    m.buttons,
                    m.joltage
                ),
                z3::SatResult::Unknown => panic!(
                    "Unknown: {}, {:?}, {:?}",
                    show_lights(&m.lights),
                    m.buttons,
                    m.joltage
                ),
            }
        })
        .sum()
}

fn parity_to_combo_map(
    buttons: &Vec<HashSet<usize>>,
    num_switches: usize,
) -> HashMap<Vec<bool>, HashMap<Vec<usize>, usize>> {
    let num_buttons = buttons.len();
    let mut parity_to_combo_map: HashMap<Vec<bool>, HashMap<Vec<usize>, usize>> = HashMap::new();
    for i in 0..(1 << num_switches) {
        let mut parity_pattern = Vec::new();
        for j in 0..num_switches {
            parity_pattern.push((i >> j) & 1 == 1);
        }
        parity_to_combo_map.insert(parity_pattern, HashMap::new());
    }

    // For each number of buttons to press (0 to num_buttons)
    for num_pressed in 0..=num_buttons {
        // Generate all combinations of size num_pressed from buttons
        // Using bit manipulation to enumerate combinations
        generate_combinations(num_buttons, num_pressed, |button_indices| {
            let mut pattern = vec![0; num_switches];
            for &btn_idx in button_indices {
                for &switch_idx in &buttons[btn_idx] {
                    pattern[switch_idx] += 1;
                }
            }
            let parity_pattern: Vec<bool> = pattern.iter().map(|&x| x % 2 == 1).collect();
            parity_to_combo_map
                .get_mut(&parity_pattern)
                .unwrap()
                .entry(pattern)
                .or_insert(num_pressed);
        });
    }

    parity_to_combo_map
}

// Helper to generate combinations - calls callback for each combination
fn generate_combinations<F>(n: usize, k: usize, mut callback: F)
where
    F: FnMut(&Vec<usize>),
{
    let mut combo = vec![0; k];
    if k == 0 {
        callback(&vec![]);
        return;
    }

    fn generate_rec<F>(
        n: usize,
        k: usize,
        start: usize,
        combo: &mut Vec<usize>,
        pos: usize,
        callback: &mut F,
    ) where
        F: FnMut(&Vec<usize>),
    {
        if pos == k {
            callback(combo);
            return;
        }
        for i in start..n {
            combo[pos] = i;
            generate_rec(n, k, i + 1, combo, pos + 1, callback);
        }
    }

    generate_rec(n, k, 0, &mut combo, 0, &mut callback);
}

fn solve2_alt_rec(
    joltage: &Vec<usize>,
    parity_pattern_costs: &HashMap<Vec<bool>, HashMap<Vec<usize>, usize>>,
    memo: &mut HashMap<Vec<usize>, usize>,
) -> usize {
    if joltage.iter().all(|&x| x == 0) {
        return 0;
    }
    if let Some(&result) = memo.get(joltage) {
        return result;
    }
    let mut min_cost = 1_000_000;

    let parity_pattern: Vec<bool> = joltage.iter().map(|&j| j % 2 == 1).collect();
    if !parity_pattern_costs.contains_key(&parity_pattern) {
        panic!("No patterns for parity_pattern: {:?}", parity_pattern);
    }

    for (buttons_to_press, &num_presses) in parity_pattern_costs.get(&parity_pattern).unwrap() {
        if buttons_to_press
            .iter()
            .zip(joltage.iter())
            .any(|(p, g)| p > g)
        {
            continue;
        }
        let remaining_joltage: Vec<usize> = joltage
            .iter()
            .zip(buttons_to_press.iter())
            .map(|(g, p)| (g - p) / 2)
            .collect();

        let recursive_cost = solve2_alt_rec(&remaining_joltage, parity_pattern_costs, memo);
        min_cost = min_cost.min(num_presses + 2 * recursive_cost);
    }

    memo.insert(joltage.clone(), min_cost);
    min_cost
}

fn solve2_alt(machines: &Vec<Machine>) -> usize {
    machines
        .iter()
        .map(|m| {
            let num_switches = &m.joltage.len();
            let parity_pattern_costs = parity_to_combo_map(&m.buttons, *num_switches);
            let mut memo: HashMap<Vec<usize>, usize> = HashMap::new();

            solve2_alt_rec(&m.joltage, &parity_pattern_costs, &mut memo)
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/10.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let machines = parse(binding.as_str());

    println!("2025.10.1: {}", solve1(&machines));
    println!("2025.10.2: {}", solve2(&machines));
    // println!("2025.10.2 alt: {}", solve2_alt(&machines));
    // println!("2025.10.2 z3: {}", solve2_z3(&machines));
}
