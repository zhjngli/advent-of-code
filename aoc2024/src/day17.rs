use std::fs;

fn combo_operand(op: usize, registers: &Vec<usize>) -> usize {
    match op {
        0 | 1 | 2 | 3 => op,
        4 | 5 | 6 => registers[op - 4],
        7 => panic!("Combo operand 7 will not appear in valid programs"),
        instr => panic!("Unrecognized instruction: {}", instr),
    }
}

fn run(registers: &mut Vec<usize>, program: &Vec<usize>) -> Vec<usize> {
    let mut out: Vec<usize> = Vec::new();
    let mut i: usize = 0;
    while i < program.len() {
        let lit_op = program[i + 1];
        match program[i as usize] {
            0 => {
                // adv
                let num = registers[0];
                let combo = combo_operand(lit_op, registers);
                registers[0] = num >> combo;
                i += 2;
            }
            1 => {
                // bxl
                registers[1] ^= lit_op;
                i += 2;
            }
            2 => {
                // bst
                let combo = combo_operand(lit_op, registers);
                registers[1] = combo % 8;
                i += 2;
            }
            3 => {
                // jnz
                match registers[0] {
                    0 => i += 2,
                    _ => i = lit_op,
                }
            }
            4 => {
                // bxc
                registers[1] ^= registers[2];
                i += 2;
            }
            5 => {
                // out
                let combo = combo_operand(lit_op, registers);
                out.push(combo % 8);
                i += 2;
            }
            6 => {
                // bdv
                let num = registers[0];
                let combo = combo_operand(lit_op, registers);
                registers[1] = num >> combo;
                i += 2;
            }
            7 => {
                // cdv
                let num = registers[0];
                let combo = combo_operand(lit_op, registers);
                registers[2] = num >> combo;
                i += 2;
            }
            instr => panic!("Unrecognized instruction: {}", instr),
        }
    }
    out
}

fn solve1(registers: &mut Vec<usize>, program: &Vec<usize>) -> String {
    run(registers, program)
        .iter()
        .map(usize::to_string)
        .collect::<Vec<_>>()
        .join(",")
}

fn solve2(registers: &mut Vec<usize>, program: &Vec<usize>) -> usize {
    let mut a = 8_usize.pow((program.len() - 1) as u32);
    let starting_power = (program.len() - 2) as u32;
    let mut power = starting_power;

    registers[0] = a;
    let mut out = run(registers, program);
    while out != *program {
        a += 8_usize.pow(power);
        registers[0] = a;
        out = run(registers, program);

        let mut i = program.len() - 1;
        let mut matched: usize = 0;
        while out[i] == program[i] && i > 0 {
            i -= 1;
            matched += 1;
            power = starting_power.checked_sub(matched as u32).unwrap_or(0);
        }
    }
    a
}

pub fn solve() {
    let result = fs::read_to_string("data/17.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let mut registers = Vec::new();
    let mut program = Vec::new();
    let mut split = false;
    binding.as_str().lines().into_iter().for_each(|l| {
        if l.is_empty() {
            split = true;
        } else if split {
            let ps: Vec<_> = l.split(": ").collect::<Vec<&str>>()[1]
                .split(',')
                .map(|ch| ch.parse::<usize>().unwrap())
                .collect();
            program.extend(ps);
        } else {
            let r = l.split(": ").collect::<Vec<&str>>()[1];
            registers.push(r.parse::<usize>().unwrap());
        }
    });
    println!("2024.17.1: {}", solve1(&mut registers, &program));
    println!("2024.17.2: {}", solve2(&mut registers, &program));
}
