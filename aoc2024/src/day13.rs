use std::fs;

use nom::{
    bytes::complete::tag,
    character::complete::{i64, newline},
    combinator::map,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

#[derive(Debug)]
struct ClawMachine {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    px: i64,
    py: i64,
}

fn parse_claw_machine(s: &str) -> IResult<&str, ClawMachine> {
    map(
        tuple((
            tag("Button A: X+"),
            i64,
            tag(", Y+"),
            i64,
            newline,
            tag("Button B: X+"),
            i64,
            tag(", Y+"),
            i64,
            newline,
            tag("Prize: X="),
            i64,
            tag(", Y="),
            i64,
        )),
        |(_, ax, _, ay, _, _, bx, _, by, _, _, px, _, py)| ClawMachine {
            ax: ax,
            ay: ay,
            bx: bx,
            by: by,
            px: px,
            py: py,
        },
    )(s)
}

fn parse_claw_machines(s: &str) -> IResult<&str, Vec<ClawMachine>> {
    separated_list1(tag("\n\n"), parse_claw_machine)(s)
}

fn _solve(claw_machines: &Vec<ClawMachine>, max_presses: i64) -> i64 {
    claw_machines
        .iter()
        .map(|c| {
            let det = c.ax * c.by - c.ay * c.bx;
            match det {
                0 => {
                    // more than 1 solution
                    if c.ax as f64 / c.ay as f64 == c.bx as f64 / c.by as f64
                        && c.ax as f64 / c.ay as f64 == c.px as f64 / c.py as f64
                    {
                        // code here is actually wrong cause i need to cast a to float
                        // then check that a is an integer in the corresponding solution
                        // but apparently no cases given where there's more than 1 solution

                        // try max button b presses, and reduce to find cheapest
                        let mut b = max_presses;
                        let mut a = (c.px - b * c.bx) / c.ax;
                        while a > max_presses || a < 0 {
                            b -= 1;
                            a = (c.px - b * c.bx) / c.ax;
                            if b < 0 {
                                return 0;
                            }
                        }
                        3 * a + b
                    } else {
                        // no solution, can't win prize
                        0
                    }
                }
                _ => {
                    // unique solution
                    let a = (c.by * c.px - c.bx * c.py) as f64 / (c.ax * c.by - c.bx * c.ay) as f64;
                    let b = (c.ax * c.py - c.ay * c.px) as f64 / (c.ax * c.by - c.bx * c.ay) as f64;
                    // must be integer, max presses, min 0 presses
                    if a.fract() != 0.
                        || b.fract() != 0.
                        || a > max_presses as f64
                        || b > max_presses as f64
                        || a < 0.
                        || b < 0.
                    {
                        return 0;
                    }
                    3 * a as i64 + b as i64
                }
            }
        })
        .sum()
}

fn solve1(claw_machines: &Vec<ClawMachine>) -> i64 {
    _solve(claw_machines, 100)
}

fn solve2(claw_machines: &Vec<ClawMachine>) -> i64 {
    let new_machines = claw_machines
        .iter()
        .map(|c| ClawMachine {
            ax: c.ax,
            ay: c.ay,
            bx: c.bx,
            by: c.by,
            px: c.px + 10000000000000,
            py: c.py + 10000000000000,
        })
        .collect();
    _solve(&new_machines, 10000000000000)
}

pub fn solve() {
    let result = fs::read_to_string("data/13.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let (_, claw_machines) = parse_claw_machines(binding.as_str()).unwrap();

    println!("2024.13.1: {}", solve1(&claw_machines));
    println!("2024.12.2: {}", solve2(&claw_machines));
}
