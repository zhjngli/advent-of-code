use std::fs;

use nom::{
    bytes::complete::tag,
    character::complete::{i64, newline},
    combinator::map,
    multi::separated_list1,
    sequence::tuple,
    IResult,
};

#[derive(Debug, Clone)]
struct Robot {
    x: i64,
    y: i64,
    vx: i64,
    vy: i64,
}

fn parse_robot(s: &str) -> IResult<&str, Robot> {
    map(
        tuple((
            tag("p="),
            i64,
            tag(","),
            i64,
            tag(" v="),
            i64,
            tag(","),
            i64,
        )),
        |(_, x, _, y, _, vx, _, vy)| Robot {
            x: x,
            y: y,
            vx: vx,
            vy: vy,
        },
    )(s)
}

fn parse_robots(s: &str) -> IResult<&str, Vec<Robot>> {
    separated_list1(newline, parse_robot)(s)
}

const BOUND_X: i64 = 101;
const BOUND_Y: i64 = 103;

fn wrap(n: i64, bound: i64) -> i64 {
    if n < bound && n >= 0 {
        n
    } else if n < 0 {
        bound + n
    } else {
        n - bound
    }
}

fn step_robots(rs: &mut Vec<Robot>) {
    rs.iter_mut().for_each(|r| {
        r.x = wrap(r.x + r.vx, BOUND_X);
        r.y = wrap(r.y + r.vy, BOUND_Y);
    });
}

fn solve1(robots: &Vec<Robot>) -> i64 {
    let mut rs = robots.clone();
    (0..100).for_each(|_| {
        step_robots(&mut rs);
    });
    let (mut q1, mut q2, mut q3, mut q4) = (0, 0, 0, 0);
    let (mx, my) = (BOUND_X / 2, BOUND_Y / 2);
    rs.iter().for_each(|r| {
        if r.x < mx && r.x >= 0 && r.y < my && r.y >= 0 {
            q1 += 1;
        } else if r.x > mx && r.x < BOUND_X && r.y < my && r.y >= 0 {
            q2 += 1;
        } else if r.x < mx && r.x >= 0 && r.y > my && r.y < BOUND_Y {
            q3 += 1;
        } else if r.x > mx && r.x < BOUND_X && r.y > my && r.y < BOUND_Y {
            q4 += 1;
        }
    });
    q1 * q2 * q3 * q4
}

fn solve2(robots: &Vec<Robot>) {
    let min_consecutive = 10;
    let mut rs = robots.clone();
    (1..BOUND_X * BOUND_Y).for_each(|i| {
        step_robots(&mut rs);
        rs.sort_by(|r1, r2| match r1.y.cmp(&r2.y) {
            std::cmp::Ordering::Equal => r1.x.cmp(&r2.x),
            ord => ord,
        });
        let mut continuous = 1;
        let mut r = &rs[0];
        for ri in 1..rs.len() {
            let next_r = &rs[ri];
            if next_r.x == r.x + 1 && next_r.y == r.y {
                continuous += 1;
            } else {
                continuous = 1;
            }
            if continuous >= min_consecutive {
                println!("found {} consecutive robots at step {}", min_consecutive, i);
                let mut pic = [[' '; BOUND_X as usize]; BOUND_Y as usize];
                rs.iter().for_each(|r| {
                    pic[r.y as usize][r.x as usize] = 'X';
                });
                pic.iter().for_each(|row| {
                    row.iter().for_each(|ch| {
                        print!("{}", ch);
                    });
                    println!();
                });
                break;
            }
            r = next_r;
        }
    });
}

pub fn solve() {
    let result = fs::read_to_string("data/14.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let (_, robots) = parse_robots(binding.as_str()).unwrap();

    println!("2024.14.1: {}", solve1(&robots));
    println!("2024.14.2:");
    solve2(&robots);
}
