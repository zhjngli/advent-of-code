use std::{cmp::max, fs};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
struct Point {
    x: isize,
    y: isize,
}

fn parse(s: &str) -> Vec<Point> {
    s.lines()
        .map(|line| {
            let coords: Vec<isize> = line
                .split(',')
                .map(|num| num.trim().parse::<isize>().unwrap())
                .collect();
            Point {
                x: coords[0],
                y: coords[1],
            }
        })
        .collect()
}

fn solve1(corners: &Vec<Point>) -> usize {
    let mut max_area = 0;
    for (i, j1) in corners.iter().enumerate() {
        for (j, j2) in corners.iter().enumerate() {
            if i >= j {
                continue;
            }
            let area = (((j1.x - j2.x).abs() + 1) * ((j1.y - j2.y).abs() + 1)) as usize;
            max_area = max(max_area, area);
        }
    }
    max_area
}

pub fn solve() {
    let result = fs::read_to_string("data/09.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let corners = parse(binding.as_str());

    println!("2025.09.1: {}", solve1(&corners));
}
