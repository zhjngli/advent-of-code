use core::panic;
use std::{
    cmp::{max, min},
    fs,
};

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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct VEdge {
    x: isize,
    ymin: isize,
    ymax: isize,
    left: bool, // interior of polygon is left from edge
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
struct HEdge {
    y: isize,
    xmin: isize,
    xmax: isize,
    down: bool, // interior of polygon is down from edge
}

fn is_rectangle_covered(
    c1: &Point,
    c2: &Point,
    v_edges: &Vec<VEdge>,
    h_edges: &Vec<HEdge>,
) -> bool {
    let xmin = min(c1.x, c2.x);
    let xmax = max(c1.x, c2.x);
    let ymin = min(c1.y, c2.y);
    let ymax = max(c1.y, c2.y);
    if xmin == xmax || ymin == ymax {
        return true;
    }
    let mut v_overlaps = Vec::new();
    let mut h_overlaps = Vec::new();
    // println!("rectangle: ({}, {}) to ({}, {})", xmin, ymin, xmax, ymax);
    for v in v_edges.iter() {
        // edge doesn't overlap rectangle in y direction
        if v.ymax <= ymin || v.ymin >= ymax {
            continue;
        }
        // edge is strictly inside rectangle (not on boundary)
        if xmin < v.x && v.x < xmax {
            v_overlaps.push(v);
        }
        // edge is on left boundary - polygon interior is outside rectangle
        else if v.x == xmin && v.left {
            v_overlaps.push(v);
        }
        // edge is on right boundary - polygon interior is outside rectangle
        else if v.x == xmax && !v.left {
            v_overlaps.push(v);
        }
    }
    for h in h_edges.iter() {
        // edge doesn't overlap rectangle in x direction
        if h.xmax <= xmin || h.xmin >= xmax {
            continue;
        }
        // edge is strictly inside rectangle (not on boundary)
        if ymin < h.y && h.y < ymax {
            h_overlaps.push(h);
        }
        // edge is on bottom boundary - polygon interior is outside rectangle
        else if h.y == ymin && h.down {
            h_overlaps.push(h);
        }
        // edge is on top boundary - polygon interior is outside rectangle
        else if h.y == ymax && !h.down {
            h_overlaps.push(h);
        }
    }
    if !v_overlaps.is_empty() || !h_overlaps.is_empty() {
        return false;
    }
    true
}

fn solve2(corners: &Vec<Point>) -> usize {
    // determine signed area using shoelace formula. then we know the CCW or CW traversal of the points
    let mut area = 0;
    for i in 0..corners.len() - 1 {
        area += corners[i].x * corners[i + 1].y - corners[i + 1].x * corners[i].y;
    }
    let is_ccw = area > 0;

    // determine edges and which side the polygon is on
    let mut h_edges = Vec::new();
    let mut v_edges = Vec::new();
    for i in 0..corners.len() {
        let c = &corners[i];
        let next = if i == corners.len() - 1 {
            &corners[0]
        } else {
            &corners[i + 1]
        };
        let dx = next.x - c.x;
        let dy = next.y - c.y;

        if dx == 0 {
            let is_left = (is_ccw && dy > 0) || (!is_ccw && dy < 0);
            v_edges.push(VEdge {
                x: c.x,
                ymin: min(c.y, next.y),
                ymax: max(c.y, next.y),
                left: is_left,
            });
        } else if dy == 0 {
            let is_down = (is_ccw && dx < 0) || (!is_ccw && dx > 0);
            h_edges.push(HEdge {
                y: c.y,
                xmin: min(c.x, next.x),
                xmax: max(c.x, next.x),
                down: is_down,
            });
        } else {
            panic!("non-axis aligned edge: {:?} to {:?}", c, next);
        }
    }

    let mut max_area = 0;
    for (i, c1) in corners.iter().enumerate() {
        for (j, c2) in corners.iter().enumerate() {
            if i >= j {
                continue;
            }
            if is_rectangle_covered(c1, c2, &v_edges, &h_edges) {
                let area = (((c1.x - c2.x).abs() + 1) * ((c1.y - c2.y).abs() + 1)) as usize;
                max_area = max(max_area, area);
            }
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
    println!("2025.09.2: {}", solve2(&corners)); // 4633717343
}
