use std::collections::HashSet;
use std::fs;

// Each shape fits in a 3x3 grid, with some cells captured (marked with '#') and some not (marked with '.')
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Shape {
    cells: Vec<(usize, usize)>,
}

impl Shape {
    fn area(&self) -> usize {
        self.cells.len()
    }

    fn vertical_flip(&self) -> Self {
        let mut cells = self
            .cells
            .iter()
            .map(|(r, c)| (*r, 2 - *c))
            .collect::<Vec<(usize, usize)>>();
        cells.sort();
        Shape { cells }
    }

    fn rotate_cw(&self) -> Self {
        let mut cells = self
            .cells
            .iter()
            .map(|(r, c)| (*c, 2 - *r))
            .collect::<Vec<(usize, usize)>>();
        cells.sort();
        Shape { cells }
    }

    fn all_orientations(&self) -> HashSet<Shape> {
        let mut orientations = HashSet::new();
        let mut current = self.clone();
        for _ in 0..4 {
            orientations.insert(current.clone());
            orientations.insert(current.vertical_flip());
            current = current.rotate_cw();
        }
        orientations
    }
}

#[derive(Debug, Clone, Copy)]
struct Region {
    width: usize,
    height: usize,
    shapes: [usize; 6],
}

fn parse(s: &str) -> ([Shape; 6], Vec<Region>) {
    let num_shapes = 6;
    let blocks = s.split("\n\n").collect::<Vec<&str>>();
    let shapes = blocks[..num_shapes]
        .iter()
        .map(|block| {
            let mut cells = Vec::new();
            block.split("\n").skip(1).enumerate().for_each(|(r, line)| {
                line.chars().enumerate().for_each(|(c, cell)| {
                    if cell == '#' {
                        cells.push((r, c));
                    }
                });
            });
            Shape { cells }
        })
        .collect::<Vec<Shape>>()
        .try_into()
        .unwrap();
    let regions = blocks[num_shapes]
        .split("\n")
        .map(|line| {
            let parts = line.split(" ").collect::<Vec<&str>>();
            let size = parts[0].split("x").collect::<Vec<&str>>();
            Region {
                width: size[0].parse::<usize>().unwrap(),
                height: size[1][..size[1].len() - 1].parse::<usize>().unwrap(),
                shapes: parts[1..]
                    .iter()
                    .map(|shape| shape.parse::<usize>().unwrap())
                    .collect::<Vec<usize>>()
                    .try_into()
                    .unwrap(),
            }
        })
        .collect::<Vec<Region>>();
    (shapes, regions)
}

fn solve1(shapes: &[Shape; 6], regions: &[Region]) -> usize {
    regions
        .iter()
        .filter(|region| {
            // area of shapes smaller than region size
            region.shapes
                .iter()
                .enumerate()
                .map(|(i, &num_shapes)| shapes[i].area() * num_shapes)
                .sum::<usize>()
                <= region.width * region.height ||
            // number of 3x3 blocks in region larger than number of shapes
            region.shapes
                .iter()
                .sum::<usize>() <= region.width / 3 * region.height / 3
        })
        .count()
}

type Placement = Vec<(usize, usize)>;

/// For a given shape type (all its orientations), generate every valid placement on a WxH grid.
/// Each placement is a sorted list of (row, col) cells. Deduplicated and sorted by first cell.
fn generate_placements(
    orientations: &HashSet<Shape>,
    width: usize,
    height: usize,
) -> Vec<Placement> {
    let mut seen = HashSet::new();
    let mut placements = Vec::new();
    for shape in orientations {
        for r in 0..height {
            for c in 0..width {
                let mut placed: Vec<(usize, usize)> = shape
                    .cells
                    .iter()
                    .map(|(dr, dc)| (r + dr, c + dc))
                    .collect();
                placed.sort();
                if placed.iter().all(|&(pr, pc)| pr < height && pc < width)
                    && seen.insert(placed.clone())
                {
                    placements.push(placed);
                }
            }
        }
    }
    placements.sort();
    placements
}

/// Backtracking solver with symmetry breaking for identical shape instances.
/// jobs[job_idx..] lists the shape index for each remaining instance to place.
/// min_first_cell enforces ordering among the same shape to avoid extra branching.
fn can_fit(
    grid: &mut Vec<Vec<bool>>,
    shape_placements: &[Vec<Placement>],
    jobs: &[usize],
    job_idx: usize,
    min_first_cell: usize,
    grid_width: usize,
) -> bool {
    if job_idx >= jobs.len() {
        return true;
    }

    let shape_idx = jobs[job_idx];
    let next_shape_is_same = job_idx + 1 < jobs.len() && jobs[job_idx + 1] == shape_idx;

    for placement in &shape_placements[shape_idx] {
        let first_cell = placement[0].0 * grid_width + placement[0].1;
        if first_cell < min_first_cell {
            continue;
        }

        if placement.iter().all(|&(r, c)| !grid[r][c]) {
            for &(r, c) in placement {
                grid[r][c] = true;
            }

            let next_min = if next_shape_is_same { first_cell + 1 } else { 0 };
            if can_fit(
                grid,
                shape_placements,
                jobs,
                job_idx + 1,
                next_min,
                grid_width,
            ) {
                for &(r, c) in placement {
                    grid[r][c] = false;
                }
                return true;
            }

            for &(r, c) in placement {
                grid[r][c] = false;
            }
        }
    }

    false
}

fn solve_algo_x(shapes: &[Shape; 6], regions: &[Region]) -> usize {
    let orientations: Vec<HashSet<Shape>> = shapes.iter().map(|s| s.all_orientations()).collect();
    regions
        .iter()
        .filter(|region| {
            let total_cells: usize = region
                .shapes
                .iter()
                .enumerate()
                .map(|(i, &count)| shapes[i].area() * count)
                .sum();
            if total_cells > region.width * region.height {
                return false;
            }

            let shape_placements: Vec<Vec<Placement>> = (0..6)
                .map(|i| {
                    if region.shapes[i] > 0 {
                        generate_placements(&orientations[i], region.width, region.height)
                    } else {
                        Vec::new()
                    }
                })
                .collect();

            // place shapes with fewer available placements first to increase pruning
            let mut shape_order: Vec<usize> = (0..6).filter(|&i| region.shapes[i] > 0).collect();
            shape_order.sort_by_key(|&i| shape_placements[i].len());

            // each job is a shape to be placed, grouped by shape type and ordered by shape_order for better pruning
            let jobs: Vec<usize> = shape_order
                .iter()
                .flat_map(|&i| std::iter::repeat(i).take(region.shapes[i]))
                .collect();

            let mut grid = vec![vec![false; region.width]; region.height];
            can_fit(&mut grid, &shape_placements, &jobs, 0, 0, region.width)
        })
        .count()
}

pub fn solve() {
    let result = fs::read_to_string("data/12.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let (shapes, regions) = parse(binding.as_str());

    println!("2025.12.1: {}", solve1(&shapes, &regions));
    println!("2025.12.1 (algo x): {}", solve_algo_x(&shapes, &regions));
}
