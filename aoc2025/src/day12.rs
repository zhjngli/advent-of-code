use std::fs;

// Each shape fits in a 3x3 grid, with some cells captured (marked with '#') and some not (marked with '.')
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Shape {
    not_in: Vec<(usize, usize)>,
}

impl Shape {
    fn area(&self) -> usize {
        9 - self.not_in.len()
    }

    // fn vertical_flip(&self) -> Self {
    //     let mut not_in = self
    //         .not_in
    //         .iter()
    //         .map(|(r, c)| (*r, 2 - *c))
    //         .collect::<Vec<(usize, usize)>>();
    //     not_in.sort();
    //     Shape { not_in }
    // }

    // fn rotate_cw(&self) -> Self {
    //     let mut not_in = self
    //         .not_in
    //         .iter()
    //         .map(|(r, c)| (*c, 2 - *r))
    //         .collect::<Vec<(usize, usize)>>();
    //     not_in.sort();
    //     Shape { not_in }
    // }

    // fn all_orientations(&self) -> HashSet<Shape> {
    //     let mut orientations = HashSet::new();
    //     let mut current = self.clone();
    //     for _ in 0..4 {
    //         orientations.insert(current.clone());
    //         orientations.insert(current.vertical_flip());
    //         current = current.rotate_cw();
    //     }
    //     orientations
    // }
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
            let mut not_in = Vec::new();
            block.split("\n").skip(1).enumerate().for_each(|(r, line)| {
                line.chars().enumerate().for_each(|(c, cell)| {
                    if cell == '.' {
                        not_in.push((r, c));
                    }
                });
            });
            Shape { not_in }
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

pub fn solve() {
    let result = fs::read_to_string("data/12.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let (shapes, regions) = parse(binding.as_str());

    println!("2025.12.1: {}", solve1(&shapes, &regions));
}
