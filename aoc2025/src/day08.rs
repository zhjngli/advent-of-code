use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
    fmt::Display,
    fs,
    hash::{Hash, Hasher},
};

trait Distance {
    fn distance(&self, other: &Self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Point {
    x: isize,
    y: isize,
    z: isize,
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl Distance for Point {
    fn distance(&self, other: &Self) -> usize {
        ((self.x - other.x).pow(2) + (self.y - other.y).pow(2) + (self.z - other.z).pow(2)) as usize
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Circuit {
    points: HashSet<Point>,
}

impl Hash for Circuit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut points: Vec<&Point> = self.points.iter().collect();
        points.sort();
        for point in points.iter() {
            point.hash(state);
        }
    }
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
                z: coords[2],
            }
        })
        .collect()
}

fn distances(junctions: &Vec<Point>) -> Vec<(usize, &Point, &Point)> {
    let mut distances: Vec<(usize, &Point, &Point)> = Vec::new();
    for (i, j1) in junctions.iter().enumerate() {
        for (j, j2) in junctions.iter().enumerate() {
            if i >= j {
                continue;
            }
            let dist = j1.distance(j2);
            distances.push((dist, j1, j2));
        }
    }
    distances.sort();
    distances
}

fn solve1(junctions: &Vec<Point>) -> usize {
    let distances = distances(junctions);

    let mut circuits: HashMap<&Point, Circuit> = HashMap::new();
    for junction in junctions.iter() {
        circuits.insert(
            junction,
            Circuit {
                points: HashSet::from([*junction]),
            },
        );
    }
    let mut i = 0;
    while i < 1000 {
        let (_, j1, j2) = distances[i];
        let mut c1 = circuits.get(j1).unwrap().clone();
        if !c1.points.contains(j2) {
            let c2 = circuits.get(j2).unwrap().clone();
            c1.points.extend(c2.points.iter().cloned());
            for j in c1.points.iter() {
                circuits
                    .get_mut(j)
                    .unwrap()
                    .points
                    .extend(c1.points.iter().cloned());
            }
        }
        i += 1;
    }

    let mut deduped_circuits: HashSet<Circuit> = HashSet::new();
    for circuit in circuits.values() {
        deduped_circuits.insert(circuit.clone());
    }

    let mut circuit_sizes = deduped_circuits
        .iter()
        .map(|s| s.points.len())
        .collect::<Vec<usize>>();
    circuit_sizes.sort_by(|a, b| b.cmp(a));
    circuit_sizes[0] * circuit_sizes[1] * circuit_sizes[2]
}

fn solve2(junctions: &Vec<Point>) -> usize {
    let distances = distances(junctions);

    let mut circuits: Vec<Circuit> = Vec::new();
    let mut circuit_index: HashMap<Point, usize> = HashMap::new();
    for (i, junction) in junctions.iter().enumerate() {
        circuit_index.insert(junction.clone(), i);
        circuits.push(Circuit {
            points: HashSet::from([*junction]),
        });
    }
    let mut last_conn = None;
    for (d, j1, j2) in distances.iter() {
        let i1 = *circuit_index.get(j1).unwrap();
        let i2 = *circuit_index.get(j2).unwrap();
        if i1 != i2 {
            let c1 = min(i1, i2);
            let c2 = max(i1, i2);
            let mut circuit1 = circuits.get_mut(c1).unwrap().clone();
            circuit1
                .points
                .extend(circuits.get(c2).unwrap().points.iter().cloned());
            for j in circuit1.points.iter() {
                circuit_index.insert(*j, c1);
            }
            circuits[c1] = circuit1;
            last_conn = Some((*d, *j1, *j2));
        }
    }

    match last_conn {
        Some((_, j1, j2)) => (j1.x * j2.x) as usize,
        None => panic!("No connections made!"),
    }
}

pub fn solve() {
    let result = fs::read_to_string("data/08.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let junctions = parse(binding.as_str());

    println!("2025.08.1: {}", solve1(&junctions));
    println!("2025.08.2: {}", solve2(&junctions));
}
