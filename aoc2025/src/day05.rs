use std::{
    cmp::{max, min},
    collections::HashSet,
    fs,
};

fn parse(s: &str) -> (Vec<(usize, usize)>, Vec<usize>) {
    let parts: Vec<&str> = s.split("\n\n").collect();
    let ranges = parts[0]
        .lines()
        .map(|l| {
            let nums: Vec<usize> = l.split('-').map(|n| n.trim().parse().unwrap()).collect();
            (nums[0], nums[1])
        })
        .collect();
    let ingredients = parts[1]
        .lines()
        .map(|l| l.trim().parse().unwrap())
        .collect();
    (ranges, ingredients)
}

fn solve1(ranges: &Vec<(usize, usize)>, ingredients: &Vec<usize>) -> usize {
    let mut fresh = HashSet::new();
    for i in ingredients {
        for (low, high) in ranges {
            if i >= low && i <= high {
                fresh.insert(i);
                break;
            }
        }
    }
    fresh.len()
}

fn solve2(ranges: &Vec<(usize, usize)>) -> usize {
    // new ranges sorted by low
    let mut merged_ranges = Vec::new();
    for (low, high) in ranges {
        let mut inserted = false;
        for i in 0..merged_ranges.len() {
            let (mlow, mhigh) = merged_ranges[i];
            if high + 1 < mlow {
                merged_ranges.insert(i, (*low, *high));
                inserted = true;
                break;
            } else if *low > mhigh + 1 {
                continue;
            } else {
                let new_low = min(*low, mlow);
                let new_high = max(*high, mhigh);
                merged_ranges[i] = (new_low, new_high);
                // merge consecutive ranges
                let mut j = i + 1;
                while j < merged_ranges.len() {
                    let (next_low, next_high) = merged_ranges[j];
                    if next_low > new_high + 1 {
                        break;
                    }
                    let merged_high = max(merged_ranges[i].1, next_high);
                    merged_ranges[i] = (merged_ranges[i].0, merged_high);
                    merged_ranges.remove(j);
                    j += 1;
                }
                inserted = true;
                break;
            }
        }
        if !inserted {
            merged_ranges.push((*low, *high));
        }
    }
    let mut total = 0;
    for (low, high) in merged_ranges {
        total += high - low + 1;
    }
    total
}

pub fn solve() {
    let result = fs::read_to_string("data/05.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });
    let (ranges, ingredients) = parse(binding.as_str());

    println!("2025.05.1: {}", solve1(&ranges, &ingredients));
    println!("2025.05.2: {}", solve2(&ranges));
}
