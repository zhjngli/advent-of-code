use std::fs;

use crate::common::transpose;

fn parse1(s: &str) -> (Vec<Vec<usize>>, Vec<char>) {
    let ls = s.lines().collect::<Vec<&str>>();
    let nums = ls[..ls.len() - 1]
        .iter()
        .map(|l| {
            l.split(' ')
                .filter(|c| *c != "")
                .map(|n| n.trim().parse().unwrap())
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<Vec<usize>>>();
    let ops: Vec<char> = ls[ls.len() - 1]
        .split(' ')
        .filter(|c| *c != "")
        .map(|c| c.chars().next().unwrap())
        .collect();
    nums.iter().for_each(|row| assert_eq!(row.len(), ops.len()));
    (nums, ops)
}

fn solve1(nums: &Vec<Vec<usize>>, ops: &Vec<char>) -> usize {
    ops.iter()
        .enumerate()
        .map(|(i, op)| {
            let col = nums.iter().map(|row| row[i]);
            match op {
                '+' => col.sum::<usize>(),
                '*' => col.product::<usize>(),
                _ => panic!("unknown op, {}", op),
            }
        })
        .sum()
}

fn parse2(input: &str) -> (Vec<Vec<usize>>, Vec<char>) {
    let ls = input.lines().collect::<Vec<&str>>();

    let (mut nums, curr) = transpose(
        ls[..ls.len() - 1]
            .iter()
            .map(|l| l.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>(),
    )
    .iter()
    .fold((Vec::new(), Vec::new()), |(mut all_nums, mut curr), row| {
        let s = row.iter().collect::<String>().trim().to_string();
        if s == "" {
            all_nums.push(curr);
            (all_nums, Vec::new())
        } else {
            curr.push(s.parse().unwrap());
            (all_nums, curr)
        }
    });
    nums.push(curr);

    let ops: Vec<char> = ls[ls.len() - 1]
        .split(' ')
        .filter(|c| *c != "")
        .map(|c| c.chars().next().unwrap())
        .collect();

    assert_eq!(nums.len(), ops.len());
    (nums, ops)
}

fn solve2(nums: &Vec<Vec<usize>>, ops: &Vec<char>) -> usize {
    nums.iter()
        .enumerate()
        .map(|(i, num_list)| match ops[i] {
            '+' => num_list.iter().sum::<usize>(),
            '*' => num_list.iter().product::<usize>(),
            _ => panic!("unknown op, {}", ops[i]),
        })
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/06.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "\n".to_string()
    });

    let (nums1, ops1) = parse1(binding.as_str());
    println!("2025.06.1: {}", solve1(&nums1, &ops1));
    let (nums2, ops2) = parse2(binding.as_str());
    println!("2025.06.2: {}", solve2(&nums2, &ops2));
}
