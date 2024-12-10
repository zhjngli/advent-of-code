use std::{collections::VecDeque, fs};

use crate::day09::HardDriveBlock::{Empty, Id};

#[derive(Debug, Clone, PartialEq, Eq)]
enum HardDriveBlock {
    Id(u64),
    Empty,
}

fn solve1(disk_map: &Vec<u32>) -> u64 {
    let (mut hard_drive, _) = disk_map
        .chunks(2)
        .filter_map(|chunk| {
            if chunk.len() == 2 {
                Some((chunk[0], chunk[1]))
            } else {
                Some((chunk[0], 0)) // no empty blocks at the end
            }
        })
        .fold(
            (VecDeque::new(), 0),
            |(mut hd, id), (file_blocks, empty_blocks)| {
                (0..file_blocks).for_each(|_| {
                    hd.push_back(Id(id));
                });
                (0..empty_blocks).for_each(|_| {
                    hd.push_back(Empty);
                });
                (hd, id + 1)
            },
        );
    let mut defragged = Vec::new();
    while hard_drive.len() > 0 {
        let block = hard_drive.pop_front();
        match block {
            Some(Id(i)) => defragged.push(i),
            Some(Empty) => {
                let mut end_block = hard_drive.pop_back();
                while end_block == Some(Empty) {
                    end_block = hard_drive.pop_back();
                }
                match end_block {
                    Some(Id(j)) => defragged.push(j),
                    Some(Empty) => panic!("got Empty when popping end of hard drive"),
                    None => (), // hard_drive fully processed
                }
            }
            None => panic!("got None when popping front of hard drive"),
        }
    }
    defragged
        .iter()
        .enumerate()
        .map(|(i, id)| i as u64 * id)
        .sum()
}

fn solve2(disk_map: &Vec<u32>) -> u64 {
    let (mut hard_drive, _) = disk_map
        .chunks(2)
        .filter_map(|chunk| {
            if chunk.len() == 2 {
                Some((chunk[0], chunk[1]))
            } else {
                Some((chunk[0], 0)) // no empty blocks at the end
            }
        })
        .fold(
            (VecDeque::new(), 0),
            |(mut hd, id), (file_blocks, empty_blocks)| {
                if file_blocks != 0 {
                    hd.push_back((Id(id), file_blocks));
                }
                if empty_blocks != 0 {
                    hd.push_back((Empty, empty_blocks));
                }
                (hd, id + 1)
            },
        );

    let mut end_block_index = hard_drive.len() as i64 - 1;
    while end_block_index >= 0 {
        let end_block = hard_drive[end_block_index as usize].clone();
        match end_block {
            (Id(end_id), end_size) => {
                for i in 0..end_block_index as usize {
                    let front_block = hard_drive[i].clone();
                    match front_block {
                        (Id(_), _) => (),
                        (Empty, front_size) => {
                            if end_size > front_size {
                                continue;
                            }
                            // move end block to empty space
                            hard_drive.remove(end_block_index as usize);
                            // replace end block with empty space
                            // don't need to merge surrounding empty spaces since we always look left
                            hard_drive.insert(end_block_index as usize, (Empty, end_size));
                            hard_drive.insert(i, (Id(end_id), end_size));
                            // remove empty block
                            hard_drive.remove(i + 1);
                            // add leftover empty block
                            if end_size < front_size {
                                hard_drive.insert(i + 1, (Empty, front_size - end_size));
                                end_block_index += 1;
                            }

                            break;
                        }
                    }
                }
            }
            (Empty, _) => (),
        }
        end_block_index -= 1;
    }

    let mut defragged = Vec::new();
    hard_drive.iter().for_each(|(block, size)| {
        let push_id;
        match block {
            Id(i) => push_id = *i,
            Empty => push_id = 0,
        }
        (0..*size).for_each(|_| defragged.push(push_id));
    });
    defragged
        .iter()
        .enumerate()
        .map(|(i, id)| i as u64 * *id)
        .sum()
}

pub fn solve() {
    let result = fs::read_to_string("data/09.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });
    let disk_map: Vec<u32> = binding
        .as_str()
        .lines()
        .flat_map(|l| l.chars().into_iter().map(|c| c.to_digit(10).unwrap()))
        .collect();

    println!("2024.09.1: {}", solve1(&disk_map));
    println!("2024.09.2: {}", solve2(&disk_map));
}
