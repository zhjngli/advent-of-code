use std::{
    collections::{HashMap, HashSet},
    fs,
};

fn next_secret(secret: usize) -> usize {
    let mut one = secret * 64;
    one ^= secret;
    one %= 16777216;
    let mut two = one / 32;
    two ^= one;
    two %= 16777216;
    let mut fin = two * 2048;
    fin ^= two;
    fin %= 16777216;
    fin
}

fn solve1(inits: &Vec<usize>) -> usize {
    inits
        .iter()
        .map(|s| {
            let mut fin = *s;
            (0..2000).for_each(|_| {
                fin = next_secret(fin);
            });
            fin
        })
        .sum()
}

fn solve2(inits: &Vec<usize>) -> usize {
    let buyer_prices: Vec<Vec<usize>> = inits
        .iter()
        .map(|s| {
            let mut seq = Vec::from([*s]);
            let mut fin = *s;
            (0..2000).for_each(|_| {
                fin = next_secret(fin);
                seq.push(fin);
            });
            seq.iter().map(|s| s % 10).collect()
        })
        .collect();
    let buyer_changes: Vec<Vec<isize>> = buyer_prices
        .iter()
        .map(|ps| {
            ps.windows(2)
                .map(|w| w[1] as isize - w[0] as isize)
                .collect()
        })
        .collect();
    let change_seq_len = 4;
    let buyer_change_seqs: Vec<Vec<&[isize]>> = buyer_changes
        .iter()
        .map(|c| c.windows(change_seq_len).collect::<Vec<&[isize]>>())
        .collect();
    let mut bananas = HashMap::new();
    for (buyer_i, seqs) in buyer_change_seqs.iter().enumerate() {
        let mut seen_seqs = HashSet::new();
        for (seq_i, seq) in seqs.iter().enumerate() {
            if seen_seqs.contains(seq) {
                continue;
            }
            let price = buyer_prices[buyer_i][seq_i + change_seq_len];
            bananas
                .entry(*seq)
                .and_modify(|b| *b += price)
                .or_insert(price);
            seen_seqs.insert(seq);
        }
    }
    *bananas.values().max().unwrap()
}

pub fn solve() {
    let result = fs::read_to_string("data/22.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "".to_string()
    });

    let secrets: Vec<usize> = binding
        .as_str()
        .lines()
        .map(|l| l.parse::<usize>().unwrap())
        .collect();
    println!("2024.22.1: {}", solve1(&secrets));
    println!("2024.22.2: {}", solve2(&secrets));
}
