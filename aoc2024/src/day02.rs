use std::fs;

fn parse(s: &str) -> Vec<Vec<i64>> {
    s.lines()
        .map(|line| {
            line.split_whitespace()
                .filter_map(|n| n.parse::<i64>().ok())
                .collect()
        })
        .collect()
}

fn is_report_safe(r: &Vec<i64>) -> bool {
    let (deltas, _) = r[1..].iter().fold((Vec::new(), r[0]), |(mut ds, p), n| {
        ds.push(n - p);
        (ds, *n)
    });
    (deltas.iter().all(|&x| x > 0) || deltas.iter().all(|&x| x < 0))
        && deltas.iter().all(|&x| x.abs() >= 1 && x.abs() <= 3)
}

fn solve1(s: &str) -> usize {
    let reports = parse(s);
    reports.iter().filter(|r| is_report_safe(r)).count()
}

fn is_report_safe2(r: &Vec<i64>) -> bool {
    let removed_reports: Vec<Vec<i64>> = (0..r.len())
        .map(|i| {
            let mut new_report = r.clone();
            new_report.remove(i);
            new_report
        })
        .collect();
    removed_reports.iter().any(|r| is_report_safe(r))
}

fn solve2(s: &str) -> usize {
    let reports = parse(s);
    reports.iter().filter(|r| is_report_safe2(r)).count()
}

pub fn solve() {
    let result = fs::read_to_string("data/02.txt");
    let binding = result.unwrap_or_else(|error| {
        eprintln!("ERROR: {}", error);
        "1 1\n".to_string()
    });
    let input = binding.as_str();

    println!("2024.02.1: {}", solve1(input));
    println!("2024.02.2: {}", solve2(input));
}
