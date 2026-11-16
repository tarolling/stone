// maps with string keys: counting how often each word appears
use std::collections::HashMap;

fn count_words(n: i64, k: i64) -> HashMap<String, i64> {
    let mut counts = HashMap::new();
    let mut x: i64 = 1;
    for _ in 0..n {
        x = (x * 1103515245 + 12345) % 2147483648;
        let word = (x / 64 % k).to_string();
        *counts.entry(word).or_insert(0) += 1;
    }
    counts
}

fn checksum(counts: &HashMap<String, i64>, k: i64) -> i64 {
    let mut total = counts.len() as i64;
    for j in 0..k {
        if let Some(count) = counts.get(&j.to_string()) {
            total += count * (j % 7 + 1);
        }
    }
    total
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let k: i64 = args[2].parse().unwrap();
    let counts = count_words(args[1].parse().unwrap(), k);
    println!("{}", checksum(&counts, k));
}
