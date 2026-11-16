// strings, tuned: numbers are formatted straight into one buffer, as C does, and words are
// slices of it rather than strings of their own
use std::fmt::Write;

fn sentence(n: i64, seed: i64) -> String {
    let mut text = String::with_capacity(n as usize * 6);
    let mut x = seed;
    for i in 0..n {
        x = (x * 1103515245 + 12345) % 2147483648;
        if i > 0 {
            text.push(' ');
        }
        write!(text, "{}", x / 65536).unwrap();
    }
    text
}

fn score(text: &str) -> i64 {
    let mut total = 0;
    for word in text.as_bytes().split(|&b| b == b' ') {
        let (Some(&first), Some(&last)) = (word.first(), word.last()) else {
            continue;
        };
        total += word.len() as i64;
        if first == b'1' {
            total += 1;
        }
        if last == b'7' {
            total += 2;
        }
        if first == last {
            total += 3;
        }
    }
    total
}

fn run(rounds: i64, n: i64) -> i64 {
    let mut total = 0;
    for r in 0..rounds {
        let text = sentence(n, r + 1);
        total += score(&text) + text.find("4242").map_or(-1, |at| at as i64);
    }
    total
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!(
        "{}",
        run(args[1].parse().unwrap(), args[2].parse().unwrap())
    );
}
