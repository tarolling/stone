// strings: formatting numbers, joining them into a sentence, and splitting it back into words
fn sentence(n: i64, seed: i64) -> String {
    let mut words = Vec::new();
    let mut x = seed;
    for _ in 0..n {
        x = (x * 1103515245 + 12345) % 2147483648;
        words.push((x / 65536).to_string());
    }
    words.join(" ")
}

fn score(text: &str) -> i64 {
    let mut total = 0;
    for word in text.split_whitespace() {
        total += word.len() as i64;
        if word.starts_with('1') {
            total += 1;
        }
        if word.ends_with('7') {
            total += 2;
        }
        let bytes = word.as_bytes();
        if bytes[0] == bytes[bytes.len() - 1] {
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
