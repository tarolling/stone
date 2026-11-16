// integer arithmetic, division, and remainder: a linear congruential generator
fn lcg(rounds: i64) -> i64 {
    let mut x: i64 = 1;
    let mut total: i64 = 0;
    for _ in 0..rounds {
        x = (x * 1103515245 + 12345) % 2147483648;
        total += x / 65536;
    }
    total
}

fn main() {
    let rounds: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", lcg(rounds));
}
