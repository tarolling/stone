// reading global constants from a function: lcg with its constants at the top level
const MULTIPLIER: i64 = 1103515245;
const INCREMENT: i64 = 12345;
const MODULUS: i64 = 2147483648;

fn lcg(rounds: i64) -> i64 {
    let mut x: i64 = 1;
    let mut total: i64 = 0;
    for _ in 0..rounds {
        x = (x * MULTIPLIER + INCREMENT) % MODULUS;
        total += x / 65536;
    }
    total
}

fn main() {
    let rounds: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", lcg(rounds));
}
