// sorting vectors of ints and floats
fn draw(x: i64) -> i64 {
    (x * 1103515245 + 12345) % 2147483648
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut x = 1;
    let mut ints = Vec::with_capacity(n);
    for _ in 0..n {
        x = draw(x);
        let high = x;
        x = draw(x);
        ints.push(high * 2147483648 + x);
    }
    ints.sort_unstable();
    let total = ints
        .iter()
        .fold(0, |total, v| (total * 31 + v % 1000000007) % 1000000007);
    println!("{} {} {}", total, ints[0], ints[n - 1]);

    let mut floats = Vec::with_capacity(n);
    for _ in 0..n {
        x = draw(x);
        floats.push(x as f64 / 2147483648.0 - 0.5);
    }
    floats.sort_unstable_by(f64::total_cmp);
    let sum = floats.iter().fold(0.0, |sum, v| sum * 0.5 + v);
    let scaled = |v: f64| (v * 1000000000.0) as i64;
    println!(
        "{} {} {}",
        scaled(sum),
        scaled(floats[0]),
        scaled(floats[n - 1])
    );
}
