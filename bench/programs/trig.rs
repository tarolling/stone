// trigonometry: a point around a wobbling circle
fn walk(steps: i64) -> f64 {
    let mut total = 0.0;
    for i in 0..steps {
        let t = i as f64 * 0.001;
        let r = 1.0 + 0.5 * (t * 3.0).sin();
        let x = r * t.cos();
        let y = r * t.sin();
        total += y.atan2(x) + (t.sin() * 0.5).asin();
    }
    total
}

fn main() {
    let steps: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", (walk(steps) * 1000.0) as i64);
}
