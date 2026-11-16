// arithmetic on whole arrays: y = 0.5 * x + y, element by element
fn saxpy(a: f64, x: &[f64], y: &mut [f64], reps: i64) {
    for _ in 0..reps {
        for (yi, xi) in y.iter_mut().zip(x) {
            *yi = a * xi + *yi;
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: i64 = args[1].parse().unwrap();
    let reps: i64 = args[2].parse().unwrap();
    let x: Vec<f64> = (0..n).map(|i| (i % 7) as f64).collect();
    let mut y: Vec<f64> = (0..n).map(|i| (i % 5) as f64).collect();
    saxpy(0.5, &x, &mut y, reps);
    let total: f64 = y.iter().fold(0.0, |sum, v| sum + v);
    println!("{}", total as i64);
}
