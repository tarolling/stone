// float arithmetic in nested indexing, tuned: one flat array per matrix, as C stores them, and slices whose
// lengths the compiler can see, so the inner loop has no bounds checks
fn matrix(n: usize, seed: i64) -> Vec<f64> {
    let mut x = seed;
    (0..n * n)
        .map(|_| {
            x = (x * 1103515245 + 12345) % 2147483648;
            x as f64 / 2147483648.0
        })
        .collect()
}

fn multiply(a: &[f64], b: &[f64], n: usize) -> Vec<f64> {
    let mut c = vec![0.0; n * n];
    for (a_row, c_row) in a.chunks_exact(n).zip(c.chunks_exact_mut(n)) {
        for (j, cell) in c_row.iter_mut().enumerate() {
            let mut total = 0.0;
            for (k, &a_k) in a_row.iter().enumerate() {
                total += a_k * b[k * n + j];
            }
            *cell = total;
        }
    }
    c
}

fn checksum(c: &[f64], n: usize) -> f64 {
    let mut total = 0.0;
    for (i, row) in c.chunks_exact(n).enumerate() {
        for (j, &cell) in row.iter().enumerate() {
            total += cell * (i + j + 1) as f64;
        }
    }
    total
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    let total = checksum(&multiply(&matrix(n, 1), &matrix(n, 2), n), n);
    println!("{}", (total * 1000000.0) as i64);
}
