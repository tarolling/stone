// float arithmetic in nested vectors: multiplying two matrices of floats from a generator
fn matrix(n: usize, seed: i64) -> Vec<Vec<f64>> {
    let mut rows = Vec::new();
    let mut x = seed;
    for _ in 0..n {
        let mut row = Vec::new();
        for _ in 0..n {
            x = (x * 1103515245 + 12345) % 2147483648;
            row.push(x as f64 / 2147483648.0);
        }
        rows.push(row);
    }
    rows
}

fn multiply(a: &[Vec<f64>], b: &[Vec<f64>], n: usize) -> Vec<Vec<f64>> {
    let mut c = Vec::new();
    for i in 0..n {
        let mut row = Vec::new();
        for j in 0..n {
            let mut total = 0.0;
            for k in 0..n {
                total += a[i][k] * b[k][j];
            }
            row.push(total);
        }
        c.push(row);
    }
    c
}

fn checksum(c: &[Vec<f64>], n: usize) -> f64 {
    let mut total = 0.0;
    for i in 0..n {
        for j in 0..n {
            total += c[i][j] * (i + j + 1) as f64;
        }
    }
    total
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    let total = checksum(&multiply(&matrix(n, 1), &matrix(n, 2), n), n);
    println!("{}", (total * 1000000.0) as i64);
}
