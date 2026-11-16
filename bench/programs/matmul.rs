// nested vector indexing: multiplying two matrices filled from a generator
fn matrix(n: usize, seed: i64) -> Vec<Vec<i64>> {
    let mut rows = Vec::new();
    let mut x = seed;
    for _ in 0..n {
        let mut row = Vec::new();
        for _ in 0..n {
            x = (x * 1103515245 + 12345) % 2147483648;
            row.push(x / 2097152);
        }
        rows.push(row);
    }
    rows
}

fn multiply(a: &[Vec<i64>], b: &[Vec<i64>], n: usize) -> Vec<Vec<i64>> {
    let mut c = Vec::new();
    for i in 0..n {
        let mut row = Vec::new();
        for j in 0..n {
            let mut total = 0;
            for k in 0..n {
                total += a[i][k] * b[k][j];
            }
            row.push(total);
        }
        c.push(row);
    }
    c
}

fn checksum(c: &[Vec<i64>], n: usize) -> i64 {
    let mut total = 0;
    for i in 0..n {
        for j in 0..n {
            total += c[i][j] * (i + j + 1) as i64;
        }
    }
    total
}

fn main() {
    let n: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    println!(
        "{}",
        checksum(&multiply(&matrix(n, 1), &matrix(n, 2), n), n)
    );
}
