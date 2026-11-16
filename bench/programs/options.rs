// options in a hot loop: a step that may find nothing, matched on every turn, and a vector of
// the options it gave, summed at the end
fn step(x: i64) -> Option<i64> {
    if x % 7 == 0 {
        return None;
    }
    Some((x * 31 + 7) % 1000003)
}

fn main() {
    let n: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut x = 1;
    let mut hits = 0;
    let mut misses = 0;
    let mut kept = Vec::new();
    for i in 0..n {
        match step(x + i) {
            Some(y) => {
                x = y;
                hits += 1;
            }
            None => {
                x += 1;
                misses += 1;
            }
        }
        if i % 16 == 0 {
            kept.push(step(i));
        }
    }
    let total: i64 = kept.iter().map(|k| k.unwrap_or(0)).sum();
    println!("{x} {hits} {misses} {total}");
}
