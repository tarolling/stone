// branches and division: the longest Collatz chain
fn chain(mut n: i64) -> i64 {
    let mut steps = 0;
    while n != 1 {
        if n % 2 == 0 {
            n /= 2;
        } else {
            n = 3 * n + 1;
        }
        steps += 1;
    }
    steps
}

fn longest(limit: i64) -> i64 {
    let mut best = 0;
    let mut best_start = 0;
    for start in 1..limit {
        let steps = chain(start);
        if steps > best {
            best = steps;
            best_start = start;
        }
    }
    best_start
}

fn main() {
    let limit: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", longest(limit));
}
