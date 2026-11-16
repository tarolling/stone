// vector pushes and indexed loads and stores: counting primes with a sieve
fn count_primes(limit: usize) -> i64 {
    let mut is_prime: Vec<i64> = Vec::new();
    for _ in 0..=limit {
        is_prime.push(1);
    }
    is_prime[0] = 0;
    is_prime[1] = 0;
    let mut p = 2;
    while p * p <= limit {
        if is_prime[p] == 1 {
            let mut m = p * p;
            while m <= limit {
                is_prime[m] = 0;
                m += p;
            }
        }
        p += 1;
    }
    is_prime.iter().sum()
}

fn main() {
    let limit: usize = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("{}", count_primes(limit));
}
