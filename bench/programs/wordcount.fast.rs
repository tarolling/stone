// maps with string keys, tuned: a fast non-cryptographic hasher that takes eight bytes at a
// time, keys formatted into a stack buffer, and a string allocated only for a new key
use std::collections::HashMap;
use std::hash::{BuildHasherDefault, Hasher};
use std::io::Write;

/// A multiply-and-rotate hasher in the style of rustc's FxHasher.
#[derive(Default)]
struct Fx(u64);

impl Hasher for Fx {
    fn write(&mut self, bytes: &[u8]) {
        let mut chunks = bytes.chunks_exact(8);
        for chunk in &mut chunks {
            let word = u64::from_le_bytes(chunk.try_into().unwrap());
            self.0 = (self.0.rotate_left(5) ^ word).wrapping_mul(0x517c_c1b7_2722_0a95);
        }
        for &byte in chunks.remainder() {
            self.0 = (self.0.rotate_left(5) ^ byte as u64).wrapping_mul(0x517c_c1b7_2722_0a95);
        }
    }

    fn finish(&self) -> u64 {
        self.0
    }
}

type Counts = HashMap<String, i64, BuildHasherDefault<Fx>>;

/// Formats `n` into `buffer` and returns the text.
fn format(buffer: &mut [u8; 24], n: i64) -> &str {
    let mut cursor = &mut buffer[..];
    write!(cursor, "{n}").unwrap();
    let len = 24 - cursor.len();
    std::str::from_utf8(&buffer[..len]).unwrap()
}

fn count_words(n: i64, k: i64) -> Counts {
    let mut counts = Counts::default();
    let mut buffer = [0; 24];
    let mut x: i64 = 1;
    for _ in 0..n {
        x = (x * 1103515245 + 12345) % 2147483648;
        let word = format(&mut buffer, x / 64 % k);
        match counts.get_mut(word) {
            Some(count) => *count += 1,
            None => {
                counts.insert(word.to_string(), 1);
            }
        }
    }
    counts
}

fn checksum(counts: &Counts, k: i64) -> i64 {
    let mut total = counts.len() as i64;
    let mut buffer = [0; 24];
    for j in 0..k {
        if let Some(count) = counts.get(format(&mut buffer, j)) {
            total += count * (j % 7 + 1);
        }
    }
    total
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let k: i64 = args[2].parse().unwrap();
    let counts = count_words(args[1].parse().unwrap(), k);
    println!("{}", checksum(&counts, k));
}
