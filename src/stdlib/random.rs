//! The builtin `random` module: pseudo-random floats, ints, and list elements.
//!
//! A program imports it like any builtin module (see [`super::BuiltinModule`]), with
//! `use random` then `random.randint(1, 6)`, or with `use random.randint` then `randint(1, 6)`.
//!
//! Both backends run the same generator, xoshiro256**, seeded through splitmix64, so after
//! `random.seed(n)` a program draws the same numbers under `stone run` and `stone build` on every
//! machine. Without a seed, the first draw seeds the generator from the system's entropy.
//! [`Rng`] is what the interpreter runs, and compiled code does the same steps in `stone.random_*`
//! routines.

use super::BuiltinDoc;
use std::hash::{BuildHasher, RandomState};

/// The name a program imports the module by, as in `use random`.
pub const MODULE: &str = "random";

/// What the module is for, which editors show for `random`.
pub const MODULE_DOC: &str = "The builtin module of pseudo-random numbers, which draws the same \
                              numbers in every backend after `random.seed(n)`.";

pub static DOCS: [BuiltinDoc; 4] = [
    BuiltinDoc {
        name: "random.seed",
        signature: "random.seed(n: int) -> none",
        description: "Starts the numbers over from `n`, so the same seed always gives the same \
                      numbers, in `stone run` and in a built program alike. Without a seed, \
                      each run draws different numbers.",
    },
    BuiltinDoc {
        name: "random.random",
        signature: "random.random() -> float",
        description: "Returns a float from 0.0 up to but not including 1.0.",
    },
    BuiltinDoc {
        name: "random.randint",
        signature: "random.randint(low: int, high: int) -> int",
        description: "Returns an int from `low` to `high`, including both, each equally likely. \
                      It stops the program if `low` is greater than `high`.",
    },
    BuiltinDoc {
        name: "random.choice",
        signature: "random.choice(items: list[T]) -> T",
        description: "Returns an element of the list, each equally likely. It stops the program \
                      if the list is empty.",
    },
];

/// The runtime error of `random.randint(low, high)` when `low` is greater than `high`.
pub const EMPTY_RANGE: &str = "empty range for randint";

/// The runtime error of `random.choice` of an empty list.
pub const EMPTY_CHOICE: &str = "cannot choose from an empty list";

/// The constants of splitmix64, which turns a seed into the generator's state.
pub const SPLITMIX_GAMMA: u64 = 0x9e37_79b9_7f4a_7c15;
pub const SPLITMIX_MUL1: u64 = 0xbf58_476d_1ce4_e5b9;
pub const SPLITMIX_MUL2: u64 = 0x94d0_49bb_1331_11eb;

/// The xoshiro256** generator.
///
/// For example, `Rng::seeded(42).random()` returns `0.08386297105988216` in every run.
#[derive(Clone, Debug, PartialEq)]
pub struct Rng {
    state: [u64; 4],
}

impl Rng {
    /// Returns a generator whose state is the next four outputs of splitmix64 started at `seed`,
    /// which is what `random.seed(seed)` does.
    pub fn seeded(seed: i64) -> Rng {
        let mut x = seed as u64;
        let mut state = [0; 4];
        for word in &mut state {
            x = x.wrapping_add(SPLITMIX_GAMMA);
            let mut z = x;
            z = (z ^ (z >> 30)).wrapping_mul(SPLITMIX_MUL1);
            z = (z ^ (z >> 27)).wrapping_mul(SPLITMIX_MUL2);
            *word = z ^ (z >> 31);
        }
        Rng { state }
    }

    /// Returns a generator seeded from the system's entropy, which Rust's `RandomState` reads.
    pub fn from_entropy() -> Rng {
        Rng::seeded(RandomState::new().hash_one(0u8) as i64)
    }

    /// Returns the next 64 bits.
    pub fn next_u64(&mut self) -> u64 {
        let [s0, s1, s2, s3] = &mut self.state;
        let result = s1.wrapping_mul(5).rotate_left(7).wrapping_mul(9);
        let t = *s1 << 17;
        *s2 ^= *s0;
        *s3 ^= *s1;
        *s1 ^= *s2;
        *s0 ^= *s3;
        *s2 ^= t;
        *s3 = s3.rotate_left(45);
        result
    }

    /// Returns a float from 0.0 up to but not including 1.0: the top 53 bits of the next draw,
    /// over 2^53.
    pub fn random(&mut self) -> f64 {
        (self.next_u64() >> 11) as f64 * (1.0 / (1u64 << 53) as f64)
    }

    /// Returns a number below `span`, each equally likely, or any 64 bits for a `span` of 0,
    /// which stands for 2^64.
    ///
    /// Draws below `2^64 % span` are thrown away, so the ones left divide evenly into `span`
    /// groups, and the result is the draw modulo `span`.
    pub fn below(&mut self, span: u64) -> u64 {
        if span == 0 {
            return self.next_u64();
        }
        let limit = span.wrapping_neg() % span;
        loop {
            let draw = self.next_u64();
            if draw >= limit {
                return draw % span;
            }
        }
    }

    /// Returns an int from `low` to `high`, including both, or the error [`EMPTY_RANGE`].
    ///
    /// For example, after `Rng::seeded(42)`, three draws of `randint(1, 6)` give 1, 1, then 6.
    pub fn randint(&mut self, low: i64, high: i64) -> Result<i64, String> {
        if low > high {
            return Err(EMPTY_RANGE.to_string());
        }
        // wraps to 0 for the whole range of ints
        let span = high.wrapping_sub(low).wrapping_add(1) as u64;
        Ok(low.wrapping_add(self.below(span) as i64))
    }

    /// Returns the index of an element of a list of `len` elements, or the error
    /// [`EMPTY_CHOICE`]. It draws as `randint(0, len - 1)` does.
    pub fn choice(&mut self, len: usize) -> Result<usize, String> {
        if len == 0 {
            return Err(EMPTY_CHOICE.to_string());
        }
        Ok(self.below(len as u64) as usize)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn seeding_follows_splitmix64() {
        // the reference implementation's first outputs from 0
        assert_eq!(
            Rng::seeded(0).state,
            [
                0xe220a8397b1dcdaf,
                0x6e789e6aa1b965f4,
                0x06c45d188009454f,
                0xf88bb8a8724c81ec,
            ]
        );
    }

    #[test]
    fn draws_follow_xoshiro256_star_star() {
        // the reference implementation's outputs from the state 1, 2, 3, 4
        let mut rng = Rng {
            state: [1, 2, 3, 4],
        };
        let draws: Vec<u64> = (0..10).map(|_| rng.next_u64()).collect();
        assert_eq!(
            draws,
            [
                11520,
                0,
                1509978240,
                1215971899390074240,
                1216172134540287360,
                607988272756665600,
                16172922978634559625,
                8476171486693032832,
                10595114339597558777,
                2904607092377533576,
            ]
        );
    }

    #[test]
    fn a_seed_gives_the_same_numbers_every_time() {
        let mut rng = Rng::seeded(42);
        let floats: Vec<f64> = (0..3).map(|_| rng.random()).collect();
        assert_eq!(
            floats,
            [0.08386297105988216, 0.3789802506626686, 0.6800434110281394]
        );
        let mut rng = Rng::seeded(42);
        let rolls: Vec<i64> = (0..10).map(|_| rng.randint(1, 6).unwrap()).collect();
        assert_eq!(rolls, [1, 1, 6, 6, 5, 1, 5, 4, 5, 6]);
    }

    #[test]
    fn randint_covers_its_whole_range() {
        let mut rng = Rng::seeded(7);
        assert_eq!(rng.randint(5, 5), Ok(5));
        assert_eq!(rng.randint(2, 1), Err(EMPTY_RANGE.to_string()));
        let mut seen = [false; 3];
        for _ in 0..100 {
            let n = rng.randint(-1, 1).unwrap();
            seen[(n + 1) as usize] = true;
        }
        assert_eq!(seen, [true; 3]);
        // the whole range of ints is a span of 2^64, which adds the whole draw to low
        let mut copy = rng.clone();
        assert_eq!(
            rng.randint(i64::MIN, i64::MAX),
            Ok((copy.next_u64() as i64).wrapping_add(i64::MIN))
        );
        let n = rng.randint(i64::MAX - 1, i64::MAX).unwrap();
        assert!(n >= i64::MAX - 1);
    }

    #[test]
    fn below_throws_away_draws_that_would_bias_it() {
        // 2^64 % 3 is 1, so a draw of 0 is thrown away
        let mut rng = Rng {
            state: [1, 2, 3, 4],
        };
        let mut copy = rng.clone();
        assert_eq!(copy.next_u64(), 11520);
        assert_eq!(copy.next_u64(), 0);
        assert_eq!(copy.next_u64(), 1509978240);
        assert_eq!(rng.below(3), 11520 % 3);
        assert_eq!(rng.below(3), 1509978240 % 3);
    }

    #[test]
    fn choice_draws_like_randint() {
        let mut a = Rng::seeded(3);
        let mut b = Rng::seeded(3);
        for len in 1..20 {
            assert_eq!(
                a.choice(len).unwrap() as i64,
                b.randint(0, len as i64 - 1).unwrap()
            );
        }
        assert_eq!(a.choice(0), Err(EMPTY_CHOICE.to_string()));
    }

    #[test]
    fn floats_are_below_one() {
        let mut rng = Rng::from_entropy();
        for _ in 0..1000 {
            let x = rng.random();
            assert!((0.0..1.0).contains(&x), "{x}");
        }
    }
}
