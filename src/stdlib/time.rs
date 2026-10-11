//! The builtin `time` module: the time of day, a clock for timing code, and sleeping.
//!
//! A program imports it like any builtin module (see [`super::BuiltinModule`]), with `use time`
//! then `time.sleep(0.5)`, or with `use time.sleep` then `sleep(0.5)`.
//!
//! The functions here are what the interpreter runs. Compiled code makes the system calls behind
//! them from `stone.time_*` routines: `clock_gettime` for the time and the clock, and
//! `nanosleep` for sleeping, splitting the seconds the same way [`split_seconds`] does.

use super::BuiltinDoc;
use std::sync::OnceLock;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

/// The name a program imports the module by, as in `use time`.
pub const MODULE: &str = "time";

/// What the module is for, which editors show for `time`.
pub const MODULE_DOC: &str = "The builtin module for the time of day, timing code, and sleeping.";

pub static DOCS: [BuiltinDoc; 3] = [
    BuiltinDoc {
        name: "time.now",
        signature: "time.now() -> float",
        description: "Returns the seconds since 1970-01-01 00:00:00 UTC, with a fraction.",
    },
    BuiltinDoc {
        name: "time.clock",
        signature: "time.clock() -> float",
        description: "Returns seconds from a fixed but arbitrary point, which never go \
                      backward, so the difference of two calls times the code between them.",
    },
    BuiltinDoc {
        name: "time.sleep",
        signature: "time.sleep(seconds: int | float) -> none",
        description: "Waits for at least `seconds`, which may have a fraction, such as \
                      `time.sleep(0.25)`. It stops the program if `seconds` is negative or nan.",
    },
];

/// The runtime error of `time.sleep` of a negative number or nan.
pub const SLEEP_NEGATIVE: &str = "sleep length must be non-negative";

/// The runtime error of `time.sleep` of 2^63 seconds or more, which no system can wait for.
pub const SLEEP_TOO_LARGE: &str = "sleep length is too large";

/// Returns the seconds since the Unix epoch, as whole seconds plus nanoseconds over 1e9, which is
/// how compiled code turns `clock_gettime`'s result into a float.
pub fn now() -> f64 {
    let elapsed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 / 1e9
}

/// Returns the seconds since the first call in this process, which never go backward.
pub fn clock() -> f64 {
    static START: OnceLock<Instant> = OnceLock::new();
    START.get_or_init(Instant::now).elapsed().as_secs_f64()
}

/// Splits `seconds` into the whole seconds and nanoseconds that `nanosleep` takes, truncating
/// each as compiled code does, or returns the runtime error for a length no one can sleep.
///
/// For example, `split_seconds(1.5)` returns `Ok((1, 500000000))`, while `split_seconds(-1.0)`
/// fails with [`SLEEP_NEGATIVE`] and so does nan. The nanoseconds are always below 1e9.
pub fn split_seconds(seconds: f64) -> Result<(i64, i64), String> {
    if seconds.is_nan() || seconds < 0.0 {
        return Err(SLEEP_NEGATIVE.to_string());
    }
    if seconds >= 9223372036854775808.0 {
        return Err(SLEEP_TOO_LARGE.to_string());
    }
    let whole = seconds as i64;
    let nanos = ((seconds - whole as f64) * 1e9) as i64;
    Ok((whole, nanos))
}

/// Waits for at least `seconds`, or returns the error [`split_seconds`] gives.
pub fn sleep(seconds: f64) -> Result<(), String> {
    let (whole, nanos) = split_seconds(seconds)?;
    std::thread::sleep(Duration::new(whole as u64, nanos as u32));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_time_and_the_clock_are_plausible() {
        assert!(now() > 1.7e9);
        let start = clock();
        assert!(clock() >= start);
    }

    #[test]
    fn sleep_lengths_split_into_seconds_and_nanoseconds() {
        assert_eq!(split_seconds(1.5), Ok((1, 500_000_000)));
        assert_eq!(split_seconds(0.0), Ok((0, 0)));
        assert_eq!(split_seconds(-0.0), Ok((0, 0)));
        assert_eq!(split_seconds(2.0), Ok((2, 0)));
        // the fraction truncates, so a length just under a second never reaches 1e9 nanoseconds
        assert_eq!(
            split_seconds(1.0 - f64::EPSILON / 2.0),
            Ok((0, 999_999_999))
        );
        assert_eq!(split_seconds(0.000_000_000_5), Ok((0, 0)));
        assert_eq!(
            split_seconds(9223372036854774784.0),
            Ok((9223372036854774784, 0))
        );
    }

    #[test]
    fn negative_nan_and_huge_lengths_are_errors() {
        for bad in [-1.0, -1e-300, f64::NAN, f64::NEG_INFINITY] {
            assert_eq!(split_seconds(bad), Err(SLEEP_NEGATIVE.to_string()), "{bad}");
        }
        for bad in [9223372036854775808.0, 1e19, f64::INFINITY] {
            assert_eq!(
                split_seconds(bad),
                Err(SLEEP_TOO_LARGE.to_string()),
                "{bad}"
            );
        }
    }

    #[test]
    fn sleep_waits_at_least_as_long_as_asked() {
        let start = Instant::now();
        sleep(0.01).unwrap();
        assert!(start.elapsed() >= Duration::from_millis(10));
    }
}
