//! The trigonometric functions of the `math` module: `sin`, `cos`, `tan`, `asin`, `acos`,
//! `atan`, and `atan2`, ported from musl, whose code comes from Sun's fdlibm, so each is within
//! one ulp of the exact result.
//!
//! Both backends must agree to the bit, so nothing here calls the system's libm, and every
//! operation is a plain IEEE operation in a fixed order, with no fused multiply-add. The parts
//! without branches (the polynomials and the reduction by pi/2) are written once, over
//! [`Arith`]: [`Floats`] computes them for the interpreter, and compiled code's lowering emits an
//! instruction for each operation. The branches around them are written here and again in the
//! lowering, as for the rest of the module.
//!
//! musl reads the bits of a float for its range tests, which compiled code has no instruction
//! for, so they are float comparisons here: a test of the high word such as `ix < 0x3fe921fc` is
//! `|x| < f64::from_bits(0x3fe921fc << 32)`, which is exact, and musl's
//! `SET_LOW_WORD(f, 0)`, which cuts `f` to a few bits so that `f * f` is exact, is a Veltkamp
//! split ([`split`]) instead.
//!
//! Arguments of at least 2^20 * pi/2 are reduced by the prelude's `rem_pio2_large`, written in
//! stone so both backends run the same code, which the interpreter passes in as a [`Large`].

use super::DOMAIN_ERROR;
use std::f64::consts::{FRAC_2_PI, FRAC_PI_2, FRAC_PI_4, PI};

/// The linked name of the prelude function that reduces an argument too large for the inline
/// reduction, which takes a positive finite float of at least [`MEDIUM`] and returns
/// `(n, y0, y1)`: the number of quarter turns in it modulo 8, and the rest, `y0 + y1`, which is
/// at most pi/4 from 0.
pub const REDUCE_LARGE: &str = "prelude.rem_pio2_large";

/// Reduces a positive finite float of at least [`MEDIUM`] by pi/2, as [`REDUCE_LARGE`] does, or
/// fails with an error of type `E`, such as an interpreter's when calls nest too deeply.
pub type Large<'a, E> = &'a mut dyn FnMut(f64) -> Result<(i64, f64, f64), E>;

/// Float arithmetic that the branch-free parts of the functions are written in, so that one
/// text says which operations they do and in which order.
///
/// For example, [`horner`] over [`Floats`] computes `1.0 + z * 2.0` for the coefficients
/// `[1.0, 2.0]`, and over the lowering emits a multiplication and then an addition.
pub trait Arith {
    /// A float, or what holds one.
    type Value: Copy;
    /// The float `x` itself.
    fn num(&mut self, x: f64) -> Self::Value;
    fn add(&mut self, a: Self::Value, b: Self::Value) -> Self::Value;
    fn sub(&mut self, a: Self::Value, b: Self::Value) -> Self::Value;
    fn mul(&mut self, a: Self::Value, b: Self::Value) -> Self::Value;
    /// `a / b` as IEEE 754 divides, which the functions only do with a `b` that is never zero.
    fn div(&mut self, a: Self::Value, b: Self::Value) -> Self::Value;
    /// `-a`, which flips the sign bit.
    fn neg(&mut self, a: Self::Value) -> Self::Value;
    /// `|a|`, which clears the sign bit.
    fn abs(&mut self, a: Self::Value) -> Self::Value;
    /// The correctly rounded square root of `a`, which is never negative here.
    fn sqrt(&mut self, a: Self::Value) -> Self::Value;
}

/// [`Arith`] on floats, which computes each operation.
pub struct Floats;

impl Arith for Floats {
    type Value = f64;
    fn num(&mut self, x: f64) -> f64 {
        x
    }
    fn add(&mut self, a: f64, b: f64) -> f64 {
        a + b
    }
    fn sub(&mut self, a: f64, b: f64) -> f64 {
        a - b
    }
    fn mul(&mut self, a: f64, b: f64) -> f64 {
        a * b
    }
    fn div(&mut self, a: f64, b: f64) -> f64 {
        a / b
    }
    fn neg(&mut self, a: f64) -> f64 {
        -a
    }
    fn abs(&mut self, a: f64) -> f64 {
        a.abs()
    }
    fn sqrt(&mut self, a: f64) -> f64 {
        a.sqrt()
    }
}

// sin's polynomial on [-pi/4, pi/4]
const S1: f64 = -0.16666666666666632;
const S2: f64 = 0.00833333333332249;
const S3: f64 = -0.0001984126982985795;
const S4: f64 = 2.7557313707070068e-06;
const S5: f64 = -2.5050760253406863e-08;
const S6: f64 = 1.58969099521155e-10;

// cos's polynomial on [-pi/4, pi/4]
const C1: f64 = 0.0416666666666666;
const C2: f64 = -0.001388888888887411;
const C3: f64 = 2.480158728947673e-05;
const C4: f64 = -2.7557314351390663e-07;
const C5: f64 = 2.087572321298175e-09;
const C6: f64 = -1.1359647557788195e-11;

// tan's polynomial on [-0.67434, 0.67434]
const T: [f64; 13] = [
    0.3333333333333341,
    0.13333333333320124,
    0.05396825397622605,
    0.021869488294859542,
    0.0088632398235993,
    0.0035920791075913124,
    0.0014562094543252903,
    0.0005880412408202641,
    0.0002464631348184699,
    7.817944429395571e-05,
    7.140724913826082e-05,
    -1.8558637485527546e-05,
    2.590730518636337e-05,
];
const PIO4_LO: f64 = 3.061616997868383e-17;

// pi/2 in three parts of 33 bits, each with what is left after it
const TOINT: f64 = 6755399441055744.0;
const PIO2_1: f64 = 1.5707963267341256;
const PIO2_1T: f64 = 6.077100506506192e-11;
const PIO2_2: f64 = 6.077100506303966e-11;
const PIO2_2T: f64 = 2.0222662487959506e-21;
const PIO2_3: f64 = 2.0222662487111665e-21;
const PIO2_3T: f64 = 8.4784276603689e-32;

// asin's and acos's rational function, and pi/2 as a sum of two floats
const PIO2_LO: f64 = 6.123233995736766e-17;
const PS: [f64; 6] = [
    0.16666666666666666,
    -0.3255658186224009,
    0.20121253213486293,
    -0.04005553450067941,
    0.0007915349942898145,
    3.479331075960212e-05,
];
const QS: [f64; 5] = [
    1.0,
    -2.403394911734414,
    2.0209457602335057,
    -0.6882839716054533,
    0.07703815055590194,
];

// atan at 0.5, 1, 1.5, and infinity, each as a sum of two floats, and atan's polynomial
pub const ATAN_HI: [f64; 4] = [0.4636476090008061, FRAC_PI_4, 0.982793723247329, FRAC_PI_2];
pub const ATAN_LO: [f64; 4] = [
    2.2698777452961687e-17,
    3.061616997868383e-17,
    1.3903311031230998e-17,
    6.123233995736766e-17,
];
const AT_EVEN: [f64; 6] = [
    0.3333333333333293,
    0.14285714272503466,
    0.09090887133436507,
    0.06661073137387531,
    0.049768779946159324,
    0.016285820115365782,
];
const AT_ODD: [f64; 5] = [
    -0.19999999999876483,
    -0.11111110405462356,
    -0.0769187620504483,
    -0.058335701337905735,
    -0.036531572744216916,
];
const PI_LO: f64 = 1.2246467991473532e-16;

/// What musl adds to a result it returns exactly, to raise the inexact flag, which changes no
/// bit of the result but is kept so the operations stay musl's.
const TINY: f64 = 7.52316384526264e-37;

/// Floats below which `sin`, `cos`, and `tan` need no reduction: `|x| < PIO4_BOUND` is musl's
/// `ix <= 0x3fe921fb`, a little more than pi/4.
pub const PIO4_BOUND: f64 = 0.7853984832763672;

/// Floats below which `sin` returns its argument, 2^-26.
pub const SIN_TINY: f64 = 1.4901161193847656e-08;

/// Floats below which `cos` returns 1, a little less than 2^-27 * sqrt(2).
pub const COS_TINY: f64 = 1.0536709282860102e-08;

/// Floats below which `tan` and `atan` return their argument, 2^-27.
pub const TAN_TINY: f64 = 7.450580596923828e-09;

/// Floats at and above which [`tan`]'s kernel works from pi/4 back, about 0.6744.
pub const TAN_BIG: f64 = 0.6743354797363281;

/// Floats below which the reduction by pi/2 is inline, a little less than 2^20 * pi/2, and at and
/// above which [`REDUCE_LARGE`] does it.
pub const MEDIUM: f64 = 1647099.0;

/// Floats at and above which `asin` uses its formula for arguments near 1, about 0.975.
pub const ASIN_NEAR_ONE: f64 = 0.9749999046325684;

/// Floats below which `acos` returns pi/2, a little more than 2^-57.
pub const ACOS_TINY: f64 = 6.938900521352129e-18;

/// 2^-26, below which `asin` returns its argument.
pub const ASIN_TINY: f64 = SIN_TINY;

/// 2^66, at and above which `atan` is pi/2.
pub const ATAN_HUGE: f64 = 73786976294838206464.0;

/// 2^64, the ratio past which `atan2` is pi/2 or 0.
pub const ATAN2_RATIO: f64 = 18446744073709551616.0;

/// 2^-16: the reduction takes musl's second round when `|y0| < |x| * 2^-16`, which holds
/// whenever musl's test (`y0`'s exponent more than 16 below `x`'s) does.
pub const SECOND_ROUND: f64 = 1.52587890625e-05;

/// 2^-49: after the second round, the reduction takes musl's third when `|y0| < |x| * 2^-49`,
/// which holds whenever musl's test (more than 49 below) does. It must come after the second,
/// since it drops the second's rounding error, which is 0 only once that much has cancelled.
pub const THIRD_ROUND: f64 = 1.7763568394002505e-15;

/// Returns `c0 + z * (c1 + z * (c2 + ...))` for the coefficients `coeffs`, innermost first.
///
/// For example, `horner(a, z, &[S3, S4])` is musl's `S3 + z*S4`.
pub fn horner<A: Arith>(a: &mut A, z: A::Value, coeffs: &[f64]) -> A::Value {
    let (&last, rest) = coeffs.split_last().expect("a coefficient");
    let mut sum = a.num(last);
    for &c in rest.iter().rev() {
        let product = a.mul(z, sum);
        let c = a.num(c);
        sum = a.add(c, product);
    }
    sum
}

/// Returns the high half of `x`'s significand, 26 bits, so that the product of two such halves
/// is exact: Veltkamp's split, `c - (c - x)` with `c = x * (2^27 + 1)`.
///
/// For example, the split of `1.0 + 2^-40` is `1.0`.
pub fn split<A: Arith>(a: &mut A, x: A::Value) -> A::Value {
    let factor = a.num(134217729.0);
    let c = a.mul(x, factor);
    let d = a.sub(c, x);
    a.sub(c, d)
}

/// Returns sin(x + y) for `|x + y| <= pi/4`, where `y` is what is left of the reduced argument
/// beyond `x` (0 for an argument that needed no reduction): musl's `__sin(x, y, 1)`.
pub fn sin_kernel<A: Arith>(a: &mut A, x: A::Value, y: A::Value) -> A::Value {
    let z = a.mul(x, x);
    let w = a.mul(z, z);
    // r = S2 + z*(S3 + z*S4) + z*w*(S5 + z*S6)
    let left = horner(a, z, &[S2, S3, S4]);
    let zw = a.mul(z, w);
    let tail = horner(a, z, &[S5, S6]);
    let right = a.mul(zw, tail);
    let r = a.add(left, right);
    let v = a.mul(z, x);
    // x - ((z*(0.5*y - v*r) - y) - v*S1)
    let half = a.num(0.5);
    let hy = a.mul(half, y);
    let vr = a.mul(v, r);
    let t = a.sub(hy, vr);
    let t = a.mul(z, t);
    let t = a.sub(t, y);
    let s1 = a.num(S1);
    let vs = a.mul(v, s1);
    let t = a.sub(t, vs);
    a.sub(x, t)
}

/// Returns cos(x + y) for `|x + y| <= pi/4`: musl's `__cos(x, y)`.
pub fn cos_kernel<A: Arith>(a: &mut A, x: A::Value, y: A::Value) -> A::Value {
    let z = a.mul(x, x);
    let w = a.mul(z, z);
    // r = z*(C1 + z*(C2 + z*C3)) + w*w*(C4 + z*(C5 + z*C6))
    let p = horner(a, z, &[C1, C2, C3]);
    let left = a.mul(z, p);
    let ww = a.mul(w, w);
    let q = horner(a, z, &[C4, C5, C6]);
    let right = a.mul(ww, q);
    let r = a.add(left, right);
    let half = a.num(0.5);
    let hz = a.mul(half, z);
    let one = a.num(1.0);
    let w = a.sub(one, hz);
    // w + (((1.0 - w) - hz) + (z*r - x*y))
    let t = a.sub(one, w);
    let t = a.sub(t, hz);
    let zr = a.mul(z, r);
    let xy = a.mul(x, y);
    let u = a.sub(zr, xy);
    let t = a.add(t, u);
    a.add(w, t)
}

/// Returns the argument of tan's kernel for a non-negative `x + y` of at least [`TAN_BIG`],
/// `(pi/4 - x) + (pi/4's tail - y)`, whose tangent the kernel turns back into tan(x + y).
pub fn tan_fold<A: Arith>(a: &mut A, x: A::Value, y: A::Value) -> A::Value {
    let pio4 = a.num(FRAC_PI_4);
    let t = a.sub(pio4, x);
    let lo = a.num(PIO4_LO);
    let u = a.sub(lo, y);
    a.add(t, u)
}

/// Returns `(w, r)` of tan's kernel, where `w = x + r` is tan(x + y) for `|x + y| < TAN_BIG`.
pub fn tan_poly<A: Arith>(a: &mut A, x: A::Value, y: A::Value) -> (A::Value, A::Value) {
    let z = a.mul(x, x);
    let w = a.mul(z, z);
    // r = T1 + w*(T3 + ...), v = z*(T2 + w*(T4 + ...)), split into odd and even
    let r = horner(a, w, &[T[1], T[3], T[5], T[7], T[9], T[11]]);
    let even = horner(a, w, &[T[2], T[4], T[6], T[8], T[10], T[12]]);
    let v = a.mul(z, even);
    let s = a.mul(z, x);
    // r = y + z*(s*(r + v) + y) + s*T0
    let t = a.add(r, v);
    let t = a.mul(s, t);
    let t = a.add(t, y);
    let t = a.mul(z, t);
    let t = a.add(y, t);
    let t0 = a.num(T[0]);
    let st = a.mul(s, t0);
    let r = a.add(t, st);
    let w = a.add(x, r);
    (w, r)
}

/// Returns tan's kernel's result for a folded argument (see [`tan_fold`]), where `s` is 1.0
/// for tan and -1.0 for -1/tan: `s - 2.0*(x + (r - w*w/(w + s)))`.
pub fn tan_unfold<A: Arith>(
    a: &mut A,
    x: A::Value,
    w: A::Value,
    r: A::Value,
    s: A::Value,
) -> A::Value {
    let ws = a.add(w, s);
    let ww = a.mul(w, w);
    let q = a.div(ww, ws);
    let t = a.sub(r, q);
    let t = a.add(x, t);
    let two = a.num(2.0);
    let t = a.mul(two, t);
    a.sub(s, t)
}

/// Returns -1/(x + r) for tan's kernel, computed accurately from `w = x + r` and its split.
pub fn tan_reciprocal<A: Arith>(a: &mut A, x: A::Value, w: A::Value, r: A::Value) -> A::Value {
    let w0 = split(a, w);
    // w0 + v = r + x
    let t = a.sub(w0, x);
    let v = a.sub(r, t);
    let minus_one = a.num(-1.0);
    let q = a.div(minus_one, w);
    let q0 = split(a, q);
    // q0 + q*(1.0 + q0*w0 + q0*v)
    let p = a.mul(q0, w0);
    let one = a.num(1.0);
    let t = a.add(one, p);
    let u = a.mul(q0, v);
    let t = a.add(t, u);
    let t = a.mul(q, t);
    a.add(q0, t)
}

/// The first round of reducing `x` by pi/2, for `|x| < MEDIUM`: returns `(n, r, w, y0)`, where
/// `n` is the nearest whole number to x / (pi/2), as a float, and `y0 = r - w` is the rest.
pub fn reduce_first<A: Arith>(a: &mut A, x: A::Value) -> (A::Value, A::Value, A::Value, A::Value) {
    // rint(x / (pi/2)), by adding and taking away 1.5 * 2^52
    let inverse = a.num(FRAC_2_PI);
    let t = a.mul(x, inverse);
    let toint = a.num(TOINT);
    let t = a.add(t, toint);
    let n = a.sub(t, toint);
    let p1 = a.num(PIO2_1);
    let t = a.mul(n, p1);
    let r = a.sub(x, t);
    let p1t = a.num(PIO2_1T);
    let w = a.mul(n, p1t);
    let y0 = a.sub(r, w);
    (n, r, w, y0)
}

/// Returns `(|y0|, ax * scale)`, the sides of the test of whether a round of reducing `x` by
/// pi/2, whose magnitude is `ax`, lost so many bits of `y0` to cancellation that the next round is
/// needed, with `scale` [`SECOND_ROUND`] or [`THIRD_ROUND`].
pub fn round_test<A: Arith>(
    a: &mut A,
    ax: A::Value,
    y0: A::Value,
    scale: f64,
) -> (A::Value, A::Value) {
    let ay = a.abs(y0);
    let scale = a.num(scale);
    let bound = a.mul(ax, scale);
    (ay, bound)
}

/// Musl's second or third round of the reduction, which subtract `n` times `part` from the last
/// round's `r` and take `n * tail` and the subtraction's rounding error as `w`, good to 118 or 151
/// bits: returns `(r, w, y0)` again.
pub fn reduce_round<A: Arith>(
    a: &mut A,
    n: A::Value,
    r: A::Value,
    part: f64,
    tail: f64,
) -> (A::Value, A::Value, A::Value) {
    let t = r;
    let part = a.num(part);
    let product = a.mul(n, part);
    let r = a.sub(t, product);
    // w = n*tail - ((t - r) - product)
    let tail = a.num(tail);
    let u = a.mul(n, tail);
    let d = a.sub(t, r);
    let d = a.sub(d, product);
    let w = a.sub(u, d);
    let y0 = a.sub(r, w);
    (r, w, y0)
}

/// The parts of musl's second and third rounds: what [`reduce_round`] subtracts, then its tail.
pub const ROUNDS: [(f64, f64, f64); 2] = [
    (SECOND_ROUND, PIO2_2, PIO2_2T),
    (THIRD_ROUND, PIO2_3, PIO2_3T),
];

/// Returns `y1`, what is left of the reduced argument beyond `y0`: `(r - y0) - w`.
pub fn reduce_tail<A: Arith>(a: &mut A, r: A::Value, y0: A::Value, w: A::Value) -> A::Value {
    let t = a.sub(r, y0);
    a.sub(t, w)
}

/// Returns `R(z)`, the rational function of `asin` and `acos`: `p(z) / q(z)`.
pub fn asin_ratio<A: Arith>(a: &mut A, z: A::Value) -> A::Value {
    let p = horner(a, z, &PS);
    let p = a.mul(z, p);
    let q = horner(a, z, &QS);
    a.div(p, q)
}

/// Returns `(s1 + s2)`, atan's polynomial in `x * x` less its first term, split into odd and even
/// powers of `x^4`.
pub fn atan_poly<A: Arith>(a: &mut A, x: A::Value) -> A::Value {
    let z = a.mul(x, x);
    let w = a.mul(z, z);
    let even = horner(a, w, &AT_EVEN);
    let s1 = a.mul(z, even);
    let odd = horner(a, w, &AT_ODD);
    let s2 = a.mul(w, odd);
    a.add(s1, s2)
}

/// Returns `x * pi/2 + TINY`, `asin` of 1 or -1.
pub fn asin_one<A: Arith>(a: &mut A, x: A::Value) -> A::Value {
    let pio2 = a.num(FRAC_PI_2);
    let t = a.mul(x, pio2);
    let tiny = a.num(TINY);
    a.add(t, tiny)
}

/// Returns `asin(x)` for `|x| < 0.5`: `x + x*R(x*x)`.
pub fn asin_small<A: Arith>(a: &mut A, x: A::Value) -> A::Value {
    let xx = a.mul(x, x);
    let r = asin_ratio(a, xx);
    let t = a.mul(x, r);
    a.add(x, t)
}

/// Returns `asin(|x|)` for `|x| >= ASIN_NEAR_ONE`, from `s = sqrt(z)` and `r = R(z)` with
/// `z = (1 - |x|) / 2`: `pi/2 - (2*(s + s*r) - pio2_lo)`.
pub fn asin_near_one<A: Arith>(a: &mut A, s: A::Value, r: A::Value) -> A::Value {
    let sr = a.mul(s, r);
    let t = a.add(s, sr);
    let two = a.num(2.0);
    let t = a.mul(two, t);
    let lo = a.num(PIO2_LO);
    let t = a.sub(t, lo);
    let pio2 = a.num(FRAC_PI_2);
    a.sub(pio2, t)
}

/// Returns `asin(|x|)` for `0.5 <= |x| < ASIN_NEAR_ONE`, from `z`, `s`, and `r` as for
/// [`asin_near_one`], splitting `s` into `f + c` so that it loses nothing.
pub fn asin_middle<A: Arith>(a: &mut A, z: A::Value, s: A::Value, r: A::Value) -> A::Value {
    let f = split(a, s);
    // c = (z - f*f) / (s + f)
    let ff = a.mul(f, f);
    let t = a.sub(z, ff);
    let u = a.add(s, f);
    let c = a.div(t, u);
    // 0.5*pio2_hi - (2*s*r - (pio2_lo - 2*c) - (0.5*pio2_hi - 2*f))
    let two = a.num(2.0);
    let t = a.mul(two, s);
    let t = a.mul(t, r);
    let lo = a.num(PIO2_LO);
    let c2 = a.mul(two, c);
    let u = a.sub(lo, c2);
    let t = a.sub(t, u);
    let pio4 = a.num(0.5 * FRAC_PI_2);
    let f2 = a.mul(two, f);
    let u = a.sub(pio4, f2);
    let t = a.sub(t, u);
    a.sub(pio4, t)
}

/// Returns `(z, sqrt(z), R(z))` with `z = (1 - ax) / 2`, for `asin` and `acos` of `|x| >= 0.5`,
/// where `ax` is `|x|` for `asin`, and for `acos`, `x` or `-x`.
pub fn half_turn<A: Arith>(a: &mut A, ax: A::Value) -> (A::Value, A::Value, A::Value) {
    let one = a.num(1.0);
    let t = a.sub(one, ax);
    let half = a.num(0.5);
    let z = a.mul(t, half);
    let s = a.sqrt(z);
    let r = asin_ratio(a, z);
    (z, s, r)
}

/// Returns `2*pi/2 + TINY`, `acos(-1)`.
pub fn acos_minus_one<A: Arith>(a: &mut A) -> A::Value {
    let two = a.num(2.0);
    let pio2 = a.num(FRAC_PI_2);
    let t = a.mul(two, pio2);
    let tiny = a.num(TINY);
    a.add(t, tiny)
}

/// Returns `pi/2 + TINY`, `acos` of a tiny `x`.
pub fn acos_tiny<A: Arith>(a: &mut A) -> A::Value {
    let pio2 = a.num(FRAC_PI_2);
    let tiny = a.num(TINY);
    a.add(pio2, tiny)
}

/// Returns `acos(x)` for `|x| < 0.5`: `pi/2 - (x - (pio2_lo - x*R(x*x)))`.
pub fn acos_small<A: Arith>(a: &mut A, x: A::Value) -> A::Value {
    let xx = a.mul(x, x);
    let r = asin_ratio(a, xx);
    let t = a.mul(x, r);
    let lo = a.num(PIO2_LO);
    let t = a.sub(lo, t);
    let t = a.sub(x, t);
    let pio2 = a.num(FRAC_PI_2);
    a.sub(pio2, t)
}

/// Returns `acos(x)` for `x <= -0.5` from `s` and `r` of [`half_turn`] of `-x`:
/// `2*(pi/2 - (s + (r*s - pio2_lo)))`.
pub fn acos_negative<A: Arith>(a: &mut A, s: A::Value, r: A::Value) -> A::Value {
    let rs = a.mul(r, s);
    let lo = a.num(PIO2_LO);
    let w = a.sub(rs, lo);
    let t = a.add(s, w);
    let pio2 = a.num(FRAC_PI_2);
    let t = a.sub(pio2, t);
    let two = a.num(2.0);
    a.mul(two, t)
}

/// Returns `acos(x)` for `x >= 0.5` from `z`, `s`, and `r` of [`half_turn`] of `x`, splitting `s`
/// into `f + c`: `2*(f + (r*s + c))`.
pub fn acos_positive<A: Arith>(a: &mut A, z: A::Value, s: A::Value, r: A::Value) -> A::Value {
    let f = split(a, s);
    let ff = a.mul(f, f);
    let t = a.sub(z, ff);
    let u = a.add(s, f);
    let c = a.div(t, u);
    let rs = a.mul(r, s);
    let w = a.add(rs, c);
    let t = a.add(f, w);
    let two = a.num(2.0);
    a.mul(two, t)
}

/// Returns the argument atan's polynomial takes for `ax` in part `id` of [`atan_part`]'s range.
pub fn atan_argument<A: Arith>(a: &mut A, ax: A::Value, id: usize) -> A::Value {
    let one = a.num(1.0);
    match id {
        // (2x - 1) / (2 + x)
        0 => {
            let two = a.num(2.0);
            let t = a.mul(two, ax);
            let t = a.sub(t, one);
            let u = a.add(two, ax);
            a.div(t, u)
        }
        // (x - 1) / (x + 1)
        1 => {
            let t = a.sub(ax, one);
            let u = a.add(ax, one);
            a.div(t, u)
        }
        // (x - 1.5) / (1 + 1.5x)
        2 => {
            let k = a.num(1.5);
            let t = a.sub(ax, k);
            let u = a.mul(k, ax);
            let u = a.add(one, u);
            a.div(t, u)
        }
        // -1 / x
        _ => {
            let minus_one = a.num(-1.0);
            a.div(minus_one, ax)
        }
    }
}

/// Returns `atan(x) + c` for `|x| < 0.4375` and a correction `c` far below an ulp of the result:
/// `x - (x*(s1 + s2) - c)`. `atan` itself passes 0 for `c`, which changes no bit.
pub fn atan_small<A: Arith>(a: &mut A, x: A::Value, c: A::Value) -> A::Value {
    let p = atan_poly(a, x);
    let t = a.mul(x, p);
    let t = a.sub(t, c);
    a.sub(x, t)
}

/// Returns `atan(|x|) + c` from the argument `t` of part `id`:
/// `atan_hi[id] - (((t*(s1 + s2) - atan_lo[id]) - c) - t)`.
pub fn atan_finish<A: Arith>(a: &mut A, t: A::Value, id: usize, c: A::Value) -> A::Value {
    let (hi, lo) = (a.num(ATAN_HI[id]), a.num(ATAN_LO[id]));
    atan_finish_with(a, t, hi, lo, c)
}

/// Returns [`atan_finish`] with the part's constants `hi` and `lo` given as values, so that
/// compiled code emits the polynomial once for every part.
pub fn atan_finish_with<A: Arith>(
    a: &mut A,
    t: A::Value,
    hi: A::Value,
    lo: A::Value,
    c: A::Value,
) -> A::Value {
    let p = atan_poly(a, t);
    let u = a.mul(t, p);
    let u = a.sub(u, lo);
    let u = a.sub(u, c);
    let u = a.sub(u, t);
    a.sub(hi, u)
}

/// Returns `c` such that `atan(q) + c` is `atan(ay / ax)` to well below an ulp, where `q` is the
/// rounded quotient `ay / ax`: its rounding error `(ay - q*ax) / ax`, times atan's derivative
/// `1 / (1 + q*q)`, which together are `(ay - q*ax) / (ax + q*ay)`. Without it, `atan2` would
/// inherit up to an ulp from the division, as musl's does. `q * ax` is computed exactly, as
/// Dekker's product of [`split`] halves, so `ax` and `ay` must be scaled into the range where
/// none of its parts overflows or underflows ([`atan2_scale`]).
pub fn quotient_correction<A: Arith>(
    a: &mut A,
    ay: A::Value,
    ax: A::Value,
    q: A::Value,
) -> A::Value {
    let qh = split(a, q);
    let ql = a.sub(q, qh);
    let xh = split(a, ax);
    let xl = a.sub(ax, xh);
    let p = a.mul(q, ax);
    // the rounding error of p, exactly
    let e = a.mul(qh, xh);
    let e = a.sub(e, p);
    let t = a.mul(qh, xl);
    let e = a.add(e, t);
    let t = a.mul(ql, xh);
    let e = a.add(e, t);
    let t = a.mul(ql, xl);
    let e = a.add(e, t);
    // ay - p is exact, since p is within an ulp of ay
    let r = a.sub(ay, p);
    let r = a.sub(r, e);
    let d = a.mul(q, ay);
    let d = a.add(ax, d);
    a.div(r, d)
}

/// The power of two that scales `atan2`'s magnitudes down when `ax` is above its inverse, or up
/// when `ax` is below it, before [`quotient_correction`]: 2^-600 and 2^600, beyond 2^500 and
/// 2^-500. Since `atan2` only corrects quotients from 2^-27 to 2^64, both magnitudes then lie
/// between about 2^-500 and 2^490.
pub fn atan2_scale(ax: f64) -> Option<f64> {
    if ax > ATAN2_BIG {
        Some(1.0 / ATAN2_FACTOR)
    } else if ax < 1.0 / ATAN2_BIG {
        Some(ATAN2_FACTOR)
    } else {
        None
    }
}

/// 2^500, past which [`atan2_scale`] scales.
pub const ATAN2_BIG: f64 = 3.273390607896142e150;

/// 2^600, the factor of [`atan2_scale`].
pub const ATAN2_FACTOR: f64 = 4.149515568880993e180;

/// Returns `atan_hi[3] + TINY`, `atan` of an `|x|` of at least 2^66.
pub fn atan_huge<A: Arith>(a: &mut A) -> A::Value {
    let hi = a.num(ATAN_HI[3]);
    let tiny = a.num(TINY);
    a.add(hi, tiny)
}

/// Returns `atan2`'s result from `z = atan(|y/x|)` for `x < 0`: `pi - (z - pi_lo)` for `y > 0`,
/// or `(z - pi_lo) - pi` for `y < 0`.
pub fn atan2_left<A: Arith>(a: &mut A, z: A::Value, y_negative: bool) -> A::Value {
    let lo = a.num(PI_LO);
    let t = a.sub(z, lo);
    let pi = a.num(PI);
    if y_negative {
        a.sub(t, pi)
    } else {
        a.sub(pi, t)
    }
}

/// The value `atan2` gives for infinite `x` and `y`, by `m`, 2 * sign(x) + sign(y).
pub fn atan2_infinities(m: u8) -> f64 {
    match m {
        0 => FRAC_PI_4,
        1 => -FRAC_PI_4,
        2 => 3.0 * PI / 4.0,
        _ => -3.0 * PI / 4.0,
    }
}

/// Returns `x` with its sign dropped as compiled code drops it: negated when below 0, so -0.0 and
/// nan come back as they are.
fn magnitude(x: f64) -> f64 {
    if x < 0.0 { -x } else { x }
}

/// Reduces `x`, with `PIO4_BOUND <= |x|` and `x` finite, by pi/2: returns `(n, y0, y1)` with
/// `x = n * pi/2 + y0 + y1`, give or take multiples of 2pi, and `|y0 + y1| <= pi/4` or close.
pub fn rem_pio2<E>(x: f64, large: Large<E>) -> Result<(i64, f64, f64), E> {
    let ax = x.abs();
    if ax < MEDIUM {
        let a = &mut Floats;
        let (n, mut r, mut w, mut y0) = reduce_first(a, x);
        for (scale, part, tail) in ROUNDS {
            let (ay, bound) = round_test(a, ax, y0, scale);
            if ay >= bound {
                break;
            }
            (r, w, y0) = reduce_round(a, n, r, part, tail);
        }
        let y1 = reduce_tail(a, r, y0, w);
        return Ok((n as i64, y0, y1));
    }
    if x < 0.0 {
        let (n, y0, y1) = large(-x)?;
        Ok((-n, -y0, -y1))
    } else {
        large(x)
    }
}

/// Returns sin(y0 + y1) after `q` quarter turns, with `q` from 0 to 3: the sine for 0, the cosine
/// for 1, and their negations for 2 and 3.
fn quadrant(q: i64, y0: f64, y1: f64) -> f64 {
    let value = if q % 2 == 0 {
        sin_kernel(&mut Floats, y0, y1)
    } else {
        cos_kernel(&mut Floats, y0, y1)
    };
    if q >= 2 { -value } else { value }
}

/// Returns `(n mod 4, y0, y1)` for `sin` or `cos` of a finite `x` beyond [`PIO4_BOUND`], or the
/// domain error for an infinity, or `None` for nan, which every function gives back.
fn reduced<E: From<String>>(x: f64, large: Large<E>) -> Result<Option<(i64, f64, f64)>, E> {
    if x.is_nan() {
        return Ok(None);
    }
    if x.is_infinite() {
        return Err(DOMAIN_ERROR.to_string().into());
    }
    let (n, y0, y1) = rem_pio2(x, large)?;
    Ok(Some((n.rem_euclid(4), y0, y1)))
}

/// Returns the sine of `x` in radians, or the error [`DOMAIN_ERROR`] for an infinity.
///
/// For example, `sin(2.0)` is `0.9092974268256817` and `sin(-0.0)` is `-0.0`.
pub fn sin<E: From<String>>(x: f64, large: Large<E>) -> Result<f64, E> {
    if x < PIO4_BOUND && x > -PIO4_BOUND {
        if x < SIN_TINY && x > -SIN_TINY {
            return Ok(x);
        }
        return Ok(quadrant(0, x, 0.0));
    }
    Ok(match reduced(x, large)? {
        Some((q, y0, y1)) => quadrant(q, y0, y1),
        None => x,
    })
}

/// Returns the cosine of `x` in radians, or the error [`DOMAIN_ERROR`] for an infinity.
///
/// For example, `cos(2.0)` is `-0.4161468365471424`.
pub fn cos<E: From<String>>(x: f64, large: Large<E>) -> Result<f64, E> {
    if x < PIO4_BOUND && x > -PIO4_BOUND {
        if x < COS_TINY && x > -COS_TINY {
            return Ok(1.0);
        }
        return Ok(quadrant(1, x, 0.0));
    }
    // a quarter turn more than sin
    Ok(match reduced(x, large)? {
        Some((q, y0, y1)) => quadrant((q + 1) % 4, y0, y1),
        None => x,
    })
}

/// Returns tan(y0 + y1), or -1/tan(y0 + y1) if `odd`, for `|y0 + y1| <= pi/4`: musl's `__tan`.
fn tan_kernel(y0: f64, y1: f64, odd: bool) -> f64 {
    let a = &mut Floats;
    if y0 >= TAN_BIG || y0 <= -TAN_BIG {
        let negative = y0 < 0.0;
        let (x, y) = if negative { (-y0, -y1) } else { (y0, y1) };
        let x = tan_fold(a, x, y);
        let (w, r) = tan_poly(a, x, 0.0);
        let s = a.num(if odd { -1.0 } else { 1.0 });
        let value = tan_unfold(a, x, w, r, s);
        return if negative { -value } else { value };
    }
    let (w, r) = tan_poly(a, y0, y1);
    if odd { tan_reciprocal(a, y0, w, r) } else { w }
}

/// Returns the tangent of `x` in radians, or the error [`DOMAIN_ERROR`] for an infinity.
///
/// For example, `tan(2.0)` is `-2.185039863261519`.
pub fn tan<E: From<String>>(x: f64, large: Large<E>) -> Result<f64, E> {
    if x < PIO4_BOUND && x > -PIO4_BOUND {
        if x < TAN_TINY && x > -TAN_TINY {
            return Ok(x);
        }
        return Ok(tan_kernel(x, 0.0, false));
    }
    Ok(match reduced(x, large)? {
        Some((q, y0, y1)) => tan_kernel(y0, y1, q % 2 == 1),
        None => x,
    })
}

/// Returns the arcsine of `x` in radians, from -pi/2 to pi/2, or the error [`DOMAIN_ERROR`] if
/// `|x| > 1`.
///
/// For example, `asin(0.7)` is `0.775397496610753`.
pub fn asin(x: f64) -> Result<f64, String> {
    let a = &mut Floats;
    let ax = magnitude(x);
    if ax >= 1.0 || ax.is_nan() {
        if ax == 1.0 {
            return Ok(asin_one(a, x));
        }
        if x.is_nan() {
            return Ok(x);
        }
        return Err(DOMAIN_ERROR.to_string());
    }
    if ax < 0.5 {
        if ax < ASIN_TINY {
            return Ok(x);
        }
        return Ok(asin_small(a, x));
    }
    let (z, s, r) = half_turn(a, ax);
    let value = if ax >= ASIN_NEAR_ONE {
        asin_near_one(a, s, r)
    } else {
        asin_middle(a, z, s, r)
    };
    Ok(if x < 0.0 { -value } else { value })
}

/// Returns the arccosine of `x` in radians, from 0 to pi, or the error [`DOMAIN_ERROR`] if
/// `|x| > 1`.
///
/// For example, `acos(-0.3)` is `1.8754889808102941`.
pub fn acos(x: f64) -> Result<f64, String> {
    let a = &mut Floats;
    let ax = magnitude(x);
    if ax >= 1.0 || ax.is_nan() {
        if ax == 1.0 {
            return Ok(if x < 0.0 { acos_minus_one(a) } else { 0.0 });
        }
        if x.is_nan() {
            return Ok(x);
        }
        return Err(DOMAIN_ERROR.to_string());
    }
    if ax < 0.5 {
        if ax < ACOS_TINY {
            return Ok(acos_tiny(a));
        }
        return Ok(acos_small(a, x));
    }
    if x < 0.0 {
        let (_, s, r) = half_turn(a, -x);
        return Ok(acos_negative(a, s, r));
    }
    let (z, s, r) = half_turn(a, x);
    Ok(acos_positive(a, z, s, r))
}

/// Returns which part of atan's range `ax`, which is at least 0.4375 and below [`ATAN_HUGE`],
/// falls in: `atan(ax)` is `ATAN_HI[id] + ATAN_LO[id] + atan(t)` for the [`atan_argument`] `t`.
pub fn atan_part(ax: f64) -> usize {
    if ax < 1.1875 {
        if ax < 0.6875 { 0 } else { 1 }
    } else if ax < 2.4375 {
        2
    } else {
        3
    }
}

/// Returns the arctangent of `x` in radians, from -pi/2 to pi/2.
///
/// For example, `atan(5.0)` is `1.373400766945016`, and `atan` of infinity is pi/2.
pub fn atan(x: f64) -> f64 {
    let a = &mut Floats;
    let negative = x < 0.0;
    let ax = magnitude(x);
    if ax >= ATAN_HUGE || ax.is_nan() {
        if x.is_nan() {
            return x;
        }
        let z = atan_huge(a);
        return if negative { -z } else { z };
    }
    if ax < 0.4375 {
        if ax < TAN_TINY {
            return x;
        }
        let c = a.num(0.0);
        return atan_small(a, x, c);
    }
    let id = atan_part(ax);
    let t = atan_argument(a, ax, id);
    let c = a.num(0.0);
    let z = atan_finish(a, t, id, c);
    if negative { -z } else { z }
}

/// Returns `atan(ay / ax)` for positive finite `ay` and `ax` whose quotient is at most 2^64, from
/// the rounded quotient `q` and, unless `q` is so small that `atan(q)` rounds to `q` within an ulp
/// anyway, the [`quotient_correction`] of its rounding.
fn atan_quotient(ay: f64, ax: f64) -> f64 {
    let a = &mut Floats;
    let q = a.div(ay, ax);
    if q < TAN_TINY {
        return q;
    }
    let (sy, sx) = match atan2_scale(ax) {
        Some(scale) => (ay * scale, ax * scale),
        None => (ay, ax),
    };
    let c = quotient_correction(a, sy, sx, q);
    if q < 0.4375 {
        return atan_small(a, q, c);
    }
    let id = atan_part(q);
    let t = atan_argument(a, q, id);
    atan_finish(a, t, id, c)
}

/// Returns the angle in radians, from -pi to pi, of the point `(x, y)` from the positive x axis,
/// as C's `atan2(y, x)` does, signed zeros and infinities included.
///
/// For example, `atan2(-1.0, -2.0)` is `-2.677945044588987` and `atan2(0.0, -0.0)` is pi.
pub fn atan2(y: f64, x: f64) -> f64 {
    if x.is_nan() || y.is_nan() {
        return x + y;
    }
    // musl returns atan(y) for x == 1, which the rest gives too: the quotient is exact, so its
    // correction is 0
    // 2 * sign(x) + sign(y), from the sign bits
    let m = (y.is_sign_negative() as u8) | ((x.is_sign_negative() as u8) << 1);
    let (ax, ay) = (x.abs(), y.abs());
    if y == 0.0 {
        return match m {
            0 | 1 => y,
            2 => PI,
            _ => -PI,
        };
    }
    if x == 0.0 {
        return if m & 1 == 1 { -FRAC_PI_2 } else { FRAC_PI_2 };
    }
    if ax == f64::INFINITY {
        return if ay == f64::INFINITY {
            atan2_infinities(m)
        } else {
            match m {
                0 => 0.0,
                1 => -0.0,
                2 => PI,
                _ => -PI,
            }
        };
    }
    // |y/x| > 2^64
    if ay > ax * ATAN2_RATIO || ay == f64::INFINITY {
        return if m & 1 == 1 { -FRAC_PI_2 } else { FRAC_PI_2 };
    }
    // |y/x| < 2^-64 with x < 0, whose atan would underflow
    let z = if m & 2 != 0 && ay * ATAN2_RATIO < ax {
        0.0
    } else {
        atan_quotient(ay, ax)
    };
    match m {
        0 => z,
        1 => -z,
        _ => atan2_left(&mut Floats, z, m == 3),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::stdlib::random::Rng;

    /// Stands in for the prelude's `rem_pio2_large`, which only stone code can call, so these
    /// tests leave huge arguments to [`huge_arguments_match_std_through_the_prelude`].
    fn large(x: f64) -> Result<(i64, f64, f64), String> {
        Err(format!("{x:e} needs the prelude"))
    }

    fn sin_of(x: f64) -> Result<f64, String> {
        sin(x, &mut large)
    }
    fn cos_of(x: f64) -> Result<f64, String> {
        cos(x, &mut large)
    }
    fn tan_of(x: f64) -> Result<f64, String> {
        tan(x, &mut large)
    }

    /// Returns how many floats apart `a` and `b` are, for floats of the same sign or zeros.
    fn ulps(a: f64, b: f64) -> u64 {
        if a == b {
            return 0;
        }
        let key = |x: f64| {
            let bits = x.to_bits() as i64;
            if bits < 0 { i64::MIN - bits } else { bits }
        };
        key(a).abs_diff(key(b))
    }

    /// Returns how many ulps `name` may be from the system's libm. glibc and musl are within an
    /// ulp of the exact value, but Apple's `tan` is not: `tan(7.001759109585632e39)` is 1.92 ulp
    /// off there, and ours 0.08, so on macOS it is a weaker oracle, and Linux, whose CI jobs hold
    /// every function to one ulp of glibc, checks the accuracy.
    fn std_slack(name: &str) -> u64 {
        if cfg!(target_os = "macos") && name == "tan" {
            3
        } else {
            1
        }
    }

    /// Checks that `ours` is within one ulp of `std`, which is the system's libm, at `count`
    /// arguments that `draw` makes from random floats in [0, 1).
    fn close_to_std(
        name: &str,
        count: usize,
        draw: impl Fn(f64, f64) -> f64,
        ours: impl Fn(f64) -> f64,
        std: impl Fn(f64) -> f64,
    ) {
        let mut rng = Rng::seeded(7);
        for _ in 0..count {
            let x = draw(rng.random(), rng.random());
            let (got, want) = (ours(x), std(x));
            assert!(
                ulps(got, want) <= std_slack(name),
                "{name}({x:e}) = {got:e}, std says {want:e}"
            );
        }
    }

    /// Draws arguments across every range the functions reduce themselves: tiny, below pi/4,
    /// inline reduction, near multiples of pi/2, and up to [`MEDIUM`], of either sign.
    fn argument(u: f64, v: f64) -> f64 {
        let sign = if v < 0.5 { -1.0 } else { 1.0 };
        let magnitude = match (v * 10.0) as u32 % 5 {
            0 => 2f64.powf(-40.0 + 40.0 * u),
            1 => u * 0.8,
            2 => u * 2000.0,
            3 => (u * 1000.0).round() * FRAC_PI_2 + (v - 0.5) * 1e-6,
            _ => u * MEDIUM,
        };
        sign * magnitude
    }

    #[test]
    fn known_values_match_python() {
        assert_eq!(sin_of(2.0), Ok(0.9092974268256817));
        assert_eq!(cos_of(2.0), Ok(-0.4161468365471424));
        assert_eq!(tan_of(2.0), Ok(-2.185039863261519));
        assert_eq!(sin_of(-3.0), Ok(-0.1411200080598672));
        assert_eq!(cos_of(-3.0), Ok(-0.9899924966004454));
        assert_eq!(tan_of(-3.0), Ok(0.1425465430742778));
        assert_eq!(sin_of(1e6), Ok(-0.34999350217129294));
        assert_eq!(cos_of(1e6), Ok(0.9367521275331447));
        assert_eq!(tan_of(1e6), Ok(-0.373624453987599));
        assert_eq!(sin_of(FRAC_PI_2), Ok(1.0));
        assert_eq!(cos_of(FRAC_PI_2), Ok(6.123233995736766e-17));
        assert_eq!(tan_of(FRAC_PI_2), Ok(1.633123935319537e16));
        assert_eq!(asin(0.7), Ok(0.775397496610753));
        assert_eq!(acos(-0.3), Ok(1.8754889808102941));
        assert_eq!(atan(5.0), 1.373400766945016);
        assert_eq!(atan2(-1.0, -2.0), -2.677945044588987);
        assert_eq!(atan2(f64::INFINITY, f64::NEG_INFINITY), 2.356194490192345);
        // musl's 6.243541237520913e-2 is 1.52 ulps from the exact value, from rounding y/x, where
        // this is 0.52 (and Python's 6.2435412375209116e-2, correctly rounded, 0.48)
        assert_eq!(
            atan2(1.4936900270856092e18, 2.389266883217223e19),
            6.243541237520912e-2
        );
    }

    #[test]
    fn special_values() {
        assert!(sin_of(-0.0).unwrap().is_sign_negative());
        assert!(tan_of(-0.0).unwrap().is_sign_negative());
        assert_eq!(cos_of(-0.0), Ok(1.0));
        assert!(sin_of(f64::NAN).unwrap().is_nan());
        assert!(cos_of(f64::NAN).unwrap().is_nan());
        assert!(tan_of(f64::NAN).unwrap().is_nan());
        for f in [sin_of, cos_of, tan_of] {
            assert_eq!(f(f64::INFINITY), Err(DOMAIN_ERROR.to_string()));
            assert_eq!(f(f64::NEG_INFINITY), Err(DOMAIN_ERROR.to_string()));
        }
        assert_eq!(asin(1.0), Ok(FRAC_PI_2));
        assert_eq!(asin(-1.0), Ok(-FRAC_PI_2));
        assert_eq!(acos(1.0), Ok(0.0));
        assert_eq!(acos(-1.0), Ok(PI));
        assert!(asin(-0.0).unwrap().is_sign_negative());
        assert!(asin(f64::NAN).unwrap().is_nan());
        assert!(acos(f64::NAN).unwrap().is_nan());
        for x in [1.0000000000000002, -2.0, f64::INFINITY] {
            assert_eq!(asin(x), Err(DOMAIN_ERROR.to_string()));
            assert_eq!(acos(x), Err(DOMAIN_ERROR.to_string()));
        }
        assert_eq!(atan(f64::INFINITY), FRAC_PI_2);
        assert_eq!(atan(f64::NEG_INFINITY), -FRAC_PI_2);
        assert!(atan(-0.0).is_sign_negative());
        assert!(atan(f64::NAN).is_nan());
        assert_eq!(atan2(0.0, -0.0), PI);
        assert_eq!(atan2(-0.0, -0.0), -PI);
        assert!(atan2(-0.0, 0.0).is_sign_negative());
        assert_eq!(atan2(0.0, 0.0), 0.0);
        assert_eq!(atan2(1.0, 0.0), FRAC_PI_2);
        assert_eq!(atan2(-1.0, -0.0), -FRAC_PI_2);
        assert_eq!(atan2(1.0, f64::NEG_INFINITY), PI);
        assert!(atan2(-1.0, f64::INFINITY).is_sign_negative());
        assert_eq!(atan2(f64::INFINITY, 1.0), FRAC_PI_2);
        assert_eq!(atan2(1e300, -1e-300), FRAC_PI_2);
        assert_eq!(atan2(-1e-300, -1e300), -PI);
        assert!(atan2(f64::NAN, 1.0).is_nan());
    }

    #[test]
    fn sin_cos_and_tan_are_within_an_ulp() {
        let count = 200_000;
        close_to_std("sin", count, argument, |x| sin_of(x).unwrap(), f64::sin);
        close_to_std("cos", count, argument, |x| cos_of(x).unwrap(), f64::cos);
        close_to_std("tan", count, argument, |x| tan_of(x).unwrap(), f64::tan);
    }

    #[test]
    fn inverses_are_within_an_ulp() {
        let count = 200_000;
        let unit = |u: f64, v: f64| {
            let x = if v < 0.3 { u * 2f64.powf(-60.0 * v) } else { u };
            if v.to_bits().is_multiple_of(2) { x } else { -x }
        };
        close_to_std("asin", count, unit, |x| asin(x).unwrap(), f64::asin);
        close_to_std("acos", count, unit, |x| acos(x).unwrap(), f64::acos);
        let any = |u: f64, v: f64| {
            let x = 2f64.powf(-80.0 + 160.0 * u);
            if v < 0.5 { x } else { -x }
        };
        close_to_std("atan", count, any, atan, f64::atan);
        let mut rng = Rng::seeded(11);
        for _ in 0..count {
            let (u, v) = (rng.random(), rng.random());
            let y = any(u, v);
            let x = any(rng.random(), rng.random());
            let (got, want) = (atan2(y, x), y.atan2(x));
            assert!(
                ulps(got, want) <= 1,
                "atan2({y:e}, {x:e}) = {got:e}, std says {want:e}"
            );
        }
        // magnitudes from subnormal to near overflow, with quotients atan2 corrects, so both of
        // atan2_scale's scalings and its absence are taken
        for _ in 0..count {
            let x = 2f64.powf(-1074.0 + 2097.0 * rng.random());
            let y = x * 2f64.powf(-40.0 + 110.0 * rng.random());
            let (y, x) = match rng.random() {
                r if r < 0.25 => (y, x),
                r if r < 0.5 => (-y, x),
                r if r < 0.75 => (y, -x),
                _ => (-y, -x),
            };
            if !y.is_finite() || y == 0.0 {
                continue;
            }
            let (got, want) = (atan2(y, x), y.atan2(x));
            assert!(
                ulps(got, want) <= 1,
                "atan2({y:e}, {x:e}) = {got:e}, std says {want:e}"
            );
        }
    }

    /// Huge arguments, which the prelude's `rem_pio2_large` reduces, through the interpreter:
    /// Python's values, then thousands of arguments from [`MEDIUM`] to the largest float, each
    /// within an ulp of the system's libm.
    /// What lets `atan2` and compiled code skip musl's case of `x == 1`.
    #[test]
    fn atan2_of_one_is_atan() {
        let mut ys = vec![
            0.0,
            -0.0,
            1e-310,
            2f64.powi(-27),
            0.4375,
            2.4375,
            1e10,
            f64::MAX,
        ];
        ys.extend([
            2f64.powi(64),
            2f64.powi(64) * 1.0000001,
            2f64.powi(65),
            2f64.powi(66),
        ]);
        ys.push(f64::INFINITY);
        let mut rng = Rng::seeded(3);
        for _ in 0..100_000 {
            ys.push(2f64.powf(-80.0 + 160.0 * rng.random()));
        }
        for y in ys {
            for y in [y, -y] {
                let (got, want) = (atan2(y, 1.0), atan(y));
                assert_eq!(got.to_bits(), want.to_bits(), "atan2({y:e}, 1)");
            }
        }
    }

    /// Returns `math.sin`, `math.cos`, and `math.tan` of each of `xs` as the interpreter computes
    /// them, through the prelude's reduction for huge arguments.
    fn huge_rows(xs: &[f64]) -> Vec<Vec<f64>> {
        let mut source = String::from("use math\nxs = [");
        source.push_str(
            &xs.iter()
                .map(|x| format!("{x:e}"))
                .collect::<Vec<_>>()
                .join(", "),
        );
        source.push_str("]\nfor x in xs:\n    print(math.sin(x), math.cos(x), math.tan(x))\n");
        let mut out = Vec::new();
        crate::driver::interpret_with(&source, &mut out, crate::interpreter::Limits::DEFAULT)
            .unwrap();
        let out = String::from_utf8(out).unwrap();
        out.lines()
            .map(|line| line.split(' ').map(|x| x.parse().unwrap()).collect())
            .collect()
    }

    #[test]
    fn huge_arguments_match_std_through_the_prelude() {
        let mut xs = vec![MEDIUM, 1e7, 1e15, 1e22, 1e100, 1e300, f64::MAX];
        let mut rng = Rng::seeded(5);
        while xs.len() < 3000 {
            let x = 2f64.powf(20.0 + 1004.0 * rng.random());
            xs.push(if rng.random() < 0.5 { -x } else { x });
        }
        let rows = huge_rows(&xs);
        assert_eq!(rows.len(), xs.len());
        assert_eq!(
            rows[3],
            [-0.8522008497671888, 0.523214785395139, -1.628778225606899]
        );
        // musl's tan(1e22), 0.54 ulp from the exact value, where glibc's -1.6287782256068988 is
        // correctly rounded
        assert_eq!(
            rows[5],
            [-0.8178819121159085, -0.5753861119575491, 1.4214488238747245]
        );
        for (x, row) in xs.iter().zip(&rows) {
            for (name, got, want) in [
                ("sin", row[0], x.sin()),
                ("cos", row[1], x.cos()),
                ("tan", row[2], x.tan()),
            ] {
                assert!(
                    ulps(got, want) <= std_slack(name),
                    "{name}({x:e}) = {got:e}, std says {want:e}"
                );
            }
        }
    }

    #[test]
    fn tan_is_within_an_ulp_where_apples_libm_is_not() {
        // 0.46 and 0.08 ulp from the exact values, as glibc rounds them, where Apple's libm
        // gives 3.8231954777211055 and 0.991242612391881, 1.54 and 1.92 ulp off
        assert_eq!(tan_of(4.468206133084965e5), Ok(3.8231954777211064));
        let row = huge_rows(&[7.001759109585632e39]);
        assert_eq!(row[0][2], 0.9912426123918813);
    }
}
