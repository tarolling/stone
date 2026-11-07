//! Format specs, the text after a `:` in a string's value, as in `"{x:.2f}"`, which say how the
//! value is written. They follow Python's format specification mini-language,
//!
//! ```text
//! [[fill]align][sign][#][0][width][grouping][.precision][type]
//! ```
//!
//! and write the same text CPython does. The lexer parses each spec once ([`FormatSpec::parse`]),
//! the checker requires the types it allows, and the functions here are what the interpreter
//! runs. Compiled code takes the same two steps: it writes the value's text, with its sign and
//! any `0x` prefix, as [`int_text`] and [`float_text`] do, then lays it out in place, grouping
//! and padding it as [`layout`] does.

use super::format_float;

/// The largest width or precision a spec may give, so a spec never asks for more text than a
/// program could ever want.
pub const MAX_SPEC_NUMBER: usize = 10_000;

/// Where the value goes in its width.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Align {
    /// `<`, the default for text.
    Left,
    /// `>`, the default for numbers.
    Right,
    /// `^`, with any odd fill character on the right.
    Center,
    /// `=`, for numbers only: the fill goes after the sign and any prefix, as in `-0003`.
    AfterSign,
}

/// Which numbers are written with a sign.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Sign {
    /// `-`, the default: only negative numbers.
    Minus,
    /// `+`: every number.
    Plus,
    /// ` `: a space before a number that is not negative.
    Space,
}

/// What a spec's type says to write.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Presentation {
    /// `d`, an int in decimal.
    Decimal,
    /// `b`, an int in binary.
    Binary,
    /// `o`, an int in octal.
    Octal,
    /// `x` or `X`, an int in hexadecimal, with uppercase digits for `X`.
    Hex { upper: bool },
    /// `f` or `F`, a number with a fixed count of digits after the point.
    Fixed { upper: bool },
    /// `e` or `E`, a number in scientific notation.
    Exponent { upper: bool },
    /// `g` or `G`, fixed or scientific by the number's size, without trailing zeros.
    General { upper: bool },
    /// `%`, a number times 100 with a fixed count of digits, then `%`.
    Percent,
    /// `s`, a str.
    Str,
}

impl Presentation {
    /// Returns the type's character, as written in a spec.
    ///
    /// For example, `Presentation::Hex { upper: true }.letter()` returns `'X'`.
    pub fn letter(self) -> char {
        match self {
            Presentation::Decimal => 'd',
            Presentation::Binary => 'b',
            Presentation::Octal => 'o',
            Presentation::Hex { upper } => upper_or(upper, 'x'),
            Presentation::Fixed { upper } => upper_or(upper, 'f'),
            Presentation::Exponent { upper } => upper_or(upper, 'e'),
            Presentation::General { upper } => upper_or(upper, 'g'),
            Presentation::Percent => '%',
            Presentation::Str => 's',
        }
    }

    /// Returns whether the type writes an int's digits, as `d` and `x` do.
    pub fn is_int(self) -> bool {
        matches!(
            self,
            Presentation::Decimal
                | Presentation::Binary
                | Presentation::Octal
                | Presentation::Hex { .. }
        )
    }

    /// Returns whether the type writes a float, which an int is converted to first, as `f` and
    /// `e` do.
    pub fn is_float(self) -> bool {
        matches!(
            self,
            Presentation::Fixed { .. }
                | Presentation::Exponent { .. }
                | Presentation::General { .. }
                | Presentation::Percent
        )
    }

    fn of(letter: char) -> Option<Presentation> {
        Some(match letter {
            'd' => Presentation::Decimal,
            'b' => Presentation::Binary,
            'o' => Presentation::Octal,
            'x' | 'X' => Presentation::Hex {
                upper: letter == 'X',
            },
            'f' | 'F' => Presentation::Fixed {
                upper: letter == 'F',
            },
            'e' | 'E' => Presentation::Exponent {
                upper: letter == 'E',
            },
            'g' | 'G' => Presentation::General {
                upper: letter == 'G',
            },
            '%' => Presentation::Percent,
            's' => Presentation::Str,
            _ => return None,
        })
    }
}

fn upper_or(upper: bool, letter: char) -> char {
    if upper {
        letter.to_ascii_uppercase()
    } else {
        letter
    }
}

/// A parsed format spec.
///
/// For example, `FormatSpec::parse(">8.2f")` gives a right-aligned width of 8 and two digits
/// after the point, and `FormatSpec::parse("08")` a width of 8 filled with zeros.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FormatSpec {
    /// The character the width is filled with, a space unless given.
    pub fill: char,
    /// Where the value goes, or `None` for its type's default.
    pub align: Option<Align>,
    pub sign: Sign,
    /// `#`: a prefix such as `0x` for an int, and a point a float always keeps.
    pub alternate: bool,
    /// The `0` before the width, with no fill given: the width is filled with zeros, after the
    /// sign for a number that gives no alignment.
    pub zero: bool,
    /// The least count of characters the value takes, or 0 for none.
    pub width: usize,
    /// `,` or `_` between groups of digits.
    pub grouping: Option<char>,
    pub precision: Option<usize>,
    pub ty: Option<Presentation>,
}

impl FormatSpec {
    /// Parses the text after a value's `:`, or returns an error message and the offset, in
    /// chars, of where in the text it went wrong.
    ///
    /// For example, `parse(".2f")` is `Ok`, and `parse(".f")` is an error at offset 1, since a
    /// `.` must be followed by a precision.
    pub fn parse(text: &str) -> Result<FormatSpec, (usize, String)> {
        let chars: Vec<char> = text.chars().collect();
        let mut pos = 0;
        let mut spec = FormatSpec {
            fill: ' ',
            align: None,
            sign: Sign::Minus,
            alternate: false,
            zero: false,
            width: 0,
            grouping: None,
            precision: None,
            ty: None,
        };
        let align_of = |c: Option<&char>| match c {
            Some('<') => Some(Align::Left),
            Some('>') => Some(Align::Right),
            Some('^') => Some(Align::Center),
            Some('=') => Some(Align::AfterSign),
            _ => None,
        };
        let mut fill_given = false;
        if let Some(align) = align_of(chars.get(1)) {
            spec.fill = chars[0];
            spec.align = Some(align);
            fill_given = true;
            pos = 2;
        } else if let Some(align) = align_of(chars.first()) {
            spec.align = Some(align);
            pos = 1;
        }
        match chars.get(pos) {
            Some('+') => spec.sign = Sign::Plus,
            Some(' ') => spec.sign = Sign::Space,
            Some('-') => {}
            _ => pos = pos.wrapping_sub(1),
        }
        pos = pos.wrapping_add(1);
        if chars.get(pos) == Some(&'z') {
            return Err((pos, "'z' is not supported in format specs".to_string()));
        }
        if chars.get(pos) == Some(&'#') {
            spec.alternate = true;
            pos += 1;
        }
        if !fill_given && chars.get(pos) == Some(&'0') {
            spec.zero = true;
            pos += 1;
        }
        spec.width = number(&chars, &mut pos)?.unwrap_or(0);
        if let Some(&separator @ (',' | '_')) = chars.get(pos) {
            spec.grouping = Some(separator);
            pos += 1;
            if let Some(',' | '_') = chars.get(pos) {
                return Err((
                    pos,
                    "a format spec groups digits with ',' or '_', not both".to_string(),
                ));
            }
        }
        if chars.get(pos) == Some(&'.') {
            pos += 1;
            match number(&chars, &mut pos)? {
                Some(precision) => spec.precision = Some(precision),
                None => {
                    return Err((
                        pos - 1,
                        "a '.' in a format spec must be followed by a precision, as in '.2f'"
                            .to_string(),
                    ));
                }
            }
        }
        let rest: String = chars[pos..].iter().collect();
        match chars[pos..] {
            [] => {}
            [letter] => match Presentation::of(letter) {
                Some(ty) => spec.ty = Some(ty),
                None if matches!(letter, 'c' | 'n') => {
                    return Err((pos, format!("format type '{letter}' is not supported")));
                }
                None => return Err((pos, format!("unknown format type '{letter}'"))),
            },
            _ => return Err((pos, format!("'{rest}' is not part of a format spec"))),
        }
        spec.check().map_err(|message| (pos, message))?;
        Ok(spec)
    }

    /// Reports what a spec's type rules out, whatever the value it is used with.
    fn check(&self) -> Result<(), String> {
        let Some(ty) = self.ty else {
            return Ok(());
        };
        let letter = ty.letter();
        if let Some(separator) = self.grouping {
            let groups = match ty {
                Presentation::Str => false,
                Presentation::Binary | Presentation::Octal | Presentation::Hex { .. } => {
                    separator == '_'
                }
                _ => true,
            };
            if !groups {
                return Err(format!("'{separator}' cannot group format type '{letter}'"));
            }
        }
        if ty == Presentation::Str {
            if self.sign != Sign::Minus {
                return Err("format type 's' takes no sign".to_string());
            }
            if self.alternate {
                return Err("format type 's' takes no '#'".to_string());
            }
            if self.align == Some(Align::AfterSign) {
                return Err("format type 's' cannot be aligned with '='".to_string());
            }
        }
        if ty.is_int() && self.precision.is_some() {
            return Err(format!("format type '{letter}' takes no precision"));
        }
        Ok(())
    }

    /// Returns whether the spec asks for something only a number has: a sign, `#`, `=`, or
    /// grouping, with no type to say more.
    pub fn needs_number(&self) -> bool {
        self.ty.is_none()
            && (self.sign != Sign::Minus
                || self.alternate
                || self.align == Some(Align::AfterSign)
                || self.grouping.is_some())
    }

    /// Returns the fill character and alignment the spec gives a number, or, if `numeric` is
    /// false, any other value, which is written as text.
    ///
    /// For example, `08` fills a number with zeros after its sign, and text with zeros on its
    /// right, as Python does.
    pub fn placement(&self, numeric: bool) -> (char, Align) {
        let fill = if self.zero { '0' } else { self.fill };
        let align = match self.align {
            Some(align) => align,
            None if numeric && self.zero => Align::AfterSign,
            None if numeric => Align::Right,
            None => Align::Left,
        };
        (fill, align)
    }
}

/// Reads the digits at `pos`, if any, as a width or precision.
fn number(chars: &[char], pos: &mut usize) -> Result<Option<usize>, (usize, String)> {
    let start = *pos;
    let mut value: usize = 0;
    while let Some(digit) = chars.get(*pos).and_then(|c| c.to_digit(10)) {
        value = value.saturating_mul(10).saturating_add(digit as usize);
        *pos += 1;
    }
    if *pos == start {
        return Ok(None);
    }
    if value > MAX_SPEC_NUMBER {
        let digits: String = chars[start..*pos].iter().collect();
        return Err((
            start,
            format!(
                "{digits} is too large for a format spec, whose numbers are at most {MAX_SPEC_NUMBER}"
            ),
        ));
    }
    Ok(Some(value))
}

/// Writes the sign `spec` gives a number, negative or not.
fn sign_text(spec: &FormatSpec, negative: bool) -> &'static str {
    match (negative, spec.sign) {
        (true, _) => "-",
        (false, Sign::Minus) => "",
        (false, Sign::Plus) => "+",
        (false, Sign::Space) => " ",
    }
}

/// Writes an int as `spec`'s int type says, with its sign and, for `#`, a prefix such as `0x`,
/// but before [`layout`] groups or pads it.
///
/// For example, an int of -255 with the spec `#x` gives `-0xff`, and with `+` gives `-255`.
pub fn int_text(value: i64, spec: &FormatSpec) -> String {
    let magnitude = value.unsigned_abs();
    let (prefix, digits) = match spec.ty {
        Some(Presentation::Binary) => ("0b", format!("{magnitude:b}")),
        Some(Presentation::Octal) => ("0o", format!("{magnitude:o}")),
        Some(Presentation::Hex { upper: false }) => ("0x", format!("{magnitude:x}")),
        Some(Presentation::Hex { upper: true }) => ("0X", format!("{magnitude:X}")),
        _ => ("", magnitude.to_string()),
    };
    let prefix = if spec.alternate { prefix } else { "" };
    format!("{}{prefix}{digits}", sign_text(spec, value < 0))
}

/// Writes a float as `spec` says, with its sign, but before [`layout`] groups or pads it. An int
/// formatted with a float's type is converted to the float nearest it first.
///
/// For example, 3.14159 with `.2f` gives `3.14`, with `e` gives `3.141590e+00`, and with `g`
/// gives `3.14159`; 0.25 with `.0%` gives `25%`. With no type, a precision makes it `g` that
/// keeps a `.0`, and no precision writes it as `print` does.
pub fn float_text(value: f64, spec: &FormatSpec) -> String {
    let negative = value.is_sign_negative() && !value.is_nan();
    let sign = sign_text(spec, negative);
    let (body, upper) = match spec.ty {
        Some(Presentation::Percent) => (format!("{}%", fixed(value * 100.0, spec)), false),
        Some(Presentation::Fixed { upper }) => (fixed(value, spec), upper),
        Some(Presentation::Exponent { upper }) => {
            let precision = spec.precision.unwrap_or(6);
            (exponent(value, precision, spec.alternate), upper)
        }
        Some(Presentation::General { upper }) => (
            general(value, spec.precision.unwrap_or(6), spec.alternate, false),
            upper,
        ),
        _ => match spec.precision {
            Some(precision) => (general(value, precision, spec.alternate, true), false),
            None => (shortest(value, spec.alternate), false),
        },
    };
    let body = if upper { body.to_uppercase() } else { body };
    format!("{sign}{body}")
}

/// Writes inf or nan without a sign, or `None` for a finite number.
fn special(value: f64) -> Option<String> {
    if value.is_nan() {
        Some("nan".to_string())
    } else if value.is_infinite() {
        Some("inf".to_string())
    } else {
        None
    }
}

/// Writes `|value|` with the spec's precision (6 if none) after the point, rounded half to
/// even on its exact value.
fn fixed(value: f64, spec: &FormatSpec) -> String {
    if let Some(text) = special(value) {
        return text;
    }
    let precision = spec.precision.unwrap_or(6);
    let text = format!("{:.*}", precision, value.abs());
    if spec.alternate && precision == 0 {
        format!("{text}.")
    } else {
        text
    }
}

/// Writes `|value|` in scientific notation with `precision` digits after the point and an
/// exponent of at least two digits, as `1.50e+03`.
fn exponent(value: f64, precision: usize, alternate: bool) -> String {
    if let Some(text) = special(value) {
        return text;
    }
    let text = format!("{:.*e}", precision, value.abs());
    let (mantissa, power) = text.split_once('e').expect("an exponent");
    let power: i32 = power.parse().expect("an exponent");
    let point = if alternate && precision == 0 { "." } else { "" };
    let power_sign = if power < 0 { '-' } else { '+' };
    format!("{mantissa}{point}e{power_sign}{:02}", power.abs())
}

/// Writes `|value|` to `precision` significant digits (1 for 0), fixed if its exponent is from
/// -4 up to the precision and scientific otherwise, dropping trailing zeros unless `alternate`.
/// With `dot_0`, as for a spec with a precision and no type, the switch to scientific comes one
/// digit sooner and a fixed number with no fraction keeps `.0`, as CPython writes them.
fn general(value: f64, precision: usize, alternate: bool, dot_0: bool) -> String {
    if let Some(text) = special(value) {
        return text;
    }
    let precision = precision.max(1);
    // the exponent after rounding to that many digits, as 9.99 to 2 digits is 1.0e+01
    let rounded = format!("{:.*e}", precision - 1, value.abs());
    let power: i64 = rounded
        .split_once('e')
        .expect("an exponent")
        .1
        .parse()
        .expect("an exponent");
    let limit = if dot_0 {
        precision as i64 - 1
    } else {
        precision as i64
    };
    let scientific = power < -4 || power >= limit;
    let mut text = if scientific {
        exponent(value, precision - 1, alternate)
    } else {
        let digits = (precision as i64 - 1 - power).max(0) as usize;
        let text = format!("{:.*}", digits, value.abs());
        if alternate && digits == 0 {
            format!("{text}.")
        } else {
            text
        }
    };
    if !alternate {
        let (mantissa, power) = match text.split_once('e') {
            Some((mantissa, power)) => (mantissa.to_string(), format!("e{power}")),
            None => (text.clone(), String::new()),
        };
        let mantissa = if mantissa.contains('.') {
            mantissa
                .trim_end_matches('0')
                .trim_end_matches('.')
                .to_string()
        } else {
            mantissa
        };
        text = format!("{mantissa}{power}");
    }
    if dot_0 && !text.contains(['.', 'e']) {
        text.push_str(".0");
    }
    text
}

/// Writes `|value|` as `print` does, with a point added before any exponent for `alternate`.
fn shortest(value: f64, alternate: bool) -> String {
    let text = format_float(value.abs());
    if alternate && !text.contains('.') && value.is_finite() {
        match text.split_once('e') {
            Some((mantissa, power)) => format!("{mantissa}.e{power}"),
            None => format!("{text}."),
        }
    } else {
        text
    }
}

/// Lays out a value's text in the spec's width: groups the digits of a number and fills the
/// rest of the width with the fill character, as CPython does. A number's text, from
/// [`int_text`] or [`float_text`], is a sign, any prefix, digits, and the rest (a point, more
/// digits, an exponent, a `%`); any other value's text is placed whole.
///
/// For example, `1234` with `,` gives `1,234`, with `010,` gives `00,001,234` (zeros are grouped
/// too), and `"ab"` with `^6` gives `  ab  `.
pub fn layout(text: &str, spec: &FormatSpec, numeric: bool) -> String {
    let (fill, align) = spec.placement(numeric);
    if !numeric {
        let missing = spec.width.saturating_sub(text.chars().count());
        let (left, right) = split_padding(missing, align);
        return format!(
            "{}{text}{}",
            fill.to_string().repeat(left),
            fill.to_string().repeat(right)
        );
    }
    let bytes = text.as_bytes();
    let mut pos = 0;
    if matches!(bytes.first(), Some(b'-' | b'+' | b' ')) {
        pos = 1;
    }
    // binary, octal, and hexadecimal digits are grouped by fours, after any `0x`
    let radix = matches!(
        spec.ty,
        Some(Presentation::Binary | Presentation::Octal | Presentation::Hex { .. })
    );
    if radix && spec.alternate {
        pos += 2;
    }
    let every = if radix { 4 } else { 3 };
    let lead = &text[..pos];
    let digits_end = pos
        + text[pos..]
            .bytes()
            .take_while(|&b| {
                if radix {
                    b.is_ascii_hexdigit()
                } else {
                    b.is_ascii_digit()
                }
            })
            .count();
    let digits = &text[pos..digits_end];
    let rest = &text[digits_end..];
    let others = lead.chars().count() + rest.chars().count();
    let min_width = if fill == '0' && align == Align::AfterSign {
        spec.width as i64 - others as i64
    } else {
        0
    };
    let grouped = group(digits, spec.grouping, every, min_width);
    let missing = spec.width.saturating_sub(others + grouped.chars().count());
    let fill_text = |count: usize| fill.to_string().repeat(count);
    match align {
        Align::AfterSign => format!("{lead}{}{grouped}{rest}", fill_text(missing)),
        _ => {
            let (left, right) = split_padding(missing, align);
            format!(
                "{}{lead}{grouped}{rest}{}",
                fill_text(left),
                fill_text(right)
            )
        }
    }
}

/// Splits `missing` characters of padding into what goes left and right of a value.
fn split_padding(missing: usize, align: Align) -> (usize, usize) {
    match align {
        Align::Left => (0, missing),
        Align::Center => (missing / 2, missing - missing / 2),
        _ => (missing, 0),
    }
}

/// Groups `digits` by `every` from the right with `separator`, padding them with zeros (grouped
/// too) to at least `min_width` characters, as CPython's `_PyUnicode_InsertThousandsGrouping`
/// does. A group of padding never starts with a separator, so the result may be one character
/// wider than `min_width`.
///
/// For example, `group("1234", Some(','), 3, 10)` returns `00,001,234`, and
/// `group("1234", Some(','), 3, 8)` returns `0,001,234`.
fn group(digits: &str, separator: Option<char>, every: usize, min_width: i64) -> String {
    if digits.is_empty() {
        return String::new();
    }
    let Some(separator) = separator else {
        let zeros = (min_width - digits.len() as i64).max(0) as usize;
        return format!("{}{digits}", "0".repeat(zeros));
    };
    let mut groups: Vec<String> = Vec::new();
    let mut remaining = digits.len() as i64;
    let mut min_width = min_width;
    let every = every as i64;
    loop {
        let length = every.min(remaining.max(min_width).max(1));
        let chars = remaining.min(length).max(0);
        let zeros = (length - remaining).max(0);
        let end = remaining as usize;
        let start = (remaining - chars) as usize;
        groups.push(format!(
            "{}{}",
            "0".repeat(zeros as usize),
            &digits[start..end]
        ));
        remaining -= chars;
        min_width -= length;
        if remaining <= 0 && min_width <= 0 {
            break;
        }
        min_width -= 1;
    }
    groups.reverse();
    groups.join(&separator.to_string())
}

/// Cuts text to at most `precision` characters, as a precision does to a str.
///
/// For example, `truncate("hello", 3)` returns `hel`.
pub fn truncate(text: &str, precision: usize) -> &str {
    match text.char_indices().nth(precision) {
        Some((end, _)) => &text[..end],
        None => text,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn spec(text: &str) -> FormatSpec {
        FormatSpec::parse(text).unwrap_or_else(|e| panic!("{text}: {e:?}"))
    }

    fn int(value: i64, text: &str) -> String {
        let spec = spec(text);
        let written = if spec.ty.is_some_and(Presentation::is_float) {
            float_text(value as f64, &spec)
        } else {
            int_text(value, &spec)
        };
        layout(&written, &spec, true)
    }

    fn float(value: f64, text: &str) -> String {
        let spec = spec(text);
        layout(&float_text(value, &spec), &spec, true)
    }

    fn text(value: &str, text: &str) -> String {
        let spec = spec(text);
        let value = match spec.precision {
            Some(precision) => truncate(value, precision),
            None => value,
        };
        layout(value, &spec, false)
    }

    #[test]
    fn specs_parse_as_python_reads_them() {
        let parsed = spec("*^+#012,.3f");
        assert_eq!(parsed.fill, '*');
        assert_eq!(parsed.align, Some(Align::Center));
        assert_eq!(parsed.sign, Sign::Plus);
        assert!(parsed.alternate);
        // with a fill given, the 0 is part of the width
        assert!(!parsed.zero);
        assert_eq!(parsed.width, 12);
        assert_eq!(parsed.grouping, Some(','));
        assert_eq!(parsed.precision, Some(3));
        assert_eq!(parsed.ty, Some(Presentation::Fixed { upper: false }));
        assert!(spec("08").zero);
        assert_eq!(spec("08").width, 8);
        assert_eq!(spec("").ty, None);
        // a fill may be any character, even one that means something elsewhere
        assert_eq!(spec("0<5").fill, '0');
        assert_eq!(spec("é>5").fill, 'é');
        assert_eq!(spec("<<5").fill, '<');
    }

    #[test]
    fn bad_specs_are_errors_where_they_go_wrong() {
        let error = |text: &str| FormatSpec::parse(text).unwrap_err();
        assert_eq!(error(".f").0, 0);
        assert!(error(".f").1.contains("precision"));
        assert_eq!(error("q"), (0, "unknown format type 'q'".to_string()));
        assert_eq!(
            error("5c"),
            (1, "format type 'c' is not supported".to_string())
        );
        assert_eq!(error("abc").1, "'abc' is not part of a format spec");
        assert!(error(",_d").1.contains("not both"));
        assert_eq!(error(",x").1, "',' cannot group format type 'x'");
        assert_eq!(error(",s").1, "',' cannot group format type 's'");
        assert_eq!(error("+s").1, "format type 's' takes no sign");
        assert_eq!(error("=5s").1, "format type 's' cannot be aligned with '='");
        assert_eq!(error(".2d").1, "format type 'd' takes no precision");
        assert!(error("z.2f").1.contains("'z'"));
        assert!(error("10001").1.contains("too large"));
        // '_' groups hexadecimal by fours
        assert!(FormatSpec::parse("_x").is_ok());
    }

    #[test]
    fn ints_are_written_as_python_writes_them() {
        assert_eq!(int(42, ""), "42");
        assert_eq!(int(42, "5"), "   42");
        assert_eq!(int(42, "<5"), "42   ");
        assert_eq!(int(42, "^5"), " 42  ");
        assert_eq!(int(-42, "05"), "-0042");
        assert_eq!(int(-42, "=+6"), "-   42");
        assert_eq!(int(42, "+"), "+42");
        assert_eq!(int(42, " "), " 42");
        assert_eq!(int(1234567, ","), "1,234,567");
        assert_eq!(int(1234567, "_"), "1_234_567");
        assert_eq!(int(1234, "010,"), "00,001,234");
        assert_eq!(int(1234, "09,"), "0,001,234");
        assert_eq!(int(1234, "08,"), "0,001,234");
        assert_eq!(int(1234, "07,"), "001,234");
        assert_eq!(int(1234, "0>10,"), "000001,234");
        assert_eq!(int(255, "x"), "ff");
        assert_eq!(int(255, "#X"), "0XFF");
        assert_eq!(int(-255, "#x"), "-0xff");
        assert_eq!(int(5, "#010b"), "0b00000101");
        assert_eq!(int(5, "b"), "101");
        assert_eq!(int(8, "#o"), "0o10");
        assert_eq!(int(0xdeadbeef, "_x"), "dead_beef");
        assert_eq!(int(0xdeadbeef, "#_x"), "0xdead_beef");
        assert_eq!(int(i64::MIN, "d"), "-9223372036854775808");
        assert_eq!(int(i64::MIN, "x"), "-8000000000000000");
        assert_eq!(int(3, ".2f"), "3.00");
        assert_eq!(int(1, "%"), "100.000000%");
    }

    #[test]
    fn floats_are_written_as_python_writes_them() {
        assert_eq!(float(1.23456, ".2f"), "1.23");
        assert_eq!(float(2.675, ".2f"), "2.67");
        assert_eq!(float(0.125, ".2f"), "0.12");
        assert_eq!(float(0.375, ".2f"), "0.38");
        assert_eq!(float(-0.001, ".2f"), "-0.00");
        assert_eq!(float(-0.0, "f"), "-0.000000");
        assert_eq!(float(1.0, ".0f"), "1");
        assert_eq!(float(1.0, "#.0f"), "1.");
        assert_eq!(float(1e300, ".0f").len(), 301);
        assert_eq!(float(1.23456, "e"), "1.234560e+00");
        assert_eq!(float(12345.678, ".2e"), "1.23e+04");
        assert_eq!(float(1e-300, "E"), "1.000000E-300");
        assert_eq!(float(0.0, ".2e"), "0.00e+00");
        assert_eq!(float(1.5, "#.0e"), "2.e+00");
        assert_eq!(float(1.23456, "g"), "1.23456");
        assert_eq!(float(1234567.0, "g"), "1.23457e+06");
        assert_eq!(float(0.0001, "g"), "0.0001");
        assert_eq!(float(0.00001, "g"), "1e-05");
        assert_eq!(float(100.0, "g"), "100");
        assert_eq!(float(100.0, "#g"), "100.000");
        assert_eq!(float(9.99, ".2g"), "10");
        assert_eq!(float(123.0, ".3"), "1.23e+02");
        assert_eq!(float(12.0, ".3"), "12.0");
        assert_eq!(float(0.5, ".3"), "0.5");
        assert_eq!(float(1e20, ".3"), "1e+20");
        assert_eq!(float(1.0, ""), "1.0");
        assert_eq!(float(1e16, ""), "1e+16");
        assert_eq!(float(1e16, "#"), "1.e+16");
        assert_eq!(float(0.25, ".1%"), "25.0%");
        assert_eq!(float(0.5, "%"), "50.000000%");
        assert_eq!(float(f64::INFINITY, "f"), "inf");
        assert_eq!(float(f64::NEG_INFINITY, "F"), "-INF");
        assert_eq!(float(f64::NAN, "+f"), "+nan");
        assert_eq!(float(-f64::NAN, "f"), "nan");
        assert_eq!(float(f64::INFINITY, "010"), "0000000inf");
        assert_eq!(float(f64::INFINITY, "%"), "inf%");
        assert_eq!(float(1234567.891, ","), "1,234,567.891");
        assert_eq!(float(1234567.891, "015,.2f"), "0,001,234,567.89");
        assert_eq!(float(1e20, ","), "1e+20");
        assert_eq!(float(-1.5, "^+9.2f"), "  -1.50  ");
        assert_eq!(float(2.5, "*<8"), "2.5*****");
        assert_eq!(float(1234.5, "_.1f"), "1_234.5");
    }

    #[test]
    fn text_is_padded_and_cut() {
        assert_eq!(text("ab", "5"), "ab   ");
        assert_eq!(text("ab", ">5"), "   ab");
        assert_eq!(text("ab", "^5"), " ab  ");
        assert_eq!(text("ab", "*^6"), "**ab**");
        assert_eq!(text("ab", "05"), "ab000");
        assert_eq!(text("hello", ".3"), "hel");
        assert_eq!(text("héllo", ".2s"), "hé");
        assert_eq!(text("héllo", "7"), "héllo  ");
        assert_eq!(text("ab", "-<1"), "ab");
    }
}
