//! Source positions and ranges, used to point diagnostics and editor features at source text.
//!
//! For example, the `42` in `x = 42` spans from line 1, col 5 up to (but not including) col 7.

/// A position in source text, with the line and column both counted from 1.
///
/// Columns count characters, not bytes, so `é` advances the column by one.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(line: usize, col: usize) -> Self {
        Pos { line, col }
    }
}

/// A half-open range of source text from `start` up to but not including `end`.
///
/// For example, `Span::new(Pos::new(1, 5), Pos::new(1, 7))` covers the two characters of `42` in
/// `x = 42`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }

    /// Returns an empty span at `pos`, for tokens like `Indent` that cover no text.
    ///
    /// For example, `Span::empty(Pos::new(2, 1))` starts and ends at line 2, col 1.
    pub fn empty(pos: Pos) -> Self {
        Span {
            start: pos,
            end: pos,
        }
    }

    /// Returns whether `pos` is inside the span, counting the position just past its end, which is
    /// where an editor's cursor sits after typing it.
    ///
    /// For example, the span of `add` at cols 1 to 4 contains cols 1 through 4.
    pub fn contains(&self, pos: Pos) -> bool {
        self.start <= pos && pos <= self.end
    }

    /// Returns the span from the start of `self` to the end of `other`.
    ///
    /// For example, joining the spans of `a` and `b` in `a + b` gives the span of the whole sum.
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
        }
    }
}
