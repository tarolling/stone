//! Conversion between stone's source positions and LSP positions.
//!
//! stone counts lines and columns from 1, with columns in characters. LSP counts both from 0, with
//! columns in UTF-16 code units by default, or UTF-8 bytes if the client and server agree on it.
//! For example, in `s = "é!"`, stone puts `!` at col 7 while LSP puts it at character 6 in UTF-16
//! and 7 in UTF-8.

use lsp_types::{Position, Range};
use stone::span::{Pos, Span};

/// How an LSP position counts columns.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Encoding {
    Utf8,
    Utf16,
}

impl Encoding {
    fn width(self, ch: char) -> u32 {
        match self {
            Encoding::Utf8 => ch.len_utf8() as u32,
            Encoding::Utf16 => ch.len_utf16() as u32,
        }
    }
}

/// The lines of a document, for converting positions within it.
pub struct LineIndex {
    lines: Vec<Vec<char>>,
    encoding: Encoding,
}

impl LineIndex {
    pub fn new(text: &str, encoding: Encoding) -> Self {
        let lines = text
            .split('\n')
            .map(|line| line.trim_end_matches('\r').chars().collect())
            .collect();
        LineIndex { lines, encoding }
    }

    fn line(&self, number: usize) -> &[char] {
        self.lines.get(number).map_or(&[], Vec::as_slice)
    }

    /// Converts a stone position to an LSP position.
    ///
    /// Columns past the end of a line, such as the end of a newline token, count one unit each.
    pub fn position(&self, pos: Pos) -> Position {
        let line = pos.line.saturating_sub(1);
        let chars = pos.col.saturating_sub(1);
        let text = self.line(line);
        let within: u32 = text
            .iter()
            .take(chars)
            .map(|&ch| self.encoding.width(ch))
            .sum();
        let past_end = chars.saturating_sub(text.len()) as u32;
        Position::new(line as u32, within + past_end)
    }

    /// Converts an LSP position to a stone position, rounding a position inside a character, such
    /// as between the two UTF-16 units of an emoji, up to the next character.
    pub fn pos(&self, position: Position) -> Pos {
        let line = position.line as usize;
        let mut units = 0;
        let mut chars = 0;
        for &ch in self.line(line) {
            if units >= position.character {
                break;
            }
            units += self.encoding.width(ch);
            chars += 1;
        }
        let past_end = position.character.saturating_sub(units) as usize;
        Pos::new(line + 1, chars + past_end + 1)
    }

    /// Converts a stone span to an LSP range.
    pub fn range(&self, span: Span) -> Range {
        Range::new(self.position(span.start), self.position(span.end))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lsp(line: u32, character: u32) -> Position {
        Position::new(line, character)
    }

    #[test]
    fn ascii_columns_shift_by_one() {
        let index = LineIndex::new("x = 1\ny = 2\n", Encoding::Utf16);
        assert_eq!(index.position(Pos::new(1, 1)), lsp(0, 0));
        assert_eq!(index.position(Pos::new(2, 5)), lsp(1, 4));
        assert_eq!(index.pos(lsp(1, 4)), Pos::new(2, 5));
    }

    #[test]
    fn utf16_counts_code_units() {
        // é is one UTF-16 unit and 😀 is two
        let index = LineIndex::new("s = \"é😀!\"\n", Encoding::Utf16);
        assert_eq!(index.position(Pos::new(1, 7)), lsp(0, 6));
        assert_eq!(index.position(Pos::new(1, 8)), lsp(0, 8));
        assert_eq!(index.pos(lsp(0, 8)), Pos::new(1, 8));
    }

    #[test]
    fn utf8_counts_bytes() {
        // é is two bytes and 😀 is four
        let index = LineIndex::new("s = \"é😀!\"\n", Encoding::Utf8);
        assert_eq!(index.position(Pos::new(1, 7)), lsp(0, 7));
        assert_eq!(index.position(Pos::new(1, 8)), lsp(0, 11));
        assert_eq!(index.pos(lsp(0, 11)), Pos::new(1, 8));
    }

    #[test]
    fn a_position_inside_a_character_rounds_up() {
        // UTF-16 unit 7 falls between the two halves of 😀
        let index = LineIndex::new("s = \"é😀!\"\n", Encoding::Utf16);
        assert_eq!(index.pos(lsp(0, 7)), Pos::new(1, 8));
    }

    #[test]
    fn positions_past_the_end_of_a_line_count_one_unit_per_column() {
        let index = LineIndex::new("é\n", Encoding::Utf16);
        // the newline token's end, just past the newline itself
        assert_eq!(index.position(Pos::new(1, 3)), lsp(0, 2));
        assert_eq!(index.pos(lsp(0, 5)), Pos::new(1, 6));
        // a line past the end of the document, where stone puts end of file
        assert_eq!(index.position(Pos::new(2, 1)), lsp(1, 0));
        assert_eq!(index.pos(lsp(4, 2)), Pos::new(5, 3));
    }

    #[test]
    fn carriage_returns_are_not_columns() {
        let index = LineIndex::new("ab\r\ncd\r\n", Encoding::Utf16);
        assert_eq!(index.position(Pos::new(2, 3)), lsp(1, 2));
    }
}
