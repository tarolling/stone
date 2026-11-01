//! A small JSON reader, for the lists of releases GitHub and GitLab answer with.
//!
//! For example, `parse(r#"[{"tag_name": "v1.0.0", "draft": false}]"#)` gives an array of one
//! object, whose `tag_name` is the string `v1.0.0`.

/// A JSON value. Numbers keep their text, since releases need none of them.
#[derive(Debug, Clone, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Number(String),
    Str(String),
    Array(Vec<Json>),
    Object(Vec<(String, Json)>),
}

impl Json {
    /// Returns the value of `key`, if this is an object that has it.
    pub fn get(&self, key: &str) -> Option<&Json> {
        match self {
            Json::Object(entries) => entries.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Json::Str(text) => Some(text),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Json::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[Json]> {
        match self {
            Json::Array(items) => Some(items),
            _ => None,
        }
    }
}

/// The deepest arrays and objects may nest, so a hostile answer cannot overflow the stack.
const MAX_DEPTH: usize = 128;

/// Reads a JSON document, or says where it is not JSON.
pub fn parse(text: &str) -> Result<Json, String> {
    let mut reader = Reader {
        bytes: text.as_bytes(),
        at: 0,
    };
    let value = reader.value(0)?;
    reader.blanks();
    if reader.at != reader.bytes.len() {
        return Err(reader.error("expected the end"));
    }
    Ok(value)
}

struct Reader<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl Reader<'_> {
    fn error(&self, message: &str) -> String {
        format!("{message} at byte {}", self.at)
    }

    fn blanks(&mut self) {
        while matches!(self.bytes.get(self.at), Some(b' ' | b'\t' | b'\n' | b'\r')) {
            self.at += 1;
        }
    }

    fn eat(&mut self, byte: u8) -> bool {
        self.blanks();
        if self.bytes.get(self.at) == Some(&byte) {
            self.at += 1;
            true
        } else {
            false
        }
    }

    fn word(&mut self, word: &str, value: Json) -> Result<Json, String> {
        if self.bytes[self.at..].starts_with(word.as_bytes()) {
            self.at += word.len();
            Ok(value)
        } else {
            Err(self.error("expected a value"))
        }
    }

    fn value(&mut self, depth: usize) -> Result<Json, String> {
        if depth > MAX_DEPTH {
            return Err(self.error("nested too deeply"));
        }
        self.blanks();
        match self.bytes.get(self.at) {
            Some(b'{') => {
                self.at += 1;
                let mut entries = vec![];
                if self.eat(b'}') {
                    return Ok(Json::Object(entries));
                }
                loop {
                    self.blanks();
                    let key = self.string()?;
                    if !self.eat(b':') {
                        return Err(self.error("expected ':'"));
                    }
                    entries.push((key, self.value(depth + 1)?));
                    if self.eat(b'}') {
                        return Ok(Json::Object(entries));
                    }
                    if !self.eat(b',') {
                        return Err(self.error("expected ',' or '}'"));
                    }
                }
            }
            Some(b'[') => {
                self.at += 1;
                let mut items = vec![];
                if self.eat(b']') {
                    return Ok(Json::Array(items));
                }
                loop {
                    items.push(self.value(depth + 1)?);
                    if self.eat(b']') {
                        return Ok(Json::Array(items));
                    }
                    if !self.eat(b',') {
                        return Err(self.error("expected ',' or ']'"));
                    }
                }
            }
            Some(b'"') => Ok(Json::Str(self.string()?)),
            Some(b't') => self.word("true", Json::Bool(true)),
            Some(b'f') => self.word("false", Json::Bool(false)),
            Some(b'n') => self.word("null", Json::Null),
            Some(b'-' | b'0'..=b'9') => {
                let start = self.at;
                while matches!(
                    self.bytes.get(self.at),
                    Some(b'-' | b'+' | b'.' | b'e' | b'E' | b'0'..=b'9')
                ) {
                    self.at += 1;
                }
                let text = std::str::from_utf8(&self.bytes[start..self.at]).unwrap_or_default();
                Ok(Json::Number(text.to_string()))
            }
            _ => Err(self.error("expected a value")),
        }
    }

    fn string(&mut self) -> Result<String, String> {
        if self.bytes.get(self.at) != Some(&b'"') {
            return Err(self.error("expected a string"));
        }
        self.at += 1;
        let mut text = Vec::new();
        loop {
            let Some(&byte) = self.bytes.get(self.at) else {
                return Err(self.error("this string is not closed"));
            };
            self.at += 1;
            match byte {
                b'"' => {
                    return String::from_utf8(text).map_err(|_| self.error("not UTF-8"));
                }
                b'\\' => {
                    let Some(&escape) = self.bytes.get(self.at) else {
                        return Err(self.error("this string is not closed"));
                    };
                    self.at += 1;
                    let c = match escape {
                        b'"' => '"',
                        b'\\' => '\\',
                        b'/' => '/',
                        b'b' => '\u{8}',
                        b'f' => '\u{c}',
                        b'n' => '\n',
                        b'r' => '\r',
                        b't' => '\t',
                        b'u' => {
                            let first = self.hex4()?;
                            let code = if (0xd800..0xdc00).contains(&first) {
                                if !self.bytes[self.at..].starts_with(b"\\u") {
                                    return Err(self.error("a surrogate is not paired"));
                                }
                                self.at += 2;
                                let second = self.hex4()?;
                                0x10000 + ((first - 0xd800) << 10) + (second.wrapping_sub(0xdc00))
                            } else {
                                first
                            };
                            char::from_u32(code).ok_or_else(|| self.error("not a character"))?
                        }
                        _ => return Err(self.error("unknown escape")),
                    };
                    let mut buffer = [0; 4];
                    text.extend_from_slice(c.encode_utf8(&mut buffer).as_bytes());
                }
                byte => text.push(byte),
            }
        }
    }

    fn hex4(&mut self) -> Result<u32, String> {
        let digits = self
            .bytes
            .get(self.at..self.at + 4)
            .and_then(|d| std::str::from_utf8(d).ok())
            .and_then(|d| u32::from_str_radix(d, 16).ok())
            .ok_or_else(|| self.error("expected four hexadecimal digits"))?;
        self.at += 4;
        Ok(digits)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn values_of_every_kind_are_read() {
        let value =
            parse(r#" { "a": [1, -2.5e3, true, false, null], "b": "x\"é😀", "c": {} } "#).unwrap();
        assert_eq!(
            value.get("a").unwrap().as_array().unwrap(),
            [
                Json::Number("1".into()),
                Json::Number("-2.5e3".into()),
                Json::Bool(true),
                Json::Bool(false),
                Json::Null
            ]
        );
        assert_eq!(value.get("b").unwrap().as_str(), Some("x\"\u{e9}\u{1f600}"));
        assert_eq!(value.get("c"), Some(&Json::Object(vec![])));
    }

    #[test]
    fn bad_documents_are_errors() {
        for bad in ["", "[1,]", "{\"a\" 1}", "\"open", "[1] 2", "tru"] {
            assert!(parse(bad).is_err(), "{bad}");
        }
        assert!(
            parse(&"[".repeat(1000))
                .unwrap_err()
                .contains("nested too deeply")
        );
    }
}
