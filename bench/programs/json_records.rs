// json_records: writing a list of records as JSON text, reading it back, and walking what was read
enum Value {
    Null,
    Boolean(bool),
    Integer(i64),
    Number(f64),
    Text(String),
    Array(Vec<Value>),
    Object(Vec<(String, Value)>),
}

fn records(n: i64) -> Value {
    let mut items = Vec::new();
    let mut x: i64 = 12345;
    for i in 0..n {
        x = (x * 1103515245 + 12345) % 2147483648;
        let mut name = format!("user{}", x % 1000);
        if x % 7 == 0 {
            name = format!("say \"hi\"\n{name}");
        }
        let tags = (0..x % 4).map(|k| Value::Text(format!("tag{k}"))).collect();
        let pos = vec![
            ("x".to_string(), Value::Integer(x % 640)),
            ("y".to_string(), Value::Integer(x / 640 % 480)),
        ];
        items.push(Value::Object(vec![
            ("id".to_string(), Value::Integer(i)),
            ("name".to_string(), Value::Text(name)),
            ("score".to_string(), Value::Number((x % 4000) as f64 / 4.0)),
            ("active".to_string(), Value::Boolean(x % 3 == 0)),
            ("tags".to_string(), Value::Array(tags)),
            ("pos".to_string(), Value::Object(pos)),
            ("note".to_string(), Value::Null),
        ]));
    }
    Value::Array(items)
}

fn write_text(out: &mut String, s: &str) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            c => out.push(c),
        }
    }
    out.push('"');
}

/// Writes `v` as compact JSON, as Python's `json.dumps` does with compact separators.
fn write(out: &mut String, v: &Value) {
    match v {
        Value::Null => out.push_str("null"),
        Value::Boolean(b) => out.push_str(if *b { "true" } else { "false" }),
        Value::Integer(n) => out.push_str(&n.to_string()),
        // Debug writes Python's shortest repr, with at least one digit after the point
        Value::Number(f) => out.push_str(&format!("{f:?}")),
        Value::Text(s) => write_text(out, s),
        Value::Array(items) => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write(out, item);
            }
            out.push(']');
        }
        Value::Object(entries) => {
            out.push('{');
            for (i, (key, item)) in entries.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_text(out, key);
                out.push(':');
                write(out, item);
            }
            out.push('}');
        }
    }
}

struct Parser<'a> {
    bytes: &'a [u8],
    at: usize,
}

impl Parser<'_> {
    fn peek(&self) -> u8 {
        self.bytes[self.at]
    }

    fn space(&mut self) {
        while matches!(self.peek(), b' ' | b'\n' | b'\r' | b'\t') {
            self.at += 1;
        }
    }

    fn text(&mut self) -> String {
        self.at += 1;
        let mut out = Vec::new();
        while self.peek() != b'"' {
            let mut byte = self.peek();
            if byte == b'\\' {
                self.at += 1;
                byte = match self.peek() {
                    b'n' => b'\n',
                    b't' => b'\t',
                    other => other,
                };
            }
            out.push(byte);
            self.at += 1;
        }
        self.at += 1;
        String::from_utf8(out).unwrap()
    }

    fn value(&mut self) -> Value {
        self.space();
        match self.peek() {
            b'{' => {
                self.at += 1;
                let mut entries = Vec::new();
                self.space();
                if self.peek() == b'}' {
                    self.at += 1;
                    return Value::Object(entries);
                }
                loop {
                    self.space();
                    let key = self.text();
                    self.space();
                    self.at += 1; // ':'
                    entries.push((key, self.value()));
                    self.space();
                    self.at += 1;
                    if self.bytes[self.at - 1] == b'}' {
                        return Value::Object(entries);
                    }
                }
            }
            b'[' => {
                self.at += 1;
                let mut items = Vec::new();
                self.space();
                if self.peek() == b']' {
                    self.at += 1;
                    return Value::Array(items);
                }
                loop {
                    items.push(self.value());
                    self.space();
                    self.at += 1;
                    if self.bytes[self.at - 1] == b']' {
                        return Value::Array(items);
                    }
                }
            }
            b'"' => Value::Text(self.text()),
            b't' => {
                self.at += 4;
                Value::Boolean(true)
            }
            b'f' => {
                self.at += 5;
                Value::Boolean(false)
            }
            b'n' => {
                self.at += 4;
                Value::Null
            }
            _ => {
                let start = self.at;
                let mut exact = true;
                if self.peek() == b'-' {
                    self.at += 1;
                }
                while self.at < self.bytes.len()
                    && matches!(self.peek(), b'0'..=b'9' | b'.' | b'e' | b'E' | b'+' | b'-')
                {
                    exact &= !matches!(self.peek(), b'.' | b'e' | b'E');
                    self.at += 1;
                }
                let number = std::str::from_utf8(&self.bytes[start..self.at]).unwrap();
                if exact {
                    Value::Integer(number.parse().unwrap())
                } else {
                    Value::Number(number.parse().unwrap())
                }
            }
        }
    }
}

fn checksum(v: &Value) -> i64 {
    match v {
        Value::Null => 1,
        Value::Boolean(b) => {
            if *b {
                3
            } else {
                2
            }
        }
        Value::Integer(n) => *n,
        Value::Number(f) => (f * 4.0) as i64,
        Value::Text(s) => s.len() as i64,
        Value::Array(items) => 7 + items.iter().map(checksum).sum::<i64>(),
        Value::Object(entries) => {
            11 + entries
                .iter()
                .map(|(key, item)| key.len() as i64 + checksum(item))
                .sum::<i64>()
        }
    }
}

fn run(rounds: i64, n: i64) -> i64 {
    let doc = records(n);
    let mut total = 0;
    for _ in 0..rounds {
        let mut text = String::new();
        write(&mut text, &doc);
        let mut parser = Parser {
            bytes: text.as_bytes(),
            at: 0,
        };
        total += text.len() as i64 + checksum(&parser.value());
    }
    total
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    println!(
        "{}",
        run(args[1].parse().unwrap(), args[2].parse().unwrap())
    );
}
