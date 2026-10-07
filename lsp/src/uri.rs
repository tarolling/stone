//! Conversion between `file://` URIs and paths.
//!
//! For example, `/tmp/my dir/main.st` and `file:///tmp/my%20dir/main.st` name the same file.

use std::path::{Path, PathBuf};

use lsp_types::Uri;

/// Returns the `file://` URI of an absolute path, percent-encoding anything but unreserved
/// characters and separators.
///
/// For example, `/tmp/my dir/builtins.st` becomes `file:///tmp/my%20dir/builtins.st`, and
/// `C:\Temp\builtins.st` becomes `file:///C:/Temp/builtins.st`.
pub fn file_uri(path: &Path) -> Option<Uri> {
    let path = path.to_str()?.replace('\\', "/");
    let path = if path.starts_with('/') {
        path
    } else {
        format!("/{path}")
    };
    let encoded: String = path
        .bytes()
        .map(|byte| {
            if byte.is_ascii_alphanumeric() || b"/-._~:".contains(&byte) {
                (byte as char).to_string()
            } else {
                format!("%{byte:02X}")
            }
        })
        .collect();
    format!("file://{encoded}").parse().ok()
}

/// Returns the path a `file://` URI names, or `None` for any other kind of URI, such as an
/// editor's unsaved `untitled:` buffer.
///
/// For example, `file:///tmp/my%20dir/main.st` is `/tmp/my dir/main.st`, and
/// `file:///C:/Temp/main.st` is `C:/Temp/main.st`.
pub fn file_path(uri: &Uri) -> Option<PathBuf> {
    let rest = uri.as_str().strip_prefix("file://")?;
    // an authority such as `localhost` may come before the path
    let path = percent_decode(&rest[rest.find('/')?..])?;
    let bytes = path.as_bytes();
    // a Windows path starts with its drive letter, as in `/C:/Temp`
    if bytes.len() >= 3 && bytes[1].is_ascii_alphabetic() && bytes[2] == b':' {
        return Some(PathBuf::from(&path[1..]));
    }
    Some(PathBuf::from(path))
}

/// Decodes `%XX` escapes, failing if they are malformed or do not decode to UTF-8.
///
/// For example, `my%20dir` decodes to `my dir`.
fn percent_decode(text: &str) -> Option<String> {
    let mut bytes = vec![];
    let mut rest = text.as_bytes();
    while let [first, tail @ ..] = rest {
        if *first == b'%' {
            let hex = std::str::from_utf8(tail.get(..2)?).ok()?;
            bytes.push(u8::from_str_radix(hex, 16).ok()?);
            rest = &tail[2..];
        } else {
            bytes.push(*first);
            rest = tail;
        }
    }
    String::from_utf8(bytes).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn file_uris_encode_their_paths() {
        let uri = file_uri(Path::new("/tmp/my dir/builtins.st")).unwrap();
        assert_eq!(uri.as_str(), "file:///tmp/my%20dir/builtins.st");
    }

    #[test]
    fn file_paths_decode_their_uris() {
        let path = |uri: &str| file_path(&uri.parse().unwrap());
        assert_eq!(
            path("file:///tmp/my%20dir/main.st"),
            Some(PathBuf::from("/tmp/my dir/main.st"))
        );
        assert_eq!(
            path("file://localhost/tmp/main.st"),
            Some(PathBuf::from("/tmp/main.st"))
        );
        assert_eq!(
            path("file:///C:/Temp/main.st"),
            Some(PathBuf::from("C:/Temp/main.st"))
        );
        assert_eq!(path("untitled:Untitled-1"), None);
        // not UTF-8
        assert_eq!(path("file:///%FF.st"), None);
    }

    #[test]
    fn paths_survive_a_round_trip() {
        let original = Path::new("/home/me/my project/é.st");
        assert_eq!(
            file_path(&file_uri(original).unwrap()).as_deref(),
            Some(original)
        );
    }
}
