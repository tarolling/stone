//! The tests of `cli.rs` for downloading packages, which need the `fetch` feature: `stone add`,
//! `remove`, `fetch`, and `tree`, and the downloads `run` makes, against a fake GitHub on
//! loopback that `STONE_GITHUB_API` points stone at.

use super::*;
use std::collections::HashMap;
use std::io::{Read, Write};
use std::net::TcpListener;
use std::sync::{Arc, Mutex};

/// Returns the CRC-32 of `data`, as gzip needs it.
fn crc32(data: &[u8]) -> u32 {
    let mut crc = !0u32;
    for &byte in data {
        crc ^= byte as u32;
        for _ in 0..8 {
            crc = if crc & 1 == 1 {
                (crc >> 1) ^ 0xedb8_8320
            } else {
                crc >> 1
            };
        }
    }
    !crc
}

/// Wraps `data` in gzip without compressing it, in stored deflate blocks.
fn gzip(data: &[u8]) -> Vec<u8> {
    let mut out = vec![0x1f, 0x8b, 8, 0, 0, 0, 0, 0, 0, 0xff];
    let chunks: Vec<&[u8]> = match data.is_empty() {
        true => vec![&[]],
        false => data.chunks(0xffff).collect(),
    };
    for (i, chunk) in chunks.iter().enumerate() {
        out.push((i + 1 == chunks.len()) as u8);
        let len = chunk.len() as u16;
        out.extend_from_slice(&len.to_le_bytes());
        out.extend_from_slice(&(!len).to_le_bytes());
        out.extend_from_slice(chunk);
    }
    out.extend_from_slice(&crc32(data).to_le_bytes());
    out.extend_from_slice(&(data.len() as u32).to_le_bytes());
    out
}

/// Makes a `.tar.gz` of `files`, each under the directory `top`, as GitHub's archives are.
fn tarball(top: &str, files: &[(&str, &str)]) -> Vec<u8> {
    let mut tar = vec![];
    for (path, text) in files {
        let mut header = [0u8; 512];
        let name = format!("{top}/{path}");
        header[..name.len()].copy_from_slice(name.as_bytes());
        header[100..108].copy_from_slice(b"0000644\0");
        header[108..116].copy_from_slice(b"0000000\0");
        header[116..124].copy_from_slice(b"0000000\0");
        header[124..136].copy_from_slice(format!("{:011o}\0", text.len()).as_bytes());
        header[136..148].copy_from_slice(b"00000000000\0");
        header[148..156].copy_from_slice(b"        ");
        header[156] = b'0';
        header[257..263].copy_from_slice(b"ustar\0");
        header[263..265].copy_from_slice(b"00");
        let sum: u32 = header.iter().map(|&b| b as u32).sum();
        header[148..156].copy_from_slice(format!("{sum:06o}\0 ").as_bytes());
        tar.extend_from_slice(&header);
        tar.extend_from_slice(text.as_bytes());
        tar.resize(tar.len().div_ceil(512) * 512, 0);
    }
    tar.resize(tar.len() + 1024, 0);
    gzip(&tar)
}

/// A fake GitHub API: each path (with its query) and the body it answers with, which tests may
/// change, and every path asked for.
#[derive(Clone, Default)]
struct FakeGitHub {
    routes: Arc<Mutex<HashMap<String, Vec<u8>>>>,
    asked: Arc<Mutex<Vec<String>>>,
}

impl FakeGitHub {
    /// Starts serving on a free loopback port, returning the API's URL.
    fn serve(&self) -> String {
        let listener = TcpListener::bind("127.0.0.1:0").unwrap();
        let url = format!("http://{}", listener.local_addr().unwrap());
        let fake = self.clone();
        std::thread::spawn(move || {
            for stream in listener.incoming() {
                let Ok(mut stream) = stream else { continue };
                let mut request = vec![];
                let mut byte = [0];
                while !request.ends_with(b"\r\n\r\n") && stream.read(&mut byte).unwrap_or(0) == 1 {
                    request.push(byte[0]);
                }
                let request = String::from_utf8_lossy(&request);
                let path = request.split(' ').nth(1).unwrap_or("").to_string();
                fake.asked.lock().unwrap().push(path.clone());
                let body = fake.routes.lock().unwrap().get(&path).cloned();
                let (status, body) = match body {
                    Some(body) => ("200 OK", body),
                    None => ("404 Not Found", b"{\"message\": \"Not Found\"}".to_vec()),
                };
                let head = format!(
                    "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n",
                    body.len()
                );
                let _ = stream.write_all(head.as_bytes());
                let _ = stream.write_all(&body);
            }
        });
        url
    }

    /// Publishes releases of `repo` with these tags, each holding `files`.
    fn release(&self, repo: &str, releases: &[(&str, Vec<(&str, String)>)]) {
        let mut routes = self.routes.lock().unwrap();
        let listing: Vec<String> = releases
            .iter()
            .map(|(tag, _)| format!("{{\"tag_name\": \"{tag}\", \"draft\": false}}"))
            .collect();
        routes.insert(
            format!("/repos/{repo}/releases?per_page=100&page=1"),
            format!("[{}]", listing.join(", ")).into_bytes(),
        );
        for (tag, files) in releases {
            let files: Vec<(&str, &str)> = files.iter().map(|(p, t)| (*p, t.as_str())).collect();
            let top = format!("{}-{tag}", repo.replace('/', "-"));
            routes.insert(
                format!("/repos/{repo}/tarball/{tag}"),
                tarball(&top, &files),
            );
        }
    }

    /// Returns how many requests asked for `path`.
    fn count(&self, path: &str) -> usize {
        self.asked
            .lock()
            .unwrap()
            .iter()
            .filter(|p| *p == path)
            .count()
    }
}

/// The files of a release of `mime`, whose `v()` gives `version` and the `num` it uses.
fn mime_release(version: &str) -> Vec<(&'static str, String)> {
    vec![
        (
            "stone.toml",
            format!(
                "[package]\nname = \"mime\"\nversion = \"{version}\"\n\n[dependencies]\n\
                 num = {{ github = \"bob/num\", version = \"0.3\" }}\n"
            ),
        ),
        (
            "lib.stn",
            format!("use num\n\npub fn v() -> str:\n    ret \"mime {version}, num \" + num.v()\n"),
        ),
        ("README.md", "# mime\n".to_string()),
    ]
}

fn num_release(version: &str) -> Vec<(&'static str, String)> {
    vec![
        (
            "stone.toml",
            format!("[package]\nname = \"num\"\nversion = \"{version}\"\n"),
        ),
        (
            "lib.stn",
            format!("pub fn v() -> str:\n    ret \"{version}\"\n"),
        ),
    ]
}

/// Runs stone with `args` in `dir/app`, with the store in `dir/home` and the fake API at `api`.
fn stone_in(dir: &Path, api: &str, args: &[&str]) -> (bool, String, String) {
    let output = Command::new(env!("CARGO_BIN_EXE_stone"))
        .args(args)
        .current_dir(dir.join("app"))
        .env("STONE_HOME", dir.join("home"))
        .env("STONE_GITHUB_API", api)
        .env_remove("GITHUB_TOKEN")
        .output()
        .expect("stone should run");
    (
        output.status.success(),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

#[test]
fn packages_are_added_downloaded_locked_and_removed() {
    let github = FakeGitHub::default();
    github.release(
        "alice/mime",
        &[
            ("v1.0.0", mime_release("1.0.0")),
            ("v1.2.0", mime_release("1.2.0")),
            ("v2.0.0-rc.1", mime_release("2.0.0-rc.1")),
        ],
    );
    github.release(
        "bob/num",
        &[
            ("v0.3.0", num_release("0.3.0")),
            ("v0.3.5", num_release("0.3.5")),
        ],
    );
    let api = github.serve();
    let dir = project(
        "package_fetching",
        &[("app/main.stn", "use mime\nprint(mime.v())\n")],
    );

    // the newest release that is not a pre-release, and the oldest num it names
    let (ok, out, err) = stone_in(&dir, &api, &["add", "github:alice/mime"]);
    assert!(ok, "{err}");
    assert_eq!(out, "added mime 1.2.0 (github:alice/mime)\n");
    assert_eq!(
        err,
        "downloading github:alice/mime 1.2.0\ndownloading github:bob/num 0.3.0\n"
    );
    let manifest = std::fs::read_to_string(dir.join("app/stone.toml")).unwrap();
    assert_eq!(
        manifest,
        "[dependencies]\nmime = { github = \"alice/mime\", version = \"1.2.0\" }\n"
    );
    let lock = std::fs::read_to_string(dir.join("app/stone.lock")).unwrap();
    assert!(
        lock.contains("name = \"mime\"") && lock.contains("name = \"num\""),
        "{lock}"
    );
    let stored = dir.join("home/packages/github.com/alice/mime/1.2.0");
    assert!(stored.join("lib.stn").is_file());
    assert!(!stored.join("README.md").exists());

    // nothing more is downloaded or asked for
    let asked = github.asked.lock().unwrap().len();
    let (ok, out, err) = stone_in(&dir, &api, &["run", "main.stn"]);
    assert!(ok, "{err}");
    assert_eq!(
        (out.as_str(), err.as_str()),
        ("mime 1.2.0, num 0.3.0\n", "")
    );
    assert_eq!(github.asked.lock().unwrap().len(), asked);

    let (ok, out, _) = stone_in(&dir, &api, &["tree"]);
    assert!(ok);
    assert_eq!(
        out,
        "app\n└── mime 1.2.0 (github:alice/mime)\n    └── num 0.3.0 (github:bob/num)\n"
    );
    let (ok, _, err) = stone_in(&dir, &api, &["fetch", "--locked"]);
    assert!(ok, "{err}");

    // without the store, --offline says what is missing, and a run downloads it again
    std::fs::remove_dir_all(dir.join("home")).unwrap();
    let (ok, _, err) = stone_in(&dir, &api, &["run", "--offline", "main.stn"]);
    assert!(!ok);
    assert!(
        err.contains("error: mime 1.2.0 (github:alice/mime) is not downloaded"),
        "{err}"
    );
    let (ok, out, err) = stone_in(&dir, &api, &["run", "main.stn"]);
    assert!(ok, "{err}");
    assert_eq!(out, "mime 1.2.0, num 0.3.0\n");
    // each package's releases were read once per run that downloaded
    assert_eq!(
        github.count("/repos/alice/mime/releases?per_page=100&page=1"),
        2
    );

    // a release whose tag moved no longer matches the lock
    github.release("bob/num", &[("v0.3.0", num_release("0.3.9"))]);
    std::fs::remove_dir_all(dir.join("home")).unwrap();
    let (ok, _, err) = stone_in(&dir, &api, &["run", "main.stn"]);
    assert!(!ok);
    assert!(
        err.contains("stone.toml:3:11: error: this package says it is version 0.3.9, but it was released as 0.3.0"),
        "{err}"
    );
    github.release(
        "bob/num",
        &[("v0.3.0", {
            let mut files = num_release("0.3.0");
            files[1].1.push_str("\npub fn extra() -> int:\n    ret 1\n");
            files
        })],
    );
    std::fs::remove_dir_all(dir.join("home")).unwrap();
    let (ok, _, err) = stone_in(&dir, &api, &["run", "main.stn"]);
    assert!(!ok);
    assert!(
        err.contains(
            "error: num 0.3.0 (github:bob/num) does not hold what it held when it was locked"
        ),
        "{err}"
    );
    github.release(
        "bob/num",
        &[
            ("v0.3.0", num_release("0.3.0")),
            ("v0.3.5", num_release("0.3.5")),
        ],
    );
    std::fs::remove_dir_all(dir.join("home")).unwrap();

    // an older release by its version, then a missing one
    let (ok, out, err) = stone_in(&dir, &api, &["add", "github:alice/mime@1.0"]);
    assert!(ok, "{err}");
    assert_eq!(out, "added mime 1.0.0 (github:alice/mime)\n");
    let (ok, _, err) = stone_in(&dir, &api, &["add", "github:alice/mime@3"]);
    assert!(!ok);
    assert_eq!(
        err,
        "error: github:alice/mime has no release matching 3.0.0 (its releases are 2.0.0-rc.1, \
         1.2.0, 1.0.0)\n"
    );

    let (ok, out, err) = stone_in(&dir, &api, &["remove", "mime"]);
    assert!(ok, "{err}");
    assert_eq!(out, "removed mime\n");
    assert_eq!(
        std::fs::read_to_string(dir.join("app/stone.toml")).unwrap(),
        "[dependencies]\n"
    );
    let lock = std::fs::read_to_string(dir.join("app/stone.lock")).unwrap();
    assert!(!lock.contains("[[package]]"), "{lock}");
}

#[test]
fn a_missing_repository_says_how_to_reach_a_private_one() {
    let api = FakeGitHub::default().serve();
    let dir = project("package_missing_repo", &[("app/main.stn", "print(1)\n")]);
    let (ok, _, err) = stone_in(&dir, &api, &["add", "github:alice/secret"]);
    assert!(!ok);
    assert_eq!(
        err,
        "error: github:alice/secret was not found (if it is private, set GITHUB_TOKEN to a token \
         that can read it)\n"
    );
    assert!(!dir.join("app/stone.toml").exists());
}
