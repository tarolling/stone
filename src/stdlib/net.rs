//! The builtin `net` module: TCP connections, and finding the addresses of a host.
//!
//! Unlike the other builtin modules, `net` is written in stone, in `net.stn`, which a program
//! that says `use net` is linked with like a module file of its own. That file parses addresses,
//! reads `/etc/hosts` and `/etc/resolv.conf`, and asks DNS servers itself, so both backends find
//! the same addresses the same way. It reaches the system through the internal module `net_sys`,
//! which only stone's own modules may use, and whose functions are here for the interpreter.
//! Compiled code makes the same system calls from `stone.net_*` routines.
//!
//! An address is a list of 4 (IPv4) or 16 (IPv6) bytes, as `net.stn` parses it, and `name` is
//! the text an error about it quotes, such as `localhost:8080`.

use super::BuiltinDoc;
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};

// the sockets themselves, over `std::net` and the C library's `poll` on Unix, and none yet on
// Windows, where a program that uses the module is an error before it runs
#[cfg(unix)]
mod system;
#[cfg(not(unix))]
#[path = "net/unsupported.rs"]
mod system;

pub use system::{Listener, Socket, connect, connect_unix, exchange, listen};

/// The name a program imports the module by, as in `use net`.
pub const MODULE: &str = "net";

/// The module's stone source, which `use net` links with the program.
pub const SOURCE: &str = include_str!("net.stn");

/// The name of the internal module through which `net.stn` reaches the system.
pub const SYS_MODULE: &str = "net_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The system calls behind the builtin `net` module.";

pub static SYS_DOCS: [BuiltinDoc; 5] = [
    BuiltinDoc {
        name: "net_sys.connect",
        signature: "net_sys.connect(address: list[int], port: int, name: str) -> \
                    result[socket, str]",
        description: "Connects to `port` at `address`.",
    },
    BuiltinDoc {
        name: "net_sys.listen",
        signature: "net_sys.listen(address: list[int], port: int, name: str) -> \
                    result[listener, str]",
        description: "Listens for connections to `port` at `address`.",
    },
    BuiltinDoc {
        name: "net_sys.exchange",
        signature: "net_sys.exchange(address: list[int], port: int, name: str, query: \
                    list[int], timeout_ms: int) -> result[list[int], str]",
        description: "Sends `query` in one UDP packet to `port` at `address`, with a random \
                      ID in its first two bytes, and returns the first packet back with the \
                      same ID, waiting at most `timeout_ms` milliseconds.",
    },
    BuiltinDoc {
        name: "net_sys.connect_unix",
        signature: "net_sys.connect_unix(path: str, name: str) -> result[socket, str]",
        description: "Connects to the Unix socket at `path`.",
    },
    BuiltinDoc {
        name: "net_sys.ready",
        signature: "net_sys.ready(s: socket, timeout_ms: int) -> bool",
        description: "Returns whether a read of `s` would not wait, checking for at most \
                      `timeout_ms` milliseconds, or forever if it is -1.",
    },
];

/// The reason an error gives for an address `net_sys` cannot use: not 4 or 16 bytes, a byte
/// out of range, or a port out of range.
pub const INVALID_ADDRESS: &str = "invalid address";

/// The reason an error gives for an exchange no reply came back to in time.
pub const TIMED_OUT: &str = "timed out";

/// The error `read_exact` gives when the connection closes before the bytes it waits for arrive.
pub const CLOSED_EARLY: &str = "connection closed early";

/// The error `take_fd` gives when no descriptor has arrived since the last.
pub const FD_MISSING: &str = "no descriptor";

/// The error writing a socket's shared memory gives before `shm_create` made it.
pub const SHM_MISSING: &str = "no shared memory";

/// The error a socket's methods give once it is closed.
pub const SOCKET_CLOSED: &str = "socket is closed";

/// The error a listener's methods give once it is closed.
pub const LISTENER_CLOSED: &str = "listener is closed";

/// The most bytes one read from a socket asks the system for.
pub const READ_SIZE: usize = 64 * 1024;

/// The most bytes a reply to an exchange may hold.
pub const MAX_REPLY: usize = 64 * 1024;

/// Returns the socket address of `port` at `address`, or `None` if either is out of range.
///
/// For example, `[127, 0, 0, 1]` and `80` are `127.0.0.1:80`, and `[1, 2, 3]` is no address.
pub fn socket_address(address: &[i64], port: i64) -> Option<SocketAddr> {
    let port = u16::try_from(port).ok()?;
    let bytes: Vec<u8> = address
        .iter()
        .map(|&b| u8::try_from(b).ok())
        .collect::<Option<_>>()?;
    let ip = match bytes.len() {
        4 => IpAddr::V4(Ipv4Addr::new(bytes[0], bytes[1], bytes[2], bytes[3])),
        16 => IpAddr::V6(Ipv6Addr::from(<[u8; 16]>::try_from(bytes).ok()?)),
        _ => return None,
    };
    Some(SocketAddr::new(ip, port))
}

/// Returns `seconds` in whole milliseconds, rounded up and at most 2^31 - 1, or -1 for zero, a
/// negative number, or nan, which wait forever, as `set_timeout` and `net.stn`'s
/// `milliseconds` count them.
///
/// For example, `timeout_ms(0.0015)` is 2 and `timeout_ms(-1.0)` is -1.
pub fn timeout_ms(seconds: f64) -> i64 {
    // written as `not (seconds > 0.0)`, so nan waits forever too
    if seconds.partial_cmp(&0.0) != Some(std::cmp::Ordering::Greater) {
        return -1;
    }
    let ms = seconds * 1000.0;
    if ms >= 2147483647.0 {
        return 2147483647;
    }
    let whole = ms as i64;
    match (whole as f64) < ms {
        true => whole + 1,
        false => whole,
    }
}

/// Returns how many bytes at the start of `bytes` end on a character boundary: all of them,
/// unless they end in the first bytes of a UTF-8 character whose last have not arrived.
///
/// For example, `b"ab\xc3"` has 2, since `\xc3` starts a character of two bytes, and `b"ab\xc3\xa9"`
/// has 4.
pub fn complete_prefix(bytes: &[u8]) -> usize {
    // a character is at most 4 bytes, so only the last 3 can be an unfinished one
    for back in 1..=bytes.len().min(3) {
        let b = bytes[bytes.len() - back];
        if b & 0xc0 == 0x80 {
            continue;
        }
        let width = match b {
            0xc0..=0xdf => 2,
            0xe0..=0xef => 3,
            0xf0..=0xf7 => 4,
            _ => 1,
        };
        return match width > back {
            true => bytes.len() - back,
            false => bytes.len(),
        };
    }
    bytes.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn addresses_are_four_or_sixteen_bytes_and_a_port() {
        assert_eq!(
            socket_address(&[127, 0, 0, 1], 80),
            Some("127.0.0.1:80".parse().unwrap())
        );
        let mut loopback = vec![0; 15];
        loopback.push(1);
        assert_eq!(
            socket_address(&loopback, 443),
            Some("[::1]:443".parse().unwrap())
        );
        assert_eq!(socket_address(&[1, 2, 3], 80), None);
        assert_eq!(socket_address(&[256, 0, 0, 1], 80), None);
        assert_eq!(socket_address(&[127, 0, 0, 1], 65536), None);
        assert_eq!(socket_address(&[127, 0, 0, 1], -1), None);
    }

    #[test]
    fn timeouts_round_up_to_milliseconds() {
        assert_eq!(timeout_ms(0.0015), 2);
        assert_eq!(timeout_ms(2.5), 2500);
        assert_eq!(timeout_ms(1e-9), 1);
        assert_eq!(timeout_ms(0.0), -1);
        assert_eq!(timeout_ms(-3.0), -1);
        assert_eq!(timeout_ms(f64::NAN), -1);
        assert_eq!(timeout_ms(1e300), 2147483647);
        assert_eq!(timeout_ms(f64::INFINITY), 2147483647);
    }

    #[test]
    fn a_read_holds_back_an_unfinished_character() {
        assert_eq!(complete_prefix(b"ab"), 2);
        assert_eq!(complete_prefix(b"ab\xc3"), 2);
        assert_eq!(complete_prefix(b"ab\xc3\xa9"), 4);
        assert_eq!(complete_prefix(b"\xe2\x82"), 0);
        assert_eq!(complete_prefix(b"\xe2\x82\xac"), 3);
        assert_eq!(complete_prefix(b"x\xf0\x9f\x98"), 1);
        assert_eq!(complete_prefix(b""), 0);
        // a stray continuation byte is left for the text check to reject
        assert_eq!(complete_prefix(b"\x80\x80\x80\x80"), 4);
    }
}
