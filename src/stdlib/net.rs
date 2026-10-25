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
use super::fs::{INVALID_TEXT, io_message, is_text, message};
use std::ffi::{c_int, c_short};
use std::io::{ErrorKind, Read, Write};
use std::net::{
    IpAddr, Ipv4Addr, Ipv6Addr, Shutdown, SocketAddr, TcpListener, TcpStream, UdpSocket,
};
use std::os::fd::AsRawFd;
use std::time::{Duration, Instant};

/// The name a program imports the module by, as in `use net`.
pub const MODULE: &str = "net";

/// The module's stone source, which `use net` links with the program.
pub const SOURCE: &str = include_str!("net.stn");

/// The name of the internal module through which `net.stn` reaches the system.
pub const SYS_MODULE: &str = "net_sys";

/// What the internal module is for.
pub const SYS_MODULE_DOC: &str = "The system calls behind the builtin `net` module.";

pub static SYS_DOCS: [BuiltinDoc; 3] = [
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
];

/// The reason an error gives for an address `net_sys` cannot use: not 4 or 16 bytes, a byte
/// out of range, or a port out of range.
pub const INVALID_ADDRESS: &str = "invalid address";

/// The reason an error gives for an exchange no reply came back to in time.
pub const TIMED_OUT: &str = "timed out";

/// The error `read_exact` gives when the connection closes before the bytes it waits for arrive.
pub const CLOSED_EARLY: &str = "connection closed early";

/// The error a socket's methods give once it is closed.
pub const SOCKET_CLOSED: &str = "socket is closed";

/// The error a listener's methods give once it is closed.
pub const LISTENER_CLOSED: &str = "listener is closed";

/// The reason fuzzing gives for every `net_sys` call, so fuzzed programs never reach the network.
const NET_DISABLED: &str = "networking is disabled while fuzzing";

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

/// Returns the address `net_sys` was given, failing with [`INVALID_ADDRESS`] about `name` if it
/// is out of range, and failing every call while fuzzing.
fn address_of(address: &[i64], port: i64, name: &str) -> Result<SocketAddr, String> {
    if cfg!(fuzzing) {
        return Err(message(NET_DISABLED, name));
    }
    socket_address(address, port).ok_or_else(|| message(INVALID_ADDRESS, name))
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

/// The `poll` events of a descriptor ready to read and ready to write.
const POLLIN: c_short = 1;
const POLLOUT: c_short = 4;

/// The `struct pollfd` libc's `poll` takes.
#[repr(C)]
struct PollFd {
    fd: c_int,
    events: c_short,
    revents: c_short,
}

/// `poll`'s count of descriptors, which is `nfds_t`: an `unsigned long` on Linux, and an
/// `unsigned int` on macOS.
#[cfg(target_os = "macos")]
type NfdsT = std::ffi::c_uint;
#[cfg(not(target_os = "macos"))]
type NfdsT = std::ffi::c_ulong;

unsafe extern "C" {
    fn poll(fds: *mut PollFd, nfds: NfdsT, timeout: c_int) -> c_int;
}

/// Waits until the descriptor `fd` is ready for `events`, for at most `timeout` milliseconds in
/// all, or forever if it is -1, failing with [`TIMED_OUT`] when the time runs out, as compiled
/// code's `.Lnet_wait` does: a signal that cuts the wait short starts it again with the time
/// left.
fn wait(fd: c_int, events: c_short, timeout: i64) -> Result<(), String> {
    if timeout < 0 {
        return Ok(());
    }
    let deadline = Instant::now() + Duration::from_millis(timeout as u64);
    loop {
        let left = deadline.saturating_duration_since(Instant::now());
        if left.is_zero() {
            return Err(TIMED_OUT.to_string());
        }
        let ms = left.as_nanos().div_ceil(1_000_000) as c_int;
        let mut pollfd = PollFd {
            fd,
            events,
            revents: 0,
        };
        // SAFETY: pollfd is one valid, writable `struct pollfd` for the whole call
        let ready = unsafe { poll(&mut pollfd, 1, ms) };
        if ready > 0 {
            return Ok(());
        }
        if ready < 0 {
            let error = std::io::Error::last_os_error();
            if error.kind() != ErrorKind::Interrupted {
                return Err(reason(&error));
            }
        }
    }
}

/// Returns the reason alone for an I/O error on a connection, such as `broken pipe`.
fn reason(error: &std::io::Error) -> String {
    let full = io_message(error, "");
    full.strip_suffix(": ''").unwrap_or(&full).to_string()
}

/// A TCP connection, with what has arrived on it but not been read yet.
#[derive(Debug)]
pub struct Socket {
    /// The connection, until `close` drops it.
    stream: Option<TcpStream>,
    /// Bytes received but not read yet.
    buffer: Vec<u8>,
    /// Whether the other end has closed the connection, so nothing more will arrive.
    ended: bool,
    /// How many milliseconds each wait may take, or -1 to wait forever.
    timeout: i64,
}

impl Socket {
    fn new(stream: TcpStream) -> Socket {
        Socket {
            stream: Some(stream),
            buffer: vec![],
            ended: false,
            timeout: -1,
        }
    }

    /// Makes each later wait give up after `seconds`, counted by [`timeout_ms`].
    pub fn set_timeout(&mut self, seconds: f64) {
        self.timeout = timeout_ms(seconds);
    }

    fn stream(&mut self) -> Result<&mut TcpStream, String> {
        self.stream
            .as_mut()
            .ok_or_else(|| SOCKET_CLOSED.to_string())
    }

    /// Sends all of `text`, returning the number of bytes sent.
    pub fn send(&mut self, text: &str) -> Result<i64, String> {
        self.send_bytes(text.as_bytes())
    }

    /// Sends all of `data`, returning the number of bytes sent. With a timeout, the socket sends
    /// without blocking, waiting for room before each part.
    pub fn send_bytes(&mut self, data: &[u8]) -> Result<i64, String> {
        let timeout = self.timeout;
        let stream = self.stream()?;
        if timeout < 0 {
            stream.write_all(data).map_err(|e| reason(&e))?;
            return Ok(data.len() as i64);
        }
        stream.set_nonblocking(true).map_err(|e| reason(&e))?;
        let mut sent = 0;
        let outcome = loop {
            if sent == data.len() {
                break Ok(sent as i64);
            }
            if let Err(e) = wait(stream.as_raw_fd(), POLLOUT, timeout) {
                break Err(e);
            }
            match stream.write(&data[sent..]) {
                Ok(n) => sent += n,
                Err(e) if matches!(e.kind(), ErrorKind::WouldBlock | ErrorKind::Interrupted) => {}
                Err(e) => break Err(reason(&e)),
            }
        };
        stream.set_nonblocking(false).map_err(|e| reason(&e))?;
        outcome
    }

    /// Waits for more data, adding it to the buffer, and returns whether any came: none comes
    /// once the other end has closed the connection.
    fn receive(&mut self) -> Result<bool, String> {
        if self.ended {
            return Ok(false);
        }
        let mut chunk = vec![0; READ_SIZE];
        let timeout = self.timeout;
        let stream = self.stream()?;
        wait(stream.as_raw_fd(), POLLIN, timeout)?;
        let n = loop {
            match stream.read(&mut chunk) {
                Ok(n) => break n,
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(reason(&e)),
            }
        };
        self.buffer.extend_from_slice(&chunk[..n]);
        self.ended = n == 0;
        Ok(n > 0)
    }

    /// Takes the first `n` bytes of the buffer as text.
    fn take(&mut self, n: usize) -> Result<String, String> {
        let bytes: Vec<u8> = self.buffer.drain(..n).collect();
        match is_text(&bytes) {
            true => Ok(String::from_utf8(bytes).unwrap_or_default()),
            false => Err(INVALID_TEXT.to_string()),
        }
    }

    /// Returns the next line, without its `\n` or `\r\n`, or `None` once the other end has
    /// closed the connection and every line has been read.
    pub fn read_line(&mut self) -> Result<Option<String>, String> {
        self.stream()?;
        let mut searched = 0;
        loop {
            if let Some(i) = self.buffer[searched..].iter().position(|&b| b == b'\n') {
                let end = searched + i;
                let mut line = self.take(end + 1)?;
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
                return Ok(Some(line));
            }
            searched = self.buffer.len();
            if !self.receive()? {
                if self.buffer.is_empty() {
                    return Ok(None);
                }
                let n = self.buffer.len();
                return self.take(n).map(Some);
            }
        }
    }

    /// Returns whatever has arrived, waiting for something if nothing has, or `""` once the other
    /// end has closed the connection. A character whose bytes have not all arrived waits for the
    /// next read.
    pub fn read(&mut self) -> Result<String, String> {
        self.stream()?;
        loop {
            let complete = match self.ended {
                true => self.buffer.len(),
                false => complete_prefix(&self.buffer),
            };
            if complete > 0 || self.ended {
                return self.take(complete);
            }
            self.receive()?;
        }
    }

    /// Reads until the other end closes the connection, returning everything that arrived.
    pub fn read_all(&mut self) -> Result<String, String> {
        self.stream()?;
        while self.receive()? {}
        let n = self.buffer.len();
        self.take(n)
    }

    /// Returns what has arrived, up to `max` bytes, waiting for something if nothing has, or
    /// nothing once the other end has closed the connection or for a `max` of 0 or less.
    pub fn read_bytes(&mut self, max: i64) -> Result<Vec<u8>, String> {
        self.stream()?;
        if max <= 0 {
            return Ok(vec![]);
        }
        if self.buffer.is_empty() {
            self.receive()?;
        }
        let n = self.buffer.len().min(max as usize);
        Ok(self.buffer.drain(..n).collect())
    }

    /// Returns exactly the next `n` bytes, waiting for them, or fails with [`CLOSED_EARLY`] if
    /// the other end closes the connection first, leaving what did arrive to read.
    pub fn read_exact(&mut self, n: i64) -> Result<Vec<u8>, String> {
        self.stream()?;
        let n = n.max(0) as usize;
        while self.buffer.len() < n {
            if !self.receive()? {
                return Err(CLOSED_EARLY.to_string());
            }
        }
        Ok(self.buffer.drain(..n).collect())
    }

    /// Reads until the other end closes the connection, returning every byte that arrived.
    pub fn read_all_bytes(&mut self) -> Result<Vec<u8>, String> {
        self.stream()?;
        while self.receive()? {}
        Ok(std::mem::take(&mut self.buffer))
    }

    /// Closes the connection, so its methods fail from now on.
    pub fn close(&mut self) {
        if let Some(stream) = self.stream.take() {
            let _ = stream.shutdown(Shutdown::Both);
        }
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

/// A TCP port waiting for connections.
#[derive(Debug)]
pub struct Listener {
    /// The listening socket, until `close` drops it.
    listener: Option<TcpListener>,
    /// The port it listens on, which stays known after `close`.
    port: i64,
    /// How many milliseconds `accept` may wait, or -1 to wait forever.
    timeout: i64,
}

impl Listener {
    /// Waits for the next connection.
    pub fn accept(&mut self) -> Result<Socket, String> {
        let listener = self
            .listener
            .as_ref()
            .ok_or_else(|| LISTENER_CLOSED.to_string())?;
        wait(listener.as_raw_fd(), POLLIN, self.timeout)?;
        loop {
            match listener.accept() {
                Ok((stream, _)) => return Ok(Socket::new(stream)),
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(reason(&e)),
            }
        }
    }

    /// Makes each later `accept` give up after `seconds`, counted by [`timeout_ms`].
    pub fn set_timeout(&mut self, seconds: f64) {
        self.timeout = timeout_ms(seconds);
    }

    /// Returns the port it listens on.
    pub fn port(&self) -> i64 {
        self.port
    }

    /// Stops listening, so `accept` fails from now on.
    pub fn close(&mut self) {
        self.listener = None;
    }
}

/// Connects to `port` at `address`, waiting at most `timeout` milliseconds, or as long as the
/// system waits if it is -1.
pub fn connect(address: &[i64], port: i64, name: &str, timeout: i64) -> Result<Socket, String> {
    let to = address_of(address, port, name)?;
    let connected = match timeout {
        ..0 => TcpStream::connect(to),
        ms => TcpStream::connect_timeout(&to, Duration::from_millis(ms as u64)),
    };
    let stream = connected.map_err(|e| match e.kind() {
        ErrorKind::TimedOut if e.raw_os_error().is_none() => message(TIMED_OUT, name),
        _ => io_message(&e, name),
    })?;
    Ok(Socket::new(stream))
}

/// Listens for connections to `port` at `address`, which may be 0 for the system to pick one.
pub fn listen(address: &[i64], port: i64, name: &str) -> Result<Listener, String> {
    let at = address_of(address, port, name)?;
    let listener = TcpListener::bind(at).map_err(|e| io_message(&e, name))?;
    let port = listener
        .local_addr()
        .map_err(|e| io_message(&e, name))?
        .port();
    Ok(Listener {
        listener: Some(listener),
        port: port as i64,
        timeout: -1,
    })
}

/// Sends `query` in one UDP packet to `port` at `address`, with a random ID in its first two
/// bytes, and returns the first packet that comes back with the same ID, waiting at most
/// `timeout_ms` milliseconds in all.
pub fn exchange(
    address: &[i64],
    port: i64,
    name: &str,
    query: &[i64],
    timeout_ms: i64,
) -> Result<Vec<i64>, String> {
    let to = address_of(address, port, name)?;
    let mut packet: Vec<u8> = query.iter().map(|&b| b as u8).collect();
    if packet.len() < 2 || query.iter().any(|&b| !(0..=255).contains(&b)) {
        return Err(message(INVALID_ADDRESS, name));
    }
    let id = random_id().map_err(|e| io_message(&e, name))?;
    packet[..2].copy_from_slice(&id);
    let from = match to {
        SocketAddr::V4(_) => SocketAddr::new(IpAddr::V4(Ipv4Addr::UNSPECIFIED), 0),
        SocketAddr::V6(_) => SocketAddr::new(IpAddr::V6(Ipv6Addr::UNSPECIFIED), 0),
    };
    let socket = UdpSocket::bind(from).map_err(|e| io_message(&e, name))?;
    socket.connect(to).map_err(|e| io_message(&e, name))?;
    socket.send(&packet).map_err(|e| io_message(&e, name))?;
    let deadline = Instant::now() + Duration::from_millis(timeout_ms.max(0) as u64);
    let mut reply = vec![0; MAX_REPLY];
    loop {
        let left = deadline.saturating_duration_since(Instant::now());
        if left.is_zero() {
            return Err(message(TIMED_OUT, name));
        }
        socket
            .set_read_timeout(Some(left))
            .map_err(|e| io_message(&e, name))?;
        match socket.recv(&mut reply) {
            Ok(n) if n >= 2 && reply[..2] == id => {
                return Ok(reply[..n].iter().map(|&b| b as i64).collect());
            }
            // another query's reply, or a stray packet
            Ok(_) => {}
            Err(e) if matches!(e.kind(), ErrorKind::WouldBlock | ErrorKind::TimedOut) => {
                return Err(message(TIMED_OUT, name));
            }
            Err(e) if e.kind() == ErrorKind::Interrupted => {}
            Err(e) => return Err(io_message(&e, name)),
        }
    }
}

/// Returns two random bytes for a query's ID, from the system, as compiled code gets them from
/// `getrandom` (`getentropy` on macOS).
fn random_id() -> std::io::Result<[u8; 2]> {
    let mut id = [0; 2];
    std::fs::File::open("/dev/urandom")?.read_exact(&mut id)?;
    Ok(id)
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
    fn a_socket_waits_no_longer_than_its_timeout() {
        let mut listener = listen(&[127, 0, 0, 1], 0, "").unwrap();
        listener.set_timeout(0.01);
        assert_eq!(listener.accept().err(), Some(TIMED_OUT.to_string()));
        let mut client = connect(&[127, 0, 0, 1], listener.port(), "", 1000).unwrap();
        let mut server = listener.accept().unwrap();
        server.set_timeout(0.01);
        assert_eq!(server.read_bytes(4), Err(TIMED_OUT.to_string()));
        client.send_bytes(b"\x00\xffab").unwrap();
        assert_eq!(server.read_exact(2), Ok(vec![0, 255]));
        assert_eq!(server.read_bytes(9), Ok(b"ab".to_vec()));
        client.close();
        assert_eq!(server.read_exact(1), Err(CLOSED_EARLY.to_string()));
        assert_eq!(server.read_all_bytes(), Ok(vec![]));
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

    #[test]
    fn a_socket_reads_lines_and_text_over_loopback() {
        let mut listener = listen(&[127, 0, 0, 1], 0, "127.0.0.1:0").unwrap();
        assert!(listener.port() > 0);
        let mut client = connect(&[127, 0, 0, 1], listener.port(), "here", -1).unwrap();
        let mut server = listener.accept().unwrap();
        assert_eq!(client.send("one\r\ntwo\nthree"), Ok(14));
        client.close();
        assert_eq!(client.send("x"), Err(SOCKET_CLOSED.to_string()));
        assert_eq!(server.read_line(), Ok(Some("one".to_string())));
        assert_eq!(server.read_line(), Ok(Some("two".to_string())));
        assert_eq!(server.read_line(), Ok(Some("three".to_string())));
        assert_eq!(server.read_line(), Ok(None));
        assert_eq!(server.read(), Ok(String::new()));
        assert_eq!(server.read_all(), Ok(String::new()));
        listener.close();
        assert_eq!(listener.accept().err(), Some(LISTENER_CLOSED.to_string()));
    }

    #[test]
    fn a_refused_connection_names_where_it_went() {
        // a port the system gave out and took back, so nothing listens there
        let port = listen(&[127, 0, 0, 1], 0, "").unwrap().port();
        assert_eq!(
            connect(&[127, 0, 0, 1], port, "localhost:1", -1).err(),
            Some("connection refused: 'localhost:1'".to_string())
        );
        assert_eq!(
            connect(&[1, 2, 3], 80, "bad:80", -1).err(),
            Some("invalid address: 'bad:80'".to_string())
        );
    }
}
