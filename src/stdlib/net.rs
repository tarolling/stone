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
use std::os::fd::{AsRawFd, RawFd};
use std::os::unix::net::UnixStream;
use std::time::{Duration, Instant};

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

/// One buffer of a [`MsgHdr`], C's `struct iovec`.
#[cfg(target_os = "linux")]
#[repr(C)]
struct IoVec {
    base: *mut std::ffi::c_void,
    len: usize,
}

/// Linux's `struct msghdr`, which `sendmsg` sends a descriptor with.
#[cfg(target_os = "linux")]
#[repr(C)]
struct MsgHdr {
    name: *mut std::ffi::c_void,
    namelen: u32,
    iov: *const IoVec,
    iovlen: usize,
    control: *mut std::ffi::c_void,
    controllen: usize,
    flags: c_int,
}

#[cfg(target_os = "linux")]
unsafe extern "C" {
    fn memfd_create(name: *const std::ffi::c_char, flags: std::ffi::c_uint) -> c_int;
    fn sendmsg(fd: c_int, msg: *const MsgHdr, flags: c_int) -> isize;
    fn recvmsg(fd: c_int, msg: *mut MsgHdr, flags: c_int) -> isize;
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

/// The connection under a socket: TCP, or a Unix socket on this machine.
#[derive(Debug)]
enum Stream {
    Tcp(TcpStream),
    Unix(UnixStream),
}

impl Stream {
    fn set_nonblocking(&self, nonblocking: bool) -> std::io::Result<()> {
        match self {
            Stream::Tcp(stream) => stream.set_nonblocking(nonblocking),
            Stream::Unix(stream) => stream.set_nonblocking(nonblocking),
        }
    }

    fn shutdown(&self) -> std::io::Result<()> {
        match self {
            Stream::Tcp(stream) => stream.shutdown(Shutdown::Both),
            Stream::Unix(stream) => stream.shutdown(Shutdown::Both),
        }
    }
}

impl AsRawFd for Stream {
    fn as_raw_fd(&self) -> RawFd {
        match self {
            Stream::Tcp(stream) => stream.as_raw_fd(),
            Stream::Unix(stream) => stream.as_raw_fd(),
        }
    }
}

impl Read for Stream {
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        match self {
            Stream::Tcp(stream) => stream.read(buf),
            Stream::Unix(stream) => stream.read(buf),
        }
    }
}

impl Write for Stream {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Stream::Tcp(stream) => stream.write(buf),
            Stream::Unix(stream) => stream.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

/// A connection, with what has arrived on it but not been read yet.
#[derive(Debug)]
pub struct Socket {
    /// The connection, until `close` drops it.
    stream: Option<Stream>,
    /// Bytes received but not read yet.
    buffer: Vec<u8>,
    /// Whether the other end has closed the connection, so nothing more will arrive.
    ended: bool,
    /// How many milliseconds each wait may take, or -1 to wait forever.
    timeout: i64,
    /// Memory to share with the other end, which `shm_create` makes and `close` drops.
    shared: Option<std::fs::File>,
    /// The newest descriptor the other end sent, until `take_fd` takes it.
    received: Option<std::fs::File>,
}

impl Socket {
    fn new(stream: Stream) -> Socket {
        Socket {
            stream: Some(stream),
            buffer: vec![],
            ended: false,
            timeout: -1,
            shared: None,
            received: None,
        }
    }

    /// Makes each later wait give up after `seconds`, counted by [`timeout_ms`].
    pub fn set_timeout(&mut self, seconds: f64) {
        self.timeout = timeout_ms(seconds);
    }

    fn stream(&mut self) -> Result<&mut Stream, String> {
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
        let (n, fd) = loop {
            let read = match stream {
                // a Unix socket may bring a descriptor along, which only Linux's runtime takes
                Stream::Unix(unix) if cfg!(target_os = "linux") => {
                    read_with_fd(unix.as_raw_fd(), &mut chunk)
                }
                _ => stream.read(&mut chunk).map(|n| (n, None)),
            };
            match read {
                Ok(read) => break read,
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(reason(&e)),
            }
        };
        if fd.is_some() {
            self.received = fd;
        }
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
            let _ = stream.shutdown();
        }
        self.shared = None;
        self.received = None;
    }

    /// Returns at most `size` bytes from the start of the file whose descriptor the other end
    /// sent most recently, as a Wayland compositor sends its keymap, and closes it.
    pub fn take_fd(&mut self, size: i64) -> Result<Vec<u8>, String> {
        use std::os::unix::fs::FileExt;
        self.stream()?;
        let file = self.received.take().ok_or_else(|| FD_MISSING.to_string())?;
        let mut data = vec![0; size.max(0) as usize];
        let mut filled = 0;
        while filled < data.len() {
            match file.read_at(&mut data[filled..], filled as u64) {
                Ok(0) => break,
                Ok(n) => filled += n,
                Err(e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(reason(&e)),
            }
        }
        data.truncate(filled);
        Ok(data)
    }

    /// Makes `size` bytes of zeros to share with the other end of a Unix socket, in place of any
    /// made before, returning `size`: a `memfd_create` file, which `shm_send` sends. Only Linux
    /// has one, so elsewhere it fails as a system call that does not exist does.
    pub fn shm_create(&mut self, size: i64) -> Result<i64, String> {
        self.stream()?;
        self.shared = None;
        let file = memory_file()?;
        file.set_len(size.max(0) as u64).map_err(|e| reason(&e))?;
        self.shared = Some(file);
        Ok(size)
    }

    /// Writes `data` to the shared memory from `offset`, returning its length. A write past the
    /// memory's end fails, rather than growing it.
    pub fn shm_write(&mut self, offset: i64, data: &[u8]) -> Result<i64, String> {
        use std::os::unix::fs::FileExt;
        self.stream()?;
        let file = self
            .shared
            .as_ref()
            .ok_or_else(|| SHM_MISSING.to_string())?;
        let size = file.metadata().map_err(|e| reason(&e))?.len();
        if offset < 0 || offset as u64 + data.len() as u64 > size {
            return Err(reason(&std::io::Error::from_raw_os_error(ENOSPC)));
        }
        file.write_all_at(data, offset as u64)
            .map_err(|e| reason(&e))?;
        Ok(data.len() as i64)
    }

    /// Sends `data` with the shared memory's descriptor, which a Unix socket passes to the other
    /// end, returning its length.
    pub fn shm_send(&mut self, data: &[u8]) -> Result<i64, String> {
        let stream = self
            .stream
            .as_mut()
            .ok_or_else(|| SOCKET_CLOSED.to_string())?;
        let file = self
            .shared
            .as_ref()
            .ok_or_else(|| SHM_MISSING.to_string())?;
        let sent = send_with_fd(stream.as_raw_fd(), data, file.as_raw_fd())?;
        // the descriptor went with the first part, and the rest is sent plainly
        stream.write_all(&data[sent..]).map_err(|e| reason(&e))?;
        Ok(data.len() as i64)
    }

    /// Returns whether a read would not wait, checking for at most `timeout` milliseconds, or
    /// forever if it is -1: when bytes not read yet are in the buffer, the other end has closed
    /// the connection or something has arrived, an error waits, or the socket is closed (so a
    /// read fails at once). A signal that cuts the wait short starts it again with all of
    /// `timeout`, as compiled code's `stone.net_ready` does.
    pub fn ready(&mut self, timeout: i64) -> bool {
        let Some(stream) = self.stream.as_ref() else {
            return true;
        };
        if !self.buffer.is_empty() || self.ended {
            return true;
        }
        let ms = timeout.clamp(-1, c_int::MAX as i64) as c_int;
        loop {
            let mut pollfd = PollFd {
                fd: stream.as_raw_fd(),
                events: POLLIN,
                revents: 0,
            };
            // SAFETY: pollfd is one valid, writable `struct pollfd` for the whole call
            let ready = unsafe { poll(&mut pollfd, 1, ms) };
            if ready >= 0 || std::io::Error::last_os_error().kind() != ErrorKind::Interrupted {
                return ready != 0;
            }
        }
    }
}

/// The error number of a write that does not fit, `ENOSPC`.
const ENOSPC: i32 = 28;

/// Returns a new, empty file in memory, closed when the program runs another.
#[cfg(target_os = "linux")]
fn memory_file() -> Result<std::fs::File, String> {
    use std::os::fd::FromRawFd;
    if cfg!(fuzzing) {
        return Err(NET_DISABLED.to_string());
    }
    // MFD_CLOEXEC
    // SAFETY: the name is a valid C string, and the call returns a descriptor or -1
    let fd = unsafe { memfd_create(c"stone".as_ptr(), 1) };
    if fd < 0 {
        return Err(reason(&std::io::Error::last_os_error()));
    }
    // SAFETY: the descriptor is new, and nothing else owns it
    Ok(unsafe { std::fs::File::from_raw_fd(fd) })
}

#[cfg(not(target_os = "linux"))]
fn memory_file() -> Result<std::fs::File, String> {
    // ENOSYS, as macOS numbers it
    Err(reason(&std::io::Error::from_raw_os_error(78)))
}

/// Sends `data` on the socket `fd` with the descriptor `shared`, made again while a signal
/// interrupts it, returning how much of `data` went.
#[cfg(target_os = "linux")]
fn send_with_fd(fd: RawFd, data: &[u8], shared: RawFd) -> Result<usize, String> {
    // a cmsghdr of 20 bytes, its length, SOL_SOCKET, and SCM_RIGHTS, then the descriptor
    let mut control = [20u64, 1 | 1 << 32, shared as u32 as u64];
    let iov = IoVec {
        base: data.as_ptr().cast_mut().cast(),
        len: data.len(),
    };
    let msg = MsgHdr {
        name: std::ptr::null_mut(),
        namelen: 0,
        iov: &iov,
        iovlen: 1,
        control: control.as_mut_ptr().cast(),
        controllen: 24,
        flags: 0,
    };
    loop {
        // SAFETY: msg points at one iovec over `data` and 24 bytes of control, all of which
        // outlive the call; MSG_NOSIGNAL
        let sent = unsafe { sendmsg(fd, &msg, 0x4000) };
        if sent >= 0 {
            return Ok(sent as usize);
        }
        let error = std::io::Error::last_os_error();
        if error.kind() != ErrorKind::Interrupted {
            return Err(reason(&error));
        }
    }
}

/// Reads what the socket `fd` has into `buf`, with a descriptor sent along, if one was,
/// close-on-exec.
#[cfg(target_os = "linux")]
fn read_with_fd(fd: RawFd, buf: &mut [u8]) -> std::io::Result<(usize, Option<std::fs::File>)> {
    use std::os::fd::FromRawFd;
    let mut control = [0u64; 3];
    let iov = IoVec {
        base: buf.as_mut_ptr().cast(),
        len: buf.len(),
    };
    let mut msg = MsgHdr {
        name: std::ptr::null_mut(),
        namelen: 0,
        iov: &iov,
        iovlen: 1,
        control: control.as_mut_ptr().cast(),
        controllen: 24,
        flags: 0,
    };
    // SAFETY: msg points at one iovec over `buf` and 24 bytes of control, which outlive the
    // call; MSG_CMSG_CLOEXEC
    let n = unsafe { recvmsg(fd, &mut msg, 0x4000_0000) };
    if n < 0 {
        return Err(std::io::Error::last_os_error());
    }
    // a cmsghdr of SOL_SOCKET and SCM_RIGHTS holds the descriptor
    let fd = match msg.controllen >= 20 && control[1] == 1 | 1 << 32 {
        // SAFETY: the kernel made the descriptor for this process, and nothing else owns it
        true => Some(unsafe { std::fs::File::from_raw_fd(control[2] as u32 as RawFd) }),
        false => None,
    };
    Ok((n as usize, fd))
}

#[cfg(not(target_os = "linux"))]
fn read_with_fd(_: RawFd, _: &mut [u8]) -> std::io::Result<(usize, Option<std::fs::File>)> {
    Err(ErrorKind::Unsupported.into())
}

#[cfg(not(target_os = "linux"))]
fn send_with_fd(_: RawFd, _: &[u8], _: RawFd) -> Result<usize, String> {
    Err(reason(&std::io::Error::from_raw_os_error(78)))
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
                Ok((stream, _)) => return Ok(Socket::new(Stream::Tcp(stream))),
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
    Ok(Socket::new(Stream::Tcp(stream)))
}

/// Connects to the Unix socket at `path`, which `name` quotes in errors.
pub fn connect_unix(path: &str, name: &str) -> Result<Socket, String> {
    if cfg!(fuzzing) {
        return Err(NET_DISABLED.to_string());
    }
    let stream = UnixStream::connect(path).map_err(|e| io_message(&e, name))?;
    Ok(Socket::new(Stream::Unix(stream)))
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
    fn a_unix_socket_says_when_something_has_arrived() {
        let dir = std::env::temp_dir().join(format!("stone-unix-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let path = dir.join("ready.sock");
        let _ = std::fs::remove_file(&path);
        let server = std::os::unix::net::UnixListener::bind(&path).unwrap();
        let name = path.to_str().unwrap();
        let mut client = connect_unix(name, "here").unwrap();
        let (mut other, _) = server.accept().unwrap();
        assert!(!client.ready(0));
        assert!(!client.ready(10));
        other.write_all(b"ab").unwrap();
        assert!(client.ready(-1));
        assert_eq!(client.read_bytes(1), Ok(b"a".to_vec()));
        // the other byte is in the buffer already
        assert!(client.ready(0));
        assert_eq!(client.read_bytes(1), Ok(b"b".to_vec()));
        drop(other);
        assert!(client.ready(-1));
        client.close();
        assert!(client.ready(-1));
        let missing = dir.join("missing.sock");
        assert_eq!(
            connect_unix(missing.to_str().unwrap(), "there").err(),
            Some("no such file or directory: 'there'".to_string())
        );
        std::fs::remove_dir_all(&dir).unwrap();
    }

    /// Reads what `stream` sends until `buf` is full, returning the descriptor that came with it,
    /// as an X server receives one.
    #[cfg(target_os = "linux")]
    fn receive_with_fd(stream: &UnixStream, buf: &mut [u8]) -> RawFd {
        unsafe extern "C" {
            fn recvmsg(fd: c_int, msg: *mut MsgHdr, flags: c_int) -> isize;
        }
        let mut control = [0u64; 3];
        let iov = IoVec {
            base: buf.as_mut_ptr().cast(),
            len: buf.len(),
        };
        let mut msg = MsgHdr {
            name: std::ptr::null_mut(),
            namelen: 0,
            iov: &iov,
            iovlen: 1,
            control: control.as_mut_ptr().cast(),
            controllen: 24,
            flags: 0,
        };
        // SAFETY: msg points at one iovec over `buf` and 24 bytes of control space
        let n = unsafe { recvmsg(stream.as_raw_fd(), &mut msg, 0) };
        assert_eq!(n as usize, buf.len());
        // the cmsghdr's length, level, and type, then the descriptor
        assert_eq!(control[0], 20);
        (control[2] & 0xffff_ffff) as RawFd
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn a_socket_shares_memory_through_a_descriptor_it_sends() {
        use std::os::fd::FromRawFd;
        use std::os::unix::fs::FileExt;
        let (near, far) = UnixStream::pair().unwrap();
        let mut socket = Socket::new(Stream::Unix(near));
        assert_eq!(socket.shm_write(0, b"x"), Err(SHM_MISSING.to_string()));
        assert_eq!(socket.shm_create(16), Ok(16));
        assert_eq!(socket.shm_write(4, b"abcd"), Ok(4));
        assert_eq!(socket.shm_send(b"head"), Ok(4));
        let mut head = [0; 4];
        let fd = receive_with_fd(&far, &mut head);
        assert_eq!(&head, b"head");
        // SAFETY: the descriptor came with the message and nothing else owns it
        let file = unsafe { std::fs::File::from_raw_fd(fd) };
        let mut seen = [0; 16];
        file.read_exact_at(&mut seen, 0).unwrap();
        assert_eq!(&seen[..8], b"\0\0\0\0abcd");
        // a write the other end makes after the descriptor went is seen through it too
        assert_eq!(socket.shm_write(0, b"zz"), Ok(2));
        file.read_exact_at(&mut seen[..2], 0).unwrap();
        assert_eq!(&seen[..2], b"zz");
        // a write past the end fails, and a closed socket has none
        assert!(socket.shm_write(15, b"ab").is_err());
        socket.close();
        assert_eq!(socket.shm_create(8), Err(SOCKET_CLOSED.to_string()));
        assert_eq!(socket.shm_write(0, b"x"), Err(SOCKET_CLOSED.to_string()));
    }

    #[test]
    #[cfg(target_os = "linux")]
    fn a_unix_socket_keeps_the_newest_descriptor_it_receives() {
        let (near, far) = UnixStream::pair().unwrap();
        let mut socket = Socket::new(Stream::Unix(near));
        assert_eq!(socket.take_fd(10), Err(FD_MISSING.to_string()));
        let dir = std::env::temp_dir().join(format!("stone-fd-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let old = dir.join("old");
        let new = dir.join("new");
        std::fs::write(&old, "old keymap").unwrap();
        std::fs::write(&new, "xkb_keymap {}").unwrap();
        for (path, data) in [(&old, &b"one"[..]), (&new, &b"two"[..])] {
            let file = std::fs::File::open(path).unwrap();
            assert_eq!(send_with_fd(far.as_raw_fd(), data, file.as_raw_fd()), Ok(3));
        }
        assert_eq!(socket.read_exact(6), Ok(b"onetwo".to_vec()));
        // the first descriptor was closed when the second came
        assert_eq!(socket.take_fd(100), Ok(b"xkb_keymap {}".to_vec()));
        assert_eq!(socket.take_fd(100), Err(FD_MISSING.to_string()));
        // a size cuts the text short
        let file = std::fs::File::open(&new).unwrap();
        send_with_fd(far.as_raw_fd(), b"!", file.as_raw_fd()).unwrap();
        assert_eq!(socket.read_exact(1), Ok(b"!".to_vec()));
        assert_eq!(socket.take_fd(3), Ok(b"xkb".to_vec()));
        socket.close();
        assert_eq!(socket.take_fd(3), Err(SOCKET_CLOSED.to_string()));
        std::fs::remove_dir_all(&dir).unwrap();
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
