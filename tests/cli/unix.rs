//! The tests of `cli.rs` that run only on Unix: the `net` module's, against a fake DNS server
//! and over Unix sockets, and the `gui` module's, against fake and real X11 and Wayland servers.

use super::*;

/// Encodes `name` as a DNS name, a length before each dot-separated label and a 0 at the end.
fn dns_name(name: &str) -> Vec<u8> {
    let mut out = vec![];
    for label in name.split('.') {
        out.push(label.len() as u8);
        out.extend_from_slice(label.as_bytes());
    }
    out.push(0);
    out
}

/// Returns a DNS server's reply to `query` from a fixed set of answers: the question's name and
/// type decide the reply, which may be malformed or never come, as the stone resolver must
/// handle.
fn dns_reply(query: &[u8]) -> Option<Vec<u8>> {
    let mut at = 12;
    let mut labels = vec![];
    while query[at] != 0 {
        let length = query[at] as usize;
        labels.push(String::from_utf8_lossy(&query[at + 1..at + 1 + length]).to_string());
        at += 1 + length;
    }
    let name = labels.join(".");
    let question = &query[12..at + 5];
    let kind = u16::from_be_bytes([query[at + 1], query[at + 2]]);
    let record = |rtype: u16, data: &[u8]| {
        let mut out = vec![0xc0, 12];
        out.extend_from_slice(&rtype.to_be_bytes());
        out.extend_from_slice(&[0, 1, 0, 0, 0, 60]);
        out.extend_from_slice(&(data.len() as u16).to_be_bytes());
        out.extend_from_slice(data);
        out
    };
    let v6 = [0x20, 0x01, 0x0d, 0xb8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7];
    let (rcode, answers): (u8, Vec<Vec<u8>>) = match (name.as_str(), kind) {
        ("alpha.test", 1) => (0, vec![record(1, &[10, 1, 2, 3])]),
        ("alpha.test", 28) => (0, vec![record(28, &v6)]),
        // another name for alpha.test, whose own record follows under that name
        ("www.test", _) => {
            let alias = record(5, &dns_name("alpha.test"));
            let mut target = dns_name("alpha.test");
            let data: &[u8] = if kind == 1 { &[10, 1, 2, 3] } else { &v6 };
            target.extend_from_slice(&kind.to_be_bytes());
            target.extend_from_slice(&[0, 1, 0, 0, 0, 60]);
            target.extend_from_slice(&(data.len() as u16).to_be_bytes());
            target.extend_from_slice(data);
            (0, vec![alias, target])
        }
        ("short.corp.test", 1) => (0, vec![record(1, &[10, 9, 9, 9])]),
        ("short.corp.test", _) => (0, vec![]),
        ("broken.test", _) => return Some(query[..5].to_vec()),
        ("refused.test", _) => (5, vec![]),
        ("slow.test", _) => return None,
        _ => (3, vec![]),
    };
    let mut reply = vec![
        query[0],
        query[1],
        0x81,
        0x80 | rcode,
        0,
        1,
        0,
        answers.len() as u8,
    ];
    reply.extend_from_slice(&[0, 0, 0, 0]);
    reply.extend_from_slice(question);
    for answer in answers {
        reply.extend_from_slice(&answer);
    }
    Some(reply)
}

#[test]
fn both_backends_resolve_names_through_dns_alike() {
    let server = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let port = server.local_addr().unwrap().port();
    std::thread::spawn(move || {
        let mut query = [0; 512];
        while let Ok((n, from)) = server.recv_from(&mut query) {
            if let Some(reply) = dns_reply(&query[..n]) {
                server.send_to(&reply, from).unwrap();
            }
        }
    });
    let source = "use net\nfor host in [\"alpha.test\", \"WWW.test.\", \"short\", \"missing.test\", \
                  \"broken.test\", \"refused.test\", \"slow.test\"]:\n    \
                  print(host, net.resolve(host))\n";
    let conf =
        format!("nameserver 127.0.0.1:{port}\nsearch corp.test\noptions timeout:1 attempts:1\n");
    let (_, [run, built]) = run_and_build_env(
        "net_dns",
        source,
        |dir| {
            std::fs::write(dir.join("resolv.conf"), &conf).unwrap();
            std::fs::write(dir.join("hosts"), "").unwrap();
        },
        &[
            ("STONE_RESOLV_CONF", "resolv.conf"),
            ("STONE_HOSTS", "hosts"),
        ],
    );
    let expected = "alpha.test ok(['10.1.2.3', '2001:db8::7'])\n\
                    WWW.test. ok(['10.1.2.3', '2001:db8::7'])\n\
                    short ok(['10.9.9.9'])\n\
                    missing.test err(\"unknown host: 'missing.test'\")\n\
                    broken.test err(\"bad DNS reply: '127.0.0.1'\")\n\
                    refused.test err(\"refused: '127.0.0.1'\")\n\
                    slow.test err(\"timed out: '127.0.0.1'\")\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

#[test]
fn both_backends_talk_over_a_unix_socket_alike() {
    let source = "use net\n\
                  s = net.connect_unix(\"echo.sock\").unwrap()\n\
                  s.send(\"ping\\n\").unwrap()\n\
                  print(s.read_line())\n\
                  s.send_bytes(b\"\\x00\\xff\\n\").unwrap()\n\
                  print(s.read_exact(3))\n";
    let (_, [run, built]) = run_and_build_env(
        "net_unix_echo",
        source,
        |dir| {
            let path = dir.join("echo.sock");
            let _ = std::fs::remove_file(&path);
            let server = std::os::unix::net::UnixListener::bind(&path).unwrap();
            std::thread::spawn(move || {
                for mut client in server.incoming().flatten() {
                    std::thread::spawn(move || {
                        let mut copy = client.try_clone().unwrap();
                        let _ = std::io::copy(&mut client, &mut copy);
                    });
                }
            });
        },
        &[],
    );
    let expected = "ok(some('ping'))\nok(b'\\x00\\xff\\n')\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

#[test]
fn both_backends_give_up_connecting_after_a_timeout() {
    // an address in a private range that nothing answers, so a connection waits until it times
    // out, unless this network refuses it at once
    let source = "use net\nprint(net.connect_timeout(\"10.255.255.1\", 80, 0.2))\n";
    let (_, [run, built]) = run_and_build_in("net_connect_timeout", source);
    let expected = "err(\"timed out: '10.255.255.1:80'\")\n";
    if String::from_utf8_lossy(&run.stdout) != expected {
        eprintln!(
            "skipping: this network does not leave 10.255.255.1 unanswered ({})",
            String::from_utf8_lossy(&run.stdout).trim()
        );
        return;
    }
    assert!(built.status.success(), "{built:?}");
    assert_eq!(String::from_utf8_lossy(&built.stdout), expected);
}

/// How the fake X server of [`fake_x_server`] describes itself.
#[derive(Clone, Copy)]
struct FakeDisplay {
    /// Whether it takes pixels most significant byte first.
    msb: bool,
    /// The depth of the root window, and the bits per pixel of its pixmap format.
    depth: u8,
    bits: u8,
    /// The most 4-byte units one request may hold.
    max_request: u16,
    /// The reason it refuses connections, if it does.
    refuse: Option<&'static str>,
    /// The events it sends once the window is mapped.
    script: FakeScript,
    /// Whether its setup reply is cut short after 20 bytes, its length saying so.
    short: bool,
    /// What it does with MIT-SHM.
    shm: FakeShm,
    /// Whether it has the Present extension, which shows pixmaps at the next vblank.
    present: bool,
}

/// The events a fake X server sends once the window is mapped.
#[derive(Clone, Copy, PartialEq)]
enum FakeScript {
    /// [`fake_events`], all at once.
    All,
    /// [`paced_events`], a part after each frame.
    Paced,
    /// An exposure alone.
    Expose,
}

/// What a fake X server does with the MIT-SHM extension.
#[derive(Clone, Copy, PartialEq)]
enum FakeShm {
    /// It does not have it.
    Absent,
    /// It shows images from the memory it is sent.
    Working,
    /// It has it, but answers every attachment with an error.
    Refusing,
}

/// The major opcode and first event code the fake X server gives MIT-SHM.
const FAKE_SHM: (u8, u8) = (130, 70);

/// The major opcode the fake X server gives the Present extension, whose events are generic.
const FAKE_PRESENT: u8 = 140;

/// Answers the Present request `request` as a fake X server that shows every pixmap at once: a
/// PresentPixmap completes and leaves its pixmap idle right away, as a copy does.
fn fake_present_request(
    stream: &mut std::os::unix::net::UnixStream,
    request: &[u8],
) -> std::io::Result<()> {
    use std::io::Write;
    let word = |at: usize| u32::from_le_bytes(request[at..at + 4].try_into().unwrap());
    match request[1] {
        // PresentQueryVersion: 1.0
        0 => {
            let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0];
            reply.extend(1u32.to_le_bytes());
            reply.extend(0u32.to_le_bytes());
            reply.resize(32, 0);
            stream.write_all(&reply)?;
        }
        // PresentSelectInput, which the test checks from the requests
        3 => {}
        // PresentPixmap(window, pixmap, serial, ...): CompleteNotify, then IdleNotify
        1 => {
            let (window, pixmap, serial) = (word(4), word(8), word(12));
            let mut complete = generic_event(1, 2);
            complete[16..20].copy_from_slice(&window.to_le_bytes());
            complete[20..24].copy_from_slice(&serial.to_le_bytes());
            // the msc, which only grows
            complete[32..36].copy_from_slice(&serial.to_le_bytes());
            let mut idle = generic_event(2, 0);
            idle[16..20].copy_from_slice(&window.to_le_bytes());
            idle[20..24].copy_from_slice(&serial.to_le_bytes());
            idle[24..28].copy_from_slice(&pixmap.to_le_bytes());
            stream.write_all(&[complete, idle].concat())?;
        }
        minor => panic!("unexpected Present request {minor}"),
    }
    Ok(())
}

/// A Present event of type `evtype`, a GenericEvent of 32 bytes and `extra` more 4-byte units.
fn generic_event(evtype: u16, extra: u32) -> Vec<u8> {
    let mut event = vec![35, FAKE_PRESENT, 0, 0];
    event.extend(extra.to_le_bytes());
    event.extend(evtype.to_le_bytes());
    event.resize(32 + 4 * extra as usize, 0);
    event
}

/// What the fake X server saw on one connection: the authorization's name and data, and every
/// request after the setup.
#[derive(Default, Debug)]
struct FakeConnection {
    auth: (Vec<u8>, Vec<u8>),
    requests: Vec<Vec<u8>>,
    /// Whether the client has closed the connection.
    closed: bool,
    /// The pixels of each ShmPutImage, as 4 bytes each by row.
    shm_images: Vec<Vec<u8>>,
}

/// C's `struct iovec`, for [`receive`].
#[repr(C)]
struct IoVec {
    base: *mut u8,
    len: usize,
}

/// Linux's `struct msghdr`, for [`receive`].
#[repr(C)]
struct MsgHdr {
    name: *mut u8,
    namelen: u32,
    iov: *mut IoVec,
    iovlen: usize,
    control: *mut u8,
    controllen: usize,
    flags: i32,
}

unsafe extern "C" {
    fn recvmsg(fd: i32, msg: *mut MsgHdr, flags: i32) -> isize;
}

/// Reads from `stream` until `buf` is full, as an X server does, adding any descriptor that
/// comes with the bytes to `fds`. On macOS, where clients never send one, it reads plainly.
fn receive(
    stream: &mut std::os::unix::net::UnixStream,
    buf: &mut [u8],
    fds: &mut Vec<std::fs::File>,
) -> std::io::Result<()> {
    use std::io::Read;
    use std::os::fd::{AsRawFd, FromRawFd};
    if !cfg!(target_os = "linux") {
        return stream.read_exact(buf);
    }
    let mut filled = 0;
    while filled < buf.len() {
        let mut control = [0u64; 3];
        let mut iov = IoVec {
            base: buf[filled..].as_mut_ptr(),
            len: buf.len() - filled,
        };
        let mut msg = MsgHdr {
            name: std::ptr::null_mut(),
            namelen: 0,
            iov: &mut iov,
            iovlen: 1,
            control: control.as_mut_ptr().cast(),
            controllen: 24,
            flags: 0,
        };
        // SAFETY: msg points at one iovec over the rest of `buf` and 24 bytes of control
        let n = unsafe { recvmsg(stream.as_raw_fd(), &mut msg, 0) };
        if n <= 0 {
            return Err(std::io::ErrorKind::UnexpectedEof.into());
        }
        if msg.controllen >= 20 && control[1] == 1 | 1 << 32 {
            // SAFETY: SCM_RIGHTS gave this process the descriptor, which nothing else owns
            fds.push(unsafe { std::fs::File::from_raw_fd(control[2] as u32 as i32) });
        }
        filled += n as usize;
    }
    Ok(())
}

/// Answers the MIT-SHM request `request` as a fake X server whose extension does `shm`, with
/// the descriptors the client sent in `fds`, the segments it attached in `segments`, and what
/// it shows recorded in `seen`.
fn fake_shm_request(
    stream: &mut std::os::unix::net::UnixStream,
    request: &[u8],
    shm: FakeShm,
    fds: &mut Vec<std::fs::File>,
    segments: &mut std::collections::HashMap<u32, std::fs::File>,
    seen: &std::sync::Mutex<FakeConnection>,
) -> std::io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::FileExt;
    let word = |at: usize| u32::from_le_bytes(request[at..at + 4].try_into().unwrap());
    let half = |at: usize| u16::from_le_bytes([request[at], request[at + 1]]) as usize;
    match request[1] {
        // ShmQueryVersion: 1.2, which has ShmAttachFd
        0 => {
            let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0];
            reply.resize(32, 0);
            stream.write_all(&reply)?;
        }
        // ShmDetach
        2 => {
            segments.remove(&word(4));
        }
        // ShmPutImage of the whole image, from its segment
        3 => {
            let (width, height) = (half(12), half(14));
            assert_eq!(
                (half(16), half(18), half(20), half(22)),
                (0, 0, width, height)
            );
            let mut pixels = vec![0; width * height * 4];
            segments[&word(32)].read_exact_at(&mut pixels, word(36) as u64)?;
            seen.lock().unwrap().shm_images.push(pixels);
            if request[30] == 1 {
                let completion = x11_event(
                    FAKE_SHM.1,
                    0,
                    &[
                        (4, word(4), 4),
                        (8, 3, 2),
                        (10, FAKE_SHM.0 as u32, 1),
                        (12, word(32), 4),
                    ],
                );
                stream.write_all(&completion)?;
            }
        }
        // ShmAttachFd, with the descriptor that came with it
        6 => {
            let fd = fds.remove(0);
            match shm {
                FakeShm::Refusing => {
                    // BadAccess
                    let error = x11_event(
                        0,
                        10,
                        &[(4, word(4), 4), (8, 6, 2), (10, FAKE_SHM.0 as u32, 1)],
                    );
                    stream.write_all(&error)?;
                }
                _ => {
                    segments.insert(word(4), fd);
                }
            }
        }
        minor => panic!("unexpected MIT-SHM request {minor}"),
    }
    Ok(())
}

/// The atoms the fake X server gives out, by name.
const FAKE_ATOMS: [(&str, u32); 4] = [
    ("WM_PROTOCOLS", 301),
    ("WM_DELETE_WINDOW", 302),
    ("_NET_WM_NAME", 303),
    ("UTF8_STRING", 304),
];

/// The fake keyboard's keysyms, two per keycode from 8: `a` on 38, `1` on 10, Escape on 9,
/// Shift on 50, the Greek alpha, a Unicode keysym, on 60, and the Cyrillic ef, a keysym of its
/// own (Cyrillic_ef and Cyrillic_EF), on 61.
fn fake_keysyms() -> Vec<u32> {
    let mut syms = vec![0; 248 * 2];
    let mut set = |keycode: usize, plain: u32, shifted: u32| {
        syms[(keycode - 8) * 2] = plain;
        syms[(keycode - 8) * 2 + 1] = shifted;
    };
    set(38, 0x61, 0x41);
    set(10, 0x31, 0x21);
    set(9, 0xff1b, 0);
    set(50, 0xffe1, 0);
    set(60, 0x0100_03b1, 0x0100_0391);
    set(61, 0x6c6, 0x6e6);
    syms
}

/// A 32-byte X11 event: `code`, `detail`, a sequence number of 0, then `fields`, each written at
/// its offset as two or four bytes, least significant first.
fn x11_event(code: u8, detail: u8, fields: &[(usize, u32, usize)]) -> [u8; 32] {
    let mut event = [0; 32];
    event[0] = code;
    event[1] = detail;
    for &(at, value, size) in fields {
        event[at..at + size].copy_from_slice(&value.to_le_bytes()[..size]);
    }
    event
}

/// A key or button event of `code` for `detail` at `time`, with the modifier `state`, at the
/// point `x`, `y` of the window.
fn x11_input(code: u8, detail: u8, time: u32, state: u32, x: i16, y: i16) -> [u8; 32] {
    x11_event(
        code,
        detail,
        &[
            (4, time, 4),
            (24, x as u16 as u32, 2),
            (26, y as u16 as u32, 2),
            (28, state, 2),
        ],
    )
}

/// The events the fake X server sends once the window `window` is mapped.
fn fake_events(window: u32) -> Vec<u8> {
    let expose = |count| x11_event(12, 0, &[(4, window, 4), (16, count, 2)]);
    let events = [
        expose(0),
        x11_event(
            22,
            0,
            &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
        ),
        x11_input(2, 38, 10, 0, 0, 0),
        // a held key repeats as a release and a press at the same time
        x11_input(3, 38, 20, 0, 0, 0),
        x11_input(2, 38, 20, 0, 0, 0),
        x11_input(3, 38, 30, 0, 0, 0),
        // Shift, Escape, Control, Caps Lock on a digit and a letter, Shift itself, and alpha
        x11_input(2, 38, 40, 1, 0, 0),
        x11_input(2, 9, 41, 0, 0, 0),
        x11_input(2, 38, 42, 4, 0, 0),
        x11_input(2, 10, 43, 2, 0, 0),
        x11_input(2, 38, 44, 2, 0, 0),
        x11_input(2, 50, 45, 0, 0, 0),
        x11_input(2, 60, 46, 0, 0, 0),
        // a legacy keysym, alone and with Caps Lock
        x11_input(2, 61, 47, 0, 0, 0),
        x11_input(2, 61, 48, 2, 0, 0),
        x11_input(4, 1, 50, 0, 10, 20),
        x11_input(5, 3, 51, 0, 11, -2),
        x11_input(4, 4, 52, 0, 0, 0),
        x11_input(5, 4, 53, 0, 0, 0),
        x11_input(4, 5, 54, 0, 0, 0),
        x11_input(6, 0, 55, 0, 5, 6),
        // a size that did not change, then an exposure in two parts
        x11_event(
            22,
            0,
            &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
        ),
        expose(1),
        expose(0),
        // a client message of another protocol, then the close button
        x11_event(33, 32, &[(4, window, 4), (8, 301, 4), (12, 999, 4)]),
        x11_event(33, 32, &[(4, window, 4), (8, 301, 4), (12, 302, 4)]),
    ];
    events.concat()
}

/// The part of its events a paced fake X server sends at stage `stage` for the window `window`:
/// an exposure once it is mapped (0), a new size and `a` after the first frame (1), `A` after
/// the first frame of the new size (2), and the close button after the next (3).
fn paced_events(window: u32, stage: usize) -> Vec<u8> {
    let events = match stage {
        0 => vec![x11_event(12, 0, &[(4, window, 4)])],
        1 => vec![
            x11_event(
                22,
                0,
                &[(4, window, 4), (8, window, 4), (20, 400, 2), (22, 300, 2)],
            ),
            x11_input(2, 38, 1, 0, 0, 0),
        ],
        2 => vec![x11_input(2, 38, 2, 1, 0, 0)],
        3 => vec![x11_event(
            33,
            32,
            &[(4, window, 4), (8, 301, 4), (12, 302, 4)],
        )],
        _ => vec![],
    };
    events.concat()
}

/// The setup reply of the fake X server described by `display`, or its refusal.
fn fake_setup(display: FakeDisplay) -> Vec<u8> {
    if let Some(reason) = display.refuse {
        let mut padded = reason.as_bytes().to_vec();
        padded.resize(reason.len().div_ceil(4) * 4, 0);
        let mut reply = vec![0, reason.len() as u8, 11, 0, 0, 0];
        reply.extend((padded.len() as u16 / 4).to_le_bytes());
        reply.extend(padded);
        return reply;
    }
    let mut more = vec![];
    more.extend(1u32.to_le_bytes()); // release
    more.extend(0x0020_0000u32.to_le_bytes()); // resource id base
    more.extend(0x001f_ffffu32.to_le_bytes()); // resource id mask
    more.extend(0u32.to_le_bytes()); // motion buffer
    more.extend(8u16.to_le_bytes()); // vendor length
    more.extend(display.max_request.to_le_bytes());
    more.extend([1, 1, display.msb as u8, 0, 32, 32, 8, 255, 0, 0, 0, 0]);
    more.extend(b"fakestn!");
    more.extend([display.depth, display.bits, 32, 0, 0, 0, 0, 0]);
    // the screen
    more.extend(0x100u32.to_le_bytes());
    more.extend(0x20u32.to_le_bytes());
    more.extend(0xff_ffffu32.to_le_bytes());
    more.extend(0u32.to_le_bytes());
    more.extend(0u32.to_le_bytes());
    for value in [1920u16, 1080, 500, 300, 1, 1] {
        more.extend(value.to_le_bytes());
    }
    more.extend(0x21u32.to_le_bytes());
    more.extend([0, 0, display.depth, 1]);
    // its one depth, with one TrueColor visual
    more.extend([display.depth, 0, 1, 0, 0, 0, 0, 0]);
    more.extend(0x21u32.to_le_bytes());
    more.extend([4, 8, 0, 1]);
    let masks = match display.depth {
        16 => [0xf800u32, 0x7e0, 0x1f],
        _ => [0xff_0000, 0xff00, 0xff],
    };
    for mask in masks {
        more.extend(mask.to_le_bytes());
    }
    more.extend([0; 4]);
    if display.short {
        more.truncate(20);
    }
    let mut reply = vec![1, 0, 11, 0, 0, 0];
    reply.extend((more.len() as u16 / 4).to_le_bytes());
    reply.extend(more);
    reply
}

/// Serves one client of the fake X server on `stream`, recording what it sends in `seen`.
fn fake_x_client(
    mut stream: std::os::unix::net::UnixStream,
    display: FakeDisplay,
    seen: &std::sync::Mutex<FakeConnection>,
) -> std::io::Result<()> {
    use std::io::{Read, Write};
    let mut setup = [0; 12];
    stream.read_exact(&mut setup)?;
    let name_len = u16::from_le_bytes([setup[6], setup[7]]) as usize;
    let data_len = u16::from_le_bytes([setup[8], setup[9]]) as usize;
    let mut name = vec![0; name_len.div_ceil(4) * 4];
    let mut data = vec![0; data_len.div_ceil(4) * 4];
    stream.read_exact(&mut name)?;
    stream.read_exact(&mut data)?;
    name.truncate(name_len);
    data.truncate(data_len);
    seen.lock().unwrap().auth = (name, data);
    stream.write_all(&fake_setup(display))?;
    let mut window = 0;
    let mut stage = 0;
    let mut fds = vec![];
    let mut segments = std::collections::HashMap::new();
    loop {
        let mut head = [0; 4];
        if receive(&mut stream, &mut head, &mut fds).is_err() {
            seen.lock().unwrap().closed = true;
            return Ok(());
        }
        let units = u16::from_le_bytes([head[2], head[3]]) as usize;
        let mut request = head.to_vec();
        request.resize(units * 4, 0);
        receive(&mut stream, &mut request[4..], &mut fds)?;
        seen.lock().unwrap().requests.push(request.clone());
        // a client that closed may have sent more already, which is still read and recorded
        let word = |at: usize| u32::from_le_bytes(request[at..at + 4].try_into().unwrap());
        let mut answer = || -> std::io::Result<()> {
            match request[0] {
                1 => window = word(4),
                8 => match display.script {
                    FakeScript::All => stream.write_all(&fake_events(window))?,
                    FakeScript::Paced => stream.write_all(&paced_events(window, 0))?,
                    FakeScript::Expose => stream.write_all(&x11_event(12, 0, &[(4, window, 4)]))?,
                },
                // a frame starts with the strip at the top, and after the first, only one as wide
                // as the new size moves the script on
                72 if display.script == FakeScript::Paced && request[18..20] == [0, 0] => {
                    if stage == 0 || request[12..14] == 400u16.to_le_bytes() {
                        stage += 1;
                        stream.write_all(&paced_events(window, stage))?;
                    }
                }
                16 => {
                    let len = u16::from_le_bytes([request[4], request[5]]) as usize;
                    let name = std::str::from_utf8(&request[8..8 + len]).unwrap();
                    let atom = FAKE_ATOMS.iter().find(|(n, _)| *n == name).unwrap().1;
                    let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0];
                    reply.extend(atom.to_le_bytes());
                    reply.resize(32, 0);
                    stream.write_all(&reply)?;
                }
                // QueryExtension, which knows MIT-SHM and Present
                98 => {
                    let len = u16::from_le_bytes([request[4], request[5]]) as usize;
                    let name = &request[8..8 + len];
                    let mut reply = vec![1, 0, 0, 0, 0, 0, 0, 0];
                    if name == b"MIT-SHM" && display.shm != FakeShm::Absent {
                        reply.extend([1, FAKE_SHM.0, FAKE_SHM.1, 150]);
                    } else if name == b"Present" && display.present {
                        reply.extend([1, FAKE_PRESENT, 0, 0]);
                    }
                    reply.resize(32, 0);
                    stream.write_all(&reply)?;
                }
                // GetInputFocus, which clients ask to wait for what they sent before it
                43 => stream.write_all(&[[1].as_slice(), &[0; 31]].concat())?,
                op if op == FAKE_PRESENT => fake_present_request(&mut stream, &request)?,
                op if op == FAKE_SHM.0 => fake_shm_request(
                    &mut stream,
                    &request,
                    display.shm,
                    &mut fds,
                    &mut segments,
                    seen,
                )?,
                101 => {
                    let (first, count) = (request[4] as usize, request[5] as usize);
                    let syms = fake_keysyms();
                    let mut reply = vec![1, 2, 0, 0];
                    reply.extend(((count * 2) as u32).to_le_bytes());
                    reply.resize(32, 0);
                    for sym in &syms[(first - 8) * 2..(first - 8 + count) * 2] {
                        reply.extend(sym.to_le_bytes());
                    }
                    stream.write_all(&reply)?;
                }
                _ => {}
            }
            Ok(())
        };
        let _ = answer();
    }
}

/// Starts a fake X server described by `display`, listening on the Unix socket `path`, and
/// returns what it sees on each connection, in order.
fn fake_x_server(
    path: &Path,
    display: FakeDisplay,
) -> std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<FakeConnection>>>>> {
    use std::sync::{Arc, Mutex};
    let _ = std::fs::remove_file(path);
    let listener = std::os::unix::net::UnixListener::bind(path).unwrap();
    let connections: Arc<Mutex<Vec<Arc<Mutex<FakeConnection>>>>> = Arc::default();
    let all = connections.clone();
    std::thread::spawn(move || {
        for stream in listener.incoming().flatten() {
            let seen = Arc::new(Mutex::new(FakeConnection::default()));
            all.lock().unwrap().push(seen.clone());
            std::thread::spawn(move || {
                // a client that closed before reading everything sent ends it with an error
                let _ = fake_x_client(stream, display, &seen);
                seen.lock().unwrap().closed = true;
            });
        }
    });
    connections
}

/// The pixels of every PutImage among `requests`, as 4 bytes each by row, `width` wide.
fn put_images(requests: &[Vec<u8>], width: usize) -> Vec<u8> {
    let mut image = vec![];
    for request in requests.iter().filter(|r| r[0] == 72) {
        assert_eq!(request[1], 2, "ZPixmap");
        assert_eq!(
            u16::from_le_bytes([request[12], request[13]]) as usize,
            width
        );
        let rows = u16::from_le_bytes([request[14], request[15]]) as usize;
        let y = u16::from_le_bytes([request[18], request[19]]) as usize;
        assert_eq!(image.len(), y * width * 4, "strips in order");
        image.extend(&request[24..24 + rows * width * 4]);
    }
    image
}

const WINDOW_PROGRAM: &str = r#"use gui

match gui.open("fake", 40, 30):
    case ok(w):
        print(w.size())
        done = false
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                if e == gui.event.close:
                    done = true
        print(w.size())
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        c.rect(5, 5, 10, 10, gui.rgb(250, 128, 7))
        w.show(c)
        w.set_title("done")
        w.close()
        es = w.events()
        print(w.is_open(), es)
    case err(reason):
        print(reason)
"#;

/// Waits until the client of each connection the fake X server has seen has closed it, and
/// returns them.
fn closed_connections(
    connections: &std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<FakeConnection>>>>,
) -> Vec<FakeConnection> {
    for _ in 0..500 {
        let all = connections.lock().unwrap();
        if all.iter().all(|c| c.lock().unwrap().closed) {
            return all
                .iter()
                .map(|c| std::mem::take(&mut *c.lock().unwrap()))
                .collect();
        }
        drop(all);
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    panic!("a client never closed its connection");
}

#[test]
fn both_backends_drive_an_x11_window_alike() {
    let expected = "(40, 30)\n\
                    event.redraw\n\
                    event.resize(400, 300)\n\
                    event.key_down('a')\n\
                    event.text('a')\n\
                    event.text('a')\n\
                    event.key_up('a')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_down('escape')\n\
                    event.key_down('a')\n\
                    event.key_down('1')\n\
                    event.text('1')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_down('shift')\n\
                    event.key_down('\u{3b1}')\n\
                    event.text('\u{3b1}')\n\
                    event.key_down('\u{444}')\n\
                    event.text('\u{444}')\n\
                    event.key_down('\u{444}')\n\
                    event.text('\u{424}')\n\
                    event.mouse_down('left', 10, 20)\n\
                    event.mouse_up('right', 11, -2)\n\
                    event.scroll(1)\n\
                    event.scroll(-1)\n\
                    event.mouse_move(5, 6)\n\
                    event.redraw\n\
                    event.close\n\
                    (400, 300)\n\
                    false []\n";
    for msb in [false, true] {
        let name = format!("gui_x11_{}", if msb { "msb" } else { "lsb" });
        let display = FakeDisplay {
            msb,
            depth: 24,
            bits: 32,
            // small, so the image goes in strips of a row each
            max_request: 64,
            refuse: None,
            script: FakeScript::All,
            short: false,
            shm: FakeShm::Absent,
            present: false,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            &name,
            WINDOW_PROGRAM,
            |dir| {
                connections = Some(fake_x_server(&dir.join("X0"), display));
                // an authority file with a cookie for another display, then one for this one
                let mut file = vec![];
                for (number, cookie) in [("1", &b"wrong"[..]), ("0", &b"secret_cookie"[..])] {
                    file.extend([1, 0, 0, 4]);
                    file.extend(b"host");
                    for field in [number.as_bytes(), b"MIT-MAGIC-COOKIE-1", cookie] {
                        file.extend((field.len() as u16).to_be_bytes());
                        file.extend(field);
                    }
                }
                std::fs::write(dir.join("Xauthority"), file).unwrap();
            },
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "Xauthority"),
            ],
        );
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
        }
        let seen = closed_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_eq!(interpreted.requests, compiled.requests);
        assert_eq!(
            interpreted.auth,
            (b"MIT-MAGIC-COOKIE-1".to_vec(), b"secret_cookie".to_vec())
        );
        // one strip of a row per request, and the colors in the server's byte order
        let requests = &interpreted.requests;
        assert_eq!(requests.iter().filter(|r| r[0] == 72).count(), 30);
        let image = put_images(requests, 40);
        assert_eq!(image.len(), 40 * 30 * 4);
        let pixel = |x: usize, y: usize| &image[(y * 40 + x) * 4..(y * 40 + x) * 4 + 4];
        let (background, square): (&[u8], &[u8]) = match msb {
            false => (&[3, 2, 1, 0], &[7, 128, 250, 0]),
            true => (&[0, 1, 2, 3], &[0, 250, 128, 7]),
        };
        assert_eq!(pixel(0, 0), background);
        assert_eq!(pixel(5, 5), square);
        assert_eq!(pixel(15, 15), background);
        // the last title, WM_NAME, holds the text alone
        let title = requests
            .iter()
            .rfind(|r| r[0] == 18 && r[8..12] == 39u32.to_le_bytes())
            .unwrap();
        assert_eq!(u32::from_le_bytes(title[20..24].try_into().unwrap()), 4);
        assert_eq!(&title[24..28], b"done");
    }
}

#[test]
fn both_backends_say_why_a_window_cannot_open() {
    let program = "use gui\n\
                   for size in [(10, 10), (0, 10), (40000, 1)]:\n    \
                   match gui.open(\"x\", size[0], size[1]):\n        \
                   case ok(_):\n            \
                   print(\"opened\")\n        \
                   case err(reason):\n            \
                   print(reason)\n";
    let refusing = FakeDisplay {
        msb: false,
        depth: 24,
        bits: 32,
        max_request: 65535,
        refuse: Some("No protocol specified"),
        script: FakeScript::All,
        short: false,
        shm: FakeShm::Absent,
        present: false,
    };
    let shallow = FakeDisplay {
        depth: 16,
        bits: 16,
        refuse: None,
        ..refusing
    };
    let short = FakeDisplay {
        refuse: None,
        short: true,
        ..refusing
    };
    let base = env!("CARGO_TARGET_TMPDIR");
    let cases = [
        (
            "gui_no_display",
            String::new(),
            None,
            "no display: neither WAYLAND_DISPLAY nor DISPLAY is set".to_string(),
        ),
        (
            "gui_bad_display",
            "nowhere".to_string(),
            None,
            "invalid display: 'nowhere'".to_string(),
        ),
        (
            "gui_bad_number",
            ":x".to_string(),
            None,
            "invalid display: ':x'".to_string(),
        ),
        (
            "gui_no_socket",
            format!("{base}/gui_no_socket/X0:0"),
            None,
            format!("no such file or directory: '{base}/gui_no_socket/X0'"),
        ),
        (
            "gui_refused",
            format!("{base}/gui_refused/X0:0"),
            Some(refusing),
            "the X server refused the connection: No protocol specified".to_string(),
        ),
        (
            "gui_shallow",
            format!("{base}/gui_shallow/X0:0"),
            Some(shallow),
            "unsupported display format: depth 16, 16 bits per pixel".to_string(),
        ),
        (
            "gui_short",
            format!("{base}/gui_short/X0:0"),
            Some(short),
            "the X server sent a setup reply that is cut short".to_string(),
        ),
    ];
    for (name, display, server, first) in cases {
        let (_, [run, built]) = run_and_build_env(
            name,
            program,
            |dir| {
                if let Some(server) = server {
                    fake_x_server(&dir.join("X0"), server);
                }
            },
            &[
                ("DISPLAY", &display),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        let expected =
            format!("{first}\ninvalid window size: 0x10\ninvalid window size: 40000x1\n");
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
    }
}

const APP_PROGRAM: &str = r#"use gui

struct typist:
    typed: str
    ticks: int
    size: tuple[int, int]

impl gui.app for typist:
    fn update(self, e: gui.event):
        match e:
            case gui.event.text(t):
                self.typed += t
            case gui.event.tick(seconds):
                if seconds >= 0.0:
                    self.ticks += 1
            case gui.event.resize(width, height):
                self.size = (width, height)
            case gui.event.close:
                print("close")
            case _:
                self.ticks += 0

    fn draw(self, c: gui.canvas) -> gui.canvas:
        c.fill(gui.rgb(1, 2, 3))
        c.text(0, 0, self.typed, gui.rgb(255, 255, 255))
        ret c

    // ten seconds of turns at most, so a broken run fails rather than waits forever
    fn done(self) -> bool:
        ret DONE or self.ticks > 600

match gui.run("app", 40, 30, typist(typed="", ticks=0, size=(40, 30))):
    case ok(state):
        print(state.typed, state.ticks > 0, state.size)
    case err(reason):
        print(reason)
"#;

#[test]
fn both_backends_run_an_app_alike() {
    // the server sends a new size and `a` after the first frame, `A` after the first of the new
    // size, so after `draw` has a canvas of that size, and the close button after the next
    let cases = [
        // done once `A` is typed, before the frame that would show it
        (
            "gui_app_done",
            "self.typed.endswith(\"A\")",
            "aA true (400, 300)\n",
        ),
        ("gui_app_closed", "false", "close\naA true (400, 300)\n"),
    ];
    for (name, done, expected) in cases {
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Paced,
            short: false,
            shm: FakeShm::Absent,
            present: false,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let (_, [run, built]) = run_and_build_env(
            name,
            &APP_PROGRAM.replace("DONE", done),
            |dir| {
                fake_x_server(&dir.join("X0"), display);
            },
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
    }
}

#[test]
fn both_backends_present_frames_alike() {
    let display = FakeDisplay {
        msb: false,
        depth: 24,
        bits: 32,
        max_request: 65535,
        refuse: None,
        script: FakeScript::Paced,
        short: false,
        shm: FakeShm::Absent,
        present: true,
    };
    let name = "gui_present";
    let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
    let mut connections = None;
    let (_, [run, built]) = run_and_build_env(
        name,
        &APP_PROGRAM.replace("DONE", "false"),
        |dir| connections = Some(fake_x_server(&dir.join("X0"), display)),
        &[
            ("DISPLAY", &socket),
            ("WAYLAND_DISPLAY", ""),
            ("XAUTHORITY", "missing"),
        ],
    );
    for output in [&run, &built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "close\naA true (400, 300)\n"
        );
    }
    let seen = closed_connections(&connections.unwrap());
    let [interpreted, compiled] = &seen[..] else {
        panic!("expected two connections, not {}", seen.len());
    };
    assert_eq!(interpreted.requests, compiled.requests);
    let requests = &interpreted.requests;
    let word = |r: &[u8], at: usize| u32::from_le_bytes(r[at..at + 4].try_into().unwrap());
    let half = |r: &[u8], at: usize| u16::from_le_bytes([r[at], r[at + 1]]);
    let window = requests
        .iter()
        .find(|r| r[0] == 1)
        .map(|r| word(r, 4))
        .unwrap();
    // completion and idleness asked for once, on the window
    let selects: Vec<_> = requests
        .iter()
        .filter(|r| r[0] == FAKE_PRESENT && r[1] == 3)
        .collect();
    assert_eq!(selects.len(), 1);
    assert_eq!((word(selects[0], 8), word(selects[0], 12)), (window, 6));
    // two pixmaps of the first size, then two of the new size in place of them
    let created: Vec<_> = requests
        .iter()
        .filter(|r| r[0] == 53)
        .map(|r| (word(r, 4), half(r, 12), half(r, 14)))
        .collect();
    let sizes: Vec<_> = created.iter().map(|&(_, w, h)| (w, h)).collect();
    assert_eq!(sizes, [(40, 30), (40, 30), (400, 300), (400, 300)]);
    assert_eq!(requests.iter().filter(|r| r[0] == 54).count(), 2);
    let pixmaps: Vec<u32> = created.iter().map(|&(id, ..)| id).collect();
    // every image goes into a pixmap, which is presented next with the next serial
    let mut drawn = None;
    let mut serials = vec![];
    for request in requests {
        if request[0] == 72 {
            assert!(
                pixmaps.contains(&word(request, 4)),
                "PutImage into a pixmap"
            );
            drawn = Some(word(request, 4));
        } else if request[0] == FAKE_PRESENT && request[1] == 1 {
            assert_eq!(request.len(), 72);
            assert_eq!(word(request, 4), window);
            assert_eq!(
                Some(word(request, 8)),
                drawn.take(),
                "the pixmap just drawn"
            );
            serials.push(word(request, 12));
            // the next vblank: target_msc 0, divisor 1, remainder 0
            assert_eq!(
                &request[48..72],
                &[[0; 8], [1, 0, 0, 0, 0, 0, 0, 0], [0; 8]].concat()
            );
        }
    }
    assert!(serials.len() >= 3, "{serials:?}");
    assert_eq!(serials, (1..=serials.len() as u32).collect::<Vec<_>>());
}

/// Starts `Xvfb` on a free display, if it is on PATH, returning it and the display's number.
fn start_xvfb() -> Option<(std::process::Child, String)> {
    use std::io::BufRead;
    let mut server = Command::new("Xvfb")
        .args([
            "-displayfd",
            "1",
            "-nolisten",
            "tcp",
            "-screen",
            "0",
            "320x240x24",
        ])
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;
    // it writes the display's number once it is ready
    let mut number = String::new();
    std::io::BufReader::new(server.stdout.take()?)
        .read_line(&mut number)
        .ok()?;
    match number.trim() {
        "" => {
            let _ = server.kill();
            None
        }
        n => Some((server, n.to_string())),
    }
}

#[test]
fn both_backends_draw_on_a_real_x_server() {
    let Some((mut server, number)) = start_xvfb() else {
        eprintln!("skipping: Xvfb is not on PATH");
        return;
    };
    let source = r#"use gui
use time

match gui.open("real", 64, 48):
    case ok(w):
        c = gui.blank(64, 48, gui.rgb(10, 20, 30))
        c.text(2, 2, "hi", gui.rgb(255, 255, 255))
        w.show(c)
        w.set_title("still real")
        // frames paced by the server, through Present where it has it
        es = []
        for _ in range(5):
            found = w.next_frame()
            es.extend(found)
            w.show(c)
        // errors come back on their own time
        time.sleep(0.3)
        found = w.events()
        es.extend(found)
        failures = 0
        for e in es:
            match e:
                case gui.event.failed(reason):
                    print(reason)
                    failures += 1
                case _:
                    cont
        print(w.is_open(), failures)
        w.close()
    case err(reason):
        print(reason)
"#;
    let display = format!(":{number}");
    let (_, [run, built]) = run_and_build_env(
        "gui_xvfb",
        source,
        |_| {},
        &[
            ("DISPLAY", &display),
            ("STONE_GUI", "x11"),
            ("XAUTHORITY", "missing"),
        ],
    );
    let _ = server.kill();
    let _ = server.wait();
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "true 0\n");
    }
}

#[test]
#[cfg(target_os = "linux")]
fn both_backends_show_images_through_shared_memory_alike() {
    let source = r#"use gui

match gui.open("shm", 40, 30):
    case ok(w):
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        c.rect(5, 5, 10, 10, gui.rgb(250, 128, 7))
        w.show(c)
        // drawn while the server may still be showing the first
        c.rect(0, 0, 2, 2, gui.rgb(255, 255, 255))
        w.show(c)
        // bigger than the memory shared so far
        w.show(gui.blank(60, 50, gui.rgb(9, 9, 9)))
        es = w.events()
        print(es)
        w.close()
    case err(reason):
        print(reason)
"#;
    for (shm, present) in [
        (FakeShm::Working, false),
        (FakeShm::Refusing, false),
        (FakeShm::Working, true),
    ] {
        let name = match (shm, present) {
            (FakeShm::Working, false) => "gui_shm",
            (FakeShm::Working, true) => "gui_shm_present",
            _ => "gui_shm_refused",
        };
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Expose,
            short: false,
            shm,
            present,
        };
        let socket = format!("{}/{name}/X0:0", env!("CARGO_TARGET_TMPDIR"));
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            name,
            source,
            |dir| connections = Some(fake_x_server(&dir.join("X0"), display)),
            &[
                ("DISPLAY", &socket),
                ("WAYLAND_DISPLAY", ""),
                ("XAUTHORITY", "missing"),
            ],
        );
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                "[event.redraw]\n",
                "{name}"
            );
        }
        let seen = closed_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_same_requests(&interpreted.requests, &compiled.requests, name);
        assert_eq!(interpreted.shm_images, compiled.shm_images, "{name}");
        let count = |op: u8, minor: u8| {
            let requests = &interpreted.requests;
            requests
                .iter()
                .filter(|r| r[0] == op && r[1] == minor)
                .count()
        };
        let put_images = interpreted.requests.iter().filter(|r| r[0] == 72).count();
        let images = &interpreted.shm_images;
        match shm {
            FakeShm::Working => {
                // attached for the first image, then again, bigger, for the last
                assert_eq!((count(130, 6), count(130, 2), count(130, 3)), (2, 1, 3));
                assert_eq!(put_images, 0);
                let pixel = |image: &[u8], x: usize, y: usize| {
                    image[(y * 40 + x) * 4..(y * 40 + x) * 4 + 4].to_vec()
                };
                assert_eq!(images.len(), 3);
                assert_eq!(pixel(&images[0], 0, 0), [3, 2, 1, 0]);
                assert_eq!(pixel(&images[0], 5, 5), [7, 128, 250, 0]);
                assert_eq!(pixel(&images[1], 0, 0), [255, 255, 255, 0]);
                assert_eq!(images[2], [9, 9, 9, 0].repeat(60 * 50));
                // with Present, each image goes into a pixmap that is then presented
                let window =
                    interpreted.requests.iter().find(|r| r[0] == 1).unwrap()[4..8].to_vec();
                let drawables: Vec<_> = interpreted
                    .requests
                    .iter()
                    .filter(|r| r[0] == 130 && r[1] == 3)
                    .map(|r| r[4..8].to_vec())
                    .collect();
                assert_eq!(
                    drawables.iter().filter(|d| **d == window).count(),
                    if present { 0 } else { 3 }
                );
                assert_eq!(count(FAKE_PRESENT, 1), if present { 3 } else { 0 });
                assert_eq!(count(53, 24), if present { 4 } else { 0 });
            }
            _ => {
                // the first attachment fails, and every image goes the plain way
                assert_eq!((count(130, 6), count(130, 3)), (1, 0));
                assert_eq!(put_images, 3);
                assert!(images.is_empty());
            }
        }
    }
}

/// How a fake Wayland compositor of [`fake_wayland_server`] behaves.
#[derive(Clone, Copy)]
struct FakeCompositor {
    /// Whether it offers `zxdg_decoration_manager_v1`, and so draws windows' frames itself.
    decorations: bool,
    /// What it sends after the app's first frame.
    script: WaylandScript,
    /// The keymap in `tests/keymaps` it sends the keyboard, and its repeats a second.
    keymap: &'static str,
    rate: u32,
    /// Whether it answers frame callbacks, as it does for a window on the screen.
    frames: bool,
}

/// What a fake compositor sends after the app's first frame.
#[derive(Clone, Copy, PartialEq)]
enum WaylandScript {
    /// A ping and a new size, then the close button after a frame of that size.
    Resize,
    /// Keys, the pointer on the content, then presses on our frame's edge, title, and close
    /// button.
    Input,
    /// A key held down.
    Hold,
}

/// Sends `data` on `stream` with the descriptor of `file`, as a compositor sends a keymap.
fn send_with_fd(
    stream: &std::os::unix::net::UnixStream,
    data: &[u8],
    file: &std::fs::File,
) -> std::io::Result<()> {
    use std::os::fd::AsRawFd;
    unsafe extern "C" {
        fn sendmsg(fd: i32, msg: *const MsgHdr, flags: i32) -> isize;
    }
    // a cmsghdr of 20 bytes, SOL_SOCKET, and SCM_RIGHTS, then the descriptor
    let mut control = [20u64, 1 | 1 << 32, file.as_raw_fd() as u64];
    let mut iov = IoVec {
        base: data.as_ptr().cast_mut(),
        len: data.len(),
    };
    let msg = MsgHdr {
        name: std::ptr::null_mut(),
        namelen: 0,
        iov: &mut iov,
        iovlen: 1,
        control: control.as_mut_ptr().cast(),
        controllen: 24,
        flags: 0,
    };
    // SAFETY: msg points at one iovec over `data` and 24 bytes of control, which outlive it
    match unsafe { sendmsg(stream.as_raw_fd(), &msg, 0) } {
        n if n as usize == data.len() => Ok(()),
        _ => Err(std::io::Error::last_os_error()),
    }
}

/// The 32-bit word of the fixed-point (24.8) number `value`.
fn fixed(value: f64) -> u32 {
    (value * 256.0) as i32 as u32
}

/// What the fake compositor saw on one connection: every request, as its bytes, and every
/// buffer committed, by the role of its surface (`frame`, `content`, or `cursor`), with its
/// width, height, and pixels (4 bytes each by row).
#[derive(Default, Debug)]
struct WaylandConnection {
    requests: Vec<Vec<u8>>,
    commits: Vec<(String, u32, u32, Vec<u8>)>,
    /// Whether the client has closed the connection.
    closed: bool,
}

/// The globals the fake compositor offers: name, interface, and version.
fn fake_globals(compositor: FakeCompositor) -> Vec<(u32, &'static str, u32)> {
    let mut globals = vec![
        (1, "wl_compositor", 5),
        (2, "wl_subcompositor", 1),
        (3, "wl_shm", 1),
        (4, "xdg_wm_base", 2),
        (5, "wl_seat", 8),
        (6, "wl_output", 3),
    ];
    if compositor.decorations {
        globals.push((7, "zxdg_decoration_manager_v1", 1));
    }
    globals
}

/// A Wayland message to `object` of `opcode` with the 32-bit arguments `args`.
fn wayland_message(object: u32, opcode: u16, args: &[u32]) -> Vec<u8> {
    let mut out = object.to_le_bytes().to_vec();
    out.extend((((8 + 4 * args.len()) as u32) << 16 | opcode as u32).to_le_bytes());
    for arg in args {
        out.extend(arg.to_le_bytes());
    }
    out
}

/// The 32-bit words of a Wayland string argument holding `text`: its length with a 0 after it,
/// then its bytes, padded to a multiple of four.
fn wayland_string(text: &str) -> Vec<u32> {
    let mut bytes = text.as_bytes().to_vec();
    bytes.push(0);
    let mut words = vec![bytes.len() as u32];
    bytes.resize(bytes.len().div_ceil(4) * 4, 0);
    words.extend(
        bytes
            .chunks(4)
            .map(|c| u32::from_le_bytes(c.try_into().unwrap())),
    );
    words
}

/// What the fake compositor knows of one connection's objects.
#[derive(Default)]
struct FakeObjects {
    /// The interface of each object, by id.
    kinds: std::collections::HashMap<u32, String>,
    /// Each pool's file, by id, and each buffer's pool, offset, width, height, and stride.
    pools: std::collections::HashMap<u32, std::fs::File>,
    buffers: std::collections::HashMap<u32, (u32, u32, u32, u32, u32)>,
    /// The role of each surface, the buffer attached to it, and the one it last committed.
    roles: std::collections::HashMap<u32, String>,
    attached: std::collections::HashMap<u32, u32>,
    shown: std::collections::HashMap<u32, u32>,
    toplevel: u32,
    xdg_surface: u32,
    decoration: u32,
    pointer: u32,
    keyboard: u32,
    /// The frame callbacks each surface asked for since its last commit.
    callbacks: std::collections::HashMap<u32, Vec<u32>>,
    /// How many content buffers were committed, and how far the script has gone.
    content_commits: usize,
    stage: usize,
}

/// Answers one request of a fake compositor, `message` to `object` of `opcode`, recording what
/// it shows in `seen`.
fn fake_wayland_request(
    stream: &mut std::os::unix::net::UnixStream,
    compositor: FakeCompositor,
    message: &[u8],
    fds: &mut Vec<std::fs::File>,
    objects: &mut FakeObjects,
    seen: &std::sync::Mutex<WaylandConnection>,
) -> std::io::Result<()> {
    use std::io::Write;
    let object = u32::from_le_bytes(message[0..4].try_into().unwrap());
    let opcode = u16::from_le_bytes([message[4], message[5]]);
    let arg = |i: usize| u32::from_le_bytes(message[8 + 4 * i..12 + 4 * i].try_into().unwrap());
    let kind = objects.kinds.get(&object).cloned().unwrap_or_default();
    let new = |objects: &mut FakeObjects, id: u32, kind: &str| {
        objects.kinds.insert(id, kind.to_string());
    };
    match (kind.as_str(), opcode) {
        ("wl_display", 0) => {
            // sync: done at once
            stream.write_all(&wayland_message(arg(0), 0, &[1]))?;
            stream.write_all(&wayland_message(1, 1, &[arg(0)]))?;
        }
        ("wl_display", 1) => {
            new(objects, arg(0), "wl_registry");
            for (name, interface, version) in fake_globals(compositor) {
                let mut args = vec![name];
                args.extend(wayland_string(interface));
                args.push(version);
                stream.write_all(&wayland_message(arg(0), 0, &args))?;
            }
        }
        ("wl_registry", 0) => {
            // bind(name, interface, version, id)
            let len = arg(1) as usize;
            let interface = std::str::from_utf8(&message[16..16 + len - 1])
                .unwrap()
                .to_string();
            let after = 3 + len.div_ceil(4);
            let id = arg(after);
            new(objects, id, &interface);
            if interface == "wl_seat" {
                // a pointer and a keyboard
                stream.write_all(&wayland_message(id, 0, &[3]))?;
            }
        }
        ("wl_compositor", 0) => new(objects, arg(0), "wl_surface"),
        ("wl_subcompositor", 1) => {
            new(objects, arg(0), "wl_subsurface");
            objects.roles.insert(arg(1), "content".to_string());
        }
        ("wl_shm", 0) => {
            new(objects, arg(0), "wl_shm_pool");
            objects.pools.insert(arg(0), fds.remove(0));
        }
        ("wl_shm_pool", 0) => {
            new(objects, arg(0), "wl_buffer");
            objects
                .buffers
                .insert(arg(0), (object, arg(1), arg(2), arg(3), arg(4)));
        }
        ("xdg_wm_base", 2) => {
            new(objects, arg(0), "xdg_surface");
            objects.xdg_surface = arg(0);
            objects
                .roles
                .entry(arg(1))
                .or_insert_with(|| "frame".to_string());
        }
        ("xdg_surface", 1) => {
            new(objects, arg(0), "xdg_toplevel");
            objects.toplevel = arg(0);
        }
        ("zxdg_decoration_manager_v1", 1) => {
            new(objects, arg(0), "zxdg_toplevel_decoration_v1");
            objects.decoration = arg(0);
        }
        ("wl_seat", 0) => {
            new(objects, arg(0), "wl_pointer");
            objects.pointer = arg(0);
        }
        ("wl_seat", 1) => {
            new(objects, arg(0), "wl_keyboard");
            objects.keyboard = arg(0);
            // the keymap, with a 0 after it as xkbcommon writes it, and the repeat rate
            let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/keymaps");
            let mut text = std::fs::read(path.join(compositor.keymap))?;
            text.push(0);
            let file = std::env::temp_dir().join(format!(
                "stone-keymap-{}-{}",
                std::process::id(),
                arg(0)
            ));
            std::fs::write(&file, &text)?;
            let opened = std::fs::File::open(&file)?;
            std::fs::remove_file(&file)?;
            send_with_fd(
                stream,
                &wayland_message(arg(0), 0, &[1, text.len() as u32]),
                &opened,
            )?;
            stream.write_all(&wayland_message(arg(0), 5, &[compositor.rate, 100]))?;
        }
        ("wl_pointer", 0) => {
            objects.roles.insert(arg(1), "cursor".to_string());
        }
        ("wl_surface", 1) => {
            objects.attached.insert(object, arg(0));
        }
        ("wl_surface", 3) => {
            new(objects, arg(0), "wl_callback");
            objects.callbacks.entry(object).or_default().push(arg(0));
        }
        ("wl_surface", 6) => {
            fake_commit(stream, compositor, object, objects, seen)?;
            if compositor.frames {
                // shown at once, after whatever the commit made the compositor send: each
                // callback done, then its id free again
                for callback in objects.callbacks.remove(&object).unwrap_or_default() {
                    objects.kinds.remove(&callback);
                    let mut out = wayland_message(callback, 0, &[16]);
                    out.extend(wayland_message(1, 1, &[callback]));
                    stream.write_all(&out)?;
                }
            }
        }
        _ => {}
    }
    Ok(())
}

/// Acts on a commit of the surface `object`, recording what it shows in `seen` and moving the
/// script on.
fn fake_commit(
    stream: &mut std::os::unix::net::UnixStream,
    compositor: FakeCompositor,
    object: u32,
    objects: &mut FakeObjects,
    seen: &std::sync::Mutex<WaylandConnection>,
) -> std::io::Result<()> {
    use std::io::Write;
    use std::os::unix::fs::FileExt;
    let role = objects.roles.get(&object).cloned().unwrap_or_default();
    match objects.attached.remove(&object) {
        Some(buffer) if buffer != 0 => {
            let (pool, offset, width, height, stride) = objects.buffers[&buffer];
            let mut pixels = vec![];
            for row in 0..height {
                let mut line = vec![0; width as usize * 4];
                let at = (offset + row * stride) as u64;
                objects.pools[&pool].read_exact_at(&mut line, at)?;
                pixels.extend(line);
            }
            seen.lock()
                .unwrap()
                .commits
                .push((role.clone(), width, height, pixels));
            let old = objects.shown.insert(object, buffer);
            if let Some(old) = old.filter(|old| *old != buffer) {
                stream.write_all(&wayland_message(old, 0, &[]))?;
            }
            if role == "content" || (role == "frame" && compositor.decorations) {
                objects.content_commits += 1;
                match compositor.script {
                    WaylandScript::Resize => fake_wayland_script(stream, objects, width)?,
                    script if objects.content_commits == 2 => {
                        stream.write_all(&fake_input(objects, script))?;
                    }
                    _ => {}
                }
            }
        }
        _ if role == "frame" && objects.stage == 0 => {
            // the first commit, with no buffer: the first configure
            objects.stage = 1;
            if objects.decoration != 0 {
                // server_side
                stream.write_all(&wayland_message(objects.decoration, 0, &[2]))?;
            }
            // no size, and the activated state (an array of 4 bytes)
            stream.write_all(&wayland_message(objects.toplevel, 0, &[0, 0, 4, 4]))?;
            stream.write_all(&wayland_message(objects.xdg_surface, 0, &[10]))?;
        }
        _ => {}
    }
    Ok(())
}

/// Moves the fake compositor's script on after a content buffer `width` pixels wide was
/// committed: after the app's first frame (the second, after the one `open` shows), a ping and a
/// new size of 300 by 200; after a frame of that width, the close button.
fn fake_wayland_script(
    stream: &mut std::os::unix::net::UnixStream,
    objects: &mut FakeObjects,
    width: u32,
) -> std::io::Result<()> {
    use std::io::Write;
    if objects.stage == 1 && objects.content_commits == 2 {
        objects.stage = 2;
        let mut out = wayland_message(objects.xdg_wm_base_id(), 0, &[77]);
        out.extend(wayland_message(objects.toplevel, 0, &[300, 200, 4, 4]));
        out.extend(wayland_message(objects.xdg_surface, 0, &[11]));
        stream.write_all(&out)?;
    } else if objects.stage == 2 && width == 300 {
        objects.stage = 3;
        stream.write_all(&wayland_message(objects.toplevel, 1, &[]))?;
    }
    Ok(())
}

/// The events of the script `script` (`Input` or `Hold`) for the window `objects` knows, its
/// content 40 by 30 in our frame.
fn fake_input(objects: &FakeObjects, script: WaylandScript) -> Vec<u8> {
    let (keyboard, pointer) = (objects.keyboard, objects.pointer);
    let surface_of = |role: &str| {
        objects
            .roles
            .iter()
            .find(|(_, r)| *r == role)
            .map_or(0, |(id, _)| *id)
    };
    let (content, frame) = (surface_of("content"), surface_of("frame"));
    let key = |code: u32, down: bool| wayland_message(keyboard, 3, &[9, 0, code, down as u32]);
    let mods =
        |depressed: u32, locked: u32| wayland_message(keyboard, 4, &[9, depressed, 0, locked, 0]);
    let mut out = wayland_message(keyboard, 1, &[9, frame, 0]);
    if script == WaylandScript::Hold {
        // `a` held down
        out.extend(key(30, true));
        return out;
    }
    // evdev codes: 30 a, 42 left Shift, 39 the key right of l, 44 the key right of left Shift,
    // and 1 Escape
    for part in [
        mods(0, 0),
        key(30, true),
        key(30, false),
        key(42, true),
        mods(1, 0),
        key(30, true),
        key(30, false),
        mods(0, 0),
        key(42, false),
        mods(0, 2),
        key(39, true),
        key(44, true),
        mods(0, 0),
        key(1, true),
        mods(4, 0),
        key(30, true),
        mods(0, 0),
    ] {
        out.extend(part);
    }
    // the pointer on the content: a move, a press and a release, a wheel step with its value,
    // and two steps' worth of a value alone
    for part in [
        wayland_message(pointer, 0, &[10, content, fixed(5.0), fixed(6.0)]),
        wayland_message(pointer, 2, &[0, fixed(10.5), fixed(20.25)]),
        wayland_message(pointer, 3, &[11, 0, 272, 1]),
        wayland_message(pointer, 3, &[12, 0, 273, 0]),
        wayland_message(pointer, 8, &[0, 1]),
        wayland_message(pointer, 4, &[0, 0, fixed(10.0)]),
        wayland_message(pointer, 5, &[]),
        wayland_message(pointer, 4, &[0, 0, fixed(-20.0)]),
        wayland_message(pointer, 5, &[]),
        wayland_message(pointer, 1, &[13, content]),
    ] {
        out.extend(part);
    }
    // our frame, 48 by 62: a press on its top left corner, then on the title bar, then on the
    // close button
    for (x, y, serial) in [(1.0, 1.0, 21), (10.0, 10.0, 22), (40.0, 10.0, 23)] {
        out.extend(wayland_message(
            pointer,
            0,
            &[serial, frame, fixed(x), fixed(y)],
        ));
        out.extend(wayland_message(pointer, 3, &[serial, 0, 272, 1]));
        out.extend(wayland_message(pointer, 3, &[serial, 0, 272, 0]));
        out.extend(wayland_message(pointer, 1, &[serial, frame]));
    }
    out
}

impl FakeObjects {
    /// The id the client bound `xdg_wm_base` to.
    fn xdg_wm_base_id(&self) -> u32 {
        self.kinds
            .iter()
            .find(|(_, kind)| *kind == "xdg_wm_base")
            .map_or(0, |(id, _)| *id)
    }
}

/// Starts a fake Wayland compositor behaving as `compositor`, listening on the Unix socket
/// `path`, and returns what it sees on each connection, in order.
fn fake_wayland_server(
    path: &Path,
    compositor: FakeCompositor,
) -> std::sync::Arc<std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<WaylandConnection>>>>> {
    use std::sync::{Arc, Mutex};
    let _ = std::fs::remove_file(path);
    let listener = std::os::unix::net::UnixListener::bind(path).unwrap();
    let connections: Arc<Mutex<Vec<Arc<Mutex<WaylandConnection>>>>> = Arc::default();
    let all = connections.clone();
    std::thread::spawn(move || {
        for mut stream in listener.incoming().flatten() {
            let seen = Arc::new(Mutex::new(WaylandConnection::default()));
            all.lock().unwrap().push(seen.clone());
            std::thread::spawn(move || {
                let mut objects = FakeObjects::default();
                objects.kinds.insert(1, "wl_display".to_string());
                let mut fds = vec![];
                loop {
                    let mut head = [0; 8];
                    if receive(&mut stream, &mut head, &mut fds).is_err() {
                        break;
                    }
                    let size = u16::from_le_bytes([head[6], head[7]]) as usize;
                    let mut message = head.to_vec();
                    message.resize(size.max(8), 0);
                    if receive(&mut stream, &mut message[8..], &mut fds).is_err() {
                        break;
                    }
                    seen.lock().unwrap().requests.push(message.clone());
                    // a client that closed may have sent more already, which is still read
                    let _ = fake_wayland_request(
                        &mut stream,
                        compositor,
                        &message,
                        &mut fds,
                        &mut objects,
                        &seen,
                    );
                }
                seen.lock().unwrap().closed = true;
            });
        }
    });
    connections
}

/// Asserts that two backends sent the same requests, naming the first that differs.
fn assert_same_requests(interpreted: &[Vec<u8>], compiled: &[Vec<u8>], name: &str) {
    for (i, (a, b)) in interpreted.iter().zip(compiled).enumerate() {
        assert_eq!(a, b, "{name}: request {i} differs");
    }
    assert_eq!(
        interpreted.len(),
        compiled.len(),
        "{name}: request counts differ, the longer ending {:?}",
        if interpreted.len() > compiled.len() {
            interpreted.last()
        } else {
            compiled.last()
        }
    );
}

/// Waits until the client of each connection the fake compositor has seen has closed it, and
/// returns them.
fn closed_wayland_connections(
    connections: &std::sync::Mutex<Vec<std::sync::Arc<std::sync::Mutex<WaylandConnection>>>>,
) -> Vec<WaylandConnection> {
    for _ in 0..500 {
        let all = connections.lock().unwrap();
        if all.iter().all(|c| c.lock().unwrap().closed) {
            return all
                .iter()
                .map(|c| std::mem::take(&mut *c.lock().unwrap()))
                .collect();
        }
        drop(all);
        std::thread::sleep(std::time::Duration::from_millis(10));
    }
    panic!("a client never closed its connection");
}

const WAYLAND_PROGRAM: &str = r#"use gui

match gui.open("way", 40, 30):
    case ok(w):
        print(w.size())
        c = gui.blank(40, 30, gui.rgb(1, 2, 3))
        done = false
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                match e:
                    case gui.event.redraw:
                        w.show(c)
                    case gui.event.resize(width, height):
                        c = gui.blank(width, height, gui.rgb(4, 5, 6))
                        w.show(c)
                    case gui.event.close:
                        done = true
                    case _:
                        cont
        w.close()
    case err(reason):
        print(reason)
"#;

#[test]
#[cfg(target_os = "linux")]
fn both_backends_drive_a_wayland_window_alike() {
    for decorations in [false, true] {
        let name = match decorations {
            false => "gui_wayland_ours",
            true => "gui_wayland_server",
        };
        let compositor = FakeCompositor {
            decorations,
            script: WaylandScript::Resize,
            keymap: "us.xkb",
            rate: 0,
            frames: true,
        };
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            name,
            WAYLAND_PROGRAM,
            |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
            &[
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", ""),
            ],
        );
        // with our own frame, the title bar is part of the 300 by 200 the compositor asked for
        let height = if decorations { 200 } else { 176 };
        let expected =
            format!("(40, 30)\nevent.redraw\nevent.resize(300, {height})\nevent.close\n");
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
        let seen = closed_wayland_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_same_requests(&interpreted.requests, &compiled.requests, name);
        assert_eq!(interpreted.commits, compiled.commits, "{name}");
        let content: Vec<_> = interpreted
            .commits
            .iter()
            .filter(|(role, ..)| *role == if decorations { "frame" } else { "content" })
            .collect();
        // black when it opens, then the app's two frames
        let sizes: Vec<_> = content.iter().map(|(_, w, h, _)| (*w, *h)).collect();
        assert_eq!(sizes, [(40, 30), (40, 30), (300, height)], "{name}");
        assert_eq!(content[0].3, [0, 0, 0, 0].repeat(40 * 30));
        assert_eq!(content[1].3, [3, 2, 1, 0].repeat(40 * 30));
        assert_eq!(content[2].3, [6, 5, 4, 0].repeat(300 * height as usize));
        let frames: Vec<_> = interpreted
            .commits
            .iter()
            .filter(|(role, ..)| *role == "frame")
            .collect();
        if !decorations {
            // our frame: a transparent border of 4 around a title bar of 24 and the content
            let (_, width, height, pixels) = frames.last().unwrap();
            assert_eq!((*width, *height), (308, 208));
            let pixel = |x: u32, y: u32| pixels[((y * width + x) * 4) as usize..][..4].to_vec();
            assert_eq!(pixel(0, 0)[3], 0);
            assert_eq!(pixel(10, 10)[3], 255);
        }
    }
}

#[test]
fn both_backends_choose_a_display_alike() {
    let program = "use gui\n\
                   match gui.open(\"x\", 10, 10):\n    \
                   case ok(w):\n        \
                   print(\"opened\", w.size())\n    \
                   case err(reason):\n        \
                   print(reason)\n";
    let base = env!("CARGO_TARGET_TMPDIR");
    let x_server = format!("{base}/gui_choice_fallback/X0:0");
    // each case's name, its environment, and what it prints
    type Case<'a> = (&'a str, Vec<(&'a str, &'a str)>, &'a str);
    let cases: [Case; 7] = [
        (
            "gui_choice_none",
            vec![("DISPLAY", ""), ("WAYLAND_DISPLAY", ""), ("STONE_GUI", "")],
            "no display: neither WAYLAND_DISPLAY nor DISPLAY is set",
        ),
        (
            "gui_choice_bad",
            vec![("STONE_GUI", "metal")],
            "STONE_GUI is 'metal', but can only be 'wayland' or 'x11'",
        ),
        (
            "gui_choice_wayland",
            vec![("STONE_GUI", "wayland"), ("WAYLAND_DISPLAY", "")],
            "no display: WAYLAND_DISPLAY is not set",
        ),
        (
            "gui_choice_x11",
            vec![("STONE_GUI", "x11"), ("DISPLAY", "")],
            "no display: DISPLAY is not set",
        ),
        (
            "gui_choice_no_runtime",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", ""),
                ("DISPLAY", ""),
            ],
            "no display: XDG_RUNTIME_DIR is not set",
        ),
        (
            "gui_choice_no_socket",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", ""),
            ],
            "no such file or directory: './wayland-9'",
        ),
        // a compositor that is not there, then the X server
        (
            "gui_choice_fallback",
            vec![
                ("STONE_GUI", ""),
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("DISPLAY", &x_server),
                ("XAUTHORITY", "missing"),
            ],
            "opened (10, 10)",
        ),
    ];
    for (name, env, expected) in cases {
        let display = FakeDisplay {
            msb: false,
            depth: 24,
            bits: 32,
            max_request: 65535,
            refuse: None,
            script: FakeScript::Expose,
            short: false,
            shm: FakeShm::Absent,
            present: false,
        };
        let (_, [run, built]) = run_and_build_env(
            name,
            program,
            |dir| {
                fake_x_server(&dir.join("X0"), display);
            },
            &env,
        );
        for output in [run, built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(
                String::from_utf8_lossy(&output.stdout),
                format!("{expected}\n"),
                "{name}"
            );
        }
    }
}

const INPUT_PROGRAM: &str = r#"use gui

match gui.open("keys", 40, 30):
    case ok(w):
        done = false
        typed = 0
        while not done:
            es = w.wait()
            for e in es:
                print(e)
                match e:
                    case gui.event.redraw:
                        w.show(gui.blank(40, 30, gui.rgb(1, 2, 3)))
                    case gui.event.text(_):
                        typed += 1
                        done = HOLD and typed == 4
                    case gui.event.close:
                        done = true
                    case _:
                        cont
        w.close()
    case err(reason):
        print(reason)
"#;

#[test]
#[cfg(target_os = "linux")]
fn both_backends_take_wayland_input_alike() {
    let expected = "event.redraw\n\
                    event.key_down('a')\n\
                    event.text('a')\n\
                    event.key_up('a')\n\
                    event.key_down('shift')\n\
                    event.key_down('a')\n\
                    event.text('A')\n\
                    event.key_up('a')\n\
                    event.key_up('shift')\n\
                    event.key_down('\u{f6}')\n\
                    event.text('\u{d6}')\n\
                    event.key_down('y')\n\
                    event.text('Y')\n\
                    event.key_down('escape')\n\
                    event.key_down('a')\n\
                    event.mouse_move(10, 20)\n\
                    event.mouse_down('left', 10, 20)\n\
                    event.mouse_up('right', 10, 20)\n\
                    event.scroll(-1)\n\
                    event.scroll(1)\n\
                    event.scroll(1)\n\
                    event.close\n";
    let compositor = FakeCompositor {
        decorations: false,
        script: WaylandScript::Input,
        keymap: "de.xkb",
        rate: 0,
        frames: true,
    };
    let mut connections = None;
    let (_, [run, built]) = run_and_build_env(
        "gui_wayland_input",
        &INPUT_PROGRAM.replace("HOLD", "false"),
        |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
        &[
            ("WAYLAND_DISPLAY", "wayland-9"),
            ("XDG_RUNTIME_DIR", "."),
            ("STONE_GUI", ""),
        ],
    );
    for output in [&run, &built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
    let seen = closed_wayland_connections(&connections.unwrap());
    let [interpreted, compiled] = &seen[..] else {
        panic!("expected two connections, not {}", seen.len());
    };
    assert_same_requests(&interpreted.requests, &compiled.requests, "input");
    // xdg_toplevel's resize from the top left corner (5) and move, each with its press's serial
    let toplevel_requests: Vec<_> = interpreted
        .requests
        .iter()
        .filter(|r| r.len() >= 16 && (r[4] == 5 || r[4] == 6) && r[6] as usize == r.len())
        .map(|r| {
            r[8..]
                .chunks(4)
                .map(|w| u32::from_le_bytes(w.try_into().unwrap()))
                .collect::<Vec<_>>()
        })
        .filter(|args: &Vec<u32>| args.len() >= 2 && (args[1] == 21 || args[1] == 22))
        .collect();
    assert_eq!(toplevel_requests.len(), 2, "{toplevel_requests:?}");
    assert_eq!(toplevel_requests[0][1..], [21, 5]);
    assert_eq!(toplevel_requests[1][1..], [22]);
}

#[test]
#[cfg(target_os = "linux")]
fn a_key_held_down_on_wayland_repeats_in_both_backends() {
    let compositor = FakeCompositor {
        decorations: false,
        script: WaylandScript::Hold,
        keymap: "us.xkb",
        rate: 50,
        frames: true,
    };
    let (_, [run, built]) = run_and_build_env(
        "gui_wayland_hold",
        &INPUT_PROGRAM.replace("HOLD", "true"),
        |dir| {
            fake_wayland_server(&dir.join("wayland-9"), compositor);
        },
        &[
            ("WAYLAND_DISPLAY", "wayland-9"),
            ("XDG_RUNTIME_DIR", "."),
            ("STONE_GUI", ""),
        ],
    );
    let expected = "event.redraw\nevent.key_down('a')\nevent.text('a')\nevent.text('a')\n\
                    event.text('a')\nevent.text('a')\n";
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), expected);
    }
}

#[test]
#[cfg(target_os = "linux")]
fn both_backends_run_an_app_on_wayland_alike() {
    let compositor = FakeCompositor {
        decorations: false,
        script: WaylandScript::Resize,
        keymap: "us.xkb",
        rate: 0,
        frames: true,
    };
    let mut connections = None;
    let (_, [run, built]) = run_and_build_env(
        "gui_wayland_app",
        &APP_PROGRAM.replace("DONE", "false"),
        |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
        &[
            ("WAYLAND_DISPLAY", "wayland-9"),
            ("XDG_RUNTIME_DIR", "."),
            ("STONE_GUI", ""),
        ],
    );
    for output in [&run, &built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "close\n true (300, 176)\n"
        );
    }
    let seen = closed_wayland_connections(&connections.unwrap());
    let [interpreted, compiled] = &seen[..] else {
        panic!("expected two connections, not {}", seen.len());
    };
    // the frames after the first that `next_frame` paced ask for a frame callback; the requests
    // themselves differ by when the first frame, paced by time, read the compositor's ping
    for seen in [interpreted, compiled] {
        assert!(!frame_callbacks(&seen.requests).is_empty());
    }
}

/// The id of each frame callback (`wl_surface.frame`) among `requests`.
fn frame_callbacks(requests: &[Vec<u8>]) -> Vec<u32> {
    requests
        .iter()
        .filter(|r| r.len() == 12 && r[4] == 3)
        .map(|r| u32::from_le_bytes(r[8..12].try_into().unwrap()))
        .collect()
}

#[test]
#[cfg(target_os = "linux")]
fn wayland_frames_follow_frame_callbacks_in_both_backends() {
    // four frames: the first shown before any pacing, then three that ask for a callback
    let source = r#"use gui
use time

match gui.open("frames", 40, 30):
    case ok(w):
        c = gui.blank(40, 30, 0)
        waited = 0.0
        for _ in range(4):
            w.show(c)
            start = time.clock()
            es = w.next_frame()
            waited = time.clock() - start
        print(waited >= 0.9 and waited < 5.0, w.is_open())
        w.close()
    case err(reason):
        print(reason)
"#;
    // a compositor that answers callbacks, and one that never does, as for a hidden window,
    // whose frames come once a second
    for (frames, expected, callbacks) in [(true, "false true\n", 3), (false, "true true\n", 1)] {
        let name = format!("gui_wayland_frames_{frames}");
        let compositor = FakeCompositor {
            decorations: false,
            script: WaylandScript::Hold,
            keymap: "us.xkb",
            rate: 0,
            frames,
        };
        let mut connections = None;
        let (_, [run, built]) = run_and_build_env(
            &name,
            source,
            |dir| connections = Some(fake_wayland_server(&dir.join("wayland-9"), compositor)),
            &[
                ("WAYLAND_DISPLAY", "wayland-9"),
                ("XDG_RUNTIME_DIR", "."),
                ("STONE_GUI", ""),
            ],
        );
        for output in [&run, &built] {
            assert!(output.status.success(), "{output:?}");
            assert_eq!(String::from_utf8_lossy(&output.stdout), expected, "{name}");
        }
        let seen = closed_wayland_connections(&connections.unwrap());
        let [interpreted, compiled] = &seen[..] else {
            panic!("expected two connections, not {}", seen.len());
        };
        assert_same_requests(&interpreted.requests, &compiled.requests, &name);
        // one callback outstanding at a time, its id reused once the compositor frees it
        let ids = frame_callbacks(&interpreted.requests);
        assert_eq!(ids.len(), callbacks, "{name}: {ids:?}");
        assert!(ids.iter().all(|id| *id == ids[0]), "{name}: {ids:?}");
    }
}

/// Starts a headless Weston, if `weston` is on PATH, in a runtime directory of its own, returning
/// it, the directory, and its socket's name.
fn start_weston(name: &str) -> Option<(std::process::Child, PathBuf, String)> {
    use std::os::unix::fs::PermissionsExt;
    let runtime = PathBuf::from(env!("CARGO_TARGET_TMPDIR")).join(format!("{name}-runtime"));
    let _ = std::fs::remove_dir_all(&runtime);
    std::fs::create_dir_all(&runtime).ok()?;
    std::fs::set_permissions(&runtime, std::fs::Permissions::from_mode(0o700)).ok()?;
    let socket = "stone-test".to_string();
    let mut server = Command::new("weston")
        .args(["--backend=headless", "--renderer=pixman", "--shell=kiosk"])
        .arg(format!("--socket={socket}"))
        .env("XDG_RUNTIME_DIR", &runtime)
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .spawn()
        .ok()?;
    for _ in 0..100 {
        if runtime.join(&socket).exists() {
            return Some((server, runtime, socket));
        }
        std::thread::sleep(std::time::Duration::from_millis(50));
    }
    let _ = server.kill();
    None
}

#[test]
#[cfg(target_os = "linux")]
fn both_backends_draw_on_a_real_wayland_compositor() {
    let Some((mut server, runtime, socket)) = start_weston("gui_weston") else {
        eprintln!("skipping: weston is not on PATH");
        return;
    };
    let source = r#"use gui
use time

match gui.open("real", 64, 48):
    case ok(w):
        c = gui.blank(64, 48, gui.rgb(10, 20, 30))
        c.text(2, 2, "hi", gui.rgb(255, 255, 255))
        w.show(c)
        w.set_title("still real")
        time.sleep(0.3)
        failures = 0
        for _ in range(3):
            es = w.events()
            for e in es:
                match e:
                    case gui.event.failed(reason):
                        print(reason)
                        failures += 1
                    case gui.event.resize(width, height):
                        c = gui.blank(width, height, gui.rgb(10, 20, 30))
                    case _:
                        cont
            w.show(c)
            es = w.next_frame()
            for e in es:
                match e:
                    case gui.event.failed(reason):
                        print(reason)
                        failures += 1
                    case _:
                        cont
        print(w.is_open(), failures)
        w.close()
    case err(reason):
        print(reason)
"#;
    let runtime = runtime.to_string_lossy().to_string();
    let (_, [run, built]) = run_and_build_env(
        "gui_weston",
        source,
        |_| {},
        &[
            ("STONE_GUI", "wayland"),
            ("WAYLAND_DISPLAY", &socket),
            ("XDG_RUNTIME_DIR", &runtime),
        ],
    );
    let _ = server.kill();
    let _ = server.wait();
    for output in [run, built] {
        assert!(output.status.success(), "{output:?}");
        assert_eq!(String::from_utf8_lossy(&output.stdout), "true 0\n");
    }
}
