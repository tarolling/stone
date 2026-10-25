# Networking

The builtin `net` module connects programs over TCP. `net.connect(host, port)` opens a
connection to a server and `net.listen(host, port)` waits for connections of its own. Both
return a result, since a host can be unknown, a port refused, or a connection cut at any time,
and so does every method of a connection.

## Connections

A connection is a `socket`. `send` writes text to it, and three methods read what arrives:
`read_line()` the next line (`none` once the other end has closed the connection), `read()`
whatever has arrived so far, and `read_all()` everything until the other end closes it.

A server is a `listener`, which `accept()` turns into one `socket` per connection. Giving
`net.listen` port 0 lets the system pick a free port, which `port()` returns. A program is one
thread, but a connection to its own port completes before it accepts it, so a program can play
both parts, which makes the example below easy to run anywhere:

```{literalinclude} ../examples/networking.stn
:language: stone
```

prints

```{literalinclude} ../examples/networking.out
:language: text
```

The client sends all three words before the server reads any, since the system holds what was
sent until it is read. Then `client.read_all()` returns once the server's end closes. Nothing
in `answer` calls `close()`: a socket closes by itself right after the last statement that uses
it, here the loop, so its end is closed by the time `answer` returns.

## Bytes and timeouts

Protocols often mix text and binary data, such as a header line followed by a body of a given
length. A string can only hold text, so binary data is `bytes`, written `b"..."`, whose indexes
are ints. `send_bytes` sends them, and `read_bytes(max)`, `read_exact(n)`, and
`read_all_bytes()` read them, from the same data `read_line` reads, so the two mix freely:

```stone
use net

fn read_message(c: socket) -> result[bytes, str]:
    // a line with the body's length, then the body
    length = c.read_line()?.unwrap_or("0").parse_int()?
    ret c.read_exact(length)
```

By default a read waits as long as it takes. `set_timeout(seconds)` makes each later wait on a
socket or listener give up after that long with `err("timed out")`, and
`net.connect_timeout(host, port, seconds)` connects the same way:

```stone
use net

fn ask(host: str, question: str) -> result[str, str]:
    c = net.connect_timeout(host, 7777, 2.0)?
    c.set_timeout(5.0)
    c.send("{question}\n")?
    ret ok(c.read_line()?.unwrap_or(""))
```

## Hosts and addresses

A host is a name, such as `example.com` or `localhost`, or an address, such as `127.0.0.1` or
`::1`. `net.resolve(host)` returns the addresses `net.connect` would try, in order: names come
from `/etc/hosts` first and then from the DNS servers in `/etc/resolv.conf`, as most programs
find them, but stone does the work itself, so the interpreter and a built program always agree.

```stone
use net

print(net.resolve("localhost"))   // ok(['127.0.0.1', '::1'])
```

That is the whole tutorial. The {doc}`../reference/syntax` and {doc}`../reference/builtins`
pages cover the details, including every method of a [socket](../reference/builtins.md#send)
and the [`net` module](../reference/builtins.md#the-net-module).
