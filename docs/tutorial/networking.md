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
