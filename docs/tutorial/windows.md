# Pictures and windows

The builtin `gui` module draws pictures and shows them in windows. A picture is a canvas, an
image a program draws on; a window shows a canvas on the screen and reports what the keyboard
and mouse do to it.

## Canvases

`gui.blank(width, height, color)` makes a canvas, and its methods draw on it: `set` a pixel,
`rect` and `outline` a rectangle, `line`, `circle` and `disc`, `blit` another canvas, and
`text` in an 8 by 8 pixel font. A color is an int, `0xRRGGBB`, as `gui.rgb(r, g, b)` makes it.
`get(x, y)` reads a pixel back, as an option since the point may be off the canvas, and
`ppm()` turns the canvas into the bytes of a PPM image file, which most image viewers open.

```{literalinclude} ../examples/canvas.stn
:language: stone
```

prints

```{literalinclude} ../examples/canvas.out
:language: text
```

A canvas is a value like a list, so `d = c` makes a canvas of its own, and a function that
draws on one returns it, as `xs = f(xs)` does. Drawing outside the canvas draws nothing there,
so a shape may hang over an edge.

## Windows

`gui.open(title, width, height)` opens a window, and `show(c)` shows a canvas in it.
`events()` returns what has happened since it was last called, without waiting, as a list of
`gui.event` values: `key_down` and `key_up` with the key's name (`"a"`, `"enter"`, `"left"`),
`text` with what a key typed, `mouse_move`, `mouse_down`, and `mouse_up` with the point (and
the button's name), `scroll`, `resize` with the new size, `redraw` when the window lost what
it showed, and `close` when its close button is pressed. `next_frame()` returns the same
events, after waiting until the screen has shown the last image, so a loop that reads events
with it, draws, and shows makes an animation at the screen's own rate, with no tearing:

```stone
use gui

w = gui.open("bounce", 320, 240).unwrap()
c = gui.blank(320, 240, 0)
x = 0
while w.is_open():
    es = w.next_frame()
    for e in es:
        match e:
            case gui.event.key_down(key):
                if key == "escape":
                    w.close()
            case gui.event.close:
                w.close()
            case _:
                cont
    c.fill(gui.rgb(20, 24, 40))
    c.disc(x, 120, 20, gui.rgb(241, 194, 50))
    w.show(c)
    x = (x + 4) % 320
```

`wait()` waits for at least one event instead, for a program that changes only when the user
does something. Pressing the close button closes nothing by itself: the program decides, here
by calling `close()`.

A window speaks the Wayland or X11 protocol to the display server itself, so a built program
still needs no C library. Every Linux desktop has a Wayland compositor or an X11 server, and a
Mac has an X11 server with XQuartz. `gui.open` finds it through `WAYLAND_DISPLAY` or `DISPLAY`,
preferring Wayland, and without either it returns an error. Where a Wayland compositor draws no
title bars, as on GNOME, `gui` draws its own.

## Apps

Most programs with a window keep some state, change it at each event, and draw it. The
`gui.app` trait names those three steps, and `gui.run(title, width, height, state)` runs them:
at each frame, as `next_frame` paces them, it gives `update` every event, plus a `tick` with
the seconds since the last one, then calls `draw` with a canvas as big as the window and shows what it returns, until
the close button is pressed or `done` returns `true`:

```stone
use gui

struct counter:
    clicks: int

impl gui.app for counter:
    fn update(self, e: gui.event):
        match e:
            case gui.event.mouse_down(_, _, _):
                self.clicks += 1
            case _:
                ret

    fn draw(self, c: gui.canvas) -> gui.canvas:
        c.fill(gui.rgb(255, 255, 255))
        c.text_scaled(10, 10, "clicks: {self.clicks}", 0, 2)
        ret c

    fn done(self) -> bool:
        ret self.clicks >= 10

print(gui.run("counter", 300, 100, counter(clicks=0)))
```

`update` changes the state in place, as a method that changes `self` does, and `draw` only
reads it. `run` returns the last state, so the program can go on with it after the window
closes.

For a bigger program, `examples/spin.stn` in the repository draws spinning 3D shapes: it builds
meshes with `math.sin` and `math.cos`, turns and lights them, sorts their faces with
`sort_by_key`, and fills them in on a canvas, all in stone. Built with
`stone build examples/spin.stn` and run as `examples/build/spin`, it draws 60 frames a second.

That is the whole tutorial. The {doc}`../reference/syntax` and {doc}`../reference/builtins`
pages cover the details, including every method of a [socket](../reference/builtins.md#send)
and of a [canvas and a window](../reference/builtins.md#the-gui-module).
