// pads s with spaces on the left until it is width characters long
pub def left(s, width);
    while s.len() < width;
        s = " " + s
    ret s
