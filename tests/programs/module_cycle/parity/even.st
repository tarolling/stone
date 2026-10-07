use parity.odd

pub def even(n);
    if n == 0;
        ret true
    ret odd.odd(n - 1)
