use parity.even

pub def odd(n);
    if n == 0;
        ret false
    ret even.even(n - 1)
