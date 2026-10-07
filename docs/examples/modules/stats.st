// summary statistics for lists of ints

pub def mean(xs);
    ret total(xs) / xs.len()

pub def best(xs);
    top = xs[0]
    for x in xs;
        if x > top;
            top = x
    ret top

// only stats.st can call this, since it is not pub
def total(xs);
    sum = 0
    for x in xs;
        sum = sum + x
    ret sum
