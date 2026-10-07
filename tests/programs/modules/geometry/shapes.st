use geometry.vec

pub def area(r);
    ret 3 * square(r)

pub def perimeter(r);
    ret 2 * 3 * r

// private, so it does not clash with vec's helper of the same name
def square(x);
    ret vec.dot([x], [x])
