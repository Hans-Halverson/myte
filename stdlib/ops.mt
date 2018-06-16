package std::ops

trait Add<a> {
    sig add(a): a
}

trait Subtract<a> {
    sig subtract(a): a
}

trait Multiply<a> {
    sig multiply(a): a
}

trait Divide<a> {
    sig divide(a): a
}

trait Power<a> {
    sig power(a): a
}

trait Remainder<a> {
    sig remainder(a): a
}

trait Index<k, v> {
    sig index(k): v
}

trait IndexAssign<k, v> {
    sig indexAssign(k, v): v
}
