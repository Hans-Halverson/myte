package let_poly_error

def g(y) {
    def f(x) {
        return y
    }

    if (f(true)) {
        return f(true) + 5
    } else {
        return 6
    }
}
