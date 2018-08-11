package LetPolyError

def g(y: a): int {
    def f(x: bool): a {
        return y
    }

    if (f(true)) {
        return f(true) + 5
    } else {
        return 6
    }
}
