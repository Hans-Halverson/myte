def f(x) {
    if (x == 0) {
        return 0
    } else {
        return g(x - 1)
    }
}

def g(x) {
    if (x == 0) {
        return 0
    } else {
        return f(x - 1)
    }
}

def main(args) {
    println(intToString(f(10)))
    println(intToString(g(10)))
}