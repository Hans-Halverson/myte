def fib(x) {
    if (x <= 1) {
        return 1
    } else {
        return fib(x - 1) + fib(x - 2)
    }
}